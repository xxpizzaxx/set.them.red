package moe.pizza.setthemred

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import moe.pizza.crestapi.CrestApi
import moe.pizza.eveapi.generated.eve.{CharacterID, CharacterAffiliation}
import moe.pizza.evewho.Evewho
import moe.pizza.setthemred.Types.Alert
import moe.pizza.sparkhelpers.SparkWebScalaHelpers._
import moe.pizza.eveapi.{EVEAPI, SyncableFuture}
import play.twirl.api.Html
import spark.Spark._
import spark._
import Utils._
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

object Webapp extends App {

  val configtext = getClass.getResourceAsStream("/config.json")
  val OM = new ObjectMapper()
  OM.registerModule(DefaultScalaModule)
  val config = OM.readValue(configtext, classOf[Config])
  val log = org.log4s.getLogger

  val crest = new CrestApi(baseurl = config.login_url, cresturl = config.crest_url, config.clientID, config.secretKey, config.redirectUrl)
  val eveapi = new EVEAPI()
  val evewho = new Evewho()
  val defaultCrestScopes = List("characterContactsRead", "characterContactsWrite")
  val SESSION = "session"

  port(9020)
  staticFileLocation("/static")

  // index page
  get("/", (req: Request, resp: Response) => {
    req.getSession match {
      case Some(s) => templates.html.base.apply("set.them.red", templates.html.main.apply(), Some(s))
      case None => templates.html.base.apply("set.them.red", templates.html.landing.apply(), None)
    }
  })
  after("/", (req: Request, resp: Response) => {
    val session = req.getSession
    session match {
      case Some(s) => req.clearAlerts
      case _ => ()
    }
  })
  // login redirect to use CCP auth
  get("/login", (req: Request, resp: Response) => {
    req.session(true)
    resp.redirect(crest.redirect("", defaultCrestScopes))
  })
  // post endpoints for doing basic things
  post("/add/character", (req: Request, resp: Response) => {
    req.getSession match {
      case Some(s) =>
        val name = req.queryParams("name")
        val id = eveapi.eve.CharacterID(Seq(name)).sync().get.result.head.characterID.toLong
        val result = Try { crest.contacts.createContact(s.characterID, s.accessToken, crest.contacts.createCharacterAddRequest(-10, id, name, true)).sync() }
        result match {
          case Success(r) => req.flash(Alerts.success, "Added %s to your watchlist".format(name))
          case Failure(t) => req.flash(Alerts.danger, "Unable to add %s to your watchlist".format(name))
        }
        resp.redirect("/")
      case None =>
        resp.redirect("/")
    }
    ()
  })
  post("/add/characters", (req: Request, resp: Response) => {
    req.getSession match {
      case Some(s) =>
        req.queryParams("names")
          .split('\n')
          .map(_.trim)
          .grouped(250)
          .map(s => eveapi.eve.CharacterID(s))
          .foldRight(Seq.empty[CharacterID.Row]){ (n, a) =>
            n.sync().get.result ++ a
          }.map { row =>
            val car = crest.contacts.createCharacterAddRequest(-10, row.characterID.toLong, row.name, true)
            Try { crest.contacts.createContact(s.characterID, s.accessToken, car) }
          }.map(s => s.map(_.sync()))
          .groupBy(_.isSuccess)
          .mapValues(_.size)
          .foreach { kv =>
            val (status, count) = kv
            status match {
              case true  => req.flash(Alerts.success, "Successfully added %d contacts to your watchlist.".format(count))
              case false => req.flash(Alerts.danger, "Failed to add %d contacts to your watchlist.".format(count))
            }
          }
        resp.redirect("/")
      case None =>
        resp.redirect("/")
    }
    ()
  })
    post("/add/corporation", (req: Request, resp: Response) => {
    req.getSession match {
      case Some(s) =>
        val corpname = req.queryParams("corp")
        val corpid = eveapi.eve.CharacterID(Seq(corpname)).sync().get.result.head.characterID.toLong
        val evewholist = evewho.corporationList(corpid).sync()
        evewholist.characters.map(c =>
          (c ,Try {crest.contacts.createContact(s.characterID, s.accessToken, crest.contacts.createCharacterAddRequest(-10, c.character_id, c.name, true))})
        )
          .map(s => (s._1,  s._2.map(_.sync(15 seconds))))
          .groupBy(_._2.isSuccess)
          .flatMap { kv =>
            val (state, attempts) = kv
            state match {
              case true => attempts
              case false =>
                attempts.map { t =>
                log.error ("failed with error %s".format (t._2.failed.get) )
                (t._1, Try {
                  crest.contacts.createContact (s.characterID, s.accessToken, crest.contacts.createCharacterAddRequest (- 10, t._1.character_id, t._1.name, true) )
                })
            }.map(s => (s._1, s._2.map(_.sync(15 seconds))))
            }
         }.groupBy{_._2.isSuccess}
          .mapValues(_.size)
          .foreach { kv =>
            val (inputstatus, count) = kv
            inputstatus match {
              case true  => req.flash(Alerts.success, "Successfully added %d contacts from corporation %s to your watchlist.".format(count, corpname))
              case false => req.flash(Alerts.danger, "Failed to add %d contacts from corporation %s to your watchlist.".format(count, corpname))
            }
          }
        resp.redirect("/")
      case None =>
        resp.redirect("/")
    }
    ()
  })
  // callback for when CCP auth sends them back
  get("/callback", (req: Request, resp: Response) => {
    val code = req.queryParams("code")
    val state = req.queryParams("state")
    val callbackresults = crest.callback(code).sync()
    val verify = crest.verify(callbackresults.access_token).sync()
    val session = new Types.Session(callbackresults.access_token, callbackresults.refresh_token.get, verify.characterName, verify.characterID, List(new Alert("success", "Thanks for logging in %s".format(verify.characterName))))
    req.setSession(session)
    // go back to the index since we've just logged in
    resp.redirect("/")
  })
}
