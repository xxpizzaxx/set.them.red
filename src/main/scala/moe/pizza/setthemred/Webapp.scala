package moe.pizza.setthemred

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import moe.pizza.crestapi.CrestApi
import moe.pizza.eveapi.generated.eve.CharacterID
import moe.pizza.evewho.Evewho
import moe.pizza.setthemred.Types.Alert
import moe.pizza.sparkhelpers.SparkWebScalaHelpers._
import moe.pizza.eveapi.{EVEAPI, SyncableFuture}
import moe.pizza.zkapi.ZKBAPI
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
  val zkb = new ZKBAPI()
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
      case Some(s) => req.clearAlerts()
      case _ => ()
    }
  })
  // login redirect to use CCP auth
  get("/login", (req: Request, resp: Response) => {
    req.session(true)
    resp.redirect(crest.redirect("", defaultCrestScopes))
  })
  // logout
  get("/logout", (req: Request, resp: Response) => {
    req.session.invalidate()
    resp.redirect("/")
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
         val pilots = req.queryParams("names")
          .split('\n')
          .map(_.trim)
          .grouped(250)
          .map(s => eveapi.eve.CharacterID(s))
          .foldRight(Seq.empty[CharacterID.Row]){ (n, a) =>
            n.sync().get.result ++ a
          }
          .map(c => Pilot(c.characterID.toLong, c.name)).toList
        massAdd(s, "your list", pilots, req)
        resp.redirect("/")
      case None =>
        resp.redirect("/")
    }
    ()
  })
  case class Pilot(characterID: Long, characterName: String)

  def massAdd(s: Types.Session, name: String, pilots: List[Pilot], req: Request) = {
      pilots.map(c =>
        (c ,Try {crest.contacts.createContact(s.characterID, s.accessToken, crest.contacts.createCharacterAddRequest(-10, c.characterID, c.characterName, true))})
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
                crest.contacts.createContact (s.characterID, s.accessToken, crest.contacts.createCharacterAddRequest (- 10, t._1.characterID, t._1.characterName, true) )
              })
          }.map(s => (s._1, s._2.map(_.sync(15 seconds))))
          }
       }.groupBy{_._2.isSuccess}
        .mapValues(_.size)
        .foreach { kv =>
          val (inputstatus, count) = kv
          inputstatus match {
            case true  => req.flash(Alerts.success, "Successfully added %d contacts from %s to your watchlist.".format(count, name))
            case false => req.flash(Alerts.danger, "Failed to add %d contacts from %s to your watchlist.".format(count, name))
          }
        }
  }
  post("/add/evewho", (req: Request, resp: Response) => {
    req.getSession match {
      case Some(s) =>
        val name = req.queryParams("corp")
        val id = eveapi.eve.CharacterID(Seq(name)).sync().get.result.head.characterID.toLong
        val typeOfThing = eveapi.eve.OwnerID(Seq(name)).sync().get.result.head.ownerGroupID.toInt
        val evewholist = typeOfThing match {
          case 2 => evewho.corporationList(id).sync().characters
          case 32 => evewho.allianceList(id).sync().characters
        }
        massAdd(s, name, evewholist.map(c => Pilot(c.character_id, c.name)), req)
        resp.redirect("/")
      case None =>
        resp.redirect("/")
    }
    ()
  })
  post("/add/zkbsupers", (req: Request, resp: Response) => {
    req.getSession match {
      case Some(s) =>
        val name = req.queryParams("group")
        val id = eveapi.eve.CharacterID(Seq(name)).sync().get.result.head.characterID.toLong
        val typeOfThing = eveapi.eve.OwnerID(Seq(name)).sync().get.result.head.ownerGroupID.toInt
        val zkblist = typeOfThing match {
          case 2 => zkb.stats.corporation(id).sync().map(s => s.supers.supercarriers.data ++ s.supers.titans.data).get
          case 32 => zkb.stats.alliance(id).sync().map(s => s.supers.supercarriers.data ++ s.supers.titans.data).get
        }
        massAdd(s, name, zkblist.map(c => Pilot(c.characterID, c.characterName)), req)
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
