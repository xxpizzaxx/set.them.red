package moe.pizza.setthemred

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import moe.pizza.crestapi.CrestApi
import moe.pizza.setthemred.Types.Alert
import moe.pizza.sparkhelpers.SparkWebScalaHelpers._
import moe.pizza.eveapi.SyncableFuture
import spark.Spark._
import spark._

import scala.concurrent.ExecutionContext.Implicits.global

object Webapp extends App {

  val configtext = getClass.getResourceAsStream("/config.json")
  val OM = new ObjectMapper()
  OM.registerModule(DefaultScalaModule)
  val config = OM.readValue(configtext, classOf[Config])

  val crest = new CrestApi(baseurl = config.login_url, cresturl = config.crest_url, config.clientID, config.secretKey, config.redirectUrl)
  val defaultCrestScopes = List("characterContactsRead", "characterContactsWrite")
  val SESSION = "session"

  staticFileLocation("/static")

  // index page
  get("/", (req: Request, resp: Response) => {
    if (req.session.attribute[Types.Session](SESSION) != null) {
      templates.html.base.apply("set.them.red", templates.html.landing.apply(), Some(req.session.attribute[Types.Session](SESSION)))
    } else {
      templates.html.base.apply("set.them.red", templates.html.landing.apply(), None)
    }
  })
  after("/", (req: Request, resp: Response) => {
    val session = Option(req.session.attribute[Types.Session](SESSION))
    session match {
      case Some(s) => req.session.attribute(SESSION, s.copy(alerts = List()))
      case _ => ()
    }
  })
  // login redirect to use CCP auth
  get("/login", (req: Request, resp: Response) => {
    req.session(true)
    resp.redirect(crest.redirect("", defaultCrestScopes))
  })
  // callback for when CCP auth sends them back
  get("/callback", (req: Request, resp: Response) => {
    val code = req.queryParams("code")
    val state = req.queryParams("state")
    val callbackresults = crest.callback(code).sync()
    val verify = crest.verify(callbackresults.access_token).sync()
    val session = new Types.Session(callbackresults.access_token, callbackresults.refresh_token.get, verify.characterName, verify.characterID, List(new Alert("success", "Thanks for logging in %s".format(verify.characterName))))
    req.session.attribute(SESSION, session)
    // go back to the index since we've just logged in
    resp.redirect("/")
  })
}
