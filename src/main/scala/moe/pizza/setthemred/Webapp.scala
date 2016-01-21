package moe.pizza.setthemred

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import moe.pizza.crestapi.CrestApi
import moe.pizza.sparkhelpers.SparkWebScalaHelpers._
import moe.pizza.eveapi.SyncableFuture
import spark.Spark._
import spark._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object Webapp extends App {

  val configtext = getClass.getResourceAsStream("/config.json")
  val OM = new ObjectMapper()
  OM.registerModule(DefaultScalaModule)
  val config = OM.readValue(configtext, classOf[Config])

  val crest = new CrestApi(baseurl = config.login_url, cresturl = config.crest_url, config.clientID, config.secretKey, config.redirectUrl)
  val defaultCrestScopes = List("characterContactsRead", "characterContactsWrite")
  val AUTHENTICATED = "authenticated"

  staticFileLocation("/static")
  // set the authenticated flag on their session if they don't have it
  before((req: Request, resp: Response) => {
    Option(req.session.attribute[Boolean](AUTHENTICATED)) match {
      case None => req.session.attribute(AUTHENTICATED, false)
      case _ => ()
    }
  }
  )

  // index page
  get("/", (req: Request, resp: Response) => {
    if (req.session.attribute[Boolean](AUTHENTICATED)) {
      templates.html.base.apply("set.them.red", "hi you're cool and logged in")
    } else {
      templates.html.base.apply("set.them.red", "hi you're cool")
    }
  })
  // login redirect to use CCP auth
  get("/login", (req: Request, resp: Response) => {
    req.session(true)
    val securetoken = Random.alphanumeric.take(32).mkString
    req.session.attribute("logintoken", securetoken)
    resp.redirect(crest.redirect(securetoken, defaultCrestScopes))
    ""
  })
  // callback for when CCP auth sends them back
  get("/callback", (req: Request, resp: Response) => {
    val code = req.queryParams("code")
    val state = req.queryParams("state")
    state match {
      case s if req.session.attribute("logintoken") != s =>
        halt(401, "stop trying to spoof logins")
      case s =>
        val callbackresults = crest.callback(code).sync()
        val verify = crest.verify(callbackresults.access_token).sync()
        req.session.attribute(AUTHENTICATED, true)
        req.session.attribute("access_token", callbackresults.access_token)
        // assume we have a refresh token since we always ask for scopes
        req.session.attribute("refresh_token", callbackresults.refresh_token.get)
        req.session.attribute("characterName", verify.characterName)
        req.session.attribute("characterID", verify.characterID)
        // go back to the index since we've just logged in
        resp.redirect("/")
    }
  })
}
