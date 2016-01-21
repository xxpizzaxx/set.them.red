package moe.pizza.setthemred

/**
 * Created by Andi on 21/01/2016.
 */
case class Config(
                   login_url: String = "https://login.eveonline.com/",
                   crest_url: String = "https://crest-tq.eveonline.com/",
                   clientID: String,
                   secretKey: String,
                   redirectUrl: String
                   )
