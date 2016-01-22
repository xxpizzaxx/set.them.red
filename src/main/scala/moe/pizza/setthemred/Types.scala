package moe.pizza.setthemred

/**
 * Created by Andi on 22/01/2016.
 */
object Types {
  case class Alert(level: String, content: String)
  case class Session(accessToken: String, refreshToken: String, characterName: String, characterID: Long, alerts: List[Alert])
}
