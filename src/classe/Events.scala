package classe

import scala.swing.event.Event

/*
 * les infos que tu dois connaitre :
 * 
 * le nombre d'armée par pays
 * les possésseurs des pays
 * les voisisn de chaque pays (si tu veux griser)
 */

case class eChangeArmy(country : Country, n : Int) extends Event
case class eChangeOwner (country : Country, newOwner : Player, precedentOwner : Player ) extends Event
case class eMoveArmy(countryFrom : Country , countryTo : Country , n : Int) extends Event
case class eContinentWon (continent : Continent , player : Player) extends Event
case class eContinentLost (continent : Continent , player : Player) extends Event
case class eGameWon(player : Player) extends Event
case class eGameLost(player : Player) extends Event
case class eFight (dicesAttaq :Array[Int], dicesDef :Array[Int], loseAttaq : Int , loseDef  : Int ) extends Event
case class eBeginTurn (player : Player, reinforcement : Int) extends Event
