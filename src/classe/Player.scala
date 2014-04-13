package classe

import scala.swing.Publisher


class Player (obj : Objective, numberOfContinent : Int, follow : Player) extends Publisher{
  var countriesOwned = 0
  var continentsOwned = 0;
  val follower = follow
  val continentComptor = new Array[Int] (numberOfContinent)
  val objective = obj;
  var stillNeed = obj.howMuchNeed
  var isAlive = true
  var reinforcementBonus = 0
  def winCountry (c : Country) ={
	countriesOwned += 1
    if (objective.isNeeded(c)) {
      stillNeed -=1
      if (stillNeed == 0) {
        publish(eGameWon(this))
      };
    }
    continentComptor(c.continent.id) += 1
    if (continentComptor(c.continent.id) == c.continent.countriesNumber) 
    { 
      /*won a continent*/
      publish(eContinentWon(c.continent,this))
      c.continent.isOwned = true
      c.continent.owner = this
      reinforcementBonus += c.continent.bonus
    }
  }
  def loseCountry (c : Country)={
	countriesOwned -= 1
    if (objective.isNeeded(c)) {
      stillNeed -=1
    }
	if (countriesOwned == 0) {
        publish(eGameLost(this))
        isAlive = false
      };
    if (continentComptor(c.continent.id) == c.continent.countriesNumber) 
    {
      /*lose a continent*/
      publish(eContinentLost(c.continent,this))
      c.continent.isOwned = false  
      reinforcementBonus -= c.continent.bonus
    }
    continentComptor(c.continent.id) -= 1
  } 
  def computeReinforcement () : Int  = {
    return ((countriesOwned / 3) + reinforcementBonus) 
  }
  
}