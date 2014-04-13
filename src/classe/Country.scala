package classe

class Country (owner0 : Player, cont : Continent){
	var neighbors : List[Country]= Nil
	def isNeighbor(c : Country) : Boolean  = {
	  neighbors.contains(c) 
	}
	var owner = owner0
	val continent = cont
	def changeOwner(newOwner : Player) = {
	  owner = newOwner
	}
	var armyNumber = 0
	def addArmy (n : Int) ={
	  /*n could be negative*/
	  armyNumber += n
	}
} 