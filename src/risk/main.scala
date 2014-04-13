package risk

import java.util.List
import com.sun.org.apache.xpath.internal.operations.Minus

trait Country
trait Player 
trait Continent

trait FixMap {
  def neighbors(country : Int) : List [Int]
  def neighbors(country:Int, country2 : Int) : Boolean
  def whichContinent (c : Int) : Int
  def continentBonus (continent : Int) : Int
  val numberOfContry : Int
}

abstract class Question [A,B] {
	def print(conditions : A) 
	def consequence (consequences : B)  
	def cancel()
}




 
abstract class MultipleQuestions


trait ThePublisher {
  def ask[A,B] (q: Question[A,B]) ()
  def askBattle () : (Int,Int) 
  def askDoBattle () : Boolean
  def askDoFight () : Boolean
  def askFight (max : Int, min : Int) : Int
  def askHowMuchMove (max : Int, min : Int) : Int
  def askReinforcement (max : Int) : (Int,Int)
  def askDoMove () : Boolean
  def askWhereMove () : (Int,Int)
  
  def printAddArmy (country: Int, n : Int )
  def printChangeOwner(country: Int, playerWin : Int)
  def printMoveArmy (countryFrom : Int, countryTo : Int , n : Int)
  def printFight (va : Array[Int], vd : Array[Int], la : Int, ld : Int )
  def printBattle (ca : Int, cd : Int)
  def printWinBattle()
  def printLoseBattle()
}


class Game (nPlayer: Int, fixMap : FixMap , ThePublisher : ThePublisher) {
  val countryOwners  = new Array[Int] (fixMap.numberOfContry)
  val countryArmy = new Array[Int] (fixMap.numberOfContry)
  val rand = new scala.util.Random(142); /*todo changer*/
    
  /*game*/
  def game () {
    begining()
    var currentPlayer = nPlayer -1 /*todo the beginer*/
    while (!doHeWin(currentPlayer)) {      
      currentPlayer += 1
      currentPlayer %= nPlayer
      turn(currentPlayer)
    }
    win(currentPlayer)
  }
  
  /*basic functions*/
  /*n could be negative*/
  /*seul à avoir acces à country army*/
  def addArmy(country: Int, n : Int ) ={ 
	countryArmy(country) += n
	ThePublisher.printAddArmy(country, n)
  }
  /*seul à avoir acces à country owner*/
  def changeOwner(country: Int, playerWin : Int) {
    countryOwners(country) = playerWin
    ThePublisher.printChangeOwner(country, playerWin)
    /*todo change les compteurs pour la win*/
  }
  /*don't check they are neigbors*/
  def moveArmy (countryFrom : Int, countryTo : Int , n : Int) { 
      addArmy(countryFrom,-n);
      addArmy(countryTo,n);
      ThePublisher.printMoveArmy(countryFrom, countryTo, n)
  }
  def Roll () : Int = {
   	val n = rand.nextInt(6) +1
   	println("Roll " + n )
   	n
  }
    z -( (-))
  /*begining*/
  def begining() = {} /*todo : place country*/
  
  /* turn*/
  
  def turn(player : Int) = {
    reinforcement (player);
    war(player)
    moving(player);
  }
  /*reinforcement*/
  def reinforcement (player : Int) {
    var n = computeReinforcement (player)
    while (n > 0) {
      val (country,k) = ThePublisher.askReinforcement(n)
      addArmy(country, k)
      n -= k 
    }
  }
  
  def computeReinforcement (player : Int) : Int = {3}  /*todo*/
  
  
  def war (player : Int) = {
    while (ThePublisher.askDoBattle()) {
    	val (c1,c2) = ThePublisher.askBattle()
    	battle (c1,c2)  
     }
   }
  def battle (ca :Int, cd: Int) {
    var fightContinue = true
    ThePublisher.printBattle(ca, cd)
    while (fightContinue && ThePublisher.askDoFight()) {
    	val ma = countryArmy(ca);
    	if (ma >1) {
    		val na = ThePublisher.askFight(scala.math.min(ma-1,3),1)
    		val nd = if (countryArmy(cd) == 1) 1 else 2; 
    		val (la,ld) = fight (na,nd)
    		addArmy(ca,-la);
    		addArmy(cd,-ld);
    		if (countryArmy(cd) == 0) {
    		fightContinue= false;
    	  	changeOwner(cd, countryOwners(ca))
    	  	ThePublisher.printWinBattle()
    	  	ThePublisher.askHowMuchMove(ma-1,na)
      	  	moveArmy(ca, cd, ThePublisher.askHowMuchMove(ma-1,na))
    		};
    	}
    	else {fightContinue= false; 
    	ThePublisher.printLoseBattle()
    	}
    }
   };
  def fight (na : Int, nd : Int) : (Int,Int) = {
    val da = scala.math.min(na, 3)
    val dd = scala.math.min(nd, 2)
	val va = new Array[Int](da)
	 val vd = new Array[Int] (dd)
	 var i = 0 
	 for (i <- 0 until da) {
	   va(i) = Roll()
	 }
	 var j = 0
	 for (j <- 0 until dd) {
	   vd(j) = Roll()
	 }
	 scala.util.Sorting.stableSort(va)
	 scala.util.Sorting.stableSort(vd)
	 val d = scala.math.min(da,dd)
	 var la = 0
	 var ld = 0
	 var k = 0 
	 for (k <- 0 until d ) {
	  if (va(da- k - 1) > vd(dd- k - 1)) {
	   ld += 1
	  }
	  else {
	    la += 1
	  }
	 }
	 ThePublisher.printFight(va, vd, la, ld)
	 (la,ld)	 
  }
  
  def moving (player : Int) {
   if (ThePublisher.askDoMove()) {
	val (c1,c2) = ThePublisher.askWhereMove()
	if (fixMap.neighbors(c1, c2) && countryOwners(c1) == player && countryOwners(c2) == player) {
	 moveArmy(c1, c2, ThePublisher.askHowMuchMove((countryArmy(c1)-1),0))
	}
	/*todo else*/	  
   }
  }
  
  def doHeWin (player : Int ) : Boolean = {false}  /*todo*/
  def win(player : Int) {/*todo*/}
  
}


object main {

  def main(args: Array[String]): Unit = {}

}