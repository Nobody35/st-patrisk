package classe

import scala.swing.Publisher


class Game (player0 : Player) extends Publisher {
  var currentPlayer  = player0 /*todo changer*/
    /***********basic functions************/
  
  /*test if the new number is negative*/
  /*n could be negative*/
  /*seul à avoir acces à armyNumber avec move army*/
  def addArmy(country: Country, n : Int ) ={ 
	country.addArmy(n)
	publish(eChangeArmy(country, country.armyNumber))
  }
  /*seul à avoir acces à owner*/
  def changeOwner(country: Country, newOwner : Player) {
    val precedentOwner = country.owner
    country.changeOwner(newOwner)
    publish(eChangeOwner(country, newOwner, precedentOwner))
    /* compute changement for continent and objective*/
    newOwner.winCountry(country)
    precedentOwner.loseCountry(country)
  }
  /*don't check they are neighbors*/
  def moveArmy (countryFrom : Country, countryTo : Country , n : Int) { 
      publish(eMoveArmy(countryFrom, countryTo, n))
	  addArmy(countryFrom,-n);
      addArmy(countryTo,n);
  }
  val rand = new scala.util.Random(142); /*todo changer*/  
  def Roll () : Int = {
   	val n = rand.nextInt(6) +1
   	println("Roll " + n )
   	n
  }
   
  
  
  /******** turn phasis*******/ 
  
  /**begining**/
  def nextTurn() {
    currentPlayer = currentPlayer.follower 
    if (!currentPlayer.isAlive) nextTurn() 
    else {
      publish(eBeginTurn(currentPlayer,currentPlayer.computeReinforcement()))
    }
    
  }
  
  /**reinforcement**/
  /*
   * the list is a list of countries,int
   * to check by interface :
   * all countries are owned by the current player
   * the sum is the number sent in eBeginTurn (the number reinforcement)
   */
  def reinforcement (toAdd  : List[(Country,Int)]) {
    var n = currentPlayer.computeReinforcement ()
    def aux (l : List[(Country,Int)]) { 
      l match {
      	case Nil => {/*if (n != 0) todo erreur*/}
      	case ((c,p)::t) => {
      	  if (c.owner == currentPlayer) {
      	    addArmy(c, p)
      	    n -= p
      	    aux (t)
      	  }
      	  else {/*todo error*/}
      	  
      	}
      }
    }
    aux(toAdd)
  }
  
  /*** battle ***/
  
  /**là je ne te donne l'enchainement**/
  
  /* 
   * battleBegin
   * battleCheck
   * battle
   * battleWon (if the battle is won)
   */
  
  
  
 /* 
  * check if the country is owned by the current player
  * send a pair :
  * the list of country enemy arround (he has to chose one)
  *  the max of army that he could put (he has to chose between 1 and this number)
  * */
  def battleBegin(countryFrom : Country) : (List[Country],Int)= {
    def isEnemy (c : Country) : Boolean = {
      c.owner != currentPlayer
    } 
	if (isEnemy(countryFrom)) {/*todo*/}
	(
	    countryFrom.neighbors.filter(isEnemy) ,
	    scala.math.min(countryFrom.armyNumber-1,3)
	)
  }
  /*j'implémente ça ?*/
  def battleCheck (countryAttaq : Country,countryDef : Country, diceAttaq : Int ) {/*todo*/}
  
  def battle (countryAttaq : Country,countryDef : Country, diceAttaq : Int ) {
    val na = diceAttaq;
    val nd = if (countryDef.armyNumber == 1) 1 else 2; 
    val (la,ld) = fight (na,nd)
    addArmy(countryAttaq,-la);
    addArmy(countryDef,-ld);
    if ((countryDef.armyNumber) == 0) {
    	changeOwner(countryDef,countryAttaq.owner)
    	/*todo ask : */
  		/*publish(ebattleWon(countryAttaq.armyNumber-1,na) */
    }
  }
  /* toMove must be as publish(ebattleWon asked*/ 
  def battleWon (countryAttaq : Country,countryDef : Country, toMove : Int ) {
    moveArmy(countryAttaq, countryDef , toMove)
  }

  
  
  
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
	 publish(eFight(va, vd, la, ld))
	 (la,ld)	 
  }
  /** endTurn**/
  def endTurn() {
    beginTurn()
  }
}
