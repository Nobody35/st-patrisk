package classe

abstract class Objective (howMuch : Int)  {
  def isNeeded (c : Country) : Boolean
  val howMuchNeed = howMuch
}