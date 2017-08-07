package recfun

import scala.collection.mutable.ListBuffer
/**
  * @author Maria-Bianca Cindroi
  * */
object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println()
    println("Paranthesis Balancing")
    val prop: String = "(Bianca"
    val sentence : List[Char] = prop.toList
    val result: Boolean = balance(sentence)
    println(result)

    println()
    println("Change Counter")
    println(countChange(20, List(1, 2, 5, 10)))
    //use unittests to check
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c != 0 && c != r){
      pascal(c - 1, r - 1) + pascal(c, r -1)
    }
    else{
      1
    }
  }

/**
  * Exercise 2
  */
def balance(chars: List[Char]): Boolean = {

  def checkCharacter(character: Char, unpairedParanth: Int): Int =
    if (character == '(') unpairedParanth + 1
    else if (character == ')')
      unpairedParanth - 1
    else
      unpairedParanth

  def checkSentence(sentence: List[Char], counted: Int): Int = {

    if (sentence.isEmpty)
      //if there are no character left, remain on same amount
      counted
    else if (counted == -1)
      //if there is an inbalanced (more closed, than opened)number of parantheses, remain on -1
      -1
    else
    //otherwise recheck on the tail of the sentence
      checkSentence(sentence.tail, checkCharacter(sentence.head, counted))
  }

  val result : Int= checkSentence(chars, 0)
  //return the result of the sentence check accourdingly, if 0 then all parantheses are matched
  //otherwise they aren't
  if (result == 0)
    true
  else
    false
}
//  checkChar(chars, 0)



/**
  * Exercise 3
  */
def countChange(money: Int, coins: List[Int]): Int = {
  //count in how many ways, one can get change

  def counter(total: List[(Int, Int)], count: Int): Int = {
    if (total.isEmpty)
      count

    //if there are still money left to be changed
    else{
      val newTotal = ListBuffer[(Int, Int)]()
      var countLeftOvers : Int = count

      for ((lastChange, counted) <- total){
        if (counted < money)
          //if there are still money left to be changed
          for (coin <- coins){
            if (coin >= lastChange){
              val recounted  = (coin, counted + coin)
              newTotal += recounted
            }
          }

        else if (counted == money)
          //otherwise finish changing
         countLeftOvers += 1
      }
      counter(newTotal.toList, countLeftOvers)
    }
  }
  val recount = coins.map{ coin => (coin, coin)}
  counter (recount, 0)
}
}
