package chamberlain_kevin

import scala.collection.mutable.ArrayBuffer

/*
 * This class is the node class that AnimalClass, Order, and Family classes inherit from. This class will hold faetures
 * for every instance created, as well as a status of whether or not the program should continue down a path or not.
 * As well as holding a status whether or not a feature has been found and the process is complete.
 */
class TaxNode {
  var features = ArrayBuffer[String]()
  var continue = true //to continue finding a feature or not
  var complete = false //whether a feature has been found

  /*
   * This function will take in the feature for every instance
   */
  def addFeature(): Unit = {

      printf("what feature:> ")
      val feat = scala.io.StdIn.readLine()
      features += feat.toLowerCase()

  }

  /*
   * This function is a getter for the complete status
   */
  def getComplete(): Boolean ={
    return complete
  }

  /*
   * This function is a getter for the continue status
   */
  def getContinue(): Boolean ={
    return continue
  }

  /*
   * This function is a setter for the complete status
   */
  def setComplete(): Unit ={
    complete = false
  }

  /*
   * This function is a setter for the continue status
   */
  def setContinue(cont : Boolean) : Unit={
    if(cont == false){
      complete = true
    }
    continue = cont
  }

  /*
   * This function is a getter function for the feature string
   */
  def getFeature(): String = {
    var temp = " "
    var j = 0
    for(i <- features){

      if(j>0){
        temp += ", "
      }
      temp += i
      j+= 1
    }
    return temp
  }

}
