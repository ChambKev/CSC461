package chamberlain_kevin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml._

/*
 * This class is for the class rank of the taxonomy hierarchy. This class is the second link in the chain of
 * responsibility. This level will facilitate getting the class name, adding data, displaying the programs data, as well
 * as loading and saving xml files.
 */
class AnimalClass(private[this] var clas : String) extends TaxNode {
  private var orders = ArrayBuffer[Order]()

  /*
   * This function is a getter for the class's name
   */
  def getClas() : String = {
    return clas
  }

  /*
   * This function facilitates the add data functionality at the class level, entering data for the order level and
   * passing the responsibility down the chain the order level of this function
   */
  def addData()= {
    printf("what order:>")
    val choice = scala.io.StdIn.readLine().toLowerCase()

    var found: Boolean = false

    for (order <- orders) {
      if (order.getOrder() == choice) {
        order.addData()
        found = true
      }
    }
    if (!found) {
      orders += new Order(choice)
      println("added order")
      printf("continue (y/n):> ")
      val choise = scala.io.StdIn.readChar().toLower

      if (choise == 'y') {
        var j = 1
        for (order <- orders){
          if(orders.size == j){
            order.addData()
          }
          j+=1
        }
      }
    }
  }

  /*
   * This function facilitates the display functionality at the class level, printing the class and the features held at
   * this level. Passing the responsibility down the chain to the order level of this function
   */
  def print(level : Int, str : String) : String = {
    var temp = ""
    var feat = getFeature()
    temp = "class: " + clas
    temp += "\n" + "feature:" + feat + "\n"
    if (orders.nonEmpty) {
      for (order <- orders) {
        temp += order.print(level + 1, temp)
      }
    }

    return temp
  }

  /*
   * This function facilitates saving the programs data to an xml value at the class level, passing the responsibility
   * down the chain to the order level.
   */
  def saveFile(): Node = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("name", clas))
    var order : Seq[Node] = Seq()
    for(x <- features){
      val text = Text(x)
      val feat = XMLHelper.makeNode("feature", null, text)
      order = order ++ feat
    }
    for (ord <- orders){
      order = order ++ ord.saveFile()
    }

    XMLHelper.makeNode("class", attr, order)//add children?
  }

  /*
   * This function facilitates loading an xml file into the program at the class level, passing the responsibility down
   * to the order level.
   */
  def loadFile(xml : Node): Unit ={
    clas = xml.attribute("name").getOrElse("").toString //attribute see notes
    val children = xml.child

    for(child <- children){
      val tag = child.label
      tag match { //tag see notes

        case "order" =>{
          val order = new Order(null)
          order.loadFile(child)
          orders += order
        }
        case "feature" =>{

          features += child.text.toLowerCase()
        }
        case _=> null

      }
    }
  }

  /*
   * This function facilitates finding a feature at the class level by searching through the features held by an
   * instance of a class and returning true to be printed out by the higher level function or if it isn't found it passes
   * the responsibility of finding the feature down the chain to the order level. If the returning order function finds
   * the feature the output will be printed here.
   */
  def findFeature(feat: String): Boolean ={
    var found : Boolean = false;
    var temp :String = ""
    for(feats <- features) {
      if (feat == feats) {
        return true
      }
    }

    for(order <- orders){
      found = order.findFeature(feat)
      if(!order.getContinue()){
        order.setContinue(true)
       return false
      }
      if(found){
       temp = order.print(0,"")
       found = false
       println(temp)
        temp = ""
        setContinue(false)

       return false
      }
    }
    false
  }

  /*
   * This function is the class level of the CoR to check every instance in the program to look for a true complete
   * variable. Passing the responsibility down the chain to the order level
   */
  def isComp(): Boolean ={
    for(order <- orders){
      if(order.getComplete()){return true}
      else{
        if(order.isComp()){return true}
        //else{return false}
      }
    }
    false
  }

  /*
   * This function is the class level of the chain to reset every instance of complete to its default setting of false,
   * passing the responsibility down to the order level
   */
  def setComp(): Unit ={
    for(order <- orders){
      order.setComplete()
      order.setComp()
    }
  }

  /*
   * This function facilitates displaying the summed species for the entire class given by the user at the class level,
   * going through every order to call the order version of this function, summing the results.
   */
  def sumSpecies(): Int ={
    var species = 0

    for(order <- orders){
      species = species + order.sumSpecies()
    }

    return species
  }

}
