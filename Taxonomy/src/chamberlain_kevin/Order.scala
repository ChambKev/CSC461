package chamberlain_kevin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.xml.{Node, Text}

/*
 * This class is for the order rank of the taxonomy hierarchy. This class is the third link in the chain of
 * responsibility. This level will facilitate getting the order name, adding data, displaying the programs data, as well
 * as loading and saving xml files.
 */
class Order(private[this] var order : String) extends TaxNode {
  private var families = ArrayBuffer[Family]()

  /*
   * This functino is a getter for the class's name(order name)
   */
  def getOrder() : String = {
    return order
  }

  /*
   * This function facilitates the add data functionality at the order level, entering data for the family level and
   * passing the responsibility down the chain the family level of this function
   */
  def addData()= {
    printf("what family:>")
    val choice = scala.io.StdIn.readLine().toLowerCase()

    var found: Boolean = false

    for (family <- families) {
      if (family.getFamily() == choice) {
        family.addData()
        found = true
      }
    }
    if (!found) {
      families += new Family(choice)
      println("added family")
      printf("continue (y/n):> ")
      val choise = scala.io.StdIn.readChar().toLower

      if (choise == 'y') {
        var j = 1
        for (family <- families){
          if(families.size == j){
            family.addData()
          }
          j+=1
        }
      }

    }
    //else end function
  }

  /*
   * This function facilitates the display functionality at the order level, printing the order and the features held at
   * this level. Passing the responsibility down the chain to the family level of this function
   */
  def print(level : Int, str : String) : String = {
    var temp = ""
    var feat = getFeature()
    temp = "--" * level +"order: " + order //CHANGED
    temp += "\n" + "--"* level +"feature:" + feat + "\n"
    if (families.nonEmpty) {
      for (family <- families) {
        temp += family.print(level + 1, temp)
      }
    }

    return temp
  }

  /*
   * This function facilitates saving the programs data to an xml value at the order level, passing the responsibility
   * down the chain to the family level.
   */
  def saveFile(): Node = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("name", order))
    var fam : Seq[Node] = Seq()
    for(x <- features){
      val text = Text(x.toLowerCase()) //HERE
      val feat = XMLHelper.makeNode("feature", null, text)
      fam = fam ++ feat
    }
    for (family <- families){
      fam = fam ++ family.saveFile()
    }

    XMLHelper.makeNode("order", attr, fam)//add children?
  }

    /*
   * This function facilitates loading an xml file into the program at the order level, passing the responsibility down
   * to the family level.
   */
  def loadFile(xml : Node): Unit ={
    order = xml.attribute("name").getOrElse("").toString
    val children = xml.child

    for(child <- children){
      val tag = child.label
      tag match {
        case "family" =>{

          val family = new Family(null)
          family.loadFile(child)
          families += family
        }
        case "feature" =>{

          features += child.text.toLowerCase() //HERE
        }
        case _=> null

      }
    }
  }

  /*
   * This function facilitates finding a feature at the order level by searching through the features held by an
   * instance of a order and returning true to be printed out by the higher level function or if it isn't found it passes
   * the responsibility of finding the feature down the chain to the family level. If the returning family function finds
   * the feature the output will be printed here.
   */
  def findFeature(feat: String): Boolean = {
    var ret : Boolean = false
    var found = false;
    var temp : String = ""
    temp = ""
    for (feats <- features) {
      if (feat == feats) {
        ret = true
        return true
      }
    }
    for (family <- families) {
      found = family.findFeature(feat)
      if(!family.getContinue()){
        family.setContinue(true)
        temp=family.print(0,"") //HOLDS FANG1
        println(temp)
        temp = ""
        return false
      }
      if (found) {
        temp = family.print(0,"")
        found = false
        setContinue(false)
        //continue = false
        println(temp)
        temp = ""
        ret = false
      }
    }
    ret
  }

  /*
   * This function is the order level of the CoR to check every instance in the program to look for a true complete
   * variable. Passing the responsibility down the chain to the family level
   */
  def isComp(): Boolean ={
    for(family <- families){
      if(family.getComplete()){return true}
    }
    false
  }

  /*
   * This function is the order level of the chain to reset every instance of complete to its default setting of false,
   * passing the responsibility down to the family level
   */
  def setComp(): Unit ={
    for(family <- families){
      family.setComplete()

    }
  }

  /*
   * This function facilitates displaying the summed species for the entire order at the order level,
   * going through every family to call the family version of this function, summing the results.
   */
  def sumSpecies(): Int ={
    var species = 0

    for(family <- families){
      species = species + family.sumSpecies()
    }

    return species
  }

}
