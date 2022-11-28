package chamberlain_kevin

import scala.collection.mutable
import scala.xml.{Node, Text}

/*
 * This class is for the family rank of the taxonomy hierarchy. This class is the third link in the chain of
 * responsibility. This level will facilitate getting the order name, adding data, displaying the programs data, as well
 * as loading and saving xml files.
 */
class Family(private[this] var family : String) extends TaxNode {
  private var summary = new Summary()

  /*
   * This function is a getter for the family name
   */
  def getFamily() : String = {
    return family
  }

  /*
   * This function facilitates the add data functionality at the family level, entering data for the family level and
   * passing the responsibility down the chain the summary level of this function
   */
  def addData(): Unit = {
    var choice = 'y'

    while (choice == 'y') {
      printf("add feature (y/n):> ")
      choice = scala.io.StdIn.readChar().toLower
      if (choice == 'y') {
        addFeature()
      }
    }

    printf("add summary (y/n):> ")
    choice = scala.io.StdIn.readChar().toLower

    if(choice == 'y'){
      summary.addData()
    }
  }

  /*
   * This function facilitates the display functionality at the family level, printing the order and the features held at
   * this level. Passing the responsibility down the chain to the summary level of this function
   */
  def print(level : Int, str : String) : String = {
    var temp = ""
    temp = "--"*level +"family: " + family
    temp += "\n" + "--"*level+"feature:" + getFeature() + "\n" //TODO fix this
    temp += summary.print(level + 1, temp)

    return temp
  }

  /*
   * This function facilitates saving the programs data to an xml value at the family level, passing the responsibility
   * down the chain to the summary level.
   */
  def saveFile(): Node = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("name", family))
    var sum : Seq[Node] = Seq()
    //var children = sum

    for(x <- features){

      val text = Text(x)
      val feat = XMLHelper.makeNode("feature", null, text)
      sum = sum ++ feat
    }

    sum = sum ++ summary.saveFile()

    XMLHelper.makeNode("family", attr, sum)//add children?
  }

  /*
   * This function facilitates loading an xml file into the program at the family level, passing the responsibility down
   * to the summary level.
   */
  def loadFile(xml : Node): Unit ={
    family = xml.attribute("name").getOrElse("").toString
    val children = xml.child

    for(child <- children){
      val tag = child.label
      tag match {
        case "summary" =>{

          summary.loadFile(child)
        }
        case "feature" =>{

          features += child.text
        }
        case _=> null

      }
    }
  }

  /*
   * This function facilitates finding a feature at the family level by searching through the features held by an
   * instance of a family and returning true to be printed out by the higher level function or if it isn't found it passes
   * back false. This is the lowest level of this function
   */
  def findFeature(feat: String): Boolean = {

    var found = false;
    for (feats <- features) {
      if (feat == feats.toLowerCase()) {
        setContinue(false)
        //continue = false

        return true
      }
    }
    false
  }

  /*
   * This function facilitates displaying the summed species for the entire family,
   * going through every summary to summing the results. The lower level of this would be the getter function for species.
   */
  def sumSpecies(): Int ={

    summary.getSpecies()

  }


}
