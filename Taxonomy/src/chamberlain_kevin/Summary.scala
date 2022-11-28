package chamberlain_kevin

import scala.collection.mutable
import scala.xml._

/*
 * This class acts as a catch all for the lower ranks of the taxonomy hierarchy (genus, species, and examples) instead
 * of adding names as the upper level classes do this class will store simply the counts. The functionality of this
 * class is its ability to display, add counts, save and load xml files.
 */
class Summary() {
  private var genus = 0
  private var species = 0
  private var examples = ""

  /*
  * This function facilitates the display functionality at the summary level, printing the genus, species, and examples
  * counts.
  *
  */
  def print(level : Int, str : String) : String = {
    var temp = ""
    examples = examples.trim
    temp = "--"*level+"genus: " + genus + "  species: " + species + "  examples: " + examples + "\n"

    return temp
  }

  /*
   * This function facilitates the add data functionality at the summary level, entering data for the genus, species,
   * and examples
   */
  def addData(): Unit = {
    printf("update genus count ("+ genus +"):> ")
    val gen = scala.io.StdIn.readInt()
    genus = gen
    printf("update species count ("+species+"):> ")
    val spec = scala.io.StdIn.readInt()
    species = spec

    var choice = 'y'
    while (choice == 'y') {
      printf("add example (y/n):> ")
      choice = scala.io.StdIn.readChar().toLower

      if (choice == 'y') {
        printf("what example:> ")
        val ex = scala.io.StdIn.readLine()
        val size = examples.size
        if(size>0){
          examples +=","
        }
        examples += " "+ex

      }
    }

  }

  /*
   * This function facilitates saving the programs data to an xml value at the summary level
   */
  def saveFile(): Node = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("species", species.toString()),("genus", genus.toString()))
    var ex: String = ""
    var i = 0

    for(x <- examples){

      ex = ex + x
      i+=1
    }
    ex = ex.trim
    val text = Text(ex)
    XMLHelper.makeNode("summary", attr, text)//add children?
  }

  /*
   * This function facilitates loading an xml file into the program at the summary level.
   */
  def loadFile(xml : Node): Unit ={
    if (xml.attributes("genus") != null){
      genus = xml.attribute("genus").getOrElse("").toString.toInt
    }

    if(xml.attributes("species") != null){
      species = xml.attribute("species").getOrElse("").toString.toInt
    }
    examples += xml.text
  }

  /*
   * This function is a getter for the species
   */
  def getSpecies(): Int ={
    return species
  }
}
