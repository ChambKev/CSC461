package chamberlain_kevin

import scala.collection.mutable.ArrayBuffer
import java.io.{FileNotFoundException, FileWriter}
import scala.xml._

/*
* The Taxonomy file is the start of all processes for this program. It is the highest level that once a class is decided
* will tickle down into the lower ranks. This file will facilitate adding data, removing classes, loading xml files,
* writing xml files (save file), finding features, and summing species. This function will also have utility functions to
* support the functionality of the other functions.
* */
class Taxonomy {
  private var classes = ArrayBuffer[AnimalClass]()

  /*
  * This function will facilitate adding data on the highest level hitting all classes. It also will start the CoR for
  * lower levels to add data.
   */
  def addData() = {
    printf("what class:> ")
    val choice = scala.io.StdIn.readLine().toLowerCase()
    var found: Boolean = false

    for (clas <- classes) {
      if (clas.getClas() == choice) {
        clas.addData()
        found = true
      }
    }
    if (!found) {
      classes += new AnimalClass(choice)
      println("added class")
      printf("continue (y/n):> ")
      val choise = scala.io.StdIn.readChar().toLower

      if (choise == 'y') {
        var j = 1
        for (clas <- classes){
          if(classes.size == j){
            clas.addData()
          }
          j+=1
        }

      }
    }
  }

  /*
   * This function is the highest level of displaying all data. It will collect the strings returned by functions lower
   * in the chain, to compile all the data held by the program.
   */
  def display(): String = {
    var temp: String = ""

    if (classes.nonEmpty) {
      for (clas <- classes) {
        temp += clas.print(0, "")
      }
    }

    return temp
  }

  /*
   * This class will facilitate removing a class at the users direction.
   */
  def remove(): Unit = {
    var temp = "removed "
    var found = false
    var test = new AnimalClass(null)

    printf("what class:> ")
    val choice = scala.io.StdIn.readLine().toLowerCase()
    for (clas <- classes) {
      if (clas.getClas() == choice) {
        test = clas
        found = true

      }
    }
    if(found){
      temp += choice
      classes -= test
      printf(temp)
    }
    else{
      printf("class not found")
    }
  }

   /*
    * This function facilitates loading an xml file into the program at the highest level
    */
  def loadFile(): Unit ={
    printf("file name:> ")
    val name = scala.io.StdIn.readLine()
    try{
      val topNode = XML.loadFile(name)
      if (topNode.label != "taxonomy"){
        println("Invalid xml file. Needs to be an taxonomy xml file")
      }
      else{
        val children = topNode.child
        for(child <- children){
          val tag = child.label
          tag match {
            case "class" => {
              val clas = new AnimalClass(null)
              clas.loadFile(child)
              classes += clas
            }
            case _=> null
          }
        }
      }
    }
    catch{
      case e: FileNotFoundException => println("could not open file: " + name + " (the system cannot find the file specified)")
    }
  }

  /*
   * This function facilitates saving an xml file at the highest level
   */
  def saveFile( ): Unit ={
    printf("file name:> ")
    val name = scala.io.StdIn.readLine()
    var animal : Seq[Node] = Seq()
    for(clas <- classes){
      animal = animal ++ clas.saveFile()
    }
    val classXml = XMLHelper.makeNode("taxonomy", null, animal)
    //--slides
    val prettyPrinter = new PrettyPrinter(80,2)
    val prettyXml = prettyPrinter.format(classXml)
    val write = new FileWriter(name)
    write.write(prettyXml)
    write.close()

  }

  /*
   * This function facilitates the find feature functionality at the highest level. After reading in the feature it will
   * moving through the classes searching each for the feature.
   */
  def findFeature(): Unit ={
    var temp : String = ""
    var found : Boolean= false

    printf("feature:> ")
    val feat = scala.io.StdIn.readLine()

    for(clas <- classes){

      found = clas.findFeature(feat)

      if(!clas.getContinue()){
        clas.setContinue(true) //resets continue to true
        setComp() //resets complete to false
        return
      }

      if(found){
        temp = clas.print(0,"")
        //found = false
        println(temp)
        temp = ""
        setComp() //resets complete to false
        return
      }
    }

    if(!isComp()){
      println(feat+" not found")
    }
    else{
      setComp() //resets complete to false
    }


  }

  /*
   * This function is the highest level of the chain to check every instance in the program to look for a true complete
   * variable.
   */
  def isComp(): Boolean ={
    for(clas <- classes){
      if(clas.getComplete()){return true}
      else{
        if(clas.isComp()){return true}
        //else{return false}
      }
    }
    false
  }

  /*
   * This function is the highest level of the chain to reset every instance of complete to its default setting of false
   */
  def setComp(): Unit ={
    for(clas <- classes){
      clas.setComplete()
      clas.setComp()
    }
  }

  /*
   * This function facilitates displaying the summed species for the entire class given by the user
   */
  // GRADING: PARALLEL

  def sumSpecies(): Unit ={
    var species = 0
    printf("what class:> ")
    val cla = scala.io.StdIn.readLine()

    //var parr = classes.par
    for(clas <- classes.par){
      if(clas.getClas() == cla){
        species = clas.sumSpecies()
      }
    }

    printf("count: "+species)
  }

}
