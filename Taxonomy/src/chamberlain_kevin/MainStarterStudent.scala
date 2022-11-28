package chamberlain_kevin

import scala.io.StdIn
/*
0. Got it running						           __X_
1.	Add + Display*	34
Prompts correct (-1 pt each missed)		__X__
Adds a one of each item and displays 	__X__
Adds multiples 							          __X__
Above displays correctly formatted 		__X__


2A) Remove + Display*	10
Prompts correct							__X__
Removes and displays correctly 			__X__


2B) Add + XML save*	16
Console added items saved correctly 		__X__
Console added multiples is saved correctly 	__X__


2C) XML load + XML save*	16
1 XML file loaded and saved correctly 	__X__
2+ XML file loaded and saved correctly	__X__


2D) XML load + Display*	16
1 XML file loaded and displays correctly 	__X__
2+ XML file loaded and displays correctly	__X__


2E) XML+ Display with bad file handing	10
Calling display on empty, and after “messy” load works	__X__
Handles files not found correctly						__X__
Handle “not taxonomy” correctly							__X__
Handles extra tag, text, and attributes correctly		__X__
Handles missing attributes correctly					__X__


3.	Stress test for above*	15
Loads in file, adds data, and displays/saves correctly		__X__
Appends a file and displays/saves correctly 				__X__
Removes animal after edits, and displays/saves correctly 	__X__


4. Find feature*	16
CoR format at least there						__X__
Prompts correct									__X__
First item found and output formatted correctly	__X__
Handles “not found case”						__X__


5.	Total species count*	12
Prompts correct				__X__
Calculated correctly		__X__
Parallelized*				__X__

Every Line with a * has its grading tag: __X__

 */
/*
* This program is meant to be able to read and write xml files, parsing them into the appropriate ranks of taxonomy.
* The user will also be able to add data manually through the menu below, as well as search for specific features.
* The user will aslo be able to search for how many species are in a class, as well as display all the data stored.
* This is all done through RDP and through the chain of responsibility.
*
 */
object MainStarterStudent extends App {
    var choice = -1
    var taxonomy = new Taxonomy

    val menu: String =
        """
          |1) Add data
          |2) Display data
          |3) Remove class
          |4) Load XML
          |5) Write XML
          |6) Find feature
          |7) Calculate species
          |0) Quit
          |Choice:> """.stripMargin

    var temp = ""
    while (choice != 0) {
        try {
            print(menu)
            //something to strip out empty lines
            temp = StdIn.readLine()
            while(temp.isEmpty)
                temp = StdIn.readLine()
            choice = temp.toInt

            choice match {
                case 0 => ;  //quit
                case 1 => taxonomy.addData()  //add Data
                case 2 => println(taxonomy.display()) //GRADING: PRINT //display data
                case 3 => taxonomy.remove() //remove class
                case 4 => taxonomy.loadFile() //GRADING: READ //load xml
                case 5 => taxonomy.saveFile() //GRADING: WRITE //write xml
                case 6 => taxonomy.findFeature() //find feature
                case 7 => taxonomy.sumSpecies() //calculate species
                case _ => println("Invalid option")
            }
        } catch {
            case e: Throwable => print(e)
        }
    }
}

