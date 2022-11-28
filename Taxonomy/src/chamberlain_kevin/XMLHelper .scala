package chamberlain_kevin

import scala.collection.mutable
import scala.xml._

/**
  * Helper static class to simplify Scala's rather poor XML writing facitlities.
  *
  * It currently assume no prefixes or namespaces, ever. Compaction defaults to off, but can be set with compact
  */
object XMLHelper {
    var compact = true

    /**
      * Simplest node with just a tag and no attributes and no children
      * @param tag the tag of the xml node
      * @return an Elem node with the given tag
      */
    def makeNode(tag: String): Elem = {
        //make leaf
        val child: Node = Text("")
        makeNode(tag, null, child)
    }

    /**
      * Node with just a tag and attributes and no children
      * @param tag the tag of the xml node
      * @param attributes a mutable hashmap with the key-value pairs of the attributes
      * @return an Elem node with the given tag and attributes
      */
    def makeNode(tag: String, attributes: mutable.HashMap[String, String]): Elem = {
        //make leaf
        val child: Node = Text("")
        makeNode(tag, attributes, child)
    }

    /**
      * Node with just a tag and attributes and one child
      * @param tag the tag of the xml node
      * @param attributes a mutable hashmap with the key-value pairs of the attributes
      * @param child the child of this node
      * @return an Elem node with the given tag, attributes. and child
      */
    def makeNode(tag: String, attributes: mutable.HashMap[String, String], child: Node): Elem = {
        //convert to sequence
        val childSeq = Seq[Node](child)
        makeNode(tag, attributes, childSeq)
    }

    /**
      * Node with just a tag and attributes and one child
      * @param tag the tag of the xml node
      * @param attributes a mutable hashmap with the key-value pairs of the attributes
      * @param children all children of this node
      * @return an Elem node with the given tag, attributes. and child
      */
    def makeNode(tag: String, attributes: mutable.HashMap[String, String], children: Seq[Node]): Elem = {
        //make default starting values
        var attr: UnprefixedAttribute = new UnprefixedAttribute("", "", Null)

        //no attribute, make Elem with Null
        if (attributes == null) {
            //the :_* just changes a list into x, y, z, ... for a variable level of argument function
            return Elem(null, tag, Null, TopScope, compact, children: _ *)
        }
        else {
            //have attribute...change to be a UnprefixedAttribute
            attr = attributes.foldLeft(attr)((x: UnprefixedAttribute, y) =>
                if (x.key == "")
                    new UnprefixedAttribute(y._1, y._2, Null)
                else
                    new UnprefixedAttribute(y._1, y._2, x)
            )
            //return Elem with attributes
            return Elem(null, tag, attr, TopScope, compact, children: _ *)
        }
    }

}
