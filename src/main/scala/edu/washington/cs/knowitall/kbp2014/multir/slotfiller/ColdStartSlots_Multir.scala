package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

object ColdStartSlots_Multir {
  
  private val coldStartSlotsResourcePath = "/edu/washington/cs/knowitall/kbp2014/multir/slotfiller/ColdStartSlotTypes_multir.txt"
  
  private val coldStartSlotsURL = getClass().getResource(coldStartSlotsResourcePath)
  require(coldStartSlotsURL != null, "Could not find resource: " + coldStartSlotsResourcePath)

    
  private val slotSet =  scala.collection.mutable.Set[String]()

  // read in document list lines with latin encoding so as not to get errors.
  scala.io.Source.fromFile(coldStartSlotsURL.getPath())(scala.io.Codec.ISO8859).getLines.foreach(line => {
    
      val name = line.trim
      
      if(!slotSet.contains(name)) slotSet.add(name)
        
    })
    
  lazy val slots = slotSet.toSet
  
}