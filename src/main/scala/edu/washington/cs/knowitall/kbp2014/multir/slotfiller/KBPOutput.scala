package edu.washington.cs.knowitall.kbp2014.multir.slotfiller


class KBPOutput (val queryId :String, val relation :String, val runId :String, val provRelation :String,
  val slotFill :String, val provFill :String, val confScore :String)
{
  val queryIdRelation :String = queryId+relation
}

object KBPOutput {
  
  // val kbpOutput = new KBPOutput()
    
}


