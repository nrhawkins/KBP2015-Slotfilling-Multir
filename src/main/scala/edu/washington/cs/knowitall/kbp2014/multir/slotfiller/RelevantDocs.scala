package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._

class RelevantDocs (val queryId :String, val docList :String)
{
  val docSet = docList.split(" ").toSet
  val totalDocs = computeTotalDocs(docSet) 
  val nwDocs = computeNwDocs(docSet)
  val ngDocs = computeNgDocs(docSet)
  val forumDocs = computeForumDocs(docSet)
  
  def computeTotalDocs(docSet :Set[String]) :Int = {
    docSet.size    
  }
  
  def computeNwDocs(docSet :Set[String]) :Int = { 
    val nwngdocs = docSet.filter(doc => !doc.startsWith("bolt")) 
    val nwdocs = nwngdocs.filter(doc => !doc.startsWith("eng-"))      
    nwdocs.size    
  }
  
  def computeNgDocs(docSet :Set[String]) :Int = {
    val ngdocs = docSet.filter(doc => doc.startsWith("eng-"))
    ngdocs.size
  }
  
  def computeForumDocs(docSet :Set[String]) :Int = {
    val forumdocs = docSet.filter(doc => doc.startsWith("bolt"))
    forumdocs.size    
  }  
  
}

object RelevantDocs {
  
  def writeRelevantDocs(relDocs :Seq[RelevantDocs], outputStream :PrintStream) = {

    val sortedRelDocs = relDocs.sortBy(rd => rd.queryId)

    outputStream.println("kbpQueryId" + "\t" + "nw" + "\t" + "ng" + "\t" + "forum" + "\t" + "totalDocs")
    
    for(rd <- sortedRelDocs){
      
      val queryId = rd.queryId
      val totalDocs = rd.totalDocs 
      val nwDocs = rd.nwDocs 
      val ngDocs = rd.ngDocs 
      val forumDocs = rd.forumDocs
      
      outputStream.println(queryId + "\t" + nwDocs + "\t" + ngDocs + "\t" + forumDocs + "\t" + totalDocs)      
    }

  }


}


