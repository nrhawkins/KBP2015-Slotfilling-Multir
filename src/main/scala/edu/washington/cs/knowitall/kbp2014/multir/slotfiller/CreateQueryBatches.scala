package edu.washington.cs.knowitall.kbp2014.multir.slotfiller


import java.io._
import java.nio.file.{Paths, Files}

import scala.collection.JavaConversions._
import scala.io.Source

import com.typesafe.config.ConfigFactory

/* 
 * Strip the output beyond the first 7 columns so that the 
 * CS-Generate-Queries.pl script from 2014 can be run on the 2015 
 * output. 
 */
object CreateQueryBatches {

  val config = ConfigFactory.load("kbp-2015-create-query-batches.conf")
  val numDocsFileName = config.getString("num-docs-file")
  val queryFileName = config.getString("query-file")
  val outFileName = config.getString("out-file")
  
  case class QueryDoc(queryID: String, numDocs: Int)  
      
  def main(args: Array[String]) {

    val roundID = "round1"
    
    val outStream = new PrintStream(outFileName)

    val queries = KBPQuery.parseKBPQueries(queryFileName, roundID)	  
    
    // --------------------------------------------------------------
    //
    // --------------------------------------------------------------
    //var qCount = 0
    val queryNumDocs: List[QueryDoc] = {
      Source.fromFile(numDocsFileName).getLines().map(line => {
        val tokens = line.trim.split(" ")
        //val tokens = line.trim.split("Size All Documents: ")
        tokens.size match {
          case 2 => {
            //qCount += 1
            //QueryDoc(qCount.toString, tokens(1).toInt)
            QueryDoc(tokens(0), tokens(1).toInt)
          }
          case _ => QueryDoc("badline", 0)
        } 
    })}.toList.filter(q => q.queryID != "badline")
    
    println("Num queryNumDocs: " + queryNumDocs.size)
  
    var totalDocs = 0
    queryNumDocs.foreach(q => {
      if(q.numDocs > 500){
        totalDocs += 500
      }
      else{
        totalDocs += q.numDocs  
      }
      println(q.queryID + " " + q.numDocs + " " + totalDocs)
    }
    )    
    
    println("Num Queries: " + queries.size) 
    val qid = (for(q <- queries) yield {
      q.id      
    }).toSet   
    println("Num Query IDs: " + qid.size) 
        
    
    //val dl = queries.filter(q => q.name.equals("Syracuse") || q.name.equals("syracuse"))
    //val dl = queries.filter(q => q.name.equals("John") || q.name.equals("john"))
    //println("dl size: " + dl.size)
    //val docs = (for(d <- dl) yield {
    //  d.doc      
    //}).toSet
    //val et = (for(d <- dl) yield {
    //  d.entityType.toString      
    //}).toSet
    //val qn = (for(d <- dl) yield {
    //  d.slotsToFill.toList(0).name 
    //}).toSet
    //println("docs size: " + docs.toSet.size) 
    //println("et size: " + et.toSet.size) 
    //docs.foreach(d => println(d)) 
    //et.foreach(d => println(d))
    //qn.foreach(d => println(d))     
    
    var querySet: Set[String] = Set()
    var sameQueries : List[List[KBPQuery]] = List()
    
    println("querySet size: " + querySet.size)
    
    for(query <- queries){
      //val qnlc = query.name.trim.toLowerCase
      val qnlc = query.name
      
      if(!querySet.contains(qnlc)){
        querySet += qnlc   
        //get all matching queries
        val matchingQueries = queries.filter(q => qnlc.equals(q.name))
        sameQueries = sameQueries ++ List(matchingQueries)        
      } 
    }
    
    println("querySet size: " + querySet.size)
    //querySet.toList.sorted.foreach(q => println(q))
    println("sameQueries size: " + sameQueries.size)
    var sqcount = 0
    var totDocs = 0
    
    sameQueries.foreach(s => {
      val qname = sameQueries(sqcount)(0).name
      val numDocs = queryNumDocs.filter(d => d.queryID ==sameQueries(sqcount)(0).id)(0).numDocs
      if(numDocs > 500)
        totDocs += 500
      else  
        totDocs += numDocs
      sqcount += 1
      println(sqcount + " " + numDocs + " " + totDocs + " " + qname) 
      //println("sqcount: " + sqcount + " " + numDocs + " " + totDocs + " " + qname)      
      //s.foreach(q => {
      //  val qnd = queryNumDocs.filter(d => d.queryID == q.id)
      //  print(qnd(0).numDocs + " ")
      //})
      //println
    })
    
    
    
    
    
    outStream.close()
   
    println("closed output streams")
    
  }  
}
  
