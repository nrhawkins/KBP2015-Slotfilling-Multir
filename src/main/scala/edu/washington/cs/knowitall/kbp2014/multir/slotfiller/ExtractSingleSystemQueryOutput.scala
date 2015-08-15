package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import java.nio.file.{Paths, Files}

import scala.collection.JavaConversions._
import scala.io.Source

import com.typesafe.config.ConfigFactory


object ExtractSingleSystemQueryOutput {

  val config = ConfigFactory.load("kbp-2015-extract-single-system-query-output.conf")
  val inQueriesFileName = config.getString("in-file-queries")
  val inOutputFileName = config.getString("in-file-output")
  val outFileName = config.getString("out-file")
  val outStatsFileName = config.getString("out-stats-file")
      
  def main(args: Array[String]) {
  
    val outStream = new PrintStream(outFileName)
    val outStatsStream = new PrintStream(outStatsFileName)
    
    // -----------------------------------------------------------------
    // The round2 output is based on round1 output from all 3 systems.
    // This script extracts from the round2 output just those entries
    // which come from a single system's round1 output
    // -----------------------------------------------------------------

    // -------------------------------------------
    // Round2 Query IDs
    // -------------------------------------------
    // Does file exist?
    if (!Files.exists(Paths.get(inQueriesFileName))) {
      System.out.println(s"Input file $inQueriesFileName doesn't exist!  " + s"Exiting...")
      sys.exit(1)
    } 
   
    // Read file, line by line to get the round2 query id's,
    // based on a single system's round1 output
    val round2QueryIDs = Source.fromFile(inQueriesFileName).getLines().map(line => {
      val tokens = line.trim.split("\"")
      //println("tokens size: " + tokens.size)
      if(tokens.size == 3){ 
        tokens(1)
      }
      else "badline"
    }).toList.filter(s => s!= "badline")

    println("Number of IDs Round 2 Queries: " + round2QueryIDs.size)
    
    // ---------------------------------------------------------------------------
    // Write new file containing only id's in the round2QueryIDs
    // ---------------------------------------------------------------------------
    // Does file exist?
    if (!Files.exists(Paths.get(inOutputFileName))) {
      System.out.println(s"Input file $inOutputFileName doesn't exist!  " + s"Exiting...")
      sys.exit(1)
    } 
    
    Source.fromFile(inOutputFileName).getLines().foreach(line => {
      try{
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s >= 8 => {                        
            if(round2QueryIDs.contains(tokens(0))){   
              outStream.println(line)
            }
          }
          case _ =>   
        }
      }catch{
        case e: Exception =>
      }  
    })   

    // ---------------------------------------
    // Write stats to file
    // ---------------------------------------
    outStatsStream.println("round2QueryIDs size: " + round2QueryIDs.size)
      
    // ---------------------------------------
    // Close output streams
    // ---------------------------------------
    outStream.close()
    outStatsStream.close()
    println("closed output streams")
    
  }
    
  
}
  
