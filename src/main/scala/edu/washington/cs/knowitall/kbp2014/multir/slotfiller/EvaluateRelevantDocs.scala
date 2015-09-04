package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import scala.io.Source

// -----------------------------------------
// main
// -----------------------------------------

import java.io._
import scala.io.Source

// -----------------------------------------
// main
// -----------------------------------------

object EvaluateRelevantDocs {

  def main(Args: Array[String]){

    //0 = relevantDocs file
    //1 = output file

    // Create Map of RelevantDoc doc list and doc type stats    
    val relDocs = createRelevantDocs(Args(0))    

    // Write Relevant Doc Stats to a File
    val outputStream = new PrintStream(Args(1))

    RelevantDocs.writeRelevantDocs(relDocs, outputStream)

    outputStream.close()
    
  } // main

  
  def createRelevantDocs(inputFile :String) :Seq[RelevantDocs] = {

    val lines = Source.fromFile(inputFile, "UTF-8").getLines.toList

    val relDocsSeq = for(line <- lines) yield {
      val idDocs = line.split("\t")
      val queryId = idDocs(0)
      val docList = idDocs(1)
      val rd = new RelevantDocs(queryId, docList)
      rd    
    }      
    relDocsSeq
    
  }

 
}// EvaluateRelevantDocs