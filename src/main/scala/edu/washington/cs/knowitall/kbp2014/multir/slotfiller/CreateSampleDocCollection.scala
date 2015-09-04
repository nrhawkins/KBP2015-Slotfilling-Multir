package edu.washington.cs.knowitall.kbp2014.multir.slotfiller


import java.io._
import java.nio.file.{Paths, Files}

import scala.collection.JavaConversions._
import scala.io.Source

import com.typesafe.config.ConfigFactory

import edu.stanford.nlp.pipeline.Annotation

/* 
 * Examine discussion forum and newsgroup docs
 */

object CreateSampleDocCollection {

  val config = ConfigFactory.load("kbp-2015-create-sample-doc-collection.conf")
  val inFileName = config.getString("in-file")
  //val outFileName = config.getString("out-file")
  val corpusOldNew = config.getString("corpus")  
      
  val annotatorHelper = new StanfordAnnotatorHelperMethods()
  
  def main(args: Array[String]) {

    SolrHelper.setConfigurations(corpusOldNew,false)
    
    //val outStream = new PrintStream(outFileName)

    val discForumDocName = "bolt-eng-DF-212-191668-3067618"    
    //val newsGroupDocName = "eng-NG-31-127207-8269360" 
      
    val discForumDoc = SolrHelper.getRawDoc(discForumDocName)      
    //val newsGroupDoc = SolrHelper.getRawDoc(newsGroupDocName)      

    println("Bolt size: " + discForumDoc.size)
    
    val bolt800 = Source.fromFile(inFileName).mkString     
    println("Bolt 800 size: " + bolt800.size)
    
    var startTime :Long = 0
	var endTime: Long = 0 
	
    startTime = System.currentTimeMillis()    
    val processedDoc = new Annotation(bolt800)
    annotatorHelper.getCorefPipeline().annotate(processedDoc)
    endTime = System.currentTimeMillis()
    println("Thread: Document took " + (endTime-startTime) + " milliseconds")         
    
    //outStream.println(discForumDoc)
    //outStream.println(newsGroupDoc)
    
    //outStream.close()
    //println("closed output streams")
    
  }  
}
  
