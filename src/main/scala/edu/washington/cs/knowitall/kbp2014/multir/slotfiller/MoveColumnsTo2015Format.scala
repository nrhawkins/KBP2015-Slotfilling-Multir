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
object MoveColumnsTo2015Format {

  val config = ConfigFactory.load("kbp-2015-move-columns.conf")
  val inFileName = config.getString("in-file")
  val outFileName = config.getString("out-file")
  
  //case class kbpAnswer(id: String, rel: String, runID: String, provAll: String, 
  //    slotFill: String, provSF: String, confScore: String)
  
      
  def main(args: Array[String]) {
  
    val outStream = new PrintStream(outFileName)

    // --------------------------------------------------------------
    // Read the input file - if a line has 8 columns, 
    // insert the last column as column 6
    // Ignore all other lines (if there are any w/o 8 cols)
    // --------------------------------------------------------------
    Source.fromFile(inFileName).getLines().foreach(line => {
      try{
        val tokens = line.trim.split("\t")
        tokens.size match {
          case 8 => outStream.println(tokens(0) + "\t" + tokens(1) + "\t" + 
              tokens(2) + "\t" + tokens(3) + "\t" + tokens(4) + "\t" + tokens(7) + 
              "\t" + tokens(5) + "\t" + tokens(6))
          case _ =>   
        }
      }catch{
        case e: Exception =>
      }  
    })   
  
      
    outStream.close()
   
    println("closed output streams")
    
  }  
}
  
