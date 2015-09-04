package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import java.nio.file.{Paths, Files}

import scala.collection.JavaConversions._
import scala.io.Source

import com.typesafe.config.ConfigFactory


object NormalizeConfidenceScore {

  val config = ConfigFactory.load("kbp-2015-normalize-conf-score.conf")
  val inFileName = config.getString("in-file")
  val outFileName = config.getString("out-file")
  val outStatsFileName = config.getString("out-stats-file")
      
  def main(args: Array[String]) {
  
    val outStream = new PrintStream(outFileName)
    val outStatsStream = new PrintStream(outStatsFileName)
    
    // ---------------------------------------------------------------
    // Read through input file to get max conf score
    // Use max conf score in formula for normalizing the conf score
    // ---------------------------------------------------------------
    
    // Does file exist?
    if (!Files.exists(Paths.get(inFileName))) {
      System.out.println(s"Input file $inFileName doesn't exist!  " + s"Exiting...")
      sys.exit(1)
    } 
   
    // Read file, line by line to get the confidence scores, 
    // take all in, so later can compute stats and write to outStatsStream
    val confScores = Source.fromFile(inFileName).getLines().map(line => {
      val tokens = line.trim.split("\t")
      if(tokens.size >= 8){ 
        tokens(7).toDouble
      }
      else -1
    }).toList.filter(s => s!= -1)
    
    val maxConfScore = confScores.max    

    // ---------------------------------------------------------------------------
    // Write new file replacing confidence score with normalized confidence score
    // ---------------------------------------------------------------------------
    Source.fromFile(inFileName).getLines().foreach(line => {
      try{
        val tokens = line.trim.split("\t")
        //println("tokens size: " + tokens.size)
        //when the sentence is appended and it overlaps, there are some lines with 1 token
        tokens.size match {
          case 0 => 
          // the slotfill is NIL in this case  
          case 4 => outStream.println(line)
          // valid slotfill, norm the conf score, and write out
          case 8 | 9 | 10 => {
            val normCS = normalizeConfScore(tokens(7).toDouble, maxConfScore) 
            val formattedNormCS = "%1.2f".format(normCS)
            //formattedNormCS = f"$formattedNormCS%10s"
            outStream.println(tokens(0) + "\t" + tokens(1) + "\t" + tokens(2) + 
            "\t" + tokens(3) + "\t" + tokens(4) + "\t" + tokens(5) + "\t" + 
            formattedNormCS + "\t" + tokens(7))
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
    outStatsStream.println("Conf Scores (n, min, max): " + confScores.size + " " +
      confScores.min + " " + confScores.max)
    val normConfScores = confScores.map(s => normalizeConfScore(s, maxConfScore))  
    outStatsStream.println("Norm Conf Scores (n, min, max): " + normConfScores.size + " " +
      normConfScores.min + " " + normConfScores.max)
      
    // ---------------------------------------
    // Close output streams
    // ---------------------------------------
    outStream.close()
    outStatsStream.close()
    println("closed output streams")
    
  }
  
  // Put confidence scores in the range 0.2 - 0.9
  def normalizeConfScore(confScore: Double, maxConfScore: Double): Double = {
    
    .2 + (confScore/maxConfScore)*.7
  }
  
  
}
  
