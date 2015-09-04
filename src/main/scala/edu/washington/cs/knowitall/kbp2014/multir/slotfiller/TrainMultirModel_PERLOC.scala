package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import java.nio.file.{Paths, Files}
import collection.JavaConverters._
import scala.io.Source

//import edu.washington.multir.development.TrainModel
import edu.washington.multirframework.multiralgorithm.Preprocess
import edu.washington.multirframework.multiralgorithm.Train

object TrainMultirModel_PERLOC {
  
  
  
  def main(Args: Array[String]){

    // WebWare6 files
    // /projects/WebWare6/anglil/MultirExperiments/MultiR/multir-release
    // val featureFile = "/homes/gws/anglil/learner/data_featurized/data"
    // 18591 data_CS_and_test
    //val featureFile = "/homes/gws/anglil/learner/data_featurized/data_CS_and_test"
    val featureFile = "/projects/WebWare6/KBP_2015/multir/data_CS_and_test"
    val newFeatureFile = "/projects/WebWare6/KBP_2015/multir/features"
    val modelFileDir = "/projects/WebWare6/KBP_2015/multir/model"

    //works
    //println("Reformatting Features File")        
    //reformatFeaturesFile(featureFile, newFeatureFile)  
      
    //works  
    //println("Preprocess: writing files: mapping, model, and train")        
    //Preprocess.run(newFeatureFile, modelFileDir, null);             
    
    println("Train: writing file: params")        
    //Train.train(modelFileDir, null);             
    Train.train(modelFileDir)               
    
    //val featureFiles = List(newFeatureFile).asJava
    //val modelFiles = List(modelFileDir).asJava

    //println("Training model")    
    //TrainModel.run(featureFiles, modelFiles, 1)    
    
  }
  
  
  def reformatFeaturesFile(featureFile: String, newFeatureFile: String){

    val inputFilename = featureFile
    val outputFilename = newFeatureFile
    
    // Does input file exist?
    if (!Files.exists(Paths.get(inputFilename))) {
      System.out.println(s"features file $inputFilename doesn't exist!  " + s"Exiting...")
      sys.exit(1)
    }
    
    // Does output file exist?
    if (Files.exists(Paths.get(outputFilename))) {
      System.out.println(s"output file $outputFilename already exists!  " + s"Exiting...")
      sys.exit(1)
    }    

    val outStream = new PrintStream(outputFilename)
    
    val lines = Source.fromFile(inputFilename).getLines()

    lines.foreach(l => {
    
      val line = l.split("\t")
      val sentence = line(11)  
      val arg1 = line(0)
      val arg2 = line(3)
      var relation = line(7)
      if(relation.contains("neg")) relation = "NA"
      val features = line.drop(12).filter(f => f != "0").mkString("\t")
      
      outStream.println(sentence + "\t" + arg1 + "\t" + arg2 + "\t" + relation + "\t" + features)      
      
      
    })
    
    outStream.close
    
  } 
  
  
}
