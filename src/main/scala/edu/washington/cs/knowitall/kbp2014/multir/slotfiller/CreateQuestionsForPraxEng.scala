package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import edu.washington.multirframework.corpus.Corpus
import edu.washington.multirframework.corpus.DefaultCorpusInformationSpecification
import edu.washington.multirframework.corpus.DocumentInformationI
import edu.washington.multirframework.corpus.DocCorefInformation
import edu.washington.multirframework.corpus.SentInformationI
import edu.washington.multirframework.corpus.SentNamedEntityLinkingInformation
import edu.washington.multirframework.corpus.TokenInformationI
import java.io._
import edu.stanford.nlp.pipeline.Annotation
import edu.washington.multir.sententialextraction.DocumentExtractor
import edu.washington.multir.preprocess.CorpusPreprocessing
import edu.stanford.nlp.ling.CoreAnnotations
import java.util.Properties
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.dcoref.CorefCoreAnnotations
import edu.stanford.nlp.dcoref.SieveCoreferenceSystem
import edu.stanford.nlp.dcoref.Document
import edu.stanford.nlp.dcoref.RuleBasedCorefMentionFinder
import edu.stanford.nlp.dcoref.Dictionaries
import collection.JavaConverters._
import edu.washington.multirframework.corpus.SentNamedEntityLinkingInformation.NamedEntityLinkingAnnotation
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import scala.io.Source
//import org.json.simple.JSONArray;
//import org.json.simple.JSONObject;

object CreateQuestionsForPraxEng {
  
  
  val annotatorHelper = new StanfordAnnotatorHelperMethods()

  val cis  = new DefaultCorpusInformationSpecification()
  val javaDocInfoList = new java.util.ArrayList[DocumentInformationI]()
  javaDocInfoList.add(new DocCorefInformation())
  cis.addDocumentInformation(javaDocInfoList)
  val javaSentInfoList = new java.util.ArrayList[SentInformationI]()
  javaSentInfoList.add(new SentNamedEntityLinkingInformation())
  cis.addSentenceInformation(javaSentInfoList)
  
  
  
  def main(Args: Array[String]){
   
      val corpusOldNew = Args(0)
      val docsToProcessFileName = Args(1)
      val docsToProcessFile = new File(docsToProcessFileName)      
      val outputStream = new PrintStream(Args(2))      
      
	  val multirExtractor = new MultiModelMultirExtractorVersion3()
	         
	  // * ----------- Solr Indexed Documents --------------- *//
	  
	  SolrHelper.setConfigurations(corpusOldNew,false)
	  		 	    
	  //if(docsToProcessFile.exists()){}
		
	  try{
	    
	    // * ----------- Process Documents --------------- * //

	    val docsToProcess :Set[String] = Source.fromFile(docsToProcessFileName, "UTF-8").getLines.toList.toSet
	    val documents :List[Option[Annotation]] = processDocuments(docsToProcess)
        val printExtractions = true

	    // * ----------- Get Extractions for Each Document --------- *//
	    
	    for(document <- documents){

	      if(document.isDefined){

	        val relationType = "PER"
	        
            val extractions = multirExtractor.extract2(document.get, relationType).asScala
        
            if(printExtractions){      
		        		        
		        for(e <- extractions){
                
		          val docText = document.get.get(classOf[CoreAnnotations.TextAnnotation])
		          val minIndex = math.min(e.getArg1().getStartOffset(),e.getArg2().getStartOffset())
		          val maxIndex = math.max(e.getArg2().getEndOffset(),e.getArg1().getEndOffset())
		
		          outputStream.println(e + ", " + docText.subSequence(minIndex,maxIndex) + "\n" )		                           		              
		          
		        }           	        
		        
		    } //printExtractions
            
            
          }
        }
	    
	       
	    
	  }
      catch{
        case e: Exception => 
	        {e.printStackTrace()
	         println("EXCEPTION: " + "docsToProcessFile Does Not Exist!") 
	        }	  
      }	  
	  
	  outputStream.close()
	  
  } //main

  
  def processDocuments(documents: Set[String]): List[Option[Annotation]] = {
    println("Number of docs = " + documents.size)
    var startTime :Long = 0
	var endTime: Long = 0    	 
	var docCount = 0
    for(doc <- documents.toList) yield{
      docCount = docCount + 1
      println("Processing Doc # :" + docCount)
      var a :Option[Annotation] = None
      val t = new Thread {
        override def run() {    
          startTime = System.currentTimeMillis()
          a =processDocument(doc)
          endTime = System.currentTimeMillis()
          println("Thread: Document took " + (endTime-startTime) + " milliseconds")      
        }
      }                                              
      t.start()
      //t.join(10000)
      t.join(180000)                                        
      a
    }
  }
  
  
  def processDocument(docName: String) : Option[Annotation]  ={ 
    try{          
      println("Processing document " +docName)
      val stanfordDoc = stanfordProcessDocument(docName)
      val cjParsedDoc = cjParseDocument(docName)
      //val linkedDoc = linkDocument(docName)      	     
    	     
      if(stanfordDoc.isDefined && cjParsedDoc.isDefined /*&& linkedDoc.isDefined*/){
        val ann = joinAnnotations(stanfordDoc.get,cjParsedDoc.get,new Annotation());
    	println("have NEL = "+ann.get(classOf[NamedEntityLinkingAnnotation]))
    	Some(ann)
    	//joinAnnotations(stanfordDoc.get,cjParsedDoc.get,linkedDoc.get)
      }
      else{
        None
      }
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
      
  }
  
  def stanfordProcessDocument(docName: String) : Option[Annotation] = {
    try{
      val rawDoc = SolrHelper.getRawDoc(docName)
      val processedDoc = new Annotation(rawDoc)
      annotatorHelper.getCorefPipeline().annotate(processedDoc)
      println("Document was Stanford Annotated")
      Some(processedDoc)
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
  }
  
  def cjParseDocument(docName: String): Option[Annotation] = {
    try{
      val rawDoc = SolrHelper.getRawDoc(docName)
      val preprocessedAndParsedDoc = CorpusPreprocessing.getTestDocumentFromRawString(rawDoc,docName)
      println("Document was cj parsed")
      Some(preprocessedAndParsedDoc)
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
  }
  
  /* def linkDocument(docName: String): Option[Annotation] ={
    try{
        val rawDoc = SolrHelper.getRawDoc(docName)
        val processedDoc = new Annotation(rawDoc)
        println("Document was linked")
        Some(processedDoc)
      }
      catch{
        case e: Exception => e.printStackTrace()
        None
      }
  } */ 
  
  def joinAnnotations(stanfordDoc: Annotation, cjParsedDoc: Annotation, linkedDoc: Annotation) : Annotation = {  
    //add coref annotations to cjParsedDoc
    cjParsedDoc.set(classOf[CorefCoreAnnotations.CorefChainAnnotation],stanfordDoc.get(classOf[CorefCoreAnnotations.CorefChainAnnotation]))
    cjParsedDoc.set(classOf[NamedEntityLinkingAnnotation],linkedDoc.get(classOf[NamedEntityLinkingAnnotation]))
    cjParsedDoc
  }
 

}
