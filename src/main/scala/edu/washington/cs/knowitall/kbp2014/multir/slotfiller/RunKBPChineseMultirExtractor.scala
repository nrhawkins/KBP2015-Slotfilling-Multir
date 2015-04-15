package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import edu.washington.multirframework.corpus.Corpus
import edu.washington.multirframework.corpus.DefaultCorpusInformationSpecification
import edu.washington.multirframework.corpus.DocumentInformationI
//import edu.washington.multirframework.corpus.DocCorefInformation
import edu.washington.multirframework.corpus.SentInformationI
//import edu.washington.multirframework.corpus.SentNamedEntityLinkingInformation
import edu.washington.multirframework.corpus.TokenInformationI
//import edu.washington.multirframework.corpus.SentNamedEntityLinkingInformation.NamedEntityLinkingAnnotation
//Xiao found a bug, when trying to add-in the linker
//import edu.washington.multirframework.corpus.MyCorpus
import edu.washington.multir.sententialextraction.DocumentExtractor
import edu.washington.multir.preprocess.CorpusPreprocessing
import java.io._
import java.util.Properties
import collection.JavaConverters._

//import edu.stanford.nlp.ling.CoreAnnotations
//import edu.stanford.nlp.pipeline.StanfordCoreNLP
//import edu.stanford.nlp.dcoref.CorefCoreAnnotations
//import edu.stanford.nlp.dcoref.SieveCoreferenceSystem
//import edu.stanford.nlp.dcoref.Document
//import edu.stanford.nlp.dcoref.RuleBasedCorefMentionFinder
//import edu.stanford.nlp.dcoref.Dictionaries
//import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
//import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation

import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel
//import edu.stanford.nlp.ling.IndexedWord
//import edu.stanford.nlp.semgraph.SemanticGraph
//import edu.stanford.nlp.semgraph.SemanticGraphEdge
//import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations
//import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.util.CoreMap
import edu.washington.multirframework.corpus.CorpusInformationSpecification.SentDocNameInformation.SentDocName;

object RunKBPChineseMultirExtractor {

        //val chineseProps = new Properties() 	    
        //chineseProps.put("annotators", "tokenize, ssplit, pos, lemma, ner");
        //chineseProps.put("annotators", "segment, ssplit, pos, ner")
	    //chineseProps.put("outputFormat", "xml")
	    //chineseProps.put("customAnnotatorClass.segment", "edu.stanford.nlp.pipeline.ChineseSegmenterAnnotator")
	    //chineseProps.put("segment.model", "edu/stanford/nlp/models/segmenter/chinese/ctb.gz")
	    //chineseProps.put("segment.sighanCorporaDict", "edu/stanford/nlp/models/segmenter/chinese")
	    //chineseProps.put("segment.serDictionary", "edu/stanford/nlp/models/segmenter/chinese/dict-chris6.ser.gz")
	    //chineseProps.put("segment.sighanPostProcessing", "true")
	    //chineseProps.put("ssplit.boundaryTokenRegex", "[.]|[!?]+|[。]|[！？]+")
	    //chineseProps.put("pos.model", "edu/stanford/nlp/models/pos-tagger/chinese-distsim/chinese-distsim.tagger")
	    //chineseProps.put("ner.model", "edu/stanford/nlp/models/ner/chinese.misc.distsim.crf.ser.gz")
	    //chineseProps.put("ner.applyNumericClassifiers", "false")
	    //chineseProps.put("ner.useSUTime", "false")
	    //chineseProps.put("encoding", "utf-8")
	    //chineseProps.put("inputEncoding", "utf-8")
	    //chineseProps.put("outputEncoding", "utf-8")
	    //chineseProps.put("parse.model", "edu/stanford/nlp/models/lexparser/chinesePCFG.ser.gz")
  
        //val chinesePipeline = new StanfordCoreNLP(chineseProps)
  
  //val annotatorHelper = new StanfordAnnotatorHelperMethods()
    val annotatorHelper = new StanfordChineseAnnotatorHelperMethods()
        
  /*val cis  = new DefaultCorpusInformationSpecification()
  val javaDocInfoList = new java.util.ArrayList[DocumentInformationI]()
  javaDocInfoList.add(new DocCorefInformation())
  cis.addDocumentInformation(javaDocInfoList)
  val javaSentInfoList = new java.util.ArrayList[SentInformationI]()
  javaSentInfoList.add(new SentNamedEntityLinkingInformation())
  cis.addSentenceInformation(javaSentInfoList)
  */  
  
  def main(Args: Array[String]){
    
      var runID = "UWashington1"
      var detailed = false
      // Include printed extractions in output file?
      var printExtractions = false
      
      
      // * ------------- Set the args ----------------------- *//
 
      val queries = KBPQuery.parseKBPQueries(Args(0))  // file of queries	  
	  val modePreprocessed = Integer.parseInt(Args(1)) // process on the fly?
      val corpusOldNew = Args(2)                       // old | new | chinese   
      val relevantDocsFileName = Args(3)               // relevantDocs file name
      val relevantDocsFile = new File(relevantDocsFileName)      
      val outputStream = new PrintStream(Args(4))      // output file name
      val outFmt = detailed match {
             case true => OutputFormatter.detailedAnswersOnly(outputStream, runID)
             case false => OutputFormatter.formattedAnswersOnly(outputStream, runID)
      }      

      
      // * ------------- Set the model ----------------------- *//

      //val multirExtractor = new MultiModelMultirExtractorVersion1Chinese()
	  //val multirExtractor = new MultiModelMultirExtractorVersion2()
	  val multirExtractor = new SingleModelMultirExtractorVersion1Chinese()
      
	  // * ----------- Get Relevant Documents --------------- *//
	  
	  // ---------------------------------------------------------------------------------------------------
      // specify corpus: corpusOldNew="chinese", and corefOnOff(this param exists, but is not implemented)
      // ---------------------------------------------------------------------------------------------------
	  SolrHelper.setConfigurations(corpusOldNew, false)
	  
	 
      // ---------------------------------------------------------------------------------	  
	  // entityRelevantDocSerialization 
	  // is a map which has a KBPQuery id as a key, and a list of docids as a value
	  // ---------------------------------------------------------------------------------
	  val entityRelevantDocSerialization = {
		    
	     if(relevantDocsFile.exists()){
		       QuerySetSerialization.getRevelantDocIdMap(relevantDocsFileName)
		 }	        
		 else{
		    // make this map and write it out
		   
		    // this map has KBPQuery as a key, and a list of docids as the value
		    val qm = SolrHelper.getRelevantDocuments(queries)
            // this map has the KBPQuery id as a key, and a list of docids as the value		    
		    val qidMap = qm.toList.map(f => (f._1.id,f._2)).toMap

		    QuerySetSerialization.writeRelevantDocIdMap(qidMap, relevantDocsFileName)
		    
		    qidMap
		 }
      }

	  System.out.println("Number of queries: " + queries.size);
	  
	  for(query <- queries){

	      println("Query: " + query.name)
	    //if(printExtractions){  
		  outputStream.println(query.name + "\n")  
	    //}
	    
	    try{
		        		      
		  var allRelevantCandidates: Seq[Candidate] = Nil		          	      
		  val relevantDocs = entityRelevantDocSerialization(query.id).toSet		      
       
          println("Query: " + query.id)
		  println("Size All Documents: " + relevantDocs.size)		  		 
		  
		  val documents :List[Option[Annotation]] = {
		    modePreprocessed match {
		      case 0 => processChineseDocuments(relevantDocs)
		      case _ => processChineseDocuments(relevantDocs)	
		    }            
		  }
		  
		  System.out.println("Processed Docs!")
		  //System.exit(0)
		  
          for(document <- documents){
            if(document.isDefined){

              println("DOCUMENT is DEFINED: " + query.id )
              //SentDocName is null
              //println("DOCUMENT is DEFINED: " + query.id + " " + document.get.get(classOf[SentDocName]))
              
              //val extractions = multirExtractor.extract(document.get, query).asScala

              val extractions = multirExtractor.extract(document.get, query).asScala
              
              //val extractions = documentExtract()
              
		      println("SIZE EXTRACTIONS: " + extractions.size)
              println("NEXT DOCUMENT")
                              
              //val relevantCandidates = FilterExtractionResults.filterExtractions(FilterExtractionResults.wrapWithCandidate(extractions), query)     

              //This one
              //val relevantCandidates = FilterExtractionResults.filterResults(FilterExtractionResults.wrapWithCandidate(extractions), query, document)                                       
		      //println("Size RelevantCandidates: " + relevantCandidates.size)		          
		      //if(relevantCandidates.size > 0) allRelevantCandidates = allRelevantCandidates ++ relevantCandidates		        
		      //for(c <- relevantCandidates){println(c.extr)}
		        
		      if(printExtractions){      
		        		        
		        for(e <- extractions){
                //for(c <- relevantCandidates){
		          //val docText = document.get.get(classOf[CoreAnnotations.TextAnnotation])
		          val minIndex = math.min(e.getArg1().getStartOffset(),e.getArg2().getStartOffset())
		          val maxIndex = math.max(e.getArg2().getEndOffset(),e.getArg1().getEndOffset())
		          //println("Sentence = " +docText.subSequence(minIndex,maxIndex))
		          //bw.write(e + "\n")
		          //bw.write("Sentence = " +docText.subSequence(minIndex,maxIndex)+"\n")		         

		          //val minOffset = math.max(minIndex-20, 0)
		          //val maxOffset = math.min(maxIndex+20, docText.length)
		          //outputStream.println(e + ", " + docText.subSequence(minIndex,maxIndex) + "\n" )		                           		              
		        }
		        
                //for(c <- relevantCandidates){
		          //val docText = document.get.get(classOf[CoreAnnotations.TextAnnotation])
		          //val minIndex = math.min(c.extr.getArg1().getStartOffset(),c.extr.getArg2().getStartOffset())
		          //val maxIndex = math.max(c.extr.getArg2().getEndOffset(),c.extr.getArg1().getEndOffset())	         
		          //val minOffset = math.max(minIndex-20, 0)
		          //val maxOffset = math.min(maxIndex+20, docText.length)
		          //outputStream.println(query.id + " " + c.extr + ", " + docText.subSequence(minIndex,maxIndex) + "\n" )		                           		              
                //}		        
		        
		      } //printExtractions
		      
              //}		    
		      
            } //if doc defined
          } //documents  
          
		  
		  
		      val slots = query.slotsToFill
 		      
		      println("Done Processing Documents")
		      println("Size allRelevantCandidates: " + allRelevantCandidates.size)
		      
		      println("SubstituteKBPRelations")    	
		      val kbpAllRelevantCandidates = FilterExtractionResults.substituteKBPRelationsChinese(allRelevantCandidates, query)
              
		      println("Make Slot Map - Best Answers")		      
		      val bestAnswers = slots map { slot => ( slot, SelectBestAnswers.reduceToMaxResults(slot, kbpAllRelevantCandidates.filter(_.extr.getRel() == slot.name)) ) } toMap
		               	      
		      outFmt.printAnswers(bestAnswers, query)
		     
		      
		  }
	      catch {case e: Exception => 
	        {e.printStackTrace()
	         println("EXCEPTION RunKBP: " + query.id + " " + query.name) 
	         outFmt.printEmpty(query)
	        }	  
	      }		    		  
		  
	      println("Finished, going to next query")
	      
    } //queries
		  	  
	  println("Finished with Queries - closing outputStreams.")
	  
	  outputStream.close()
	
	  
	  println("outputStreams closed.")
	  
  } //main
  
  
  def processChineseDocuments(documents: Set[String]): List[Option[Annotation]] = {
    println("Number of docs = " + documents.size)
    var startTime :Long = 0
	var endTime: Long = 0    	 
	var docCount = 0
    for(doc <- documents.toList) yield{
      docCount = docCount + 1
      //println("Processing Doc # :" + docCount)
      var a :Option[Annotation] = None
      val t = new Thread {
        override def run() {    
          startTime = System.currentTimeMillis()
          a =processChineseDocument(doc)
          endTime = System.currentTimeMillis()
          //println("Thread: Document took " + (endTime-startTime) + " milliseconds")      
        }
      }                                              
      t.start()
      //t.join(10000)
      t.join(180000)                                        
      a
    }
  }
  
  def processChineseDocument(docName: String) : Option[Annotation]  ={ 
    try{                
      val stanfordDoc = stanfordProcessChineseDocument(docName)
      
      if(stanfordDoc.isDefined){
        stanfordDoc
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
  
  def stanfordProcessChineseDocument(docName: String) : Option[Annotation] = {
    try{
      val rawDoc = SolrHelper.getRawDoc(docName)
      val processedDoc = new Annotation(rawDoc)
      annotatorHelper.getChinesePipeline().annotate(processedDoc)
      println("Document was Stanford Annotated")
      Some(processedDoc)
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
  }
  
}
