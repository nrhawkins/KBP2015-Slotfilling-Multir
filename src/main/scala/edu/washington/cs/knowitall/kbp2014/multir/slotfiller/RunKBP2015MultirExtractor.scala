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
import edu.stanford.nlp.ling.CoreAnnotations.DocIDAnnotation
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
import edu.washington.multirframework.corpus.MyCorpus
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
//import edu.illinois.cs.cogcomp.wikifier.Linker
import edu.washington.multirframework.argumentidentification.KBPTaggersArgumentIdentification
//this class was used in tac2013 for tagging
//import edu.knowitall.tac2013.app.SemanticTaggers
import edu.knowitall.taggers.tag.TaggerCollection
import edu.washington.multirframework.corpus.CorpusInformationSpecification.SentDocNameInformation.SentDocName;
import KBPQueryEntityType._

object RunKBP2015MultirExtractor {  
  
  val annotatorHelper = new StanfordAnnotatorHelperMethods()

  val cis  = new DefaultCorpusInformationSpecification()
  val javaDocInfoList = new java.util.ArrayList[DocumentInformationI]()
  javaDocInfoList.add(new DocCorefInformation())
  cis.addDocumentInformation(javaDocInfoList)
  val javaSentInfoList = new java.util.ArrayList[SentInformationI]()
  javaSentInfoList.add(new SentNamedEntityLinkingInformation())
  cis.addSentenceInformation(javaSentInfoList)
    
  
  def main(Args: Array[String]){
    
      var runID = "UWashington4"
      var detailed = false
      //var detailed = true
      // --------------------------------
      // For SingleNameResolver
      // --------------------------------
      //val round1QueriesFile = "tac_2014_kbp_english_cold_start_evaluation_queries.xml"
      //val round2QueriesFile = "multir_round2_queries.xml"
      val round1QueriesFile = "queries2015_r1.xml"
      val round2QueriesFile = "queries2015_r1.xml"
      
      println("Running KBP!")  
      
      // ---------------------------------------------------------------------
      // Get args
      // ---------------------------------------------------------------------
      
      val modePreprocessed = Integer.parseInt(Args(1))
      val corpusOldNew = Args(2)
      val relevantDocsFileName = Args(3)
      val relevantDocsFile = new File(relevantDocsFileName)      
      val outputStream = new PrintStream(Args(4))
      val roundID = Args(5)
      val batchDrop = Args(6).toInt
      val batchDropRight = Args(7).toInt
      
      println("roundID: " + roundID)
      println("batchDrop: " + batchDrop)
      println("batchDropRight: " + batchDropRight)
      println("corpus: " + corpusOldNew)
      
      // ---------------------------------------------------------------------
      // Select the Solr Index
      // ---------------------------------------------------------------------
            
      SolrHelper.setConfigurations(corpusOldNew, false)
      
      // ---------------------------------------------------------------------
      // Parse queries
      // ---------------------------------------------------------------------
      
      val queriesNoAliases = KBPQuery.parseKBPQueries(Args(0), roundID)	  
      val queries = KBPQuery.getAliases(queriesNoAliases)

      println("Number of Queries: " + queries.size)
      
      
      val outFmt = detailed match {
             case true => OutputFormatter.detailedAnswersOnly(outputStream, runID)
             case false => OutputFormatter.formattedAnswersOnly(outputStream, runID)
      }

      // ----------------------------------------------------------------------------
	  // Specify Multir Extractor
      // ----------------------------------------------------------------------------
      
	  val multirExtractor = new MultiModelMultirExtractorVersionColdStart()	  
      //val multirExtractor = new SingleModelMultirExtractorVersion1()	
	  

	  // ----------------------------------------------------------------------------
	  // Single Name Resolver
      // ----------------------------------------------------------------------------
	  
	  try{
	  
	    var queryNameSetRound2 = Set[String]()
        var queryNameSetRound1 = Set[String]()

        queryNameSetRound1 = KBPQuery.parseKBPQueriesToGetNames(round1QueriesFile)
        queryNameSetRound2 = KBPQuery.parseKBPQueriesToGetNames(round2QueriesFile)           
        
        //println("SNR: r1 size: " + queryNameSetRound1.size)
        //println("SNR: r2 size: " + queryNameSetRound2.size)
      
        // For PER queries which have a single name, replace that name with a full name,
        // if one can be determined
        for(query <- queries){	    
	      var singleQueryNamePER = false
          
	      singleQueryNamePER = query.entityType match{
            case PER if(query.name.split(" ").size == 1) => {
              val (single, qname) = SingleNameResolver.singleQueryName(query, queryNameSetRound2, queryNameSetRound1)
              if(!single) query.name = qname 
              single
        	}
	        case _ => false
	      }          
        }
	    
	  }catch {case e: Exception => 
	      {e.printStackTrace()
	       println("EXCEPTION: SingleNameResolver") 
	      }	  
	  } 

	  //return
	  
	  // ----------------------------------------------------------------------------	
      // Get Relevant Docs
	  // ----------------------------------------------------------------------------
	  
	  val entityRelevantDocSerialization = {
		    
	        if(relevantDocsFile.exists()){
		      QuerySetSerialization.getRevelantDocIdMap(relevantDocsFileName)
		    }
	        
	        // LATER: insert modePreprocessed here, when get connection to Derby DB
	        // mode 0 - don't have linking information, select reldocs which contain queryname
	        // mode 1 - use linking information to select the reldocs
	        
		    else{
		      // make this map and write it out
              val qm = SolrHelper.getRelevantDocuments(queries)
              // use cold start method if working with a list of files which are a subset of 
              // and existing solr index
		      //val qm = SolrHelper.getRelevantDocumentsColdStart(queries)
		      val qidMap = qm.toList.map(f => (f._1.id,f._2)).toMap
		      QuerySetSerialization.writeRelevantDocIdMap(qidMap, relevantDocsFileName)
		      qidMap
		    }
      }  
	  
	  // --------------------------------------------------------------------------------------	
      // Group the Queries - create a list of List[KBPQuery], 
	  // each element of the list is a list of KBPQueries which have the same query name,
	  // so that the doc processing can be shared
	  // --------------------------------------------------------------------------------------
	  
      var querySet: Set[String] = Set()
      var sameQueries : List[List[KBPQuery]] = List()
    
      for(query <- queries){
        val qn = query.name
      
        if(!querySet.contains(qn)){
          //add query name to the set, which indicates it has been processed
          querySet += qn   
          //get all matching queries for this query name
          val matchingQueries = queries.filter(q => qn.equals(q.name))
          sameQueries = sameQueries ++ List(matchingQueries)        
        } 
      }	  
	
      println("sameQueries size: " + sameQueries.size)	  

      //--------------------------------------------------------------------------
      //If we want to run a subset of the collapsed queries, this is that subset
      //--------------------------------------------------------------------------
      val sameQueriesBatch = sameQueries.drop(batchDrop).dropRight(batchDropRight)
      
      println("sameQueriesBatch size: " + sameQueriesBatch.size)	  
      
      // ----------------------------------------------------------------
      // Examine memory use
      // ----------------------------------------------------------------

      println
	  println("total memory: " + Runtime.getRuntime().totalMemory())
      //the Xmx value
      println("max memory: " + Runtime.getRuntime().maxMemory())
      println("free memory: " + Runtime.getRuntime().freeMemory())  		        
      println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
      println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
      println    

      var queryCount = 0
      
      for(sameQueryName <- sameQueriesBatch){        

        queryCount += 1
        
        val firstQuery = sameQueryName(0) 
        val queryName = firstQuery.name
        println
        println("query " + queryCount + ": " + firstQuery.id + " " + queryName)
        println
        
        // -------------------------------------------------------------------
        // singleQueryNamePER -- If query entityType is PER and query name is 
        // a single name (i.e. couldn't be resolved to a full name above),
        // set flag here to true, to use to drop it
        // -------------------------------------------------------------------
        
	    var singleQueryNamePER = false
     
        singleQueryNamePER = firstQuery.entityType match{
            case PER if(queryName.split(" ").size == 1) => true
	        case _ => false
	    }
      
        // -----------------------------------------------------------------------
        // anyRelevantSlots -- If the query set has no slots being filled by
        // this PERLOC Multir model, set flag here to false, to use to drop it
        // -----------------------------------------------------------------------
                
        var anyRelevantSlots = false
        
        val sameQueriesRelevantSlot = sameQueryName.filter(q => ColdStartSlots_Multir.slots.contains(q.slotsToFill.toList(0).name))
        
        println("sameQueriesRelevantSlot size: " + sameQueriesRelevantSlot.size)        
              
        if(sameQueriesRelevantSlot.size > 0) anyRelevantSlots = true  
                
        // ------------------------------------------------------------------------------------------------
        // Proceed if this set of queries does not have a single PER name, and has relevant slots to fill
        // ------------------------------------------------------------------------------------------------
        if(!singleQueryNamePER && anyRelevantSlots){
          
	      try{

	        // --------------------------------------------------------------------------
	        // Set of Relevant Extraction Candidates from the set of relevant documents
	        // --------------------------------------------------------------------------
	        var allRelevantCandidates: Seq[Candidate] = Nil

	        // --------------------------------------
            // Process Documents for this Query Set  
            // --------------------------------------    
		        		              	      
		    val relevantDocs = entityRelevantDocSerialization(firstQuery.id).toSet		      
		  
		    val documents :List[Option[Annotation]] = {		    
		      modePreprocessed match {
		        case 0 => processDocuments(relevantDocs)
		        case _ => processDocuments(relevantDocs)	
		        //case _ => processDocuments(nwDocuments)		      
		      } 
		    }
		    println("Getting Extractions")
		  
            for(document <- documents){
              if(document.isDefined){
                val extractions = multirExtractor.extract(document.get, firstQuery).asScala		          		          		      
                val relevantCandidates = FilterExtractionResults.filterResults(FilterExtractionResults.wrapWithCandidate(extractions), firstQuery, document)     
                if(relevantCandidates.size > 0) allRelevantCandidates = allRelevantCandidates ++ relevantCandidates
              }
            }
		  
		    println("Processing Each Query in the Set")
  
		    var querySetCount = 0
            for(query <- sameQueriesRelevantSlot){		      

              querySetCount += 1
              
              println("query " + querySetCount)
              
		      val slots = query.slotsToFill
		       	
		      val kbpAllRelevantCandidates = FilterExtractionResults.substituteKBPRelationsColdStart(allRelevantCandidates, query)
              
		      val bestAnswers = slots map { slot => ( slot, SelectBestAnswers.reduceToMaxResults(slot, kbpAllRelevantCandidates.filter(_.extr.getRel() == slot.name)) ) } toMap
		               	      
		      outFmt.printAnswers(bestAnswers, query)    
		    }		   
		  
	    }
	    catch {case e: Exception => 
	      {e.printStackTrace()
	         println("EXCEPTION: " + firstQuery.id + " " + firstQuery.name) 
	         //outFmt.printEmpty(query)
	      }	  
	    }		    		  	 
      }   
	  else{
	    println("Skipping this query: anyRelevantSlots= " + anyRelevantSlots + " singleQueryNamePER: " + singleQueryNamePER)
	    //Don't need to print NIL for Cold Start
	    //outFmt.printEmpty(query)
	  }
	     
	  println("Finished, going to next query")
      println
	    
    } //queries
		  	  
	  println("Finished with Queries")
	  
	  outputStream.close()
	  
	  println("Closed outputStreams")

	  println
	  println("total memory: " + Runtime.getRuntime().totalMemory())
      //the Xmx value
      println("max memory: " + Runtime.getRuntime().maxMemory())
      println("free memory: " + Runtime.getRuntime().freeMemory())  		        
      println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
      println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
	  
  } //main
  
  
  def processDocumentsStanford(documents: Set[String]):  List[Option[Annotation]]  = {
    println("Number of docs = " + documents.size)
       var start :Long = 0
       var end: Long = 0    
    for(doc <- documents.toList) yield{
      start = System.currentTimeMillis()
      val a =processDocument(doc)
      end = System.currentTimeMillis()
      println("Document took " + (end-start) + " milliseconds")
      a
    }
  }
  
  def processDocuments(documents: Set[String]): List[Option[Annotation]] = {
    println("Number of docs = " + documents.size)
    var startTime :Long = 0
	var endTime: Long = 0    	 
	var docCount = 0
	var docs = documents.toList
	// Setting max number of documents to 500
    val maxSize = 500
    //val maxSize = 5
    if(docs.size > maxSize){docs = docs.dropRight(docs.size-maxSize)}
    println("Docs.size: " + docs.size)
    //for(doc <- documents.toList) yield{
    for(doc <- docs) yield{
      docCount = docCount + 1
      println("Processing Doc # :" + docCount + " " + doc)
      var a :Option[Annotation] = None
      val t = new Thread {
        override def run() {    
          startTime = System.currentTimeMillis()
          //a =processDocument(doc)
          a = stanfordProcessDocument(doc)
          endTime = System.currentTimeMillis()
          println("Thread: Document took " + (endTime-startTime) + " milliseconds")      
        }
      }                                              
      t.start()
      //t.join(10000)
      t.join(180000) 
      t.stop()
      a
    }
  }
  
  def cjParseDocument(docName: String): Option[Annotation] = {
    try{
      val rawDoc = SolrHelper.getRawDoc(docName)
      if(rawDoc.length < 20000){
      val preprocessedAndParsedDoc = CorpusPreprocessing.getTestDocumentFromRawString(rawDoc,docName)
      println("Document was cj parsed")
      Some(preprocessedAndParsedDoc)
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
  
  def linkDocument(docName: String): Option[Annotation] ={
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
  }
  
  def processDocument(docName: String) : Option[Annotation]  ={ 
    try{          
      println("Processing document " +docName)
      val stanfordDoc = stanfordProcessDocument(docName)
      val cjParsedDoc = cjParseDocument(docName)
      //val linkedDoc = linkDocument(docName)      	     
    	     
      if(stanfordDoc.isDefined && cjParsedDoc.isDefined /*&& linkedDoc.isDefined*/){
        val ann = joinAnnotations(stanfordDoc.get,cjParsedDoc.get,new Annotation());
    	//println("have NEL = "+ann.get(classOf[NamedEntityLinkingAnnotation]))
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
  
  def joinAnnotations(stanfordDoc: Annotation, cjParsedDoc: Annotation, linkedDoc: Annotation) : Annotation = {  
    //add coref annotations to cjParsedDoc
    cjParsedDoc.set(classOf[CorefCoreAnnotations.CorefChainAnnotation],stanfordDoc.get(classOf[CorefCoreAnnotations.CorefChainAnnotation]))
    cjParsedDoc.set(classOf[NamedEntityLinkingAnnotation],linkedDoc.get(classOf[NamedEntityLinkingAnnotation]))
    cjParsedDoc
  }
  
  def stanfordProcessDocument(docName: String) : Option[Annotation] = {
    try{
      val rawDoc = SolrHelper.getRawDoc(docName)
      if(rawDoc.length < 20000){
        val processedDoc = new Annotation(rawDoc)
        annotatorHelper.getCorefPipeline().annotate(processedDoc)
        //need to set below when not running CJ
        processedDoc.set(classOf[DocIDAnnotation], docName)
        println("Document was Stanford Annotated: " + processedDoc.get(classOf[DocIDAnnotation]))
        //this is null below, comes from CJ call
        //println("DwSA SentDocName: " + processedDoc.get(classOf[SentDocName]))
        Some(processedDoc)
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
  

}
