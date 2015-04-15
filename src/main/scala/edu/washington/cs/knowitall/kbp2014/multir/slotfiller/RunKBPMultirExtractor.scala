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
import edu.washington.multirframework.corpus.MyCorpus
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
//import edu.illinois.cs.cogcomp.wikifier.Linker
import edu.washington.multirframework.argumentidentification.KBPTaggersArgumentIdentification
//this class was used in tac2013 for tagging
//import edu.knowitall.tac2013.app.SemanticTaggers
import edu.knowitall.taggers.tag.TaggerCollection

object RunKBPMultirExtractor {
  
  
  val annotatorHelper = new StanfordAnnotatorHelperMethods()

  val cis  = new DefaultCorpusInformationSpecification()
  val javaDocInfoList = new java.util.ArrayList[DocumentInformationI]()
  javaDocInfoList.add(new DocCorefInformation())
  cis.addDocumentInformation(javaDocInfoList)
  val javaSentInfoList = new java.util.ArrayList[SentInformationI]()
  javaSentInfoList.add(new SentNamedEntityLinkingInformation())
  cis.addSentenceInformation(javaSentInfoList)
  
  
  
  def main(Args: Array[String]){
    
      var runID = "UWashington3"
      var detailed = false
      // Include printed extractions in output file?
      var printExtractions = true
      // Print a tab-delimited file of features and feature weights?
      var printModelTrace = true

      // ------------------------------------------------------------------
      // This is the TAC_2009 Corpus
      // ------------------------------------------------------------------
      val fullCorpusPath = "jdbc:derby:/homes/gws/nhawkins/KBP2014-Slotfilling-Multir/FullCorpus"
      //val fullCorpusPath = "jdbc:derby://rv-n14.cs.washington.edu:49152//scratch2/usr/nhawkins/FullCorpus-UIUCNotableTypes"
      //val fullCorpusPath = "jdbc:derby:/projects/WebWare5/multir-multilingual/training/testrunNH/derbyDB_First100Sentences"
      val multirCorpus = new MyCorpus(fullCorpusPath,cis,true)
      //val fullCorpusPath = "jdbc:derby:/homes/gws/nhawkins/KBP2014-Slotfilling-Multir/FullCorpus/"
      //val multirCorpus = new MyCorpus(fullCorpusPath,cis,true)
      // ------------------------------------------------------------------
      // ------------------------------------------------------------------
      
      /*val docIterator = multirCorpus.getDocumentIterator()
      var count = 0
      var addToCount = 0
      while(docIterator.hasNext) {    
        //addToCount = docIterator.next match{
        //  case Nil => 0  
        //  case  _ => 1  
        //}                
        //if(docIterator.next.size() > 0 ){count = count + addToCount}
      }
      println("Derby DB number of docs: " + count) */
      
      // Column 'SENTNAMEDENTITYLINKINGINFORMATION' not found.
      val doc = multirCorpus.getDocument("XIN__ENG__20021028.0184.LDC2007T07")
      // doesn't work
      //val doc = multirCorpus.getDocument("AFP__ENG__20080310.5012.LDC2009T13")
      
      //val doc = multirCorpus.getDocument("AFP_ENG_20100310.0561")
      //val doc = multirCorpus.getDocument("AFP__ENG__20100310.0561")
      //val doc = multirCorpus.getDocument("AFP__ENG__201003")
      //val doc = multirCorpus.getDocument("AFP_ENG_201003")
      
      //if(doc.size>0)println("doc size > 0")else println("doc size <= 0")      
      //if(Some(doc).isEmpty)println("Some(doc) is empty")else println("Some(doc) is not empty")
      
      val sentence0 = doc.get(classOf[SentencesAnnotation]).get(0)
      val sentence1 = doc.get(classOf[SentencesAnnotation]).get(1)
      val sentence2 = doc.get(classOf[SentencesAnnotation]).get(2)
      
      println("sent 2= " + sentence2 )
      
      val url = getClass.getResource("/edu/washington/multir/taggers/ReligionTaggers/religions.xml")
      require(url != null, "Could not find resource: religions.xml")
      TaggerCollection.fromPath(url.getPath())
      //val ReligionTagger = 
      
      //val kbpTaggersAI = new KBPTaggersArgumentIdentification()
      //val args = kbpTaggersAI.identifyArguments(doc, sentence2)
      //println("args: " + args.size())
      
      //println("args 0: " + args.get(0))      
      
      //val sent0Link = doc.get(classOf[SentencesAnnotation]).get(0).get(classOf[NamedEntityLinkingAnnotation])      
      
      //val docCoref = doc.get(classOf[CorefChainAnnotation])     

      //println("sent 0= " + sentence0 )
      //println("sent 1= " + sentence1 )
      //println("sent0 link info: " + sent0Link)
      //println("doc coref info: " + docCoref)
      println("Running KBP!")  
            
      // call with a chunked sentence
      //val tokens = sentence1.get(classOf[CoreAnnotations.TokensAnnotation])    
      //println("tokens: " + tokens.size())
      //val types = SemanticTaggers.useJobTitleTagger(tokens)
      //println("types: " + types.length)
      
      
      System.exit(0)

      /*val locationName = "China"
      val nd = NellData.getNellData(locationName)
      
      val location = nd match {
        //case Some(x) => assignLocationNell(x, locationName)
        case Some(x) => {println("NELL: city " + x.highestProbabilityIsCity)
                         println("NELL: stateorprovince " + x.highestProbabilityIsStateOrProvince)
                         println("NELL: country " + x.highestProbabilityIsCountry)
                        }
        case None => println("No Nell Data")  
        //case None => assignLocationTipster(locationName)
      }    

      val locationTuple = (TipsterData.cities.contains(locationName.toLowerCase()),
                         TipsterData.stateOrProvinces.contains(locationName.toLowerCase()),
                         TipsterData.countries.contains(locationName.toLowerCase()))

      println("TIPSTER: city " + locationTuple._1)                   
      println("TIPSTER: stateprov " + locationTuple._2)                   
      println("TIPSTER: country " + locationTuple._3)
      
      System.exit(0) */
      
      val queries = KBPQuery.parseKBPQueries(Args(0))	  
	  val modePreprocessed = Integer.parseInt(Args(1))
      val corpusOldNew = Args(2)
      val relevantDocsFileName = Args(3)
      val relevantDocsFile = new File(relevantDocsFileName)      
      val outputStream = new PrintStream(Args(4))
      val outFmt = detailed match {
             case true => OutputFormatter.detailedAnswersOnly(outputStream,runID)
             case false => OutputFormatter.formattedAnswersOnly(outputStream,runID)
      }
      
      val outputStreamTrace = new PrintStream(Args(5))
      
      // ----------------------------------------------------------
      // Current Status: coref is ON, with no flag to turn it off
      // ----------------------------------------------------------
      //val coref = Args(5).toLowerCase() match {
      //  case "off" => "off"
      //  case "on" => "on"
      //  case _ => "off"
      //}
      
	  val multirExtractor = new MultiModelMultirExtractorVersion2()
	  
      
      
	  // * ----------- Get Relevant Documents --------------- *//
	  
	  //System.exit(0)

	  SolrHelper.setConfigurations(corpusOldNew,false)
	  
	  //System.exit(0)
	  
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
		      val qidMap = qm.toList.map(f => (f._1.id,f._2)).toMap
		      QuerySetSerialization.writeRelevantDocIdMap(qidMap, relevantDocsFileName)
		      qidMap
		    }
      }
	
	  // * ---- Check Number of Unique Documents ---- * //
      // 
	  // results:
	  //  -- 6022 for 100 queries from 2014 
	  //  -- 4912 for 100 queries from 2013 
	  
	  //val uniqueDocs = entityRelevantDocSerialization.values.flatten.toSet
	  //println("Unique Docs:" + uniqueDocs.size )	  
	  //System.exit(0)
	  
	  
	  // * ----------- Process Queries --------------- *//

	  println("Running " + queries.size + " queries.")
      println("QID_Map: " + entityRelevantDocSerialization.keySet)
	  for(key<-entityRelevantDocSerialization.keySet){
	    println(entityRelevantDocSerialization(key).toSet)  
	  }
      
      
	  for(query <- queries){

	    if(printExtractions){  
		  outputStream.println(query.name + "\n")  
	    }
	    
	    try{
		        		      
		  var allRelevantCandidates: Seq[Candidate] = Nil
		  println("Starting Query Processing: Size allRelevantCandidates: " + allRelevantCandidates.size)
		  
		  
		          	      
		  val relevantDocs = entityRelevantDocSerialization(query.id).toSet		      
		  val nwngDocuments = relevantDocs.filter(doc => !doc.startsWith("bolt") ) 
          val nwDocuments = nwngDocuments.filter(doc => !doc.startsWith("eng-"))		    
       
          println("Query: " + query.id)
		  println("Size All Documents: " + relevantDocs.size)
		  println("Size NW Documents: " + nwDocuments.size)
		  
		  //System.exit(0)
		  
		  val documents :List[Option[Annotation]] = {
		    
		    modePreprocessed match {
		      case 0 => processDocuments(relevantDocs)
		      //case 0 => processDocuments(nwDocuments)
		      // Do not have a 2013 Corpus in a Derby DB
		      //case 1 => for(doc <- nwDocuments.toList) yield{
		        //val docNameWithDoubleUnderscores :String = doc.replace("_","__") 
		        //println("Derby DB: Getting doc: " + doc)
		        //println("Derby DB: Getting doc: " + docNameWithDoubleUnderscores)
                //val document = multirCorpus.getDocument(docNameWithDoubleUnderscores)
                //val document = multirCorpus.getDocument(doc)
                //println("doc coref info: " + document.get(classOf[CorefChainAnnotation]))    
                //if(document.size > 0) Some(document) else None    
		      //}  		     
		      case _ => processDocuments(nwDocuments)		      
		    }
            
		  }

		  //val queryLink = getQueryLink(query.doc)
		  
          for(document <- documents){
            if(document.isDefined){

              println("DOCUMENT is DEFINED: " + query.id)
              
              // * --------- Print Model Trace ---------- * //
              
              if(printModelTrace){
                
                document match {
                  case Some(ann) => multirExtractor.printTrace(ann, query, outputStreamTrace)
                  //case Some(ann) => multirExtractor.printFeaturesTrace(ann, query, outputStreamTrace)
                  case None => outputStreamTrace.println("document matched None")
                }  
                
                //System.exit(0)
                
              }
              //else{
              
              
              val extractions = multirExtractor.extract(document.get, query).asScala
		          		          
		      println("Size Extractions: " + extractions.size)
                              
              //val relevantCandidates = FilterExtractionResults.filterExtractions(FilterExtractionResults.wrapWithCandidate(extractions), query)     
              val relevantCandidates = FilterExtractionResults.filterResults(FilterExtractionResults.wrapWithCandidate(extractions), query, document)                   
                    
		      println("Size RelevantCandidates: " + relevantCandidates.size)
		          
		      if(relevantCandidates.size > 0)
		        allRelevantCandidates = allRelevantCandidates ++ relevantCandidates

		        
		      for(c <- relevantCandidates){
		        println(c.extr)	                           		              
		      }
		        
		      if(printExtractions){      
		        		        
		        for(e <- extractions){
                //for(c <- relevantCandidates){
		          val docText = document.get.get(classOf[CoreAnnotations.TextAnnotation])
		          val minIndex = math.min(e.getArg1().getStartOffset(),e.getArg2().getStartOffset())
		          val maxIndex = math.max(e.getArg2().getEndOffset(),e.getArg1().getEndOffset())
		          //println("Sentence = " +docText.subSequence(minIndex,maxIndex))
		          //bw.write(e + "\n")
		          //bw.write("Sentence = " +docText.subSequence(minIndex,maxIndex)+"\n")		         

		          //val minOffset = math.max(minIndex-20, 0)
		          //val maxOffset = math.min(maxIndex+20, docText.length)
		          outputStream.println(e + ", " + docText.subSequence(minIndex,maxIndex) + "\n" )		                           		              
		        }
		        
                for(c <- relevantCandidates){
		          val docText = document.get.get(classOf[CoreAnnotations.TextAnnotation])
		          val minIndex = math.min(c.extr.getArg1().getStartOffset(),c.extr.getArg2().getStartOffset())
		          val maxIndex = math.max(c.extr.getArg2().getEndOffset(),c.extr.getArg1().getEndOffset())	         
		          //val minOffset = math.max(minIndex-20, 0)
		          //val maxOffset = math.min(maxIndex+20, docText.length)
		          outputStream.println(query.id + " " + c.extr + ", " + docText.subSequence(minIndex,maxIndex) + "\n" )		                           		              
                }		        
		        
		      } //printExtractions
		      
              //}		    
		      
            } //if doc defined
          } //documents

		      //if(!printModelTrace){
          
		      val slots = query.slotsToFill
 		      
		      println("Done Processing Documents")
		      println("Size allRelevantCandidates: " + allRelevantCandidates.size)
		      
		      println("SubstituteKBPRelations")    	
		      val kbpAllRelevantCandidates = FilterExtractionResults.substituteKBPRelations(allRelevantCandidates, query)
              
		      println("Make Slot Map - Best Answers")		      
		      val bestAnswers = slots map { slot => ( slot, SelectBestAnswers.reduceToMaxResults(slot, kbpAllRelevantCandidates.filter(_.extr.getRel() == slot.name)) ) } toMap
		               	      
		      outFmt.printAnswers(bestAnswers, query)
		      //}
		      
		  }
	      catch {case e: Exception => 
	        {e.printStackTrace()
	         println("EXCEPTION: " + query.id + " " + query.name) 
	         outFmt.printEmpty(query)
	        }	  
	      }		    		  
		  
	      println("Finished, going to next query")
	      
    } //queries
		  	  
	  println("Finished with Queries")
	  
	  outputStream.close()
	  outputStreamTrace.close()
	  
	  println("Closed outputStreams")
	  
  } //main
  
  /*def getQueryLink(docNameWithOffsets :String) :String = {

    //Annotation document = ... // where your process the documents(via stanfordProcess, cjparse, linking and then joinAnnotation).
    //         Linker.instance().link(document); // call the linker 
    //         for (CoreMap sent : annotation.get(SentencesAnnotation.class)) {
    //            // get the links for each sentence. the triple is Pair<startToken, endToken>, mid and confidence
    //            List<Triple<Pair<Integer, Integer>, String, Float>> links = sent.get(NamedEntityLinkingAnnotation.class);
    //            // do stuff
    //            // ...
    //         }
    
    val doc = new Annotation(docNameWithOffsets)
    val linker = Linker.instance().link(docNameWithOffsets)
    
    val queryLink = ""

    queryLink
  } */
  
  
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
  
  def joinAnnotations(stanfordDoc: Annotation, cjParsedDoc: Annotation, linkedDoc: Annotation) : Annotation = {  
    //add coref annotations to cjParsedDoc
    cjParsedDoc.set(classOf[CorefCoreAnnotations.CorefChainAnnotation],stanfordDoc.get(classOf[CorefCoreAnnotations.CorefChainAnnotation]))
    cjParsedDoc.set(classOf[NamedEntityLinkingAnnotation],linkedDoc.get(classOf[NamedEntityLinkingAnnotation]))
    cjParsedDoc
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
  

}
