package edu.washington.cs.knowitall.kbp2014.multir.slotfiller;

import java.io.File;
import java.util.List;
import java.util.Map;

import scala.collection.Iterator;

//import java.util.ArrayList;
//import java.util.List;


public class RunKBP {
		
	
	
	public static void main(String [ ] args)
	{   
		// ------------------------------------
		// Get Queries
		// ------------------------------------
		scala.collection.immutable.List<KBPQuery> queries = KBPQuery.parseKBPQueries(args[0]);			
        // If I find I need a java list instead of a scala list, *might* want this
		//List<KBPQuery> myList = new ArrayList<KBPQuery>();

		// ----------------------------------------------------------------------
		// Print Queries
		// ----------------------------------------------------------------------
		System.out.println("Printing " + queries.size() + " queries....");
		
		Iterator<KBPQuery> iter = queries.iterator();
		
		while (iter.hasNext()){
		    System.out.println(iter.hasNext());
		}

		// -----------------------------------------------------------------------
		// Handle: mode = [0 | 1] document processing at run time (0), or not (1)
        //------------------------------------------------------------------------
		Integer mode = Integer.parseInt(args[1]);		

		if(mode == 0){
			  // args[2] = "old" or "new" corpus
			  String conf = args[2];
			  SolrHelper.setConfigurations(conf,false);
		
		      // args[3] == path to map file	 			    
			  File f = new File(args[3]);
			  if(f.exists() && !f.isDirectory()){
			     Map<String, List<String>> MapentityRelevantDocSerialization = QuerySetSerialization.getRevelantDocIdMap(args[3]);
			  }
			  else{			    	
			     // make this map and write it out
			     // val m = getRelevantDocuments(queries,solr).toMap
			     Map<KBPQuery,List<String>> qm = SolrHelper.getRelevantDocuments(queries);
			     
			     Map<String, List<String>> qidMap = new Map<String, List<String>>();

			     for (Entry<String,String> e : qm.entrySet()) {
			       qidMap.put(e.getKey().id, e.getValue());
			     }			     
			     QuerySetSerialization.writeRelevantDocIdMap(qidMap,args[3]);
			     Map<String, List<String>> MapentityRelevantDocSerialization = qidMap;
			    }
			  
			  multirExtractor = new MultiModelMultirExtractorVersion1();
			  bw = new BufferedWriter(new FileWriter(new File(args[4])));
			  //extract
			  sortedQueries = queries. .sortBy(entityRelevantDocSerialization.get(f.id).size)
					  
			  for(query <- sortedQueries){
			      val documents = processDocuments(entityRelevantDocSerialization.values.flatten.toSet)
			      for(document <- documents){
			        if(document.isDefined){
			          val extractions = multirExtractor.extract(document.get, query).asScala
			          for(e <- extractions){
			            println(e)
			            val docText = document.get.get(classOf[CoreAnnotations.TextAnnotation])
			            val minIndex = math.min(e.getArg1().getStartOffset(),e.getArg2().getStartOffset())
			            val maxIndex = math.max(e.getArg2().getEndOffset(),e.getArg1().getEndOffset())
			            println("Sentence = " +docText.subSequence(minIndex,maxIndex))
			            bw.write(e + "\n")
			            bw.write("Sentence = " +docText.subSequence(minIndex,maxIndex)+"\n")
			          }
			        }
			      }
			  }
			  bw.close()	  		  
			  
	  }
	
		
	}//main

}//class
