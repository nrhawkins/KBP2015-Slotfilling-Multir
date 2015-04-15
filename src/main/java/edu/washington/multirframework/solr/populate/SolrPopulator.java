/*
 * solr files on rv-n16.cs.washington.edu: 
 * /scratch/usr/nhawkins/solr/solr/kbpdev
 */

package edu.washington.multirframework.solr.populate;

import java.io.File;
//import java.io.FilenameFilter;
//import java.io.IOException;
import java.util.ArrayList;
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer;
import org.apache.solr.common.SolrInputDocument;
import edu.washington.multirframework.annotation.AnnotatedDoc;
//import edu.washington.cs.knowitall.kbp2014.multir.slotfiller.util.DocUtils;
import java.util.Iterator;
import edu.washington.multirframework.util.DocUtils; 
import edu.washington.multirframework.util.DocSplitter; 
import edu.stanford.nlp.util.Pair;

public class SolrPopulator {
	
	public static void main(String[] args) throws Exception {
		
		String inputPath = args[0];
    	String solrUrl = args[1];
    	
    	int threadCount = 4;
    	int queueSize = 1000;    	
 
    	// Each file contains articles for one month, an example file is: XIN_CMN_199212
    	// The Chinese KBP directory contains 460 files; i.e. 460 months of articles
        
    	File[] files = DocUtils.findFiles(new File(inputPath)); 
        
        System.out.println("number of files: " + files.length );
        //System.exit(0);
        
        // The DocSplitter splits each month-file into the individual articles, 
        // which is determined by the tags <DOC> ... </DOC> 
    	DocSplitter splitDocs = new DocSplitter();
    	boolean cleanXML = true;
        ArrayList<Pair<String,String>> docPairs = new ArrayList<Pair<String,String>>();
        int fileCount = 0;
        
        for(File file : files){
        	
        	if(!file.getName().endsWith("txt")){

        	fileCount = fileCount + 1;	        		
           	ArrayList<AnnotatedDoc> docs = new ArrayList<AnnotatedDoc>();
        	
        	//Replace the <tags> and /n with spaces, so Stanford CoreNLP can separate sentences,
        	//and provide document-level offsets for the sentences.
        	
            if(!file.isDirectory()){
            	
            	docs = splitDocs.convert(file, cleanXML);         
        	
                System.out.println("file number: " + fileCount );
                System.out.println("number of docs: " + docs.size() );
            
                for(AnnotatedDoc doc: docs){	
            
        	       //Make pairs of (docId, docString) to add to Solr

                   String docString = "";            	
                   String docId = "";

                   try{ docString = doc.getFirstSentence(); docId = doc.getID(); } catch(Exception e){}

                   Pair<String,String> docPair = new Pair<>(docId, docString);
        	
        	       docPairs.add(docPair);    	  
                
                }
            }    
        	}
        }
        
    	System.out.println("inputPath: " + inputPath);
    	System.out.println("sorlUrl: " + solrUrl);
    	System.out.println("number of files: " + files.length );
        System.out.println("number of doc pairs: " + docPairs.size() );
    	
        System.out.println("docPair 0 docid: " + docPairs.get(0).first);
        System.out.println("docPair 0 docstring: " + docPairs.get(0).second);
        
    	
        // Add docPairs to Solr
    	
    	populate(docPairs, solrUrl, queueSize, threadCount);
    			
	}
	
	private static ConcurrentUpdateSolrServer getDefaultSolrServer(String solrUrl, int queueSize, int threadCount) {
		
		return new ConcurrentUpdateSolrServer(solrUrl, queueSize, threadCount); 
	}
	
	public static void populate(ArrayList<Pair<String,String>> docPairs, String solrUrl, int queueSize, int threadCount) {
		
	   ConcurrentUpdateSolrServer solrServer = getDefaultSolrServer(solrUrl, queueSize, threadCount);

	   System.out.println("Got solrServer");
	   
	   try{
		   
		  System.out.println("Deleting old documents."); 
		   
		  //delete old documents
          solrServer.deleteByQuery("*:*");
          solrServer.commit();

          System.out.println("Adding all new documents."); 
          
          //add all new documents
          for(Pair<String,String> docPair : docPairs){
             
        	  SolrInputDocument solrDoc = new SolrInputDocument();
              solrDoc.addField("docid", docPair.first);
              solrDoc.addField("docstring", docPair.second);
              
              solrServer.add(solrDoc);
          }

          System.out.println("Solr all docs added: committing and shutdown."); 
          
          solrServer.commit();
          solrServer.shutdown();
          
	   }catch(Exception e){e.getMessage(); e.printStackTrace();}       
		
	}
	

	
	
}
