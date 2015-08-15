package edu.washington.cs.knowitall.kbp2014.multir.slotfiller;

//import java.util.ArrayList;
//import java.util.List;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreAnnotations.DocIDAnnotation;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.Pair;
import edu.stanford.nlp.util.Triple;
import edu.washington.multir.sententialextraction.DocumentExtractor;
import edu.washington.multirframework.corpus.CorpusInformationSpecification.SentDocNameInformation.SentDocName;
import edu.washington.multirframework.corpus.CorpusInformationSpecification.SentGlobalIDInformation.SentGlobalID;
import edu.washington.multirframework.corpus.SentOffsetInformation.SentStartOffset;
import edu.washington.multirframework.data.Argument;
//import edu.washington.multirframework.featuregeneration.InstanceFeatureGenerator;
import edu.washington.multirframework.featuregeneration.DefaultFeatureGeneratorNoCJ;
//import edu.washington.multirframework.featuregeneration.DefaultFeatureGenerator;
import edu.washington.multirframework.featuregeneration.FeatureGenerator;
import edu.washington.multirframework.argumentidentification.*;

public class SingleModelMultirExtractorVersion1 {  //extends MultiModelMultirExtractor {

	protected SententialInstanceGeneration sig;
	protected String modelFilePath;
	protected ArgumentIdentification ai;
	protected FeatureGenerator fg;
	
	public SingleModelMultirExtractorVersion1(){
		
		sig = DefaultSententialInstanceGeneration.getInstance();
		modelFilePath = "/projects/WebWare6/KBP_2015/multir/model";
		ai = PERLOCArgumentIdentification.getInstance();
		//ai = NERArgumentIdentification.getInstance();
		//ai = NERArgumentIdentificationPlusMISC.getInstance();
		//ai = ExtendedNERArgumentIdentification.getInstance();
		//fg = new DefaultFeatureGenerator();	    
		//modified to not rely on token offsets class made available by the processing docs steps used when using Charniak-Johnson
	    fg = new DefaultFeatureGeneratorNoCJ();	    
	}
		
	public List<Extraction> extract(Annotation doc, KBPQuery q) throws IOException{
		
		List<Extraction> extractions = new ArrayList<>();
				
		List<CoreMap> sentences = doc.get(CoreAnnotations.SentencesAnnotation.class);

		//System.out.println("smme: num sentences: " + sentences.size());
		
		DocumentExtractor de = new DocumentExtractor(modelFilePath, fg, ai, sig);
		
		String docID = doc.get(DocIDAnnotation.class);
		//int sentenceCount = 0;
		
	    for(CoreMap s : sentences){
	    	
	      String senText = s.get(CoreAnnotations.TextAnnotation.class);
	      
	      if(senText.length() < 400){

	      List<CoreLabel> tokens = s.get(CoreAnnotations.TokensAnnotation.class);
		  int sentStartOffset = 0;
		  if(tokens.size() > 0) sentStartOffset = tokens.get(0).beginPosition();
	    	  
	      //this SentStartOffset.class is working, with CJ	  
	      //System.out.println("Sentence: " + docID + " " + s.get(SentStartOffset.class) + " " + s); 	
	      //System.out.println("Sentence: " + docID + " " + sentStartOffset + " " + s); 	
	      	      
	      //System.out.println("Tokens: " + tokens.size());
	      
	      //for(int i =0; i < tokens.size(); i++){
	      //  String ner = tokens.get(i).get(CoreAnnotations.NamedEntityTagAnnotation.class);
	      //  System.out.print(ner + " ");
	      //  System.out.print(tokens.get(i).originalText() + " " + ner + " " + tokens.get(i).beginPosition() + " " + tokens.get(i).endPosition() + " ");
	      //  System.out.print(i + " "); 
	      //}	      
	      //System.out.println();
	      
	      List<Argument> arguments = ai.identifyArguments(doc, s);

	      //for(Argument a : arguments){
	      //   System.out.println("Arguments: " + a.getArgName());
	      //}   
	      
	      List<Pair<Argument,Argument>> sententialPairs = sig.generateSententialInstances(arguments, s);
	      
	      for(Pair<Argument,Argument> sententialPair : sententialPairs){
				Triple<String,Double,Double> result = de.extractFromSententialInstance(sententialPair.first, sententialPair.second, s, doc);
				String rel = result.first;				
				double score = result.third;
				
				//System.out.println("Relation: " + rel + " Score: " + score + " " + sententialPair.first + " " + sententialPair.second);
				
				if(!rel.equals("NA")){
					//add new extraction
					Argument arg1 = sententialPair.first;
					Argument arg2 = sententialPair.second;
					
					System.out.println("Relation: " + rel + " Score: " + score + " " + sententialPair.first + " " + sententialPair.second);
					
					//These offsets look good.
					//System.out.println("arg1: " + arg1.getArgName());
					//System.out.println("arg1 so: " + arg1.getStartOffset());
					//System.out.println("arg1 eo: " + arg1.getEndOffset());
					
					//SentStartOffset is null
                    //System.out.println("sent so: " + s.get(SentStartOffset.class));					

					//arg1 = new Argument(arg1.getArgName(),s.get(SentStartOffset.class)+arg1.getStartOffset(),s.get(SentStartOffset.class)+arg1.getEndOffset());
					//arg2 = new Argument(arg2.getArgName(),s.get(SentStartOffset.class)+arg2.getStartOffset(),s.get(SentStartOffset.class)+arg2.getEndOffset());

					arg1 = new Argument(arg1.getArgName(),sentStartOffset+arg1.getStartOffset(),sentStartOffset+arg1.getEndOffset());
					arg2 = new Argument(arg2.getArgName(),sentStartOffset+arg2.getStartOffset(),sentStartOffset+arg2.getEndOffset());
					
					String arg1Link = null;
					String arg2Link = null;
					String arg1BestMention = null;
					String arg2BestMention = null;
					//this docName is ok for English with CJ, null for Chinese
					//String docName = doc.get(SentDocName.class);
					//System.out.println("docName: " + docName);					
					//Set this docID when running Stanford only, no CJ
					String docName = doc.get(DocIDAnnotation.class);
                    //System.out.println("docName: " + docName);
					Integer sentNum = s.get(SentGlobalID.class);
					//sentNum is null
					//System.out.println("sent num: " + sentNum);
					Integer arg1BestMentionSentNum = null;
					Integer arg2BestMentionSentNum = null;
					
					Extraction e = new Extraction(arg1,arg2,rel,score,
							arg1Link,arg2Link,arg1BestMention,arg2BestMention,
							docName,sentNum,arg1BestMentionSentNum,arg2BestMentionSentNum,
							senText);
					extractions.add(e);
				}
			}
	      	      	    
	      }  
		}		
		
		return extractions;
	}
	
}
