package edu.washington.cs.knowitall.kbp2014.multir.slotfiller;

//import java.util.ArrayList;
//import java.util.List;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.Pair;
import edu.stanford.nlp.util.Triple;
import edu.washington.multir.sententialextraction.DocumentExtractor;
import edu.washington.multirframework.corpus.CorpusInformationSpecification.SentDocNameInformation.SentDocName;
import edu.washington.multirframework.corpus.CorpusInformationSpecification.SentGlobalIDInformation.SentGlobalID;
import edu.washington.multirframework.corpus.SentOffsetInformation.SentStartOffset;
import edu.washington.multirframework.data.Argument;
import edu.washington.multirframework.featuregeneration.DefaultFeatureGeneratorStanford;
import edu.washington.multirframework.featuregeneration.FeatureGenerator;
import edu.washington.multirframework.argumentidentification.*;

public class SingleModelMultirExtractorVersion1Chinese {  //extends MultiModelMultirExtractor {

	protected SententialInstanceGeneration sig;
	protected String modelFilePath;
	protected ArgumentIdentification ai;
	protected FeatureGenerator fg;
	
	public SingleModelMultirExtractorVersion1Chinese(){

		//sig: edu.washington.multirframework.argumentidentification.DefaultSententialInstanceGeneration
		//ai: edu.washington.multirframework.argumentidentification.ChineseNERArgumentIdentification
		//fg: edu.washington.multirframework.featuregeneration.DefaultFeatureGeneratorStanford 
		
		sig = DefaultSententialInstanceGeneration.getInstance();
		modelFilePath = "/projects/WebWare5/multir-multilingual/model_zh";	
		ai = ChineseNERArgumentIdentification.getInstance();
		//fg = new DefaultFeatureGeneratorStanford();
	    fg = DefaultFeatureGeneratorStanford.getInstance();
		
	}
		
	public List<Extraction> extract(Annotation doc, KBPQuery q) throws IOException{
		
		List<Extraction> extractions = new ArrayList<>();
				
		List<CoreMap> sentences = doc.get(CoreAnnotations.SentencesAnnotation.class);
				
		DocumentExtractor de = new DocumentExtractor(modelFilePath, fg, ai, sig);
		
	    for(CoreMap s : sentences){
	    	
	      System.out.println("Sentence: " + s); 	
			
	      List<Argument> arguments = ai.identifyArguments(doc, s);	

	      for(Argument a : arguments){
	         System.out.println("Arguments: " + a.getArgName());
	      }   
	      
	      List<Pair<Argument,Argument>> sententialPairs = sig.generateSententialInstances(arguments, s);
	      
	      for(Pair<Argument,Argument> sententialPair : sententialPairs){
				Triple<String,Double,Double> result = de.extractFromSententialInstance(sententialPair.first, sententialPair.second, s, doc);
				String rel = result.first;				
				double score = result.third;
				
				System.out.println("Relation: " + rel + " Score: " + score + " " + sententialPair.first + " " + sententialPair.second);
				
				if(!rel.equals("NA")){
					//add new extraction
					Argument arg1 = sententialPair.first;
					Argument arg2 = sententialPair.second;
					
					System.out.println("arg1: " + arg1.getArgName());
					System.out.println("arg1 so: " + arg1.getStartOffset());
					System.out.println("arg1 eo: " + arg1.getEndOffset());
					
					//SentStartOffset is null
                    System.out.println("sent so: " + s.get(SentStartOffset.class));					

					//arg1 = new Argument(arg1.getArgName(),s.get(SentStartOffset.class)+arg1.getStartOffset(),s.get(SentStartOffset.class)+arg1.getEndOffset());
					//arg2 = new Argument(arg2.getArgName(),s.get(SentStartOffset.class)+arg2.getStartOffset(),s.get(SentStartOffset.class)+arg2.getEndOffset());
					
					String arg1Link = null;
					String arg2Link = null;
					String arg1BestMention = null;
					String arg2BestMention = null;
					String docName = doc.get(SentDocName.class);
					Integer sentNum = s.get(SentGlobalID.class);
					Integer arg1BestMentionSentNum = null;
					Integer arg2BestMentionSentNum = null;
					
					Extraction e = new Extraction(arg1,arg2,rel,score,
							arg1Link,arg2Link,arg1BestMention,arg2BestMention,
							docName,sentNum,arg1BestMentionSentNum,arg2BestMentionSentNum);
					extractions.add(e);
				}
			}
	      	      	    
	      
		}		
		
		return extractions;
	}
	
}
