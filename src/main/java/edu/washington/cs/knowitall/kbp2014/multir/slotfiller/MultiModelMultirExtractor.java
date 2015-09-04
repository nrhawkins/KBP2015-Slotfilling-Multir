package edu.washington.cs.knowitall.kbp2014.multir.slotfiller;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
//import java.util.TreeMap;
import java.util.Comparator;
import java.util.Collections;

import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.Pair;
import edu.stanford.nlp.util.Triple;
import edu.washington.multir.sententialextraction.DocumentExtractor;
import edu.washington.multirframework.argumentidentification.ArgumentIdentification;
import edu.washington.multirframework.argumentidentification.SententialInstanceGeneration;
import edu.washington.multirframework.corpus.CorpusInformationSpecification.SentDocNameInformation.SentDocName;
import edu.washington.multirframework.corpus.CorpusInformationSpecification.SentGlobalIDInformation.SentGlobalID;
import edu.washington.multirframework.corpus.SentOffsetInformation.SentStartOffset;
import edu.washington.multirframework.data.Argument;
import edu.washington.multirframework.featuregeneration.FeatureGenerator;
import edu.washington.multirframework.multiralgorithm.Mappings;

public abstract class MultiModelMultirExtractor {

	protected List<SententialInstanceGeneration> sigs;
	protected List<String> modelFilePaths;
	protected ArgumentIdentification ai;
	protected FeatureGenerator fg;
	
	public MultiModelMultirExtractor(){
		sigs = new ArrayList<>();
		modelFilePaths = new ArrayList<>();
		ai = null;
		fg = null;
	}
	
	//public List<Extraction> extract(Annotation doc, String queryType) throws IOException{
	public List<Extraction> extract(Annotation doc, KBPQuery q) throws IOException{
		List<Extraction> extractions = new ArrayList<>();

		//List<Pair<SententialInstanceGeneration,DocumentExtractor>> sigModelPairs = getSigModelPairs(queryType);
		List<Pair<SententialInstanceGeneration,DocumentExtractor>> sigModelPairs = getSigModelPairs(q);
		List<CoreMap> sentences = doc.get(CoreAnnotations.SentencesAnnotation.class);
		for(CoreMap s : sentences){
			List<Argument> arguments = ai.identifyArguments(doc,s);
			for(Pair<SententialInstanceGeneration,DocumentExtractor> sigModelPair : sigModelPairs){
				DocumentExtractor de = sigModelPair.second;
				SententialInstanceGeneration sig = sigModelPair.first;
				List<Pair<Argument,Argument>> sententialPairs = sig.generateSententialInstances(arguments, s);
				for(Pair<Argument,Argument> sententialPair : sententialPairs){
					Triple<String,Double,Double> result = de.extractFromSententialInstance(sententialPair.first, sententialPair.second, s, doc);
					String rel = result.first;
					double score = result.third;
					if(!rel.equals("NA")){
						//add new extraction
						Argument arg1 = sententialPair.first;
						Argument arg2 = sententialPair.second;
						arg1 = new Argument(arg1.getArgName(),s.get(SentStartOffset.class)+arg1.getStartOffset(),s.get(SentStartOffset.class)+arg1.getEndOffset());
						arg2 = new Argument(arg2.getArgName(),s.get(SentStartOffset.class)+arg2.getStartOffset(),s.get(SentStartOffset.class)+arg2.getEndOffset());
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
		}
		System.out.println("MMME EXTRACT: " + extractions.size());
		
		return extractions;
	}
	
	public List<Extraction> extract2(Annotation doc, String queryType) throws IOException{
	//public List<Extraction> extract(Annotation doc, KBPQuery q) throws IOException{
		List<Extraction> extractions = new ArrayList<>();

		List<Pair<SententialInstanceGeneration,DocumentExtractor>> sigModelPairs = getSigModelPairs2(queryType);
		//List<Pair<SententialInstanceGeneration,DocumentExtractor>> sigModelPairs = getSigModelPairs(q);
		List<CoreMap> sentences = doc.get(CoreAnnotations.SentencesAnnotation.class);
		for(CoreMap s : sentences){
			List<Argument> arguments = ai.identifyArguments(doc,s);
			for(Pair<SententialInstanceGeneration,DocumentExtractor> sigModelPair : sigModelPairs){
				DocumentExtractor de = sigModelPair.second;
				SententialInstanceGeneration sig = sigModelPair.first;
				List<Pair<Argument,Argument>> sententialPairs = sig.generateSententialInstances(arguments, s);
				for(Pair<Argument,Argument> sententialPair : sententialPairs){
					Triple<String,Double,Double> result = de.extractFromSententialInstance(sententialPair.first, sententialPair.second, s, doc);
					String rel = result.first;
					double score = result.third;
					if(!rel.equals("NA")){
						//add new extraction
						Argument arg1 = sententialPair.first;
						Argument arg2 = sententialPair.second;
						arg1 = new Argument(arg1.getArgName(),s.get(SentStartOffset.class)+arg1.getStartOffset(),s.get(SentStartOffset.class)+arg1.getEndOffset());
						arg2 = new Argument(arg2.getArgName(),s.get(SentStartOffset.class)+arg2.getStartOffset(),s.get(SentStartOffset.class)+arg2.getEndOffset());
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
		}
		System.out.println("MMME EXTRACT: " + extractions.size());
		
		return extractions;
	}
	
	//private List<Pair<SententialInstanceGeneration,DocumentExtractor>> getSigModelPairs(String queryType) throws IOException{
	private List<Pair<SententialInstanceGeneration,DocumentExtractor>> getSigModelPairs(KBPQuery q) throws IOException{
		List<Pair<SententialInstanceGeneration,DocumentExtractor>> sigModelPairs = new ArrayList<>();
		
		for(int i =0; i < sigs.size(); i++){
			String modelFilePath = modelFilePaths.get(i);
			BufferedReader br = new BufferedReader(new FileReader(new File(modelFilePath+"/mapping")));
			String firstLine = br.readLine();
			Integer numRels = Integer.parseInt(firstLine.trim());
			for(int j =0; j < numRels; j++){
				String nextRel = br.readLine().trim();
				//if(nextRel.contains(queryType.toLowerCase())){
				if(nextRel.contains(q.entityType().toString().toLowerCase())){
					DocumentExtractor de = new DocumentExtractor(modelFilePath,fg,ai,sigs.get(i));
					Pair<SententialInstanceGeneration,DocumentExtractor> newPair = new Pair<>(sigs.get(i),de);
					sigModelPairs.add(newPair);
					break;
				}
			}
			br.close();
		}
		return sigModelPairs;
	}
	
	private List<Pair<SententialInstanceGeneration,DocumentExtractor>> getSigModelPairs2(String queryType) throws IOException{
    //private List<Pair<SententialInstanceGeneration,DocumentExtractor>> getSigModelPairs(KBPQuery q) throws IOException{
			List<Pair<SententialInstanceGeneration,DocumentExtractor>> sigModelPairs = new ArrayList<>();
			
			for(int i =0; i < sigs.size(); i++){
				String modelFilePath = modelFilePaths.get(i);
				BufferedReader br = new BufferedReader(new FileReader(new File(modelFilePath+"/mapping")));
				String firstLine = br.readLine();
				Integer numRels = Integer.parseInt(firstLine.trim());
				for(int j =0; j < numRels; j++){
					String nextRel = br.readLine().trim();
					if(nextRel.contains(queryType.toLowerCase())){
					//if(nextRel.contains(q.entityType().toString().toLowerCase())){
						DocumentExtractor de = new DocumentExtractor(modelFilePath,fg,ai,sigs.get(i));
						Pair<SententialInstanceGeneration,DocumentExtractor> newPair = new Pair<>(sigs.get(i),de);
						sigModelPairs.add(newPair);
						break;
					}
				}
				br.close();
			}
			return sigModelPairs;
		}
	
	
	private List<Pair<SententialInstanceGeneration,DocumentExtractor>> getSigModelPairsFromPrintTrace(KBPQuery q, PrintStream outputStreamTrace) throws IOException{
		List<Pair<SententialInstanceGeneration,DocumentExtractor>> sigModelPairs = new ArrayList<>();
		
		//outputStreamTrace.println("Models:");
		for(int i =0; i < sigs.size(); i++){
			String modelFilePath = modelFilePaths.get(i);

			//outputStreamTrace.println(modelFilePath);	
			
			BufferedReader br = new BufferedReader(new FileReader(new File(modelFilePath+"/mapping")));
			
			String firstLine = br.readLine();
			
			Integer numRels = Integer.parseInt(firstLine.trim());
			
			for(int j =0; j < numRels; j++){
				String nextRel = br.readLine().trim();
				//outputStreamTrace.println(nextRel);	
				
				//if(nextRel.contains(queryType.toLowerCase())){
				if(nextRel.contains(q.entityType().toString().toLowerCase())){
					
					//outputStreamTrace.println(modelFilePath);	
					
					DocumentExtractor de = new DocumentExtractor(modelFilePath,fg,ai,sigs.get(i));
					Pair<SententialInstanceGeneration,DocumentExtractor> newPair = new Pair<>(sigs.get(i),de);
					
					//outputStreamTrace.println("Adding sigModel Pair:" + sigs.get(i));
					
					sigModelPairs.add(newPair);
					break;
				}
			}
			br.close();
		}
        		
		outputStreamTrace.println();
		return sigModelPairs;
	}
	
	public void printTrace(Annotation doc, KBPQuery q, PrintStream outputStreamTrace) throws IOException{
	
		//List<Extraction> extractions = new ArrayList<>();

		outputStreamTrace.println("PRINTING TRACE FOR NEXT DOC: ");
		
		List<Pair<SententialInstanceGeneration,DocumentExtractor>> sigModelPairs = getSigModelPairsFromPrintTrace(q, outputStreamTrace);
		List<CoreMap> sentences = doc.get(CoreAnnotations.SentencesAnnotation.class);

		for(CoreMap s : sentences){
			List<Argument> arguments = ai.identifyArguments(doc,s);

			// Print Sentence			
			String sentenceText = s.get(CoreAnnotations.TextAnnotation.class);
			outputStreamTrace.println("Sentence: " + "\n");
			sentenceText = sentenceText.replace("\n", " ");
			sentenceText = sentenceText.replace("\r", "");
			outputStreamTrace.println(sentenceText + "\n");
            
			//Look for Third (ORDINAL) Guard (MISC) Division (MISC), founded in, 1941 (DATE) 
            //Look for David Wivell, reporter
			List<CoreLabel> tokens = s.get(CoreAnnotations.TokensAnnotation.class);
            for(CoreLabel token : tokens){showTokenType(token, outputStreamTrace);}
			
			// Print arguments identified in each sentence
			outputStreamTrace.println("Arguments:" + "\n");			
			for (Argument arg : arguments) {outputStreamTrace.println(arg.getArgName());}
			outputStreamTrace.println();			
			
			for(Pair<SententialInstanceGeneration,DocumentExtractor> sigModelPair : sigModelPairs){
				//DocumentExtractor de = sigModelPair.second;
				SententialInstanceGeneration sig = sigModelPair.first;
				List<Pair<Argument,Argument>> sententialPairs = sig.generateSententialInstances(arguments, s);

				//Print pairs of arguments
				outputStreamTrace.println("Argument Pairs from Model:" + "\n");
				for(Pair<Argument,Argument> sententialPair : sententialPairs){
					outputStreamTrace.println(sententialPair.first + ", " + sententialPair.second);
				}
				outputStreamTrace.println();
			}
		}
	
	}

	private void showTokenType(CoreLabel token, PrintStream outputStreamTrace) {
		String ner = token.get(CoreAnnotations.NamedEntityTagAnnotation.class);
			
		outputStreamTrace.println("TOKEN TYPE: " + token.value() +"\t" + ner);
	    //System.out.println("TOKEN TYPE: " + token.value() +"\t" + ner);
		//return false;
	}
	
	class Feature{
		  Double weight;
		  String name;
	
		  public Feature(Double featureWeight, String featureName) {
			    weight = featureWeight;
			    name = featureName;
			}
		  
		  public Double getWeight() {
			  return weight;
		  }
		  public String getName() {
			  return name;
		  }
		  
    }

	class MyComparator implements Comparator<Feature>{
      public int compare(Feature feature1, Feature feature2){
    	  Double wtDiff = Math.abs(feature1.getWeight()) - Math.abs(feature2.getWeight()); 
          int wtDiffPosNeg = (int)Math.floor(wtDiff) + (int)Math.ceil(wtDiff); 
    	  return wtDiffPosNeg;
	  }
    }
	
	public void printFeaturesTrace(Annotation doc, KBPQuery q, PrintStream outputStreamTrace) throws IOException{

		List<Pair<SententialInstanceGeneration,DocumentExtractor>> sigModelPairs = getSigModelPairs(q);
		List<CoreMap> sentences = doc.get(CoreAnnotations.SentencesAnnotation.class);

		for(CoreMap s : sentences){
			List<Argument> arguments = ai.identifyArguments(doc,s);

			// Print Sentence			
			String sentenceText = s.get(CoreAnnotations.TextAnnotation.class);
			sentenceText = sentenceText.replace("\n", " ");
			sentenceText = sentenceText.replace("\r", "");
			
			// Print arguments identified in each sentence		
			//for (Argument arg : arguments) {outputStreamTrace.println(arg.getArgName());}	
			
			for(Pair<SententialInstanceGeneration,DocumentExtractor> sigModelPair : sigModelPairs){
				DocumentExtractor de = sigModelPair.second;
				SententialInstanceGeneration sig = sigModelPair.first;
				List<Pair<Argument,Argument>> sententialPairs = sig.generateSententialInstances(arguments, s);

				//Print pairs of arguments
				for(Pair<Argument,Argument> sententialPair : sententialPairs){
					
					Argument arg1 = sententialPair.first;
					Argument arg2 = sententialPair.second;
					//result Triple<>(relation, conf, parse.score)
					Triple<String,Double,Double> result = de.extractFromSententialInstance(arg1, arg2, s, doc);
					String rel = result.first;
					double score = result.third;
					
					if(!rel.equals("NA")){
						
					  // get features and weights	
						
					  String arg1ID = null;
				      String arg2ID = null;
						
					  List<String> features = 
								fg.generateFeatures(arg1.getStartOffset(), arg1.getEndOffset(), 
										arg2.getStartOffset(), arg2.getEndOffset(), 
										arg1ID,arg2ID, s, doc);	
					  
					  Mappings mapping = de.getMapping();
					  Map<Integer,Double> featureMap = de.extractFromSententialInstanceWithFeatureScores(arg1, arg2, s, doc).second;	
					  //Map<Integer,Map<Integer,Double>> featureMap = de.extractFromSententialInstanceWithAllFeatureScores(sententialPair.first, sententialPair.second, s, doc).second;
					  //Map<Integer,Map<Integer,Double>> featureMap = de.extractFromSententialInstanceWithAllFeatureScoresIgnoreNA(sententialPair.first, sententialPair.second, s, doc).second;
					  
					  
					  // print to trace file
				      outputStreamTrace.print(q.name() + "\t" + q.id() + "\t" + arg1.getArgName() + "\t" + arg2.getArgName() + "\t"
								+ rel + "\t" + score + "\t" + sentenceText + "\t");					

				      //for (String feature : features) {
				    //	 int featureID = mapping.getFeatureID(feature, false); 
				    //     if(featureID >= 0){
				    //       outputStreamTrace.println(featureID + "\t" + feature + "\t" + featureMap.get(featureID) + "\t"); 
				    //     }				    	  
				    //  }
				      
				      // Make FeatureID to Feature Map
				      HashMap<Integer,String> ftid2ft = new HashMap<Integer,String>();
				      for (String feature : features) {
				        int featureID = mapping.getFeatureID(feature, false); 
				        ftid2ft.put(featureID, feature);
				      }
				      
				      // Make TreeMap, the keys are sorted, key = feature weight, value = feature string
				      //TreeMap<Double,String> sortedFtWts = new TreeMap<Double,String>();
				      //for (Integer featureID : featureMap.keySet()) {sortedFtWts.put(featureMap.get(featureID),ftid2ft.get(featureID));}

				      //Print Features Sorted by Feature Weight
				      //for (Double key : .keySet()) {
				      
					  List<Feature> featuresWts = new ArrayList<Feature>();
					  
					  for (Integer featureID : featureMap.keySet()) {						
					    featuresWts.add(new Feature(featureMap.get(featureID), ftid2ft.get(featureID)));					  
			          }

					  Collections.sort(featuresWts, new MyComparator());
					  Collections.reverse(featuresWts);
				      
					  for(int i = 0; i < featuresWts.size(); i++) {
						  outputStreamTrace.print(featuresWts.get(i).getName() + "\t" + featuresWts.get(i).getWeight() + "\t");
					  } 					  
					  outputStreamTrace.println();
				      
				      //
				      // Make Reverse Map
				      //
				      //HashMap<String, Character> reversedHashMap = new HashMap<String, Character>();
				      //for (String key : myHashMap.keySet()){
				      //    reversedHashMap.add(myHashMap.get(key), key);
				      //}
				      //outputStreamTrace.println();
				      //for (Integer featureId : featureMap.keySet()) {outputStreamTrace.println(featureId + "\t" + featureMap.get(featureId) + "\t");}
					  //for (String feature : features) {outputStreamTrace.println(feature + "\t");}
					  //for (Integer featureId : featureMap.keySet()) {outputStreamTrace.println(featureId + "\t" + featureMap.get(featureId) + "\t");}
					  //for (Integer key : featureMap.keySet()) {
					  //  for(Integer key2: featureMap.get(key).keySet()){outputStreamTrace.print("key: " + featureMap.get(key).get(key2) + "\t");}
					  //  outputStreamTrace.print("\t");
					  //}
					  
					  //outputStreamTrace.print(features.size() + "\t" + featureMap.size());
					  //outputStreamTrace.println();
					}
				   
				}
				//outputStreamTrace.println();
			}
		}
	
	}
	
	
}
