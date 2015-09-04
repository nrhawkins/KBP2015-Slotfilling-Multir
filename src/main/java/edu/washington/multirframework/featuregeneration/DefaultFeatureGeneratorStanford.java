package edu.washington.multirframework.featuregeneration;

//import java.io.BufferedWriter;
//import java.io.File;
//import java.io.FileOutputStream;
//import java.io.OutputStream;
//import java.io.OutputStreamWriter;
//import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.Triple;
import edu.stanford.nlp.semgraph.SemanticGraph;
import edu.stanford.nlp.semgraph.SemanticGraphEdge;
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.*;
import edu.stanford.nlp.ling.IndexedWord;
//import edu.washington.multirframework.annotation.MultiRInstance;
//import edu.washington.multirframework.annotation.MultiRReader;
import edu.washington.multirframework.util.NoDependencyPathException;
//import edu.washington.multirframework.annotation.IndexedWord;
//import edu.washington.multirframework.annotation.PartOfSpeechAnnotation;
import edu.washington.multirframework.argumentidentification.ChineseNERArgumentIdentification;
//import edu.washington.multirframework.corpus.SentDependencyInformation.DependencyAnnotation;
//import edu.washington.multirframework.corpus.TokenOffsetInformation.SentenceRelativeCharacterOffsetBeginAnnotation;
//import edu.washington.multirframework.corpus.TokenOffsetInformation.SentenceRelativeCharacterOffsetEndAnnotation;


/**
 * Default implementation of FeatureGenerator
 * inspired by the Mintz features, but slightly
 * different.
 * @author jgilme1
 * @modified by Victoria Lin
 */
public class DefaultFeatureGeneratorStanford implements FeatureGenerator {
	
	private static DefaultFeatureGeneratorStanford instance = null;
	
	public static DefaultFeatureGeneratorStanford getInstance(){
		if(instance == null) instance = new DefaultFeatureGeneratorStanford();
		return instance;
		}
	
	public static void main(String[] args) throws Exception {
		/* 
		 * read multiR annotation files
		 */
		/*File inputDir = new File(args[0]);
		MultiRReader multirReader = new MultiRReader();
		ArrayList<MultiRInstance> trainInsts = multirReader.read(inputDir);
		*/
		
		/*
		 * compute and write features
		 */
		
		/*DefaultFeatureGenerator feagen = new DefaultFeatureGenerator();
		File outputDir = new File(args[1]);
		BufferedWriter featureFile = new BufferedWriter(
				new OutputStreamWriter(
						new FileOutputStream(
								new File(outputDir, "training.feature")), "UTF-8"));
		
		for (MultiRInstance trainInst : trainInsts) {
			StringBuilder featuresBuilder = new StringBuilder();
			featuresBuilder.append(trainInst.getSentenceID());
			// featuresBuilder.append("\t");
			
			//Add arg1link and arg2link
			featuresBuilder.append(trainInst.getEntity1());
			featuresBuilder.append("\t");
			featuresBuilder.append(trainInst.getEntity2());
			featuresBuilder.append("\t");
			
			//Add relation
			featuresBuilder.append(trainInst.getRelation());
			//featuresBuilder.append("\t");
			
			List<String> featureList = feagen.generateFeatures(trainInst.getTokens(), 
					trainInst.getPOS(), 
					trainInst.getDepRoles(), 
					trainInst.getDepStarts(), 
					trainInst.getDepEnds(), 
					trainInst.getTokenStarts(), 
					trainInst.getTokenEnds(), 
					trainInst.getNER(), 
					trainInst.getEntity1(), 
					trainInst.getE1StartOffset(), 
					trainInst.getE1EndOffset(), 
					trainInst.getEntity2(), 
					trainInst.getE2StartOffset(), 
					trainInst.getE2EndOffset(), 
					trainInst.getRelation());
		
			if (featureList == null) {
				System.err.println("Warning: null feature list "
						+ trainInst.getSentenceID()
						+ trainInst.getSentenceText() + "\t"
						+ trainInst.getEntity1() + "\t"
						+ trainInst.getEntity2());
			} else if (featureList.get(featureList.size() - 1).equals(
				"no_dependency_path"))	{
				System.err.println("Warning: no dependency path: "
                        + trainInst.getSentenceID()
                        + trainInst.getSentenceText() + "\t"
                        + trainInst.getEntity1() + "\t"
                        + trainInst.getEntity2());
			}

			for (String feature : featureList) {
				featuresBuilder.append("\t");
				featuresBuilder.append(feature);
			}
			
			featureFile.write(featuresBuilder.toString());
			featureFile.write(System.getProperty("line.separator"));
		}
		
		featureFile.close();
		*/
	}

	public List<String> generateFeatures(String[] tokens, String[] pos, 
			String[] depRoles, String[] depStarts, String[] depEnds, 
			String[] tokenStarts, String[] tokenEnds, String[] ner,
            String entity1, int e1StartOffset, int e1EndOffset,
            String entity2, int e2StartOffset, int e2EndOffset, String relation )
            throws NoDependencyPathException {
		
		int NumOfTokens = tokens.length;
        if (NumOfTokens < 1){
        	System.out.println("Warning: zero length sentence encountered"
        			+ "in feature generation.");
            return null;
        }
        if (pos.length != NumOfTokens) {
        	System.out.println("Error: pos and token length mismatch.");
        	System.exit(1);
        }
        if (ner.length != NumOfTokens) {
        	System.out.println("Error: ner and token length mismatch.");
        	System.exit(1);
        }
        /* if (depRoles.length != NumOfTokens) {
        	System.out.println("Error: depRoles and token length mismatch.");
        	System.exit(1);
        }
        if (depStarts.length != NumOfTokens) {
        	System.out.println("Error: depStarts and token length mismatch.");
        	System.exit(1);
        }
        if (tokenStarts.length != NumOfTokens) {
        	System.out.println("Error: tokenStarts and token length mismatch.");
        	System.exit(1);
        } */
        if (tokenEnds.length != NumOfTokens) {
        	System.out.println("Error: tokenEnds and token length mismatch.");
        	System.exit(1);
        }

        /*System.out.println("GF: tokens: " + tokens.length + " " + tokens[0]);
        System.out.println("GF: pos: " + pos.length + " " + pos[0]);
        System.out.println("GF: depRoles: " + depRoles.length + " " + depRoles[0]);
        System.out.println("GF: depStarts: " + depStarts.length + " " + depStarts[0]);
        System.out.println("GF: depEnds: " + depEnds.length + " " + depEnds[0]);
        System.out.println("GF: tokenStarts: " + tokenStarts.length + " " + tokenStarts[0]);
        System.out.println("GF: tokenEnds: " + tokenEnds.length + " " + tokenEnds[0]);
        System.out.println("GF: ner: " + ner.length + " " + ner[0]);
        System.out.println("GF: e1: " + entity1 + " " + e1StartOffset + " " + e1EndOffset);
        System.out.println("GF: e2: " + entity2 + " " + e2StartOffset + " " + e2EndOffset);
        System.out.println("GF: rel: " + relation); */
        
        //------------------------------------------------------------------------------
        //Create inputs for the function, originalMultirFeatures(), 
        //which computes the features for a triple, i.e. an entity pair and a relation,
        //------------------------------------------------------------------------------

        //initialize dependency parents to -1
        int[] depChildren = new int[NumOfTokens];
        for(int i = 0; i < depChildren.length; i ++){
           depChildren[i] = -1;
        }
        String[] depTypes = new String[NumOfTokens];
        
        String arg1ner = "";
        String arg2ner = "";
        int[] arg1Pos = new int[2];
        int[] arg2Pos = new int[2];

        //---------------------------------------------------------------------------------
        //set arg1ner, arg2ner, arg1Pos, arg2Pos
        //by iterating over tokens
        //Pos in arg1Pos and arg2Pos stands for position in token array
        //---------------------------------------------------------------------------------
        for(int i = 0; i < tokens.length; i++){

            int begOffset = Integer.parseInt(tokenStarts[i]);
            int endOffset = Integer.parseInt(tokenEnds[i]);

            // if the token matches the argument set the ner and argPos values
            if(begOffset == e1StartOffset){
               String nerType = ner[i];
               if(nerType != null){
                  arg1ner = nerType;
               }
               arg1Pos[0] = i;
            }
  
            if(endOffset == e1EndOffset){
               arg1Pos[1] = i;
            }

            if(begOffset == e2StartOffset){
               String nerType = ner[i];
               if(nerType != null){
                   arg2ner = nerType;
               }
               arg2Pos[0] = i;
            }

            if(endOffset == e2EndOffset){
                arg2Pos[1] = i;
            }
        	
        }

		// debugging
		/* System.out.println(Arrays.toString(arg1Pos));
		System.out.println(Arrays.toString(arg2Pos));
        */

        if(depRoles.length > 0){
			for(int i = 0; i < depRoles.length; i++){
			   int parent = Integer.parseInt(depStarts[i]) - 1;
			   String type = depRoles[i];
			   int child = Integer.parseInt(depEnds[i]) - 1;
			
			   //child and parent should not be equivalent
			   if(parent==child){
			      child = -1;
			   }
			
			   if(parent < tokens.length) {
				  if (parent >= 0) {
			      	 depChildren[parent] = child;
			      	 depTypes[parent] = type;
				  }
			   }
			   else{
			      System.err.println("ERROR BETWEEN DEPENDENCY PARSE AND TOKEN SIZE");
			          return new ArrayList<String> ();
			   }
			}
        }
        else {
        	System.err.println("Warning: sentence with no dependency structure encountered"
        			+ "in feature generation.");
        }
         
        // add 1 to end Pos values
        arg1Pos[1] += 1;
        arg2Pos[1] += 1;
         

		//-----------------------------------------------------
		//Compute origMultirFeatures
		//-----------------------------------------------------
        List<String> featureList = originalMultirFeatures(tokens,
                     pos, depChildren, depTypes, arg1Pos, arg2Pos, arg1ner, arg2ner);
		
		return featureList;
	}
	
	/** 
	 * Modified based on RelationECML getFeatures algorithm...
	 * @param tokens
	 * @param postags
	 * @param depChildren
	 * @param depTypes
	 * @param arg1Pos
	 * @param arg2Pos
	 * @param arg1ner
	 * @param arg2ner
	 * @return
	 */
	public static  List<String> originalMultirFeatures( String[] tokens, 
			String[] postags,
			int[] depChildren, String[] depTypes,
			int[] arg1Pos, int[] arg2Pos, String arg1ner, String arg2ner) {

		List<String> features = new ArrayList<String>();

		// it's easier to deal with first, second
		int[] first = arg1Pos, second = arg2Pos;
		String firstNer = arg1ner, secondNer = arg2ner;
		if (arg1Pos[0] > arg2Pos[0]) {
			second = arg1Pos; first = arg2Pos;
			firstNer = arg2ner; secondNer = arg1ner;
		}
		
		// define the inverse prefix
		String inv = (arg1Pos[0] > arg2Pos[0])? 
				"inverse_true" : "inverse_false";
		
		// define the middle parts
		StringBuilder middleTokens = new StringBuilder();
		for (int i=first[1]; i < second[0]; i++) {
			if (i > first[1]) {
				middleTokens.append(" ");
			}
			middleTokens.append(tokens[i]);
		}
		
		if (second[0] - first[1] > 10) {
			middleTokens.setLength(0);
			middleTokens.append("*LONG*");
		}
		
		// define the prefixes and suffixes
		String[] prefixTokens = new String[2];
		String[] suffixTokens = new String[2];
		
		for (int i=0; i < 2; i++) {
			int tokIndex = first[0] - i - 1;
			if (tokIndex < 0) prefixTokens[i] = "B_" + tokIndex;
			else prefixTokens[i] = tokens[tokIndex];
		}

		for (int i=0; i < 2; i++) {
			int tokIndex = second[1] + i;
			if (tokIndex >= tokens.length) suffixTokens[i] = "B_" + (tokIndex - tokens.length + 1);
			else suffixTokens[i] = tokens[tokIndex];
		}

		String[] prefixes = new String[3];
		String[] suffixes = new String[3];

		prefixes[0] = suffixes[0] = "";
		prefixes[1] = prefixTokens[0];
		prefixes[2] = prefixTokens[1] + " " + prefixTokens[0];
		suffixes[1] = suffixTokens[0];
		suffixes[2] = suffixTokens[0] + " " + suffixTokens[1];
		
		// generate the features in the same order as in ecml data
		String mto = middleTokens.toString();
		
		features.add(inv + "|" + firstNer + "|" + mto + "|" + secondNer);
		features.add(inv + "|" + prefixes[1] + "|" + firstNer + "|" + mto + "|" + secondNer + "|" + suffixes[1]);
		features.add(inv + "|" + prefixes[2] + "|" + firstNer + "|" + mto + "|" + secondNer + "|" + suffixes[2]);
		
		// dependency features
		if (depChildren == null || depChildren.length < tokens.length) {
			System.err.println("Warning: no dependency annotation.");
			return features;
		}

		// identify head words of arg1 and arg2
		// (start at end, while inside entity, jump)
		int head1 = arg1Pos[1]-1;
		int loopIterationCount =0;
		while (depChildren[head1] >= arg1Pos[0] && depChildren[head1] < arg1Pos[1]) {
			  head1 = depChildren[head1];
			  //avoid infinite loop
			  if(loopIterationCount == 100){
				  break;
			  }
			  loopIterationCount++;
		}
		int head2 = arg2Pos[1]-1;
		//System.out.println(head1 + " " + head2);
		loopIterationCount =0;
		while (depChildren[head2] >= arg2Pos[0] && depChildren[head2] < arg2Pos[1]) {
			head2 = depChildren[head2];
			//avoid infinite loop
			  if(loopIterationCount == 100){
				  break;
			  }
			  loopIterationCount++;
		}

		
		
		// find path of dependencies from first to second
		int[] path1 = new int[tokens.length];
		for (int i=0; i < path1.length; i++) path1[i] = -1;
		path1[0] = head1; // last token of first argument
		for (int i=1; i < path1.length; i++) {
			path1[i] = depChildren[path1[i-1]];
			if (path1[i] == -1) break;
		}	
		int[] path2 = new int[tokens.length];
		for (int i=0; i < path2.length; i++) path2[i] = -1;
		path2[0] = head2; // last token of second argument
		for (int i=1; i < path2.length; i++) {
			path2[i] = depChildren[path2[i-1]];
			if (path2[i] == -1) break;
		}
		int lca = -1;
		int lcaUp = 0, lcaDown = 0;
		outer:
		for (int i=0; i < path1.length; i++)
			for (int j=0; j < path2.length; j++) {
				if (path1[i] == -1 || path2[j] == -1) {
					break; // no path
				}
				if (path1[i] == path2[j]) {
					lca = path1[i];
					lcaUp = i;
					lcaDown = j;
					break outer;
				}
			}
		
		if (lca < 0) {
			System.err.println("Warning: no dependency path found between two entity heads.");
			features.add("no_dependency_path");
			return features;
			// System.exit(1); // no dependency path (shouldn't happen)
		}
		String[] dirs = new String[lcaUp + lcaDown];
		String[] strs = new String[lcaUp + lcaDown];
		String[] rels = new String[lcaUp + lcaDown];

		StringBuilder middleDirs = new StringBuilder();
		StringBuilder middleRels = new StringBuilder();
		StringBuilder middleStrs = new StringBuilder();

		if (lcaUp + lcaDown < 12) {
			
			/* for (int i=0; i < lcaUp; i++) {
				dirs[i] = "->";
				strs[i] = i > 0? tokens[path1[i]] : "";
				rels[i] = depTypes[path1[i]];
				//System.out.println("[" + depTypes[path1[i]] + "]->");
			} */
			for (int i=1; i < lcaUp+1; i++) {
                dirs[i-1] = "<-";
                strs[i-1] = (path1[i] != head2) ? tokens[path1[i]] : "";
                rels[i-1] = depTypes[path1[i-1]];
                //System.out.println("[" + depTypes[path1[i]] + "]->");
            }
			for (int j=0; j < lcaDown; j++) {
			//for (int j=lcaDown-1; j >= 0; j--) {
				// dirs[lcaUp + j] = "<-";
				dirs[lcaUp + j] = "->";
				strs[lcaUp + j] = (lcaDown-j-1 > 0 && path2[lcaDown-j-1] != head1) ? tokens[path2[lcaDown-j-1]] : ""; // word taken from above
				rels[lcaUp + j] = depTypes[path2[lcaDown-j-1]]; 
				/* strs[lcaUp + j] = (j > 0)? tokens[path2[j]] : ""; // word taken from above
				rels[lcaUp + j] = depTypes[path2[j]]; */
				//System.out.println("[" + depTypes[path2[j]] + "]<-");
			}
			
			for (int i=0; i < dirs.length; i++) {
				middleDirs.append(dirs[i]);
				/* middleRels.append("[" + rels[i] + "]" + dirs[i]);
				middleStrs.append(strs[i] + "[" + rels[i] + "]" + dirs[i]);*/
				middleRels.append(dirs[i] + "[" + rels[i] + "]");
				middleStrs.append(dirs[i] + strs[i] + "[" + rels[i] + "]");
			}
		}
		else {
				middleDirs.append("*LONG-PATH*");
				middleRels.append("*LONG-PATH*");
				middleStrs.append("*LONG-PATH*");
		}
	
		String basicDir = arg1ner + "|" + middleDirs.toString() + "|" + arg2ner;
		String basicDep = arg1ner + "|" + middleRels.toString() + "|" + arg2ner;
		String basicStr = arg1ner + "|" + middleStrs.toString() + "|" + arg2ner;
		
		
		List<String> arg1dirs = new ArrayList<String>();
		List<String> arg1deps = new ArrayList<String>();
		List<String> arg1strs = new ArrayList<String>();
		List<String> arg2dirs = new ArrayList<String>();
		List<String> arg2deps = new ArrayList<String>();
		List<String> arg2strs = new ArrayList<String>();
		
		// case 1: pointing into the argument pair structure (always attach to lhs):
		for (int i=0; i < tokens.length; i++) {
			// make sure itself is not either argument
			//if (i >= first[0] && i < first[1]) continue;
			//if (i >= second[0] && i < second[1]) continue;
			if (i == head1) continue;
			if (i == head2) continue;
			
			// make sure i is not on path
			boolean onPath = false;
			for (int j=0; j < lcaUp; j++) if (path1[j] == i) onPath = true;
			for (int j=0; j < lcaDown; j++) if (path2[j] == i) onPath = true;
			if (onPath) continue;
			// make sure i points to first or second arg
			//if (depChildren[i] >= first[0] && depChildren[i] < first[1]) lws.add(i);
			//if (depChildren[i] >= second[0] && depChildren[i] < second[1]) rws.add(i);
			if (depChildren[i] == head1) {
				//lws.add(i);
				/* arg1dirs.add("->");				
				arg1deps.add("[" + depTypes[i] + "]->");
				arg1strs.add(tokens[i] + "[" + depTypes[i] + "]->"); */
				arg1dirs.add("<-");              
				arg1deps.add("<-[" + depTypes[i] + "]");
				arg1strs.add("<-" + tokens[i] + "[" + depTypes[i] + "]"); 
			}
			if (depChildren[i] == head2) {
				//rws.add(i);			
				arg2dirs.add("->");				
				arg2deps.add("->[" + depTypes[i] + "]");
				arg2strs.add("->" + tokens[i] + "[" + depTypes[i] + "]"); 
				/* arg2dirs.add("<-");              
                arg2deps.add("<-[" + depTypes[i] + "]");
                arg2strs.add("<-" + tokens[i] + "[" + depTypes[i] + "]"); */
			}
		}
		
		
		
		// case 2: pointing out of argument
		// if (lcaUp == 0 && depChildren[head1] != -1 || depChildren[head1] == head2) {
		if (lcaUp == 0 && depChildren[head1] != -1) {
			/* arg1dirs.add("<-");				
			arg1deps.add("[" + depTypes[head1] + "]<-");
			arg1strs.add(tokens[depChildren[head1]] + "[" + depTypes[head1] + "]<-");
			
			if (depChildren[depChildren[head1]] != -1) {
				arg1dirs.add("<-");
				arg1deps.add("[" + depTypes[depChildren[head1]] + "]<-");
				arg1strs.add(tokens[depChildren[depChildren[head1]]] + "[" + depTypes[depChildren[head1]] + "]<-");
			} */
			arg1dirs.add("->");              
            arg1deps.add("->[" + depTypes[head1] + "]");
            arg1strs.add("->" + tokens[depChildren[head1]] + "[" + depTypes[head1] + "]");
            
            if (depChildren[depChildren[head1]] != -1) {
                arg1dirs.add("->");
                arg1deps.add("->[" + depTypes[depChildren[head1]] + "]");
                arg1strs.add("->" + tokens[depChildren[depChildren[head1]]] + "[" + depTypes[depChildren[head1]] + "]");
			} 
		}
		// if parent is not on path or if parent is 
		// if (lcaDown == 0 && depChildren[head2] != -1 || depChildren[head2] == head1) { // should this actually attach to rhs???
		if (lcaDown == 0 && depChildren[head2] != -1) {
			arg2dirs.add("<-");
			arg2deps.add("<-[" + depTypes[head2] + "]");
			arg2strs.add("<-" + tokens[depChildren[head2]] + "[" + depTypes[head2] + "]");
			
			if (depChildren[depChildren[head2]] != -1) {
				arg2dirs.add("<-");
				arg2deps.add("<-[" + depTypes[depChildren[head2]] + "]");
				arg2strs.add("<-" + tokens[depChildren[depChildren[head2]]] + "[" + depTypes[depChildren[head2]] + "]");
			} 
			/* arg1dirs.add("->");
            arg1deps.add("->[" + depTypes[head2] + "]");
            arg1strs.add("->" + tokens[head2] + "[" + depTypes[head2] + "]");
            
            if (depChildren[depChildren[head2]] != -1) {
                arg1dirs.add("->");
                arg1deps.add("->[" + depTypes[depChildren[head2]] + "]");
                arg1strs.add("->" + tokens[depChildren[head2]] + "[" + depTypes[depChildren[head2]] + "]");
            } */
		}
		
		
		
		//features.add("dir:" + basicDir);		
		//features.add("dep:" + basicDep);

		
		// left and right, including word
		for (String w1 : arg1strs)
			for (String w2 : arg2strs)
				features.add("str:" + w1 + "|" + basicStr + "|" + w2);
		
		/*
		for (int lw : lws) {
			for (int rw : rws) {
				features.add("str:" + tokens[lw] + "[" + depTypes[lw] + "]<-" + "|" + basicStr
						+ "|" + "[" + depTypes[rw] + "]->" + tokens[rw]);
			}
		}
		 */
	
	
		
		// only left
		for (int i=0; i < arg1dirs.size(); i++) {
			features.add("str:" + arg1strs.get(i) + "|" + basicStr);
			features.add("dep:" + arg1deps.get(i) + "|" + basicDep);
			features.add("dir:" + arg1dirs.get(i) + "|" + basicDir);
		}
		
		
		// only right
		for (int i=0; i < arg2dirs.size(); i++) {
			features.add("str:" + basicStr + "|" + arg2strs.get(i));
			features.add("dep:" + basicDep + "|" + arg2deps.get(i));
			features.add("dir:" + basicDir + "|" + arg2dirs.get(i));
		}

		features.add("str:" + basicStr);

		return features;
	}
	

	// @Override
		public List<String> generateFeatures(Integer arg1StartOffset,
				Integer arg1EndOffset, Integer arg2StartOffset,
				Integer arg2EndOffset, String arg1ID, String arg2ID, CoreMap sentence, Annotation document) {

		ArrayList<String> tokenValues = new ArrayList<String>();
		ArrayList<String> posValues = new ArrayList<String>();
		ArrayList<String> tokenStartsValues = new ArrayList<String>();
		ArrayList<String> tokenEndsValues = new ArrayList<String>();
		ArrayList<String> nerValues = new ArrayList<String>();

		ArrayList<String> depRolesValues = new ArrayList<String>();
		ArrayList<String> depStartsValues = new ArrayList<String>();
		ArrayList<String> depEndsValues = new ArrayList<String>();
		
	    List<CoreLabel> tokens = sentence.get(CoreAnnotations.TokensAnnotation.class);
        SemanticGraph dependencies = sentence.get(BasicDependenciesAnnotation.class);
	    
	    //------------------------------------
	    //Setup arrays for token info
        //------------------------------------	    
	    for (CoreLabel token : tokens) {
			tokenValues.add(token.value());	
            tokenStartsValues.add(String.valueOf(token.beginPosition()));
            tokenEndsValues.add(String.valueOf(token.endPosition()));            
            nerValues.add(token.get(NamedEntityTagAnnotation.class));
            posValues.add(token.get(PartOfSpeechAnnotation.class));		
		}
	    
	    String[] tokenStrings = tokenValues.toArray(new String[tokenValues.size()]);
	    String[] tokenStarts = tokenStartsValues.toArray(new String[tokenStartsValues.size()]);
	    String[] tokenEnds = tokenEndsValues.toArray(new String[tokenEndsValues.size()]);
        String[] pos = posValues.toArray(new String[posValues.size()]);
        String[] ner = nerValues.toArray(new String[nerValues.size()]);	    
	    
        //-----------------------------------	    
        //Setup arrays for dependency info
        //-----------------------------------	    
	    	    
        for (IndexedWord root : dependencies.getRoots()) {
            depRolesValues.add("root");
            depStartsValues.add("0");
            depEndsValues.add(String.valueOf(root.index()));
        }
        for (SemanticGraphEdge edge : dependencies.edgeListSorted()) {
            depRolesValues.add(edge.getRelation().toString());
            depStartsValues.add(String.valueOf(edge.getTarget().index()));
            depEndsValues.add(String.valueOf(edge.getSource().index()));
        }
        
        String[] depRoles = depRolesValues.toArray(new String[depRolesValues.size()]);
        String[] depStarts = depStartsValues.toArray(new String[depStartsValues.size()]);
        String[] depEnds = depEndsValues.toArray(new String[depEndsValues.size()]);
        
        try{
        	
   		   return generateFeatures(tokenStrings, pos, depRoles, depStarts, depEnds, 
				tokenStarts, tokenEnds, ner,
	            "arg1", arg1StartOffset, arg1EndOffset,
	            "arg2", arg2StartOffset, arg2EndOffset, "relation");
   		   
        }catch(Exception e){
        	e.printStackTrace();
        	System.err.println("NoDependencyPathException");
			return new ArrayList<String>();           
        }	            
	            
	}	
	
	// @Override
	/*public List<String> generateFeatures(Integer arg1StartOffset,
			Integer arg1EndOffset, Integer arg2StartOffset,
			Integer arg2EndOffset, String arg1ID, String arg2ID, CoreMap sentence, Annotation document) {

		System.out.println("starting to generateFeatures");
		
		List<CoreLabel> tokens = sentence.get(CoreAnnotations.TokensAnnotation.class);
		
		//StringBuilder sentenceSB = new StringBuilder();
		//for(int i =0; i < tokens.size(); i++){			
		//	sentenceSB.append(tokens.get(i).value() + " ");
		//}
		//String sentenceString = sentenceSB.toString().trim();

		String sentenceString = sentence.get(CoreAnnotations.TextAnnotation.class);
		
		//initialize arguments
		String[] tokenStrings = new String[tokens.size()];
		String[] posTags = new String[tokens.size()];
		
		//initialize dependency parents to -1
		int[] depChildren = new int[tokens.size()];
		for(int i = 0; i < depChildren.length; i ++){
			depChildren[i] = -1;
		}
		
		String[] depTypes = new String[tokens.size()];
		
		
		String arg1ner = "";
		String arg2ner = "";
		int[] arg1Pos = new int[2];
		int[] arg2Pos = new int[2];

		//iterate over tokens
		for(int i =0; i < tokens.size(); i++){
			
			CoreLabel token = tokens.get(i);
			
			//set the tokenString value
			tokenStrings[i] =token.value();
			
			//set the pos value
			String pos = token.get(CoreAnnotations.PartOfSpeechAnnotation.class);
			if(pos == null){
				posTags[i] = "";
			}
			else{
				posTags[i] = pos;
			}
			
			//int begOffset = token.get(SentenceRelativeCharacterOffsetBeginAnnotation.class);
			//int endOffset = token.get(SentenceRelativeCharacterOffsetEndAnnotation.class);

			int begOffset = sentenceString.indexOf(token.value());
			int endOffset = begOffset + token.value().length() - 1;
			
			// if the token matches the argument set the ner and argPos values
			if(begOffset == arg1StartOffset){
				String ner = token.get(CoreAnnotations.NamedEntityTagAnnotation.class);
				if(ner != null){
					arg1ner = ner;
				}
				arg1Pos[0] = i;
			}
			
			if(endOffset == arg1EndOffset){
				arg1Pos[1] = i;
			}
			
			
			if(begOffset == arg2StartOffset){
				String ner = token.get(CoreAnnotations.NamedEntityTagAnnotation.class);
				if(ner != null){
					arg2ner = ner;
				}
				arg2Pos[0] = i;
			}
			
			if(endOffset == arg2EndOffset){
				arg2Pos[1] = i;
			}
		}
		
		System.out.println("Token Strings: " + tokenStrings[0] + " " + tokenStrings[1] + " " + tokenStrings[2]);
		System.out.println("POS Tags: " + posTags[0] + " " + posTags[1] + " " + posTags[2]);
		System.out.println("arg1Pos: " + arg1Pos);
        System.out.println("arg2Pos: " + arg2Pos);
        System.out.println("arg1ner: " + arg1ner);
        System.out.println("arg2ner: " + arg2ner);
		
		//dependency conversions..
		List<Triple<Integer,String,Integer>> dependencyData = sentence.get(DependencyAnnotation.class);

		if(dependencyData != null){
			for(Triple<Integer,String,Integer> dep : dependencyData){
				int parent = dep.first -1;
				String type = dep.second;
				int child = dep.third -1;
	
				//child and parent should not be equivalent
				if(parent == child){
					parent = -1;
				}
				
				if(child < tokens.size()){
					depChildren[child] = parent;
					depTypes[child] = type;
				}
				else{
					System.err.println("ERROR BETWEEN DEPENDENCY PARSE AND TOKEN SIZE");
					return new ArrayList<String>();
				}
			}
		}
		else{
			return new ArrayList<String>();
		}
		
		//add 1 to end Pos values
		arg1Pos[1] += 1;
		arg2Pos[1] += 1;

        System.out.println("Dep Children: " + depChildren);
        System.out.println("Dep Types: " + depTypes);
        
		
		return originalMultirFeatures(tokenStrings, posTags, depChildren, depTypes, arg1Pos, arg2Pos, arg1ner, arg2ner);
	}	
	*/
	
}
