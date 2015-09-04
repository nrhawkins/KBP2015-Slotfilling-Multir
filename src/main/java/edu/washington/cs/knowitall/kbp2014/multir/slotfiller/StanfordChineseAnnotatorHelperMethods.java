package edu.washington.cs.knowitall.kbp2014.multir.slotfiller;

//import java.io.File;
//import java.io.FileInputStream;
//import java.io.FileNotFoundException;
//import java.io.IOException;
//import java.util.HashMap;
//import java.util.ArrayList;
//import java.util.List;
//import java.util.Map;
import java.util.Properties;
//import java.util.regex.Pattern;

//import org.apache.commons.io.IOUtils;

/*import edu.knowitall.collection.immutable.Interval;
import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.time.TimeAnnotations.TimexAnnotation;
import edu.stanford.nlp.time.Timex;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.dcoref.CorefChain;
import edu.stanford.nlp.dcoref.CorefChain.CorefMention;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefClusterIdAnnotation;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefGraphAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.time.TimeAnnotations.TimexAnnotation;
import edu.stanford.nlp.time.Timex;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.IntTuple;
import edu.stanford.nlp.util.Pair;
import edu.washington.cs.knowitall.kbp2014.multir.slotfiller.util.DocUtils;*/


//import edu.stanford.nlp.ling.CoreAnnotations.*;
//import edu.stanford.nlp.ling.CoreLabel;
//import edu.stanford.nlp.ling.IndexedWord;
//import edu.stanford.nlp.semgraph.SemanticGraph;
//import edu.stanford.nlp.semgraph.SemanticGraphEdge;
//import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations;
import edu.stanford.nlp.pipeline.*;
//import edu.stanford.nlp.util.CoreMap;
//import edu.washington.multirframework.annotation.ParserAnnotator;


public class StanfordChineseAnnotatorHelperMethods {
	
	private final StanfordCoreNLP chinesePipeline;	
	
	public StanfordChineseAnnotatorHelperMethods(){

		Properties chineseProps = new Properties(); 	    
		chineseProps.put("annotators", "segment, ssplit, pos, ner");
		//chineseProps.put("annotators", "segment, ssplit, pos, ner, parse");
	    //chineseProps.put("annotators", "segment, ssplit, pos, ner, parse, dcoref");
	    chineseProps.put("outputFormat", "xml");
	    chineseProps.put("customAnnotatorClass.segment", "edu.stanford.nlp.pipeline.ChineseSegmenterAnnotator");
	    chineseProps.put("segment.model", "edu/stanford/nlp/models/segmenter/chinese/ctb.gz");
	    chineseProps.put("segment.sighanCorporaDict", "edu/stanford/nlp/models/segmenter/chinese");
	    chineseProps.put("segment.serDictionary", "edu/stanford/nlp/models/segmenter/chinese/dict-chris6.ser.gz");
	    chineseProps.put("segment.sighanPostProcessing", "true");
	    chineseProps.put("ssplit.boundaryTokenRegex", "[.]|[!?]+|[。]|[！？]+");
	    chineseProps.put("pos.model", "edu/stanford/nlp/models/pos-tagger/chinese-distsim/chinese-distsim.tagger");
	    chineseProps.put("ner.model", "edu/stanford/nlp/models/ner/chinese.misc.distsim.crf.ser.gz");
	    chineseProps.put("ner.applyNumericClassifiers", "false");
	    chineseProps.put("ner.useSUTime", "false");
	    chineseProps.put("encoding", "utf-8");
	    chineseProps.put("parse.model", "edu/stanford/nlp/models/lexparser/chinesePCFG.ser.gz");
	    
		this.chinesePipeline = new StanfordCoreNLP(chineseProps);		

		String[] flags = new String[0];     
    	this.chinesePipeline.addAnnotator(new ParserAnnotator("edu/stanford/nlp/models/lexparser/chinesePCFG.ser.gz", false, 100000, flags));  
	}
	
	public StanfordCoreNLP getChinesePipeline(){return chinesePipeline;}
	
	//public static void main(String[] args) throws FileNotFoundException, IOException{
	public static void main(String[] args) {
		StanfordChineseAnnotatorHelperMethods sh = new StanfordChineseAnnotatorHelperMethods();
		System.out.println("StanfordChineseAnnotatorHelperMethods");
	}


}
