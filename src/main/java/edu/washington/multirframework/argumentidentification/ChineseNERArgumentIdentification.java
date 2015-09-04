package edu.washington.multirframework.argumentidentification;

import java.util.ArrayList;
import java.util.List;

import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.util.CoreMap;
import edu.washington.multirframework.corpus.TokenOffsetInformation.SentenceRelativeCharacterOffsetBeginAnnotation;
import edu.washington.multirframework.corpus.TokenOffsetInformation.SentenceRelativeCharacterOffsetEndAnnotation;
import edu.washington.multirframework.data.Argument;


/**
 * Implements <code>ArgumentIdentification</code> method <code>identifyArguments</code>
 * to get all Arguments with an NER type. 
 * @author jgilme1
 *
 */
public class ChineseNERArgumentIdentification implements ArgumentIdentification {

	//only NER Types considered
	private static String[] relevantNERTypes = {"ORG", "PERSON", "LOC", "GPE"};
	
	private static ChineseNERArgumentIdentification instance = null;
	
	
	private ChineseNERArgumentIdentification(){}
	public static ChineseNERArgumentIdentification getInstance(){
		if(instance == null) instance = new ChineseNERArgumentIdentification();
		return instance;
		}
	
	
	@Override
	/**
	 * Returns a List of Argument for all of the unique arguments in
	 * the sentence.
	 */
	public List<Argument> identifyArguments(Annotation d, CoreMap s) {
		List<Argument> arguments = new ArrayList<Argument>();
		List<CoreLabel> tokens = s.get(CoreAnnotations.TokensAnnotation.class);
		List<List<CoreLabel>> argumentTokenSpans = new ArrayList<List<CoreLabel>>();
		//StringBuilder sentenceSB = new StringBuilder();
		
		//add candidate token spans
		for(int i =0; i < tokens.size();){
			//sentenceSB.append(tokens.get(i).value() + " "); 
			if (isRelevant(tokens.get(i))){
				List<CoreLabel> tokenSequence = getRelevantTokenSequence(tokens,i);
				argumentTokenSpans.add(tokenSequence);
				i += tokenSequence.size();
			}
			else{
				i++;
			}
		}

		//String sentenceString = sentenceSB.toString().trim();
		String sentenceString = s.get(CoreAnnotations.TextAnnotation.class);
		
		//for each candidate string check in the KB for all ids that 
		//share that string and return as possible arguments
		for(List<CoreLabel> argumentTokenSpan : argumentTokenSpans){
			StringBuilder argumentSB = new StringBuilder();
			for(CoreLabel token: argumentTokenSpan){			
				argumentSB.append(token.value());
				argumentSB.append(" ");
			}
			String argumentString = argumentSB.toString().trim();
			
            //Returning null pointer error
			//int tokenStartOffset = argumentTokenSpan.get(0).get(SentenceRelativeCharacterOffsetBeginAnnotation.class);
			//int tokenEndOffset = argumentTokenSpan.get(argumentTokenSpan.size()-1).get(SentenceRelativeCharacterOffsetEndAnnotation.class);

			int tokenStartOffset = argumentTokenSpan.get(0).beginPosition();
			int tokenEndOffset = argumentTokenSpan.get(argumentTokenSpan.size()-1).endPosition();
			
			//int tokenStartOffset = sentenceString.indexOf(argumentString);
			//int tokenEndOffset = tokenStartOffset + argumentString.length() - 1;
			
			Argument arg = new Argument(argumentString, tokenStartOffset, tokenEndOffset);
			arguments.add(arg);
		}
		return arguments;
	}
	
	//get contiguous sequences of tokens that share a relevant named entity type
	private List<CoreLabel> getRelevantTokenSequence(List<CoreLabel> tokens,
			int i) {
		List<CoreLabel> tokenSequence = new ArrayList<CoreLabel>();
		tokenSequence.add(tokens.get(i));
		String ner = tokens.get(i).get(CoreAnnotations.NamedEntityTagAnnotation.class);
		i++;
		while(i < tokens.size()){
			String nextNer = tokens.get(i).get(CoreAnnotations.NamedEntityTagAnnotation.class);
			if(ner.equals(nextNer)){
				tokenSequence.add(tokens.get(i));
			}
			else{
				break;
			}
			i++;
		}
		return tokenSequence;
	}
	
	private boolean isRelevant(CoreLabel token) {
		String ner = token.get(CoreAnnotations.NamedEntityTagAnnotation.class);
		for(String relevantNER : relevantNERTypes){
			if(relevantNER.equals(ner)){
				return true;
			}
		}
		return false;
	}
}
