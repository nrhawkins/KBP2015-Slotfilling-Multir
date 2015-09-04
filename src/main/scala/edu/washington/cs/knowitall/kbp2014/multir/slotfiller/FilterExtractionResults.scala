package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import edu.washington.cs.knowitall.kbp2014.multir.slotfiller.util.DocUtils
import edu.stanford.nlp.pipeline.Annotation;
import edu.knowitall.collection.immutable.Interval
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.ling.CoreAnnotations._;
import KBPQueryEntityType._

object FilterExtractionResults {

  val queryCounter = new java.util.concurrent.atomic.AtomicInteger

  /*def filterExtractions(extrs: Seq[Extraction], query: KBPQuery): Seq[Extraction] = {
    extrs.filter(extr => extr.getArg1().getArgName() == query.name)
  }*/

  def filterExtractions(candidates: Seq[Candidate], query: KBPQuery): Seq[Candidate] = {

    candidates.filter(candidate => candidate.extr.getArg1().getArgName() == query.name)

  }
  
  def satisfiesEntityExactMatchFilter(candidate: Candidate, query: KBPQuery): Boolean = {

    if(candidate.extr.getArg1().getArgName() == query.name) true else false
    
  }
  
  def satisfiesEntityExactMatchCollapsedFilter(candidate: Candidate, query: KBPQuery): Boolean = {

    if(candidate.extr.getArg1().getArgName().split(" ").mkString == query.name) true else false
    
  }
  
  def satisfiesEntityFirstCharacterFilter(candidate: Candidate, query: KBPQuery): Boolean = {

    query.entityType match {
      
      case PER => if(candidate.extr.getArg1().getArgName().startsWith(query.name(0).toString)) true else false      
      case ORG => false
    }        
  }
  
  def satisfiesThresholdFilter(candidate: Candidate): Boolean = {

    // Threshold of 7.5 mil established by Stephen's analysis
    if(candidate.extr.getScore() > 7500000) true else false
    
  }
  
  def wrapWithCandidate(extrs: Seq[Extraction]): Seq[Candidate] = {
    extrs.map { extr =>
      new Candidate(queryCounter.getAndIncrement, extr)
    }
  }
  
  def substituteKBPRelationsChinese(candidates: Seq[Candidate], query :KBPQuery): Seq[Candidate] = {

  
    for(c <- candidates){
      
       c.extr.getRel() match {
         
              case s if(s.contains("RE")) => 
         
              //PER relations to substitute
              case s if (s.contains("birth")) => { 
                var foundLocation = false
                if(LocationData.citiesChinese.contains(c.extr.getArg2().getArgName())){ 
                  c.extr.setRel("per:city_of_birth")
                  foundLocation = true
                }
                if(LocationData.stateOrProvincesChinese.contains(c.extr.getArg2().getArgName())){
                  c.extr.setRel("per:stateorprovince_of_birth")
                  foundLocation = true
                }
                if(LocationData.countriesChinese.contains(c.extr.getArg2().getArgName())){
                  c.extr.setRel("per:country_of_birth")
                  foundLocation = true
                }
                if(!foundLocation) c.extr.setRel("per:city_of_birth")
              }
              case s if (s.contains("death")) => { 
                var foundLocation = false
                if(LocationData.citiesChinese.contains(c.extr.getArg2().getArgName())){  
                  c.extr.setRel("per:city_of_death")
                  foundLocation = true
                }
                if(LocationData.stateOrProvincesChinese.contains(c.extr.getArg2().getArgName())){
                  c.extr.setRel("per:stateorprovince_of_death")
                  foundLocation = true
                }
                if(LocationData.countriesChinese.contains(c.extr.getArg2().getArgName())) {
                  c.extr.setRel("per:country_of_death")
                  foundLocation = true
                }
                if(!foundLocation) c.extr.setRel("per:city_of_death")
              }
              case s if (s.contains("lived")) => { 
                var foundLocation = false
                if(LocationData.citiesChinese.contains(c.extr.getArg2().getArgName())){
                  c.extr.setRel("per:cities_of_residence")
                  foundLocation = true
                }
                if(LocationData.stateOrProvincesChinese.contains(c.extr.getArg2().getArgName())){
                  c.extr.setRel("per:statesorprovinces_of_residence")
                  foundLocation = true
                }
                if(LocationData.countriesChinese.contains(c.extr.getArg2().getArgName())){
                  c.extr.setRel("per:countries_of_residence")     
                  foundLocation = true
                }
                if(!foundLocation) c.extr.setRel("per:cities_of_residence")
              }
              case s if (s.contains("children")) => c.extr.setRel("per:children")
              case s if (s.contains("parents")) => c.extr.setRel("per:parents")
              case s if (s.contains("spouse")) => c.extr.setRel("per:spouse")
              //ORG relations to substitute
              case s if (s.contains("founders")) => c.extr.setRel("org:founded_by")
              case s if (s.contains("headquarters")) => { 
                var foundLocation = false
                if(LocationData.citiesChinese.contains(c.extr.getArg2().getArgName())){
                  c.extr.setRel("org:city_of_headquarters")
                  foundLocation = true
                }
                if(LocationData.stateOrProvincesChinese.contains(c.extr.getArg2().getArgName())){
                  c.extr.setRel("org:statesorprovince_of_headquarters")
                  foundLocation = true
                }
                if(LocationData.countriesChinese.contains(c.extr.getArg2().getArgName())){
                  c.extr.setRel("org:country_of_headquarters") 
                  foundLocation = true
                }
                if(!foundLocation) c.extr.setRel("org:city_of_headquarters")
              }
              //No need to substitute, relation is in KBP format already
              case _ => 
       }      
    }
    
    candidates
    
  }
  
  def substituteKBPRelations(candidates: Seq[Candidate], query :KBPQuery): Seq[Candidate] = {
  //def substituteKBPRelations(extrs: Seq[Extraction]): Seq[Extraction] = {

    // Substitute KBP Relations in places where Freebase relations still exist    

    for (c <- candidates) {
    //for (e <- extrs) {

      if (c.extr.getRel().startsWith("/")) {
        println("Relations Starts with /")

        if (c.extr.getRel().contains("alias")) {
          println("Relation Contains alias")
          
          if(query.entityType.toString.toLowerCase == "per"){c.extr.setRel("per:alternate_names")}
          else if(query.entityType.toString.toLowerCase == "org"){c.extr.setRel("org:alternate_names")}
          
        } else {

          var arg2type = "notLocation"

          if (c.extr.getRel().contains("place")) {
            arg2type = LocationHelper.assignLocationMultipleDatabases(c.extr)
          }

          val arg2typePlural = Map("city" -> "cities", "stateorprovince" -> "statesorprovinces", "country" -> "countries")

          if (arg2type != "notLocation") {

            c.extr.getRel() match {
              case s if (s.contains("birth")) => c.extr.setRel("per:" + arg2type + "_of_birth")
              case s if (s.contains("death")) => c.extr.setRel("per:" + arg2type + "_of_death")
              case s if (s.contains("lived")) => c.extr.setRel("per:" + arg2typePlural(arg2type) + "_of_residence")
            }
          }
          // else, don't substitute a relation, in which case the extraction will be ignored because it doesn't match a slot name        
        }
      }
    }

    candidates
    //extrs
  }

  /*private def satisfiesRequirementAtInterval(start: Integer, end: Integer, specificationStrings: Array[String], candidate: Candidate ): Boolean = {
    
        var intervalHead = start
        for(specificationString <- specificationStrings){
          //if the string is a semantic category requirement string
          if(intervalHead >= end){
            return false
          }
          if (semanticCategoryPattern.findFirstIn(specificationString).isDefined){
            val newIntervalHead = updateIntervalHead(specificationString,candidate,Interval.closed(intervalHead,end),false)
            if(newIntervalHead.isDefined){
              intervalHead = newIntervalHead.get.end
            }
            else{
              return false
            }
          }
          //if the string is simply a word to match
          else{
            val extractionTokenWithoutBrackets = candidate.extr.sentence.chunkedTokens(Interval.closed(intervalHead, intervalHead+1)).head.string.replace("[", "").replace("]", "").toLowerCase()
   
            if(specificationString.toLowerCase().trim() == extractionTokenWithoutBrackets){
              intervalHead = intervalHead+1
            }
            else{
              return false
            }
          }
        }
        return true
  }*/

  private def filterOutBrackets(specificationStrings: Array[String], rel: String): Option[Array[String]] = {

    var newLength = specificationStrings.length
    val relWords = rel.split(" ")
    if (relWords.last.contains("[")) {
      if (relWords.last.replace("[", "").replace("]", "").trim().toLowerCase() == specificationStrings.last) {
        return Some(specificationStrings.take(specificationStrings.length - 1))
      } else {
        return None
      }
    } else {
      Some(specificationStrings)
    }

  }
  /*  private def satisfiesRequirementAtIntervalFromEnd(start: Integer, end: Integer, specificationStrings: Array[String], candidate: Candidate ): Boolean = {
        val remainingSpecificationStringsOption = filterOutBrackets(specificationStrings,candidate.extr.rel.originalText)
        if(remainingSpecificationStringsOption.isEmpty) return false
        var intervalTail = end
        for(specificationString <- remainingSpecificationStringsOption.get.reverse){
          //if the string is a semantic category requirement string
          if(start >= intervalTail){
            return false
          }
          if (semanticCategoryPattern.findFirstIn(specificationString).isDefined){
            val newIntervalTail = updateIntervalHead(specificationString,candidate,Interval.open(start,intervalTail),true)
            if(newIntervalTail.isDefined){
              intervalTail = newIntervalTail.get.start
            }
            else{
              return false
            }
          }
          //if the string is simply a word to match
          else{
            val extractionTokenWithoutBrackets = candidate.extr.sentence.chunkedTokens(Interval.closed(intervalTail-1, intervalTail)).head.string.replace("[", "").replace("]", "").toLowerCase()
   
            if(specificationString.toLowerCase().trim() == extractionTokenWithoutBrackets){
              intervalTail = intervalTail -1
            }
            else{
              return false
            }
          }
        }
        return true
  }*/

  /*  private def intervalMatches(t: Interval, target: Interval, backwards: Boolean): Boolean = {
     
    if(backwards){
      if(t.end == target.end){
        return true
      }
      else{
        return false
      }
      
    }
    else{
      if(t.start == target.start){
        return true
      }
      else{ return false}
    }
  }
*/

  /*  private def updateIntervalHead(semanticTypeRaw: String, candidate: Candidate, interval :Interval, backwards: Boolean ): Option[Interval] = {

    val semanticType = semanticTypeRaw.replace("<","").replace(">", "").trim()
    
    val chunkedSentence = candidate.extr.sentence.chunkedTokens

    if (semanticType == "Organization") {
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
        if (t.descriptor() == "StanfordORGANIZATION") {
          if (intervalMatches(t.interval, interval, backwards)) {
            return Some(t.interval())
          }
        }
      }
      return None
    } else if (semanticType == "Person") {
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
        if (intervalMatches(t.interval, interval, backwards)) {
          if (t.descriptor() == "StanfordPERSON") {
            return Some(t.interval())
          }
        }
      }
      return None
    } else if (semanticType == "Location" || semanticType == "City" || semanticType == "Country" || semanticType == "Stateorprovince") {
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
        if (intervalMatches(t.interval, interval, backwards)) {
          if (t.descriptor() == "StanfordLOCATION") {
            return Some(t.interval())
          }
        }
      }
      return None
    }

    else if (semanticType == "School") {
      val types = SemanticTaggers.useEducationalOrganizationTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }

      return None

    } else if (semanticType == "JobTitle") {
      val types = SemanticTaggers.useJobTitleTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }

      return None

    } else if (semanticType == "HeadJobTitle"){
      
      val types = SemanticTaggers.useHeadJobTitleTagger(chunkedSentence)
      for(t <- types){
        if (intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }
      
      return None
    }
      else if (semanticType == "Nationality") {

      val types = SemanticTaggers.useNationalityTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }

      return None

    } else if (semanticType == "Religion") {
      val types = SemanticTaggers.useReligionTagger(chunkedSentence)

      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }

      return None

    } else if (semanticType == "Date") {
      val types = SemanticTaggers.useDateTagger(chunkedSentence)
      
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }

      return None

    } else if (semanticType == "ProperNoun"){
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }
      
      return None
    } else if ((semanticType =="<integer>-year-old") || (semanticType == "Integer")){

      val types = SemanticTaggers.useIntegerTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }
      return None
    } else if (semanticType =="Crime"){
      val types = SemanticTaggers.useCrimeTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }
      return None
    } else {
      return None
    }
  } */

  /*  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2BeginsFilter(candidate: Candidate): Boolean = {
    

    candidate.pattern.arg2Begins match {
      case Some(arg2BeginsString) => {
        
        val patternSpecificationStrings = arg2BeginsString.split(" ")
        val intervalHead = candidate.extr.arg2.tokenInterval.start
        val end = candidate.extr.arg2.tokenInterval.end
        satisfiesRequirementAtInterval(intervalHead, end, patternSpecificationStrings, candidate)
        
        
      }
      case None => {
        //if nothing specified then throw out any extractions where arg2 does begin with a preposition
        val firstTokenInArg2Option = candidate.extr.arg2.tokens.headOption
        if(firstTokenInArg2Option.isDefined){
          val firstTokenInArg2 = firstTokenInArg2Option.get
          if(firstTokenInArg2.isPreposition){
            false
          }
          else{
            true
          }
        }
        else{
          false
        }
      }
    }
  } */

  /*  private def satisfiesRelFilter(candidate: Candidate): Boolean = {

    //if there are no semantic strings in the relation condidtion then count backwards for
    //the start interval and call satisfiesRequirementAtInterval
    candidate.pattern.relString match {
      case Some(relString) => {
        
        val patternSpecificationStrings = relString.split(" ")
        val intervalHead = candidate.extr.rel.tokenInterval.start
        val end = candidate.extr.rel.tokenInterval.end
        val b = satisfiesRequirementAtIntervalFromEnd(intervalHead, end, patternSpecificationStrings, candidate)
        b
      }
      case None => true
    }
  } 
  */

  /* private def satisfiesTermFilters(candidate: Candidate) : Boolean = {
    
    //arg1 Terms
    val arg1TermsSatisfied =
      candidate.pattern.arg1Terms match {
        case Some(arg1Terms) => {
              val arg1Interval = candidate.extr.arg1.tokenInterval
              val patternSpecificationStrings = arg1Terms.split(" ")
              var foundMatchingSubsequence = false
              for(x <- arg1Interval){
             if(satisfiesRequirementAtInterval(x,arg1Interval.end,patternSpecificationStrings,candidate)){
             foundMatchingSubsequence = true
             }
              }
              foundMatchingSubsequence
        }
        case None => true
      }


    //arg2 Terms
    val arg2TermsSatisfied =
      candidate.pattern.arg2Terms match {
        case Some(arg2Terms) => {
              val arg2Interval = candidate.extr.arg2.tokenInterval
              val patternSpecificationStrings = arg2Terms.split(" ")
              var foundMatchingSubsequence = false
              for(x <- arg2Interval){
             if(satisfiesRequirementAtInterval(x,arg2Interval.end,patternSpecificationStrings,candidate)){
             foundMatchingSubsequence = true
             }
              }
              foundMatchingSubsequence
        }
        case None => true
      }
    
    
    (arg1TermsSatisfied && arg2TermsSatisfied)
  }
*/
  
/*private def getTokenBeginningAtByteOffset(annotatedDocument :Annotation, beg :Integer):CoreLabel = {

  val sentences = annotatedDocument.get(SentencesAnnotation.class);
  for(sentence <-sentences){
    for(CoreLabel token : sentence.get(TokensAnnotation.class)){
      if(token.beginPosition() == beg ){
        return token;
      }
    }
  }
  return null;
}*/
  
  private def satisfiesEntityFilter(document: Option[Annotation])(kbpQuery: KBPQuery)(candidate: Candidate): Boolean = {

    val queryEntity = kbpQuery.name
    val arg1 = candidate.extr.getArg1().getArgName()      
    
    //val inSameCorefChain = Option(
    val inSameCorefChain :Boolean =
      document match {
        case Some(ann) => DocUtils.stanfordHelper.inSameCorefChainKBPEntityMentionAndQueryName(kbpQuery.name, candidate.extr.getArg1().getArgName(),
          candidate.entityOffsetInterval, ann)
        //case None => Interval.closed(0,0)
        case None => false
      }

    //  val annotation.get(CorefChainAnnotation.class) .keySet()
    //for (Integer key: annotation.get(CorefChainAnnotation.class) .keySet()) {
   	//  for (CorefMention mention: annotation.get(CorefChainAnnotation.class).get(key).getMentionsInTextualOrder()) {
        //		System.out.println(mention.mentionSpan);
	//}  }
      
    if (!inSameCorefChain) {
      println("Not in Same Coref Chain")
      false
    } else {
      //update entity string and interval to match that of the coreferenced kbp query entity
     // candidate.trimmedEntity.setString(kbpQuery.name)
      //val supportingByteOffsets = DocUtils.getByteOffSetsOfFirstOccurenceOfString(candidate.extr.getDocName(), kbpQuery.name)
      //val supportingByteOffsets = DocUtils.getByteOffSetsOfFirstOccurenceOfString(candidate.extr.sentence.docId,kbpQuery.name)
      //if the offsets can be found..
      //if (supportingByteOffsets.isDefined) {
      //  candidate.trimmedEntity.setSupportingByteOffsets(supportingByteOffsets.get)
      //}
      true
    }
    //}
    //else{
    // true
    //}

  }

  /*private def loadStoplist(rsrc: String) = {
    val resource = getClass.getResource(rsrc)
    require(resource != null, s"Couldn't find $rsrc")
    using(io.Source.fromURL(resource)) { source =>
      source.getLines.map(_.toLowerCase.trim).toSet
    }
  }
  
  private val cityStoplistFile = "/edu/knowitall/tac2013/findSlotFillersApp/CityStoplist.txt"
  private val provinceStoplistFile = "/edu/knowitall/tac2013/findSlotFillersApp/ProvinceStoplist.txt"
  private val countryStoplistFile = "/edu/knowitall/tac2013/findSlotFillersApp/CountryStoplist.txt"
    
  private val cityStoplist = loadStoplist(cityStoplistFile)
  private val provinceStoplist = loadStoplist(provinceStoplistFile)
  private val countryStoplist = loadStoplist(countryStoplistFile)
  
  private def satisfiesLocationStoplist(candidate: Candidate): Boolean = {

    val fillText = candidate.trimmedFill.string.toLowerCase
    val slotType = candidate.pattern.slotType.getOrElse({ "" })

    if (slotType == "City") {
      !cityStoplist.contains(fillText)
    } else if (slotType == "Stateorprovince") {
      !provinceStoplist.contains(fillText)
    } else if (slotType == "Country") {
      !countryStoplist.contains(fillText)
    } else {
      true
    }
  }
  */

  /*private def satisfiesSemanticFilter(candidate: Candidate): Boolean = {

    val pattern = candidate.pattern
    val types = candidate.types
    val trimmedFill = candidate.trimmedFill
    val slotType = pattern.slotType.getOrElse({ "" })
    val slotLocation = pattern.slotFillIn match {
      case Some("arg1") => candidate.extr.arg1.tokenInterval
      case Some("arg2") => candidate.extr.arg2.tokenInterval
      case Some("relation") => candidate.extr.rel.tokenInterval
      case _ => return false
    }

    val chunkedSentence = candidate.extr.sentence.chunkedTokens

    if (slotType == "Organization" || slotType == "Person" || slotType == "Stateorprovince" ||
      slotType == "City" || slotType == "Country") {

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) {
          slotType match {
            case "Organization" => {
              if (t.descriptor() == "StanfordORGANIZATION") {
                return true
              }
            }
            case "Person" => {
              if (t.descriptor() == "StanfordPERSON") {
                return true
              }
            }
            // default case will be location
            case _ => {
              //if trimmed Fill does not exist then the Candidate
              //constructor has filtered out this extraction
              val typesInSlotFill = types.filter(t => (t.interval().intersects(slotLocation)))
              if (findLocationTaggedType(typesInSlotFill, slotType).isDefined) {
                return true
              } else {
                return false
              }
            }
          }
        }
      }

      return false
    } else if (slotType == "School" || slotType == "JobTitle" ||slotType == "HeadJobTitle" ||
        slotType == "Nationality" || slotType == "Religion" || slotType == "Date" ||
        slotType == "ProperNoun" || slotType =="<integer>-year-old" || slotType == "Integer" ||
        slotType =="Crime") {
      
      for (t <- types) {
        if (t.interval().intersects(slotLocation)){
            return true
          }
        }

      return false
    }
    else {

      return true
    }
  }
*/

  /*  def satisfiesLengthFilter(candidate: Candidate): Boolean = {
    
    //if an alternate name slot fill is longer than 5 tokens, it should be filtered out
    if(Slot.fromName(candidate.pattern.slotName).isAlternateName){
      if(candidate.trimmedFill.interval.length >5) return false
    }
    
    true
  }
  
  val titleStopListPath = "/edu/knowitall/tac2013/findSlotFillersApp/TitleStoplist.txt"

  val titleStopList = loadStoplist(titleStopListPath)

  def satisfiesSlotFilter(candidate: Candidate): Boolean = {

    val fillText = candidate.trimmedFill.string.toLowerCase
    val slotFillIn = candidate.pattern.slotFillIn.getOrElse("")
    
    // require that a slot filler in arg2 start within the first 4 tokens.
    if (slotFillIn == "arg2" && candidate.trimmedFill.interval.start - candidate.extr.arg2.tokenInterval.start > 4) false
    
    else if (Slot.fromName(candidate.pattern.slotName).isCauseOfDeath) {
      val fillTokens = candidate.extr.sentence.chunkedTokens(candidate.trimmedFill.interval)
      !fillTokens.headOption.exists(_.isPronoun)
      
    } else if (Slot.fromName(candidate.pattern.slotName).isTitle) {
      !titleStopList.contains(fillText)
    }
    else true
  }
  
  /**
* matches if any word starts with &
* e.g. &amp or &gt.
* A single & does not match.
*/
  val htmlEntityPattern = ".*\\s+&\\w+.*".r.pattern
  def satisfiesHtmlFilter(candidate: Candidate): Boolean = {
    !htmlEntityPattern.matcher(candidate.trimmedFill.string).matches
  }
*/

  //filters results from solr by calling helper methods that look at the KbpSlotToOpenIEData specifications and compare
  //that data with the results from solr to see if the relation is still a candidate
  //
  def filterResults(unfiltered: Seq[Candidate], kbpQuery: KBPQuery, document: Option[Annotation]): Seq[Candidate] = {

    def combinedFilter(candidate: Candidate) = (
      //satisfiesLengthFilter(candidate) &&
      //satisfiesArg2BeginsFilter(candidate) &&
      //satisfiesRelFilter(candidate) &&
      //satisfiesHtmlFilter(candidate) &&
      //satisfiesTermFilters(candidate) &&
      //satisfiesSlotFilter(candidate) &&
      //satisfiesLocationStoplist(candidate) &&
      //satisfiesSemanticFilter(candidate) &&
      (satisfiesEntityExactMatchFilter(candidate, kbpQuery) || satisfiesEntityFilter(document)(kbpQuery)(candidate)) &&  
      satisfiesThresholdFilter(candidate))

    unfiltered filter combinedFilter
  }

  def filterResultsChinese(unfiltered: Seq[Candidate], kbpQuery: KBPQuery, document: Option[Annotation]): Seq[Candidate] = {

    def combinedFilter(candidate: Candidate) = (
      //satisfiesLengthFilter(candidate) &&
      //satisfiesArg2BeginsFilter(candidate) &&
      //satisfiesRelFilter(candidate) &&
      //satisfiesHtmlFilter(candidate) &&
      //satisfiesTermFilters(candidate) &&
      //satisfiesSlotFilter(candidate) &&
      //satisfiesLocationStoplist(candidate) &&
      //satisfiesSemanticFilter(candidate) &&
      //(satisfiesEntityExactMatchFilter(candidate, kbpQuery) || satisfiesEntityFilter(document)(kbpQuery)(candidate)) &&  
      //satisfiesThresholdFilter(candidate)
      satisfiesEntityExactMatchFilter(candidate, kbpQuery) || satisfiesEntityFirstCharacterFilter(candidate, kbpQuery) ||
      satisfiesEntityExactMatchCollapsedFilter(candidate, kbpQuery)
      )

    unfiltered filter combinedFilter
  }
  
  
}

  
