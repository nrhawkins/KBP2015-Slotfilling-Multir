package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import edu.washington.multirframework.data.Argument

object SelectBestAnswers {
  
  /*def deduplicate(candidates: Seq[Candidate]) = {
    candidates.groupBy(_.deduplicationKey).map {
      case (key, duplicates) =>
        duplicates.maxBy(_.extr.confidence)
    } toSeq
  }
 */    
  
  def reduceToMaxResults(slot :Slot, candidates :Seq[Candidate]):Seq[Candidate] = {
    
    if(candidates.size > 1) {
      
     val sortedCandidates = candidates.sortBy(candidate => candidate.extr.getScore()).reverse 
      
     slot.maxResults match {
       
       case 1 => Seq(sortedCandidates.head) 
      
       case 9 => { 
         
         var slotFills: collection.mutable.Set[String] = collection.mutable.Set()
         
         val dedupedCandidates = for (c <- sortedCandidates) yield {
           
           val slotFill = c.extr.getArg2().getArgName()
           if(!slotFills.contains(slotFill)) { 
             slotFills += slotFill 
           }
           else{             
             val arg2 = new Argument("dropdupe",0,0)
             c.extr.setArg2(arg2)
           }
           c           
         }
         
         val returnCandidates = dedupedCandidates.toList.filter(c => c.extr.getArg2().getArgName() != "dropdupe").toSeq
         returnCandidates         
         
         //Seq(sortedCandidates.head) 
         /*val candidatesMap = candidates map 
           { candidate => (candidate.extr.getArg2().getArgName() , 
                           candidates.filter(_.extr.getArg2().getArgName()
                        == candidate.extr.getArg2().getArgName()).sortBy(candidate => candidate.extr.getScore()).reverse.head )
           }  toMap	
                        
                           
         // Make a map of unique slot fills from the list of candidates
         //val candidatesMap = candidates map { candidate => (candidate.extr.getArg2().getArgName(), candidate) } toMap 

         // Ensure that of the duplicates, if any, the one with highest confidence score is in the map
         //candidates.foreach(candidate => if(candidate.extr.getScore() > candidatesMap(candidate.extr.getArg2().getArgName()).extr.getScore()) 
         //  candidatesMap + candidate.extr.getArg2().getArgName() -> candidate
         //)
         
         // ToDo: Still need to mergeNames (like VA and Virginia, Bill Clinton and William Jefferson Clinton)
         if(candidatesMap.values.toSeq.size > 9) candidatesMap.values.toSeq.take(9)
         else candidatesMap.values.toSeq
       */
       }
       
       // Could be a case = 0
       case _ => Seq(sortedCandidates.head) 
       
     }
       
    }
    else{
   
      candidates
       
    }   
    
  }
  
  
}