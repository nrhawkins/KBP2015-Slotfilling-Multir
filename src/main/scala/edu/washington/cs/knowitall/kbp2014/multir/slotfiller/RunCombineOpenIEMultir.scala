package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import scala.io.Source

// -----------------------------------------
// main
// -----------------------------------------

object RunCombineOpenIEMultir {

  def main(Args: Array[String]){
    //0 = queries file
    //1 = OpenIE Results file
    //2 = Multir Results file
    //3 = output file

    val runID = "UWashington1"
    val divideScoreMap1 = false
    val divideScoreMap2 = true
    
    val outputStream = new PrintStream(Args(3))

    val openIEMap = createMap(Args(1), divideScoreMap1)
    val multirMap = createMap(Args(2), divideScoreMap2)

    // -----------------------------
    // Combine the Results
    // ----------------------------- 
    
    println("Size OpenIE Map: " + openIEMap.size)
    println("Size Multir Map: " + multirMap.size)
    
    val queries = KBPQuery.parseKBPQueries(Args(0))

    for(query <- queries){
      
      val slots = query.slotsToFill
      for(slot <- slots){

        val slotsToFill = slot.maxResults              
        val queryRelRunString = query.id + "\t" + slot.name + "\t" + runID

        val slotResult = getSlotResult(queryRelRunString, query.id+slot.name, openIEMap, multirMap, slot.maxResults)
        
        outputStream.print(slotResult)
      }
    }

    outputStream.close()  
  } // main

  def getSlotResult(queryRelRunString :String, queryIdRel :String, openIEMap :Map[String,Seq[KBPOutput]],
    multirMap :Map[String,Seq[KBPOutput]], maxResults :Int) :String = {

      maxResults match {
       case 0 => getSingleMapResult(queryRelRunString, queryIdRel, openIEMap, multirMap) 
       case 1 => getSingleMapResult(queryRelRunString, queryIdRel, openIEMap, multirMap)
       //case 9 => getSingleMapResult(queryRelRunString, queryIdRel, openIEMap, multirMap)
       case 9 => getMultipleMapResult(queryRelRunString, queryIdRel, openIEMap, multirMap)
       case _ => ""
     }   
  
  } 

  def prepareOutputString(startOutputString :String, kbpOutput :KBPOutput) :String = {
    
    val result = startOutputString +"\t"+ kbpOutput.provRelation + "\t" + kbpOutput.slotFill + 
      "\t" + kbpOutput.provFill + "\t" + kbpOutput.confScore + "\n" 
      
    result
  }
  
  def getSingleMapResult(startOutputString :String, key :String, map1 :Map[String,Seq[KBPOutput]], 
      map2 :Map[String,Seq[KBPOutput]]) :String = {
  
    //println("get single result:" + key)
    
    val result = if(map1.contains(key) && map1(key).head.slotFill != "NIL"){ 
                     //println("value in map1")
                     prepareOutputString(startOutputString, map1(key).head) 
                   }  
                   else { if(map2.contains(key) && map2(key).head.slotFill != "NIL") {
                            //println("value in map2")
                            prepareOutputString(startOutputString, map2(key).head) 
                          }
                          else
                          startOutputString + "\t" + "NIL" + "\n"
    }
    result
  }

  def getMultipleMapResultOrig(startOutputString :String, key :String, map1 :Map[String,Seq[KBPOutput]], 
      map2 :Map[String,Seq[KBPOutput]] ) :String = {
    
    var map1String :Seq[String] = Nil
    var map2String :Seq[String] = Nil
    var numToAdd = 0 
    var map1StringValues :Seq[String] = if(map1.contains(key)) {for(kbpOutput <- map1(key)) yield{
      kbpOutput.slotFill
    } } else Nil
    var map1Values :Seq[KBPOutput] = if(map1.contains(key)) map1(key) else Nil
    var map2Values :Seq[KBPOutput] = Nil
    
    val result = if(map1.contains(key) && map1(key).head.slotFill != "NIL"){ 
                     println("values in map1")
                     map1String = for(kbpOutput <- map1(key)) yield{
                       prepareOutputString(startOutputString, kbpOutput)
                     }
                     numToAdd = 9 - map1String.size                   
                     if(numToAdd > 0){                 
    
                       if(map2.contains(key) && map2(key).head.slotFill != "NIL") {
                            println("value in map2")
                            //Want to restrict map2String to kbpOutput not in map1String
                            map2Values.filter(x => !map1StringValues.contains(x.slotFill))
                            if(map2Values.size > 0){
                              map2String = for(kbpOutput <- map2Values) yield {
                                prepareOutputString(startOutputString, kbpOutput) 
                              }
                              map1String.mkString + map2String.mkString  
                            }
                            else{
                              map1String.mkString 
                            }                          
                       }
                       else{map1String.mkString}     
                     } 
                     else{map1String.mkString}   
                 }    
                 else{
                   if(map2.contains(key) && map2(key).head.slotFill != "NIL"){                  
                     println("values in map2")
                     map2String = for(kbpOutput <- map2(key)) yield{
                       prepareOutputString(startOutputString, kbpOutput)
                     }
                     map2String.mkString
                   }
                   else{
                     startOutputString + "\t" + "NIL" + "\n"}
                 }
    result
  }

  def getMultipleMapResult(startOutputString :String, key :String, map1 :Map[String,Seq[KBPOutput]], 
      map2 :Map[String,Seq[KBPOutput]] ) :String = {
    
    val map1NIL = if(map1.contains(key) && map1(key).head.slotFill != "NIL") false else true
    val map2NIL = if(map2.contains(key) && map2(key).head.slotFill != "NIL") false else true

    val nilCombo = if(map1NIL & map2NIL) 0 else if(!map1NIL & map2NIL) 1 else if(map1NIL & !map2NIL) 2 else 3

    if(key=="SF13_ENG_001per:spouse") println("nilcombo: " + nilCombo)
    
    nilCombo match {
      
      // both NIL, return NIL
      case 0 => startOutputString + "\t" + "NIL" + "\n"

      // OpenIE notNIL, Multir NIL, return OpenIE 
      case 1 => { val map1String :Seq[String] = for(kbpOutput <- map1(key)) yield{
                       prepareOutputString(startOutputString, kbpOutput) }                               
                  map1String.mkString
                }

      // OpenIE NIL, Multir notNIL, return Multir   
      case 2 => { val map2String :Seq[String] = for(kbpOutput <- map2(key)) yield{
                       prepareOutputString(startOutputString, kbpOutput) }                              
                  map2String.mkString
                }
        
      // OpenIE notNIL, Multir notNIL, return OpenIE, unless top_employees, then combine the two 
      case 3 => if(!key.contains("top_members")){
                  val map1String :Seq[String] = for(kbpOutput <- map1(key)) yield{
                       prepareOutputString(startOutputString, kbpOutput) }                             
                  map1String.mkString 
                }
                else{  
                  val map1String :Seq[String] = for(kbpOutput <- map1(key)) yield{
                       prepareOutputString(startOutputString, kbpOutput) }                             
                  val map2String :Seq[String] = for(kbpOutput <- map2(key)) yield{
                       prepareOutputString(startOutputString, kbpOutput) } 
                  map1String.mkString + map2String.mkString  
                }       
      
      case _ => startOutputString + "\t" + "NIL" + "\n"
        
    }
    
  }
  
  
  def createKBPOutput(lines :List[String], divideScore :Boolean) :Seq[KBPOutput] = {
    //var count = 0
    for(line <- lines) yield{
       val tokens = line.split("\t")

       //println(count + " " +tokens.size)
       //if(tokens.size < 4) println(line)
       //count = count + 1       
       
       var queryId :String = "NIL"
       var relation :String = "NIL"
       var runId :String = "NIL"
       var provRelation :String = "NIL"
       var slotFill :String = "NIL"
       var provFill :String = "NIL"
       var confScore :String = "NIL"
         
       if(tokens.length>=4){
          queryId = tokens(0)
          relation = tokens(1)
          runId = tokens(2)
          provRelation = tokens(3)
       }   
       if(tokens.length==7){
          slotFill = tokens(4)
          provFill = tokens(5)
          confScore = if(divideScore) applyFormulaToConfidenceScore(tokens(6))          
            else tokens(6)         
       }

       val kbpOutput = new KBPOutput(queryId,relation,runId,provRelation,slotFill,provFill,confScore)
       kbpOutput
    }
  }
  
  def applyFormulaToConfidenceScore(score :String) :String = {
    val divisor = 1000000000
    var adjustedScore = score.toFloat/divisor/10 + 0.4
    if(adjustedScore > .8) adjustedScore = .8
    adjustedScore.toString    
  }

  def createMap(inputFile :String, divideScore :Boolean) :Map[String, Seq[KBPOutput]] = {
    val lines = Source.fromFile(inputFile, "UTF-8").getLines.toList
    //maybe wrap this around createKBPOutput
    //if(lines(0).startsWith("SF") ){}
    val kbpOutput = createKBPOutput(lines, divideScore)
    val outputMap = kbpOutput.groupBy(_.queryIdRelation)
    outputMap
  }

 
}//RunCombineOpenIEMultir