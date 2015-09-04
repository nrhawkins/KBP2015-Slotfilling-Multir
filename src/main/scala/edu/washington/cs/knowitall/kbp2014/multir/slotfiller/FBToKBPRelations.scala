package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import scala.io.Source
import java.net.URL
import edu.knowitall.common.Resource

//class FBToKBPRelations(){
//}

object FBToKBPRelations {
  
  private def requireResource(urlString: String): URL = {
    val url = getClass.getResource(urlString)
    require(url != null, "Could not find resource: " + urlString)
    url
  }
  
  val personResource = "/edu/washington/cs/knowitall/kbp2014/multir/slotfiller/FBToKBPPersonRelations.txt"
  private def personUrl = requireResource(personResource) 
  
  val organizationResource = "/edu/washington/cs/knowitall/kbp2014/multir/slotfiller/FBToKBPOrganizationRelations.txt"  
  private def organizationUrl = requireResource(organizationResource)
  
  
  private def loadRelations2(relationResource: String) :Map[String, String] = {
    
    val lines = Source.fromFile(relationResource).getLines()

    val myMap = lines map { line => (line.split('\t')(0), line.split('\t')(1) ) } toMap
    
    myMap
    
  }
  
  private def loadRelations(relationUrl: URL) :Map[String, String] = {
  
    Resource.using(Source.fromURL(relationUrl)) { relationSource =>
    
      val lines = relationSource.getLines()
        
      val myMap = lines map { line => (line.split('\t')(0), line.split('\t')(1) ) } toMap
    
    myMap   
        
    }        
          
  }  
  
  
  lazy val personRelations :Map[String, String] = loadRelations(personUrl)

  lazy val organizationRelations :Map[String, String] = loadRelations(organizationUrl)
  
    
    
  def getFBToKBPRelationsMap(relationsType :String) : Map[String, String] = {
    
    val FBToKBPRelationsMap = relationsType match {
      case "PER" => personRelations
      case "ORG" => organizationRelations
    }    
    
    FBToKBPRelationsMap
  }
  
  
    
}