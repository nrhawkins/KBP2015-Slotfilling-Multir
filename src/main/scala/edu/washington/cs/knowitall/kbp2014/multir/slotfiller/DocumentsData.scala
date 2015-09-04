package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import edu.stanford.nlp.pipeline.Annotation

@SerialVersionUID(100L) class DocumentsData (var documents :List[Option[Annotation]]) extends Serializable
{
  val documentsList = documents    
   
}

object DocumentsData {
  
  def createNewDocumentsData(documents :List[Option[Annotation]]) = new DocumentsData(documents)
  
  def writeDocuments (documents :List[Option[Annotation]]) = {
    
    val newDocsData = createNewDocumentsData(documents)
    val oos = new ObjectOutputStream(new FileOutputStream("/tmp/DocumentsData"))
    oos.writeObject(newDocsData)
    
  }
   
  def readDocuments (documents :List[Option[Annotation]]) = {
     
    val ois = new ObjectInputStream(new FileInputStream("/tmp/DocumentsData"))
    val documentsData = ois.readObject.asInstanceOf[DocumentsData]
    ois.close
    
  }    
  
}


