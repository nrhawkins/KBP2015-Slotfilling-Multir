package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import com.typesafe.config.ConfigFactory
import scala.io.Source
import java.nio.file.{Paths, Files}

object LocationData {

  val config = ConfigFactory.load("create-location-lists.conf")
  
  val cityzhFile = config.getString("chinese-city-file")
  val stateorprovincezhFile = config.getString("chinese-stateorprovince-file")
  val countryzhFile = config.getString("chinese-country-file") 
  
  // ------------------------------------------------------------------------
  // city mids:
  // ------------------------------------------------------------------------
  case class CityInputLine(name: String)
  // ------------------------------------------------------------------------
  // stateorprovince mids:
  // ------------------------------------------------------------------------
  case class StateOrProvinceInputLine(name: String)
  // ------------------------------------------------------------------------
  // country mids:
  // ------------------------------------------------------------------------
  case class CountryInputLine(name: String)
  
  // -------------------------------------------------------
  // Cities - names in Chinese 
  // -------------------------------------------------------
  val cities_zh = {

    val inputFilename = cityzhFile

    // Does file exist?
    if (!Files.exists(Paths.get(inputFilename))) {
      System.out.println(s"File $inputFilename doesn't exist!  " + s"Exiting...")
      sys.exit(1)
    }
    
    Source.fromFile(inputFilename).getLines().map(line => {
      val tokens = line.trim.split("\t")
      try{
        //CityInputLine(tokens(0))
        tokens(0)
      }catch{
        case e: Exception => "ignoreThisLine"
      }
        
    }).toList.filter(l => l != "ignoreThisLine")
    }

    //63235 city names
    //println("Cities_zh size: " + cities_zh.size)
  
    // -------------------------------------------------------
    // StateOrProvinces - names in Chinese
    // -------------------------------------------------------
    val stateorprovinces_zh = {

      val inputFilename = stateorprovincezhFile

      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"File $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        try{
          tokens(0)
          //StateOrProvinceInputLine(tokens(0))
        }catch{
          case e: Exception => "ignoreThisLine"
        }
        
      }).toList.filter(l => l != "ignoreThisLine")
    }

    //1035 stateorprovince names
    //println("StateorProvinces_zh size: " + stateorprovinces_zh.size)
    
    // -------------------------------------------------------
    // Country mids
    // -------------------------------------------------------
    val countries_zh = {

      val inputFilename = countryzhFile

      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"File $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        try{
          tokens(0)
          //CountryInputLine(tokens(0))
        }catch{
          case e: Exception => "ignoreThisLine"
        }
        
      }).toList.filter(l => l != "ignoreThisLine")
    }

    //1415 country names
    //println("Country_zh size: " + countries_zh.size)
    
  
    lazy val citiesChinese = cities_zh.toSet
    lazy val countriesChinese = countries_zh.toSet    
    lazy val stateOrProvincesChinese = stateorprovinces_zh.toSet

  
}

