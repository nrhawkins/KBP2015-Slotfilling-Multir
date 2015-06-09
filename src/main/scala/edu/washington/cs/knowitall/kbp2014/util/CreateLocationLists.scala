package edu.washington.cs.knowitall.kbp2014.util

import java.io.PrintWriter
import java.nio.file.{Paths, Files}

import scala.collection.mutable._
import scala.io.Source

import com.typesafe.config.{Config, ConfigFactory}

/**
 * Main method takes:
 * 1) a file of freebase mids and aliases
 * 2) a file of freebase mids for cities
 * 3) a file of freebase mids for statesorprovinces
 * 4) a file of freebase mids for countries
 * and outputs: 
 * 1) a file of city names in Chinese
 * 2) a file of stateorprovince names in Chinese
 * 3) a file of country names in Chinese
 */

object CreateLocationLists {

  // ------------------------------------------------------------------------
  // alias mids:
  // ------------------------------------------------------------------------
  case class AliasInputLine(mid: String, name: String)
  // ------------------------------------------------------------------------
  // city mids:
  // ------------------------------------------------------------------------
  case class CityInputLine(mid: String)
  // ------------------------------------------------------------------------
  // stateorprovince mids:
  // ------------------------------------------------------------------------
  case class StateOrProvinceInputLine(mid: String)
  // ------------------------------------------------------------------------
  // country mids:
  // ------------------------------------------------------------------------
  case class CountryInputLine(mid: String)
                           
  // ----------------------------------------------------------
  // Configuration File - specifies input and output files
  // ----------------------------------------------------------
  val config = ConfigFactory.load("create-location-lists.conf")
  
  val aliasFile = config.getString("freebase-mid-alias-file")

  val cityFile = config.getString("freebase-mid-city-file")
  val stateorprovinceFile = config.getString("freebase-mid-stateorprovince-file")
  val countryFile = config.getString("freebase-mid-country-file")
  
  val cityzhFile = config.getString("chinese-city-file")
  val stateorprovincezhFile = config.getString("chinese-stateorprovince-file")
  val countryzhFile = config.getString("chinese-country-file") 
  
  // -----------------------------------------------------------------
  // -----------------------------------------------------------------
  // Main - 
  // -----------------------------------------------------------------
  // -----------------------------------------------------------------
  def main(args: Array[String]) {

    println("Ignoring args: " + args.length)
    println("Reading mids Files")

    // -------------------------------------------------------
    // Aliases
    // -------------------------------------------------------
    val aliases = {

      val inputFilename = aliasFile

      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"File $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        try{
          AliasInputLine(tokens(0), tokens(1))
        }catch{
          case e: Exception => AliasInputLine("ignoreThisLine", "tokens(1)")
        }
        
      }).toList.filter(l => l.mid != "ignoreThisLine")
    }

    //340837 alias.zh
    println("Aliases size: " + aliases.size)

    // -------------------------------------------------------
    // City mids
    // -------------------------------------------------------
    val cityMIDS = {

      val inputFilename = cityFile

      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"File $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        try{
          CityInputLine(tokens(0))
        }catch{
          case e: Exception => CityInputLine("ignoreThisLine")
        }
        
      }).toList.filter(l => l.mid != "ignoreThisLine")
    }

    //77472 city.mids
    println("City mids size: " + cityMIDS.size)
    
    // -------------------------------------------------------
    // StateOrProvince mids
    // -------------------------------------------------------
    val stateorprovinceMIDS = {

      val inputFilename = stateorprovinceFile

      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"File $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        try{
          StateOrProvinceInputLine(tokens(0))
        }catch{
          case e: Exception => StateOrProvinceInputLine("ignoreThisLine")
        }
        
      }).toList.filter(l => l.mid != "ignoreThisLine")
    }

    //506 stateorprovince.mids
    println("StateorProvince mids size: " + stateorprovinceMIDS.size)
    
    // -------------------------------------------------------
    // Country mids
    // -------------------------------------------------------
    val countryMIDS = {

      val inputFilename = countryFile

      // Does file exist?
      if (!Files.exists(Paths.get(inputFilename))) {
        System.out.println(s"File $inputFilename doesn't exist!  " + s"Exiting...")
        sys.exit(1)
      }

      Source.fromFile(inputFilename).getLines().map(line => {
        val tokens = line.trim.split("\t")
        try{
          CountryInputLine(tokens(0))
        }catch{
          case e: Exception => CountryInputLine("ignoreThisLine")
        }
        
      }).toList.filter(l => l.mid != "ignoreThisLine")
    }

    //545 country.mids
    println("Country mids size: " + countryMIDS.size)
    
    


    // --------------------------------------------------------
    // --------------------------------------------------------
    // Check if output files exist already
    // If they do, exit with error message
    // --------------------------------------------------------
    // --------------------------------------------------------

    // Check if the "city in Chinese" file exists; if it does, exit with error message
    if (Files.exists(Paths.get(cityzhFile))) {
      System.out.println(s"Cities in Chinese file $cityzhFile already exists!  " +
        "\nExiting...")
      sys.exit(1)
    }

    // Check if the "stateorprovince in Chinese" file exists; if it does, exit with error message
    if (Files.exists(Paths.get(stateorprovincezhFile))) {
      System.out.println(s"States or Provinces in Chinese file $stateorprovincezhFile already exists!  " +
        "\nExiting...")
      sys.exit(1)
    }

    // Check if the "country in Chinese" file exists; if it does, exit with error message
    if (Files.exists(Paths.get(countryzhFile))) {
      System.out.println(s"Countries in Chinese file $countryzhFile already exists!  " +
        "\nExiting...")
      sys.exit(1)
    }
    
    // ------------------------------------------------------------
    // ------------------------------------------------------------
    // Scoring Report - write out
    //   1) the extractions and their score (correct or incorrect)
    //   2) summary of results, overall and by relation
    //      -- # correct, # incorrect, precision
    // ------------------------------------------------------------
    // ------------------------------------------------------------

    val cityzh = new PrintWriter(cityzhFile)    
    cityMIDS.foreach(c => {
      val cities = aliases.filter(a => a.mid == c.mid)
      cities.foreach(c => cityzh.append(c.name + "\n"))
      }
    )
   
    val stateorprovincezh = new PrintWriter(stateorprovincezhFile)
    stateorprovinceMIDS.foreach(s => {
      val stateorprovinces = aliases.filter(a => a.mid == s.mid)
      stateorprovinces.foreach(s => stateorprovincezh.append(s.name + "\n"))
      }
    )   
    
    val countryzh = new PrintWriter(countryzhFile)
    countryMIDS.foreach(c => {
      val countries = aliases.filter(a => a.mid == c.mid)
      countries.foreach(c => countryzh.append(c.name + "\n"))
      }
    )


    println("es: Closing PrintWriters")

    cityzh.close()
    stateorprovincezh.close()
    countryzh.close()

  }

  
}
