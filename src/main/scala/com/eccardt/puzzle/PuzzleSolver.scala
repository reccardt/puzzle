package com.eccardt.puzzle

/**
 * Weekend Edition Sunday, July 26, 2009 ·   
 * http://www.npr.org/templates/story/story.php?storyId=107018210 
 * Name a well-known U.S. city in six letters. Drop the first and fourth letters 
 * so the remaining four letters, in order, will name another well-known U.S. city.
 * What cities are these?
 * 
 * Hint: The two cities are in adjoining states.
 */

object PuzzleSolver {
  import scala.xml._

  // The article contains a table of cities with population over 100,000. 
  def screenScrapeWikipedia = {
	 import java.net.URL
	 import scala.io.Source
	 // for Scala version 2.8:
	 //val lines = Source.fromURL(new URL("http://en.wikipedia.org/wiki/List_of_United_States_cities_by_population")).getLines().toList
	 // for 2.7.5:
	 val lines = Source.fromURL(new URL("http://en.wikipedia.org/wiki/List_of_United_States_cities_by_population")).getLines.toList
	 val tableLines = lines.dropWhile(! _.contains("wikitable sortable")).takeWhile(! _.contains("</table>"))
	 XML.loadString(tableLines.mkString + "</table>")
  }

  def main(args: Array[String]): Unit = {
	 val theXML = screenScrapeWikipedia
	 // By its nature, screen scraping usually breaks fairly quickly. I saved the
	 // table in a file just in case:
	 //val theXML = XML.loadFile("/usr/local/eclipseworkspace/PuzzleSolver/src/cities.xml")

	 // The rows have 4 tds, the second of which contains the city name.
	 val allCities = ( (0,List[String]()) /: theXML \\ "td") { 
		(x,theCity) => x match {
		  case (counter,lastList) if(counter % 4 != 1) => (counter + 1,lastList)
		  case (counter,lastList) => (counter + 1,theCity.text.toLowerCase :: lastList)
		}
	 }._2
	 //val fourLetterCities = {for(c <- allCities if(c.length == 4)) yield c}.toSet
	 // for 2.7.5:
	 val fourLetterCities = (Set[String]() /: {for(c <- allCities if(c.length == 4)) yield c}) 
	 	{(lastSet,theCity)=> lastSet + theCity}
	
	 for(c <- allCities if(c.length == 6)) {
		val fourLetterCity = c.substring(1,3) + c.substring(4)
		if(fourLetterCities contains fourLetterCity) 
		  println(c.capitalize + " -> " + fourLetterCity.capitalize)
	 }
  }
}
