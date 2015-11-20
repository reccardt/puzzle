/*
 * List of nations from http://www.undemocracy.com/nations because a simple copy and paste
 * was all that was needed.
 * NPR puzzle July 07, 2013
 * Next week's challenge: Rearrange the letters of INDIA + BELARUS to name two other countries. What are they?
 */
import scala.language.postfixOps

object Nations {
  
  def hasAll(s: String,t: String): Boolean = {
	  if(s.length == 0) true
	  else if(t.contains(s.substring(0,1))) 
	    hasAll(s.substring(1),t.replaceFirst(s.substring(0,1), ""))
	  else false
  }
  
  def twelveLetters2(s: String,l: List[String],tl: List[String]): List[String] = {
    if(l.isEmpty)
      tl
    else if((s + l.head).length == 12)
      twelveLetters2(s,l.tail,(s + l.head) :: tl)
    else
      twelveLetters2(s,l.tail,tl)
  }
  
  def twelveLetters(s: String,l: List[String],tl: List[String]): List[String] = {
    if(l.isEmpty) tl
    else twelveLetters(l.head,l.tail,twelveLetters2(s,l,tl))
  }
  
  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val lines = Source.fromFile("nations.txt").getLines
    val nations = {for(l <- lines) 
      yield (l.trim.toLowerCase.replaceAll(" ",""))} toList
    val tl = twelveLetters(nations.head,nations.tail,Nil)
    tl foreach(n => if(hasAll(n,"belarusindia")) println(n))
  }

}
