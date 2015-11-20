/* The NPR Weekend Edition Sunday puzzle from April 28, 2013 deals with proverbs.
 * I took the proverbs from here http://www.phrases.org.uk/meanings/proverbs.html
 * and copied and pasted them into the file proverbs.txt. 
 * 
 * The rules from the NPR website:
 *  Next week's challenge from listener Matt Jones of Portland, Ore.: The first 12 letters of the alphabet are A to L. Think of a familiar,
 *  six-word proverb that contains 11 of these 12 letters. The letters may be used more than once, and you may use additional letters from
 *  the second half of the alphabet. What proverb is this?
 */

object Proverbs {

  def getCount(proverb: Set[Char], letters: List[Char], countSoFar: Int): Int = {
    if(letters.isEmpty)
      countSoFar
    else if(proverb contains letters.head)
      getCount(proverb,letters.tail,countSoFar + 1)
    else 
      getCount(proverb,letters.tail,countSoFar)
  }
  
  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val lines = Source.fromFile("proverbs.txt").getLines
    val sixWords = for(l <- lines if l.trim.split(' ').length == 6) // take all 6 word proverbs 
      yield (l.trim.toLowerCase.toCharArray.toSet, l) // and make a list of tuples of the set of letters they contain and the proverb itself
    val firstTwelve = List.range('a', 'm') // the first 12 letters of the alphabet, a through l
    sixWords foreach(_ match {case (letters,proverb) => if(getCount(letters,firstTwelve,0) >= 11) println(proverb)})
  }

}
