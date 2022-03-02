
import com.github.vickumar1981.stringdistance.StringConverter._
import java.io.{FileNotFoundException, IOException}
import scala.io.Source
class Trie[Val] {
  private var store = new Node(Map(), None)

  /** *
   * to put data into the trie ds
   *
   * @param key   :String which need to be inserted in trie
   * @param value :Integer to store along with key as value
   * @return
   */
  def put(key: String, value: Val): Unit = {
    store = store.put(key, value, 0)}

  /** *
   * to check is data in trie
   *
   * @param key :String which need to be found in trie
   * @return: Option[Val] where the data is inserted with some value or none
   */
  def get(key: String): Option[Val] = {
    val loggerObj = new Logger()
    loggerObj.logger.info("function get of trie started")
    store.get(key).contents}

  private class Node(kids: Map[Char, Node], val contents: Option[Val]) {

    def children: Map[Char, Node] = kids withDefaultValue new Node(Map(), None)


    /** *
     * to put data into the trie ds
     *
     * @param key      :String which need to be inserted in trie
     * @param value    :Integer to store along with key as value
     * @param position : is used to increment from 0 to length of key
     * @return here the data is inserted with some value
     */
    def put(key: String, value: Val, position: Int): Node =
      if (position == key.length) new Node(kids, Some(value))
      else {
        val char = key.charAt(position)
        new Node(kids updated(char, children(char).put(key, value, position + 1)), contents)
      }
    /** *
     * find the string in trie
     *
     * @param key      :String which need to be found in trie
     * @param position : position is used to increment from 0 to length of key
     * @return:  Node where the data is inserted with some value
     */

    def get(key: String, position: Int): Node = {
      val loggerObj = new Logger()
      loggerObj.logger.info("function get started")
      if (position == key.length) this else children(key.charAt(position)).get(key, position + 1)
    }
    /** *
     * find the string in trie
     *
     * @param key :String which need to be found in trie
     * @return: Node where the data is inserted with some value
     */

    def get(key: String): Node = {
      val loggerObj = new Logger()
      loggerObj.logger.info("function get started")
      get(key, 0)
    }
  }
}

class SpellChecker {
  val trieObj = new Trie[Int]
  /** *
   * check the given string is present in dictionary or not.
   *
   * @param inputString    :The phrase given by user
   * @param dictionaryList :dictionary words stored in list
   * @return the full corrected phrase is returned
   */
  def checkWords(inputString: String, dictionaryList: List[String]): String = {
    val loggerObj = new Logger()
    loggerObj.logger.info("function check string started")
    loggerObj.startTime()
    if (inputString.isEmpty() || dictionaryList.isEmpty) {
      return "empty input"
    }
    val inputLowercase = inputString.toLowerCase.trim()
    val input = inputLowercase.replace(".", "")
    // splitting given string to words
    val words = input.split(" ")
    //zipping the word in order the user gave so it wont shuffle during correcting and checking
    val zipedWord = words.zipWithIndex
    val arrayWords = new Array[String](words.length)
    for (i <- 0 until words.length) {
      if (trieObj.get(zipedWord(i)._1) != None) {// extracting string from zipped input
        arrayWords(zipedWord(i)._2) = zipedWord(i)._1 // inserts into the appropriate input position
      }
      else {
        val q = correctingWord(zipedWord(i)._1, dictionaryList)
        arrayWords(zipedWord(i)._2) = q
      }
    }
    loggerObj.stopTime()
    loggerObj.logger.info("function check string ended" + loggerObj.getTime)
    arrayWords.mkString(" ")
  }


  /** *reads the contents in the dictionary file and insert it into trie data structure
   *
   * @param path :dictionary path
   * @return dictionarywords in list
   */
  def readDictionary(path: String): List[String] = {
    val loggerObj = new Logger()
    loggerObj.logger.info("function read dictionary started")
    loggerObj.startTime()
    var dictionaryList: List[String] = List(" ")
    try {
      val lines = Source.fromFile(path)
      val source = lines.getLines()
      dictionaryList = source.toList // converting each lines of word to list
      lines.close()
    }
    catch {
      case e: FileNotFoundException => loggerObj.logger.info("Couldn't find that file.")
        return List("file not found exception")
      case e1: IOException => loggerObj.logger.info("input output exception")
        return List("IO exception")
    }
    for (line <- dictionaryList.indices) {
      trieObj.put(dictionaryList(line), 1) // inserting dictionary words into trie
    }
    loggerObj.stopTime()
    loggerObj.logger.info("function read dictionary ended" + loggerObj.getTime)
    dictionaryList
  }
  /** *
   * it corrects the incorrect word using dictionary list
   *
   * @param incorrectWord  :String from user inputString not present in dictionary
   * @param dictionaryList :List[String] where the dictionary words are present
   * @return: returns a  corrected string
   */
  def correctingWord(incorrectWord: String, dictionaryList: List[String]): String = {
    val loggerObj = new Logger()
    loggerObj.logger.info("function correcting word started")
    loggerObj.startTime()
    var string = ""
    // finding edit distance for dictionary and given incorrect string
    val distance = dictionaryList.map(x => x.damerauDist(incorrectWord))
    // zipping the edit distance of the string with it.
    val zippedList = dictionaryList.zip(distance)
    //filtering the edit distance if it is 1
    val filteredEdit1 = zippedList.filter(x => x._2 == 1)
    val result = filteredEdit1.isEmpty
    if (!result) {
      string = filteredEdit1.head._1 //get string(hii) from List("hii",1) 1-edit distance
    }
    else {
      //filtering the edit distance if it is 2
      val filteredEdit2 = zippedList.filter(x => x._2 == 2)
      val withEdit2= filteredEdit2.isEmpty
      if (!withEdit2) {
        string = filteredEdit2.head._1
      }
    }
    loggerObj.stopTime()
    loggerObj.logger.info("function correcting word ended" + loggerObj.getTime)
    string
  }

}
object Main {
  val spellCheckerObj = new SpellChecker

  def main(args: Array[String]): Unit = {
    val path = "/home/preethia/Desktop/SpellChecker/engmix.txt"
    val dictionaryList = spellCheckerObj.readDictionary(path)
    var flag = true
    while (flag) {
      println("Enter word for spellchecking and  number for exit")
      val reg="""[0-9]""".r
      println("Enter your input:")
        val inputString = scala.io.StdIn.readLine()
        reg.findFirstMatchIn(inputString) match {
          case None =>
            println("The corrected input is  " + spellCheckerObj.checkWords(inputString, dictionaryList))
          case Some(_) => print("Exited")
            flag = false
        }
      }
    }
}










