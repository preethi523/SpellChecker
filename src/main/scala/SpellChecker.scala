
import Trie.Trie
import com.github.vickumar1981.stringdistance.StringConverter._

import java.nio.charset.CodingErrorAction
import scala.collection.mutable.ArrayBuffer
import scala.io.{Codec, Source}

object Trie {
  class Trie[Val] {
    private var store = new Node(Map(), None)

    def put(key: String, value: Val): Unit = store = store.put(key, value, 0)

    def get(key: String): Option[Val] = store.get(key).contents

    private class Node(kids: Map[Char, Node], val contents: Option[Val]) {
      val count: Int =
        (if (contents.isEmpty) 0 else 1) + kids.values.foldLeft(0)((acc, node) => acc + node.count)

      def children: Char => Node = kids withDefaultValue new Node(Map(), None)

      def put(key: String, value: Val, pos: Int): Node =
        if (pos == key.length) new Node(kids, Some(value))
        else {
          val char = key.charAt(pos)
          new Node(kids updated(char, children(char).put(key, value, pos + 1)), contents)
        }

      def put(key: String, value: Val): Node = put(key, value, 0)

      def get(key: String, pos: Int): Node =
        if (pos == key.length) this else children(key.charAt(pos)).get(key, pos + 1)

      def get(key: String): Node = get(key, 0)

      def traverse[T](f: (Node, Stream[T], String) => T, prefix: String): T = {
        val keys = kids.keys.toVector.sorted.toStream
        val vals = keys.map(key => children(key).traverse[T](f, prefix ++ key.toString))
        f(this, vals, prefix)
      }
    }

    /** *
     *it corrects the incorrect word and by  dictionary list
     * @param incorrectWord :String from user inputString not present in dictionary
     * @param dictionaryList          :List[String] where the
     */
    def correctingWord(incorrectWord: String, dictionaryList:List[String]) = {
      val loggerObj = new Logger()
       loggerObj.logger.info("function correcting word started")
      loggerObj.startTime()
      var correctedWord = ""
     // var damerauDistance=0
      for (line <-dictionaryList.indices) {
        if (incorrectWord.damerauDist(dictionaryList(line))== 1) {
          correctedWord = (dictionaryList(line) + " ").mkString
        }
//         else if(incorrectWord.damerauDist(dictionaryList(line))>1){
//         correctedWord=(dictionaryList(line)+" ").mkString
//         }
      }
      loggerObj.stopTime()
      loggerObj.logger.info("function correcting word ended" + loggerObj.getTime)
      correctedWord
    }
  }
}

object Main {
  val p = new Trie[Int]
  def main(args: Array[String]): Unit = {
    val path = "C:\\Users\\hai\\Desktop\\SpellChecker\\engmix.txt"
    val a = System.currentTimeMillis()
    val DictionaryList = readDictionary(path)
    println("Enter your input:")
    val inputString = scala.io.StdIn.readLine()
    println("Is given input is :" + inputString)
    val inputLowercase = inputString.toLowerCase.trim()
    val input = inputLowercase.replace(".", "")
    val s = input.split(" ")
    var correctInput = new ArrayBuffer[String]()
    var correctedInput = new ArrayBuffer[String]()
    var finalCorrect= new ArrayBuffer[String]()

    for (i <- 0 until s.length) {
      if (p.get(s(i)) != None) {
        correctInput +=s(i)
        finalCorrect+=correctInput.mkString(" ")
        print(finalCorrect.mkString)
       // println(correctInput.mkString(" "))
       // correctInput=correctInput +s(i)+ " "
      }
      // if (pre.get(s(i))==Some(1)) print(s(i))
      else {
         correctedInput  += p.correctingWord(s(i), DictionaryList)
         finalCorrect +=correctedInput.mkString(" ")
        print(finalCorrect.mkString)
        //println(correctedInput.mkString(" "))
      }
    }
//println(correctInput.mkString(" ") + " "+ correctedInput.mkString(" "))
 var c= finalCorrect.length
    print(finalCorrect.mkString)
    val b = System.currentTimeMillis()
   val diff = b - a
    print(diff)
  }

  /***reads the contents in the dictionary file and insert it into trie data structure
   *
   * @param path:dictionary path
   * @return dictionarywords in list
   */
  def readDictionary(path:String)={
    val loggerObj = new Logger()
    loggerObj.logger.info("function readdictionary started")
    loggerObj.startTime()
    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    // val a = System.currentTimeMillis()
    val lines = Source.fromFile(path).getLines().toList
    for (line <- lines.indices) {
      p.put(lines(line), 1)
    }
    loggerObj.stopTime()
    loggerObj.logger.info("function read dictionary ended" + loggerObj.getTime)
     lines
  }
}

