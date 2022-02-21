import Trie.Trie
import com.github.vickumar1981.stringdistance.StringConverter._

import java.nio.charset.CodingErrorAction
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
    def editDistance(s: String, dic: String): Int = {
      val damerauDist: Int = s.damerauDist(dic)
      damerauDist
    }
    def spellCorrection(s: String, path: String) = {
      implicit val codec = Codec("UTF-8")
      codec.onMalformedInput(CodingErrorAction.REPLACE)
      codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
      val lines = Source.fromFile(path).getLines().toList
      for (line <- lines.indices) {

       if( editDistance(s, lines(line))==1) print(lines(line)+" ")

       //else print(editDistance(s,lines(line)))
      }

    }
  }
}
  object Main {
    def main(args: Array[String]): Unit = {
      val p = new Trie[Int]
      val path = "C:\\Users\\hai\\Desktop\\engmix.txt"
      implicit val codec = Codec("UTF-8")
      codec.onMalformedInput(CodingErrorAction.REPLACE)
      codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
      val a = System.currentTimeMillis()
      val lines = Source.fromFile(path).getLines().toList
      for (line <- lines.indices) {
        // println(lines(line))
        (p.put(lines(line), 1))
      }
      val inputString = "This is an Apple. It is good for health. An Appple a day keeps doctor awau"
      val input1 = inputString.toLowerCase.trim()
       val input =input1.replace(".","")
      val s = input.split(" ")
      for (i <- 0 until s.length) {
        if (p.get(s(i)) != None) print(s(i) + " ")
        else p.spellCorrection(s(i), path)
      }
      val b = System.currentTimeMillis()
      val diff = b - a
      print(diff)
    }
}
