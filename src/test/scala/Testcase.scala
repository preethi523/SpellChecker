
import org.scalatest.funsuite.AnyFunSuite

class Testcase extends AnyFunSuite {
  val spellCheckerObj= new SpellChecker
  val trieObj = new Trie[Int]
  val correctPath = "/home/preethia/Desktop/SpellChecker/testdictionary.txt"
  val path = "/home/preethia/Desktop/SpellChecker/testdictionar.txt"
  trieObj.put("apple", 1)

  test(" read dictionary") {
    assert(spellCheckerObj.readDictionary(correctPath) == List("a", "abbey", "abbeys", "abbot", "abjurer", "ablatives", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
    )
    assert(spellCheckerObj.readDictionary(path) == List("file not found exception"))
  }

  test("get word") {
    assert(trieObj.get("apple") == Some(1))
    assert(trieObj.get("hi") == None
    )
  }

  test("correcting words") {
    assert(spellCheckerObj.correctingWord("abbat", List("a", "abbot", "abjurer", "abjurersblative", "ablatives", "ablaut", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
    ) == "abbot")
    assert(spellCheckerObj.correctingWord("a", List("a", "abbot", "abjurer", "ablatives", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
    ) == "")
    assert(spellCheckerObj.correctingWord("abb", List("a", "abbey", "abbeys", "abbot", "abjurer", "ablatives", "ablaut", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
    ) == "a")
  }

  test("check the word") {
    assert(spellCheckerObj.checkWords("ablom", List("a", "abbey", "abbeys", "abbot", "abjurer", "abjurersblative", "ablatives", "ablaut", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
    ) == "abloom")
    assert(spellCheckerObj.checkWords("ablutio", List("a", "abbey", "abbeys", "abbot", "abjurer", "abjurersblative", "ablatives", "ablaut", "ablaze", "able", "abler", "ablest", "abloom", "ablutions")
    ) == "ablutions")
    assert(spellCheckerObj.checkWords("hi", List()) == "empty input")
  }
}
