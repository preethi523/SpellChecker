//import Trie.Trie
//import org.scalatest.funsuite.AnyFunSuite
//class Testcase  extends  AnyFunSuite{
//
//  val trieObj = new Trie[Int]
//  val correctPath="/home/preethia/Desktop/SpellChecker/testdictionary.txt"
//  val path="/home/preethia/Desktop/SpellChecker/testdictionar.txt"
//  trieObj.put("apple",1)
//
//  test(" read dictionary") {
//    assert(Main.readDictionary(path) == List("file not found exception"))
//    assert(Main.readDictionary(correctPath) == List("a", "abbey", "abbeys", "abbot", "abjurer", "ablatives", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
//    )
//  }
//
//    test("get word"){
//      assert(trieObj.get("apple")==Some(1))
//      assert(trieObj.get("hi")==None
//     )
//    }
//    test("correcting words")
//  {assert(spellObj.correctingWord("abbat",List("a", "abbot", "abjurer", "abjurersblative", "ablatives", "ablaut", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
//  )=="abbot")
//   assert(spellObj.correctingWord("a",List("a", "abbot", "abjurer", "ablatives", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
//   )=="")
//    assert(spellObj.correctingWord("abb",List("a", "abbey", "abbeys", "abbot", "abjurer", "ablatives", "ablaut", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
//    )=="a")
//}
// test("check the word"){
//
//   assert(spellObj.checkString("ablom",List("a", "abbey", "abbeys", "abbot", "abjurer", "abjurersblative", "ablatives", "ablaut", "ablaze", "able", "abler", "ablest", "abloom", "ablution", "ablutions")
//   )=="abloom")
//   assert(spellObj.checkString("ablutio",List("a", "abbey", "abbeys", "abbot", "abjurer", "abjurersblative", "ablatives", "ablaut", "ablaze", "able", "abler", "ablest", "abloom", "ablutions")
//   )=="ablutions")
//  assert(spellObj.checkString("hi",List())== "empty input")
// }
//
//
//}
