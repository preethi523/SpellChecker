import com.swabunga.spell.event.*;
import com.swabunga.spell.engine.*;

import java.io.*;
import java.util.*;

public class JazzyTest1 implements SpellCheckListener
{

  SpellChecker checker;
  ArrayList misspelled;

  public JazzyTest1()
  {
    createDictionary();

    FileWordTokenizer texTok = new FileWordTokenizer(new 
File("c:/Java/Jazzy/SourceCode/src/com/swabunga/test/spell/event/test.tex"),
        new TeXWordFinder());

    // how the heck does "misspelled" get populated? through the spellingError method? (possibly)
    misspelled = new ArrayList();

    checker.addSpellCheckListener(this);
    checker.checkSpelling(texTok);

    Iterator it = misspelled.iterator();
    while (it.hasNext())
    {
      System.out.println("misspelled: " + it.next());
    }
  }

  private void createDictionary()
  {
    File dict = new File("resources/english.0");
    try
    {
      checker = new SpellChecker(new SpellDictionaryHashMap(dict));
    }
    catch (FileNotFoundException e)
    {
      System.err.println("Dictionary File " + dict + " not found! " + e);
      System.exit(1);
    }
    catch (IOException ex)
    {
      System.err.println("IO problem: " + ex);
      System.exit(2);
    }
  }

  public void spellingError(SpellCheckEvent event)
  {
    event.ignoreWord(true);
    misspelled.add(event.getInvalidWord());
  }

  public static void main(String[] args)
  {
    new JazzyTest1();
  }

}