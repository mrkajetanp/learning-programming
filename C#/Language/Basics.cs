using System;
using System.Text;

namespace LanguageBasics {

  class Basics {
    public delegate void Del(string message);

    public static void DelegateMethod(string message) {
      Console.WriteLine(message);
    }

    static string getValueFromArray(string[] array, int index) {
      try {
        return array[index];
      }
      catch (System.IndexOutOfRangeException ex) {
        Console.WriteLine("Index is out of range: " + index);
        throw;
      }
    }

    static int Main() {
      string name = "Jane " + "Doe";
      Console.WriteLine(name);
      var test = 10;
      Console.WriteLine(test);
      var phrase = "ha";
      var manyPhrases = new StringBuilder();
      for (var i = 0 ; i < 50 ; i++) {
        manyPhrases.Append(phrase);
      }
      Console.WriteLine("Let's say " + manyPhrases);

      var text = "aoeuidhtns";
      foreach (var ch in text) {
        Console.Write(ch + " ");
      }
      Console.WriteLine();

      // string[] vowels1 = { "a", "e", "i", "o", "u"};
      var vowels2 = new string[] { "a1", "e", "i", "o", "u"};
      var vowels3 = new string[5];
      vowels3[0] = "a";
      vowels3[1] = "b";

      foreach (var vow in vowels2) {
        Console.Write(vow + " ");
      }
      Console.WriteLine();

      Del exampleDel2 = DelegateMethod;
      Del exampleDel3 = new Del(DelegateMethod);
      exampleDel2("test");
      exampleDel3("tested");

      // Console.WriteLine(getValueFromArray(vowels2, 8));

      // These object instantiations produce equivalent results
      // var instance1 = new ExampleClass();
      // ExampleClass instance2 = new ExampleClass();

      // You can use object inintializers to simplify object creation
      // var instance3 = new ExampleClass { Name = "Desktop", ID = 37414, Location = "Redmond"};
      /*
      Instead of:
      var instance3 = new ExampleClass();
      instance3.name = "Desktop";
      instance3.ID = 37414;
      etc.
       */

       // use lambdas to define event handlers

       // LINQ Queries
       // var seattleCustomers = from cust in customers where cust.City == "Seattle" select cust.Name;
       // var scoreQuery = from student in students
       //                  from score in student.Scores
       //                  where score > 90
       //                  select new { Last = student.LastName, score};

      return 0;
    }
  }
} 
