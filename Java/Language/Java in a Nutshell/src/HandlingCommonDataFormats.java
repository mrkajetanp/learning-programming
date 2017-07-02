import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Created by cajetan on 02/07/2017.
 */
public class HandlingCommonDataFormats {
    public static void run() {
        System.out.println("*** Handling Common Data Formats ***");

        strings();
        numbersAndMaths();

        System.out.println();
    }

    static void strings() {
        // string literals

        System.out.println("Dog's length is: " + "dog".length());

        // string concatenation

        String a = "one";
        String b = "two";
        String c = a + " " + b;

        System.out.println(c);

        // regex

        Pattern p = Pattern.compile("honou?r");
        Matcher mUK = p.matcher("For Brutus is an honourable man.");
        Matcher mUS = p.matcher("For Brutus is an honorable man.");

        System.out.println("Matches UK spelling? " + mUK.find());
        System.out.println("Matches US spelling? " + mUS.find());

        // matches both "minimize" and "minimise"
        // Pattern.compile("minimi[sz]e");

        p = Pattern.compile("\\d");
        Matcher m = p.matcher("Apollo 13");
        System.out.println("matches?: " + m.find());
        System.out.println("group?: " + m.group());

        // using regexps with collections
        String[] inputs = { "Cat", "Dog", "Ice-9", "99 Luftballoons" };
        List<String> ls = Arrays.asList(inputs);
        List<String> withDigits = ls.stream().filter(p.asPredicate()).collect(Collectors.toList());
        System.out.println(withDigits);
    }

    static void numbersAndMaths() {
        // number's byte representation - two's complement
        byte b = (byte) 0b1111_1111; // -1
        System.out.println("b = " + b);
        b++;
        System.out.println("b = " + b);

        b = (byte) 0b1111_1110; // -2
        System.out.println("b = " + b);
        b++;
        System.out.println("b = " + b);

        b = (byte) 0b1000_0000; // -128
        System.out.println("b = " + b);

        // floating-point numbers

        double d = 0.3;
        double d2 = 0.2;
        System.out.println("d = " + d + ", d2 = " + d2);
        System.out.println("(d2 - d) = " + (d2-d));

        BigDecimal bd = new BigDecimal(d);
        System.out.println("bd = " + bd);

        bd = new BigDecimal(0.3);
        System.out.println("bd = " + bd);

        bd = new BigDecimal(BigInteger.ONE);
        try {
            bd.divide(new BigDecimal(3.0));
        } catch (ArithmeticException e) {
            System.err.println("Well it wasn't the brightest idea today..");
        }
        System.out.println("bd = " + bd);
    }
}














