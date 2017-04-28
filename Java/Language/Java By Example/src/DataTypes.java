/**
 * Created by cajetan on 4/28/17.
 */

public class DataTypes {
    public static void run() {
        System.out.println("*** Data Types ***");

        strings();
        arrays();
    }

    private static void strings() {
        String strOne = "test" + " me";
        System.out.println("strOne = " + strOne);

        int age = 17;
        String strTwo = "I'm " + (age + 1) + " years old.";
        System.out.println("strTwo = " + strTwo);

        String s1 = "testString";
        String s2 = new String(s1);

        System.out.println(s1 + " equals " + s2 + " -> " + s1.equals(s2));
        System.out.println(s1 + " == " + s2 + " -> " + (s1 == s2));
    }

    private static void arrays() {
        int[] arrOne = new int[3];
        
        System.out.println("arrOne.length = " + arrOne.length);

        // arrays are initialized to a default value
        System.out.println("arrOne[1] = " + arrOne[1]);

        try {
            System.out.println("arrOne[10] = " + arrOne[10]);
        } catch (java.lang.ArrayIndexOutOfBoundsException e) {
            System.out.println("IndexOutOfBoundsException occurred!");
        }

        arrOne[0] = 2;
        arrOne[1] = 8;
        arrOne[2] = 35;

        for (int x : arrOne) {
            System.out.print(x + " ");
        }
        System.out.println("");

        int[][] arrTwo = new int[2][2];
        arrTwo[0][0] = 1;
        arrTwo[0][1] = 2;
        arrTwo[1][0] = 3;
        arrTwo[1][1] = 4;

        for (int[] a : arrTwo) {
            System.out.print("[ ");
            for (int x : a)
                System.out.print(x + " ");
            System.out.print("]");

            System.out.println("");
        }
        System.out.println("");

        // jagged arrays are possible, too

        int[][] arrThree = {
                {1, 2, 3},
                {4, 5},
                {7, 8, 9},
        };

        for (int[] a : arrThree) {
            System.out.print("[ ");
            for (int x : a)
                System.out.print(x + " ");
            System.out.print("]");

            System.out.println("");
        }
    }
}