
public class Factorials {
    static int factorial (int n) {
        if (n == 1)
            return 1;
        else
            return n * factorial (n-1);
    }

    public static void main (String[] args) {
        System.out.println (factorial (4));
        System.out.println (factorial (5));
        System.out.println (factorial (6));
    }
}
