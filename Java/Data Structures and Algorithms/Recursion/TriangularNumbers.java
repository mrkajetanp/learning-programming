
public class TriangularNumbers {
    public static int triangle (int n) {
        if (n == 1)
            return 1;
        else
            return (n + triangle (n-1));
    }

    public static int triangleFormula (int n) {
        return ((n*n+n)/2);
    }

    public static void main (String[] args) {
        System.out.println (triangle (4));
        System.out.println (triangleFormula (4));

        System.out.println (triangle (6));
        System.out.println (triangleFormula (6));

        System.out.println (triangle (1000));
        System.out.println (triangleFormula (1000));
    }
}

