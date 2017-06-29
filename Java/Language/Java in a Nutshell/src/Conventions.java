/**
 * Created by cajetan on 5/31/17.
 */

// A properly doc-commented class
/**
 * This class represents <i>complex numbers</i>
 *
 * @author Cajetan Puchalski
 * @version 1.0
 */
class Complex {
    /**
     * Holds the real part of this complex number.
     * @see #y
     */
    protected double x;

    /**
     * Holds the imaginary part of this complex number.
     * @see #x
     */
    protected double y;

    /**
     * Creates a new Complex object that represents the complex number
     * x+yi.
     * @param x The real part of the complex number.
     * @param y The imaginary part of the complex number.
     */
    public Complex(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Adds two Complex objects and produces a third object that
     * represents their sum.
     * @param c1 A Complex object
     * @param c2 Another Complex object
     * @return A new complex object that represents the sum of
     * <code>c1</code> and <code>c2</code>.
     * @exception java.lang.NullPointerException
     * If either argument is <code>null</code>.
     */
    public static Complex add(Complex c1, Complex c2) {
        return new Complex(c1.x + c2.x, c1.y + c2.y);
    }
}

public class Conventions {
    public static void run() {
        System.out.println("*** Conventions ***");

        System.out.println();
    }
}
