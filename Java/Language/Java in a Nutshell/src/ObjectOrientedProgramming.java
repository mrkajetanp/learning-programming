/**
 * Created by cajetan on 4/22/17.
 */

class Circle {
    public static final double PI = 3.14159;

    // an instance field with default value
    protected double r = 1.0;

    public Circle() {
        this(1.0);
    }

    // a constructor calling another constructor
    public Circle(double r) {
        this.r = r;
    }

    public static double radiansToDegrees(double radians) {
        return radians * 180 / PI;
    }

    public double area() {
        return PI * r * r;
    }

    public double circumference() {
        return 2 * PI * r;
    }
}

class PlaneCircle extends Circle {
    private final double cx, cy;

    public PlaneCircle(double r, double x, double y) {
        super(r); // constructor of the superclass
        this.cx = x;
        this.cy = y;
    }

    public double getCentreX() {
        return cx;
    }

    public double getCentreY() {
        return cy;
    }

    public boolean isInside(double x, double y) {
        double dx = x - cx;
        double dy = y - cy;
        double distance = Math.sqrt(dx*dx + dy*dy);

        return (distance < r);
    }
}

class TrigCircle {
    private static final int NUMPTS = 500;
    private static double[] sines = new double[NUMPTS];
    private static double[] cosines = new double[NUMPTS];

    // a static initializer that fills in the arrays
    static {
        double x = 0.0;
        double delta_x = (Circle.PI/2)/(NUMPTS-1);
        for (int i = 0 ; i < NUMPTS ; i++, x += delta_x) {
            sines[i] = Math.sin(x);
            cosines[i] = Math.cos(x);
        }
    }

    // an instance initializer that does, well, nothing
    {
        // ...
    }

    // rest is omitted
}

class A {
    int i = 1;
    int f() {
        return i;
    }
}

class B extends A {
    int i;
    int f() {
        i = super.i + 1;

        return super.f() + i;
    }
}

public class ObjectOrientedProgramming {
    public static void run() {
        System.out.println("*** Object Oriented Programming ***");

        System.out.println("Circle.PI = " + Circle.PI);
        System.out.println("Circle.radiansToDegrees(4) = " + Circle.radiansToDegrees(4));
        System.out.println("Circle.radiansToDegrees(5) = " + Circle.radiansToDegrees(5));

        Circle c1 = new Circle();
        Circle c2 = new Circle(2.0);

        System.out.println("c1.area() = " + c1.area());
        System.out.println("c1.circumference() = " + c1.circumference());

        System.out.println("c2.area() = " + c2.area());
        System.out.println("c2.circumference() = " + c2.circumference());

        PlaneCircle pc = new PlaneCircle(1.0, 0.0, 0.0);
        // conversion possible, pc is still a Circle object
        Circle c = pc;
        // con't do the opposite though

        System.out.println("");
    }
}
