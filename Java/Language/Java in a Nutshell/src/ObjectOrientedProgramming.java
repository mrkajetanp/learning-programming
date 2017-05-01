/**
 * Created by cajetan on 4/22/17.
 */

class Circle {
    public static final double PI = 3.14159;

    // an instance field with default value
    protected double radius = 1.0;

    protected void checkRadius(double radius) {
        if (radius < 0.0)
            throw new IllegalArgumentException("radius may not be negative");
    }

    public Circle() {
        this(1.0);
    }

    // a constructor calling another constructor
    public Circle(double radius) {
        checkRadius(radius);
        this.radius = radius;
    }

    public double getRadius() {
        return radius;
    }

    public void setRadius(double radius) {
        checkRadius(radius);
        this.radius = radius;
    }

    public static double radiansToDegrees(double radians) {
        return radians * 180 / PI;
    }

    public double area() {
        return PI * radius * radius;
    }

    public double circumference() {
        return 2 * PI * radius;
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

        return (distance < radius);
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
    private int a = 10;
    protected int b = 15;

    int f() {
        return i;
    }
}

class B extends A {
    int i;

    int getB() {
        return this.b;
    }

    int f() {
        i = super.i + 1;

        return super.f() + i;
    }
}

abstract class MyShape {
    public abstract double area();
    public abstract double circumference();
}

class MyCircle extends MyShape {
    public static final double PI = 3.14159265358979;
    protected double radius;

    public MyCircle(double radius) {
        this.radius = radius;
    }

    public double getRadius() {
        return radius;
    }

    public double area() {
        return PI * radius * radius;
    }

    public double circumference() {
        return 2 * PI * radius;
    }
}

class MyRectangle extends MyShape {
    protected double width;
    protected double height;

    public MyRectangle(double width, double height) {
        this.width = width;
        this.height = height;
    }

    public double getWidth() {
        return width;
    }

    public double getHeight() {
        return height;
    }

    public double area() {
        return width * height;
    }

    public double circumference() {
        return 2 * (width + height);
    }
}

public class ObjectOrientedProgramming {

    private static void typeConversions() {
        Object o = "string";
        String s = (String) o;
        System.out.println("s = " + s);

        Object o2 = new int[] {1, 2, 3};
        int[] a = (int[]) o2;
        System.out.println("a[0] = " + a[0]);
    }

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

        A testA = new A();
        System.out.println("testA.b = " + testA.b);
        B testB = new B();
        System.out.println("testB.b = " + testB.b);
        System.out.println("testB.getB() = " + testB.getB());

        MyShape[] shapes = new MyShape[3];
        shapes[0] = new MyCircle(2.0);
        shapes[1] = new MyRectangle(1.0, 3.0);
        shapes[2] = new MyRectangle(4.0, 2.0);

        double totalArea = 0;
        for (int i = 0 ; i < shapes.length ; i++)
            totalArea += shapes[i].area();

        System.out.println("totalArea = " + totalArea);

        typeConversions();

        System.out.println("");
    }
}
