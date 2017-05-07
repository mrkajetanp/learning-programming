import com.sun.istack.internal.NotNull;
import sun.awt.image.ImageWatched;

import java.io.File;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;

/**
 * Created by cajetan on 30/04/2017.
 */

interface Centered {
    void setCenter(double x, double y);
    double getCenterX();
    double getCenterY();
}

// an interface can extend multiple interfaces

interface Positionable extends Centered {
    void setUpperRightCorner(double x, double y);
    double getUpperRightX();
    double getUpperRightY();
}

class Rectangle {
    int height;
    int width;

    public Rectangle(int height, int width) {
        this.height = height;
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public int getWidth() {
        return width;
    }

    public int area() {
        return height * width;
    }

    public int circumference() {
        return height*2 + width*2;
    }
}

// implementing an interface

class CenteredRectangle extends Rectangle implements Centered {
    private double cx, cy;

    public CenteredRectangle(int height, int width, double cx, double cy) {
        super(height, width);
        this.cx = cx;
        this.cy = cy;
    }


    public void setCenter(double x, double y) {
        cx = x;
        cy = y;
    }

    public double getCenterX() {
        return cx;
    }

    public double getCenterY() {
        return cy;
    }
}

interface Box<T> {
    void box(T t);
    T unbox();
}

class Utils {
    public static <T> T comma(T a, T b) {
        return a;
    }
}

enum PrimaryColor {
    RED, GREEN, BLUE
}

class Shape {
    public double area() {
        return 2.0;
    }
}

class Triangle extends Shape {
    public Triangle(int one, int two, int three, int aOne, int aTwo, int aThree) {

    }
}

class MyRectangle2 extends Shape {
    public MyRectangle2(int a, int b) {

    }
}

enum RegularPolygon {
    TRIANGLE(3), SQUARE(4), PENTAGON(5), HEXAGON(6);

    private Shape shape;

    public Shape getShape() {
        return shape;
    }

    RegularPolygon(int sides) {
        switch (sides) {
            case 3:
                shape = new Triangle(1, 1, 1, 60, 60, 60);
                break;
            case 4:
                shape = new MyRectangle2(10, 20);
                break;
            case 5:
                // ...
                break;
            case 6:
                // ...
                break;
        }
    }
}

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@interface Nickname {
    String[] value() default {};
}

class TestAnnotation {
    @Nickname(value = "heyFunction")
    public static void testAnnotation() {
    }
}

// Nested Types - Static members

class LinkedStack {

    // static member interface determines how objects are linked
    interface Linkable {
        Linkable getNext();
        void setNext(Linkable node);
    }

    // head of the list
    Linkable head;

    public void push(Linkable node) {
        // ...
    }

    public Object pop() {
        // ...
        return new Object();
    }
}

class LinkableInteger implements LinkedStack.Linkable {
    int i;

    public LinkableInteger(int i) {
        this.i = i;
    }

    LinkedStack.Linkable next;

    public LinkedStack.Linkable getNext() {
        return next;
    }

    public void setNext(LinkedStack.Linkable node) {
        next = node;
    }
}

// non-static member class

class LinkedStackNSMC {

    // our static member interface
    public interface Linkable {
        public Linkable getNext();
        public void setNext(Linkable node);
    }

    // head of the list
    private Linkable head;

    public void push(Linkable node) {
        // ...
    }

    public Linkable pop() {
        // ...
        return head;
    }

    // method returns an Iterator object for this LinkedStack
    public Iterator<Linkable> iterator() {
        return new LinkedIterator();
    }

    // implementation of the Iterator interface
    // defined as a non-static member class
    protected class LinkedIterator implements Iterator<Linkable> {
        Linkable current;

        public LinkedIterator() {
            current = head;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public Linkable next() {
            if (current == null)
                throw new java.util.NoSuchElementException();

            Linkable value = current;
            current = current.getNext();
            return value;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}

// using a local class
class LinkedStackLC {
    // our static member interface
    public interface Linkable {
        public Linkable getNext();
        public void setNext(Linkable node);
    }

    // head of the list
    private Linkable head;

    public void push(Linkable node) {
        // ...
    }

    public Linkable pop() {
        // ...
        return head;
    }

    public Iterator<Linkable> Iterator() {

        // definition of LinkedIterator as a local class
        class LinkedIterator implements Iterator<Linkable> {
            Linkable current;

            // constructor uses a private field of the containing class
            public LinkedIterator() {
                current = head;
            }

            @Override
            public boolean hasNext() {
                return current != null;
            }

            @Override
            public Linkable next() {
                if (current == null)
                    throw new java.util.NoSuchElementException();

                Linkable value = current;
                current = current.getNext();
                return value;
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        }

        return new LinkedIterator();
    }
}

// with an anonymous class
class LinkedStackAC {
    // our static member interface
    public interface Linkable {
        public Linkable getNext();
        public void setNext(Linkable node);
    }

    // head of the list
    private Linkable head;

    public void push(Linkable node) {
        // ...
    }

    public Linkable pop() {
        // ...
        return head;
    }

    public Iterator<Linkable> iterator() {

        return new Iterator<Linkable>() {
            Linkable current;

            // instance initializer instead of a constructor
            {
                current = head;
            }

            @Override
            public boolean hasNext() {
                return current != null;
            }

            @Override
            public Linkable next() {
                if (current == null)
                    throw new java.util.NoSuchElementException();

                Linkable value = current;
                current = current.getNext();
                return value;
            }
        };
    }
}

class Weird {
    public static interface IntHolder {
        public int getValue();
    }

    public static void run() {
        IntHolder[] holders = new IntHolder[10];
        for (int i = 0 ; i < 10 ; i++) {
            final int fi = i;

            // A local class
            class MyIntHolder implements IntHolder {
                @Override
                public int getValue() {
                    return fi;
                }
            }

            holders[i] = new MyIntHolder();
        }

        // local class is now out of scope
        // we have 10 valid instances of that class in an array
        // local variable fi is still in scope for all of these instances
        // we can call getValue() on every one of them
        System.out.println("Weird instances: ");
        for (int i = 0 ; i < 10 ; i++) {
            System.out.print(holders[i].getValue() + " ");
        }
        System.out.println("");
    }
}

class Lambdas {

    interface MathOperation {
        int operation(int a, int b);
    }

    private static int operate(int a, int b, MathOperation mathOperation) {
        return mathOperation.operation(a, b);
    }

    public static void run() {
        File dir = new File("src");
        String[] fileList = dir.list((f, s) -> s.endsWith(".java"));

        MathOperation addition = (int a, int b) -> a + b;
        MathOperation subtraction = (a, b) -> a - b;

        System.out.println("10 + 5 = " + operate(10, 5, addition));
        System.out.println("10 - 5 = " + operate(10, 5, subtraction));
    }
}

public class TheJavaTypeSystem {
    public static void run() {
        System.out.println("*** The Java Type System ***");

        // ?? @NotNull String = "I'm sure I'm not null..";

        generics();
        Weird.run();

        Lambdas.run();
    }

    static void generics() {
        // List<CenteredRectangle> shapes = new ArrayList<CenteredRectangle>();
        List<CenteredRectangle> shapes = new ArrayList<>();
        shapes.add(new CenteredRectangle(2, 4, 1, 1));
        shapes.add(new CenteredRectangle(4, 8, 3, 2));

        System.out.println("shapes.get(0).area() = " + shapes.get(0).area());
        System.out.println("shapes.get(1).area() = " + shapes.get(1).area());

        ArrayList<String> myList = new ArrayList<>();
        myList.add("one");
        myList.add("two");

        ArrayList<?> mysteryList = myList;
        System.out.println("(String)mysteryList.get(0) = " + mysteryList.get(0));

        // need casting to insert anything other than a null
        ((ArrayList<String>)mysteryList).add("test_test");
        System.out.println("(String)mysteryList.get(2) = " + mysteryList.get(2));
    }
}
