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


public class TheJavaTypeSystem {
    public static void run() {
        System.out.println("*** The Java Type System ***");

        generics();
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
