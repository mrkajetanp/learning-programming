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

public class TheJavaTypeSystem {
    public static void run() {
        System.out.println("*** The Java Type System ***");
    }
}











