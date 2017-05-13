/**
 * Created by cajetan on 5/8/17.
 */

class Singleton {
    private final static Singleton instance = new Singleton();
    private static boolean isInitialized = false;

    private Singleton() {
        super();
    }

    private void init() {
        // initialization here
    }

    // only way to get a reference to the instance
    public static synchronized Singleton getInstance() {
        if (isInitialized)
            return instance;

        instance.init();
        isInitialized = true;
        return instance;
    }
}

class CircleD implements Comparable<CircleD> {
    private final int x;
    private final int y;
    private final int r;

    public CircleD(int x, int y, int r) {
        if (r < 0)
            throw new IllegalArgumentException("negative radius");

        this.x = x;
        this.y = y;
        this.r = r;
    }

    public CircleD(CircleD original) {
        x = original.x;
        y = original.getY();
        r = original.getR();
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public int getR() {
        return r;
    }

    @Override
    public String toString() {
        return String.format("center = (%d, %d) ; radius = %d", x, y, r);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;

        if (!(obj instanceof CircleD))
            return false;

        CircleD that = (CircleD) obj;
        return (this.x == that.getX() && this.y == that.getY() && this.r == that.getR());
    }

    @Override
    public int hashCode() {
        int result = 17;
        result = 37*result + x;
        result = 37*result + y;
        result = 37*result + r;
        return result;
    }

    @Override
    public int compareTo(CircleD o) {
        long result = (long) o.y - this.y;

        if (result == 0)
            result = (long) this.x - o.x;

        if (result == 0)
            result = (long) this.r - o.r;

        return Long.signum(result);
    }
}

public class ObjectOrientedDesign {
    public static void run() {
        System.out.println("*** Object-Oriented Design ***");
        CircleD test = new CircleD(2, 2, 3);
        System.out.println("test.toString(): " + test.toString());
    }
}
