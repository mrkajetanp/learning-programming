
class ArraySh {
    private long[] theArray;
    private int nItems;

    public ArraySh(int max) {
        theArray = new long[max];
        nItems = 0;
    }

    public void insert(long value) {
        theArray[nItems] = value;
        nItems++;
    }

    public void display() {
        System.out.print("A = ");
        for (int i = 0 ; i < nItems ; ++i)
            System.out.print(theArray[i] + " ");
        System.out.println("");
    }

    public void shellSort() {
        int inner;
        int outer;
        long temp;

        int h = 1;
        while (h <= nItems/3)
            h = h*3+1; // (1, 4, 13, 40, 121, ...)

        while (h > 0) {
            for (outer = h ; outer < nItems ; ++outer) { // h-sort the array
                temp = theArray[outer];
                inner = outer;

                while (inner > h-1 && theArray[inner-h] >= temp) {
                    theArray[inner] = theArray[inner-h];
                    inner -= h;
                }
                theArray[inner] = temp;
            }
            h = (h-1) / 3; // decrease h
        }
    }
}

public class ShellSort {
    public static void main(String[] args) {
        int maxSize = 10;
        ArraySh arr = new ArraySh(maxSize);

        for (int i = 0 ; i < maxSize ; ++i) {
            long n = (int)(java.lang.Math.random()*99);
            arr.insert(n);
        }
        arr.display();
        arr.shellSort();
        arr.display();
    }
}
