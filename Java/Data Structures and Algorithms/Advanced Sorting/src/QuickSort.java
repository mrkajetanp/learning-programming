class ArrayIns {
    private long[] theArray;
    private int nItems;

    public ArrayIns(int max) {
        theArray = new long[max];
        nItems = 0;
    }

    public void insert(long value) {
        theArray[nItems] = value;
        nItems++;
    }

    public void display() {
        System.out.print("A = ");
        for (int i = 0 ; i < nItems ; ++i) {
            System.out.print(theArray[i] + " ");
        }
        System.out.println("");
    }

    public void quickSort() {
        recQuickSort(0, nItems-1);
    }

    public void recQuickSort(int left, int right) {
        if (right-left <= 0)
            return;
        else {
            long pivot = theArray[right];
            int partition = partitionIt(left, right, pivot);
            recQuickSort(left, partition-1);
            recQuickSort(partition+1, right);
        }
    }

    public int partitionIt(int left, int right, long pivot) {
        int leftPtr = left-1;
        int rightPtr = right;
        while (true) {
            while (theArray[++leftPtr] < pivot)
                ;

            while (rightPtr > 0 && theArray[--rightPtr] > pivot)
                ;

            if (leftPtr >= rightPtr)
                break;
            else
                swap(leftPtr, rightPtr);
        }
        swap(leftPtr, right); // restore pivot
        return leftPtr;
    }

    public void swap(int a, int b) {
        long temp = theArray[a];
        theArray[a] = theArray[b];
        theArray[b] = temp;
    }
}

public class QuickSort {
    public static void main(String[] args) {
        int maxSize = 16;
        ArrayIns arr = new ArrayIns(maxSize);
        for (int i = 0 ; i < maxSize ; ++i) {
            long n = (int)(java.lang.Math.random()*99);
            arr.insert(n);
        }
        arr.display();
        arr.quickSort();
        arr.display();
    }
}
