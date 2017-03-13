class ArrayIns2 {
    private long[] theArray;
    private int nItems;

    public ArrayIns2(int max) {
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

    public void quickSort() {
        recQuickSort(0, nItems-1);
        // insertionSort(0, nItems-1); // the other option
    }

    public void recQuickSort(int left, int right) {
        int size = right-left+1;
        if (size < 10) // sort by insertion sort if array is small
            insertionSort(left, right);
        else {
            long median = medianOf3(left, right);
            int partition = partitionIt(left, right, median);
            recQuickSort(left, partition-1);
            recQuickSort(partition+1, right);
        }
    }

    public long medianOf3(int left, int right) {
        int center = (left+right)/2;

        if (theArray[left] > theArray[center]) // order left & center
            swap(left, center);

        if (theArray[left] > theArray[right]) // order left & right
            swap(left, right);

        if (theArray[center] > theArray[right])
            swap(center, right);

        swap(center, right-1); // put pivot on the right
        return theArray[right-1];
    }

    public void swap(int dex1, int dex2) {
        long temp = theArray[dex1];
        theArray[dex1] = theArray[dex2];
        theArray[dex2] = temp;
    }

    public int partitionIt(int left, int right, long pivot) {
        int leftPtr = left;
        int rightPtr = right-1;
        while (true) {
            while (theArray[++leftPtr] < pivot)
                ;
            while (theArray[--rightPtr] > pivot)
                ;
            if (leftPtr >= rightPtr)
                break;
            else
                swap(leftPtr, rightPtr);
        }
        swap(leftPtr, right-1);
        return leftPtr;
    }

    public void insertionSort(int left, int right) {
        int in, out;
        for (out = left+1 ; out <= right ; ++out) {
            long temp = theArray[out];
            in = out;
            while (in>left && theArray[in-1] >= temp) {
                theArray[in] = theArray[in-1]; // shift item to the right
                --in;
            }
            theArray[in] = temp;
        }
    }
}

class QuickSort3 {
    public static void main(String[] args) {
        int maxSize = 16;
        ArrayIns2 arr = new ArrayIns2(maxSize);

        for (int i = 0 ; i < maxSize ; ++i) {
            long n = (int)(java.lang.Math.random()*99);
            arr.insert(n);
        }
        arr.display();
        arr.quickSort();
        arr.display();
    }
}
