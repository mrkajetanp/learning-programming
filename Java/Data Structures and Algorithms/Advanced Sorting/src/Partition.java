
class ArrayPar {
    private long[] theArray;
    private int nItems;

    public ArrayPar (int maxItems) {
        theArray = new long[maxItems];
        nItems = 0;
    }

    public void insert(long value) {
        theArray[nItems++] = value;
    }

    public int size() {
        return nItems;
    }

    public void display() {
        System.out.print("A = ");
        for (int i = 0 ; i < nItems ; ++i)
            System.out.print(theArray[i] + " ");
        System.out.println("");
    }

    public int partitionIt(int left, int right, long pivot) {
        int leftPtr = left-1;
        int rightPtr = right+1;

        while (true) {
            while (leftPtr < right && theArray[++leftPtr] < pivot) // find bigger item
                ;

            while (rightPtr > left && theArray[--rightPtr] > pivot) // find smaller item
                ;

            if (leftPtr >= rightPtr)
                break;
            else
                swap(leftPtr, rightPtr);
        }
        return leftPtr;
    }

    public void swap(int a, int b) {
        long temp = theArray[a];
        theArray[a] = theArray[b];
        theArray[b] = temp;
    }
}

class Partition {
    public static void main(String[] args) {
        int maxSize = 16;
        ArrayPar arr = new ArrayPar(maxSize);

        for (int i = 0 ; i < maxSize ; ++i)
            arr.insert((int)(java.lang.Math.random()*199));

        arr.display();
        long pivot = 99;
        System.out.println("Pivot is " + pivot);
        int size = arr.size();

        int partDex = arr.partitionIt(0, size-1, pivot);
        System.out.println("Partitioned at index " + partDex);
        arr.display();
    }
}


