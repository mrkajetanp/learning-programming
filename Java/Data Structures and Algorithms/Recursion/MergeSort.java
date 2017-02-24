
class DArray {
    private long[] theArray;
    private int nItems;

    public DArray (int max) {
        theArray = new long[max];
        nItems = 0;
    }

    public void insert (long value) {
        theArray[nItems++] = value;
    }

    public void display () {
        for (int i = 0 ; i < nItems ; ++i)
            System.out.print (theArray[i] + " ");
        System.out.println ("");
    }

    public void mergeSort () {
        long[] workspace = new long[nItems];
        recMergeSort (workspace, 0, nItems-1);
    }

    private void recMergeSort (long[] workSpace, int lowerBound, int upperBound) {
        if (lowerBound == upperBound) // if range is 1
            return;
        else {
            int mid = (lowerBound+upperBound)/2;
            recMergeSort (workSpace, lowerBound, mid); // sort low half
            recMergeSort (workSpace, mid+1, upperBound); // sort high half
            merge (workSpace, lowerBound, mid+1, upperBound); // merge them
        }
    }

    private void merge(long[] workSpace, int lowPtr, int highPtr, int upperBound) {
        int i = 0; // workspace index
        int lowerBound = lowPtr;
        int mid = highPtr-1;
        int n = upperBound-lowerBound+1; // number of items

        while (lowPtr <= mid && highPtr <= upperBound) {
            if (theArray[lowPtr] < theArray[highPtr])
                workSpace[i++] = theArray[lowPtr++];
            else
                workSpace[i++] = theArray[highPtr++];
        }

        while (lowPtr <= mid)
            workSpace[i++] = theArray[lowPtr++];

        while (highPtr <= upperBound)
            workSpace[i++] = theArray[highPtr++];

        for (i = 0 ; i < n ; ++i)
            theArray[lowerBound+i] = workSpace[i];
    }
}


public class MergeSort {
    public static void main (String[] args) {
        int maxSize = 100;
        DArray arr = new DArray (maxSize);

        arr.insert (64);
        arr.insert (21);
        arr.insert (33);
        arr.insert (70);
        arr.insert (12);
        arr.insert (85);
        arr.insert (44);
        arr.insert (3);
        arr.insert (99);
        arr.insert (0);
        arr.insert (108);
        arr.insert (36);

        arr.display ();

        arr.mergeSort ();

        arr.display ();
    }
}




