
class OrdArray {
    private long[] a;
    private int nItems;

    public OrdArray (int max) {
        a = new long[max];
        nItems = 0;
    }

    public int size () {
        return nItems;
    }

    public int find (long searchKey) {
        return recFind (searchKey, 0, nItems-1);
    }

    private int recFind (long searchKey, int lowerBound, int upperBound) {
        int curIn = (lowerBound + upperBound) / 2;
        if (a[curIn] == searchKey) // found it
            return curIn;
        else if (lowerBound > upperBound)
            return nItems; // can't find it
        else {
            if (a[curIn] < searchKey) // it's in upper half
                return recFind (searchKey, curIn+1, upperBound);
            else // it's in lower half
                return recFind (searchKey, lowerBound, curIn-1);
        }
    }

    public void insert (long value) {
        int i;
        for (i = 0 ; i < nItems ; i++)
            if (a[i] > value)
                break;
        for (int j = nItems ; j > i ; j--) // move bigger ones up
            a[j] = a[j-1];
        a[i] = value;
        nItems++;
    }

    public void display () {
        for (int i = 0 ; i < nItems ; i++)
            System.out.print (a[i] + " ");
        System.out.println ("");
    }
}

public class BinarySearch {
    public static void main (String[] args) {
        int maxSize = 100;
        OrdArray arr = new OrdArray (maxSize);

        arr.insert (72);
        arr.insert (33);
        arr.insert (80);
        arr.insert (13);
        arr.insert (99);
        arr.insert (38);
        arr.insert (45);
        arr.insert (61);
        arr.insert (100);
        arr.insert (98);
        arr.insert (74);

        arr.display ();

        int searchKey = 27;
        if (arr.find(searchKey) != arr.size())
            System.out.println ("Found " + searchKey + "!");
        else
            System.out.println ("Can't find " + searchKey + "!");

        searchKey = 38;
        if (arr.find(searchKey) != arr.size())
            System.out.println ("Found " + searchKey + "!");
        else
            System.out.println ("Can't find " + searchKey + "!");

    }
}



















