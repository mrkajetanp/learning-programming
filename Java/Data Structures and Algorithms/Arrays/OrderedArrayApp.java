import java.lang.Math.*;

class OrderedArray {
	private long[] arr;
	private int nItems;

	public OrderedArray (int max) {
		arr = new long[max];
		nItems = 0;
	}

	public int size () {
		return nItems;
	}

	public int find (long searchKey) {
		int lowerBound = 0;
		int upperBound = nItems-1;
		int curIn;

		while (true) {
			curIn = (lowerBound + upperBound) / 2;
			if (arr[curIn] == searchKey)
				return curIn;
			else if (lowerBound > upperBound)
				return -1;
			else {
				if (arr[curIn] < searchKey)
					lowerBound = curIn + 1;
				else
					upperBound = curIn - 1;
			}
		}
	}

	public void insert (long value) {
		int i;
		for (i = 0 ; i < nItems ; i++)
			if (arr[i] > value)
				break;

		for (int j = nItems ; j > i ; j--)
			arr[j] = arr[j-1];
		arr[i] = value;
		nItems++;
	}

	public boolean delete (long value) {
		int i = find (value);
		if (i == -1)
			return false;
		else {
			for (int j = i ; j < nItems ; j++)
				arr[j] = arr[j+1];
			nItems--;
			return true;
		}
	}

	public void display () {
		for (int i = 0 ; i < nItems ; i++)
			System.out.print(arr[i] + " ");
		System.out.println("");
	}

	public long at (int index) {
		return arr[index];
	}

	public OrderedArray merge (OrderedArray two) {
		OrderedArray result = new OrderedArray (this.size() + two.size() + 5);
		int counter = Math.min (this.size(), two.size());
		int i;

		for (i = 0 ; i < counter ; i++)
			if (this.at(i) < two.at(i)) {
				result.insert (this.at(i));
				result.insert (two.at(i));
			}
			else {
				result.insert (two.at(i));
				result.insert (this.at(i));
			}
		for ( ; i < Math.max (this.size(), two.size()) ; i++)
			result.insert (this.at(i));

		return result;
	}
}

class OrderedArrayApp {
	public static void main (String[] args) {
		int maxSize = 100;
		OrderedArray arr = new OrderedArray (maxSize);
		OrderedArray arr2 = new OrderedArray (maxSize);

		arr.insert (77);
		arr.insert (99);
		arr.insert (44);
		arr.insert (55);
		arr.insert (22);
		arr.insert (88);
		arr.insert (11);
		arr.insert (00);
		arr.insert (66);
		arr.insert (33);

		arr2.insert (24);
		arr2.insert (18);
		arr2.insert (58);
		arr2.insert (93);
		arr2.insert (71);
		arr2.insert (32);

		int searchKey = 55;
		if ( arr.find (searchKey) != -1 )
			System.out.println("Found " + searchKey);
		else
			System.out.println("Couldn't find " + searchKey);

		System.out.println("Size: " + arr.size());
		arr.display ();

		arr.delete (00);
		arr.delete (55);
		arr.delete (99);

		System.out.println("Size: " + arr.size());
		arr.display ();

		System.out.println("At 5: " + arr.at(5));

		arr.merge (arr2).display ();
	}
}