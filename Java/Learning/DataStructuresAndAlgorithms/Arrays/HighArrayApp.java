
class HighArray {
	private long[] arr;
	private int nItems;

	public HighArray (int max) {
		arr = new long[max];
		nItems = 0;
	}

	public boolean find (long searchKey) {
		int i;
		for (i = 0 ; i < nItems ; i++)
			if (arr[i] == searchKey)
				break;
		if (i == nItems)
			return false;
		else
			return true;
	}

	public void insert (long value) {
		arr[nItems] = value;
		nItems++;
	}

	public boolean delete (long value) {
		int i;
		for (i = 0 ; i < nItems ; i++)
			if ( value == arr[i] )
				break;
		if (i == nItems)
			return false;
		else {
			for (int j = i ; j < nItems ; j++)
				arr[j] = arr[j+1];
			nItems--;
			return true;
		}
	}

	public long getMax () {
		if (nItems == 0)
			return -1;
		long max = 0;
		for (int i = 0 ; i < nItems ; i++)
			if (arr[i] > max)
				max = arr[i];
		return max;
	}

	public long removeMax () {
		long maxVal = this.getMax();
		this.delete (maxVal);
		return maxVal;
	}

	public void display () {
		for (int i = 0 ; i < nItems ; i++)
			System.out.print(arr[i] + " ");
		System.out.println ("");
	}

	public void noDups () {
		for (int i = 0 ; i < nItems ; i++)
			for (int j = i+1 ; j < nItems ; j++)
				if (arr[j] == arr[i])
					arr[j] = -1;
		while (this.find(-1))
			this.delete(-1);
	}
}

public class HighArrayApp {
	public static void main (String[] args) {
		int maxSize = 100;
		HighArray arr = new HighArray (maxSize);

		arr.insert (77);
		arr.insert (99);
		arr.insert (44);
		arr.insert (55);
		arr.insert (22);
		arr.insert (88);
		arr.insert (11);
		arr.insert (00);
		arr.insert (66);
		arr.insert (66);
		arr.insert (33);

		arr.display ();
		System.out.println("Max Value: " + arr.getMax());

		int searchKey = 35;
		if (arr.find (searchKey))
	 		System.out.println ("Found " + searchKey);
		else
			System.out.println ("Can't find " + searchKey);

		arr.delete (00);
		arr.delete (55);
		arr.delete (99);

		arr.display ();
		System.out.println("Max Value: " + arr.getMax());
		System.out.println("Max Value: " + arr.removeMax());
		arr.display ();
		System.out.println("Max Value: " + arr.removeMax());
		arr.display ();
		arr.insert (22);
		arr.insert (66);
		arr.insert (11);
		arr.display ();
		arr.noDups();
		arr.display ();
	}
}
