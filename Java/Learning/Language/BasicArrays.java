
public class Arrays {
	public static void main (String[] args) {
		int[] intArray;	// defines a reference to an array
		intArray = new int[100];	// creates the array, and sets intArray to refer to it
		// int[] intArray = new int[100];
		System.out.println ("Length: " + intArray.length);
		intArray[3] = 32;
		System.out.println("Fourth element: " + intArray[3]);

		long[] arr = { 77, 99, 44, 55, 22, 88, 11, 00, 66, 33 };
		int nElems = 0; // number of items
		int i;	// loop counter
		long searchKey;	// key of item to search for
		nElems = 10;

		for (i = 0 ; i < nElems ; i++)
			System.out.print(arr[i] + " ");
		System.out.println("");

		searchKey = 66;	// find item with key 66

		for (i = 0 ; i < nElems ; i++)
			if (arr[i] == searchKey)
				break;
		if (i == nElems)
			System.out.println("Can't find " + searchKey);
		else
			System.out.println("Found " + searchKey);

		searchKey = 55;
		for (i = 0 ; i < nElems ; i++)
			if (arr[i] == searchKey)
				break;
		for (int j = i ; j < nElems-1 ; j++)
			arr[j] = arr[j+1];
		nElems--;

		for (i = 0 ; i < nElems ; i++)
			System.out.print(arr[i] + " ");
		System.out.println("");
	}
}