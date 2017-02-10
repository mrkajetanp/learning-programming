import java.util.concurrent.ThreadLocalRandom;

class ArraySorts {
	private long[] arr;
	private int nItems;

	public ArraySorts (int max) {
		arr = new long[max];
		nItems = 0;
	}

	public void insert (long value) {
		arr[nItems] = value;
		nItems++;
	}

	public void display () {
		for (int i = 0 ; i < nItems ; i++)
			System.out.print(arr[i] + " ");
		System.out.println("");
	}

	public void bubbleSort () {
		for (int i = nItems-1 ; i > 1 ; i--)
			for (int j = 0 ; j < i ; j++)
				if (arr[j] > arr[j+1])
					swap (j, j+1);

	}

	public void selectionSort () {
		int min;
		for (int i = 0 ; i < nItems-1 ; i++) {
			min = i;
			for (int j = i+1 ; j < nItems ; j++ )
				if (arr[j] < arr[min])
					min = j;
			swap (i, min);
		}
	}

	public void insertionSort () {
		int i, j;
		for (i = 0 ; i < nItems ; i++) {
			long temp = arr[i];
			j = i;
			while ((j > 0) && (arr[j-1] >= temp)) {
				arr[j] = arr[j-1];
				j--;
			}
			arr[j] = temp;
		}
	}

	public void scramble () {
		for (int i = 0 ; i < nItems ; i++)
			arr[i] = ThreadLocalRandom.current().nextInt(0, 100 + 1);
	}

	private void swap (int a, int b) {
		long temp = arr[a];
		arr[a] = arr[b];
		arr[b] = temp;
	}
}

public class SortingArrays {
	public static void main (String[] args) {

		int maxSize = 100;
		ArraySorts arr = new ArraySorts (maxSize);

		arr.insert (77);
		arr.insert (33);
		arr.insert (99);
		arr.insert (44);
		arr.insert (130);

		System.out.println("Bubble Sort: ");
		arr.display();
		arr.bubbleSort();
		arr.display();
		System.out.println("");

		System.out.println("Selection Sort: ");
		arr.scramble();
		arr.display();
		arr.selectionSort();
		arr.display();
		System.out.println("");


		System.out.println("Insertion Sort: ");
		arr.scramble();
		arr.display();
		arr.insertionSort();
		arr.display();
	}
}
