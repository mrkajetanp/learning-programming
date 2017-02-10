
class Person {
	private String lastName;
	private String firstName;
	private int age;

	public Person (String last, String first, int a) {
		lastName = last;
		firstName = first;
		age = a;
	}

	public void displayPerson () {
		System.out.println(lastName + " " + firstName + ", " + age);
	}

	public String getLast () {
		return lastName;
	}
}

class ArrayInOb {
	private Person[] arr;
	private int nItems;

	public ArrayInOb (int max) {
		arr = new Person[max];
		nItems = 0;
	}

	public void insert (String last, String first, int age) {
		arr[nItems] = new Person (last, first, age);
		nItems++;
	}

	public void display () {
		for (int i = 0 ; i < nItems ; i++)
			arr[i].displayPerson();
		System.out.println("");
	}

	public void insertionSort () {
		int i, j;

		for (i = 1 ; i < nItems ; i++) {
			Person temp = arr[i];
			j = i;
			while ((j > 0) && arr[j-1].getLast().compareTo(temp.getLast()) > 0) {
				arr[j] = arr[j-1];
				--j;
			}
			arr[j] = temp;
		}
	}
}

public class SortingObjects {
	public static void main (String[] args) {
		int maxSize = 100;
		ArrayInOb arr = new ArrayInOb (maxSize);

		arr.insert ("Evans", "Patty", 24);
		arr.insert ("Smith", "John", 45);
		arr.insert ("Smith", "Thomas", 17);
		arr.insert ("Crawford", "Jack", 44);
		arr.insert ("Blake", "Joe", 24);

		System.out.println("Before sorting: ");
		arr.display();

		arr.insertionSort();

		System.out.println("After sorting: ");
		arr.display();
	}
}




















