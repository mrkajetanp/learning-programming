class Node {
	public String name;
	public int number;
	public Node next; // next node in list

	public Node (String s, int n) {
		name = s;
		number = n;
		next = null; // not necessary
	}

	public void display () {
		System.out.print("{" + name + ", " + number + "} ");
	}
}

class SinglyLinkedList {
	private Node first;

	public SinglyLinkedList () {
		first = null; // not necessary
	}

	public boolean isEmpty () {
		return (first == null);
	}

	public void insertFirst (String s, int n) {
		Node newNode = new Node (s, n);
		newNode.next = first;
		first = newNode;
	}

	public Node deleteFirst () {
		Node temp = first;
		first = first.next;
		return temp;
	}

	public void display () {
		System.out.print("List (first --> last): ");
		Node curr = first;
		while (curr != null) {
			curr.display();
			curr = curr.next;
		}
		System.out.println("");
	}
}

class SinglyLinkedListApp {
	public static void main (String[] args) {
		SinglyLinkedList theList = new SinglyLinkedList ();

		theList.insertFirst ("One", 11);
		theList.insertFirst ("Two", 22);
		theList.insertFirst ("Three", 33);
		theList.insertFirst ("Four", 44);

		theList.display();

		while (!theList.isEmpty()) {
			Node aNode = theList.deleteFirst();
			System.out.print("Deleted ");
			aNode.display();
			System.out.println("");
		}

		theList.display();

		theList.insertFirst ("Cajetan", 666);

		theList.display();
	}
}
