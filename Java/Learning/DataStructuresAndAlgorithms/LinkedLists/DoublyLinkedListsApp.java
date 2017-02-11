
class ListNode {
	public long dData;
	public ListNode next;
	public ListNode prev;

	public ListNode (long d) {
		dData = d;
	}

	public void display () {
		System.out.print (dData + " ");
	}
}

class DoublyLinkedList {
	private ListNode first;
	private ListNode last;

	public DoublyLinkedList () {
		first = null;
		last = null;
	}

	public boolean isEmpty () {
		return (first == null);
	}

	public void insertFirst (long dd) {
		ListNode newNode = new ListNode (dd);
		if (isEmpty ())
			last = newNode;
		else
			first.prev = newNode;
		newNode.next = first;
		first = newNode;
	}

	public void insertLast (long dd) {
		ListNode newNode = new ListNode (dd);
		if (isEmpty ())
			first = newNode;
		else {
			last.next = newNode;
			newNode.prev = last;
		}
		last = newNode;
	}

	public ListNode deleteFirst () {
		ListNode temp = first;
		if (first.next == null) // if only one item
			last = null;
		else
			first.next.prev = null;
		first = first.next;
		return temp;
	}

	public ListNode deleteLast () {
		ListNode temp = last;
		if (first.next == null) // if only one item
			first = null;
		else
			last.prev.next = null;
		last = last.prev;
		return temp;
	}

	public boolean insertAfter (long key, long dd) {
		ListNode current = first;
		while (current.dData != key) {
			current = current.next;
			if (current == null)
				return false;
		}
		ListNode newNode = new ListNode (dd);
		if (current == last) { // if last link
			newNode.next = null;
			last = newNode;
		}
		else {
			newNode.next = current.next;
			current.next.prev = newNode;
		}
		newNode.prev = current;
		current.next = newNode;
		return true;
	}

	public ListNode deleteKey (long key) {
		ListNode current = first;
		while (current.dData != key) {
			current = current.next;
			if (current == null)
				return null;
		}
		if (current == first)
			first = current.next;
		else
			current.prev.next = current.next;

		if (current == last)
			last = current.prev;
		else
			current.next.prev = current.prev;
		return current;
	}

	public void displayForward () {
		ListNode current = first;
		while (current != null) {
			current.display ();
			current = current.next;
		}
		System.out.println("");
	}

	public void displayBackward () {
		ListNode current = last;
		while (current != null) {
			current.display ();
			current = current.prev;
		}
		System.out.println("");
	}
}

public class DoublyLinkedListsApp {
	public static void main (String[] args) {
		DoublyLinkedList theList = new DoublyLinkedList ();

		theList.insertFirst (22);
		theList.insertFirst (44);
		theList.insertFirst (66);

		theList.insertLast (11);
		theList.insertLast (33);
		theList.insertLast (55);

		theList.displayForward ();
		theList.displayBackward ();

		theList.deleteFirst ();
		theList.deleteLast ();
		theList.deleteKey (11);

		theList.displayForward ();

		theList.insertAfter (22, 77);
		theList.insertAfter (33, 88);

		theList.displayForward ();
	}
}












