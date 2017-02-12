
class ListNode {
	public long dData;
	public ListNode next;

	public ListNode (long d) {
		dData = d;
	}

	public void display () {
		System.out.print(dData + " ");
	}
}

class DEList {
	private ListNode first;
	private ListNode last;

	public DEList () {
		first = null;
		last = null;
	}

	public boolean isEmpty () {
		return first == null;
	}

	public void insertFirst (long dd) {
		ListNode newNode = new ListNode (dd);
		if (isEmpty())
			last = newNode;
		newNode.next = first;
		first = newNode;
	}

	public void insertLast (long dd) {
		ListNode newNode = new ListNode (dd);
		if (isEmpty())
			first = newNode;
		else
			last.next = newNode;
		last = newNode;
	}

	public long deleteFirst () {
		long temp = first.dData;
		if (first.next == null)
			last = null;
		first = first.next;
		return temp;
	}

	public void display () {
		ListNode current = first;
		while (current != null) {
			current.display ();
			current = current.next;
		}
		System.out.println("");
	}
}

public class DoubleEndedListApp {
	public static void main (String[] args) {
		DEList theList = new DEList ();

		theList.insertFirst (22);
		theList.insertFirst (44);
		theList.insertFirst (66);

		theList.insertLast (11);
		theList.insertLast (33);
		theList.insertLast (55);

		theList.display ();

		theList.deleteFirst ();
		theList.deleteFirst ();

		theList.display ();
	}
}
