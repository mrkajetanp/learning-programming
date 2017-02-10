
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

	public void insertLast (long dd) {
		ListNode newNode = new ListNode (dd);
		if ( isEmpty() )
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

class Queue {
	private DEList theList;

	public Queue () {
		theList = new DEList ();
	}

	public boolean isEmpty () {
		return theList.isEmpty ();
	}

	public void insert (long j) {
		theList.insertLast (j);
	}

	public long remove () {
		return theList.deleteFirst ();
	}

	public void display () {
		System.out.print("Queue (front-->rear): ");
		theList.display ();
	}
}

public class LinkedListQueueApp {
	public static void main (String[] args) {
		Queue theQueue = new Queue ();

		theQueue.insert (60);
		theQueue.insert (80);

		theQueue.display ();

		theQueue.insert (33);
		theQueue.insert (99);

		theQueue.display ();

		theQueue.remove ();
		theQueue.remove ();

		theQueue.display ();
	}
}













