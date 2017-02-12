
class ListNode {
	public long dData;
	public ListNode next;

	public ListNode (long dd) {
		dData = dd;
	}

	public void display () {
		System.out.print(dData + " ");
	}
}

class LinkedList {
	private ListNode first;

	public LinkedList () {
		first = null;
	}

	public boolean isEmpty () {
		return (first == null);
	}

	public void insertFirst (long dd) {
		ListNode newNode = new ListNode (dd);
		newNode.next = first;
		first = newNode;
	}

	public long deleteFirst () {
		ListNode temp = first;
		first = first.next;
		return temp.dData;
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

class Stack {
	private LinkedList theList;

	public Stack () {
		theList = new LinkedList ();
	}

	public void push (long j) {
		theList.insertFirst (j);
	}

	public long pop () {
		return theList.deleteFirst ();
	}

	public boolean isEmpty () {
		return (theList.isEmpty());
	}

	public void display () {
		System.out.print ("Stack (top-->bottom): ");
		theList.display ();
	}
}

public class LinkedListStackApp {
	public static void main (String[] args) {
		Stack theStack = new Stack ();

		theStack.push (20);
		theStack.push (40);

		theStack.display ();

		theStack.push (60);
		theStack.push (80);

		theStack.display ();

		theStack.pop ();
		theStack.pop ();

		theStack.display ();
	}
}
