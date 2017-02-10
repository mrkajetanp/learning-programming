
class ListNode {
	public int iData;
	public double dData;
	public ListNode next;

	public ListNode (int id, double dd) {
		iData = id;
		dData = dd;
	}

	public void display () {
		System.out.print("{" + iData + ", " + dData + "} ");
	}
}

class SLinkList {
	private ListNode first;

	public SLinkList () {
		first = null;
	}

	public void insertFirst (int id, double dd) {
		ListNode newNode = new ListNode (id, dd);
		newNode.next = first;
		first = newNode;
	}

	public ListNode find (int key) {
		ListNode current = first;
		while (current.iData != key) {
			if (current.next == null)
				return null;
			else
				current = current.next;
		}
		return current;
	}

	public ListNode delete (int key) {
		ListNode current = first;
		ListNode previous = first;

		while (current.iData != key) {
			if (current.next == null)
				return null;
			else {
				previous = current;
				current = current.next;
			}
		}
		if (current == first)
			first = first.next;
		else
			previous.next = current.next;
		return current;
	}

	public void display () {
		System.out.print("List (first-->last): ");
		ListNode current = first;
		while (current != null) {
			current.display();
			current = current.next;
		}
		System.out.println("");
	}
}

public class SLL2 {
	public static void main (String[] args) {
		SLinkList theList = new SLinkList ();

		theList.insertFirst (22, 2.99);
		theList.insertFirst (44, 4.99);
		theList.insertFirst (66, 6.99);
		theList.insertFirst (88, 8.99);

		theList.display();

		ListNode f = theList.find (44);
		if (f != null)
			System.out.println("Found ListNode with key " + f.iData);
		else
			System.out.println("Can't find the ListNode.");

		ListNode d = theList.delete (66);

		if (d != null)
			System.out.println("Deleted ListNode with key " + d.iData);
		else
			System.out.println("Can't delete the ListNode.");

		theList.display();
	}
}
