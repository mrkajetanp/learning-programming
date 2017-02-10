
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

class SortedList {
	private ListNode first;

	public SortedList () {
		first = null;
	}

	public void insert (long key) {
		ListNode newNode = new ListNode (key);
		ListNode previous = null; // start at first
		ListNode current = first;

		while (current != null && key > current.dData) {
			previous = current;
			current = current.next;
		}

		if (previous == null)
			first = newNode;
		else
			previous.next = newNode;
		newNode.next = current;
	}

	public ListNode remove () {
		ListNode temp = first;
		first = first.next;
		return temp;
	}

	public void display () {
		System.out.print("List (first-->last): ");
		ListNode current = first;
		while (current != null) {
			current.display ();
			current = current.next;
		}
		System.out.println("");
	}
}

public class SortedListApp {
	public static void main (String[] args) {
		SortedList theSortedList = new SortedList ();

		theSortedList.insert (20);
		theSortedList.insert (40);

		theSortedList.display ();

		theSortedList.insert (10);
		theSortedList.insert (50);
		theSortedList.insert (30);

		theSortedList.display ();

		theSortedList.remove ();

		theSortedList.display ();
	}
}
