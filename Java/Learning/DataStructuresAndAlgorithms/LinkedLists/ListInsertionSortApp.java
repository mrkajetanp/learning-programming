
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

	public SortedList (ListNode[] nodeArr) {
		first = null;
		for (int i = 0 ; i < nodeArr.length ; i++)
			insert (nodeArr[i]);
	}

	public void insert (ListNode k) {
		ListNode previous = null;
		ListNode current = first;

		while (current != null && k.dData > current.dData) {
			previous = current;
			current = current.next;
		}

		if (previous == null)
			first = k;
		else
			previous.next = k;
		k.next = current;
	}

	public ListNode remove () {
		ListNode temp = first;
		first = first.next;
		return temp;
	}
}

class ListInsertionSortApp {
	public static void main (String[] args) {
		int size = 10;
		ListNode[] nodeArray = new ListNode[size];

		for (int i = 0 ; i < size ; i++) {
			int n = (int)(java.lang.Math.random()*99);
			ListNode newNode = new ListNode (n);
			nodeArray[i] = newNode;
		}

		System.out.print("Unsorted array: ");
		for (int i = 0 ; i < size ; i++)
			System.out.print(nodeArray[i].dData + " ");
		System.out.println("");

		SortedList theSortedList = new SortedList (nodeArray);

		for (int i = 0 ; i < size ; i++)
			nodeArray[i] = theSortedList.remove ();

		System.out.print("Sorted array: ");
		for (int i = 0 ; i < size ; i++)
			System.out.print(nodeArray[i].dData + " ");
		System.out.println("");
	}
}
