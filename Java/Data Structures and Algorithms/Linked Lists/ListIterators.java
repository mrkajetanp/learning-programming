
class ListNode {
    public long dData;
    public ListNode next;

    public ListNode (long dd) {
        dData = dd;
    }

    public void display () {
        System.out.print (dData + " ");
    }
}

class LinkedList {
    private ListNode first;

    public LinkedList () {
        first = null;
    }

    public ListNode getFirst () {
        return first;
    }

    public void setFirst (ListNode f) {
        first = f;
    }

    public boolean isEmpty () {
        return (first == null);
    }

    public ListIterator getIterator () {
        return new ListIterator (this);
    }

    public void display () {
        ListNode current = first;
        while (current != null) {
            current.display ();
            current = current.next;
        }
        System.out.println ("");
    }
}

class ListIterator {
    private ListNode current;
    private ListNode previous;
    private LinkedList list;

    public ListIterator (LinkedList theList) {
        list = theList;
        reset ();
    }

    public void reset () {
        current = list.getFirst ();
        previous = null;
    }

    public boolean atEnd () {
        return (current.next == null);
    }

    public void nextNode () {
        previous = current;
        current = current.next;
    }

    public ListNode getCurrent () {
        return current;
    }

    public void insertAfter (long dd) {
        ListNode newNode = new ListNode (dd);
        if (list.isEmpty ()) {
            list.setFirst (newNode);
            current = newNode;

        }
        else {
            newNode.next = current.next;
            current.next = newNode;
            nextNode ();
        }
    }

    public void insertBefore (long dd) {
        ListNode newNode = new ListNode (dd);
        if (previous == null) {
            newNode.next = list.getFirst ();
            list.setFirst (newNode);
            reset ();
        }
        else {
            newNode.next = previous.next;
            previous.next = newNode;
            current = newNode;
        }
    }

    public long deleteCurrent () {
        long value = current.dData;
        if (previous == null) {
            list.setFirst (current.next);
            reset ();
        }
        else {
            previous.next = current.next;
            if (atEnd ())
                reset ();
            else
                current = current.next;
        }
        return value;
    }
}

public class ListIterators {
    public static void main (String[] args) {
        LinkedList theList = new LinkedList ();
        ListIterator iter1 = theList.getIterator ();

        iter1.insertAfter (40);
		iter1.insertAfter (20);
        iter1.insertAfter (60);
        iter1.insertAfter (80);

        theList.display ();

        System.out.println (iter1.getCurrent().dData);

        iter1.reset ();
        iter1.nextNode ();

		System.out.println (iter1.getCurrent().dData);
	iter1.deleteCurrent ();

	    theList.display ();

		System.out.println (iter1.getCurrent().dData);

	iter1.insertBefore (43);

        theList.display ();
    }
}

