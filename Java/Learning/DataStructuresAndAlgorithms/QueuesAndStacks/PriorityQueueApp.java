
class PriorityQ {
	// sorted array, from max at 0 to min at size-1
	private int maxSize;
	private long[] queArray;
	private int nItems;

	public PriorityQ (int s) {
		maxSize = s;
		queArray = new long[maxSize];
		nItems = 0;
	}

	public void insert (long item) {
		int i;
		if (nItems == 0)
			queArray[nItems++] = item;
		else {
			for (i = nItems-1 ; i >= 0; i--) {
				if (item > queArray[i])
					queArray[i+1] = queArray[i];
				else
					break;
			}
			queArray[i+1] = item;
			nItems++;
		}
	}

	public long remove () {
		return queArray[--nItems];
	}

	public long peekMin () {
		return queArray[nItems-1];
	}

	public boolean isEmpty () {
		return (nItems == 0);
	}

	public boolean isFull () {
		return (nItems == maxSize);
	}
}

public class PriorityQueueApp {
	public static void main (String[] args) {
		PriorityQ thePQ = new PriorityQ (5);
		thePQ.insert (30);
		thePQ.insert (50);
		thePQ.insert (10);
		thePQ.insert (40);
		thePQ.insert (20);

		while (!thePQ.isEmpty()) {
			System.out.print(thePQ.remove() + " ");
		}
		System.out.println("");
	}
}
