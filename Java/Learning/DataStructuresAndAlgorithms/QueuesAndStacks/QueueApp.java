class Queue {
	private int maxSize;
	private long[] queArray;
	private int front;
	private int rear;
	private int nItems;

	public Queue (int s) {
		maxSize = s;
		queArray = new long[maxSize];
		front = 0;
		rear = -1;
		nItems = 0;
	}

	public void enqueue (long i) {
		if (rear == maxSize-1) // deal with wraparound
			rear = -1;
		queArray[++rear] = i;
		nItems++;
	}

	public long dequeue () {
		long temp = queArray[front++];
		if (front == maxSize) // deal with wraparound
			front = 0;
		nItems--;
		return temp;
	}

	public long peekFront () {
		return queArray[front];
	}

	public boolean isEmpty () {
		return (nItems==0);
	}

	public boolean isFull () {
		return (nItems == maxSize);
	}

	public int size () {
		return nItems;
	}
}

public class QueueApp {
	public static void main (String[] args) {
		Queue theQueue = new Queue (5);

		theQueue.enqueue (10);
		theQueue.enqueue (20);
		theQueue.enqueue (30);
		theQueue.enqueue (40);

		theQueue.dequeue ();
		theQueue.dequeue ();
		theQueue.dequeue ();

		theQueue.enqueue (50);
		theQueue.enqueue (60);
		theQueue.enqueue (70);
		theQueue.enqueue (80);

		while (!theQueue.isEmpty())
			System.out.print(theQueue.dequeue() + " ");
		System.out.println("");

	}
}















