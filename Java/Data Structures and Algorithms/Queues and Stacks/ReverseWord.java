class StackX {
	private int maxSize;
	private char[] stackArray;
	private int top;

	// Constructor
	public StackX (int max) {
		maxSize = max;
		stackArray = new char[maxSize];
		top = -1; // uninitialized
	}

	// Put item on top of the stack
	public void push (char j) {
		stackArray[++top] = j;
	}

	public char pop () {
		return stackArray[top--];
	}

	public char peek () {
		return stackArray[top];
	}

	public boolean isEmpty () {
		return (top == -1);
	}
}

class Reverser {
	private String input;
	private String output;

	public Reverser (String in) {
		input = in;
	}

	public String doRev () {
		int stackSize = input.length(); // get max stack size
		StackX theStack = new StackX (stackSize);

		for (int i = 0; i < input.length() ; i++) {
			char ch = input.charAt (i);
			theStack.push (ch);
		}

		output = "";

		while (!theStack.isEmpty()) {
			char ch = theStack.pop();
			output += ch;
		}
		return output;
	}
}

public class ReverseWord {
	public static void main (String[] args) {

		Reverser theReverser = new Reverser ("hello my friend");
		
		System.out.println("Reversed: " + theReverser.doRev());
	}
}







