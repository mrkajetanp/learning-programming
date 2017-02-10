
class StackX {
	private int maxSize;
	private char[] stackArray;
	private int top;

	public StackX (int s) {
		maxSize = s;
		stackArray = new char[maxSize];
		top = -1;
	}

	public void push (char c) {
		stackArray[++top] = c;
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

class BracketChecker {
	private String input;

	public BracketChecker (String in) {
		input = in;
	}

	public void check () {
		int stackSize = input.length();
		StackX theStack = new StackX (stackSize);

		for (int i = 0 ; i < input.length() ; i++) {
			char ch = input.charAt(i);
			switch (ch) {
				case '{':
				case '(':
				case '[':
					theStack.push (ch);
					break;
				case '}':
				case ')':
				case ']':
					if (!theStack.isEmpty()) {
						char chx = theStack.pop();
						if ( (ch=='}' && chx!='{') ||
							(ch==']' && chx!='[') ||
							(ch==')' && chx!='(') )
							System.out.println("Error: " + ch + " at " + i);
					}
					else 	// prematurely empty
						System.out.println("Error: " + ch + " at " + i);
					break;
				default:
					break;
			}
		}
		if (!theStack.isEmpty())
			System.out.println("Error: Missing right delimiter");
	}
}

public class BracketParsing {
	public static void main (String[] args) {
		String input1 = "a{bcd(ee[x])}";
		String input2 = "a{bcd(ee[x]}";
		String input3 = "a{bcd(ee[x}}";
		BracketChecker theChecker = new BracketChecker (input1);
		theChecker.check();
	}
}
