#include <iostream>
#include <vector>

using std::string;
using std::cout;
using std::cin;
using std::endl;
using std::vector;

vector<int> readInts (std::istream& is) {
	vector<int> res;
	int i;
	while (is >> i)
		res.push_back(i);
	return res;
}

void cinStringsWithNesting () {
	while (cin) {
		for (int i; cin >> i ;) {
			// use the integer here
		}

		if (cin.eof()) {
			// we reached the end-of-file
		}
		else if (cin.fail()) { // a potentially recoverable error
			cin.clear(); // reset the state to good()
			char ch;
			if (cin >> ch) { // look for nesting with { ... }
				switch (ch) {
					case '{':
						// ... start nested structure
						break;
					case '}':
						// ... end nested structure
						break;
					default:
						cin.setstate (std::ios_base::failbit); // add fail() to cin's state
				}
			}
		}
	}
}

int main () {
	vector<int> myInts;
	//	myInts = readInts (cin);

	for (int& x : myInts)
		cout << x << " ";
	cout << endl;

	cout << "Size: " << myInts.size() << endl;

	string name;
	// cin >> name;

	cout << "Hello, " << name << " !" << endl;

	//	std::getline (cin, name);

	cout << "Hello, " << name << " !" << endl;

	return 0;
}
