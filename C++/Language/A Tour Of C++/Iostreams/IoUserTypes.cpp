#include <iostream>

using std::cout;
using std::endl;
using std::cin;
using std::string;
using std::ostream;
using std::istream;

struct Entry {
	string name;
	int number;
};

ostream& operator<<(ostream& os, const Entry& e) {
	return os << "{\"" << e.name << "\", " << e.number << "}";
}

istream& operator>>(istream& is, Entry& e) {
	char c, c2;
	if (is>>c && c=='{' && is>>c2 && c2=='"') { // starts with a {"
		string name;
		while (is.get(c) && c != '"') // anything before a " is part of the name
			name += c;

		if (is>>c && c==',') {
			int number = 0;
			if (is >> number >> c && c=='}') { // read the number and a }
				e = {name, number};
				return is;
			}
		}
	}
	is.setstate(std::ios_base::failbit);
	return is;
}

int main () {
	Entry eone;
	eone.name = "Cajetan";
	eone.number = 666;

	cout << eone << endl;

	return 0;
}
