#include <iostream>
#include <list>

using std::cout;
using std::endl;
using std::cin;
using std::string;

typedef struct Entry {
	string name;
	int number;
} Entry;

std::ostream& operator<< (std::ostream& os, Entry e) {
	return os << "{ \"" << e.name << "\", " << e.number << " }";
}

int getNumber (std::list<Entry>& phoneBook, const string& s) { // searching a list
	for (const auto& x : phoneBook)
		if (x.name == s)
			return x.number;
	return 0;
}

// Explicit and implicit use of iterators

int getNumberIterators (std::list<Entry> phoneBook, const string& s) {
	for (auto p = phoneBook.begin() ; p != phoneBook.end() ; ++p)
		if (p->name == s)
			return p->number;
	return 0;
}

void f (std::list<Entry> phoneBook, const Entry& ee, std::list<Entry>::iterator p, std::list<Entry>::iterator q) {
	phoneBook.insert (p, ee); // add ee before the element referred to by p
	phoneBook.erase (q); // remove the element referred to by q
}

int main () {
	std::list<Entry> phoneBook = {
		{ "David Hume", 123456 },
		{ "Karl Tanner", 334211 },
		{ "John Nash", 3456231 }
	};

	cout << getNumber (phoneBook, "John Nash") << endl;
	cout << getNumberIterators (phoneBook, "John Nash") << endl;

	return 0;
}
