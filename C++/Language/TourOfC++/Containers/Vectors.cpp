#include <iostream>
#include <vector>

using std::cout;
using std::endl;
using std::cin;
using std::string;

// vector adaptation with range-checking
template <typename T>
class Vec : public std::vector<T> {
	public:
		using std::vector<T>::vector; // use the constructors from vector

		T& operator[] (int i) {
			return std::vector<T>::at(i);
		}

		const T& operator[] (int i) const {
			return std::vector<T>::at(i);
		}
};

typedef struct Entry {
	string name;
	int number;
} Entry;

std::ostream& operator<< (std::ostream& os, Entry e) {
	return os << "{ \"" << e.name << "\", " << e.number << " }";
}

//void inputEntries (std::vector<Entry> v) {
//	for (Entry e ; cin >> e ;) // >> has to be overloaded
//		v.push_back(e);
//}

int main () {
	std::vector<Entry> phoneBook = {
		{ "David Hume", 123456 },
		{ "Karl Tanner", 334211 },
		{ "John Nash", 3456231 }
	};

	for (Entry& e : phoneBook)
		cout << e << endl;

	cout << "Phone book size: " << phoneBook.size() << endl;

	std::vector<int> v1 = {1, 2, 3, 4}; // size is 4
	std::vector<string> v2; // size is 0
	std::vector<Entry*> v3 (23); // size is 23, initial value = nullptr
	std::vector<double> v4 (32, 9.9); // size is 32, initial value: 9.9

	for (double& x : v4)
		cout << x << " ";
	cout << endl;

	Vec<int> ve1 = {2, 4, 5, 8};

	for (auto& x : ve1)
		cout << x << " ";
	cout << endl;

	try {
		cout << ve1[ve1.size()] << endl;
	}
	catch (std::out_of_range) {
		cout << "Range error." << endl;
	}

	return 0;
}











