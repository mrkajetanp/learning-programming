#include <iostream>
#include <map>
#include <unordered_map>

using std::cout;
using std::endl;
using std::cin;
using std::string;

struct Record {
	string name;
	int product_code;
	// ...
};

struct RHash { // custom hash function for Record
	size_t operator() (const Record& r) const {
		return std::hash<string>()(r.name)^std::hash<int>()(r.product_code);
	}
};

int main () {
	// map is always ordered
	std::map<string, int> phoneBook = {
		{ "David Hume", 123456 },
		{ "Karl Tanner", 334211 },
		{ "John Nash", 3456231 }
	};

	cout << phoneBook["David Hume"] << endl;

	for (auto& x : phoneBook) // map is made of pairs
		cout << x.first << " " << x.second << endl;

	// unordered_map uses hashing

	std::unordered_map<string, int> phoneBook2 = {
		{ "David Hume", 123456 },
		{ "Karl Tanner", 334211 },
		{ "John Nash", 3456231 }
	};

	cout << endl;

	cout << phoneBook2["Karl Tanner"] << endl;

	for (auto& x : phoneBook2)
		cout << x.first << " " << x.second << endl;

	return 0;
}
