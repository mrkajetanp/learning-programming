#include <iostream>
#include <vector>
#include <complex>

using std::cout;
using std::endl;
using std::cin;
using std::string;

void initializations () {
	int values[] = { 1, 2, 3, 4, 5 };
	for (const int& x : values)
		cout << x << " ";
	cout << endl;

	std::vector<int> v = { 2, 3, 5, 7, 11, 13, 17 };
	for (const int& x : v)
		cout << x << " ";
	cout << endl;

	std::vector<string> cities = { "London", "Warsaw", "New York", "Washington", "Los Angeles" };
	for (const string& s : cities)
		cout << s << ", ";
	cout << endl;

	std::complex<double> c = { 4.0, 3.0 };

	//	int i; // undefined
	//	int j {}; // 0
	//	int* p; // undefined
	//	int* q {}; // nullptr
	//	cout << j << endl;
}

void print (std::initializer_list<int> vals) {
	for (auto p = vals.begin() ; p != vals.end() ; ++p) { // process a list of values
		cout << *p << " ";
	}
	cout << endl;
}

void usingListClass () {
	print ({12, 34, 33, 88, 92, 14});
}



int main () {
	usingListClass();
	

	return 0;
}
