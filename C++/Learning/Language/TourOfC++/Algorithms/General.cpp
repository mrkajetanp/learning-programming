#include <iostream>
#include <algorithm>
#include <vector>
#include <list>

using std::cout;
using std::endl;
using std::cin;
using std::string;

typedef struct Entry {
	string name;
	int number;
} Entry;

// Defining a comparison operator so that the sort function can work on the custom type
bool operator< (const Entry& x, const Entry& y) {
	return x.name < y.name;
}

void sortAndExtractUnique (std::vector<int>& vec, std::list<int>& lst) {
	std::sort (vec.begin(), vec.end());
	std::unique_copy (vec.begin(), vec.end(), lst.begin());
}

std::list<int> sortAndReturnUniqueList (std::vector<int>& vec) {
	std::list<int> res;
	std::sort (vec.begin(), vec.end());
	unique_copy (vec.begin(), vec.end(), std::back_inserter(res));
	return res;
}

void print (std::vector<int>& vec) {
	for (auto& x : vec)
		cout << x << " ";
	cout << endl;
}

void print (std::list<int>& lst) {
	for (auto& x : lst)
		cout << x << " ";
	cout << endl;
}

int main () {
	std::vector<int> v1 = { 3, 5, 2, 1, 8, 6, 9, 4, 1, 8, 9, 3 };
	std::list<int> l1 (10);
	print (v1);
	print (l1);

	cout << "After: " << endl;

	sortAndExtractUnique (v1, l1);
	print (v1);
	print (l1);

	// Version without having to allocate memory, a better one
	std::list<int> l2 = sortAndReturnUniqueList (v1);

	print (l2);


	return 0;
}
