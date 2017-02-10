#include <iostream>
#include <utility>
#include <set>

using std::cout;
using std::endl;
using std::cin;
using std::string;

void createAndInsert (std::set<int>& coll) {
	int x = 10;
	// ...
	coll.insert (x); // inserts copy of x
	coll.insert (x+x); // inserts copy of temporary rvalue
	coll.insert (x); // inserts copy of x (but it is not used any longer)

	// Better way

	coll.insert (x); // inserts copy of x (OK, x is still used)
	coll.insert (x+x); // moves (or copies) contents of temporary rvalue
	coll.insert (std::move(x)); // moves (or copies) contents of x into coll

	/*
	* Collection needs to provide a version of insert() that deal with rvalues (X&&)
	* insert (const T& x); // for lvalues: copies the value
	* insert (T&& x); // for rvalues: moves the value
	*/

	/*
	* Classes should provide both a copy assignment and a move assignment operator:
	* X& operator= (const X& lvalue); // copy
	* X& operator= (X&& rvalue); // move
	*/
}

int main () {
	cout << "ok" << endl;
	return 0;
}
