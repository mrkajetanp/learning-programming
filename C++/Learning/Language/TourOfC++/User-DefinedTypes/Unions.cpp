#include <iostream>

using std::cout;
using std::endl;

enum Type { str, num };

union Value {
	char* s;
	int i;
};

struct Entry {
	char* name;
	Type t;
	Value v;	// use v.s if t==str, use v.i if t==num
};

void f (Entry& p) {
	if (p.t == str)
		cout << p.v.s;
	else if (p.t == num)
		cout << p.v.i;
	cout << endl;
}

int main () {
	Entry e;
	e.name = (char*)"test";
	e.t = num;
	e.v.i = 10;
	// e.t = str;
	// e.v.s = (char*)"heyo";

	f (e);

	return 0;
}