#include <iostream>
#include <map>
#include <functional>

using std::cout;
using std::endl;
using std::cin;
using std::string;

// Function with varying return types
template <typename T1, typename T2>
auto add (T1 x, T2 y) -> decltype (x+y) {
	return x+y;
}

std::function<int(int, int)> returnLambda () {
	return [] (int x, int y) {
		return x*y;
	};
}

int main () {
	[] {
		cout << "hello lambda 1" << endl;
	} (); // directly called lambda

	auto l2 = [] {
		cout << "hello lambda 2" << endl;
	};

	l2 ();
	l2 ();

	auto l3 = [] (const string& s) {
		cout << s << endl;
	};

	l3 ("test");
	l3 ("hehe");

	auto l4 = [] {
		return 66;
	};

	cout << l4() << endl;

	auto l5 = [] () -> double { // lamba with explicit return value
		return 42.2;
	};

	cout << l5() << endl;

	int b = 10;

	[=] { // outer scope is passed by value, vars are read-only
		cout << b << endl;
	} ();

	[&] { // passed by reference, read-write access
		cout << b << " ";
		++b;
		cout << b << endl;
	} ();

	// Lamba changed the value of b
	cout << b << endl;

	int x = 0, y = 42;
	auto qqq = [x, &y] { // can write to y but not to x // [=, &y]
		cout << "x: " << x << endl;
		cout << "y: " << y << endl;
		++y;
		cout << "y: " << y << endl;
	};

	cout << "Y before: " << y << endl;
	qqq();
	cout << "Y after: " << y << endl;

	int id = 0;
	auto f = [id] () mutable {
		cout << "id: " << id << endl;
		++id;
	};
	id = 42;
	f();
	f();
	f();
	cout << id << endl;
	cout << endl;

	auto lf = returnLambda();
	cout << lf(6,6) << endl;

	std::map<string, float> coll;
	decltype (coll)::value_type elem;

	cout << add (5, 2.2) << endl;

	return 0;
}












