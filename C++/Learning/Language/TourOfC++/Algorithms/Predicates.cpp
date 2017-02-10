#include <iostream>
#include <map>
#include <algorithm>

using std::cout;
using std::endl;
using std::cin;
using std::string;

// A predicate should not modify the elements to which it is applied

struct GreaterThan {
	int val;
	GreaterThan (int v) : val (v) {}
	bool operator() (const std::pair<string, int>& r) {
		return r.second > val;
	}
};

void f (std::map<string, int>& m) {
	auto gtThan = [] (const std::pair<string, int>& r) {
		return r.second > 42;
	};
	auto p = std::find_if (m.begin(), m.end(), gtThan); // lambda
	auto p2 = std::find_if (m.begin(), m.end(), GreaterThan(42));
	// ...
}

int main () {
	return 0;
}
