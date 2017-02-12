#include <iostream>
#include <bitset>
#include <vector>
#include <tuple>
#include <array>

using std::cout;
using std::endl;
using std::cin;
using std::string;

void f1 (int* p, int s) {
	cout << *p << " - " << s << endl;
}

void arrays () {
	std::array<int, 3> a1 {1,2,3};

	f1 (a1.data(), a1.size()); // C-style

	for (int& x : a1)
		cout << x << " ";
	cout << endl;
}

void bitsets () {
	std::bitset<8> bs1 {"11000110"};
	std::bitset<8> bs2 = { 399 };

	std::bitset<8> bs3 = ~bs1;
	std::bitset<8> bs4 = bs1&bs3;
	std::bitset<8> bs5 = bs1 << 2;

	cout << bs1.to_string() << endl;
	cout << bs4.to_string() << endl;
	cout << bs5.to_string() << endl;

	cout << bs2.to_ulong() << endl;

	cout << bs1 << endl;
	cout << bs2 << endl;
}

void pairsAndTuples () {
	auto pp = std::make_pair ("hehe", 2); // pair of <vector<int>::iterator,int>

	cout << "Pair: " << pp.first << " " << pp.second << endl;

	std::tuple<string,int,double> t2 {"hehe", 10, 3.5};
	auto t = std::make_tuple (string{"Herring"}, 10, 1.23);

	string s = std::get<0> (t);
	int x = std::get<1> (t);
	double d = std::get<2> (t);

	cout << "Tuple: " << s << " " << x << " " << d << endl;
}

int main () {
	arrays();
	bitsets();
	pairsAndTuples();
	return 0;
}
