#include <iostream>
#include <utility>
#include <string>
#include <tuple>
#include <stdexcept>

using std::cout;
using std::endl;
using std::string;

// Implementation of a pair
template <typename T1, typename T2>
struct myPair {
	T1 first;
	T2 second;
};

template <typename T1, typename T2>
std::ostream& operator << (std::ostream& os, const std::pair<T1, T2>& p) {
	return os << "[" << p.first << "," << p.second << "]";
}

void pairs () {
	std::pair<int, std::string> p1, p2;
	p1 = std::make_pair(10, "test");
	p2 = std::make_pair(18, "hello");
	std::cout << p1 << " " << p2 << std::endl;
	std::swap (p1, p2);
	std::cout << p1 << " " << p2 << std::endl;

	std::pair<int, float> pIFOne (34, 3.4);

	cout << std::get<0> (pIFOne) << " " << std::get<1> (pIFOne) << endl;
	cout << std::tuple_size<std::pair<int, float>>::value << endl;

	std::cout << std::endl;
}

std::tuple<double, char, std::string> getMyTuple () {
	return std::make_tuple (3.8, 'A', "Janusz Pawlacz");
}

int add (int a, int b) {
	return a+b;
}

void tuples () {
	auto tupleOne = getMyTuple();
	std::cout << std::get<0>(tupleOne) << " "
			<< std::get<1>(tupleOne) << " "
			<< std::get<2>(tupleOne) << std::endl;
	double gpa1;
	std::string name1;
	std::tie(gpa1, std::ignore, name1) = getMyTuple();
	std::cout << gpa1 << " " << " " << name1 << std::endl;

	std::pair<int, float> p1 (10, 18.3);

	auto tt = std::tuple_cat (tupleOne, p1);

	cout << std::get<4> (tt) << endl;

	// std::cout << std::apply (add, std::make_tuple(3, 8)) << std::endl;
}

int main () {
	std::cout << "Pairs: " << std::endl;
	pairs();
	std::cout << "Tuples: " << std::endl;
	tuples();
	return 0;
}
