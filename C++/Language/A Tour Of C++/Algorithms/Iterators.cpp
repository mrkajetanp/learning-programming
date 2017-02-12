#include <iostream>
#include <vector>
#include <list>
#include <algorithm>
#include <iterator>

using std::cout;
using std::endl;
using std::cin;
using std::string;

bool hasC (const string& s, char c) {
	auto p = std::find (s.begin(), s.end(), c);
	if (p != s.end())
		return true;
	else
		return false;
}

// Equivalent to hasC, just shorter
bool hasC2 (const string& s, char c) {
	return find (s.begin(), s.end(), c) != s.end();
}

template <typename C, typename V>
std::vector<typename C::iterator> findAllGeneral (C& c, V v) {
	std::vector<typename C::iterator> res;
	for (auto p = c.begin () ; p != c.end () ; ++p)
		if (*p == v)
			res.push_back (p);
	return res;
}

std::vector<string::iterator> findAll (string& s, char c) {
	std::vector<string::iterator> res;
	for (auto p = s.begin() ; p != s.end() ; ++p)
		if (*p == c)
			res.push_back (p);
	return res;
}

void testFindAll () {
	string m = "Marry had a little lamb.";
	for (auto p : findAll (m, 'a'))
		if (*p != 'a')
			std::cerr << "a bug!" << endl;
}

void testFindAllGeneral () {
	string m = "Mary had a little lamb.";
	for (auto p : findAllGeneral (m, 'a'))
		if (*p != 'a')
			std::cerr << "string bug!" << endl;

	std::list<double> ld = { 1.1, 2.3, 5.4, 1.1, 3.2, 8.3 };
	for (auto p : findAllGeneral (ld, 1.1))
		if (*p != 1.1)
			std::cerr << "list bug!" << endl;

	std::vector<string> vs { "red", "green", "dark", "blue", "sky", "green", "red" };
	for (auto p : findAllGeneral (vs, "red"))
		if (*p != "red")
			std::cerr << "vector bug!" << endl;

	for (auto p : findAllGeneral (vs, "green"))
		*p = "vert";

	for (auto p : vs)
		cout << p << " ";
	cout << endl;
}

void streamIterators () {
	std::ostream_iterator<string> oo = cout;
	*oo = "hello, ";
	++oo;
	*oo = "world\n";

	std::istream_iterator<string> ii = cin;
	std::istream_iterator<string> eos = {};
}

void rFSEDW () { // this is weird
	string from, to;
	cin >> from >> to; // get source and target file names

	std::ifstream is { from }; // input stream for file from
	std::istream_iterator<string> ii { is }; // input iterator for stream
	std::istream_iterator<string> eos {}; // input sentinel
	std::ofstream os { to }; // output stream for file "to"
	std::ostream_iterator<string> oo {os, "\n"}; // output iterator for stream
	std::vector<string> b = {ii, eos}; // b is a vector initialized from input
	std::sort (b.begin(), b.end()); // sort the buffer
	std::unique_copy (b.begin(), b.end(), oo); // copy buffer to output, discard duplicates
	return !is.eof() || !os; // return error state

}

int main () {
	testFindAll();
	testFindAllGeneral();
	streamIterators();
	return 0;
}








