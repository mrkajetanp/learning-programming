#include <iostream>
#include <functional>
#include <algorithm>
#include <limits>
#include <iterator>
#include <vector>
#include <type_traits>

using std::cout;
using std::endl;
using std::string;

int f1 (double a) {
	return a*2;
}


void functions () {
	cout << endl << "Functions: " << endl;

	std::function<int(double)> fct = { f1 };
	cout << fct (7) << endl;

	fct = [] (double d) { return d*3; };

	cout << fct (7) << endl;
}

template <typename Ran>
void sortHelper (Ran beg, Ran end, std::random_access_iterator_tag) {
	std::sort (beg, end);
}

template <typename For>
void sortHelper (For beg, For end, std::forward_iterator_tag) {
	std::vector<typename For::value_type> v {beg, end};
	std::sort (v.begin(), v.end());
	std::copy (v.begin(), v.end(), beg);
}

template <typename C>
void mySort (C& c) {
	using iter = typename C::Iterator;
	sortHelper (c.begin(), c.end(), std::iterator_traits<iter>::iterator_category);
}

void typeFunctions () {
	cout << endl << "Type functions: " << endl;

	constexpr float min = std::numeric_limits<float>::min(); // smallest positive float
	cout << min << endl;

	constexpr int szi = sizeof (int);
	cout << szi << endl;
}

template <typename Scalar>
class complex {
	private:
		Scalar re, im;
	public:
		static_assert (std::is_arithmetic<Scalar>(), "Support only for complex of arithmetic types.");

};

void typePredicates () {
	bool b1 = std::is_arithmetic<int>(); // yes, int is an arithmetic type
	bool b2 = std::is_arithmetic<string>(); // no, string is not an arithmetic type
	cout << b1 << " " << b2 << endl;

	/*
	* is_class
	* is_pod
	* is_literal_type
	* has_virtual_destructor
	* is_base_of
	* etc.
	*/
}

int main () {
	functions ();
	typeFunctions ();
	typePredicates ();
	return 0;
}
