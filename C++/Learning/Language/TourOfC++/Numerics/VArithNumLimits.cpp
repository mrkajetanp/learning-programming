#include <iostream>
#include <valarray>
#include <limits>

using std::cout;
using std::endl;
using std::cin;
using std::string;

void f1 (std::valarray<double>& a1, std::valarray<double>& a2) {
	std::valarray<double> a = a1*3.14+a2/a1;
	a2 += a1*3.14;
	a = abs (a);
	double d = a2[7];
	cout << d << endl;
}

void numericLimits () {
	static_assert (std::numeric_limits<char>::is_signed, "unsigned chars!");
	static_assert (100000<std::numeric_limits<int>::max(), "small ints!");
}

int main () {
	numericLimits ();
	return 0;
}
