#include <iostream>
#include <cmath>
#include <cerrno>
#include <limits>

using std::cout;
using std::endl;
using std::cin;
using std::string;

void f1 () {
	errno = 0;
	
	sqrt (-1);
	if (errno == EDOM)
		std::cerr << "sqrt() not defined for negative argument!" << endl;

	errno = 0;

	pow (std::numeric_limits<double>::max(), 2);
	if (errno == ERANGE)
		std::cerr << "result of pow() too large to represent as a double" << endl;
}

int main () {
	f1 ();
	return 0;
}
