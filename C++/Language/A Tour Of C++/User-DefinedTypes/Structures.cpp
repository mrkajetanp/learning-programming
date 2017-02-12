#include <iostream>

using std::cout;
using std::cin;
using std::endl;

struct Vector {
	int size;	// number of elements
	double* elem;	// pointer to elements
};



void vectorInit (Vector& v, int s) {
	v.elem = new double[s];
	v.size = s;
}


double readAndSum (int s) {
	Vector v;
	vectorInit (v, s);
	for (int i = 0; i != s ; ++i)
		cin >> v.elem[i];

	double sum = 0;   

	for (int i = 0; i < s; ++i)
		sum += v.elem[i];
	return sum;
}

int main () {
	cout << "hey there" << endl;

	int a = 0;
	if (a)
		cout << "hehe" << endl;

	return 0;
}
