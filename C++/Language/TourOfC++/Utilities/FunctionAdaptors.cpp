#include <iostream>
#include <functional>
#include <vector>
#include <algorithm>

using std::cout;
using std::endl;
using std::string;

typedef struct Shape {
	void draw () { cout << "draw!" << endl; }
} Shape;

double cube (double d) {
	return d*d*d;
}

int pow (int a, int b) { // not a power, doesn't matter
	return a*b;
}

double pow (double a, double b) { // same here
	return a*b;
}

void f (int a, const string& s) {
	cout << a*2 << " -> " << s << endl;
}

	void binds () {
	auto cube2 = std::bind (cube, 2);
	cout << cube2() << endl;

	auto f2 = std::bind (f, 8, std::placeholders::_1);
	f2 ("hehe");
	f2 ("xd");

	// ugly but works, you have to specify which function to use when they are overloaded
	auto pow2 = std::bind ((double(*)(double, double))pow, std::placeholders::_1, 2);

	cout << pow2 (9) << endl;
}

void user (Shape *p) {
	p->draw();
	auto draw = std::mem_fn (&Shape::draw);
	draw (p);
}

void drawAll (std::vector<Shape*>& v) {
	std::for_each (v.begin(), v.end(), std::mem_fn(&Shape::draw));
}

void mem_fns () {
}

int main () {
	binds();
	return 0;
}
