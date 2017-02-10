#include <iostream>
#include <chrono>
#include <math.h>

using std::cout;
using std::endl;
using std::cin;
using std::string;

using namespace std::chrono;

void doWork () {
	int b;
	for (int i = 0; i < 10000000; ++i) {
		b = sqrt (i);
	}
}

int main () {
	auto t0 = high_resolution_clock::now();

	doWork();

	auto t1 = high_resolution_clock::now();

	cout << duration_cast<milliseconds> (t1-t0).count() << " msec" << endl;

	return 0;
}
