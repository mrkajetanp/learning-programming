#include <iostream>
#include <bitset>
#include <fstream>
#include <sstream>

using std::cout;
using std::endl;
using std::cin;
using std::string;

void formatting () {
	cout << 1234 << ", " << std::hex << 1234 << ", " << std::oct << 1234 << endl;
	std::bitset<8> x (1234);
	cout << x << endl;

	constexpr double d = 123.456;

	cout << d << "; "
		<< std::scientific << d << "; "
		<< std::hexfloat << d << "; "
		<< std::fixed << d << "; "
		<< std::defaultfloat << d << endl;

	cout.precision (4);
	cout << 1.233 << " " << 12.2345 << endl;
	cout << 1234533423 << endl; // doesn't work for integers
	cout << endl;
}

void fileStreams () {
	std::ofstream ofs ("target"); // output
	if (!ofs)
		std::cerr << "Couldn't open target file for writing." << endl;

	std::fstream ifs; // input + output
	if (!ifs)
		std::cerr << "Couldn't open target file for reading." << endl;
}

void stringStreams () {
	std::ostringstream oss;
	oss << "hello my dear friend " << std::hex << 1234;
	string test = oss.str();
	cout << test << endl;
}

int main () {
	formatting();
	stringStreams();

	return 0;
}
