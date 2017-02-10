#include <iostream>
#include <bitset>
#include <vector>

using std::cout;
using std::endl;
using std::cin;
using std::string;

// Default template arguments
template <typename T, typename container = std::vector<T>>
class MyClass {
	typename T::SubType * ptr;

	template <typename S>
	void f (S);
};

// Function guarantees that T is initialized with zero for fundamental types
template <typename T>
void f () {
	T x = T();
}

int main () {
	// These bitsets have different types, they can't be assigned or compared
	std::bitset<16> flags16; // bitset with 16 bits
	std::bitset<16> flags32; // bitset with 32 bits

	int i2 = int(); // initialized with 0
	cout << i2 << endl;

	//	MyClass<int> x1; // MyClass<int, vector<int>>

	return 0;
}
