#include <iostream>
#include <memory>

using std::cout;
using std::endl;
using std::cin;
using std::string;

typedef struct X {
	X () {}
	X (int x) : a(x) {  }
	int a;
	string b;
} X;

void f (int i, int j) {
	// sp will automatically use delete
	std::unique_ptr<X> sp { new X };
	if (i < 99)
		return; // or throw
	if (j < 77)
		return;
	sp->a = 10;
}

std::unique_ptr<X> makeX (int i) {
	// make an X and give it to a unique_ptr
	// check i, etc..
	return std::unique_ptr<X> {new X(i)};
}

int main () {
	auto ptr1 = makeX (15);
	cout << ptr1->a << endl;

	auto sptr1 = std::make_shared<X> (10);


	return 0;
}
