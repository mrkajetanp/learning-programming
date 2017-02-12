#include <iostream>
#include <utility>

using std::cout;
using std::endl;

void swaps () {
    cout << "Swaps: " << endl;
    int a = 8, b = 4;
    cout << a << " " << b << endl;
    std::swap (a, b);
    cout << a << " " << b << endl;
    cout << endl;
}

void exchanges () {
    cout << "Exchanges: " << endl;
    int a = 10, b = 15;
    cout << a << " " << b << endl;
    cout << endl;
}


int main () {
    swaps();
    exchanges();

    return 0;
}
