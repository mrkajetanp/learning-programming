#include <iostream>

using std::cout;
using std::endl;

enum class Color { red, blue, green };
enum class TrafficLight { green, yellow, red };

// Plain enum
enum Color2 { red, green, blue };

Color& operator++ (Color& t) {
	switch (t) {
		case Color::red:
			return t = Color::blue;
		case Color::blue:
			return t = Color::green;
		case Color::green:
			return t = Color::red;
	}
}

int main () {
	Color col = Color::red;
	TrafficLight light = TrafficLight::red;

	// Color x = red; // error, which red?
	// Color y = TrafficLight::red; // error, that red is not a Color
	Color z = Color::red; // OK

	// int i = Color::red; // error
	// Color c = 2; // error

	// We can define operators for enums

	Color next = ++z; // next becomes Color::blue
    cout << "OK" << endl;

	int colo = green;
	cout << colo << endl;
	return 0;
}
