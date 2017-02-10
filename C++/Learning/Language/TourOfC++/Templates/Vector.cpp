#include <iostream>
#include <string>
#include <list>
#include <new>
#include <stdexcept>

using std::string;
using std::list;
using std::cout;
using std::endl;

/*
 * =====================================================================================
 *        Class:  Vector
 *  Description:  Generic Vector class using templates.
 * =====================================================================================
 */

template <typename T>
class Vector {
	private:
		T* elem;	// points to an array of sz elements of type T
		int sz;
	public:
		explicit Vector (int s);
		~Vector() { delete[] elem; }

		// copy and move operations

		T& operator[] (int i);
		const T& operator[] (int i) const;
		int size() const { return sz; }
};

template <typename T>
Vector<T>::Vector (int s) {
	if (s < 0)
		throw std::bad_array_new_length();
	elem = new T[s];
	sz = s;
}

template <typename T>
const T& Vector<T>::operator[] (int i) const {
	if (i < 0 || size() <= i)
		throw std::out_of_range("Vector::operator[]");
	return elem[i];
}

template <typename T>
T* begin (Vector<T>& x) {
	return x.size() ? &x[0] : nullptr;
}

template <typename T>
T* end (Vector<T>& x) {
	return begin(x)+x.size();
}

//void f2 (Vector<string>& vs) {
//	for (auto& s : vs)
//		cout << s << endl;
//}

void write (const Vector<string>& vs) {
	for (int i = 0; i < vs.size() ; ++i)
		cout << vs[i] << endl;
}

// Function Objects - Functors

template <typename T>
class LessThan {
	private:
		const T val;
	public:
		LessThan (const T& v) : val (v) {}
		bool operator()(const T& x) const { return x < val; } // call operator
};

template <typename C, typename P>
int count (const C& c, P pred) {
	int cnt = 0;
	for (const auto& x : c)
		if (pred(x))
			++cnt;
	return cnt;
}

// +Variadic Templates

template <typename T>
void g (T x) {
	cout << x << " ";
}

void f () {} // do nothing

template <typename T, typename... Tail>
void f (T head, Tail... tail) {
	g (head);
	f (tail...);
}

// -Variadic Templates

// +Aliasing

template <typename Key, typename Value>
class Map {
	// ...
};

template <typename Value>
using StringMap = Map<string, Value>;

StringMap<int> m; // m is a Map<string, int>

// -Aliasing

int main () {
	Vector<char> vc (200);
	Vector<string> vs (17);
	Vector<list<int>> vli (45);

	LessThan<int> lti (42);
	LessThan<string> lts ("Okay");

	bool b1 = lti (35);
	bool b2 = lts ("Zees");
	cout << b1 << " " << b2 << endl;

	f (1, 2.2, "hey-there", 0, "ok bye");

	//	int howMany = count (vc, LessThan<int>(10)); // counts how many numbers in vc are lt 10
	//	int howMany = count (vc, [&](int a){ return a < 10; }); // the same with lambda function
	return 0;
}










