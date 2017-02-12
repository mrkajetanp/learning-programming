#include <iostream>

using std::cout;
using std::endl;

/*
 * =====================================================================================
 *        Class:  Complex
 *  Description:  An example of a concrete arithmetic type class
 * =====================================================================================
 */

class Complex {
	private:
		double re, im;
	public:
		Complex (double r, double i) : re(r), im(i) {}
		Complex (double r) : re(r), im(0) {}
		Complex () : re(0), im(0) {}

		double real () const { return re; }
		void real (double d) { re = d; }
		double imag () const { return im; }
		void imag (double d) { im = d; }

		// Overloading Class Operators
		Complex& operator+=(Complex z) { re += z.re, im +=z.im; return *this; }
		Complex& operator-=(Complex z) { re -= z.re, im -= z.im; return *this; }
		Complex& operator*=(Complex z) { re *= z.re, im *= z.im; return *this; }
		Complex& operator/=(Complex z) { re /= z.re, im /= z.im; return *this; }
};

Complex operator+(Complex a, Complex b) { return a+=b; }
Complex operator-(Complex a, Complex b) { return a+=b; }
Complex operator-(Complex a) { return {-a.real(), -a.imag()}; }
Complex operator*(Complex a, Complex b) { return a*=b; }
Complex operator/(Complex a, Complex b) { return a/=b; }

bool operator== (Complex a, Complex b) {
	return a.real()==b.real() && a.imag()==b.imag();
}

bool operator!= (Complex a, Complex b) {
	return !(a==b);
}

Complex sqrt (Complex);

/*
 * =====================================================================================
 *        Class:  Vector
 *  Description:  An example of a concrete container class
 * =====================================================================================
 */

class Vector {
	public:
		Vector () : elem (new double[1]), sz(1) {
			elem[0] = 0;
		}
		Vector (int s) : elem (new double [s]), sz (s) {
			for (int i = 0; i != s; ++i)
				elem[i] = 0;
		}

		~Vector () { delete[] elem; }

		// Copy constructor
		Vector (const Vector& a) : elem (new double[a.sz]), sz(a.sz) {
			for (int i = 0; i != sz; ++i)
				elem[i] = a.elem[i];
		}

		// Copy assignment
		Vector& operator= (const Vector& a) {
			double* p = new double[a.sz];
			for (int i = 0; i != a.sz ; ++i)
				p[i] = a.elem[i];
			delete[] elem;
			elem = p;
			sz = a.sz;
			return *this;
		}

		Vector (std::initializer_list<double> lst) :
			elem (new double[lst.size()]), sz(static_cast<int>(lst.size())) {
				std::copy (lst.begin(), lst.end(), elem);
		}

		void pushBack (double) {

		}

		double& operator[] (int i) { return elem[i]; } // element access
		int size() const { return sz; }
	private:
		double* elem;
		int sz;
};


/*
 * =====================================================================================
 *        Class:  Container
 *  Description:  An example of an abstract class
 * =====================================================================================
 */

class Container {
	public:
		virtual double& operator[] (int) = 0; // pure virtual function
		virtual int size () const = 0;	// const member function, pure virtual
		virtual ~Container () {}

};


/*
 * ===  FUNCTION  ======================================================================
 *         Name:  use
 *  Description:  Function using Container class
 * =====================================================================================
 */

void use (Container &c) {
	const int sz = c.size();
	for (int i = 0 ; i != sz ; i++)
		std::cout << c[i] << std::endl;
}

Vector read (std::istream& is) {
	Vector v;
	for (double d ; is>>d;)
		v.pushBack (d);
	return v;
}


/*
 * =====================================================================================
 *        Class:  VectorContainer
 *  Description:  Vector class derived from container
 * =====================================================================================
 */

class VectorContainer : public Container {
	Vector v;
	public:
		VectorContainer (int s) : v (s) {}
		~VectorContainer () {}

		double& operator[] (int i) { return v[i]; }
		int size() const { return v.size(); }
};

void g () {
	//	VectorContainer vc {10, 18, 234, 32 , 23 , 3, 123};
	//	use (vc);
}

int main () {
	Vector v (6);
	Complex a (2.3);
	Complex b (1/a);
	Complex c (a+2*Complex(1,2.3));

	Vector v1 = { 1, 2, 3, 4, 5 };
	Vector v2 = { 1.23, 3.45, 6.7, 8 };

	std::cout << a.real() << std::endl;
	std::cout << b.real() << std::endl;
	std::cout << c.real() << std::endl;

	return 0;
}








