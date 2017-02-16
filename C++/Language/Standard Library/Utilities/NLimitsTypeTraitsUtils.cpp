#include <iostream>
#include <limits>

void numericLimits () {
    std::cout << std::endl;
    std::cout << "*** Numeric Limits ***" << std::endl;

    std::cout << std::numeric_limits<int>::min() << std::endl;
    std::cout << std::numeric_limits<int>::max() << std::endl;
}

template <typename T>
void foo (const T& val) {
    if (std::is_pointer<T>::value)
        std::cout << "foo() called for a pointer to: " << val << std::endl;
    else
        std::cout << "foo() called for a value: " << val << std::endl;
}

template <typename T>
void foo2_impl (T val, std::true_type) {
    std::cout << "Integral: " << val << std::endl;
    // ...
}

template <typename T>
void foo2_impl (T val, std::false_type) {
    std::cout << "Floating: " << val << std::endl;
    // ...
}

template <typename T>
void foo2 (T val) {
    foo2_impl (val, std::is_integral<T>());
}

// Function returns the smaller value, and std::commo_type automatically chooses the return type
template <typename T1, typename T2>
typename std::common_type<T1,T2>::type min (const T1& x, const T2& y) {
    return (x < y) ? x : y;
}


void typeTraits () {
    std::cout << std::endl;
    std::cout << "*** Type Traits ***" << std::endl;
    int a = 20;
    int *b = &a;

    foo (a);
    foo (std::ref(a));
    foo (std::cref(a));
    foo (b);

    float c = 33.3;
    foo2 (a);
    foo2 (c);

    std::cout << min (10, 13.3) << std::endl; // returns 10 (int)
    std::cout << min (20, 13.3) << std::endl; // returns 13.3 (float)


    std::cout << std::endl;
}

int main () {
    numericLimits ();
    typeTraits ();
    return 0;
}

