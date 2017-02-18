#include <iostream>
#include <boost/locale.hpp>

// Should build it with boost support
int main () {
    std::cout << "test" << std::endl;
    boost::locale::generator gen;
    std::locale loc = gen ("");

    return 0;
}
