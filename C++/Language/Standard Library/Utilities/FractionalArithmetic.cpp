#include <iostream>
#include <ratio>

void ratios () {
    typedef std::ratio<5,3> FiveThirds;
    std::cout << FiveThirds::num << "/" << FiveThirds::den << std::endl;

    typedef std::ratio<25,15> AlsoFiveThirds;
    std::cout << AlsoFiveThirds::num << "/" << AlsoFiveThirds::den << std::endl;

    typedef std::ratio<42,42> one;
    std::cout << one::num << "/" << one::den << std::endl;

    typedef std::ratio<0> zero;
    std::cout << zero::num << "/" << zero::den << std::endl;

    typedef std::ratio<7, -3> neg;
    std::cout << neg::num << "/" << neg::den << std::endl;

    typedef std::ratio_add<std::ratio<2,7>,std::ratio<2,6>>::type myRatio;

    std::cout << myRatio::num << "/" << myRatio::den << std::endl;

    bool ifEqual = std::ratio_equal<std::ratio<5,3>,std::ratio<25,15>>::value;

    std::cout << std::boolalpha << ifEqual << std::endl;



}

int main () {
    ratios ();

    return 0;
}
