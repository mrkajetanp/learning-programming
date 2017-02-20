#include <iostream>

void clocks () {
    auto systemStart = std::chrono::system_clock::now();
    std::chrono::seconds twentySeconds (20);
    std::chrono::hours aDay (24);
    std::chrono::milliseconds aMillisecond (1);

    std::chrono::milliseconds ms;

    ms += twentySeconds + aDay; // 86,400,000
    --ms; // 86,399,999
    ms *= 2; // 172,839,998
    std::cout << ms.count() << " ms" << std::endl;
    std::cout << std::chrono::nanoseconds(ms).count() << " ns" << std::endl;

    std::chrono::seconds twoPointFiveMins (150);
    std::chrono::minutes
        mins = std::chrono::duration_cast<std::chrono::minutes> (twoPointFiveMins);
    std::cout << mins.count() << std::endl;

    auto diff = std::chrono::system_clock::now() - systemStart;
    auto nsec = std::chrono::duration_cast<std::chrono::nanoseconds> (diff);
    std::cout << "Program runs: " << nsec.count() << "ns" << std::endl;



}

int main () {
    clocks ();

    return 0;
}
