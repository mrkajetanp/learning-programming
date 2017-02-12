#include <iostream>
#include <numeric>
#include <list>
#include <vector>
#include <random>
#include <functional>

using std::cout;
using std::endl;
using std::cin;
using std::string;

class RandInt {
	private:
		std::default_random_engine re;
		std::uniform_int_distribution<> dist;
	public:
		RandInt (int low, int high) : dist { low, high } {  }
		int operator() () { return dist(re); }
};

void numericalAlgorithms () {
	std::list<double> lst1 = { 1, 2, 3, 4, 5, 9999.9999 };
	auto s = std::accumulate (lst1.begin(), lst1.end(), 0.0);
	cout << s << endl;
}

void randomNumbers () {
	using myEngine = std::default_random_engine; // type of randomization engine
	using myDistribution = std::uniform_int_distribution<>; // type of distribution

	myEngine re {};
	myDistribution oneSixty { 1, 60 };
	auto die = std::bind (oneSixty, re);
	cout << die() << " " << die() << " " << die() << endl;

	constexpr int max = 8;
	RandInt rnd = { 0, max };
	std::vector<int> histogram (max+1);
	for (int i = 0; i != 200 ; ++i)
		++histogram[rnd()];

	for (int i = 0 ; i != static_cast<int>(histogram.size()) ; ++i) {
		cout << i << " ";
		for (int j = 0 ; j != histogram[i] ; ++j)
			cout << '*';
		cout << endl;
	}
}

int main () {
	numericalAlgorithms ();
	randomNumbers ();
	return 0;
}
