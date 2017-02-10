#include <iostream>

/*

Given any number of arrays each sorted in ascending order, find the nth smallest number of all their elements.
All the arguments except the last will be arrays, the last argument is n.
nthSmallest([1,5], [2], [4,8,9], 4) // returns 5 because it's the 4th smallest value
Be mindful of performance.



*/

using std::vector;
using std::cout;
using std::endl;

int nthSmallest(vector<vector<int>> arr, int n) {

	return 0;
}

int main() {
	int expected = 5;
	int actual = nthSmallest({ {1,5}, {2}, {4,8,9} }, 4);

	return 0;
}
