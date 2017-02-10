#include <iostream>
#include <gtest/gtest.h>

int f (int a) {
	return a*2;
}

void basicAssertions () {
	ASSERT_EQ (f(3), 6) << "f(3) and 6 are not equal!";
}

int main (int argc, char* argv[]) {
	testing::InitGoogleTest (&argc, argv);
	basicAssertions ();
	return RUN_ALL_TESTS ();

	return 0;
}
