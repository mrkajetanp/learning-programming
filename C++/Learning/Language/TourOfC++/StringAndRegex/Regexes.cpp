#include <iostream>
#include <regex>

using std::regex;
using std::smatch;
using std::string;

int main () {
	string line = "aoeusoetuh oestuhaoesuth soeu";

	regex pat (R"(\w{2}\s*\d{5}(-\d{4})?)");
	smatch matches; // smatch is a vector of string matches

	std::regex_search(line, matches, pat); // finds pat in line and stores them in matches
	//	regex_match

	// There is a regex_iterator

	return 0;
}

