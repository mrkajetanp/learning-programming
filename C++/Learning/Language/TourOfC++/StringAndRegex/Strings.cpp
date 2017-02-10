#include <iostream>

using std::string;
using std::cout;
using std::endl;

string compose (const string& name, const string& domain) {
	return name + '@' + domain;
}

string name = "Cajetan Puchalski";

void m3 () {
	string s = name.substr (8, 9);
	cout << name << endl;

	name.replace (0, 7, "hello");
	std::cout << name << std::endl;

	name[0] = toupper (name[0]);
	std::cout << name << std::endl;

	cout << endl;
}

void respond (const string& answer) {
	string hoho = "hoho";
	if (answer == hoho)
		cout << "Hoho it is !" << endl;
	else if (answer == "yes")
		cout << "Yes indeed." << endl;
}

int main () {
	auto addr = compose ("cajetan.puchalski", "gmail.com");

	std::cout << addr << std::endl;
	addr += " !";
	std::cout << addr << std::endl;
	cout << endl;

	m3 ();

	respond ("hoho");
	respond ("yes");

	for (auto& c : addr)
		cout << c << " ";
	cout << endl;

	return 0;
}
