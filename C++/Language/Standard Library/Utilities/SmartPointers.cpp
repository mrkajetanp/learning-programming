#include <iostream>
#include <string>
#include <vector>
#include <memory>

using std::cout;
using std::endl;
using std::cin;
using std::string;

void sharedPtrs () {
	std::cout << std::endl << "*** Shared Pointers ***" << std::endl;
	// two shared pointers representing two people by their name
	std::shared_ptr<string> pNico = std::make_shared<string> ("nico"); // a better way
	std::shared_ptr<string> pJutta (new string ("jutta"));

	std::shared_ptr<string> pNico4;
	//	pNico4 = new string ("nico"); // error, no assignment for ordinary pointers
	pNico4.reset (new string ("nico")); // OK

	// Capitalize people names
	(*pNico)[0] = 'N';
	pJutta->replace (0, 1, "J");

	// put them multiple times in a container
	std::vector<std::shared_ptr<string>> whoMadeCoffee;
	whoMadeCoffee.push_back (pJutta);
	whoMadeCoffee.push_back (pJutta);
	whoMadeCoffee.push_back (pNico);
	whoMadeCoffee.push_back (pJutta);
	whoMadeCoffee.push_back (pNico);

	// print all elements
	for (auto ptr : whoMadeCoffee)
		cout << *ptr << " ";
	cout << endl;

	// overwrite a name
	*pNico = "Nicolai";

	for (auto ptr : whoMadeCoffee)
		cout << *ptr << " ";
	cout << endl;

	cout << "use_count: " << whoMadeCoffee[0].use_count () << endl;

	/*  Custom Deleter  */
	std::shared_ptr<string> pNico2 (new string ("nico"), [] (string* p) {
																cout << "delete " << *p << endl;
																delete p;
															});
	pNico2 = nullptr; // pNico does not refer to the string any longer
	cout << "..." << endl;
	whoMadeCoffee.resize (2); // all copies of the string pNico are destroyed

	//	std::shared_ptr<int> p (new int[10]); // ERROR but compiles
	std::shared_ptr<int> p (new int[10], [] (int* p) {
												delete[] p;
											});

	// you can also use a helper function
	std::shared_ptr<int> p2 (new int[10], std::default_delete<int[]>());

	//	std::shared_ptr<int[]> p3 (new int[10]); // does not compile
}

class Person {
	public:
		string name;
		std::shared_ptr<Person> mother;
		std::shared_ptr<Person> father;
		std::vector<std::weak_ptr<Person>> kids;

	Person (const string& n, std::shared_ptr<Person> m = nullptr,
								std::shared_ptr<Person> f = nullptr)
							: name (n), mother (m), father (f) {
		// constructor
	}

	~Person () {
		cout << "delete " << name << endl;
	}

};

std::shared_ptr<Person> initFamily (const string& name) {
	std::shared_ptr<Person> mom (new Person (name + "'s mom"));
	std::shared_ptr<Person> dad (new Person (name + "'s dad"));
	std::shared_ptr<Person> kid (new Person (name, mom, dad));
	mom->kids.push_back (kid);
	dad->kids.push_back (kid);
	return kid;
}

void weakPtrs () {
	cout << endl << "*** Weak Pointers ***" << endl;
	std::shared_ptr<Person> p = initFamily ("Nico");
	cout << "Nico's family exists." << endl;
	cout << "- nico is shared " << p.use_count() << " times" << endl;
	cout << "- name of 1st kid of Nico's mom: " << p->mother->kids[0].lock()->name << endl;
	p = initFamily ("Jim");
	cout << "Jim's family exists!" << endl;

	try {
		std::shared_ptr<string> sp (new string("hi")); // create shared pointer
		std::weak_ptr<string> wp = sp; // create weak pointer out of it
		sp.reset(); // release object of shared pointer
		cout << wp.use_count() << endl; // prints: 0
		cout << std::boolalpha << wp.expired() << endl; // prints: true
		std::shared_ptr<string> p (wp); // throws std::bad_weak_ptr
	}
	catch (const std::exception& e) {
		std::cerr << "Exception: " << e.what() << endl; // prints bad_weak_ptr
	}
}

void uniquePtrs () {
//	std::unique_ptr<int[]> p (new int[10]); // OK, unique_ptr to an array of ints
//	std::unique_ptr<int, void(*)(int*)> p2 (new int[10], [] (int* p) { // custom deleter
//			delete[] p;
//			});
	std::cout << std::endl << "*** Unique Pointers ***" << std::endl;
	std::unique_ptr<std::string> sUp1 (new std::string ("nico"));
	(*sUp1)[0] = 'N';
	sUp1->append ("lai");
	if (sUp1)
		std::cout << *sUp1 << std::endl;

	sUp1.release ();

	if (sUp1)
		std::cout << *sUp1 << std::endl;

	std::unique_ptr<int> up2 (new int (10));
	//	std::unique_ptr<int> up3 (up2); // not possible, copy constructor won't work
	std::unique_ptr<int> up3 (std::move(up2));
	std::cout << *up3 << std::endl;

	std::unique_ptr<int> up4 (new int (5));
	std::unique_ptr<int> up5;
	//	up5 = up4; // can't copy unique_ptr
	up5 = std::move (up4);
	std::cout << *up5 << std::endl;

	std::unique_ptr<int[]> up6 (new int[10]); // automatically handles deletion
	up6[2] = 3;
	std::cout << up6[2] << std::endl;
}

int main () {
	sharedPtrs ();
	weakPtrs ();
	uniquePtrs ();
	return 0;
}

