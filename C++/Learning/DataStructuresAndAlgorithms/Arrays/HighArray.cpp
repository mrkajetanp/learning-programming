#include <iostream>

using std::cout;
using std::endl;
using std::cin;
using std::string;

class HighArray {
	private:
		int nItems;
		long *arr;
	public:
		HighArray (int max);
		~HighArray ();
		void insert (long value);
		bool remove (long value);
		bool find (long searchKey);
		long getMax ();
		long removeMax ();
		void display ();
		int size ();
		void noDups ();
};

HighArray::HighArray (int max) {
	arr = new long [max];
	nItems = 0;
	cout << "HighArray constructed!" << endl;
}

HighArray::~HighArray () {
	delete[] arr;
	cout << "HighArray destructed!" << endl;
}

void HighArray::insert (long value) {
	arr[nItems] = value;
	nItems++;
}

bool HighArray::remove (long value) {
	int i;
	for (i = 0 ; i < nItems ; i++)
		if (value == arr[i])
			break;
	if (i == nItems)
		return false;
	else {
		for (int j = i ; j < nItems ; j++)
			arr[j] = arr[j+1];
		nItems--;
		return true;
	}
}

bool HighArray::find (long searchKey) {
	int i;
	for (i = 0; i < nItems; ++i)
		if (arr[i] == searchKey)
			break;
	if (i == nItems)
		return false;
	else
		return true;
}

long HighArray::getMax () {
	if (nItems == 0)
		return -1;
	long max = 0;
	for (int i = 0; i < nItems ; ++i)
		if (arr[i] > max)
			max = arr[i];
	return max;
}

long HighArray::removeMax () {
	long maxVal = this->getMax ();
	this->remove (maxVal);
	return maxVal;
}

void HighArray::display () {
	for (int i = 0 ; i < nItems ; i++)
		cout << arr[i] << " ";
	cout << endl;
}

int HighArray::size () {
	return nItems;
}

int main () {
	HighArray ha (100);
	ha.insert (10);
	ha.insert (17);
	ha.insert (33);
	ha.insert (46);
	ha.display ();
	ha.remove (17);
	ha.remove (33);
	ha.insert (66);
	ha.insert (99);
	ha.insert (24);
	ha.display ();

	cout << "Max: " << ha.getMax() << endl;
	ha.removeMax ();
	ha.display ();
	cout << "Max: " << ha.getMax() << endl;

	cout << "Size: " << ha.size() << endl;

	if (ha.find (24))
		cout << "24 found!" << endl;
	else
		cout << "24 not found!" << endl;

	ha.remove (24);
	ha.display ();

	if (ha.find (24))
		cout << "24 found!" << endl;
	else
		cout << "24 not found!" << endl;

	return 0;
}












