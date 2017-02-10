#include <iostream>
#include <vector>
#include <mutex>
#include <thread>
#include <chrono>
#include <future>

using std::cout;
using std::endl;
using std::cin;
using std::string;
using std::vector;

class F {
	private:
		const vector<double>& v; // source of input
		double* res; // target of output
	public:
		F (const std::vector<double>& vv, double* p) : v (vv), res (p) {  }
		void operator()() {
			*res = 0;
			for (double d : v)
				*res += d;
		}
};

void f (const std::vector<double>& v, double* res) { // take input from v, place result in *res
	*res = 0;
	for (double d : v)
		*res += d;
}

void tasksAndThreads () {
	vector<double> someVec { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	vector<double> vec2 { 10, 11, 12, 13, 14 };

	double res1, res2;

	std::thread t1 { f, std::cref (someVec), &res1 }; // f (some_vec, &res1) executes in 1 thread
	std::thread t2 { F{vec2, &res2} }; // F (vec2, &res2)() executes in separate thread

	t1.join();
	t2.join();

	cout << res1 << " " << res2 << endl;
}

std::mutex m, m1, m2, m3; // controlling mutex
int sh = 0; // shared data

void f2 () {
	std::unique_lock<std::mutex> lck (m); // acquire mutex
	sh += 8; // manipulate shared data
	// release mutex implicitly at the end of the block
	m.unlock (); // release it explicitly
}

void antiDeadlock () {
	// defer_lock - don't yet try to acquire the mutex
	std::unique_lock<std::mutex> lck1 ( m1, std::defer_lock );
	std::unique_lock<std::mutex> lck2 ( m2, std::defer_lock );
	std::unique_lock<std::mutex> lck3 ( m3, std::defer_lock );

	std::lock (lck1, lck2, lck3); // acquire all three locks
	// manipulate shared data
} // implicitly release all mutexes

void sharingData () {
	f2 ();
	antiDeadlock ();
}

void waitingForEvents () {
	auto t0 = std::chrono::high_resolution_clock::now();
	std::this_thread::sleep_for (std::chrono::milliseconds(20));
	auto t1 = std::chrono::high_resolution_clock::now();

	cout << std::chrono::duration_cast<std::chrono::nanoseconds> (t1-t0).count() << " nanoseconds passed" << endl;
}

void communicatingTasks () {
}

int main() {
	tasksAndThreads ();
	sharingData ();
	waitingForEvents ();
}
















