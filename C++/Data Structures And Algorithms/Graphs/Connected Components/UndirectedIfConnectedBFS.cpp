// Check if undirected Graph is connected using BFS

#include <iostream>
#include <list>
#include <queue>

using std::cout;
using std::endl;
using std::list;

class Graph {
private:
  int V;
  list<int> *adj;
public:
  Graph(int V) {
    this->V = V;
    adj = new list<int>[V];
  }
  void addEdge(int v, int w);
  void BFS(int s, bool visited[]);
  void printGraph();
  bool isConnected();
};

// Add edge connecting v and w
void Graph::addEdge(int v, int w) {
  adj[v].push_back(w);
  adj[w].push_back(v);
}

// Recursive function to print BFS starting from s
void Graph::BFS(int s, bool visited[]) {
  list<int> q;
  list<int>::iterator i;
  visited[s] = true;
  q.push_back(s);
  while (!q.empty()) {
    s = q.front();
    q.pop_front();
    for (i = adj[s].begin() ; i != adj[s].end() ; ++i) {
      if (!visited[*i]) {
        visited[*i] = true;
        q.push_back(*i);
      }
    }
  }
}

void Graph::printGraph() {
  list<int> ad = *adj;
  list<int>::const_iterator i;
  for (i = ad.begin() ; i != ad.end() ; ++i)
    cout << *i << " ";
  cout << endl;
}

int main() {
  Graph g1(5);
  g1.addEdge(0, 1);
  g1.addEdge(1, 2);
  g1.addEdge(2, 3);
  g1.printGraph();

  return 0;
}
