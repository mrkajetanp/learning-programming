#!/usr/bin/env python3

class Graph:
    def __init__(self):
        self.vertices = {}

    def print(self):
        for i in self.vertices:
            print(i, " : ", " -> ".join([str(j) for j in self.vertices[i]]))

    def add_edge(self, from_vertex, to_vertex):
        if from_vertex in self.vertices:
            self.vertices[from_vertex].append(to_vertex)
        else:
            self.vertices[from_vertex] = [to_vertex]

    def BFS(self, start_vertex):
        visited = set()
        result = [start_vertex]
        queue = [start_vertex]

        # mark the start node as visited, add to queue
        visited.add(start_vertex)

        while queue:
            vertex = queue.pop(0)

            # loop through adjacent vertices and add not visited to queue
            for adjacent in self.vertices[vertex]:
                if adjacent not in visited:
                    queue.append(adjacent)
                    visited.add(adjacent)
                    result.append(adjacent)

        return result


    def DFS(self, start_vertex):
        visited = set()
        result = []
        stack = [start_vertex]

        while stack:
            vertex = stack.pop()
            if vertex not in visited:
                result.append(vertex)
            visited.add(vertex)

            for adj in reversed(self.vertices[vertex]):
                if adj not in visited:
                    stack.append(adj)

        return result


if __name__ == "__main__":
    g = Graph()

    g.add_edge("S", "A")
    g.add_edge("S", "B")
    g.add_edge("S", "C")

    g.add_edge("A", "S")
    g.add_edge("A", "D")

    g.add_edge("B", "S")
    g.add_edge("B", "E")

    g.add_edge("C", "S")
    g.add_edge("C", "F")

    g.add_edge("D", "A")
    g.add_edge("D", "G")

    g.add_edge("E", "B")
    g.add_edge("E", "G")

    g.add_edge("F", "C")
    g.add_edge("F", "G")

    g.add_edge("G", "D")
    g.add_edge("G", "E")
    g.add_edge("G", "F")

    g.print()

    print(g.BFS("S"))
    print(g.DFS("S"))
