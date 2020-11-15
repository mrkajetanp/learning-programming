#!/usr/bin/env python3

class Node:
    def __init__(self, value, parent):
        self.value = value
        self.parent = parent
        self.left = None
        self.right = None

    def __repr__(self):
        from pprint import pformat

        if self.left is None and self.right is None:
            return str(self.value)
        return pformat({"%s" % (self.value): (self.left, self.right)}, indent=1)

class BinarySearchTree:
    def __init__(self, root=None):
        self.root = root


    def __str__(self):
        return str(self.root)


    def __reassign_nodes(self, node, new_children):
        if new_children is not None:
            new_children.parent = node.parent
        if node.parent is not None:
            if self.is_right(node):
                node.parent.right = new_children
            else:
                node.parent.left = new_children
        else:
            self.root = new_children


    def is_right(self, node):
        return node == node.parent.right


    def empty(self):
        return self.root is None


    def __insert(self, value):
        new_node = Node(value, None)

        if self.empty():
            self.root = new_node
        else:
            parent_node = self.root
            while True:
                if value < parent_node.value:
                    if parent_node.left is None:
                        parent_node.left = new_node
                        break
                    else:
                        parent_node = parent_node.left
                else:
                    if parent_node.right is None:
                        parent_node.right = new_node
                        break
                    else:
                        parent_node = parent_node.right
            new_node.parent = parent_node


    def insert(self, *values):
        for value in values:
            self.__insert(value)
        return self


    def inorder(self):
        result = []

        node = self.root

        def traverse(arr, node):
            if node:
                traverse(arr, node.left)
                arr.append(node.value)
                traverse(arr, node.right)

        traverse(result, node)
        return result


    def search(self, value):
        if self.empty():
            raise IndexError("tree empty")
        else:
            node = self.root
            while node is not None and node.value is not value:
                node = node.left if value < node.value else node.right
            return node


    def get_max(self, node=None):
        if node is None:
            node = self.root
        if not self.empty():
            while node.right is not None:
                node = node.right
        return node


    def remove(self, value):
        node = self.search(value)
        if node is not None:
            if node.left is None and node.right is None:
                self.__reassign_nodes(node, None)
            elif node.left is None:
                self.__reassign_nodes(node, node.right)
            elif node.right is None:
                self.__reassign_nodes(node, node.left)
            else:
                tmp_node = self.get_max(node.left)
                self.remove(tmp_node.value)
                node.value = tmp_node.value


data = [8, 3, 6, 1, 10, 14, 9, 11]

t = BinarySearchTree()

for x in data:
    t.insert(x)

print(t)
print(t.inorder())
print(t.remove(8))
print(t.inorder())
