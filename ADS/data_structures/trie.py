#!/usr/bin/env python3

class TrieNode:
    def __init__(self):
        self.nodes = dict()
        self.is_leaf = False


    def insert(self, word):
        curr = self
        for char in word:
            if char not in curr.nodes:
                curr.nodes[char] = TrieNode()
            curr = curr.nodes[char]
        curr.is_leaf = True


    def find(self, word):
        curr = self
        for char in word:
            if char not in curr.nodes:
                return False
            curr = curr.nodes[char]
        return curr.is_leaf


    def print(self):
        self.print_words("")
        print()


    def print_words(self, word=""):
        node = self

        if node.is_leaf:
            print(word, end=" ")

        for key, value in node.nodes.items():
            value.print_words(word + key)


root = TrieNode()

root.insert("banana")
root.insert("apple")
root.insert("beast")
root.insert("bread")
root.insert("cookie")
root.insert("test")

root.print()

print(root.find("cookie"))
print(root.find("oopsie"))
