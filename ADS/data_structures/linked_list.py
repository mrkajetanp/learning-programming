#!/usr/bin/env python3

class Node:
    def __init__(self, data):
        self.data = data
        self.next = None

    def __repr__(self):
        return f"Node({self.data})"


class LinkedList:
    def __init__(self):
        self.head = None


    def __iter__(self):
        node = self.head
        while node:
            yield node.data
            node = node.next


    def __len__(self):
        return len(tuple(iter(self)))


    def __repr__(self):
        return "->".join([str(item) for item in self])


    def __getitem__(self, index):
        if not 0 <= index < len(self):
            raise ValueError("index out of range")
        for i, node in enumerate(self):
            if i == index:
                return node


    def insert_tail(self, data):
        self.insert_nth(len(self), data)


    def insert_head(self, data):
        self.insert_nth(0, data)


    def insert_nth(self, index, data):
        if not 0 <= index <= len(self):
            raise ValueError("index out of range")

        new_node = Node(data)

        if self.head is None:
            self.head = new_node
        elif index == 0:
            new_node.next = self.head
            self.head = new_node
        else:
            temp = self.head
            for _ in range(index - 1):
                temp = temp.next
            new_node.next = temp.next
            temp.next = new_node


lst = LinkedList()
lst.insert_tail(1)
lst.insert_tail(2)
lst.insert_tail(3)
lst.insert_head(0)
lst.insert_tail(5)
print(lst)
