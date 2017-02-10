from Node import Node

class LinkedList(object):

	def __init__(self):
		self.head = None
		self.counter = 0

	# O(1)
	def insertStart(self, data):
		self.counter += 1

		newNode = Node(data)

		if not self.head:
			self.head = newNode
		else:
			newNode.nextNode = self.head
			self.head = newNode

	# O(1) thanks to having the counter
	def size(self):
		return self.counter;

	def insertEnd(self, data):
		if self.head is None:
			self.insertStart(data)
			return

		self.counter += 1
		newNode = Node(data)
		actualNode = self.head

		while actualNode.nextNode is not None:
			actualNode = actualNode.nextNode

		actualNode.nextNode = newNode

	# O(n)
	def remove(self, data):
		if self.head:
			if data == self.head.data:
				self.head = self.head.nextNode
			else:
				self.head.remove(data, self.head)

	def printList(self):
		currNode = self.head

		while currNode is not None:
			print("%d " % currNode.data, end="");
			currNode = currNode.nextNode

		print("")
