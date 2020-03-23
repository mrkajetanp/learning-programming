import random

class Book:
    def __init__(self, author, title):
        self.author = author
        self.title = title

    def __str__(self):
        return self.title + " by " + self.author

    def __repr__(self):
        return self.author + " - " + self.title

def dot_product(a, b):
    return sum([x*y for x, y in zip(a, b)])

def vector_difference(a, b):
    return [x for x in a if x not in b]

books = []
with open("books.txt") as book_list:
    books = [l.strip().split(",") for l in book_list]
    books = [Book(x[0], x[1]) for x in books]

ratings = {}
with open("ratings.txt") as rating_list:
    last_name = ""
    for l in rating_list:
        if l.strip()[0].isalpha():
            last_name = l.strip()
        else:
            ratings[last_name] = [int(x) for x in l.strip().split(" ")]

