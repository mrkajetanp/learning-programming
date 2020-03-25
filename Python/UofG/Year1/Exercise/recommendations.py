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

def get_similarity(ratings, name_a, name_b):
    a = ratings[name_a]
    b = ratings[name_b]
    return dot_product(a, b)

def get_users_recommended_books(books, ratings, name, user):
    result = []
    for i, rating in enumerate(ratings[user]):
        if rating > 2 and ratings[name][i] == 0:
            result.append(books[i])
    return result

def rate_random_books(books, ratings, number):
    ratings = [0] * 55
    rated = set()

    while len(rated) < number:
        next_book = random.randint(0, 54)
        if books[next_book] in rated:
            continue

        acceptable_ratings = [-5, -3, 0, 1, 3, 5]
        rating = 0
        while True:
            try:
                rating = int(input("Rate {}: ".format(books[next_book])))
                if rating not in acceptable_ratings:
                    raise ValueError()
                break
            except ValueError:
                print("Incorrect rating, try again")

        if rating == 0:
            continue

        rated.add(books[next_book])
        ratings[next_book] = rating

    return ratings

def add_to_ratings_file(name, ratings):
    with open("ratings.txt", "a") as rating_file:
        rating_file.write(name + "\n")
        rating_file.write(" ".join([str(x) for x in ratings]) + "\n")

def get_recommendations(books, ratings, name, number):
    similarities = [(x, dot_product(ratings[x], ratings[name])) for x in ratings if x != name]
    similarities = sorted(similarities, key=lambda x: x[1])

    for x in similarities:
        print(x[0], "similarity with", name + ":", x[1])
    print()

    print("Recommending based on similarity algorithm")
    print("+++++++++++++++++++++++++++++++++++")

    recommended = set()
    i = 1
    while len(recommended) < number:
        user = ""
        try:
            user = similarities[-i][0]
        except IndexError:
            break

        recommendations = get_users_recommended_books(books, ratings, name, user)
        if len(vector_difference(recommendations, recommended)) > 0:
            print("Recommended by user:", user)

        for r in recommendations:
            if r in recommended:
                continue

            print("        ", r)

            recommended.add(r)

            if len(recommended) == number:
                break
        i += 1


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

name = input("What is your name? ")
number = 0

if name not in ratings:
    print("User", name, "is not in the database, please rate the following books first.")
    new_ratings = rate_random_books(books, ratings, 11)
    add_to_ratings_file(name, new_ratings)
    ratings[name] = new_ratings

while True:
    try:
        number = int(input("How many recommendations? "))
        break
    except ValueError:
        print("Incorrect number, try again")

print()

get_recommendations(books, ratings, name, number)
