import random

### Utility functions ###

class Book:
    def __init__(self, author, title):
        self.author = author
        self.title = title

    def __str__(self):
        return self.title + " by " + self.author

    def __repr__(self):
        return self.author + " - " + self.title

# Calculates the dot product of vectors a and b
def dot_product(a, b):
    return sum([x*y for x, y in zip(a, b)])

# Returns the difference of a and b
def difference(a, b):
    return [x for x in a if x not in b]

### Database functions ###

# Reads books from file, stores them in a list and returns it
def get_books(filename):
    books = []
    with open(filename) as book_list:
        books = [l.strip().split(",") for l in book_list]
        books = [Book(x[0], x[1]) for x in books]
    return books

# Reads ratings from file, stores them in a map mapping names to lists of ratings and returns it
def get_ratings(filename):
    ratings = {}
    with open(filename) as rating_list:
        last_name = ""
        for l in rating_list:
            # If a line starts with a letter, it's a name, otherwise it's the rating list
            if l.strip()[0].isalpha():
                # Store the name for next iteration
                last_name = l.strip()
            else:
                # Encountered a list of ratings
                # Add it to the map using the last encountered name
                ratings[last_name] = [int(x) for x in l.strip().split(" ")]
    return ratings

# Returns a list of 55 integers containing user-given ratings for randomly chosen books
# Used for new users to fill in their database entry
# Puts in books that weren't covered by the random search as 0 (not read)
# Keeps giving books to rate until user name has rated "number" of books differently than 0
def rate_random_books(books, ratings, number):
    # List with 55 0s, just like in the ratings.txt file
    ratings = [0] * 55
    # Set with books already rated by the user
    rated = set()

    # Keep iterating until the user rates the required number of books
    while len(rated) < number:
        next_book = random.randint(0, 54)
        # Make sure books aren't repeated
        if books[next_book] in rated:
            continue

        # Make sure that user gives an acceptable rating, keep asking until that happens
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

        # Don't consider the book 'rated' if the user hasn't read it
        if rating == 0:
            continue

        rated.add(books[next_book])
        ratings[next_book] = rating

    return ratings

## Writes an entry to the ratings file in accordance with the already existing pattern
def add_to_ratings_file(user_name, ratings):
    with open("ratings.txt", "a") as rating_file:
        rating_file.write(user_name + "\n")
        rating_file.write(" ".join([str(x) for x in ratings]) + "\n")

### Recommendations functions ###

# Returns a list of books recommended for "user_name" by "other_user_name"
def get_users_recommended_books(books, ratings, user_name, other_user_name):
    result = []
    for i, rating in enumerate(ratings[other_user_name]):
        # Only pick "other_user_name"s books rated 3 or 5 that "user_name" didn't read yet
        if rating > 2 and ratings[user_name][i] == 0:
            result.append(books[i])
    return result

## Main function in the program, gets "number" recommendations for user with name "user_name"
def get_recommendations(books, ratings, user_name, number):
    # Get similarity scores with all the other users using the dot product and sort them
    similarities = [(x, dot_product(ratings[x], ratings[user_name]))
                    for x in ratings if x != user_name]
    similarities = sorted(similarities, key=lambda x: x[1])

    output_file = open("output.txt", "w")

    for x in similarities:
        line = x[0] + " similarity with " + user_name + ": " + str(x[1])
        print(line)
        output_file.write(line + "\n")
    print()

    print("Recommending based on similarity algorithm")
    print("+++++++++++++++++++++++++++++++++++")
    output_file.write("\nRecommending based on similarity algorithm\n")
    output_file.write("+++++++++++++++++++++++++++++++++++\n")

    # Set containing already recommended books
    recommended = set()
    # Counter for incrementally choosing users from the end of the similarities list
    i = 1
    while len(recommended) < number:
        # Guard against program crashing if user wants more recommendations than available books
        other_user_name = ""
        try:
            other_user_name = similarities[-i][0]
        except IndexError:
            break

        recommendations = get_users_recommended_books(
            books, ratings, user_name, other_user_name
        )
        # Don't print the line if there are no books to be recommended by "other_user_name"
        if len(difference(recommendations, recommended)) > 0:
            print("Recommended by other_user_name:", other_user_name)
            output_file.write("Recommended by other_user_name: " + other_user_name + "\n")

        for r in recommendations:
            # Skip if the book has already been recommended
            if r in recommended:
                continue

            # Print the indented recommendation
            print("        ", r)
            output_file.write("         {}\n".format(r))

            recommended.add(r)
            ## Stop iterating if you reach enough recommendations
            if len(recommended) == number:
                break
        i += 1

    output_file.close()

### Main functionality ###

books = get_books("books.txt")
ratings = get_ratings("ratings.txt")

user_name = input("What is your user_name? ")
number = 0

# Update the database if the user name is not in it yet
if user_name not in ratings:
    print("User", user_name, "is not in the database, please rate the following books first.")
    new_rating = rate_random_books(books, ratings, 11)
    add_to_ratings_file(user_name, new_rating)
    ratings[user_name] = new_rating

# Make sure that user gives a valid number as the number of recommendations
while True:
    try:
        number = int(input("How many recommendations? "))
        if number < 1:
            raise ValueError()
        break
    except ValueError:
        print("Incorrect number, try again")

print()

get_recommendations(books, ratings, user_name, number)
