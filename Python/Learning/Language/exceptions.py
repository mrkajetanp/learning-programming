try:
    x = int(input("Please enter a number: "))
    print("Number: {}".format(x))
except ValueError:
    print("Oops!  That was no valid number.  Try again...")

