people = 30
cars = 40
trucks = 15

if cars > people:
    print("Take the cars.")
elif cars < people:
    print("Don't take the cars!")
else:
    print("We can't decide ;/")

if trucks > cars:
    print("Too many trucks!")
elif trucks < cars:
    print("Maybe we could take the trucks?")
else:
    print("We can't decide.")

if people > trucks:
    print("Let's just take the trucks!")
else:
    print("Let's stay home then :v")

"""
and
or
not

!!Prints from python 2, don't copy this shit :v

print "You enter a dark room with two doors.  Do you go through door #1 or door #2?"

door = raw_input("> ")

if door == "1":
    print "There's a giant bear here eating a cheese cake.  What do you do?"
    print "1. Take the cake."
    print "2. Scream at the bear."

    bear = raw_input("> ")

    if bear == "1":
        print "The bear eats your face off.  Good job!"
    elif bear == "2":
        print "The bear eats your legs off.  Good job!"
    else:
        print "Well, doing %s is probably better.  Bear runs away." % bear

elif door == "2":
    print "You stare into the endless abyss at Cthulhu's retina."
    print "1. Blueberries."
    print "2. Yellow jacket clothespins."
    print "3. Understanding revolvers yelling melodies."

    insanity = raw_input("> ")

    if insanity == "1" or insanity == "2":
        print "Your body survives powered by a mind of jello.  Good job!"
    else:
        print "The insanity rots your eyes into a pool of muck.  Good job!"

else:
    print "You stumble around and fall on a knife and die.  Good job!"

#How do I tell if a number is between a range of numbers?
#   You have two options: Use 0 < x < 10 or 1 <= x < 10, which is classic notation, or use x in range(1, 10).
"""
