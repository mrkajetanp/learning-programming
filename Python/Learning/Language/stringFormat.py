#{} means just the next positional argument, with default format;
#{0} means the argument with index 0, with default format;
#{:d} is the next positional argument, with decimal integer format;
#{0:d} is the argument with index 0, with decimal integer format.
# %r -> raw data
hilarious = False
joke_eval = "Funny? %r"
print(joke_eval % hilarious)

name = "Kajtek"
age = 16
height = 188
hair = "Blonde" 

print("What about %s ?" % name)
print("What about {:s} ?".format(name))
print("He's {:d} years old and {:d} cm high.".format(age, height)) 

l = "Left.."
r = ".Right"
print(l+r)


print("." * 10)  # what'd that do?

end1 = "C"
end2 = "h"
end3 = "e"
end4 = "e"
end5 = "s"
end6 = "e"
end7 = "B"
end8 = "u"
end9 = "r"
end10 = "g"
end11 = "e"
end12 = "r"

# watch that comma at the end.  try removing it to see what happens
print(end1 + end2 + end3 + end4 + end5 + end6, end=" ")
print(end7 + end8 + end9 + end10 + end11 + end12)

days = "Mon Tue Wed Thu Fri Sat Sun"
months = "\nJan\nFeb\nMarch\nApr\nMay\nJun\nJul\nAug"

print("\nDays: ", days)
print("Months:", months)
print("""
heey
let's print some things in here
or maybe in here? 
why not here?
damn it's nice!
""")

tab_cat = "\tI'm a tabbed cat."
cat = "I'm split\non a line."
backslash_cat = "I'm \\ a \\ cat."

fat_cat = """
List:
\t* Cat food
\t* Fishes
\t* Catnip\n\t* Grass
"""

print(tab_cat)
print(cat)
print(backslash_cat)
print(fat_cat)

print('\n')

print("I am 6'2\" tall.") #escape "
print('I am 6\'2" tall.') #escape '






