from sys import argv

script, user_name = argv
prompt = '>'

print(user_name, script)
print("Do you like me " + user_name + "?")
likes = input(prompt)

print("Where do you live " + user_name + "?")
lives = input(prompt)

print("""
So you said {:s} about liking me.
You live in {:s}.
""".format(likes, lives))

print("Script: ", script)

age = int(input("Age? "))
print("Age*2: ", age*2)


