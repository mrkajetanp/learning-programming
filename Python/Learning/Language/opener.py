#!/usr/bin/python
import sys

try:
    script, filename = sys.argv
except ValueError as inst:
    print('Too few arguments!')
    print(inst)
    sys.exit()

txt = open(filename)

print("Opening file -", filename, "\n")
print(txt.read())

txt.close()

print("Type the filename: ")
file_2 = input("> ")
txt_2 = open(file_2)

print("Line 1:", txt_2.readline(), end="")
print("Line 2:", txt_2.readline())
txt_2.close()
