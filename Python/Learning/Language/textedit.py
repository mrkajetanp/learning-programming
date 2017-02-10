from sys import argv

script, filename = argv

print("Warning! This program will erase {:s} !".format(filename))

input("...")

print("Opening the file..")
target = open(filename, 'w') #w for write -> acts like .truncate()
#r(read), w(write), a(append) 

target.truncate()

line1 = input("line 1: ")
line2 = input("line 2: ")
line3 = input("line 3: ")

target.write(line1)
target.write("\n")
target.write(line2)
target.write("\n")
target.write(line3)
target.write("\n")


target.close()
