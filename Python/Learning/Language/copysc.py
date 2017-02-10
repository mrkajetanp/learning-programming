from sys import argv
from os.path import exists

script, from_file, to_file = argv

print(from_file, ">", to_file)

indata = open(from_file).read()
#indata = in_file.read() **

print(len(indata))
print(exists(to_file))
input("...")

out_file = open(to_file, 'w')
out_file.write(indata)

print("Done.")

out_file.close()
#in_file.close() **
