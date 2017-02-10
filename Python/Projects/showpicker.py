#!/usr/bin/python

import random

showsnum = int(input("How many shows: "))
shows = []

for i in range(0, showsnum):
    shows.append(input("Show: "))

print(shows[random.randint(0, showsnum-1)])
