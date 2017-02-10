#!/usr/bin/python
import os, sys

while True:
    text = input('> ')
    if(text == 'cls()'):
        os.system('clear')
        continue
    if(text == 'exit()'):
        sys.exit()

    os.system('google-translate en pl ' + text)