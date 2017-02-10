# output formatting
# reprlib provides a version of repr()
import reprlib
print(reprlib.repr(set('supercalifragilisticexpialidocious')))

# pprint - more sophisticated control over printing objects in a way readable by the interpreter
import pprint
t = [[[['black','cyan'],'white',['green','red']],[['magenta','yellow'],'blue']]]
pprint.pprint(t, width=30)
print()

# the textwrap method formats paragraphs of text to fit a given screen width
import textwrap
doc = """The wrap() method is just like fill() except that it returns a list of strings with newlines to separate the wrapped lines."""
print(textwrap.fill(doc, width=40))

# the locale module accesses a database of culture specific data formats
import locale
locale.setlocale(locale.LC_ALL,'en_US.utf8')
conv = locale.localeconv() # get a mapping of conventions
x = 1234567.8
print(locale.format('%d', x, grouping=True))
print(locale.format_string('%s%.*f', (conv['currency_symbol'], conv['frac_digits'], x), grouping=True))

# templating
# the string module includes a versatile Template class with a simplified syntax
from string import Template
t = Template('${village}folk send $$10 to $cause.')
print(t.substitute(village='Nottingham', cause='the ditch fund'))

# the substitute method raises KeyError when a placeholder is not supplied in a dictionary or a keyword argument
# safe_substitute() - will leave placeholders unchanged if data is missing
t2 = Template('Return the $item to $owner.')
d = dict(item='unladen swallow')
try:
    t2.substitute(d)
except KeyError as keinst:
    print('KeyError occurred.')
    print(type(keinst), '-', keinst)

print(t2.safe_substitute(d))

# template subclasses can specify a custom delimeter
import time, os.path
photofiles = ['img_1074.jpg', 'img_1076.jpg', 'img_1077.jpg']

class BatchRename(Template):
    delimiter = '%'

# fmt = input('Enter rename style (%d-date %n-seqnum %f-format): ')
fmt = 'Cajetan_%n%f'
t = BatchRename(fmt)
date = time.strftime('%d%b%y')
for i, filename in enumerate(photofiles):
    base, ext = os.path.splitext(filename)
    newname = t.substitute(d=date, n=i, f=ext)
    print('{0} -> {1}'.format(filename, newname))
# another application for templating is separating program logic from the details of multiple output formats

# the struct module provides pack() and unpack() functions for working with variable length binary record formats
# following example shows how to loop through header information in a ZIP file without using the zipfile module
# pack codes H and I represent two and four byte unsigned numbers respectively. the '<' indicates that they are std size and in little-endian byte order
"""
import struct

with open('myfile.zip','rb') as f:
    data = f.read()

start = 0
for i in range(3):
    start += 14
    fields = struct.unpack('<IIIHH', data[start:start+6])
    crc32, comp_size, uncomp_size, filenamesize, extra_size = fields

    start += 16
    filename = data[start:start+filenamesize]
    start += filenamesize
    extra = data[start:start+extra_size]
    print(filename, hex(crc32), comp_size, uncomp_size)

    start += extra_size + comp_size # skip to the next header
"""

# multi-threading
# how the high level threading module can run tasks in background while the main program continues to run
print()
import threading, zipfile

class AsyncZip(threading.Thread):
    def __init__(self, infile, outfile):
        threading.Thread.__init__(self)
        self.infile = infile
        self.outfile = outfile

    def run(self):
        f = zipfile.ZipFile(self.outfile,'w',zipfile.ZIP_DEFLATED)
        f.write(self.infile)
        f.close()
        print('Finished background zip of:', self.infile)

background = AsyncZip('mydata.txt', 'myarchive.zip')
print('Task in another thread is starting...', '\n')

background.start()
print('The main program continues to run in foreground.')
print('Something happens in here...', '\n')

background.join() # wait for the background task to finish
print('Main program waited until background was done.')

# the principial challenge of multi-threaded applications is coordinating threads that share data or other resources
# it is good to use queue objects with multi-threading

print()

# logging
# the logging module offers a full featured and flexible logging system
import logging
logging.debug('Debugging information')
logging.info('Informational message')
logging.warning('Warning:config file %s not found', 'server.conf')
logging.error('Error occurred')
logging.critical('Critical error - shutting down')
# the logging system can be configured directly from python or can be loaded from a user editable cfg file file for customized logging without altering the application

# weak references
import weakref, gc
class A:
    def __init__(self,value):
        self.value = value
    def __repr__(self):
        return str(self.value)

a = A(10) # creating a reference
d = weakref.WeakValueDictionary()
d['primary'] = a # does not create a reference
print(d['primary']) # fetch the if it is still alive
del a # removes the one reference
gc.collect() # run garbage collector right away
# d['primary'] # entry was automatically removed so it will cause an error

# tools for working with lists




















