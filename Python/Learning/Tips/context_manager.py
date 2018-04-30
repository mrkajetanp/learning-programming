from contextlib import contextmanager

class File(object):
    def __init__(self, file_name, method):
        self.file_obj = open(file_name, method)
    def __enter__(self):
        return self.file_obj
    def __exit__(self, type, value, traceback):
        self.file_obj.close()


with File('demo.txt', 'w') as opened_file:
    opened_file.write("hello there!")


@contextmanager
def open_file(name):
    f = open(file, 'w')
    yield f
    f.close()
