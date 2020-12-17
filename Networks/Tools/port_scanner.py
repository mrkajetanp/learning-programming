#!/usr/bin/env python3

import sys
import socket
from datetime import datetime

if len(sys.argv) == 2:
    target = socket.gethostbyname(sys.argv[1])  # Hostname to IPv4
else:
    print('Invalid amount of arguments')

print('-' * 50)
print('Scanning target', target)
print('Time started:', str(datetime.now()))
print('-' * 50)

try:
    for port in range(50, 85):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        socket.setdefaulttimeout(1)
        result = s.connect_ex((target, port))  # returns an error indicator
        if result == 0:
            print('Port {} is open.'.format(port))
        s.close()

except KeyboardInterrupt:
    print('\nExiting..')
    sys.exit()

except socket.gaierror:
    print('Hostname could not be resolved')
    sys.exit()

except socket.error:
    print('Could not connect to the server')
    sys.exit()
