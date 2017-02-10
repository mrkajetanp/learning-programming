"""
Happy now?
"""
from terminaltables import AsciiTable, SingleTable

table_data_large = [
    ['', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'],
    ['8:00 - 8:45', 'Mathematics', 'PE', '', '', ''],
    ['8:55 - 9:40', 'History', 'Advisory', 'Business', 'French', 'Mathematics'],
    ['9:50 - 10:35', 'Civics', 'Business', 'Polish', 'Biology', 'English'],
    ['10:55 - 11:40', 'Safety E', 'Polish', 'Mathematics', 'History', 'PE'],
    ['11:50 - 12:35', 'Religion', 'English', 'Physics', 'Mathematics', 'Chemistry'],
    ['12:55 - 13:40', 'Polish', 'Geography', 'Religion', 'Culture', 'IT'],
    ['13:50 - 14:35', 'Polish', 'French', 'PE', 'English', 'IT']
]

table_data_small = [
    ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'],
    ['Mathematics', 'PE', '', '', ''],
    ['History', 'Advisory', 'Business', 'French', 'Mathematics'],
    ['Civics', 'Business', 'Polish', 'Biology', 'English'],
    ['Safety E', 'Polish', 'Mathematics', 'History', 'PE'],
    ['Religion', 'English', 'Physics', 'Mathematics', 'Chemistry'],
    ['Polish', 'Geography', 'Religion', 'Culture', 'IT'],
    ['Polish', 'French', 'PE', 'English', 'IT']
]

tableAsci = AsciiTable(table_data_large)
tableSingle = SingleTable(table_data_large)

if not (tableSingle.ok):    tableSingle = SingleTable(table_data_small)
if not (tableAsci.ok):    tableAsci = AsciiTable(table_data_small)

tableSingle.justify_columns[0] = 'center'
tableSingle.justify_columns[1] = 'center'
tableSingle.justify_columns[2] = 'center'
tableSingle.justify_columns[3] = 'center'
tableSingle.justify_columns[4] = 'center'
tableSingle.justify_columns[5] = 'center'

print(tableSingle.table)
print(tableAsci.table)
