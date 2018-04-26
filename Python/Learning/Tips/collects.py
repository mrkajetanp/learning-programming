from collections import defaultdict
from collections import OrderedDict
from collections import Counter

favourites = defaultdict(list)
favourites['hello'].append("there")
favourites['hello'].append("yourself")

print(favourites)

dic = {'test': 'test'}
# dic['hello'].append("there")
print(dic)

colours = OrderedDict([("Red", 198), ("Green", 170), ("Blue", 160)])

for key, value in colours.items():
    print(key, value)

colours = (
    ('Yasoob', 'Yellow'),
    ('Ali', 'Blue'),
    ('Arham', 'Green'),
    ('Ali', 'Black'),
    ('Yasoob', 'Red'),
    ('Ahmed', 'Silver'),
)

favs = Counter(name for name, colour in colours)
print(favs)
