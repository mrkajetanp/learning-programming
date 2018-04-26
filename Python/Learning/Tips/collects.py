from collections import defaultdict
from collections import OrderedDict
from collections import Counter
from collections import deque
from collections import namedtuple

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

d = deque()
d.append('1')
d.append('2')
d.append('3')

print(len(d))
print(d[0])
print(d[-1])

d = deque(range(5))
print(d)
d.popleft()
print(d)
d.pop()
print(d)

d = deque(maxlen=30)

Animal = namedtuple('Animal', 'name age type')
perry = Animal(name='Perry', age=31, type='Cat')
print(perry.name, perry.age, perry.type)
print(perry[0], perry[1], perry[2])
