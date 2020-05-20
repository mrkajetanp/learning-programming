
void basics() {
  var number = 6;
  printNum(number);

  var one = '1';
  assert(int.parse(one) == 1);
  assert(double.parse('1.1') == 1.1);
  assert(2.toString() == '2');
  assert(3.1415.toStringAsFixed(2) == '3.14');
  assert(''.isEmpty);

  var works = 'The plus operator ' + 'works!';
  var also_works = 'Even without the +, still ''works!';
  print(works);
  print(also_works);

  lists();
  sets();
  maps();
}

void lists() {
  var list = [1, 2, 3];
  assert(list.length == 3);
  assert(list[1] == 2);

  var list2;

  assert([0, ...list].length == 4);
  assert([0, ...?list2].length == 1);

  var promoActive = true;
  var nav = [
    'Home',
    'Furniture',
    'Plants',
    if (promoActive) 'Outlet'
  ];

  print(nav);

  var listOfInts = [1, 2, 3];
  var listOfStrings = [
    '#0',
    for (var i in listOfInts) '#$i'
  ];
  assert(listOfStrings[1] == '#1');
}

void sets() {
  var halogens = {'fluorine', 'chlorine', 'bromine', 'iodine', 'astatine'};
  var elements = <String>{};
  elements.add('fluorine');
  elements.addAll(halogens);
  assert(elements.length == 5);
}

void maps() {
  var gifts = {
    // Key:    Value
    'first': 'partridge',
    'second': 'turtledoves',
    'fifth': 'golden rings'
  };

  var nobleGases = Map();
  nobleGases[2] = 'helium';
  nobleGases[10] = 'neon';
  nobleGases[18] = 'argon';

  bool isNoble(int atomicNumber) => nobleGases[atomicNumber] != null;

  print(isNoble(2));
}


void printNum(int num) {
  dynamic name = 'Bob';
  print('The $name is $num');

  int lineCount;
  assert(lineCount == null);

  // final finalName = 'James';
  // finalName = 'something';
  // const pi = 3.14;

  // var foo = const [];
  // const var foo = [];
}
