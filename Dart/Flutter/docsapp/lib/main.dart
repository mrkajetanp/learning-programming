import 'package:flutter/material.dart';

import 'basics.dart';

void main() {
  runApp(MaterialApp(
      title: 'Shopping App',
      home: ShoppingList(
        products: <Product>[
          Product(name: 'Eggs'),
          Product(name: 'Flour'),
          Product(name: 'Chips'),
          Product(name: 'Booze'),
        ],
      )));
}
