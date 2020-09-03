// DO NOT EDIT. This is code generated via package:easy_localization/generate.dart
import 'dart:ui';

import 'package:easy_localization/easy_localization.dart' show AssetLoader;

class CodegenLoader extends AssetLoader{
  const CodegenLoader();

  @override
  Future<Map<String, dynamic>> load(String fullPath, Locale locale ) {
    return Future.value(mapLocales[locale.toString()]);
  }

    static const Map<String,dynamic> en_US = {
  "title": "Hello",
  "msg": "Hello {} in the {} world ",
  "msg_named": "{} are written in the {lang} language",
  "clickMe": "Click me",
  "profile": {
    "reset_password": {
      "label": "Reset Password",
      "username": "Username",
      "password": "password"
    }
  },
  "clicked": {
    "zero": "You clicked {} times!",
    "one": "You clicked {} time!",
    "two": "You clicked {} times!",
    "few": "You clicked {} times!",
    "many": "You clicked {} times!",
    "other": "You clicked {} times!"
  },
  "amount": {
    "zero": "Your amount : {} ",
    "one": "Your amount : {} ",
    "two": "Your amount : {} ",
    "few": "Your amount : {} ",
    "many": "Your amount : {} ",
    "other": "Your amount : {} "
  },
  "gender": {
    "male": "Hi man ;) ",
    "female": "Hello girl :)",
    "with_arg": {
      "male": "Hi man ;) {}",
      "female": "Hello girl :) {}"
    }
  },
  "reset_locale": "Reset Saved Language"
};
  static const Map<String,dynamic> fr_FR = {
  "title": "Bonjour",
  "msg": "Hello {} in the {} world ",
  "msg_named": "{} are written in the {lang} language",
  "clickMe": "Click me",
  "profile": {
    "reset_password": {
      "label": "Reset Password",
      "username": "Username",
      "password": "password"
    }
  },
  "clicked": {
    "zero": "You clicked {} times!",
    "one": "You clicked {} time!",
    "two": "You clicked {} times!",
    "few": "You clicked {} times!",
    "many": "You clicked {} times!",
    "other": "You clicked {} times!"
  },
  "amount": {
    "zero": "Your amount : {} ",
    "one": "Your amount : {} ",
    "two": "Your amount : {} ",
    "few": "Your amount : {} ",
    "many": "Your amount : {} ",
    "other": "Your amount : {} "
  },
  "gender": {
    "male": "Hi man ;) ",
    "female": "Hello girl :)",
    "with_arg": {
      "male": "Hi man ;) {}",
      "female": "Hello girl :) {}"
    }
  },
  "reset_locale": "Reset Saved Language"
};
  static const Map<String, Map<String,dynamic>> mapLocales = {"en_US": en_US, "fr_FR": fr_FR};
}
