import fs = require("fs");

class Student {
  fullName: string;
  constructor(public firstName, public lastName) {
    this.fullName = firstName + " " + lastName;
  }
}

interface Person {
  firstName: string;
  lastName: string;
}

function greeter(person: Person) {
  return "Hello " + person.firstName + " " + person.lastName + "!";
}

let userCajetan = new Student("Cajetan", "Puchalski");

console.log(greeter(userCajetan));

let x = 10;

let a = (x) => x *= 2;

console.log(a(x));
