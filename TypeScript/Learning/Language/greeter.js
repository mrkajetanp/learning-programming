define(["require", "exports"], function (require, exports) {
    "use strict";
    var Student = (function () {
        function Student(firstName, lastName) {
            this.firstName = firstName;
            this.lastName = lastName;
            this.fullName = firstName + " " + lastName;
        }
        return Student;
    }());
    function greeter(person) {
        return "Hello " + person.firstName + " " + person.lastName + "!";
    }
    var userCajetan = new Student("Cajetan", "Puchalski");
    console.log(greeter(userCajetan));
    var x = 10;
    var a = function (x) { return x *= 2; };
    console.log(a(x));
});
