var fs = require("fs");

var data = fs.readFileSync("text.txt");

console.log("Program begun");
console.log(data.toString());
console.log("Program Ended");
