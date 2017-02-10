var fs = require("fs");

console.log("Program begun");

fs.readFile("text.txt", function(err, data) {
  if (err)  return console.error(err);
  console.log(data.toString());
});

console.log("Program ended");
