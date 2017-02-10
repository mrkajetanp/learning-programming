console.log(__filename);
console.log(__dirname);

const dirName: string = __dirname + "/";

console.log(dirName);

function printHello () {
  console.log("Hello!");
}

console.log("Prorgam starts..");
let t = setTimeout(printHello, 2000);
clearTimeout(t);
setTimeout(printHello, 2000);

console.log("Interval..");

t = setInterval(() => {
  console.log("Interval hello here...");
}, 2000);

setTimeout(() => {
  clearInterval(t);
}, 7000);
