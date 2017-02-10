console.log(__filename);
console.log(__dirname);
var dirName = __dirname + "/";
console.log(dirName);
function printHello() {
    console.log("Hello!");
}
console.log("Prorgam starts..");
var t = setTimeout(printHello, 2000);
clearTimeout(t);
setTimeout(printHello, 2000);
console.log("Interval..");
t = setInterval(function () {
    console.log("Interval hello here...");
}, 2000);
setTimeout(function () {
    clearInterval(t);
}, 7000);
