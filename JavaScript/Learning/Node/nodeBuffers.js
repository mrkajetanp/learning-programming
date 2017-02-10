// Creating Buffers

var tbuff1 = new Buffer(10); // buffer of 10 octets
var tbuff2 = new Buffer([10, 20, 30, 40, 50]); // buffer from the given array
var tbuff3 = new Buffer("Simply easy learning."); // buffer from the string

// Writing to Buffers
// buf.write(string[, offset][, length][, encoding])

var buff1 = new Buffer(256);
len = buff1.write("Simply easy learning");
console.log("Octets written: " + len);

// Reading from Buffers
// buf.toString([encoding][, start][, end])

buff1 = new Buffer(26);
for (let i = 0 ; i < 26 ; i++)  buff1[i] = i + 97;

console.log( buff1.toString('ascii')); // encoded with ascii
console.log( buff1.toString() ); // encoded with utf8
console.log( buff1.toString('ascii', 0, 5));
console.log( buff1.toString('utf8', 0, 5));
console.log( buff1.toString(undefined, 0, 5)); // utf8
console.log( buff1.toString(undefined, 2, 5));

// Convert Buffer to JSON

buff1 = new Buffer("Learning node is so cool.");
var jsonBuff1 = buff1.toJSON();
console.log(jsonBuff1);

// Concatenate Buffers
// Buffer.concat(list[, totalLength])

var buff2a = new Buffer("Everyone knows that ");
var buff2b = new Buffer("life sucks.");
var buff2ab = Buffer.concat([buff2a, buff2b]);
console.log("Buffer 2ab: " + buff2ab.toString());

// Comparing Buffers
// buf.compare(otherBuffer);

var buff3a = new Buffer("ABCD");
var buff3b = new Buffer("ABC");
var result = buff3a.compare(buff3b);

console.log("Result: " + result);
if (result < 0) console.log("Buffer " + buff3a + " comes before " + buff3b);
else if (result == 0) console.log("Buffers " + buff3a + " and " + buff3b + " are equal.");
else console.log("Buffer " + buff3a + " comes after " + buff3b);

// Copying Buffers
// buf.copy(targetBuffer[, targetStart][, sourceStart][, sourceEnd])

var buff3x = new Buffer(4);
buff3a.copy(buff3x);
console.log(buff3x.toString());

// Slice Buffer
// buf.slice([start][, end])

var buff4 = new Buffer("It's friday night and I won't be long.");
console.log(buff4.toString());
console.log(buff4.slice(0,11).toString());

var buff5 = new Buffer("hello there my friend");
console.log("Buffer's length is: " + buff5.length); // length in bytes

/*
Class Methods reference:
Buffer.isEncoding(encoding);
Buffer.isBuffer(obj);
Buffer.byteLength(string, [, encoding]); // returns byte length of a given string
Buffer.concat([buffer1, buffer2...]); // Concatenates given Buffers
Buffer.compare(buff1, buff2); // same as buff1.compare(buff2) but useful for sorting buffers in arr
 */
