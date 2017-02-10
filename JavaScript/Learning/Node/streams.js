var fs = require("fs");
var zlib = require("zlib");
var data = '';

function readingFromStream() {
  var readerStream = fs.createReadStream('text.txt');
  readerStream.setEncoding('UTF8');

  readerStream.on('data', function(chunk) {
    data += chunk;
  });

  readerStream.on('end', function() {
    console.log(data);
  });

  readerStream.on('error', function(error) {
    console.log(err.stack);
  });
  console.log("Reading finished.");
}

function writingToStream() {
  data = "Just some random text in here\n";
  var writerStream = fs.createWriteStream('output.txt');
  writerStream.write(data, 'UTF8');
  writerStream.end();

  writerStream.on('finish', function() {
    console.log("Write completed.");
  });

  writerStream.on('error', function(err) {
    console.log(err.stack);
  });

  console.log("Writing finished.");
}

function pipingTheStreams() {
  var readerStream = fs.createReadStream('text.txt');
  var writerStream = fs.createWriteStream('output.txt');
  readerStream.pipe(writerStream);
}

function chainingTheStreams() {
  fs.createReadStream('text.txt').pipe(zlib.createGzip()).pipe(fs.createWriteStream('text.txt.gz'));
  console.log("File compressed.");
  fs.createReadStream('text.txt.gz').pipe(zlib.createGunzip()).pipe(fs.createWriteStream('text.txt'));
  console.log("File decompressed.");
}
