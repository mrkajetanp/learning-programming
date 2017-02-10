import fs = require("fs");

const codeFolderPath = "/Users/cajetan/Programming/TypeScript/Learning/Node/";

function readingAFile () {
  fs.readFile(codeFolderPath + "input.txt", (err, data) => {
    if (err)  return console.error(err);
    console.log("Asynchronous read:\n" + data.toString());
  });
}

function openingAFile () {
  console.log("Opening a file..");
  fs.open(codeFolderPath + "input.txt", "r+", (err, fd) => {
    if (err)  return console.error(err);
    console.log("File opened succesfully!");
  });
}

function gettingFileInfo () {
  console.log("Getting file info...");
  fs.stat(codeFolderPath + "input.txt", (err, stats) => {
    if (err)  return console.error(err);
    console.log(stats);
    console.log("Got file info succesfully.");

    console.log("isFile: " + stats.isFile());
    console.log("isDirectory: " + stats.isDirectory());
  });
}

function writingAFile () {
  console.log("Writing to a file..");
  fs.writeFile(codeFolderPath + "input2.txt", "Some important text to be written.", (err) => {
    if (err)  return console.error(err);
    console.log("Data written succesfully!");
    console.log("Let's read newly written data.");
    fs.readFile(codeFolderPath + "input2.txt", (err, data) => {
      if (err)  return console.error(err);
      console.log("Asynchronous read: " + data.toString());
    });
  });
}

function readingFromAFile () {
  // fs.read(fd, buffer, offset, length, position, callback)
  // fs.close(fd, callback)

  let buf = new Buffer(1024);

  console.log("Opening a file..");
  fs.open(codeFolderPath + "input.txt", "r+", (err, fd) => {
    if (err)  return console.error(err);
    console.log("File opened successfully!");
    console.log("Reading a file..");
    fs.read(fd, buf, 0, buf.length, 0, (err, bytes) => {
      if (err)  console.log(err);
      console.log(bytes + " bytes read.");

      // Print only read bytes to avoid junk
      if (bytes > 0)  console.log("\n"+buf.slice(0, bytes).toString());
      fs.close(fd, (err) => {
        if (err)  console.log(err);
        console.log("File closed successfully.");
      });
    });
  });
}

function truncatingAFile () {
  // fs.ftruncate(fd, len, callback)
  let buf = new Buffer(1024);
  console.log("Opening a file..");
  fs.open(codeFolderPath+"input.txt", "r+", (err, fd) => {
    if (err)  return console.error(err);
    console.log("File opened successfully!");
    console.log("Truncating file after 10 bytes..");

    // Truncate the opened file
    fs.ftruncate(fd, 10, (err) => {
      if (err)  return console.error(err);
      console.log("File truncated successfully.");
      console.log("Reading the same file..");

      fs.read(fd, buf, 0, buf.length, 0, (err, bytes) => {
        if (err)   return console.error(err);
        if (bytes > 0)  console.log("\n"+buf.slice(0, bytes).toString());
        fs.close(fd, err => {
          if (err)  console.log(err);
          console.log("File closed successfully.");
        });
      });
    });
  });
}

function deletingAFile () {
  // fs.unlink(path, callback);
  console.log("Deling an existing file..");
  fs.unlink(codeFolderPath+"input.txt", (err) => {
    if (err)  return console.error(err);
    console.log("File deleted successfully.");
  });
}

function creatingADirectory () {
  // fs.mkdir(path[, mode], callback);
  console.log("Creating a directory 'test'..");
  fs.mkdir(codeFolderPath+"test", (err) => {
    if (err)  return console.error(err);
    console.log("Directory created successfully!");
  });
}

function readingADirectory () {
  // fs.readdir(path, callback);
  console.log("Reading directory with code..");
  fs.readdir(codeFolderPath, (err, files) => {
    if (err)  return console.error(err);
    files.forEach((file) => {
      console.log(file);
    });
  });
}

function removingADirectory () {
  // fs.rmdir(path, callback);
  console.log("Removing a directory 'test'..");
  fs.rmdir(codeFolderPath+"test", (err) => {
    if (err)  return console.error(err);
    console.log("Reading directory with code..");
    fs.readdir(codeFolderPath, (err, files) => {
      if (err)  return console.error(err);
      files.forEach((file) => {
        console.log(file);
      });
    });
  });
}
