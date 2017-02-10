define(["require", "exports", "fs"], function (require, exports, fs) {
    "use strict";
    var codeFolderPath = "/Users/cajetan/Programming/TypeScript/Learning/Node/";
    function readingAFile() {
        fs.readFile(codeFolderPath + "input.txt", function (err, data) {
            if (err)
                return console.error(err);
            console.log("Asynchronous read:\n" + data.toString());
        });
    }
    function openingAFile() {
        console.log("Opening a file..");
        fs.open(codeFolderPath + "input.txt", "r+", function (err, fd) {
            if (err)
                return console.error(err);
            console.log("File opened succesfully!");
        });
    }
    function gettingFileInfo() {
        console.log("Getting file info...");
        fs.stat(codeFolderPath + "input.txt", function (err, stats) {
            if (err)
                return console.error(err);
            console.log(stats);
            console.log("Got file info succesfully.");
            console.log("isFile: " + stats.isFile());
            console.log("isDirectory: " + stats.isDirectory());
        });
    }
    function writingAFile() {
        console.log("Writing to a file..");
        fs.writeFile(codeFolderPath + "input2.txt", "Some important text to be written.", function (err) {
            if (err)
                return console.error(err);
            console.log("Data written succesfully!");
            console.log("Let's read newly written data.");
            fs.readFile(codeFolderPath + "input2.txt", function (err, data) {
                if (err)
                    return console.error(err);
                console.log("Asynchronous read: " + data.toString());
            });
        });
    }
    function readingFromAFile() {
        var buf = new Buffer(1024);
        console.log("Opening a file..");
        fs.open(codeFolderPath + "input.txt", "r+", function (err, fd) {
            if (err)
                return console.error(err);
            console.log("File opened successfully!");
            console.log("Reading a file..");
            fs.read(fd, buf, 0, buf.length, 0, function (err, bytes) {
                if (err)
                    console.log(err);
                console.log(bytes + " bytes read.");
                if (bytes > 0)
                    console.log("\n" + buf.slice(0, bytes).toString());
                fs.close(fd, function (err) {
                    if (err)
                        console.log(err);
                    console.log("File closed successfully.");
                });
            });
        });
    }
    function truncatingAFile() {
        var buf = new Buffer(1024);
        console.log("Opening a file..");
        fs.open(codeFolderPath + "input.txt", "r+", function (err, fd) {
            if (err)
                return console.error(err);
            console.log("File opened successfully!");
            console.log("Truncating file after 10 bytes..");
            fs.ftruncate(fd, 10, function (err) {
                if (err)
                    return console.error(err);
                console.log("File truncated successfully.");
                console.log("Reading the same file..");
                fs.read(fd, buf, 0, buf.length, 0, function (err, bytes) {
                    if (err)
                        return console.error(err);
                    if (bytes > 0)
                        console.log("\n" + buf.slice(0, bytes).toString());
                    fs.close(fd, function (err) {
                        if (err)
                            console.log(err);
                        console.log("File closed successfully.");
                    });
                });
            });
        });
    }
    function deletingAFile() {
        console.log("Deling an existing file..");
        fs.unlink(codeFolderPath + "input.txt", function (err) {
            if (err)
                return console.error(err);
            console.log("File deleted successfully.");
        });
    }
    function creatingADirectory() {
        console.log("Creating a directory 'test'..");
        fs.mkdir(codeFolderPath + "test", function (err) {
            if (err)
                return console.error(err);
            console.log("Directory created successfully!");
        });
    }
    function readingADirectory() {
        console.log("Reading directory with code..");
        fs.readdir(codeFolderPath, function (err, files) {
            if (err)
                return console.error(err);
            files.forEach(function (file) {
                console.log(file);
            });
        });
    }
    function removingADirectory() {
        console.log("Removing a directory 'test'..");
        fs.rmdir(codeFolderPath + "test", function (err) {
            if (err)
                return console.error(err);
            console.log("Reading directory with code..");
            fs.readdir(codeFolderPath, function (err, files) {
                if (err)
                    return console.error(err);
                files.forEach(function (file) {
                    console.log(file);
                });
            });
        });
    }
});
