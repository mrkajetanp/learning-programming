"use strict";
var express = require("express");
function helloWorld() {
    var app = express();
    app.get("/", function (req, res) {
        res.send("Hello World?");
    });
    var server = app.listen(8088, function () {
        var host = server.address().address;
        var port = server.address().port;
        console.log("Example app listening at http://%s:%s", host, port);
    });
}
helloWorld();
