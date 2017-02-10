define(["require", "exports", "events"], function (require, exports, events) {
    "use strict";
    var eventEmitter = new events.EventEmitter();
    var connectHandler = function connected() {
        console.log("Connection successful.");
        eventEmitter.emit("data_received");
    };
    eventEmitter.on("connection", connectHandler);
    eventEmitter.on("data_received", function () {
        console.log("Data received succesfully.");
    });
    eventEmitter.emit("connection");
    console.log("Program ended.");
});
