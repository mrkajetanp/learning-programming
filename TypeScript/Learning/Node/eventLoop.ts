import events = require("events");

let eventEmitter = new events.EventEmitter();

let connectHandler = function connected() {
  console.log("Connection successful.");
  eventEmitter.emit("data_received");
};
// Bind the connection event with the handler
eventEmitter.on("connection", connectHandler);

// Bind the data_received event with the anonymous function
eventEmitter.on("data_received", function() {
  console.log("Data received succesfully.");
});

// Fire the connection event
eventEmitter.emit("connection");

console.log("Program ended.");
