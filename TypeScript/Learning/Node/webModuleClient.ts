import http = require("http");

// Options to be used by request

let options = {
  host: "localhost",
  port: 8088,
  path: "/index.html"
};

// Callback function is used to deal with response
let callback = (response) => {
  // Continuously update stream with data
  let body = "";
  response.on("data", (data) => {
    body += data;
  });
  response.on("end", () => {
    // Data received completely.
    console.log(body);
  });
};

let req = http.request(options, callback);
req.end();
