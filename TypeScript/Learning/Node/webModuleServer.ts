import http = require("http");
import fs = require("fs");
import url = require("url");

http.createServer( (request, response) => {
  let pathname = url.parse(request.url).pathname; // Parse the request containing file name
  console.log("Request made for: " + pathname + " received."); // Print name of the requested file
  fs.readFile(pathname.substr(1), (err, data) => {
    if (err) {
        console.log(err);
        // HTTP Status: 404 : NOT FOUND
        // Content Type: text/plain
        response.writeHead(404, {"Content-Type": "text/html"});
    } else {
      // Page found
      // HTTP Status: 200 : OK
      // Content Type: text/plain
      response.writeHead(200, {"Content-Type": "text/html"});
      response.write(data.toString());
    }
    response.end(); // Send the response body
  });
}).listen(8088);

console.log("Server running at 127.0.0.1:8088/");
