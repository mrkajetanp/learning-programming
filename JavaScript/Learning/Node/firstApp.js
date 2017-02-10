var http = require('http');

http.createServer(function (request, response) {
  // Send the HTTP Header
  // HTTP Status : 200 : OK
  // Context type: text / plain
  response.writeHead(200, {'Content-Type': 'text/plain'});

  // Send the response body as "Hello World"
  response.write("Let's test it ! \n");
  response.end('Hello World!\n');
}).listen(8081);

console.log("Server running at http://127.0.0.1:8081/");
