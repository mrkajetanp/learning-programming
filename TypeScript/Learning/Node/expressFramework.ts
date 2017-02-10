import express = require("express");

function helloWorld () {
  let app = express();

  app.get("/", (req, res) => {
    res.send("Hello World?");
  });

  let server = app.listen(8088, () => {
    let host = server.address().address;
    let port = server.address().port;
    console.log("Example app listening at http://%s:%s", host, port);
  });
}

helloWorld();
