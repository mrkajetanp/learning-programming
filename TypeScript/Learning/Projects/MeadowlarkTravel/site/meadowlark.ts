/// <reference path="./typings/index.d.ts"/>

import * as express from "express";
import * as moment from "moment";
import * as fortune from "./lib/fortune";
import * as weather from "./lib/weather";
import * as credentials from "./lib/credentials";
import * as bodyParser from "body-parser";
import * as formidable from "formidable";
import * as cookieParser from "cookie-parser";
import * as expressSession from "express-session";

let app = express();

app.set("view engine", "hbs");
app.set("view options", { layout: "layouts/main"});
app.use(express.static(__dirname + "/public"));
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser(credentials.cookieSecret));
app.use(expressSession({
  secret: credentials.cookieSecret,
  resave: false,
  saveUninitialized: true
}));
app.set("port", process.env.PORT || 8088);

// detecting test=1 in the querystring
app.use((req, res, next) => {
  res.locals.showTests = app.get("env") !== "production" && req.query.test === "1";
  if (res.locals.showTests) console.log("Page test requested..");
  next();
});

app.use((req, res, next) => {
  // if there is a flash message, transfer it to the context, then clear it
  res.locals.flash = req.session.flash;
  delete req.session.flash;
  next();
});

app.get("/", (req, res) => {
  console.log("Request receieved for home page..");
  res.render("home", { fortune: fortune.getFortune });
});

app.get("/about", (req, res) => {
  console.log("Request received for about page..");
  res.render("about", {
    pageTestScript: "/qa/tests-about.js"
  });
});

app.get("/tours/hood-river", (req, res) => {
  console.log("/tours/hood-river requested..");
  res.render("tours/hood-river");
});

app.get("/tours/oregon-coast", (req, res) => {
  console.log("/tours/oregon-coast requested..");
  res.render("tours/oregon-coast");
});

app.get("/tours/request-group-rate", (req, res) => {
  console.log("/tours/request-group-rate/ requested..");
  res.render("tours/request-group-rate");
});

// Passing a content to a view, including querystring, cookie and session values
app.get("/greeting", (req, res) => {
  res.render("about", {
    message: "welcome",
    style: req.query.style,
    userid: req.cookie.userid,
    username: req.session.username,
  });
});

// rendering a view without a layout
app.get("/no-layout", (req, res) => {
  res.render("no-layout", { layout: null });
});

// The layout file views/layouts/custom.hbs will be used
app.get("/custom", (req, res) => {
  res.render("custom-layout", { layout: "custom" });
});

// Rendering a plaintext output
app.get("/test", (req, res) => {
  res.type("text/plain");
  res.send("this is a test");
});

app.get("/headers", (req, res) => {
  console.log("/headers requested..");
  res.set("Content-Type", "text/plain");
  let s = "";
  for (let key in req.headers) {
    s += (key + ": " + req.headers[key] + "\n");
  }
  res.send(s);
});

let tours = [
  { id: 0, name: "Hood River", price: 99.99 },
  { id: 1, name: "Oregon Coast", price: 149.95 },
];

// Simple GET endpoint returning only JSON
app.get("/api/tours", (req, res) => {
    res.json(tours);
});

// GET endpoint that returns JSON, XML or text depending on a client
app.get("/api/tours2", (req, res) => {
  let toursXml = "<?xml version=\"1.0\"?><tours>" + tours.map(p => {
    return "<tour price=\"" + p.price + "\" id=\"" + p.id + "\">" + p.name + "</tour>";
  }).join("") + "</tours>";
  let toursText = tours.map(p => {
    return p.id + ": " + p.name + " (" + p.price + ")";
  }).join("\n");
  res.format({
    "application/json": () => {
      res.json(tours);
    },
    "application/xml": () => {
      res.type("application/xml");
      res.send(toursXml);
    },
    "text/xml": () => {
      res.type("text/xml");
      res.send(toursXml);
    },
    "text/plain": () => {
      res.type("text/plain");
      res.send(toursText);
    },
    "default": () => {
      res.status(406).send("Not acceptable.");
    }
  });
});

app.get("/error", (req, res) => {
  console.log("/error requested..");
  res.status(500);
  res.render("500");
  // res.status(500).render("500");
});

app.get("/newsletter", (req, res) => {
  res.render("newsletter", { csrf: "CSRF token goes here" });
});

app.get("/thank-you", (req, res) => {
  res.send("Thank you !");
});

app.post("/process", (req, res) => {
  if (req.xhr || req.accepts("json,html") === "json") {
    // if there was en error, we would send { error: "description" }
    res.send({ success: true });
  } else {
    // if there was en error, we would redirect to another page
    res.redirect(303, "/thank-you");
  }
  /*
  console.log("Form (from querystring): " + req.query.form);
  console.log("CSRF token (from hidden form field): " + req.body._csrf);
  console.log("Name (from visible form field): " + req.body.name);
  console.log("Email (from visible form field): " + req.body.email);
  */
});

app.get("/contest/vacation-photo", (req, res) => {
    let now = new Date();
    res.render("contest/vacation-photo", {
      year: now.getFullYear(),
      month: now.getMonth()
    });
});

app.get("/cookies/testone", (req, res) => {
  console.log("/cookies/testone requested..");
  res.cookie("monster", "nom nom");
  res.cookie("signedMonster", "signed nom nom", { signed: true });
  let monster = req.cookies.monster;
  let signedMonster = req.signedCookies.signedMonster;
  console.log(req.cookies);
  console.log(req.signedCookies);
  console.log("User name: " + req.session.userName);
  // res.clearCookie("monster");
  // res.clearCookie("signedMonster");
  res.send("Cookies test..");
});

app.get("/cookies/session-test", (req, res) => {
  console.log("/cookies/session-test requested..");
  req.session.userName = "Anonymous";
  let colorScheme = req.session.colorScheme || "dark";
  // req.session.userName = null; // sets it to null but doesn't remove it
  // delete req.session.colorScheme; // removes colorScheme
  res.send("Session test..");
});

app.get("/cookies/flash-error", (req, res) => {
    req.session.flash = {
      type: "danger",
      intro: "Validation error!",
      message: "The email address you entered was not valid..",
    };
    res.render("home");
});

app.get("/cookies/flash-success", (req, res) => {
    req.session.flash = {
      type: "success",
      intro: "Thank you!",
      message: "You've now been signed up for the newsletter..",
    };
    res.render("home");
});

app.post("/contest/vacation-photo/:year/:month", (req, res) => {
  let form = new formidable.IncomingForm();
  form.parse(req, (err, fields, files) => {
    if (err)  return res.redirect(303, "/error");
    console.log("Received fields: ");
    console.log(fields);
    console.log("Receieved files: ");
    console.log(files);
    res.redirect(303, "/thank-you");
  });
});

/*
FIXME: Broken partial middleware
app.use((req, res, next) => {
  if (!res.locals.partials) res.locals.partials = {};
  res.locals.partials.weather = weather.getWeatherData();
  next();
});
*/

// Custom 404 Page - page not found handler
app.use((req, res, next) => {
  console.log("404 page requested..");
  res.status(404);
  res.render("404");
});

// Custom 500 page - error handler
app.use((err, req, res, next) => {
  console.log("Request receieved for 500 page..");
  console.error(err.stack);
  res.status(500);
  res.render("500");
});

/*
Processing forms

// Basic Form Processing
// body-parser middleware must be linked in
app.post("/process-contact", (req, res) => {
  console.log("Receieved contact from " + req.body.name + " <" req.body.email + ">");
  // test to database..
  res.redirect(303, "/thank-you");
});

// More robust form processing
app.post("/process-contact", (req, res) => {
  console.log("Receieved contact from " + req.body.name + " <" + req.body.email + ">");
  try {
    // save to database
    return res.xhr ?
    res.render({ success: true });
    res.redirect(303, "/thank-you");
  } catch (ex) {
    return res.xhr ? res.json({ error: "Database error." }) : res.redirect(303, "/database-error");
  }
});

PUT endpoint updates a product and returns JSON. Parameters are passed in the querystring.
(the ":id" in the route string tells Express to add an id property to req.params)

// API that updates a tour and returns JSON ; params are passed using querystring
app.put("/api/tour/:id", (req, res) => {
  let p = tours.some(p => return p.id === req.params.id );
  if (p) {
    if (req.query.name) p.name = req.query.name;
    if (req.query.price) p.price = req.query.price;
    res.json({success: true});
  } else {
    res.json({error: "No such tour exists." });
  }
});

// API that deletes a product
api.del("/api/tour/:id", (req, res) => {
  let i;
  for (let i = tours.length-1 ; i >= 0 ; i--)
    if (tours[i].id === req.params.id ) break;
  if (i >= 0) {
    tours.splice(i, 1);
    res.json({success: true});
  } else {
    res.json({ error: "No such tour exists."});
  }
});

*/

moment().format("");
app.listen(app.get("port"), () => {
  console.log("Server started on http://localhost:" + app.get("port") +
    " on " + moment().format("L, LTS"));
});
