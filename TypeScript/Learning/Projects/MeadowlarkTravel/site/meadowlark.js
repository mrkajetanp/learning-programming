"use strict";
var express = require("express");
var moment = require("moment");
var fortune = require("./lib/fortune");
var credentials = require("./lib/credentials");
var bodyParser = require("body-parser");
var formidable = require("formidable");
var cookieParser = require("cookie-parser");
var expressSession = require("express-session");
var app = express();
app.set("view engine", "hbs");
app.set("view options", { layout: "layouts/main" });
app.use(express.static(__dirname + "/public"));
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser(credentials.cookieSecret));
app.use(expressSession({
    secret: credentials.cookieSecret,
    resave: false,
    saveUninitialized: true
}));
app.set("port", process.env.PORT || 8088);
app.use(function (req, res, next) {
    res.locals.showTests = app.get("env") !== "production" && req.query.test === "1";
    if (res.locals.showTests)
        console.log("Page test requested..");
    next();
});
app.use(function (req, res, next) {
    res.locals.flash = req.session.flash;
    delete req.session.flash;
    next();
});
app.get("/", function (req, res) {
    console.log("Request receieved for home page..");
    res.render("home", { fortune: fortune.getFortune });
});
app.get("/about", function (req, res) {
    console.log("Request received for about page..");
    res.render("about", {
        pageTestScript: "/qa/tests-about.js"
    });
});
app.get("/tours/hood-river", function (req, res) {
    console.log("/tours/hood-river requested..");
    res.render("tours/hood-river");
});
app.get("/tours/oregon-coast", function (req, res) {
    console.log("/tours/oregon-coast requested..");
    res.render("tours/oregon-coast");
});
app.get("/tours/request-group-rate", function (req, res) {
    console.log("/tours/request-group-rate/ requested..");
    res.render("tours/request-group-rate");
});
app.get("/greeting", function (req, res) {
    res.render("about", {
        message: "welcome",
        style: req.query.style,
        userid: req.cookie.userid,
        username: req.session.username,
    });
});
app.get("/no-layout", function (req, res) {
    res.render("no-layout", { layout: null });
});
app.get("/custom", function (req, res) {
    res.render("custom-layout", { layout: "custom" });
});
app.get("/test", function (req, res) {
    res.type("text/plain");
    res.send("this is a test");
});
app.get("/headers", function (req, res) {
    console.log("/headers requested..");
    res.set("Content-Type", "text/plain");
    var s = "";
    for (var key in req.headers) {
        s += (key + ": " + req.headers[key] + "\n");
    }
    res.send(s);
});
var tours = [
    { id: 0, name: "Hood River", price: 99.99 },
    { id: 1, name: "Oregon Coast", price: 149.95 },
];
app.get("/api/tours", function (req, res) {
    res.json(tours);
});
app.get("/api/tours2", function (req, res) {
    var toursXml = "<?xml version=\"1.0\"?><tours>" + tours.map(function (p) {
        return "<tour price=\"" + p.price + "\" id=\"" + p.id + "\">" + p.name + "</tour>";
    }).join("") + "</tours>";
    var toursText = tours.map(function (p) {
        return p.id + ": " + p.name + " (" + p.price + ")";
    }).join("\n");
    res.format({
        "application/json": function () {
            res.json(tours);
        },
        "application/xml": function () {
            res.type("application/xml");
            res.send(toursXml);
        },
        "text/xml": function () {
            res.type("text/xml");
            res.send(toursXml);
        },
        "text/plain": function () {
            res.type("text/plain");
            res.send(toursText);
        },
        "default": function () {
            res.status(406).send("Not acceptable.");
        }
    });
});
app.get("/error", function (req, res) {
    console.log("/error requested..");
    res.status(500);
    res.render("500");
});
app.get("/newsletter", function (req, res) {
    res.render("newsletter", { csrf: "CSRF token goes here" });
});
app.get("/thank-you", function (req, res) {
    res.send("Thank you !");
});
app.post("/process", function (req, res) {
    if (req.xhr || req.accepts("json,html") === "json") {
        res.send({ success: true });
    }
    else {
        res.redirect(303, "/thank-you");
    }
});
app.get("/contest/vacation-photo", function (req, res) {
    var now = new Date();
    res.render("contest/vacation-photo", {
        year: now.getFullYear(),
        month: now.getMonth()
    });
});
app.get("/cookies/testone", function (req, res) {
    console.log("/cookies/testone requested..");
    res.cookie("monster", "nom nom");
    res.cookie("signedMonster", "signed nom nom", { signed: true });
    var monster = req.cookies.monster;
    var signedMonster = req.signedCookies.signedMonster;
    console.log(req.cookies);
    console.log(req.signedCookies);
    console.log("User name: " + req.session.userName);
    res.send("Cookies test..");
});
app.get("/cookies/session-test", function (req, res) {
    console.log("/cookies/session-test requested..");
    req.session.userName = "Anonymous";
    var colorScheme = req.session.colorScheme || "dark";
    res.send("Session test..");
});
app.get("/cookies/flash-error", function (req, res) {
    req.session.flash = {
        type: "danger",
        intro: "Validation error!",
        message: "The email address you entered was not valid..",
    };
    res.render("home");
});
app.get("/cookies/flash-success", function (req, res) {
    req.session.flash = {
        type: "success",
        intro: "Thank you!",
        message: "You've now been signed up for the newsletter..",
    };
    res.render("home");
});
app.post("/contest/vacation-photo/:year/:month", function (req, res) {
    var form = new formidable.IncomingForm();
    form.parse(req, function (err, fields, files) {
        if (err)
            return res.redirect(303, "/error");
        console.log("Received fields: ");
        console.log(fields);
        console.log("Receieved files: ");
        console.log(files);
        res.redirect(303, "/thank-you");
    });
});
app.use(function (req, res, next) {
    console.log("404 page requested..");
    res.status(404);
    res.render("404");
});
app.use(function (err, req, res, next) {
    console.log("Request receieved for 500 page..");
    console.error(err.stack);
    res.status(500);
    res.render("500");
});
moment().format("");
app.listen(app.get("port"), function () {
    console.log("Server started on http://localhost:" + app.get("port") +
        " on " + moment().format("L, LTS"));
});
//# sourceMappingURL=meadowlark.js.map