"use strict";
var fortune = require("../lib/fortune");
var chai_1 = require("chai");
suite("Fortune cookie tests:", function () {
    test("getFortune() should return a fortune", function () {
        chai_1.assert.equal(typeof fortune.getFortune(), "string");
    });
});
//# sourceMappingURL=tests-unit.js.map