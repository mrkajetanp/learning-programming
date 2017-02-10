/// <reference path="../typings/index.d.ts"/>

import * as fortune from "../lib/fortune";
import { assert } from "chai";

suite("Fortune cookie tests:", () => {
  test("getFortune() should return a fortune", () => {
    assert.equal(typeof fortune.getFortune(), "string");
  });
});
