var Browser = require('zombie');
var assert = require('chai').assert;

var browser;

suite('Cross-Page Tests', () => {
  setup(() => {
    browser = new Browser();
  });

  test('Requesting a group rate quote from the hood river tour page ' +
        'should populate the referrer field', (done) => {
          var referrer = 'http://localhost:8088/tours/hood-river';
          browser.visit(referrer, () => {
            browser.clickLink('.requestGroupRate', () => {
              assert.equal(browser.field('referrer').value, referrer);
              done();
            });
          });
        });
  test('Requesting a group rate from the oregon coast tour page should ' +
        'populate the referrer field', (done) => {
          var referrer = 'http://localhost:8088/tours/oregon-coast';
          browser.visit(referrer, () => {
            browser.clickLink('.requestGroupRate', () => {
              assert.equal(browser.field('referrer').value, referrer);
              done();
            });
          });
        });
  test('Visiting the "request group rate" page directly should result ' +
        'in an empty referrer field', (done) => {
          browser.visit('http://localhost:8088/tours/request-group-rate', () => {
            assert.equal(browser.field('referrer').value, '');
            done();
          });
  });
});
