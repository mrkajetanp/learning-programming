suite('"About" Page Tests', () => {
  test("Page should contain a link to the contact page", () => {
    assert($('a[href="/contact"]').length);
  });
});
