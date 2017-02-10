'use strict';
describe('About Page', function () {
    beforeEach(function () {
        browser.get('/about');
    });
    it('should have h3 header', function () {
        var subject = element(by.tagName('h3')).isPresent();
        expect(subject).toEqual(true);
    });
    it('should have h3 header with "About Page" text', function () {
        var subject = element(by.tagName('h3'));
        expect(subject.getText()).toBe('About Page');
    });
});

//# sourceMappingURL=about.component.e2e.js.map
