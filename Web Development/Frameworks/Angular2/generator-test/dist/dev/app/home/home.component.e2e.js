'use strict';
describe('Home Page', function () {
    beforeEach(function () {
        browser.get('/');
    });
    it('should have h3 header', function () {
        var subject = element(by.tagName('h3')).isPresent();
        expect(subject).toEqual(true);
    });
    it('should have h3 header with "Home Page" text', function () {
        var subject = element(by.tagName('h3'));
        expect(subject.getText()).toBe('Home Page');
    });
});

//# sourceMappingURL=home.component.e2e.js.map
