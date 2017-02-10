'use strict';
describe('App', function () {
    beforeEach(function () {
        browser.get('/');
    });
    it('should have a title', function () {
        var subject = browser.getTitle();
        var result = 'Angular2 + TypeScript';
        expect(subject).toEqual(result);
    });
    it('should have header', function () {
        var subject = element(by.tagName('h1')).isPresent();
        expect(subject).toEqual(true);
    });
    it('should have correct header text', function () {
        var subject = element(by.tagName('h1'));
        expect(subject.getText()).toBe('My Angular2 + TypeScript App');
    });
    it('should have footer', function () {
        var subject = element(by.tagName('footer')).isPresent();
        expect(subject).toEqual(true);
    });
});

//# sourceMappingURL=app.component.e2e.js.map
