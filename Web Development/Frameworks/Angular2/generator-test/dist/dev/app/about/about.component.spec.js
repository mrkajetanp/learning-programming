"use strict";
var testing_1 = require('@angular/core/testing');
var about_component_1 = require('./about.component');
describe('AboutComponent', function () {
    beforeEach(function () { return testing_1.TestBed.configureTestingModule({ declarations: [about_component_1.AboutComponent] }); });
    it('should instantiate the AboutComponent', function () {
        var fixture = testing_1.TestBed.createComponent(about_component_1.AboutComponent);
        expect(fixture.componentInstance instanceof about_component_1.AboutComponent).toBe(true, 'should create AboutComponent');
    });
});

//# sourceMappingURL=about.component.spec.js.map
