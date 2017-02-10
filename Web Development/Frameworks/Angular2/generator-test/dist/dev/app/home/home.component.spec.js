"use strict";
var testing_1 = require('@angular/core/testing');
var home_component_1 = require('./home.component');
describe('HomeComponent', function () {
    beforeEach(function () { return testing_1.TestBed.configureTestingModule({ declarations: [home_component_1.HomeComponent] }); });
    it('should instantiate the HomeComponent', function () {
        var fixture = testing_1.TestBed.createComponent(home_component_1.HomeComponent);
        expect(fixture.componentInstance instanceof home_component_1.HomeComponent).toBe(true, 'should create HomeComponent');
    });
});

//# sourceMappingURL=home.component.spec.js.map
