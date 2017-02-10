"use strict";
var testing_1 = require('@angular/core/testing');
var testing_2 = require('@angular/router/testing');
var app_component_1 = require('./app.component');
describe('AppComponent', function () {
    beforeEach(function () { return testing_1.TestBed.configureTestingModule({
        imports: [testing_2.RouterTestingModule],
        declarations: [app_component_1.AppComponent]
    }); });
    it('should instantiate the AppComponent', function () {
        var fixture = testing_1.TestBed.createComponent(app_component_1.AppComponent);
        expect(fixture.componentInstance instanceof app_component_1.AppComponent).toBe(true, 'should create AppComponent');
    });
});

//# sourceMappingURL=app.component.spec.js.map
