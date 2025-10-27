sap.ui.define([
    "sap/m/MessageToast"
], function(MessageToast) {
    'use strict';

    return {
        edit: function(oEvent) {
            MessageToast.show("Custom handler invoked.");
        }
    }
});
