sap.ui.define([
    "sap/m/MessageToast"
], function(MessageToast) {
    'use strict';

    return {
        onEdit: function(oEvent) {
            MessageToast.show("Custom handler invoked.");
        }
    }
});
