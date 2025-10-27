const { cloneElement } = require("react");

sap.ui.define([
    "sap/m/MessageToast",
    "sap/m/ColumnListItem"
], function(MessageToast) {
    'use strict';

    return {
        onInit: function(oEvent) {            
            this.oTableDisplay = this.getView().byId("Article::sap.suite.ui.generic.template.ListReport");
            this.oTableEdit = deepClone(this.oTableDisplay);
        },

        onEdit: function(oEvent) {
            MessageToast.show("Custom handler invoked.");
        }
    }
});
