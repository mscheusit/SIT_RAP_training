sap.ui.define([
    "sap/ui/base/ManagedObject",
    "sap/ui/core/Fragment",
    "sap/ui/Device",
    "sap/ui/core/syncStyleClass"
], function (ManagedObject, Fragment, Device, syncStyleClass) {
    "use strict"

    return ManagedObject.extend("zsitc.articler.controller.AddInfoDialog", {

        constructor: function (oView) {
            this._oView = oView
        },

        exit: function () {
            delete this._oView;
        },

        open: function () {
            var oView = this._oView;

            // create the dialog lazily
            var oAddInfoDialog = oView.byId("addInfoDialog");
            if (!oAddInfoDialog) {
                var oFragmentController = {
                    onCloseDialog: function () {
                        oView.byId("addInfoDialog").close();
                    }
                }

                // load asynchronous XML fragment
                var that = this;
                Fragment.load({
                    id: oView.getId(),
                    name: "zsitc.articler.view.AddInfoDialog",
                    controller: oFragmentController
                }).then(function (oDialog) {
                    // connect dialog to the root view of the component (models, lifecycle)
                    oView.addDependent(oDialog);
                    // forward compact/cozy style into dialog
                    syncStyleClass(that.getContentDensityClass(), oView, oDialog);
                    oDialog.open();
                })
            } else {
                oAddInfoDialog.open();
            }
        },

        getContentDensityClass: function () {
            if (!this._sContentDensityClass) {
                if (Device.support.touch) {
                    this._sContentDensityClass = "sapUiSizeCozy";
                } else {
                    this._sContentDensityClass = "sapUiSizeCompact";
                }
            }
            return this._sContentDensityClass;
        },
    });
});