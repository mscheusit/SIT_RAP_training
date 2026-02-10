sap.ui.define(["sap/ui/core/UIComponent", "sap/suite/ui/generic/template/extensionAPI/ReuseComponentSupport"], function (UIComponent, ReuseComponentSupport) {
    "use strict";
    return UIComponent.extend("zsitc.articler.SupplierList.Component", {
        metadata: {
            "manifest": "json"
        },

        // Standard life time event of a component. Used to transform this component into a reuse component for Fiori Elements
        init: function () {
            //Transform this component into a reuse component for Fiori Elements:
            ReuseComponentSupport.mixInto(this, "myPropertiesModelName");
            // Defensive call of init of the super class:
            (UIComponent.prototype.init || jQuery.noop).apply(this, arguments);

            var oRoot = this.getRootControl();
            if (oRoot) {
                this.getComponentModel().setProperty("/View", oRoot);
            }
        },

        // Wird beim Navigieren zur Seite aufgerufen
        stStart: function (oModel, oBindingContext, oExtensionAPI) {
            this._handleContext(oExtensionAPI);
        },

        // Wird aufgerufen, wenn sich der Kontext ändert (z.B. neue Auswahl im LR)
        stRefresh: function (oModel, oBindingContext, oExtensionAPI) {
            this._handleContext(oExtensionAPI);
        },

        _handleContext: function (oExtensionAPI) {
            const oComponentModel = this.getComponentModel();
            const sPath = oExtensionAPI.getNavigationController().getCurrentKeys()[1];
            const sBindingPath = "/Article(" + sPath + ")";
            const oPathSpec = {
                path: sBindingPath
            };

            const oSupplierListView = oComponentModel.getProperty("/View");
            oSupplierListView.bindElement(oPathSpec);
        }
    });
});