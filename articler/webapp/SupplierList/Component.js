sap.ui.define(["sap/ui/core/UIComponent", "sap/suite/ui/generic/template/extensionAPI/ReuseComponentSupport"], function (UIComponent, ReuseComponentSupport) {
    "use strict";
    return UIComponent.extend("zsitc.articler.SupplierList.Component", {
        metadata: {
            "manifest": "json"
        },

        // Standard life time event of a component. Used to transform this component into a reuse component for Fiori Elements
  init: function(){
    //Transform this component into a reuse component for Fiori Elements:
    ReuseComponentSupport.mixInto(this, "myPropertiesModelName");    
    // Defensive call of init of the super class:
    (UIComponent.prototype.init || jQuery.noop).apply(this, arguments);
  },

        // Wird beim Navigieren zur Seite aufgerufen
        stStart: function (oModel, oBindingContext, oExtensionAPI) {
            this._handleContext(oBindingContext);
        },

        // Wird aufgerufen, wenn sich der Kontext ändert (z.B. neue Auswahl im LR)
        stRefresh: function (oModel, oBindingContext, oExtensionAPI) {
            this._handleContext(oBindingContext);
        },

        _handleContext: function (oBindingContext) {
            if (oBindingContext) {
                // Pfad extrahieren, um z.B. eigene Smart Controls zu binden
                var sPath = oBindingContext.getPath();
                console.log("Navigiert mit Kontext-Pfad: " + sPath);

                // Zugriff auf Daten des Objekts
                var oData = oBindingContext.getObject();
                console.log("ID des Objekts:", oData.ID);
            }
        }
    });
});