sap.ui.define(["sap/suite/ui/generic/template/Canvas/Component"], function(CanvasComponent) {
    "use strict";
    return CanvasComponent.extend("zsitc.articler.SupplierList.Component", {
        metadata: { "manifest": "json" },

        // Wird beim Navigieren zur Seite aufgerufen
        stStart: function(oModel, oBindingContext, oExtensionAPI) {
            this._handleContext(oBindingContext);
        },

        // Wird aufgerufen, wenn sich der Kontext ändert (z.B. neue Auswahl im LR)
        stRefresh: function(oModel, oBindingContext, oExtensionAPI) {
            this._handleContext(oBindingContext);
        },

        _handleContext: function(oBindingContext) {
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