sap.ui.define([
    "sap/m/MessageToast",
    "sap/m/Input",
    "sap/m/Select",
    "sap/ui/core/Item",
    "sap/m/Text",
    "sap/m/ColumnListItem",
    "sap/ui/core/ListItem",
    "../../controller/AddInfoDialog",
    "sap/ui/model/json/JSONModel"
], function (MessageToast, Input, Select, Item, Text, ColumnListItem, ListItem, AddInfoDialog, JSONModel) {
    'use strict';

    return {
        onInit: function (oEvent) {
            this.oTable = this.getView().byId('responsiveTable');
        },

        _onEdit: function (oEvent) {
            var oModel = this.getView().getModel();
            if (this.oActualTemplate == null || this.oActualTemplate == this.oTemplateColumnListItem) {
                // Get selected contexts. The method can be called directly on extensionAPI
                /*const aSelectedContexts = this.extensionAPI.getSelectedContexts();
                // Loop through the selected items and process their data
                aSelectedContexts.forEach(function (oContext, i) {
                    // Get the bound data of the item
                    //const oData = oContext.getObject();
                    // Do something with the data, e.g., log it
                    //console.log(oData);
                    const sPath = oContext.getPath();
                    oModel.setProperty(sPath + "/Editable", true);
                });*/

                if (this.oEditTemplateColumnListItem == null) {
                    const oColumnsListItem = this.oTable.getItems()[0];
                    var aCells = oColumnsListItem.getCells();
                    this.oTemplateColumnListItem = new ColumnListItem({ cells: aCells });
                    this.oTemplateColumnListItem.mProperties = oColumnsListItem.mProperties;
                    this.oTemplateColumnListItem.mProperties.selected = false;

                    var aEditCells = [];
                    const aColumns = this.oTable.getColumns();
                    for (var i = 0; i < aColumns.length; i++) {
                        const aSId = aColumns[i].sId.split("-");
                        const strColumnName = '{' + aSId[aSId.length - 1] + '}';
                        if (i <= 2) {
                            if (i == 1) {
                                aEditCells[i] = new Select({
                                    width: "400px",
                                    selectedKey: strColumnName,
                                    items: {
                                        path: "/ZSITC_I_MM_ARTICLE_TYPE_VH", // Bind to the article type VH array in the model
                                        template: new Item({
                                            key: "{Value}", // The key for internal management
                                            text: "{Description}"      // The text displayed in the list
                                        })
                                    }
                                });
                            } else {
                                aEditCells[i] = new Input({
                                    value: strColumnName,
                                    editable: true, //'{Editable}'
                                });
                            }
                        } else {
                            aEditCells[i] = aCells[i].clone();
                            /*aEditCells[i] = new Text({
                                text: strColumnName
                            });*/
                        }
                    }
                    this.oEditTemplateColumnListItem = new ColumnListItem({ cells: aEditCells })
                    this.oEditTemplateColumnListItem.mProperties = oColumnsListItem.mProperties;
                }

                this._rebindTable(this.oEditTemplateColumnListItem);
            } else {
                /* const aItems = this.oTable.getItems();
                aItems.forEach(function (oItem, i) {
                    const oContext = oItem.getBindingContext();
                    const sPath = oContext.getPath();
                    oModel.setProperty(sPath + "/Editable", false);
                }); */

                oModel.resetChanges();
                this._rebindTable(this.oTemplateColumnListItem);
            }

            MessageToast.show("Custom handler invoked.");
        },

        _rebindTable: function (oTemplate) {
            const oBinding = this.oTable.getBinding("items");
            if (oBinding) {
                var aAppliedFilters = oBinding.getFilters("Application");
                var aAppliedSorters = oBinding.aSorters;
            }

            this.oTable.unbindItems();
            this.getView().getModel().refresh(true, true);
            this.oTable.bindItems({
                path: "/Article",
                template: oTemplate,
                templateShareable: true,
                key: "ArticleID",
                filters: aAppliedFilters,
                sorter: aAppliedSorters
            });

            this.oActualTemplate = oTemplate;
        },

        _onSave(oEvent) {
            const oModel = this.getView().getModel();
            if (!oModel.hasPendingChanges()) {
                MessageToast.show("There are no changes.");
                return;
            }

            oModel.submitChanges({
                success: function (oData) {
                    if (oData.__batchResponses) {
                        this._saveCompleted(oData)
                    }
                }.bind(this),
                error: function (oError) {
                    this._oDataErrorHandle(oError).bind(this);
                }
            });

            /*  const aItems = this.oTable.getItems();
             var aData = [];
             aItems.forEach(function (oItem) {
                 const oContext = oItem.getBindingContext();
                 const oObject = oContext.getObject();
                 // if (oObject.Editable) {
                 aData.push(oContext.getObject());
                 //};
             });
             this._updateBackend(aData);*/
        },

        _updateBackend: function (aData) {
            if (aData.length === 0) {
                return;
            }
            oModel = this.getView().getModel();

            this.getView().setBusy(true)
            oModel.setDeferredGroups(["id1"]);

            for (var i = 0; i < aData.length; i++) {
                let url = "/Article('" + this._adjustGuid(aData[i].ArticleID) + "')";
                oModel.update(url, aData[i], {
                    groupId: "id1",
                    success: function (oData, response) {
                    },
                    error: function (oError) {
                    }
                });
            }
            oModel.submitChanges({
                batchGroupId: "id1",
                success: function (oData, response) {
                    if (oData.__batchResponses) {
                        this._saveCompleted(oData.__batchResponses[0].__changeResponses)
                    }
                }.bind(this),
                error: function (oError) {
                    this._oDataErrorHandle(oError).bind(this);
                }
            });
        },

        _saveCompleted: function (oData) {
            //disable busy            
            this.getView().setBusy(false);

            /* let sErrorMessage = this.getBapiErrors(oData._batchResponses[0].__changeResponses);
            if (sErrorMessage.length > 0) {
                MessageBox.error(sErrorMessage);
            } else {
                //notify about success
                var message = this.getView().getModel("i18n").getResourceBundle().getText("dataSaved");
                MessageToast.show(message);
            } */
            this._rebindTable(this.oTemplateColumnListItem);
        },

        _oDataErrorHandle: function (oError) {
        },

        onBeforeRebindTableExtension: function (oEvent) {
            //var oBindingParams = oEvent.getParameter("bindingParams");
            //oBindingParams.parameters = oBindingParams.parameters || {};

            if (this.oEditTemplateColumnListItem = this.oActualTemplate) {
                this.getView().getModel().resetChanges();
            }
        },

        _adjustGuid: function (sGuid) {
            return sGuid.replace('{', '%7B')  // Replace %7B with {
                .replace('}', '%7D'); // Replace %7D with }
        },

        _onAddInfo(oEvent) {
            // set Model for addInfo
            const oBindingContexts = this.extensionAPI.getSelectedContexts();
            const oBindingContext = oBindingContexts[0];
            if (!oBindingContext) {
                return;
            }
            const oView = this.getView();
            
            //const oObject = oBindingContext.getObject();            
            //const oData = {
            //    article: {
            //        ArticleNo: oObject.ArticleNo
            //    }
            //};
            //const oModel = new JSONModel(oData);
            //oView.setModel(oModel, "addInfo");
            
            const sPath = oBindingContext.getPath().substr(1);
            oView.bindElement({
                path: "/" + window.decodeURIComponent(oBindingContext.getPath().substr(1))
            });

            if (!this._addInfoDialog) {
                // set dialog
                this._addInfoDialog = new AddInfoDialog(oView);
            }
            this._addInfoDialog.open();
        }
    }
});
