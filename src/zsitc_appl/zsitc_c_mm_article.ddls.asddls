@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article'
@Metadata.ignorePropagatedAnnotations: false
@Metadata.allowExtensions: false

@UI: {
  headerInfo:{
    typeName: 'Article',
    typeNamePlural: 'Articles',
    title: {
      type: #STANDARD,
      value: 'ArticleNo'
      }//,
//    description: {
//      type: #STANDARD,
//      value: 'RegularVendor'
//      }
    },
  presentationVariant: [{
    sortOrder: [{
      by: 'CreatedAt',
      direction: #DESC
      }],
    visualizations: [{
      type: #AS_LINEITEM
      }]
    }]
   }

//@Search.searchable: true

define root view entity ZSITC_C_MM_Article
  provider contract transactional_query
  as projection on ZSITC_I_MM_Article
{
          @UI.facet: [
              {
                  label: 'General Information',
                  id: 'GeneralInfo',
                  purpose: #STANDARD,
                  position: 10 ,
                  type: #IDENTIFICATION_REFERENCE
              },
              {
                  label: 'Text',
                  position: 20,
                  type: #LINEITEM_REFERENCE,
                  targetElement: '_Text'
              }
            ]
          @UI.hidden: true
  key     ArticleID,

          @UI: { lineItem: [ { position: 10 } ],
                 selectionField: [ { position: 10 } ],
                 identification: [ { position: 10 } ] }
          ArticleNo,

          @UI: { lineItem: [ { position: 20 } ],
                 selectionField: [ { position: 20 } ],
                 identification: [ { position: 20 } ],
                 textArrangement: #TEXT_ONLY }
          @ObjectModel.text.element: ['ArticleTypeText']
          @Consumption.valueHelpDefinition: [{entity: {name: 'ZSITC_I_MM_ARTICLE_TYPE_VH', element: 'Value' }, useForValidation: true}]
          ArticleType,

          @UI.hidden: true
          _Type.Description as ArticleTypeText,

          @UI: { lineItem: [ { position: 30 } ],
                 selectionField: [ { position: 30 } ],
                 identification: [ { position: 30 } ] }
          RegularVendor,

          @UI: { lineItem: [ { position: 40 },
                             { type: #FOR_ACTION,
                               dataAction: 'release',
                               label: 'Release Article' } ],
                 selectionField: [ { position: 40 } ],
                 identification: [ { position: 40 } ] }
          Released,

          @UI: { lineItem: [ { position: 50 } ],
                  selectionField: [ { position: 50 } ],
                  identification: [ { position: 50 } ] }
          ReleasedDate,

          @UI: { lineItem: [ { position: 60 } ],
                 selectionField: [ { position: 60 } ],
                 identification: [ { position: 60 } ] }
          CreatedBy,

          @UI: { lineItem: [ { position: 70 } ],
                 selectionField: [ { position: 70 } ],
                 identification: [ { position: 70 } ] }
          CreatedAt,

          @UI: { lineItem: [ { position: 80 } ],
                 selectionField: [ { position: 80 } ],
                 identification: [ { position: 80 } ] }
          LastChangedBy,

          @UI: { lineItem: [ { position: 90 } ],
                 selectionField: [ { position: 90 } ],
                 identification: [ { position: 90 } ] }
          LastChangedAt,

          //virtual Editable : abap_boolean,

          /* Associations */
          _Text     : redirected to composition child ZSITC_C_MM_Article_Text,
          _Supplier : redirected to composition child ZSITC_C_MM_Article_Supplier,
          _Type
}
