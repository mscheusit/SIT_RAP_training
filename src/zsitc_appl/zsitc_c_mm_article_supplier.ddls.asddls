@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article Text'
@Metadata.ignorePropagatedAnnotations: false
@Metadata.allowExtensions: false

@UI: {
  headerInfo:{
    typeName: 'Article',
    typeNamePlural: 'Articles',
    title: {
      type: #STANDARD,
      value: 'Supplier'
      }    
    },
  presentationVariant: [{
    sortOrder: [{
      by: 'Supplier',
      direction: #ASC
      }],
    visualizations: [{
      type: #AS_LINEITEM
      }]
    }]
   }
   
define view entity ZSITC_C_MM_Article_Supplier
  as projection on ZSITC_I_MM_Article_Supplier
{
@UI.facet: [
          {
              label: 'General Information',
              id: 'GeneralInfo',
              purpose: #STANDARD,
              position: 10 ,
              type: #IDENTIFICATION_REFERENCE
          }
        ]
      @UI.hidden: true
  key ArticleID,

      @UI: { lineItem: [ { position: 10 } ],
             selectionField: [ { position: 10 } ],
             identification: [ { position: 10 } ] }
  key Supplier,
      
      @UI: { lineItem: [ { position: 30 } ],
             selectionField: [ { position: 30 } ],
             identification: [ { position: 30 } ] }
      LastChangedBy,

      @UI: { lineItem: [ { position: 40 } ],
             selectionField: [ { position: 40 } ],
             identification: [ { position: 40 } ] }
      LastChangedAt,

      /* Associations */
      _Article : redirected to parent ZSITC_C_MM_Article
}
