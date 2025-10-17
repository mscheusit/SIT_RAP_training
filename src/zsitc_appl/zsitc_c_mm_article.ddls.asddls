@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

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
              label: 'Booking Information',
              position: 20,
              type: #LINEITEM_REFERENCE,
              targetElement: '_Text'
          }
        ]
      @UI.hidden: true
  key ArticleID,

      @UI: { lineItem: [ { position: 10 } ],
             selectionField: [ { position: 10 } ],
             identification: [ { position: 10 } ] }
      ArticleNo,

      @UI: { lineItem: [ { position: 20 } ],
             selectionField: [ { position: 20 } ],
             identification: [ { position: 20 } ] }
      Released,

      @UI: { lineItem: [ { position: 30 } ],
             selectionField: [ { position: 30 } ],
             identification: [ { position: 30 } ] }
      CreatedBy,

      @UI: { lineItem: [ { position: 40 } ],
             selectionField: [ { position: 40 } ],
             identification: [ { position: 40 } ] }
      CreatedAt,

      @UI: { lineItem: [ { position: 50 } ],
             selectionField: [ { position: 50 } ],
             identification: [ { position: 50 } ] }
      LastChangedBy,

      @UI: { lineItem: [ { position: 60 } ],
             selectionField: [ { position: 60 } ],
             identification: [ { position: 60 } ] }
      LastChangedAt,

      /* Associations */
      _Text : redirected to composition child ZSITC_C_MM_Article_Text

}
