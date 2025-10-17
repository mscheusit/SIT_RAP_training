@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article Text'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true

define view entity ZSITC_C_MM_Article_Text
  as projection on ZSITC_I_MM_Article_Text
{
      @UI.hidden: true
  key ArticleID,

      @UI: { lineItem: [ { position: 10 } ],
                selectionField: [ { position: 10 } ],
                identification: [ { position: 10 } ] }
  key Language,

      @UI: { lineItem: [ { position: 20 } ],
                selectionField: [ { position: 20 } ],
                identification: [ { position: 20 } ] }
      Description,

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
