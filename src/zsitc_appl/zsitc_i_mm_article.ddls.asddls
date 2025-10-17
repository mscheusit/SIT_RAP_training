@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article'
@Metadata.ignorePropagatedAnnotations: false

define root view entity ZSITC_I_MM_Article
  as select from ZSITC_I_MM_Article_Basis
  composition [0..*] of ZSITC_I_MM_Article_Text as _Text
{
  key ArticleID,
      ArticleNo,
      Released,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,

      /* Associations */
      _Text
}
