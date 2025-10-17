@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article Text'
@Metadata.ignorePropagatedAnnotations: false

define view entity ZSITC_I_MM_Article_Text
  as select from ZSITC_I_MM_Article_Text_Basis
  association to parent ZSITC_I_MM_Article as _Article on $projection.ArticleID = _Article.ArticleID
{
  key ArticleID,
  key Language,
      Description,
      LastChangedBy,
      LastChangedAt,

      /* Associations */
      _Article
}
