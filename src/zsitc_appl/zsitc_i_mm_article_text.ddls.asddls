@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article Text'
@Metadata.ignorePropagatedAnnotations: false

define view entity ZSITC_I_MM_Article_Text
  as select from zsitc_mm_articlt
  association to parent ZSITC_I_MM_Article as _Article on $projection.ArticleID = _Article.ArticleID
{

  key artid           as ArticleID,
  key spras           as Language,

      bezei           as Description,

      @Semantics.user.localInstanceLastChangedBy: true
      last_changed_by as LastChangedBy,

      //local ETag field --> OData ETag
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at as LastChangedAt,

      /* Associations */
      _Article
}
