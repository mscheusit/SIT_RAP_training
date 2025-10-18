@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article'
@Metadata.ignorePropagatedAnnotations: false

define root view entity ZSITC_I_MM_Article
  as select from zsitc_mm_article
  composition [0..*] of ZSITC_I_MM_Article_Text as _Text
{

  key artid           as ArticleID,

      artnr           as ArticleNo,
      released        as Released,

      @Semantics.user.createdBy: true
      created_by      as CreatedBy,

      @Semantics.systemDateTime.createdAt: true
      created_at      as CreatedAt,

      @Semantics.user.localInstanceLastChangedBy: true
      last_changed_by as LastChangedBy,

      // local ETag field --> OData ETag
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at as LastChangedAt,

      /* Associations */
      _Text
}
