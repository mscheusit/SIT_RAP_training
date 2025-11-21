@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article'
@Metadata.ignorePropagatedAnnotations: false

/*+[hideWarning] { "IDS" : [ "CARDINALITY_CHECK" ]  } */
define root view entity ZSITC_I_MM_Article
  as select from zsitc_mm_article
  composition [0..*] of ZSITC_I_MM_Article_Text    as _Text
  association [1..1] to ZSITC_I_MM_ARTICLE_TYPE_VH as _Type on $projection.ArticleType = _Type.Value
{

  key artid           as ArticleID,

      artnr           as ArticleNo,
      arttp           as ArticleType,

      @EndUserText.label: 'Regular Vendor'
      rlifn           as RegularVendor,

      @EndUserText.label: 'Released'
      released        as Released,

      @EndUserText.label: 'Released Date'
      released_date   as ReleasedDate,

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
      _Text,
      _Type
}
