@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Article Type'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #S,
    dataClass: #CUSTOMIZING
}
@ObjectModel.resultSet.sizeCategory: #XS

define view entity ZSITC_I_MM_ARTICLE_TYPE_VH
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE( p_domain_name : 'ZMM_D_ARTTP' ) as Value
  association [0..1] to DDCDS_CUSTOMER_DOMAIN_VALUE_T as _text on  Value.domain_name    = _text.domain_name
                                                               and Value.value_position = _text.value_position
                                                               and _text.language       = $session.system_language
{
      @UI.hidden: true
  key Value.domain_name                                                        as Name,
      @UI.hidden: true
  key Value.value_position                                                     as ValuePosition,
      @UI.hidden: true
      Value.value_low                                                          as Value,
      @UI.lineItem: [{ position:  10 }]
      coalesce( _text( p_domain_name : 'ZMM_D_ARTTP' ).text, Value.value_low ) as Description
}
