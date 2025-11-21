CLASS zcl_mm_i_article DEFINITION

  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-DATA gv_eml_bo_bl_enabled TYPE abap_boolean value abap_true.
    CLASS-METHODS:
      create_key RETURNING VALUE(rv_key) TYPE zmm_e_artnr RAISING cx_uuid_error,
      is_update_able IMPORTING is_article TYPE ZSITC_I_MM_Article RETURNING VALUE(rv_upd_able) TYPE abap_boolean.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_MM_I_ARTICLE IMPLEMENTATION.


  METHOD create_key.

    rv_key = cl_system_uuid=>create_uuid_c22_static( ).

  ENDMETHOD.


  METHOD is_update_able.

    rv_upd_able = xsdbool( is_article-Released IS INITIAL ).

  ENDMETHOD.
ENDCLASS.
