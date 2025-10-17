CLASS zcl_mm_i_article DEFINITION

  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create_key RETURNING VALUE(rv_key) TYPE zmm_e_artnr RAISING cx_uuid_error.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_mm_i_article IMPLEMENTATION.

  METHOD create_key.

    rv_key = cl_system_uuid=>create_uuid_c22_static( ).

  ENDMETHOD..

ENDCLASS.
