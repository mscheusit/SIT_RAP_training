CLASS zcl_ca_data_list DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS free .
    METHODS set_dirty
      IMPORTING
        !iv_id TYPE zca_e_objid .
    METHODS add
      IMPORTING
        !iv_id   TYPE zca_e_objid OPTIONAL
        !it_item TYPE zca_t_ddata .
    METHODS get
      IMPORTING
        !iv_id           TYPE zca_e_objid OPTIONAL
      EXPORTING
        !ev_fully_loaded TYPE abap_boolean
        !et_item         TYPE zca_t_ddata .
    METHODS constructor
      IMPORTING
        !io_srvm      TYPE REF TO zcl_ca_service_manager
        !io_srvm_data TYPE REF TO zcl_ca_service_manager .
  PROTECTED SECTION.

    DATA mo_srvm TYPE REF TO zcl_ca_service_manager .
    DATA mt_list TYPE zca_t_data .

    METHODS on_data_freed
      FOR EVENT data_freed OF zcl_ca_service_manager
      IMPORTING
        !ed_data .
    METHODS on_data_inserted
      FOR EVENT data_insd OF zcl_ca_service_manager
      IMPORTING
        !ed_data
        !eo_object .
    METHODS on_data_dirty
      FOR EVENT data_dirty OF zcl_ca_service_manager
      IMPORTING
        !ed_data .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_data_list IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_DATA_LIST->ADD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID(optional)
* | [--->] IT_ITEM                        TYPE        ZCA_T_DDATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    IF iv_id IS SUPPLIED.
      READ TABLE mt_list ASSIGNING FIELD-SYMBOL(<ls_list>) WITH TABLE KEY id = iv_id.
      IF sy-subrc = 0.
        LOOP AT it_item ASSIGNING FIELD-SYMBOL(<ls_item>).
          INSERT <ls_item> INTO TABLE <ls_list>-t_item.
        ENDLOOP.
        <ls_list>-fully_loaded = abap_true.
      ELSE.
        INSERT VALUE #( id = iv_id
                        fully_loaded = abap_true
                        t_item = it_item ) INTO TABLE mt_list.
      ENDIF.
    ELSEIF it_item IS NOT INITIAL.
      DATA(ldr_item) = it_item[ 1 ].
      ASSIGN ldr_item->* TO <ls_item>.
      DATA(lv_id) = mo_srvm->get_id( <ls_item> ).
      READ TABLE mt_list ASSIGNING <ls_list> WITH TABLE KEY id = lv_id.
      IF sy-subrc = 0.
        LOOP AT it_item ASSIGNING <ls_item>.
          INSERT <ls_item> INTO TABLE <ls_list>-t_item.
        ENDLOOP.
        <ls_list>-fully_loaded = abap_true.
      ELSE.
        INSERT VALUE #( id = lv_id
                        fully_loaded = abap_true
                        t_item = it_item ) INTO TABLE mt_list.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_DATA_LIST->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SRVM                        TYPE REF TO ZCL_CA_SERVICE_MANAGER
* | [--->] IO_SRVM_DATA                   TYPE REF TO ZCL_CA_SERVICE_MANAGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    mo_srvm = io_srvm.
    SET HANDLER:
      on_data_inserted FOR io_srvm_data,
      on_data_freed    FOR io_srvm_data,
      on_data_dirty    FOR io_srvm_data.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_DATA_LIST->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID(optional)
* | [<---] EV_FULLY_LOADED                TYPE        abap_boolean
* | [<---] ET_ITEM                        TYPE        ZCA_T_DDATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    CLEAR:
      ev_fully_loaded,
      et_item.

    READ TABLE mt_list ASSIGNING FIELD-SYMBOL(<ls_list>) WITH TABLE KEY id = iv_id.
    IF sy-subrc = 0.
      ev_fully_loaded = <ls_list>-fully_loaded.
      IF <ls_list>-fully_loaded = abap_true.
        et_item = <ls_list>-t_item.
      ELSE.
        CLEAR <ls_list>-t_item.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_DATA_LIST->ON_DATA_DIRTY
* +-------------------------------------------------------------------------------------------------+
* | [--->] ED_DATA                        LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_data_dirty.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    ASSIGN ed_data->* TO FIELD-SYMBOL(<ls_any>).
    ASSIGN mt_list[ id = mo_srvm->get_id( <ls_any> ) ] TO FIELD-SYMBOL(<ls_list>).
    IF sy-subrc = 0.
      <ls_list>-fully_loaded = abap_false.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_DATA_LIST->ON_DATA_FREED
* +-------------------------------------------------------------------------------------------------+
* | [--->] ED_DATA                        LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_data_freed.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    ASSIGN ed_data->* TO FIELD-SYMBOL(<ls_any>).
    READ TABLE mt_list ASSIGNING FIELD-SYMBOL(<ls_list>) WITH TABLE KEY id = mo_srvm->get_id( <ls_any> ).
    IF sy-subrc = 0.
      DELETE <ls_list>-t_item WHERE table_line = ed_data.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_DATA_LIST->ON_DATA_INSERTED
* +-------------------------------------------------------------------------------------------------+
* | [--->] ED_DATA                        LIKE
* | [--->] EO_OBJECT                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_data_inserted.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    DATA lo_object TYPE REF TO zcl_ca_srvm_bo.

    ASSIGN ed_data->* TO FIELD-SYMBOL(<ls_any>).
    DATA(lv_id) = mo_srvm->get_id( <ls_any> ).
    IF lv_id IS NOT INITIAL.
      add( iv_id   = lv_id
           it_item = VALUE #( ( ed_data ) ) ).

      IF eo_object IS BOUND.
        TRY.
            mo_srvm->get( EXPORTING iv_id = lv_id
                          IMPORTING eo_object = lo_object ).
            IF lo_object IS BOUND.
              lo_object->add_object( eo_object ).
            ENDIF.
          CATCH cx_root ##CATCH_ALL.                    "#EC NO_HANDLER
        ENDTRY.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_DATA_LIST->SET_DIRTY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_dirty.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    ASSIGN mt_list[ id = iv_id ] TO FIELD-SYMBOL(<ls_list>).
    IF sy-subrc = 0.
      <ls_list>-fully_loaded = abap_false.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_DATA_LIST->FREE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD free.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    CLEAR mt_list.
*    FREE mo_srvm.
*
*    " deactivate handler
*    SET HANDLER:
*      on_data_inserted FOR ALL INSTANCES ACTIVATION space,
*      on_data_freed    FOR ALL INSTANCES ACTIVATION space,
*      on_set_dirty     FOR ALL INSTANCES ACTIVATION space.

  ENDMETHOD.
ENDCLASS.
