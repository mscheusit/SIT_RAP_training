CLASS zcl_ca_srvm_bo DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    EVENTS object_chgd
      EXPORTING
        VALUE(eo_object) TYPE REF TO zcl_ca_srvm_bo .
    EVENTS object_freed
      EXPORTING
        VALUE(eo_object) TYPE REF TO zcl_ca_srvm_bo .
    EVENTS object_dirty
      EXPORTING
        VALUE(eo_object) TYPE REF TO zcl_ca_srvm_bo .

    METHODS add_object
      IMPORTING
        !io_object TYPE REF TO zcl_ca_srvm_bo .
    METHODS raise_event_chgd .
    METHODS raise_event_dirty .
    METHODS raise_event_freed .
    METHODS get_class_name
      RETURNING
        VALUE(rv_class_name) TYPE char30 .
    METHODS clean_up
      IMPORTING
        !iv_cancel TYPE abap_boolean DEFAULT abap_false .
    METHODS constructor
      IMPORTING
        !id_key   TYPE REF TO data
        !id_xdata TYPE REF TO zca_s_xdata
        !id_ydata TYPE REF TO zca_s_ydata
        !io_srvm  TYPE REF TO zcl_ca_service_manager .
    METHODS free .
    METHODS get_d_key
      RETURNING
        VALUE(rd_key) TYPE REF TO data .
    METHODS get_s_data
      IMPORTING
        !iv_action TYPE zca_e_action OPTIONAL
        !is_key    TYPE any
      EXPORTING
        !es_data   TYPE any
      RAISING
        zcx_ca_exception .
    METHODS related_object_chgd
      IMPORTING
        !iv_chgd   TYPE abap_boolean
        !io_object TYPE REF TO zcl_ca_srvm_bo OPTIONAL .
    METHODS set_action
      IMPORTING
        !iv_action TYPE zca_e_action .
    METHODS set_dirty .
    METHODS set_s_data
      IMPORTING
        !iv_action TYPE zca_e_action OPTIONAL
        !is_data   TYPE any .
    METHODS prepare_for_action
      IMPORTING
        !iv_action TYPE zca_e_action
      RAISING
        zcx_ca_exception .
    METHODS get_action
      RETURNING
        VALUE(rv_action) TYPE zca_e_action .
    METHODS save
      IMPORTING
        !iv_synchron     TYPE char1 OPTIONAL
        !iv_commit       TYPE char1 OPTIONAL
        !iv_force_update TYPE abap_boolean DEFAULT abap_false
      RAISING
        zcx_ca_exception .
    METHODS get_id
      RETURNING
        VALUE(rv_objid) TYPE zca_e_objid .
    METHODS get_maintenance_object
      RETURNING
        VALUE(rv_maintenance_object) TYPE char10 .
  PROTECTED SECTION.

    DATA md_xdata TYPE REF TO zca_s_xdata .
    DATA md_ydata TYPE REF TO zca_s_ydata .
    DATA mo_srvm TYPE REF TO zcl_ca_service_manager .
    DATA md_key TYPE REF TO data .
    DATA mv_maintenance_object TYPE char10 .
    DATA mv_related_object_chgd TYPE abap_boolean VALUE abap_false ##NO_TEXT.
    DATA mv_class_name TYPE char30 .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_srvm_bo IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->ADD_OBJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_OBJECT                      TYPE REF TO ZCL_CA_SRVM_BO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_object ##NEEDED.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->CLEAN_UP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CANCEL                      TYPE        abap_boolean (default =ABAP_FALSE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD clean_up.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    IF md_xdata IS BOUND AND md_xdata->action IS NOT INITIAL.
      mo_srvm->clean_up( it_objid  = VALUE #( ( md_xdata->objid ) )
                         iv_cancel = iv_cancel ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] ID_KEY                         TYPE REF TO DATA
* | [--->] ID_XDATA                       TYPE REF TO ZCA_S_XDATA
* | [--->] ID_YDATA                       TYPE REF TO ZCA_S_YDATA
* | [--->] IO_SRVM                        TYPE REF TO ZCL_CA_SERVICE_MANAGER
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

    md_key = id_key.
    md_xdata = id_xdata.
    md_ydata = id_ydata.
    mo_srvm = io_srvm.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->FREE
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

    IF mo_srvm IS BOUND AND md_xdata IS BOUND.
      mo_srvm->free( it_objid = VALUE #( ( md_xdata->objid ) ) ).
    ELSE.
      FREE mo_srvm.
    ENDIF.

    CLEAR:
      mv_class_name,
      mv_maintenance_object,
      mv_related_object_chgd WITH abap_false.

    FREE:
      md_key,
      md_xdata,
      md_ydata.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->GET_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ACTION                      TYPE        ZCA_E_ACTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_action.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    rv_action = md_xdata->action.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->GET_CLASS_NAME
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_CLASS_NAME                  TYPE        SEOCLSNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_class_name.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    rv_class_name = mv_class_name.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->GET_D_KEY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RD_KEY                         TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_d_key.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    rd_key = md_key.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->GET_ID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_OBJID                       TYPE        ZCA_E_OBJID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_id.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    rv_objid = md_xdata->objid.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->GET_MAINTENANCE_OBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_MAINTENANCE_OBJECT          TYPE        CHAR10
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_maintenance_object.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    rv_maintenance_object = mv_maintenance_object.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->GET_S_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION(optional)
* | [--->] IS_KEY                         TYPE        ANY
* | [<---] ES_DATA                        TYPE        ANY
* | [!CX!] ZCX_CA_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_s_data.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    IF md_xdata->fully_loaded = abap_true AND iv_action IS INITIAL.
      ASSIGN md_xdata->data->* TO FIELD-SYMBOL(<ls_data>).
      es_data = CORRESPONDING #( <ls_data> ).
    ELSE.
      CALL METHOD mo_srvm->get
        EXPORTING
          iv_action = iv_action
          is_key    = is_key
        IMPORTING
          es_data   = es_data.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->PREPARE_FOR_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION
* | [!CX!] ZCX_CA_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD prepare_for_action.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    mo_srvm->prepare_for_action( iv_action = iv_action
                                 io_object = me ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->RAISE_EVENT_CHGD
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD raise_event_chgd.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    RAISE EVENT object_chgd EXPORTING eo_object = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->RAISE_EVENT_DIRTY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD raise_event_dirty.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    RAISE EVENT object_dirty EXPORTING eo_object = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->RAISE_EVENT_FREED
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD raise_event_freed.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    RAISE EVENT object_freed EXPORTING eo_object = me.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->RELATED_OBJECT_CHGD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CHGD                        TYPE        abap_boolean
* | [--->] IO_OBJECT                      TYPE REF TO ZCL_CA_SRVM_BO(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD related_object_chgd.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    mv_related_object_chgd = iv_chgd.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SYNCHRON                    TYPE        CHAR1(optional)
* | [--->] IV_COMMIT                      TYPE        CHAR1(optional)
* | [--->] IV_FORCE_UPDATE                TYPE        abap_boolean (default =ABAP_FALSE)
* | [!CX!] ZCX_CA_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*                                             *

    mo_srvm->save( iv_synchron     = iv_synchron
                   iv_commit       = iv_commit
                   it_objid        = VALUE #( ( md_xdata->objid ) )
                   iv_force_update = boolc( iv_force_update = abap_true OR mv_related_object_chgd  = abap_true ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->SET_DIRTY
* +-------------------------------------------------------------------------------------------------+
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

    IF md_xdata->fully_loaded = abap_true.
      mo_srvm->set_dirty( it_objid = VALUE #( ( md_xdata->objid ) ) ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->SET_S_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION(optional)
* | [--->] IS_DATA                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_s_data.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    TRY.
        ASSIGN md_key->* TO FIELD-SYMBOL(<lv_objid>).
        mo_srvm->set_data( iv_action = iv_action
                           iv_id     = CONV #( <lv_objid> )
                           is_data   = is_data ).
      CATCH cx_root ##CATCH_ALL.                        "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SRVM_BO->SET_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_action.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    mo_srvm->set_action( iv_id     =  md_xdata->objid
                         iv_action = iv_action ).

  ENDMETHOD.
ENDCLASS.
