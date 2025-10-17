class ZCL_CA_SRVM_BO definition
  public
  abstract
  create public .

public section.

  events OBJECT_CHGD
    exporting
      value(EO_OBJECT) type ref to ZCL_CA_SRVM_BO .
  events OBJECT_FREED
    exporting
      value(EO_OBJECT) type ref to ZCL_CA_SRVM_BO .
  events OBJECT_DIRTY
    exporting
      value(EO_OBJECT) type ref to ZCL_CA_SRVM_BO .

  methods ADD_OBJECT
    importing
      !IO_OBJECT type ref to ZCL_CA_SRVM_BO .
  methods RAISE_EVENT_CHGD .
  methods RAISE_EVENT_DIRTY .
  methods RAISE_EVENT_FREED .
  methods GET_CLASS_NAME
    returning
      value(RV_CLASS_NAME) type CHAR30 .
  methods CLEAN_UP
    importing
      !IV_CANCEL type abap_boolean default ABAP_FALSE .
  methods CONSTRUCTOR
    importing
      !ID_KEY type ref to DATA
      !ID_XDATA type ref to ZCA_S_XDATA
      !ID_YDATA type ref to ZCA_S_YDATA
      !IO_SRVM type ref to ZCL_CA_SERVICE_MANAGER .
  methods FREE .
  methods GET_D_KEY
    returning
      value(RD_KEY) type ref to DATA .
  methods GET_S_DATA
    importing
      !IV_ACTION type ZCA_E_ACTION optional
      !IS_KEY type ANY
    exporting
      !ES_DATA type ANY
    raising
      ZCX_CA_EXCEPTION .
  methods RELATED_OBJECT_CHGD
    importing
      !IV_CHGD type abap_boolean
      !IO_OBJECT type ref to ZCL_CA_SRVM_BO optional .
  methods SET_ACTION
    importing
      !IV_ACTION type ZCA_E_ACTION .
  methods SET_DIRTY .
  methods SET_S_DATA
    importing
      !IV_ACTION type ZCA_E_ACTION optional
      !IS_DATA type ANY .
  methods PREPARE_FOR_ACTION
    importing
      !IV_ACTION type ZCA_E_ACTION
    raising
      ZCX_CA_EXCEPTION .
  methods GET_ACTION
    returning
      value(RV_ACTION) type ZCA_E_ACTION .
  methods SAVE
    importing
      !IV_SYNCHRON type CHAR1 optional
      !IV_COMMIT type CHAR1 optional
      !IV_FORCE_UPDATE type abap_boolean default ABAP_FALSE
    raising
      ZCX_CA_EXCEPTION .
  methods GET_ID
    returning
      value(RV_OBJID) type ZCA_E_OBJID .
  methods GET_MAINTENANCE_OBJECT
    returning
      value(RV_MAINTENANCE_OBJECT) type CHAR10 .
protected section.

  data MD_XDATA type ref to ZCA_S_XDATA .
  data MD_YDATA type ref to ZCA_S_YDATA .
  data MO_SRVM type ref to ZCL_CA_SERVICE_MANAGER .
  data MD_KEY type ref to DATA .
  data MV_MAINTENANCE_OBJECT type CHAR10 .
  data MV_RELATED_OBJECT_CHGD type abap_boolean value ABAP_FALSE ##NO_TEXT.
  data MV_CLASS_NAME type CHAR30 .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CA_SRVM_BO IMPLEMENTATION.


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
      ASSIGN md_xdata->data->* TO FIELD-SYMBOL(<lfs_data>).
      es_data = CORRESPONDING #( <lfs_data> ).
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
