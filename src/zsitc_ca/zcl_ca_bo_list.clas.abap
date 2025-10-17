CLASS zcl_ca_bo_list DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS clean_up
      IMPORTING
        !iv_clean_up_objects TYPE abap_boolean DEFAULT abap_false
        !iv_cancel           TYPE abap_boolean DEFAULT abap_false .
    METHODS set_dirty .
    METHODS add
      IMPORTING
        !io_object TYPE REF TO zcl_ca_srvm_bo .
    METHODS get
      RETURNING
        VALUE(rt_object) TYPE zca_t_srvm_bo .
    METHODS constructor
      IMPORTING
        !io_object    TYPE REF TO zcl_ca_srvm_bo
        !io_srvm_data TYPE REF TO zcl_ca_service_manager .
  PROTECTED SECTION.

    DATA mo_object TYPE REF TO zcl_ca_srvm_bo .
    DATA mt_list TYPE zca_t_srvm_bo .

    METHODS on_object_chgd
      FOR EVENT object_chgd OF zcl_ca_srvm_bo
      IMPORTING
        !eo_object .
    METHODS on_object_freed
      FOR EVENT object_freed OF zcl_ca_srvm_bo
      IMPORTING
        !eo_object .
    METHODS on_object_dirty
      FOR EVENT object_dirty OF zcl_ca_srvm_bo
      IMPORTING
        !eo_object .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_bo_list IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_BO_LIST->ADD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_OBJECT                      TYPE REF TO ZCL_CA_SRVM_BO
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

    INSERT io_object INTO TABLE mt_list.
    IF sy-subrc = 0 AND
       io_object IS BOUND AND
       io_object->get_maintenance_object( ) = mo_object->get_maintenance_object( ).

      " inform maintenance object that an object is added
      on_object_chgd( io_object ).

      SET HANDLER:
        on_object_chgd  FOR io_object,
        on_object_freed FOR io_object,
        on_object_dirty FOR io_object.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_BO_LIST->CLEAN_UP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLEAN_UP_OBJECTS            TYPE        abap_boolean (default =ABAP_FALSE)
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

    IF iv_clean_up_objects = abap_true.
      DATA(lt_list) = mt_list.
      LOOP AT lt_list INTO DATA(lo_object) WHERE table_line IS BOUND ##INTO_OK. "#EC CI_SORTSEQ
        lo_object->clean_up( iv_cancel = iv_cancel ).
      ENDLOOP.
    ENDIF.

    " deactivate handler
    SET HANDLER:
      on_object_chgd FOR ALL INSTANCES ACTIVATION space,
      on_object_dirty FOR ALL INSTANCES ACTIVATION space,
      on_object_freed FOR ALL INSTANCES ACTIVATION space.

    CLEAR mt_list.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_BO_LIST->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_OBJECT                      TYPE REF TO ZCL_CA_SRVM_BO
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

    mo_object = io_object.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_BO_LIST->GET
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_OBJECT                      TYPE        ZCA_T_SRVM_BO
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

    rt_object = mt_list.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_BO_LIST->ON_OBJECT_CHGD
* +-------------------------------------------------------------------------------------------------+
* | [--->] EO_OBJECT                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_object_chgd.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    IF mo_object->get_maintenance_object( ) = eo_object->get_maintenance_object( ).
      mo_object->related_object_chgd( iv_chgd   = abap_true
                                      io_object = eo_object ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_BO_LIST->ON_OBJECT_DIRTY
* +-------------------------------------------------------------------------------------------------+
* | [--->] EO_OBJECT                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_object_dirty.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    " deactivate handler
    SET HANDLER:
      on_object_chgd FOR ALL INSTANCES ACTIVATION space,
      on_object_dirty FOR ALL INSTANCES ACTIVATION space,
      on_object_freed FOR ALL INSTANCES ACTIVATION space.

    CLEAR mt_list.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_BO_LIST->ON_OBJECT_FREED
* +-------------------------------------------------------------------------------------------------+
* | [--->] EO_OBJECT                      LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_object_freed.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    " deactivate handler
    SET HANDLER:
      on_object_chgd FOR eo_object ACTIVATION space,
      on_object_dirty FOR eo_object ACTIVATION space,
      on_object_freed FOR eo_object ACTIVATION space.

    DELETE mt_list WHERE table_line = eo_object.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_BO_LIST->SET_DIRTY
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

    DATA(lt_list) = mt_list.
    LOOP AT lt_list INTO DATA(lo_object) WHERE table_line IS BOUND ##INTO_OK. "#EC CI_SORTSEQ
      lo_object->set_dirty( ).
    ENDLOOP.
    IF sy-subrc <> 0.
      CLEAR mt_list.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
