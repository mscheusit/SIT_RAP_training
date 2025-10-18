CLASS zcl_ca_service_manager DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_x TYPE char1 VALUE 'X' ##NO_TEXT.
    CONSTANTS gc_aktyp_insert TYPE char1 VALUE 'I' ##NO_TEXT.
    CONSTANTS gc_aktyp_update TYPE char1 VALUE 'U' ##NO_TEXT.
    CONSTANTS gc_aktyp_delete TYPE char1 VALUE 'D' ##NO_TEXT.

    EVENTS data_chgd
      EXPORTING
        VALUE(ed_data) TYPE REF TO data .
    EVENTS data_freed
      EXPORTING
        VALUE(ed_data) TYPE REF TO data .
    EVENTS data_dirty
      EXPORTING
        VALUE(ed_data) TYPE REF TO data .
    EVENTS data_insd
      EXPORTING
        VALUE(ed_data)   TYPE REF TO data
        VALUE(eo_object) TYPE REF TO zcl_ca_srvm_bo OPTIONAL .

    METHODS set_enqueue_params
      IMPORTING
        !iv_enqueue_scope   TYPE char1 DEFAULT ''
        !iv_enqueue_wait    TYPE char1 DEFAULT ''
        !iv_enqueue_collect TYPE char1 DEFAULT '' .
    CLASS-METHODS add_message
      IMPORTING
        !is_message     TYPE zca_bal_s_msg OPTIONAL
        !is_bapireturn1 TYPE bapireturn1 OPTIONAL .
    METHODS messaging
      IMPORTING
        !iv_action TYPE zca_e_action OPTIONAL
        !is_data   TYPE any
      EXPORTING
        !ev_kappl  TYPE any
        !ev_objkey TYPE any
        !ev_kalsm  TYPE any
        !et_msgpa  TYPE STANDARD TABLE .
    METHODS set_action
      IMPORTING
        !iv_id     TYPE zca_e_objid
        !iv_action TYPE zca_e_action .
    METHODS messaging_update
      IMPORTING
        !iv_synchron TYPE char1
        !iv_kappl    TYPE any
        !iv_objkey   TYPE any .
    METHODS clean_up
      IMPORTING
        !iv_cancel TYPE abap_boolean DEFAULT abap_false
        !it_objid  TYPE zca_t_objid OPTIONAL .
    METHODS set_dirty
      IMPORTING
        !it_objid TYPE zca_t_objid OPTIONAL .
    METHODS free
      IMPORTING
        !it_objid TYPE zca_t_objid OPTIONAL .
    METHODS get_messages
      RETURNING
        VALUE(rt_message) TYPE zca_bal_t_msg .
    METHODS get
      IMPORTING
        VALUE(iv_action)      TYPE zca_e_action OPTIONAL
        !iv_id                TYPE zca_e_objid OPTIONAL
        !is_key               TYPE any OPTIONAL
        !it_range             TYPE zca_t_range OPTIONAL
        !iv_read_range_option TYPE zca_e_read_range_option OPTIONAL
      EXPORTING
        !es_data              TYPE any
        !eo_object            TYPE REF TO zcl_ca_srvm_bo
        !et_data              TYPE STANDARD TABLE
        !et_object            TYPE zca_t_srvm_bo
        !et_ddata             TYPE zca_t_ddata
      RAISING
        zcx_ca_exception .
    METHODS prepare_for_action
      IMPORTING
        !iv_action TYPE zca_e_action
        !io_object TYPE REF TO zcl_ca_srvm_bo OPTIONAL
        !it_object TYPE zca_t_srvm_bo OPTIONAL
      RAISING
        zcx_ca_exception .
    METHODS set_data
      IMPORTING
        VALUE(iv_action) TYPE zca_e_action OPTIONAL
        !iv_enqueue      TYPE abap_boolean DEFAULT abap_true
        !iv_id           TYPE zca_e_objid OPTIONAL
        !is_data         TYPE any OPTIONAL
        !it_data         TYPE STANDARD TABLE OPTIONAL
      EXPORTING
        !et_object       TYPE zca_t_srvm_bo
      RETURNING
        VALUE(rt_data)   TYPE zca_t_ddata
      RAISING
        zcx_ca_exception .
    METHODS dequeue
      IMPORTING
        !iv_id TYPE zca_e_objid .
    METHODS enqueue
      IMPORTING
        !iv_action TYPE zca_e_action
        !iv_id     TYPE zca_e_objid
      RAISING
        zcx_ca_exception .
    METHODS save
      IMPORTING
        !iv_synchron     TYPE char1 OPTIONAL
        !iv_commit       TYPE char1 OPTIONAL
        !it_objid        TYPE zca_t_objid OPTIONAL
        !iv_force_update TYPE abap_boolean DEFAULT abap_false
      RAISING
        zcx_ca_exception .
    METHODS get_id
      IMPORTING
        !is_data        TYPE any
      RETURNING
        VALUE(rv_objid) TYPE zca_e_objid .
  PROTECTED SECTION.

    DATA mt_xdata TYPE zca_t_xdata .
    DATA mt_ydata TYPE zca_t_ydata .
    CLASS-DATA gt_message TYPE zca_bal_t_msg .
    DATA mv_enqueue_scope TYPE char1 VALUE '1' ##NO_TEXT.
    DATA mv_enqueue_collect TYPE char1 VALUE ' ' ##NO_TEXT.
    DATA mv_enqueue_wait TYPE char1 VALUE space ##NO_TEXT.

    METHODS move_id_to_data
      IMPORTING
        !iv_id   TYPE zca_e_objid
      CHANGING
        !cs_data TYPE any .
    METHODS enrich_id
      IMPORTING
        !is_data TYPE any OPTIONAL
      CHANGING
        !cv_id   TYPE zca_e_objid .
    METHODS get_ddic_db
      RETURNING
        VALUE(rv_ddic_db) TYPE char120 .
    METHODS data_is_dirty
      IMPORTING
        !iv_id TYPE zca_e_objid .
    METHODS create_object
      IMPORTING
        !is_xdata        TYPE zca_s_xdata
        !is_ydata        TYPE zca_s_ydata
      RETURNING
        VALUE(ro_object) TYPE REF TO zcl_ca_srvm_bo .
    METHODS is_valid
      IMPORTING
        !iv_action TYPE zca_e_action
        !is_data   TYPE any
      RAISING
        zcx_ca_exception .
    METHODS select_by_key
      IMPORTING
        !is_key  TYPE any
      EXPORTING
        !es_data TYPE any .
    METHODS select_by_range
      IMPORTING
        !iv_read_range_option TYPE zca_e_read_range_option
        !it_range             TYPE zca_t_range
      EXPORTING
        !et_data              TYPE STANDARD TABLE .
    METHODS save_db
      IMPORTING
        !iv_synchron    TYPE char1
        !iv_commit      TYPE char1
        !iv_action      TYPE zca_e_action
        !it_data        TYPE STANDARD TABLE
        !it_ydata       TYPE STANDARD TABLE OPTIONAL
      EXPORTING
        !ev_commit_done TYPE abap_boolean
      RAISING
        zcx_ca_exception .
    METHODS enrich_data_before_save
      IMPORTING
        !iv_action TYPE zca_e_action
        !id_data   TYPE REF TO data .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_service_manager IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->SET_ENQUEUE_PARAMS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENQUEUE_SCOPE               TYPE        CHAR1 (default ='')
* | [--->] IV_ENQUEUE_WAIT                TYPE        CHAR1 (default ='')
* | [--->] IV_ENQUEUE_COLLECT             TYPE        DDENQCOLL (default ='')
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_enqueue_params.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    IF iv_enqueue_scope IS SUPPLIED.
      mv_enqueue_scope = iv_enqueue_scope.
    ENDIF.
    IF iv_enqueue_wait IS SUPPLIED.
      mv_enqueue_wait = iv_enqueue_wait.
    ENDIF.
    IF iv_enqueue_collect IS SUPPLIED.
      mv_enqueue_collect = iv_enqueue_collect.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->SET_DIRTY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_OBJID                       TYPE        ZCA_T_OBJID(optional)
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

    DATA lt_r_objid TYPE RANGE OF zca_e_objid.

    IF it_objid IS NOT INITIAL.
      lt_r_objid = VALUE #( FOR lv_objid IN it_objid ( sign   = 'I'
                                                       option = 'EQ'
                                                       low    = lv_objid ) ).
    ENDIF.

    LOOP AT mt_xdata ASSIGNING FIELD-SYMBOL(<ls_xdata>) WHERE objid IN lt_r_objid. "#EC CI_SORTSEQ
      RAISE EVENT data_dirty EXPORTING ed_data = <ls_xdata>-data.
      data_is_dirty( <ls_xdata>-objid ).
      CLEAR <ls_xdata>-fully_loaded.
      IF <ls_xdata>-object IS BOUND.
        <ls_xdata>-object->raise_event_dirty( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->SET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION(optional)
* | [--->] IV_ENQUEUE                     TYPE        abap_boolean (default =ABAP_TRUE)
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID(optional)
* | [--->] IS_DATA                        TYPE        ANY(optional)
* | [--->] IT_DATA                        TYPE        STANDARD TABLE(optional)
* | [<---] ET_OBJECT                      TYPE        ZCA_T_SRVM_BO
* | [<-()] RT_DATA                        TYPE        ZCA_T_DDATA
* | [!CX!] ZCX_CA_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_data.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    DATA:
      lv_fully_loaded TYPE abap_boolean VALUE abap_true,
      lv_objid        TYPE zca_e_objid,
      lt_object       TYPE zca_t_srvm_bo.

    CLEAR et_object.

    IF is_data IS NOT INITIAL.
      IF iv_id IS INITIAL.
        lv_objid = get_id( is_data ).
      ELSE.
        lv_objid = iv_id.
      ENDIF.
      READ TABLE mt_xdata ASSIGNING FIELD-SYMBOL(<ls_xdata>) WITH TABLE KEY objid = lv_objid.
      IF sy-subrc = 0.
        IF iv_action IS NOT INITIAL.
          IF iv_action = gc_aktyp_insert.
            RAISE EXCEPTION TYPE zcx_ca_exception.
          ENDIF.
          IF <ls_xdata>-action IS INITIAL AND iv_enqueue = abap_true.
            lv_fully_loaded = abap_false.
            TRY.
                enqueue( iv_action = iv_action
                         iv_id     = lv_objid ).
              CATCH cx_root ##CATCH_ALL.
                CLEAR iv_action.
            ENDTRY.
          ENDIF.
          <ls_xdata>-action = iv_action.
        ENDIF.
        ASSIGN <ls_xdata>-data->* TO FIELD-SYMBOL(<ls_x_data>).
        <ls_x_data> = CORRESPONDING #( is_data ).

        IF iv_action IS INITIAL AND <ls_xdata>-action IS INITIAL.
          READ TABLE mt_ydata ASSIGNING FIELD-SYMBOL(<ls_ydata>) WITH TABLE KEY objid = lv_objid.
          IF sy-subrc = 0.
            ASSIGN <ls_ydata>-data->* TO FIELD-SYMBOL(<ls_y_data>).
            <ls_y_data> = <ls_x_data>.
          ENDIF.
        ENDIF.

        RAISE EVENT data_chgd EXPORTING ed_data = <ls_xdata>-data.
        IF <ls_xdata>-object IS BOUND.
          <ls_xdata>-object->raise_event_chgd( ).
        ENDIF.
      ELSE.
        IF iv_action IS NOT INITIAL AND iv_enqueue = abap_true.
          IF iv_action = gc_aktyp_insert.
            enrich_id(
              EXPORTING
                is_data = is_data
              CHANGING
                cv_id = lv_objid ).
          ELSE.
            lv_fully_loaded = abap_false.
          ENDIF.
          TRY.
              enqueue( iv_action = iv_action
                       iv_id = lv_objid ).
            CATCH cx_root ##CATCH_ALL.
              CLEAR iv_action.
          ENDTRY.
        ENDIF.

        IF lv_objid IS INITIAL.
          RAISE EXCEPTION TYPE zcx_ca_exception.
        ENDIF.

        INSERT VALUE #( objid = lv_objid ) INTO TABLE mt_xdata ASSIGNING <ls_xdata>.
        IF iv_action IS NOT INITIAL.
          <ls_xdata>-action = iv_action.
        ENDIF.
        CREATE DATA <ls_xdata>-data LIKE is_data.

        INSERT VALUE #( objid = lv_objid ) INTO TABLE mt_ydata ASSIGNING <ls_ydata>.
        CREATE DATA <ls_ydata>-data LIKE is_data.

        ASSIGN <ls_xdata>-data->* TO <ls_x_data>.
        <ls_x_data> = CORRESPONDING #( is_data ).
        IF iv_action = gc_aktyp_insert.
          move_id_to_data( EXPORTING iv_id = lv_objid
                           CHANGING  cs_data = <ls_x_data> ).
        ENDIF.

        ASSIGN <ls_ydata>-data->* TO <ls_y_data>.
        <ls_y_data> = <ls_x_data>.

        <ls_xdata>-object = create_object( is_xdata = <ls_xdata>
                                            is_ydata = <ls_ydata> ).

        IF <ls_xdata>-object IS BOUND.
          RAISE EVENT data_insd EXPORTING ed_data = <ls_xdata>-data eo_object = <ls_xdata>-object.
        ELSE.
          RAISE EVENT data_insd EXPORTING ed_data = <ls_xdata>-data.
        ENDIF.
      ENDIF.
      <ls_xdata>-fully_loaded = lv_fully_loaded.
      INSERT <ls_xdata>-data INTO TABLE rt_data.

      IF et_object IS REQUESTED.
        INSERT <ls_xdata>-object INTO TABLE et_object.
      ENDIF.
    ELSEIF it_data IS NOT INITIAL.
      LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        TRY.
            IF et_object IS REQUESTED.
              INSERT LINES OF set_data(
                EXPORTING
                  iv_action = iv_action
                  is_data   = <ls_data>
                IMPORTING
                  et_object = lt_object ) INTO TABLE rt_data.
              INSERT LINES OF lt_object INTO TABLE et_object.
            ELSE.
              INSERT LINES OF set_data( iv_action = iv_action
                                        is_data   = <ls_data> ) INTO TABLE rt_data.
            ENDIF.
          CATCH cx_root ##CATCH_ALL ##NO_HANDLER.
        ENDTRY.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->SELECT_BY_RANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_READ_RANGE_OPTION           TYPE        ZCA_E_READ_RANGE_OPTION
* | [--->] IT_RANGE                       TYPE        ZCA_T_RANGE
* | [<---] ET_DATA                        TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD select_by_range.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    CLEAR et_data.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->SELECT_BY_KEY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_KEY                         TYPE        ANY
* | [<---] ES_DATA                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD select_by_key.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    CLEAR es_data.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->SAVE_DB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SYNCHRON                    TYPE        CHAR1
* | [--->] IV_COMMIT                      TYPE        CHAR1
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION
* | [--->] IT_DATA                        TYPE        STANDARD TABLE
* | [--->] IT_YDATA                       TYPE        STANDARD TABLE(optional)
* | [<---] EV_COMMIT_DONE                 TYPE        abap_boolean
* | [!CX!] ZCX_CA_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save_db.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    ev_commit_done = abap_false.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SYNCHRON                    TYPE        CHAR1(optional)
* | [--->] IV_COMMIT                      TYPE        CHAR1(optional)
* | [--->] IT_OBJID                       TYPE        ZCA_T_OBJID(optional)
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
*----------------------------------------------------------------------*

    DATA:
      lv_commit_done  TYPE abap_boolean VALUE abap_false,
*      lv_kappl        TYPE kappl,
*      lv_objkey       TYPE na_objkey,
      lv_ddic_db      TYPE char120,
      ldr_data_db_ins TYPE REF TO data,
      ldr_data_db_upd TYPE REF TO data,
      ldr_data_db_del TYPE REF TO data,
      lt_r_objid      TYPE RANGE OF zca_e_objid,
      lt_objects      TYPE zca_t_srvm_bo.

    FIELD-SYMBOLS:
      <lft_data_db_ins>  TYPE STANDARD TABLE,
      <lft_data_db_upd>  TYPE STANDARD TABLE,
      <lft_data_db_del>  TYPE STANDARD TABLE,
      <lft_ydata_db_upd> TYPE STANDARD TABLE.

    CLEAR gt_message.

    CHECK mt_xdata IS NOT INITIAL.

    IF it_objid IS NOT INITIAL.
      lt_r_objid = VALUE #( FOR lv_objid IN it_objid ( sign   = 'I'
                                                       option = 'EQ'
                                                       low    = lv_objid ) ).
    ENDIF.

    LOOP AT mt_xdata ASSIGNING FIELD-SYMBOL(<ls_xdata>) WHERE objid IN lt_r_objid AND
                                                               action IS NOT INITIAL. "#EC CI_SORTSEQ
      IF <ls_xdata>-object IS BOUND.
        INSERT <ls_xdata>-object INTO TABLE lt_objects.
      ENDIF.
      TRY.
          IF lv_ddic_db IS INITIAL.
            lv_ddic_db = get_ddic_db( ).
          ENDIF.
          enrich_data_before_save( iv_action = <ls_xdata>-action
                                   id_data   = <ls_xdata>-data ).
          ASSIGN <ls_xdata>-data->* TO FIELD-SYMBOL(<ls_data>).

*          messaging(
*            EXPORTING
*              iv_action = <ls_xdata>-action
*              is_data   = <ls_data>
*            IMPORTING
*              ev_kappl  = lv_kappl
*              ev_objkey = lv_objkey ).
          CASE <ls_xdata>-action.
            WHEN gc_aktyp_insert.
              is_valid( iv_action = <ls_xdata>-action
                        is_data   =  <ls_data> ).
              IF ldr_data_db_ins IS NOT BOUND.
                CREATE DATA ldr_data_db_ins TYPE STANDARD TABLE OF (lv_ddic_db).
                ASSIGN ldr_data_db_ins->* TO <lft_data_db_ins>.
              ENDIF.
              APPEND INITIAL LINE TO <lft_data_db_ins> ASSIGNING FIELD-SYMBOL(<ls_data_db>).
              MOVE-CORRESPONDING <ls_data> TO <ls_data_db>.

              " ERDAT, ERZET, ERNAM
              ASSIGN COMPONENT 'ERDAT' OF STRUCTURE <ls_data_db> TO FIELD-SYMBOL(<lv_any>).
              IF sy-subrc = 0 AND <lv_any> IS INITIAL.
                <lv_any> = sy-datum.
              ENDIF.
              ASSIGN COMPONENT 'ERZET' OF STRUCTURE <ls_data_db> TO <lv_any>.
              IF sy-subrc = 0 AND <lv_any> IS INITIAL.
                <lv_any> = sy-uzeit.
              ENDIF.
              ASSIGN COMPONENT 'ERNAM' OF STRUCTURE <ls_data_db> TO <lv_any>.
              IF sy-subrc = 0 AND <lv_any> IS INITIAL.
                <lv_any> = sy-uname.
              ENDIF.

              " AEDAT, AEZET, AENAM
              ASSIGN COMPONENT 'AEDAT' OF STRUCTURE <ls_data_db> TO <lv_any>.
              IF sy-subrc = 0 AND <lv_any> IS INITIAL.
                <lv_any> = sy-datum.
              ENDIF.
              ASSIGN COMPONENT 'AEZET' OF STRUCTURE <ls_data_db> TO <lv_any>.
              IF sy-subrc = 0 AND <lv_any> IS INITIAL.
                <lv_any> = sy-uzeit.
              ENDIF.
              ASSIGN COMPONENT 'AENAM' OF STRUCTURE <ls_data_db> TO <lv_any>.
              IF sy-subrc = 0 AND <lv_any> IS INITIAL.
                <lv_any> = sy-uname.
              ENDIF.
            WHEN gc_aktyp_update.
              READ TABLE mt_ydata ASSIGNING FIELD-SYMBOL(<ls_ydata>) WITH TABLE KEY objid = <ls_xdata>-objid.
              IF sy-subrc = 0.
                ASSIGN <ls_ydata>-data->* TO FIELD-SYMBOL(<ls_ydat>).
                IF iv_force_update = abap_true OR <ls_data> <> <ls_ydat>.
                  is_valid( iv_action = <ls_xdata>-action
                            is_data   =  <ls_data> ).
                  IF ldr_data_db_upd IS NOT BOUND.
                    CREATE DATA ldr_data_db_upd TYPE STANDARD TABLE OF (lv_ddic_db).
                    ASSIGN ldr_data_db_upd->* TO <lft_data_db_upd>.
                    CREATE DATA ldr_data_db_upd TYPE STANDARD TABLE OF (lv_ddic_db).
                    ASSIGN ldr_data_db_upd->* TO <lft_ydata_db_upd>.
                  ENDIF.
                  APPEND INITIAL LINE TO <lft_data_db_upd> ASSIGNING <ls_data_db>.
                  MOVE-CORRESPONDING <ls_data> TO <ls_data_db>.

                  " AEDAT, AEZET, AENAM
                  ASSIGN COMPONENT 'AEDAT' OF STRUCTURE <ls_data_db> TO <lv_any>.
                  IF sy-subrc = 0.
                    <lv_any> = sy-datum.
                  ENDIF.
                  ASSIGN COMPONENT 'AEZET' OF STRUCTURE <ls_data_db> TO <lv_any>.
                  IF sy-subrc = 0.
                    <lv_any> = sy-uzeit.
                  ENDIF.
                  ASSIGN COMPONENT 'AENAM' OF STRUCTURE <ls_data_db> TO <lv_any>.
                  IF sy-subrc = 0.
                    <lv_any> = sy-uname.
                  ENDIF.

                  APPEND INITIAL LINE TO <lft_ydata_db_upd> ASSIGNING FIELD-SYMBOL(<ls_ydata_db>).
                  MOVE-CORRESPONDING <ls_ydat> TO <ls_ydata_db>.
                ENDIF.
              ENDIF.
            WHEN gc_aktyp_delete.
              IF ldr_data_db_del IS NOT BOUND.
                CREATE DATA ldr_data_db_del TYPE STANDARD TABLE OF (lv_ddic_db).
                ASSIGN ldr_data_db_del->* TO <lft_data_db_del>.
              ENDIF.
              APPEND INITIAL LINE TO <lft_data_db_del> ASSIGNING <ls_data_db>.
              MOVE-CORRESPONDING <ls_data> TO <ls_data_db>.
          ENDCASE.

*          IF lv_kappl IS NOT INITIAL AND lv_objkey IS NOT INITIAL.
*            messaging_update( iv_synchron = iv_synchron
*                              iv_kappl    = lv_kappl
*                              iv_objkey   = lv_objkey ).
*          ENDIF.
        CATCH cx_root ##CATCH_ALL.
          DATA(lv_error) = abap_true.
      ENDTRY.
    ENDLOOP.

    IF lv_error = abap_true.
      RAISE EXCEPTION TYPE zcx_ca_exception.
    ENDIF.

    IF <lft_data_db_del> IS ASSIGNED.
      save_db( EXPORTING
                 iv_synchron = iv_synchron
                 iv_commit   = iv_commit
                 iv_action   = gc_aktyp_delete
                 it_data     = <lft_data_db_del>
               IMPORTING
                 ev_commit_done = lv_commit_done ).
    ENDIF.

    IF <lft_data_db_upd> IS ASSIGNED.
      save_db( EXPORTING
                 iv_synchron = iv_synchron
                 iv_commit   = iv_commit
                 iv_action   = gc_aktyp_update
                 it_data     = <lft_data_db_upd>
                 it_ydata    = <lft_ydata_db_upd>
               IMPORTING
                  ev_commit_done = lv_commit_done ).
    ENDIF.

    IF <lft_data_db_ins> IS ASSIGNED.
      save_db( EXPORTING
                 iv_synchron = iv_synchron
                 iv_commit   = iv_commit
                 iv_action   = gc_aktyp_insert
                 it_data     = <lft_data_db_ins>
               IMPORTING
                 ev_commit_done = lv_commit_done ).
    ENDIF.

    IF iv_commit IS NOT INITIAL.
      IF lv_commit_done = abap_false.
        IF iv_synchron IS INITIAL.
          COMMIT WORK.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

      IF lt_objects IS INITIAL.
        IF it_objid IS INITIAL.
          clean_up( ).
        ELSE.
          clean_up( it_objid = it_objid ).
        ENDIF.
      ELSE.
        LOOP AT lt_objects INTO DATA(lo_object) ##INTO_OK.
          lo_object->clean_up( ).
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->PREPARE_FOR_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION
* | [--->] IO_OBJECT                      TYPE REF TO ZCL_CA_SRVM_BO(optional)
* | [--->] IT_OBJECT                      TYPE        ZCA_T_SRVM_BO(optional)
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

    CHECK iv_action IS NOT INITIAL.

    IF io_object IS SUPPLIED.
      DATA(ldr_key) = io_object->get_d_key( ).
      ASSIGN ldr_key->* TO FIELD-SYMBOL(<ls_key>).
      get( EXPORTING
             iv_action = iv_action
             is_key    = <ls_key>
           IMPORTING
             eo_object = DATA(lo_object) ).
      IF lo_object->get_action( ) IS INITIAL.
        RAISE EXCEPTION TYPE zcx_ca_exception.
      ENDIF.
    ELSEIF it_object IS SUPPLIED.
      LOOP AT it_object INTO lo_object ##INTO_OK.
        ldr_key = lo_object->get_d_key( ).
        ASSIGN ldr_key->* TO <ls_key>.
        get( iv_action = iv_action
             is_key  = <ls_key> ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->MOVE_ID_TO_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID
* | [<-->] CS_DATA                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD move_id_to_data ##NEEDED.
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
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->MESSAGING_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SYNCHRON                    TYPE        CHAR1
* | [--->] IV_KAPPL                       TYPE        KAPPL
* | [--->] IV_OBJKEY                      TYPE        NA_OBJKEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD messaging_update ##NEEDED.
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
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->MESSAGING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION(optional)
* | [--->] IS_DATA                        TYPE        ANY
* | [<---] EV_KAPPL                       TYPE        KAPPL
* | [<---] EV_OBJKEY                      TYPE        NA_OBJKEY
* | [<---] EV_KALSM                       TYPE        KALSM_D
* | [<---] ET_MSGPA                       TYPE        MSGPA_TTY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD messaging ##NEEDED.
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
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->IS_VALID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION
* | [--->] IS_DATA                        TYPE        ANY
* | [!CX!] ZCX_CA_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_valid ##NEEDED.
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
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->GET_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_MESSAGE                     TYPE        BAL_T_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_messages.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    SORT gt_message.
    DELETE ADJACENT DUPLICATES FROM gt_message.
    rt_message = gt_message.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->GET_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        ANY
* | [<-()] RV_OBJID                       TYPE        ZCA_E_OBJID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_id ##NEEDED.
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
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->GET_DDIC_DB
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_DDIC_DB                     TYPE        TROBJ_NAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_ddic_db ##NEEDED.
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
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION(optional)
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID(optional)
* | [--->] IS_KEY                         TYPE        ANY(optional)
* | [--->] IT_RANGE                       TYPE        ZCA_T_RANGE(optional)
* | [--->] IV_READ_RANGE_OPTION           TYPE        ZCA_E_READ_RANGE_OPTION(optional)
* | [<---] ES_DATA                        TYPE        ANY
* | [<---] EO_OBJECT                      TYPE REF TO ZCL_CA_SRVM_BO
* | [<---] ET_DATA                        TYPE        STANDARD TABLE
* | [<---] ET_OBJECT                      TYPE        ZCA_T_SRVM_BO
* | [<---] ET_DDATA                       TYPE        ZCA_T_DDATA
* | [!CX!] ZCX_CA_EXCEPTION
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

    DATA:
      lv_objid   TYPE zca_e_objid,
      lv_read_db TYPE abap_boolean VALUE abap_false,
      ldr_data   TYPE REF TO data.

    FIELD-SYMBOLS <lft_data> TYPE ANY TABLE.

    FREE eo_object.
    CLEAR:
      es_data,
      et_data,
      et_object,
      et_ddata.

    IF iv_id IS INITIAL.
      IF is_key IS SUPPLIED.
        lv_objid = get_id( is_key ).
      ENDIF.
    ELSE.
      lv_objid = iv_id.
    ENDIF.
    IF lv_objid IS NOT INITIAL.
      READ TABLE mt_xdata ASSIGNING FIELD-SYMBOL(<ls_xdata>) WITH TABLE KEY objid = lv_objid.
      IF sy-subrc = 0.
        IF <ls_xdata>-action IS INITIAL.
          CASE iv_action.
            WHEN gc_aktyp_insert OR
                 gc_aktyp_update OR
                 gc_aktyp_delete.
              TRY.
                  enqueue( iv_action = iv_action
                           iv_id     = lv_objid ).
                  lv_read_db = abap_true. " force select
                CATCH cx_root ##CATCH_ALL.
                  CLEAR iv_action.
              ENDTRY.
            WHEN OTHERS.
              IF <ls_xdata>-fully_loaded = abap_false.
                lv_read_db = abap_true. " force select
              ENDIF.
          ENDCASE.
        ENDIF.
      ELSE.
        CASE iv_action.
          WHEN gc_aktyp_insert OR
               gc_aktyp_update OR
               gc_aktyp_delete.
            TRY.
                enqueue( iv_action = iv_action
                         iv_id     = lv_objid ).
              CATCH cx_root ##CATCH_ALL.
                CLEAR iv_action.
            ENDTRY.
        ENDCASE.
        lv_read_db = abap_true.
      ENDIF.

      IF is_key IS SUPPLIED AND lv_read_db = abap_true.
        CLEAR gt_message.

        DATA(lv_ddic_db) = get_ddic_db( ).
        CREATE DATA ldr_data TYPE (lv_ddic_db).
        ASSIGN ldr_data->* TO FIELD-SYMBOL(<ls_data>).
        IF iv_action = gc_aktyp_insert.
          <ls_data> = CORRESPONDING #( is_key ).
        ELSE.
          select_by_key(
            EXPORTING
              is_key  = is_key
            IMPORTING
              es_data = <ls_data> ).
        ENDIF.

        IF es_data IS REQUESTED.
          es_data = CORRESPONDING #( <ls_data> ).
        ENDIF.
        IF <ls_data> IS NOT INITIAL.
          IF et_ddata IS REQUESTED.
            APPEND LINES OF set_data( iv_action = iv_action
                                      iv_enqueue = abap_false  " enqueue has already been called
                                      is_data   = <ls_data> ) TO et_ddata.
          ELSE.
            set_data( iv_action  = iv_action
                      iv_enqueue = abap_false " enqueue has already been called before
                      is_data    = <ls_data> ).
          ENDIF.
          IF eo_object IS REQUESTED.
            READ TABLE mt_xdata ASSIGNING <ls_xdata> WITH TABLE KEY objid = lv_objid.
            IF sy-subrc = 0.
              eo_object = <ls_xdata>-object.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSEIF <ls_xdata> IS ASSIGNED.
        IF es_data IS REQUESTED.
          ASSIGN <ls_xdata>-data->* TO <ls_data>.
          es_data = CORRESPONDING #( <ls_data> ).
        ENDIF.
        IF eo_object IS REQUESTED.
          eo_object = <ls_xdata>-object.
        ENDIF.
      ENDIF.

    ELSEIF it_range IS SUPPLIED.
      CLEAR gt_message.

      lv_ddic_db = get_ddic_db( ).
      CREATE DATA ldr_data TYPE STANDARD TABLE OF (lv_ddic_db).
      ASSIGN ldr_data->* TO <lft_data>.
      select_by_range(
        EXPORTING
          it_range             = it_range
          iv_read_range_option = iv_read_range_option
        IMPORTING
          et_data              = <lft_data> ).

      IF et_data IS REQUESTED.
        et_data = CORRESPONDING #( <lft_data> ).
      ENDIF.
      IF <lft_data> IS NOT INITIAL.
        IF et_ddata IS REQUESTED.
          APPEND LINES OF set_data( iv_action = iv_action
                                    it_data   = <lft_data> ) TO et_ddata.
        ELSE.
          set_data( iv_action = iv_action
                    it_data   = <lft_data> ).
        ENDIF.
        IF et_object IS REQUESTED.
          LOOP AT <lft_data> ASSIGNING <ls_data>.
            READ TABLE mt_xdata ASSIGNING <ls_xdata> WITH TABLE KEY objid = get_id( <ls_data> ).
            IF sy-subrc = 0.
              INSERT <ls_xdata>-object INTO TABLE et_object.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->FREE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_OBJID                       TYPE        ZCA_T_OBJID(optional)
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

    DATA lt_r_objid TYPE RANGE OF zca_e_objid.

    IF it_objid IS NOT INITIAL.
      lt_r_objid = VALUE #( FOR lv_objid IN it_objid ( sign   = 'I'
                                                       option = 'EQ'
                                                       low    = lv_objid ) ).
    ENDIF.

    LOOP AT mt_xdata ASSIGNING FIELD-SYMBOL(<ls_xdata>) WHERE objid IN lt_r_objid. "#EC CI_SORTSEQ
      DATA(lv_tabix) = sy-tabix.
      IF <ls_xdata>-action IS NOT INITIAL.
        dequeue( <ls_xdata>-objid ).
      ENDIF.
      IF <ls_xdata>-object IS BOUND.
        <ls_xdata>-object->raise_event_freed( ).
        FREE <ls_xdata>-object.
      ENDIF.
      RAISE EVENT data_freed EXPORTING ed_data = <ls_xdata>-data.
      FREE <ls_xdata>-data.
      DELETE TABLE mt_ydata WITH TABLE KEY objid = <ls_xdata>-objid.
      DELETE mt_xdata INDEX lv_tabix.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->ENRICH_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        ANY(optional)
* | [<-->] CV_ID                          TYPE        ZCA_E_OBJID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD enrich_id ##NEEDED.
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
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->ENQUEUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID
* | [!CX!] ZCX_CA_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD enqueue ##NEEDED.
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
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->DEQUEUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dequeue ##NEEDED.
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
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->DATA_IS_DIRTY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD data_is_dirty ##NEEDED.
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
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->CREATE_OBJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_XDATA                       TYPE        ZCA_S_XDATA
* | [--->] IS_YDATA                       TYPE        ZCA_S_YDATA
* | [<-()] RO_OBJECT                      TYPE REF TO ZCL_CA_SRVM_BO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_object ##NEEDED.
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
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->CLEAN_UP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CANCEL                      TYPE        abap_boolean (default =ABAP_FALSE)
* | [--->] IT_OBJID                       TYPE        ZCA_T_OBJID(optional)
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

    DATA lt_r_objid TYPE RANGE OF zca_e_objid.

    IF it_objid IS NOT INITIAL.
      lt_r_objid = VALUE #( FOR lv_objid IN it_objid ( sign   = 'I'
                                                       option = 'EQ'
                                                       low    = lv_objid ) ).
    ENDIF.

    LOOP AT mt_xdata ASSIGNING FIELD-SYMBOL(<ls_xdata>) WHERE objid IN lt_r_objid AND
                                                               action IS NOT INITIAL. "#EC CI_SORTSEQ
      CASE <ls_xdata>-action.
        WHEN gc_aktyp_insert.
          dequeue( <ls_xdata>-objid ).
          IF iv_cancel = abap_true.
            free( it_objid = VALUE #( ( <ls_xdata>-objid ) ) ).
          ELSE.
            RAISE EVENT data_dirty EXPORTING ed_data = <ls_xdata>-data.
            data_is_dirty( <ls_xdata>-objid ).
            CLEAR <ls_xdata>-fully_loaded.
            CLEAR <ls_xdata>-action.
            IF <ls_xdata>-object IS BOUND.
              <ls_xdata>-object->raise_event_dirty( ).
            ENDIF.
          ENDIF.
        WHEN gc_aktyp_update.
          dequeue( <ls_xdata>-objid ).
          IF iv_cancel = abap_true.
            READ TABLE mt_ydata ASSIGNING FIELD-SYMBOL(<ls_ydata>) WITH TABLE KEY objid = <ls_xdata>-objid.
            IF sy-subrc = 0.
              ASSIGN <ls_xdata>-data->* TO FIELD-SYMBOL(<ls_xdat>).
              IF sy-subrc = 0.
                ASSIGN <ls_ydata>-data->* TO FIELD-SYMBOL(<ls_ydat>).
                IF sy-subrc = 0.
                  <ls_xdat> = <ls_ydat>.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            RAISE EVENT data_dirty EXPORTING ed_data = <ls_xdata>-data.
            data_is_dirty( <ls_xdata>-objid ).
            CLEAR <ls_xdata>-fully_loaded.
            IF <ls_xdata>-object IS BOUND.
              <ls_xdata>-object->raise_event_dirty( ).
            ENDIF.
          ENDIF.
          CLEAR <ls_xdata>-action.
          IF <ls_xdata>-object IS BOUND.
            <ls_xdata>-object->set_dirty( ).
            <ls_xdata>-object->related_object_chgd( iv_chgd = abap_false
                                                    io_object = <ls_xdata>-object ).
          ENDIF.
        WHEN gc_aktyp_delete.
          dequeue( <ls_xdata>-objid ).
          IF iv_cancel = abap_true.
            READ TABLE mt_ydata ASSIGNING <ls_ydata> WITH TABLE KEY objid = <ls_xdata>-objid.
            IF sy-subrc = 0.
              ASSIGN <ls_xdata>-data->* TO <ls_xdat>.
              IF sy-subrc = 0.
                ASSIGN <ls_ydata>-data->* TO <ls_ydat>.
                IF sy-subrc = 0.
                  <ls_xdat> = <ls_ydat>.
                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR <ls_xdata>-action.
          ELSE.
            free( it_objid = VALUE #( ( <ls_xdata>-objid ) ) ).
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CA_SERVICE_MANAGER=>ADD_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MESSAGE                     TYPE        BAL_S_MSG(optional)
* | [--->] IS_BAPIRETURN1                 TYPE        BAPIRETURN1(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_message.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

    IF is_message IS NOT INITIAL.
      APPEND is_message TO gt_message.
    ELSEIF is_bapireturn1 IS NOT INITIAL.
      APPEND VALUE #(  msgty = is_bapireturn1-type
                       msgid = is_bapireturn1-id
                       msgno = is_bapireturn1-number
                       msgv1 = is_bapireturn1-message_v1
                       msgv2 = is_bapireturn1-message_v2
                       msgv3 = is_bapireturn1-message_v3
                       msgv4 = is_bapireturn1-message_v4 ) TO gt_message.
    ELSE.
      APPEND VALUE #( msgty = sy-msgty
                      msgid = sy-msgid
                      msgno = sy-msgno
                      msgv1 = sy-msgv1
                      msgv2 = sy-msgv2
                      msgv3 = sy-msgv3
                      msgv4 = sy-msgv4 ) TO gt_message.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CA_SERVICE_MANAGER->SET_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ID                          TYPE        ZCA_E_OBJID
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

    READ TABLE mt_xdata ASSIGNING FIELD-SYMBOL(<ls_xdata>) WITH TABLE KEY objid = iv_id.
    IF sy-subrc = 0.
      <ls_xdata>-action = iv_action.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_CA_SERVICE_MANAGER->ENRICH_DATA_BEFORE_SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION                      TYPE        ZCA_E_ACTION
* | [--->] ID_DATA                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD enrich_data_before_save ##NEEDED.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2022 initial creation
*----------------------------------------------------------------------*

  ENDMETHOD.
ENDCLASS.
