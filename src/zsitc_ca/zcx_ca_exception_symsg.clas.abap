class ZCX_CA_EXCEPTION_SYMSG definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  data MSGID type SYMSGID read-only .
  data MSGNO type SYMSGNO read-only .
  data MSGTY type SYMSGTY read-only .
  data MSGV1 type SYMSGV read-only .
  data MSGV2 type SYMSGV read-only .
  data MSGV3 type SYMSGV read-only .
  data MSGV4 type SYMSGV read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGID type SYMSGID optional
      !MSGNO type SYMSGNO optional
      !MSGTY type SYMSGTY optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
  methods GET_MESSAGE
    exporting
      !EV_MSGID type SYMSGID
      !EV_MSGTY type SYMSGTY
      !EV_MSGNO type SYMSGNO
      !EV_MSGV1 type SYMSGV
      !EV_MSGV2 type SYMSGV
      !EV_MSGV3 type SYMSGV
      !EV_MSGV4 type SYMSGV .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_EXCEPTION_SYMSG IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGID = MSGID .
me->MSGNO = MSGNO .
me->MSGTY = MSGTY .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD get_message.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2023 initial creation
*----------------------------------------------------------------------*

    IF msgty IS INITIAL.
      ev_msgty = if_t100_dyn_msg~msgty.
      ev_msgty = if_t100_message~t100key-msgid.
      ev_msgno = if_t100_message~t100key-msgno.
      ev_msgv1 = if_t100_dyn_msg~msgv1.
      ev_msgv2 = if_t100_dyn_msg~msgv2.
      ev_msgv3 = if_t100_dyn_msg~msgv3.
      ev_msgv4 = if_t100_dyn_msg~msgv4.
    ELSE.
      ev_msgty = msgty.
      ev_msgid = msgid.
      ev_msgno = msgno.
      ev_msgv1 = msgv1.
      ev_msgv2 = msgv2.
      ev_msgv3 = msgv3.
      ev_msgv4 = msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD if_message~get_longtext.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2023 initial creation
*----------------------------------------------------------------------*

    IF msgty IS INITIAL.
      result = super->if_message~get_longtext( ).
    ELSE.
      MESSAGE ID msgid TYPE msgty NUMBER msgno
                  WITH msgv1 msgv2 msgv3 msgv4 INTO result.
    ENDIF.

  ENDMETHOD.


  METHOD if_message~get_text.
*----------------------------------------------------------------------*
* Purpose:
*----------------------------------------------------------------------*
* Changes:                                                             *
* Task/Def    Author          Date       Changes                       *
* ----------  --------------  --------   ------------------------------*
*             Scheu           09/02/2023 initial creation
*----------------------------------------------------------------------*

    IF msgty IS INITIAL.
      result = super->if_message~get_text( ).
    ELSE.
      MESSAGE ID msgid TYPE msgty NUMBER msgno
                  WITH msgv1 msgv2 msgv3 msgv4 INTO result.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
