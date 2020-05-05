class ZCX_EUI_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.
*"* public components of class ZCX_EUI_EXCEPTION
*"* do not include other source files here!!!
  type-pools ABAP .

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_EUI_EXCEPTION,
      msgid type symsgid value 'ZEUI_MESSAGE',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_EUI_EXCEPTION .
  data MSGV1 type MSGV1 .
  data MSGV2 type MSGV2 .
  data MSGV3 type MSGV3 .
  data MSGV4 type MSGV4 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type MSGV1 optional
      !MSGV2 type MSGV2 optional
      !MSGV3 type MSGV3 optional
      !MSGV4 type MSGV4 optional .
  class-methods RAISE_SYS_ERROR
    importing
      !IV_MESSAGE type CSEQUENCE optional
      value(IO_ERROR) type ref to CX_ROOT optional
    raising
      ZCX_EUI_EXCEPTION .
  class-methods RAISE_DUMP
    importing
      !IV_MESSAGE type CSEQUENCE optional
      !IO_ERROR type ref to CX_ROOT optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_EUI_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_EUI_EXCEPTION .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


METHOD raise_dump.
  DATA:
    lv_message TYPE string.

  IF io_error IS SUPPLIED.
    lv_message = io_error->get_text( ).
  ELSE.
    lv_message = iv_message.
  ENDIF.

  MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
  MESSAGE lv_message TYPE 'X'.
ENDMETHOD.


METHOD raise_sys_error.
  DATA:
    BEGIN OF ls_string,
      part1 TYPE symsgv,
      part2 TYPE symsgv,
      part3 TYPE symsgv,
      part4 TYPE symsgv,
    END OF ls_string.
  DATA lv_incl TYPE syrepid.
  DATA lv_line TYPE i.
  DATA lv_text TYPE string.

  " From string
  IF iv_message IS NOT INITIAL.
    ls_string = iv_message.
  ENDIF.

  WHILE ls_string IS INITIAL AND io_error IS NOT INITIAL.
    ls_string = io_error->get_text( ).

    " For debug
    io_error->get_source_position(
     IMPORTING
       include_name = lv_incl
       source_line  = lv_line ).
    " put break-point here ---> { lv_incl } - { lv_line }

    io_error = io_error->previous.
  ENDWHILE.

  " Any error based on system message
  IF ls_string IS INITIAL.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_string.
  ENDIF.

  " Devided to blocks
  RAISE EXCEPTION TYPE zcx_eui_exception
    EXPORTING
      textid = zcx_eui_exception=>zcx_eui_exception
      msgv1  = ls_string-part1
      msgv2  = ls_string-part2
      msgv3  = ls_string-part3
      msgv4  = ls_string-part4.
ENDMETHOD.
ENDCLASS.
