class ZCL_EUI_CRC64 definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  constants:
    BEGIN OF mc_dref,
      data_value TYPE STRING VALUE 'DATA_VALUE',
      type_info  TYPE STRING VALUE 'TYPE_INFO',
      no_info    TYPE STRING VALUE 'NO_INFO',
    END OF mc_dref .

  methods CONSTRUCTOR
    importing
      !IV_DREF type STRING default MC_DREF-DATA_VALUE
      !IV_LOG type ABAP_BOOL optional .
  methods ADD_TO_HASH
    importing
      !IV_INPUT type ANY
      !IV_NAME type CSEQUENCE default 'R'
    returning
      value(RO_CRC64) type ref to ZCL_EUI_CRC64 .
  methods GET_HASH
    returning
      value(RV_HASH) type CHAR16 .
  class-methods CALC_STRING_HASH
    importing
      !IV_STRING type STRING optional
      !IT_TABLE type STRINGTAB optional
    returning
      value(RV_HASH) type HASH160 .
  class-methods CALC_XSTRING_HASH
    importing
      !IV_XSTRING type XSTRING
    returning
      value(RV_HASH) type HASH160 .
protected section.
private section.
*"* private components of class ZCL_EUI_CRC64
*"* do not include other source files here!!!

  types:
    BEGIN OF ts_LOG_info,
      name TYPE string.
      INCLUDE TYPE sci_crc64.
    TYPES:
    END OF ts_LOG_info .
  types:
    tt_LOG_info TYPE STANDARD TABLE OF ts_LOG_info WITH DEFAULT KEY .

  data MV_CRC64 type SCI_CRC64 .
  data MV_DREF type STRING .
  data MV_LOG type ABAP_BOOL .
  data MT_LOG type TT_LOG_INFO .

  methods _GET_SUB_NAME
    importing
      !IV_NAME type CSEQUENCE
      !IV_SUFFIX type CSEQUENCE
      !IV_DASH type STRING default '-'
    returning
      value(RV_SUB_NAME) type STRING .
  methods _ADD_TYPE_INFO
    importing
      !IO_TYPE type ref to CL_ABAP_TYPEDESCR
      !IV_NAME type CSEQUENCE .
  methods _ADD_TABLE
    importing
      !IV_INPUT type ANY
      !IV_NAME type CSEQUENCE .
  methods _ADD_STRUCTURE
    importing
      !IO_TYPE type ref to CL_ABAP_TYPEDESCR
      !IV_INPUT type ANY
      !IV_NAME type CSEQUENCE .
  methods _ADD_OBJECT
    importing
      !IV_INPUT type ANY
      !IV_NAME type CSEQUENCE .
  methods _SHOW_LOG .
ENDCLASS.



CLASS ZCL_EUI_CRC64 IMPLEMENTATION.


METHOD add_to_hash.
  " For chain calls
  ro_crc64 = me.
**********************************************************************
**********************************************************************

  DATA: lo_type TYPE REF TO cl_abap_typedescr,
        lv_dref TYPE REF TO data.

  FIELD-SYMBOLS <lv_input> TYPE any.
  ASSIGN iv_input TO <lv_input>.

  DO.
    lo_type = cl_abap_typedescr=>describe_by_data( <lv_input> ).

    " 1 - DATA ref
    IF lo_type->type_kind = cl_abap_typedescr=>typekind_dref.
      lv_dref = <lv_input>.
      ASSIGN lv_dref->* TO <lv_input>.
      CONTINUE.
    ENDIF.

    EXIT.
  ENDDO.

**********************************************************************
**********************************************************************

  " 2 - DATA ref
  IF lv_dref IS NOT INITIAL AND mv_dref <> mc_dref-data_value.
    " Just same type?
    IF mv_dref = mc_dref-type_info.
      _add_type_info( io_type = lo_type
                      iv_name = iv_name ).
    ENDIF.
    " or WHEN mc_dref-no_info
    RETURN.
  ENDIF.

  CASE lo_type->type_kind.
    WHEN cl_abap_typedescr=>typekind_table.
      _add_table( iv_input = <lv_input>
                  iv_name  = iv_name ).
      RETURN.

    WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.
      _add_structure( io_type  = lo_type
                      iv_input = <lv_input>
                      iv_name  = iv_name ).
      RETURN.

    WHEN cl_abap_typedescr=>typekind_oref OR cl_abap_typedescr=>typekind_intf.
      _add_object( iv_input = <lv_input>
                   iv_name  = iv_name ).
      RETURN.

    WHEN cl_abap_typedescr=>typekind_string.
      DATA lv_input TYPE char40.
      lv_input = calc_string_hash( iv_string = <lv_input> ).

    WHEN cl_abap_typedescr=>typekind_xstring.
      lv_input = calc_xstring_hash( iv_xstring = <lv_input> ).
  ENDCASE.

  IF lv_input IS NOT INITIAL.
    ASSIGN lv_input TO <lv_input>.
  ENDIF.

  DATA l_length TYPE i.
  DESCRIBE FIELD <lv_input> LENGTH l_length IN BYTE MODE.
  CALL 'ABAP_CRC64'
       ID 'INPUT'  FIELD <lv_input>
       ID 'LENGTH' FIELD l_length
       ID 'CRC1'   FIELD mv_crc64-i1
       ID 'CRC2'   FIELD mv_crc64-i2.                     "#EC CI_CCALL

  IF sy-subrc <> 0.
    DATA lv_message TYPE string.
    CONCATENATE iv_name 'parameter error'(par) INTO lv_message SEPARATED BY space.
    zcx_eui_no_check=>raise_sys_error( iv_message = lv_message ).
  ENDIF.

  CHECK mv_log = abap_true.
  DATA ls_log LIKE LINE OF mt_log.
  MOVE-CORRESPONDING mv_crc64 TO ls_log.
  ls_log-name = iv_name.
  APPEND ls_log TO mt_log.
ENDMETHOD.


METHOD calc_string_hash.
  DATA lv_line TYPE string.

  IF iv_string IS SUPPLIED.
    lv_line = iv_string.
  ELSEIF it_table IS SUPPLIED.
    CONCATENATE LINES OF it_table INTO lv_line.
  ELSE.
    zcx_eui_no_check=>raise_sys_error( iv_message = 'Wrong parameters of CALC_STRING_HASH' ).
  ENDIF.

  CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
    EXPORTING
      data   = lv_line
    IMPORTING
      hash   = rv_hash
    EXCEPTIONS
      OTHERS = 1.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD calc_xstring_hash.
  CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
    EXPORTING
      data   = iv_xstring
    IMPORTING
      hash   = rv_hash
    EXCEPTIONS
      OTHERS = 1.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD constructor.
  mv_dref = iv_dref.
  mv_log  = iv_log.
ENDMETHOD.


METHOD get_hash.
  _show_log( ).

  FIELD-SYMBOLS <lv_raw> TYPE x.
  ASSIGN mv_crc64 TO <lv_raw> CASTING.
  WRITE <lv_raw> TO rv_hash.
ENDMETHOD.


METHOD _add_object.
  DATA lv_name   TYPE string.
  DATA lo_object TYPE REF TO cl_abap_objectdescr.
  FIELD-SYMBOLS <ls_attr>  LIKE LINE OF lo_object->attributes.
  FIELD-SYMBOLS <lv_value> TYPE any.

  lo_object ?= cl_abap_objectdescr=>describe_by_object_ref( iv_input ).
  LOOP AT lo_object->attributes ASSIGNING <ls_attr>.
    CONCATENATE 'IV_INPUT->' <ls_attr>-name INTO lv_name.

    " Add ZCL_EUI_CRC64 as a friend to access private attributes
    ASSIGN (lv_name) TO <lv_value>.
    CHECK sy-subrc = 0.

    lv_name = _get_sub_name( iv_name   = iv_name
                             iv_dash   = `->`
                             iv_suffix = <ls_attr>-name ).
    add_to_hash( iv_input = <lv_value>
                 iv_name  = lv_name ).
  ENDLOOP.
ENDMETHOD.


METHOD _add_structure.
  DATA lo_struc TYPE REF TO cl_abap_structdescr.
  DATA lv_name  TYPE string.
  FIELD-SYMBOLS <ls_comp> LIKE LINE OF lo_struc->components.
  FIELD-SYMBOLS <lv_value> TYPE any.

  lo_struc ?= io_type.
  LOOP AT lo_struc->components ASSIGNING <ls_comp>.
    ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE iv_input TO <lv_value>.
    lv_name = _get_sub_name( iv_name   = iv_name
                             iv_suffix = <ls_comp>-name ).

    add_to_hash( iv_input = <lv_value>
                 iv_name  = lv_name ).
  ENDLOOP.
ENDMETHOD.


METHOD _add_table.
  FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
  FIELD-SYMBOLS <lv_value> TYPE any.
  DATA lv_tabix TYPE sytabix VALUE 0.
  DATA lv_index TYPE string.
  DATA lv_name  TYPE string.

  ASSIGN iv_input TO <lt_table>.
  LOOP AT <lt_table> ASSIGNING <lv_value>.
    IF mv_log = abap_true.
      " for hashed tables cannot use sy-tabix
      lv_tabix = lv_tabix + 1.
      lv_index = lv_tabix.
      CONDENSE lv_index.
      CONCATENATE `[` lv_index `]` INTO lv_index.
      lv_name = _get_sub_name( iv_name   = iv_name
                               iv_dash   = ``
                               iv_suffix = lv_index ).
    ENDIF.

    add_to_hash( iv_input = <lv_value>
                 iv_name  = lv_name ).
  ENDLOOP.
ENDMETHOD.


METHOD _add_type_info.
  DATA lv_name TYPE string.
  lv_name = _get_sub_name( iv_name   = iv_name
                           iv_suffix = 'TYPE_KIND' ).
  add_to_hash( iv_input = io_type->type_kind
               iv_name  = lv_name ).

  lv_name = _get_sub_name( iv_name   = iv_name
                           iv_suffix = 'ABSOLUTE_NAME' ).
  add_to_hash( iv_input = io_type->absolute_name
               iv_name  = lv_name ).
ENDMETHOD.


METHOD _get_sub_name.
  IF mv_log = abap_true.
    CONCATENATE iv_name iv_dash iv_suffix INTO rv_sub_name.
  ENDIF.
ENDMETHOD.


METHOD _show_log.
  CHECK mv_log = abap_true.

  DATA lr_table TYPE REF TO data.
  GET REFERENCE OF mt_log INTO lr_table.

  DATA lt_catalog TYPE lvc_t_fcat.
  DATA lr_catalog TYPE REF TO lvc_s_fcat.
  APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
  lr_catalog->fieldname = 'NAME'.
  lr_catalog->coltext   = `Name`.

  APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
  lr_catalog->fieldname = 'I1'.
  lr_catalog->coltext   = `Part 1`.

  APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
  lr_catalog->fieldname = 'I2'.
  lr_catalog->coltext   = `Part 2`.

  DATA lo_alv TYPE REF TO zcl_eui_alv.
  CREATE OBJECT lo_alv
    EXPORTING
      ir_table       = lr_table
      it_mod_catalog = lt_catalog.

  lo_alv->popup( iv_row_end = 25 ).                      "#EC NUMBER_OK
  lo_alv->show( ).
ENDMETHOD.
ENDCLASS.
