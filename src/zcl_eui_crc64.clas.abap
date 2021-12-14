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
protected section.
private section.

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

  methods _SHOW_LOG .
ENDCLASS.



CLASS ZCL_EUI_CRC64 IMPLEMENTATION.


METHOD add_to_hash.
  " For chain calls
  ro_crc64 = me.

  DATA lv_name TYPE string.
  DEFINE add_2_name.
    IF mv_log = abap_true.
      CONCATENATE iv_name `-` &1 INTO lv_name.
    ENDIF.
  END-OF-DEFINITION.
**********************************************************************
**********************************************************************

  FIELD-SYMBOLS <lv_input> TYPE any.
  ASSIGN iv_input TO <lv_input>.

  DATA lv_is_dref TYPE abap_bool VALUE abap_false.
  DO.
    DATA lo_type TYPE REF TO cl_abap_typedescr.
    lo_type = cl_abap_typedescr=>describe_by_data( <lv_input> ).

    " DATA ref
    IF lo_type->type_kind = lo_type->typekind_dref.
      lv_is_dref = abap_true.
      DATA lv_ref TYPE REF TO data.
      lv_ref = <lv_input>.
      ASSIGN lv_ref->* TO <lv_input>.
      CONTINUE.
    ENDIF.

    " Ordinary data
    IF lv_is_dref <> abap_true.
      EXIT.
    ENDIF.


    CASE mv_dref.
      WHEN mc_dref-no_info.
        RETURN.

      WHEN mc_dref-type_info.
        add_2_name 'TYPE_KIND'.
        add_to_hash( iv_input = lo_type->type_kind
                     iv_name  = lv_name ).
        add_2_name 'ABSOLUTE_NAME'.
        add_to_hash( iv_input = lo_type->absolute_name
                     iv_name  = lv_name ).
        RETURN.

      WHEN mc_dref-data_value.
      WHEN OTHERS.
    ENDCASE.

    EXIT.
  ENDDO.

  CASE lo_type->type_kind.
    WHEN lo_type->typekind_table.
      FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
      FIELD-SYMBOLS <lv_value> TYPE any.
      DATA lv_index TYPE string.

      ASSIGN <lv_input> TO <lt_table>.
      LOOP AT <lt_table> ASSIGNING <lv_value>.
        IF mv_log = abap_true.
          lv_index = sy-tabix.
          CONDENSE lv_index.
          CONCATENATE `[` lv_index `]` INTO lv_index.
          add_2_name lv_index.
        ENDIF.

        add_to_hash( iv_input = <lv_value>
                     iv_name  = lv_name ).
      ENDLOOP.
      RETURN.

    WHEN lo_type->typekind_struct1 OR lo_type->typekind_struct2.
      DATA lo_struc TYPE REF TO cl_abap_structdescr.
      FIELD-SYMBOLS <ls_comp> LIKE LINE OF lo_struc->components.
      lo_struc ?= lo_type.
      LOOP AT lo_struc->components ASSIGNING <ls_comp>.
        ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <lv_input> TO <lv_value>.
        add_2_name <ls_comp>-name.
        add_to_hash( iv_input = <lv_value>
                     iv_name  = lv_name ).
      ENDLOOP.
      RETURN.

    WHEN lo_type->typekind_string.
      DATA lv_input TYPE char40.
      lv_input = zcl_eui_prog=>calc_string_hash( iv_string = <lv_input> ).

    WHEN lo_type->typekind_xstring.
      lv_input = zcl_eui_prog=>calc_xstring_hash( iv_xstring = <lv_input> ).

    WHEN OTHERS.
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
    zcx_eui_no_check=>raise_sys_error( iv_message = `parameter error` ).
  ENDIF.

  CHECK mv_log = abap_true.
  DATA ls_log LIKE LINE OF mt_log.
  MOVE-CORRESPONDING mv_crc64 TO ls_log.
  ls_log-name = iv_name.
  APPEND ls_log TO mt_log.
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


METHOD _show_log.
  CHECK mv_log = abap_true.

  DATA lr_table TYPE REF TO data.
  GET REFERENCE OF mt_log INTO lr_table.

  DATA lo_alv TYPE REF TO zcl_eui_alv.
  CREATE OBJECT lo_alv
    EXPORTING
      ir_table = lr_table.

  lo_alv->popup( ).
  lo_alv->show( ).
ENDMETHOD.
ENDCLASS.
