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
      !IV_DREF type STRING default MC_DREF-DATA_VALUE .
  methods ADD_TO_HASH
    importing
      !IV_INPUT type ANY
    returning
      value(RO_CRC64) type ref to ZCL_EUI_CRC64 .
  methods GET_HASH
    returning
      value(RV_HASH) type CHAR16 .
protected section.
private section.

  data MV_CRC64 type SCI_CRC64 .
  data MV_DREF type STRING .
ENDCLASS.



CLASS ZCL_EUI_CRC64 IMPLEMENTATION.


METHOD add_to_hash.
  " For chain calls
  ro_crc64 = me.

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
        add_to_hash( lo_type->type_kind ).
        add_to_hash( lo_type->absolute_name ).
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

      ASSIGN <lv_input> TO <lt_table>.
      LOOP AT <lt_table> ASSIGNING <lv_value>.
        add_to_hash( <lv_value> ).
      ENDLOOP.
      RETURN.

    WHEN lo_type->typekind_struct1 OR lo_type->typekind_struct2.
      DATA lo_struc TYPE REF TO cl_abap_structdescr.
      FIELD-SYMBOLS <ls_comp> LIKE LINE OF lo_struc->components.
      lo_struc ?= lo_type.
      LOOP AT lo_struc->components ASSIGNING <ls_comp>.
        ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE <lv_input> TO <lv_value>.
        add_to_hash( <lv_value> ).
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

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( iv_message = `parameter error` ).
ENDMETHOD.


METHOD constructor.
  mv_dref = iv_dref.
ENDMETHOD.


METHOD get_hash.
  FIELD-SYMBOLS <lv_raw> TYPE x.
  ASSIGN mv_crc64 TO <lv_raw> CASTING.
  WRITE <lv_raw> TO rv_hash.
ENDMETHOD.
ENDCLASS.
