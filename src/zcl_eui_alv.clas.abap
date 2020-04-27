class ZCL_EUI_ALV definition
  public
  inheriting from ZCL_EUI_MANAGER
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IR_TABLE type ref to DATA
      !IT_TOOLBAR type TTB_BUTTON optional
      !IT_MOD_CATALOG type LVC_T_FCAT optional
      !IS_LAYOUT type LVC_S_LAYO optional
      !IS_VARIANT type DISVARIANT optional
      !IT_FILTER type LVC_T_FILT optional
      !IT_SORT type LVC_T_SORT optional
      !IV_STATUS_NAME type GUI_STATUS optional
      !IV_STATUS_PROG type SYREPID optional
      !IT_STATUS_EXCLUDE type ZIF_EUI_MANAGER=>TT_STATUS_EXCLUDE optional
      !IV_STATUS_TITLE type CSEQUENCE optional
      !IV_READ_ONLY type ABAP_BOOL default ABAP_TRUE
      !IS_FIELD_DESC type ref to ZCL_EUI_TYPE=>TS_FIELD_DESC optional .
  methods GET_GRID
    returning
      value(RO_GRID) type ref to CL_GUI_ALV_GRID .
  class-methods UPDATE_COMPLEX_FIELDS
    importing
      !IR_TABLE type ref to DATA
      !IT_SUB_FIELD type ZCL_EUI_TYPE=>TT_FIELD_DESC optional
      !IS_SUB_FIELD type ZCL_EUI_TYPE=>TS_FIELD_DESC optional .

  methods ZIF_EUI_MANAGER~PAI
    redefinition .
  methods ZIF_EUI_MANAGER~PBO
    redefinition .
protected section.
private section.

  data MO_GRID type ref to CL_GUI_ALV_GRID .
  data MR_TABLE type ref to DATA .
  data MT_TOOLBAR type TTB_BUTTON .
  data MT_MOD_CATALOG type LVC_T_FCAT .
  data MS_LAYOUT type LVC_S_LAYO .
  data MS_VARIANT type DISVARIANT .
  data MT_FILTER type LVC_T_FILT .
  data MT_SORT type LVC_T_SORT .
  data MO_HELPER type ref to LCL_HELPER .
ENDCLASS.



CLASS ZCL_EUI_ALV IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   iv_status_name    = iv_status_name
   iv_status_prog    = iv_status_prog
   it_status_exclude = it_status_exclude
   iv_status_title   = iv_status_title
   iv_read_only      = iv_read_only ).

  mr_table             = ir_table.
  mt_toolbar           = it_toolbar.
  mt_mod_catalog       = it_mod_catalog.
  ms_layout            = is_layout.
  ms_variant           = is_variant.
  mt_filter            = it_filter.
  mt_sort              = it_sort.

  CREATE OBJECT mo_helper
    EXPORTING
      io_eui_alv    = me
      is_field_desc = is_field_desc.
ENDMETHOD.


METHOD get_grid.
  ro_grid = mo_grid.
ENDMETHOD.


METHOD update_complex_fields.
  DATA:
    lt_sub_field LIKE it_sub_field,
    ls_sub_field TYPE REF TO zcl_eui_type=>ts_field_desc,
    lv_fld_name  TYPE string,
    lv_beg_txt   TYPE string,
    lv_end_txt   TYPE string,
    BEGIN OF ls_range,
      sign   TYPE char1,
      option TYPE char2,
      low    TYPE text255,
      high   TYPE text255,
    END OF ls_range.
  FIELD-SYMBOLS:
    <lt_table_dest> TYPE STANDARD TABLE,
    <ls_dest>       TYPE any,
    <lv_ui_ext>     TYPE string,
    <lt_sub_table>  TYPE ANY TABLE,
    <ls_sub_src>    TYPE any.

  " Create standard table for alv editing
  ASSIGN ir_table->* TO <lt_table_dest>.

  " Show info about sub field
  LOOP AT it_sub_field REFERENCE INTO ls_sub_field
       WHERE ui_type = zcl_eui_type=>mc_ui_type-table
          OR ui_type = zcl_eui_type=>mc_ui_type-range.  "#EC CI_HASHSEQ
    INSERT ls_sub_field->* INTO TABLE lt_sub_field.
  ENDLOOP.

  IF is_sub_field IS NOT INITIAL.
    INSERT is_sub_field INTO TABLE lt_sub_field.
  ENDIF.

  " No need
  CHECK lt_sub_field IS NOT INITIAL.

  " Update each row
  LOOP AT <lt_table_dest> ASSIGNING <ls_dest>.
    LOOP AT lt_sub_field REFERENCE INTO ls_sub_field.
      " Source table
      ASSIGN COMPONENT ls_sub_field->name OF STRUCTURE <ls_dest> TO <lt_sub_table>.

      " Destination string
      CONCATENATE ls_sub_field->name `_UI` INTO lv_fld_name.
      ASSIGN COMPONENT lv_fld_name OF STRUCTURE <ls_dest> TO <lv_ui_ext>.

      " No text
      CLEAR <lv_ui_ext>.

      CASE ls_sub_field->ui_type.
          " Show count
        WHEN zcl_eui_type=>mc_ui_type-table.
          IF <lt_sub_table>[] IS NOT INITIAL.
            <lv_ui_ext> = lines( <lt_sub_table> ).
            CONCATENATE 'Count'(cnt) <lv_ui_ext> INTO <lv_ui_ext> SEPARATED BY space.
          ENDIF.

          " Show all values in range
        WHEN zcl_eui_type=>mc_ui_type-range.
          LOOP AT <lt_sub_table> ASSIGNING <ls_sub_src>.
            MOVE-CORRESPONDING <ls_sub_src> TO ls_range.

            " Do not show SIGN
            IF ls_range-sign = 'I'.
              CLEAR lv_beg_txt.
            ELSE.
              CONCATENATE ls_range-sign `:` INTO lv_beg_txt.
            ENDIF.

            " Do not show HIGH
            IF ls_range-high IS INITIAL.
              CLEAR lv_end_txt.
            ELSE.
              CONCATENATE `:` ls_range-high INTO lv_end_txt.
            ENDIF.

            CONCATENATE <lv_ui_ext> ` ` lv_beg_txt ls_range-option `:` ls_range-low lv_end_txt INTO <lv_ui_ext>.
          ENDLOOP.

          " Delete first ` `
          IF sy-subrc = 0.
            <lv_ui_ext> = <lv_ui_ext>+1.
          ENDIF.

      ENDCASE.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD zif_eui_manager~pai.
  " Write data back
  mo_grid->check_changed_data( ).

  super->pai(
   EXPORTING
    iv_command = iv_command
   CHANGING
    cv_close   = cv_close ).

  CHECK cv_close = abap_true.
  mo_helper->after_close(
   EXPORTING
     iv_close_cmd = mv_close_cmd
   CHANGING
     cv_close     = cv_close ).
ENDMETHOD.


METHOD zif_eui_manager~pbo.
  " Initialize 1 time
  IF io_container IS NOT INITIAL.
    mo_helper->pbo_init(
     io_container = io_container ).
  ENDIF.

  super->pbo(
   io_container  = io_container
   iv_set_status = iv_set_status  ).
ENDMETHOD.
ENDCLASS.
