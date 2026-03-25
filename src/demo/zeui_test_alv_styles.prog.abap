*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zeui_test_alv_styles.

TYPE-POOLS:
 abap.

SELECTION-SCREEN BEGIN OF BLOCK bl_grp WITH FRAME.
  PARAMETERS:
    p_merge AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK bl_grp.

**********************************************************************
**********************************************************************
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_demo,
        field01(20),
        field02(20),
        field03(20),
        field04(20),
        field05(20),
        field06(20),
        field07(20),
        field08(20),
        field09(20),
        field10(20),
        field11(20),
        field12(20),
      END OF ts_demo,
      tt_demo TYPE STANDARD TABLE OF ts_demo WITH DEFAULT KEY.

    DATA:
      mt_alv         TYPE tt_demo.

    METHODS:
      start_of_selection,
      _get_demo_data     RETURNING VALUE(rt_demo) TYPE tt_demo,
      _get_field_catalog RETURNING VALUE(rt_fieldcatalog) TYPE lvc_t_fcat,
      _on_change_styles  FOR EVENT change_styles OF zcl_eui_alv IMPORTING sender.
ENDCLASS.


**********************************************************************
**********************************************************************

CLASS lcl_report IMPLEMENTATION.
  METHOD _get_demo_data.
    DATA ls_demo TYPE ts_demo.

    CLEAR ls_demo.
    ls_demo-field01 = 'TRICKTRESOR'.
    ls_demo-field02 = 'TRICKTRESOR'.
    ls_demo-field03 = 'F'.
    ls_demo-field04 = 'P'.
    ls_demo-field05 = 'P'.
    ls_demo-field06 = 'P'.
    ls_demo-field07 = 'P'.
    ls_demo-field08 = 'P'.
    ls_demo-field09 = 'M'.
    ls_demo-field10 = 'K'.
    ls_demo-field11 = 'K'.
    ls_demo-field12 = 'K'.
    APPEND ls_demo TO rt_demo.

    CLEAR ls_demo.
    ls_demo-field01 = 'TRICKTRESOR'.
    ls_demo-field02 = 'TRICKTRESOR'.
    ls_demo-field03 = 'HQ'.
    ls_demo-field04 = 'HC'.
    ls_demo-field08 = 'HW'.
    ls_demo-field09 = 'HC'.
    ls_demo-field10 = 'HC'.
    ls_demo-field12 = 'HW'.
    APPEND ls_demo TO rt_demo.

    CLEAR ls_demo.
    ls_demo-field01 = 'Bezeichnung'.
    ls_demo-field02 = 'Radius'.
    ls_demo-field03 = 'WPX 12'.
    ls_demo-field04 = 'WAP 25'.
    ls_demo-field05 = 'WAP 35'.
    ls_demo-field06 = 'WTP 35'.
    ls_demo-field07 = 'WXP 45'.
    ls_demo-field08 = 'WPM'.
    ls_demo-field09 = 'WXM 35'.
    ls_demo-field10 = 'WAK 15'.
    ls_demo-field11 = 'WAK 25'.
    ls_demo-field12 = 'WKM'.
    APPEND ls_demo TO rt_demo.

    CLEAR ls_demo.
    ls_demo-field01 = 'SPMW 060304 T - A 27'.
    ls_demo-field02 = '0.54'.
    ls_demo-field03 = icon_led_green.
    ls_demo-field04 = icon_led_yellow.
    ls_demo-field05 = icon_led_red.
    ls_demo-field08 = icon_led_yellow.
    APPEND ls_demo TO rt_demo.

    CLEAR ls_demo.
    ls_demo-field01 = 'SPMW 060304 - A 57'.
    ls_demo-field02 = '0.43'.
    ls_demo-field03 = icon_led_yellow.
    ls_demo-field05 = icon_led_red.
    ls_demo-field08 = icon_led_yellow.
    ls_demo-field10 = icon_led_yellow.
    ls_demo-field11 = icon_led_red.
    ls_demo-field12 = icon_led_yellow.
    APPEND ls_demo TO rt_demo.

    CLEAR ls_demo.
    ls_demo-field01 = 'SPMW 060304 - D 51'.
    ls_demo-field02 = '0.76'.
    ls_demo-field04 = icon_led_yellow.
    ls_demo-field05 = icon_led_red.
    ls_demo-field06 = icon_led_red.
    ls_demo-field07 = icon_led_red.
    APPEND ls_demo TO rt_demo.

    CLEAR ls_demo.
    ls_demo-field01 = 'SPMW 060304 - F 55'.
    ls_demo-field02 = '0.44'.
    ls_demo-field03 = icon_led_red.
    ls_demo-field05 = icon_led_green.
    ls_demo-field06 = icon_led_yellow.
    ls_demo-field07 = icon_led_red.
    ls_demo-field09 = icon_led_yellow.
    ls_demo-field10 = icon_led_green.
    ls_demo-field11 = icon_led_yellow.
    ls_demo-field12 = icon_led_yellow.
    APPEND ls_demo TO rt_demo.
  ENDMETHOD.

  METHOD _get_field_catalog.
    DATA ls_field_catalog TYPE lvc_s_fcat.
    DATA lv_num_index     TYPE n LENGTH 2.

    DO 12 TIMES.
      CLEAR ls_field_catalog.
      lv_num_index = sy-index.

      IF lv_num_index = 8.
        ls_field_catalog-edit = abap_true.
      ENDIF.

      CONCATENATE 'FIELD' lv_num_index INTO ls_field_catalog-fieldname.

      ls_field_catalog-reptext   = ls_field_catalog-fieldname.
      APPEND ls_field_catalog TO rt_fieldcatalog.
    ENDDO.
  ENDMETHOD.

  METHOD start_of_selection.
    DATA:
      lr_table       TYPE REF TO data,
      lt_mod_catalog TYPE lvc_t_fcat.


    mt_alv[] = _get_demo_data( ).
    lt_mod_catalog = _get_field_catalog( ).

**********************************************************************
    " Main table & ALV manager
**********************************************************************
    DATA lo_alv TYPE REF TO zcl_eui_alv.

    " Pass by reference
    GET REFERENCE OF mt_alv INTO lr_table.
    CREATE OBJECT lo_alv
      EXPORTING
        ir_table       = lr_table
        it_mod_catalog = lt_mod_catalog.
    SET HANDLER _on_change_styles FOR lo_alv.
    lo_alv->show( me ).
  ENDMETHOD.

  METHOD _on_change_styles.
    INCLUDE <cl_alv_control>.

    DATA: lo_grid    TYPE REF TO cl_gui_alv_grid,
          lo_style   TYPE REF TO zcl_eui_alv_style,
          lt_columns TYPE zcl_eui_alv_style=>tt_col_info,
          ls_column  TYPE zcl_eui_alv_style=>ts_col_info,
          lt_rows    TYPE zcl_eui_alv_style=>tt_row_info,
          ls_row     TYPE zcl_eui_alv_style=>ts_row_info,
          lt_values  TYPE zcl_eui_alv_style=>tt_val_info,
          ls_value   TYPE zcl_eui_alv_style=>ts_val_info.

    CHECK p_merge = abap_true.

    lo_grid = sender->get_grid( ).
    CREATE OBJECT lo_style
      EXPORTING
        io_grid = lo_grid.

**********************************************************************
*    lo_style->set_style(
*          it_columns   = VALUE #( ( column = 'FIELD03'
*                                    style  = alv_style_color_group BIT-OR alv_style_align_center_center ) )
*        )->set_style(
*          it_columns   = VALUE #( style  = alv_style_color_heading BIT-OR alv_style_align_center_center
*                                ( column = 'FIELD04' ) ( column = 'FIELD05' ) ( column = 'FIELD06' ) ( column = 'FIELD07' ) )
*        )->set_style(
*          it_columns   = VALUE #( ( column = 'FIELD09'
*                                    style  = alv_style_color_total BIT-OR alv_style_align_center_center ) )
*        )->set_style(
*          it_columns   = VALUE #( style  = alv_style_color_negative BIT-OR alv_style_align_center_center
*                                ( column = 'FIELD10' ) ( column = 'FIELD11' ) ( column = 'FIELD12' ) )
*        )->set_style(
*          it_rows      = VALUE #( ( row_id = 4
*                                    style  = alv_style_color_positive BIT-OR alv_style_align_center_center BIT-OR alv_style_font_italic ) )
*          it_columns   = VALUE #( ( column = 'FIELD02' ) )
*        )->set_style(
*          it_rows      = VALUE #( ( row_id = 3 ) )  " <-- 1 cell only
*          it_columns   = VALUE #( ( column     = 'FIELD01'
*                                    style      = alv_style_color_negative BIT-OR alv_style_align_right_center
*                                    mergehoriz = 2 " <--- Merge without any condition
*                                ) )
*        )->set_style(
*           it_columns = VALUE #( style   = alv_style_disabled
*                                ( column = 'FIELD08' ) )
*           it_values  = VALUE #( ( value = icon_led_yellow ) )
*        ).

    " Style 1: FIELD03
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-column = 'FIELD03'.
    ls_column-style  = alv_style_color_group BIT-OR alv_style_align_center_center.
    APPEND ls_column TO lt_columns.
    lo_style->set_style( it_columns = lt_columns ).

    " Style 2: FIELD04 to FIELD07
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-style  = alv_style_color_heading BIT-OR alv_style_align_center_center.
    ls_column-column = 'FIELD04'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD05'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD06'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD07'. APPEND ls_column TO lt_columns.
    lo_style->set_style( it_columns = lt_columns ).

    " Style 3: FIELD09
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-column = 'FIELD09'.
    ls_column-style  = alv_style_color_total BIT-OR alv_style_align_center_center.
    APPEND ls_column TO lt_columns.
    lo_style->set_style( it_columns = lt_columns ).

    " Style 4: FIELD10 to FIELD12
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-style  = alv_style_color_negative BIT-OR alv_style_align_center_center.
    ls_column-column = 'FIELD10'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD11'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD12'. APPEND ls_column TO lt_columns.
    lo_style->set_style( it_columns = lt_columns ).

    " Style 5: Row 4, FIELD02
    CLEAR lt_rows. CLEAR ls_row.
    ls_row-row_id = 4.
    ls_row-style  = alv_style_color_positive BIT-OR alv_style_align_center_center BIT-OR alv_style_font_italic.
    APPEND ls_row TO lt_rows.
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-column = 'FIELD02'. APPEND ls_column TO lt_columns.
    lo_style->set_style( it_rows = lt_rows it_columns = lt_columns ).

    " Style 6: Row 3, FIELD01
    CLEAR lt_rows. CLEAR ls_row.
    ls_row-row_id = 3. APPEND ls_row TO lt_rows.
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-column     = 'FIELD01'.
    ls_column-style      = alv_style_color_negative BIT-OR alv_style_align_right_center.
    ls_column-mergehoriz = 2.
    APPEND ls_column TO lt_columns.
    lo_style->set_style( it_rows = lt_rows it_columns = lt_columns ).

    " Style 7: FIELD08, icon
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-style  = alv_style_disabled.
    ls_column-column = 'FIELD08'. APPEND ls_column TO lt_columns.
    CLEAR lt_values. CLEAR ls_value.
    ls_value-value = icon_led_yellow. APPEND ls_value TO lt_values.
    lo_style->set_style( it_columns = lt_columns it_values = lt_values ).


**********************************************************************
    " Merge
*    lo_style->set_style(
*          "it_rows      = VALUE #( ( row_id = 1 ) )
*          it_columns   = VALUE #( style = alv_style_align_center_center BIT-OR alv_style_font_bold
*                                   "( column = 'FIELD04' )
*                                   ( column = 'FIELD05' ) ( column = 'FIELD06' ) ( column = 'FIELD07' )
*                                   "( column = 'FIELD08' )
*                                )
*          it_values    = VALUE #( ( merge_mode = zcl_eui_alv_style=>c_merge_mode-equal
*                                              +  zcl_eui_alv_style=>c_merge_mode-merge_horizontal ) )
*        )->set_style(
*          "it_rows      = VALUE #( ( row_id = 1 ) )
*          it_columns   = VALUE #( style  = alv_style_align_center_center BIT-OR alv_style_font_bold
*                                ( column = 'FIELD10' ) ( column = 'FIELD11' ) ( column = 'FIELD12' ) )
*          it_values    = VALUE #( ( merge_mode = zcl_eui_alv_style=>c_merge_mode-equal
*                                              +  zcl_eui_alv_style=>c_merge_mode-merge_horizontal ) )
*        )->set_style(
*          it_rows      = VALUE #( ( row_id = 2 ) )
*          it_columns   = VALUE #( style  = alv_style_align_center_center
*                                ( column = 'FIELD04' ) ( column = 'FIELD05' ) ( column = 'FIELD06' ) ( column = 'FIELD07' ) ( column = 'FIELD08' ) )
*          it_values    = VALUE #( ( merge_mode = zcl_eui_alv_style=>c_merge_mode-value_and_empty
*                                              +  zcl_eui_alv_style=>c_merge_mode-merge_horizontal ) )
*        )->set_style(
*          it_rows      = VALUE #( ( row_id = 2 ) )
*          it_columns   = VALUE #( style  = alv_style_align_center_center
*                                ( column = 'FIELD10' ) ( column = 'FIELD11' ) )
*          it_values    = VALUE #( ( merge_mode = zcl_eui_alv_style=>c_merge_mode-value_and_empty
*                                              +  zcl_eui_alv_style=>c_merge_mode-merge_horizontal ) )
*        )->set_style(
*          it_columns   = VALUE #( style  = alv_style_align_center_center BIT-OR alv_style_font_bold BIT-OR alv_style_color_key
*                                ( column = 'FIELD01' ) ( column = 'FIELD02' ) )
*          " Merge vertical +  horizontal
*          it_values    = VALUE #( ( merge_mode = zcl_eui_alv_style=>c_merge_mode-equal
*                                              +  zcl_eui_alv_style=>c_merge_mode-merge_horizontal
*                                              +  zcl_eui_alv_style=>c_merge_mode-merge_vertical ) )
*        )->set_style(
*          it_columns = VALUE #( style  = alv_style_align_center_center BIT-OR alv_style_font_bold
*                                ( column = 'FIELD01' ) )
*          " Merge by mask
*          it_values    = VALUE #( ( value      = 'SPMW 060304 -*'
*                                    merge_mode = zcl_eui_alv_style=>c_merge_mode-contains_pattern
*                                              +  zcl_eui_alv_style=>c_merge_mode-merge_vertical ) ) ).

    " Merge 1
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-style = alv_style_align_center_center BIT-OR alv_style_font_bold.
    ls_column-column = 'FIELD05'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD06'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD07'. APPEND ls_column TO lt_columns.
    CLEAR lt_values. CLEAR ls_value.
    ls_value-merge_mode = zcl_eui_alv_style=>c_merge_mode-equal + zcl_eui_alv_style=>c_merge_mode-merge_horizontal.
    APPEND ls_value TO lt_values.
    lo_style->set_style( it_columns = lt_columns it_values = lt_values ).

    " Merge 2
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-style = alv_style_align_center_center BIT-OR alv_style_font_bold.
    ls_column-column = 'FIELD10'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD11'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD12'. APPEND ls_column TO lt_columns.
    CLEAR lt_values. CLEAR ls_value.
    ls_value-merge_mode = zcl_eui_alv_style=>c_merge_mode-equal + zcl_eui_alv_style=>c_merge_mode-merge_horizontal.
    APPEND ls_value TO lt_values.
    lo_style->set_style( it_columns = lt_columns it_values = lt_values ).

    " Merge 3
    CLEAR lt_rows. CLEAR ls_row.
    ls_row-row_id = 2. APPEND ls_row TO lt_rows.
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-style = alv_style_align_center_center.
    ls_column-column = 'FIELD04'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD05'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD06'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD07'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD08'. APPEND ls_column TO lt_columns.
    CLEAR lt_values. CLEAR ls_value.
    ls_value-merge_mode = zcl_eui_alv_style=>c_merge_mode-value_and_empty + zcl_eui_alv_style=>c_merge_mode-merge_horizontal.
    APPEND ls_value TO lt_values.
    lo_style->set_style( it_rows = lt_rows it_columns = lt_columns it_values = lt_values ).

    " Merge 4
    CLEAR lt_rows. CLEAR ls_row.
    ls_row-row_id = 2. APPEND ls_row TO lt_rows.
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-style = alv_style_align_center_center.
    ls_column-column = 'FIELD10'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD11'. APPEND ls_column TO lt_columns.
    CLEAR lt_values. CLEAR ls_value.
    ls_value-merge_mode = zcl_eui_alv_style=>c_merge_mode-value_and_empty + zcl_eui_alv_style=>c_merge_mode-merge_horizontal.
    APPEND ls_value TO lt_values.
    lo_style->set_style( it_rows = lt_rows it_columns = lt_columns it_values = lt_values ).

    " Merge 5
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-style = alv_style_align_center_center BIT-OR alv_style_font_bold BIT-OR alv_style_color_key.
    ls_column-column = 'FIELD01'. APPEND ls_column TO lt_columns.
    ls_column-column = 'FIELD02'. APPEND ls_column TO lt_columns.
    CLEAR lt_values. CLEAR ls_value.
    ls_value-merge_mode = zcl_eui_alv_style=>c_merge_mode-equal + zcl_eui_alv_style=>c_merge_mode-merge_horizontal + zcl_eui_alv_style=>c_merge_mode-merge_vertical.
    APPEND ls_value TO lt_values.
    lo_style->set_style( it_columns = lt_columns it_values = lt_values ).

    " Merge 6
    CLEAR lt_columns. CLEAR ls_column.
    ls_column-style = alv_style_align_center_center BIT-OR alv_style_font_bold.
    ls_column-column = 'FIELD01'. APPEND ls_column TO lt_columns.
    CLEAR lt_values. CLEAR ls_value.
    ls_value-value      = 'SPMW 060304 -*'.
    ls_value-merge_mode = zcl_eui_alv_style=>c_merge_mode-contains_pattern + zcl_eui_alv_style=>c_merge_mode-merge_vertical.
    APPEND ls_value TO lt_values.
    lo_style->set_style( it_columns = lt_columns it_values = lt_values ).

**********************************************************************

    " For columns use FIX_COLUMN = 'X' in field catalogue
    lo_style->set_property( iv_property = 'FixedRows'
                            iv_value    = 2 ).
  ENDMETHOD.
ENDCLASS.


**********************************************************************
**********************************************************************
DATA:
  go_report     TYPE REF TO lcl_report.                     "#EC NEEDED

INITIALIZATION.
  CREATE OBJECT go_report.

START-OF-SELECTION.
  go_report->start_of_selection( ).
