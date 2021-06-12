*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zeui_test_alv_filter.

TYPE-POOLS:
 abap,
 icon,
 col.

SELECTION-SCREEN BEGIN OF BLOCK bl_grp WITH FRAME.
PARAMETERS:
  p_max  TYPE i DEFAULT 255 OBLIGATORY,
  p_menu AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl_grp.


**********************************************************************
**********************************************************************
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_alv.
        INCLUDE TYPE sflight.
      TYPES:
        t_color TYPE lvc_t_scol,
      END OF ts_alv.
    DATA:
      mt_alv         TYPE STANDARD TABLE OF ts_alv WITH DEFAULT KEY.

    METHODS:
      start_of_selection,

      _on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          sender
          e_ucomm,

      _add_color_filters
        IMPORTING
          io_alv TYPE REF TO zcl_eui_alv,

      _make_filter
        IMPORTING
                  fieldname        TYPE lvc_s_filt-fieldname
                  option           TYPE lvc_s_filt-option OPTIONAL
                  low              TYPE lvc_s_filt-low
        RETURNING VALUE(rt_filter) TYPE lvc_t_filt.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_report IMPLEMENTATION.
  METHOD start_of_selection.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_alv  "#EC CI_NOWHERE
    FROM sflight
    UP TO p_max ROWS.

    DATA lr_table TYPE REF TO data.
    GET REFERENCE OF mt_alv INTO lr_table.

    DATA ls_layout TYPE lvc_s_layo.
    ls_layout-ctab_fname = 'T_COLOR'.

    DATA lt_toolbar TYPE ttb_button.
    DATA lr_toolbar TYPE REF TO stb_button.
    APPEND INITIAL LINE TO lt_toolbar REFERENCE INTO lr_toolbar.
    lr_toolbar->function = '_ADD_ROWS_'.
    lr_toolbar->text     = 'Add selected rows'(add).
    lr_toolbar->icon     = icon_insert_row.

    DATA lo_alv TYPE REF TO zcl_eui_alv.
    CREATE OBJECT lo_alv
      EXPORTING
        ir_table   = lr_table
        is_layout  = ls_layout
        it_toolbar = lt_toolbar.

    _add_color_filters( lo_alv ).
    lo_alv->show( io_handler = me ).
  ENDMETHOD.

  METHOD _on_user_command.
    CHECK e_ucomm = '_ADD_ROWS_'.

    DATA lt_sel_rows TYPE lvc_t_row.
    sender->get_selected_rows( IMPORTING et_index_rows = lt_sel_rows ).
    IF lt_sel_rows IS INITIAL.
      MESSAGE 'Please select rows first' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA lt_alv LIKE mt_alv.
    FIELD-SYMBOLS <ls_sel_rows> LIKE LINE OF lt_sel_rows.
    LOOP AT lt_sel_rows ASSIGNING <ls_sel_rows>.
      DATA ls_alv LIKE LINE OF lt_alv.
      READ TABLE mt_alv INDEX <ls_sel_rows>-index INTO ls_alv.

      " Without any rules (colors)
      CLEAR ls_alv-t_color[].
      APPEND ls_alv TO lt_alv.
    ENDLOOP.

    " In the begining
    INSERT LINES OF lt_alv INTO mt_alv INDEX 1.

    " And update
    DATA ls_stable TYPE lvc_s_stbl.
    ls_stable-col = ls_stable-row = 'X'.
    sender->refresh_table_display( is_stable = ls_stable ).
  ENDMETHOD.

  METHOD _add_color_filters.
    DATA lo_filter TYPE REF TO zcl_eui_alv_filter.
    CREATE OBJECT lo_filter.

    DATA lt_filter TYPE lvc_t_filt.
    DATA ls_color  TYPE lvc_s_colo.

**********************************************************************
    " SIGN='I' & OPTION='EQ' by default

    " --1--
    lt_filter = _make_filter( fieldname = 'PRICE' " <--- color this field
                              option    = 'LE'
                              low       = '500' ).
    ls_color-col = col_positive.
    lo_filter->add_rule( it_filter = lt_filter
                         iv_desc   = 'Low price'
                         is_color  = ls_color
                         " iv_field  = '' " Use 'PRICE' field for column
    ).

    " --2--
    lt_filter = _make_filter( fieldname = 'SEATSMAX'
                              option    = 'GE'
                              low       = '400'  ).
    ls_color-col = col_heading.
    lo_filter->add_rule( it_filter = lt_filter
                         " iv_desc   = 'Very big planes'  " <--- Get description from filter
                         is_color  = ls_color
                         iv_field  = 'PLANETYPE,SEATSMAX' " <--- color 2 fields
    ).

    " --3--
    lt_filter = _make_filter( fieldname = 'SEATSOCC'
                              low       = '0'  ).
    ls_color-col = col_negative.
    lo_filter->add_rule( it_filter = lt_filter
                         " iv_desc   = 'Occupied seats is equal to zero'  " <--- Get description from filter
                         is_color  = ls_color
                         iv_field  = '*' " <--- all fields in row
    ).

    lo_filter->add_button( io_alv  = io_alv
                           " is_button = use own button style
                           iv_menu = p_menu ).
  ENDMETHOD.

  METHOD _make_filter.
    FIELD-SYMBOLS <ls_filter> LIKE LINE OF rt_filter.
    APPEND INITIAL LINE TO rt_filter ASSIGNING <ls_filter>.
    <ls_filter>-fieldname = fieldname.
    <ls_filter>-option    = option.
    <ls_filter>-low       = low.
  ENDMETHOD.
ENDCLASS.
**********************************************************************
**********************************************************************

DATA:
  go_report     TYPE REF TO lcl_report.

INITIALIZATION.
  CREATE OBJECT go_report.

START-OF-SELECTION.
  go_report->start_of_selection( ).
