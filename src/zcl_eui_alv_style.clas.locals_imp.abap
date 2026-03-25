*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_helper DEFINITION FINAL INHERITING FROM cl_gui_alv_grid.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_style_table IMPORTING io_grid               TYPE REF TO cl_gui_alv_grid
                      RETURNING VALUE(rr_style_table) TYPE REF TO lvc_t_data,

      get_data_table  IMPORTING io_grid              TYPE REF TO cl_gui_alv_grid
                      RETURNING VALUE(rr_data_table) TYPE REF TO data,

      set_grid_property IMPORTING io_grid     TYPE REF TO cl_gui_alv_grid
                                  iv_property TYPE csequence
                                  iv_value    TYPE any.
ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.
  METHOD get_style_table.
    GET REFERENCE OF io_grid->mt_data INTO rr_style_table.
  ENDMETHOD.

  METHOD get_data_table.
    rr_data_table = io_grid->mt_outtab.
  ENDMETHOD.

  METHOD set_grid_property.
    io_grid->set_property(
          EXPORTING  property = iv_property
                     value    = iv_value
          EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      zcx_eui_no_check=>raise_sys_error( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
