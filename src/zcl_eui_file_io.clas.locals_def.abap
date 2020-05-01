*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF mc_gen_type,
        number TYPE string VALUE 'NUMBER',
        date   TYPE string VALUE 'DATA',
        time   TYPE string VALUE 'TIME',
        skip   TYPE string VALUE 'SKIP',
      END OF mc_gen_type.

    TYPES:
      " For speed
      BEGIN OF ts_col_ind,
        col TYPE char3,
        ind TYPE i,
      END OF ts_col_ind.

    CLASS-DATA:
      " Backward compatibility (could be 1 table)
      mt_col_ind TYPE SORTED TABLE OF ts_col_ind WITH UNIQUE KEY col,
      mt_ind_col TYPE SORTED TABLE OF ts_col_ind WITH UNIQUE KEY ind.

    CLASS-METHODS:
      fill_mapping
        IMPORTING
          ir_table     TYPE REF TO data
          ir_excel_map TYPE REF TO zcl_eui_file_io=>tt_excel_map
        EXPORTING
          et_fieldcat  TYPE lvc_t_fcat
          er_excel_map TYPE REF TO zcl_eui_file_io=>tt_excel_map,

      export_2_table
        IMPORTING
                  ir_table     TYPE REF TO data
                  io_handler   TYPE REF TO object
                  ir_source    TYPE REF TO data
                  iv_row_from  TYPE sytabix
                  io_file      TYPE REF TO zcl_eui_file_io " for messages only
                  ir_excel_map TYPE REF TO zcl_eui_file_io=>tt_excel_map
        RAISING   zcx_eui_exception.
ENDCLASS.


CLASS zcl_eui_file_io DEFINITION LOCAL FRIENDS lcl_helper.
