*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_json_util DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
     mt_xsdboolean TYPE stringtab.

    CLASS-METHODS:
      class_constructor.
ENDCLASS.

CLASS lcl_salv_util DEFINITION INHERITING FROM cl_salv_model_list FINAL.
  PUBLIC SECTION.

    CLASS-METHODS:
      _get_grid_from_salv
        IMPORTING
          io_salv           TYPE REF TO cl_salv_model_list
        RETURNING
          VALUE(ro_gui_alv) TYPE REF TO cl_gui_alv_grid
        RAISING
          cx_salv_msg.
ENDCLASS.

CLASS lcl_grid_util DEFINITION INHERITING FROM cl_gui_alv_grid FINAL.
  PUBLIC SECTION.

    CLASS-METHODS:
      _get_grid_table
        IMPORTING
          io_alv          TYPE REF TO cl_gui_alv_grid
        RETURNING
          VALUE(rr_table) TYPE REF TO data .
ENDCLASS.

CLASS lcl_assert_util DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_class_name
        RETURNING VALUE(rv_class_name) TYPE seoclass-clsname.
ENDCLASS.
