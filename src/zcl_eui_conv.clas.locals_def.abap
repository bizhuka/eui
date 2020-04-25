*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_helper DEFINITION INHERITING FROM cl_salv_model_list FINAL.
  PUBLIC SECTION.

    CLASS-DATA:
     mt_xsdboolean TYPE stringtab.

    CLASS-METHODS:
      class_constructor,

      alv_from_salv
        IMPORTING
          io_salv           TYPE REF TO cl_salv_model_list
        RETURNING
          VALUE(ro_gui_alv) TYPE REF TO cl_gui_alv_grid
        RAISING
          cx_salv_msg.
ENDCLASS.

CLASS zcl_eui_conv DEFINITION LOCAL FRIENDS lcl_helper.
