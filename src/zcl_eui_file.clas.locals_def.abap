*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPE-POOLS:
 abap.

CLASS lcl_doi DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:

      show_in_browser
        IMPORTING
          io_file      TYPE REF TO zcl_eui_file
          io_container TYPE REF TO cl_gui_container,

      show_in_doi
        IMPORTING
          io_file      TYPE REF TO zcl_eui_file
          io_container TYPE REF TO cl_gui_container
        CHANGING
          co_ole_app   TYPE ole2_object,

      web_dynpro_attach
        IMPORTING
          i_filename         TYPE string                          " File name with extension
          i_content          TYPE xstring
          i_inplace          TYPE abap_bool
          i_in_new_window    TYPE abap_bool DEFAULT abap_false
          VALUE(i_mime_type) TYPE string    OPTIONAL.
ENDCLASS.


CLASS zcl_eui_file DEFINITION LOCAL FRIENDS lcl_doi.
