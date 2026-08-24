INTERFACE zif_eui_ole PUBLIC.
  TYPE-POOLS:
    abap,
    ole2.

  DATA:
    " Excel & Word
    mv_ole_app    TYPE ole2_object READ-ONLY,
    mv_ole_doc    TYPE ole2_object READ-ONLY,

    mv_class      TYPE char40      READ-ONLY,
    mv_mime_type  TYPE string      READ-ONLY,

    " HTML & PDF
    mv_in_browser TYPE abap_bool   READ-ONLY,
    mv_proxy_app  TYPE char40      READ-ONLY.

  METHODS:
    init,

    call_method
      IMPORTING
                io_object        TYPE ole2_object OPTIONAL " Default mv_ole_app
                iv_method        TYPE csequence
                iv_param1        TYPE any         OPTIONAL
                iv_param2        TYPE any         OPTIONAL
                iv_param3        TYPE any         OPTIONAL
                iv_param4        TYPE any         OPTIONAL
                iv_param5        TYPE any         OPTIONAL
                iv_param6        TYPE any         OPTIONAL
                iv_param7        TYPE any         OPTIONAL
      RETURNING VALUE(ro_result) TYPE ole2_object,

    get_property
      IMPORTING
                io_object        TYPE ole2_object OPTIONAL " Default mv_ole_app
                iv_prop          TYPE csequence
      RETURNING VALUE(ro_result) TYPE ole2_object,

    set_property
      IMPORTING
        io_object TYPE ole2_object OPTIONAL " Default mv_ole_app
        iv_prop   TYPE csequence
        iv_value  TYPE any,

    open
      IMPORTING
        iv_path    TYPE csequence
        iv_visible TYPE abap_bool,

    show IMPORTING io_container TYPE REF TO cl_gui_container,

    is_web_dynpro IMPORTING
                            iv_filename          TYPE string                          " File name with extension
                            iv_inplace           TYPE abap_bool
                            iv_in_new_window     TYPE abap_bool DEFAULT abap_false
                  RETURNING VALUE(rv_web_dynpro) TYPE abap_bool,

    save_as IMPORTING iv_path       TYPE csequence
                      iv_ext_format TYPE i
                      iv_quit       TYPE abap_bool.
ENDINTERFACE.
