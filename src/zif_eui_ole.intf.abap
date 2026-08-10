INTERFACE zif_eui_ole PUBLIC.
  TYPE-POOLS:
    abap,
    ole2.

  DATA:
    " Excel & Word
    mv_ole_app    TYPE ole2_object READ-ONLY,
    mv_ole_doc    TYPE ole2_object READ-ONLY,

    mv_class      TYPE text40      READ-ONLY,
    mv_mime_type  TYPE string      READ-ONLY,

    " HTML & PDF
    mv_in_browser TYPE abap_bool   READ-ONLY,
    mv_proxy_app  TYPE text40      READ-ONLY.

  METHODS:
    init,

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
