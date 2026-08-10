CLASS zcl_eui_ole DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_eui_ole.

    ALIASES:
    " Excel & Word
     mv_ole_app   FOR zif_eui_ole~mv_ole_app,
     mv_ole_doc   FOR zif_eui_ole~mv_ole_doc,
     mv_class     FOR zif_eui_ole~mv_class,
     mv_mime_type FOR zif_eui_ole~mv_mime_type,

    " HTML & PDF
    mv_in_browser FOR zif_eui_ole~mv_in_browser,
    mv_proxy_app  FOR zif_eui_ole~mv_proxy_app.

    METHODS:
      constructor IMPORTING io_file TYPE REF TO zcl_eui_file.

  PRIVATE SECTION.
    DATA:
      mo_file TYPE REF TO zcl_eui_file.

    METHODS:
      _show_in_browser
        IMPORTING
          io_container TYPE REF TO cl_gui_container,

      _show_in_doi
        IMPORTING
          io_container TYPE REF TO cl_gui_container.
ENDCLASS.



CLASS zcl_eui_ole IMPLEMENTATION.
  METHOD constructor.
    mo_file = io_file.
  ENDMETHOD.

  METHOD zif_eui_ole~init.
    " detect by extension
    IF mo_file->mv_extension CP `xls*` OR mo_file->mv_extension = zcl_eui_file=>mc_extension-csv.
      mv_class      = `Excel.Application`.
      mv_proxy_app  = `Excel.Sheet`.
      CASE mo_file->mv_extension.
        WHEN `xls`.
          mv_mime_type  = `application/vnd.ms-excel`.
        WHEN `xlsx`.
          mv_mime_type  = `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`.
        WHEN `xlsm`.
          mv_mime_type  = `application/vnd.ms-excel.sheet.macroEnabled.12`.
      ENDCASE.
    ELSEIF mo_file->mv_extension CP `doc*`.
      mv_class      = `Word.Application`.
      mv_proxy_app  = `Word.Document`.
      CASE mo_file->mv_extension.
        WHEN `doc`.
          mv_mime_type  = `application/msword`.
        WHEN `docx`.
          mv_mime_type  = `application/vnd.openxmlformats-officedocument.wordprocessingml.document`.
        WHEN `docm`.
          mv_mime_type  = `application/vnd.ms-word.document.macroEnabled.12`.
      ENDCASE.
    ELSEIF mo_file->mv_extension CP `htm*`.
      mv_in_browser = abap_true.
      mv_mime_type  = `text/html`.
    ELSEIF mo_file->mv_extension = zcl_eui_file=>mc_extension-pdf.
      mv_in_browser = abap_true.
      mv_mime_type  = `application/pdf`.
    ENDIF.
  ENDMETHOD.

  METHOD zif_eui_ole~open.
    DATA lo_docs TYPE ole2_object.

    " Open with OLE for call a macro. Only for .docx, .docm, .xlsx, .xlsm
    IF mv_ole_app IS INITIAL.
      " Create 1 time only (or use existing)
      CREATE OBJECT mv_ole_app mv_class.
    ENDIF.

    IF mv_class = `Excel.Application`. " Excel
      GET PROPERTY OF mv_ole_app 'Workbooks' = lo_docs.
    ELSE.                              " Word
      GET PROPERTY OF mv_ole_app 'Documents' = lo_docs.
    ENDIF.

    CALL METHOD OF lo_docs 'Open' = mv_ole_doc
      EXPORTING
        #1 = iv_path.

    IF iv_visible = abap_true.
      SET PROPERTY OF mv_ole_app 'Visible' = 1.
    ENDIF.
  ENDMETHOD.

  METHOD zif_eui_ole~show.
    " In browser
    IF mv_in_browser = abap_true.
      _show_in_browser( io_container ).
    ELSE. " Use doi
      _show_in_doi( io_container ).
    ENDIF.
  ENDMETHOD.

  METHOD _show_in_browser.
    " Convert to table
    DATA lt_solix_tab   TYPE solix_tab.
    DATA lv_solix_len   TYPE i.
    DATA lo_html_viewer TYPE REF TO cl_gui_html_viewer.
    DATA lv_url         TYPE text1000.
    DATA lv_subtype     TYPE text10.

    CREATE OBJECT lo_html_viewer
      EXPORTING
        parent = io_container.

    " Convert
    zcl_eui_conv=>xstring_to_binary(
      EXPORTING
        iv_xstring = mo_file->mv_xstring
      IMPORTING
        ev_length  = lv_solix_len
        et_table   = lt_solix_tab ).

    " Load
    lv_subtype = mo_file->mv_extension.
    lo_html_viewer->load_data(
      EXPORTING
        type         = 'application'
        subtype      = lv_subtype
        size         = lv_solix_len
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_solix_tab
      EXCEPTIONS
        OTHERS       = 1 ).

    " Oops
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      zcx_eui_exception=>raise_dump( iv_message = sy-msgli ).
    ENDIF.

    " Show it
    lo_html_viewer->show_url(
      url      = lv_url
      in_place = abap_true ).
  ENDMETHOD.

  METHOD _show_in_doi.
    " Convert to table
    DATA lt_solix_tab   TYPE solix_tab.
    DATA lv_solix_len   TYPE i.
    DATA lo_control     TYPE REF TO i_oi_container_control.
    DATA lo_document    TYPE REF TO i_oi_document_proxy.
    DATA ls_handle      TYPE cntl_handle.

    c_oi_container_control_creator=>get_container_control( IMPORTING control = lo_control ).
    lo_control->init_control( EXPORTING inplace_enabled     = abap_true
                                        r3_application_name = sy-tcode
                                        parent              = io_container ).

    lo_control->get_document_proxy( EXPORTING document_type  = mv_proxy_app
                                    IMPORTING document_proxy = lo_document ).

    " Show document
    IF zcl_eui_file=>file_exist( mo_file->mv_full_path ) = abap_true.
      DATA lv_url TYPE swk_url.
      CONCATENATE `FILE://` mo_file->mv_full_path INTO lv_url.
      lo_document->open_document( document_url = lv_url
                                  open_inplace = abap_true ).
    ELSE.
      " Convert
      zcl_eui_conv=>xstring_to_binary(
        EXPORTING
          iv_xstring = mo_file->mv_xstring
        IMPORTING
          ev_length  = lv_solix_len
          et_table   = lt_solix_tab ).

      lo_document->open_document_from_table( document_size  = lv_solix_len
                                             document_table = lt_solix_tab
                                             open_inplace   = abap_true ).
    ENDIF.

    " For OLE
    lo_document->get_document_handle(
      IMPORTING
        handle = ls_handle ).

    " Get Application object (the same for Word & Excel)
    GET PROPERTY OF ls_handle-obj 'Application' = mv_ole_app.
  ENDMETHOD.

  METHOD zif_eui_ole~save_as.
  BREAK-POINT.
    CALL METHOD OF mv_ole_doc 'SaveAs'
      EXPORTING
        #1 = iv_path
        #2 = iv_ext_format.

    IF iv_quit = abap_true.
      CALL METHOD OF mv_ole_app 'QUIT'.
      FREE OBJECT: mv_ole_doc, mv_ole_app.
    ENDIF.
  ENDMETHOD.

  METHOD zif_eui_ole~is_web_dynpro.
    DATA lv_mime_type TYPE string.

    IF wdr_task=>application IS INITIAL.
      RETURN.
    ENDIF.
    rv_web_dynpro = abap_true.

    " Use as default
    lv_mime_type = mv_mime_type.
    IF lv_mime_type IS INITIAL.
      lv_mime_type = 'RAW'.
    ENDIF.

    cl_wd_runtime_services=>attach_file_to_response(
      i_filename      = iv_filename
      i_content       = mo_file->mv_xstring
      i_inplace       = iv_inplace
      " Use defaults
      i_in_new_window = iv_in_new_window
      i_mime_type     = lv_mime_type ).
  ENDMETHOD.
ENDCLASS.
