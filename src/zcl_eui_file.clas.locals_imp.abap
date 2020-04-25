*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_doi IMPLEMENTATION.

  METHOD show_in_browser.
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
       iv_xstring = io_file->mv_xstring
     IMPORTING
       ev_length  = lv_solix_len
       et_table   = lt_solix_tab ).

    " Load
    lv_subtype = io_file->mv_extension.
    lo_html_viewer->load_data(
     EXPORTING
       type                 = 'application'
       subtype              = lv_subtype
       size                 = lv_solix_len
     IMPORTING
       assigned_url         = lv_url
     CHANGING
       data_table           = lt_solix_tab
     EXCEPTIONS
       OTHERS               = 1 ).

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

  METHOD show_in_doi.
    " Convert to table
    DATA lt_solix_tab   TYPE solix_tab.
    DATA lv_solix_len   TYPE i.
    DATA lo_control     TYPE REF TO i_oi_container_control.
    DATA lo_document    TYPE REF TO i_oi_document_proxy.
    DATA ls_handle      TYPE cntl_handle.

    c_oi_container_control_creator=>get_container_control( IMPORTING control = lo_control ).
    lo_control->init_control( EXPORTING  inplace_enabled     = abap_true
                                         r3_application_name = sy-tcode
                                         parent              = io_container ).

    lo_control->get_document_proxy( EXPORTING document_type  = io_file->ms_ole_info-proxy_app
                                    IMPORTING document_proxy = lo_document ).

    " Show documnet
    DATA lv_exist TYPE abap_bool.
    cl_gui_frontend_services=>file_exist(
      EXPORTING
        file   = io_file->mv_full_path
      RECEIVING
        result = lv_exist
      EXCEPTIONS
        OTHERS = 0 ). " prevent GUI messages when file not found
    IF lv_exist = abap_true.
      DATA lv_url TYPE swk_url.
      CONCATENATE `FILE://` io_file->mv_full_path INTO lv_url.
      lo_document->open_document( document_url = lv_url
                                  open_inplace = abap_true ).
    ELSE.
      " Convert
      zcl_eui_conv=>xstring_to_binary(
       EXPORTING
         iv_xstring = io_file->mv_xstring
       IMPORTING
         ev_length  = lv_solix_len
         et_table   = lt_solix_tab ).

      lo_document->open_document_from_table( document_size    = lv_solix_len
                                             document_table   = lt_solix_tab
                                             open_inplace     = abap_true ).
    ENDIF.

    " For OLE
    lo_document->get_document_handle(
       IMPORTING
        handle = ls_handle ).

    " Get Application object (the same for Word & Excel)
    GET PROPERTY OF ls_handle-obj 'Application' = co_ole_app.
  ENDMETHOD.

  METHOD web_dynpro_attach.
    cl_wd_runtime_services=>attach_file_to_response(
      i_filename      = i_filename
      i_content       = i_content
      i_inplace       = i_inplace

      " Use defaults
      i_in_new_window = i_in_new_window
      i_mime_type     = i_mime_type ).
  ENDMETHOD.
ENDCLASS.
