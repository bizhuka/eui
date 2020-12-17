class ZCL_EUI_FILE definition
  public
  inheriting from ZCL_EUI_MANAGER
  create public .

public section.
  type-pools OLE2 .

  types:
    BEGIN OF TS_OLE_INFO,
      " Excel & Word
      app      TYPE ole2_object,
      class    TYPE text40,

      " Html & pdf
      in_browser   TYPE abap_bool,
      proxy_app    TYPE text40,
    END OF TS_OLE_INFO .

  constants:
    BEGIN OF MC_EXTENSION,
     xlsx TYPE STRING VALUE 'xlsx',
     csv  TYPE STRING VALUE 'csv',
     docx TYPE STRING VALUE 'docx',
     html TYPE STRING VALUE 'html',
     pdf  TYPE STRING VALUE 'pdf',
   END OF MC_EXTENSION .
  data MV_XSTRING type XSTRING read-only .

  methods CONSTRUCTOR
    importing
      !IV_FILE_NAME type CSEQUENCE optional
      !IV_XSTRING type XSTRING optional
      !IV_STATUS_NAME type GUI_STATUS optional
      !IV_STATUS_PROG type SYREPID optional
      !IT_STATUS_EXCLUDE type ZIF_EUI_MANAGER=>TT_STATUS_EXCLUDE optional
      !IV_STATUS_TITLE type CSEQUENCE optional .
  methods GET_OLE_INFO
    returning
      value(RS_OLE_INFO) type TS_OLE_INFO .
  methods GET_FULL_PATH
    returning
      value(RV_FULL_PATH) type STRING .
  methods IMPORT_FROM_FILE
    importing
      value(IV_FULL_PATH) type STRING optional
      !IV_WINDOW_TITLE type CSEQUENCE default 'Import'
      value(IV_DEFAULT_EXTENSION) type STRING optional
      value(IV_DEFAULT_FILENAME) type STRING optional
      value(IV_FILE_FILTER) type STRING optional
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE
    raising
      ZCX_EUI_EXCEPTION .
  methods IMPORT_FROM_BINARY
    importing
      !IT_TABLE type STANDARD TABLE
      !IV_LENGTH type I
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE .
  methods IMPORT_FROM_STRING
    importing
      !IV_STRING type STRING
      !IV_ENCODING type ABAP_ENCODING default ZCL_EUI_CONV=>MC_ENCODING-UTF_8
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE .
  methods IMPORT_FROM_XSTRING
    importing
      !IV_XSTRING type XSTRING
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE .
  methods DOWNLOAD
    importing
      !IV_FULL_PATH type CSEQUENCE optional
      !IV_SAVE_DIALOG type ABAP_BOOL default ABAP_FALSE
      !IV_WINDOW_TITLE type CSEQUENCE default 'Export'
      value(IV_DEFAULT_EXTENSION) type STRING optional
      value(IV_DEFAULT_FILENAME) type STRING optional
      value(IV_FILE_FILTER) type STRING optional
      !IV_FILETYPE type CHAR10 default 'BIN'
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE
    raising
      ZCX_EUI_EXCEPTION .
  methods TO_APP_SERVER
    importing
      !IV_FULL_PATH type CSEQUENCE
      !IV_OVERWRITE type ABAP_BOOL default ABAP_TRUE .
  methods OPEN
    raising
      ZCX_EUI_EXCEPTION .
  methods OPEN_BY_OLE
    importing
      !IV_VISIBLE type ABAP_BOOL default ABAP_TRUE
    changing
      !CV_OLE_APP type OLE2_OBJECT optional
      !CV_OLE_DOC type OLE2_OBJECT optional
    raising
      ZCX_EUI_EXCEPTION .
  class-methods SPLIT_FILE_PATH
    importing
      !IV_FULLPATH type CSEQUENCE
    exporting
      !EV_PATH type CSEQUENCE
      !EV_FILENAME type CSEQUENCE
      !EV_FILE_NOEXT type CSEQUENCE
      !EV_EXTENSION type CSEQUENCE .
  class-methods FILE_EXIST
    importing
      !IV_FULL_PATH type CSEQUENCE
    returning
      value(RV_EXIST) type ABAP_BOOL .

  methods ZIF_EUI_MANAGER~PBO
    redefinition .
  methods ZIF_EUI_MANAGER~SHOW
    redefinition .
protected section.

  data MV_EXTENSION type STRING .
  data MV_FILE_NAME type STRING .

  methods SET_FULL_PATH
    importing
      !IV_FULL_PATH type CSEQUENCE optional .
private section.

  data MV_FULL_PATH type STRING .
  data MS_OLE_INFO type TS_OLE_INFO .
ENDCLASS.



CLASS ZCL_EUI_FILE IMPLEMENTATION.


METHOD constructor.
  super->constructor(
   iv_status_name    = iv_status_name
   iv_status_prog    = iv_status_prog
   it_status_exclude = it_status_exclude
   iv_status_title   = iv_status_title
   iv_read_only      = abap_true ).

  mv_xstring = iv_xstring.
  set_full_path( iv_file_name ).
ENDMETHOD.


METHOD download.
  DATA lv_full_path  TYPE string.
  DATA lv_path       TYPE string.
  DATA lv_sep        TYPE char1.
  DATA lv_len        TYPE i.
  DATA lv_action     TYPE i.
  DATA lt_bin_data   TYPE solix_tab.
  DATA lv_filesize   TYPE i.
  DATA lv_no_ext     TYPE string.
  DATA lv_guid       TYPE guid_32.
  DATA lv_title      TYPE string.

  " Side results of method
  set_full_path( ).
  ro_file = me.

  lv_full_path = iv_full_path.
  zcl_eui_file=>split_file_path(
   EXPORTING
     iv_fullpath   = lv_full_path
   IMPORTING
     ev_filename   = iv_default_filename  " lv_file_name
     ev_path       = lv_path
     ev_extension  = iv_default_extension " lv_extension
     ev_file_noext = lv_no_ext
   ).

  " Then just export to Excel or CSV
  set_if_initial iv_default_extension mv_extension. " lv_extension

  " Create file name if is not supplied
  IF iv_default_filename IS INITIAL. " lv_file_name
    CONCATENATE sy-datum(4) `-` sy-datum+4(2) `-` sy-datum+6(2) ` `
                sy-uzeit(2) `-` sy-uzeit+2(2) `-` sy-uzeit+4(2) INTO lv_no_ext.

    CONCATENATE lv_no_ext `.`
                iv_default_extension  " lv_extension
     INTO iv_default_filename.        " lv_file_name.
  ENDIF.

**********************************************************************
  " Add as an attachment to Web dynpro
**********************************************************************
  IF wdr_task=>application IS NOT INITIAL.
    lcl_doi=>web_dynpro_attach(
        i_filename      = iv_default_filename " lv_file_name " File name with extension
        i_content       = me->mv_xstring
        i_inplace       = abap_false ). " <--- just download
    RETURN.
  ENDIF.

  " Get path is not supplied
  IF lv_path IS INITIAL.
    CASE iv_save_dialog.
      WHEN abap_true.
        " Texts for dialog
        prepare_dialog.

        lv_title = iv_window_title.
        cl_gui_frontend_services=>file_save_dialog(
          EXPORTING
            window_title      = lv_title
            default_extension = iv_default_extension " lv_extension
            default_file_name = iv_default_filename  " lv_file_name
            file_filter       = iv_file_filter
          CHANGING
            filename          = iv_default_filename  " lv_file_name
            fullpath          = lv_full_path
            path              = lv_path
            user_action       = lv_action ).

        IF lv_action <> cl_gui_frontend_services=>action_ok OR iv_default_filename IS INITIAL.
          MESSAGE s009(zeui_message) INTO sy-msgli.
          zcx_eui_exception=>raise_sys_error( ).
        ENDIF.

      WHEN OTHERS.
        " Save to temp dir No need to clean files (cl_gui_frontend_services=>file_delete). SAP gui cleans 'SAP GUI\tmp\' automatically
        cl_gui_frontend_services=>get_temp_directory( CHANGING temp_dir = lv_path EXCEPTIONS OTHERS = 1 ).
        CHECK sy-subrc = 0.

        " Add file separator
        cl_gui_frontend_services=>get_file_separator(
         CHANGING
           file_separator = lv_sep ).
        cl_gui_cfw=>flush( ).

        lv_len = strlen( lv_path ) - 1.
        IF lv_path+lv_len(1) <> lv_sep.
          CONCATENATE lv_path lv_sep INTO lv_path.
        ENDIF.

        " And create new path
        CONCATENATE lv_path iv_default_filename INTO lv_full_path. " lv_file_name

        " Already exist. Create unique name
        IF zcl_eui_file=>file_exist( lv_full_path ) = abap_true.
          lv_guid = zcl_eui_conv=>guid_create( ).
          CONCATENATE lv_path lv_no_ext ` ` lv_guid `.` iv_default_extension INTO lv_full_path.
        ENDIF.
    ENDCASE.
  ENDIF.

**********************************************************************
  DATA lt_solix_tab TYPE solix_tab.
  DATA lv_solix_len TYPE i.

  zcl_eui_conv=>xstring_to_binary(
   EXPORTING
     iv_xstring = me->mv_xstring
   IMPORTING
     ev_length  = lv_solix_len
     et_table   = lt_solix_tab ).

  IF lv_solix_len < 10000000.
    " For small files
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize = lv_solix_len
        filename     = lv_full_path
        filetype     = iv_filetype
      CHANGING
        data_tab     = lt_solix_tab
      EXCEPTIONS
        OTHERS       = 1 ).
  ELSE.
    DATA lv_dest   TYPE rfcdes-rfcdest.
    DATA lv_cpath  TYPE text1000.

    DO 1 TIMES.
      " For big files
      CALL FUNCTION 'SCMS_FE_START_REG_SERVER'
        EXPORTING
          destname    = 'SAPFTP'
        IMPORTING
          destination = lv_dest
        EXCEPTIONS
          OTHERS      = 1.
      CHECK sy-subrc = 0.

      " create_folders( cv_fullpath ).
      lv_cpath = lv_full_path.
      CALL FUNCTION 'FTP_R3_TO_CLIENT'
        EXPORTING
          fname           = lv_cpath
          rfc_destination = lv_dest
          blob_length     = lv_solix_len
        TABLES
          blob            = lt_solix_tab
        EXCEPTIONS
          OTHERS          = 1.
      CHECK sy-subrc = 0.

      CALL FUNCTION 'SCMS_FE_STOP_REG_SERVER'
        CHANGING
          destination = lv_dest.
    ENDDO.
  ENDIF.

  " Oops
  IF sy-subrc <> 0.
    zcx_eui_exception=>raise_sys_error( ).
  ENDIF.

  " Save side results
  set_full_path( lv_full_path ).
ENDMETHOD.                                               "#EC CI_VALPAR


METHOD file_exist.
  DATA lv_full_path TYPE string.
  lv_full_path = iv_full_path.
  cl_gui_frontend_services=>file_exist(
    EXPORTING
      file   = lv_full_path
    RECEIVING
      result = rv_exist
    EXCEPTIONS
      OTHERS = 0 ). " prevent GUI messages when file not found
ENDMETHOD.


METHOD get_full_path.
  rv_full_path = mv_full_path.
ENDMETHOD.


METHOD get_ole_info.
  rs_ole_info = ms_ole_info.
ENDMETHOD.


METHOD import_from_binary.
  mv_xstring = zcl_eui_conv=>binary_to_xstring(
   it_table  = it_table
   iv_length = iv_length ).
  ro_file = me.
ENDMETHOD.


METHOD import_from_file.
  DATA          lt_file     TYPE filetable.
  DATA          lv_action   TYPE i.
  DATA          lv_rc       TYPE i.
  DATA          lt_bin_data TYPE solix_tab.
  DATA          lv_filesize TYPE i.
  DATA          lv_title    TYPE string.
  FIELD-SYMBOLS <ls_file>   LIKE LINE OF lt_file.

  " Side results of method
  set_full_path( ).

  " No full path
  IF iv_full_path IS INITIAL.
    " Texts for dialog
    prepare_dialog.

    lv_title = iv_window_title.
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title      = lv_title
        default_extension = iv_default_extension
        default_filename  = iv_default_filename
        file_filter       = iv_file_filter
      CHANGING
        file_table        = lt_file
        user_action       = lv_action
        rc                = lv_rc ).
    IF lv_action <> cl_gui_frontend_services=>action_ok OR lt_file IS INITIAL.
      MESSAGE s009(zeui_message) INTO sy-msgli.
      zcx_eui_exception=>raise_sys_error( ).
    ENDIF.

    " Get first file (always subrc = 0)
    READ TABLE lt_file ASSIGNING <ls_file> INDEX 1.
    iv_full_path = <ls_file>-filename.
  ENDIF.

  " load file
  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename   = iv_full_path
      filetype   = 'BIN'
    IMPORTING
      filelength = lv_filesize
    CHANGING
      data_tab   = lt_bin_data
    EXCEPTIONS
      OTHERS     = 1 ).
  IF sy-subrc <> 0.
    zcx_eui_exception=>raise_sys_error( ).
  ENDIF.

  " Set from binary
  ro_file = import_from_binary(
   it_table  = lt_bin_data
   iv_length = lv_filesize ).

  " Save side results
  set_full_path( iv_full_path ).
ENDMETHOD.                                               "#EC CI_VALPAR


METHOD import_from_string.
  mv_xstring = zcl_eui_conv=>string_to_xstring(
   iv_string   = iv_string
   iv_encoding = iv_encoding ).

  " Add bom
  DATA lv_bom TYPE xstring.
  CASE iv_encoding.
    WHEN zcl_eui_conv=>mc_encoding-utf_16be.
      lv_bom = cl_abap_char_utilities=>byte_order_mark_big.
    WHEN zcl_eui_conv=>mc_encoding-utf_16le.
      lv_bom = cl_abap_char_utilities=>byte_order_mark_little.
    WHEN zcl_eui_conv=>mc_encoding-utf_8.
      lv_bom = cl_abap_char_utilities=>byte_order_mark_utf8.
  ENDCASE.

  IF lv_bom IS NOT INITIAL.
    CONCATENATE lv_bom mv_xstring INTO mv_xstring IN BYTE MODE.
  ENDIF.

  ro_file = me.
ENDMETHOD.


METHOD import_from_xstring.
  mv_xstring = iv_xstring.
  ro_file = me.
ENDMETHOD.


METHOD open.
  " No need ro_file = me.

  cl_gui_frontend_services=>execute(
   EXPORTING
    document               = me->mv_full_path
    operation              = 'OPEN'
   EXCEPTIONS
    OTHERS                 = 1 ).
  cl_gui_cfw=>flush( ).
  CHECK sy-subrc <> 0.

  zcx_eui_exception=>raise_sys_error( ).
ENDMETHOD.


METHOD open_by_ole.
  DATA lo_docs      TYPE ole2_object.

  " Excel and word only
  IF ms_ole_info-class IS INITIAL.
    MESSAGE s012(zeui_message) WITH mv_extension INTO sy-msgli.
    zcx_eui_exception=>raise_sys_error( ).
  ENDIF.

  " Open with OLE for call a macro. Only for .docx, .docm, .xlsx, .xlsm
  IF cv_ole_app IS INITIAL.
    " Create 1 time only (or use existing)
    CREATE OBJECT cv_ole_app ms_ole_info-class.
  ENDIF.

  " Excel
  IF ms_ole_info-class = `Excel.Application`.
    GET PROPERTY OF cv_ole_app 'Workbooks' = lo_docs.
  ELSE. " Word
    GET PROPERTY OF cv_ole_app 'Documents' = lo_docs.
  ENDIF.

  CALL METHOD OF lo_docs 'Open' = cv_ole_doc
    EXPORTING
      #1 = me->mv_full_path.

  IF iv_visible = abap_true.
    SET PROPERTY OF cv_ole_app 'Visible' = 1.
  ENDIF.

  " No need ro_file = me.
ENDMETHOD.


METHOD set_full_path.
  CLEAR:
    " Do not change it!  mv_extension,
    mv_file_name,
    mv_full_path.

  " If have predifined name
  CHECK iv_full_path IS NOT INITIAL.

  DATA lv_len  TYPE i.
  DATA lv_path TYPE string.

  " Just extension
  lv_len = strlen( iv_full_path ).
  IF iv_full_path NS `.` AND lv_len <= 4.
    mv_extension = iv_full_path.
  ELSE.
    split_file_path(
     EXPORTING
       iv_fullpath  = iv_full_path
     IMPORTING
       ev_path      = lv_path
       ev_filename  = mv_file_name
       ev_extension = mv_extension ).
  ENDIF.

  " It is fullpath
  IF lv_path IS NOT INITIAL.
    mv_full_path = iv_full_path.
  ENDIF.

  " Have cases based on MC_EXTENSION
  TRANSLATE mv_extension TO LOWER CASE.

  " No need
  IF ms_ole_info IS NOT INITIAL.
    RETURN.
  ENDIF.

  " detect by extension
  IF mv_extension CP `xls*` OR mv_extension = mc_extension-csv.
    ms_ole_info-class     = `Excel.Application`.
    ms_ole_info-proxy_app = `Excel.Sheet`.
  ELSEIF mv_extension CP `doc*`.
    ms_ole_info-class     = `Word.Application`.
    ms_ole_info-proxy_app = `Word.Document`.
  ELSEIF mv_extension CP `htm*` OR mv_extension  = mc_extension-pdf.
    ms_ole_info-in_browser = abap_true.
  ENDIF.
ENDMETHOD.


METHOD split_file_path.
********************
  DEFINE set_if_requested.
    IF &1 IS REQUESTED.
      &1 = &2.
    ENDIF.
  END-OF-DEFINITION.
********************

  DATA:
    lv_ind     TYPE i,
    lv_cnt     TYPE i,
    lv_dot_pos TYPE i.

* TODO Debug
*  CLEAR:
*    ev_path,
*    ev_filename,
*    ev_file_noext,
*    ev_extension.

  " What ?
  CHECK iv_fullpath IS NOT INITIAL.

  " Prepare vars
  "  set_if_requested ev_ev_tension ''.
  lv_ind = strlen( iv_fullpath ) - 1.
  TRY.
      WHILE lv_ind > 0 AND iv_fullpath+lv_ind(1) <> '\' AND iv_fullpath+lv_ind(1) <> '/'.
        IF iv_fullpath+lv_ind(1) = '.' AND lv_dot_pos IS INITIAL. " Only 1 time
          lv_dot_pos = lv_ind + 1.
          set_if_requested ev_extension iv_fullpath+lv_dot_pos.
        ENDIF.
        lv_ind = lv_ind - 1.
        lv_cnt = sy-index.
      ENDWHILE.
    CATCH cx_sy_range_out_of_bounds.
      RETURN.
  ENDTRY.

  " Fill ev_path, ev_filename, EV_FILE_NOEXT, ev_ev_tension
  IF lv_ind = 0.
    set_if_requested ev_filename iv_fullpath.
    set_if_requested ev_path     ''.

    IF lv_dot_pos IS INITIAL.
      set_if_requested ev_file_noext iv_fullpath.
    ELSE.
      lv_cnt = lv_dot_pos - 1.
      set_if_requested ev_file_noext iv_fullpath(lv_cnt).
    ENDIF.
  ELSE.
    lv_ind = lv_ind + 1.
    set_if_requested ev_filename iv_fullpath+lv_ind(lv_cnt).
    set_if_requested ev_path     iv_fullpath(lv_ind).

    IF lv_dot_pos IS INITIAL.
      set_if_requested ev_file_noext iv_fullpath+lv_ind(lv_cnt).
    ELSE.
      lv_cnt = lv_dot_pos - lv_ind - 1.
      set_if_requested ev_file_noext iv_fullpath+lv_ind(lv_cnt).
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD to_app_server.
  DATA lv_file TYPE char200.
  lv_file = iv_full_path.

  " Downport to 7.02
  DATA lr_content TYPE REF TO data.
  FIELD-SYMBOLS <lt_content> TYPE STANDARD TABLE.
  CREATE DATA lr_content TYPE STANDARD TABLE OF ('RCGREPFILE').
  ASSIGN lr_content->* TO <lt_content>.

  DATA lv_length TYPE i.
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = mv_xstring
    IMPORTING
      output_length = lv_length
    TABLES
      binary_tab    = <lt_content>.

  sy-cprog = 'RC1TCG3Z'.
  CALL FUNCTION 'C13Z_RAWDATA_WRITE'
    EXPORTING
      i_file           = lv_file
      i_file_size      = lv_length
      i_file_overwrite = iv_overwrite
    TABLES
      i_rcgrepfile_tab = <lt_content>
    EXCEPTIONS
      no_permission    = 1
      open_failed      = 2
      ap_file_exists   = 3
      close_failed     = 4
      write_failed     = 5
      OTHERS           = 6.
  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD zif_eui_manager~pbo.
  " Initilize 1 time
  IF io_container IS NOT INITIAL.
    " In browser
    IF ms_ole_info-in_browser = abap_true.
      lcl_doi=>show_in_browser(
         io_file      = me
         io_container = io_container  ).
    ELSE. " Use doi
      lcl_doi=>show_in_doi(
       EXPORTING
         io_file      = me
         io_container = io_container
       CHANGING
         co_ole_app    = ms_ole_info-app ).
    ENDIF.
  ENDIF.

  super->pbo(
   io_container  = io_container
   iv_set_status = iv_set_status  ).
ENDMETHOD.


METHOD zif_eui_manager~show.
  " Web dynpro
  IF wdr_task=>application IS NOT INITIAL.
    IF mv_file_name IS INITIAL.
      CONCATENATE sy-datum(4) `-` sy-datum+4(2) `-` sy-datum+6(2) ` `
                  sy-uzeit(2) `-` sy-uzeit+2(2) `-` sy-uzeit+4(2) `.`
                  mv_extension
       INTO mv_file_name.
    ENDIF.

    lcl_doi=>web_dynpro_attach(
        i_filename      = mv_file_name   " File name with extension
        i_content       = me->mv_xstring
        i_inplace       = abap_true ).   " <--- Show inplace
    RETURN.
  ENDIF.

  " Oops!
  IF ms_ole_info-class IS INITIAL AND ms_ole_info-in_browser <> abap_true.
    MESSAGE s013(zeui_message) WITH mv_extension INTO sy-msgli.
    zcx_eui_exception=>raise_dump( ).
    RETURN.
  ENDIF.

  super->show(
   io_handler      = io_handler
   iv_handlers_map = iv_handlers_map ).
  " No need ro_file = me.
ENDMETHOD.
ENDCLASS.
