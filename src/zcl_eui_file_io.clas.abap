class ZCL_EUI_FILE_IO definition
  public
  inheriting from ZCL_EUI_FILE
  final
  create public .

public section.

  types:
    BEGIN OF TS_EXCEL_MAP,
     " Field of internal table
     field        TYPE fieldname,

     " Convenient for Excel
     column_name  TYPE char3,

     " More convenient for CSV. Could be filled automatically
     column_index TYPE i,

     " FM of conversion. ALPHA for exmple
     fm          TYPE string,

     " Generic type
     gen_type    TYPE string,
   END OF TS_EXCEL_MAP .
  types:
    TT_EXCEL_MAP type STANDARD TABLE OF TS_EXCEL_MAP WITH DEFAULT KEY .
  types:
    BEGIN OF s_column_fdt,
      id TYPE fdt_uuid,
      name TYPE string,
      display_name TYPE string,
      is_result   TYPE abap_bool,
      type TYPE REF TO cl_abap_datadescr,
   END OF s_column_fdt .
  types:
    t_column_fdt TYPE STANDARD TABLE OF s_column_fdt WITH DEFAULT KEY .

  events MAPPING_ERROR
    exporting
      value(IV_SOURCE) type ANY
      value(IV_ROW) type SYTABIX
      value(IS_EXCEL_MAP) type TS_EXCEL_MAP
      value(IO_ERROR) type ref to CX_ROOT
      value(CV_VALUE) type ref to DATA
      value(CS_ROW) type ref to DATA .

  methods EXPORT_TO_ITAB
    importing
      !IV_ROW_FROM type I default 1
      !IO_HANDLER type ref to OBJECT optional
      !IR_TABLE type ref to DATA
      !IT_EXCEL_MAP type ref to TT_EXCEL_MAP optional
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE
    raising
      ZCX_EUI_EXCEPTION .
  methods EXPORT_TO_ITAB_XLSX
    importing
      !IV_ROW_FROM type I default 1
      !IV_SHEET_NAME type STRING optional
      !IO_HANDLER type ref to OBJECT optional
      !IR_TABLE type ref to DATA
      !IT_EXCEL_MAP type ref to TT_EXCEL_MAP optional
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE
    raising
      ZCX_EUI_EXCEPTION .
  methods EXPORT_TO_ITAB_CSV
    importing
      !IV_ROW_FROM type I default 1
      !IV_ENCODING type ABAP_ENCODING default ZCL_EUI_CONV=>MC_ENCODING-UTF_16LE
      !IV_ROW_DELIMITER type CSEQUENCE default CL_ABAP_CHAR_UTILITIES=>CR_LF
      !IV_FIELD_DELIMITER type CSEQUENCE default CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
      !IO_HANDLER type ref to OBJECT optional
      !IR_TABLE type ref to DATA
      !IT_EXCEL_MAP type ref to TT_EXCEL_MAP optional
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE
    raising
      ZCX_EUI_EXCEPTION .
  methods IMPORT_FROM_ITAB
    importing
      !IR_TABLE type ref to DATA
      !IT_EXCEL_MAP type ref to TT_EXCEL_MAP optional
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE
    raising
      ZCX_EUI_EXCEPTION .
  methods IMPORT_FROM_ITAB_XLSX_0
    importing
      !IR_TABLE type ref to DATA
      !IV_SHEET_NAME type STRING optional
      !IT_EXCEL_MAP type ref to TT_EXCEL_MAP optional
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE .
  methods IMPORT_FROM_ITAB_XLSX_1
    importing
      !IR_TABLE type ref to DATA
      !IT_EXCEL_MAP type ref to TT_EXCEL_MAP optional
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE
    raising
      ZCX_EUI_EXCEPTION .
  methods IMPORT_FROM_ITAB_CSV
    importing
      !IR_TABLE type ref to DATA
      !IV_ENCODING type ABAP_ENCODING default ZCL_EUI_CONV=>MC_ENCODING-UTF_16LE
      !IV_ROW_DELIMITER type CSEQUENCE default CL_ABAP_CHAR_UTILITIES=>CR_LF
      !IV_FIELD_DELIMITER type CSEQUENCE default CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
      !IT_EXCEL_MAP type ref to TT_EXCEL_MAP optional
    returning
      value(RO_FILE) type ref to ZCL_EUI_FILE .
  class-methods COLUMN_2_INT
    importing
      value(IV_COLUMN) type CSEQUENCE
    returning
      value(RV_COLUMN) type I .
  class-methods INT_2_COLUMN
    importing
      value(IV_COLUMN) type I
    returning
      value(RV_COLUMN) type CHAR3
    raising
      ZCX_EUI_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EUI_FILE_IO IMPLEMENTATION.


METHOD column_2_int.
  DATA: lv_uccpi   TYPE i,
        lv_factor  TYPE i,
        lv_offset  TYPE i,
        lv_char    TYPE c,
        lr_col_ind TYPE REF TO lcl_helper=>ts_col_ind,
        ls_col_ind TYPE lcl_helper=>ts_col_ind.

*   Upper case
  TRANSLATE iv_column TO UPPER CASE.
  CONDENSE iv_column NO-GAPS.

  " For speed
  READ TABLE lcl_helper=>mt_col_ind REFERENCE INTO lr_col_ind
   WITH TABLE KEY col = iv_column.
  IF sy-subrc = 0.
    rv_column = lr_col_ind->ind.
    RETURN.
  ENDIF.

*   Get string lenght and align to right
  lv_offset = 3 - strlen( iv_column ).

  SHIFT iv_column RIGHT BY lv_offset PLACES.

*   Claculate column position
  DO 3 TIMES.
    lv_offset = sy-index - 1.
    lv_char = iv_column+lv_offset(1).
    IF lv_char IS INITIAL.
      CONTINUE.
    ENDIF.
    lv_uccpi   = cl_abap_conv_out_ce=>uccpi( lv_char ).
    lv_factor  = 26 ** ( 3 - sy-index ).
    rv_column  = rv_column + ( lv_uccpi MOD 64 ) * lv_factor.
  ENDDO.

  " Add to both tables
  CONDENSE iv_column.
  ls_col_ind-col = iv_column.
  ls_col_ind-ind = rv_column.
  INSERT ls_col_ind INTO TABLE lcl_helper=>mt_col_ind.
  INSERT ls_col_ind INTO TABLE lcl_helper=>mt_ind_col.
ENDMETHOD.


METHOD export_to_itab.
**********************************************************************
  " Import fill ITAB[]
**********************************************************************
  CASE me->mv_extension.
    WHEN mc_extension-xlsx.
      " defaults IV_SHEET_NAME
      export_to_itab_xlsx(
        iv_row_from  = iv_row_from
        io_handler   = io_handler
        ir_table     = ir_table
        it_excel_map = it_excel_map ).

    WHEN mc_extension-csv.
      " defaults IV_ENCODING, IV_ROW_DELIMITER, IV_FIELD_DELIMITER
      export_to_itab_csv(
        iv_row_from  = iv_row_from
        io_handler   = io_handler
        ir_table     = ir_table
        it_excel_map = it_excel_map ).

    WHEN OTHERS.
      MESSAGE s010(zeui_message) WITH me->mv_extension INTO sy-msgli.
      zcx_eui_exception=>raise_sys_error( ).
  ENDCASE.

  ro_file = me.
ENDMETHOD.


METHOD export_to_itab_csv.
**********************************************************************
  " Create mapping
**********************************************************************
  DATA lr_excel_map LIKE it_excel_map.
  lcl_helper=>fill_mapping(
   EXPORTING
    ir_table     = ir_table
    ir_excel_map = it_excel_map
   IMPORTING
    er_excel_map = lr_excel_map ).

**********************************************************************
  " Fill raw table from ME->MV_FILE_NAME
  DATA lr_source TYPE REF TO data.
**********************************************************************

  DATA lv_max_column      TYPE i VALUE 0.
  DATA lv_name            TYPE string.
  DATA lt_comp            TYPE abap_component_tab.
  DATA lo_struc           TYPE REF TO cl_abap_structdescr.
  DATA lo_table           TYPE REF TO cl_abap_tabledescr.
  FIELD-SYMBOLS <ls_comp> LIKE LINE OF lt_comp.

  " Find max column
  FIELD-SYMBOLS <ls_excel_map> TYPE ts_excel_map.
  LOOP AT lr_excel_map->* ASSIGNING <ls_excel_map>.
    IF lv_max_column < <ls_excel_map>-column_index.
      lv_max_column = <ls_excel_map>-column_index.
    ENDIF.
  ENDLOOP.

  " What ?
  CHECK lv_max_column > 0.

  " Create structure for import
  DO lv_max_column TIMES.
    " New field name
    lv_name = sy-index.
    CONDENSE lv_name.
    CONCATENATE 'FLD_' lv_name INTO lv_name.

    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = lv_name. "confusing int_2_column( sy-index ).
    <ls_comp>-type ?= cl_abap_elemdescr=>get_string( ).
  ENDDO.

  " Create table
  lo_struc = cl_abap_structdescr=>create( lt_comp ).
  lo_table = cl_abap_tabledescr=>create(  p_line_type = lo_struc ).

  " Sorce
  FIELD-SYMBOLS <lt_source>  TYPE STANDARD TABLE.

  CREATE DATA lr_source TYPE HANDLE lo_table.
  ASSIGN lr_source->* TO <lt_source>.

**********************************************************************
  " Fill source
**********************************************************************
  DATA           lv_document    TYPE string.
  DATA           lt_row         TYPE stringtab.
  DATA           lt_field       TYPE stringtab.
  DATA           lv_len         TYPE i.
  FIELD-SYMBOLS  <lv_row>       LIKE LINE OF lt_row.
  FIELD-SYMBOLS  <lv_field>     LIKE LINE OF lt_field.
  FIELD-SYMBOLS  <ls_source>    TYPE any.
  FIELD-SYMBOLS  <ls_src_field> TYPE any.

  " WAS cl_gui_frontend_services=>gui_upload( filetype = 'DAT')
  lv_document = zcl_eui_conv=>xstring_to_string(
    iv_xstring  = me->mv_xstring
    iv_encoding = iv_encoding
  ).

  " All rows
  SPLIT lv_document AT iv_row_delimiter INTO TABLE lt_row.
  LOOP AT lt_row ASSIGNING <lv_row>.
    CLEAR lt_field.
    SPLIT <lv_row> AT iv_field_delimiter INTO TABLE lt_field.

    APPEND INITIAL LINE TO <lt_source> ASSIGNING <ls_source>.
    LOOP AT lt_field ASSIGNING <lv_field>.
      ASSIGN COMPONENT sy-tabix OF STRUCTURE <ls_source> TO <ls_src_field>.

      " No more fields
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      " TODO in qutes ?
      <ls_src_field> = <lv_field>.

      " Manage quotes
      CHECK <ls_src_field> CP `"*"`.

      " Delete first and last char
      lv_len = strlen( <ls_src_field> ) - 2.
      <ls_src_field> = <ls_src_field>+1(lv_len).

      " In CSV "" -> "
      REPLACE ALL OCCURRENCES OF `""` IN <ls_src_field> WITH `"`.
    ENDLOOP.
  ENDLOOP.

**********************************************************************
  " And import from lr_source
**********************************************************************
  lcl_helper=>export_2_table(
    ir_table     = ir_table
    io_handler   = io_handler
    ir_source    = lr_source
    iv_row_from  = iv_row_from
    io_file      = me " for messages only
    ir_excel_map = lr_excel_map ).

  ro_file = me.
ENDMETHOD.


METHOD export_to_itab_xlsx.
  "NOTE: No cl_fdt_xl_spreadsheet in ABAP 7.01
**********************************************************************
  " Create mapping
**********************************************************************
  DATA lr_excel_map LIKE it_excel_map.
  lcl_helper=>fill_mapping(
   EXPORTING
    ir_table     = ir_table
    ir_excel_map = it_excel_map
   IMPORTING
    er_excel_map = lr_excel_map ).

**********************************************************************
  " Fill raw table from ME->MV_FILE_NAME
  DATA lr_source     TYPE REF TO data.
  DATA lo_excel TYPE REF TO object. " if_fdt_doc_spreadsheet.
  DATA lv_sheet_name LIKE iv_sheet_name.
  DATA lt_worksheet TYPE stringtab. " if_fdt_doc_spreadsheet=>t_worksheet_names.

  " Create XLSX reader
  TRY.
      CREATE OBJECT lo_excel TYPE ('CL_FDT_XL_SPREADSHEET')
        EXPORTING
          document_name = me->mv_file_name
          xdocument     = me->mv_xstring.
    CATCH cx_sy_create_error.
      zcx_eui_exception=>raise_dump( iv_message = 'Minimum version 7.02!' ).
  ENDTRY.

  " Get all sheet names
  CALL METHOD lo_excel->('IF_FDT_DOC_SPREADSHEET~GET_WORKSHEET_NAMES')
    IMPORTING
      worksheet_names = lt_worksheet.

  " TODO exception
  lv_sheet_name = iv_sheet_name.
  IF lv_sheet_name IS NOT INITIAL OR lt_worksheet IS INITIAL.
    READ TABLE lt_worksheet TRANSPORTING NO FIELDS
     WITH KEY table_line = lv_sheet_name.
    IF sy-subrc <> 0.
      MESSAGE s001(zeui_message) WITH lv_sheet_name mv_file_name INTO sy-msgli.
      zcx_eui_exception=>raise_sys_error( ).
    ENDIF.
  ELSE.
    IF lines( lt_worksheet ) > 1.
      MESSAGE s002(zeui_message) WITH mv_file_name INTO sy-msgli.
      zcx_eui_exception=>raise_sys_error( ).
    ENDIF.

    " Get the firsy one
    READ TABLE lt_worksheet INTO lv_sheet_name INDEX 1.
  ENDIF.

  CALL METHOD lo_excel->('IF_FDT_DOC_SPREADSHEET~GET_ITAB_FROM_WORKSHEET')
    EXPORTING
      worksheet_name = lv_sheet_name
    RECEIVING
      itab           = lr_source.

**********************************************************************
  " And import from lr_source
**********************************************************************
  lcl_helper=>export_2_table(
   EXPORTING
    ir_table     = ir_table
    io_handler   = io_handler
    ir_source    = lr_source
    iv_row_from  = iv_row_from
    io_file      = me " for messages only
    ir_excel_map = lr_excel_map ).

  ro_file = me.
ENDMETHOD.


METHOD import_from_itab.
**********************************************************************
  " Generate new file
**********************************************************************
  CASE me->mv_extension.
      " For more complex reports use XTT
      " @see -> https://github.com/bizhuka/xtt/wiki
    WHEN mc_extension-xlsx.
      TRY.
          ro_file = import_from_itab_xlsx_1(  " > 7.02 ?
             ir_table     = ir_table
             it_excel_map = it_excel_map ).
        CATCH zcx_eui_exception.
          ro_file = import_from_itab_xlsx_0(  " = 7.02
             " iv_sheet_name = `testOk`
             ir_table     = ir_table
             it_excel_map = it_excel_map ).
      ENDTRY.

    WHEN mc_extension-csv.
      " defaults IV_ENCODING, IV_ROW_DELIMITER, IV_FIELD_DELIMITER
      ro_file = import_from_itab_csv(
        ir_table     = ir_table
        it_excel_map = it_excel_map ).

    WHEN OTHERS.
      MESSAGE s011(zeui_message) WITH me->mv_extension INTO sy-msgli.
      zcx_eui_exception=>raise_sys_error( ).
  ENDCASE.
ENDMETHOD.


METHOD import_from_itab_csv.
  DATA lt_fieldcat             TYPE lvc_t_fcat.
  DATA lv_result               TYPE string.
  DATA lv_string               TYPE string.
  DATA lv_char                 TYPE text10.
  FIELD-SYMBOLS <ls_fieldcat>  LIKE LINE OF lt_fieldcat.
  FIELD-SYMBOLS <lt_table>     TYPE STANDARD TABLE.
  FIELD-SYMBOLS <ls_row>       TYPE any.
  FIELD-SYMBOLS <lv_value>     TYPE any.

  " Fill mapping
  lcl_helper=>fill_mapping(
   EXPORTING
    ir_table     = ir_table
    ir_excel_map = it_excel_map
   IMPORTING
    et_fieldcat  = lt_fieldcat ).

  LOOP AT lt_fieldcat ASSIGNING <ls_fieldcat>.
    IF lv_result IS INITIAL.
      lv_result = <ls_fieldcat>-reptext.
    ELSE.
      CONCATENATE lv_result
                    iv_field_delimiter <ls_fieldcat>-reptext INTO lv_result.
    ENDIF.
  ENDLOOP.

  ASSIGN ir_table->* TO <lt_table>.
  LOOP AT <lt_table> ASSIGNING <ls_row>.
    " New line
    CONCATENATE lv_result iv_row_delimiter INTO lv_result.

    LOOP AT lt_fieldcat ASSIGNING <ls_fieldcat>.
      " Add delimeter
      IF sy-tabix <> 1.
        CONCATENATE lv_result iv_field_delimiter INTO lv_result.
      ENDIF.

      ASSIGN COMPONENT <ls_fieldcat>-fieldname OF STRUCTURE <ls_row> TO <lv_value>.

      " TODO other type?
      IF <ls_fieldcat>-inttype = cl_abap_typedescr=>typekind_date OR <ls_fieldcat>-inttype = cl_abap_typedescr=>typekind_time.
        WRITE <lv_value> TO lv_char.
        CONCATENATE lv_result lv_char INTO lv_result.
      ELSE.
        lv_string = <lv_value>.

        " Manage `"`
        IF lv_string CS `"`.
          REPLACE ALL OCCURRENCES OF `"` IN lv_string WITH `""`.
          CONCATENATE `"` lv_string `"` INTO lv_string.
        ENDIF.

        CONCATENATE lv_result lv_string INTO lv_result.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  ro_file = me->import_from_string(
     iv_string    = lv_result
     iv_encoding  = iv_encoding ).
ENDMETHOD.


METHOD import_from_itab_xlsx_0.
  "NOTE: No cl_fdt_xl_spreadsheet in ABAP 7.01
  DATA lt_fieldcat    TYPE lvc_t_fcat.
  DATA ls_fieldcat    TYPE REF TO lvc_s_fcat.
  DATA lt_column      TYPE REF TO data.  " if_fdt_doc_spreadsheet=>t_column.
  DATA ls_column      TYPE s_column_fdt. " REF TO if_fdt_doc_spreadsheet=>s_column.
  DATA lv_result      TYPE xstring.
  DATA lv_sheets      TYPE string.
  FIELD-SYMBOLS <lt_column> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <ls_column> TYPE any.

  " Fill mapping
  lcl_helper=>fill_mapping(
   EXPORTING
    ir_table     = ir_table
    ir_excel_map = it_excel_map
   IMPORTING
    et_fieldcat  = lt_fieldcat ).

  TRY.
      CREATE DATA lt_column TYPE ('IF_FDT_DOC_SPREADSHEET=>T_COLUMN').
      ASSIGN lt_column->* TO <lt_column>.
    CATCH cx_sy_create_error.
      zcx_eui_exception=>raise_dump( iv_message = 'Minimum version 7.02!' ).
  ENDTRY.

  " List of columns
  LOOP AT lt_fieldcat REFERENCE INTO ls_fieldcat WHERE rollname IS NOT INITIAL.
    ls_column-id           = sy-tabix.
    ls_column-name         = ls_fieldcat->fieldname.
    ls_column-display_name = ls_fieldcat->reptext.
    ls_column-is_result    = abap_true.
    ls_column-type         ?= cl_abap_typedescr=>describe_by_name( ls_fieldcat->rollname ).

    " And add
    APPEND INITIAL LINE TO <lt_column> ASSIGNING <ls_column>.
    MOVE-CORRESPONDING ls_column TO <ls_column>.
  ENDLOOP.

  DATA lo_err TYPE REF TO cx_sy_dyn_call_error.
  TRY.
      CALL METHOD ('CL_FDT_XL_SPREADSHEET')=>('IF_FDT_DOC_SPREADSHEET~CREATE_DOCUMENT')
        EXPORTING
          columns      = <lt_column>
          itab         = ir_table
          iv_call_type = 1 " 7.02 if_fdt_doc_spreadsheet=>gc_call_dec_table
        RECEIVING
          xdocument    = lv_result.
    CATCH cx_sy_dyn_call_error INTO lo_err.
      " 7.02
      CALL METHOD ('CL_FDT_XL_SPREADSHEET')=>('IF_FDT_DOC_SPREADSHEET~CREATE_DOCUMENT')
        EXPORTING
          columns     = <lt_column>
          itab        = ir_table
          is_dt_excel = abap_false
        RECEIVING
          xdocument   = lv_result.
  ENDTRY.

**********************************************************************
  " Delete Sheet2 - Sheet4
**********************************************************************
  " Load zip archive from XSTRING
  DATA lo_zip       TYPE REF TO cl_abap_zip.
  DATA ls_zip_file  TYPE REF TO cl_abap_zip=>t_file.
  DATA ls_doc       TYPE string.

  CREATE OBJECT lo_zip.
  lo_zip->load( lv_result ).

  zcl_eui_conv=>xml_from_zip(
   EXPORTING
    io_zip    = lo_zip
    iv_name   = `xl/workbook.xml`
   IMPORTING
    ev_sdoc   = ls_doc ).

  " Set sheet name
  IF iv_sheet_name IS INITIAL.
    lv_sheets = `Sheet1`.
  ELSE.
    lv_sheets = iv_sheet_name.
  ENDIF.
  CONCATENATE `<sheets><sheet name= "` lv_sheets  `" sheetId="1" r:id="rId1"/></sheets>` INTO lv_sheets.

  " Only 1 sheet
  REPLACE FIRST OCCURRENCE OF REGEX
    `(<sheets>).*(</sheets>)` IN ls_doc WITH
    lv_sheets.

  IF sy-subrc = 0.
    " Update workbook XML
    zcl_eui_conv=>xml_to_zip(
       io_zip  = lo_zip
       iv_name = `xl/workbook.xml`
       iv_sdoc = ls_doc  ).

    " Find all sheets
    LOOP AT lo_zip->files REFERENCE INTO ls_zip_file WHERE
        name CP `xl/worksheets/sheet*.xml` AND name <> `xl/worksheets/sheet1.xml`.
      lo_zip->delete(
       EXPORTING
         name   = ls_zip_file->name
       EXCEPTIONS
         OTHERS = 1 ).
    ENDLOOP.

    " Return new ZIP
    lv_result = lo_zip->save( ).
  ENDIF.

  ro_file = import_from_xstring( lv_result ).
ENDMETHOD.


METHOD import_from_itab_xlsx_1.
  DATA lt_fieldcat TYPE lvc_t_fcat.
  DATA lo_salv_ex_res TYPE REF TO cl_salv_ex_result_data_table. " object ?

  " Fill mapping
  lcl_helper=>fill_mapping(
   EXPORTING
    ir_table     = ir_table
    ir_excel_map = it_excel_map
   IMPORTING
    et_fieldcat  = lt_fieldcat ).

  TRY.
      CALL METHOD ('CL_SALV_EX_UTIL')=>('FACTORY_RESULT_DATA_TABLE')
        EXPORTING
          r_data              = ir_table
          t_fieldcatalog      = lt_fieldcat
        RECEIVING
          r_result_data_table = lo_salv_ex_res.

      " 7.02
      CALL METHOD ('CL_SALV_BS_LEX')=>('EXPORT_FROM_RESULT_DATA_TABLE')
        EXPORTING
          is_format            = 'xlsx' " if_salv_bs_lex_format=>mc_format_xlsx
          ir_result_data_table = lo_salv_ex_res
        IMPORTING
          er_result_file       = me->mv_xstring.
    CATCH cx_sy_dyn_call_error.
      CLEAR me->mv_xstring.
      zcx_eui_exception=>raise_sys_error( iv_message = `Old SAP version` ).
  ENDTRY.

  ro_file = me.
ENDMETHOD.


METHOD int_2_column.
  DATA:
    lr_col_ind TYPE REF TO lcl_helper=>ts_col_ind,
    ls_col_ind TYPE lcl_helper=>ts_col_ind,
    lv_module  TYPE i,
    lv_uccpi   TYPE i,
    lv_text    TYPE sychar02.

  " For speed
  READ TABLE lcl_helper=>mt_ind_col REFERENCE INTO lr_col_ind
   WITH TABLE KEY ind = iv_column.
  IF sy-subrc = 0.
    rv_column = lr_col_ind->col.
    RETURN.
  ENDIF.

  IF iv_column > 16384 OR iv_column < 1.
    MESSAGE s004(zeui_message) WITH iv_column INTO sy-msgli.
    zcx_eui_exception=>raise_sys_error( ).
  ENDIF.

  ls_col_ind-ind = iv_column.
  WHILE iv_column GT 0.
    lv_module = ( iv_column - 1 ) MOD 26.
    lv_uccpi  = 65 + lv_module.

    iv_column = ( iv_column - lv_module ) / 26.

    lv_text   = cl_abap_conv_in_ce=>uccpi( lv_uccpi ).
    CONCATENATE lv_text rv_column INTO rv_column.
  ENDWHILE.

  " Add to both tables
  ls_col_ind-col = rv_column.
  INSERT ls_col_ind INTO TABLE lcl_helper=>mt_col_ind.
  INSERT ls_col_ind INTO TABLE lcl_helper=>mt_ind_col.
ENDMETHOD.
ENDCLASS.
