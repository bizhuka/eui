class ZCL_EUI_CONV definition
  public
  final
  create public .

public section.
  type-pools ABAP .
  type-pools JS .

  types ABAP_ENCODING type CHAR20 .

  constants:
    BEGIN OF mc_encoding,
      win_1251 TYPE abap_encoding VALUE '1504',
      utf_8    TYPE abap_encoding VALUE '4110',
      utf_16be TYPE abap_encoding VALUE '4102',
      utf_16le TYPE abap_encoding VALUE '4103',
    END OF mc_encoding .
  constants:
    BEGIN OF mc_json_mode,
      standard TYPE string VALUE '1-2',
      safe     TYPE string VALUE '2-1',
    END OF mc_json_mode .

  class-methods MOVE_CORRESPONDING
    importing
      !IS_SOURCE type ANY
      !IV_EXCEPT type CSEQUENCE optional
      !IV_EXCEPT_INITIAL type ABAP_BOOL optional
    changing
      !CS_DESTINATION type ANY
      !CT_COMPONENT type ABAP_COMPDESCR_TAB optional .
  class-methods BINARY_TO_STRING
    importing
      !IT_TABLE type STANDARD TABLE
      !IV_LENGTH type I
      !IV_ENCODING type ABAP_ENCODING default MC_ENCODING-UTF_8
    returning
      value(RV_STRING) type STRING .
  class-methods BINARY_TO_XSTRING
    importing
      !IT_TABLE type STANDARD TABLE
      !IV_LENGTH type I
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods XSTRING_TO_BINARY
    importing
      !IV_XSTRING type XSTRING
    exporting
      !EV_LENGTH type I
      !ET_TABLE type SOLIX_TAB .
  class-methods XSTRING_TO_BASE64
    importing
      !IV_XSTRING type XSTRING
    returning
      value(RV_BASE64) type STRING .
  class-methods XSTRING_TO_STRING
    importing
      !IV_XSTRING type XSTRING
      !IV_ENCODING type ABAP_ENCODING default MC_ENCODING-UTF_8
    returning
      value(RV_STRING) type STRING .
  class-methods STRING_TO_XSTRING
    importing
      !IV_STRING type STRING
      !IV_ENCODING type ABAP_ENCODING default MC_ENCODING-UTF_8
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods STRING_TO_TEXT_TABLE
    importing
      !IV_STRING type STRING
    exporting
      !ET_TEXT type TABLE
      !EV_LENGTH type I .
  class-methods XML_TO_STR
    importing
      !IO_DOC type ref to IF_IXML_DOCUMENT
    exporting
      !EV_STR type STRING
      !EV_XSTR type XSTRING .
  class-methods STR_TO_XML
    importing
      !IV_STR type STRING optional
      !IV_XSTR type XSTRING optional
    returning
      value(RO_DOC) type ref to IF_IXML_DOCUMENT .
  class-methods XML_FROM_ZIP
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IV_NAME type CSEQUENCE
    exporting
      !EO_XMLDOC type ref to IF_IXML_DOCUMENT
      !EV_SDOC type STRING .
  class-methods XML_TO_ZIP
    importing
      !IO_ZIP type ref to CL_ABAP_ZIP
      !IV_NAME type STRING
      !IV_XDOC type XSTRING optional
      !IV_SDOC type STRING optional
      !IO_XMLDOC type ref to IF_IXML_DOCUMENT optional .
  class-methods TO_JSON
    importing
      !IM_DATA type ANY
      !IV_PURE type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_JSON) type STRING .
  class-methods FROM_JSON
    importing
      !IV_JSON type STRING
      !IV_MODE type STRING default MC_JSON_MODE-STANDARD
    exporting
      !EX_DATA type ANY
      !EV_OK type ABAP_BOOL .
  class-methods GET_GRID_FROM_SALV
    importing
      !IO_SALV type ref to CL_SALV_MODEL_LIST
    returning
      value(RO_GUI_ALV) type ref to CL_GUI_ALV_GRID .
  class-methods GET_GRID_TABLE
    importing
      !IO_ALV type ref to CL_GUI_ALV_GRID
    returning
      value(RR_TABLE) type ref to DATA .
  class-methods GUID_CREATE
    returning
      value(RV_GUID) type GUID_32 .
  class-methods ASSERT_EQUALS
    importing
      !EXP type ANY
      !ACT type ANY
      !MSG type CSEQUENCE optional
      !LEVEL type AUNIT_LEVEL default IF_AUNIT_CONSTANTS=>CRITICAL
      !TOL type F optional
      !QUIT type AUNIT_FLOWCTRL default IF_AUNIT_CONSTANTS=>METHOD
      !IGNORE_HASH_SEQUENCE type ABAP_BOOL default ABAP_FALSE
    returning
      value(ASSERTION_FAILED) type ABAP_BOOL .
  class-methods ASSERT_DIFFERS
    importing
      !EXP type SIMPLE
      !ACT type SIMPLE
      !MSG type CSEQUENCE optional
      !LEVEL type AUNIT_LEVEL default IF_AUNIT_CONSTANTS=>CRITICAL
      !TOL type F optional
      !QUIT type AUNIT_FLOWCTRL default IF_AUNIT_CONSTANTS=>METHOD
    returning
      value(ASSERTION_FAILED) type ABAP_BOOL .
protected section.
private section.

  class-methods _ABAP_2_JSON
    importing
      !IM_DATA type DATA
      !IV_NAME type STRING optional
    returning
      value(RV_JSON) type STRING .
  class-methods _JSON_2_ABAP
    importing
      !JSON_STRING type STRING optional
      !VAR_NAME type STRING optional
      !PROPERTY_PATH type STRING default 'json_obj'
    exporting
      value(PROPERTY_TABLE) type JS_PROPERTY_TAB
    changing
      !JS_OBJECT type ref to CL_JAVA_SCRIPT optional
      value(ABAP_DATA) type ANY optional
    raising
      ZCX_EUI_EXCEPTION .
ENDCLASS.



CLASS ZCL_EUI_CONV IMPLEMENTATION.


METHOD assert_differs.
  " Could use assert ?
  DATA lv_class_name TYPE seoclass-clsname.
  lv_class_name = lcl_assert_util=>get_class_name( ).
  CHECK lv_class_name IS NOT INITIAL.

  CALL METHOD (lv_class_name)=>assert_differs
    EXPORTING
      exp              = exp
      act              = act
      msg              = msg
      level            = level
      tol              = tol
      quit             = quit
    RECEIVING
      assertion_failed = assertion_failed.
ENDMETHOD.


METHOD assert_equals.
  " Could use assert ?
  DATA lv_class_name TYPE seoclass-clsname.
  lv_class_name = lcl_assert_util=>get_class_name( ).
  CHECK lv_class_name IS NOT INITIAL.

  CALL METHOD (lv_class_name)=>assert_equals
    EXPORTING
      exp                  = exp
      act                  = act
      msg                  = msg
      level                = level
      tol                  = tol
      quit                 = quit
      ignore_hash_sequence = ignore_hash_sequence
    RECEIVING
      assertion_failed     = assertion_failed.
ENDMETHOD.


METHOD binary_to_string.
  TRY.
      CALL FUNCTION 'SCMS_BINARY_TO_STRING'
        EXPORTING
          input_length = iv_length
          encoding     = iv_encoding
        IMPORTING
          text_buffer  = rv_string
        TABLES
          binary_tab   = it_table.
    CATCH cx_sy_dyn_call_error. " TODO CONV classes?
      CALL FUNCTION 'SCMS_BINARY_TO_STRING'
        EXPORTING
          input_length = iv_length
        IMPORTING
          text_buffer  = rv_string
        TABLES
          binary_tab   = it_table.
  ENDTRY.
ENDMETHOD.


METHOD BINARY_TO_XSTRING.
  " cl_bcs_convert=>solix_to_xstring( )
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = iv_length
    IMPORTING
      buffer       = rv_xstring
    TABLES
      binary_tab   = it_table.
ENDMETHOD.


METHOD from_json.
  CLEAR:
   ev_ok,
   ex_data.

  " No need. Always have {"DATA":} ?
  CHECK iv_json IS NOT INITIAL.

  " Detect order
  DATA lt_mode TYPE stringtab.
  SPLIT iv_mode AT '-' INTO TABLE lt_mode.

  DATA lv_mode TYPE string.
  LOOP AT lt_mode INTO lv_mode.
    CASE lv_mode.
      " in most cases it works fine regardless the fact that in system there is no if_sxml=>co_xt_json declared
      WHEN '1'.
        TRY.
            CALL TRANSFORMATION id SOURCE XML iv_json
                                   RESULT data = ex_data.
            " Ok
            ev_ok = abap_true.
          CATCH cx_transformation_error.
            ev_ok = abap_false.
        ENDTRY.

      " For version ABAP 7.02 patch level 006
      WHEN '2'.
        TRY.
            _json_2_abap(
             EXPORTING
              json_string = iv_json
              var_name    = 'DATA'
             CHANGING
              abap_data   = ex_data ).

            " Ok
            ev_ok = abap_true.
          CATCH zcx_eui_exception.
            ev_ok = abap_false.
        ENDTRY.
    ENDCASE.

    " All done
    IF ev_ok = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  " Is not procced in caller!
  IF ev_ok <> abap_true AND ev_ok IS NOT REQUESTED.
    " Jump to code below to get file
    zcx_eui_exception=>raise_dump( iv_message = `Error in 'IV_JSON'` ). "#EC NOTEXT

    " For debug
    DATA lo_file TYPE REF TO zcl_eui_file.
    lo_file->import_from_string( iv_json ).
    TRY.
        lo_file->download( iv_full_path   = 'dump_json.txt'
                           iv_save_dialog = abap_true ).
        lo_file->open( ).
      CATCH zcx_eui_exception.
    ENDTRY.
  ENDIF.
ENDMETHOD.


METHOD get_grid_from_salv.
  DATA lo_error TYPE REF TO cx_salv_msg.

  TRY.
      ro_gui_alv = lcl_salv_util=>_get_grid_from_salv( io_salv ).
    CATCH cx_salv_msg INTO lo_error.
      MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMETHOD.


METHOD get_grid_table.
  rr_table = lcl_grid_util=>_get_grid_table( io_alv ).
ENDMETHOD.


METHOD guid_create.
  " № 1
  TRY.
      CALL METHOD ('CL_SYSTEM_UUID')=>('IF_SYSTEM_UUID_STATIC~CREATE_UUID_C32')
        RECEIVING
          uuid = rv_guid.
    CATCH cx_root.                                       "#EC CATCH_ALL
      CLEAR rv_guid.
  ENDTRY.
  CHECK rv_guid IS INITIAL.

  " № 2
  TRY.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_32 = rv_guid.
    CATCH cx_root.                                       "#EC CATCH_ALL
      CLEAR rv_guid.
  ENDTRY.
  CHECK rv_guid IS INITIAL.

  CONCATENATE sy-datum(4) `-` sy-datum+4(2) `-` sy-datum+6(2) ` `
              sy-uzeit(2) `-` sy-uzeit+2(2) `-` sy-uzeit+4(2) INTO rv_guid.
ENDMETHOD.


METHOD MOVE_CORRESPONDING.
  DATA:
    lo_struc  TYPE REF TO cl_abap_structdescr,
    lt_except TYPE STANDARD TABLE OF fieldname WITH DEFAULT KEY.
  FIELD-SYMBOLS:
    <ls_component> LIKE LINE OF ct_component,
    <lv_src>       TYPE any,
    <lv_dest>      TYPE any.

  " Could be slow in the loops
  IF ct_component IS INITIAL.
    lo_struc ?= cl_abap_typedescr=>describe_by_data( is_source ).
    ct_component = lo_struc->components.
  ENDIF.

  " Just iqnore fields. No need to optimize
  IF iv_except IS NOT INITIAL.
    SPLIT iv_except AT ';' INTO TABLE lt_except.
    SORT lt_except BY table_line.
  ENDIF.

  LOOP AT ct_component ASSIGNING <ls_component>.
    " №1 - ignore
    IF iv_except IS NOT INITIAL.
      READ TABLE lt_except TRANSPORTING NO FIELDS BINARY SEARCH
       WITH KEY table_line = <ls_component>-name.
      CHECK sy-subrc <> 0.
    ENDIF.

    ASSIGN COMPONENT:
     <ls_component>-name OF STRUCTURE is_source      TO <lv_src>,
     <ls_component>-name OF STRUCTURE cs_destination TO <lv_dest>.
    CHECK sy-subrc = 0.

    " №2 - ignore
    IF iv_except_initial = abap_true.
      CHECK <lv_src> IS NOT INITIAL.
    ENDIF.

    <lv_dest> = <lv_src>.
  ENDLOOP.
ENDMETHOD.


METHOD string_to_text_table.
  " cl_bcs_convert=>string_to_soli( ).
  CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
    EXPORTING
      text      = iv_string
    IMPORTING
      length    = ev_length
    TABLES
      ftext_tab = et_text.
ENDMETHOD.


METHOD STRING_TO_XSTRING.
  " rv_xstring = cl_bcs_convert=>string_to_xstring( iv_string = iv_string iv_codepage = IV_ENCODING ).
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text     = iv_string
      encoding = iv_encoding
    IMPORTING
      buffer   = rv_xstring.
ENDMETHOD.


METHOD STR_TO_XML.
  DATA:
    lo_xml     TYPE REF TO if_ixml,
    lo_factory TYPE REF TO if_ixml_stream_factory,
    lo_stream  TYPE REF TO if_ixml_istream,
    lo_parser  TYPE REF TO if_ixml_parser.

  lo_xml      = cl_ixml=>create( ).
  lo_factory  = lo_xml->create_stream_factory( ).

  " From xstring
  IF iv_xstr IS SUPPLIED.
    lo_stream   = lo_factory->create_istream_xstring( iv_xstr ).
  ELSEIF iv_str IS SUPPLIED. " From string
    lo_stream   = lo_factory->create_istream_cstring( iv_str ).
  ELSE.
    RETURN.
  ENDIF.

  " Parse a document and return re_doc
  ro_doc = lo_xml->create_document( ).
  lo_parser = lo_xml->create_parser(
    stream_factory  = lo_factory
    istream         = lo_stream
    document        = ro_doc ).
  lo_parser->set_normalizing( ).
  lo_parser->set_validating( mode = if_ixml_parser=>co_no_validation ).
  lo_parser->parse( ).
ENDMETHOD.


METHOD to_json.
  DATA:
    lv_end TYPE i,
    lv_beg TYPE i.

  rv_json = _abap_2_json( im_data = im_data iv_name = 'DATA' ).
  CONCATENATE `{` rv_json `}` INTO rv_json.

  " delete surroundin DATA
  IF iv_pure = abap_true.
    lv_end = strlen( rv_json ).

    IF rv_json(9) CP `{"DATA":"`.
      lv_beg = 9.
      lv_end = lv_end - 11.
    ELSE.
      lv_beg = 8.
      lv_end = lv_end - 9.
    ENDIF.

    rv_json = rv_json+lv_beg(lv_end).
  ENDIF.
ENDMETHOD.


METHOD XML_FROM_ZIP.
  DATA:
    lv_value TYPE xstring.

  " As a string
  IF ev_sdoc IS REQUESTED.
    CLEAR ev_sdoc.
  ENDIF.

  " As an object
  IF eo_xmldoc IS REQUESTED.
    CLEAR eo_xmldoc.
  ENDIF.

  " Try to read the document from archive
  io_zip->get(
   EXPORTING
    name                    = iv_name
   IMPORTING
    content                 = lv_value
   EXCEPTIONS
    OTHERS                  = 1 ).
  CHECK sy-subrc = 0.

  " As a string
  IF ev_sdoc IS REQUESTED.
    ev_sdoc = xstring_to_string( lv_value ).
  ENDIF.

  " As an object
  IF eo_xmldoc IS REQUESTED.
    eo_xmldoc = str_to_xml( iv_xstr = lv_value ).
  ENDIF.
ENDMETHOD.


METHOD XML_TO_STR.
  DATA:
    lo_xml     TYPE REF TO if_ixml,
    lo_encode  TYPE REF TO if_ixml_encoding,
    lo_factory TYPE REF TO if_ixml_stream_factory,
    lo_stream  TYPE REF TO if_ixml_ostream,
    lo_rendr   TYPE REF TO if_ixml_renderer.

  lo_xml    = cl_ixml=>create( ).

*    In a different encoding
*    IF im_charset IS NOT INITIAL.
  lo_encode = lo_xml->create_encoding( byte_order    = if_ixml_encoding=>co_platform_endian
                                       character_set = 'UTF-8' ).
  io_doc->set_encoding( lo_encode ).
*    ENDIF.

  lo_factory = lo_xml->create_stream_factory( ).

  " Return a xtring
  IF ev_xstr IS REQUESTED.
    CLEAR ev_xstr.
    lo_stream = lo_factory->create_ostream_xstring( string = ev_xstr ).
  ELSEIF ev_str IS REQUESTED. " Return a string
    CLEAR ev_str.
    lo_stream = lo_factory->create_ostream_cstring( string = ev_str ).
  ELSE.
    RETURN.
  ENDIF.

  lo_rendr = lo_xml->create_renderer( ostream  = lo_stream document = io_doc ).
  lo_rendr->render( ).

  " Change encoding. still utf-16 in header
  IF ev_str IS REQUESTED.
    REPLACE FIRST OCCURRENCE OF 'utf-16' IN ev_str WITH 'UTF-8'.
  ENDIF.
ENDMETHOD.


METHOD XML_TO_ZIP.
  DATA:
   lv_value TYPE xstring.

  " From string
  IF iv_xdoc IS SUPPLIED.
    lv_value = iv_xdoc.
  ELSEIF iv_sdoc IS SUPPLIED.
    " Transform string to xString
    lv_value = string_to_xstring( iv_sdoc ).
  ELSEIF io_xmldoc IS SUPPLIED. " From object
    " Transform document to xString
    xml_to_str(
     EXPORTING
       io_doc     = io_xmldoc
     IMPORTING
       ev_xstr    = lv_value ).
  ELSE.
    RETURN.
  ENDIF.

  " Delete from ZIP
  io_zip->delete( EXPORTING name = iv_name EXCEPTIONS OTHERS = 1 ).

  " Add to ZIP
  io_zip->add( name = iv_name content = lv_value ).
ENDMETHOD.


METHOD xstring_to_base64.
  TRY.
      CALL METHOD ('CL_HTTP_UTILITY')=>('ENCODE_X_BASE64')
        EXPORTING
          unencoded = iv_xstring
        RECEIVING
          encoded   = rv_base64.
    CATCH cx_sy_dyn_call_error.
      " 7.00 doesn't have cl_http_utility=>encode_x_base64( ).
      DATA lv_error TYPE i.
      SYSTEM-CALL ict  "#EC CI_SYSTEMCALL
        DID
          86 " ihttp_scid_base64_escape_x
        PARAMETERS
          iv_xstring
          rv_base64
          lv_error.                      " < return code
  ENDTRY.
ENDMETHOD.


METHOD XSTRING_TO_BINARY.
  " et_table = cl_bcs_convert=>xstring_to_solix( iv_xstring ).
  " ev_length = xstrlen( iv_xstring ).
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = iv_xstring
    IMPORTING
      output_length = ev_length
    TABLES
      binary_tab    = et_table.
ENDMETHOD.


METHOD XSTRING_TO_STRING.
  DATA:
    lo_conv  TYPE REF TO cl_abap_conv_in_ce.

  lo_conv = cl_abap_conv_in_ce=>create(
   encoding = iv_encoding
   input    = iv_xstring ).

  lo_conv->read(
   IMPORTING
    data =  rv_string ).
ENDMETHOD.


METHOD _abap_2_json.
  CONSTANTS:
    c_comma TYPE c VALUE ',',
    c_colon TYPE c VALUE ':',
    c_quote TYPE c VALUE '"'.

  DATA:
    dont_quote     TYPE xfeld,
    json_fragments TYPE TABLE OF string,
    rec_rv_json    TYPE string,
    l_comps        TYPE i,
    l_lines        TYPE i,
    l_index        TYPE i,
    l_value        TYPE string,
    l_name         TYPE string,
    l_strudescr    TYPE REF TO cl_abap_structdescr,
    lo_type        TYPE REF TO cl_abap_typedescr,
    lo_subtype     TYPE REF TO cl_abap_typedescr,
    lv_type        TYPE REF TO string.

  FIELD-SYMBOLS:
    <abap_data> TYPE any,
    <itab>      TYPE ANY TABLE,
    <comp>      TYPE any,
    <abapcomp>  TYPE abap_compdescr.

  DEFINE get_scalar_value.
    " &1 : assigned var
    " &2 : abap data
    " &3 : abap type
    &1 = &2.
****************************************************
* Adapt some basic ABAP types (pending inclusion of all basic abap types?)
* Feel free to customize this for your needs
    CASE &3->type_kind.
*       1. ABAP numeric types
      WHEN 'I'. " Integer
        CONDENSE &1.
        IF sign( &1 ) < 0.
          SHIFT &1 BY 1 PLACES RIGHT CIRCULAR.
        ENDIF.
        dont_quote = 'X'.

      WHEN 'F'. " Float
        CONDENSE &1.
        dont_quote = 'X'.

      WHEN 'P'. " Packed number (used in quantities or currency, for example)
        CONDENSE &1.
        IF sign( &1 ) < 0.
          SHIFT &1 BY 1 PLACES RIGHT CIRCULAR.
        ENDIF.
        dont_quote = 'X'.

      WHEN 'X'. " Hexadecimal
        CONDENSE &1.
        CONCATENATE '0x' &1 INTO &1.
*        dont_quote = 'X'.
*        "Quote it, as JSON doesn't support Hex or Octal as native types.

*       2. ABAP char types
      WHEN 'D'. " Date type
        CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2) INTO &1.
        " &1 = convert_to_timestamp( iv_date = &1 ). 'T00:00:00'

      WHEN 'T'. " Time representation
        CONCATENATE &1(2) ':' &1+2(2) ':' &1+4(2) INTO &1.
        " '0000-00-00T'  &1 = convert_to_timestamp( iv_time = &1 ).

      WHEN 'N'. " Numeric text field
*           condense &1.

      WHEN 'C' OR 'g'. " Char sequences and Strings
        READ TABLE lcl_json_util=>mt_xsdboolean TRANSPORTING NO FIELDS BINARY SEARCH
         WITH KEY table_line = &3->absolute_name.
        IF sy-subrc = 0.
          dont_quote = 'X'.
          IF &2 = abap_true.
            &1 = 'true'.
          ELSE.
            &1 = 'false'.
          ENDIF.
        ELSEIF &3->absolute_name = '\TYPE=XSDDATETIME_LOCAL'." others ?

          IF strlen( &1 ) = 14.
            CONCATENATE &1(4) '-' &1+4(2) '-' &1+6(2)
               'T' &1+8(2) ':' &1+10(2) ':' &1+12(2) INTO &1.
*            lv_date = &1(8).
*            lv_time = &1+8.
*            &1 = convert_to_timestamp( iv_date = lv_date iv_time = lv_time ).
          ELSE.
            &1 = ''.
          ENDIF.
        ELSE.
          " Put safe chars
          REPLACE ALL OCCURRENCES OF '\' IN &1 WITH '\\' .
          REPLACE ALL OCCURRENCES OF '"' IN &1 WITH '\"' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN &1 WITH '\r\n' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN &1 WITH '\n' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN &1 WITH '\t' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace IN &1 WITH '\b' .
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN &1 WITH '\f' .
        ENDIF.

      WHEN 'y'.  " XSTRING
        &1 = xstring_to_base64( &2 ).

      WHEN OTHERS.
* Don't hesitate to add and modify scalar abap types to suit your taste.

    ENDCASE.
** End of scalar data preparing.

* Enclose value in quotes (or not)
    IF dont_quote NE 'X'.
      CONCATENATE c_quote &1 c_quote INTO &1.
    ENDIF.

    CLEAR dont_quote.

  END-OF-DEFINITION.


***************************************************
*  Prepare field names, JSON does quote names!!   *
*  You must be strict in what you produce.        *
***************************************************
  IF iv_name IS NOT INITIAL.
    CONCATENATE c_quote iv_name c_quote c_colon INTO rec_rv_json.
    APPEND rec_rv_json TO json_fragments.
    CLEAR rec_rv_json.
  ENDIF.

**
* Get ABAP data type
  lo_type = cl_abap_typedescr=>describe_by_data( im_data ).
  " DESCRIBE FIELD im_data TYPE l_type COMPONENTS l_comps.

***************************************************
*  Get rid of data references
***************************************************
  IF lo_type->type_kind EQ cl_abap_typedescr=>typekind_dref.
    ASSIGN im_data->* TO <abap_data>.
    IF sy-subrc = 0.
      lo_type = cl_abap_typedescr=>describe_by_data( <abap_data> ).
    ELSE.
      CLEAR lo_type.
    ENDIF.

    IF sy-subrc <> 0 OR lo_type IS INITIAL.
      APPEND '{}' TO json_fragments.
      CONCATENATE LINES OF json_fragments INTO rv_json.
      EXIT.
    ENDIF.
  ELSE.
    ASSIGN im_data TO <abap_data>.
  ENDIF.

* Get ABAP data type again and start
  " DESCRIBE FIELD <abap_data> TYPE l_type COMPONENTS l_comps.

  " A little fastaer than description of description
  TRY.
      l_strudescr ?= lo_type.
    CATCH cx_sy_move_cast_error.
      CLEAR l_strudescr.
  ENDTRY.

***************************************************
*  Tables
***************************************************
  IF lo_type->type_kind EQ cl_abap_typedescr=>typekind_table.
* '[' JSON table opening bracket
    APPEND '[' TO json_fragments.
    ASSIGN <abap_data> TO <itab>.
    l_lines = lines( <itab> ).
    LOOP AT <itab> ASSIGNING <comp>.
      ADD 1 TO l_index.
*> Recursive call for each table row:
      rec_rv_json = _abap_2_json( im_data = <comp> ).
      APPEND rec_rv_json TO json_fragments.
      CLEAR rec_rv_json.
      IF l_index < l_lines.
        APPEND c_comma TO json_fragments.
      ENDIF.
    ENDLOOP.
    APPEND ']' TO json_fragments.
* ']' JSON table closing bracket

***************************************************
*  Objects
***************************************************
  ELSEIF lo_type->type_kind EQ cl_abap_typedescr=>typekind_oref.
    " Dump in 7.02 Only for jekyll info
    TRY.
        CALL METHOD ('/UI2/CL_JSON')=>serialize
          EXPORTING
            data   = <abap_data>
          RECEIVING
            r_json = rec_rv_json.
      CATCH cx_sy_dyn_call_error.
        rec_rv_json = '{}'.
    ENDTRY.
    APPEND rec_rv_json TO json_fragments.
    CLEAR rec_rv_json.

***************************************************
*  Structures
***************************************************
  ELSE.
    IF l_strudescr IS NOT INITIAL. " lo_class->absolute_name = '\CLASS=CL_ABAP_STRUCTDESCR'. " l_comps IS NOT INITIAL.
* '{' JSON object opening curly brace
      APPEND '{' TO json_fragments.
      " l_strudescr ?=  lo_type. " cl_abap_typedescr=>describe_by_data( <abap_data> ).
      l_comps = lines( l_strudescr->components ).
      LOOP AT l_strudescr->components ASSIGNING <abapcomp>.
        l_index = sy-tabix .
        ASSIGN COMPONENT <abapcomp>-name OF STRUCTURE <abap_data> TO <comp>.
        l_name = <abapcomp>-name.
** ABAP names are usually in caps, set upcase to avoid the conversion to lower case.
        " DESCRIBE FIELD <comp> TYPE s_type.
        lo_subtype = cl_abap_typedescr=>describe_by_data( <comp> ).
        IF lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_table   OR lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_dref OR
           lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_struct1 OR lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_struct2.
*> Recursive call for non-scalars:
          rec_rv_json = _abap_2_json( im_data = <comp> iv_name = l_name ).
        ELSE.
          IF lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_oref OR lo_subtype->type_kind EQ cl_abap_typedescr=>typekind_iref.
            rec_rv_json = '"REF UNSUPPORTED"'.
          ELSE.
            get_scalar_value rec_rv_json <comp> lo_subtype.
          ENDIF.
          CONCATENATE c_quote l_name c_quote c_colon rec_rv_json INTO rec_rv_json.
        ENDIF.
        APPEND rec_rv_json TO json_fragments.
        CLEAR rec_rv_json. CLEAR l_name.
        IF l_index < l_comps.
          APPEND c_comma TO json_fragments.
        ENDIF.
      ENDLOOP.
      APPEND '}' TO json_fragments.
* '}' JSON object closing curly brace

****************************************************
*                  - Scalars -                     *
****************************************************
    ELSE.
      get_scalar_value l_value <abap_data> lo_type.
      APPEND l_value TO json_fragments.

    ENDIF.
* End of structure/scalar IF block.
***********************************

  ENDIF.
* End of main IF block.
**********************

* Use a loop in older releases that don't support concatenate lines.
  CONCATENATE LINES OF json_fragments INTO rv_json.
ENDMETHOD.


METHOD _json_2_abap.
*/************************************************/*
*/ Input any abap data and this method tries to   /*
*/ fill it with the data in the JSON string.      /*
*/  Thanks to Juan Diaz for helping here!!        /*
*/************************************************/*

  TYPE-POOLS: abap, js.

  DATA:
    js_script         TYPE string,
    js_started        TYPE i VALUE 0,
    l_json_string     TYPE string,
    js_property_table TYPE   js_property_tab,
    js_property       TYPE LINE OF js_property_tab,
    l_property_path   TYPE string,
    compname          TYPE string,
    item_path         TYPE string.

  DATA:
*    l_type   TYPE c,
    l_value  TYPE string,
    linetype TYPE string,
    l_comp   TYPE LINE OF abap_compdescr_tab.

  DATA:
    datadesc  TYPE REF TO cl_abap_typedescr,
    drefdesc  TYPE REF TO cl_abap_typedescr,
    linedesc  TYPE REF TO cl_abap_typedescr,
    strudesc  TYPE REF TO cl_abap_structdescr,
    tabldesc  TYPE REF TO cl_abap_tabledescr,
    data_desc TYPE REF TO cl_abap_datadescr.

  DATA newline TYPE REF TO data.

  FIELD-SYMBOLS:
    <abap_data> TYPE any,
    <itab>      TYPE ANY TABLE,
    <comp>      TYPE any,
    <jsprop>    TYPE LINE OF js_property_tab,
    <abapcomp>  TYPE abap_compdescr.

  DATA lo_type TYPE REF TO cl_abap_typedescr.

  DEFINE assign_scalar_value.
    "   &1   <abap_data>
    "   &2   js_property-value
    "DESCRIBE FIELD &1 TYPE l_type.
    lo_type = cl_abap_typedescr=>describe_by_data( &1 ).

    l_value = &2.
* convert or adapt scalar values to ABAP.
    CASE lo_type->type_kind.
      WHEN 'D'. " date type
        IF l_value CS '-'.
          REPLACE ALL OCCURRENCES OF '-' IN l_value WITH space.
          CONDENSE l_value NO-GAPS.
        ENDIF.
      WHEN 'T'. " time type
        IF l_value CS ':'.
          REPLACE ALL OCCURRENCES OF ':' IN l_value WITH space.
          CONDENSE l_value NO-GAPS.
        ENDIF.
      WHEN 'C'. " char
        READ TABLE lcl_json_util=>mt_xsdboolean TRANSPORTING NO FIELDS BINARY SEARCH
         WITH KEY table_line = lo_type->absolute_name.
        IF sy-subrc = 0.
          CASE l_value.
            WHEN 'true'.
              l_value = abap_true.

            WHEN 'false'.
              l_value = abap_false.

            WHEN OTHERS.
              l_value = abap_undefined.

          ENDCASE.
        ENDIF.

      WHEN OTHERS.
        " may be other conversions or checks could be implemented here.
    ENDCASE.
    &1 = l_value.
  END-OF-DEFINITION.


  IF js_object IS NOT BOUND.

    IF json_string IS INITIAL. EXIT. ENDIF. " exit method if there is nothing to parse

    l_json_string = json_string.
    " js_object = cl_java_script=>create( STACKSIZE = 16384 ).
    js_object = cl_java_script=>create( stacksize = 16384 heapsize = 960000 ).

***************************************************
*  Parse JSON using JavaScript                    *
***************************************************
    js_object->bind( EXPORTING name_obj = 'abap_data' name_prop = 'json_string'    CHANGING data = l_json_string ).
    js_object->bind( EXPORTING name_obj = 'abap_data' name_prop = 'script_started' CHANGING data = js_started ).

* We use the JavaScript engine included in ABAP to read the JSON string.
* We simply use the recommended way to eval a JSON string as specified
* in RFC 4627 (http://www.ietf.org/rfc/rfc4627.txt).
*
* Security considerations:
*
*   Generally there are security issues with scripting languages.  JSON
*   is a subset of JavaScript, but it is a safe subset that excludes
*   assignment and invocation.
*
*   A JSON text can be safely passed into JavaScript's eval() function
*   (which compiles and executes a string) if all the characters not
*   enclosed in strings are in the set of characters that form JSON
*   tokens.  This can be quickly determined in JavaScript with two
*   regular expressions and calls to the test and replace methods.
*
*      var my_JSON_object = !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(
*             text.replace(/"(\\.|[^"\\])*"/g, ''))) &&
*         eval('(' + text + ')');

    CONCATENATE

         'var json_obj; '
         'var json_text; '

         'function start() { '
         '  if(abap_data.script_started) { return; } '
         '  json_text = abap_data.json_string;'
         '  json_obj = !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test( '
         '      json_text.replace(/"(\\.|[^"\\])*"/g, ''''))) && '
         '    eval(''('' + json_text + '')''); '
         '  abap_data.script_started = 1; '
         '} '

         'if(!abap_data.script_started) start(); '


       INTO js_script RESPECTING BLANKS SEPARATED BY cl_abap_char_utilities=>newline.

    js_object->compile( script_name = 'json_parser'     script = js_script ).
    js_object->execute( script_name = 'json_parser' ).

    IF js_object->last_error_message IS NOT INITIAL.
      zcx_eui_exception=>raise_sys_error( iv_message = js_object->last_error_message ).
    ENDIF.

  ENDIF.
** End of JS processing.

**
  IF var_name IS NOT INITIAL.
    CONCATENATE property_path var_name INTO l_property_path SEPARATED BY '.'.
  ELSE.
    l_property_path = property_path.
  ENDIF.
**
**
  js_property_table = js_object->get_properties_scope_global( property_path = l_property_path ).
  property_table = js_property_table.

* Exit if abap_data is not supplied, normally when called
* from json_deserialize to get top level properties
  IF abap_data IS NOT SUPPLIED.
    EXIT.
  ENDIF. "***

*
* Get ABAP data type, dereference if necessary and start
  datadesc = cl_abap_typedescr=>describe_by_data( abap_data ).
  IF datadesc->kind EQ cl_abap_typedescr=>kind_ref.
    ASSIGN abap_data->* TO <abap_data>.
  ELSE.
    ASSIGN abap_data TO <abap_data>.
  ENDIF.
  datadesc = cl_abap_typedescr=>describe_by_data( <abap_data> ).


  CASE datadesc->kind.

    WHEN cl_abap_typedescr=>kind_elem.
* Scalar: process ABAP elements. Assume no type conversions for the moment.
      IF var_name IS INITIAL.
        zcx_eui_exception=>raise_sys_error( iv_message = 'VAR_NAME is required for scalar values.' ).
      ENDIF.
      js_property_table = js_object->get_properties_scope_global( property_path = property_path ).
      READ TABLE js_property_table WITH KEY name = var_name INTO js_property.
      IF sy-subrc EQ 0.
        assign_scalar_value <abap_data> js_property-value.
      ENDIF.


    WHEN cl_abap_typedescr=>kind_struct.
* Process ABAP structures
      strudesc ?= datadesc.
      LOOP AT js_property_table ASSIGNING <jsprop>.
        compname = <jsprop>-name.
        TRANSLATE compname TO UPPER CASE.
        READ TABLE strudesc->components WITH KEY name = compname INTO l_comp.
        IF sy-subrc EQ 0.
          ASSIGN COMPONENT l_comp-name OF STRUCTURE <abap_data> TO <comp>.
          CASE l_comp-type_kind.
            WHEN    cl_abap_typedescr=>typekind_struct1  " 'v'
                 OR cl_abap_typedescr=>typekind_struct2  " 'u'
                 OR cl_abap_typedescr=>typekind_table.   " 'h' (may need a different treatment one day)
              CONCATENATE l_property_path <jsprop>-name INTO item_path SEPARATED BY '.'.
*> Recursive call here
              _json_2_abap( EXPORTING property_path = item_path CHANGING abap_data = <comp> js_object = js_object ).

            WHEN OTHERS.
* Process scalars in structures (same as the kind_elem above)
              assign_scalar_value <comp> <jsprop>-value.

          ENDCASE.
        ENDIF.
      ENDLOOP.

    WHEN cl_abap_typedescr=>kind_table.
* Process ABAP tables
      IF js_property_table IS NOT INITIAL.
        tabldesc ?= datadesc.
        data_desc ?= tabldesc->get_table_line_type( ).

        ASSIGN <abap_data> TO <itab>.
        LOOP AT js_property_table INTO js_property WHERE name NE 'length'. " the JS object length

*          linetype = linedesc->get_relative_name( ).
          CREATE DATA newline TYPE HANDLE data_desc.
          ASSIGN newline->* TO <comp>.

          CASE js_property-kind.
            WHEN 'O'.
              CONCATENATE l_property_path js_property-name INTO item_path SEPARATED BY '.'.
              CONDENSE item_path.
*> Recursive call here
              _json_2_abap( EXPORTING property_path = item_path CHANGING abap_data = newline js_object = js_object ).
            WHEN OTHERS. " Assume scalars, 'S', 'I', or other JS types
              " Process scalars in plain table components(same as the kind_elem above)
              assign_scalar_value <comp> js_property-value.
          ENDCASE.
          INSERT <comp> INTO TABLE <itab>.
          FREE newline.
        ENDLOOP.
      ENDIF.

    WHEN OTHERS. " kind_class, kind_intf
      " forget it.

  ENDCASE.
ENDMETHOD.                                               "#EC CI_VALPAR
ENDCLASS.
