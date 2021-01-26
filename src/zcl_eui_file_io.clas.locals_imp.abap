*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_helper IMPLEMENTATION.
  METHOD fill_mapping.
    DATA:
      lt_fieldcat  LIKE et_fieldcat,
      ls_excel_map TYPE zcl_eui_file_io=>ts_excel_map.
    FIELD-SYMBOLS:
      <lt_excel_map> TYPE zcl_eui_file_io=>tt_excel_map,
      <ls_excel_map> TYPE zcl_eui_file_io=>ts_excel_map,
      <ls_fieldcat>  TYPE lvc_s_fcat.
    IF ir_table IS INITIAL. " zcx_eui_no_check=>raise_sys_error ?
      zcx_eui_exception=>raise_dump( iv_message = `Pass 'IR_TABLE' parameter to ZCL_EUI_FILE_IO` ).
    ENDIF.

    " Use exiting
    IF ir_excel_map IS NOT INITIAL.
      er_excel_map = ir_excel_map.
    ELSE.
      "  New mapping
      CREATE DATA er_excel_map.
    ENDIF.

    " Main mapping
    ASSIGN er_excel_map->* TO <lt_excel_map>.

    " Detect fm for conversion exit
    lt_fieldcat = zcl_eui_type=>get_catalog( ir_table = ir_table ).
    CLEAR et_fieldcat.

    " No mapping
    IF <lt_excel_map> IS INITIAL.
      LOOP AT lt_fieldcat ASSIGNING <ls_fieldcat>.
        ls_excel_map-field        = <ls_fieldcat>-fieldname.
        ls_excel_map-column_index = sy-tabix.
        APPEND ls_excel_map TO <lt_excel_map>.
      ENDLOOP.
    ENDIF.

    " Fill fm & gen type
    SORT lt_fieldcat BY fieldname.
    LOOP AT <lt_excel_map> ASSIGNING <ls_excel_map>.
      " If not in catalog skip it
      READ TABLE lt_fieldcat ASSIGNING <ls_fieldcat> BINARY SEARCH
       WITH KEY fieldname = <ls_excel_map>-field.
      IF sy-subrc <> 0.
        <ls_excel_map>-gen_type = mc_gen_type-skip.
        CONTINUE.
      ENDIF.

      " For export to excel
      IF et_fieldcat IS REQUESTED.
        APPEND <ls_fieldcat> TO et_fieldcat.
      ENDIF.

      " Only if not supplied
      IF <ls_excel_map>-column_index IS INITIAL.
        <ls_excel_map>-column_index = zcl_eui_file_io=>column_2_int( <ls_excel_map>-column_name ).
      ENDIF.

      " Detect conversion Exit
      <ls_excel_map>-fm = <ls_fieldcat>-edit_mask.

      CASE <ls_fieldcat>-inttype.
        WHEN cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float         OR
          cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_int                 OR
          cl_abap_typedescr=>typekind_int1 OR cl_abap_typedescr=>typekind_int2               OR
          cl_abap_typedescr=>typekind_numeric OR
          '/' OR " cl_abap_typedescr=>typekind_decfloat
          'a' OR " cl_abap_typedescr=>typekind_decfloat16
          'e'.   " cl_abap_typedescr=>typekind_decfloat34
          <ls_excel_map>-gen_type = mc_gen_type-number.

        WHEN cl_abap_typedescr=>typekind_date.
          <ls_excel_map>-gen_type = mc_gen_type-date.

        WHEN cl_abap_typedescr=>typekind_time.
          <ls_excel_map>-gen_type = mc_gen_type-time.

        WHEN cl_abap_typedescr=>typekind_table. " xstring?

      ENDCASE.

      " Get FM name
      IF <ls_excel_map>-fm IS NOT INITIAL.
        REPLACE `==` IN <ls_excel_map>-fm WITH ``.
        CONCATENATE 'CONVERSION_EXIT_' <ls_excel_map>-fm '_INPUT' INTO <ls_excel_map>-fm.
      ENDIF.
    ENDLOOP.

    " delete table fields only
    DELETE <lt_excel_map> WHERE gen_type = lcl_helper=>mc_gen_type-skip.
  ENDMETHOD.

  METHOD export_2_table.
    DATA lo_event_caller  TYPE REF TO zcl_eui_event_caller.
    DATA lv_has_handler   TYPE abap_bool.
    DATA lv_row           TYPE sy-tabix.
    DATA lo_error         TYPE REF TO cx_root.
    DATA lv_error_message TYPE text255.
    DATA lv_message       TYPE text255.
    DATA lv_input         TYPE text1000.
    DATA lr_value         TYPE REF TO data.
    DATA lr_row           TYPE REF TO data.

    FIELD-SYMBOLS <lt_table>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_source>    TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_row>       TYPE any.
    FIELD-SYMBOLS <ls_excel_map> TYPE zcl_eui_file_io=>ts_excel_map.
    FIELD-SYMBOLS <lv_src>       TYPE any.
    FIELD-SYMBOLS <lv_dest>      TYPE any.
    FIELD-SYMBOLS <ls_import>    TYPE any.

    " Ref to table
    ASSIGN ir_source->* TO <lt_source>.

    " Have any data?
    IF <lt_source> IS INITIAL.
      MESSAGE s003(zeui_message) WITH io_file->mv_file_name INTO sy-msgli.
      zcx_eui_exception=>raise_sys_error( ).
    ENDIF.

    " If have some errors
    DO 1 TIMES.
      CHECK io_handler IS NOT INITIAL.

      " Send events to IO_HANDLER
      CREATE OBJECT lo_event_caller.
      lo_event_caller->add_handler( io_handler = io_handler ).

      " Is ok ?
      lv_has_handler = lo_event_caller->has_handler(
        iv_of_class  = 'ZCL_EUI_FILE_IO'
        iv_for_event = 'MAPPING_ERROR' ).
      CHECK lv_has_handler <> abap_true.

      " Cannot call
      CLEAR lo_event_caller.
    ENDDO.

    " Add one by one
    ASSIGN ir_table->* TO <lt_table>.
    CLEAR <lt_table>.

    LOOP AT <lt_source> ASSIGNING <ls_import> FROM iv_row_from.
      lv_row = sy-tabix.
      " New row
      APPEND INITIAL LINE TO <lt_table> ASSIGNING <ls_row>.

      " Fill items
      LOOP AT ir_excel_map->* ASSIGNING <ls_excel_map>.
        UNASSIGN:
           <lv_src>,
           <lv_dest>.

        ASSIGN COMPONENT:
           <ls_excel_map>-column_index OF STRUCTURE <ls_import> TO <lv_src>,
           <ls_excel_map>-field        OF STRUCTURE <ls_row>    TO <lv_dest>.

        " Oops
        IF <lv_src> IS NOT ASSIGNED.
          MESSAGE s005(zeui_message) WITH <ls_excel_map>-column_name <ls_excel_map>-column_index io_file->mv_file_name INTO sy-msgli.
          zcx_eui_exception=>raise_sys_error( ).
        ENDIF.
        " Sometimes have leading spaces
        lv_input = <lv_src>.
        CONDENSE lv_input.

        IF <lv_dest> IS NOT ASSIGNED.
          MESSAGE s006(zeui_message) WITH <ls_excel_map>-field INTO sy-msgli.
          zcx_eui_exception=>raise_sys_error( ).
        ENDIF.

        " No errors
        CLEAR lo_error.

        TRY.
            IF <ls_excel_map>-fm IS NOT INITIAL.
              " Try to convert
              CALL FUNCTION <ls_excel_map>-fm
                EXPORTING
                  input         = lv_input
                IMPORTING
                  output        = <lv_dest>
                EXCEPTIONS
                  error_message = 1
                  OTHERS        = 2.
            ELSE.
              CASE <ls_excel_map>-gen_type.
                WHEN lcl_helper=>mc_gen_type-number.
                  REPLACE FIRST OCCURRENCE OF ',' IN lv_input WITH '.'.
                  CONDENSE lv_input NO-GAPS.
                  <lv_dest> = lv_input.
                  sy-subrc = 0.

                WHEN lcl_helper=>mc_gen_type-time.
                  REPLACE ALL OCCURRENCES OF `:` IN lv_input WITH ``.
                  CONCATENATE lv_input+0(2) lv_input+2(2) lv_input+4(2) INTO <lv_dest>.
                  sy-subrc = 0.

                WHEN lcl_helper=>mc_gen_type-date.
                  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                    EXPORTING
                      date_external = lv_input
                    IMPORTING
                      date_internal = <lv_dest>
                    EXCEPTIONS
                      error_message = 1
                      OTHERS        = 2.

                WHEN OTHERS.
                  <lv_dest> = lv_input.
                  sy-subrc = 0.
              ENDCASE.
            ENDIF.

            " Just create own exception
            CHECK sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_error_message.
            zcx_eui_exception=>raise_sys_error( iv_message = lv_error_message ).

          CATCH cx_root INTO lo_error.
        ENDTRY.

        " All ok ?
        CHECK lo_error IS NOT INITIAL.

        " if have no handlers then dump
        IF lo_event_caller IS INITIAL.
          zcx_eui_exception=>raise_dump( io_error = lo_error ).
        ENDIF.

        " Send event to handler
        GET REFERENCE OF <lv_dest> INTO lr_value.
        GET REFERENCE OF <ls_row>  INTO lr_row.

        lo_event_caller->call_handlers(
         iv_of_class     = 'ZCL_EUI_FILE_IO'
         iv_for_event    = 'MAPPING_ERROR'
         iv_param_nam_00 = 'SENDER'          iv_param_val_00 = io_file
         iv_param_nam_01 = 'IV_SOURCE'       iv_param_val_01 = <lv_src>
         iv_param_nam_02 = 'IV_ROW'          iv_param_val_02 = lv_row
         iv_param_nam_03 = 'IS_EXCEL_MAP'    iv_param_val_03 = <ls_excel_map>
         iv_param_nam_04 = 'IO_ERROR'        iv_param_val_04 = lo_error
         iv_param_nam_05 = 'CV_VALUE'        iv_param_val_05 = lr_value
         iv_param_nam_06 = 'CS_ROW'          iv_param_val_06 = lr_row ).

*      RAISE EVENT mapping_error EXPORTING
*        IV_SOURCE    = <lv_src>
*        IV_ROW       = lv_row
*        IS_EXCEL_MAP = <ls_excel_map>
*        IO_ERROR     = lo_error
*        CV_VALUE     = lr_value
*        CS_ROW       = lr_row.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
