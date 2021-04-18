*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zeui_test_excel.

TYPE-POOLS:
 abap.

**********************************************************************
**********************************************************************

SELECTION-SCREEN BEGIN OF BLOCK bl_grp WITH FRAME.
PARAMETERS:
  p_csv  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl_grp.

**********************************************************************
**********************************************************************
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_alv.
        INCLUDE TYPE spfli.
      TYPES:
        color TYPE lvc_t_scol,
      END OF ts_alv,
      tt_alv TYPE STANDARD TABLE OF ts_alv WITH DEFAULT KEY.

    DATA:
      " Main data to Export/Import
      mr_alv    TYPE REF TO tt_alv,

      mo_logger TYPE REF TO zcl_eui_logger.

    CONSTANTS:
      BEGIN OF mc_cmd,
        export TYPE syucomm VALUE 'EXPORT',
        show   TYPE syucomm VALUE 'SHOW',
        import TYPE syucomm VALUE 'IMPORT',
      END OF mc_cmd.

    METHODS:
      start_of_selection,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          sender
          e_ucomm,

      export
        IMPORTING
          io_grid TYPE REF TO cl_gui_alv_grid
          iv_show TYPE abap_bool,

      import
        IMPORTING
          io_grid TYPE REF TO cl_gui_alv_grid,

      on_mapping_error FOR EVENT mapping_error OF zcl_eui_file_io
        IMPORTING
          iv_source
          iv_row
          is_excel_map
          io_error
          cv_value
          cs_row,

      on_pbo_event FOR EVENT pbo_event OF zif_eui_manager
        IMPORTING
          sender
          io_container.
ENDCLASS.


**********************************************************************
**********************************************************************
DATA:
  go_report     TYPE REF TO lcl_report.

**********************************************************************
**********************************************************************
CLASS lcl_report IMPLEMENTATION.
  METHOD start_of_selection.
    DATA:
      lr_table       TYPE REF TO data,
      lo_salv        TYPE REF TO cl_salv_table,
      ls_layout	     TYPE lvc_s_layo,
      lt_toolbar     TYPE ttb_button,
      lt_mod_catalog TYPE lvc_t_fcat,
      lv_txt         TYPE string.
    FIELD-SYMBOLS:
      <ls_alv>         TYPE ts_alv,
      <ls_button>      LIKE LINE OF lt_toolbar,
      <ls_mod_catalog> LIKE LINE OF lt_mod_catalog.

**********************************************************************
    " Fill with test data
**********************************************************************
    CREATE DATA mr_alv.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE mr_alv->*
    FROM spfli.

**********************************************************************
    " Prepare layout
**********************************************************************
    ls_layout-cwidth_opt = abap_true.
    ls_layout-zebra      = abap_true.
    ls_layout-ctab_fname = 'COLOR'.

    ls_layout-grid_title = 'Export / Import'.
    ls_layout-smalltitle = abap_true.

**********************************************************************
    " Create additional buttons
**********************************************************************
    IF p_csv = abap_true.
      lv_txt = 'CSV'.
    ELSE.
      lv_txt = 'Excel'.
    ENDIF.

    APPEND INITIAL LINE TO lt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = mc_cmd-export.
    <ls_button>-icon     = icon_export.
    CONCATENATE `Export to ` lv_txt INTO <ls_button>-text.

    APPEND INITIAL LINE TO lt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = mc_cmd-show.
    <ls_button>-icon     = icon_xls.
    CONCATENATE `Export to ` lv_txt `  and show in_place` INTO <ls_button>-text.


    APPEND INITIAL LINE TO lt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = mc_cmd-import.
    <ls_button>-icon     = icon_import.
    CONCATENATE `Import from ` lv_txt INTO <ls_button>-text.

**********************************************************************
    " Change field catalog
**********************************************************************

    " What to change (MOVE-CORRESPONDING EXCEPT_INITIAL)
    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = 'MANDT'.
    <ls_mod_catalog>-tech     = abap_true.

**********************************************************************
    " Main table & ALV manager
**********************************************************************
    DATA lo_eui_alv TYPE REF TO zcl_eui_alv.

    " Pass by reference
    CREATE OBJECT lo_eui_alv
      EXPORTING
        ir_table       = mr_alv
        " grid parameters
        is_layout      = ls_layout
        it_mod_catalog = lt_mod_catalog
        it_toolbar     = lt_toolbar.

    " Instead of set handler
    lo_eui_alv->show( io_handler = me ).
  ENDMETHOD.

  METHOD on_user_command.
    CASE e_ucomm.
      WHEN mc_cmd-export.
        export(
         io_grid = sender
         iv_show = abap_false ).

      WHEN mc_cmd-show.
        export(
         io_grid = sender
         iv_show = abap_true ).

      WHEN mc_cmd-import.
        import( sender ).

    ENDCASE.
  ENDMETHOD.

  METHOD export.
    DATA lv_ext      TYPE string.
    DATA lo_file     TYPE REF TO zcl_eui_file_io.
    DATA lo_error    TYPE REF TO zcx_eui_exception.

    " Detect exporting mode
    CASE p_csv.
      WHEN abap_true.
        lv_ext = zcl_eui_file=>mc_extension-csv.
      WHEN OTHERS.
        lv_ext = zcl_eui_file=>mc_extension-xlsx.
    ENDCASE.

    TRY.
        " In new syntax just  --->   new ZCL_EUI_FILE( )->IMPORT_FROM_ITAB( )->DOWNLOAD( )->OPEN( )
        CREATE OBJECT lo_file
          EXPORTING
            iv_file_name = lv_ext.

        " No mapping. All fields. Import a file content FROM INTERNAL table
        lo_file->import_from_itab(
         ir_table = mr_alv ).

        IF iv_show = abap_true.
          lo_file->show( io_handler = me ).
        ELSE.
          lo_file->download( iv_save_dialog = abap_true ).
          lo_file->open( ). " open_by_ole( ) For ole
        ENDIF.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    MESSAGE 'Export to file is complete!' TYPE 'S'.
  ENDMETHOD.

  METHOD import.
    DATA:
      lo_file  TYPE REF TO zcl_eui_file_io,
      lv_ext   TYPE string,
      lo_error TYPE REF TO zcx_eui_exception,
      lt_map   TYPE REF TO zcl_eui_file_io=>tt_excel_map,
      ls_map   TYPE REF TO zcl_eui_file_io=>ts_excel_map.

    IF mo_logger IS INITIAL.
      " For title -> EXPORTING is_header = VALUE #( object = '/GC1/GC' subobject = 'LOG' )
      CREATE OBJECT mo_logger.
    ELSE.
      mo_logger->clear( ).
    ENDIF.
    TRY.
        " If just want to load part of table (OR order is different in Excel & mt_alv[])
        CREATE DATA lt_map.
        APPEND INITIAL LINE TO lt_map->* REFERENCE INTO ls_map.
        ls_map->field       = 'CARRID'.
        ls_map->column_name = 'B'. " Or column_index = 2

        APPEND INITIAL LINE TO lt_map->* REFERENCE INTO ls_map.
        ls_map->field       = 'CONNID'.
        ls_map->column_name = 'C'.

        APPEND INITIAL LINE TO lt_map->* REFERENCE INTO ls_map.
        ls_map->field       = 'COUNTRYFR'.
        ls_map->column_name = 'D'.

        APPEND INITIAL LINE TO lt_map->* REFERENCE INTO ls_map.
        ls_map->field       = 'DEPTIME'.
        ls_map->column_name = 'K'.

        APPEND INITIAL LINE TO lt_map->* REFERENCE INTO ls_map.
        ls_map->field       = 'ARRTIME'.
        ls_map->column_name = 'L'.

        " Check is number in handler
        APPEND INITIAL LINE TO lt_map->* REFERENCE INTO ls_map.
        ls_map->field       = 'DISTANCE'.
        ls_map->column_name = 'M'.

        " Detect importing mode
        CASE p_csv.
          WHEN abap_true.
            lv_ext = zcl_eui_file=>mc_extension-csv.
          WHEN OTHERS.
            lv_ext = zcl_eui_file=>mc_extension-xlsx.
        ENDCASE.

        " In new syntax just  --->   new ZCL_EUI_FILE( )->IMPORT_FROM_FILE( )->EXPORT_TO_ITAB( )
        CREATE OBJECT lo_file
          EXPORTING
            iv_file_name = lv_ext.

        " Show file open dialog if path is not supplied
        lo_file->import_from_file( ).

        " Export file content to -> INTERNAL table
        lo_file->export_to_itab(
            ir_table        = mr_alv
            iv_row_from     = 2       " Skip header
            io_handler      = me      " without an error handler would be dump if DISTANCE have chars
            it_excel_map    = lt_map  " Is optional!
        ).

      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    MESSAGE 'Table is uploaded!' TYPE 'S'.
    " Could fill with texts of mt_alv
    io_grid->refresh_table_display( ).

    " Button and log itself
    mo_logger->show_as_button( ).

    DATA ls_profile TYPE bal_s_prof.
    ls_profile-title = 'Test title'.
    mo_logger->show( iv_profile = zcl_eui_logger=>mc_profile-popup
                     is_profile = ls_profile ).
  ENDMETHOD.

  METHOD on_mapping_error.
    DATA:
      lv_text  TYPE string,
      lv_error TYPE string,
      ls_cell  TYPE lvc_s_scol.
    FIELD-SYMBOLS:
      <lv_distance> TYPE spfli-distance,
      <ls_alv>      TYPE ts_alv.

    " Value
    IF is_excel_map-field = 'DISTANCE'.
      ASSIGN cv_value->* TO <lv_distance>.
      " Could analyze IV_SOURCE from excel
      <lv_distance> = 77777.
    ENDIF.

    " alternative for cf_reca_message_list=>create( )
    lv_text  = iv_row.
    lv_error = io_error->get_text( ). " mo_logger->add_exception( io_exception = io_error ).
    CONCATENATE `In row ` lv_text ` there is an error:` lv_error INTO lv_error.
    mo_logger->add_text( iv_text  = lv_error
                         iv_msgty = 'E' ).

    " Change color of cell
    ASSIGN cs_row->* TO <ls_alv>.
    CHECK sy-subrc = 0.

    ls_cell-fname = is_excel_map-field.
    ls_cell-color-col = '6'.
    APPEND ls_cell TO <ls_alv>-color.
  ENDMETHOD.

  METHOD on_pbo_event.
    " CALL METHOD get_ole_info( ) of sender

    " Just for test. @SHOW( )
    APPEND 'EXIT' TO sender->ms_status-exclude.
    APPEND 'CANC' TO sender->ms_status-exclude.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

INITIALIZATION.
  CREATE OBJECT go_report.

START-OF-SELECTION.
  go_report->start_of_selection( ).
