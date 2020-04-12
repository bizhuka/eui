*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_helper IMPLEMENTATION.

  METHOD constructor.
    DATA lv_ok                    TYPE abap_bool.
    DATA lt_sub_field             TYPE STANDARD TABLE OF zcl_eui_type=>ts_field_desc.
    DATA ls_sub_field             TYPE REF TO zcl_eui_type=>ts_field_desc.
    DATA lv_tabix                 TYPE sytabix.
    DATA lv_fld_name              TYPE string.
    DATA ls_ui_ext                TYPE zcl_eui_type=>ts_field_desc.
    DATA lo_struc_desc            TYPE REF TO cl_abap_structdescr.
    DATA lo_err                   TYPE REF TO zcx_eui_exception.
    DATA lr_type                  TYPE REF TO data.
    FIELD-SYMBOLS <ls_src>        TYPE any.
    FIELD-SYMBOLS <ls_dest>       TYPE any.
    FIELD-SYMBOLS <lt_table_src>  TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_table_dest> TYPE STANDARD TABLE.

    mo_eui_alv    = io_eui_alv.
    mr_table      = io_eui_alv->mr_table.
    ms_field_desc = is_field_desc.

    " Based on field description
    CHECK ms_field_desc IS NOT INITIAL.

    " Transform to catalog
    zcl_eui_conv=>from_json(
     EXPORTING
      iv_json = ms_field_desc->sub_fdesc
     IMPORTING
      ev_ok   = lv_ok
      ex_data = lt_sub_field ).
    IF lv_ok <> abap_true.
      MESSAGE s018(zeui_message) WITH ms_field_desc->name DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Create new structure
    TRY.
        " Add new field for each table and range
        LOOP AT lt_sub_field REFERENCE INTO ls_sub_field
           WHERE ui_type = zcl_eui_type=>mc_ui_type-table
              OR ui_type = zcl_eui_type=>mc_ui_type-range.
          lv_tabix = sy-tabix + 1.

          " New string field
          CONCATENATE ls_sub_field->name `_UI` INTO lv_fld_name.
          ls_ui_ext = zcl_eui_type=>get_field_desc(
           iv_data       = lv_fld_name " type string
           iv_field_name = lv_fld_name ).

          " Add as new subfield
          ls_ui_ext-label = ls_sub_field->label.
          INSERT ls_ui_ext INTO lt_sub_field INDEX lv_tabix.
        ENDLOOP.

        " Add all
        INSERT LINES OF lt_sub_field INTO TABLE mt_sub_field.
        lo_struc_desc = zcl_eui_type=>create_structure( it_field_desc = mt_sub_field ).
      CATCH zcx_eui_exception INTO lo_err.
        MESSAGE lo_err TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Destination structure (based on source)
    CREATE DATA lr_type TYPE HANDLE lo_struc_desc.
    ASSIGN lr_type->* TO <ls_dest>.
    CREATE DATA mr_table LIKE STANDARD TABLE OF <ls_dest>.

    " Create standard table for alv editing
    ASSIGN mr_table->* TO <lt_table_dest>.

    " Copy row by row form source
    ASSIGN mo_eui_alv->mr_table->* TO <lt_table_src>.
    LOOP AT <lt_table_src> ASSIGNING <ls_src>.
      APPEND INITIAL LINE TO <lt_table_dest> ASSIGNING <ls_dest>.
      MOVE-CORRESPONDING <ls_src> TO <ls_dest>.
    ENDLOOP.

    zcl_eui_alv=>update_complex_fields(
     ir_table     = mr_table
     it_sub_field = mt_sub_field ).

**********************************************************************
    " Show by ALV manager
**********************************************************************

    " Instead of set handler
    mo_eui_alv->popup( ).

    " Static PF status no need on_pbo_event.
    mo_eui_alv->ms_status-is_fixed = abap_true.
    mo_eui_alv->ms_status-title    = ms_field_desc->label.

    " 2 buttons
    IF mo_eui_alv->mv_read_only = abap_true. " <> lcl_opt=>is_editable( ms_field_desc->is_editable )
      APPEND zif_eui_manager=>mc_cmd-ok TO mo_eui_alv->ms_status-exclude.
    ENDIF.
  ENDMETHOD.

  METHOD prepare_variant.
    IF cs_variant-report IS INITIAL.
      cs_variant-report = sy-cprog.
    ENDIF.

    IF cs_variant-handle IS INITIAL.
      cs_variant-handle = mo_eui_alv->ms_screen-dynnr.
    ENDIF.

    " For complex UI
    CHECK ms_field_desc IS NOT INITIAL.

    DATA lv_sum     TYPE num4.
    DATA lv_cnt     TYPE i.
    DATA lv_ind     TYPE i.

    lv_sum = 0.
    lv_cnt = strlen( ms_field_desc->name ).
    DO lv_cnt TIMES.
      lv_ind = sy-index - 1.
      lv_sum = lv_sum + cl_abap_conv_out_ce=>uccpi( ms_field_desc->name+lv_ind(1) ).
    ENDDO.
    lv_sum = 9999 - lv_sum.
    cs_variant-handle  = lv_sum.
  ENDMETHOD.

  METHOD prepare_layout.
*    IF mo_eui_alv->mv_read_only <> abap_true.
*      cs_layout-edit = abap_true. " lcl_opt=>is_editable( ms_field_desc->is_editable ).
*    ENDIF.

    " Default selection mode
    IF cs_layout-sel_mode IS INITIAL.
      cs_layout-sel_mode   = 'A'.       " Many rows
      cs_layout-cwidth_opt = abap_true. " Optimize width
    ENDIF.

    CHECK ms_field_desc IS NOT INITIAL.
    IF mo_eui_alv->mv_read_only = abap_true.
      CONCATENATE `Edit values of ` ms_field_desc->name INTO cs_layout-grid_title.
    ELSE.
      CONCATENATE `View values of ` ms_field_desc->name INTO cs_layout-grid_title.
    ENDIF.
    cs_layout-smalltitle = abap_true.
  ENDMETHOD.

  METHOD get_field_catalog.
    DATA lt_catalog_fields         TYPE abap_compdescr_tab. " Filled 1 time with all LVC_S_FCAT fields
    DATA lv_drdn_hndl              TYPE i.
    DATA ls_mod_catalog            LIKE LINE OF mo_eui_alv->mt_mod_catalog.
    DATA ls_sub_field              TYPE REF TO zcl_eui_type=>ts_field_desc.
    FIELD-SYMBOLS <ls_fieldcat>    LIKE LINE OF rt_fieldcat.
    FIELD-SYMBOLS <ls_mod_catalog> LIKE LINE OF mo_eui_alv->mt_mod_catalog.

    " Get field catalog
    IF me->mr_table IS NOT INITIAL. " AND me->ms_field_desc IS NOT INITIAL.
      rt_fieldcat = zcl_eui_type=>get_catalog( ir_table = me->mr_table ).
    ELSE.
      rt_fieldcat = zcl_eui_type=>get_catalog( ir_table = mo_eui_alv->mr_table ).
    ENDIF.

    " Change field catalog
    CLEAR lt_catalog_fields.
    LOOP AT rt_fieldcat ASSIGNING <ls_fieldcat>.

      LOOP AT mo_eui_alv->mt_mod_catalog ASSIGNING <ls_mod_catalog>.
        " Start of group (1 symbol)
        IF <ls_mod_catalog>-fieldname = '+'.
          ls_mod_catalog = <ls_mod_catalog>.
          CONTINUE.
        ELSEIF <ls_mod_catalog>-fieldname(1) = '+'.
          " Group continue
          ls_mod_catalog-fieldname = <ls_mod_catalog>-fieldname+1.
        ELSE.
          " No group
          ls_mod_catalog = <ls_mod_catalog>.
        ENDIF.

        " By mask ?
        IF ls_mod_catalog-fieldname CS '*'.
          CHECK <ls_fieldcat>-fieldname CP ls_mod_catalog-fieldname.
        ELSE.
          CHECK <ls_fieldcat>-fieldname = ls_mod_catalog-fieldname.
        ENDIF.

        CLEAR ls_mod_catalog-fieldname. " Do not copy field name
        zcl_eui_conv=>move_corresponding(
         EXPORTING
           is_source         = ls_mod_catalog
           iv_except_initial = abap_true    " <--- Move-corresponding except initial
         CHANGING
           cs_destination    = <ls_fieldcat>
           ct_component      = lt_catalog_fields ).
      ENDLOOP.

***    too short
***    " For F4
***    IF <ls_fieldcat>-rollname CP '*-*'.
***      SPLIT <ls_fieldcat>-rollname AT '-' INTO
***       <ls_fieldcat>-ref_table
***       <ls_fieldcat>-ref_field.
***    ENDIF.

      " Change field catalog
      READ TABLE mt_sub_field REFERENCE INTO ls_sub_field
       WITH KEY name = <ls_fieldcat>-fieldname.
      IF sy-subrc = 0.
        " <ls_fieldcat>-edit      = mo_eui_alv->ms_layout-edit.

        " Change text
        IF ls_sub_field->label IS NOT INITIAL.
          <ls_fieldcat>-coltext = ls_sub_field->label.
        ENDIF.

        " Show as link
        IF ls_sub_field->ui_type = zcl_eui_type=>mc_ui_type-string.
          <ls_fieldcat>-hotspot = abap_true.
        ENDIF.

        " For F4
        IF ls_sub_field->rollname CP '*-*'.
          SPLIT ls_sub_field->rollname AT '-' INTO
           <ls_fieldcat>-ref_table
           <ls_fieldcat>-ref_field.
        ENDIF.
      ENDIF.

      " for domain values
      IF <ls_fieldcat>-ref_table IS NOT INITIAL AND <ls_fieldcat>-ref_field IS NOT INITIAL.
        zcl_eui_type=>find_dropdown(
         EXPORTING
          io_grid      = mo_eui_alv->mo_grid
         CHANGING
          cs_fieldcat  = <ls_fieldcat>
          cv_drdn_hndl = lv_drdn_hndl ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD pbo_init.
    DATA lt_fieldcat   TYPE lvc_t_fcat.
    DATA lv_editable   TYPE abap_bool.
    FIELD-SYMBOLS:
      <lt_table>       TYPE STANDARD TABLE.

    " Get from ref
    IF me->mr_table IS NOT INITIAL.
      ASSIGN me->mr_table->*         TO <lt_table>.
    ELSE.
      ASSIGN mo_eui_alv->mr_table->* TO <lt_table>.
    ENDIF.

    " Create ALV
    CREATE OBJECT mo_eui_alv->mo_grid
      EXPORTING
        i_parent = io_container
*       i_appl_events = abap_true
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

**********************************************************************
    " Catch all events
    SET HANDLER: " on_pbo_event FOR me, on_pai_event FOR me,
      on_toolbar       FOR mo_eui_alv->mo_grid,
      on_user_command  FOR mo_eui_alv->mo_grid,
      on_top_of_page   FOR mo_eui_alv->mo_grid,
      on_hotspot_click FOR mo_eui_alv->mo_grid,
      on_double_click  FOR mo_eui_alv->mo_grid,
      on_data_changed  FOR mo_eui_alv->mo_grid.

**********************************************************************
    " VARIANT
    prepare_variant(
     CHANGING
      cs_variant = mo_eui_alv->ms_variant ).

**********************************************************************
    " LAYOUT
    prepare_layout(
     CHANGING
      cs_layout = mo_eui_alv->ms_layout ).

**********************************************************************
    " Get field catalog
    lt_fieldcat = get_field_catalog( ).

**********************************************************************
    " Editable? Set additional Events if editable
    DO 1 TIMES.
      lv_editable = mo_eui_alv->ms_layout-edit.
      CHECK lv_editable <> abap_true.

      " Check in fied catalog
      READ TABLE lt_fieldcat TRANSPORTING NO FIELDS
       WITH KEY edit = abap_true.
      CHECK sy-subrc = 0.
      lv_editable = abap_true.
    ENDDO.

    " Set events
    IF lv_editable = abap_true.
      mo_eui_alv->mo_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
      mo_eui_alv->mo_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

      mo_eui_alv->mo_grid->set_ready_for_input( 1 ).
    ENDIF.

**********************************************************************
    DATA lv_save TYPE char01 VALUE 'A'. " restrict_none

*    " Save variant - Always can save variant
*    AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
*      ID 'ACTVT' FIELD '23'.
*    IF sy-subrc <> 0.
*      lv_save = 'U'. " restrict_user_dependant
*    ENDIF.

    " And show
    mo_eui_alv->mo_grid->set_table_for_first_display(
      EXPORTING
        is_variant          = mo_eui_alv->ms_variant
        i_save              = lv_save
        is_layout           = mo_eui_alv->ms_layout
      CHANGING
        it_outtab           = <lt_table>
        it_fieldcatalog     = lt_fieldcat
        it_filter           = mo_eui_alv->mt_filter
        it_sort             = mo_eui_alv->mt_sort
      EXCEPTIONS
        OTHERS              = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD after_close.
    DATA:
      lr_data       TYPE REF TO data,
      lo_tab_desc   TYPE REF TO cl_abap_tabledescr,
      lo_struc_desc TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS:
      <lt_table_src>  TYPE STANDARD TABLE,
      <lt_table_dest> TYPE ANY TABLE,
      <lt_temp>       TYPE ANY TABLE,
      <ls_src>        TYPE any,
      <ls_dest>       TYPE any.

    CASE iv_close_cmd.
      WHEN zif_eui_manager=>mc_cmd-ok.
        " Source
        ASSIGN mr_table->* TO <lt_table_src>.

        " Destination
        ASSIGN mo_eui_alv->mr_table->* TO <lt_table_dest>.

        " Create new table for safety
        lo_tab_desc ?= cl_abap_typedescr=>describe_by_data( <lt_table_dest> ).
        CREATE DATA lr_data TYPE HANDLE lo_tab_desc .
        ASSIGN lr_data->* TO <lt_temp>.

        " Field
        lo_struc_desc ?= lo_tab_desc->get_table_line_type( ).
        CREATE DATA lr_data TYPE HANDLE lo_struc_desc.
        ASSIGN lr_data->* TO <ls_dest>.

        " And copy back
        LOOP AT <lt_table_src>  ASSIGNING <ls_src>.
          CLEAR <ls_dest>.
          MOVE-CORRESPONDING <ls_src> TO <ls_dest>.
          INSERT <ls_dest> INTO TABLE <lt_temp>.
        ENDLOOP.

        IF lines( <lt_table_src> ) = lines( <lt_temp> ).
          <lt_table_dest> = <lt_temp>.

          " Go back
          MESSAGE s019(zeui_message).
        ELSE.
          MESSAGE s020(zeui_message) WITH ms_field_desc->name DISPLAY LIKE 'E'.
          cv_close = abap_false.
        ENDIF.

      WHEN zif_eui_manager=>mc_cmd-cancel.
        IF mo_eui_alv->mv_read_only <> abap_true.
          MESSAGE s130(ed) WITH 'Edit'(edt) DISPLAY LIKE 'W'.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD on_data_changed.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'DATA_CHANGED'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'ER_DATA_CHANGED' iv_param_val_01 = er_data_changed
     iv_param_nam_02 = 'E_ONF4'          iv_param_val_02 = e_onf4
     iv_param_nam_03 = 'E_ONF4_BEFORE'   iv_param_val_03 = e_onf4_before
     iv_param_nam_04 = 'E_ONF4_AFTER'    iv_param_val_04 = e_onf4_after
     iv_param_nam_05 = 'E_UCOMM'         iv_param_val_05 = e_ucomm ).
  ENDMETHOD.

  METHOD on_double_click.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'DOUBLE_CLICK'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_ROW'           iv_param_val_01 = e_row
     iv_param_nam_02 = 'E_COLUMN'        iv_param_val_02 = e_column ).
  ENDMETHOD.


  METHOD on_hotspot_click.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'HOTSPOT_CLICK'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_ROW_ID'        iv_param_val_01 = e_row_id
     iv_param_nam_02 = 'E_COLUMN_ID'     iv_param_val_02 = e_column_id
     iv_param_nam_03 = 'ES_ROW_NO'       iv_param_val_03 = es_row_no ).

    CHECK ms_field_desc IS NOT INITIAL.

    DATA ls_field_desc  LIKE ms_field_desc.
    DATA lv_name        TYPE abap_attrname.
    DATA lv_len         TYPE i.
    DATA lv_refresh     TYPE abap_bool.
    DATA lr_cur_value   TYPE REF TO data.
    DATA lr_text        TYPE REF TO string.
    DATA lo_manager     TYPE REF TO zcl_eui_manager.
    DATA ls_layout      TYPE lvc_s_layo.

    FIELD-SYMBOLS:
      <ls_sub_field> LIKE LINE OF mt_sub_field,
      <lt_table>     TYPE STANDARD TABLE,
      <ls_item>      TYPE any,
      <lv_value>     TYPE any.

    " Get current row
    ASSIGN mr_table->* TO <lt_table>.
    READ TABLE <lt_table> ASSIGNING <ls_item> INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    " Get from field catalog
    READ TABLE mt_sub_field ASSIGNING <ls_sub_field>
     WITH TABLE KEY name = e_column_id.
    CHECK sy-subrc = 0.

    " Is table in table?
    IF <ls_sub_field>-name CP `*_UI` AND <ls_sub_field>-ui_type = zcl_eui_type=>mc_ui_type-string.
      " Get length by old abap syntax
      lv_len = strlen( <ls_sub_field>-name ).
      lv_len = lv_len - 3.
      lv_name = <ls_sub_field>-name(lv_len).
      READ TABLE mt_sub_field ASSIGNING <ls_sub_field>
       WITH TABLE KEY name = lv_name.
    ENDIF.

    " Create reference to data in table
    CREATE DATA ls_field_desc.
    MOVE-CORRESPONDING <ls_sub_field> TO ls_field_desc->*.

    " Data
    ASSIGN COMPONENT ls_field_desc->name OF STRUCTURE <ls_item> TO <lv_value>.
    CHECK <lv_value> IS ASSIGNED.
    GET REFERENCE OF <lv_value> INTO lr_cur_value.

    CASE <ls_sub_field>-ui_type.
**********************************************************************
        " Edit string in memo editor
      WHEN zcl_eui_type=>mc_ui_type-string.
        lr_text ?= lr_cur_value.
        CREATE OBJECT lo_manager TYPE zcl_eui_memo
          EXPORTING
            ir_text      = lr_text
            iv_read_only = mo_eui_alv->mv_read_only.

**********************************************************************
        " Edit sub table in alv editor
      WHEN zcl_eui_type=>mc_ui_type-table.
        IF mo_eui_alv->mv_read_only <> abap_true.
          ls_layout-edit = abap_true.
        ENDIF.

        CREATE OBJECT lo_manager TYPE zcl_eui_alv
          EXPORTING
            ir_table      = lr_cur_value
            is_field_desc = ls_field_desc
            is_layout     = ls_layout
            iv_read_only  = mo_eui_alv->mv_read_only.

**********************************************************************
        " Show range for table item
      WHEN zcl_eui_type=>mc_ui_type-range.
        lv_refresh = zcl_eui_screen=>show_range(
          is_field_desc = ls_field_desc->*
          ir_cur_value  = lr_cur_value
          iv_read_only  = mo_eui_alv->mv_read_only ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    " if have an editor
    IF lo_manager IS NOT INITIAL.
      lo_manager->popup( ).

      IF lo_manager->show( ) = zif_eui_manager=>mc_cmd-ok.
        lv_refresh = abap_true.
      ENDIF.
    ENDIF.

    " Update value
    IF lv_refresh = abap_true.
      zcl_eui_alv=>update_complex_fields(
       ir_table     = mr_table
       it_sub_field = mt_sub_field ).
      sender->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_toolbar.
    IF e_object IS NOT INITIAL.
      APPEND LINES OF mo_eui_alv->mt_toolbar TO e_object->mt_toolbar.
    ENDIF.

    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'TOOLBAR'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_OBJECT'        iv_param_val_01 = e_object
     iv_param_nam_02 = 'E_INTERACTIVE'   iv_param_val_02 = e_interactive ).
  ENDMETHOD.

  METHOD on_top_of_page.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'TOP_OF_PAGE'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_DYNDOC_ID'     iv_param_val_01 = e_dyndoc_id
     iv_param_nam_02 = 'TABLE_INDEX'     iv_param_val_02 = table_index ).
  ENDMETHOD.

  METHOD on_user_command.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'USER_COMMAND'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_UCOMM '        iv_param_val_01 = e_ucomm  ).
  ENDMETHOD.

ENDCLASS.
