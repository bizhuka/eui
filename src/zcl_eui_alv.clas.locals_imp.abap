*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_helper IMPLEMENTATION.

  METHOD constructor.
    mo_eui_alv    = io_eui_alv.
    mr_table      = io_eui_alv->mr_table.
  ENDMETHOD.

  METHOD set_field_desc.
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

    " Based on field description
    ms_field_desc = is_field_desc.

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
    lv_sum = 9999 - lv_sum.                              "#EC NUMBER_OK
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
      CONCATENATE 'View values of'(vvo) ms_field_desc->name INTO cs_layout-grid_title SEPARATED BY space.
    ELSE.
      CONCATENATE 'Edit values of'(evo) ms_field_desc->name INTO cs_layout-grid_title SEPARATED BY space.
    ENDIF.
    cs_layout-smalltitle = abap_true.
  ENDMETHOD.

  METHOD get_field_catalog.
    DATA lr_table TYPE REF TO data.
    IF me->mr_table IS NOT INITIAL. " AND me->ms_field_desc IS NOT INITIAL.
      lr_table = me->mr_table.
    ELSE.
      lr_table = mo_eui_alv->mr_table.
    ENDIF.
    rt_fieldcat = zcl_eui_type=>get_mod_catalog( ir_table       = lr_table
                                                 it_mod_catalog = mo_eui_alv->mt_mod_catalog ).
    " Change field catalog
    FIELD-SYMBOLS <ls_fieldcat> LIKE LINE OF rt_fieldcat.
    LOOP AT rt_fieldcat ASSIGNING <ls_fieldcat>.
***    too short
***    " For F4
***    IF <ls_fieldcat>-rollname CP '*-*'.
***      SPLIT <ls_fieldcat>-rollname AT '-' INTO
***       <ls_fieldcat>-ref_table
***       <ls_fieldcat>-ref_field.
***    ENDIF.

      DATA ls_sub_field TYPE REF TO zcl_eui_type=>ts_field_desc.
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
          cv_drdn_hndl = mv_drdn_hndl ).
      ENDIF.

      " Default SH for date & time
      CHECK <ls_fieldcat>-ref_table IS INITIAL AND <ls_fieldcat>-ref_field IS INITIAL.
      CASE <ls_fieldcat>-inttype.
        WHEN cl_abap_typedescr=>typekind_date.
          <ls_fieldcat>-ref_table = 'SYST'.
          <ls_fieldcat>-ref_field = 'DATUM'.
        WHEN cl_abap_typedescr=>typekind_time.
          <ls_fieldcat>-ref_table = 'SYST'.
          <ls_fieldcat>-ref_field = 'UZEIT'.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD _check_f4_table.
    FIELD-SYMBOLS <ls_f4_table> LIKE LINE OF mt_f4_table.
    LOOP AT mt_f4_table ASSIGNING <ls_f4_table>.
      " Delete from F4 ?
      DATA lv_tabix TYPE sy-tabix.
      lv_tabix = sy-tabix.

      FIELD-SYMBOLS <ls_fieldcat> LIKE LINE OF ct_fieldcat.
      READ TABLE ct_fieldcat ASSIGNING <ls_fieldcat>  "##WARN_OK
       WITH KEY fieldname = <ls_f4_table>-field.
      CHECK sy-subrc = 0.

      FIELD-SYMBOLS <lt_f4_any_table> TYPE ANY TABLE.
      ASSIGN <ls_f4_table>-tab->* TO <lt_f4_any_table>.
      CHECK sy-subrc = 0 AND lines( <lt_f4_any_table> ) <= 16.

      DATA lt_std_field TYPE tttext255.
      DATA lr_std_table TYPE REF TO data.
      _get_std_table( EXPORTING ir_ant_table = <ls_f4_table>-tab
                      IMPORTING et_std_field = lt_std_field
                                er_std_table = lr_std_table ).

      DATA lv_key_fld TYPE text255.
      DATA lv_txt_fld TYPE text255.
      CLEAR: lv_key_fld, lv_txt_fld.

      DATA lr_field    TYPE REF TO text255.
      LOOP AT lt_std_field REFERENCE INTO lr_field.
        CASE sy-tabix.
          WHEN 1.
            lv_key_fld = lv_txt_fld = lr_field->*.
          WHEN 2.
            lv_txt_fld = lr_field->*.
          WHEN 3.                                           " Or 4 ?
            CLEAR lv_key_fld.
            EXIT.
        ENDCASE.
      ENDLOOP.

      " All conditions passed. Convert to dropbox
      CHECK lv_key_fld IS NOT INITIAL.
      ADD 1 TO mv_drdn_hndl.
      <ls_fieldcat>-drdn_hndl = mv_drdn_hndl.
      <ls_fieldcat>-drdn_alias = abap_true.

      DATA lt_dropdown             TYPE lvc_t_dral.
      DATA lr_dropdown             TYPE REF TO lvc_s_dral.
      FIELD-SYMBOLS <lt_std_table> TYPE STANDARD TABLE.
      FIELD-SYMBOLS <ls_item>      TYPE any.
      FIELD-SYMBOLS <lv_key_fld>   TYPE any.
      FIELD-SYMBOLS <lv_txt_fld>   TYPE any.

      ASSIGN lr_std_table->* TO <lt_std_table>.
      LOOP AT <lt_std_table> ASSIGNING <ls_item>.
        ASSIGN COMPONENT: lv_key_fld OF STRUCTURE <ls_item> TO <lv_key_fld>,
                          lv_txt_fld OF STRUCTURE <ls_item> TO <lv_txt_fld>.

        APPEND INITIAL LINE TO lt_dropdown REFERENCE INTO lr_dropdown.
        lr_dropdown->handle    = mv_drdn_hndl.
        lr_dropdown->int_value = <lv_key_fld>.
        lr_dropdown->value     = <lv_txt_fld>.

        " Key + Text
        CHECK lr_dropdown->int_value <> lr_dropdown->value.
        CONCATENATE lr_dropdown->int_value ` - ` lr_dropdown->value INTO lr_dropdown->value.
      ENDLOOP.

      DELETE mt_f4_table INDEX lv_tabix.
    ENDLOOP.

    CHECK lt_dropdown IS NOT INITIAL.
    io_grid->set_drop_down_table( it_drop_down_alias = lt_dropdown ).
  ENDMETHOD.

  METHOD is_editable.
    rv_editable = mo_eui_alv->ms_layout-edit.
    CHECK rv_editable <> abap_true.

    " Check in fied catalog
    READ TABLE mo_eui_alv->mt_mod_catalog TRANSPORTING NO FIELDS
     WITH KEY edit = abap_true.
    CHECK sy-subrc = 0.
    rv_editable = abap_true.
  ENDMETHOD.

  METHOD pbo_init.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    " Get from ref
    IF me->mr_table IS NOT INITIAL.
      ASSIGN me->mr_table->*         TO <lt_table>.
    ELSE.
      ASSIGN mo_eui_alv->mr_table->* TO <lt_table>.
    ENDIF.

    " Create ALV
    DATA lv_has_top   TYPE abap_bool.
    DATA lo_splitter  TYPE REF TO cl_gui_splitter_container.
    DATA lo_top       TYPE REF TO cl_gui_container.
    DATA lo_container TYPE REF TO cl_gui_container.

**********************************************************************
    " Has event handler for TOP_OF_PAGE
    lv_has_top = mo_eui_alv->mo_event_caller->has_handler(
        iv_of_class  = 'CL_GUI_ALV_GRID'
        iv_for_event = 'TOP_OF_PAGE' ).
    IF mv_top_of_page_height IS INITIAL.
      CLEAR lv_has_top.
    ENDIF.

    IF lv_has_top <> abap_true.
      lo_container = io_container.
    ELSE.
      CREATE OBJECT lo_splitter
        EXPORTING
          parent  = io_container
          rows    = 2
          columns = 1.
      lo_top       = lo_splitter->get_container( row       = 1
                                                 column    = 1 ).
      lo_container = lo_splitter->get_container( row       = 2
                                                 column    = 1 ).
      lo_splitter->set_row_height( id     = 1
                                   height = mv_top_of_page_height ).
    ENDIF.

    CREATE OBJECT mo_eui_alv->mo_grid
      EXPORTING
        i_parent = lo_container
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
      on_toolbar                   FOR mo_eui_alv->mo_grid,
      on_menu_button               FOR mo_eui_alv->mo_grid,
      on_user_command              FOR mo_eui_alv->mo_grid,
      on_top_of_page               FOR mo_eui_alv->mo_grid,
      on_hotspot_click             FOR mo_eui_alv->mo_grid,
      on_double_click              FOR mo_eui_alv->mo_grid,
      on_button_click              FOR mo_eui_alv->mo_grid,
      on_data_changed              FOR mo_eui_alv->mo_grid,
      on_data_changed_finished     FOR mo_eui_alv->mo_grid,
      on_context_menu_request      FOR mo_eui_alv->mo_grid,
      on_f4                        FOR mo_eui_alv->mo_grid,
      on_after_refresh             FOR mo_eui_alv->mo_grid,
      on_delayed_changed_selection FOR mo_eui_alv->mo_grid.

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
    DATA lt_fieldcat TYPE lvc_t_fcat.
    lt_fieldcat = get_field_catalog( ).
    _check_f4_table( EXPORTING io_grid     = mo_eui_alv->mo_grid
                     CHANGING  ct_fieldcat = lt_fieldcat ).

**********************************************************************
    " Has event handler for ONF4
    DATA lv_has_f4   TYPE abap_bool.
    DATA lt_f4       TYPE lvc_t_f4.
    DATA ls_f4       TYPE lvc_s_f4.
    DATA lr_fieldcat TYPE REF TO lvc_s_fcat.

    DO 1 TIMES.
      lv_has_f4 = mo_eui_alv->mo_event_caller->has_handler(
              iv_of_class  = 'CL_GUI_ALV_GRID'
              iv_for_event = 'ONF4' ).
      IF mt_f4_table IS NOT INITIAL.
        lv_has_f4 = abap_true.
      ENDIF.
      CHECK lv_has_f4 = abap_true.

      LOOP AT lt_fieldcat REFERENCE INTO lr_fieldcat WHERE f4availabl = abap_true.
        ls_f4-fieldname = lr_fieldcat->fieldname.
        ls_f4-register  = abap_true.
        INSERT ls_f4 INTO TABLE lt_f4.
      ENDLOOP.

      CHECK lt_f4[] IS NOT INITIAL.
      mo_eui_alv->mo_grid->register_f4_for_fields( it_f4 = lt_f4 ).
    ENDDO.

**********************************************************************
    " Has event handler for ONF4
    DATA lv_delayed_changed_selection TYPE abap_bool.
    DO 1 TIMES.
      lv_delayed_changed_selection = mo_eui_alv->mo_event_caller->has_handler(
              iv_of_class  = 'CL_GUI_ALV_GRID'
              iv_for_event = 'DELAYED_CHANGED_SEL_CALLBACK' ).
      CHECK lv_delayed_changed_selection = abap_true.

      mo_eui_alv->mo_grid->register_delayed_event(
       i_event_id = cl_gui_alv_grid=>mc_evt_delayed_change_select ).
    ENDDO.

**********************************************************************
    " Editable? Set additional Events if editable
    IF is_editable( ) = abap_true.
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

    " Raise TOP_OF_PAGE
    CHECK lo_top IS NOT INITIAL.

    CREATE OBJECT mo_dyndoc
      EXPORTING
        style = 'ALV_GRID'.
    mo_eui_alv->mo_grid->list_processing_events( i_event_name = 'TOP_OF_PAGE'
                                                 i_dyndoc_id  = mo_dyndoc ).

    mo_dyndoc->display_document(
      EXPORTING
        reuse_control = abap_true
        parent        = lo_top
      EXCEPTIONS
        OTHERS        = 0 ).
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
    IF mo_eui_alv->mt_skip_msg[] IS NOT INITIAL AND er_data_changed IS NOT INITIAL.
      DATA: lr_item  TYPE REF TO lvc_s_msg1,
            lv_tabix TYPE sytabix.
      LOOP AT er_data_changed->mt_protocol[] REFERENCE INTO lr_item.
        lv_tabix = sy-tabix.
        CHECK mo_eui_alv->is_skipped(
                iv_msgid = lr_item->msgid
                iv_msgno = lr_item->msgno
                iv_msgty = lr_item->msgty ) = abap_true.

        DELETE er_data_changed->mt_protocol[] INDEX lv_tabix.
      ENDLOOP.
    ENDIF.

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

  METHOD on_data_changed_finished.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'DATA_CHANGED_FINISHED'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_MODIFIED'      iv_param_val_01 = e_modified
     iv_param_nam_02 = 'ET_GOOD_CELLS'   iv_param_val_02 = et_good_cells ).
  ENDMETHOD.

  METHOD on_double_click.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'DOUBLE_CLICK'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_ROW'           iv_param_val_01 = e_row
     iv_param_nam_02 = 'E_COLUMN'        iv_param_val_02 = e_column
     iv_param_nam_03 = 'ES_ROW_NO'       iv_param_val_03 = es_row_no ).
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
    READ TABLE mt_sub_field ASSIGNING <ls_sub_field> "##WARN_OK
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
            ir_text     = lr_text
            iv_editable = mo_eui_alv->ms_layout-edit.

**********************************************************************
        " Edit sub table in alv editor
      WHEN zcl_eui_type=>mc_ui_type-table.
        IF mo_eui_alv->mv_read_only <> abap_true.
          ls_layout-edit = abap_true.
        ENDIF.

        DATA lo_alv TYPE REF TO zcl_eui_alv.
        CREATE OBJECT lo_alv
          EXPORTING
            ir_table  = lr_cur_value
            is_layout = ls_layout.
        lo_alv->set_field_desc( ls_field_desc ).
        lo_manager = lo_alv.
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
      DATA ls_stable TYPE lvc_s_stbl.
      ls_stable-col = ls_stable-row = abap_true.
      sender->refresh_table_display( is_stable = ls_stable ).
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

  METHOD on_menu_button.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'MENU_BUTTON'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_OBJECT'        iv_param_val_01 = e_object
     iv_param_nam_02 = 'E_UCOMM'         iv_param_val_02 = e_ucomm ).
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
     iv_param_nam_01 = 'E_UCOMM'         iv_param_val_01 = e_ucomm  ).
  ENDMETHOD.

  METHOD on_button_click.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'BUTTON_CLICK'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'ES_COL_ID'       iv_param_val_01 = es_col_id
     iv_param_nam_02 = 'ES_ROW_NO'       iv_param_val_02 = es_row_no ).
  ENDMETHOD.

  METHOD on_after_refresh.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'AFTER_REFRESH'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender ).
  ENDMETHOD.

  METHOD on_context_menu_request.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'CONTEXT_MENU_REQUEST'
     iv_param_nam_00 = 'SENDER'       iv_param_val_00 = sender
     iv_param_nam_01 = 'E_OBJECT'     iv_param_val_01 = e_object ).
  ENDMETHOD.

  METHOD on_f4.
    CHECK _self_f4( io_grid      = sender
                    iv_fieldname = e_fieldname
                    is_row_no    = es_row_no ) <> abap_true.

    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'ONF4'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_FIELDNAME'     iv_param_val_01 = e_fieldname
     iv_param_nam_02 = 'E_FIELDVALUE'    iv_param_val_02 = e_fieldvalue
     iv_param_nam_03 = 'ES_ROW_NO'       iv_param_val_03 = es_row_no
     iv_param_nam_04 = 'ER_EVENT_DATA'   iv_param_val_04 = er_event_data
     iv_param_nam_05 = 'ET_BAD_CELLS'    iv_param_val_05 = et_bad_cells
     iv_param_nam_06 = 'E_DISPLAY'       iv_param_val_06 = e_display ).
  ENDMETHOD.

  METHOD _self_f4.
    " self process ?
    DATA lr_f4_table TYPE REF TO zcl_eui_alv=>ts_f4_table.
    READ TABLE mt_f4_table REFERENCE INTO lr_f4_table
     WITH TABLE KEY field = iv_fieldname.
    CHECK sy-subrc = 0.
    rv_self = abap_true.

    " Get STD table
    DATA lr_std_table  TYPE REF TO data.
    DATA lt_std_field  TYPE tttext255.
    DATA lv_key_field  TYPE dfies-fieldname.
    _get_std_table( EXPORTING ir_ant_table = lr_f4_table->tab
                    IMPORTING er_std_table = lr_std_table
                              et_std_field = lt_std_field ).
    CHECK lr_std_table IS NOT INITIAL.

    FIELD-SYMBOLS <lt_std_table> TYPE STANDARD TABLE.
    ASSIGN lr_std_table->* TO <lt_std_table>.

    " Key is alwasy first
    READ TABLE lt_std_field INTO lv_key_field INDEX 1.
    CHECK lv_key_field IS NOT INITIAL.

    DATA lt_return TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.
    DATA lr_return TYPE REF TO ddshretval.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield   = lv_key_field
        value_org  = 'S'
      TABLES
        value_tab  = <lt_std_table>
        return_tab = lt_return
      EXCEPTIONS
        OTHERS     = 3.
    CHECK sy-subrc = 0.

    " Only 1 value
    READ TABLE lt_return REFERENCE INTO lr_return INDEX 1.
    CHECK sy-subrc = 0.

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_item>  TYPE any.
    FIELD-SYMBOLS <lv_dest>  TYPE any.

    " Get current row
    ASSIGN mr_table->* TO <lt_table>.
    READ TABLE <lt_table> ASSIGNING <ls_item> INDEX is_row_no-row_id.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE <ls_item> TO <lv_dest>.
    <lv_dest> = lr_return->fieldval.

    DATA ls_stable TYPE lvc_s_stbl.
    ls_stable-col = ls_stable-row = abap_true.
    io_grid->refresh_table_display( is_stable = ls_stable ).
  ENDMETHOD.

  METHOD on_delayed_changed_selection.
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'DELAYED_CHANGED_SEL_CALLBACK'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender ).
  ENDMETHOD.

  METHOD _get_std_table.
    CLEAR: er_std_table,
           et_std_field.

    FIELD-SYMBOLS <lt_f4_any_table> TYPE ANY TABLE.
    ASSIGN ir_ant_table->* TO <lt_f4_any_table>.
    CHECK <lt_f4_any_table> IS ASSIGNED.

    " Get table info
    DATA lr_tdesc TYPE REF TO cl_abap_tabledescr.
    DATA lr_sdesc TYPE REF TO cl_abap_structdescr.
    lr_tdesc ?= cl_abap_tabledescr=>describe_by_data( <lt_f4_any_table> ).
    lr_sdesc ?= lr_tdesc->get_table_line_type( ).

    " Always 1 field
    DATA ls_key LIKE LINE OF lr_tdesc->key.
    READ TABLE lr_tdesc->key INTO ls_key INDEX 1.
    APPEND ls_key-name TO et_std_field.

    " For new structure
    DATA lr_comp      TYPE REF TO abap_compdescr.
    DATA lt_any_comp  TYPE cl_abap_structdescr=>component_table.
    DATA lt_std_comp  TYPE cl_abap_structdescr=>component_table.
    DATA ls_component LIKE LINE OF lt_any_comp.

    " For ignoring fields
    lt_any_comp = lr_sdesc->get_components( ).
    LOOP AT lr_sdesc->components REFERENCE INTO lr_comp WHERE type_kind <> cl_abap_typedescr=>typekind_table
                                                          AND type_kind <> cl_abap_typedescr=>typekind_string.
      READ TABLE lt_any_comp INTO ls_component WITH KEY name = lr_comp->name.
      APPEND ls_component TO lt_std_comp.

      CHECK lr_comp->name <> ls_key-name.
      APPEND lr_comp->name TO et_std_field.
    ENDLOOP.
    lr_sdesc = cl_abap_structdescr=>create( lt_std_comp ).

    " Result
    lr_tdesc = cl_abap_tabledescr=>create( p_line_type = lr_sdesc  ).
    CREATE DATA er_std_table TYPE HANDLE lr_tdesc.

    " Fill STD from ANY
    _fill_std_corresponding( ir_any_f4_table = ir_ant_table
                             ir_std_f4_table = er_std_table ).
  ENDMETHOD.

  METHOD _fill_std_corresponding.
    FIELD-SYMBOLS <lt_f4_any_table> TYPE ANY TABLE.
    FIELD-SYMBOLS <lt_f4_std_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_src>          TYPE any.
    FIELD-SYMBOLS <ls_dest>         TYPE any.

    ASSIGN ir_any_f4_table->* TO <lt_f4_any_table>.
    ASSIGN ir_std_f4_table->* TO <lt_f4_std_table>.

    LOOP AT <lt_f4_any_table> ASSIGNING <ls_src>.
      APPEND INITIAL LINE TO <lt_f4_std_table> ASSIGNING <ls_dest>.
      MOVE-CORRESPONDING <ls_src> TO <ls_dest>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
