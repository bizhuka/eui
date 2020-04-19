*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_screen IMPLEMENTATION.
  METHOD constructor.
    mo_eui_screen = io_eui_screen.
    mr_context = ir_context.
  ENDMETHOD.

  " Fill mapping
  METHOD fill_from_context.
    DATA ls_map                TYPE zcl_eui_screen=>ts_map.
    DATA lt_unq_type           TYPE REF TO zcl_eui_type=>tt_unique_type.
    DATA lo_struc              TYPE REF TO cl_abap_structdescr.
    DATA ls_comp               TYPE REF TO abap_compdescr.
    DATA lv_index              TYPE num2.
    DATA ls_fieldcat           TYPE lvc_s_fcat.
    DATA lt_unq_rollname       TYPE SORTED TABLE OF zcl_eui_type=>ts_field_desc-rollname WITH UNIQUE KEY table_line.
    DATA lv_text               TYPE string.
    FIELD-SYMBOLS <ls_context> TYPE any.
    FIELD-SYMBOLS <lv_field>   TYPE any.

    " Only if data passed
    CHECK mr_context IS NOT INITIAL.

    ASSIGN mr_context->* TO <ls_context>.
    CREATE DATA lt_unq_type.

    " Each field is parameter or select-option
    lo_struc ?= cl_abap_structdescr=>describe_by_data( <ls_context> ).
    LOOP AT lo_struc->components REFERENCE INTO ls_comp.
      lv_index = sy-tabix.

      " Nome of paramater
      CLEAR ls_map.
      ls_map-par_name = get_parameter_name( is_comp  = ls_comp
                                            iv_index = lv_index ).

      " Save current value in ref
      ASSIGN COMPONENT ls_comp->name OF STRUCTURE <ls_context> TO <lv_field>.
      GET REFERENCE OF <lv_field> INTO ls_map-cur_value.

      " Get description
      ls_map-field_desc = zcl_eui_type=>get_field_desc(
       iv_field_name  = ls_comp->name
       iv_data        = <lv_field>
       ir_unique_type = lt_unq_type ).

      " Is listbox ?
      CASE ls_map-ui_type.
        WHEN zcl_eui_type=>mc_ui_type-range.

        WHEN zcl_eui_type=>mc_ui_type-table OR zcl_eui_type=>mc_ui_type-string.
          ls_map-rollname = 'SYINDEX'.
          zcl_eui_type=>find_table_fieldname(
           EXPORTING
            ir_unique_type = lt_unq_type
           CHANGING
            cv_rollname    = ls_map-rollname ).

        WHEN OTHERS.
          " № 0
          zcl_eui_type=>split_type(
           EXPORTING iv_datatype = ls_map-rollname
           IMPORTING ev_table    = ls_fieldcat-ref_table
                     ev_field    = ls_fieldcat-ref_field ).

          zcl_eui_type=>is_list_box(
           EXPORTING
             iv_tabname   = ls_fieldcat-ref_table
             iv_fieldname = ls_fieldcat-ref_field
           IMPORTING
             ev_list_box  = ls_map-is_list_box ).
      ENDCASE.

      INSERT ls_map-rollname INTO TABLE lt_unq_rollname.
      " Check
      IF sy-subrc <> 0 OR ls_map-rollname NP '*-*'.
        zcx_eui_exception=>raise_sys_error( iv_message = 'Cannot create a unique pair TABLE-FIELD_NAME' ).
      ENDIF.

      " № 1
      IF  ls_map-ui_type = zcl_eui_type=>mc_ui_type-table
       OR ls_map-ui_type = zcl_eui_type=>mc_ui_type-string
       OR ls_map-ui_type = zcl_eui_type=>mc_ui_type-range.

        zcl_eui_type=>split_type(
         EXPORTING iv_datatype = ls_map-rollname
         IMPORTING ev_table    = ls_fieldcat-ref_table
                   ev_field    = ls_fieldcat-ref_field ).

        " And add
        lv_text = ls_fieldcat-ref_table.
        INSERT lv_text INTO TABLE me->mt_unq_table.
      ENDIF.

      APPEND ls_map TO me->mt_map.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_parameter_name.
    rv_name = is_comp->name.
  ENDMETHOD.

  METHOD customize.
    DATA ls_map                TYPE REF TO zcl_eui_screen=>ts_map.
    DATA lv_message            TYPE string.

    IF is_screen-name IS NOT INITIAL OR is_screen-group1 IS NOT INITIAL.
      APPEND is_screen TO mt_screen.
    ENDIF.

    " IF field exist
    CHECK is_map-name IS NOT INITIAL.
    READ TABLE mt_map REFERENCE INTO ls_map
     WITH KEY name = is_map-name.
    IF sy-subrc <> 0.
      CONCATENATE `Unknown field name ` is_map-name INTO lv_message.
      zcx_eui_exception=>raise_dump( iv_message = lv_message ).
    ENDIF.

    " Label, input, required or sub_fdesc
    zcl_eui_conv=>move_corresponding(
     EXPORTING
       is_source         = is_map
       iv_except_initial = abap_true
     CHANGING
       cs_destination    = ls_map->* ).
  ENDMETHOD.

  METHOD show.
  ENDMETHOD.

  METHOD pbo.
    CHECK iv_after = abap_true.

    " Cannot call LOOP AT ACREEN & set SCREEN data
    IF  sy-dynnr <> mo_eui_screen->ms_screen-dynnr.
      " TODO OR sy-cprog <> ms_screen->prog.
      RETURN.
    ENDIF.

**********************************************************************
    " Set parameters from context ( 1 time only )
    IF mv_pbo_init_params = abap_true.
      mv_pbo_init_params = abap_false.

      DATA ls_map                TYPE REF TO zcl_eui_screen=>ts_map.
      DATA lv_name               TYPE string.
      FIELD-SYMBOLS <lv_param>   TYPE any.
      FIELD-SYMBOLS <lv_src>     TYPE any.

      LOOP AT mt_map REFERENCE INTO ls_map WHERE ui_type <> zcl_eui_type=>mc_ui_type-table
                                             AND ui_type <> zcl_eui_type=>mc_ui_type-string.
        " Name of parameter
        CONCATENATE `(` mo_eui_screen->ms_screen-prog `)` ls_map->par_name INTO lv_name.

        " Name of SELECT-OPTION
        IF ls_map->ui_type = zcl_eui_type=>mc_ui_type-range.
          CONCATENATE lv_name `[]` INTO lv_name.
        ENDIF.

        ASSIGN (lv_name) TO <lv_param>.
        IF sy-subrc <> 0.
          MESSAGE s014(zeui_message) WITH ls_map->par_name sy-cprog INTO sy-msgli.
          zcx_eui_exception=>raise_dump( iv_message = sy-msgli ).
        ENDIF.

        ASSIGN ls_map->cur_value->* TO <lv_src>.
        <lv_param> = <lv_src>.
      ENDLOOP.
    ENDIF.

**********************************************************************
    " Set parameters labels ( 1 time only )
    IF mv_pbo_set_labels = abap_true.
      mv_pbo_set_labels = abap_false.

      LOOP AT mt_map REFERENCE INTO ls_map.
        CONCATENATE `(` mo_eui_screen->ms_screen-prog `)%_` ls_map->par_name `_%_APP_%-TEXT` INTO lv_name.
        ASSIGN (lv_name) TO <lv_param>.
        CHECK sy-subrc = 0.

        <lv_param> = ls_map->label.
      ENDLOOP.
    ENDIF.

**********************************************************************
    " LOOP AT SCREEN

    DATA lv_rem                   TYPE string.
    DATA ls_screen_dst            TYPE screen.
    DATA ls_screen_src            TYPE screen.
    DATA lt_scr_fields            TYPE abap_compdescr_tab. " Filled 1 time with all ls_screen fields
    DATA lv_input                 TYPE screen-input.
    FIELD-SYMBOLS <ls_screen_src> LIKE LINE OF mt_screen.

    LOOP AT SCREEN.
      " Have rule or not ?
      ls_screen_dst = screen.

      " Detect name
      SPLIT ls_screen_dst-name AT '-' INTO lv_name lv_rem.
      REPLACE FIRST OCCURRENCE OF '_%_APP_%' IN lv_name WITH ''.
      IF sy-subrc = 0.
        lv_name = lv_name+2.
      ENDIF.

      " Find in map
      READ TABLE mt_map REFERENCE INTO ls_map
       WITH KEY par_name = lv_name.
      IF sy-subrc = 0.
        " Set required for parameter or select-option
        IF ls_map->required IS NOT INITIAL AND ( ls_screen_dst-group3 = 'PAR' OR ls_screen_dst-group3 = 'LOW' ).
          ls_screen_dst-required = ls_map->required.
        ENDIF.

        lv_input = ''.
        IF ls_map->input = '0'. " AND ls_screen_dst-group3 <> 'VPU'. " But not for tables
          lv_input = '0'.
        ENDIF.

        IF ls_map->ui_type = zcl_eui_type=>mc_ui_type-table OR
           ls_map->ui_type = zcl_eui_type=>mc_ui_type-string.

          IF ls_screen_dst-group3 = 'LOW' OR ls_screen_dst-group3 = 'TOT' OR ls_screen_dst-group3 = 'HGH'.
            ls_screen_dst-active    = '0'.
            ls_screen_dst-invisible = '1'.
            lv_input                = '0'.
          ENDIF.
        ENDIF.

        " Do not edit
        IF lv_input = '0'.
          ls_screen_dst-input     = '0'.
        ENDIF.
      ENDIF.

      LOOP AT mt_screen ASSIGNING <ls_screen_src>.
        " Start of PSEUDO group (1 symbol)
        IF <ls_screen_src>-name = '+'.
          ls_screen_src = <ls_screen_src>.
          CONTINUE.
        ELSEIF <ls_screen_src>-name(1) = '+'.
          " PSEUDO group continue
          ls_screen_src-name = <ls_screen_src>-name+1.
        ELSE.
          ls_screen_src = <ls_screen_src>.
        ENDIF.

        IF ls_screen_src-group1 IS NOT INITIAL.      " By group 01
          CHECK ls_screen_dst-group1 = ls_screen_src-group1.
        ELSEIF ls_screen_src-name CS '*'.            " By mask
          CHECK ls_screen_dst-name   CP ls_screen_src-name.
        ELSE.
          CHECK ls_screen_dst-name   = ls_screen_src-name.
        ENDIF.

        CLEAR ls_screen_src-name. " Do not copy SCREEN-NAME

        zcl_eui_conv=>move_corresponding(
         EXPORTING
           is_source         = ls_screen_src
           iv_except_initial = abap_true    " <--- Move-corresponding except initial
         CHANGING
           cs_destination    = ls_screen_dst
           ct_component      = lt_scr_fields ).
      ENDLOOP.

      " Special case for selection push button
      IF ls_screen_dst-group3 = 'VPU' AND ls_screen_dst-active = '1'.
        ls_screen_dst-input = '1'.
      ENDIF.

      " And change ls_screen
      CHECK ls_screen_dst <> screen.
      screen = ls_screen_dst.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD read_from_screen.
    DATA lv_prog               TYPE string.
    DATA lv_param              TYPE string.
    DATA lv_name               TYPE string.
    FIELD-SYMBOLS <lv_param>   TYPE any.
    FIELD-SYMBOLS <lv_dest>    TYPE any.

    " Get name of paramater or range
    lv_param = ir_map->par_name.
    lv_prog  = mo_eui_screen->ms_screen-prog.

    " Name of SELECT-OPTION
    IF ir_map->ui_type = zcl_eui_type=>mc_ui_type-range.
      CONCATENATE lv_param `[]` INTO lv_param.
    ENDIF.

    " Name of parameter
    CONCATENATE `(` lv_prog `)` lv_param INTO lv_name.

    ASSIGN (lv_name) TO <lv_param>.
    IF sy-subrc <> 0.
      MESSAGE s014(zeui_message) WITH ir_map->par_name sy-cprog INTO sy-msgli.
      zcx_eui_exception=>raise_dump( iv_message = sy-msgli ).
    ENDIF.

    ASSIGN ir_map->cur_value->* TO <lv_dest>.
    <lv_dest> = <lv_param>.
  ENDMETHOD.

  METHOD check_pai.
    IF cv_close = abap_true.
      cv_read_after = abap_true.
    ENDIF.

    cv_map_index = -1.
    IF iv_command CP '%*'
      AND ( iv_command+4(4)   = mo_eui_screen->ms_screen-dynnr
         OR iv_command+4(4)   = space ).
      cv_map_index = 0.
    ENDIF.
  ENDMETHOD.

  METHOD call_editor.
    DATA ls_map        TYPE REF TO zcl_eui_screen=>ts_map.
    DATA lr_text       TYPE REF TO string.
    DATA lo_manager    TYPE REF TO zif_eui_manager.
    DATA lr_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc.
    DATA lv_read_only  TYPE abap_bool.
    DATA ls_layout     TYPE lvc_s_layo.

    " Get current field
    READ TABLE mt_map REFERENCE INTO ls_map INDEX iv_map_index.
    CHECK sy-subrc = 0.

    " Edit or not
    IF mo_eui_screen->mv_read_only = abap_true OR ls_map->input = '0'.
      lv_read_only   = abap_true.
    ELSE.
      ls_layout-edit = abap_true.
    ENDIF.

    CASE ls_map->ui_type.
      WHEN zcl_eui_type=>mc_ui_type-table.
        " As refernce
        GET REFERENCE OF ls_map->field_desc INTO lr_field_desc.

        CREATE OBJECT lo_manager TYPE zcl_eui_alv
          EXPORTING
            ir_table      = ls_map->cur_value
            is_field_desc = lr_field_desc
            is_layout     = ls_layout
            iv_read_only  = lv_read_only.

      WHEN zcl_eui_type=>mc_ui_type-string.
        lr_text ?= ls_map->cur_value.
        CREATE OBJECT lo_manager TYPE zcl_eui_memo
          EXPORTING
            ir_text      = lr_text
            iv_read_only = lv_read_only.

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    " Yes is editor
    CHECK lo_manager IS NOT INITIAL.

    lo_manager->popup( ).

    lo_manager->show( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_scr_free IMPLEMENTATION.
  METHOD get_parameter_name.
    CONCATENATE `%%DYN0` iv_index INTO rv_name.
  ENDMETHOD.

  METHOD pbo.
    IF iv_before = abap_true.
      " Do not set pf-status
      cv_set_status      = abap_false.
      mv_pbo_init_params = abap_false.
      mv_pbo_set_labels  = abap_false.
      RETURN.
    ENDIF.

    CHECK iv_after = abap_true.
    DATA lv_set_status TYPE abap_bool VALUE abap_true.

    CASE sy-dynnr.
      WHEN '0100'. " for full screen TODO

      WHEN '1104'. " for popup
        TYPES rsscr_tab         TYPE STANDARD TABLE OF rsscr WITH DEFAULT KEY.
        FIELD-SYMBOLS <lt_sscr> TYPE rsscr_tab.

        ASSIGN ('(SAPLSSEL)%_SSCR[]') TO <lt_sscr> CASTING.
        CHECK sy-subrc = 0.

        " Hide action for SELECT-OPTION VALU_PUSH
        LOOP AT mt_map TRANSPORTING NO FIELDS
           WHERE ui_type = zcl_eui_type=>mc_ui_type-table
              OR ui_type = zcl_eui_type=>mc_ui_type-string.
          sy-tabix = 1105 * 1000 + sy-tabix * 2 + 2.  "TODO
          DELETE <lt_sscr> WHERE numb = sy-tabix.
        ENDLOOP.

      WHEN OTHERS.
        CLEAR lv_set_status.
    ENDCASE.

    " set status
    IF lv_set_status = abap_true.
      CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
        EXPORTING
          p_status  = mo_eui_screen->ms_status-name
          p_program = mo_eui_screen->ms_status-prog
        TABLES
          p_exclude = mo_eui_screen->ms_status-exclude.
    ENDIF.

    " And call parent method
    super->pbo( iv_after = iv_after ).
  ENDMETHOD.

  METHOD check_pai.
    FIELD-SYMBOLS <ls_sscrfields> TYPE sscrfields.
    ASSIGN ('(SAPLSSEL)SSCRFIELDS') TO <ls_sscrfields>.

    super->check_pai(
     EXPORTING
       iv_command    = iv_command
     CHANGING
       cv_close      = cv_close
       cv_read_after = cv_read_after
       cv_map_index  = cv_map_index ).

    " Do not read after close
    cv_read_after = abap_false.

    DO 1 TIMES.
      " Detect index
      CHECK cv_map_index = 0.
      cv_map_index = iv_command+1(3) / 2 - 1.

      " Is ok
      CHECK cv_map_index > 0 AND <ls_sscrfields> IS ASSIGNED.

      " Get current field info
      DATA ls_map TYPE REF TO zcl_eui_screen=>ts_map.
      READ TABLE mt_map REFERENCE INTO ls_map INDEX cv_map_index.
      CHECK sy-subrc = 0.

      CHECK ls_map->ui_type = zcl_eui_type=>mc_ui_type-table
         OR ls_map->ui_type = zcl_eui_type=>mc_ui_type-string.

      " Clear event if editor
      CLEAR <ls_sscrfields>-ucomm.
      RETURN.
    ENDDO.

    " Change command
    IF cv_close = abap_true AND <ls_sscrfields> IS ASSIGNED.
      IF mo_eui_screen->mv_close_cmd = zif_eui_manager=>mc_cmd-cancel.
        <ls_sscrfields>-ucomm = zif_eui_manager=>mc_cmd-cancel.
      ELSE.
        <ls_sscrfields>-ucomm = zif_eui_manager=>mc_cmd-return.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD show.
    DATA lt_range          TYPE rsds_trange.

    DATA lr_map            TYPE REF TO zcl_eui_screen=>ts_map.
    DATA lt_event          TYPE STANDARD TABLE OF rsdsevents.
    DATA ls_event          TYPE REF TO rsdsevents.
    DATA ls_restrict       TYPE sscr_restrict_ds.
    DATA ls_list_tab       TYPE REF TO sscr_opt_list.
    DATA lt_scr_fld        TYPE STANDARD TABLE OF rsdsfields.
    DATA ls_scr_fld        TYPE rsdsfields.
    DATA lt_scr_fld_txt    TYPE STANDARD TABLE OF rsdstexts.
    DATA ls_scr_fld_txt    TYPE rsdstexts.
    DATA ls_scr_fld_value  TYPE sscr_ass_ds.
    DATA lt_scr_fld_evt    TYPE STANDARD TABLE OF rsdsevflds.
    DATA ls_scr_fld_evt    TYPE rsdsevflds.
    DATA ls_range          TYPE REF TO rsds_range.
    DATA ls_range_sub1     TYPE REF TO rsds_frange.
    DATA ls_range_sub2     TYPE REF TO rsdsselopt.
    DATA lr_data           TYPE REF TO data.
    DATA lv_message        TYPE string.
    FIELD-SYMBOLS <lt_val> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_val> TYPE any.
    FIELD-SYMBOLS <lv_val> TYPE any.

    CHECK iv_after = abap_true.

    " events
    APPEND INITIAL LINE TO lt_event REFERENCE INTO ls_event.
    ls_event->event = 'O'. " AT SELECTION-SCREEN OUTPUT
    ls_event->prog  = zcl_eui_manager=>mc_eui_screen_fugr.
    ls_event->form  = 'FREE_SCREEN_PBO'.

    APPEND INITIAL LINE TO lt_event REFERENCE INTO ls_event.
    ls_event->event = 'A'. " AT SELECTION-SCREEN
    ls_event->prog  = zcl_eui_manager=>mc_eui_screen_fugr.
    ls_event->form  = 'FREE_SCREEN_PAI'.

    APPEND INITIAL LINE TO ls_restrict-opt_list_tab REFERENCE INTO ls_list_tab.
    ls_list_tab->name       = 'JUST_EQ'.
    ls_list_tab->options-eq = abap_true.

    LOOP AT mt_map REFERENCE INTO lr_map.
      CLEAR:
       ls_scr_fld,
       ls_scr_fld_txt,
       ls_scr_fld_value,
       ls_scr_fld_evt.

      " General options
      ls_scr_fld_value-kind = 'S'.
      ls_scr_fld_txt-text   = lr_map->label.

      " Find for table
      IF lr_map->ui_type = zcl_eui_type=>mc_ui_type-table OR lr_map->ui_type = zcl_eui_type=>mc_ui_type-string.
        " Option
        ls_scr_fld_value-sg_main       = '*'.
        ls_scr_fld_value-sg_addy       = ' '.
      ELSE.
        CASE lr_map->ui_type.
          WHEN zcl_eui_type=>mc_ui_type-range.
            " Option
            ls_scr_fld_value-sg_main       = '*'.
            ls_scr_fld_value-sg_addy       = ' '.
            " ls_scr_fld_value-op_main       = JUST_EQ | NOINTERVLS | NOPATTERN

          WHEN zcl_eui_type=>mc_ui_type-table OR zcl_eui_type=>mc_ui_type-string.

          WHEN OTHERS. "ZCL_EUI_TYPE=>mc_ui_type-parameter.
            " Parameter
            ls_scr_fld_value-sg_main       = 'I'.
            ls_scr_fld_value-sg_addy       = 'N'.
            ls_scr_fld_value-op_main       = 'JUST_EQ'.

        ENDCASE.
      ENDIF.

      " Should already prepared
      CLEAR ls_scr_fld-fieldname.
      SPLIT lr_map->rollname AT '-' INTO
        ls_scr_fld-tablename
        ls_scr_fld-fieldname.

      " Oops!
      IF ls_scr_fld-fieldname IS INITIAL.
        CONCATENATE `Cannot split ` lr_map->rollname INTO lv_message.
        zcx_eui_exception=>raise_dump( iv_message = lv_message ).
      ENDIF.


      " Fields
      APPEND ls_scr_fld TO lt_scr_fld.

      " Text
      ls_scr_fld_txt-tablename  = ls_scr_fld-tablename.
      ls_scr_fld_txt-fieldname  = ls_scr_fld-fieldname.
      APPEND ls_scr_fld_txt TO lt_scr_fld_txt.

      " Option
      ls_scr_fld_value-tablename   = ls_scr_fld-tablename.
      ls_scr_fld_value-fieldname   = ls_scr_fld-fieldname.
      APPEND ls_scr_fld_value TO ls_restrict-ass_tab.

      " Event
      IF ls_scr_fld_evt IS NOT INITIAL.
        ls_scr_fld_evt-tablename   = ls_scr_fld-tablename.
        ls_scr_fld_evt-fieldname   = ls_scr_fld-fieldname.
        APPEND ls_scr_fld_evt TO lt_scr_fld_evt.
      ENDIF.

************************
      " Values
************************
      CHECK lr_map->ui_type <> zcl_eui_type=>mc_ui_type-table AND
            lr_map->ui_type <> zcl_eui_type=>mc_ui_type-string.

      APPEND INITIAL LINE TO lt_range REFERENCE INTO ls_range.
      ls_range->tablename = ls_scr_fld-tablename.

      APPEND INITIAL LINE TO ls_range->frange_t REFERENCE INTO ls_range_sub1.
      ls_range_sub1->fieldname = ls_scr_fld-fieldname.

      lr_data = lr_map->cur_value.
      CASE lr_map->ui_type.
        WHEN zcl_eui_type=>mc_ui_type-range.
          ASSIGN lr_data->* TO <lt_val>.

          LOOP AT <lt_val> ASSIGNING <ls_val>.
            APPEND INITIAL LINE TO ls_range_sub1->selopt_t REFERENCE INTO ls_range_sub2.
            ls_range_sub2->sign   = get_text_value( is_data = <ls_val> iv_field = 'SIGN' ).
            ls_range_sub2->option = get_text_value( is_data = <ls_val> iv_field = 'OPTION' ).
            ls_range_sub2->low    = get_text_value( is_data = <ls_val> iv_field = 'LOW'    is_field_desc = lr_map->field_desc ).
            ls_range_sub2->high   = get_text_value( is_data = <ls_val> iv_field = 'HIGH'   is_field_desc = lr_map->field_desc ).
          ENDLOOP.

        WHEN zcl_eui_type=>mc_ui_type-table OR zcl_eui_type=>mc_ui_type-string.

        WHEN OTHERS. " ZCL_EUI_TYPE=>mc_ui_type-parameter
          ASSIGN lr_data->* TO <lv_val>.
          " No need if empty
          CHECK sy-subrc = 0 AND <lv_val> IS NOT INITIAL.
          APPEND INITIAL LINE TO ls_range_sub1->selopt_t REFERENCE INTO ls_range_sub2.
          ls_range_sub2->sign   = 'I'.
          ls_range_sub2->option = 'EQ'.
          ls_range_sub2->low    = get_text_value( is_data = <lv_val> is_field_desc = lr_map->field_desc ).
          " Add empty high
          CREATE DATA lr_data LIKE <lv_val>.
          ASSIGN lr_data->* TO <lv_val>.
          ls_range_sub2->high   = get_text_value( is_data = <lv_val> is_field_desc = lr_map->field_desc ).
      ENDCASE.
    ENDLOOP.

**********************************************************************
    " Free selection dialog
**********************************************************************
    DATA lt_init           TYPE rsds_texpr.
    DATA lv_title          TYPE sytitle.
    DATA lv_sel_id         TYPE rsdynsel-selid.
    DATA lt_range_ret      TYPE rsds_trange.
    DATA lv_as_popup       TYPE abap_bool.

    " Transform to internal format
    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
      EXPORTING
        field_ranges = lt_range[]
      IMPORTING
        expressions  = lt_init[].

    " initialization
    CALL FUNCTION 'FREE_SELECTIONS_INIT'
      EXPORTING
        kind                     = 'F'
        expressions              = lt_init
        restriction              = ls_restrict
      IMPORTING
        selection_id             = lv_sel_id
      TABLES
        fields_tab               = lt_scr_fld
        field_texts              = lt_scr_fld_txt
        events                   = lt_event
        event_fields             = lt_scr_fld_evt
      EXCEPTIONS
        fields_incomplete        = 1
        fields_no_join           = 2
        field_not_found          = 3
        no_tables                = 4
        table_not_found          = 5
        expression_not_supported = 6
        incorrect_expression     = 7
        illegal_kind             = 8
        area_not_found           = 9
        inconsistent_area        = 10
        kind_f_no_fields_left    = 11
        kind_f_no_fields         = 12
        too_many_fields          = 13
        dup_field                = 14
        field_no_type            = 15
        field_ill_type           = 16
        dup_event_field          = 17
        node_not_in_ldb          = 18
        area_no_field            = 19
        OTHERS                   = 20.
    IF sy-subrc <> 0.
      MESSAGE s021(zeui_message) DISPLAY LIKE 'E' WITH 'FREE_SELECTIONS_INIT' sy-subrc.
      RETURN.
    ENDIF.

*    " Hide
*    lcl_opt=>set_menu_visible( abap_false ).

    " Popup ?
    LOOP AT mt_map TRANSPORTING NO FIELDS WHERE ui_type = zcl_eui_type=>mc_ui_type-table
                                             OR ui_type = zcl_eui_type=>mc_ui_type-string.
      CLEAR mo_eui_screen->ms_popup.
      EXIT.
    ENDLOOP.

    " Cannot show as modal window
    IF mo_eui_screen->ms_popup-col_beg IS INITIAL.
      mo_eui_screen->ms_screen-dynnr = '0100'.
      lv_as_popup      = abap_false.
    ELSE.
      mo_eui_screen->ms_screen-dynnr = '1105'. "as_window = abap_true
      lv_as_popup      = abap_true.
    ENDIF.

    " call dialog
    lv_title = mo_eui_screen->ms_status-title.
    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id    = lv_sel_id
        title           = lv_title
        frame_text      = lv_title
        as_window       = lv_as_popup
        start_col       = mo_eui_screen->ms_popup-col_beg
        start_row       = mo_eui_screen->ms_popup-row_beg
        status          = 0
        just_display    = mo_eui_screen->mv_read_only
        tree_visible    = space
      IMPORTING
        field_ranges    = lt_range_ret
      TABLES
        fields_tab      = lt_scr_fld
      EXCEPTIONS
        internal_error  = 1
        no_action       = 2
        selid_not_found = 3
        illegal_status  = 4
        OTHERS          = 5.

    " Cancel
    IF sy-subrc <> 0.
      cv_close_cmd = zif_eui_manager=>mc_cmd-cancel.
    ELSE.
      " Ok
      cv_close_cmd = zif_eui_manager=>mc_cmd-ok.

      DATA lv_name          TYPE zcl_eui_screen=>ts_map-rollname.

      " Write back one by one
      LOOP AT lt_range_ret REFERENCE INTO ls_range.

        LOOP AT ls_range->frange_t REFERENCE INTO ls_range_sub1.
          CONCATENATE ls_range->tablename '-' ls_range_sub1->fieldname INTO lv_name.

          " Read by key
          READ TABLE mt_map REFERENCE INTO lr_map
           WITH KEY rollname = lv_name.
          CHECK sy-subrc = 0.

          read_free_sel_value( is_map   = lr_map
                               it_range = ls_range_sub1->selopt_t ).
        ENDLOOP.
      ENDLOOP.
    ENDIF.
********************
    " Return pressed button info
    IF mo_eui_screen->ms_status-prog <> zcl_eui_screen=>mc_eui_screen_fugr.
      cv_close_cmd = mo_eui_screen->mv_close_cmd.
    ENDIF.
  ENDMETHOD.

  METHOD get_text_value.
    DATA lv_kind           TYPE abap_typekind.
    DATA lv_len            TYPE i VALUE 0.
    DATA lv_len_n          TYPE dd01l-leng.
    DATA lv_sign           TYPE dd01l-signflag.
    DATA lv_decimals       TYPE dd01l-decimals.
    DATA lv_tabname        TYPE dd03l-tabname.
    DATA lv_fieldname      TYPE dd03l-fieldname.
    FIELD-SYMBOLS <lv_val> TYPE any.

    IF iv_field IS NOT INITIAL.
      ASSIGN COMPONENT iv_field OF STRUCTURE is_data TO <lv_val>.
    ELSE.
      ASSIGN is_data TO <lv_val>.
    ENDIF.

    DESCRIBE FIELD <lv_val> TYPE lv_kind.

    " Number types only
    CASE lv_kind.
      WHEN 'b'. " TYPEKIND_INT1
        lv_len = 3.
      WHEN 's'. " TYPEKIND_INT2
        lv_len = 5.
      WHEN 'a'. " TYPEKIND_DECFLOAT16
        lv_len = 24.
      WHEN 'e'. " TYPEKIND_DECFLOAT34
        lv_len = 46.
      WHEN '8'. " TYPEKIND_INT8
        lv_len = 20.
      WHEN 'F'. " TYPEKIND_FLOAT
        DESCRIBE FIELD <lv_val> LENGTH lv_len IN BYTE MODE.
      WHEN 'I' OR " TYPEKIND_INT
           'P'.   " TYPEKIND_PACKED
        DO 1 TIMES.
          SPLIT is_field_desc-rollname AT '-' INTO lv_tabname lv_fieldname.
          CHECK sy-subrc = 0.

          " Get tech info from domain
          SELECT SINGLE dd01l~leng dd01l~signflag dd01l~decimals INTO (lv_len_n, lv_sign, lv_decimals)
          FROM dd03l
            INNER JOIN dd01l ON dd01l~domname = dd03l~domname AND dd01l~as4local = 'A' AND dd01l~as4vers = '0000'
          WHERE tabname   = lv_tabname
            AND fieldname = lv_fieldname.
          CHECK sy-subrc = 0.
          lv_len = lv_len_n.

          " +1
          IF lv_decimals > 0.
            ADD 1 TO lv_len.
          ENDIF.

          " +1
          IF lv_sign = abap_true.
            ADD 1 TO lv_len.
          ENDIF.
        ENDDO.

        " Default with sign
        IF lv_len <= 1.
          CASE lv_kind.
            WHEN 'I'.
              lv_len = 11.
            WHEN 'P'.
              lv_len = 17.
          ENDCASE.
        ENDIF.
    ENDCASE.

    " Simple types or 'N 'zcl_eui_type=>mc_ui_type-numc
    IF lv_len = 0.
      rv_text = <lv_val>.
      RETURN.
    ENDIF.

    WRITE <lv_val> TO rv_text(lv_len) EXPONENT 0 NO-GROUPING RIGHT-JUSTIFIED.
    REPLACE FIRST OCCURRENCE OF ',' IN rv_text WITH '.'.
  ENDMETHOD.

  METHOD read_from_screen.
    DATA lv_prog               TYPE string.
    DATA lv_param              TYPE string.
    DATA lv_name               TYPE string.
    DATA lv_ok                 TYPE abap_bool.
    FIELD-SYMBOLS <lv_param>   TYPE any.
    FIELD-SYMBOLS <lv_dest>    TYPE any.

    " Get name of paramater or range
    lv_param = ir_map->par_name.

    " Always SELECT-OPTION
    lv_prog  = 'SAPLSSEL'.
    CONCATENATE lv_param `[]` INTO lv_param.

    " Name of parameter
    CONCATENATE `(` lv_prog `)` lv_param INTO lv_name.

    ASSIGN (lv_name) TO <lv_param>.
    IF sy-subrc <> 0.
      MESSAGE s014(zeui_message) WITH ir_map->par_name sy-cprog INTO sy-msgli.
      zcx_eui_exception=>raise_dump( iv_message = sy-msgli ).
    ENDIF.

    lv_ok = me->read_free_sel_value(
       is_map   = ir_map
       it_range = <lv_param> ).

    " Second attempt
    IF lv_ok <> abap_true.
      REPLACE FIRST OCCURRENCE OF '%%DYN' IN lv_name WITH '%%INT'.
      ASSIGN (lv_name) TO <lv_param>.

      me->read_free_sel_value(
        is_map   = ir_map
        it_range = <lv_param> ).
    ENDIF.
  ENDMETHOD.

  METHOD read_free_sel_value.
    FIELD-SYMBOLS <lt_val>   TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lv_val>   TYPE any.
    FIELD-SYMBOLS <ls_range> TYPE any.

    CASE is_map->ui_type.
        " Alreday set
      WHEN zcl_eui_type=>mc_ui_type-table OR zcl_eui_type=>mc_ui_type-string.


        " Write back a range
      WHEN zcl_eui_type=>mc_ui_type-range.
        ASSIGN is_map->cur_value->* TO <lt_val>.
        CHECK sy-subrc = 0.

        CLEAR <lt_val>.
        LOOP AT it_range ASSIGNING <ls_range>.
          APPEND INITIAL LINE TO <lt_val> ASSIGNING <lv_val>.
          MOVE-CORRESPONDING <ls_range> TO <lv_val>.
          rv_ok = abap_true.
        ENDLOOP.

        " Write back a parameter
      WHEN OTHERS. "ZCL_EUI_TYPE=>mc_ui_type-parameter.
        ASSIGN is_map->cur_value->* TO <lv_val>.
        CHECK sy-subrc = 0.

        " ls_range_sub1->selopt_t[] could be empty
        CLEAR <lv_val>.
        READ TABLE it_range ASSIGNING <ls_range> INDEX 1.
        CHECK sy-subrc = 0.

        ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_range> TO <ls_range>.
        CHECK sy-subrc = 0.

        <lv_val> = <ls_range>.
        rv_ok = abap_true.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_scr_auto IMPLEMENTATION.
  METHOD get_parameter_name.
    CONCATENATE `P_` iv_index INTO rv_name.
  ENDMETHOD.

  METHOD pbo.
    DATA ls_map                       TYPE REF TO zcl_eui_screen=>ts_map.
    DATA lv_name                      TYPE string.
    FIELD-SYMBOLS <ls_current_screen> TYPE sydb0_screen.
    FIELD-SYMBOLS <lv_title>          TYPE csequence.

    CHECK iv_after = abap_true.

    ASSIGN ('(RSDBRUNT)CURRENT_SCREEN') TO <ls_current_screen> CASTING.
    CHECK <ls_current_screen> IS ASSIGNED
      AND <ls_current_screen>-program = mo_eui_screen->ms_screen-prog
      AND <ls_current_screen>-dynnr   = mo_eui_screen->ms_screen-dynnr.

    " Hide action for SELECT-OPTION VALU_PUSH
    LOOP AT mt_map REFERENCE INTO ls_map
       WHERE ui_type = zcl_eui_type=>mc_ui_type-table
          OR ui_type = zcl_eui_type=>mc_ui_type-string.
      DELETE <ls_current_screen>-selopts WHERE name = ls_map->par_name.
    ENDLOOP.

    " Set title
    CONCATENATE `(` mo_eui_screen->ms_screen-prog `)S_TITLE` INTO lv_name.
    ASSIGN (lv_name) TO <lv_title>.
    IF <lv_title> IS ASSIGNED.
      <lv_title> = mo_eui_screen->ms_status-title.
    ENDIF.

    " And call parent method
    super->pbo( iv_after = iv_after ).
  ENDMETHOD.

  METHOD check_pai.
    super->check_pai(
     EXPORTING
       iv_command    = iv_command
     CHANGING
       cv_close      = cv_close
       cv_read_after = cv_read_after
       cv_map_index  = cv_map_index ).

    CHECK cv_map_index = 0.
    cv_map_index = iv_command+1(3) - 1.
  ENDMETHOD.

  METHOD show.
    CONSTANTS c_dynnr  TYPE sydynnr VALUE '9459'.

    CHECK iv_before = abap_true.

    " Text of parameters
    me->mv_pbo_set_labels = abap_true.
    " Dynnr always the same
    mo_eui_screen->ms_screen-dynnr = c_dynnr.

    " No data is passed
    IF mt_map IS INITIAL.
      zcx_eui_exception=>raise_sys_error( iv_message = 'Pass IR_CONTEXT param!' ).
    ENDIF.

    " SUBSCREEN declaration
    DATA lt_code       TYPE stringtab.
    DATA lv_code       TYPE string.
    DATA lv_table_name TYPE string.
    DATA lr_map        TYPE REF TO zcl_eui_screen=>ts_map.

    APPEND mc_auto_gen_head         TO lt_code.
    APPEND `*CAN_UPDATE=TRUE`       TO lt_code.
    APPEND `REPORT DYNAMIC_SUBSCR.` TO lt_code.

    " Add table declrations
    IF mt_unq_table IS NOT INITIAL.
      APPEND `TABLES:` TO lt_code.
      LOOP AT mt_unq_table INTO lv_table_name.
        lv_code = ``.
        IF sy-tabix <> 1.
          lv_code = `, `.
        ENDIF.

        CONCATENATE lv_code lv_table_name INTO lv_code.
        APPEND lv_code TO lt_code.
      ENDLOOP.
      " Last dot
      APPEND `.` TO lt_code.
    ENDIF.

**********************************************************************
    " Begin of screen
    APPEND `` TO lt_code.
    CONCATENATE `SELECTION-SCREEN BEGIN OF SCREEN ` c_dynnr ` AS SUBSCREEN.` INTO lv_code.
    APPEND lv_code TO lt_code.
    APPEND `SELECTION-SCREEN BEGIN OF BLOCK bl_main WITH FRAME TITLE s_title.` TO lt_code.

    LOOP AT mt_map REFERENCE INTO lr_map.
      CASE lr_map->ui_type.
        WHEN zcl_eui_type=>mc_ui_type-table OR zcl_eui_type=>mc_ui_type-string.
          CONCATENATE `SELECT-OPTIONS ` lr_map->par_name ` FOR SYST-INDEX.` INTO lv_code.

        WHEN zcl_eui_type=>mc_ui_type-range.
          CONCATENATE `SELECT-OPTIONS ` lr_map->par_name ` FOR ` lr_map->rollname  `.`  INTO lv_code.

        WHEN OTHERS.
          IF lr_map->ui_type = zcl_eui_type=>mc_ui_type-boolean.
            CONCATENATE `PARAMETERS ` lr_map->par_name ` AS CHECKBOX.` INTO lv_code.
          ELSEIF lr_map->is_list_box = abap_true.
            CONCATENATE `PARAMETERS ` lr_map->par_name ` TYPE ` lr_map->rollname ` AS LISTBOX VISIBLE LENGTH 50.` INTO lv_code.
          ELSE.
            CONCATENATE `PARAMETERS ` lr_map->par_name ` TYPE ` lr_map->rollname  `.`   INTO lv_code.
          ENDIF.
      ENDCASE.

      APPEND lv_code TO lt_code.
    ENDLOOP.

**********************************************************************
    " END ofd screen
    APPEND `` TO lt_code.
    APPEND `SELECTION-SCREEN END OF BLOCK bl_main.`           TO lt_code.
    CONCATENATE `SELECTION-SCREEN END OF SCREEN ` c_dynnr `.` INTO lv_code.
    APPEND lv_code                                            TO lt_code.

    " For EVENT call FM ZFM_EUI_PBO_SUB_SCREEN!
    APPEND ``  TO lt_code.
    APPEND `AT SELECTION-SCREEN OUTPUT.`              TO lt_code.
    " PBO
    CONCATENATE `PERFORM AUTO_SCREEN_PBO IN PROGRAM ` zcl_eui_screen=>mc_eui_screen_fugr `.` INTO lv_code.
    APPEND lv_code TO lt_code.

    DATA lv_can_update TYPE abap_bool.
    lv_can_update = check_can_update( ).
    CASE lv_can_update.
      WHEN abap_undefined.

      WHEN abap_true.

        " Created but now is in permanent package
      WHEN abap_false.
        RETURN.
    ENDCASE.

    " Create program
    " Sorry but usally is strictly prohibited. Please create { mo_eui_screen->ms_screen-prog } by hand based on { lt_code }.
    zcx_eui_exception=>raise_dump( iv_message = 'Cannot call INSERT REPORT' ).
    " And then change second row to *CAN_UPDATE=FALSE

*    INSERT REPORT mo_eui_screen->ms_screen-prog FROM lt_code.
*    " wrong syntax in { lt_code }
*    IF sy-subrc <> 0.
*      zcx_eui_exception=>raise_sys_error( iv_message = 'Cannot generate report' ).
*    ENDIF.

    " SubRoutine pool cannot contain screens ???
    " GENERATE SUBROUTINE POOL lt_code NAME ls_screen->prog  MESSAGE lv_message LINE lv_pos.  "#EC CI_GENERATE. <--- in lt_code[]

    " Ooops!
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD check_can_update.
    DATA lt_code       TYPE stringtab.
    DATA lv_code       TYPE string.

    " No include ok
    rv_ok = abap_undefined.
    READ REPORT mo_eui_screen->ms_screen-prog INTO lt_code.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " have prefix ok
    LOOP AT lt_code INTO lv_code TO 2.
      CASE sy-tabix.
        WHEN 1.
          IF lv_code <> mc_auto_gen_head.
            rv_ok = abap_false.
            RETURN.
          ENDIF.

        WHEN 2.
          IF lv_code = `*CAN_UPDATE=TRUE`.
            rv_ok = abap_true.
            RETURN.
          ENDIF.

      ENDCASE.
    ENDLOOP.

    rv_ok = abap_false.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_scr_dpop IMPLEMENTATION.
  METHOD show.
    DATA lv_prog      TYPE char30.
    DATA lv_title     TYPE text255.
    DATA lv_cancelled TYPE abap_bool.
    DATA lt_attr      TYPE sci_atttab.

    CHECK iv_before = abap_true.

    lv_prog  = mo_eui_screen->ms_screen-prog.
    lv_title = mo_eui_screen->ms_status-title.

    " Create screen
    lt_attr = me->create_dyn_popup(  ).

    lv_cancelled = cl_ci_query_attributes=>generic(
      p_name       = lv_prog
      p_title      = lv_title
      p_display    = mo_eui_screen->mv_read_only
      p_attributes = lt_attr ).

    IF lv_cancelled = abap_true.
      cv_close_cmd = zif_eui_manager=>mc_cmd-cancel.
    ELSE.
      cv_close_cmd = zif_eui_manager=>mc_cmd-ok.
    ENDIF.
  ENDMETHOD.

  METHOD create_dyn_popup.
    " No data is passed
    IF mt_map IS INITIAL.
      zcx_eui_exception=>raise_sys_error( iv_message = 'Pass IR_CONTEXT param!' ).
    ENDIF.

    " POPUP declaration
    DATA lr_map        TYPE REF TO zcl_eui_screen=>ts_map.
    DATA ls_attr       TYPE sci_attent.

    " Title group
    ls_attr-kind = 'G'.
    ls_attr-text = mo_eui_screen->ms_status-title.
    GET REFERENCE OF sy-index INTO ls_attr-ref.
    APPEND ls_attr TO rt_attr.

    LOOP AT mt_map REFERENCE INTO lr_map.
      CLEAR ls_attr.
      ls_attr-text = lr_map->label.
      ls_attr-ref  = lr_map->cur_value.
      IF lr_map->required = '1'.
        ls_attr-obligatory = abap_true.
      ENDIF.

      CASE lr_map->ui_type.
          " Not supported
        WHEN zcl_eui_type=>mc_ui_type-table OR zcl_eui_type=>mc_ui_type-string.
          CONTINUE.

        WHEN zcl_eui_type=>mc_ui_type-range.
          ls_attr-kind = 'S'. " Select-option

        WHEN OTHERS.
          IF lr_map->ui_type = zcl_eui_type=>mc_ui_type-boolean.
            ls_attr-kind = 'С'. " Checkbox
          ELSEIF lr_map->is_list_box = abap_true.
            ls_attr-kind = 'L'. " Listbox
          ELSE.
            ls_attr-kind = 'T'. " Parameter
          ENDIF.
      ENDCASE.

      APPEND ls_attr TO rt_attr.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
