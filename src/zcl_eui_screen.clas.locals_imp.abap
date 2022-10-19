*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_screen IMPLEMENTATION.
  METHOD constructor.
    mo_eui_screen   = io_eui_screen.
    mr_context      = ir_context.
    mv_unq_rollname = iv_unq_rollname.
  ENDMETHOD.

  " Fill mapping
  METHOD fill_from_context.
    DATA ls_map                TYPE zcl_eui_screen=>ts_map.
    DATA lt_unq_type           TYPE REF TO zcl_eui_type=>tt_unique_type.
    DATA lo_struc              TYPE REF TO cl_abap_structdescr.
    DATA ls_comp               TYPE REF TO abap_compdescr.
    DATA lv_index              TYPE num2.
    DATA lt_unq_rollname       TYPE SORTED TABLE OF zcl_eui_type=>ts_field_desc-rollname WITH UNIQUE KEY table_line.
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
          _check_is_list_box( CHANGING cs_map = ls_map ).
      ENDCASE.

      DO 1 TIMES.
        CHECK mv_unq_rollname = abap_true.
        INSERT ls_map-rollname INTO TABLE lt_unq_rollname.

        CHECK  sy-subrc <> 0 OR ls_map-rollname NP '*-*'.
        zcx_eui_exception=>raise_sys_error( iv_message = 'Cannot create a unique pair TABLE-FIELD_NAME' ).
      ENDDO.

      APPEND ls_map TO me->mt_map.
    ENDLOOP.
  ENDMETHOD.

  METHOD _check_is_list_box.
    DATA ls_fieldcat           TYPE lvc_s_fcat.
    zcl_eui_type=>split_type(
     EXPORTING iv_datatype = cs_map-rollname
     IMPORTING ev_table    = ls_fieldcat-ref_table
               ev_field    = ls_fieldcat-ref_field ).

    zcl_eui_type=>is_list_box(
     EXPORTING iv_tabname   = ls_fieldcat-ref_table
               iv_fieldname = ls_fieldcat-ref_field
     IMPORTING ev_list_box  = cs_map-is_list_box ).
  ENDMETHOD.

  METHOD get_parameter_name.
    rv_name = is_comp->name.
  ENDMETHOD.

  METHOD customize.
    DATA lr_customize TYPE REF TO zcl_eui_screen=>ts_customize.
    DATA ls_screen    TYPE zcl_eui_screen=>ts_screen.
    DATA ls_map       TYPE zcl_eui_screen=>ts_map.
    DATA lr_map       TYPE REF TO zcl_eui_screen=>ts_map.
    DATA lv_message   TYPE string.
    DATA lv_skip      TYPE string.

    LOOP AT it_customize REFERENCE INTO lr_customize.
      " All by name
      MOVE-CORRESPONDING lr_customize->* TO ls_screen.

      " For map
      CLEAR ls_map.
      ls_map-label      = lr_customize->label.
      ls_map-sub_fdesc  = lr_customize->sub_fdesc.
      ls_map-rollname   = lr_customize->rollname.
      ls_map-command    = lr_customize->command.

      "№ 1
      IF   ls_screen-name   IS NOT INITIAL
        OR ls_screen-group1 IS NOT INITIAL
        OR ls_screen-group2 IS NOT INITIAL.
        " Previous SCREEN option
        DELETE mt_screen WHERE name   = ls_screen-name
                           AND group1 = ls_screen-group1
                           AND group2 = ls_screen-group2.
        APPEND ls_screen TO mt_screen.
      ENDIF.

      "№ 2 IF have something
      CHECK ls_map IS NOT INITIAL
        AND ls_screen-name IS NOT INITIAL.

      LOOP AT mt_map REFERENCE INTO lr_map WHERE name CP ls_screen-name. " was 'EQ'
        lv_skip = 'ROLLNAME'.
        IF ls_map-rollname IS NOT INITIAL AND lr_map->ui_type <> zcl_eui_type=>mc_ui_type-table AND lr_map->ui_type <> zcl_eui_type=>mc_ui_type-string.
          _check_is_list_box( CHANGING cs_map = ls_map ).
          CLEAR lv_skip.
        ENDIF.

        " Label or sub_fdesc
        zcl_eui_conv=>move_corresponding(
         EXPORTING
           is_source         = ls_map
           iv_except_initial = abap_true
           iv_except         = lv_skip
         CHANGING
           cs_destination    = lr_map->* ).
      ENDLOOP.
      IF sy-subrc <> 0.
        CONCATENATE `Unknown field name ` ls_screen-name INTO lv_message.
        zcx_eui_exception=>raise_dump( iv_message = lv_message ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD show.
  ENDMETHOD.

  METHOD get_screen_by_map.
    " Just return INPUT & REQUIRED
    READ TABLE mt_screen INTO rs_screen
     WITH KEY name = iv_name.
  ENDMETHOD.

  METHOD pbo.
    CHECK iv_after = abap_true.

    " Cannot call LOOP AT ACREEN & set SCREEN data
    IF  sy-dynnr <> mo_eui_screen->ms_screen-dynnr.
      " TODO OR sy-cprog <> ms_screen->prog.
      RETURN.
    ENDIF.

    set_initial_values( ).

**********************************************************************
    " Set parameters labels ( 1 time only )
    IF mv_pbo_set_labels = abap_true.
      mv_pbo_set_labels = abap_false.

      DATA ls_map  TYPE REF TO zcl_eui_screen=>ts_map.
      DATA lv_name TYPE string.
      FIELD-SYMBOLS <lv_param> TYPE any.
      LOOP AT mt_map REFERENCE INTO ls_map.
        CONCATENATE `(` mo_eui_screen->ms_screen-prog `)%_` ls_map->par_name `_%_APP_%-TEXT` INTO lv_name.
        ASSIGN (lv_name) TO <lv_param>.
        CHECK sy-subrc = 0.

        <lv_param> = ls_map->label.
      ENDLOOP.
    ENDIF.

**********************************************************************
    " LOOP AT SCREEN

    DATA lv_rem         TYPE string.
    DATA ls_screen_dst  TYPE screen.
    DATA ls_screen_src  LIKE LINE OF mt_screen.
    DATA lt_scr_fields  TYPE abap_compdescr_tab. " Filled 1 time with all ls_screen fields
    DATA lv_input       TYPE screen-input.
    DATA ls_screen      TYPE zcl_eui_screen=>ts_screen.

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
      DO 1 TIMES.
        READ TABLE mt_map REFERENCE INTO ls_map
         WITH KEY par_name = lv_name.
        CHECK sy-subrc = 0.

        IF mo_eui_screen->mv_read_only = 'X'.
          ls_screen_dst-input = '0'.
        ENDIF.

        " Get by screen option
        LOOP AT mt_screen INTO ls_screen.       " ls_screen = get_screen_by_map( ls_map->name ).
          CHECK ls_map->name CP ls_screen-name. " was 'EQ'

          " Set required for parameter or select-option
          IF ls_screen-required IS NOT INITIAL AND ( ls_screen_dst-group3 = 'PAR' OR ls_screen_dst-group3 = 'LOW' ).
            ls_screen_dst-required = ls_screen-required.
          ENDIF.

          lv_input = ''.
          IF ls_screen-input = '0'. " AND ls_screen_dst-group3 <> 'VPU' 'PBU'. " But not for tables
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
            ls_screen_dst-input = '0'.
          ENDIF.

          " TODO change SCREEN-NAME instead ?
          CHECK ls_screen_dst-name = ls_map->par_name
            AND ls_map->ui_type <> zcl_eui_type=>mc_ui_type-table
            AND ls_map->ui_type <> zcl_eui_type=>mc_ui_type-string.
          CLEAR: ls_screen-name,
                 ls_screen-input,
                 ls_screen-required.
          zcl_eui_conv=>move_corresponding( EXPORTING is_source         = ls_screen
                                                      iv_except_initial = abap_true    " <--- Move-corresponding except initial
                                            CHANGING  cs_destination    = ls_screen_dst
                                                      ct_component      = lt_scr_fields ).
        ENDLOOP.
      ENDDO.

      FIELD-SYMBOLS <ls_screen_src> LIKE LINE OF mt_screen.
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
        ELSEIF ls_screen_src-group2 IS NOT INITIAL.  " By group 02
          CHECK ls_screen_dst-group2 = ls_screen_src-group2.
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
      IF ( ls_screen_dst-group3 = 'VPU' OR ls_screen_dst-group3 = 'PBU' ) AND ls_screen_dst-active = '1'.
        ls_screen_dst-input = '1'.
      ENDIF.

      " And change
      CHECK ls_screen_dst <> screen.
      screen = ls_screen_dst.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_initial_values.
    " Set parameters from context ( 1 time only )
    CHECK mv_pbo_init_params = abap_true OR iv_force = abap_true.
    mv_pbo_init_params = abap_false.

    DATA ls_map TYPE REF TO zcl_eui_screen=>ts_map.
    LOOP AT mt_map REFERENCE INTO ls_map WHERE ui_type <> zcl_eui_type=>mc_ui_type-table
                                           AND ui_type <> zcl_eui_type=>mc_ui_type-string.
      " Name of parameter
      DATA lv_name TYPE string.
      CONCATENATE `(` mo_eui_screen->ms_screen-prog `)` ls_map->par_name INTO lv_name.

      " Name of SELECT-OPTION
      IF ls_map->ui_type = zcl_eui_type=>mc_ui_type-range.
        CONCATENATE lv_name `[]` INTO lv_name.
      ENDIF.

      FIELD-SYMBOLS <lv_param> TYPE any.
      ASSIGN (lv_name) TO <lv_param>.
      IF sy-subrc <> 0.
        MESSAGE s014(zeui_message) WITH ls_map->par_name sy-cprog INTO sy-msgli.
        zcx_eui_exception=>raise_dump( iv_message = sy-msgli ).
      ENDIF.

      FIELD-SYMBOLS <lv_src> TYPE any.
      ASSIGN ls_map->cur_value->* TO <lv_src>.
      <lv_param> = <lv_src>.
    ENDLOOP.

    " Set listbox search helps
    FIELD-SYMBOLS <ls_screen_src> LIKE LINE OF mt_screen.
    LOOP AT mt_screen ASSIGNING <ls_screen_src> WHERE
          name IS NOT INITIAL
      AND t_listbox IS NOT INITIAL.

      " Get name
      DATA lv_id TYPE char80.
      READ TABLE mt_map REFERENCE INTO ls_map
       WITH KEY name = <ls_screen_src>-name.
      IF sy-subrc = 0.
        lv_id = ls_map->par_name.
      ELSE.
        lv_id = <ls_screen_src>-name.
      ENDIF.

      " Initialize 1 time only
      DATA: lv_set_vrm TYPE abap_bool,
            lt_listbox TYPE vrm_values.
      CLEAR: lv_set_vrm,
             lt_listbox.
      CALL FUNCTION 'VRM_GET_VALUES'
        EXPORTING
          id     = lv_id
        IMPORTING
          values = lt_listbox
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0 OR lt_listbox IS INITIAL.
        lv_set_vrm = abap_true.
      ELSE.
        " Are values changed?
        DATA: lv_hash1 TYPE char16,
              lv_hash2 TYPE char16.
        lv_hash1 = _get_hash( lt_listbox ).
        lv_hash2 = _get_hash( <ls_screen_src>-t_listbox[] ).
        IF lv_hash1 <> lv_hash2.
          lv_set_vrm = abap_true.
        ENDIF.
      ENDIF.

      " And set
      CHECK lv_set_vrm = abap_true.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = lv_id
          values = <ls_screen_src>-t_listbox[]
        EXCEPTIONS
          OTHERS = 0.
    ENDLOOP.
  ENDMETHOD.

  METHOD _get_hash.
    DATA lo_crc64 TYPE REF TO zcl_eui_crc64.
    CREATE OBJECT lo_crc64.
    lo_crc64->add_to_hash( iv_value ).
    rv_hash = lo_crc64->get_hash( ).
  ENDMETHOD.

  METHOD read_from_screen.
    DATA lv_prog               TYPE string.
    DATA lv_param              TYPE string.
    DATA lv_name               TYPE string.
    FIELD-SYMBOLS <lv_param>   TYPE any.
    FIELD-SYMBOLS <lv_dest>    TYPE any.

    " Not initialized yet
    CHECK mv_pbo_init_params <> abap_true.

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
    DATA ls_layout     TYPE lvc_s_layo.
    DATA ls_screen     TYPE zcl_eui_screen=>ts_screen.

    " Get current field
    READ TABLE mt_map REFERENCE INTO ls_map INDEX iv_map_index.
    CHECK sy-subrc = 0.

    " Get by screen option
    ls_screen = get_screen_by_map( ls_map->name ).

    " Edit or not
    IF mo_eui_screen->mv_read_only <> abap_true AND ls_screen-input <> '0'.
      ls_layout-edit = abap_true.
    ENDIF.

    CASE ls_map->ui_type.
      WHEN zcl_eui_type=>mc_ui_type-table.
        DATA lo_alv TYPE REF TO zcl_eui_alv.
        CREATE OBJECT lo_alv
          EXPORTING
            ir_table  = ls_map->cur_value
            is_layout = ls_layout.
        lo_manager = lo_alv.

        " delegate to alv
        FIELD-SYMBOLS <ls_item> LIKE LINE OF mo_eui_screen->mt_skip_msg[].
        LOOP AT mo_eui_screen->mt_skip_msg[] ASSIGNING <ls_item>.
          lo_alv->skip_message( iv_msgid = <ls_item>-msgid
                                iv_msgno = <ls_item>-msgno
                                iv_msgty = <ls_item>-msgty ).
        ENDLOOP.

        " As refernce
        GET REFERENCE OF ls_map->field_desc INTO lr_field_desc.
        lo_alv->set_field_desc( lr_field_desc ).

        _find_f4_tables( is_field_desc = lr_field_desc->*
                         io_alv        = lo_alv ).

      WHEN zcl_eui_type=>mc_ui_type-string.
        lr_text ?= ls_map->cur_value.
        CREATE OBJECT lo_manager TYPE zcl_eui_memo
          EXPORTING
            ir_text     = lr_text
            iv_editable = ls_layout-edit.

      WHEN OTHERS.
        RETURN.
    ENDCASE.

    " Yes is editor
    CHECK lo_manager IS NOT INITIAL.

    lo_manager->popup( iv_row_end = 28 ).                "#EC NUMBER_OK

    lo_manager->show( ).
  ENDMETHOD.

  METHOD _find_f4_tables.
    DATA lt_sub_fld  TYPE zcl_eui_type=>tt_field_desc.
    DATA lr_sub_fld  TYPE REF TO zcl_eui_type=>ts_field_desc.
    DATA lt_f4_table TYPE zcl_eui_alv=>tt_f4_table.
    DATA ls_f4_table TYPE zcl_eui_alv=>ts_f4_table.

    " Find all F4 tables
    lt_sub_fld = zcl_eui_type=>get_sub_field_desc( is_field_desc ).
    LOOP AT lt_sub_fld REFERENCE INTO lr_sub_fld WHERE f4_table IS NOT INITIAL. "#EC CI_HASHSEQ
      DATA lr_f4_map TYPE REF TO zcl_eui_screen=>ts_map.
      READ TABLE mt_map REFERENCE INTO lr_f4_map
       WITH KEY name = lr_sub_fld->f4_table.
      CHECK sy-subrc = 0.

      CLEAR ls_f4_table.
      ls_f4_table-field = lr_sub_fld->name.
      ls_f4_table-tab   = lr_f4_map->cur_value.
      INSERT ls_f4_table INTO TABLE lt_f4_table.
    ENDLOOP.

    " And set
    CHECK lt_f4_table IS NOT INITIAL.
    io_alv->set_f4_table( lt_f4_table ).
  ENDMETHOD.

  METHOD is_fixed_values_list.
    CLEAR: ev_is_fixed,
           ev_update.

    DATA lv_low_only TYPE abap_bool VALUE abap_true.

    FIELD-SYMBOLS <ls_range> TYPE any.
    LOOP AT ct_range ASSIGNING <ls_range>.
      FIELD-SYMBOLS: <lv_sign>   TYPE tvarv_sign,
                     <lv_option> TYPE tvarv_opti.
      ASSIGN COMPONENT: 'SIGN'   OF STRUCTURE <ls_range> TO <lv_sign>,
                        'OPTION' OF STRUCTURE <ls_range> TO <lv_option>.
      CHECK <lv_sign> <> 'I' OR <lv_option> <> 'EQ'.
      lv_low_only = abap_false.
      EXIT.
    ENDLOOP.

    CHECK lv_low_only = abap_true.

    DATA lt_dropdown TYPE lvc_t_dral.
    lt_dropdown = zcl_eui_type=>find_dropdown( is_fieldcat = is_catalog
                                               iv_show_key = abap_false ).
    CHECK lines( lt_dropdown[] ) >= 2.
    ev_is_fixed = abap_true.

**********************************************************************
    TYPES: BEGIN OF ts_f4,
             mark TYPE abap_bool,
             key  TYPE char30,
             text TYPE string,
           END OF ts_f4.
    DATA lt_f4   TYPE STANDARD TABLE OF ts_f4.
    DATA lr_f4   TYPE REF TO ts_f4.

    SORT lt_dropdown BY int_value.
    LOOP AT ct_range ASSIGNING <ls_range>.
      FIELD-SYMBOLS <lv_val> TYPE any.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_range> TO <lv_val>.

      DATA ls_dropdown LIKE LINE OF lt_dropdown.
      READ TABLE lt_dropdown INTO ls_dropdown BINARY SEARCH
       WITH KEY int_value = <lv_val>.
      CHECK sy-subrc  = 0.
      DELETE lt_dropdown INDEX sy-tabix.

      APPEND INITIAL LINE TO lt_f4 REFERENCE INTO lr_f4.
      lr_f4->key  = ls_dropdown-int_value.
      lr_f4->text = ls_dropdown-value.
      lr_f4->mark = abap_true.
    ENDLOOP.

    LOOP AT lt_dropdown INTO ls_dropdown.
      APPEND INITIAL LINE TO lt_f4 REFERENCE INTO lr_f4.
      lr_f4->key  = ls_dropdown-int_value.
      lr_f4->text = ls_dropdown-value.
    ENDLOOP.

    DATA lr_table TYPE REF TO data.
    GET REFERENCE OF lt_f4 INTO lr_table.

    DATA lt_catalog TYPE lvc_t_fcat.
    DATA lr_catalog TYPE REF TO lvc_s_fcat.

    APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
    lr_catalog->fieldname = 'MARK'.
    lr_catalog->checkbox  = abap_true.
    lr_catalog->coltext   = '-'.
    IF iv_read_only <> abap_true.
      lr_catalog->edit = abap_true.
    ENDIF.

    APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
    lr_catalog->fieldname = 'KEY'.
    lr_catalog->coltext   = 'Key'(key).

    APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
    lr_catalog->fieldname = 'TEXT'.
    lr_catalog->coltext   = 'Text'(txt).

    DATA ls_layout TYPE lvc_s_layo.
    ls_layout-smalltitle = ls_layout-no_rowmark = ls_layout-no_toolbar = abap_true.
    ls_layout-grid_title = is_catalog-coltext.

    DATA lo_alv TYPE REF TO zcl_eui_alv.
    CREATE OBJECT lo_alv
      EXPORTING
        ir_table       = lr_table
        it_mod_catalog = lt_catalog
        is_layout      = ls_layout.
    lo_alv->popup( iv_col_beg = 25
                   iv_col_end = 97
                   iv_row_beg = 3
                   iv_row_end = 13 ).
    CHECK lo_alv->show( ) = 'OK'.
    ev_update = abap_true.

    CLEAR ct_range.
    LOOP AT lt_f4 REFERENCE INTO lr_f4 WHERE mark = abap_true.
      APPEND INITIAL LINE TO ct_range ASSIGNING <ls_range>.
      ASSIGN COMPONENT: 'SIGN'   OF STRUCTURE <ls_range> TO <lv_sign>,
                        'OPTION' OF STRUCTURE <ls_range> TO <lv_option>,
                        'LOW'    OF STRUCTURE <ls_range> TO <lv_val>.
      <lv_sign>   = 'I'.
      <lv_option> = 'EQ'.
      <lv_val>    = lr_f4->key.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_scr_free IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
     io_eui_screen   = io_eui_screen
     ir_context      = ir_context
     iv_unq_rollname = abap_true ).
  ENDMETHOD.

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
          sy-tabix = 1105 * 1000 + sy-tabix * 2 + 2.     "#EC NUMBER_OK
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
        fields_incomplete        = 1   "#EC NUMBER_OK
        fields_no_join           = 2   "#EC NUMBER_OK
        field_not_found          = 3   "#EC NUMBER_OK
        no_tables                = 4   "#EC NUMBER_OK
        table_not_found          = 5   "#EC NUMBER_OK
        expression_not_supported = 6   "#EC NUMBER_OK
        incorrect_expression     = 7   "#EC NUMBER_OK
        illegal_kind             = 8   "#EC NUMBER_OK
        area_not_found           = 9   "#EC NUMBER_OK
        inconsistent_area        = 10  "#EC NUMBER_OK
        kind_f_no_fields_left    = 11  "#EC NUMBER_OK
        kind_f_no_fields         = 12  "#EC NUMBER_OK
        too_many_fields          = 13  "#EC NUMBER_OK
        dup_field                = 14  "#EC NUMBER_OK
        field_no_type            = 15  "#EC NUMBER_OK
        field_ill_type           = 16  "#EC NUMBER_OK
        dup_event_field          = 17  "#EC NUMBER_OK
        node_not_in_ldb          = 18  "#EC NUMBER_OK
        area_no_field            = 19  "#EC NUMBER_OK
        OTHERS                   = 20. "#EC NUMBER_OK
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
        lv_len = 3.                                      "#EC NUMBER_OK
      WHEN 's'. " TYPEKIND_INT2
        lv_len = 5.                                      "#EC NUMBER_OK
      WHEN 'a'. " TYPEKIND_DECFLOAT16
        lv_len = 24.                                     "#EC NUMBER_OK
      WHEN 'e'. " TYPEKIND_DECFLOAT34
        lv_len = 46.                                     "#EC NUMBER_OK
      WHEN '8'. " TYPEKIND_INT8
        lv_len = 20.                                     "#EC NUMBER_OK
      WHEN 'F'. " TYPEKIND_FLOAT
        DESCRIBE FIELD <lv_val> LENGTH lv_len IN BYTE MODE.
      WHEN 'I' OR " TYPEKIND_INT
           'P'.   " TYPEKIND_PACKED
        DO 1 TIMES.
          SPLIT is_field_desc-rollname AT '-' INTO lv_tabname lv_fieldname.
          CHECK sy-subrc = 0.

          " Get tech info from domain
          SELECT SINGLE dd01l~leng dd01l~signflag dd01l~decimals INTO (lv_len_n, lv_sign, lv_decimals) "#EC CI_NOORDER
          FROM dd03l
            INNER JOIN dd01l ON dd01l~domname = dd03l~domname AND dd01l~as4local = 'A' AND dd01l~as4vers = '0000'
          WHERE dd03l~tabname   = lv_tabname
            AND dd03l~fieldname = lv_fieldname
            AND dd03l~as4local  = 'A'
            AND dd03l~as4vers   = '0000'.
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
              lv_len = 11.                               "#EC NUMBER_OK
            WHEN 'P'.
              lv_len = 17.                               "#EC NUMBER_OK
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
CLASS lcl_scr_dync IMPLEMENTATION.
  METHOD show.
    CHECK iv_before = abap_true.
    _create_program( ).
  ENDMETHOD.

  METHOD _create_program.
    " Already created?
    CHECK mo_eui_screen->ms_screen-dynnr = zcl_eui_screen=>mc_dynnr-dynamic.
    mo_eui_screen->ms_screen-dynnr = '9999'.

    DATA lo_prog TYPE REF TO zcl_eui_prog.
    CREATE OBJECT lo_prog EXPORTING iv_cprog = mo_eui_screen->ms_screen-prog.

    DATA: lo_crc64 TYPE REF TO zcl_eui_crc64, lv_hash TYPE string.
    CREATE OBJECT lo_crc64 EXPORTING iv_dref = zcl_eui_crc64=>mc_dref-no_info.
    " Version of screen code
    lo_crc64->add_to_hash( '005' ).
    lo_crc64->add_to_hash( mt_map ).
    lo_crc64->add_to_hash( mo_eui_screen->ms_status ).
    lo_crc64->add_to_hash( mo_eui_screen->ms_popup ).
    lv_hash = lo_crc64->get_hash( ).

    IF lv_hash = lo_prog->get_attribute( 'HASH').
      mo_eui_screen->ms_screen-prog = lo_prog->mv_cprog.
      RETURN.
    ENDIF.

    DATA: lt_code      TYPE stringtab, lv_tech_info TYPE string.
    lt_code = _make_screen_code( ).
    CONCATENATE '"EUI=X,GENERATE-DATE=' sy-datum ',HASH=' lv_hash INTO lv_tech_info.
    INSERT lv_tech_info INTO lt_code INDEX 1.

    mo_eui_screen->ms_screen-prog  = lo_prog->generate( it_code  = lt_code
                                                        iv_cprog = lo_prog->mv_cprog ).
  ENDMETHOD.

  METHOD _make_screen_code.
    " No data is passed
    IF mt_map IS INITIAL.
      zcx_eui_no_check=>raise_sys_error( iv_message = 'Pass IR_CONTEXT param!' ).
    ENDIF.

    APPEND `REPORT DYNAMIC_SUBSCR.`                                            TO rt_code.
    APPEND ``                                                                  TO rt_code.
    APPEND `SELECTION-SCREEN BEGIN OF SCREEN 9999 AS SUBSCREEN.`               TO rt_code.

    DATA lv_line TYPE string VALUE `SELECTION-SCREEN BEGIN OF BLOCK bl_main.`.
    DATA lv_button_offset TYPE num2 VALUE 35.
    IF mo_eui_screen->ms_status-title IS NOT INITIAL AND ( mo_eui_screen->ms_popup IS INITIAL OR mo_eui_screen->ms_popup-col_end >= 100 ).
      REPLACE FIRST OCCURRENCE OF `.` IN lv_line WITH ` WITH FRAME TITLE s_title.`.
      lv_button_offset = 33.                             "#EC NUMBER_OK
    ENDIF.
    APPEND lv_line TO rt_code.

    DATA lr_map        TYPE REF TO zcl_eui_screen=>ts_map.
    DATA: l_par_name TYPE sychar08 VALUE 'X', l_for_name TYPE sychar08 VALUE 'F', l_cmt_name TYPE sychar08 VALUE 'T'.
    DATA l_line        TYPE string.
    DATA lv_command    TYPE string.

    LOOP AT mt_map REFERENCE INTO lr_map.
      UNPACK sy-tabix TO l_par_name+1(*).

      CASE lr_map->ui_type.
        WHEN zcl_eui_type=>mc_ui_type-table OR zcl_eui_type=>mc_ui_type-string.
          APPEND `SELECTION-SCREEN BEGIN OF LINE.` TO rt_code.

          l_cmt_name+1 = l_par_name+1.
          CONCATENATE `SELECTION-SCREEN COMMENT 1(31) `  l_cmt_name `.` INTO l_line.
          APPEND l_line  TO rt_code.

          CONCATENATE `SELECTION-SCREEN  PUSHBUTTON ` lv_button_offset `(19) ` l_par_name ` USER-COMMAND ` l_par_name `.` INTO l_line.
          APPEND l_line TO rt_code.
          APPEND `SELECTION-SCREEN END OF LINE.` TO rt_code.

        WHEN zcl_eui_type=>mc_ui_type-range.
          l_for_name+1 = l_par_name+1.
          CONCATENATE `DATA ` l_for_name ` TYPE ` lr_map->rollname `.` INTO l_line.
          APPEND l_line TO rt_code.
          CONCATENATE `SELECT-OPTIONS ` l_par_name ` FOR ` l_for_name `.` INTO l_line.
          APPEND l_line TO rt_code.

        WHEN OTHERS. " Parameter
          CONCATENATE `PARAMETERS ` l_par_name ` TYPE ` lr_map->rollname INTO l_line.

          CLEAR lv_command.
          IF lr_map->command IS NOT INITIAL.
            CONCATENATE ` USER-COMMAND ` lr_map->command INTO lv_command.
          ENDIF.

          " Get by screen option
          DATA ls_screen TYPE zcl_eui_screen=>ts_screen.
          ls_screen = get_screen_by_map( lr_map->name ).

          IF lr_map->ui_type = zcl_eui_type=>mc_ui_type-boolean.
            CONCATENATE l_line ` AS CHECKBOX` lv_command INTO l_line.
          ELSEIF lr_map->is_list_box = abap_true OR ls_screen-t_listbox[] IS NOT INITIAL.
            CONCATENATE l_line ` AS LISTBOX VISIBLE LENGTH 50` lv_command INTO l_line.
          ENDIF.
          CONCATENATE l_line `.` INTO l_line.

          APPEND l_line TO rt_code.
      ENDCASE.
    ENDLOOP.

    APPEND `SELECTION-SCREEN END OF BLOCK bl_main.`  TO rt_code.
    APPEND `SELECTION-SCREEN END OF SCREEN 9999.`    TO rt_code.
    APPEND ``                                        TO rt_code.
    APPEND `INCLUDE zeui_dynamic_screen_events.`     TO rt_code.
  ENDMETHOD.

  METHOD get_parameter_name.
    DATA lv_index TYPE n LENGTH 7.
    lv_index = iv_index.
    CONCATENATE `X` lv_index INTO rv_name.
  ENDMETHOD.

  METHOD check_pai.
    super->check_pai( EXPORTING iv_command    = iv_command
                      CHANGING  cv_close      = cv_close
                                cv_read_after = cv_read_after
                                cv_map_index  = cv_map_index ).
    CHECK iv_command CP 'X*'.
    cv_map_index = iv_command+1.
  ENDMETHOD.

  METHOD pbo.
    DATA ls_map                       TYPE REF TO zcl_eui_screen=>ts_map.
    DATA lv_name                      TYPE string.
    FIELD-SYMBOLS <ls_current_screen> TYPE sydb0_screen.
    FIELD-SYMBOLS <lv_title>          TYPE csequence.

    IF iv_before = abap_true.
      _create_program( ).
      RETURN.
    ENDIF.
    CHECK iv_after = abap_true.

    ASSIGN ('(RSDBRUNT)CURRENT_SCREEN') TO <ls_current_screen> CASTING.
    CHECK <ls_current_screen> IS ASSIGNED
      AND <ls_current_screen>-program = mo_eui_screen->ms_screen-prog
      AND <ls_current_screen>-dynnr   = mo_eui_screen->ms_screen-dynnr.

    " Hide action for SELECT-OPTION VALU_PUSH
    LOOP AT mt_map REFERENCE INTO ls_map
       WHERE ui_type = zcl_eui_type=>mc_ui_type-table
          OR ui_type = zcl_eui_type=>mc_ui_type-string.
      DATA l_name TYPE string.
      FIELD-SYMBOLS <l_scr_field> TYPE csequence.

      " Comment
      CONCATENATE `(` mo_eui_screen->ms_screen-prog `)T` ls_map->par_name+1 INTO l_name.
      ASSIGN (l_name) TO <l_scr_field>.
      <l_scr_field> = ls_map->label.

      " Button icon
      CONCATENATE `(` mo_eui_screen->ms_screen-prog `)` ls_map->par_name INTO l_name.
      ASSIGN (l_name) TO <l_scr_field>.

      CASE ls_map->ui_type.
        WHEN zcl_eui_type=>mc_ui_type-table.
          FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
          ASSIGN ls_map->cur_value->* TO <lt_table>.
          <l_scr_field> = lines( <lt_table> ).
          CONDENSE <l_scr_field>.
          CONCATENATE icon_wd_table 'Rows'(row) ` ` <l_scr_field> INTO <l_scr_field>.

        WHEN zcl_eui_type=>mc_ui_type-string.
          FIELD-SYMBOLS <lv_string> TYPE string.
          ASSIGN ls_map->cur_value->* TO <lv_string>.
          <l_scr_field> = strlen( <lv_string> ).
          CONDENSE <l_scr_field>.
          CONCATENATE icon_change_text 'Chars'(chr) ` ` <l_scr_field> INTO <l_scr_field>.
      ENDCASE.
    ENDLOOP.

    " Set title
    CONCATENATE `(` mo_eui_screen->ms_screen-prog `)S_TITLE` INTO lv_name.
    ASSIGN (lv_name) TO <lv_title>.
    IF <lv_title> IS ASSIGNED.
      <lv_title> = mo_eui_screen->ms_status-title.
    ENDIF.

    " Text of parameters
    mv_pbo_set_labels = abap_true.

    " And call parent method
    super->pbo( iv_after = iv_after ).
  ENDMETHOD.
ENDCLASS.
