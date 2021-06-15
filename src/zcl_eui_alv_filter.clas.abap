class ZCL_EUI_ALV_FILTER definition
  public
  final
  create public

  global friends ZCL_EUI_EVENT_CALLER .

public section.
  type-pools ABAP .
  type-pools CNTB .
  type-pools COL .
  type-pools ICON .

  methods CONSTRUCTOR .
  methods CLEAR_RULES .
  methods ADD_RULE
    importing
      !IT_FILTER type LVC_T_FILT
      !IV_DESC type CSEQUENCE optional
      !IS_COLOR type LVC_S_COLO optional
      !IV_FIELD type CSEQUENCE optional
    returning
      value(RO_FILTER) type ref to ZCL_EUI_ALV_FILTER .
  methods ADD_BUTTON
    importing
      !IO_ALV type ref to ZCL_EUI_ALV
      !IS_BUTTON type STB_BUTTON optional
      !IV_MENU type ABAP_BOOL optional .
protected section.
private section.

  types:
    BEGIN OF ts_lvc_s_scol,
      fld_names TYPE string, " FNAME TYPE LVC_FNAME
      color	    TYPE lvc_s_colo,
      "NOKEYCOL TYPE LVC_NOKEYC
    END OF ts_lvc_s_scol .
  types:
    tt_lvc_s_scol TYPE STANDARD TABLE OF ts_lvc_s_scol WITH DEFAULT KEY .
  types:
    BEGIN OF ts_rule,
      selected  TYPE os_boolean,
      desc      TYPE string,
      t_filter  TYPE lvc_t_filt,
      t_color   TYPE tt_lvc_s_scol, "lvc_t_scol,
      count     TYPE i,
      own_color TYPE lvc_t_scol,
    END OF ts_rule .
  types:
    tt_rule TYPE STANDARD TABLE OF ts_rule WITH DEFAULT KEY .
  types:
    tt_color_filed   TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line .

  constants:
    BEGIN OF mc_cmd,
      popup     TYPE syucomm VALUE '_AF_SHOW_POPUP_',
      menu      TYPE syucomm VALUE '_AF_MENU_',
      clear_all TYPE numc3 VALUE 001,
    END OF mc_cmd .
  data MT_RULE type TT_RULE .
  data MO_ALV type ref to ZCL_EUI_ALV .
  data MV_SKIP type ABAP_BOOL .

  methods _ON_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !SENDER
      !E_UCOMM .
  methods _ON_AFTER_REFRESH
    for event AFTER_REFRESH of CL_GUI_ALV_GRID
    importing
      !SENDER .
  methods _ON_MENU_BUTTON
    for event MENU_BUTTON of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_UCOMM .
  methods _FILTER_BY_POPUP
    returning
      value(RV_OK) type ABAP_BOOL .
  methods _FILTER_BY_MENU
    importing
      !IV_COMMAND type SYUCOMM
    returning
      value(RV_OK) type ABAP_BOOL .
  methods _ON_POPUP_DATA_CHANGED
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !SENDER
      !ER_DATA_CHANGED .
  methods _SET_SELECTED
    importing
      !IV_INDEX type I
      !IV_VALUE type CSEQUENCE default ABAP_UNDEFINED .
  methods _UPDATE_RULE_TABLE .
  methods _GET_RULE_COLOR_FILED
    importing
      !IS_RULE type TS_RULE
      !IT_CATALOG type LVC_T_FCAT
    exporting
      !ET_FIELD type TT_COLOR_FILED
      !ES_COLOR type LVC_S_SCOL .
  methods _GET_RULE_DESCRIPTION
    importing
      !IS_RULE type TS_RULE
      !IT_CATALOG type LVC_T_FCAT
    returning
      value(RV_DESC) type STRING .
ENDCLASS.



CLASS ZCL_EUI_ALV_FILTER IMPLEMENTATION.


METHOD add_button.
  mo_alv = io_alv.

  DATA ls_button LIKE is_button.
  ls_button-butn_type = cntb_btype_sep.
  mo_alv->add_button( ls_button ).

  ls_button = is_button.
  IF ls_button-icon IS INITIAL.
    ls_button-icon = icon_color.
  ENDIF.

  IF iv_menu = abap_true.
    ls_button-butn_type = cntb_btype_menu.
    ls_button-function  = mc_cmd-menu.
  ELSE.
    ls_button-butn_type = cntb_btype_button.
    ls_button-function  = mc_cmd-popup.

    IF ls_button-text <> abap_undefined.
      ls_button-text = 'Show filters'(sfl).
    ENDIF.
  ENDIF.

  mo_alv->add_button( is_button       = ls_button
                      io_handler      = me
                      iv_handlers_map = '_ON_USER_COMMAND;_ON_AFTER_REFRESH;_ON_MENU_BUTTON' ).
ENDMETHOD.


METHOD add_rule.
  " For add_rule( )->add_rule( ) chains
  ro_filter = me.

  FIELD-SYMBOLS <ls_rule> LIKE LINE OF mt_rule.
  APPEND INITIAL LINE TO mt_rule ASSIGNING <ls_rule>.
  <ls_rule>-desc = iv_desc.

  " Set defaults
  DATA ls_filter LIKE LINE OF it_filter.
  LOOP AT it_filter INTO ls_filter.
    IF ls_filter-sign IS INITIAL.
      ls_filter-sign = 'I'.
    ENDIF.
    IF ls_filter-option IS INITIAL.
      ls_filter-option = 'EQ'.
    ENDIF.

    APPEND ls_filter TO <ls_rule>-t_filter.
  ENDLOOP.

**********************************************************************
  CHECK is_color IS NOT INITIAL.
  FIELD-SYMBOLS <ls_column_color> LIKE LINE OF <ls_rule>-t_color[].
  APPEND INITIAL LINE TO <ls_rule>-t_color[] ASSIGNING <ls_column_color>.
  <ls_column_color>-fld_names = iv_field.
  <ls_column_color>-color     = is_color.

**********************************************************************
  DATA lt_field TYPE STANDARD TABLE OF fieldname.
  DATA lv_field TYPE fieldname.

  SPLIT 'SELECTED,DESC,COUNT' AT ',' INTO TABLE lt_field.
  LOOP AT lt_field INTO lv_field.
    FIELD-SYMBOLS <ls_color> TYPE lvc_s_scol.
    APPEND INITIAL LINE TO <ls_rule>-own_color[] ASSIGNING <ls_color>.
    <ls_color>-fname = lv_field.
    <ls_color>-color = is_color.
  ENDLOOP.
ENDMETHOD.


METHOD clear_rules.
  CLEAR mt_rule[].

  FIELD-SYMBOLS <ls_rule> LIKE LINE OF mt_rule.
  APPEND INITIAL LINE TO mt_rule ASSIGNING <ls_rule>.
  <ls_rule>-desc     = 'Clear all filters'(caf).
  <ls_rule>-selected = abap_true.
ENDMETHOD.


METHOD constructor.
  clear_rules( ).
ENDMETHOD.


METHOD _filter_by_menu.
  " Always ok
  rv_ok = abap_true.

  DATA: lv_index    TYPE i, lv_selected TYPE abap_bool VALUE abap_undefined.
  lv_index = iv_command+9.

  IF lv_index = mc_cmd-clear_all.
    lv_selected = abap_true.
  ENDIF.

  _set_selected( iv_index = lv_index
                 iv_value = lv_selected ).
ENDMETHOD.


METHOD _filter_by_popup.
  FIELD-SYMBOLS <ls_rule> LIKE LINE OF mt_rule.

  DATA lt_prev_selected TYPE STANDARD TABLE OF abap_bool.
  LOOP AT mt_rule ASSIGNING <ls_rule>.
    APPEND <ls_rule>-selected TO lt_prev_selected.
  ENDLOOP.

**********************************************************************
  DATA lt_catalog TYPE lvc_t_fcat.
  DATA lr_catalog TYPE REF TO lvc_s_fcat.
  APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
  lr_catalog->fieldname = 'SELECTED'.
  lr_catalog->checkbox  = abap_true.
  lr_catalog->edit      = 'X'.
  lr_catalog->coltext   = 'Filter'(flt).

  APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
  lr_catalog->fieldname = 'COUNT'.
  lr_catalog->coltext   = 'Count'(cnt).

  APPEND INITIAL LINE TO lt_catalog REFERENCE INTO lr_catalog.
  lr_catalog->fieldname = 'DESC'.
  lr_catalog->coltext   = 'Description'(des).

**********************************************************************
  DATA lr_table TYPE REF TO data.
  GET REFERENCE OF mt_rule INTO lr_table.

  DATA ls_layout TYPE lvc_s_layo.
  ls_layout-no_rowmark = ls_layout-no_toolbar = 'X'. " ls_layout-no_headers =
  ls_layout-ctab_fname = 'OWN_COLOR'.

  DATA lo_popup TYPE REF TO zcl_eui_alv.
  CREATE OBJECT lo_popup
    EXPORTING
      ir_table       = lr_table
      it_mod_catalog = lt_catalog
      is_layout      = ls_layout.

  lo_popup->popup( iv_col_beg = 25
                   iv_col_end = 70
                   iv_row_beg = 3
                   iv_row_end = 10 ).
  IF lo_popup->show( io_handler      = me
                        iv_handlers_map = '_ON_POPUP_DATA_CHANGED' ) <> 'OK'.
    " Restore values
    DATA lv_selected TYPE abap_bool.
    LOOP AT lt_prev_selected INTO lv_selected.
      READ TABLE mt_rule ASSIGNING <ls_rule> INDEX sy-tabix.
      <ls_rule>-selected = lv_selected.
    ENDLOOP.
    RETURN.
  ENDIF.

  rv_ok = abap_true.
ENDMETHOD.


METHOD _get_rule_color_filed.
  CLEAR: et_field,
         es_color.

  FIELD-SYMBOLS <ls_color> LIKE LINE OF is_rule-t_color[].
  READ TABLE is_rule-t_color[] INDEX 1 ASSIGNING <ls_color>.

  es_color-color = <ls_color>-color.
  CASE <ls_color>-fld_names.
    WHEN ``.
      " All fields in filter condition
      FIELD-SYMBOLS <ls_filter> LIKE LINE OF is_rule-t_filter.
      LOOP AT is_rule-t_filter ASSIGNING <ls_filter>.
        INSERT <ls_filter>-fieldname INTO TABLE et_field.
      ENDLOOP.

    WHEN `*`.
      " All fields in field catalog
      FIELD-SYMBOLS <ls_catalog> LIKE LINE OF it_catalog.
      LOOP AT it_catalog ASSIGNING <ls_catalog> WHERE tech    <> 'X'
                                                  AND inttype <> cl_abap_typedescr=>typekind_table.
        INSERT <ls_catalog>-fieldname INTO TABLE et_field.
      ENDLOOP.

    WHEN OTHERS.
      DATA lt_fields TYPE STANDARD TABLE OF fieldname.
      IF <ls_color>-fld_names CS ','.
        SPLIT <ls_color>-fld_names AT ',' INTO TABLE lt_fields.
      ELSE.
        SPLIT <ls_color>-fld_names AT ';' INTO TABLE lt_fields.
      ENDIF.

      DATA lv_field TYPE fieldname.
      LOOP AT lt_fields INTO lv_field.
        INSERT lv_field INTO TABLE et_field.
      ENDLOOP.
  ENDCASE.

ENDMETHOD.


METHOD _get_rule_description.
  rv_desc = is_rule-desc.
  CHECK rv_desc IS INITIAL.

  DATA lt_range  TYPE rs_t_rscedst.
  DATA lr_range  TYPE REF TO rscedst.
  DATA lr_filt   TYPE REF TO lvc_s_filt.
  LOOP AT is_rule-t_filter REFERENCE INTO lr_filt.
    APPEND INITIAL LINE TO lt_range REFERENCE INTO lr_range.
    MOVE-CORRESPONDING lr_filt->* TO lr_range->*.
    lr_range->fnam = lr_filt->fieldname.
  ENDLOOP.

  CALL FUNCTION 'RSDS_RANGE_TO_WHERE'
    EXPORTING
      i_t_range = lt_range
    IMPORTING
      e_where   = rv_desc
    EXCEPTIONS
      OTHERS    = 0.

  LOOP AT is_rule-t_filter REFERENCE INTO lr_filt.
    DATA lr_catalog TYPE REF TO lvc_s_fcat.
    READ TABLE it_catalog REFERENCE INTO lr_catalog
     WITH KEY fieldname = lr_filt->fieldname.
    CHECK sy-subrc = 0.

    REPLACE ALL OCCURRENCES OF lr_catalog->fieldname IN rv_desc WITH lr_catalog->coltext.
  ENDLOOP.
ENDMETHOD.


METHOD _on_after_refresh.
  CHECK mv_skip <> abap_true.

  _update_rule_table( ).

  IF sender IS NOT INITIAL.
    mv_skip = abap_true.
    DATA ls_stable TYPE lvc_s_stbl.
    ls_stable-col = ls_stable-row = 'X'.
    sender->refresh_table_display( is_stable = ls_stable ).
  ENDIF.
  mv_skip = abap_false.
ENDMETHOD.


METHOD _on_menu_button.
  CHECK e_ucomm = mc_cmd-menu.

  _update_rule_table( ).

  DATA lt_catalog TYPE lvc_t_fcat.
  lt_catalog = zcl_eui_type=>get_catalog( ir_table = mo_alv->mr_table ).

  DATA lr_rule TYPE REF TO ts_rule.
  LOOP AT mt_rule REFERENCE INTO lr_rule.
    DATA lv_index TYPE numc3.
    lv_index = sy-tabix.

    DATA lv_text TYPE gui_text.
    lv_text = _get_rule_description( is_rule    = lr_rule->*
                                     it_catalog = lt_catalog ).
    " Count of items bu rule
    DATA lv_count TYPE text5.
    WRITE lr_rule->count TO lv_count RIGHT-JUSTIFIED.
    CONCATENATE lv_count ` - ` lv_text INTO lv_text.

    " Menu code
    DATA lv_fcode TYPE syucomm.
    CONCATENATE mc_cmd-menu lv_index INTO lv_fcode.

    e_object->add_function( fcode   = lv_fcode
                            text    = lv_text
                            checked = lr_rule->selected ).
  ENDLOOP.
ENDMETHOD.


METHOD _on_popup_data_changed.
  " Only checkbox click
  FIELD-SYMBOLS <ls_cell> LIKE LINE OF er_data_changed->mt_good_cells[].
  LOOP AT er_data_changed->mt_good_cells[] ASSIGNING <ls_cell> WHERE fieldname = 'SELECTED'.
    _set_selected( iv_index = <ls_cell>-row_id
                   iv_value = <ls_cell>-value ).
  ENDLOOP.

  DATA ls_stable TYPE lvc_s_stbl.
  ls_stable-col = ls_stable-row = 'X'.
  sender->refresh_table_display( is_stable = ls_stable ).
ENDMETHOD.


METHOD _on_user_command.
  DATA lv_ok TYPE abap_bool.
  IF e_ucomm = mc_cmd-popup.
    lv_ok = _filter_by_popup( ).
  ELSEIF e_ucomm CS mc_cmd-menu.
    lv_ok = _filter_by_menu( e_ucomm ).
  ENDIF.
  CHECK lv_ok = abap_true.

  " What items is selecter
  DATA lt_filter TYPE lvc_t_filt.
  FIELD-SYMBOLS <ls_rule> LIKE LINE OF mt_rule.
  LOOP AT mt_rule ASSIGNING <ls_rule> WHERE selected = 'X'.
    APPEND LINES OF <ls_rule>-t_filter[] TO lt_filter.
  ENDLOOP.

  sender->set_filter_criteria( EXPORTING  it_filter = lt_filter
                               EXCEPTIONS OTHERS    = 0 ).

  DATA ls_stable TYPE lvc_s_stbl.
  ls_stable-col = ls_stable-row = 'X'.
  sender->refresh_table_display( is_stable = ls_stable ).
ENDMETHOD.


METHOD _set_selected.
  FIELD-SYMBOLS <ls_rule> LIKE LINE OF mt_rule.
  READ TABLE mt_rule ASSIGNING <ls_rule> INDEX iv_index.
  " CHECK sy-subrc = 0. better dump

  IF iv_value <> abap_undefined.
    <ls_rule>-selected = iv_value.
  ELSEIF <ls_rule>-selected = abap_true. " just invert
    <ls_rule>-selected = abap_false.
  ELSE.
    <ls_rule>-selected = abap_true.
  ENDIF.

  " Change other selection ?
  CHECK <ls_rule>-selected = abap_true.

  DATA lv_sel_first TYPE abap_bool VALUE abap_undefined.
  DATA lv_sel_other TYPE abap_bool VALUE abap_undefined.

  IF iv_index = mc_cmd-clear_all.
    lv_sel_other = abap_false.
  ELSE.
    lv_sel_first = abap_false.
  ENDIF.

  LOOP AT mt_rule ASSIGNING <ls_rule>.
    IF sy-tabix = mc_cmd-clear_all.
      IF lv_sel_first <> abap_undefined.
        <ls_rule>-selected = lv_sel_first.
      ENDIF.
      CONTINUE.
    ENDIF.

    IF lv_sel_other = abap_undefined.
      RETURN.
    ENDIF.
    <ls_rule>-selected = lv_sel_other.
  ENDLOOP.
ENDMETHOD.


METHOD _update_rule_table.
  FIELD-SYMBOLS: <lt_table>   TYPE INDEX TABLE,
                 <ls_row>     TYPE any,
                 <t_lvc_scol> TYPE lvc_t_scol.
  ASSIGN mo_alv->mr_table->* TO <lt_table>.
  IF mo_alv->ms_layout-ctab_fname IS NOT INITIAL.
    LOOP AT <lt_table> ASSIGNING <ls_row>.
      ASSIGN COMPONENT mo_alv->ms_layout-ctab_fname OF STRUCTURE <ls_row> TO <t_lvc_scol>.
      CLEAR <t_lvc_scol>.
    ENDLOOP.
  ENDIF.

**********************************************************************
  DATA lo_filter TYPE REF TO object.
  CREATE OBJECT lo_filter TYPE ('CL_ALV_LVC_FILTER_SERVICE'). " TODO Work in 7.00 ?

  DATA lt_catalog TYPE lvc_t_fcat.
  lt_catalog = zcl_eui_type=>get_catalog( ir_table = mo_alv->mr_table ).

  FIELD-SYMBOLS <ls_rule> LIKE LINE OF mt_rule.
  DATA: lt_ind_inside TYPE lvc_t_fidx, lv_tabix TYPE sytabix, lv_index TYPE i.
  LOOP AT mt_rule ASSIGNING <ls_rule>.
    lv_tabix = sy-tabix.

    <ls_rule>-desc = _get_rule_description( is_rule    = <ls_rule>
                                            it_catalog = lt_catalog ).
    CLEAR lt_ind_inside.
    CALL METHOD lo_filter->('APPLY')
      EXPORTING
        rt_data               = mo_alv->mr_table
        t_fieldcatalog        = lt_catalog
        t_filter              = <ls_rule>-t_filter[]
      IMPORTING
        t_filter_index_inside = lt_ind_inside.
    IF lv_tabix = mc_cmd-clear_all.
      <ls_rule>-count = lines( <lt_table> ).
    ELSE.
      <ls_rule>-count = lines( lt_ind_inside ).
    ENDIF.
**********************************************************************
    CHECK <ls_rule>-t_color[] IS NOT INITIAL
      AND mo_alv->ms_layout-ctab_fname IS NOT INITIAL.

    DATA: lt_field TYPE tt_color_filed,
          ls_color TYPE lvc_s_scol.
    _get_rule_color_filed( EXPORTING is_rule    = <ls_rule>
                                     it_catalog = lt_catalog
                           IMPORTING et_field   = lt_field
                                     es_color   = ls_color ).
    LOOP AT lt_ind_inside INTO lv_index.
      " Get color table
      READ TABLE <lt_table> ASSIGNING <ls_row> INDEX lv_index.
      ASSIGN COMPONENT mo_alv->ms_layout-ctab_fname OF STRUCTURE <ls_row> TO <t_lvc_scol>.

      DATA lv_field LIKE LINE OF lt_field.
      LOOP AT lt_field INTO lv_field.
        ls_color-fname = lv_field.
        APPEND ls_color TO <t_lvc_scol>.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
