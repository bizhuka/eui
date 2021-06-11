class ZCL_EUI_ALV_FILTER definition
  public
  final
  create public

  global friends ZCL_EUI_EVENT_CALLER .

public section.
  type-pools ABAP .
  type-pools COL .
  type-pools ICON .

  methods CONSTRUCTOR .
  methods CLEAR .
  methods ADD
    importing
      !IT_FILTER type LVC_T_FILT
      !IV_DESC type CSEQUENCE optional
      !IS_COLOR type LVC_S_COLO optional
      !IV_FIELD type CSEQUENCE optional .
  methods APPLY
    importing
      !IO_ALV type ref to ZCL_EUI_ALV
      !IS_BUTTON type STB_BUTTON optional .
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
    tt_color_filed TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line .

  data MT_RULE type TT_RULE .
  data MO_ALV type ref to ZCL_EUI_ALV .

  methods _ON_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !SENDER
      !E_UCOMM .
  methods _ON_AFTER_REFRESH
    for event AFTER_REFRESH of CL_GUI_ALV_GRID .
  methods _GET_RULE_COLOR_FILED
    importing
      !IS_RULE type TS_RULE
      !IT_CATALOG type LVC_T_FCAT
    exporting
      !ET_FIELD type TT_COLOR_FILED
      !ES_COLOR type LVC_S_SCOL .
ENDCLASS.



CLASS ZCL_EUI_ALV_FILTER IMPLEMENTATION.


METHOD add.
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


METHOD apply.
  mo_alv = io_alv.
**********************************************************************
  DATA ls_button LIKE is_button.
  ls_button = is_button.
  IF ls_button-icon IS INITIAL.
    ls_button-icon = icon_color.
  ENDIF.
  IF ls_button-text <> abap_undefined.
    ls_button-text = 'Show filters'(sfl).
  ENDIF.

  ls_button-function = '_SHOW_FILTERS_'.
  mo_alv->add_button( is_button       = ls_button
                      io_handler      = me
                      iv_handlers_map = '_ON_USER_COMMAND;_ON_AFTER_REFRESH' ).

**********************************************************************
  _on_after_refresh( ).
ENDMETHOD.


METHOD clear.
  CLEAR mt_rule[].

  FIELD-SYMBOLS <ls_rule> LIKE LINE OF mt_rule.
  APPEND INITIAL LINE TO mt_rule ASSIGNING <ls_rule>.
  <ls_rule>-desc = 'Clear all filters'(caf).
ENDMETHOD.


METHOD constructor.
  clear( ).
ENDMETHOD.


METHOD _get_rule_color_filed.
  CLEAR: et_field,
         et_field.

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


METHOD _on_after_refresh.
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
  CREATE OBJECT lo_filter TYPE ('CL_ALV_LVC_FILTER_SERVICE').

  DATA lt_catalog TYPE lvc_t_fcat.
  lt_catalog = zcl_eui_type=>get_catalog( ir_table = mo_alv->mr_table ).

  FIELD-SYMBOLS <ls_rule> LIKE LINE OF mt_rule.
  DATA: lt_ind_inside TYPE lvc_t_fidx, lv_tabix TYPE sytabix, lv_index TYPE i.
  LOOP AT mt_rule ASSIGNING <ls_rule>.
    lv_tabix = sy-tabix.

    CLEAR lt_ind_inside.
    CALL METHOD lo_filter->('APPLY')
      EXPORTING
        rt_data               = mo_alv->mr_table
        t_fieldcatalog        = lt_catalog
        t_filter              = <ls_rule>-t_filter[]
      IMPORTING
        t_filter_index_inside = lt_ind_inside.
    IF lv_tabix = 1.
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


METHOD _on_user_command.
  CHECK e_ucomm = '_SHOW_FILTERS_'.

  FIELD-SYMBOLS <ls_rule> LIKE LINE OF mt_rule.
  LOOP AT mt_rule ASSIGNING <ls_rule>.
    <ls_rule>-selected = abap_false.
  ENDLOOP.

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
  CHECK lo_popup->show( ) = 'OK'.

  DATA lt_filter LIKE <ls_rule>-t_filter.
  LOOP AT mt_rule ASSIGNING <ls_rule> WHERE selected = 'X'.
    APPEND LINES OF <ls_rule>-t_filter[] TO lt_filter.
  ENDLOOP.

  sender->set_filter_criteria( EXPORTING  it_filter = lt_filter
                               EXCEPTIONS OTHERS    = 0 ).

  DATA ls_stable TYPE lvc_s_stbl.
  ls_stable-col = ls_stable-row = 'X'.
  sender->refresh_table_display( is_stable = ls_stable ).
ENDMETHOD.
ENDCLASS.
