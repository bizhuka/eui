class ZCL_EUI_TREE definition
  public
  inheriting from ZCL_EUI_MANAGER
  final
  create public .

public section.
*"* public components of class ZCL_EUI_TREE
*"* do not include other source files here!!!
  type-pools ABAP .

  class CL_GUI_COLUMN_TREE definition load .
  methods CONSTRUCTOR
    importing
      !IR_TABLE type ref to DATA
      !IS_HEADER type TREEV_HHDR
      !IT_MOD_CATALOG type LVC_T_FCAT optional
      !NODE_SELECTION_MODE type I default CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
      !HIDE_SELECTION type ABAP_BOOL optional
      !ITEM_SELECTION type ABAP_BOOL default ABAP_TRUE
      !NO_TOOLBAR type ABAP_BOOL optional
      !NO_HTML_HEADER type ABAP_BOOL optional
      !IV_EDITABLE type ABAP_BOOL default ABAP_FALSE .
  methods GET_TREE
    returning
      value(RO_TREE) type ref to CL_GUI_ALV_TREE .

  methods ZIF_EUI_MANAGER~PBO
    redefinition .
protected section.
*"* protected components of class ZCL_EUI_TREE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_EUI_TREE
*"* do not include other source files here!!!

  types:
    BEGIN OF ts_constructor,
      node_selection_mode TYPE  i,
      hide_selection      TYPE  abap_bool,
      item_selection      TYPE  abap_bool,
      no_toolbar          TYPE  abap_bool,
      no_html_header      TYPE  abap_bool,
   END OF ts_constructor .
  types:
    BEGIN OF ts_first_display,
    s_header  TYPE treev_hhdr,
    t_catalog TYPE lvc_t_fcat,
   END OF ts_first_display .

  data MR_TABLE type ref to DATA .
  data MO_TREE type ref to CL_GUI_ALV_TREE .
  data MS_CONSTRUCTOR type TS_CONSTRUCTOR .
  data MS_FIRST_DISPLAY type TS_FIRST_DISPLAY .

  methods _SET_HANDLERS .
  methods _ON_LINK_CLICK
    for event LINK_CLICK of CL_GUI_ALV_TREE
    importing
      !SENDER
      !FIELDNAME
      !NODE_KEY .
  methods _ON_EXPAND_NC
    for event EXPAND_NC of CL_GUI_ALV_TREE
    importing
      !SENDER
      !NODE_KEY .
ENDCLASS.



CLASS ZCL_EUI_TREE IMPLEMENTATION.


METHOD constructor.
  super->constructor( iv_editable = iv_editable ).
  mr_table = ir_table.

  ms_constructor-node_selection_mode = node_selection_mode.
  ms_constructor-hide_selection      = hide_selection.
  ms_constructor-item_selection      = item_selection.
  ms_constructor-no_toolbar          = no_toolbar.
  ms_constructor-no_html_header      = no_html_header.

  ms_first_display-s_header  = is_header.
  ms_first_display-t_catalog = zcl_eui_type=>get_mod_catalog(
    ir_table       = mr_table
    it_mod_catalog = it_mod_catalog ).

  FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
  ASSIGN mr_table->* TO <lt_table>.
  IF sy-subrc <> 0 OR <lt_table>[] IS NOT INITIAL.
    zcx_eui_no_check=>raise_sys_error( iv_message = 'Please pass valid empty standard table'(ppv) ).
  ENDIF.
ENDMETHOD.


METHOD get_tree.
  ro_tree = mo_tree.
ENDMETHOD.


METHOD zif_eui_manager~pbo.
  " Initialize 1 time
  IF io_container IS NOT INITIAL AND mo_tree IS INITIAL.
    CREATE OBJECT mo_tree
      EXPORTING
        parent              = io_container
        node_selection_mode = ms_constructor-node_selection_mode
        hide_selection      = ms_constructor-hide_selection
        item_selection      = ms_constructor-item_selection
        no_toolbar          = ms_constructor-no_toolbar
        no_html_header      = ms_constructor-no_html_header.

    _set_handlers( ).

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    ASSIGN mr_table->* TO <lt_table>.

    mo_tree->set_table_for_first_display(
      EXPORTING is_hierarchy_header = ms_first_display-s_header
      CHANGING  it_outtab           = <lt_table>
                it_fieldcatalog     = ms_first_display-t_catalog[] ).
  ENDIF.

  super->pbo(
   io_container  = io_container
   iv_set_status = iv_set_status  ).
ENDMETHOD.


METHOD _on_expand_nc.
  mo_event_caller->call_handlers(
   iv_of_class     = 'CL_GUI_ALV_TREE'
   iv_for_event    = 'EXPAND_NC'
   iv_param_nam_00 = 'SENDER'       iv_param_val_00 = sender
   iv_param_nam_01 = 'NODE_KEY'     iv_param_val_01 = node_key ).
ENDMETHOD.


METHOD _on_link_click.
  mo_event_caller->call_handlers(
   iv_of_class     = 'CL_GUI_ALV_TREE'
   iv_for_event    = 'LINK_CLICK'
   iv_param_nam_00 = 'SENDER'       iv_param_val_00 = sender
   iv_param_nam_01 = 'FIELDNAME'    iv_param_val_01 = fieldname
   iv_param_nam_02 = 'NODE_KEY'     iv_param_val_02 = node_key ).
ENDMETHOD.


METHOD _set_handlers.
  SET HANDLER: _on_link_click FOR mo_tree,
               _on_expand_nc  FOR mo_tree.

  DATA lt_event TYPE cntl_simple_events.
  DATA ls_event TYPE cntl_simple_event.
  mo_tree->get_registered_events( IMPORTING  events = lt_event
                                  EXCEPTIONS OTHERS = 0 ).

  DATA lv_has_handler TYPE abap_bool.
  lv_has_handler = mo_event_caller->has_handler(
          iv_of_class  = 'CL_GUI_ALV_TREE'
          iv_for_event = 'LINK_CLICK' ).
  IF lv_has_handler = abap_true.
    ls_event-eventid = cl_gui_column_tree=>eventid_link_click.
    APPEND ls_event TO lt_event.
  ENDIF.

  lv_has_handler = mo_event_caller->has_handler(
          iv_of_class  = 'CL_GUI_ALV_TREE'
          iv_for_event = 'EXPAND_NC' ).
  IF lv_has_handler = abap_true.
    ls_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
    APPEND ls_event TO lt_event.
  ENDIF.

  SORT lt_event.
  DELETE ADJACENT DUPLICATES FROM lt_event COMPARING ALL FIELDS.
  mo_tree->set_registered_events( EXPORTING  events = lt_event
                                  EXCEPTIONS OTHERS = 1 ).
  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( iv_message = 'Cannot registere events for TREE').
ENDMETHOD.
ENDCLASS.
