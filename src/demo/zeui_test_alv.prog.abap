*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zeui_test_alv.

TYPE-POOLS:
 abap.

SELECTION-SCREEN BEGIN OF BLOCK bl_grp WITH FRAME.
PARAMETERS:
  p_popup AS CHECKBOX DEFAULT 'X',
  p_reado AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl_grp.


**********************************************************************
**********************************************************************
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_alv,
        title    TYPE string,
        sum_1    TYPE bf_rbetr,
        sum_2    TYPE bf_rbetr,
        fld_tech TYPE string,
        hide2    TYPE string,
        ok       TYPE abap_bool, " checkbox ---> auto detect for domain XSDBOOLEAN
        bdc_mode TYPE ettcd_mode,
      END OF ts_alv.

    DATA:
     " 1-st level ALV
      mt_alv         TYPE STANDARD TABLE OF ts_alv WITH DEFAULT KEY.

    METHODS:
      start_of_selection,

      " Cannot call private methods! PRIVATE SECTION. METHODS:

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          sender
          e_row_id
          e_column_id,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm,

      on_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id,

      on_pbo_event FOR EVENT pbo_event OF zif_eui_manager
        IMPORTING
          sender,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object,

      on_double_click FOR EVENT double_click OF cl_gui_alv_grid.

*      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_txt_editor DEFINITION INHERITING FROM zcl_eui_manager FINAL.
  PUBLIC SECTION.

    DATA:
      mr_text     TYPE REF TO string,
      mo_grid     TYPE REF TO cl_gui_alv_grid,
      mo_textedit TYPE REF TO cl_gui_textedit.

    METHODS:
      constructor
        IMPORTING
          io_grid TYPE REF TO cl_gui_alv_grid
          ir_text TYPE REF TO string,

      " just set as self handler    pbo REDEFINITION
      on_pbo_event FOR EVENT pbo_event OF zif_eui_manager
        IMPORTING
          sender
          io_container,

      " just set as self handler    pai REDEFINITION
      on_pai_event FOR EVENT pai_event OF zif_eui_manager
        IMPORTING
          iv_command.
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
      lt_sort        TYPE lvc_t_sort,
      ls_sort        TYPE REF TO lvc_s_sort,
      lt_mod_catalog TYPE lvc_t_fcat.
    FIELD-SYMBOLS:
      <ls_alv>         LIKE LINE OF mt_alv,
      <ls_button>      LIKE LINE OF lt_toolbar,
      <ls_mod_catalog> LIKE LINE OF lt_mod_catalog.

**********************************************************************
    " Create data
**********************************************************************
    APPEND INITIAL LINE TO mt_alv ASSIGNING <ls_alv>.
    <ls_alv>-title = 'Text 1'.
    <ls_alv>-sum_1 = '1.11'.
    <ls_alv>-sum_2 = '2.22'.

    APPEND INITIAL LINE TO mt_alv ASSIGNING <ls_alv>.
    <ls_alv>-title = 'Text 2'.
    <ls_alv>-sum_1 = '3.33'.
    <ls_alv>-sum_2 = '4.44'.

    APPEND INITIAL LINE TO mt_alv ASSIGNING <ls_alv>.
    <ls_alv>-title = 'Text 2'.
    <ls_alv>-sum_1 = '5.55'.
    <ls_alv>-sum_2 = '6.66'.

**********************************************************************
    " Prepare layout
**********************************************************************
    ls_layout-cwidth_opt = abap_true.
    ls_layout-zebra      = abap_true.

    ls_layout-grid_title = 'Title for SALV'.
    ls_layout-smalltitle = abap_true.
    IF p_reado <> abap_true.
      ls_layout-edit = abap_true.
    ENDIF.

**********************************************************************
    " Change field catalog
**********************************************************************

    " What to change (MOVE-CORRESPONDING EXCEPT_INITIAL)
    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = 'TITLE'.
    <ls_mod_catalog>-hotspot   = abap_true.
    <ls_mod_catalog>-scrtext_s = <ls_mod_catalog>-scrtext_m = <ls_mod_catalog>-scrtext_l = 'Caption'.

    " Use mask
    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = 'SUM_*'.
    <ls_mod_catalog>-do_sum    = abap_true.

    " Only for second field
    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = 'SUM_2'.
    <ls_mod_catalog>-hotspot   = abap_true.

    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = 'OK'.
    <ls_mod_catalog>-checkbox  = 'X'.  " auto detect for domain XSDBOOLEAN
    <ls_mod_catalog>-scrtext_s = <ls_mod_catalog>-scrtext_m = <ls_mod_catalog>-scrtext_l = <ls_mod_catalog>-coltext = 'Ok'.

    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = 'BDC_MODE'.
    <ls_mod_catalog>-ref_table = 'ETTCD'.
    <ls_mod_catalog>-ref_field = 'MODE'.

    " Use group (if cannot use mask, from 7.3 could be very short)
    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = '+'.
    <ls_mod_catalog>-tech      = abap_true.
    " Copy from prev '+' field
    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = '+FLD_TECH'.
    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = '+HIDE2'.

**********************************************************************
    " Sort (Sort & filter - for read_only mode only ?)
**********************************************************************
    IF p_reado = abap_true.
      APPEND INITIAL LINE TO lt_sort REFERENCE INTO ls_sort.
      ls_sort->fieldname = 'TITLE'.
      ls_sort->down      = abap_true.
      ls_sort->subtot    = abap_true.
      ls_sort->expa      = abap_true.
    ENDIF.

**********************************************************************
    " Create additional buttons
**********************************************************************
    APPEND INITIAL LINE TO lt_toolbar ASSIGNING <ls_button>.
    <ls_button>-function = 'TEST_BUTTON'.
    <ls_button>-icon     = icon_complete.
    <ls_button>-text     = 'Press me!'.

**********************************************************************
    " Main table & ALV manager
**********************************************************************
    DATA lo_alv TYPE REF TO zcl_eui_alv.

    " Show top of page ?
    DATA lv_height TYPE i VALUE 12.
    IF p_popup = abap_true.
      CLEAR lv_height.
    ENDIF.

    " Pass by reference
    GET REFERENCE OF mt_alv INTO lr_table.
    CREATE OBJECT lo_alv
      EXPORTING
        ir_table       = lr_table
        " grid parameters
        is_layout      = ls_layout
        it_mod_catalog = lt_mod_catalog
        it_toolbar     = lt_toolbar
        it_sort        = lt_sort
        iv_read_only   = p_reado.
    lo_alv->set_top_of_page_height( lv_height ).

    " Popup?
    IF p_popup = abap_true.
      lo_alv->popup( ).
    ENDIF.

    " Instead of set handler
    lo_alv->show(
     io_handler        = me
     " If omit map all  (Could be several nad nested ON_USER_COMMAND)
*     iv_handlers_map   = 'ON_HOTSPOT_CLICK;ON_USER_COMMAND;ON_PBO_EVENT'
    ).
  ENDMETHOD.

  METHOD on_top_of_page.
    e_dyndoc_id->add_text( text      = 'Test of ZCL_EUI_ALV'
                           sap_style = cl_dd_area=>large ).

    e_dyndoc_id->new_line( repeat = 1 ).

    e_dyndoc_id->add_link( text =  'EUI library'
                           url  =  'https://bizhuka.github.io/eui/' ).
  ENDMETHOD.

  METHOD on_hotspot_click.
    DATA:
      lo_txt_editor TYPE REF TO zif_eui_manager,
      lr_text       TYPE REF TO string.
    FIELD-SYMBOLS:
      <ls_alv> LIKE LINE OF mt_alv.

    " Only for text
    " for sums could call another zcl_eui_alv=>show( )
    IF e_column_id <> 'TITLE'.
      MESSAGE 'No drilldown to SUM, please press CAPTION field' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    " Get current item
    READ TABLE mt_alv ASSIGNING <ls_alv> INDEX e_row_id-index.
    CHECK sy-subrc = 0.

    GET REFERENCE OF <ls_alv>-title INTO lr_text.
    CREATE OBJECT lo_txt_editor TYPE lcl_txt_editor
      EXPORTING
        io_grid = sender
        ir_text = lr_text.

    " In popup
    lo_txt_editor->popup( ).

    " Show in next SCREEN
    lo_txt_editor->show( io_handler = lo_txt_editor ).
  ENDMETHOD.

  METHOD on_toolbar.
    " Hide default buttons. to add use IT_TOOLBAR[] instead!
    DELETE e_object->mt_toolbar WHERE
      function CP '*MB_SUM*'     OR
      function CP '*MB_SUBTOT*'.
  ENDMETHOD.

  METHOD on_double_click.
    MESSAGE 'DOUBLE_CLICK' TYPE 'S'.
  ENDMETHOD.

  METHOD on_user_command.
    DATA:
      lt_t005t       TYPE STANDARD TABLE OF t005t WITH DEFAULT KEY,
      lr_table       TYPE REF TO data,
      lt_filter      TYPE lvc_t_filt,
      ls_layout	     TYPE lvc_s_layo,
      lt_mod_catalog TYPE lvc_t_fcat.
    FIELD-SYMBOLS:
      <ls_mod_catalog> LIKE LINE OF lt_mod_catalog,
      <ls_filter>      LIKE LINE OF lt_filter.

    CHECK e_ucomm = 'TEST_BUTTON'.

    " Show another table
    SELECT * INTO TABLE lt_t005t
    FROM t005t
    WHERE spras = sy-langu.
    GET REFERENCE OF lt_t005t INTO lr_table.

**********************************************************************
    " Change field catalog
**********************************************************************
    " Hide all by default
    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = '*'.
    <ls_mod_catalog>-no_out    = abap_true.

    " Show only started with L*
    APPEND INITIAL LINE TO lt_mod_catalog ASSIGNING <ls_mod_catalog>.
    <ls_mod_catalog>-fieldname = 'L*'.
    <ls_mod_catalog>-no_out    = abap_undefined. " Cannot write empty values

**********************************************************************
    " Prepare layout
**********************************************************************
    ls_layout-grid_title = 'Sub GRID!'.

**********************************************************************
    " Filter table
**********************************************************************
    APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
    <ls_filter>-fieldname = 'LAND1'.
    <ls_filter>-sign      = 'I'.
    <ls_filter>-option    = 'CP'.
    <ls_filter>-low       = 'A*'.

**********************************************************************
    " Main table & ALV manager
**********************************************************************
    DATA lo_alv TYPE REF TO zcl_eui_alv.

    " Pass by reference
    CREATE OBJECT lo_alv
      EXPORTING
        ir_table       = lr_table
        " grid parameters
        is_layout      = ls_layout
        it_mod_catalog = lt_mod_catalog
        iv_read_only   = p_reado.
    lo_alv->popup( ).

    " Instead of set handler
    lo_alv->show(
     " Update title bar only
     io_handler      = me
     iv_handlers_map = 'ON_PBO_EVENT' " ;ON_PAI_EVENT for 'OK' command
    ).
  ENDMETHOD.

  METHOD on_pbo_event.
    " Set handler for ALV->CLICK_COL_HEADER (Not in the basic list)
    CONCATENATE `SCREEN № ` sy-dynnr INTO sender->ms_status-title.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_txt_editor IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_read_only = p_reado ).
    mo_grid = io_grid.
    mr_text = ir_text.
  ENDMETHOD.

  METHOD on_pbo_event.
    DATA lv_mode TYPE i.

    " Update title
    sender->ms_status-title = 'Text editor'.

    " Hide button
    IF p_reado = abap_true.
      APPEND zcl_eui_manager=>mc_cmd-ok TO ms_status-exclude.
    ENDIF.

    " Initilize 1 time
    IF io_container IS NOT INITIAL.
      CREATE OBJECT mo_textedit
        EXPORTING
          parent = io_container
        EXCEPTIONS
          OTHERS = 1.

      " Set text
      mo_textedit->set_textstream( mr_text->* ).
      IF p_reado = abap_true.
        lv_mode = cl_gui_textedit=>true.
      ELSE.
        lv_mode = cl_gui_textedit=>false.
      ENDIF.
      mo_textedit->set_readonly_mode( lv_mode ).
    ENDIF.
  ENDMETHOD.

  METHOD on_pai_event.
    CHECK iv_command = 'OK'.

    " Update with new text
    IF p_reado <> abap_true.
      mo_textedit->get_textstream(
       IMPORTING
         text = mr_text->* ).
      cl_gui_cfw=>flush( ).

      " Update table and close
      mo_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.


**********************************************************************
**********************************************************************
INITIALIZATION.
  CREATE OBJECT go_report.

START-OF-SELECTION.
  go_report->start_of_selection( ).
