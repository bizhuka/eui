class ZCL_EUI_MENU definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ts_menu.
            INCLUDE TYPE stb_button.
    TYPES:
      " For menu
      par_function TYPE ui_func,
      ftype        TYPE cua_ftyp,
      hidden       TYPE cua_ftyp,
      accelerator  TYPE cua_path,
      top          TYPE flag,
      " Do not show
      hide         TYPE abap_bool,
      END OF ts_menu .
  types:
    tt_menu TYPE STANDARD TABLE OF ts_menu WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER optional
      !IO_HANDLER type ref to OBJECT optional .
  methods CREATE_TOOLBAR
    importing
      !IT_MENU type TT_MENU
      !IV_WIDTH type I optional
      !IV_CHECK_TCODE type ABAP_BOOL default ABAP_TRUE
    returning
      value(RO_MENU) type ref to ZCL_EUI_MENU .
  methods GET_CONTAINER
    returning
      value(RO_CONTAINER) type ref to CL_GUI_CONTAINER .
  methods GET_TOOLBAR
    returning
      value(RO_TOOLBAR) type ref to CL_GUI_TOOLBAR .
  methods ADD_HANDLER
    importing
      !IO_HANDLER type ref to OBJECT .
  methods CHANGE_HANDLER
    importing
      !IO_HANDLER type ref to OBJECT .
protected section.
private section.

  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MO_CONTAINER type ref to CL_GUI_CONTAINER .
  data MO_EVENT_CALLER type ref to ZCL_EUI_EVENT_CALLER .

  methods ON_FUNCTION_SELECTED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !SENDER
      !FCODE .
ENDCLASS.



CLASS ZCL_EUI_MENU IMPLEMENTATION.


METHOD add_handler.
  DATA lo_error TYPE REF TO zcx_eui_exception.

  CHECK io_handler IS NOT INITIAL.

  TRY.
      " Specify receiver
      IF mo_event_caller IS INITIAL.
        CREATE OBJECT mo_event_caller.
      ENDIF.

      mo_event_caller->add_handler( io_handler ).
    CATCH zcx_eui_exception INTO lo_error.
      zcx_eui_exception=>raise_dump( io_error = lo_error ).
  ENDTRY.
ENDMETHOD.


METHOD change_handler.
  DATA lo_error TYPE REF TO zcx_eui_exception.

  CLEAR mo_event_caller.
  CHECK io_handler IS NOT INITIAL.

  TRY.
      " Specify receiver
      CREATE OBJECT mo_event_caller.
      mo_event_caller->add_handler( io_handler ).
    CATCH zcx_eui_exception INTO lo_error.
      zcx_eui_exception=>raise_dump( io_error = lo_error ).
  ENDTRY.
ENDMETHOD.


METHOD constructor.
  " Use SUPPLIED container
  IF io_container IS NOT INITIAL.
    mo_container = io_container.
  ENDIF.

  change_handler( io_handler ).
ENDMETHOD.


METHOD create_toolbar.
  DATA:
    lp_dialog_status TYPE char1,
    lp_gui           TYPE char1,
    lp_cat           TYPE char1,
    lt_stb_menu      TYPE SORTED TABLE OF stb_btnmnu WITH UNIQUE KEY function,
    ls_stb_menu      TYPE stb_btnmnu,
    lv_icon          TYPE icon-id,
    lv_width         TYPE i,
    ls_event         TYPE REF TO cntl_simple_event,
    lt_event         TYPE cntl_simple_events.
  FIELD-SYMBOLS:
    <ls_menu>     LIKE LINE OF it_menu,
    <ls_stb_menu> TYPE stb_btnmnu.

  " For syntax like --->  new ZCL_EUI_MENU( lo_container )->CREATE_TOOLBAR( )->GET_TOOLBAR( )
  ro_menu = me.

  " Do not work in standard transactions (except SE38, SE80 ...)
  IF iv_check_tcode = abap_true.
    CHECK sy-tcode CP 'Z*'
       OR sy-tcode CP 'Y*'
       OR sy-tcode CP 'SE*'.
  ENDIF.

  " if we are called by RFC in a dialogless BAPI or in update task or in batch suppress the starter
  GET PARAMETER ID 'FLAG_DIALOG_STATUS' FIELD lp_dialog_status.

  CALL FUNCTION 'CAT_IS_ACTIVE'
    IMPORTING
      active = lp_cat.

  CALL FUNCTION 'RFC_IS_GUI_ON'
    IMPORTING
      on = lp_gui.

  CHECK  " TODO sy-binpt IS INITIAL
         sy-batch IS INITIAL
     AND sy-oncom <> 'V'
     AND lp_dialog_status IS INITIAL
     AND lp_cat = space
     AND lp_gui = 'Y'.

**********************************************************************
  " Calc new width
**********************************************************************
  lv_width = iv_width.
  IF lv_width = 0.
    LOOP AT it_menu ASSIGNING <ls_menu> WHERE hide <> abap_true
                                          AND par_function IS INITIAL.
      CASE <ls_menu>-butn_type.
        WHEN cntb_btype_menu.
          ADD 51 TO lv_width.

        WHEN cntb_btype_sep.
          ADD 6 TO lv_width.

        WHEN OTHERS.
          ADD 41 TO lv_width.
      ENDCASE.
    ENDLOOP.
  ENDIF.

**********************************************************************
  " Create new toolbar. Could be different buttons each time
**********************************************************************

  IF mo_toolbar IS NOT INITIAL.
    mo_toolbar->delete_all_buttons( ).
  ENDIF.

  " Create new container if needed
  IF mo_container IS INITIAL. " have to recretae for GOS. bug with contaoner width
    CREATE OBJECT mo_container TYPE cl_gui_gos_container
      EXPORTING
        width                   = lv_width
        no_autodef_progid_dynnr = abap_true
      EXCEPTIONS
        OTHERS                  = 5.
    CHECK sy-subrc = 0.
  ENDIF.

  " New toolbar
  IF mo_toolbar IS INITIAL.
    CREATE OBJECT mo_toolbar
      EXPORTING
        parent = mo_container.

    " 1 event only
    APPEND INITIAL LINE TO lt_event REFERENCE INTO ls_event.
    ls_event->appl_event = abap_true.
    ls_event->eventid = cl_gui_toolbar=>m_id_function_selected.
    mo_toolbar->set_registered_events( events = lt_event ).

    " Set new handler
    SET HANDLER on_function_selected FOR mo_toolbar ACTIVATION abap_true.
  ENDIF.

**********************************************************************
  " Create buttons
**********************************************************************
  LOOP AT it_menu ASSIGNING <ls_menu> WHERE hide <> abap_true.
    " No search in ICON table
    lv_icon = <ls_menu>-icon.

    IF <ls_menu>-par_function IS INITIAL.
      mo_toolbar->add_button(
        fcode       = <ls_menu>-function
        icon        = <ls_menu>-icon
        is_disabled = <ls_menu>-disabled
        butn_type   = <ls_menu>-butn_type
        text        = <ls_menu>-text
        quickinfo   = <ls_menu>-quickinfo
        is_checked  = <ls_menu>-checked ).
      " CHECK ERRORS
    ELSE.
      " Instead of CL_GUI_TOOLBAR=>M_TABLE_CTXMENU[]
      READ TABLE lt_stb_menu ASSIGNING <ls_stb_menu>
         WITH TABLE KEY function = <ls_menu>-par_function.

      " Create new one
      IF sy-subrc <> 0.
        CREATE OBJECT ls_stb_menu-ctmenu.
        ls_stb_menu-function = <ls_menu>-par_function.
        INSERT ls_stb_menu INTO TABLE lt_stb_menu ASSIGNING <ls_stb_menu>.
      ENDIF.

      CASE <ls_menu>-butn_type.
        WHEN cntb_btype_sep.
          <ls_stb_menu>-ctmenu->add_separator( ).

        WHEN cntb_btype_menu.
          " Save for sub menu
          CREATE OBJECT ls_stb_menu-ctmenu.
          ls_stb_menu-function = <ls_menu>-function.
          INSERT ls_stb_menu INTO TABLE lt_stb_menu.

          " And add it
          <ls_stb_menu>-ctmenu->add_submenu(
            menu        = ls_stb_menu-ctmenu " <-- sub menu
            icon        = lv_icon
            disabled    = <ls_menu>-disabled
            text        = <ls_menu>-text
            hidden      = <ls_menu>-hidden
            accelerator = <ls_menu>-accelerator ).

        WHEN OTHERS.
          <ls_stb_menu>-ctmenu->add_function(
            fcode       = <ls_menu>-function
            icon        = lv_icon
            disabled    = <ls_menu>-disabled
            text        = <ls_menu>-text
            checked     = <ls_menu>-checked
            ftype       = <ls_menu>-ftype
            hidden      = <ls_menu>-hidden
            accelerator = <ls_menu>-accelerator
            insert_at_the_top = <ls_menu>-top ).
      ENDCASE.

      " Set menu or delete it
      mo_toolbar->set_static_ctxmenu(
       EXPORTING
        fcode     = <ls_menu>-par_function
        ctxmenu   = <ls_stb_menu>-ctmenu
       EXCEPTIONS
        OTHERS    = 1 ).
    ENDIF.
  ENDLOOP.
ENDMETHOD.


METHOD get_container.
  ro_container = me->mo_container.
ENDMETHOD.


METHOD get_toolbar.
  ro_toolbar = me->mo_toolbar.
ENDMETHOD.


METHOD on_function_selected.
  CHECK mo_event_caller IS NOT INITIAL.

  mo_event_caller->call_handlers(
   iv_of_class     = 'CL_GUI_TOOLBAR'
   iv_for_event    = 'FUNCTION_SELECTED'
   iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
   iv_param_nam_01 = 'FCODE'           iv_param_val_01 = fcode ).
ENDMETHOD.
ENDCLASS.
