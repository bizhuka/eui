class ZCL_EUI_TEST_SCREEN_02 definition
  public
  final
  create public .

public section.

  class-methods SHOW_INITIAL_SCREEN .
  methods ON_START_PAI
    for event PAI_EVENT of ZIF_EUI_MANAGER
    importing
      !SENDER
      !IV_COMMAND
      !CV_CLOSE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EUI_TEST_SCREEN_02 IMPLEMENTATION.


METHOD on_start_pai.
  DATA:
    lo_screen    TYPE REF TO zcl_eui_screen,
    lr_context   TYPE REF TO DATA.
  FIELD-SYMBOLS:
    <ls_context> TYPE any. " in real app use global -> TS_CONTEXT_MAIN

  " No data check. just close screen
  CHECK iv_command CP 'CMD_*'.

  " Get access to screen data
  lo_screen ?= sender.
  lr_context = lo_screen->get_context( ).
  ASSIGN lr_context->* TO <ls_context>.

  " For demo only
  IF <ls_context> IS INITIAL.
    MESSAGE 'Set at least one checkbox' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  cv_close->* = abap_true.
ENDMETHOD.


METHOD show_initial_screen.
  CONSTANTS
   c_cprog TYPE sycprog VALUE 'ZEUI_TEST_SCREEN_02'.

  TYPES:
**********************************************************************
    " Screen declarations
**********************************************************************

    " Main screen
    BEGIN OF ts_context_main,
      v_make_gray     TYPE xsdboolean,
      v_make_required TYPE xsdboolean,
      v_make_lsitbox  TYPE xsdboolean,
    END OF ts_context_main,

    " Checked SCREEN context
    BEGIN OF ts_context,
      p_bukrs  TYPE bukrs,
      p_bdc_m  TYPE ettcd_mode, " <-- listbox by domain
      p_mandt  TYPE t001-mandt, " <-- listbox in runtime
      p_check  TYPE xsdboolean,
      s_user   TYPE offline_log_user_itab, " Range <-- cl_ci_query_attributes no SH
      p_land1  TYPE t005t-land1,
      p_fld_i  TYPE syindex,       " do not use i! use from dictionary
      p_fld_i2 TYPE sytabix,       " do not use i! use from dictionary
      " p_memo     TYPE stringval, " String & tables also Ok
    END OF ts_context,

    " For listbox
    BEGIN OF ts_t000,
      mandt TYPE t000-mandt,
      mtext TYPE t000-mtext,
    END OF ts_t000.

**********************************************************************
**********************************************************************
  DATA lo_screen_main  TYPE REF TO zcl_eui_screen.
  DATA lr_context_main TYPE REF TO ts_context_main.
  DATA lo_handler      TYPE REF TO zcl_eui_test_screen_02.
  DATA lv_cmd          TYPE syucomm.
  DATA lo_error        TYPE REF TO zcx_eui_exception.
  " Checked screen
  DATA lo_screen       TYPE REF TO zcl_eui_screen.
  DATA ls_context      TYPE REF TO ts_context.
  DATA lt_scr          TYPE zcl_eui_screen=>tt_screen.
  DATA ls_scr          TYPE REF TO screen.
  DATA ls_user         LIKE LINE OF ls_context->s_user.
  DATA lv_message      TYPE string.
  DATA lv_dynnr        TYPE sydynnr.
  DATA lv_prog         TYPE sycprog.
  DATA lt_t000         TYPE STANDARD TABLE OF ts_t000 WITH DEFAULT KEY.
  DATA lr_t000         TYPE REF TO ts_t000.
  DATA lt_listbox      TYPE vrm_values.
  DATA lr_listbox      TYPE REF TO vrm_value.
  FIELD-SYMBOLS:
    <lv_text> TYPE string.

  " Pass data to main screen
  CREATE DATA lr_context_main.
  " all checked
  lr_context_main->v_make_gray     =
  lr_context_main->v_make_required =
  lr_context_main->v_make_lsitbox  = abap_true.

  " Event handler
  CREATE OBJECT lo_handler.

  " Endless loop
  WHILE 1 EQ 1.
    TRY.
        " Pass params
        CREATE OBJECT lo_screen_main
          EXPORTING
            iv_dynnr        = zcl_eui_screen=>mc_dynnr-free_sel
            iv_status_prog  = c_cprog
            iv_status_name  = 'START_STATUS'
            iv_status_title = 'Start screen'
            ir_context      = lr_context_main.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Prepare for PBO
    lo_screen_main->customize( name     = 'V_MAKE_GRAY'
                               iv_label = 'Make gray' ).
    lo_screen_main->customize( name     = 'V_MAKE_REQUIRED'
                               iv_label = 'Make required' ).
    lo_screen_main->customize( name     = 'V_MAKE_LSITBOX'
                               iv_label = 'Fill listbox' ).


    " Next screen interation
    lv_cmd = lo_screen_main->show(
     io_handler      = lo_handler
     " For demo purpose
     iv_handlers_map = 'ON_START_PAI' ). " <- Optional (by default call all handlers)

    " Pressed cancel
    IF lv_cmd NP 'CMD_*'.
      RETURN. " Cancel pressed
    ENDIF.

**********************************************************************
    " Fill context
    CREATE DATA ls_context.
    ls_context->p_bukrs    = '1000'.
    ls_context->p_bdc_m    = 'A'.
    ls_context->p_mandt    = sy-mandt.
    ls_context->p_check    = abap_true.

    " Dfault value for SELECT-OPTION
    ls_user-sign   = 'I'.
    ls_user-option = 'CP'.
    ls_user-low    = 'BC*'.
    APPEND ls_user TO ls_context->s_user.

    CASE lv_cmd.
      WHEN 'CMD_01'.
        " Just use declaration in another app
        lv_dynnr = 1020.
        lv_prog  = c_cprog.

      WHEN 'CMD_02'.
        " @see SE37 -> FREE_SELECTIONS_DIALOG
        lv_dynnr = zcl_eui_screen=>mc_dynnr-free_sel.

      WHEN 'CMD_03'.
        " prog name less than 30 symbols (sy-cprog 40 chars)
        " @see CL_CI_QUERY_ATTRIBUTES=>GENERIC
        lv_dynnr = zcl_eui_screen=>mc_dynnr-dyn_popup.
        CONCATENATE c_cprog '_ZZZ1' INTO lv_prog.

      WHEN 'CMD_04'.
        " mc_dynnr-auto_gen in testing
        lv_dynnr = zcl_eui_screen=>mc_dynnr-auto_gen.
        CONCATENATE c_cprog '^9999' INTO lv_prog.

        " Create unqique name for program/ Cannot use GENERATE SUBROUTINE :(
        " For test ->  lv_prog = 'ZZZ_SCREEN' -> follow the instructions
    ENDCASE.

    TRY.
        " Pass params
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr        = lv_dynnr
            iv_cprog        = lv_prog
            iv_status_title = 'Test dynamic screens' " <--- status & title is fixed
            ir_context      = ls_context.            " <--- Set initial values
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

**********************************************************************
    " For pbo
    IF lr_context_main->v_make_required = abap_true.
      lo_screen->customize( name = 'P_BUKRS' required = '1' ).
      lo_screen->customize( name = 'S_USER'  required = '1' ).
      lo_screen->customize( name = 'P_FLD_I' required = '1' ).
    ENDIF.

    " Check gray
    IF lr_context_main->v_make_gray = abap_true.
      lo_screen->customize( name = 'P_LAND1' input = '0' ). " <-- CMD_03!
    ENDIF.

    " Fill listbox runtime
    CLEAR lt_listbox.
    IF lr_context_main->v_make_lsitbox = abap_true.
      " Get all mandt
      CLEAR lt_t000.
      SELECT mandt mtext INTO TABLE lt_t000
      FROM t000.

      " Fill listbox
      LOOP AT lt_t000 REFERENCE INTO lr_t000.
        APPEND INITIAL LINE TO lt_listbox REFERENCE INTO lr_listbox.
        lr_listbox->key  = lr_t000->mandt.
        lr_listbox->text = lr_t000->mtext.
      ENDLOOP.
    ENDIF.

    " Set listbox
    lo_screen->customize( name = 'P_MANDT' required   = '1'
                                           iv_label   = 'Client number'
                                           it_listbox = lt_listbox ).

    lo_screen->customize( name = 'P_CHECK' iv_label = 'Checkbox' ).
    lo_screen->customize( name = 'P_BDC_M' iv_label = 'BDC mode' ).

    " As popup
    lo_screen->popup( iv_col_end = 118 ).

    " If pressed OK
    CHECK lo_screen->show( ) = 'OK'.
    CONCATENATE `Bukrs value = ` ls_context->p_bukrs INTO lv_message.
    MESSAGE lv_message TYPE 'I'.
  ENDWHILE.
ENDMETHOD.
ENDCLASS.
