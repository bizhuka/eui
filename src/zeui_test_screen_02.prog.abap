*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zeui_test_screen_02.

" Screen 1000
PARAMETERS:
  p_use_po RADIOBUTTON GROUP gr1,
  p_use_fr RADIOBUTTON GROUP gr1,
  p_use_sc RADIOBUTTON GROUP gr1,
  p_use_au RADIOBUTTON GROUP gr1.
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  p_01_obl AS CHECKBOX DEFAULT 'X'.

**********************************************************************
" p_use_sc = 'X' Staticly declared screen 1020
**********************************************************************
TABLES: usr02.
SELECTION-SCREEN BEGIN OF SCREEN 1020 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bl_1020 WITH FRAME TITLE text-tit.
PARAMETERS:
  p_bukrs TYPE bukrs,
  p_bdc_m TYPE ettcd-mode AS LISTBOX VISIBLE LENGTH 50,
  p_check AS CHECKBOX.
SELECT-OPTIONS:
  s_user FOR usr02-bname.
PARAMETERS:
  p_land1  TYPE t005t-land1,
  p_fld_i  TYPE syindex,
  p_fld_i2 TYPE sytabix.
SELECTION-SCREEN END OF BLOCK bl_1020.
SELECTION-SCREEN END OF SCREEN 1020.

**********************************************************************
**********************************************************************
CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      " If SCREEN in another program pass values
      BEGIN OF ts_context,
        p_bukrs  TYPE bukrs,
        p_bdc_m  TYPE ettcd_mode,
        p_check  TYPE xsdboolean,
        s_user   TYPE offline_log_user_itab, " Range
        p_land1  TYPE t005t-land1,
        p_fld_i  TYPE syindex,     " do not use i! use from dictionary
        p_fld_i2 TYPE sytabix,     " do not use i! use from dictionary
        " p_memo     TYPE stringval,   " String Ok, tables in development
      END OF ts_context.

    DATA:
      mo_screen TYPE REF TO zcl_eui_screen.

    METHODS:
      start_of_selection,

      pbo.
ENDCLASS.

**********************************************************************
**********************************************************************
DATA:
  go_main  TYPE REF TO lcl_main.

**********************************************************************
**********************************************************************
CLASS lcl_main IMPLEMENTATION.
  METHOD start_of_selection.
    DATA ls_context TYPE REF TO ts_context.
    DATA lt_scr     TYPE zcl_eui_screen=>tt_screen.
    DATA ls_scr     TYPE REF TO screen.
    DATA lo_error   TYPE REF TO zcx_eui_exception.
    DATA ls_user    LIKE LINE OF ls_context->s_user.
    DATA lv_message TYPE string.
    DATA lv_dynnr   TYPE sydynnr.
    DATA lv_prog    TYPE sycprog.
    FIELD-SYMBOLS   <lv_text> TYPE string.
**********************************************************************
    " Fill context
    CREATE DATA ls_context.
    ls_context->p_bukrs    = '1000'.
    ls_context->p_bdc_m    = 'A'.
    ls_context->p_check    = abap_true.

    " Dfault value for SELECT-OPTION
    ls_user-sign   = 'I'.
    ls_user-option = 'CP'.
    ls_user-low    = 'BC*'.
    APPEND ls_user TO ls_context->s_user.

    CASE abap_true.
      WHEN p_use_fr.
        lv_dynnr = zcl_eui_screen=>mc_dynnr-free_sel.

      WHEN p_use_au.
        " mc_dynnr-auto_gen in testing
        " Create unqique name for program/ Cannot use GENERATE SUBROUTINE :(
        lv_dynnr = zcl_eui_screen=>mc_dynnr-auto_gen.
        CONCATENATE sy-cprog '_1000' INTO lv_prog.

      WHEN p_use_po.
        " prog name less than 30 symbols (sy-cprog 40 chars)
        lv_dynnr = zcl_eui_screen=>mc_dynnr-dyn_popup.
        CONCATENATE sy-cprog '_1000' INTO lv_prog.

      WHEN p_use_sc.
        " For OOP classes you could use external programs
        lv_dynnr = 1020.
        lv_prog  = sy-cprog.
    ENDCASE.

    TRY.
        " Pass params
        CREATE OBJECT mo_screen
          EXPORTING
            iv_dynnr   = lv_dynnr
            iv_cprog   = lv_prog
            ir_context = ls_context. " <--- Set initial values
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

**********************************************************************
    " For pbo
    IF p_01_obl = abap_true.
      mo_screen->customize( iv_fieldname = 'P_BUKRS' required = '1' ).
      mo_screen->customize( iv_fieldname = 'S_USER'  required = '1' ).
      mo_screen->customize( iv_fieldname = 'P_FLD_I' required = '1' ).
    ENDIF.

    mo_screen->customize( iv_fieldname = 'P_CHECK' iv_label = 'Checkbox' ).
    mo_screen->customize( iv_fieldname = 'P_BDC_M' iv_label = 'BDC mode' ).

    " Avoid error
    ASSIGN COMPONENT 'P_MEMO' OF STRUCTURE ls_context->* TO <lv_text>.
    IF <lv_text> IS ASSIGNED.
      mo_screen->customize( iv_fieldname = 'P_MEMO' iv_label = 'Test edit text' ).
    ENDIF.

    mo_screen->popup( iv_col_end = 118 ).

    " Do not clear status & title
    mo_screen->ms_status-title    = 'Test dynamic screens'.
    mo_screen->ms_status-is_fixed = abap_true.

    " If pressed OK
    IF mo_screen->show( io_handler = me ) = 'OK'.
      CONCATENATE `Bukrs value = ` ls_context->p_bukrs INTO lv_message.
      MESSAGE lv_message TYPE 'I'.
    ELSE.
      MESSAGE 'The action is cancelled' TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.

  METHOD pbo.
    " LOOP AT SCREEN
    IF sy-dynnr = '1020'.
      mo_screen->pbo( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.


**********************************************************************
**********************************************************************

INITIALIZATION.
  CREATE OBJECT go_main.

START-OF-SELECTION.
  go_main->start_of_selection( ).

AT SELECTION-SCREEN OUTPUT.
  go_main->pbo( ).
