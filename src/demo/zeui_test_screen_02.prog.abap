*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zeui_test_screen_02.

TABLES: usr02.


**********************************************************************
" CMD_01 -> 01-Statically decl. screen
**********************************************************************

SELECTION-SCREEN BEGIN OF SCREEN 1020 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bl_1020 WITH FRAME TITLE TEXT-tit.
PARAMETERS:
  p_bukrs TYPE bukrs,
  p_bdc_m TYPE ettcd-mode AS LISTBOX VISIBLE LENGTH 50,
  p_mandt TYPE t001-mandt AS LISTBOX VISIBLE LENGTH 50,
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
" CMD_04 -> 04-Dynamically decl. screen
**********************************************************************

SELECTION-SCREEN BEGIN OF SCREEN 9999 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bl_main WITH FRAME TITLE s_title.
PARAMETERS p_01 TYPE tcury-bukrs.
PARAMETERS p_02 TYPE ettcd-mode AS LISTBOX VISIBLE LENGTH 50.
PARAMETERS p_03 TYPE t001-mandt AS LISTBOX VISIBLE LENGTH 50.
PARAMETERS p_04 AS CHECKBOX.
SELECT-OPTIONS p_05 FOR usr02-bname. " <--- Change SH
PARAMETERS p_06 TYPE t005t-land1.
PARAMETERS p_07 TYPE syst-index.
PARAMETERS p_08 TYPE syst-tabix.

SELECTION-SCREEN END OF BLOCK bl_main.
SELECTION-SCREEN END OF SCREEN 9999.



**********************************************************************
**********************************************************************

INITIALIZATION.
  zcl_eui_test_screen_02=>show_initial_screen( ).

AT SELECTION-SCREEN OUTPUT.
  " For 01 & 04 only
  IF sy-dynnr = '1020' OR sy-dynnr = '9999'.
    CALL FUNCTION 'ZFM_EUI_PBO'.
  ENDIF.
