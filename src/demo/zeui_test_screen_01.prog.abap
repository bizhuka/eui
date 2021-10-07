*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zeui_test_screen_01.

TYPE-POOLS:
 abap.

" Screen 1000
PARAMETERS:
  p_01_gry AS CHECKBOX DEFAULT ' ',
  p_02_obl AS CHECKBOX DEFAULT 'X'.

" Screen 1010
SELECTION-SCREEN BEGIN OF SCREEN 1010 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK bl_1010. "WITH FRAME TITLE TEXT-tit.
PARAMETERS : p_fld_01 TYPE sytabix,
             p_fld_02 TYPE sytabix.
SELECTION-SCREEN END OF BLOCK bl_1010.
SELECTION-SCREEN END OF SCREEN 1010.

**********************************************************************
**********************************************************************
CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      " If SCREEN in another program pass values
      BEGIN OF ts_context,
        p_fld_01 LIKE p_fld_01,
        p_fld_02 LIKE p_fld_02,
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
    DATA lo_error   TYPE REF TO zcx_eui_exception.

**********************************************************************
    " Fill context
    " p_fld_01 = 777 is of course easy, but if you fill another program screen
    CREATE DATA ls_context.
    ls_context->p_fld_01 = 777.

    TRY.
        " Pass params
        CREATE OBJECT mo_screen
          EXPORTING
            iv_dynnr   = '1010'      " Only for that DYNNR
            ir_context = ls_context. " <--- Set initial values
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
**********************************************************************
    " For pbo
    IF p_01_gry = abap_true.
      mo_screen->customize( name = 'P_FLD_01' input = '0' ).
    ENDIF.

    IF p_02_obl = abap_true.
      mo_screen->customize( name = 'P_FLD_02' required = '1' ).
    ENDIF.

    " As popup
    DATA lv_col_end TYPE i.
    mo_screen->get_dimension( IMPORTING ev_col_end = lv_col_end ).
    mo_screen->popup( iv_col_end = lv_col_end ).

    " If pressed OK
    IF mo_screen->show( io_handler = me ) = 'OK'.
      MESSAGE 'OK is pressed' TYPE 'I'.
    ELSE.
      MESSAGE 'The action is cancelled' TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.

  METHOD pbo.
    " LOOP AT SCREEN
    IF sy-dynnr = '1010'.
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
