*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zeui_test_screen_00.


**********************************************************************
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK bl_grp WITH FRAME.
PARAMETERS:
  p_radio1 RADIOBUTTON GROUP radg USER-COMMAND for_pbo DEFAULT 'X',
  p_01_req TYPE werks_d MODIF ID gr1,
  p_02     TYPE werks_d MODIF ID gr1,

  p_radio2 RADIOBUTTON GROUP radg,
  p_03     TYPE werks_d MODIF ID gr2,
  p_04_req TYPE werks_d MODIF ID gr2.
SELECTION-SCREEN END OF BLOCK bl_grp.


**********************************************************************
**********************************************************************
CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      pbo.

ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_main IMPLEMENTATION.
  METHOD pbo.
    DATA lo_screen  TYPE REF TO zcl_eui_screen.
    DATA lv_group1  TYPE screen-group1.
    DATA lo_error   TYPE REF TO zcx_eui_exception.
    TRY.
        CREATE OBJECT lo_screen
          EXPORTING
            iv_dynnr = '1000'.
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    " Mask 1
    lo_screen->customize(
       name      = '*P_0*'
       input     = '0'
       active    = '0'
       invisible = '1' ).

    " Make recomended by mask
    lo_screen->customize(
      name      = 'P_*_REQ'
      required  = '2' ).

    " Visible by SCREEN-GROUP1
    CASE abap_true.
      WHEN p_radio1.
        lv_group1 = 'GR1'.

      WHEN p_radio2.
        lv_group1 = 'GR2'.
    ENDCASE.

    lo_screen->customize(
      group1    = lv_group1 " COND #( WHEN p_radio1 = abap_true THEN 'GR1' ELSE 'GR2' )
      input     = '1'
      active    = '1'
      invisible = '0' ).

    " Start of group (could be standard parameter in LDB screen)
    lo_screen->customize( name = '+' input = '0').
    " Copy params from +
    lo_screen->customize( name = '+P_02').
    lo_screen->customize( name = '+P_03').

    lo_screen->pbo( ).
  ENDMETHOD.

ENDCLASS.


**********************************************************************
**********************************************************************
AT SELECTION-SCREEN OUTPUT.
  lcl_main=>pbo( ).
