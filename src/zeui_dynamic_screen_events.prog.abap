*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
  zcl_eui_screen=>top_pbo( ).

AT SELECTION-SCREEN.
  " Only push button commands
  IF sy-ucomm CP 'X0*'.
    zcl_eui_screen=>top_pai( iv_ucomm = sy-ucomm ).
  ENDIF.
