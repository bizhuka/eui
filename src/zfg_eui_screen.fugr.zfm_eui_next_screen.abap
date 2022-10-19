FUNCTION zfm_eui_next_screen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_SCR_MANAGER) TYPE REF TO  ZIF_EUI_MANAGER
*"     REFERENCE(IV_IS_TOP) TYPE  ABAP_BOOL OPTIONAL
*"----------------------------------------------------------------------

  " ### NO_CALL
  IF io_scr_manager->ms_screen-dynnr = zcl_eui_screen=>mc_dynnr-free_sel OR iv_is_top = abap_true.
    go_top_manager = io_scr_manager.
    RETURN.
  ENDIF.

  " Move to next screen
  lcl_stack=>push_stack( io_manager   = io_scr_manager
                         "iv_read_only = iv_read_only
                         ).
ENDFUNCTION.
