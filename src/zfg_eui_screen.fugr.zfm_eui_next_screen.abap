FUNCTION zfm_eui_next_screen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IO_SCR_MANAGER) TYPE REF TO  ZIF_EUI_MANAGER
*"     REFERENCE(IV_READ_ONLY) TYPE  ABAP_BOOL OPTIONAL
*"----------------------------------------------------------------------

  " Move to next screen
  lcl_stack=>push_stack( io_manager   = io_scr_manager
                         iv_read_only = iv_read_only ).
ENDFUNCTION.
