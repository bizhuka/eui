FUNCTION zfm_eui_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------

  IF lcl_stack=>mt_stack[] IS INITIAL AND go_top_manager IS NOT INITIAL.
    go_top_manager->pbo( ).
    RETURN.
  ENDIF.

  " Do not show errors
  CHECK lcl_stack=>mt_stack IS NOT INITIAL.

  " Just call EVENT
  lcl_stack=>pbo_0100( iv_create_cont = abap_false ).
ENDFUNCTION.
