FUNCTION zfm_eui_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  " Do not show errors
  CHECK lcl_stack=>mt_stack IS NOT INITIAL.

  " Just call EVENT
  lcl_stack=>pbo_0100( iv_create_cont = abap_false ).
ENDFUNCTION.
