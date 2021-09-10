FUNCTION ZFM_EUI_PAI.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_UCOMM) TYPE  SYUCOMM
*"----------------------------------------------------------------------
  " Do not show errors
  CHECK lcl_stack=>mt_stack IS NOT INITIAL.

  " Just call EVENT
  gv_ok_code = iv_ucomm.
  lcl_stack=>pai_0700( CHANGING cv_cmd = gv_ok_code ).
ENDFUNCTION.
