FUNCTION zfm_eui_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_UCOMM) TYPE  SYUCOMM
*"----------------------------------------------------------------------

  IF lcl_stack=>mt_stack[] IS INITIAL AND go_top_manager IS NOT INITIAL.
    DATA lv_close TYPE abap_bool.
    go_top_manager->pai( EXPORTING iv_command = iv_ucomm
                         CHANGING  cv_close   = lv_close ).
    RETURN.
  ENDIF.

  " Do not show errors
  CHECK lcl_stack=>mt_stack IS NOT INITIAL.

  " Just call EVENT
  gv_ok_code = iv_ucomm.
  lcl_stack=>pai_0700( EXPORTING iv_silent = abap_true " TODO Pass as PARAM ???
                       CHANGING  cv_cmd    = gv_ok_code ).
ENDFUNCTION.
