class ZCL_EUI_MANAGER definition
  public
  create public .

public section.
  type-pools ABAP .

  interfaces ZIF_EUI_MANAGER .

  aliases MC_CMD
    for ZIF_EUI_MANAGER~MC_CMD .
  aliases MS_POPUP
    for ZIF_EUI_MANAGER~MS_POPUP .
  aliases MS_SCREEN
    for ZIF_EUI_MANAGER~MS_SCREEN .
  aliases MS_STATUS
    for ZIF_EUI_MANAGER~MS_STATUS .
  aliases PAI
    for ZIF_EUI_MANAGER~PAI .
  aliases PBO
    for ZIF_EUI_MANAGER~PBO .
  aliases POPUP
    for ZIF_EUI_MANAGER~POPUP .
  aliases SET_STATUS
    for ZIF_EUI_MANAGER~SET_STATUS .
  aliases SHOW
    for ZIF_EUI_MANAGER~SHOW .
  aliases TS_STATUS
    for ZIF_EUI_MANAGER~TS_STATUS .

  constants MC_EUI_SCREEN_FUGR type SYCPROG value 'SAPLZFG_EUI_SCREEN' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_EDITABLE type ABAP_BOOL .
protected section.

  data MO_EVENT_CALLER type ref to ZCL_EUI_EVENT_CALLER .
  data MV_READ_ONLY type ABAP_BOOL .
  data MV_CLOSE_CMD type SYUCOMM .
private section.
ENDCLASS.



CLASS ZCL_EUI_MANAGER IMPLEMENTATION.


METHOD constructor.
  " Invert xsdbool( <> 'X' )
  IF iv_editable <> abap_true.
    mv_read_only = abap_true.
  ENDIF.

  CREATE OBJECT mo_event_caller.
ENDMETHOD.


METHOD zif_eui_manager~pai.
  DATA lr_close LIKE REF TO cv_close.

  CASE iv_command.
    WHEN mc_cmd-ok OR mc_cmd-return.
      cv_close = abap_true.

    WHEN mc_cmd-cancel.
      cv_close = abap_true.
      IF mv_read_only <> abap_true.
        MESSAGE 'The action was cancelled'(opc) TYPE 'S' DISPLAY LIKE 'W'.
      ENDIF.
  ENDCASE.

  " No need
  IF mo_event_caller IS NOT INITIAL.
    GET REFERENCE OF cv_close INTO lr_close.
    mo_event_caller->call_handlers(
     iv_of_class     = 'ZIF_EUI_MANAGER'
     iv_for_event    = 'PAI_EVENT'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = me
     iv_param_nam_01 = 'IV_COMMAND'      iv_param_val_01 = iv_command
     iv_param_nam_02 = 'CV_CLOSE'        iv_param_val_02 = lr_close ).
  ENDIF.

  IF cv_close = abap_true.
    mv_close_cmd = iv_command.
  ENDIF.
ENDMETHOD.


METHOD zif_eui_manager~pbo.
  " No need to update
  IF ms_status-is_fixed <> abap_true AND iv_set_status = abap_true.
    CLEAR ms_status.
  ENDIF.

  IF mo_event_caller IS NOT INITIAL.
    mo_event_caller->call_handlers(
     iv_of_class     = 'ZIF_EUI_MANAGER'
     iv_for_event    = 'PBO_EVENT'
     iv_param_nam_00 = 'SENDER'            iv_param_val_00 = me
     iv_param_nam_01 = 'IO_CONTAINER'      iv_param_val_01 = io_container ).
  ENDIF.

**********************************************************************
  " Set PF-STATUS
  IF ms_status-name IS INITIAL AND ms_status-prog IS INITIAL.
    ms_status-prog = mc_eui_screen_fugr.

    IF ms_popup-col_beg IS INITIAL.
      " Full screen
      ms_status-name = 'OK_CANCEL_FULL_SCR'.
    ELSE.
      " Popup
      ms_status-name = 'OK_CANCEL_POPUP'.
    ENDIF.
  ENDIF.

  " Edit or read only
  IF ms_status-prog = mc_eui_screen_fugr.
    CASE ms_status-name.
      WHEN 'OK_CANCEL_POPUP'.
        IF mv_read_only = abap_true.
          APPEND mc_cmd-ok     TO ms_status-exclude.
        ENDIF.

      WHEN 'OK_CANCEL_FULL_SCR'.
        IF mv_read_only = abap_true.
          APPEND mc_cmd-ok     TO ms_status-exclude.
        ELSE.
          APPEND mc_cmd-return TO ms_status-exclude.
        ENDIF.
    ENDCASE.
  ENDIF.

  " Just for speed
  SORT ms_status-exclude.
  DELETE ADJACENT DUPLICATES FROM ms_status-exclude.

  " And set
  CHECK iv_set_status = abap_true.
  SET PF-STATUS ms_status-name EXCLUDING ms_status-exclude OF PROGRAM ms_status-prog.

  " Set title bar
  IF ms_status-title IS INITIAL.
    SELECT SINGLE text INTO ms_status-title
    FROM trdirt
    WHERE name  = sy-cprog
      AND sprsl = sy-langu.
  ENDIF.

  CHECK ms_status-title IS NOT INITIAL.
  SET TITLEBAR 'TITLE_100' OF PROGRAM mc_eui_screen_fugr WITH ms_status-title.
ENDMETHOD.


METHOD zif_eui_manager~popup.
  DATA ls_popup LIKE ms_popup.

  ls_popup-col_beg  = iv_col_beg.
  ls_popup-col_end  = iv_col_end.
  ls_popup-row_beg  = iv_row_beg.
  ls_popup-row_end  = iv_row_end.
  ls_popup-no_shift = iv_no_shift.

  IF ls_popup-col_beg IS INITIAL.
    ls_popup-col_beg = 1.
  ENDIF.

  IF ls_popup-col_end IS INITIAL.
    ls_popup-col_end = 150.
  ENDIF.

  IF ls_popup-row_beg IS INITIAL.
    ls_popup-row_beg = 1.
  ENDIF.

  IF ls_popup-row_end IS INITIAL.
    ls_popup-row_end = 30.
  ENDIF.

  " And set
  ms_popup = ls_popup.

  " For chains
  ro_manager = me.
ENDMETHOD.


METHOD zif_eui_manager~set_status.
  ms_status = is_status.
  IF ms_status IS NOT INITIAL.
    ms_status-is_fixed = abap_true.
  ENDIF.

  ro_manager = me.
ENDMETHOD.


METHOD zif_eui_manager~show.
  DATA lo_error   TYPE REF TO zcx_eui_exception.

  IF io_handler IS NOT INITIAL.
    TRY.
        mo_event_caller->add_handler(
            io_handler      = io_handler
            iv_handlers_map = iv_handlers_map ).
      CATCH zcx_eui_exception INTO lo_error.
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDIF.

  " Just call nect screen
  CALL FUNCTION 'ZFM_EUI_NEXT_SCREEN'
    EXPORTING
      io_scr_manager = me
      iv_read_only   = me->mv_read_only.

  " return as a result
  rv_close_cmd = mv_close_cmd.
ENDMETHOD.
ENDCLASS.
