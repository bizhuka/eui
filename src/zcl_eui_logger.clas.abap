class ZCL_EUI_LOGGER definition
  public
  final
  create public

  global friends ZCL_EUI_EVENT_CALLER .

public section.

  types:
    BEGIN OF ts_msg,
        msgid TYPE sy-msgid,
        msgno TYPE sy-msgno,
        msgty TYPE sy-msgty,
        msgv1 TYPE sy-msgv1,
        msgv2 TYPE sy-msgv2,
        msgv3 TYPE sy-msgv3,
        msgv4 TYPE sy-msgv4,
      END OF ts_msg .
  types:
    tt_unique_msg TYPE SORTED TABLE OF ts_msg WITH UNIQUE KEY table_line .

  constants:
    BEGIN OF mc_msg_types,
        none    TYPE string VALUE '',
        fatal   TYPE string VALUE 'X',
        error   TYPE string VALUE 'AEX',
        warning TYPE string VALUE 'WAEX',
        info    TYPE string VALUE 'IWAEX',
        all     TYPE string VALUE ' SIWAEX',
      END OF mc_msg_types .
  constants:
    BEGIN OF mc_probclass,
        very_high TYPE balprobcl VALUE '1',
        high      TYPE balprobcl VALUE '2',
        medium    TYPE balprobcl VALUE '3',
        low       TYPE balprobcl VALUE '4',
        none      TYPE balprobcl VALUE ' ',
      END OF mc_probclass .
  constants:
    BEGIN OF mc_msgty,
        fatal   TYPE symsgty   VALUE 'X',
        error   TYPE symsgty   VALUE 'E',
        cancel  TYPE symsgty   VALUE 'A',
        warning TYPE symsgty   VALUE 'W',
        info    TYPE symsgty   VALUE 'I',
        debug   TYPE symsgty   VALUE 'S',
        none    TYPE symsgty   VALUE ' ',
      END OF mc_msgty .
  constants:
    BEGIN OF mc_profile,
        default  TYPE funcname VALUE 'BAL_DSP_PROFILE_SINGLE_LOG_GET',
        popup    TYPE funcname VALUE 'BAL_DSP_PROFILE_POPUP_GET',
        no_tree  TYPE funcname VALUE 'BAL_DSP_PROFILE_NO_TREE_GET',
        detlevel TYPE funcname VALUE 'BAL_DSP_PROFILE_DETLEVEL_GET',
        standard TYPE funcname VALUE 'BAL_DSP_PROFILE_STANDARD_GET',
        none     TYPE funcname VALUE '',
      END OF mc_profile .

  methods CONSTRUCTOR
    importing
      !IS_HEADER type BAL_S_LOG optional
      !IV_MSG_TYPES type STRING default MC_MSG_TYPES-ALL
      !IV_UNIQUE type ABAP_BOOL optional .
  methods ADD
    importing
      !IS_MSG type ANY optional
      !IV_MSGTY type SYMSGTY optional
      !IV_PROBCLASS type BALPROBCL default MC_PROBCLASS-LOW
      !IS_CONTEXT type BAL_S_CONT optional
      !IS_PARAMS type BAL_S_PARM optional
      !IV_DETLEVEL type BALLEVEL default '1'
    returning
      value(RO_LOGGER) type ref to ZCL_EUI_LOGGER .
  methods ADD_TEXT
    importing
      !IV_TEXT type CSEQUENCE
      !IV_MSGTY type SYMSGTY optional
      !IV_PROBCLASS type BALPROBCL default MC_PROBCLASS-LOW
      !IS_CONTEXT type BAL_S_CONT optional
      !IS_PARAMS type BAL_S_PARM optional
      !IV_DETLEVEL type BALLEVEL default '1'
    returning
      value(RO_LOGGER) type ref to ZCL_EUI_LOGGER .
  methods ADD_EXCEPTION
    importing
      !IS_EXC type BAL_S_EXC optional
      !IO_EXCEPTION type ref to CX_ROOT optional
      !IV_MSGTY type SYMSGTY optional
      !IV_PROBCLASS type BALPROBCL default MC_PROBCLASS-HIGH
      !IV_DETLEVEL type BALLEVEL default '1'
    returning
      value(RO_LOGGER) type ref to ZCL_EUI_LOGGER .
  methods ADD_BATCH
    importing
      !IT_MSG type ANY TABLE
    returning
      value(RO_LOGGER) type ref to ZCL_EUI_LOGGER .
  methods SAVE
    importing
      !IV_IN_UPDATE_TASK type ABAP_BOOL default ABAP_FALSE
    returning
      value(RO_LOGGER) type ref to ZCL_EUI_LOGGER .
  methods SHOW
    importing
      !IV_PROFILE type FUNCNAME default MC_PROFILE-DEFAULT
      !IS_PROFILE type BAL_S_PROF optional .
  methods SHOW_AS_BUTTON
    importing
      !IV_WRITE_MESSAGE type CSEQUENCE optional
    returning
      value(RO_LOGGER) type ref to ZCL_EUI_LOGGER .
  methods GET_MESSAGES
    importing
      !IV_MSG_TYPES type STRING optional
    returning
      value(RT_MSG) type SFB_T_BAL_S_MSG .
  methods HAS_MESSAGES
    importing
      !IV_MSG_TYPES type STRING
    returning
      value(RV_OK) type ABAP_BOOL .
  methods CLEAR .
protected section.
private section.

  data MS_HEADER type BAL_S_LOG .
  data MV_MSG_TYPES type STRING .
  data MV_HANDLE type BALLOGHNDL .
  data MO_MENU type ref to ZCL_EUI_MENU .
  data MV_UNIQUE type ABAP_BOOL .
  data MT_UNIQUE_MSG type TT_UNIQUE_MSG .

  methods _ON_BUTTON_PRESSED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods _IS_MSG_OK
    importing
      !IV_MSGTY type SYMSGTY
    returning
      value(RV_OK) type ABAP_BOOL .
  methods _CORRESPONDING
    importing
      !IS_MSG type ANY
    returning
      value(RS_MSG) type BAL_S_MSG .
ENDCLASS.



CLASS ZCL_EUI_LOGGER IMPLEMENTATION.


METHOD add.
  " Like autosave
  ro_logger = me. " logger->add( )->save( ).

  DATA ls_msg TYPE bal_s_msg. "LIKE is_msg.
  IF is_msg IS NOT INITIAL.
    ls_msg = _corresponding( is_msg ).
  ELSE.
    ls_msg-msgty     = sy-msgty.
    ls_msg-msgid     = sy-msgid.
    ls_msg-msgno     = sy-msgno.
    ls_msg-msgv1     = sy-msgv1.
    ls_msg-msgv2     = sy-msgv2.
    ls_msg-msgv3     = sy-msgv3.
    ls_msg-msgv4     = sy-msgv4.
    ls_msg-probclass = iv_probclass.
    ls_msg-context   = is_context.
    ls_msg-params    = is_params.
    ls_msg-detlevel  = iv_detlevel.
  ENDIF.

  " Rewrite only if supplied
  IF iv_msgty IS NOT INITIAL.
    ls_msg-msgty = iv_msgty.
  ENDIF.

  CHECK _is_msg_ok( ls_msg-msgty ) = abap_true.

  " Short list of messages
  IF mv_unique = abap_true.
    DATA ls_unique_msg TYPE ts_msg.
    MOVE-CORRESPONDING ls_msg TO ls_unique_msg.
    INSERT ls_unique_msg INTO TABLE mt_unique_msg.
    CHECK sy-subrc = 0.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle     = mv_handle
      i_s_msg          = ls_msg
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD add_batch.
  ro_logger = me. " logger->add_batch( )->show( ).

  FIELD-SYMBOLS <ls_msg> TYPE any.
  LOOP AT it_msg ASSIGNING <ls_msg>.
    add( is_msg = <ls_msg> ).
  ENDLOOP.
ENDMETHOD.


METHOD add_exception.
  " Like autosave
  ro_logger = me. " logger->add_exception( )->save( ).

  DATA ls_exc LIKE is_exc.
  IF is_exc IS NOT INITIAL.
    ls_exc = is_exc.
  ELSE.
    ls_exc-exception = io_exception.
    ls_exc-msgty     = iv_msgty.
    ls_exc-probclass = iv_probclass.
    ls_exc-detlevel  = iv_detlevel.
  ENDIF.

  " Default in code. Not declaration
  IF ls_exc-msgty IS INITIAL.
    ls_exc-msgty = mc_msgty-error.
  ENDIF.

  CHECK _is_msg_ok( ls_exc-msgty ) = abap_true.

  CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
    EXPORTING
      i_log_handle     = mv_handle
      i_s_exc          = ls_exc
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD add_text.
  " Like autosave
  ro_logger = me. " logger->add_text( )->save( ).

  DATA lv_msgty LIKE iv_msgty.

  " Info by default
  lv_msgty  = iv_msgty.
  IF lv_msgty IS INITIAL.
    lv_msgty = mc_msgty-info.
  ENDIF.

  CHECK _is_msg_ok( lv_msgty ) = abap_true.

  DATA lv_text TYPE text255.
  lv_text = iv_text.
  CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_text           = lv_text
      i_log_handle     = mv_handle
      i_msgty          = lv_msgty
      i_probclass      = iv_probclass
      i_s_context      = is_context
      i_s_params       = is_params
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD clear.
  CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
    EXPORTING
      i_log_handle  = mv_handle
    EXCEPTIONS
      log_not_found = 1
      OTHERS        = 2.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD constructor.
  mv_msg_types = iv_msg_types.
  mv_unique    = iv_unique.

  " For CALL FUNCTION 'BAL_DB_SAVE' only
  " Pass like that -> is_header = VALUE #( object, subobject, extnumber
*                       aldate_del = sy-datum + 90
*                       del_before = 'X' " can be deleted before deletion date is reached ?
*                      ).

  ms_header = is_header.

  set_default ms_header-aldate  sy-datum.
  set_default ms_header-altime  sy-uzeit.
  set_default ms_header-aluser  sy-uname.
  set_default ms_header-altcode sy-tcode.
  set_default ms_header-alprog  sy-cprog.
  " Calculated default
  DATA lv_delete_date TYPE d.
  lv_delete_date = ms_header-aldate + 30. " delete after 30 days
  set_default ms_header-aldate_del lv_delete_date.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = ms_header
    IMPORTING
      e_log_handle            = mv_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD get_messages.
  DATA ls_filter TYPE bal_s_mfil.
  DATA ls_msgty  TYPE REF TO bal_s_msty.
  DATA lv_len    TYPE i.
  DATA lv_off    TYPE i.

  " Fill if supplied
  lv_len = strlen( iv_msg_types ).
  DO lv_len TIMES.
    lv_off = sy-index - 1.

    APPEND INITIAL LINE TO ls_filter-msgty REFERENCE INTO ls_msgty.
    ls_msgty->sign   = 'I'.
    ls_msgty->option = 'EQ'.
    ls_msgty->low    = iv_msg_types+lv_off(1).
  ENDDO.

  DATA lt_msg_handle TYPE bal_t_msgh.
  DATA lt_log_handle TYPE bal_t_logh.

  " Try to find any messages
  APPEND mv_handle TO lt_log_handle.
  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_t_log_handle = lt_log_handle
      i_s_msg_filter = ls_filter
    IMPORTING
      e_t_msg_handle = lt_msg_handle
    EXCEPTIONS
      msg_not_found  = 1.
  CHECK sy-subrc = 0.

  FIELD-SYMBOLS <ls_msg_handle> LIKE LINE OF lt_msg_handle.
  LOOP AT lt_msg_handle ASSIGNING <ls_msg_handle>.
    DATA ls_msg LIKE LINE OF rt_msg.
    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = <ls_msg_handle>
      IMPORTING
        e_s_msg        = ls_msg
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.
    CHECK sy-subrc = 0.
    APPEND ls_msg TO rt_msg.
  ENDLOOP.
ENDMETHOD.


METHOD has_messages.
  DATA lt_msg TYPE sfb_t_bal_s_msg.
  lt_msg = get_messages( iv_msg_types ).
  LOOP AT lt_msg TRANSPORTING NO FIELDS WHERE msgty CA iv_msg_types.
    rv_ok = abap_true.
    RETURN.
  ENDLOOP.
ENDMETHOD.


METHOD save.
  ro_logger = me. " logger->save( )->show( ).

  DATA lt_log_handle TYPE bal_t_logh.
  APPEND mv_handle TO lt_log_handle.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = lt_log_handle
      i_in_update_task = iv_in_update_task
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD show.
  DATA ls_profile LIKE is_profile.

  IF iv_profile IS NOT INITIAL.
    CALL FUNCTION iv_profile
      IMPORTING
        e_s_display_profile = ls_profile.
  ENDIF.

  " Merge 2 profiles
  " Pass like that -> is_profile = VALUE #( TITLE = 'Text' USE_GRID = 'X' NO_TOOLBAR = 'X' ).
  IF is_profile IS NOT INITIAL.
    zcl_eui_conv=>move_corresponding(
     EXPORTING
       is_source         = is_profile
       iv_except_initial = abap_true    " <--- Move-corresponding except initial
     CHANGING
       cs_destination    = ls_profile ).
  ENDIF.

  DATA lt_log_handle TYPE bal_t_logh.
  APPEND mv_handle TO lt_log_handle.
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile  = ls_profile
      i_t_log_handle       = lt_log_handle
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD show_as_button.
  ro_logger = me. " logger->show_as_button( )->show( ).

  " No logs at all
  CHECK get_messages( ) IS NOT INITIAL
     OR mo_menu IS NOT INITIAL.

  DATA lt_menu TYPE zcl_eui_menu=>tt_menu.
  DATA ls_menu TYPE REF TO zcl_eui_menu=>ts_menu.

  APPEND INITIAL LINE TO lt_menu REFERENCE INTO ls_menu.
  ls_menu->function = 'SHOW_MESSAGES'.
  IF has_messages( mc_msg_types-error ) = abap_true.
    ls_menu->icon = icon_error_protocol.
  ELSE.
    ls_menu->icon = icon_protocol.
  ENDIF.

  IF mo_menu IS INITIAL.
    CREATE OBJECT mo_menu
      EXPORTING
        io_handler = me.
  ENDIF.
  mo_menu->create_toolbar( lt_menu ).

  " New screen
  CHECK iv_write_message IS NOT INITIAL.

  " Skip UT
  DATA lt_callstack TYPE abap_callstack.
  CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack = lt_callstack.
  READ TABLE lt_callstack TRANSPORTING NO FIELDS
   WITH KEY blocktype = 'METHOD' blockname = 'INVOKE_TEST_METHOD' flag_system = 'X'.
  CHECK sy-subrc <> 0.

  IF ls_menu->icon = icon_error_protocol.
    WRITE / iv_write_message COLOR COL_NEGATIVE.
  ELSE.
    WRITE / iv_write_message COLOR COL_KEY.
  ENDIF.
ENDMETHOD.


METHOD _corresponding.
  " Majority of structures
  MOVE-CORRESPONDING is_msg TO rs_msg.

  " Second attempt
  CHECK rs_msg-msgno IS INITIAL.

  TYPES:
    BEGIN OF ts_minority,
      type       TYPE sy-msgty,
      id         TYPE sy-msgid,
      number     TYPE sy-msgno,
      message_v1 TYPE sy-msgv1,
      message_v2 TYPE sy-msgv2,
      message_v3 TYPE sy-msgv3,
      message_v4 TYPE sy-msgv4,

      " Only BDCMSGCOLL
      msgtyp     TYPE sy-msgty,
      msgnr      TYPE sy-msgno,
    END OF ts_minority.

  " Next attempt
  DATA ls_minority TYPE ts_minority.
  MOVE-CORRESPONDING is_msg TO ls_minority.

  " Only BDCMSGCOLL
  IF ls_minority-msgtyp IS NOT INITIAL.
    set_default rs_msg-msgty ls_minority-msgtyp.
    set_default rs_msg-msgno ls_minority-msgnr.
    RETURN.
  ENDIF.

  " Last case
  rs_msg-msgty = ls_minority-type.
  rs_msg-msgid = ls_minority-id.
  rs_msg-msgno = ls_minority-number.
  rs_msg-msgv1 = ls_minority-message_v1.
  rs_msg-msgv2 = ls_minority-message_v2.
  rs_msg-msgv3 = ls_minority-message_v3.
  rs_msg-msgv4 = ls_minority-message_v4.
ENDMETHOD.


METHOD _is_msg_ok.
  " Is message type in list ?
  CHECK iv_msgty CA mv_msg_types.
  rv_ok = abap_true.
ENDMETHOD.


METHOD _on_button_pressed.
  CHECK fcode = 'SHOW_MESSAGES'.
  DATA ls_profile TYPE bal_s_prof.

  " Prepare title
  DO 1 TIMES.
    CHECK ms_header-object IS NOT INITIAL AND ms_header-subobject IS NOT INITIAL.

    DATA lv_objtxt TYPE balobjt-objtxt.
    SELECT SINGLE objtxt INTO lv_objtxt
    FROM balobjt
    WHERE spras  = sy-langu
      AND object = ms_header-object.
    CHECK sy-subrc = 0.

    DATA lv_subobjtxt TYPE balsubt-subobjtxt.
    SELECT SINGLE subobjtxt INTO lv_subobjtxt
    FROM balsubt
    WHERE spras     = sy-langu
      AND object    = ms_header-object
      AND subobject = ms_header-subobject.
    CHECK sy-subrc = 0.

    CONCATENATE lv_objtxt ` - ` lv_subobjtxt INTO ls_profile-title.
  ENDDO.

  ls_profile-start_col = ls_profile-start_row = 1.
  show( iv_profile = mc_profile-popup
        " Set additional values
        is_profile = ls_profile ).
ENDMETHOD.
ENDCLASS.
