class ZCL_EUI_MSG_MANAGER definition
  public
  inheriting from ZCL_EUI_MANAGER
  create public .

public section.
  type-pools ABAP .

  methods SKIP_MESSAGE
    importing
      !IV_MSGID type SYMSGID
      !IV_MSGNO type SYMSGNO
      !IV_MSGTY type SYMSGTY optional .
protected section.

  types:
    BEGIN OF ts_skip_msg,
      msgid TYPE symsgid,
      msgno TYPE symsgno,
      msgty TYPE symsgty,
    END OF ts_skip_msg .
  types:
    tt_skip_msg TYPE SORTED TABLE OF ts_skip_msg WITH UNIQUE KEY table_line .

  data MT_SKIP_MSG type TT_SKIP_MSG .

  methods IS_SKIPPED
    importing
      !IV_MSGID type SYMSGID
      !IV_MSGNO type SYMSGNO
      !IV_MSGTY type SYMSGTY
    returning
      value(RV_SKIP) type ABAP_BOOL .
private section.
ENDCLASS.



CLASS ZCL_EUI_MSG_MANAGER IMPLEMENTATION.


METHOD is_skipped.
  " Skip by all fields
  READ TABLE mt_skip_msg TRANSPORTING NO FIELDS
   WITH TABLE KEY msgid = iv_msgid
                  msgno = iv_msgno
                  msgty = iv_msgty.
  IF sy-subrc = 0.
    rv_skip = abap_true.
    RETURN.
  ENDIF.

  " Skip by 2 fields
  READ TABLE mt_skip_msg TRANSPORTING NO FIELDS
   WITH TABLE KEY msgid = iv_msgid
                  msgno = iv_msgno
                  msgty = ' '. " Or '*' ?
  IF sy-subrc = 0.
    rv_skip = abap_true.
    RETURN.
  ENDIF.
ENDMETHOD.


METHOD skip_message.
  DATA ls_skip LIKE LINE OF mt_skip_msg.
  ls_skip-msgid = iv_msgid.
  ls_skip-msgno = iv_msgno.
  ls_skip-msgty = iv_msgty.

  INSERT ls_skip INTO TABLE mt_skip_msg.
ENDMETHOD.
ENDCLASS.
