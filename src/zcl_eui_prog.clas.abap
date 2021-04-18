class ZCL_EUI_PROG definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  data MV_CPROG type SYCPROG read-only .

  methods CONSTRUCTOR
    importing
      !IV_CPROG type SYCPROG optional .
  class-methods GENERATE
    importing
      !IT_CODE type STRINGTAB
      !IV_CPROG type SYCPROG
    returning
      value(RV_PROG) type SYCPROG .
  methods GET_ATTRIBUTE
    importing
      !IV_KEY type CSEQUENCE
    returning
      value(RV_VALUE) type STRING .
protected section.
private section.

  types:
    BEGIN OF ts_hash,
      hash TYPE hash160,
      prog TYPE programm,
    END OF ts_hash .
  types:
    tt_hash TYPE SORTED TABLE OF ts_hash WITH UNIQUE KEY hash .
  types:
    BEGIN OF ts_attribute,
      key TYPE string,
      val TYPE string,
    END OF ts_attribute .
  types:
    tt_attribute TYPE SORTED TABLE OF ts_attribute WITH UNIQUE KEY key .

  data MT_ATTRIBUTE type TT_ATTRIBUTE .
  class-data MT_HASH type TT_HASH .

  class-methods _CALC_HASH
    importing
      !IT_CODE type STRINGTAB
    returning
      value(RV_HASH) type HASH160 .
  class-methods _SAVE_PROG
    importing
      !IT_CODE type STRINGTAB
      !IV_CPROG type SYCPROG .
  class-methods _GENERATE_SUBROUTINE
    importing
      !IT_CODE type STRINGTAB
    returning
      value(RV_PROG) type SYCPROG .
ENDCLASS.



CLASS ZCL_EUI_PROG IMPLEMENTATION.


METHOD constructor.
  CHECK iv_cprog IS NOT INITIAL.

  mv_cprog = iv_cprog.

  " Name that cannot be created in SE38
  IF mv_cprog NS `%`.
    CONCATENATE mv_cprog(1) `%` mv_cprog+1 INTO mv_cprog.
  ENDIF.

  DATA lt_code TYPE stringtab.
  READ REPORT mv_cprog INTO lt_code.
  CHECK sy-subrc = 0.

  " Tech info in the firts line
  DATA: lv_line TYPE string, lt_line TYPE stringtab.
  READ TABLE lt_code INTO lv_line INDEX 1.
  SPLIT lv_line+1 AT `,` INTO TABLE lt_line.

  DATA ls_attribute LIKE LINE OF mt_attribute.
  LOOP AT lt_line INTO lv_line.
    SPLIT lv_line AT `=` INTO ls_attribute-key
                              ls_attribute-val.
    INSERT ls_attribute INTO TABLE mt_attribute.
  ENDLOOP.

  CHECK get_attribute( `EUI` ) IS INITIAL.
  zcx_eui_no_check=>raise_sys_error( iv_message = `The app is not auto-generated!` ).
ENDMETHOD.


METHOD generate.
  " Already created ?
  DATA ls_hash TYPE ts_hash.
  ls_hash-hash = _calc_hash( it_code ).

  READ TABLE mt_hash INTO ls_hash
   WITH TABLE KEY hash = ls_hash-hash.
  IF sy-subrc = 0.
    rv_prog = ls_hash-prog.
    RETURN.
  ENDIF.

  " Permanently in in SE38!
  IF iv_cprog IS NOT INITIAL.
    ls_hash-prog = iv_cprog.
    _save_prog( it_code  = it_code
                iv_cprog = ls_hash-prog ).
  ELSE.
    ls_hash-prog = _generate_subroutine( it_code ).
  ENDIF.

  rv_prog = ls_hash-prog.
  INSERT ls_hash INTO TABLE mt_hash.
ENDMETHOD.


METHOD get_attribute.
  FIELD-SYMBOLS <ls_attribute> LIKE LINE OF mt_attribute.
  READ TABLE mt_attribute ASSIGNING <ls_attribute>
   WITH TABLE KEY key = iv_key.
  CHECK sy-subrc = 0.

  rv_value = <ls_attribute>-val.
ENDMETHOD.


METHOD _calc_hash.
  DATA lv_line TYPE string.
  CONCATENATE LINES OF it_code INTO lv_line.
  CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
    EXPORTING
      data   = lv_line
    IMPORTING
      hash   = rv_hash
    EXCEPTIONS
      OTHERS = 1.

  CHECK sy-subrc <> 0.
  zcx_eui_no_check=>raise_sys_error( ).
ENDMETHOD.


METHOD _generate_subroutine.
  DATA lv_message TYPE string.
  DATA lv_pos     TYPE i.
  DATA lv_word    TYPE string.
  GENERATE SUBROUTINE POOL it_code NAME rv_prog
    MESSAGE lv_message
    LINE    lv_pos
    WORD    lv_word.                "#EC CI_GENERATE. <--- in lt_code[]
  CHECK sy-subrc <> 0.

  " Ooops! wrong syntax in MT_ABAP_CODE!
  zcx_eui_no_check=>raise_sys_error( iv_message = lv_message ).
ENDMETHOD.


METHOD _save_prog.
  " Saving directly is prohibited
  DATA lt_saver TYPE stringtab.

  APPEND `REPORT SAVER.`                            TO lt_saver.
  APPEND ``                                         TO lt_saver.
  APPEND `FORM START USING it_code  TYPE stringtab` TO lt_saver.
  APPEND `                 iv_cprog TYPE sycprog.`  TO lt_saver.
  APPEND `  INSERT REPORT iv_cprog FROM it_code.`   TO lt_saver.
  APPEND `  DATA lv_message TYPE string.`           TO lt_saver.
  APPEND `  DATA lv_pos     TYPE i.`                TO lt_saver.
  APPEND `  DATA lv_word    TYPE string.`           TO lt_saver.
  APPEND `  GENERATE REPORT iv_cprog`               TO lt_saver.
  APPEND `    MESSAGE lv_message`                   TO lt_saver.
  APPEND `    LINE    lv_pos`                       TO lt_saver.
  APPEND `    WORD    lv_word.`                     TO lt_saver.
  APPEND `  CHECK sy-subrc <> 0.`                   TO lt_saver.
  APPEND `  ZCX_EUI_NO_CHECK=>RAISE_SYS_ERROR( iv_message = lv_message ).`  TO lt_saver.
  APPEND `ENDFORM.`                                 TO lt_saver.

  DATA lv_saver TYPE string.
  lv_saver = generate( it_code  = lt_saver
                       iv_cprog = `` ).

  PERFORM start IN PROGRAM (lv_saver) " IF FOUND
    USING it_code
          iv_cprog.
ENDMETHOD.
ENDCLASS.
