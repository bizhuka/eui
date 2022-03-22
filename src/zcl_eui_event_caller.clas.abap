class ZCL_EUI_EVENT_CALLER definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  methods ADD_HANDLER
    importing
      !IO_HANDLER type ref to OBJECT
      !IV_HANDLERS_MAP type CSEQUENCE optional
      !IV_FIRST type ABAP_BOOL default ABAP_FALSE
      !IV_ACTIVATE type ABAP_BOOL default ABAP_TRUE
    raising
      ZCX_EUI_EXCEPTION .
  methods CALL_HANDLERS
    importing
      !IV_OF_CLASS type ABAP_CLASSNAME
      !IV_FOR_EVENT type ABAP_EVNTNAME
      !IV_PARAM_NAM_00 type CSEQUENCE optional
      !IV_PARAM_VAL_00 type ANY optional
      !IV_PARAM_NAM_01 type CSEQUENCE optional
      !IV_PARAM_VAL_01 type ANY optional
      !IV_PARAM_NAM_02 type CSEQUENCE optional
      !IV_PARAM_VAL_02 type ANY optional
      !IV_PARAM_NAM_03 type CSEQUENCE optional
      !IV_PARAM_VAL_03 type ANY optional
      !IV_PARAM_NAM_04 type CSEQUENCE optional
      !IV_PARAM_VAL_04 type ANY optional
      !IV_PARAM_NAM_05 type CSEQUENCE optional
      !IV_PARAM_VAL_05 type ANY optional
      !IV_PARAM_NAM_06 type CSEQUENCE optional
      !IV_PARAM_VAL_06 type ANY optional
      !IV_PARAM_NAM_07 type CSEQUENCE optional
      !IV_PARAM_VAL_07 type ANY optional
      !IV_PARAM_NAM_08 type CSEQUENCE optional
      !IV_PARAM_VAL_08 type ANY optional
      !IV_PARAM_NAM_09 type CSEQUENCE optional
      !IV_PARAM_VAL_09 type ANY optional
      !IV_PARAM_NAM_10 type CSEQUENCE optional
      !IV_PARAM_VAL_10 type ANY optional
      !IV_PARAM_NAM_11 type CSEQUENCE optional
      !IV_PARAM_VAL_11 type ANY optional
      !IV_PARAM_NAM_12 type CSEQUENCE optional
      !IV_PARAM_VAL_12 type ANY optional
      !IV_PARAM_NAM_13 type CSEQUENCE optional
      !IV_PARAM_VAL_13 type ANY optional
      !IV_PARAM_NAM_14 type CSEQUENCE optional
      !IV_PARAM_VAL_14 type ANY optional .
  methods HAS_HANDLER
    importing
      !IV_OF_CLASS type ABAP_CLASSNAME
      !IV_FOR_EVENT type ABAP_EVNTNAME
    returning
      value(RV_OK) type ABAP_BOOL .
protected section.
private section.

  types:
    BEGIN OF TS_LISTENER,
     listener    type ref to object,
     t_methdescr type abap_methdescr_tab,
     t_friend    type abap_frndtypes_tab,
   END OF TS_LISTENER .
  types:
    TT_LISTENER type STANDARD TABLE OF TS_LISTENER WITH DEFAULT KEY .

  data MT_LISTENER type TT_LISTENER .
ENDCLASS.



CLASS ZCL_EUI_EVENT_CALLER IMPLEMENTATION.


METHOD add_handler.
  DATA:
    lt_event        TYPE STANDARD TABLE OF abap_methname,
    lo_object_descr TYPE REF TO cl_abap_classdescr,
    lv_tabix        TYPE sytabix,
    ls_method       TYPE REF TO abap_methdescr,
    ls_item         LIKE LINE OF mt_listener.
  FIELD-SYMBOLS:
     <ls_event> LIKE LINE OF lt_event.

  " No need to call anything
  CHECK io_handler IS NOT INITIAL.

  " Deactivate
  IF iv_activate <> abap_true.
    DELETE mt_listener WHERE listener = io_handler.
    RETURN.
  ENDIF.

  " Already exist ?
  READ TABLE mt_listener TRANSPORTING NO FIELDS
   WITH KEY listener = io_handler.
  CHECK sy-subrc <> 0.

  " Call public methods of ...
  ls_item-listener = io_handler.

  " Get all handler methods
  lo_object_descr ?= cl_abap_objectdescr=>describe_by_object_ref( ls_item-listener ).
  ls_item-t_methdescr = lo_object_descr->methods.
  ls_item-t_friend    = lo_object_descr->get_friend_types( ).
  DELETE ls_item-t_methdescr WHERE for_event IS INITIAL.

  " Exact only specefic methods
  IF iv_handlers_map IS NOT INITIAL.
    SPLIT iv_handlers_map AT ';' INTO TABLE lt_event.

    " Check declared or not
    LOOP AT lt_event ASSIGNING <ls_event>.
      READ TABLE ls_item-t_methdescr TRANSPORTING NO FIELDS
       WITH TABLE KEY name = <ls_event>.
      CHECK sy-subrc <> 0.

      MESSAGE s007(zeui_message) WITH <ls_event> INTO sy-msgli.
      zcx_eui_exception=>raise_sys_error( ).
    ENDLOOP.

    " Could be many similar EVENT handlers
    SORT lt_event BY table_line.
    LOOP AT ls_item-t_methdescr REFERENCE INTO ls_method.
      lv_tabix = sy-tabix.

      " Delete unnecessary
      READ TABLE lt_event TRANSPORTING NO FIELDS BINARY SEARCH
       WITH KEY table_line = ls_method->name.
      CHECK sy-subrc <> 0.

      DELETE ls_item-t_methdescr INDEX lv_tabix.
    ENDLOOP.
  ENDIF.

  " More proiority
  IF iv_first = abap_true.
    INSERT ls_item INTO mt_listener INDEX 1.
  ELSE.
    APPEND ls_item TO mt_listener.
  ENDIF.
ENDMETHOD.


METHOD call_handlers.
  DATA:
    lt_param_input TYPE abap_parmbind_tab,
    lt_param_call  LIKE lt_param_input,
    ls_param       TYPE abap_parmbind,
    lo_error       TYPE REF TO cx_sy_dyn_call_error,
    lv_is_friend   TYPE abap_bool.
  FIELD-SYMBOLS:
    <ls_param>     LIKE LINE OF lt_param_input,
    <ls_methdescr> TYPE abap_methdescr,
    <ls_parameter> LIKE LINE OF <ls_methdescr>-parameters,
    <ls_item>      LIKE LINE OF mt_listener.


**********************************************************************
  DATA          lv_index   TYPE numc2.
  DATA          lv_name    TYPE string.
  FIELD-SYMBOLS <lv_name>  TYPE csequence.
  FIELD-SYMBOLS <lv_value> TYPE any.

  DO 15 TIMES.                                           "#EC NUMBER_OK
    " get next parameter
    lv_index = sy-index - 1.
    CONCATENATE 'IV_PARAM_NAM_' lv_index INTO lv_name.
    ASSIGN (lv_name) TO <lv_name>.

    " Stop no params
    IF <lv_name> IS INITIAL.
      EXIT.
    ENDIF.

    CONCATENATE 'IV_PARAM_VAL_' lv_index INTO lv_name.
    ASSIGN (lv_name) TO <lv_value>.

    ls_param-name = <lv_name>.
    ls_param-kind = cl_abap_objectdescr=>exporting.
    GET REFERENCE OF <lv_value> INTO ls_param-value.

    INSERT ls_param INTO TABLE lt_param_input.
  ENDDO.
**********************************************************************

  LOOP AT mt_listener ASSIGNING <ls_item>.
    " Public or friend
    lv_is_friend = abap_false.
    READ TABLE <ls_item>-t_friend TRANSPORTING NO FIELDS
     WITH KEY table_line->absolute_name = '\CLASS=ZCL_EUI_EVENT_CALLER'.
    IF sy-subrc = 0.
      lv_is_friend = abap_true.
    ENDIF.

    LOOP AT <ls_item>-t_methdescr ASSIGNING <ls_methdescr>
       WHERE of_class  = iv_of_class
         AND for_event = iv_for_event.

      " Make method public!
      IF <ls_methdescr>-visibility <> cl_abap_objectdescr=>public AND lv_is_friend <> abap_true.
        MESSAGE s008(zeui_message) WITH <ls_methdescr>-name INTO sy-msgli.
        zcx_eui_exception=>raise_dump( iv_message = sy-msgli ).
      ENDIF.

      " Fill each time
      CLEAR lt_param_call.
      LOOP AT <ls_methdescr>-parameters ASSIGNING <ls_parameter>.
        " If have decalaration in method
        READ TABLE lt_param_input ASSIGNING <ls_param>
         WITH TABLE KEY name = <ls_parameter>-name.
        CHECK sy-subrc = 0.

        INSERT <ls_param> INTO TABLE lt_param_call.
      ENDLOOP.

      TRY.
          CALL METHOD <ls_item>-listener->(<ls_methdescr>-name)
            PARAMETER-TABLE
            lt_param_call.
        CATCH cx_sy_dyn_call_error INTO lo_error.
          zcx_eui_exception=>raise_dump( io_error = lo_error ).
      ENDTRY.
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.


METHOD has_handler.
  FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_listener.

  rv_ok = abap_false.
  LOOP AT mt_listener ASSIGNING <ls_item>.
    READ TABLE <ls_item>-t_methdescr TRANSPORTING NO FIELDS
      WITH KEY of_class  = iv_of_class
               for_event = iv_for_event.

    " Use the method for express test
    CHECK sy-subrc = 0.
    rv_ok = abap_true.
    RETURN.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
