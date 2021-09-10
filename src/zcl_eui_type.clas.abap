class ZCL_EUI_TYPE definition
  public
  final
  create public .

public section.
  type-pools ABAP .

  types:
    BEGIN OF ts_field_desc,
        name        TYPE zdeui_attr_name, " abap_attrname, " 61 char
        sys_type    TYPE abap_typekind, " SYSTEM
        ui_type     TYPE zdeui_ui_type, " Only for KIND = P
        length      TYPE i,             " Only for KIND = P
        decimals    TYPE i,             " Only for KIND = P
        " For editing in ALV
        rollname    TYPE zdeui_db_field,
        label       TYPE dfies-fieldtext,
        f4_table    TYPE zdeui_f4_table,
        " Table description
        table_kind  TYPE abap_tablekind,
        unique      TYPE abap_bool,
        key         TYPE abap_keydescr_tab,
        key_defkind TYPE abap_keydefkind,
        sub_fdesc   TYPE string,
      END OF ts_field_desc .
  types:
    tt_field_desc TYPE HASHED TABLE OF ts_field_desc WITH UNIQUE KEY name .
  types:
    tt_unique_type TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line .

  constants:
    BEGIN OF MC_UI_TYPE,
    " Simple UI types
    CHAR type STRING value 'char',
    NUMC type STRING value 'numc',
    NUMERIC type STRING value 'numeric',
    BOOLEAN type STRING value 'boolean',
    DATE type STRING value 'date',
    TIME type STRING value 'time',
    DATETIME type STRING value 'datetime',
    " Complext UI types
    STRING type STRING value 'string',
    RANGE  type STRING value 'range',
    TABLE  type STRING value 'table',
    " For tech only
    STRUCT type STRING value 'struct',
    OBJECT type STRING value 'object',
   END OF MC_UI_TYPE .

  class-methods GET_CATALOG
    importing
      value(IO_SALV) type ref to CL_SALV_TABLE optional
      !IR_TABLE type ref to DATA optional
      value(IR_STRUC) type ref to DATA optional
      !IO_STRUC type ref to CL_ABAP_STRUCTDESCR optional
      !IV_SORT type ABAP_BOOL optional
      !IV_TUNE_UP type ABAP_BOOL default ABAP_TRUE
    returning
      value(RT_FIELDCAT) type LVC_T_FCAT .
  class-methods IS_LIST_BOX
    importing
      !IV_TABNAME type DFIES-TABNAME
      !IV_FIELDNAME type DFIES-FIELDNAME
    exporting
      !EV_LIST_BOX type ABAP_BOOL
      !ES_SH_DESC type SHLP_DESCR .
  class-methods SPLIT_TYPE
    importing
      !IV_DATATYPE type CSEQUENCE
    exporting
      !EV_TABLE type CSEQUENCE
      !EV_FIELD type CSEQUENCE .
  class-methods FIND_DROPDOWN
    importing
      !IO_GRID type ref to CL_GUI_ALV_GRID
    changing
      !CS_FIELDCAT type LVC_S_FCAT
      !CV_DRDN_HNDL type I .
  class-methods GET_FIELD_DESC
    importing
      !IV_FIELD_NAME type CSEQUENCE optional
      !IV_DATA type ANY optional
      !IS_SH_FIELD type DFIES optional
      !IR_UNIQUE_TYPE type ref to TT_UNIQUE_TYPE optional
      !IV_TECH type ABAP_BOOL optional
    returning
      value(RS_FIELD_DESC) type TS_FIELD_DESC
    raising
      ZCX_EUI_EXCEPTION .
  class-methods CREATE_TYPE_DESCR
    importing
      !IV_ROLLNAME type CSEQUENCE optional
      !IS_FIELD_DESC type TS_FIELD_DESC optional
      value(IR_TYPE) type ref to DATA optional
    returning
      value(RO_TYPE) type ref to CL_ABAP_DATADESCR
    raising
      ZCX_EUI_EXCEPTION .
  class-methods CREATE_STRUCTURE
    importing
      !IO_RANGE type ref to CL_ABAP_DATADESCR optional
      !IV_SUB_FDESC type STRING optional
      !IT_FIELD_DESC type TT_FIELD_DESC optional
    returning
      value(RO_STRUCT) type ref to CL_ABAP_STRUCTDESCR
    raising
      ZCX_EUI_EXCEPTION .
  class-methods FIND_TABLE_FIELDNAME
    importing
      !IR_UNIQUE_TYPE type ref to TT_UNIQUE_TYPE
    changing
      !CV_ROLLNAME type CSEQUENCE
      !CV_LABEL type CSEQUENCE optional .
  class-methods GET_SUB_FIELD_DESC
    importing
      !IS_FIELD_DESC type TS_FIELD_DESC
    returning
      value(RT_FIELD_DESC) type TT_FIELD_DESC .
protected section.
private section.
*"* private components of class ZCL_EUI_TYPE
*"* do not include other source files here!!!

  class-methods _ADD_COMP_INFO
    importing
      !IO_STRUC type ref to CL_ABAP_STRUCTDESCR
      !IS_ROW type ANY
      !IV_TECH type ABAP_BOOL
      !IR_UNIQUE_TYPE type ref to TT_UNIQUE_TYPE
    changing
      !CS_FIELD_DESC type TS_FIELD_DESC
    raising
      ZCX_EUI_EXCEPTION .
  class-methods _ADD_COMP_INFO_CLASS
    importing
      !IO_CLASS type ref to CL_ABAP_CLASSDESCR
      !IO_INST type ANY
      !IV_TECH type ABAP_BOOL
      !IR_UNIQUE_TYPE type ref to TT_UNIQUE_TYPE
    changing
      !CS_FIELD_DESC type TS_FIELD_DESC
    raising
      ZCX_EUI_EXCEPTION .
ENDCLASS.



CLASS ZCL_EUI_TYPE IMPLEMENTATION.


METHOD create_structure.
  DATA:
    lt_comp      TYPE abap_component_tab,
    lt_sub_fdesc TYPE tt_field_desc,
    lv_ok        TYPE abap_bool.
  FIELD-SYMBOLS:
    <ls_field_desc> TYPE ts_field_desc,
    <ls_subfield>   TYPE ts_field_desc,
    <ls_comp>       LIKE LINE OF lt_comp.

  " №2 For select-options
  IF io_range IS NOT INITIAL.
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'SIGN'.
    <ls_comp>-type = cl_abap_elemdescr=>get_c( p_length = 1 ).

    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'OPTION'.
    <ls_comp>-type = cl_abap_elemdescr=>get_c( p_length = 2 ).

    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'LOW'.
    <ls_comp>-type = io_range.

    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'HIGH'.
    <ls_comp>-type = io_range.
  ENDIF.

  " №3 For table's strcuture
  DO 1 TIMES.
    CHECK iv_sub_fdesc IS NOT INITIAL.
    zcl_eui_conv=>from_json(
     EXPORTING
       iv_json = iv_sub_fdesc
     IMPORTING
       ex_data = lt_sub_fdesc
       ev_ok   = lv_ok ).
    CHECK lv_ok = abap_true.

    LOOP AT lt_sub_fdesc ASSIGNING <ls_subfield>.
      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = <ls_subfield>-name.
      <ls_comp>-type = create_type_descr( is_field_desc = <ls_subfield> ).
    ENDLOOP.
  ENDDO.

  " №4 Called from constructor if have in DB cluster
  LOOP AT it_field_desc ASSIGNING <ls_field_desc>.
    " Create sub levels
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = <ls_field_desc>-name.
    <ls_comp>-type = create_type_descr( is_field_desc = <ls_field_desc> ).
  ENDLOOP.

*  TRY.
      ro_struct = cl_abap_structdescr=>create( lt_comp ).
*    CATCH cx_sy_type_creation INTO DATA(lo_prev_error).
*      zcx_eui_exception=>raise_sys_error( io_error = lo_prev_error ).
*  ENDTRY.

ENDMETHOD.


METHOD create_type_descr.
  DATA:
    lo_line       TYPE REF TO cl_abap_datadescr,
    lo_prev_error TYPE REF TO cx_root,
    lo_type       TYPE REF TO cl_abap_typedescr.
  DATA lv_sys_type TYPE abap_typekind.
  DATA lv_message  TYPE string.

  " No type
  CLEAR ro_type.

  TRY.
      " №0
      IF is_field_desc IS SUPPLIED.

*    " structure for DB
*    IF mo_ui_ext IS NOT INITIAL.
*      TRY.
*          ir_type = mo_ui_ext->create_field_type(
*           iv_name = is_field_desc-name
*           iv_type = zif_prog_params_ui_ext=>mc_type_db ).
*        CATCH cx_sy_dyn_call_illegal_method.
*          CLEAR ir_type.
*      ENDTRY.
*      CHECK ir_type IS INITIAL.
*    ENDIF.

        " For tables speed 1
        IF is_field_desc-rollname IS NOT INITIAL.
          " Try to create
          TRY.
              ro_type = create_type_descr(
                iv_rollname = is_field_desc-rollname ).
            CATCH zcx_eui_exception.
              CLEAR ro_type.
          ENDTRY.
        ENDIF.

        IF ro_type IS INITIAL AND is_field_desc-ui_type <> mc_ui_type-struct.
          lv_sys_type = is_field_desc-sys_type.

          " For old option wrong is_field_desc-sys_type
          IF lv_sys_type = cl_abap_typedescr=>typekind_table AND is_field_desc-ui_type = mc_ui_type-range.
            " usually char
            lv_sys_type = cl_abap_typedescr=>typekind_char.
            " Show waning
            CONCATENATE `No right type for ` is_field_desc-name ` ` is_field_desc-rollname `!` INTO lv_message.
            MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'W'.
          ENDIF.

          CASE lv_sys_type.
            WHEN cl_abap_typedescr=>typekind_char.
              ro_type = cl_abap_elemdescr=>get_c( p_length = is_field_desc-length ).
            WHEN cl_abap_typedescr=>typekind_date.
              ro_type = cl_abap_elemdescr=>get_d( ).
            WHEN cl_abap_typedescr=>typekind_int.
              ro_type = cl_abap_elemdescr=>get_i( ).
            WHEN cl_abap_typedescr=>typekind_float.
              ro_type = cl_abap_elemdescr=>get_f( ).
            WHEN cl_abap_typedescr=>typekind_num.
              ro_type = cl_abap_elemdescr=>get_n( p_length = is_field_desc-length ).
            WHEN cl_abap_typedescr=>typekind_packed.
              ro_type = cl_abap_elemdescr=>get_p( p_length = is_field_desc-length p_decimals = is_field_desc-decimals ).
            WHEN cl_abap_typedescr=>typekind_string.
              ro_type = cl_abap_elemdescr=>get_string( ).
            WHEN cl_abap_typedescr=>typekind_time.
              ro_type = cl_abap_elemdescr=>get_t( ).
            WHEN cl_abap_typedescr=>typekind_table.
              " Below in code CASE is_field_desc-ui_type.

            WHEN OTHERS.
              MESSAGE s015(zeui_message) WITH is_field_desc-name INTO sy-msgli.
              zcx_eui_exception=>raise_sys_error( ).
          ENDCASE.
        ENDIF.

        CASE is_field_desc-ui_type.
          WHEN mc_ui_type-range.
            IF ro_type IS INITIAL.
              CONCATENATE `Cannot create range for ` is_field_desc-name ` ` is_field_desc-rollname `!` INTO lv_message.
              zcx_eui_exception=>raise_sys_error( iv_message = lv_message ).
            ENDIF.

            " Call №2 recursion
            lo_line = create_structure( io_range = ro_type ).
            ro_type = cl_abap_tabledescr=>create( p_line_type = lo_line ).

          WHEN mc_ui_type-table.
            " Call №3 recursion
            IF ro_type IS INITIAL.
              ro_type = create_structure( iv_sub_fdesc = is_field_desc-sub_fdesc ).
            ENDIF.

            IF     is_field_desc-table_kind = cl_abap_tabledescr=>tablekind_std
               AND is_field_desc-key_defkind = 'U' " From 7.40 KEYDEFKIND_USER
               AND is_field_desc-key IS INITIAL.
              ro_type = cl_abap_tabledescr=>create( p_line_type = ro_type ). " 'D' - KEYDEFKIND_DEFAULT
            ELSE.
              ro_type = cl_abap_tabledescr=>create(
                p_line_type   = ro_type
                p_table_kind  = is_field_desc-table_kind
                p_unique      = is_field_desc-unique
                p_key         = is_field_desc-key
                p_key_kind    = is_field_desc-key_defkind ).
            ENDIF.

          WHEN mc_ui_type-struct.
            ro_type = create_structure( iv_sub_fdesc = is_field_desc-sub_fdesc ).
        ENDCASE.
      ENDIF.

      CHECK ro_type IS INITIAL.

      " №0
      IF iv_rollname NP '*-*'.
        cl_abap_datadescr=>describe_by_name(
         EXPORTING
          p_name         = iv_rollname
         RECEIVING
          p_descr_ref    = lo_type
         EXCEPTIONS
          type_not_found = 1 ).

        " Handle somewhere else
        IF sy-subrc <> 0.
          MESSAGE s017(zeui_message) WITH iv_rollname INTO sy-msgli.
          zcx_eui_exception=>raise_sys_error( ).
        ENDIF.
        ro_type ?= lo_type.
        RETURN.
      ENDIF.

      " №1 - Create from text
      IF ir_type IS INITIAL AND iv_rollname IS NOT INITIAL.
        CREATE DATA ir_type TYPE (iv_rollname).
      ENDIF.

      " №2 - Based on incoming reference
      cl_abap_datadescr=>describe_by_data_ref(
       EXPORTING
        p_data_ref           = ir_type
       RECEIVING
        p_descr_ref          = lo_type
       EXCEPTIONS
        reference_is_initial = 1 ).

      " Handle somewhere else
      IF sy-subrc <> 0.
        MESSAGE s017(zeui_message) WITH iv_rollname INTO sy-msgli.
        zcx_eui_exception=>raise_sys_error( ).
      ENDIF.
      ro_type ?= lo_type.
    CATCH cx_root INTO lo_prev_error.
      CLEAR ro_type.
      zcx_eui_exception=>raise_sys_error( io_error = lo_prev_error ).
  ENDTRY.
ENDMETHOD.


METHOD find_dropdown.
  DATA:
    ls_sh_desc        TYPE shlp_descr,
    lv_list_box       TYPE abap_bool,
    lt_fielddescr     TYPE ddfields,
    ls_field          TYPE REF TO dfies,
    lt_field_desc     TYPE tt_field_desc,
    ls_field_desc     TYPE ts_field_desc,
    lo_struc          TYPE REF TO cl_abap_structdescr,
    lo_table          TYPE REF TO cl_abap_tabledescr,
    lr_table          TYPE REF TO data,
    lt_shlp_return    TYPE STANDARD TABLE OF ddshretval,
    ls_shlp_return    TYPE REF TO ddshretval,
    lv_prev_pos       TYPE i,
    ls_call_control   TYPE ddshf4ctrl,
    ls_fld_prop       TYPE REF TO ddshfprop,
    lt_shlp_descr_tab TYPE shlp_desct,
    lt_shlp_record    TYPE STANDARD TABLE OF seahlpres,
    lt_dropdown       TYPE lvc_t_dral,
    ls_dropdown       TYPE lvc_s_dral.
  FIELD-SYMBOLS:
    <lt_table> TYPE STANDARD TABLE,
    <ls_row>   TYPE any,
    <lv_value> TYPE any,
    <lv_low>   TYPE any,
    <lv_txt>   TYPE csequence.

  " No need
  IF cs_fieldcat-ref_table = abap_undefined AND cs_fieldcat-ref_field = abap_undefined.
    CLEAR cs_fieldcat-ref_table.
    CLEAR cs_fieldcat-ref_field.
    RETURN.
  ENDIF.

  " No need
  CHECK cs_fieldcat-checkbox <> abap_true
    AND cs_fieldcat-hotspot  <> abap_true
    AND cs_fieldcat-inttype  <> cl_abap_typedescr=>typekind_int
    AND cs_fieldcat-inttype  <> 'b' "cl_abap_typedescr=>typekind_int1
    AND cs_fieldcat-inttype  <> 's' "cl_abap_typedescr=>typekind_int2
    AND cs_fieldcat-inttype  <> '8'."cl_abap_typedescr=>typekind_int8.

  " Get top SH
  is_list_box(
   EXPORTING
    iv_tabname   = cs_fieldcat-ref_table
    iv_fieldname = cs_fieldcat-ref_field
   IMPORTING
    ev_list_box  = lv_list_box
    es_sh_desc   = ls_sh_desc ).
  CHECK lv_list_box = abap_true.

  " Work with copy
  lt_fielddescr[] = ls_sh_desc-fielddescr[].

  TRY.
      " Strucure fields
      LOOP AT lt_fielddescr REFERENCE INTO ls_field.
        ls_field_desc = get_field_desc( is_sh_field = ls_field->* ).
        INSERT ls_field_desc INTO TABLE lt_field_desc.
      ENDLOOP.

      " Output table
      lo_struc = create_structure( it_field_desc = lt_field_desc ).
      lo_table = cl_abap_tabledescr=>create( p_line_type = lo_struc ).
    CATCH zcx_eui_exception.
      RETURN.
  ENDTRY.

  " Asign it
  CREATE DATA lr_table TYPE HANDLE lo_table.
  ASSIGN lr_table->* TO <lt_table>.

  CALL FUNCTION 'F4IF_SELECT_VALUES'
    EXPORTING
      shlp           = ls_sh_desc
      maxrows        = 0         " all values of domain
      call_shlp_exit = abap_true " 'SELECT' only!
    TABLES
      return_tab     = lt_shlp_return.

**********************************************************************
  " Copied from --> METHOD get_sh_table.
**********************************************************************
  " Show all fields
  LOOP AT ls_sh_desc-fieldprop REFERENCE INTO ls_fld_prop.
    ls_fld_prop->shlpoutput = abap_true.
  ENDLOOP.

  " Call with SELECT event only (probably no texts)
  IF ls_sh_desc-intdescr-selmexit IS INITIAL.
    CALL FUNCTION 'F4IF_SELECT_VALUES'
      EXPORTING
        shlp           = ls_sh_desc
        maxrows        = 0         " all values of domain
        call_shlp_exit = abap_true " 'SELECT' only!
      TABLES
        return_tab     = lt_shlp_return.
  ELSE.
    " Get records first
    CALL FUNCTION 'F4IF_SELECT_VALUES'
      EXPORTING
        shlp           = ls_sh_desc
        maxrows        = 0         " all values of domain
        call_shlp_exit = abap_true
      TABLES
        record_tab     = lt_shlp_record.

    " Disp event
    ls_call_control-step       = 'DISP'.
    ls_call_control-maxrecords = 0. " all values of domain

    APPEND ls_sh_desc TO lt_shlp_descr_tab.
    CALL FUNCTION ls_sh_desc-intdescr-selmexit
      TABLES
        shlp_tab    = lt_shlp_descr_tab
        record_tab  = lt_shlp_record
      CHANGING
        shlp        = ls_sh_desc
        callcontrol = ls_call_control.

    " To normal state -> lt_shlp_return
    CLEAR lt_shlp_return.
    PERFORM transform_outval IN PROGRAM saplsdsd
      TABLES lt_shlp_record lt_shlp_return
      USING ls_call_control ls_sh_desc.
  ENDIF.

  " Write data to table
  lv_prev_pos = 0.
  SORT ls_sh_desc-fielddescr BY fieldname.
  LOOP AT lt_shlp_return REFERENCE INTO ls_shlp_return.
    " New row ?
    IF lv_prev_pos <> ls_shlp_return->recordpos.
      APPEND INITIAL LINE TO <lt_table> ASSIGNING <ls_row>.
    ENDIF.
    lv_prev_pos = ls_shlp_return->recordpos.

    " value
    ASSIGN COMPONENT ls_shlp_return->fieldname OF STRUCTURE <ls_row> TO <lv_value>.
    CHECK sy-subrc = 0.

    " Copy field field
    READ TABLE ls_sh_desc-fielddescr REFERENCE INTO ls_field BINARY SEARCH
      WITH KEY fieldname = ls_shlp_return->fieldname.

    " Special case for certain types
    CASE ls_field->inttype.
      WHEN cl_abap_typedescr=>typekind_time.
        CONCATENATE ls_shlp_return->fieldval+0(2)
                    ls_shlp_return->fieldval+3(2)
                    ls_shlp_return->fieldval+6(2) INTO <lv_value>.

      WHEN cl_abap_typedescr=>typekind_date.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external = ls_shlp_return->fieldval
          IMPORTING
            date_internal = <lv_value>
          EXCEPTIONS
            OTHERS        = 1.
        IF sy-subrc <> 0.
          CLEAR <lv_value>.
        ENDIF.

        " Integer, byte, short
      WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
        REPLACE ALL OCCURRENCES OF '.' IN ls_shlp_return->fieldval WITH ''.
        <lv_value> = ls_shlp_return->fieldval.

      WHEN OTHERS.
        <lv_value> = ls_shlp_return->fieldval.
    ENDCASE.
  ENDLOOP.

**********************************************************************
**********************************************************************

  " Next handle
  ADD 1 TO cv_drdn_hndl.

  " Prepare field catalog
  cs_fieldcat-drdn_hndl  = cv_drdn_hndl.
  cs_fieldcat-drdn_alias = abap_true.

  LOOP AT <lt_table> ASSIGNING <ls_row>.
    ASSIGN COMPONENT:
     '_LOW'  OF STRUCTURE <ls_row> TO <lv_low>,
     '_TEXT' OF STRUCTURE <ls_row> TO <lv_txt>.

    ls_dropdown-handle    = cs_fieldcat-drdn_hndl.
    ls_dropdown-int_value = <lv_low>.
    ls_dropdown-value     = <lv_low>.
    CONCATENATE ls_dropdown-value ` - ` <lv_txt> INTO ls_dropdown-value.

    " Add new item to dropdown
    APPEND ls_dropdown TO lt_dropdown.
  ENDLOOP.

  io_grid->set_drop_down_table(
   it_drop_down_alias = lt_dropdown ).
ENDMETHOD.


METHOD find_table_fieldname.
  TYPES:
    BEGIN OF ts_dd03l,
      tabname    TYPE dd03l-tabname,
      fieldname  TYPE dd03l-fieldname,
      shlporigin TYPE dd03l-shlporigin,
      "ddtext     TYPE dd03t-ddtext,
      tab_len    TYPE i,
    END OF ts_dd03l.

  DATA:
    lv_rollname TYPE rollname,
    lt_dd03l    TYPE STANDARD TABLE OF ts_dd03l,
    ls_dd03l    TYPE REF TO ts_dd03l,
    lv_tabfld   TYPE string,
    ls_dd04t    TYPE dd04t,
    lo_type     TYPE REF TO cl_abap_datadescr.
  FIELD-SYMBOLS:
    <lt_unique_type> TYPE tt_unique_type.

  " Table Fields
  CHECK cv_rollname IS NOT INITIAL.
  lv_rollname = cv_rollname.

  SELECT d~tabname d~fieldname d~shlporigin INTO CORRESPONDING FIELDS OF TABLE lt_dd03l "#EC TOO_MANY_ITAB_FIELDS
  FROM dd03l AS d UP TO 100 ROWS
  WHERE d~rollname = lv_rollname AND d~as4local = 'A' AND d~depth = 0.

  " Find short table name
  LOOP AT lt_dd03l REFERENCE INTO ls_dd03l.
    ls_dd03l->tab_len = strlen( ls_dd03l->tabname ).

    " In the end
    IF ls_dd03l->shlporigin IS NOT INITIAL.
      ls_dd03l->tab_len = ls_dd03l->tab_len - 1000.
    ENDIF.

    IF ls_dd03l->tabname CP '/*'.
      ls_dd03l->tab_len = ls_dd03l->tab_len - 1000.
    ENDIF.
  ENDLOOP.
  SORT lt_dd03l BY tab_len ASCENDING.

  " Try to find
  ASSIGN ir_unique_type->* TO <lt_unique_type>.
  LOOP AT lt_dd03l REFERENCE INTO ls_dd03l.
    CONCATENATE ls_dd03l->tabname '-' ls_dd03l->fieldname INTO lv_tabfld.

    " if type exist
    TRY.
        lo_type = create_type_descr( iv_rollname = lv_tabfld ).
      CATCH zcx_eui_exception.
        CLEAR lo_type.
    ENDTRY.
    CHECK lo_type IS NOT INITIAL.

    " Get next item
    IF ir_unique_type IS NOT INITIAL.
      READ TABLE <lt_unique_type> TRANSPORTING NO FIELDS
       WITH TABLE KEY table_line = lv_tabfld.
      CHECK sy-subrc <> 0.

      " Do not repeat types
      INSERT lv_tabfld INTO TABLE <lt_unique_type>.
    ENDIF.
    cv_rollname = lv_tabfld.

    DO 1 TIMES.
      " If have no text
      CHECK cv_label IS SUPPLIED AND cv_label IS INITIAL.

      " №2
      SELECT SINGLE * INTO ls_dd04t
      FROM dd04t
      WHERE rollname   = lv_rollname
        AND ddlanguage = sy-langu
        AND as4local   = 'A'
        AND as4vers    = 0.
      CHECK sy-subrc = 0.

      IF ls_dd04t-ddtext IS NOT INITIAL.
        cv_label = ls_dd04t-ddtext.
      ELSE.
        cv_label = ls_dd04t-reptext.
      ENDIF.
    ENDDO.

    RETURN.
  ENDLOOP.
ENDMETHOD.


METHOD get_catalog.
  DATA:
    lr_table        TYPE REF TO data,
    lr_columns      TYPE REF TO cl_salv_columns_table,
    lr_aggregations TYPE REF TO cl_salv_aggregations,
    lo_error        TYPE REF TO cx_salv_error.
  FIELD-SYMBOLS:
    <ls_data>     TYPE any,
    <lt_table>    TYPE STANDARD TABLE,
    <ls_fieldcat> LIKE LINE OF rt_fieldcat.

  " №0 - If no SALV control
  IF io_salv IS INITIAL.
    " №1 - Based on structure description
    IF io_struc IS NOT INITIAL.
      CREATE DATA ir_struc TYPE HANDLE io_struc.
    ENDIF.

    " №2 - Based on standard table
    IF ir_table IS NOT INITIAL.
      ASSIGN ir_table->* TO <lt_table>.
    ELSEIF ir_struc IS NOT INITIAL.
      " №3 - Based on structure reference
      ASSIGN ir_struc->* TO <ls_data>.

      " Create table
      CREATE DATA lr_table LIKE STANDARD TABLE OF <ls_data>.
      ASSIGN lr_table->* TO <lt_table>.
    ELSE. " zcx_eui_no_check=>raise_sys_error ?
      zcx_eui_exception=>raise_dump( iv_message = `Pass 'IR_TABLE' or 'IR_STRUC' parameter` ).
    ENDIF.

    " Create temporary SALV control
    TRY.
        cl_salv_table=>factory(
         IMPORTING
           r_salv_table   = io_salv
         CHANGING
           t_table        = <lt_table>  ).
      CATCH cx_salv_error INTO lo_error.                "#EC NO_HANDLER
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
  ENDIF.

  " Fields catalog
  lr_columns      = io_salv->get_columns( ).
  lr_aggregations = io_salv->get_aggregations( ).
  rt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
    r_columns      = lr_columns
    r_aggregations = lr_aggregations ).

  " For binary search only
  IF iv_sort = abap_true.
    SORT rt_fieldcat BY fieldname.
  ENDIF.

  " Default behaviour change field
  CHECK iv_tune_up = abap_true.

  " Small tune up
  LOOP AT rt_fieldcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-col_id = <ls_fieldcat>-col_pos = sy-tabix.

    IF <ls_fieldcat>-coltext IS INITIAL.
      <ls_fieldcat>-coltext = <ls_fieldcat>-reptext.
    ENDIF.

    IF <ls_fieldcat>-coltext IS INITIAL.
      <ls_fieldcat>-coltext = <ls_fieldcat>-scrtext_l.
    ENDIF.

    IF <ls_fieldcat>-domname = 'XSDBOOLEAN'.
      <ls_fieldcat>-checkbox = abap_true.
    ENDIF.
  ENDLOOP.
ENDMETHOD.


METHOD get_field_desc.
  FIELD-SYMBOLS <lv_row> TYPE  any.

  IF is_sh_field IS NOT INITIAL.
    rs_field_desc-name     = is_sh_field-fieldname.
    rs_field_desc-sys_type = is_sh_field-inttype.
    rs_field_desc-length   = is_sh_field-leng.
    rs_field_desc-decimals = is_sh_field-decimals.
    rs_field_desc-label    = is_sh_field-fieldtext.
    rs_field_desc-rollname = is_sh_field-rollname.
  ELSE.
    DATA lo_type TYPE REF TO cl_abap_typedescr.

    " №1 Type & value
    lo_type = cl_abap_typedescr=>describe_by_data( iv_data ).
    ASSIGN iv_data TO <lv_row>.

    " With tech info
    IF    lo_type->type_kind = cl_abap_typedescr=>typekind_dref
      AND iv_data IS NOT INITIAL
      AND iv_tech = abap_true.
      " №2 Type & value
      lo_type = cl_abap_typedescr=>describe_by_data_ref( iv_data ).
      ASSIGN iv_data->* TO <lv_row>.
    ENDIF.

    rs_field_desc-name     = iv_field_name.
    rs_field_desc-sys_type = lo_type->type_kind. "kind.
    rs_field_desc-length   = lo_type->length.
    rs_field_desc-decimals = lo_type->decimals.
    IF lo_type->is_ddic_type( ) = abap_true.
      rs_field_desc-rollname = lo_type->get_relative_name( ).
    ENDIF.
  ENDIF.

  CASE rs_field_desc-sys_type.

    WHEN cl_abap_typedescr=>typekind_char.
      " Get all boolean types
      DATA lv_domname TYPE dd04l-domname.
      SELECT SINGLE domname INTO lv_domname
      FROM dd04l
      WHERE rollname = rs_field_desc-rollname
        AND as4local = 'A'
        AND as4vers  = 0000.

      " Also CHAR
      CASE lv_domname.
        WHEN 'XSDBOOLEAN' OR 'OS_BOOLEAN'.
          rs_field_desc-ui_type = mc_ui_type-boolean.

        WHEN 'XSDDATETIME_Z' OR 'XSDDATETIME_LONG_Z' OR
             'XSDDATETIME_OFFSET' OR 'XSDDATETIME_LOCAL' OR 'XSDDATETIME_LOCAL_DT'.
          rs_field_desc-ui_type = mc_ui_type-datetime.

        WHEN OTHERS.
          rs_field_desc-ui_type  = mc_ui_type-char.
          " lv_find_db_field = abap_true.
      ENDCASE.

    WHEN cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_numeric.
      rs_field_desc-ui_type  = mc_ui_type-numc.
      " lv_find_db_field = abap_true.

      " Memo text
    WHEN cl_abap_typedescr=>typekind_string.
      rs_field_desc-ui_type  = mc_ui_type-string.
      rs_field_desc-rollname = 'STRINGVAL'.

    WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.
      DATA lr_struct_descr TYPE REF TO cl_abap_structdescr.
      IF iv_tech = abap_true.
        lr_struct_descr ?= lo_type.
        rs_field_desc-ui_type = mc_ui_type-struct.
      ENDIF.

    WHEN cl_abap_typedescr=>typekind_oref.
      DATA lr_class_descr TYPE REF TO cl_abap_classdescr.
      IF iv_tech = abap_true AND iv_data IS NOT INITIAL.
        lr_class_descr ?= cl_abap_classdescr=>describe_by_object_ref( iv_data ).
        rs_field_desc-ui_type = mc_ui_type-object.
      ENDIF.

    WHEN cl_abap_typedescr=>typekind_table.
      DATA lr_table_descr  TYPE REF TO cl_abap_tabledescr.

      rs_field_desc-ui_type  = mc_ui_type-table.
      lr_table_descr ?= lo_type.

      rs_field_desc-table_kind   = lr_table_descr->table_kind.
      rs_field_desc-unique       = lr_table_descr->has_unique_key.
      rs_field_desc-key          = lr_table_descr->key.
      rs_field_desc-key_defkind  = lr_table_descr->key_defkind.

      " No need for standardc table
      IF rs_field_desc-table_kind = cl_abap_tabledescr=>tablekind_std.
        CLEAR rs_field_desc-key.
      ENDIF.

      " Only for structures & objects
      TRY.
          lr_struct_descr ?= lr_table_descr->get_table_line_type( ).
        CATCH cx_sy_move_cast_error.
          MESSAGE s016(zeui_message) WITH rs_field_desc-name INTO sy-msgli.
          zcx_eui_exception=>raise_sys_error( ).
      ENDTRY.

      " Get first row
      FIELD-SYMBOLS <lt_value> TYPE ANY TABLE.
      ASSIGN <lv_row> TO <lt_value>.
      UNASSIGN <lv_row>.
      LOOP AT <lt_value> ASSIGNING <lv_row>.
        EXIT.
      ENDLOOP.

      " Create 1 row on a fly
      IF <lv_row> IS NOT ASSIGNED.
        " Create STANDARD table for field catalog!
        DATA lr_row TYPE REF TO data.
        CREATE DATA lr_row TYPE HANDLE lr_struct_descr.
        ASSIGN lr_row->* TO <lv_row>.
      ENDIF.

      " Date
    WHEN cl_abap_typedescr=>typekind_date.
      rs_field_desc-ui_type  = mc_ui_type-date.

      " Time
    WHEN cl_abap_typedescr=>typekind_time.
      rs_field_desc-ui_type  = mc_ui_type-time.

      " Integer, byte, short
    WHEN cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1  OR cl_abap_typedescr=>typekind_int2.
      rs_field_desc-ui_type  = mc_ui_type-numeric.

      " Double
    WHEN cl_abap_typedescr=>typekind_packed OR cl_abap_typedescr=>typekind_float OR
         '/' OR 'a' OR 'e'. " cl_abap_typedescr=>typekind_decfloat  OR cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34.
      rs_field_desc-ui_type  = mc_ui_type-numeric.

    WHEN OTHERS.

  ENDCASE.

  " Structure & tables
  IF lr_struct_descr IS NOT INITIAL.
    _add_comp_info( EXPORTING io_struc       = lr_struct_descr
                              iv_tech        = iv_tech
                              ir_unique_type = ir_unique_type
                              is_row         = <lv_row>
                    CHANGING  cs_field_desc  = rs_field_desc ).
  ELSEIF lr_class_descr IS NOT INITIAL.
    _add_comp_info_class( EXPORTING io_class       = lr_class_descr
                                    iv_tech        = iv_tech
                                    ir_unique_type = ir_unique_type
                                    io_inst        = <lv_row>
                          CHANGING  cs_field_desc  = rs_field_desc ).
  ENDIF.

  " TABLE-FIELDNAME from search help
  IF is_sh_field-reffield IS NOT INITIAL.
    CONCATENATE is_sh_field-reftable '-' is_sh_field-reffield INTO rs_field_desc-rollname.
  ENDIF.

  " Try to find TABLE-FIELDNAME
  IF " lv_find_db_field = abap_true AND
    rs_field_desc-ui_type <> mc_ui_type-table  AND
    rs_field_desc-ui_type <> mc_ui_type-string AND
    rs_field_desc-rollname NP '*-*'.
    find_table_fieldname(
     EXPORTING
      ir_unique_type = ir_unique_type
     CHANGING
      cv_rollname    = rs_field_desc-rollname
      cv_label       = rs_field_desc-label ).
  ENDIF.

  " Set default text
  IF rs_field_desc-label IS INITIAL.
    rs_field_desc-label = rs_field_desc-name.
  ENDIF.
ENDMETHOD.


METHOD get_sub_field_desc.
  DATA lv_ok TYPE abap_bool.
  zcl_eui_conv=>from_json( EXPORTING iv_json = is_field_desc-sub_fdesc
                           IMPORTING ev_ok   = lv_ok
                                     ex_data = rt_field_desc ).
  CHECK lv_ok <> abap_true.
  MESSAGE s022(zeui_message) WITH is_field_desc-name DISPLAY LIKE 'E'.
ENDMETHOD.


METHOD is_list_box.
  CLEAR:
   ev_list_box,
   es_sh_desc.

  CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
    EXPORTING
      tabname   = iv_tabname
      fieldname = iv_fieldname
    IMPORTING
      shlp      = es_sh_desc
    EXCEPTIONS
      OTHERS    = 1.

  " Fixed values of domains
  CHECK sy-subrc = 0 AND es_sh_desc-shlptype = 'FV'.
  ev_list_box = abap_true.
ENDMETHOD.


METHOD split_type.
  " Check is table and field name
  CHECK iv_datatype CP '*-*'.

  TRY.
      " Exist in data dictionary?
      zcl_eui_type=>create_type_descr( iv_rollname = iv_datatype ).

      " DB and field
      SPLIT iv_datatype AT '-' INTO ev_table ev_field.
    CATCH zcx_eui_exception.
      CLEAR:
       ev_table,
       ev_field.
  ENDTRY.
ENDMETHOD.


METHOD _add_comp_info.
  " For speed creation
  IF io_struc->is_ddic_type( ) = abap_true.
    DATA ls_header TYPE x030l.
    ls_header = io_struc->get_ddic_header( ).
    cs_field_desc-rollname = ls_header-tabname.
  ENDIF.

  CLEAR cs_field_desc-sub_fdesc.

  DATA lt_sub_fdesc    TYPE tt_field_desc. " lv_find_db_field TYPE abap_bool
  DATA ls_subfield     TYPE ts_field_desc.
  FIELD-SYMBOLS <ls_comp_tab> TYPE abap_compdescr.
  FIELD-SYMBOLS <lv_subvalue> TYPE any.
  LOOP AT io_struc->components ASSIGNING <ls_comp_tab>.
    ASSIGN COMPONENT <ls_comp_tab>-name OF STRUCTURE is_row TO <lv_subvalue>.

    " Recursion
    ls_subfield = get_field_desc( iv_field_name  = <ls_comp_tab>-name
                                  iv_data        = <lv_subvalue>
                                  ir_unique_type = ir_unique_type
                                  iv_tech        = iv_tech ).

    " No need to save
    IF    cs_field_desc-ui_type = mc_ui_type-table AND iv_tech = abap_true
      AND ls_subfield-ui_type IS INITIAL AND ls_subfield-sys_type = cl_abap_typedescr=>typekind_dref.
      CONTINUE.
    ENDIF.

    INSERT ls_subfield INTO TABLE lt_sub_fdesc.
  ENDLOOP.

  cs_field_desc-sub_fdesc = zcl_eui_conv=>to_json( im_data = lt_sub_fdesc ).

**********************************************************************
  " Select option ?
  DATA lv_cnt TYPE i. "DO 1 TIMES.
  lv_cnt = lines( lt_sub_fdesc ).

  CHECK lv_cnt = 4
    AND cs_field_desc-ui_type = mc_ui_type-table.
  " Check by name
  LOOP AT lt_sub_fdesc TRANSPORTING NO FIELDS WHERE
     name = 'SIGN' OR name = 'OPTION' OR name = 'LOW' OR name = 'HIGH'. "#EC CI_HASHSEQ
    lv_cnt = lv_cnt - 1.
  ENDLOOP.

  " Select-option
  CHECK lv_cnt = 0.
  cs_field_desc-ui_type  = mc_ui_type-range.

  " No need in components
  CLEAR cs_field_desc-sub_fdesc.

  " Where to find TABLE-FIELDNAME
  FIELD-SYMBOLS <ls_subfield> LIKE ls_subfield.
  READ TABLE lt_sub_fdesc ASSIGNING <ls_subfield>
   WITH TABLE KEY name = 'LOW'.
  cs_field_desc-rollname = <ls_subfield>-rollname.
  cs_field_desc-label    = <ls_subfield>-label.
  " TODO Old options!!!  cs_field_desc-sys_type = <ls_subfield>-sys_type.
  " lv_find_db_field = abap_true.
ENDMETHOD.


METHOD _add_comp_info_class.
  " For speed creation
  IF io_class->is_ddic_type( ) = abap_true.
    DATA ls_header TYPE x030l.
    ls_header = io_class->get_ddic_header( ).
    cs_field_desc-rollname = ls_header-tabname.
  ENDIF.

  CLEAR cs_field_desc-sub_fdesc.

  DATA lt_sub_fdesc    TYPE tt_field_desc. " lv_find_db_field TYPE abap_bool
  DATA ls_subfield     TYPE ts_field_desc.
  FIELD-SYMBOLS <ls_attr>     TYPE abap_attrdescr.
  FIELD-SYMBOLS <lv_subvalue> TYPE any.
  LOOP AT io_class->attributes ASSIGNING <ls_attr> WHERE visibility = cl_abap_objectdescr=>public.
    DATA lv_field TYPE string.
    CONCATENATE 'IO_INST->' <ls_attr>-name INTO lv_field.
    ASSIGN (lv_field) TO <lv_subvalue>.

    " Recursion
    ls_subfield = get_field_desc( iv_field_name  = <ls_attr>-name
                                  iv_data        = <lv_subvalue>
                                  ir_unique_type = ir_unique_type
                                  iv_tech        = iv_tech ).

    " No need to save
    IF    cs_field_desc-ui_type = mc_ui_type-table AND iv_tech = abap_true
      AND ls_subfield-ui_type IS INITIAL AND ls_subfield-sys_type = cl_abap_typedescr=>typekind_dref.
      CONTINUE.
    ENDIF.

    INSERT ls_subfield INTO TABLE lt_sub_fdesc.
  ENDLOOP.

  cs_field_desc-sub_fdesc = zcl_eui_conv=>to_json( im_data = lt_sub_fdesc ).
ENDMETHOD.
ENDCLASS.
