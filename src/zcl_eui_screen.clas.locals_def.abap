*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_screen DEFINITION.
  PUBLIC SECTION.
    DATA:
      " Parent
      mo_eui_screen      TYPE REF TO zcl_eui_screen,

      " Screen context & map of fields
      mr_context         TYPE REF TO data,
      mv_unq_rollname    TYPE abap_bool,
      mt_map             TYPE zcl_eui_screen=>tt_map,

      " For pbo
      mt_screen          TYPE zcl_eui_screen=>tt_screen,

      " Initilize with values & Set lables
      mv_pbo_init_params TYPE abap_bool VALUE abap_true,
      mv_pbo_set_labels  TYPE abap_bool.

    METHODS:
      constructor
        IMPORTING
          io_eui_screen   TYPE REF TO zcl_eui_screen
          ir_context      TYPE REF TO data
          iv_unq_rollname TYPE abap_bool DEFAULT abap_false,

      fill_from_context FINAL
        RAISING zcx_eui_exception,

      _check_is_list_box
        CHANGING
          cs_map TYPE zcl_eui_screen=>ts_map,

      get_parameter_name
        IMPORTING
                  is_comp        TYPE REF TO abap_compdescr
                  iv_index       TYPE num2
        RETURNING VALUE(rv_name) TYPE string,

      customize
        IMPORTING
          it_customize TYPE zcl_eui_screen=>tt_customize,

      get_screen_by_map
        IMPORTING
                  iv_name          TYPE zcl_eui_screen=>ts_map-name
        RETURNING VALUE(rs_screen) TYPE zcl_eui_screen=>ts_screen,

      pbo
        IMPORTING
          iv_before     TYPE abap_bool OPTIONAL
          iv_after      TYPE abap_bool OPTIONAL
        CHANGING
          cv_set_status TYPE abap_bool OPTIONAL,

      set_initial_values
        IMPORTING
          iv_force TYPE abap_bool OPTIONAL,

      _get_hash
        IMPORTING
                  iv_value       TYPE any
        RETURNING VALUE(rv_hash) TYPE char16,

      show
        IMPORTING
                  iv_before    TYPE abap_bool OPTIONAL
                  iv_after     TYPE abap_bool OPTIONAL
        CHANGING
                  cv_close_cmd TYPE syucomm
        RAISING   zcx_eui_exception,

      " PAI
      check_pai
        IMPORTING
                  iv_command    TYPE syucomm
        CHANGING
                  cv_map_index  TYPE i
                  cv_close      TYPE abap_bool
                  cv_read_after TYPE abap_bool
        RAISING   cx_sy_conversion_no_number,

      call_editor FINAL
        IMPORTING
          iv_map_index TYPE i,

      read_from_screen
        IMPORTING
          ir_map TYPE REF TO zcl_eui_screen=>ts_map,

      _find_f4_tables
        IMPORTING
          is_field_desc TYPE zcl_eui_type=>ts_field_desc
          io_alv        TYPE REF TO zcl_eui_alv.

    CLASS-METHODS:
      is_fixed_values_list
        IMPORTING is_catalog   TYPE lvc_s_fcat
                  iv_read_only TYPE abap_bool
        EXPORTING ev_update    TYPE abap_bool
                  ev_is_fixed  TYPE abap_bool
        CHANGING  ct_range     TYPE STANDARD TABLE.
ENDCLASS.

CLASS lcl_scr_free DEFINITION INHERITING FROM lcl_screen FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_eui_screen TYPE REF TO zcl_eui_screen
          ir_context    TYPE REF TO data,

      get_text_value
        IMPORTING
          is_data        TYPE any
          is_field_desc  TYPE zcl_eui_type=>ts_field_desc  OPTIONAL
          iv_field       TYPE csequence                    OPTIONAL
        RETURNING
          VALUE(rv_text) TYPE rsdsselopt-low,

      read_free_sel_value
        IMPORTING
                  is_map       TYPE REF TO zcl_eui_screen=>ts_map
                  it_range     TYPE STANDARD TABLE
        RETURNING VALUE(rv_ok) TYPE abap_bool,

      get_parameter_name REDEFINITION,
      pbo                REDEFINITION,
      check_pai          REDEFINITION,
      show               REDEFINITION,
      read_from_screen   REDEFINITION.
ENDCLASS.

CLASS lcl_scr_dync DEFINITION INHERITING FROM lcl_screen FINAL.
  PUBLIC SECTION.
    METHODS:
      _create_program,
      _make_screen_code
        RETURNING VALUE(rt_code) TYPE stringtab,

      get_parameter_name REDEFINITION,
      show               REDEFINITION,
      check_pai          REDEFINITION,
      pbo                REDEFINITION.
ENDCLASS.

CLASS zcl_eui_screen DEFINITION LOCAL FRIENDS lcl_screen.
CLASS zcl_eui_screen DEFINITION LOCAL FRIENDS lcl_scr_free.
CLASS zcl_eui_screen DEFINITION LOCAL FRIENDS lcl_scr_dync.


CLASS lcl_fv_dialog DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF mc_cmd,
        show_range   TYPE syucomm VALUE '_SHOW_RANGE',
        select_all   TYPE syucomm VALUE '_SELECT_ALL',
        deselect_all TYPE syucomm VALUE '_DESELECT_ALL',
      END OF mc_cmd.

    TYPES: BEGIN OF ts_f4,
             mark TYPE abap_bool,
             key  TYPE char30,
             text TYPE string,
           END OF ts_f4,
           tt_f4 TYPE STANDARD TABLE OF ts_f4 WITH DEFAULT KEY.

    DATA:
           _mr_alv TYPE REF TO tt_f4.

    METHODS: show IMPORTING it_range     TYPE STANDARD TABLE
                            iv_read_only TYPE abap_bool
                            iv_title     TYPE csequence
                  EXPORTING ev_cmd       TYPE syucomm
                            et_f4        TYPE tt_f4
                  CHANGING  ct_dropdown  TYPE lvc_t_dral,

      _on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      _on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING sender e_ucomm,

      _on_pai_event FOR EVENT pai_event OF zif_eui_manager
        IMPORTING
          sender
          iv_command
          cv_close.
ENDCLASS.
