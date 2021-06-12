*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      " Owner class
      mo_eui_alv            TYPE REF TO zcl_eui_alv,
      ms_field_desc         TYPE REF TO zcl_eui_type=>ts_field_desc,

      " Own field catalog
      mt_sub_field          TYPE zcl_eui_type=>tt_field_desc,

      " F4 & dropdown
      mt_f4_table           TYPE zcl_eui_alv=>tt_f4_table,
      mv_drdn_hndl          TYPE i,

      " Own table
      mr_table              TYPE REF TO data,

      " TOP_OF_PAGE
      mv_top_of_page_height TYPE i,
      mo_dyndoc             TYPE REF TO cl_dd_document.

    METHODS:
      constructor
        IMPORTING
          io_eui_alv TYPE REF TO zcl_eui_alv,

      set_field_desc
        IMPORTING
          is_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc,

      prepare_layout
        CHANGING
          cs_layout TYPE lvc_s_layo,

      prepare_variant
        CHANGING
          cs_variant TYPE disvariant,

      get_field_catalog
        RETURNING VALUE(rt_fieldcat) TYPE lvc_t_fcat,

      is_editable
        RETURNING VALUE(rv_editable) TYPE abap_bool,

      pbo_init
        IMPORTING
          io_container TYPE REF TO cl_gui_container,

      after_close
        IMPORTING
          iv_close_cmd TYPE syucomm
        CHANGING
          cv_close     TYPE abap_bool,

      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          sender
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm,

      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING
          sender
          e_row
          e_column
          es_row_no,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          sender
          e_row_id
          e_column_id
          es_row_no,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          sender
          e_object
          e_interactive,

      on_menu_button FOR EVENT menu_button OF cl_gui_alv_grid
        IMPORTING
          sender
          e_object
          e_ucomm,

      on_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          sender
          e_dyndoc_id
          table_index,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          sender
          e_ucomm,

      on_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid
        IMPORTING
          sender,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING
          sender
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display,

      _check_f4_table
        IMPORTING
          io_grid     TYPE REF TO cl_gui_alv_grid
        CHANGING
          ct_fieldcat TYPE lvc_t_fcat,

      _self_f4
        IMPORTING
                  io_grid        TYPE REF TO cl_gui_alv_grid
                  iv_fieldname   TYPE csequence
                  is_row_no      TYPE lvc_s_roid
        RETURNING VALUE(rv_self) TYPE abap_bool,

      _get_std_table
        IMPORTING
                  ir_ant_table TYPE REF TO data
        EXPORTING er_std_table TYPE REF TO data
                  et_std_field TYPE tttext255,

      _fill_std_corresponding
        IMPORTING
          ir_any_f4_table TYPE REF TO data
          ir_std_f4_table TYPE REF TO data.
ENDCLASS.

CLASS zcl_eui_alv DEFINITION LOCAL FRIENDS lcl_helper.
