*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_helper DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      " Owner class
      mo_eui_alv    TYPE REF TO zcl_eui_alv,
      ms_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc,

      " Own field catalog
      mt_sub_field  TYPE zcl_eui_type=>tt_field_desc,

      " Own table
      mr_table      TYPE REF TO data,

      " TOP_OF_PAGE
      mo_dyndoc     TYPE REF TO cl_dd_document.

    METHODS:
      constructor
        IMPORTING
          io_eui_alv    TYPE REF TO zcl_eui_alv
          is_field_desc TYPE REF TO zcl_eui_type=>ts_field_desc,

      prepare_layout
        CHANGING
          cs_layout TYPE lvc_s_layo,

      prepare_variant
        CHANGING
          cs_variant TYPE disvariant,

      get_field_catalog
        RETURNING VALUE(rt_fieldcat) TYPE lvc_t_fcat,

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

      on_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
            sender
            e_dyndoc_id
            table_index,

      on_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            sender
            e_ucomm,

      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING
            sender
            e_fieldname
            e_fieldvalue
            es_row_no
            er_event_data
            et_bad_cells
            e_display.
ENDCLASS.

CLASS zcl_eui_alv DEFINITION LOCAL FRIENDS lcl_helper.
