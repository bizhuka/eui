CLASS zcl_eui_alv_style DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPE-POOLS abap.

    TYPES:
      x4 TYPE x LENGTH 4,

      BEGIN OF ts_col_info,
        column     TYPE lvc_fname,
        style      TYPE x4,
        style2     TYPE x4,
        col_pos    TYPE int4,
        mergehoriz TYPE int4,
        ignore_row TYPE abap_bool,
      END OF ts_col_info,
      tt_col_info TYPE STANDARD TABLE OF ts_col_info WITH DEFAULT KEY,

      BEGIN OF ts_row_info,
        row_id     TYPE int4,
        style      TYPE x4,
        style2     TYPE x4,
        mergevert  TYPE int4,
        ignore_col TYPE abap_bool,
      END OF ts_row_info,
      tt_row_info TYPE SORTED TABLE OF ts_row_info WITH UNIQUE KEY row_id,

      BEGIN OF ts_val_info,
        value      TYPE string,
        merge_mode TYPE x4,
        style      TYPE x4,
        style2     TYPE x4,
        " mergevert  TYPE int4, mergehoriz TYPE int4, <-- too strange
      END OF ts_val_info,
      tt_val_info TYPE STANDARD TABLE OF ts_val_info WITH DEFAULT KEY.

    CONSTANTS:

      BEGIN OF c_merge_mode,
        equal            TYPE x4 VALUE '00000001', " Merge strictly identical values
        value_and_empty  TYPE x4 VALUE '00000002', " Merge identical values AND subsequent empty cells
        " Use value parameter
        contains_pattern TYPE x4 VALUE '00000003',
        regex            TYPE x4 VALUE '00000004',
        merge_vertical   TYPE x4 VALUE '00000080', " Dec 128
        merge_horizontal TYPE x4 VALUE '00000100', " Dec 256
      END OF c_merge_mode.

    METHODS constructor
      IMPORTING
        io_grid TYPE REF TO cl_gui_alv_grid.

    METHODS:
      set_style IMPORTING it_columns         TYPE tt_col_info           OPTIONAL
                          it_rows            TYPE tt_row_info           OPTIONAL
                          it_values          TYPE tt_val_info           OPTIONAL
                          iv_add             TYPE abap_bool DEFAULT abap_true
                RETURNING VALUE(ro_instance) TYPE REF TO zcl_eui_alv_style,
      set_property IMPORTING iv_property TYPE csequence
                             iv_value    TYPE any.
    DATA mr_style_table TYPE REF TO lvc_t_data READ-ONLY.

  PRIVATE SECTION.
    TYPES:
      lvc_s_data_ref TYPE REF TO lvc_s_data,

      BEGIN OF ts_style_table_ref,
        row_id   TYPE lvc_s_data-row_id,
        col_pos  TYPE lvc_s_data-col_pos,
        data_ref TYPE lvc_s_data_ref,
      END OF ts_style_table_ref,
      tt_style_table_ref TYPE SORTED TABLE OF ts_style_table_ref WITH UNIQUE KEY row_id col_pos.


    DATA mr_data_table          TYPE REF TO data.
    DATA mo_grid                TYPE REF TO cl_gui_alv_grid.

    METHODS:
      _merge IMPORTING it_columns TYPE tt_col_info
                       it_rows    TYPE tt_row_info
                       it_values  TYPE tt_val_info
                       iv_add     TYPE abap_bool DEFAULT abap_true,

      _apply_style IMPORTING iv_style      TYPE x4
                             iv_style2     TYPE x4
                             iv_mergevert  TYPE lvc_s_data-mergevert  OPTIONAL
                             iv_mergehoriz TYPE lvc_s_data-mergehoriz OPTIONAL
                             iv_add        TYPE abap_bool
                   CHANGING  cs_style      TYPE lvc_s_data,
      _get_ready_columns IMPORTING it_columns        TYPE tt_col_info
                         RETURNING VALUE(rt_columns) TYPE tt_col_info.
ENDCLASS.



CLASS zcl_eui_alv_style IMPLEMENTATION.


  METHOD constructor.
    mo_grid        = io_grid.
    mr_style_table = lcl_helper=>get_style_table( mo_grid ).
    mr_data_table  = lcl_helper=>get_data_table( mo_grid ).
  ENDMETHOD.


  METHOD _get_ready_columns.
    DATA: lt_fieldcatalog TYPE lvc_t_fcat,
          ls_column       TYPE ts_col_info.
    FIELD-SYMBOLS: <ls_column>        TYPE ts_col_info,
                   <lfs_fieldcatalog> TYPE lvc_s_fcat.

    rt_columns = it_columns[].

    mo_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fieldcatalog ).

    LOOP AT rt_columns ASSIGNING <ls_column>.
      IF  ( <ls_column>-column IS INITIAL     AND <ls_column>-col_pos IS INITIAL )
       OR ( <ls_column>-column IS NOT INITIAL AND <ls_column>-col_pos IS NOT INITIAL ).
        zcx_eui_no_check=>raise_sys_error( iv_message = 'Pass the column name or col_pos field'(e01) ).
      ENDIF.

      IF <ls_column>-column IS NOT INITIAL.
        READ TABLE lt_fieldcatalog ASSIGNING <lfs_fieldcatalog> WITH KEY fieldname = <ls_column>-column.
      ELSE.
        READ TABLE lt_fieldcatalog ASSIGNING <lfs_fieldcatalog> WITH KEY col_pos = <ls_column>-col_pos.
      ENDIF.
      IF sy-subrc <> 0.
        zcx_eui_no_check=>raise_sys_error( iv_message = 'The column was not found in the catalogue'(e02) ).
      ENDIF.

      <ls_column>-column  = <lfs_fieldcatalog>-fieldname.
      <ls_column>-col_pos = <lfs_fieldcatalog>-col_pos.
    ENDLOOP.

    SORT rt_columns BY col_pos.
  ENDMETHOD.

  METHOD set_property.
    lcl_helper=>set_grid_property( io_grid     = mo_grid
                                   iv_property = iv_property
                                   iv_value    = iv_value ).
  ENDMETHOD.


  METHOD set_style.
    DATA: lt_columns         TYPE tt_col_info,
          lt_no_merge_values TYPE tt_val_info,
          lt_merge_values    TYPE tt_val_info.
    DATA: ls_row    TYPE ts_row_info,
          lv_row_ok TYPE i,
          ls_col    TYPE ts_col_info,
          lv_col_ok TYPE i,
          ls_val    TYPE ts_val_info,
          lv_val_ok TYPE i.
    DATA: lv_style      TYPE x4,
          lv_style2     TYPE x4,
          lv_mergevert  TYPE i, " int4
          lv_mergehoriz TYPE i. " int4

    FIELD-SYMBOLS: <lt_style_table> TYPE lvc_t_data,
                   <ls_val>         TYPE ts_val_info,
                   <ls_style_row>   TYPE lvc_s_data.

    ro_instance = me.

    ASSIGN mr_style_table->* TO <lt_style_table>.
    IF <lt_style_table> IS NOT ASSIGNED OR <lt_style_table> IS INITIAL.
      RETURN.
    ENDIF.

    lt_columns = _get_ready_columns( it_columns[] ).

    LOOP AT it_values ASSIGNING <ls_val>.
      IF <ls_val>-merge_mode IS INITIAL.
        APPEND <ls_val> TO lt_no_merge_values.
      ELSE.
        APPEND <ls_val> TO lt_merge_values.
      ENDIF.
    ENDLOOP.

    IF it_values[] IS INITIAL OR lt_no_merge_values[] IS NOT INITIAL.
      LOOP AT <lt_style_table> ASSIGNING <ls_style_row>.
        " Filter by Rows
        CLEAR: ls_row, lv_row_ok.
        IF it_rows IS NOT INITIAL.
          READ TABLE it_rows WITH TABLE KEY row_id = <ls_style_row>-row_id INTO ls_row.
          lv_row_ok = sy-subrc.
        ENDIF.

        " Filter by Columns
        CLEAR: ls_col, lv_col_ok.
        IF lt_columns IS NOT INITIAL.
          " Find column name from col_pos in style row
          READ TABLE lt_columns WITH KEY col_pos = <ls_style_row>-col_pos BINARY SEARCH INTO ls_col.
          lv_col_ok = sy-subrc.
        ENDIF.

        " Filter by Values.
        CLEAR: ls_val, lv_val_ok.
        IF lt_no_merge_values[] IS NOT INITIAL.
          lv_val_ok = 4.
          LOOP AT lt_no_merge_values INTO ls_val.
            IF ls_val-value CS '*'.
              CHECK <ls_style_row>-value CP ls_val-value.
            ELSE.
              CHECK <ls_style_row>-value EQ ls_val-value.
            ENDIF.

            lv_val_ok = 0.
            EXIT.
          ENDLOOP.
        ENDIF.

        CHECK ( lv_row_ok = 0 OR ls_col-ignore_row = abap_true )
          AND ( lv_col_ok = 0 OR ls_row-ignore_col = abap_true )
          AND ( lv_val_ok = 0 ).

        " Apply Style
        lv_style  = ls_row-style  BIT-OR ls_col-style.
        lv_style  = lv_style      BIT-OR ls_val-style.

        lv_style2 = ls_row-style2 BIT-OR ls_col-style2.
        lv_style2 = lv_style2     BIT-OR ls_val-style2.

        lv_mergevert  = ls_row-mergevert.
        lv_mergehoriz = ls_col-mergehoriz.

        _apply_style( EXPORTING iv_style      = lv_style
                                iv_style2     = lv_style2
                                iv_mergevert  = lv_mergevert
                                iv_mergehoriz = lv_mergehoriz
                                iv_add        = iv_add
                      CHANGING  cs_style      = <ls_style_row> ).
      ENDLOOP.
    ENDIF.

    IF lt_merge_values[] IS NOT INITIAL.
      _merge( it_columns = lt_columns[]
              it_values  = lt_merge_values[]
              it_rows    = it_rows[] ).
    ENDIF.
  ENDMETHOD.


  METHOD _apply_style.
    DATA lv_style  TYPE x4.
    DATA lv_style2 TYPE x4.

    IF iv_add <> abap_true.
      " Just set
      cs_style-style  = iv_style.
      cs_style-style2 = iv_style2.
    ELSE.
      lv_style  = cs_style-style.
      lv_style2 = cs_style-style2.

      lv_style  = iv_style  BIT-OR lv_style.
      lv_style2 = iv_style2 BIT-OR lv_style2.

      cs_style-style  = lv_style.
      cs_style-style2 = lv_style2.
    ENDIF.

    IF iv_mergevert IS NOT INITIAL.
      cs_style-mergevert = iv_mergevert.
    ENDIF.

    IF iv_mergehoriz IS NOT INITIAL.
      cs_style-mergehoriz = iv_mergehoriz.
    ENDIF.
  ENDMETHOD.


  METHOD _merge.
    CONSTANTS:
       lc_no_value TYPE string VALUE `~~~!!!@@@`.

    DATA: lv_data_row_cnt TYPE i,
          lv_col_cnt      TYPE i.
    DATA: lt_style_table_ref TYPE tt_style_table_ref,
          ls_ref             TYPE ts_style_table_ref.

    DATA: lv_merge_mode TYPE x4,
          lv_mask       TYPE x4.
    DATA: lv_direction_flag TYPE x4.
    DATA: lv_outer_cnt TYPE i,
          lv_inner_cnt TYPE i.
    DATA: lv_outer_idx TYPE i,
          lv_inner_idx TYPE i.
    DATA: lv_merge_count   TYPE i,
          lv_prev_pos      TYPE i,
          lv_prev_pos_calc TYPE i,
          lv_prev_val      TYPE string.
    DATA: lr_master_row     TYPE lvc_s_data_ref.
    DATA: lv_curr_val  TYPE string,
          lv_curr_pos  TYPE i,
          lv_is_match  TYPE abap_bool,
          lv_match_off TYPE i,
          lv_match_len TYPE i.
    DATA: lv_row_idx TYPE i,
          lv_col_idx TYPE i.
    DATA: ls_row_info       TYPE ts_row_info.
    DATA: lr_style_row      TYPE lvc_s_data_ref.
    DATA: lv_style  TYPE x4,
          lv_style2 TYPE x4.

    FIELD-SYMBOLS: <lt_style_table> TYPE lvc_t_data,
                   <lt_data_table>  TYPE STANDARD TABLE,
                   <ls_style_table> TYPE lvc_s_data,
                   <ls_value>       TYPE ts_val_info,
                   <ls_data_row>    TYPE any,
                   <ls_col>         TYPE ts_col_info,
                   <lv_val>         TYPE any,
                   <ls_style>       TYPE ts_style_table_ref.

    ASSIGN mr_style_table->* TO <lt_style_table>.
    IF <lt_style_table> IS NOT ASSIGNED OR <lt_style_table> IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN mr_data_table->* TO <lt_data_table>.

    DESCRIBE TABLE <lt_data_table> LINES lv_data_row_cnt.
    DESCRIBE TABLE it_columns      LINES lv_col_cnt.

    " Both data dimensions MUST be > 0.
    IF lv_data_row_cnt = 0 OR lv_col_cnt = 0.
      RETURN.
    ENDIF.

    " For subtotal rows -> row_id = -1
    LOOP AT <lt_style_table> ASSIGNING <ls_style_table> WHERE row_id > 0.
      ls_ref-row_id  = <ls_style_table>-row_id.
      ls_ref-col_pos = <ls_style_table>-col_pos.
      GET REFERENCE OF <ls_style_table> INTO ls_ref-data_ref.
      INSERT ls_ref INTO TABLE lt_style_table_ref.
    ENDLOOP.

    LOOP AT it_values ASSIGNING <ls_value>.
      lv_mask = 1 + 2 + 4 + 8.
      lv_merge_mode = <ls_value>-merge_mode BIT-AND lv_mask.

      DO 2 TIMES. " Index 1 = Vertical, Index 2 = Horizontal
        IF sy-index = 1.
          lv_direction_flag = c_merge_mode-merge_vertical.
        ELSE.
          lv_direction_flag = c_merge_mode-merge_horizontal.
        ENDIF.

        lv_mask = <ls_value>-merge_mode BIT-AND lv_direction_flag.
        CHECK lv_mask IS NOT INITIAL.

        " Determine Matrix Bounds Dynamically
        IF lv_direction_flag = c_merge_mode-merge_vertical.
          lv_outer_cnt = lv_col_cnt.
          lv_inner_cnt = lv_data_row_cnt.
        ELSE.
          lv_outer_cnt = lv_data_row_cnt.
          lv_inner_cnt = lv_col_cnt.
        ENDIF.

        DO lv_outer_cnt TIMES.
          lv_outer_idx = sy-index.

          lv_merge_count = 0.
          lv_prev_pos    = 0.
          lv_prev_val    = lc_no_value.
          CLEAR lr_master_row.

          DO lv_inner_cnt TIMES.
            lv_inner_idx = sy-index.

            " DRY Core logic: Assign dimensions
            IF lv_direction_flag = c_merge_mode-merge_vertical.
              lv_row_idx = lv_inner_idx.
              lv_col_idx = lv_outer_idx.
            ELSE.
              lv_row_idx = lv_outer_idx.
              lv_col_idx = lv_inner_idx.
            ENDIF.

            CLEAR ls_row_info.
            IF it_rows[] IS NOT INITIAL.
              READ TABLE it_rows WITH TABLE KEY row_id = lv_row_idx INTO ls_row_info.
              IF sy-subrc <> 0.
                lv_merge_count = 0.
                CLEAR lr_master_row.
                CONTINUE.
              ENDIF.
            ENDIF.

            READ TABLE <lt_data_table> INDEX lv_row_idx ASSIGNING <ls_data_row>.
            READ TABLE it_columns      INDEX lv_col_idx ASSIGNING <ls_col>.

            " Not <ls_style_row>-value! because check <lv_val> IS INITIAL
            ASSIGN COMPONENT <ls_col>-column OF STRUCTURE <ls_data_row> TO <lv_val>.
            ASSERT sy-subrc = 0.

            READ TABLE lt_style_table_ref INTO ls_ref
                 WITH TABLE KEY row_id = lv_row_idx col_pos = <ls_col>-col_pos.
            IF sy-subrc <> 0.
              lv_merge_count = 0.
              CLEAR lr_master_row.
              CONTINUE.
            ENDIF.
            lr_style_row = ls_ref-data_ref.

            lv_curr_val = <lv_val>.

            " Can be called multiple times for the same field,
            " so skip clearing the merging info to allow merging based on multiple criteria
*            IF lv_direction_flag = c_merge_mode-merge_vertical.
*              lr_style_row->mergevert  = 0.
*            ELSE.
*              lr_style_row->mergehoriz = 0.
*            ENDIF.

            IF lv_direction_flag = c_merge_mode-merge_vertical.
              lv_curr_pos = lv_row_idx.
            ELSE.
              lv_curr_pos = <ls_col>-col_pos.
            ENDIF.

            lv_is_match = abap_false.

            " Test matrix adjacency matching
            lv_prev_pos_calc = lv_curr_pos - 1.
            IF lv_inner_idx > 1 AND lr_master_row IS BOUND AND lv_prev_pos_calc = lv_prev_pos.
              CASE lv_merge_mode.
                WHEN c_merge_mode-equal.
                  IF lv_curr_val = lv_prev_val.
                    lv_is_match = abap_true.
                  ENDIF.
                WHEN c_merge_mode-value_and_empty.
                  IF lv_curr_val = lv_prev_val OR <lv_val> IS INITIAL.
                    lv_is_match = abap_true.
                  ENDIF.
                WHEN c_merge_mode-contains_pattern.
                  IF lv_curr_val CP <ls_value>-value AND lv_prev_val CP <ls_value>-value.
                    lv_is_match = abap_true.
                  ENDIF.
                WHEN c_merge_mode-regex.
                  FIND REGEX <ls_value>-value IN lv_prev_val MATCH OFFSET lv_match_off MATCH LENGTH lv_match_len.
                  IF sy-subrc = 0 AND lv_match_off = 0 AND lv_match_len = strlen( lv_prev_val ).
                    FIND REGEX <ls_value>-value IN lv_curr_val MATCH OFFSET lv_match_off MATCH LENGTH lv_match_len.
                    IF sy-subrc = 0 AND lv_match_off = 0 AND lv_match_len = strlen( lv_curr_val ).
                      lv_is_match = abap_true.
                    ENDIF.
                  ENDIF.
                WHEN OTHERS.
                  zcx_eui_no_check=>raise_sys_error( iv_message = 'The merge operation is undefined'(e03) ).
              ENDCASE.
            ENDIF.

            IF lv_is_match = abap_true.
              lv_merge_count = lv_merge_count + 1.

              IF lv_direction_flag = c_merge_mode-merge_vertical.
                lr_master_row->mergevert  = lv_merge_count.
              ELSE.
                lr_master_row->mergehoriz = lv_merge_count.
              ENDIF.

              lv_style  = <ls_col>-style  BIT-OR ls_row_info-style.
              lv_style  = lv_style        BIT-OR <ls_value>-style.

              lv_style2 = <ls_col>-style2 BIT-OR ls_row_info-style2.
              lv_style2 = lv_style2       BIT-OR <ls_value>-style2.

              _apply_style( EXPORTING iv_style  = lv_style
                                      iv_style2 = lv_style2
                                      iv_add    = iv_add
                            CHANGING  cs_style  = lr_master_row->* ).

              IF lv_merge_mode = c_merge_mode-value_and_empty.
                lv_prev_val = lv_curr_val.
              ENDIF.
            ELSE.
              " Chain break -> re-assign pointer
              lv_merge_count = 0.
              lr_master_row  = lr_style_row.
              lv_prev_val    = lv_curr_val.
            ENDIF.

            lv_prev_pos = lv_curr_pos.
          ENDDO.
        ENDDO.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
