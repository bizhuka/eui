*"* use this source file for your ABAP unit test classes

CLASS lcl_test DEFINITION FOR TESTING FINAL "#AU Risk_Level Harmless
                                          "#AU Duration Short
  .
  PUBLIC SECTION.
    METHODS:
      column_a_to_1 FOR TESTING,
      column_z_to_26 FOR TESTING,
      column_ac_to_29 FOR TESTING,
      column_xfc_to_16383 FOR TESTING,
      lowercase_column FOR TESTING,
      spaced_column FOR TESTING,
      empty_column_to_zero FOR TESTING,
      index_2_to_b FOR TESTING,
      index_25_to_y FOR TESTING,
      index_27_to_aa FOR TESTING,
      index_28_to_ab FOR TESTING,
      max_index_to_xfd FOR TESTING,
      zero_index_rejected FOR TESTING,
      negative_index_rejected FOR TESTING,
      oversized_index_rejected FOR TESTING.

  PRIVATE SECTION.
    METHODS _assert_column_to_int
      IMPORTING
        iv_column   TYPE string
        iv_expected TYPE i.

    METHODS _assert_int_to_column
      IMPORTING
        iv_index    TYPE i
        iv_expected TYPE string.

    METHODS _assert_index_rejected
      IMPORTING
        iv_index TYPE i.
ENDCLASS.

**********************************************************************
**********************************************************************
CLASS lcl_test IMPLEMENTATION.
  METHOD _assert_column_to_int.
    DATA lv_actual TYPE i.
    lv_actual = zcl_eui_file_io=>column_2_int( iv_column ).

    zcl_eui_conv=>assert_equals(
      act = lv_actual
      exp = iv_expected
      msg = |Column { iv_column } should have index { iv_expected }, actual: { lv_actual }| ).
  ENDMETHOD.

  METHOD _assert_int_to_column.
    DATA lv_actual TYPE char3.
    lv_actual = zcl_eui_file_io=>int_2_column( iv_index ).

    zcl_eui_conv=>assert_equals(
      act = lv_actual
      exp = iv_expected
      msg = |Index { iv_index } should have column { iv_expected }, actual: { lv_actual }| ).
  ENDMETHOD.

  METHOD _assert_index_rejected.
    DATA lv_exception_raised TYPE abap_bool.

    TRY.
        zcl_eui_file_io=>int_2_column( iv_index ).
      CATCH zcx_eui_exception.
        lv_exception_raised = abap_true.
    ENDTRY.

    zcl_eui_conv=>assert_equals(
      act = lv_exception_raised
      exp = abap_true
      msg = |Invalid column index { iv_index } was not rejected| ).
  ENDMETHOD.

  METHOD column_a_to_1.
    _assert_column_to_int( iv_column = 'A' iv_expected = 1 ).
  ENDMETHOD.

  METHOD column_z_to_26.
    _assert_column_to_int( iv_column = 'Z' iv_expected = 26 ).
  ENDMETHOD.

  METHOD column_ac_to_29.
    _assert_column_to_int( iv_column = 'AC' iv_expected = 29 ).
  ENDMETHOD.

  METHOD column_xfc_to_16383.
    _assert_column_to_int( iv_column = 'XFC' iv_expected = 16383 ).
  ENDMETHOD.

  METHOD lowercase_column.
    _assert_column_to_int( iv_column = 'ad' iv_expected = 30 ).
  ENDMETHOD.

  METHOD spaced_column.
    _assert_column_to_int( iv_column = ' A E ' iv_expected = 31 ).
  ENDMETHOD.

  METHOD empty_column_to_zero.
    _assert_column_to_int( iv_column = '' iv_expected = 0 ).
  ENDMETHOD.

  METHOD index_2_to_b.
    _assert_int_to_column( iv_index = 2 iv_expected = 'B' ).
  ENDMETHOD.

  METHOD index_25_to_y.
    _assert_int_to_column( iv_index = 25 iv_expected = 'Y' ).
  ENDMETHOD.

  METHOD index_27_to_aa.
    _assert_int_to_column( iv_index = 27 iv_expected = 'AA' ).
  ENDMETHOD.

  METHOD index_28_to_ab.
    _assert_int_to_column( iv_index = 28 iv_expected = 'AB' ).
  ENDMETHOD.

  METHOD max_index_to_xfd.
    _assert_int_to_column( iv_index = 16384 iv_expected = 'XFD' ).
  ENDMETHOD.

  METHOD zero_index_rejected.
    _assert_index_rejected( 0 ).
  ENDMETHOD.

  METHOD negative_index_rejected.
    _assert_index_rejected( -1 ).
  ENDMETHOD.

  METHOD oversized_index_rejected.
    _assert_index_rejected( 16385 ).
  ENDMETHOD.
ENDCLASS.
