REPORT  zeui_test_crc64.

TYPE-POOLS:
 abap,
 col.

PARAMETERS:
 p_log AS CHECKBOX DEFAULT 'X'.

**********************************************************************
**********************************************************************
CLASS lcl_data DEFINITION FINAL
   " access to private attributes
   FRIENDS zcl_eui_crc64.

  PUBLIC SECTION.
    METHODS:
     constructor.

    DATA mv_a TYPE string.

  PRIVATE SECTION.
    TYPES:
     BEGIN OF ts_row,
       fld1 TYPE char1,
       fld2 TYPE char1,
     END OF ts_row.
    DATA lt_table TYPE STANDARD TABLE OF ts_row WITH DEFAULT KEY.
ENDCLASS.

CLASS lcl_data IMPLEMENTATION.
  METHOD constructor.
    mv_a = `A`.

    DATA lr_row TYPE REF TO ts_row.
    APPEND INITIAL LINE TO lt_table REFERENCE INTO lr_row.
    lr_row->fld1 = 'T'.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
     start_of_selection.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD start_of_selection.
    DO 2 TIMES.
      DATA lv_index TYPE syindex.
      lv_index = sy-index.

      DATA lo_data TYPE REF TO lcl_data.
      CREATE OBJECT lo_data.

      DATA lo_crc64 TYPE REF TO zcl_eui_crc64.
      CREATE OBJECT lo_crc64
        EXPORTING
          iv_log = p_log.
      lo_crc64->add_to_hash( lo_data ).

      CASE lv_index.
        WHEN 1.
          DATA lv_hash1 TYPE char16.
          lv_hash1 = lo_crc64->get_hash( ).
        WHEN 2.
          DATA lv_hash2 TYPE char16.
          lv_hash2 = lo_crc64->get_hash( ).
      ENDCASE.

      CHECK lv_index = 2.
      IF lv_hash1 = lv_hash2.
        WRITE / `The hash is the same` COLOR COL_POSITIVE.
      ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
INITIALIZATION.
  DATA go_report TYPE REF TO lcl_report.
  CREATE OBJECT go_report.

START-OF-SELECTION.
  go_report->start_of_selection( ).
