*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

**********************************************************************
**********************************************************************

CLASS lcl_json_util IMPLEMENTATION.

  METHOD class_constructor.
    FIELD-SYMBOLS <lv_type> LIKE LINE OF mt_xsdboolean.

    " Get all boolean types
    SELECT rollname INTO TABLE mt_xsdboolean
    FROM dd04l
    WHERE ( domname = 'XSDBOOLEAN' OR domname = 'OS_BOOLEAN' ) AND as4local = 'A'.

    " Add text for speed
    LOOP AT mt_xsdboolean ASSIGNING <lv_type>.
      CONCATENATE '\TYPE=' <lv_type> INTO <lv_type>.
    ENDLOOP.

    " Is standard table
    SORT mt_xsdboolean BY table_line.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_salv_util IMPLEMENTATION.
  METHOD _get_grid_from_salv.
    DATA:
      lo_grid_adapter TYPE REF TO cl_salv_grid_adapter,
      lo_fs_adapter   TYPE REF TO cl_salv_fullscreen_adapter,
      lo_root         TYPE REF TO cx_root.

    IF io_salv->model <> if_salv_c_model=>table.
      RAISE EXCEPTION TYPE cx_salv_msg
        EXPORTING
          msgid = '00'
          msgno = '001'
          msgty = 'E'
          msgv1 = 'Incorrect SALV Type'.
    ENDIF.

    TRY.
        lo_grid_adapter ?= io_salv->r_controller->r_adapter.
      CATCH cx_root INTO lo_root.
        "could be fullscreen adaptper
        TRY .
            lo_fs_adapter ?= io_salv->r_controller->r_adapter.
          CATCH cx_root INTO lo_root.
            RAISE EXCEPTION TYPE cx_salv_msg
              EXPORTING
                previous = lo_root
                msgid    = '00'
                msgno    = '001'
                msgty    = 'E'
                msgv1    = 'Check PREVIOUS exception'.
        ENDTRY.
    ENDTRY.

    IF lo_grid_adapter IS NOT INITIAL.
      ro_gui_alv = lo_grid_adapter->get_grid( ).
    ELSEIF lo_fs_adapter IS NOT INITIAL.
      ro_gui_alv = lo_fs_adapter->get_grid( ).
    ELSE.
      RAISE EXCEPTION TYPE cx_salv_msg
        EXPORTING
          msgid = '00'
          msgno = '001'
          msgty = 'W'
          msgv1 = 'Adapter is not bound yet'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_grid_util IMPLEMENTATION.
  METHOD _get_grid_table.
    rr_table = io_alv->mt_outtab.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

CLASS lcl_assert_util IMPLEMENTATION.
  METHOD get_class_name.
    " Let see >=7.02
    SELECT SINGLE clsname INTO rv_class_name
    FROM seoclass
    WHERE clsname = 'CL_ABAP_UNIT_ASSERT'.

    " Let see >=7.00 or even lower
    IF rv_class_name IS INITIAL.
      SELECT SINGLE clsname INTO rv_class_name
      FROM seoclass
      WHERE clsname = 'CL_AUNIT_ASSERT'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
