FUNCTION-POOL zfg_eui_screen.              "MESSAGE-ID ..

* INCLUDE LZHT_FUGR_SCREEND...               " Local class definition


TYPE-POOLS:
  abap,
  icon.

DATA:
  " OK_CODE
  gv_ok_code         TYPE syucomm,

  " Screen areas
  gv_screen_prog_00  TYPE syrepid,  gv_screen_dynnr_00 TYPE sydynnr,
  gv_screen_prog_01  TYPE syrepid,  gv_screen_dynnr_01 TYPE sydynnr,
  gv_screen_prog_02  TYPE syrepid,  gv_screen_dynnr_02 TYPE sydynnr,
  gv_screen_prog_03  TYPE syrepid,  gv_screen_dynnr_03 TYPE sydynnr,
  gv_screen_prog_04  TYPE syrepid,  gv_screen_dynnr_04 TYPE sydynnr,
  gv_screen_prog_05  TYPE syrepid,  gv_screen_dynnr_05 TYPE sydynnr,
  gv_screen_prog_06  TYPE syrepid,  gv_screen_dynnr_06 TYPE sydynnr,
  gv_screen_prog_07  TYPE syrepid,  gv_screen_dynnr_07 TYPE sydynnr.

" Free selection dialog
DATA go_free_manager TYPE REF TO zif_eui_manager.

**********************************************************************
**********************************************************************
CLASS lcl_stack DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      " Current screen 100+
      dynnr     TYPE sydynnr,

      " Responsible 4 pbo & pai
      manager   TYPE REF TO zif_eui_manager,

      " Current container
      container TYPE REF TO cl_gui_custom_container.

    CLASS-DATA:
      " Stack of screens
      mt_stack       TYPE STANDARD TABLE OF REF TO lcl_stack,
      mv_dynnr_index TYPE n LENGTH 4 VALUE 99.

    CLASS-METHODS:
      push_stack
        IMPORTING
          io_manager TYPE REF TO zif_eui_manager,

      get_stack
        IMPORTING
                  iv_check        TYPE abap_bool DEFAULT abap_true
        RETURNING VALUE(ro_stack) TYPE REF TO lcl_stack,

      pop_stack,

      pbo_0700,

      pai_0700
        CHANGING
          cv_cmd TYPE syucomm,

      pbo_0100
        IMPORTING
          iv_create_cont TYPE abap_bool.
ENDCLASS.

CLASS lcl_stack IMPLEMENTATION.
  METHOD push_stack.
    DATA lo_stack   TYPE REF TO lcl_stack.
    DATA lv_dynnr   TYPE sydynnr.
    DATA lv_col_beg TYPE i.
    DATA lv_row_beg TYPE i.
    DATA lv_col_end TYPE i.
    DATA lv_row_end TYPE i.
    DATA lv_shift   TYPE i.

    " ### NO_CALL
    IF io_manager->ms_screen-dynnr = zcl_eui_screen=>mc_dynnr-free_sel OR
       io_manager->ms_screen-dynnr = zcl_eui_screen=>mc_dynnr-dyn_popup.
      go_free_manager = io_manager.
      RETURN.
    ENDIF.

    " Next screen
    ADD 1 TO mv_dynnr_index.
    CREATE OBJECT lo_stack.

    lo_stack->dynnr   = mv_dynnr_index.
    lo_stack->manager = io_manager.
    INSERT lo_stack INTO TABLE mt_stack.

    " Show screen
    lv_dynnr = lo_stack->dynnr + 600.
    IF io_manager->ms_popup-col_beg IS INITIAL.
      CALL SCREEN lv_dynnr.
    ELSE.
      lv_col_beg = io_manager->ms_popup-col_beg.
      lv_row_beg = io_manager->ms_popup-row_beg.
      lv_col_end = io_manager->ms_popup-col_end.
      lv_row_end = io_manager->ms_popup-row_end.

      " Just to see better next screen
      IF io_manager->ms_popup-no_shift <> abap_true AND mv_dynnr_index > 100.
        lv_shift = mv_dynnr_index  - 100.
        ADD lv_shift TO:
          lv_col_beg, lv_row_beg,
          lv_col_end, lv_row_end.
      ENDIF.

      CALL SCREEN lv_dynnr STARTING AT lv_col_beg lv_row_beg
                           ENDING   AT lv_col_end lv_row_end.
    ENDIF.
  ENDMETHOD.

  METHOD get_stack.
    DATA lv_index TYPE i.
    DATA lv_dynnr TYPE num4.

    lv_index = lines( mt_stack ).
    READ TABLE mt_stack INTO ro_stack INDEX lv_index.
    IF sy-subrc <> 0.
      zcx_eui_exception=>raise_dump( iv_message = 'No current stack ?' ).
    ENDIF.

    " Screen index
    lv_dynnr = sy-dynnr.
    IF lv_dynnr >= 700.
      lv_dynnr = lv_dynnr  - 600.
    ENDIF.

    IF iv_check = abap_true AND ( mv_dynnr_index <> lv_dynnr OR mv_dynnr_index <> ro_stack->dynnr ).
      zcx_eui_exception=>raise_dump( iv_message = 'Do not call LEAVE SCREEN 0 by code. Use CV_CLOSE in PAI!' ).
    ENDIF.
  ENDMETHOD.

  METHOD pop_stack.
    DATA lo_stack TYPE REF TO lcl_stack.
    lo_stack = get_stack( ).

    " delete container
    DO 1 TIMES.
      " ?
      IF lo_stack->manager->ms_screen-prog IS INITIAL.
        zcx_eui_exception=>raise_dump( iv_message = 'The SCREEN program is not set' ).
      ENDIF.

      CHECK lo_stack->manager->ms_screen-prog = sy-repid.

      IF lo_stack->container IS INITIAL.
        zcx_eui_exception=>raise_dump( iv_message = 'Deleting container several times' ).
      ENDIF.

      lo_stack->container->finalize( ).
      lo_stack->container->free( ).
      CLEAR lo_stack->container.
    ENDDO.

    " Delete last item from stack
    DELETE mt_stack WHERE table_line->dynnr = mv_dynnr_index.
    IF sy-subrc <> 0.
      zcx_eui_exception=>raise_dump( iv_message = 'Cannot delete by index' ).
    ENDIF.

    " Previous screen
    SUBTRACT 1 FROM mv_dynnr_index.

    " Close screen (Only HERE)
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD pbo_0700.
    DATA lo_manager   TYPE REF TO zif_eui_manager.
    DATA lo_stack     TYPE REF TO lcl_stack.

    " Get manager
    lo_stack   = get_stack( ).
    lo_manager = lo_stack->manager.

    " Prepare iv_dynnr = sy-dynnr
    lo_manager->pbo(
       " io_container  = lo_stack->container " SEND EMPTY !!!
       iv_set_status = abap_true
    ).

**********************************************************************
    DATA ls_screen               TYPE REF TO zif_eui_manager=>ts_screen.
    DATA lv_name                 TYPE string.
    FIELD-SYMBOLS <lv_scr_prog>  TYPE repid.
    FIELD-SYMBOLS <lv_scr_dynnr> TYPE sydynnr.

    " By default show 100 in 700
    GET REFERENCE OF lo_manager->ms_screen INTO ls_screen.
    IF ls_screen->prog IS INITIAL.
      ls_screen->prog  = sy-repid.
      ls_screen->dynnr = lo_stack->dynnr.
    ENDIF.

    " Get global vars
    CONCATENATE 'GV_SCREEN_PROG_'   sy-dynnr+2(2) INTO lv_name.
    ASSIGN (lv_name) TO <lv_scr_prog>.
    CONCATENATE 'GV_SCREEN_DYNNR_'  sy-dynnr+2(2) INTO lv_name.
    ASSIGN (lv_name) TO <lv_scr_dynnr>.

    " set from manager
    <lv_scr_prog>  = ls_screen->prog.
    <lv_scr_dynnr> = ls_screen->dynnr.
  ENDMETHOD.

  METHOD pbo_0100.
    DATA lo_stack        TYPE REF TO lcl_stack.
    DATA lv_name         TYPE text40.
    DATA lo_container    LIKE lo_stack->container.

    lo_stack = get_stack( iv_check = iv_create_cont ).

    " Create for the first time
    IF lo_stack->container IS INITIAL AND iv_create_cont = abap_true.
      CONCATENATE 'EMPTY_' lo_stack->dynnr INTO lv_name.
      CREATE OBJECT lo_stack->container
        EXPORTING
          container_name = lv_name.

      " Send 1 time only
      lo_container = lo_stack->container.
    ENDIF.

    " Prepare    iv_dynnr = lo_stack->dynnr
    lo_stack->manager->pbo( io_container = lo_container ).
  ENDMETHOD.

  METHOD pai_0700.
    DATA lo_manager TYPE REF TO zif_eui_manager.
    DATA lo_stack   TYPE REF TO lcl_stack.
    DATA lv_ok_cmd  LIKE gv_ok_code.
    DATA lv_close   TYPE abap_bool.

    " Get manager
    lo_stack   = get_stack( ).
    lo_manager = lo_stack->manager.

    lv_ok_cmd = cv_cmd.
    CLEAR cv_cmd.

    lo_manager->pai(
     EXPORTING
      iv_command = lv_ok_cmd
     CHANGING
      cv_close   = lv_close ).

    " Yes close
    CHECK lv_close = abap_true.
    pop_stack( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************

FORM auto_screen_pbo.                                       "#EC CALLED
  " Just call EVENT
  lcl_stack=>pbo_0100( iv_create_cont = abap_false ).
ENDFORM.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM free_screen_pbo TABLES ct_seldyn STRUCTURE rsseldyn
                            ct_fldnum STRUCTURE rsdsfldnum. "#EC CALLED
  go_free_manager->pbo( ).
ENDFORM.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM free_screen_pai TABLES ct_seldyn STRUCTURE rsseldyn
                            ct_fldnum STRUCTURE rsdsfldnum. "#EC CALLED

  DATA lv_close                 TYPE abap_bool.
  FIELD-SYMBOLS <ls_sscrfields> TYPE sscrfields.

  " Current command
  ASSIGN ('(SAPLSSEL)SSCRFIELDS') TO <ls_sscrfields>.
  CHECK <ls_sscrfields> IS ASSIGNED.

  go_free_manager->pai(
   EXPORTING
     iv_command = <ls_sscrfields>-ucomm
   CHANGING
     cv_close   = lv_close ).
ENDFORM.

**********************************************************************
**********************************************************************

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  lcl_stack=>pbo_0100( iv_create_cont = abap_true ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
*  lcl_stack=>pai_0100( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pbo_0700 OUTPUT.
  lcl_stack=>pbo_0700( ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_0700 INPUT.
  lcl_stack=>pai_0700( CHANGING cv_cmd = gv_ok_code ).
ENDMODULE.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE pai_exit INPUT.
*  lcl_stack=>pop_stack( ).
  gv_ok_code = zif_eui_manager=>mc_cmd-cancel.
  lcl_stack=>pai_0700( CHANGING cv_cmd = gv_ok_code ).
ENDMODULE.
