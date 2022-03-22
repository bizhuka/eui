*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zeui_test_menu.

TYPE-POOLS:
 abap,
 icon.

SELECTION-SCREEN BEGIN OF BLOCK bl_grp WITH FRAME.
PARAMETERS:
  p_scr0 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK bl_grp.

**********************************************************************
**********************************************************************
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF mc_func,
        but_01           TYPE stb_button-function VALUE 'BUT_01',
        but_gos_sub_menu TYPE stb_button-function VALUE 'BUT_GOS_HIDE_SHOW',
        but_gos_hide     TYPE stb_button-function VALUE 'BUT_GOS_HIDE',
        but_gos_show     TYPE stb_button-function VALUE 'BUT_GOS_SHOW',
      END OF mc_func.

    DATA:
      mt_menu      TYPE zcl_eui_menu=>tt_menu,

      mo_gos_menu  TYPE REF TO zcl_eui_menu,
      mo_scr0_menu TYPE REF TO zcl_eui_menu.

    METHODS:
      constructor,

      start_of_selection,

      on_function_selected FOR EVENT function_selected OF cl_gui_toolbar
        IMPORTING
          sender
          fcode.
ENDCLASS.


**********************************************************************
**********************************************************************
CLASS lcl_report IMPLEMENTATION.
  METHOD constructor.
    FIELD-SYMBOLS:
       <ls_button> LIKE LINE OF mt_menu.

    APPEND INITIAL LINE TO mt_menu ASSIGNING <ls_button>.
    <ls_button>-function = mc_func-but_01.
    <ls_button>-icon     = icon_complete.

    " Delimiter
    APPEND INITIAL LINE TO mt_menu ASSIGNING <ls_button>.
    <ls_button>-butn_type = cntb_btype_sep.

    """""""""""""""""""""""
    " Sub menu
    """""""""""""""""""""""
    APPEND INITIAL LINE TO mt_menu ASSIGNING <ls_button>.
    <ls_button>-function  = mc_func-but_gos_sub_menu.
    <ls_button>-icon      = icon_okay.
    <ls_button>-butn_type = cntb_btype_menu.

    APPEND INITIAL LINE TO mt_menu ASSIGNING <ls_button>.
    <ls_button>-par_function = mc_func-but_gos_sub_menu.
    <ls_button>-function     = mc_func-but_gos_hide.
    <ls_button>-text         = 'Hide gos menu'.

    APPEND INITIAL LINE TO mt_menu ASSIGNING <ls_button>.
    <ls_button>-par_function = mc_func-but_gos_sub_menu.
    <ls_button>-function     = mc_func-but_gos_show.
    <ls_button>-text         = 'Show gos menu'.

    " If no container passed use GOS container
    CREATE OBJECT mo_gos_menu
      EXPORTING
        io_handler = me. " Set handler without calling ->GET_TOOLBAR( )

    " Gos menu
    mo_gos_menu->create_toolbar(
     it_menu    = mt_menu ).

    SET TITLEBAR 'TITLE_BAR' WITH 'Test menu'.
  ENDMETHOD.

  METHOD start_of_selection.
    WRITE / 'Creating new menu ...'.
    CHECK p_scr0 = abap_true.

    " Create same menu
    CREATE OBJECT mo_scr0_menu
      EXPORTING
        io_container = cl_gui_container=>screen0
        io_handler   = me. " Same event handler

    mo_scr0_menu->create_toolbar(
     it_menu    = mt_menu ).
  ENDMETHOD.

  METHOD on_function_selected.
    DATA:
      lo_container TYPE REF TO cl_gui_container.

    CASE fcode.
      WHEN mc_func-but_01.
        IF sender = mo_gos_menu->get_toolbar( ).
          MESSAGE 'Gos menu pressed!' TYPE 'S'.
          RETURN.
        ENDIF.

        " Just if have several menus and only 1 shared event handler
        IF mo_scr0_menu IS NOT INITIAL AND sender = mo_scr0_menu->get_toolbar( ).
          MESSAGE 'cl_gui_container=>screen0 menu pressed!' TYPE 'S'.
          RETURN.
        ENDIF.

      WHEN mc_func-but_gos_hide OR mc_func-but_gos_show.
        " Get container
        lo_container = mo_gos_menu->get_container( ).

        " Invert
        IF fcode = mc_func-but_gos_hide.
          lo_container->set_visible( abap_false ).
        ELSE.
          lo_container->set_visible( abap_true ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.


**********************************************************************
**********************************************************************
DATA:
  go_report     TYPE REF TO lcl_report.                     "#EC NEEDED

INITIALIZATION.
  CREATE OBJECT go_report.

START-OF-SELECTION.
  go_report->start_of_selection( ).
