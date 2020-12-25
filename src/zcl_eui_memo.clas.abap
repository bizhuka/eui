class ZCL_EUI_MEMO definition
  public
  inheriting from ZCL_EUI_MANAGER
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IR_TEXT type ref to STRING
      !IV_READ_ONLY type ABAP_BOOL optional .

  methods ZIF_EUI_MANAGER~PAI
    redefinition .
  methods ZIF_EUI_MANAGER~PBO
    redefinition .
protected section.
private section.

  data MR_TEXT type ref to STRING .
  data MO_TEXTEDIT type ref to CL_GUI_TEXTEDIT .
ENDCLASS.



CLASS ZCL_EUI_MEMO IMPLEMENTATION.


METHOD constructor.
  super->constructor( iv_read_only = iv_read_only ).

  mr_text      = ir_text.
ENDMETHOD.


METHOD zif_eui_manager~pai.
  FIELD-SYMBOLS <lv_text> TYPE string.

  super->pai(
   EXPORTING
    iv_command = iv_command
   CHANGING
    cv_close   = cv_close ).

  " Write data back
  CASE iv_command.
    WHEN zif_eui_manager=>mc_cmd-ok.
      " Destination
      ASSIGN mr_text->* TO <lv_text>.

      mo_textedit->get_textstream(
       IMPORTING
         text = <lv_text> ).
      cl_gui_cfw=>flush( ).

      MESSAGE 'Text copied back' TYPE 'S'.

    WHEN zif_eui_manager=>mc_cmd-cancel.
      MESSAGE 'Text editing is cancelled!' TYPE 'S' DISPLAY LIKE 'W'.
  ENDCASE.
ENDMETHOD.


METHOD zif_eui_manager~pbo.
  DATA lv_mode              TYPE i.
  FIELD-SYMBOLS <lv_text>   TYPE string.

  " Initialize 1 time
  IF io_container IS NOT INITIAL.
    " Text editor
    CREATE OBJECT mo_textedit
      EXPORTING
        parent = io_container
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    " Update data
    ASSIGN mr_text->* TO <lv_text>.
    mo_textedit->set_textstream( <lv_text> ).
    IF mv_read_only = abap_true.
      lv_mode = cl_gui_textedit=>true.
    ELSE.
      lv_mode = cl_gui_textedit=>false.
    ENDIF.
    mo_textedit->set_readonly_mode( lv_mode ).
  ENDIF.

  super->pbo(
   io_container  = io_container
   iv_set_status = iv_set_status  ).
ENDMETHOD.
ENDCLASS.
