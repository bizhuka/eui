interface ZIF_EUI_MANAGER
  public .

  type-pools ABAP .

  types:
    BEGIN OF TS_POPUP,
      COL_BEG  TYPE i,
      COL_END  TYPE i,
      ROW_BEG  TYPE i,
      ROW_END  TYPE i,
      " by default move POPUP right bottom corner by 1
      NO_SHIFT TYPE ABAP_BOOL,
    END OF TS_POPUP .
  types:
    TT_STATUS_EXCLUDE type STANDARD TABLE OF syucomm WITH DEFAULT KEY .
  types:
    BEGIN OF TS_STATUS,
      NAME     TYPE GUI_STATUS,
      PROG     TYPE SYREPID,
      EXCLUDE	 TYPE TT_STATUS_EXCLUDE,

      " Title bar
      TITLE    TYPE STRING,

      " Do not change in PBO
      IS_FIXED TYPE ABAP_BOOL,
    END OF TS_STATUS .
  types:
    BEGIN OF TS_SCREEN,
      PROG     TYPE SYREPID,
      DYNNR	   TYPE SYDYNNR,
    END OF TS_SCREEN .

  constants:
    BEGIN OF MC_CMD,
     OK     TYPE SYUCOMM VALUE 'OK',
     CANCEL TYPE SYUCOMM VALUE 'DCAN',
     RETURN TYPE SYUCOMM VALUE 'CRET',
   END OF MC_CMD .
  data MS_POPUP type TS_POPUP .
  data MS_STATUS type TS_STATUS .
  data MS_SCREEN type TS_SCREEN .

  events PBO_EVENT
    exporting
      value(IO_CONTAINER) type ref to CL_GUI_CONTAINER optional .
  events PAI_EVENT
    exporting
      value(IV_COMMAND) type SYUCOMM
      value(CV_CLOSE) type ref to ABAP_BOOL .

  methods PBO
    importing
      !IO_CONTAINER type ref to CL_GUI_CONTAINER optional
      !IV_SET_STATUS type ABAP_BOOL default ABAP_FALSE .
  methods PAI
    importing
      !IV_COMMAND type SYUCOMM
    changing
      !CV_CLOSE type ABAP_BOOL .
  methods POPUP
    importing
      !IV_COL_BEG type I optional
      !IV_COL_END type I optional
      !IV_ROW_BEG type I optional
      !IV_ROW_END type I optional
      !IV_NO_SHIFT type ABAP_BOOL default ABAP_FALSE
    returning
      value(RO_MANAGER) type ref to ZIF_EUI_MANAGER .
  methods SHOW
    importing
      !IO_HANDLER type ref to OBJECT optional
      !IV_HANDLERS_MAP type CSEQUENCE optional
    returning
      value(RV_CLOSE_CMD) type SYUCOMM .
endinterface.
