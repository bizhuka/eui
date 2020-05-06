## ZCL_EUI_SCREEN

### Screen manager

If you have a global class, how to use screens in it correctly?\
According to the SAP recommendation, you can use function groups.\
But how convenient is this? 

![image](https://user-images.githubusercontent.com/36256417/81061683-3887b580-8eee-11ea-8343-b55a1da73fc8.png)

***

### SE38 -> ZEUI_TEST_SCREEN_02

Usually, to customize the screen, the developer:
1. Initializes the screen with data by passing context
    * Filling `TABLES: ZSS_SCREEN_0200.` for regular screens
    * Filling parameters of `SELECTION-SCREEN BEGIN OF SCREEN` for selection screens    
1. Performs PBO logic
    * `SET PF-STATUS`, `SET TITLEBAR`
    * `AT SELECTION-SCREEN OUTPUT. -> pbo( ) -> LOOP AT SCREEN`    
1. Next is checking user input
    * `AT SELECTION-SCREEN. -> pai( ) -> MESSAGE TYPE 'E'`    
1. After the screen show, further processing of the user entered data
    * `DATA(lv_value) = ZSS_SCREEN_0200-FIELD_01.`

If you use global classes, you probably know the feeling of the archaic nature of this method\
We will consider each step separately.
    
---

### 1) Context transfer

Whether it’s web-dynpro, selection or regular screens, it’s most convenient to use a structure to describe the screen.\
In it, you can specify the correct data element and text, search helps etc.

That is, you can say the structure is your screen. **structure = screen**

For the class **ZCL_EUI_SCREEN** is often sufficient to pass such a structure\
It will contain the name of the fields and the initial values.
```abap
    " Checked SCREEN context
    BEGIN OF ts_context,
      p_bukrs  TYPE bukrs,
      p_bdc_m  TYPE ettcd_mode, " <-- listbox by domain
      p_mandt  TYPE t001-mandt, " <-- listbox in runtime
      p_check  TYPE xsdboolean,
      s_user   TYPE offline_log_user_itab, " Range <-- cl_ci_query_attributes no SH
      p_land1  TYPE t005t-land1,
      p_fld_i  TYPE syindex,       " do not use i! use from dictionary
      p_fld_i2 TYPE sytabix,       " do not use i! use from dictionary
      " p_memo     TYPE stringval, " String & tables also Ok
    END OF ts_context,
```

If you just need to pass the initial values to the screen fill **ir_context** parameter
```abap
    DATA(lo_scr_1020) = NEW zcl_eui_screen(
        ir_context = new ts_context( p_bukrs = '1000' )
    ).
```   
         
For item **4)** after closing the screen, through the context you can get the values entered by a user\
So actually ir_context works like **CHANGING** parameter

```abap
    DATA(lr_context) = new new ts_context( p_bukrs = '1000' ).

    " Context transfer
    DATA(lo_scr_1020) = NEW zcl_eui_screen(
        ir_context = lr_context ).

    " If the user clicked OK
    check lo_scr_1020->show( ) = 'OK'.

    " Get result
    DATA(lv_new_bukrs) = lr_context->p_bukrs.
```


### 1.1) Screen number IV_DYNNR parameter

If you are familiar with two common SAP techniques,\
You do not need to declare the screen through `SELECTION-SCREEN BEGIN OF SCREEN` in the program, just use:

* CALL FUNCTION **'FREE_SELECTIONS_DIALOG'**
    * `new zcl_eui_screen( iv_dynnr = zcl_eui_screen=>mc_dynnr-free_sel )`    
* **CL_CI_QUERY_ATTRIBUTES**=>GENERIC( )
    * `new zcl_eui_screen( iv_dynnr = zcl_eui_screen=>mc_dynnr-auto_gen )`
* Если вы хотите использовать свой, уже объявленный в другой программе, экран. Укажите его в конструкторе
    * `new zcl_eui_screen( iv_dynnr = '1020'  iv_cprog = 'ZEUI_TEST_SCREEN_02' )`
    
***

### 2) PBO
**2.1** To use custom PF-STATUS and TITLEBAR, pass them to the constructor

```abap
    " PF-STATUS & SET TITELEBAR 
    DATA(lo_scr_1020) = NEW zcl_eui_screen(
        iv_status_prog  = c_cprog
        iv_status_name  = 'START_STATUS'
        iv_status_title = 'Start screen' )
```

**2.2** LOOP AT SCREEN

Often in PBO this block is quite confusing\
On the idea of setting SCREEN through the parameters I saw here https://entropii.net/?p=2927\
I hope the meaning of this syntax is clear without explanation, since the parameters correspond to the **SCREEN** fields

##### SE38 -> ZEUI_TEST_SCREEN_00
```abap
  " Gray
  lo_screen->customize( name = 'P_FLD_01' input = '0' ).

  " Obligatory & change text
  lo_screen->customize(
    name     = 'P_FLD_02'
    required = '1'
    iv_label = 'Make required'  ).

  " Hide by mask
  lo_screen->customize(
    name      = '*P_0*'
    input     = '0'
    active    = '0'
    invisible = '1' ).

  " Hide or show by group
  lo_screen->customize(
    group1    = COND #( WHEN p_radio1 = abap_true THEN 'GR1' ELSE 'GR2' )
    input     = '1'
    active    = '1'
    invisible = '0' ).

    " Set listbox
  lo_screen->customize(
    name       = 'P_MANDT'
    required   = '1'
    iv_label   = 'Client number'
    it_listbox = lt_listbox ).
```

***

### 3) User input validation
Like all child classes of **ZCL_EUI_MANAGER** handlers for **PAI** or PBO (if screen->customize( ) is not enough) can be passed to the SHOW method

![image](https://user-images.githubusercontent.com/36256417/81134948-cb1c6900-8f6f-11ea-8182-0d62843492ef.png)

```abap
    " Next screen interation
    lv_cmd = lo_screen_main->show(
     io_handler      = lo_handler
     " For demo purpose
     iv_handlers_map = 'ON_START_PAI' ). " <- Optional (by default call all handlers)

    " Pressed cancel
    IF lv_cmd NP 'CMD_*'.
      RETURN. " Cancel pressed
    ENDIF.
```

Handler ON_START_PAI accepts the function code **IV_COMMAND** and can close the current screen __CV_CLOSE->* = 'X'__ 

![image](https://user-images.githubusercontent.com/36256417/81135290-f3589780-8f70-11ea-8767-f66fd56b1c55.png)


Since all 4 methods have the same signature, the actions they perform are similar\
So after closing the screen, the SHOW method always returns the last function code. Often enough to check it for 'OK'

```abap
    " If pressed OK
    CHECK lo_screen->show( ) = 'OK'.
``` 

### 4) Post Processing
About item 4), it has already been written above that the context can be passed (and then received) through the **ir_context** parameter.\
After closing the screen, it will contain the entered data.

```abap
    DATA(lo_scr_1020) = NEW zcl_eui_screen(
        ir_context = new ts_context( p_bukrs = '1000' )
    ).
```

Обычно для этого подходит ссылка на **локальную** структуру.\
Чтобы получить ее и прочитать текущий контекст в PAI нужно воспользоваться методом **get_context**\
Предварительно ссылку **sender** надо преобразовать на **ZCL_EUI_SCREEN**

Usually a reference to the **local** structure is suitable for this.\
To get it and read the current context in PAI you need to use the **get_context** method\
But firstly the **sender** must be converted to **ZCL_EUI_SCREEN**

```abap
METHOD on_start_pai.  
  " Main Screen Structure
  DATA lr_context TYPE TS_CONTEXT_MAIN.

  " Get access to screen data
  data(lo_screen) = cast ZCL_EUI_SCREEN( sender ).
  lr_context ?= lo_screen->get_context( ).

  " For demo only
  IF lr_context->FIELD_01 IS INITIAL.
    MESSAGE 'Field is initial' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  cv_close->* = abap_true.
ENDMETHOD.
```