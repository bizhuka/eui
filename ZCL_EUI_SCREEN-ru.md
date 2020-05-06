## ZCL_EUI_SCREEN

### Менеджер экранов

Если у вас есть глобальный класс, как правильно использовать в нем экраны?\
Согласно рекомендации SAP можно использовать группы функций.\
Но насколько удобен подобный способ? 

![image](https://user-images.githubusercontent.com/36256417/81061683-3887b580-8eee-11ea-8343-b55a1da73fc8.png)

***

### SE38 -> ZEUI_TEST_SCREEN_02

Обычно чтобы настроить экран разработчик:
1. Инициализирует экран данными посредством передачи контекста
    * Заполнение `TABLES: ZSS_SCREEN_0200.` для обычных экранов
    * Заполнение полей `SELECTION-SCREEN BEGIN OF SCREEN` для селективных    
1. Выполняет логику PBO 
    * `SET PF-STATUS`, `SET TITLEBAR`
    * `AT SELECTION-SCREEN OUTPUT. -> pbo( ) -> LOOP AT SCREEN`    
1. Далее идет проверка ввода пользователя
    * `AT SELECTION-SCREEN. -> pai( ) -> MESSAGE TYPE 'E'`    
1. После показа идет дальнейшая обработка введенных данных
    * `DATA(lv_value) = ZSS_SCREEN_0200-FIELD_01.`

Если вы используете глобальные классы, вам наверное знакомо ощущение всей архаичности такого способа\
Рассмотрим каждый шаг по отдельности
    
---

### 1) Передача контекста

Будь то web-dynpro, селективные или обычные экраны, удобнее всего использовать структуру для описания экрана.\
В нем можно указать правильный элемент данных и текстом, средство поиска итп.

То есть можно сказать структура и есть ваш экран. **структура=экран**

Для этого классу **ZCL_EUI_SCREEN** зачастую достаточно передать такую структуру\
Она будет содержать наименование полей и начальные значения
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

Если вам нужно просто передать начальные значения в экран заполните **ir_context**
```abap
    DATA(lo_scr_1020) = NEW zcl_eui_screen(
        ir_context = new ts_context( p_bukrs = '1000' )
    ).
```   
         
Для пункта **4)** после закрытия экрана, через контекст можно получить введенные пользователям значения\
Те по своей сути ir_context работает как **CHANGING** параметр

```abap
    DATA(lr_context) = new ts_context( p_bukrs = '1000' ).

    " Передача контекста
    DATA(lo_scr_1020) = NEW zcl_eui_screen(
        ir_context = lr_context ).

    " Если пользователь нажал OK
    check lo_scr_1020->show( ) = 'OK'.

    " Получаем результат
    DATA(lv_new_bukrs) = lr_context->p_bukrs.
```


### 1.1) Номер экрана параметр IV_DYNNR

Если вы знакомы с двумя распространенными техниками SAP,\
Вам не надо объявлять экран через `SELECTION-SCREEN BEGIN OF SCREEN` в программе, просто используйте:

* CALL FUNCTION **'FREE_SELECTIONS_DIALOG'**
    * `new zcl_eui_screen( iv_dynnr = zcl_eui_screen=>mc_dynnr-free_sel )`    
* **CL_CI_QUERY_ATTRIBUTES**=>GENERIC( )
    * `new zcl_eui_screen( iv_dynnr = zcl_eui_screen=>mc_dynnr-dyn_popup )`
* Если вы хотите использовать свой, уже объявленный в другой программе, экран. Укажите его в конструкторе
    * `new zcl_eui_screen( iv_dynnr = '1020'  iv_cprog = 'ZEUI_TEST_SCREEN_02' )`
    
* Режим iv_dynnr = zcl_eui_screen=>mc_dynnr-**auto_gen** генерирует подобно CL_CI_QUERY_ATTRIBUTES программу, но не сохраняет ее тк **INSERT REPORT** обычно запрещен
***

### 2) PBO
**2.1** Для использования кастомного PF-STATUS и TITLEBAR передайте их в конструктор

```abap
    " PF-STATUS & SET TITELEBAR 
    DATA(lo_scr_1020) = NEW zcl_eui_screen(
        iv_status_prog  = c_cprog
        iv_status_name  = 'START_STATUS'
        iv_status_title = 'Start screen' )
```

**2.2** LOOP AT SCREEN

Часто в PBO данный блок является довольно запутанным\
На идею настройки SCREEN через параметры увидел тут https://entropii.net/?p=2927\
Смысл такого синтаксиса надеюсь понятен и без объяснения, так как параметры соответствуют полям **SCREEN**

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

### 3) Проверка ввода пользователя
Как и все дочерние классы **ZCL_EUI_MANAGER** обработчик для **PAI** или PBO (если screen->customize( ) не хватает) можно передать в метод SHOW

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

Handler ON_START_PAI принимает код функции **IV_COMMAND** и может закрыть текущий экран __CV_CLOSE->* = 'X'__ 

![image](https://user-images.githubusercontent.com/36256417/81135290-f3589780-8f70-11ea-8767-f66fd56b1c55.png)


Так как все 4 метода имеют одинаковую сигнатуру, то и выполняемые ими действия имеют сходный характер\
Так после закрытия экрана, метод SHOW всегда возвращает последний код функции. Зачастую достаточно проверить его на 'OK'

```abap
    " If pressed OK
    CHECK lo_screen->show( ) = 'OK'.
``` 

### 4) Обработка введенных данных
Про пункт 4) уже было написано выше что контекст можно передать (и потом получить) через параметр **ir_context**.\
После закрытия экрана он будет содержать введенные данные.

```abap
    DATA(lo_scr_1020) = NEW zcl_eui_screen(
        ir_context = new ts_context( p_bukrs = '1000' )
    ).
```

Обычно для этого подходит ссылка на **локальную** структуру.\
Чтобы получить ее и прочитать текущий контекст в PAI нужно воспользоваться методом **get_context**\
Предварительно ссылку **sender** надо преобразовать на **ZCL_EUI_SCREEN**

```abap
METHOD on_start_pai.  
  " Структура основного экрана
  DATA lr_context TYPE TS_CONTEXT_MAIN.

  " Доступ к данным (чтение) экрана
  data(lo_screen) = cast ZCL_EUI_SCREEN( sender ).
  lr_context ?= lo_screen->get_context( ).

  " Для примера
  IF lr_context->FIELD_01 IS INITIAL.
    MESSAGE 'Field is initial' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  cv_close->* = abap_true.
ENDMETHOD.
```