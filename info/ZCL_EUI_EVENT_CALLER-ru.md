## ZCL_EUI_EVENT_CALLER

### "Слабое связывание" через хэндлеры

&nbsp;&nbsp;&nbsp;Иногда ООП может довольно сильно усложнить структуру программы. <br/>
Если не идти в дебри проектирования, наследования, паттернов проектирования, лямбда выражений и прочего умного и сложного,
часто требуется просто **вызвать метод**, который **реализован** совсем в другом месте, тем самым разделить код на более мелкие части, которые не сильно связаны между собой.<br/>
Для этого в ABAP есть два основных механизма для слабого связывания классов между собой **интерфейсы** и **события**.<br/>
Но как не городить огород и не усложнять себе жизнь с наследованием и интерфейсами и прочими плюшками ООП?  

![image](https://user-images.githubusercontent.com/36256417/81942205-45ee1f80-9613-11ea-9f95-209d1db5a2df.png)

<br/>
<br/>
<br/>

***

&nbsp;&nbsp;&nbsp; Интерфейсы очень гибкий инструмент для разбиения кода на логические блоки, который позволяет делать код не таким связанным, в отличии от классы.<br/>
Но их имплементация в ABAP, в плане написания кода, дело довольно многословное и не всегда удобное.
Также то обстоятельство что нужно имплементировать каждый метод в интерфейсе (если это только не класс тестирования) вводит дополнительные ограничения на их использование.

К примеру если у вас есть интерфейс по управлению [экранами](ZCL_EUI_SCREEN-ru.md), вам нужно реализовать оба метода **PAI** и **PBO**.<br/>       
Если PBO вам не нужен, вы делаете его пустым и потом еще пишете для ATC что он нужен (так себе удовольствие)

Тогда можно для гибкости объявить их событиями

![image](https://user-images.githubusercontent.com/36256417/81945943-2f969280-9618-11ea-88c9-b2c4abc0c379.png)

И сделать что то вроде
```abap
    " Send container & current screen number
    RAISE EVENT zif_eui_manager~pbo_event
     EXPORTING
       io_container = io_container
       iv_dynnr = sy-dynnr.
...

    " Handler returns cv_close = 'X' to close SCREEN
    RAISE EVENT zif_eui_manager~pai_event
     EXPORTING
       iv_command = sy-ucomm
       cv_close = REF #( lv_close ).
```

Но что делать если вам нужно вызвать событие другого класса?<br/>
К примеру сделать вызов события из локально класса (Class relevant to local definitions), когда он реализует часть логику глобального класса. <br/>
**RAISE EVENT** можно вызывать только собственного класса.

Или к примеру если у вас есть обертка на [ALV](ZCL_EUI_ALV-ru.md) и вы хотите **переслать** событие от CL_GUI_ALV_GRID в другое место, те отловить для примера **on_double_click**, и потом делегировать выполнение другому классу. 

***

#### ZCL_EUI_EVENT_CALLER

&nbsp;&nbsp;&nbsp;Одно из простых решений это вызвать метод обработчика событий через dynamic call.<br/>
Те описание хэндлера можно представить в качестве интерфейса с методом который можно вызвать через CALL METHOD with **PARAMETER-TABLE**.
 
Когда в коде есть описание вызываемого метода, мы зная его сигнатуру можем вызвать его динамический
```abap
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            sender
            e_row_id
            e_column_id,
```

#### Реализация ZCL_EUI_EVENT_CALLER

Для начала нужно знать что за метод нужно вызвать

![image](https://user-images.githubusercontent.com/36256417/81949867-bd747c80-961c-11ea-86fe-32f5f6c5ca7a.png)

* IO_HANDLER - Объект чей метод нужно вызвать 
* IV_HANDLERS_MAP - Если у вас есть несколько обработчиков on_double_click для разных ALV, нужно укзать имя метода 
* IV_FIRST - Вызвать первым? RAISE EVENT не гарантирует порядок вызова
* IV_ACTIVATE - Активировать/Деактивировать

<br/>
<br/>
<br/>

А далее остается просто вызвать обработчики событий с помощью **CALL_HANDLERS**  

![image](https://user-images.githubusercontent.com/36256417/81950696-ba2dc080-961d-11ea-9895-acc26a3f971b.png)

Вместо такой конструкции
```abap
    " Handler returns cv_close = 'X' to close SCREEN
    RAISE EVENT zif_eui_manager~pai_event
     EXPORTING
       iv_command = sy-ucomm
       cv_close = REF #( lv_close ).
```

Вызываем обработчики вот так
```abap
    mo_event_caller->call_handlers(
     iv_of_class     = 'ZIF_EUI_MANAGER'
     iv_for_event    = 'PAI_EVENT'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = me
     iv_param_nam_01 = 'IV_COMMAND'      iv_param_val_01 = iv_command
     iv_param_nam_02 = 'CV_CLOSE'        iv_param_val_02 = lr_close ).
```

Делегируем on_double_click вот так
```abap
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'DOUBLE_CLICK'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_ROW'           iv_param_val_01 = e_row
     iv_param_nam_02 = 'E_COLUMN'        iv_param_val_02 = e_column ).
```

***

Если вы дочитали до сюда и поняли основную идею, жму вашу руку на расстоянии

В результате устанавливать обработчики (или callback методы), можно через 1 параметр **io_handler** который имеет описание всех необходимых хэндлеров

Для **PAI_EVENT** интерфейса ZIF_EUI_MANAGER
```abap
 " Handle of PAI_EVENT
 CHECK mo_screen->show( io_handler = me ) = 'OK'.
```

Для **ON_USER_COMMAND** класса CL_GUI_ALV_GRID
```abap
    " Instead of set handler
    lo_eui_alv->show(
     io_handler        = me

     " If omit map with all (Could be several ON_USER_COMMAND)
     iv_handlers_map   = 'ON_HOTSPOT_CLICK;ON_USER_COMMAND;ON_PBO_EVENT'
    ).
```

***

Пример использования в SE38
* ZEUI_TEST_ALV
* ZEUI_TEST_SCREEN_02