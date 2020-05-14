## ZCL_EUI_ALV

### ALV внутри другого ALV
Как показать консультанту что сумма рассчитана правильно?

Часто само написание отчета занимает немного времени, но ~~дальнейшая "Разборка в Бронксе"~~ совместный поиск ошибки занимает намного больше физических и ментальных усилий, как разработчика так и постановщика задачи.

Один из самых простых способов - это показать в popup ALV из чего сложилась данная сумма.<br/>
Те предположим что отчет с collect-ил сумму в основной таблице, при drilldown показываем все позиции из чего данную сумму собрали   

![](https://raw.githubusercontent.com/wiki/bizhuka/py_demo/src/alv_0.png)

***

### Пример SE38 -> ZEUI_TEST_ALV

### Пример SE38 -> ZR_PY000_DEMO
Подробнее можно посмотреть тут
* https://github.com/bizhuka/py_demo
* https://github.com/bizhuka/py_demo/wiki

---

Азы и Буки большинства ABAP-ра наверное классы **CL_GUI_ALV_GRID** и **CL_SALV_TABLE**

Хоть и последний является более новым, он:
* по умолчанию не поддерживает  редактирование (есть конечно же несколько видов трюков как это можно обойти)
* настройка field catalog, toolbar, layout и variant происходит через вызовы методов<br/>
те имхо CL_SALV_TABLE более многословный чем CL_GUI_ALV_GRID (начиная с 7.40) где вся настройка ALV происходит, по старинке, через таблицы и структуры 

CL_GUI_ALV_GRID в свою очередь имеет 1 но очень существенный недостаток - рисовать скрины для основной и popup таблицы ~~любят делать мазохисты~~ весьма утомительное занятие для и без того загруженного работой программиста ~~между сном и просмотрами сериальчика во время обеда~~

***

Для простых случаев весь показ ALV можно написать почти 1 строкой

```abap
 NEW zcl_eui_alv( ir_table = REF #( mt_alv ) )->
    popup( )->
    show( ).
```
Написано в 3 строки для ясности<br/>
Да и то что *можно* так писать, не означает что так *нужно* делать

***

### Более подробно

Сразу перейдем к синтаксису 7.40

#### 1) CREATE
```abap
      " Create new ALV
      DATA(lo_alv) = NEW zcl_eui_alv(
       " Данные для проваливания
       ir_table       = REF #( lt_rt )

       " Что за виды оплат используем
       it_filter      = VALUE LVC_T_FILT( ( fieldname = 'LGART' SIGN = 'I' OPTION = 'EQ' LOW = ... ) )

       " Поставим поле сумма ближе к началу
       it_mod_catalog = VALUE LVC_T_FCAT( ( fieldname = 'BETRG' col_pos = 5 do_sum = abap_true ) )

       " В шапке табельный номер + тех информация
       is_layout      = VALUE LVC_S_LAYO(
          grid_title = |{ <ls_alv>-pernr } - { <ls_lgart>-name } ({ <ls_lgart>-label })|
          smalltitle = abap_true )

       " Если данных много лучше дополнительно сгруппировать данные
       it_sort        = VALUE LVC_T_SORT(
         ( fieldname = 'SRTZA' subtot = abap_true expa = abap_true )
         ( fieldname = 'LGART' subtot = abap_true expa = abap_true ) ) ).
```

Для тех кто помнит не только REUSE, но и вывод отчетов с помощью `WRITE` + COLOR + HOTSPOT события ~~тот старый пердун~~ все думаю понятно.<br/>
А так однократное проваливание в LVC_S_LAYO и DISVARIANT (вариант) и двукратное проваливание LVC_T_FILT, LVC_T_FCAT, TTB_BUTTON (toolbar) и LVC_T_SORT снимет большинство вопросов про то как это работает.

Параметр **IT_MOD_CATALOG** не собранный с нуля field catalog! Он просто дополняет его не пустыми значениями. Также для удобства можно указывать маску для полей<br/>
`fieldname = 'SUM*' do_sum = 'X' hotspot = 'X'`

---

#### 2) POPUP
Как и в SALV если popup не нужен ничего не делаем, иначе вызываем

```abap
      " As popup
      lo_alv->popup( ).
```
В него можно передать размеры окна
* IV_COL_BEG
* IV_COL_END
* IV_ROW_BEG
* IV_ROW_END 

Собственно ради этого метода и создавался данный класс (вложенность popup ограничена 7 экранами) 

---

#### 3) SHOW 
Вызываем сам показ ALV

```abap
      " show ALV
      lo_alv->show( ).
```

Данный метод возвращает код функции закрытия. К примеру если `check lo_alv->show( ) = 'OK'.`<br/>
PF-STATUS и TITLE BAR можно указать статический в конструкторе или динамический по событию
 
```abap
      on_pbo_event FOR EVENT pbo_event OF zif_eui_manager
        IMPORTING
            sender    "<-- CAST to ZCL_EUI_ALV
            iv_dynnr.
```
В данном событии можно получить сам контрол **CL_GUI_ALV_GRID** вызвав `ZCL_EUI_ALV->GET_GRID( )`

Но в большинстве случаев в **SHOW** можно передать объект **io_handler** который может иметь обработчики события CL_GUI_ALV_GRID:
* on_user_command
* on_hotspot_click
* on_double_click
* on_toolbar
* on_top_of_page
* on_data_changed

В них **sender** это и есть CL_GUI_ALV_GRID.

---

### Редактирование

Последний handler **on_data_changed** нужен только для случая когда редактируется GRID LVC_S_LAYO-EDIT = 'X' или отдельное поле LVC_S_FCAT-EDIT

Проверку введенных данных можно осуществить в методе
```abap
      on_pai_event FOR EVENT pai_event OF zif_eui_manager
        IMPORTING
            iv_command
            cv_close. " Установите cv_close->* = abap_false для отмены закрытия
```

Если двух кнопок 'OK' и 'CANCEL' не достаточно (только CANCEL для режима READ_ONLY)

*небольшая ремарка*
* можно поменять статус по событию **on_pbo_event** (для динамики)
* Или 1 раз в конструкторе (при статике)
  * IV_STATUS_NAME
  * IV_STATUS_PROG
  * IT_STATUS_EXCLUDE
  * IV_STATUS_TITLE