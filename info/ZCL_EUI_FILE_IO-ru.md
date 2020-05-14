### ZCL_EUI_FILE_IO

Импорт данных во внутреннюю таблицу из Excel или CSV файла с указанием мэппинга и обработкой ошибок

![image](https://user-images.githubusercontent.com/36256417/80778543-34703680-8b82-11ea-9f7a-21a4b7cd6acd.png)


***

#### Пример SE38 -> ZEUI_TEST_EXCEL

---

Наверное многие знают про `CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'` для импорта из Excel<br/>
и `cl_gui_frontend_services=>gui_upload( filetype = 'DAT'` для импорта из CSV.

Но данные методы имеют ряд ограничений:
* `ALSM_EXCEL_TO_INTERNAL_TABLE` работает через OLE (есть проблемы при копировании большого объема данных)
* Оба работают только с Presentation Server
* Нет обработки ошибок в файле и мэппинга данных (Какой столбец куда должен попасть) 

---

#### Особенности реализации 

* Класс **ZCL_EUI_FILE_IO** наследует от ZCL_EUI_FILE. И умеет все что умеет делать родитель<br/>
https://github.com/bizhuka/eui/blob/master/ZCL_EUI_FILE-ru.md

* Для Excel используется класс **CL_FDT_XL_SPREADSHEET** (доступный с 7.02) который лишен проблем с OLE.
https://codezentrale.de/abap-excel-datei-xlsx-in-interne-tabele-laden-cl_fdt_xl_spreadsheet-2/

* CSV в свою очередь поддерживает разные символы для разделения данных и разные кодировки

![image](https://user-images.githubusercontent.com/36256417/80709060-195fe100-8b06-11ea-8405-aa0a83c93a94.png)

* Обработка ошибок в файле (дата или число не правильного формата) происходит с помощю события **MAPPING_ERROR**<br/>
В обработчике можно обработать ошибку и изменить само значение

![image](https://user-images.githubusercontent.com/36256417/80709353-9723ec80-8b06-11ea-9b20-ff9fce0fe3a7.png)

* В таблице мэппинга вы можете указать **column_name** или **column_index**<br/>
Сам мэппинг не обязателен. Если он не указан он будет совпадать с внутренней таблицей, те первый столбец таблицы столбец A, второй B итд 

```abap
     " Field of internal table
     field        TYPE fieldname,

     " Convenient for Excel
     column_name  TYPE char3,

     " More convenient for CSV. Could be filled automatically
     column_index TYPE i,
```

---

Тк данный класс наследует от **ZCL_EUI_FILE**, то ему доступны его методы.<br/>
Это дает возможность писать **цепочки** как и в родителе  

Для загрузки из файла цепочка может выглядеть следующим образом 
```abap
 " Если указать расширение диалоги сохранения(открытия) и метод EXPORT_TO_ITAB будут сразу же настроены
 " На 'xlsx' или 'csv'. Также в конструктор можно передать XSTRING
 new ZCL_EUI_FILE( IV_FILE_NAME = 'xlsx' )->
    " Данный шаг может быть заменен на загрузку из любого источника
    IMPORT_FROM_FILE( )->
    " Сам импорт с указанием обработчика ошибок
    EXPORT_TO_ITAB( io_handler = me)
```

---

#### IMPORT_FROM_ITAB
Импорт из внутренней таблицы(**те выгрузку в файл**) работает наподобие простого отчета<br/>
Для более сложных отчетов лучше использовать https://github.com/bizhuka/xtt

TODO Экспорт нескольких внутренних таблиц в 1 файл без указания шаблона(Для debug)