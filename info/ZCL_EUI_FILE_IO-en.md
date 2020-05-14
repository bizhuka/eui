### ZCL_EUI_FILE_IO

Import data into an internal table from Excel or CSV file with mapping and error handling

![image](https://user-images.githubusercontent.com/36256417/80778543-34703680-8b82-11ea-9f7a-21a4b7cd6acd.png)

***

#### Example SE38 -> ZEUI_TEST_EXCEL

---

Probably many SAP developers know about `CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'` for import from Excel<br/>
and `cl_gui_frontend_services=>gui_upload( filetype = 'DAT'` for import from CSV.

But these methods have several limitations:
* `ALSM_EXCEL_TO_INTERNAL_TABLE` works by OLE (there are problems when copying a large amount of data)
* Both work only with Presentation Server
* There is no error handling in the file and data mapping (Which column should go where) 

---

#### Implementation features

* Class **ZCL_EUI_FILE_IO** inherits from ZCL_EUI_FILE. And everything that a parent can do<br/>
https://github.com/bizhuka/eui/blob/master/ZCL_EUI_FILE-en.md

* For Excel, the **CL_FDT_XL_SPREADSHEET** class is used (available from 7.02). Which is devoid of problems with OLE.
https://codezentrale.de/abap-excel-datei-xlsx-in-interne-tabele-laden-cl_fdt_xl_spreadsheet-2/

* CSV in turn supports different characters for data separation and different encodings

![image](https://user-images.githubusercontent.com/36256417/80709060-195fe100-8b06-11ea-8405-aa0a83c93a94.png)

* Error handling in the file (date or number has the wrong format) occurs using the **MAPPING_ERROR** event<br/>
In the callback, you can handle the error and change the value itself

![image](https://user-images.githubusercontent.com/36256417/80709353-9723ec80-8b06-11ea-9b20-ff9fce0fe3a7.png)

* In the mapping table, you can specify **column_name** or **column_index**<br/>
Mapping itself is optional. If it is not specified, it will match the internal table that passed, the first column of the ITAB is column A, second B, etc. 

```abap
     " Field of internal table
     field        TYPE fieldname,

     " Convenient for Excel
     column_name  TYPE char3,

     " More convenient for CSV. Could be filled automatically
     column_index TYPE i,
```

---

Since this class inherits from **ZCL_EUI_FILE**, its methods are available to it.<br/>
This makes it possible to write **chains** as in the parent

For loading from a file, the chain may look as follows
```abap
 " If you specify the extension, the save(open) dialogs and the EXPORT_TO_ITAB method will be immediately configured
 " to 'xlsx' or 'csv'. You can also pass XSTRING to the constructor
 new ZCL_EUI_FILE( IV_FILE_NAME = 'xlsx' )->
    " This step can be replaced by data loading from any source
    IMPORT_FROM_FILE( )->
    " Import itself with an error handler
    EXPORT_TO_ITAB( io_handler = me)
```

---

#### IMPORT_FROM_ITAB
Import from the internal table (**or export to a file**) works like a simple report<br/>
For more complex reports, it is better to use https://github.com/bizhuka/xtt

TODO Export multiple internal tables to 1 file without specifying a template (For debug)