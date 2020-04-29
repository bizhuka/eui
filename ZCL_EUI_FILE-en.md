## ZCL_EUI_FILE

Example decomposition of the **DOWNLOAD** method or thinking in OOP within 1 day

***

### Example SE38 -> ZEUI_TEST_EXCEL

---

If you have a method with a dozen optional parameters you worth ~~start smoking more often~~ divide it into several smaller and more understandable methods

Once upon a time there was a simple method **DOWNLOAD** for uploading files.\
It took 2 parameters __IT_TABLE__ and its length __IV_LENGTH__.\
He was not at all complicated, until unloading from IV_**X**STRING was needed.\
And then from the usual line __IV_STRING__ + encoding __IV_ENCODING__ (utf-8 by default)

I decided that it would be nice to open the file after downloading __IV_OPEN__\
Yes, and sometimes it was necessary to open it through __OLE__ (+ 1 parameter) to call the macro.\
Then the optimization was made for downloading files via FTP for files over 10 megabytes\
Everything was fine, but then sometimes the files needed to be downloaded to a folder specified by the user\
or have a permanent name\
or generate random one to upload to **sap_tmp**

---

As a result **DOWNLOAD** inside it looked like a roll of cheap shuttle paper from the supermarket and I wanted to use it less and less   

Gathering the will into a fist ~~and firstly the monitor has been wiped with alcohol fume after a recent corporate party~~ it was decided to finally divide it into smaller parts

---

Firstly holding a file is easiest in one XSTRING attribute (and pass it in the constructor)

![image](https://user-images.githubusercontent.com/36256417/80464453-40c27c80-8953-11ea-99ae-545095f7c6aa.png)

But you can change MV_XSTRING from different sources

![image](https://user-images.githubusercontent.com/36256417/80464724-a44caa00-8953-11ea-88e7-85dd847929af.png)

each method returns itself as a result

![image](https://user-images.githubusercontent.com/36256417/80464964-f7bef800-8953-11ea-868b-1b9ce63cc315.png)

This makes it possible to call the following **DOWNLOAD** (or SHOW) method as a chain

![image](https://user-images.githubusercontent.com/36256417/80465232-5be1bc00-8954-11ea-9d4b-af93f9ba04e8.png)

If you do not need to do anything after downloading, the call chain breaks, otherwise you can call one of the following to open the file

![image](https://user-images.githubusercontent.com/36256417/80465722-f3dfa580-8954-11ea-981a-ed28670df1a2.png) 

---

As a result, if you need to download a file from the internal table, call the Save file dialogue and then open it\
a **chain** will look like

```abap
 new ZCL_EUI_FILE( )->
    IMPORT_FROM_ITAB( )->
    DOWNLOAD( IV_SAVE_DIALOG = 'X' )->
    OPEN( )
```

If you need to download data from STRING in UTF-16LE encoding, download the file to a specific folder and open it through Excel

```abap
 new ZCL_EUI_FILE( )->
    IMPORT_FROM_STRING(  IV_ENCODING = utf_16be )->
    DOWNLOAD( IV_FULL_PATH = ... )->
    OPEN_BY_OLE( )
```

If at the end you need to show it inside the SAP GUI inplace, change DOWNLOAD( ) to **SHOW( )**

```abap
 new ZCL_EUI_FILE( )->
    IMPORT_FROM_STRING(  IV_ENCODING = utf_16be )->
    SHOW( )
```

Each of the steps can ~~throw~~ ~~raise~~ cause an exception, therefore it is better to wrap it in **TRY**\
That is 1 exception handler for the entire call chain
```abap
    TRY.
        new ZCL_EUI_FILE( )->IMPORT_FROM_STRING(  IV_ENCODING = utf_16be )->DOWNLOAD( IV_FULL_PATH = ... )->OPEN_BY_OLE( ).
    CATCH zcx_eui_exception INTO DATA(lo_error).
        MESSAGE lo_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY. 
```
