## ZCL_EUI_EVENT_CALLER

### Loose coupling through handlers

&nbsp;&nbsp;&nbsp;Sometimes OOP can complicate the program structure quite a lot. <br/>
If you do not go into the jungle of design, inheritance, design patterns, lambda expressions and other smart and complex things,
often you just need to **call the method**, which **is implemented** in a completely different place, thereby splitting the code into smaller parts that are not tightly coupled.<br/>
To do this, ABAP has two main mechanisms for weakly binding classes to each other **interfaces** and **events**. <br/>
But how not to complicate your life with inheritance and interfaces and other OOP goodies?  

![image](https://user-images.githubusercontent.com/36256417/81942205-45ee1f80-9613-11ea-9f95-209d1db5a2df.png)

<br/>
<br/>
<br/>

***

&nbsp;&nbsp;&nbsp; Interfaces are a very flexible tool for breaking code into logical blocks, which allows you to make code less connected, unlike classes. <br/>
But their implementation in ABAP, in terms of writing code, is quite verbose and not always convenient.
Also, the fact that you need to implement each method in the interface (unless it is a testing class) introduces additional restrictions on their use.

For example, if you have an interface for managing [screens](ZCL_EUI_SCREEN-en.md), you need to implement both **PAI** and **PBO** methods. <br/>
If you do not need a PBO, you make it empty and then write to ATC that it is needed (so-so pleasure)

Then for flexibility you can declare them events

![image](https://user-images.githubusercontent.com/36256417/81945943-2f969280-9618-11ea-88c9-b2c4abc0c379.png)

And do something like
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

But what if you need to trigger an event of another class?<br/>
For example, make an event call from a local class (Class relevant to local definitions) when it implements part of the logic of a global class. <br/>
**RAISE EVENT** can only be called in its own class.

Or for example, if you have a wrapper on [ALV](ZCL_EUI_ALV-en.md) and you want to **forward** the event from CL_GUI_ALV_GRID to another place,
then catch for example **on_double_click**, and then delegate execution to another class. 

***

#### ZCL_EUI_EVENT_CALLER

&nbsp;&nbsp;&nbsp;One simple solution is to call the event handler method via dynamic call. <br/>
Those handler descriptions can be presented as an interface with a method that can be called via CALL METHOD with **PARAMETER-TABLE**.
 
When the code contains a description of the called method, we can call it dynamically by knowing its signature 
```abap
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
            sender
            e_row_id
            e_column_id,
```

#### Implementation of ZCL_EUI_EVENT_CALLER

First you need to know what method you need to call

![image](https://user-images.githubusercontent.com/36256417/81949867-bd747c80-961c-11ea-86fe-32f5f6c5ca7a.png)

* IO_HANDLER - An object whose method needs to be called
* IV_HANDLERS_MAP - If you have several on_double_click handlers for different ALVs, you need to specify the method name
* IV_FIRST - Call first? RAISE EVENT does not guarantee call order
* IV_ACTIVATE - Activate / Deactivate

<br/>
<br/>
<br/>

And then it remains just to call the event handlers using **CALL_HANDLERS**

![image](https://user-images.githubusercontent.com/36256417/81950696-ba2dc080-961d-11ea-9895-acc26a3f971b.png)

Instead of such code
```abap
    " Handler returns cv_close = 'X' to close SCREEN
    RAISE EVENT zif_eui_manager~pai_event
     EXPORTING
       iv_command = sy-ucomm
       cv_close = REF #( lv_close ).
```

We call handlers like this
```abap
    mo_event_caller->call_handlers(
     iv_of_class     = 'ZIF_EUI_MANAGER'
     iv_for_event    = 'PAI_EVENT'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = me
     iv_param_nam_01 = 'IV_COMMAND'      iv_param_val_01 = iv_command
     iv_param_nam_02 = 'CV_CLOSE'        iv_param_val_02 = lr_close ).
```

We delegate on_double_click like this
```abap
    mo_eui_alv->mo_event_caller->call_handlers(
     iv_of_class     = 'CL_GUI_ALV_GRID'
     iv_for_event    = 'DOUBLE_CLICK'
     iv_param_nam_00 = 'SENDER'          iv_param_val_00 = sender
     iv_param_nam_01 = 'E_ROW'           iv_param_val_01 = e_row
     iv_param_nam_02 = 'E_COLUMN'        iv_param_val_02 = e_column ).
```

***

If you read up to here and understood the main idea, I shake your hand at a distance

As a result, you can install handlers (or callback methods) through 1 parameter **io_handler** which has a description of all the necessary handlers

For **PAI_EVENT** interface ZIF_EUI_MANAGER
```abap
 " Handle of PAI_EVENT
 CHECK mo_screen->show( io_handler = me ) = 'OK'.
```

For **ON_USER_COMMAND** class CL_GUI_ALV_GRID
```abap
    " Instead of set handler
    lo_eui_alv->show(
     io_handler        = me

     " If omit map with all (Could be several ON_USER_COMMAND)
     iv_handlers_map   = 'ON_HOTSPOT_CLICK;ON_USER_COMMAND;ON_PBO_EVENT'
    ).
```

***

Example usage in SE38
* ZEUI_TEST_ALV
* ZEUI_TEST_SCREEN_02