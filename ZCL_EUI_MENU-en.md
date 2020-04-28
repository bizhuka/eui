## ZCL_EUI_MENU

When ```PF-STATUS``` is full of the buttons and you cannot write another ```SELECTION-SCREEN - FUNCTION KEY``` since all keys are specified in LDB, you can add your own button just like GOS does.

 ![image](https://user-images.githubusercontent.com/36256417/80451042-50829680-893c-11ea-98cf-04eda51a6b9f.png)

***

### Example SE38 -> ZEUI_TEST_MENU

---

Class **ZCL_EUI_MENU** creates a menu based on CL_GUI_TOOLBAR and uses CL_GUI_GOS_CONTAINER if no other container has been passed to to him.

Menus can be created (or recreated) by calling **CREATE_TOOLBAR** method.

It takes 2 parameters:
* IV_WIDTH – optional parameter. If the toolbar has texts, not just icons
* IT_MENU – hierarchical menu with a description of the buttons

Main parameters for CL_GUI_TOOLBAR passed with **STB_BUTTON** (function id, icon, text).

Separator created by specifying ```butn_type = cntb_btype_sep```
 
To create a hierarchy you have to fill **PAR_FUNCTION** parameter

![image](https://user-images.githubusercontent.com/36256417/80451272-d9013700-893c-11ea-9f6e-43b588689d68.png) 

---

The event handler itself is specified in the constructor (**IO_HANDLER** parameter) or in the CHANGE_HANDLER method

This parameter represents the object in which the public method should be declared like this

```abap
ON_FUNCTION_SELECTED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
```
The name of the method does not matter since the method is searched by its signature
 
![image](https://user-images.githubusercontent.com/36256417/80451340-0948d580-893d-11ea-8023-05defe15d6df.png)

---

To change the menu in RunTime (for example, hide the menu after starting the program) you can access the container or CL_GUI_TOOLBAR itself
 
![image](https://user-images.githubusercontent.com/36256417/80451378-2382b380-893d-11ea-810e-661d0fef4f3d.png)



