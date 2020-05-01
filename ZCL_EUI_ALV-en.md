## ZCL_EUI_ALV

### ALV inside another ALV
How to show the consultant that the amount is calculated correctly?

Often writing the report itself takes a little time, but ~~further "Rumble in the Bronx"~~ joint search for errors takes much more physical and mental efforts, both of the developer and the task manager.

One of the easiest ways is to show in popup ALV what this amount is made of.\
Those suppose that a report is **collect**ing amount in the main table, during drilldown we show all the positions from which this amount was collected   

![](https://raw.githubusercontent.com/wiki/bizhuka/py_demo/src/alv_0.png)

***

### Example SE38 -> ZEUI_TEST_ALV

### Example SE38 -> ZR_PY000_DEMO
More details can be found here
* https://github.com/bizhuka/py_demo
* https://github.com/bizhuka/py_demo/wiki

---

The basic skills of most ABAP developers are probably classes **CL_GUI_ALV_GRID** and **CL_SALV_TABLE**

Although the latter is newer, it:
* does not support editing by default (There are of course several types of tricks to get around this)
* customization of field catalog, toolbar, layout and variant occurs through method calls\
those imho CL_SALV_TABLE more verbose than CL_GUI_ALV_GRID (начиная с 7.40) where all ALV tuning happens, as usual, through tables and structures 

CL_GUI_ALV_GRID, in turn, has 1 but a very significant drawback - to create screens for the main and popup tables ~~love to do masochists~~ a very tedious task for an already busy programmer ~~between sleeping and watching the show during lunch~~

***

For simple cases, the entire ALV display can be written 1 line :metal:

```abap
 NEW zcl_eui_alv( ir_table = REF #( mt_alv ) )->
    popup( )->
    show( ).
```

It is written in 3 lines for clarity :smile:\
And the fact that you *can* write so does not mean that you *need* to do

***

### In details

Let's move on to the syntax 7.40

#### 1) CREATE
```abap
      " Create new ALV
      DATA(lo_alv) = NEW zcl_eui_alv(
       " Data for drilldown
       ir_table       = REF #( lt_rt )

       " What kind of payments do we use
       it_filter      = VALUE LVC_T_FILT( ( fieldname = 'LGART' SIGN = 'I' OPTION = 'EQ' LOW = ... ) )

       " Put the amount field closer to the beginning
       it_mod_catalog = VALUE LVC_T_FCAT( ( fieldname = 'BETRG' col_pos = 5 do_sum = abap_true ) )

       " In the header, the personnel number + tech. info
       is_layout      = VALUE LVC_S_LAYO(
          grid_title = |{ <ls_alv>-pernr } - { <ls_lgart>-name } ({ <ls_lgart>-label })|
          smalltitle = abap_true )

       " If there is a lot of data, it is better to group it
       it_sort        = VALUE LVC_T_SORT(
         ( fieldname = 'SRTZA' subtot = abap_true expa = abap_true )
         ( fieldname = 'LGART' subtot = abap_true expa = abap_true ) ) ).
```

For those who remember not only REUSE, but also reporting using `WRITE` + COLOR + HOTSPOT events ~~that old fart~~ I think everything is clear.\
And so a single drilldown in LVC_S_LAYO и DISVARIANT (alv variant) and double drilldown in LVC_T_FILT, LVC_T_FCAT, TTB_BUTTON (toolbar) и LVC_T_SORT will remove most of the questions about how it works.

Parameter **IT_MOD_CATALOG** not assembled from scratch field catalog! It simply complements it with non-empty values. Also, for convenience, you can specify a mask for the fields\
`fieldname = 'SUM*' do_sum = 'X' hotspot = 'X'`

---

#### 2) POPUP
As in SALV, if popup is not needed, we do nothing, otherwise we call

```abap
      " As popup
      lo_alv->popup( ).
```
You can transfer window sizes to it
* IV_COL_BEG
* IV_COL_END
* IV_ROW_BEG
* IV_ROW_END 

Actually, for the sake of this method, this class was created (popup nesting is limited to 7 screens)

---

#### 3) SHOW 
Call the ALV show itself

```abap
      " show ALV
      lo_alv->show( ).
```

This method returns the code of the closing function. For example, if `check lo_alv->show( ) = 'OK'.`\
PF-STATUS и TITLE BAR can be specified statically in the constructor or dynamically by event
 
```abap
      on_pbo_event FOR EVENT pbo_event OF zif_eui_manager
        IMPORTING
            sender    "<-- CAST to ZCL_EUI_ALV
            iv_dynnr.
```

In this event, you can get the control itself **CL_GUI_ALV_GRID** by calling `ZCL_EUI_ALV-> GET_GRID ()`

But in most cases **io_handler** can be passed to **SHOW** method, which can have CL_GUI_ALV_GRID event handlers:
* on_user_command
* on_hotspot_click
* on_double_click
* on_toolbar
* on_top_of_page
* on_data_changed

In them **sender** this is CL_GUI_ALV_GRID.

---

### Editing

Last handler **on_data_changed** only needed when editing GRID LVC_S_LAYO-EDIT = 'X' or single field LVC_S_FCAT-EDIT

Validation of the entered data can be done in the method
```abap
      on_pai_event FOR EVENT pai_event OF zif_eui_manager
        IMPORTING
            iv_command
            cv_close. " Set cv_close->* = abap_false to cancel closing
```

If the 'OK' and 'CANCEL' buttons are not enough (CANCEL only for READ_ONLY mode)

*small remark*
* You can change the status of the event **on_pbo_event** (dynamically)
* Or 1 time in the constructor (statically)
  * IV_STATUS_NAME
  * IV_STATUS_PROG
  * IT_STATUS_EXCLUDE
  * IV_STATUS_TITLE