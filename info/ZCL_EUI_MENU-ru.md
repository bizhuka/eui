## ZCL_EUI_MENU

Когда ```PF-STATUS``` полон кнопками и нельзя использовать ```SELECTION-SCREEN - FUNCTION KEY``` по причине того, что ЛБД заняла все 4 кнопки, можно добавить свою кнопку также как делает это GOS.

 ![image](https://user-images.githubusercontent.com/36256417/80451042-50829680-893c-11ea-98cf-04eda51a6b9f.png)

***

### Пример SE38 -> ZEUI_TEST_MENU

---

Класс **ZCL_EUI_MENU** создает меню на основе CL_GUI_TOOLBAR и использует CL_GUI_GOS_CONTAINER если ему не передали другого контейнера.

Меню можно создать (или пересоздать) вызвав метод **CREATE_TOOLBAR**.

Он принимает 2 параметра:
* IV_WIDTH – optional параметр. Если toolbar имеет тексты, а не только иконки
* IT_MENU – иерархическое меню с описанием кнопок

Основные параметры для CL_GUI_TOOLBAR указаны в структуре **STB_BUTTON** (id функции, иконка, текст).

Разделитель создается указанием ```butn_type = cntb_btype_sep```
 
Для создания иерархии нужно заполнить параметр **PAR_FUNCTION**

![image](https://user-images.githubusercontent.com/36256417/80451272-d9013700-893c-11ea-9f6e-43b588689d68.png) 

---

Сам обработчик события указывается в конструкторе (параметр **IO_HANDLER**) или в методе CHANGE_HANDLER

Этот параметр представляет собой объект в котором должен быть объявлен публичный метод

```abap
ON_FUNCTION_SELECTED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
```
Название метода значения не имеет так как метод ищется по его сигнатуре
 
![image](https://user-images.githubusercontent.com/36256417/80451340-0948d580-893d-11ea-8023-05defe15d6df.png)

---

Чтобы изменять меню в RunTime (к примеру скрыть меню после запуска программы) можно получить доступ до контейнера или самого CL_GUI_TOOLBAR
 
![image](https://user-images.githubusercontent.com/36256417/80451378-2382b380-893d-11ea-810e-661d0fef4f3d.png)



