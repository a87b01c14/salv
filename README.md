# ABAP salv
Encapsulated SALV class, which can conveniently display ALV

## Demo1  The simplest usage scenario

```ABAP
*&---------------------------------------------------------------------*
*& Report ZDEMO_SALV01
*&---------------------------------------------------------------------*
*& Basic usage
*&---------------------------------------------------------------------*
REPORT zdemo_salv01.

DATA: lt_sflight TYPE STANDARD TABLE OF sflight.

START-OF-SELECTION.


  SELECT * UP TO 100 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @lt_sflight
  FROM sflight.

  NEW zcl_salv(
    im_table = REF #( lt_sflight )
    im_t_hide = VALUE #( ( fieldname = 'MANDT' ) )
    im_t_text = VALUE #( ( fieldname = 'SEATSMAX_B' text = 'CHANGE TEXT' ) )
    )->display( ).
```

![image](https://github.com/a87b01c14/salv/blob/main/WX20230619-152948%402x.png)


## Demo2  add colnumn link

```ABAP
*&---------------------------------------------------------------------*
*& Report ZDEMO_SALV02
*&---------------------------------------------------------------------*
*& hostspot
*&---------------------------------------------------------------------*
REPORT zdemo_salv02.

DATA: gt_sflight TYPE STANDARD TABLE OF sflight.

START-OF-SELECTION.


  SELECT * UP TO 100 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @gt_sflight
  FROM sflight.

  NEW zcl_salv(
    im_table = REF #( gt_sflight )
    im_t_events = VALUE #( ( name = zcl_salv=>events-link_click form = 'FRM_LINK_CLICK' ) )
    im_t_hide = VALUE #( ( fieldname = 'MANDT' ) )
    im_t_hotspot = VALUE #( ( fieldname = 'CARRID' ) )
    )->display( ).

FORM frm_link_click USING sender TYPE REF TO cl_salv_events_table
                             row    TYPE salv_de_row
                             column TYPE salv_de_column.
  READ TABLE gt_sflight INTO DATA(ls_sflight) INDEX row.
  CHECK sy-subrc = 0.
  CASE column .
    WHEN 'CARRID' .
      MESSAGE |{ ls_sflight-carrid } { ls_sflight-connid } { ls_sflight-fldate }| TYPE 'I'.
  ENDCASE.
ENDFORM.
```

![image](https://github.com/a87b01c14/salv/blob/main/WX20230619-153508%402x.png)

## Demo3  add toolbar button

```ABAP
*&---------------------------------------------------------------------*
*& Report ZDEMO_SALV03
*&---------------------------------------------------------------------*
*& add gui status
*&---------------------------------------------------------------------*
REPORT zdemo_salv03.

DATA: gt_sflight TYPE STANDARD TABLE OF sflight.
DATA: go_salv TYPE REF TO zcl_salv.

START-OF-SELECTION.


  SELECT * UP TO 100 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @gt_sflight
  FROM sflight.

  go_salv = NEW zcl_salv(
    im_table     = REF #( gt_sflight )
    im_pfstatus  = 'ZSALV_STATUS'
    im_t_events  = VALUE #( ( name = zcl_salv=>events-link_click form = 'FRM_LINK_CLICK' )
                            ( name = zcl_salv=>events-added_function form = 'FRM_ADDED_FUNCTION' ) )
    im_t_hide    = VALUE #( ( fieldname = 'MANDT' ) )
    im_t_hotspot = VALUE #( ( fieldname = 'CARRID' ) ) ).
  go_salv->display( ).

FORM frm_link_click USING sender TYPE REF TO cl_salv_events_table
                             row    TYPE salv_de_row
                             column TYPE salv_de_column.
  READ TABLE gt_sflight INTO DATA(ls_sflight) INDEX row.
  CHECK sy-subrc = 0.
  CASE column .
    WHEN 'CARRID' .
      MESSAGE |{ ls_sflight-carrid } { ls_sflight-connid } { ls_sflight-fldate }| TYPE 'I'.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      FORM  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
FORM frm_added_function USING p_sender TYPE REF TO cl_salv_events_table
                                   p_ucomm TYPE salv_de_function.

  CASE p_ucomm.
    WHEN 'POST'.
      PERFORM frm_post.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM FRM_SAVE
*&---------------------------------------------------------------------*
*& TEXT
*&---------------------------------------------------------------------*
*& -->  P1        TEXT
*& <--  P2        TEXT
*&---------------------------------------------------------------------*
FORM frm_post.
  DATA: lt_rows TYPE salv_t_row,
        l_row   TYPE i.

  DATA: lt_item LIKE gt_sflight.
  DATA: lv_msgty TYPE bapi_mtype,
        lv_msgtx TYPE bapi_msg.

  DATA: wl_answer TYPE c.
*  CALL FUNCTION 'POPUP_TO_CONFIRM'
*    EXPORTING
*      text_question         = TEXT-003 "是否调整库存？
*      icon_button_1         = TEXT-004 "是
*      icon_button_2         = TEXT-005 "否
*      default_button        = '2'
*      display_cancel_button = ''
*      start_column          = 25
*      start_row             = 6
*    IMPORTING
*      answer                = wl_answer
*    EXCEPTIONS
*      text_not_found        = 1
*      OTHERS                = 2.
*
*  IF wl_answer = '1'.
*  ELSE.
*    RETURN.
*  ENDIF.

* GET SELECTED ROWS
  lt_rows = go_salv->get_selected_rows( ).
  DATA(lv_line) = lines( lt_rows ).
  IF lv_line = 0.
    MESSAGE 'NO SELECTED ROWS' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT lt_rows INTO l_row.
    " DO SOMETHING
    " UPDATE <fs_table>
    READ TABLE gt_sflight ASSIGNING FIELD-SYMBOL(<fs_sflight>) INDEX l_row.
    IF sy-subrc = 0.
      <fs_sflight>-seatsocc += 1.
      lv_msgtx = |POST { <fs_sflight>-carrid } { <fs_sflight>-connid } { <fs_sflight>-fldate }|.
    ENDIF.
  ENDLOOP.
  MESSAGE lv_msgtx TYPE 'I'.
  go_salv->refresh( ).
ENDFORM.
```

![image](https://github.com/a87b01c14/salv/blob/main/WX20230619-153531%402x.png)


## Demo4  add a column to display row number

```ABAP
*&---------------------------------------------------------------------*
*& Report ZDEMO_SALV04
*&---------------------------------------------------------------------*
*& add a column:row_no
*&---------------------------------------------------------------------*
REPORT zdemo_salv04.

TYPES: BEGIN OF gty_sflight,
         row_no TYPE lineid.
         INCLUDE STRUCTURE sflight.
TYPES: END OF gty_sflight.

DATA: gt_sflight TYPE STANDARD TABLE OF gty_sflight.

START-OF-SELECTION.


  SELECT * UP TO 100 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @gt_sflight
  FROM sflight.

  NEW zcl_salv(
    im_table = REF #( gt_sflight )
    im_t_hide = VALUE #( ( fieldname = 'MANDT' ) )
    im_t_hotspot = VALUE #( ( fieldname = 'CARRID' ) )
    )->display( ).
```

![image](https://github.com/a87b01c14/salv/blob/main/WX20230619-153611%402x.png)

When you apply filtering, the row numbers are automatically renumbered

![image](https://github.com/a87b01c14/salv/blob/main/WX20230619-153624%402x.png)


## Demo5  SET COLUMN/ROW/CELL COLOR

```ABAP
*&---------------------------------------------------------------------*
*& Report ZDEMO_SALV05
*&---------------------------------------------------------------------*
*& SET COLUMN/ROW/CELL COLOR
*&---------------------------------------------------------------------*
REPORT zdemo_salv05.

TYPES: BEGIN OF gty_sflight,
         row_no  TYPE lineid.
         INCLUDE STRUCTURE sflight.
TYPES:   t_color TYPE lvc_t_scol,
       END OF gty_sflight.

DATA: gt_sflight TYPE STANDARD TABLE OF gty_sflight.

DATA: lt_s_color TYPE lvc_t_scol,
      ls_s_color TYPE lvc_s_scol.

START-OF-SELECTION.


  SELECT * UP TO 100 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @gt_sflight
  FROM sflight.


  LOOP AT gt_sflight ASSIGNING FIELD-SYMBOL(<fs_sflight>).
    REFRESH: lt_s_color.
    IF sy-tabix MOD 3 = 0.
* set row color
      ls_s_color-color-col = 5.
      ls_s_color-color-int = 0.
      ls_s_color-color-inv = 0.
      APPEND ls_s_color TO lt_s_color.
      CLEAR  ls_s_color.
    ENDIF.
    IF sy-tabix MOD 2 = 0.
* set cell color
      ls_s_color-fname = 'PAYMENTSUM'.
      ls_s_color-color-col = 6.
      ls_s_color-color-int = 0.
      ls_s_color-color-inv = 0.
      APPEND ls_s_color TO lt_s_color.
      CLEAR  ls_s_color.
    ENDIF.
    <fs_sflight>-t_color = lt_s_color.
  ENDLOOP.

  NEW zcl_salv(
    im_table = REF #( gt_sflight )
    im_t_hide = VALUE #( ( fieldname = 'MANDT' ) )
    im_t_hotspot = VALUE #( ( fieldname = 'CARRID' ) )
    im_t_color = VALUE #( ( fieldname = 'SEATSMAX' color = '3' ) )
    im_color_name = 'T_COLOR'
    )->display( ).
```

![image](https://github.com/a87b01c14/salv/blob/main/WX20230619-153639%402x.png)
