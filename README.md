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
DATA: lo_salv TYPE REF TO zcl_salv.

START-OF-SELECTION.


  SELECT * UP TO 100 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @lt_sflight
  FROM sflight.

  "SALV creation with only table passed
  lo_salv = NEW zcl_salv(
    im_table = lt_sflight
  ).


  lo_salv->hide_column( 'MANDT' ).
  lo_salv->set_column_text( im_colname = 'SEATSMAX_B' im_text = 'CHANGE TEXT' ).
  lo_salv->set_column_key( im_colname = 'PLANETYPE' ).

  "Display full screen grid
  lo_salv->display( ).
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

DATA: lt_sflight TYPE STANDARD TABLE OF sflight.
DATA: go_salv TYPE REF TO zcl_salv.

START-OF-SELECTION.


  SELECT * UP TO 100 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @lt_sflight
  FROM sflight.

  "SALV creation with only table passed
  go_salv = NEW zcl_salv(
    im_table    = lt_sflight
    im_t_events = VALUE #( ( name = zcl_salv=>events-link_click form = 'FRM_LINK_CLICK' ) )
  ).


  go_salv->hide_column( 'MANDT' ).
  go_salv->set_column_key( im_colname = 'PLANETYPE' ).
  go_salv->set_column_hotspot( im_colname = 'CARRID' ).

  "Display full screen grid
  go_salv->display( ).

FORM frm_link_click USING sender TYPE REF TO cl_salv_events_table
                             row    TYPE salv_de_row
                             column TYPE salv_de_column.

  FIELD-SYMBOLS:<fs_table> TYPE STANDARD TABLE,
                <fs_row>   TYPE any,
                <fs_col>   TYPE any.

  DATA: ref_table TYPE REF TO data.

  ref_table = go_salv->get_data( ).
  ASSIGN ref_table->* TO <fs_table>.
  CHECK sy-subrc = 0.
  READ TABLE <fs_table> ASSIGNING <fs_row> INDEX row.
  CHECK sy-subrc = 0.
  CASE column .
    WHEN 'CARRID' .
      ASSIGN COMPONENT column OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_cell>).
      CHECK sy-subrc = 0.
      MESSAGE <fs_cell> TYPE 'I'.
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

  "SALV creation with only table passed
  go_salv = NEW zcl_salv(
    im_table    = gt_sflight
    im_pfstatus = 'ZSALV_STATUS'
    im_t_events = VALUE #( ( name = zcl_salv=>events-link_click form = 'FRM_LINK_CLICK' )
                           ( name = zcl_salv=>events-added_function form = 'FRM_ADDED_FUNCTION')
                         ) ).


  go_salv->hide_column( 'MANDT' ).
  go_salv->set_column_key( im_colname = 'PLANETYPE' ).
  go_salv->set_column_hotspot( im_colname = 'CARRID' ).

  "Display full screen grid
  go_salv->display( ).

FORM frm_link_click USING sender TYPE REF TO cl_salv_events_table
                             row    TYPE salv_de_row
                             column TYPE salv_de_column.

  FIELD-SYMBOLS:<fs_table> TYPE STANDARD TABLE,
                <fs_row>   TYPE any,
                <fs_col>   TYPE any.

  DATA: ref_table TYPE REF TO data.

  ref_table = go_salv->get_data( ).
  ASSIGN ref_table->* TO <fs_table>.
  CHECK sy-subrc = 0.
  READ TABLE <fs_table> ASSIGNING <fs_row> INDEX row.
  CHECK sy-subrc = 0.
  CASE column .
    WHEN 'CARRID' .
      ASSIGN COMPONENT column OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_cell>).
      CHECK sy-subrc = 0.
      MESSAGE <fs_cell> TYPE 'I'.
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
  FIELD-SYMBOLS:<fs_table>     TYPE STANDARD TABLE,
                <fs_table_sel> TYPE STANDARD TABLE,
                <fs_row>       LIKE LINE OF gt_sflight,
                <fs_col>       TYPE any.

  DATA: ref_table TYPE REF TO data.
  DATA: lt_data_sel LIKE gt_sflight.
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
  ref_table = go_salv->get_selected_data( ).
  ASSIGN ref_table->* TO <fs_table_sel>.
  lt_data_sel = CORRESPONDING #( <fs_table_sel> ).
  CHECK sy-subrc = 0.

  DATA(lv_line) = lines( lt_data_sel ).
  IF lv_line = 0.
    MESSAGE 'NO SELECTED ROWS' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  ref_table = go_salv->get_data( ).
  ASSIGN ref_table->* TO <fs_table>.
  CHECK sy-subrc = 0.

  LOOP AT lt_data_sel INTO DATA(ls_data_sel).
    " DO SOMETHING
    " UPDATE <fs_table>
*     READ TABLE <fs_table> ASSIGNING <fs_row> WITH KEY ('MATNR') = ls_data_sel-matnr
*                                                       ('WERKS') = ls_data_sel-werks
*                                                       ('LGORT') = ls_data_sel-lgort.
*      IF sy-subrc = 0.
*        <fs_row>-mblnr = lv_mblnr.
*        <fs_row>-bapi_msg = lv_msgtx.
*        <fs_row>-mjahr = lv_mjahr.
*      ENDIF.
  ENDLOOP.
  MESSAGE 'POST' TYPE 'I'.
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
DATA: go_salv TYPE REF TO zcl_salv.

START-OF-SELECTION.


  SELECT * UP TO 100 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @gt_sflight
  FROM sflight.

  "SALV creation with only table passed
  go_salv = NEW zcl_salv(
    im_table = gt_sflight
  ).


  go_salv->hide_column( 'MANDT' ).
  go_salv->set_column_key( im_colname = 'PLANETYPE' ).
  go_salv->set_column_hotspot( im_colname = 'CARRID' ).

  "Display full screen grid
  go_salv->display( ).
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
DATA: go_salv TYPE REF TO zcl_salv.

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


  "SALV creation with only table passed
  go_salv = NEW zcl_salv(
    im_table = gt_sflight
  ).

  go_salv->hide_column( 'MANDT' ).
  go_salv->set_column_key( im_colname = 'PLANETYPE' ).
  go_salv->set_column_hotspot( im_colname = 'CARRID' ).

* set column color
  go_salv->set_column_colors( im_colname = 'SEATSMAX'  im_color = '3' ).

* set row/cell color
  go_salv->set_column_color( im_colname = 'T_COLOR' ).

  "Display full screen grid
  go_salv->display( ).
```

![image](https://github.com/a87b01c14/salv/blob/main/WX20230619-153639%402x.png)
