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
