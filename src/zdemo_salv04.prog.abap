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
