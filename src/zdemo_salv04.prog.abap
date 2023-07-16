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
