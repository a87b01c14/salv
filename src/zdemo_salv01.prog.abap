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
    im_title = 'SET TITLE SALV DEMO01'
    )->display( ).
