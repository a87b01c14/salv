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
    im_table    = lt_sflight
    im_pfstatus = 'STANDARD_FULLSCREEN'
    im_pfreport = 'SAPLSLVC_FULLSCREEN'
  ).


  lo_salv->hide_column( 'MANDT' ).
  lo_salv->set_column_text( im_colname = 'SEATSMAX_B' im_text = 'CHANGE TEXT' ).
  lo_salv->set_column_key( im_colname = 'PLANETYPE' ).

  "Display full screen grid
  lo_salv->display( ).
