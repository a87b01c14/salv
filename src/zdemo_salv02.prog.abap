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
