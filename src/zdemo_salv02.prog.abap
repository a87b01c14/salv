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
