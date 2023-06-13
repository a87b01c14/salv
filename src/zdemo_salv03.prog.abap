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
