*&---------------------------------------------------------------------*
*& Report ZDEMO_SALV06
*&---------------------------------------------------------------------*
*& add gui status
*&---------------------------------------------------------------------*
REPORT zdemo_salv06.

DATA: gt_sflight TYPE STANDARD TABLE OF sflight.
DATA: go_salv TYPE REF TO zcl_salv.
DATA go_container TYPE REF TO cl_gui_custom_container.

START-OF-SELECTION.
  SELECT * UP TO 100 ROWS
     INTO CORRESPONDING FIELDS OF TABLE @gt_sflight
     FROM sflight.

  go_salv = NEW zcl_salv(
    im_title      = 'SALV DEMO06'
    im_table      = REF #( gt_sflight )
    im_t_events   = VALUE #( ( name = zcl_salv=>events-link_click form = 'FRM_LINK_CLICK' )
                             ( name = zcl_salv=>events-added_function form = 'FRM_ADDED_FUNCTION' ) )
    im_t_hide     = VALUE #( ( fieldname = 'MANDT' ) )
    im_t_hotspot  = VALUE #( ( fieldname = 'CARRID' ) )
    im_t_function = VALUE #( ( name = '1POST' icon = icon_checked text = 'POST' )
                              ( name = '2CANCEL' icon = icon_cancel text = 'CANCEL' ) ) ).
  go_salv->add_button( im_name = '3DUMMY' im_icon = icon_dummy im_text = 'ADDBUTTON' ).
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
    WHEN '1POST'.
      PERFORM frm_post.
    WHEN '2CANCEL'.
      MESSAGE 'CANCELED' TYPE 'I'.
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
