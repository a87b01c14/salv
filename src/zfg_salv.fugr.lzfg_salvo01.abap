*----------------------------------------------------------------------*
***INCLUDE LZFG_SALVO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  DATA: lv_title TYPE string.
  IF go_salv->mo_table IS NOT BOUND.
    lv_title = COND #( WHEN go_salv->mv_title IS NOT INITIAL THEN go_salv->mv_title ELSE sy-title ).
*    SET PF-STATUS 'STANDARD_FULLSCREEN'.
    SET TITLEBAR '0100' WITH lv_title.
    go_salv->set_container( cl_gui_container=>screen0 ).
    go_salv->pbo(  ).
  ENDIF.
  go_salv->mo_table->display( ) .
ENDMODULE.
