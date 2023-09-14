FUNCTION Z_SALV_DISPLAY .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IO_SALV) TYPE REF TO  ZCL_SALV
*"----------------------------------------------------------------------
  go_salv = io_salv.
  CALL SCREEN 100.

ENDFUNCTION.
