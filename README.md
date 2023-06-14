# ABAP salv
Encapsulated SALV class, which can conveniently display ALV

```ABAP
  SELECT * UP TO 100 ROWS
  INTO CORRESPONDING FIELDS OF TABLE @lt_sflight
  FROM sflight.

  "SALV creation with only table passed
  lo_salv = NEW zcl_salv(
    im_table = lt_sflight
  ).

  lo_salv->hide_column( 'MANDT' ).
  lo_salv->set_column_text( im_colname = 'SEATSMAX_B' im_text = '商务舱最大容量' ).
  lo_salv->set_column_key( im_colname = 'PLANETYPE' ).

  "Display full screen grid
  lo_salv->display( ).
```

![image](https://github.com/a87b01c14/salv/blob/main/WX20230613-182143%402x.png)
