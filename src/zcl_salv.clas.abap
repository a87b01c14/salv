CLASS zcl_salv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_salv_event_receiver .

    TYPES:
      "fname
      BEGIN OF ty_fname,
        fieldname TYPE lvc_fname,
      END OF ty_fname .
    TYPES:
      tt_hide TYPE HASHED TABLE OF ty_fname WITH UNIQUE KEY fieldname .
    TYPES:
      tt_hotspot TYPE HASHED TABLE OF ty_fname WITH UNIQUE KEY fieldname .
    TYPES:
      tt_key TYPE HASHED TABLE OF ty_fname WITH UNIQUE KEY fieldname .
    TYPES:
      "列描述
      BEGIN OF ty_text,
        fieldname TYPE lvc_fname,
        text      TYPE string,
      END OF ty_text .
    TYPES:
      tt_text TYPE HASHED TABLE OF ty_text WITH UNIQUE KEY fieldname .
    TYPES:
      "列颜色
      BEGIN OF ty_color,
        fieldname TYPE lvc_fname,
        color     TYPE lvc_col,
      END OF ty_color .
    TYPES:
      tt_color TYPE HASHED TABLE OF ty_color WITH UNIQUE KEY fieldname .
    TYPES:
      "自定义按钮
      BEGIN OF ty_function,
        name     TYPE salv_de_function,
        icon     TYPE icon_d,
        text     TYPE string,
        tooltip  TYPE string,
        position TYPE salv_de_function_pos,
      END OF ty_function .
    TYPES:
      tt_function TYPE STANDARD TABLE OF ty_function WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF events,
        top_of_page          TYPE slis_formname VALUE 'TOP_OF_PAGE',
        end_of_page          TYPE slis_formname VALUE 'END_OF_PAGE',
        before_salv_function TYPE slis_formname VALUE 'BEFORE_SALV_FUNCTION',
        after_salv_function  TYPE slis_formname VALUE 'AFTER_SALV_FUNCTION',
        added_function       TYPE slis_formname VALUE 'ADDED_FUNCTION',
        double_click         TYPE slis_formname VALUE 'DOUBLE_CLICK',
        link_click           TYPE slis_formname VALUE 'LINK_CLICK',
      END OF events .

    DATA mo_table TYPE REF TO cl_salv_table .
    DATA mv_title          TYPE lvc_title .
    METHODS constructor
      IMPORTING
        VALUE(im_repid)    TYPE sy-repid OPTIONAL
        VALUE(im_table)    TYPE REF TO data
        !im_container      TYPE REF TO cl_gui_container OPTIONAL
        !im_container_name TYPE string OPTIONAL
        VALUE(im_row_name) TYPE lvc_fname DEFAULT 'ROW_NO'
        !im_handle         TYPE slis_handl OPTIONAL
        !im_pfstatus       TYPE sypfkey OPTIONAL
        !im_pfreport       TYPE sy-repid OPTIONAL
        !im_t_excluding    TYPE kkblo_t_extab OPTIONAL
        !im_t_events       TYPE slis_t_event OPTIONAL
        !im_t_hide         TYPE zcl_salv=>tt_hide OPTIONAL
        !im_t_text         TYPE zcl_salv=>tt_text OPTIONAL
        !im_t_key          TYPE zcl_salv=>tt_key OPTIONAL
        !im_t_hotspot      TYPE zcl_salv=>tt_hotspot OPTIONAL
        !im_t_color        TYPE zcl_salv=>tt_color OPTIONAL
        !im_t_function     TYPE zcl_salv=>tt_function OPTIONAL
        !im_color_name     TYPE lvc_fname OPTIONAL
        !im_title          TYPE lvc_title OPTIONAL .
    METHODS display .
    METHODS hide_column
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname .
    METHODS set_column_f4
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname
        VALUE(im_value)   TYPE sap_bool .
    METHODS set_column_text
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname
        VALUE(im_text)    TYPE string .
    METHODS set_column_position
      IMPORTING
        VALUE(im_colname)  TYPE lvc_fname
        VALUE(im_position) TYPE i .
    METHODS set_column_sign
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname
        VALUE(im_flag)    TYPE c .
    METHODS set_column_zero
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname
        VALUE(im_flag)    TYPE c .
    METHODS set_column_hotspot
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname .
    METHODS set_column_key
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname .
    METHODS set_column_color
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname .
    METHODS set_column_colors
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname
        !im_color         TYPE lvc_col .
    METHODS set_f4_checktable
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname
        VALUE(im_tabname) TYPE tabname .
    METHODS set_rowno_fname
      IMPORTING
        VALUE(im_colname) TYPE lvc_fname .
    METHODS set_ddic_reference
      IMPORTING
        VALUE(im_colname)   TYPE lvc_fname
        VALUE(im_tabname)   TYPE tabname
        VALUE(im_fieldname) TYPE lvc_fname .
    METHODS get_selected_rows
      RETURNING
        VALUE(re_value) TYPE salv_t_row .
    METHODS refresh .
    METHODS delete_data
      IMPORTING
        VALUE(im_index) TYPE i .
    METHODS get_selected_data
      RETURNING
        VALUE(re_table) TYPE REF TO data .
    METHODS get_data
      RETURNING
        VALUE(re_value) TYPE REF TO data .
    METHODS get_event
      RETURNING
        VALUE(ro_event) TYPE REF TO cl_salv_events_table .
    METHODS set_container
      IMPORTING
        !im_container      TYPE REF TO cl_gui_container
        !im_container_name TYPE string OPTIONAL .
    METHODS pbo .
    METHODS pai
      CHANGING
        !cv_ucomm LIKE sy-ucomm .
    METHODS add_button
      IMPORTING
        !im_name     TYPE salv_de_function
        !im_icon     TYPE icon_d OPTIONAL
        !im_text     TYPE string OPTIONAL
        !im_tooltip  TYPE string OPTIONAL
        !im_position TYPE salv_de_function_pos OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA cb_top_of_page TYPE slis_formname VALUE 'FRM_SALV_TOP_OF_PAGE' ##NO_TEXT.
    DATA cb_end_of_page TYPE slis_formname VALUE 'FRM_SALV_END_OF_PAGE' ##NO_TEXT.
    DATA cb_before_salv_function TYPE slis_formname VALUE 'FRM_SALV_BEFORE_SALV_FUNCTION' ##NO_TEXT.
    DATA cb_after_salv_function TYPE slis_formname VALUE 'FRM_SALV_AFTER_SALV_FUNCTION' ##NO_TEXT.
    DATA cb_added_function TYPE slis_formname VALUE 'FRM_SALV_ADDED_FUNCTION' ##NO_TEXT.
    DATA cb_double_click TYPE slis_formname VALUE 'FRM_SALV_DOUBLE_CLICK' ##NO_TEXT.
    DATA cb_link_click TYPE slis_formname VALUE 'FRM_SALV_LINK_CLICK' ##NO_TEXT.

    DATA mt_excluding TYPE kkblo_t_extab .
    DATA mo_data TYPE REF TO data .
    DATA mo_columns TYPE REF TO cl_salv_columns .
    DATA mo_events TYPE REF TO cl_salv_events_table .
    DATA rowno_fname TYPE lvc_fname VALUE 'ROW_NO' ##NO_TEXT.

    DATA mv_repid          TYPE sy-repid .
    DATA mt_events         TYPE slis_t_event .
    DATA mo_container      TYPE REF TO cl_gui_container.
    DATA mv_container_name TYPE string .
    DATA mv_handle         TYPE slis_handl .
    DATA mv_pfstatus       TYPE sypfkey .
    DATA mv_pfreport       TYPE sy-repid .
    DATA mt_hide           TYPE zcl_salv=>tt_hide .
    DATA mt_text           TYPE zcl_salv=>tt_text .
    DATA mt_key            TYPE zcl_salv=>tt_key .
    DATA mt_hotspot        TYPE zcl_salv=>tt_hotspot .
    DATA mt_color          TYPE zcl_salv=>tt_color .
    DATA mt_function       TYPE zcl_salv=>tt_function .
    DATA mv_color_name     TYPE lvc_fname .
    CLASS-DATA pernr_fname TYPE lvc_fname VALUE 'PERNR' ##NO_TEXT.

    METHODS set_column
      IMPORTING
        !im_t_hide     TYPE zcl_salv=>tt_hide
        !im_t_text     TYPE zcl_salv=>tt_text
        !im_t_key      TYPE zcl_salv=>tt_key
        !im_t_hotspot  TYPE zcl_salv=>tt_hotspot
        !im_t_color    TYPE zcl_salv=>tt_color
        !im_color_name TYPE lvc_fname .
    METHODS set_rowno
      CHANGING
        !ch_data TYPE STANDARD TABLE .
    METHODS set_events .
ENDCLASS.



CLASS zcl_salv IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS: <ft_data> TYPE STANDARD TABLE.

    DATA: callstack TYPE abap_callstack.
    IF im_repid IS INITIAL.
      CALL FUNCTION 'SYSTEM_CALLSTACK'
        IMPORTING
          callstack = callstack.

      ASSIGN callstack[ 2 ] TO FIELD-SYMBOL(<stack>).
      IF sy-subrc EQ 0.
        mv_repid = <stack>-mainprogram.
      ENDIF.
    ELSE.
      mv_repid = im_repid.
    ENDIF.
    rowno_fname       = im_row_name.
    mt_events         = im_t_events.
    mo_data           = im_table.
    mo_container      = im_container.
    mv_container_name = im_container_name.
    mv_handle         = im_handle.
    mv_pfstatus       = im_pfstatus.
    mv_pfreport       = im_pfreport.
    mt_hide           = im_t_hide.
    mt_text           = im_t_text.
    mt_key            = im_t_key.
    mt_hotspot        = im_t_hotspot.
    mt_color          = im_t_color.
    mt_function       = im_t_function.
    mv_color_name     = im_color_name.
    mv_title          = im_title.
    ASSIGN im_table->* TO <ft_data>.
    "设置行号
    set_rowno( CHANGING ch_data = <ft_data> ).

    "设置事件
    set_events( ).

  ENDMETHOD.


  METHOD delete_data.
    FIELD-SYMBOLS:<ft_data> TYPE STANDARD TABLE.
    ASSIGN mo_data->* TO <ft_data>.
    DELETE <ft_data> INDEX im_index.
  ENDMETHOD.


  METHOD display.
    IF mt_function IS NOT INITIAL.
      CALL FUNCTION 'Z_SALV_DISPLAY'
        EXPORTING
          io_salv = me.
    ELSE.
      pbo( ).
      mo_table->display( ) .
    ENDIF.
  ENDMETHOD.


  METHOD hide_column .
    DATA: lr_column TYPE REF TO cl_salv_column_table.
    TRY.
        lr_column ?= mo_columns->get_column( im_colname ).
        lr_column->set_visible( cl_salv_column_table=>false )."隐藏列，注：这里虽然是隐藏了，但在布局设置里还是可以看到此列的，如果使布局里也看不到，则需要gr_column->set_technical( 'X' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_column_text .
    DATA: lv_scrtext_l TYPE scrtext_l,
          lv_scrtext_m TYPE scrtext_m,
          lv_scrtext_s TYPE scrtext_s.
    DATA: lr_column    TYPE REF TO cl_salv_column_table.

    lv_scrtext_l = im_text.
    lv_scrtext_m = im_text.
    lv_scrtext_s = im_text.
    TRY.
        lr_column ?= mo_columns->get_column( im_colname ).
        lr_column->set_short_text( lv_scrtext_s ).
        lr_column->set_medium_text( lv_scrtext_m ).
        lr_column->set_long_text( lv_scrtext_l ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_column_sign .
    DATA: lr_column    TYPE REF TO cl_salv_column_table.
    TRY.
        lr_column ?= mo_columns->get_column( im_colname ).
        lr_column->set_sign( im_flag ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_column_hotspot .
    DATA: lr_column TYPE REF TO cl_salv_column_table.
    TRY.
        lr_column ?= mo_columns->get_column( im_colname ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    IF sy-subrc = 0.
*   Set the HotSpot
      TRY.
          CALL METHOD lr_column->set_cell_type
            EXPORTING
              value = if_salv_c_cell_type=>hotspot.
          .
        CATCH cx_salv_data_error .
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD set_column_key .
    DATA: lo_column_list TYPE REF TO cl_salv_column_list.
    TRY.
        lo_column_list ?= mo_columns->get_column( im_colname ).
        lo_column_list->set_key( if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD get_data.
    re_value = mo_data.
  ENDMETHOD.


  METHOD get_selected_data.
    FIELD-SYMBOLS:<ft_data>  TYPE STANDARD TABLE,
                  <fs_table> TYPE STANDARD TABLE,
                  <fs_row>   TYPE any.
    DATA: lr_selections TYPE REF TO cl_salv_selections,
          lt_rows       TYPE salv_t_row,
          l_row         TYPE i,
          lo_row        TYPE REF TO data.


    ASSIGN mo_data->* TO <ft_data>.
    CREATE DATA re_table LIKE <ft_data>.
    ASSIGN re_table->* TO <fs_table>.
    lr_selections = mo_table->get_selections( ).
    lt_rows = lr_selections->get_selected_rows( ).
    LOOP AT lt_rows INTO l_row.
      APPEND INITIAL LINE TO <fs_table> ASSIGNING <fs_row>.
      READ TABLE <ft_data> INTO <fs_row> INDEX l_row.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_selected_rows.
    DATA: lr_selections TYPE REF TO cl_salv_selections.
    lr_selections = mo_table->get_selections( ).
    re_value = lr_selections->get_selected_rows( ).
  ENDMETHOD.


  METHOD refresh.
    mo_table->refresh( ).
    CALL METHOD me->zif_salv_event_receiver~salv_after_salv_function( mo_events ).
  ENDMETHOD.


  METHOD set_column_color .
    DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table.
    TRY.
        lo_cols_tab ?= mo_table->get_columns( ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    IF sy-subrc = 0.
      TRY.
          lo_cols_tab->set_color_column( im_colname ).
        CATCH cx_salv_data_error .
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD set_column_colors .
    DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table.
    DATA: lo_col_tab  TYPE REF TO cl_salv_column_table.
    DATA: ls_color TYPE lvc_s_colo.
    TRY.
        lo_cols_tab ?= mo_table->get_columns( ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    IF sy-subrc = 0.
      "整列设置颜色
      TRY.
          lo_col_tab ?= lo_cols_tab->get_column( im_colname ).       "获取字段
          ls_color-col = im_color.                             "定义颜色
          ls_color-int = 1.                                        "反转颜色启用/关闭 1/0
          ls_color-inv = 0.                                        "增强颜色启用/关闭 1/0
          "颜色代码：
          "col_background        '0'.
          "col_heading           '1'.
          "COL_NORMAL            '2'.
          "col_total             '3'.
          "col_key               '4'.
          "col_positive          '5'.
          "COL_NEGATIVE          '6'.
          "col_group             '7'.

          lo_col_tab->set_color( ls_color ).                       "设置颜色
        CATCH cx_salv_not_found.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD set_column_f4 .
    DATA: lr_column    TYPE REF TO cl_salv_column_table.
    TRY.
        lr_column ?= mo_columns->get_column( im_colname ).
        lr_column->set_f4( im_value ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_column_position .
    mo_columns->set_column_position( columnname = im_colname
                                     position   = im_position ).
  ENDMETHOD.


  METHOD get_event.
    ro_event = mo_events.
  ENDMETHOD.


  METHOD set_column_zero .
    DATA: lr_column    TYPE REF TO cl_salv_column_table.
    TRY.
        lr_column ?= mo_columns->get_column( im_colname ).
        lr_column->set_zero( im_flag ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_ddic_reference.
    DATA: lo_column_list TYPE REF TO cl_salv_column_list.
    TRY.
        lo_column_list ?= mo_columns->get_column( im_colname ).
        lo_column_list->set_ddic_reference( value = VALUE #( table = im_tabname field = COND #( WHEN im_fieldname IS INITIAL THEN im_colname ELSE im_fieldname ) ) ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_events.
    DATA: ls_events TYPE slis_alv_event.
    LOOP AT mt_events INTO ls_events WHERE NOT form IS INITIAL.
      CASE ls_events-name.
        WHEN events-top_of_page.
          cb_top_of_page  = ls_events-form.
        WHEN events-end_of_page.
          cb_end_of_page = ls_events-form.
        WHEN events-before_salv_function.
          cb_before_salv_function = ls_events-form.
        WHEN events-after_salv_function.
          cb_after_salv_function   = ls_events-form.
        WHEN events-added_function.
          cb_added_function = ls_events-form.
        WHEN events-double_click.
          cb_double_click = ls_events-form.
        WHEN events-link_click.
          cb_link_click = ls_events-form.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_f4_checktable.
    DATA: lo_column_list TYPE REF TO cl_salv_column_list.
    TRY.
        lo_column_list ?= mo_columns->get_column( im_colname ).
        lo_column_list->set_f4_checktable( im_tabname ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_rowno.
    LOOP AT ch_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      ASSIGN COMPONENT rowno_fname OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_col>).
      IF sy-subrc = 0.
        <fs_col> = sy-tabix.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_rowno_fname.
    rowno_fname = im_colname.
  ENDMETHOD.


  METHOD zif_salv_event_receiver~salv_added_function.
    PERFORM (cb_added_function) IN PROGRAM (mv_repid) IF FOUND USING sender
                                                                         e_salv_function. "salv_de_function
  ENDMETHOD.


  METHOD zif_salv_event_receiver~salv_after_salv_function.
    TYPES: BEGIN OF seln_ty,
             column TYPE string,
             r_seln TYPE RANGE OF string,
           END OF seln_ty.
    DATA: lr_filters    TYPE REF TO cl_salv_filters,
          lr_filter     TYPE REF TO cl_salv_filter,
          lt_filters    TYPE salv_t_filter_ref,
          ls_filter     TYPE salv_s_filter_ref,
          ltr_selns     TYPE salv_t_selopt_ref,
          lr_seln       TYPE REF TO cl_salv_selopt,
          lth_sel_range TYPE HASHED TABLE OF seln_ty WITH UNIQUE KEY column,
          ls_sel_range  TYPE seln_ty,
          ls_sel        LIKE LINE OF ls_sel_range-r_seln,
          lv_row_no     TYPE i.
    DATA: lv_flag(1).
    FIELD-SYMBOLS: <fs_col>  TYPE any,
                   <fs_col1> TYPE any,
                   <ft_data> TYPE STANDARD TABLE.

* Build data set to export
    ASSIGN mo_data->* TO <ft_data>.

    lr_filters = mo_table->get_filters( ).
    lt_filters = lr_filters->get( ).

** Go through the filters on each columns, build select ranges
    LOOP AT lt_filters INTO ls_filter.
      ls_sel_range-column = ls_filter-columnname.
      CLEAR ls_sel_range-r_seln.
      ltr_selns = ls_filter-r_filter->get( ).
      LOOP AT ltr_selns INTO lr_seln.
        ls_sel-sign   = lr_seln->get_sign( ).
        ls_sel-option = lr_seln->get_option( ).
        ls_sel-low    = lr_seln->get_low( ).
        ls_sel-high   = lr_seln->get_high( ).
        INSERT ls_sel INTO TABLE ls_sel_range-r_seln.
      ENDLOOP.

      INSERT ls_sel_range INTO TABLE lth_sel_range.
    ENDLOOP.

* Go through the data eliminating records that don't match the filters
    LOOP AT <ft_data> ASSIGNING FIELD-SYMBOL(<fs_data>).
      ASSIGN COMPONENT rowno_fname OF STRUCTURE <fs_data> TO <fs_col>.
      CHECK sy-subrc = 0.
      CLEAR: lv_flag.
      <fs_col> = sy-tabix.
      LOOP AT lth_sel_range INTO ls_sel_range.
        ASSIGN COMPONENT ls_sel_range-column OF STRUCTURE <fs_data> TO <fs_col1>.
        CHECK <fs_col1> NOT IN ls_sel_range-r_seln.
        lv_flag = 'X'.
        EXIT.
      ENDLOOP.
      IF lv_flag = ''.
        ADD 1 TO lv_row_no.
        <fs_col> = lv_row_no.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_salv_event_receiver~salv_before_salv_function.
  ENDMETHOD.


  METHOD zif_salv_event_receiver~salv_double_click.
    PERFORM (cb_double_click) IN PROGRAM (mv_repid) IF FOUND USING sender   "cl_salv_events_table
                                                                       row      "salv_de_row
                                                                       column.  "salv_de_column
  ENDMETHOD.


  METHOD zif_salv_event_receiver~salv_link_click.
    FIELD-SYMBOLS:<ft_data> TYPE STANDARD TABLE.
    ASSIGN mo_data->* TO <ft_data>.
    CASE column.
      WHEN pernr_fname.
        READ TABLE  <ft_data> ASSIGNING FIELD-SYMBOL(<fs_data>) INDEX row.
        CHECK sy-subrc = 0.
        ASSIGN COMPONENT column OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_col>).
        SET PARAMETER ID 'PER' FIELD <fs_col>.
        CALL TRANSACTION 'PA20'.
      WHEN OTHERS.
        PERFORM (cb_link_click) IN PROGRAM (mv_repid) IF FOUND USING sender   "cl_salv_events_table
                                                                         row      "salv_de_row
                                                                         column.  "salv_de_column
    ENDCASE.
  ENDMETHOD.


  METHOD set_column.
    DATA:
      lr_column   TYPE REF TO cl_salv_column_table,
      lt_col_list TYPE salv_t_column_ref,
      ls_col_list TYPE salv_s_column_ref,
      ls_header   TYPE dd01v,
      ls_ddic     TYPE salv_s_ddic_reference,
      lv_domain   TYPE domname.

*   SET THE HOTSPOT FOR PERNR COLUMN
    set_column_hotspot('PERNR' ).
*   SET COLOR NAME
    IF im_color_name IS NOT INITIAL.
      set_column_color( im_colname = im_color_name ).
    ENDIF.
    lt_col_list = mo_columns->get( ).
    LOOP AT lt_col_list INTO ls_col_list.
      CLEAR : ls_header, lv_domain.
      TRY.
          lr_column ?= mo_columns->get_column( columnname = ls_col_list-columnname ).
          "设置字段默认带符号,默认不显示0
          DATA(lv_inttype) = lr_column->get_ddic_inttype( ).
          IF lv_inttype CA 'IPF'.
            lr_column->set_sign( abap_false ).
            lr_column->set_zero( abap_false ).
          ENDIF.

          IF im_t_hide[] IS NOT INITIAL.
            READ TABLE im_t_hide TRANSPORTING NO FIELDS WITH TABLE KEY fieldname = ls_col_list-columnname.
            IF sy-subrc = 0.
              lr_column->set_visible( cl_salv_column_table=>false ).
              lr_column->set_technical( if_salv_c_bool_sap=>true ).
            ENDIF.
          ENDIF.
          IF im_t_text[] IS NOT INITIAL.
            READ TABLE im_t_text INTO DATA(ls_text) WITH TABLE KEY fieldname = ls_col_list-columnname.
            IF sy-subrc = 0.
              lr_column->set_short_text( CONV #( ls_text-text ) ).
              lr_column->set_medium_text( CONV #( ls_text-text ) ).
              lr_column->set_long_text( CONV #( ls_text-text ) ).
            ENDIF.
          ENDIF.

          IF im_t_key[] IS NOT INITIAL.
            READ TABLE im_t_key TRANSPORTING NO FIELDS WITH TABLE KEY fieldname = ls_col_list-columnname.
            IF sy-subrc = 0.
              set_column_key( im_colname = ls_col_list-columnname ).
            ENDIF.
          ENDIF.

          IF im_t_hotspot[] IS NOT INITIAL.
            READ TABLE im_t_hotspot TRANSPORTING NO FIELDS WITH TABLE KEY fieldname = ls_col_list-columnname.
            IF sy-subrc = 0.
              set_column_hotspot( im_colname = ls_col_list-columnname ).
            ENDIF.
          ENDIF.

          IF im_t_color[] IS NOT INITIAL.
            READ TABLE im_t_color INTO DATA(ls_color) WITH TABLE KEY fieldname = ls_col_list-columnname.
            IF sy-subrc = 0.
              set_column_colors( im_colname = ls_col_list-columnname im_color = ls_color-color ).
            ENDIF.
          ENDIF.

          "设置字段参考表
          lv_domain = lr_column->get_ddic_domain( ).
          cl_reca_ddic_doma=>get_complete(
            EXPORTING
              id_name   = lv_domain
            IMPORTING
              es_header = ls_header
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2 ).

          IF sy-subrc EQ 0 AND ls_header-entitytab IS NOT INITIAL.
            MOVE ls_header-entitytab TO ls_ddic-table.
            MOVE lv_domain           TO ls_ddic-field.
            SELECT COUNT( * ) FROM dd03vv WHERE tabname = ls_ddic-table AND fieldname = ls_ddic-field.
            CHECK sy-subrc  = 0.
            lr_column->set_ddic_reference( value = ls_ddic ).
          ENDIF.
        CATCH cx_salv_not_found .
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD pbo.
    DATA: lr_functions TYPE REF TO cl_salv_functions_list,
          lt_func_list TYPE salv_t_ui_func,
          lr_selection TYPE REF TO cl_salv_selections,
          lr_dspset    TYPE REF TO cl_salv_display_settings,
          lr_layout    TYPE REF TO cl_salv_layout,
          ls_key       TYPE salv_s_layout_key,
          lf_variant   TYPE slis_vari.
    FIELD-SYMBOLS: <ft_data> TYPE STANDARD TABLE.
    TRY.
        ASSIGN mo_data->* TO <ft_data>.
        IF mo_container IS BOUND.
          cl_salv_table=>factory( EXPORTING r_container    = mo_container
                                            container_name = mv_container_name
                                  IMPORTING r_salv_table   = mo_table
                                  CHANGING  t_table        = <ft_data> ).
        ELSEIF mt_function IS INITIAL.
          cl_salv_table=>factory( IMPORTING r_salv_table = mo_table CHANGING t_table = <ft_data> ).
        ENDIF.

        lr_selection = mo_table->get_selections( ).
        lr_selection->set_selection_mode( if_salv_c_selection_mode=>row_column )."可以以行、列的方式进行选择

        "设置自定义状态栏
        IF mv_pfstatus IS NOT INITIAL.
          mo_table->set_screen_status( pfstatus      = mv_pfstatus
                                       report        = COND #( WHEN mv_pfreport IS NOT INITIAL THEN mv_pfreport ELSE mv_repid )
                                       set_functions = mo_table->c_functions_all
                                       excluding     = mt_excluding
                                       ).
        ENDIF.
        lr_functions = mo_table->get_functions( ).
        IF mt_function IS NOT INITIAL.
          LOOP AT mt_function INTO DATA(ls_function).
            lr_functions->add_function(
              name     = ls_function-name
              icon     = CONV #( ls_function-icon )
              text     = ls_function-text
              tooltip  = ls_function-tooltip
              position = COND #( WHEN ls_function-position IS NOT INITIAL THEN ls_function-position ELSE if_salv_c_function_position=>right_of_salv_functions )
            ).
          ENDLOOP.
        ENDIF.
        lr_functions->set_default( abap_true ).
        lr_functions->set_all( abap_true ).
        lt_func_list = lr_functions->get_functions( ).

        mo_columns = mo_table->get_columns( ).
        mo_columns->set_optimize( abap_true ).

        set_column( im_t_hide     = mt_hide
                    im_t_text     = mt_text
                    im_t_key      = mt_key
                    im_t_hotspot  = mt_hotspot
                    im_t_color    = mt_color
                    im_color_name = mv_color_name ).

*****设置斑马线*****
        lr_dspset = mo_table->get_display_settings( ).
        lr_dspset->set_striped_pattern( abap_true ).
*设置title
        IF mv_title IS NOT INITIAL AND mt_function IS INITIAL.
          lr_dspset->set_list_header( mv_title ).
        ENDIF.

*设置事件
        mo_events  = mo_table->get_event(  ) .
        SET HANDLER: me->zif_salv_event_receiver~salv_link_click FOR mo_events,
                     me->zif_salv_event_receiver~salv_double_click FOR mo_events,
                     me->zif_salv_event_receiver~salv_added_function FOR mo_events,
                     me->zif_salv_event_receiver~salv_after_salv_function FOR mo_events.


        lr_layout = mo_table->get_layout( ).
        ls_key-report = mv_repid.
        ls_key-handle = mv_handle.
        lr_layout->set_key( ls_key ).
        lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
        lr_layout->set_default( abap_true )."可以保存为默认布局
      CATCH cx_salv_msg.
        "HANDLE EXCEPTION
      CATCH cx_salv_existing cx_salv_wrong_call.
        "handle exception
    ENDTRY.
  ENDMETHOD.


  METHOD zif_salv_event_receiver~salv_end_of_page.

  ENDMETHOD.


  METHOD zif_salv_event_receiver~salv_top_of_page.

  ENDMETHOD.


  METHOD set_container.
    mo_container = im_container.
    mv_container_name = im_container_name.
  ENDMETHOD.


  METHOD pai.
    DATA: ucomm TYPE sy-ucomm.
    ucomm = cv_ucomm.
    CLEAR cv_ucomm.
    CHECK ucomm NE space.
    CASE ucomm.
      WHEN 'BACK'.
        ucomm = '&F03'.
      WHEN 'RW'.
        ucomm = '&F12'.
      WHEN '%EX'.
        ucomm = '&F15'.
      WHEN '%PRI'.
        ucomm = '&PRINT'.
      WHEN '%SC'.
        ucomm = '&FIND'.
      WHEN '%SC+'.
        ucomm = '&FIND_MORE'.
    ENDCASE.
    IF ucomm = '&F03' OR
       ucomm = '&F12' OR
       ucomm = '&F15'.
      LEAVE TO SCREEN 0.
    ELSE.
      mo_table->set_function( ucomm ).
    ENDIF.

  ENDMETHOD.


  METHOD add_button.
    APPEND VALUE #( name = im_name icon = im_icon text = im_text tooltip = im_tooltip position = im_position ) TO mt_function.
  ENDMETHOD.
ENDCLASS.
