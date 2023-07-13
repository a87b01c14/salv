class ZCL_SALV definition
  public
  final
  create public .

public section.

  interfaces ZIF_SALV_EVENT_RECEIVER .

  data MV_REPID type SY-REPID .
  data MT_EVENTS type SLIS_T_EVENT .
  constants:
    BEGIN OF events,
        top_of_page          TYPE slis_formname VALUE 'TOP_OF_PAGE',
        end_of_page          TYPE slis_formname VALUE 'END_OF_PAGE',
        before_salv_function TYPE slis_formname VALUE 'BEFORE_SALV_FUNCTION',
        after_salv_function  TYPE slis_formname VALUE 'AFTER_SALV_FUNCTION',
        added_function       TYPE slis_formname VALUE 'ADDED_FUNCTION',
        double_click         TYPE slis_formname VALUE 'DOUBLE_CLICK',
        link_click           TYPE slis_formname VALUE 'LINK_CLICK',
      END OF events .
  data CB_TOP_OF_PAGE type SLIS_FORMNAME value 'FRM_SALV_TOP_OF_PAGE' ##NO_TEXT.
  data CB_END_OF_PAGE type SLIS_FORMNAME value 'FRM_SALV_END_OF_PAGE' ##NO_TEXT.
  data CB_BEFORE_SALV_FUNCTION type SLIS_FORMNAME value 'FRM_SALV_BEFORE_SALV_FUNCTION' ##NO_TEXT.
  data CB_AFTER_SALV_FUNCTION type SLIS_FORMNAME value 'FRM_SALV_AFTER_SALV_FUNCTION' ##NO_TEXT.
  data CB_ADDED_FUNCTION type SLIS_FORMNAME value 'FRM_SALV_ADDED_FUNCTION' ##NO_TEXT.
  data CB_DOUBLE_CLICK type SLIS_FORMNAME value 'FRM_SALV_DOUBLE_CLICK' ##NO_TEXT.
  data CB_LINK_CLICK type SLIS_FORMNAME value 'FRM_SALV_LINK_CLICK' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      value(IM_REPID) type SY-REPID optional
      value(IM_TABLE) type STANDARD TABLE
      !IM_CONTAINER type ref to CL_GUI_CONTAINER optional
      !IM_CONTAINER_NAME type STRING optional
      value(IM_ROW_NAME) type LVC_FNAME default 'ROW_NO'
      !IM_HANDLE type SLIS_HANDL optional
      !IM_PFSTATUS type SYPFKEY optional
      !IM_PFREPORT type SY-REPID optional
      !IM_T_EXCLUDING type KKBLO_T_EXTAB optional
      !IM_T_EVENTS type SLIS_T_EVENT optional .
  methods DISPLAY .
  methods HIDE_COLUMN
    importing
      value(IM_COLNAME) type LVC_FNAME .
  methods SET_COLUMN_F4
    importing
      value(IM_COLNAME) type LVC_FNAME
      value(IM_VALUE) type SAP_BOOL .
  methods SET_COLUMN_TEXT
    importing
      value(IM_COLNAME) type LVC_FNAME
      value(IM_TEXT) type STRING .
  methods SET_COLUMN_POSITION
    importing
      value(IM_COLNAME) type LVC_FNAME
      value(IM_POSITION) type I .
  methods SET_COLUMN_SIGN
    importing
      value(IM_COLNAME) type LVC_FNAME
      value(IM_FLAG) type C .
  methods SET_COLUMN_ZERO
    importing
      value(IM_COLNAME) type LVC_FNAME
      value(IM_FLAG) type C .
  methods SET_COLUMN_HOTSPOT
    importing
      value(IM_COLNAME) type LVC_FNAME .
  methods SET_COLUMN_KEY
    importing
      value(IM_COLNAME) type LVC_FNAME .
  methods SET_COLUMN_COLOR
    importing
      value(IM_COLNAME) type LVC_FNAME .
  methods SET_COLUMN_COLORS
    importing
      value(IM_COLNAME) type LVC_FNAME
      !IM_COLOR type LVC_COL .
  methods SET_F4_CHECKTABLE
    importing
      value(IM_COLNAME) type LVC_FNAME
      value(IM_TABNAME) type TABNAME .
  methods SET_ROWNO_FNAME
    importing
      value(IM_COLNAME) type LVC_FNAME .
  methods SET_DDIC_REFERENCE
    importing
      value(IM_COLNAME) type LVC_FNAME
      value(IM_TABNAME) type TABNAME .
  methods GET_SELECTED_ROWS
    returning
      value(RE_VALUE) type SALV_T_ROW .
  methods REFRESH .
  methods DELETE_DATA
    importing
      value(IM_INDEX) type I .
  methods GET_SELECTED_DATA
    returning
      value(RE_TABLE) type ref to DATA .
  methods GET_DATA
    returning
      value(RE_VALUE) type ref to DATA .
  methods GET_EVENT
    returning
      value(RO_EVENT) type ref to CL_SALV_EVENTS_TABLE .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_excluding TYPE kkblo_t_extab .
    DATA rt_data TYPE REF TO data .
    DATA r_table TYPE REF TO cl_salv_table .
    DATA r_columns TYPE REF TO cl_salv_columns .
    DATA r_events TYPE REF TO cl_salv_events_table .
    DATA rowno_fname TYPE lvc_fname VALUE 'ROW_NO' ##NO_TEXT.
    CLASS-DATA pernr_fname TYPE lvc_fname VALUE 'PERNR' ##NO_TEXT.

    METHODS set_rowno
      CHANGING
        !ch_data TYPE STANDARD TABLE .
    METHODS set_events .
ENDCLASS.



CLASS ZCL_SALV IMPLEMENTATION.


  METHOD constructor.
    DATA: lr_functions TYPE REF TO cl_salv_functions_list,
          lt_func_list TYPE salv_t_ui_func,
          lr_selection TYPE REF TO cl_salv_selections,
          lr_column    TYPE REF TO cl_salv_column_table,
          lr_dspset    TYPE REF TO cl_salv_display_settings,
          lr_layout    TYPE REF TO cl_salv_layout,
          ls_key       TYPE salv_s_layout_key,
          lf_variant   TYPE slis_vari.

    DATA: lt_col_list TYPE salv_t_column_ref,
          ls_col_list TYPE salv_s_column_ref,
          ls_header   TYPE dd01v,
          ls_ddic     TYPE salv_s_ddic_reference,
          lv_domain   TYPE domname.

    FIELD-SYMBOLS: <rt_data> TYPE STANDARD TABLE.

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
    rowno_fname = im_row_name.
    mt_events = im_t_events.
    CREATE DATA rt_data LIKE im_table.
    ASSIGN rt_data->* TO <rt_data>.
    <rt_data>[] = im_table[].
    "设置行号
    set_rowno( CHANGING ch_data = <rt_data> ).

    "设置事件
    set_events( ).

    TRY.
        IF im_container IS NOT INITIAL.
          cl_salv_table=>factory( EXPORTING r_container    = im_container
                                            container_name = im_container_name
                                  IMPORTING r_salv_table   = r_table
                                  CHANGING  t_table        = <rt_data> ).
        ELSE.
          cl_salv_table=>factory( IMPORTING r_salv_table = r_table CHANGING t_table = <rt_data> ).
        ENDIF.
      CATCH cx_salv_msg.
        "HANDLE EXCEPTION
    ENDTRY.
    lr_selection = r_table->get_selections( ).
    lr_selection->set_selection_mode( if_salv_c_selection_mode=>row_column )."可以以行、列的方式进行选择

    "设置自定义状态栏
    IF im_pfstatus IS NOT INITIAL.
      r_table->set_screen_status( pfstatus      = im_pfstatus
                                  report        = COND #( WHEN im_pfreport IS NOT INITIAL THEN im_pfreport ELSE mv_repid )
                                  set_functions = r_table->c_functions_all
*XCLUDING     = MT_EXCLUDING
                                  ).
    ENDIF.
    lr_functions = r_table->get_functions( ).
    lr_functions->set_default( abap_true ).
    lr_functions->set_all( abap_true ).
    lt_func_list = lr_functions->get_functions( ).

    r_columns = r_table->get_columns( ).
    r_columns->set_optimize( abap_true ).

    lt_col_list = r_columns->get( ).
    LOOP AT lt_col_list INTO ls_col_list.
      CLEAR : ls_header, lv_domain.
      TRY.
          lr_column ?= r_columns->get_column( columnname = ls_col_list-columnname ).
          "设置字段默认带符号,默认不显示0
          DATA(lv_inttype) = lr_column->get_ddic_inttype( ).
          IF lv_inttype CA 'IPF'.
            lr_column->set_sign( abap_false ).
            lr_column->set_zero( abap_false ).
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



*****设置斑马线*****
    lr_dspset = r_table->get_display_settings( ).
    lr_dspset->set_striped_pattern( abap_true ).
*
*   SET THE HOTSPOT FOR PERNR COLUMN
    set_column_hotspot('PERNR' ).

*设置事件
    r_events  = r_table->get_event(  ) .
    SET HANDLER: me->zif_salv_event_receiver~salv_link_click FOR r_events,
                 me->zif_salv_event_receiver~salv_double_click FOR r_events,
                 me->zif_salv_event_receiver~salv_added_function FOR r_events,
                 me->zif_salv_event_receiver~salv_after_salv_function FOR r_events.


    lr_layout = r_table->get_layout( ).
    ls_key-report = im_repid.
    ls_key-handle = im_handle.
    lr_layout->set_key( ls_key ).
    lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    lr_layout->set_default( abap_true )."可以保存为默认布局
  ENDMETHOD.


  METHOD delete_data.
    FIELD-SYMBOLS:<rt_data> TYPE STANDARD TABLE.
    ASSIGN rt_data->* TO <rt_data>.
    DELETE <rt_data> INDEX im_index.
  ENDMETHOD.


  METHOD display.
    r_table->display( ) .
  ENDMETHOD.


  METHOD hide_column .
    DATA: lr_column TYPE REF TO cl_salv_column_table.
    TRY.
        lr_column ?= r_columns->get_column( im_colname ).
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
        lr_column ?= r_columns->get_column( im_colname ).
        lr_column->set_short_text( lv_scrtext_s ).
        lr_column->set_medium_text( lv_scrtext_m ).
        lr_column->set_long_text( lv_scrtext_l ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_column_sign .
    DATA: lr_column    TYPE REF TO cl_salv_column_table.
    TRY.
        lr_column ?= r_columns->get_column( im_colname ).
        lr_column->set_sign( im_flag ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_column_hotspot .
    DATA: lr_column TYPE REF TO cl_salv_column_table.
    TRY.
        lr_column ?= r_columns->get_column( im_colname ).
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
        lo_column_list ?= r_columns->get_column( im_colname ).
        lo_column_list->set_key( if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD get_data.
    re_value = rt_data.
  ENDMETHOD.


  METHOD get_selected_data.
    FIELD-SYMBOLS:<rt_data>  TYPE STANDARD TABLE,
                  <fs_table> TYPE STANDARD TABLE,
                  <fs_row>   TYPE any.
    DATA: lr_selections TYPE REF TO cl_salv_selections,
          lt_rows       TYPE salv_t_row,
          l_row         TYPE i,
          lo_row        TYPE REF TO data.


    ASSIGN rt_data->* TO <rt_data>.
    CREATE DATA re_table LIKE <rt_data>.
    ASSIGN re_table->* TO <fs_table>.
    lr_selections = r_table->get_selections( ).
    lt_rows = lr_selections->get_selected_rows( ).
    LOOP AT lt_rows INTO l_row.
      APPEND INITIAL LINE TO <fs_table> ASSIGNING <fs_row>.
      READ TABLE <rt_data> INTO <fs_row> INDEX l_row.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_selected_rows.
    DATA: lr_selections TYPE REF TO cl_salv_selections.
    lr_selections = r_table->get_selections( ).
    re_value = lr_selections->get_selected_rows( ).
  ENDMETHOD.


  METHOD refresh.
    r_table->refresh( ).
    CALL METHOD me->zif_salv_event_receiver~salv_after_salv_function( r_events ).
  ENDMETHOD.


  METHOD set_column_color .
    DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table.
    TRY.
        lo_cols_tab ?= r_table->get_columns( ).
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
        lo_cols_tab ?= r_table->get_columns( ).
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
        lr_column ?= r_columns->get_column( im_colname ).
        lr_column->set_f4( im_value ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_column_position .
    r_columns->set_column_position( columnname = im_colname
                                    position   = im_position ).
  ENDMETHOD.


  METHOD get_event.
    ro_event = r_events.
  ENDMETHOD.


  METHOD set_column_zero .
    DATA: lr_column    TYPE REF TO cl_salv_column_table.
    TRY.
        lr_column ?= r_columns->get_column( im_colname ).
        lr_column->set_zero( im_flag ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD set_ddic_reference.
    DATA: lo_column_list TYPE REF TO cl_salv_column_list.
    TRY.
        lo_column_list ?= r_columns->get_column( im_colname ).
        lo_column_list->set_ddic_reference( value = VALUE #( table = im_tabname field = im_colname ) ).
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
        lo_column_list ?= r_columns->get_column( im_colname ).
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
                   <rt_data> TYPE STANDARD TABLE.

* Build data set to export
    ASSIGN rt_data->* TO <rt_data>.

    lr_filters = r_table->get_filters( ).
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
    LOOP AT <rt_data> ASSIGNING FIELD-SYMBOL(<fs_data>).
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
    FIELD-SYMBOLS:<rt_data> TYPE STANDARD TABLE.
    ASSIGN rt_data->* TO <rt_data>.
    CASE column.
      WHEN pernr_fname.
        READ TABLE  <rt_data> ASSIGNING FIELD-SYMBOL(<fs_data>) INDEX row.
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
ENDCLASS.
