INTERFACE zif_salv_event_receiver
  PUBLIC .


  METHODS salv_top_of_page
    FOR EVENT top_of_page OF cl_salv_events_table
    IMPORTING
      !r_top_of_page
      !page
      !table_index
      !sender .
  METHODS salv_end_of_page
    FOR EVENT end_of_page OF cl_salv_events_table
    IMPORTING
      !r_end_of_page
      !page
      !sender .
  METHODS salv_before_salv_function
    FOR EVENT before_salv_function OF cl_salv_events_table
    IMPORTING
      !e_salv_function
      !sender .
  METHODS salv_after_salv_function
    FOR EVENT after_salv_function OF cl_salv_events_table
    IMPORTING
      !e_salv_function
      !sender .
  METHODS salv_added_function
    FOR EVENT added_function OF cl_salv_events_table
    IMPORTING
      !e_salv_function
      !sender .
  METHODS salv_double_click
    FOR EVENT double_click OF cl_salv_events_table
    IMPORTING
      !row
      !column
      !sender .
  METHODS salv_link_click
    FOR EVENT link_click OF cl_salv_events_table
    IMPORTING
      !row
      !column
      !sender .
ENDINTERFACE.
