*&---------------------------------------------------------------------*
*& Include          ZSD_SHIPAGILE_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

      on_single_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column,

      on_double_click_alv2 FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

      on_single_click_alv2 FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_double_click.
    PERFORM show_cell_info USING 0 row column TEXT-i05.
  ENDMETHOD.                    "on_double_click

  METHOD on_single_click.
    PERFORM show_cell_info USING 0 row column TEXT-i04.
  ENDMETHOD.                    "on_single_click

  METHOD on_double_click_alv2.

  ENDMETHOD.                    "on_double_click

  METHOD on_single_click_alv2.

  ENDMETHOD.
ENDCLASS.
