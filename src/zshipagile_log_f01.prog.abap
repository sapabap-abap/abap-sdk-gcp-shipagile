*&---------------------------------------------------------------------*
*& Include          ZSHIPAGILE_LOG_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form MAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main .
  DATA : lt_log  TYPE STANDARD TABLE OF zshipagile_log.
  PERFORM get_log_data CHANGING lt_log.
  PERFORM display_log USING  lt_log.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LOG_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_log_data CHANGING lt_log  TYPE STANDARD TABLE.
  SELECT * FROM zshipagile_log
  INTO TABLE lt_log
  WHERE zdate     IN s_date  AND
        user_name IN s_uname AND
        document  IN s_doc   AND
        document_type IN s_doctyp."#EC CI_NOFIELD
ENDFORM.
FORM display_log USING lt_log TYPE STANDARD TABLE.

  DATA: o_columns TYPE REF TO cl_salv_columns_table,
        o_column  TYPE REF TO cl_salv_column_table,
        o_alv2    TYPE REF TO cl_salv_table.
*set header
  DATA: lo_header   TYPE REF TO cl_salv_form_layout_grid,
        lo_h_label  TYPE REF TO cl_salv_form_label,
        lo_h_flow   TYPE REF TO cl_salv_form_layout_flow,
        o_functions TYPE REF TO cl_salv_functions_list.

  DATA: lo_excep TYPE REF TO cx_root.


  TRY.
      cl_salv_table=>factory(
      EXPORTING
      list_display   = if_salv_c_bool_sap=>false
        container_name = 'NAME'
        IMPORTING
          r_salv_table   = o_alv2
        CHANGING
          t_table        = lt_log
             ).


      o_functions = o_alv2->get_functions( ).
      o_functions->set_all( 'X' ).

      o_columns = o_alv2->get_columns( ).
      o_columns->set_optimize('X').


*   header object
      CREATE OBJECT lo_header.
*
*   To create a Lable or Flow we have to specify the target
*     row and column number where we need to set up the output
*     text.
*
*   information in Bold
      lo_h_label = lo_header->create_label( row = 1 column = 1 ).
      lo_h_label->set_text( TEXT-003 ).
*
      lo_h_flow = lo_header->create_flow( row = 3  column = 1 ).
      lo_h_flow->create_text( text = TEXT-004  ).

      lo_h_flow = lo_header->create_flow( row = 3  column = 2 ).
      lo_h_flow->create_text( text = sy-datum ).

      lo_h_flow = lo_header->create_flow( row = 4  column = 2 ).
      lo_h_flow->create_text( text = TEXT-005 ).

* Hide column

      o_columns = o_alv2->get_columns( ).
      o_column ?= o_columns->get_column('MANDT').
      o_column->set_visible( if_salv_c_bool_sap=>false ).

      o_columns = o_alv2->get_columns( ).
      o_column ?= o_columns->get_column('GUID').
      o_column->set_visible( if_salv_c_bool_sap=>false ).

* set the top of list using the header for Online.
      o_alv2->set_top_of_list( lo_header ).
*
* set the top of list using the header for Print.
      o_alv2->set_top_of_list_print( lo_header ).
    CATCH cx_salv_not_found INTO lo_excep.
      IF lo_excep IS BOUND.
        WRITE : lo_excep->get_text( ).
      ENDIF.
    CATCH  cx_salv_msg INTO lo_excep.
      IF lo_excep IS BOUND.
        WRITE : lo_excep->get_text( ).
      ENDIF.
  ENDTRY.
*display
  o_alv2->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_OK_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM handle_ok_code .
  CONSTANTS: c_b1(2) TYPE c VALUE 'B1'.
  CONSTANTS: c_cps_log(9) TYPE c VALUE 'ZCPS_LOG'.
  CONSTANTS: c_balobj(9) TYPE c VALUE 'BALOBJ'.
  CONSTANTS: c_balsubobj(9) TYPE c VALUE 'BALSUBOBJ'.
  CONSTANTS: c_tcode(9) TYPE c VALUE 'SLG1'.
  CASE sy-ucomm.
    WHEN c_b1.
      SET  PARAMETER ID c_balobj  FIELD c_cps_log.
      SET  PARAMETER ID c_balsubobj  FIELD c_cps_log.
      CALL TRANSACTION c_tcode."#EC CI_CALLTA
  ENDCASE.
ENDFORM.
