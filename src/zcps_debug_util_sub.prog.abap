
*&---------------------------------------------------------------------*
*& Include          ZSD_SHIPAGILE_SUBROUTINE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form MAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main_report .

  PERFORM get_data.
* initialize handler

  CREATE OBJECT o_event_handler.

  CALL SCREEN 100.

ENDFORM.
FORM get_data.
  DATA : lt_vttp TYPE STANDARD TABLE OF  vttp.
  SELECT vbeln FROM likp INTO CORRESPONDING FIELDS OF TABLE gt_final UP TO 20 ROWS.
ENDFORM.

FORM display_grid.

  IF o_alv IS NOT BOUND  AND  o_container IS NOT BOUND.

    CREATE OBJECT o_container
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = o_container->dock_at_left
        extension = 300.

    cl_salv_table=>factory(
    EXPORTING
    list_display   = if_salv_c_bool_sap=>false
      r_container    = o_container
      container_name = 'NAME'
      IMPORTING
        r_salv_table   = o_alv
      CHANGING
        t_table        = gt_final
           ).

    o_functions = o_alv->get_functions( ).
    o_functions->set_all( 'X' ).

    o_columns = o_alv->get_columns( ).
*    o_columns->set_optimize('X').

    TRY.
        o_column ?= o_columns->get_column( 'CHECKBOX' ).
        o_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
        o_column->set_long_text( 'Selection' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        o_column ?= o_columns->get_column( 'VBELN' ).
        o_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    o_alv_events = o_alv->get_event( ).


    SET HANDLER o_event_handler->on_double_click FOR o_alv_events.
*... §7.3 register to the event LINK_CLICK
    SET HANDLER o_event_handler->on_single_click FOR o_alv_events.
*display
    o_alv->display( ).
  ENDIF.
ENDFORM.
FORM show_cell_info USING i_level  TYPE i
                          i_row    TYPE i
                          i_column TYPE lvc_fname
                          i_text   TYPE string.

  DATA: delivery TYPE vbeln_vl.

  CASE  i_column .
    WHEN 'CHECKBOX'.
      PERFORM toggle_checkboc  USING i_level
                                     i_row   .
    WHEN 'VBELN'."Delivery
      PERFORM display_delivery_payload  USING  i_level
                                               i_row.
    WHEN 'THNUM'."Shipment

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
FORM display_document.


  PERFORM popup_to_confirm USING  TEXT-002
                           CHANGING answer .

  FIELD-SYMBOLS <fs> LIKE LINE OF gt_final.
  READ TABLE gt_final ASSIGNING <fs> WITH KEY  checkbox = c_x.
  IF <fs> IS ASSIGNED.
    CASE answer .
      WHEN '1'.
        SET PARAMETER ID  'TNR' FIELD <fs>-tknum.
        CALL TRANSACTION 'VT02N' AND SKIP FIRST SCREEN.
      WHEN  '2'.
        SET PARAMETER ID  'VL' FIELD <fs>-vbeln.
        CALL TRANSACTION 'VL02N' AND SKIP FIRST SCREEN.
    ENDCASE.
  ELSE.
    MESSAGE TEXT-e01 TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TOGGLE_CHECKBOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_<FS>_CHECKBOX  text
*&---------------------------------------------------------------------*
FORM toggle_checkboc  USING i_level  TYPE i
                            i_row    TYPE i.

  FIELD-SYMBOLS <fs> LIKE LINE OF gt_final.
  READ TABLE gt_final ASSIGNING <fs> INDEX i_row.
  IF <fs> IS ASSIGNED.
    IF <fs>-checkbox EQ c_x.
      CLEAR <fs>-checkbox.
    ELSEIF  <fs>-checkbox NE c_x.
      <fs>-checkbox = c_x.
    ENDIF.
    IF o_alv IS BOUND.
      o_alv->refresh( ).
    ENDIF.
  ENDIF.
ENDFORM.
FORM popup_to_confirm    USING  text TYPE itex132
                         CHANGING answer TYPE c.

  CALL FUNCTION 'POPUP_WITH_2_BUTTONS_TO_CHOOSE'
    EXPORTING
      defaultoption = '1'
      diagnosetext1 = text
*     DIAGNOSETEXT2 = ' '
*     DIAGNOSETEXT3 = ' '
      textline1     = ' '
*     TEXTLINE2     = ' '
*     TEXTLINE3     = ' '
      text_option1  = TEXT-003
      text_option2  = TEXT-004
      titel         = TEXT-005
    IMPORTING
      answer        = answer.

ENDFORM.
FORM   display_delivery_payload USING i_level  TYPE i
                                      i_row    TYPE i.

  DATA: delivery TYPE vbeln_vl.

  FIELD-SYMBOLS :<fs> LIKE LINE OF gt_final.

  READ TABLE gt_final ASSIGNING <fs> INDEX i_row.
  IF <fs> IS ASSIGNED.
*Get delivery
    delivery = <fs>-vbeln.
*Fill gt_payload
    PERFORM  fill_components_values.
* Perform ALV2 refresh
    o_alv2->refresh(  ).
  ENDIF.

ENDFORM.
FORM  get_components .
  DATA : wa_payload TYPE ty_payload.

  go_strucdescr ?= cl_abap_typedescr=>describe_by_data( data_payload ).

  gt_components_delivery =  go_strucdescr->components .

  LOOP AT gt_components_delivery INTO  gs_components.
    wa_payload-component = gs_components-name.
    APPEND wa_payload TO gt_payload.
  ENDLOOP.
  CLEAR :wa_payload,gt_components_delivery,gs_components.
ENDFORM.
FORM alv_grid_delivery.


*set header
  DATA: lo_header  TYPE REF TO cl_salv_form_layout_grid,
        lo_h_label TYPE REF TO cl_salv_form_label,
        lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.

  IF o_alv2 IS NOT BOUND  AND  o_container2 IS NOT BOUND.

    CREATE OBJECT o_container2
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = o_container2->dock_at_left
        extension = 500.

    cl_salv_table=>factory(
    EXPORTING
    list_display   = if_salv_c_bool_sap=>false
      r_container    = o_container2
      container_name = 'NAME'
      IMPORTING
        r_salv_table   = o_alv2
      CHANGING
        t_table        = gt_payload
           ).

    o_functions = o_alv2->get_functions( ).
    o_functions->set_all( 'X' ).

    o_columns = o_alv2->get_columns( ).
    o_columns->set_optimize('X').

    TRY.
        o_column ?= o_columns->get_column( 'COMPONENT' ).
*        o_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
        o_column->set_long_text( ' Field  ' ).
        o_column->set_short_text( ' Field ' ).
        o_column->set_medium_text( ' Field ' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        o_column ?= o_columns->get_column( 'VALUE' ).
*        o_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
        o_column->set_long_text( '  Field Value  ' ).
        o_column->set_short_text( 'Value' ).
        o_column->set_medium_text( ' Field Value ' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    o_alv_events = o_alv2->get_event( ).

*
*   header object
    CREATE OBJECT lo_header.
*
*   To create a Lable or Flow we have to specify the target
*     row and column number where we need to set up the output
*     text.
*
*   information in Bold
    lo_h_label = lo_header->create_label( row = 1 column = 1 ).
    lo_h_label->set_text( 'Delivery Payload' ).
*

*   set the top of list using the header for Online.
    o_alv2->set_top_of_list( lo_header ).
*
*   set the top of list using the header for Print.
    o_alv2->set_top_of_list_print( lo_header ).

    SET HANDLER o_event_handler->on_double_click_alv2 FOR o_alv_events.
*... §7.3 register to the event LINK_CLICK
    SET HANDLER o_event_handler->on_single_click_alv2 FOR o_alv_events.
*display
    o_alv2->display( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_COMPONENTS_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_components_values .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_TABSTRIP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM handle_tabstrip USING ok_code.
  PERFORM ok_code_screen_2000  USING sy-dynnr ok_code.
ENDFORM.
FORM ok_code_screen_2000 USING    dynnr TYPE sydynnr
                           ucomm TYPE syucomm.

  IF dynnr EQ  2000.
    CASE ucomm.
      WHEN c_back.
        LEAVE TO SCREEN 0.
      WHEN  c_f8.
        PERFORM main_report.
    ENDCASE.
  ENDIF.

ENDFORM.

FORM ok_code_screen_3000  USING     dynnr TYPE sydynnr
                                           ucomm TYPE syucomm.

  IF dynnr EQ 3000.
    CASE ucomm.

      WHEN c_back.
        LEAVE TO SCREEN 0.
      WHEN c_cancel OR c_exit.
        LEAVE PROGRAM.
    ENDCASE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_SINGLE_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_single_log .

  DATA : s_doc TYPE RANGE OF  zshipagile_log-document.
  DATA : wa_doc  LIKE LINE  OF s_doc.
  DATA : s_doctyp TYPE RANGE OF  zshipagile_log-document_type.
  DATA : wa_doctyp  LIKE LINE  OF s_doctyp.

  wa_doc-sign = wa_doctyp-sign ='I'.
  wa_doc-option = wa_doctyp-option ='EQ'.


  PERFORM popup_to_confirm  USING  TEXT-015
                            CHANGING answer .

  FIELD-SYMBOLS <fs> LIKE LINE OF gt_final.
  READ TABLE gt_final ASSIGNING <fs> WITH KEY  checkbox = c_x.
  IF <fs> IS ASSIGNED.
    CASE answer .
      WHEN '1'."shipment
        wa_doc-low = <fs>-tknum.
        APPEND wa_doc TO s_doc[].
        wa_doctyp-low = 'S'.
        APPEND wa_doctyp TO s_doctyp.
        CLEAR:wa_doc,wa_doctyp.
      WHEN  '2'."delivery
        wa_doc-low =  <fs>-vbeln.
        APPEND wa_doc TO s_doc[].
        wa_doctyp-low = 'D'.
        APPEND wa_doctyp TO s_doctyp.
        CLEAR:wa_doc,wa_doctyp.
    ENDCASE.
    IF s_doc[] IS NOT  INITIAL AND s_doctyp[] IS  NOT INITIAL.
      SUBMIT zshipagile_log WITH s_doc IN  s_doc  WITH s_doctyp IN s_doctyp.
    ENDIF.
  ELSE.
    MESSAGE TEXT-e01 TYPE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .


  PERFORM  get_components.

  tab-prog = sy-repid.
  tab-dynnr = 2000.
  tab-activetab = c_tab1.

*Tab text
  tab1  = TEXT-007. "config




ENDFORM.


FORM lock_table USING table  TYPE rstable-tabname.
  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      tabname        = table
*     VARKEY         =
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
FORM unlock_table USING table TYPE rstable-tabname.
  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
*     MODE_RSTABLE       = 'E'
      tabname = table.
ENDFORM.
