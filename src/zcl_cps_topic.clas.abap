class ZCL_CPS_TOPIC definition
  public
  final
  create public .

public section.

  interfaces ZIF_TOPIC .

  aliases PUBLISH
    for ZIF_TOPIC~PUBLISH .
protected section.
private section.

  aliases LS_LOG
    for ZIF_TOPIC~LS_LOG .
  aliases GET_AUTH
    for ZIF_TOPIC~GET_AUTH .
  aliases GET_CONFIG
    for ZIF_TOPIC~GET_CONFIG .
  aliases GET_LOGGER
    for ZIF_TOPIC~GET_LOGGER .

  data LO_LOG type ref to ZIF_LOGGER .
  data LO_AUTH type ref to ZIF_AUTH .
  data LO_CONFIG type ref to ZIF_TOPIC_CONFIG .

  methods POST_CALL
    importing
      !BODY type STRING
      !URL type STRING
      !TOKEN type STRING
      !DISPLAY_RESPONSE type FLAG optional
    returning
      value(RESPONSE) type STRING .
ENDCLASS.



CLASS ZCL_CPS_TOPIC IMPLEMENTATION.


  METHOD post_call.

********************************************************************
** Local Data Declarations
********************************************************************

    DATA: lo_http_client      TYPE REF TO if_http_client,
          lo_rest_client      TYPE REF TO cl_rest_http_client,
          lo_response         TYPE REF TO if_rest_entity,
          lr_root             TYPE REF TO cx_root,
          lo_request          TYPE REF TO if_rest_entity,
*          reason              TYPE  string,
*          http_status         TYPE  string,
*          content_length      TYPE  string,
*          location            TYPE  string,
          lv_error            TYPE  string,
          lv_url              TYPE  string,
          lv_value            TYPE  string,
          content_type        TYPE  string,
          lo_cx_bal_exception TYPE REF TO cx_bal_exception.
********************************************************************
**Macro Definition
********************************************************************
    DEFINE add_error ##NEEDED.
      ls_log-msgty = &1.
      ls_log-msgno = &2.
      ls_log-msgid = &3.
      ls_log-msgv1 = &4.

*       add to log
      IF &1 EQ 'E' AND &5 EQ abap_true.
*       *Save log
      lo_log->add_msg( ls_log ).
      lo_log->save( ).
      ELSE.
      lo_log->add_msg( ls_log ).
      ENDIF.
      CLEAR ls_log.
    END-OF-DEFINITION.

    TRY.
********************************************************************
**Get Token
********************************************************************
        IF token IS  INITIAL .
          add_error 'E' '005' 'ZCPS_MESSAGE' '' 'X' ##BOOL_OK."Token is missing
          RETURN.
        ELSE.
          add_error 'S' '006' 'ZCPS_MESSAGE' token '' ##BOOL_OK."Token generated: &1
        ENDIF.

********************************************************************
**Get Url
********************************************************************
        IF url IS INITIAL.
          add_error 'E' '007' 'ZCPS_MESSAGE' '' 'X' ##BOOL_OK." Endpoint URL to topic  is missing in config
          RETURN.
        ELSE.
          add_error 'S' '008' 'ZCPS_MESSAGE' url '' ##BOOL_OK." " Endpoint URL called is &1
        ENDIF.
********************************************************************
** Create Request
********************************************************************
        lv_url = url.
        TRANSLATE lv_url TO LOWER CASE.
        CONCATENATE lv_url '?access_token=' token INTO lv_url.
        CONCATENATE 'Bearer' token  INTO lv_value ##NO_TEXT.

        cl_http_client=>create_by_url(
        EXPORTING
        url                = lv_url  "get url from topic
        ssl_id             = 'ANONYM'
        IMPORTING
        client             = lo_http_client
        EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
        IF sy-subrc  NE  0.
          CASE sy-subrc.
            WHEN 1.
              lv_error = 'argument_not_found' ##NO_TEXT.
            WHEN 2.
              lv_error = 'plugin_not_active' ##NO_TEXT.
            WHEN 3.
              lv_error = 'internal_error'  ##NO_TEXT.
          ENDCASE.
          add_error 'E' '015' 'ZCPS_MESSAGE' lv_error 'X' ##BOOL_OK.
          CLEAR lv_error.
          RETURN.
        ENDIF.
*
*        lo_http_client ?= lo_http_client.
*
        lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
        CREATE OBJECT lo_rest_client
          EXPORTING
            io_http_client = lo_http_client.
**
        lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
        lo_request = lo_rest_client->if_rest_client~create_request_entity( ).
        lo_request->set_header_field(
          iv_name = 'Authorization'  ##NO_TEXT
        iv_value = lv_value ).

        lo_request->set_header_field(
        iv_name = 'Content-Type' ##NO_TEXT
        iv_value =  'application/json' ).

        lo_request->set_string_data( body ).

*********************************************************************
** Post Request
********************************************************************
        TRY.
            lo_rest_client->if_rest_resource~post( lo_request ).
          CATCH cx_rest_client_exception INTO lr_root.

*        log error response and return
            lo_log->add_errortext( i_errortext = lr_root->get_text( ) ).
            RETURN.
        ENDTRY.
********************************************************************
** Collect response
********************************************************************

        lo_response    =  lo_rest_client->if_rest_client~get_response_entity( ).
*        http_status    =  lo_rest_client->if_rest_client~get_status( ).
*        reason         =  lo_response->get_header_field( '~status_reason' ).
*        content_length =  lo_response->get_header_field( 'content-length' ).
*        location       =  lo_response->get_header_field( 'location' ).
        content_type   =  lo_response->get_header_field( 'content-type' )  ##NO_TEXT.
        response       =  lo_response->get_string_data( ).

        IF display_response EQ 'X'.
          CASE content_type.
            WHEN 'text/html; charset=UTF-8'.
              CALL METHOD cl_demo_output=>display_html( response ).
            WHEN  OTHERS.
              CALL METHOD cl_demo_output=>display_text( response ).
          ENDCASE.
        ENDIF.

        CLEAR : lv_url.
      CATCH   cx_bal_exception INTO lo_cx_bal_exception.
        WRITE : lo_cx_bal_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_topic~get_auth.
    DATA : lo_cx_sy_create_object_error TYPE  REF TO   cx_sy_create_object_error,
           lo_cx_bal_exception          TYPE REF TO cx_bal_exception.

    TRY.
        CREATE OBJECT lo_auth TYPE zcl_cps_auth.
      CATCH  cx_sy_create_object_error INTO lo_cx_sy_create_object_error.
*        log error response and return
        TRY.
            IF lo_log IS BOUND.
              lo_log->add_errortext( i_errortext = lo_cx_sy_create_object_error->get_text( )  ).
            ELSE.
              lo_log = get_logger( ).
              lo_log->add_errortext( i_errortext = lo_cx_sy_create_object_error->get_text( )  ).
            ENDIF.
            RETURN.
          CATCH cx_bal_exception  INTO lo_cx_bal_exception .
            WRITE :  lo_cx_bal_exception->get_text( ).
        ENDTRY.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_topic~get_config.
    TRY.
        CREATE OBJECT lo_config TYPE zcl_cps_topic_config.
      CATCH  cx_sy_create_object_error ##HO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_topic~get_logger.
    DATA :  lo_cx_sy_create_object_error  TYPE REF TO cx_sy_create_object_error.
    DATA :  lo_cx_bal_exception  TYPE REF TO cx_bal_exception.
    TRY.
        CREATE OBJECT lo_log TYPE zcl_cps_logger.
      CATCH cx_sy_create_object_error  INTO   lo_cx_sy_create_object_error .
        WRITE :  lo_cx_sy_create_object_error->get_text( ).
      CATCH cx_bal_exception INTO lo_cx_bal_exception.
        WRITE : lo_cx_bal_exception->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_topic~publish.

********************************************************************
** Local Data Declarations
********************************************************************
    DATA :
      response            TYPE  string,
      data                TYPE REF TO data,
      display_response    TYPE flag,
      token               TYPE string,
      lo_cx_bal_exception TYPE REF TO cx_bal_exception.


    FIELD-SYMBOLS : <fs_table_ref> TYPE any.
    FIELD-SYMBOLS : <fs_message_id> TYPE any.
    FIELD-SYMBOLS : <fs_table> TYPE ANY TABLE.
    FIELD-SYMBOLS : <fs> TYPE any  .


********************************************************************
**Get helper objects
********************************************************************
    TRY.
        lo_log ?= me->get_logger( ).
        lo_config ?= me->get_config( ).
        lo_auth ?= me->get_auth( ).

********************************************************************
**Macro Definition
********************************************************************
        DEFINE add_error.
          ls_log-msgty = &1.
          ls_log-msgno = &2.
          ls_log-msgid = &3.
          ls_log-msgv1 = &4.

*       add to log
          IF &1 EQ 'E' AND &5 EQ abap_true   ##BOOL_OK.
*       *Save log
          lo_log->add_msg( ls_log ).
          lo_log->save( ).
          ELSE.
          lo_log->add_msg( ls_log ).
          ENDIF.
          CLEAR ls_log.
        END-OF-DEFINITION.
********************************************************************
**Check if all parameters values are supplied
********************************************************************
        IF profile IS  INITIAL.
          add_error 'E' '001' 'ZCPS_MESSAGE' '' 'X' ##BOOL_OK."Profile Parameter Missing
          RETURN.
        ELSE.
          add_error 'S' '002' 'ZCPS_MESSAGE' profile '' ##BOOL_OK ."Profile used to sign  JWT is &1
        ENDIF.


        IF topic IS INITIAL.
          add_error 'E' '003' 'ZCPS_MESSAGE' '' 'X' ##BOOL_OK ."Topic Parameter Missing
          RETURN.
        ELSE.
          add_error 'S' '004' 'ZCPS_MESSAGE' topic ''  ##BOOL_OK."Topic used to publish message is &1
        ENDIF.


********************************************************************
**Set _profile
********************************************************************

        lo_auth->set_profile(  profile ).
        lo_auth->get_config(  ).

*******************************************************************
**Token
********************************************************************
        token = lo_auth->get_token( ).

********************************************************************
** POST CALL
********************************************************************
        GET PARAMETER ID 'ZDISPLAY_RESPONSE' FIELD display_response.

        response  = me->post_call(  body     = lo_message->get_message( )
                                    url      = lo_config->get_endpoint( topic )
                                    token    = token
                                    display_response = display_response ).

********************************************************************
** Deserialize response
********************************************************************
        IF  response IS NOT  INITIAL.
          /ui2/cl_json=>deserialize(
            EXPORTING
              json             = response
              pretty_name      = /ui2/cl_json=>pretty_mode-none
            CHANGING
              data             =  data ).
        ENDIF.
********************************************************************
** Extract Message ID response
********************************************************************
        ASSIGN data->* TO <fs>.
        IF <fs> IS ASSIGNED.
          ASSIGN COMPONENT 'MESSAGEIDS' OF STRUCTURE <fs> TO <fs_table_ref>.
          IF <fs_table_ref> IS ASSIGNED.
            ASSIGN <fs_table_ref>->* TO  <fs_table>.
            LOOP AT <fs_table> ASSIGNING <fs>.
              ASSIGN <fs>->* TO <fs_message_id>.
              CLEAR message_id.
              message_id = <fs_message_id>.
            ENDLOOP.
          ENDIF.
        ENDIF.
*log message Id.
        IF message_id IS INITIAL.
          add_error 'E' '009' 'ZCPS_MESSAGE' '' '' ##BOOL_OK."Error no  Message id recieved from server
        ELSE.
          add_error 'S' '010' 'ZCPS_MESSAGE' message_id '' ##BOOL_OK."Message id recieved from server  &1
        ENDIF.

        lo_log->log_message_details( document = lo_message->get_document_details( ) message_id = message_id ).
*Save log
        lo_log->save( ).
      CATCH cx_bal_exception INTO  lo_cx_bal_exception.
        WRITE :   lo_cx_bal_exception->get_text( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
