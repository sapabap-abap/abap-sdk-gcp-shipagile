class ZCL_CPS_SUBSCRIPTION definition
  public
  final
  create public .

public section.

  interfaces ZIF_SUBSCRIPTION .

  aliases PULL
    for ZIF_SUBSCRIPTION~PULL .
protected section.
private section.

  aliases LS_LOG
    for ZIF_SUBSCRIPTION~LS_LOG .
  aliases GET_AUTH
    for ZIF_SUBSCRIPTION~GET_AUTH .
  aliases GET_CONFIG
    for ZIF_SUBSCRIPTION~GET_CONFIG .
  aliases GET_LOGGER
    for ZIF_SUBSCRIPTION~GET_LOGGER .

  data LO_LOG type ref to ZIF_LOGGER .
  data LO_AUTH type ref to ZIF_AUTH .
  data LO_CONFIG type ref to ZIF_SUBSCRIPTION_CONFIG .

  methods POST_CALL
    importing
      !BODY type STRING
      !URL type STRING
      !SUBSCRIPTION type STRING
      !TOKEN type STRING
      !DISPLAY_RESPONSE type FLAG
    returning
      value(RESPONSE) type STRING .
ENDCLASS.



CLASS ZCL_CPS_SUBSCRIPTION IMPLEMENTATION.


METHOD post_call.

********************************************************************
** Local Data Declarations
********************************************************************

  DATA: lo_http_client TYPE REF TO if_http_client,
        lo_rest_client TYPE REF TO cl_rest_http_client,
        lo_response    TYPE REF TO if_rest_entity,
        lr_root        TYPE REF TO cx_root,
        lo_request     TYPE REF TO if_rest_entity,
        l_response     TYPE  string,
        reason         TYPE  string,
        http_status    TYPE  string,
        content_length TYPE  string,
        location       TYPE  string,
        lv_url         TYPE  string,
        lv_value       TYPE  string,
        content_type   TYPE  string,
        lv_error       TYPE  string.
********************************************************************
**Macro Definition
********************************************************************
  DEFINE add_error.
    ls_log-msgty = &1.
    ls_log-msgno = &2.
    ls_log-msgid = &3.
    ls_log-msgv1 = &4.
*       add to log
    IF &1 EQ 'E' AND &5  EQ abap_true.
*       *Save log
    lo_log->add_msg( ls_log ).
    lo_log->save( ).
    ELSE.
    lo_log->add_msg( ls_log ).
    ENDIF.
    CLEAR ls_log.
  END-OF-DEFINITION.
********************************************************************
**Get Token
********************************************************************
  IF token IS  INITIAL .
    add_error 'E' '005' 'ZCPS_MESSAGE' ''  'X'."Token is missing
    RETURN.
  ELSE.
    add_error 'S' '006' 'ZCPS_MESSAGE' token  ' '."Token generated: &1
  ENDIF.

********************************************************************
**Get Url
********************************************************************
  IF url IS INITIAL.
    add_error 'E' '007' 'ZCPS_MESSAGE' ''  'X'." Endpoint URL to topic  is missing in config
    RETURN.
  ELSE.
    add_error 'S' '008' 'ZCPS_MESSAGE' url ' '." Endpoint URL called is &1
  ENDIF.
********************************************************************
** Create Request
********************************************************************
  lv_url = url.
  TRANSLATE lv_url TO LOWER CASE.
  CONCATENATE lv_url '?access_token=' token INTO lv_url.
  CONCATENATE 'Bearer' token  INTO lv_value.

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
*
  lo_http_client ?= lo_http_client.
*
  lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
  CREATE OBJECT lo_rest_client
    EXPORTING
      io_http_client = lo_http_client.
**
  lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
  lo_request = lo_rest_client->if_rest_client~create_request_entity( ).
  lo_request->set_header_field(
    iv_name = 'Authorization'
  iv_value = lv_value ).

  lo_request->set_header_field(
  iv_name = 'Content-Type'
  iv_value =  'application/json' ).

  lo_request->set_string_data( body ).

*********************************************************************
** Post Request
********************************************************************
  TRY.
      lo_rest_client->if_rest_resource~post( lo_request ).
    CATCH cx_rest_client_exception INTO lr_root.

      l_response = lr_root->get_text( ).
*        log error response and return
      lo_log->add_errortext( i_errortext = l_response ).
      RETURN.
  ENDTRY.
********************************************************************
** Collect response
********************************************************************

  lo_response    =  lo_rest_client->if_rest_client~get_response_entity( ).
  http_status    =  lo_rest_client->if_rest_client~get_status( ).
  reason         =  lo_response->get_header_field( '~status_reason' ).
  content_length =  lo_response->get_header_field( 'content-length' ).
  location       =  lo_response->get_header_field( 'location' ).
  content_type   =  lo_response->get_header_field( 'content-type' ).
  response       =  lo_response->get_string_data( ).
  IF display_response EQ 'X'.
    CASE content_type.
      WHEN 'text/html; charset=UTF-8'.
        CALL METHOD cl_demo_output=>display_html( response ).
      WHEN  OTHERS.
        CALL METHOD cl_demo_output=>display_text( response ).
    ENDCASE.
  ENDIF.
ENDMETHOD.


  METHOD zif_subscription~get_auth.
    TRY.
        CREATE OBJECT lo_auth TYPE zcl_cps_auth.
      CATCH  cx_sy_create_object_error .
    ENDTRY.
  ENDMETHOD.


  METHOD zif_subscription~get_config.
    TRY.
        CREATE OBJECT lo_config TYPE zcl_cps_subscription_config.
      CATCH  cx_sy_create_object_error .
    ENDTRY.
  ENDMETHOD.


  method ZIF_SUBSCRIPTION~GET_LOGGER.
      TRY.
        CREATE OBJECT lo_log TYPE zcl_cps_logger.
      CATCH cx_sy_create_object_error .
    ENDTRY.
  endmethod.


  METHOD zif_subscription~pull.

********************************************************************
** Local Data Declarations
********************************************************************
    DATA :
      response            TYPE  string,
      lv_profile          TYPE  zjwt_profile-profile_name,
      display_response    TYPE flag,
      data                TYPE REF TO data,
      lv_body             TYPE string,
      lo_cx_bal_exception TYPE REF TO cx_bal_exception,
      ls_message          TYPE zsubs_pull_msg_object.



    FIELD-SYMBOLS : <fs_table_ref>    TYPE any,
                    <fs_message_id>   TYPE any,
                    <fs_table>        TYPE ANY TABLE,
                    <data>            TYPE any,
                    <message>         TYPE any,
                    <ackid>           TYPE any,
                    <structure>       TYPE any,
                    <deep_structure>       TYPE any,
                    <attributes>      TYPE any,
                    <value>           TYPE any,
                    <deliveryattempt> TYPE any.

********************************************************************
**Macro Definition
********************************************************************
    DEFINE add_error.
      ls_log-msgty = &1.
      ls_log-msgno = &2.
      ls_log-msgid = &3.
      ls_log-msgv1 = &4.
*       add to log
      IF &1 EQ 'E' AND &5  EQ abap_true.
*       *Save log
      lo_log->add_msg( ls_log ).
      lo_log->save( ).
      ELSE.
      lo_log->add_msg( ls_log ).
      ENDIF.
      CLEAR ls_log.
    END-OF-DEFINITION.

    DEFINE assign_component.
      ASSIGN COMPONENT &1 OF STRUCTURE &2 TO &3.
    END-OF-DEFINITION.
********************************************************************
**Get helper objects
********************************************************************
    TRY.
        lo_log ?= me->get_logger( ).
        lo_config ?= me->get_config( ).
        lo_auth ?= me->get_auth( ).
********************************************************************
**Check if all parameters values are supplied
********************************************************************
        IF profile IS  INITIAL.
          add_error  'E' '001'  'ZCPS_MESSAGE' ' ' 'X'.
          RETURN.
        ELSE.
          add_error  'S' '002'  'ZCPS_MESSAGE' profile ''. "Profile used to sign  JWT is &1
        ENDIF.
        IF subscription IS INITIAL.
          add_error  'E' '003'  'ZCPS_MESSAGE' ' ' 'X'."subscriptionParameter Missing
          RETURN.
        ELSE.
          add_error  'S' '004'  'ZCPS_MESSAGE' subscription ''. "subscriptionused to publish message is &1
        ENDIF.
********************************************************************
**Set _profile
********************************************************************

        lo_auth->set_profile(  profile ).
        lo_auth->get_config(  ).

********************************************************************
** POST CALL
********************************************************************
        GET PARAMETER ID 'ZDISPLAY_RESPONSE' FIELD display_response.


        CONCATENATE '{' '"returnImmediately": false, "maxMessages": 2000' '}' INTO lv_body SEPARATED BY space.

        response  = me->post_call(  body     = lv_body
                                    url      = lo_config->get_endpoint( subscription )
                                    subscription   = subscription
                                    token    = lo_auth->get_token( )
                                    display_response = display_response ).

        SEARCH response FOR  'error' .

        IF sy-subrc NE 0 .

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
          ASSIGN data->* TO <data>.
          IF <data> IS ASSIGNED.
            assign_component 'RECEIVEDMESSAGES'  <data>  <fs_table_ref>.
            IF <fs_table_ref> IS ASSIGNED.
              ASSIGN <fs_table_ref>->* TO  <fs_table>.
              LOOP AT <fs_table> ASSIGNING <data>.
*          process each message
                ASSIGN <data>->* TO <structure>.
                IF <structure>  IS ASSIGNED.
                  assign_component 'ACKID'   <structure>   <ackid>.
                  IF <ackid> IS ASSIGNED.
                    ASSIGN <ackid>->* TO <value>.
                    ls_message-ack_id  = <value>.
                  ENDIF.
                  assign_component 'DELIVERYATTEMPT'   <structure>  <deliveryattempt>.
                  IF <deliveryattempt> IS ASSIGNED.
                    ASSIGN <deliveryattempt>->* TO <value>.
                    ls_message-delivery_attempt = <value>.
                  ENDIF.
                  assign_component 'MESSAGE'   <structure>  <message>.
                  IF <message> IS ASSIGNED.
                    ASSIGN <message>->* TO <deep_structure>.
                    CHECK <deep_structure> IS ASSIGNED.
                    assign_component 'DATA'  <deep_structure>  <structure>.
                    ASSIGN  <structure>->* TO <value>.
                    ls_message-message-data  = <value>.
                    assign_component 'ATTRIBUTES'  <deep_structure>  <structure>.
                    ASSIGN <structure>->* TO <attributes>.
                    assign_component 'CLIENTID' <attributes> <structure>.
                    ASSIGN <structure>->* TO <value>.
                    ls_message-message-attributes-client_id  = <value>.
                    assign_component 'MESSAGEID'  <deep_structure>  <structure>.
                    ASSIGN <structure>->* TO <value>.
                    ls_message-message-message_id  = <value>.
                    assign_component 'PUBLISHTIME'  <deep_structure>  <structure>.
                    ASSIGN  <structure>->* TO <value>.
                    ls_message-message-publish_time =  <value>.
                  ENDIF.

                  APPEND ls_message TO lt_message.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
*Save log
        lo_log->save( ).
      CATCH cx_bal_exception INTO lo_cx_bal_exception .

    ENDTRY.


  ENDMETHOD.
ENDCLASS.
