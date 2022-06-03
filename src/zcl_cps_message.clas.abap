class ZCL_CPS_MESSAGE definition
  public
  final
  create public .

public section.

  interfaces ZIF_MESSAGE .

  aliases MESSAGE_JSON
    for ZIF_MESSAGE~MESSAGE_JSON .
  aliases PROFILE
    for ZIF_MESSAGE~PROFILE .
  aliases TOPIC
    for ZIF_MESSAGE~TOPIC .
  aliases ACKNOWLEDGE
    for ZIF_MESSAGE~ACKNOWLEDGE .
  aliases CONVERT_TO_JSON
    for ZIF_MESSAGE~CONVERT_TO_JSON .
  aliases CONVERT_TO_STRUCTURE
    for ZIF_MESSAGE~CONVERT_TO_STRUCTURE .
  aliases DECODE_BASE64
    for ZIF_MESSAGE~DECODE_BASE64 .
  aliases ENCODE_BASE64
    for ZIF_MESSAGE~ENCODE_BASE64 .
  aliases GET_ACKNOWLEDGED_MESSAGE_LIST
    for ZIF_MESSAGE~GET_ACKNOWLEDGED_MESSAGE_LIST .
  aliases GET_DOCUMENT_DETAILS
    for ZIF_MESSAGE~GET_DOCUMENT_DETAILS .
  aliases GET_MESSAGE
    for ZIF_MESSAGE~GET_MESSAGE .
  aliases GET_PULLED_MESSAGES
    for ZIF_MESSAGE~GET_PULLED_MESSAGES .
  aliases SET_DOCUMENT_DETAILS
    for ZIF_MESSAGE~SET_DOCUMENT_DETAILS .
  aliases SET_MESSAGE
    for ZIF_MESSAGE~SET_MESSAGE .
  aliases SET_PROFILE
    for ZIF_MESSAGE~SET_PROFILE .
  aliases SET_QUEUE_OF_PULLED_MESSAGES
    for ZIF_MESSAGE~SET_QUEUE_OF_PULLED_MESSAGES .
  aliases SET_SUBSCRIPTION
    for ZIF_MESSAGE~SET_SUBSCRIPTION .
  aliases SET_TOPIC
    for ZIF_MESSAGE~SET_TOPIC .
  aliases WRAP_MESSAGE_INTO_CONTAINER
    for ZIF_MESSAGE~WRAP_MESSAGE_INTO_CONTAINER .

  methods POST_CALL
    importing
      !BODY type STRING
      !URL type STRING
      !TOPIC type STRING
      !TOKEN type STRING
    returning
      value(RESPONSE) type STRING .
protected section.
private section.

  aliases LS_LOG
    for ZIF_MESSAGE~LS_LOG .
  aliases PUBLISH_JSON
    for ZIF_MESSAGE~PUBLISH_JSON .
  aliases SAP_DOCUMENT
    for ZIF_MESSAGE~SAP_DOCUMENT .
  aliases SUBSCRIPTION
    for ZIF_MESSAGE~SUBSCRIPTION .
  aliases URL
    for ZIF_MESSAGE~URL .
  aliases GET_AUTH
    for ZIF_MESSAGE~GET_AUTH .
  aliases GET_LOGGER
    for ZIF_MESSAGE~GET_LOGGER .

  data LT_MESSAGES type ZSUBS_PULL_MSG_TABLE .
  data LO_AUTH type ref to ZIF_AUTH .
  data LO_LOG type ref to ZIF_LOGGER .
  data LT_ACK_MESSAGE type ZSUBS_PULL_MSG_TABLE .
ENDCLASS.



CLASS ZCL_CPS_MESSAGE IMPLEMENTATION.


  METHOD zif_message~get_auth.

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


METHOD post_call.

********************************************************************
** Local Data Declarations
********************************************************************

  DATA: lo_http_client      TYPE REF TO if_http_client,
        lo_rest_client      TYPE REF TO cl_rest_http_client,
        lo_response         TYPE REF TO if_rest_entity,
        lr_root             TYPE REF TO cx_root,
        lo_request          TYPE REF TO if_rest_entity,
*        reason           TYPE  string,
*        http_status      TYPE  string,
*        content_length   TYPE  string,
*        location         TYPE  string,
        lv_url              TYPE  string,
        lv_value            TYPE  string,
        content_type        TYPE  string,
        lv_error            TYPE  string,
        display_response    TYPE flag,
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
  END-OF-DEFINITION ##NEEDED.
********************************************************************
**Get Token
********************************************************************

  TRY.
      IF token IS  INITIAL .
        add_error 'E' '005' 'ZCPS_MESSAGE' '' 'X' ##BOOL_OK."Profile Parameter Missing
        RETURN.
      ELSE.
        add_error 'S' '006' 'ZCPS_MESSAGE' token ' ' ##BOOL_OK."Token generated: &1
      ENDIF.

********************************************************************
**Get Url
********************************************************************
      IF url IS INITIAL.
        add_error 'E' '007' 'ZCPS_MESSAGE' '' 'X' ##BOOL_OK." Endpoint URL to topic  is missing in config
        RETURN.
      ELSE.
        add_error 'S' '008' 'ZCPS_MESSAGE' url ' '   ##BOOL_OK." Endpoint URL called is &1
      ENDIF.
********************************************************************
** Create Request
********************************************************************
      CONCATENATE lv_url '?access_token=' token INTO lv_url.
      CONCATENATE 'Bearer' token  INTO lv_value  ##NO_TEXT.

      cl_http_client=>create_by_url(
      EXPORTING
      url                = url  "get url from topic
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
*      lo_http_client ?= lo_http_client.
*
      lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
      CREATE OBJECT lo_rest_client
        EXPORTING
          io_http_client = lo_http_client.
**
      lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
      lo_request = lo_rest_client->if_rest_client~create_request_entity( ).
      lo_request->set_header_field(
        iv_name = 'Authorization' ##NO_TEXT
      iv_value = lv_value ).

      lo_request->set_header_field(
      iv_name = 'Content-Type'    ##NO_TEXT
      iv_value =  'application/json' ).

      lo_request->set_string_data( body ).

*********************************************************************
** Post Request
********************************************************************
      TRY.
          lo_rest_client->if_rest_resource~post( lo_request ).
        CATCH cx_rest_client_exception INTO lr_root.
*        log error response and return
          lo_log->add_errortext( i_errortext = lr_root->get_text( )  ).
          RETURN.
      ENDTRY.
********************************************************************
** Collect response
********************************************************************

      lo_response    =  lo_rest_client->if_rest_client~get_response_entity( ).
*      http_status    =  lo_rest_client->if_rest_client~get_status( ).
*      reason         =  lo_response->get_header_field( '~status_reason' ).
*      content_length =  lo_response->get_header_field( 'content-length' ).
*      location       =  lo_response->get_header_field( 'location' ).
*      content_type   =  lo_response->get_header_field( 'content-type' ).
      response       =  lo_response->get_string_data( ).

      GET PARAMETER ID 'ZDISPLAY_RESPONSE' FIELD  display_response.

      IF display_response EQ 'X'.
        CASE content_type.
          WHEN 'text/html; charset=UTF-8'.
            CALL METHOD cl_demo_output=>display_html( response ).
          WHEN  OTHERS.
            CALL METHOD cl_demo_output=>display_text( response ).
        ENDCASE.
      ENDIF.
    CATCH cx_bal_exception INTO  lo_cx_bal_exception.
      WRITE :   lo_cx_bal_exception->get_text( ).
  ENDTRY.
ENDMETHOD.


  METHOD zif_message~set_queue_of_pulled_messages.
    me->lt_messages = in_lt_messages.
  ENDMETHOD.


  METHOD zif_message~acknowledge.


    DATA: url         TYPE string,
          response    TYPE string,
          lv_body     TYPE string,
          lv_ack_id   TYPE string,
          lv_token    TYPE string,
          jwt_profile TYPE zjwt_profile,
          lo_cx_bal_exception TYPE REF TO cx_bal_exception.

    FIELD-SYMBOLS :  <wa_message>  LIKE  LINE OF lt_messages.

********************************************************************
**Macro Definition
********************************************************************
    DEFINE add_error.
      ls_log-msgty = &1.
      ls_log-msgno = &2.
      ls_log-msgid = &3.
      ls_log-msgv1 = &4.

*     add to log
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
        lo_log ?= me->get_logger( ).

********************************************************************
**Set _profile
********************************************************************
        lo_auth ?= me->get_auth( ).
        lo_auth->set_profile(  profile ).
        lo_auth->get_config(  ).
        jwt_profile = lo_auth->get_profile( ).
********************************************************************
**Url
********************************************************************
        url  = TEXT-001."'https://pubsub.googleapis.com/v1/projects/&1/subscriptions/&2:acknowledge'
        REPLACE '&1' IN url WITH jwt_profile-project.
        REPLACE '&2' IN url WITH subscription.
        TRANSLATE url TO LOWER  CASE.
********************************************************************
** Token
********************************************************************
        lv_token = lo_auth->get_token( ).

********************************************************************
** Generate Body
********************************************************************
        lv_body =  '{ "ackIds": [ &3 ] }' ##NO_TEXT.

        IF ack_id IS SUPPLIED.
          REPLACE '&3' IN lv_body WITH  ack_id .
        ELSE.
          LOOP AT  lt_messages ASSIGNING  <wa_message>.
            IF <wa_message> IS ASSIGNED.
              IF sy-tabix EQ 1.
                lv_ack_id = <wa_message>-ack_id.
              ELSE.
                CONCATENATE <wa_message>-ack_id ',' INTO  lv_ack_id.
              ENDIF.
            ENDIF.
            REPLACE '&3' IN lv_body WITH  lv_ack_id .

********************************************************************
** POST CALL
********************************************************************

            response  = me->post_call(  body     = lv_body
                                        url      = url
                                        topic    = subscription
                                        token    = lv_token
                                        ).

            IF response IS INITIAL.
*      message is acknowledged if respnse is initial
              APPEND  <wa_message>  TO lt_ack_message.
            ENDIF.

          ENDLOOP.
        ENDIF.
      CATCH cx_bal_exception INTO  lo_cx_bal_exception.
        WRITE :   lo_cx_bal_exception->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_message~convert_to_json.

*Create JSON
     json = /ui2/cl_json=>serialize(
             data             =  message_structure
                   compress         = 'X'
*               name             = name
             pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
*               type_descr       = type_descr
*               assoc_arrays     = C_BOOL-FALSE
*               ts_as_iso8601    = C_BOOL-FALSE
*               expand_includes  = C_BOOL-TRUE
*               assoc_arrays_opt = C_BOOL-FALSE
*               numc_as_string   = C_BOOL-FALSE
*               name_mappings    = name_mappings
                ).


  ENDMETHOD.


  METHOD zif_message~convert_to_structure.


    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json        = message_json
*       jsonx       =
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*       assoc_arrays     = C_BOOL-FALSE
*       assoc_arrays_opt = C_BOOL-FALSE
      CHANGING
        data        = message_structure.

  ENDMETHOD.


  METHOD zif_message~decode_base64.
    message_json = cl_http_utility=>decode_base64( encoded = base_64_msg ).
  ENDMETHOD.


  method ZIF_MESSAGE~ENCODE_BASE64.


    base_64_msg = cl_http_utility=>encode_base64( unencoded = json ).

    REPLACE ALL OCCURRENCES OF '=' IN  base_64_msg WITH ''.
    REPLACE ALL OCCURRENCES OF '+' IN  base_64_msg WITH '-'.
    REPLACE ALL OCCURRENCES OF '/' IN  base_64_msg WITH '_'.

  endmethod.


  METHOD zif_message~get_acknowledged_message_list.
    r_lt_ack_message = lt_ack_message.
  ENDMETHOD.


  METHOD zif_message~get_document_details.
    r_sap_document = sap_document.
  ENDMETHOD.


  METHOD zif_message~get_logger.

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


  method ZIF_MESSAGE~GET_MESSAGE.
    out_publish_json = me->publish_json.
  endmethod.


  method ZIF_MESSAGE~GET_PULLED_MESSAGES.
    ex_lt_messages = lt_messages.
  endmethod.


  METHOD zif_message~set_document_details.
    sap_document   = in_sap_document.
  ENDMETHOD.


  method ZIF_MESSAGE~SET_MESSAGE.
     me->publish_json = in_publish_json.
  endmethod.


  method ZIF_MESSAGE~SET_PROFILE.
    me->profile  = im_profile.
  endmethod.


  METHOD zif_message~set_subscription.
    me->subscription  = im_subscription.
  ENDMETHOD.


  method ZIF_MESSAGE~SET_TOPIC.
    me->topic =  im_topic.
  endmethod.


  METHOD zif_message~wrap_message_into_container.
    DATA :
           message_wrapper TYPE zmessage_wrapper,
           message_object  TYPE zmessage_object.

**Data should be  base64 url encode
**{
**  "messages": [
**    {
**      "data": "ewogICJTYWxlc09yZGVyIjoiMTIzNCIKfQ=="
**    }
**  ]
**}
    message_object-data = me->encode_base64( in_json ).
    message_object-attributes-client_id = sy-mandt.
    message_object-attributes-created_by = ''.
    APPEND message_object  TO  message_wrapper-messages.

*Set final delivery message
    me->set_message( me->convert_to_json(  message_wrapper ) ).

    CLEAR :message_wrapper,message_object .
  ENDMETHOD.
ENDCLASS.
