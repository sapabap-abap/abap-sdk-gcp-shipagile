class ZCL_CPS_FACADE definition
  public
  final
  create public .

public section.

  interfaces ZIF_CPS_FACADE .

  aliases LT_MESSAGE_ACK_SUCCESS
    for ZIF_CPS_FACADE~LT_MESSAGE_ACK_SUCCESS .
  aliases LT_PULLED_MESSAGES
    for ZIF_CPS_FACADE~LT_PULLED_MESSAGES .
  aliases ACKNOWLEDGE
    for ZIF_CPS_FACADE~ACKNOWLEDGE .
  aliases CREATE_MESSAGE
    for ZIF_CPS_FACADE~CREATE_MESSAGE .
  aliases PUBLISH
    for ZIF_CPS_FACADE~PUBLISH .
  aliases SUBSCRIBE
    for ZIF_CPS_FACADE~SUBSCRIBE .

  methods CONSTRUCTOR .
protected section.
private section.

  aliases LO_AUTH
    for ZIF_CPS_FACADE~LO_AUTH .
  aliases LO_LOG
    for ZIF_CPS_FACADE~LO_LOG .
  aliases LO_SUBSCRIPTION
    for ZIF_CPS_FACADE~LO_SUBSCRIPTION .
  aliases LO_TOPIC
    for ZIF_CPS_FACADE~LO_TOPIC .
  aliases LT_MESSAGES
    for ZIF_CPS_FACADE~LT_pulled_MESSAGES .
ENDCLASS.



CLASS ZCL_CPS_FACADE IMPLEMENTATION.


  METHOD constructor.
    TRY.
        CREATE OBJECT :lo_topic        TYPE zcl_cps_topic,
                       lo_subscription TYPE zcl_cps_subscription,
                       lo_auth         TYPE zcl_cps_auth,
                       lo_log          TYPE zcl_cps_logger.
      CATCH cx_sy_create_object_error .

    ENDTRY.
  ENDMETHOD.


  METHOD zif_cps_facade~acknowledge.
    lo_message->acknowledge(  EXPORTING subscription = lo_message->subscription profile  = lo_message->profile ).
  ENDMETHOD.


  METHOD zif_cps_facade~create_message.
    DATA :   lo_message_object TYPE REF TO zcl_cps_message.

    CREATE OBJECT lo_message_object TYPE zcl_cps_message.

    lo_message =  lo_message_object .

  ENDMETHOD.


  METHOD zif_cps_facade~publish.

    message_id  =   lo_topic->publish( lo_message = lo_message
                                       topic      = lo_message->topic
                                       profile    = lo_message->profile ) .
  ENDMETHOD.


  METHOD zif_cps_facade~subscribe.

    CALL METHOD lo_subscription->pull
      EXPORTING
        subscription = lo_message->subscription
        profile      = lo_message->profile
      RECEIVING
        lt_message   = lt_messages.

    lo_message->set_queue_of_pulled_messages( lt_messages ).

  ENDMETHOD.
ENDCLASS.
