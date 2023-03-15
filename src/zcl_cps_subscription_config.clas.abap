class ZCL_CPS_SUBSCRIPTION_CONFIG definition
  public
  final
  create public .

public section.

  interfaces ZIF_SUBSCRIPTION_CONFIG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CPS_SUBSCRIPTION_CONFIG IMPLEMENTATION.


  METHOD zif_subscription_config~get_endpoint.
    DATA : lv_topic      TYPE string,
           lv_topic_name TYPE string.
    lv_topic =  topic_to_subscribe.
    SELECT SINGLE  name FROM zcps_endpointgen INTO lv_topic_name WHERE  zalias = lv_topic AND type = 'S'.
    IF sy-subrc = 0 .
      SELECT SINGLE url FROM zcps_subs INTO subscription_url  WHERE sub_name = lv_topic_name.
    ENDIF.
    CLEAR :lv_topic,lv_topic_name.
  ENDMETHOD.
ENDCLASS.
