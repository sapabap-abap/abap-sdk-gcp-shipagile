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
    DATA : lv_topic TYPE string.
    lv_topic =  topic_to_subscribe.
    TRANSLATE lv_topic  TO UPPER CASE.
    SELECT SINGLE url FROM zcps_subs INTO subscription_url  WHERE sub_name = lv_topic.
    CLEAR lv_topic.
  ENDMETHOD.
ENDCLASS.
