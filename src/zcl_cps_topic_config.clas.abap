CLASS zcl_cps_topic_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_topic_config .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CPS_TOPIC_CONFIG IMPLEMENTATION.


  METHOD zif_topic_config~get_endpoint.
    DATA : lv_topic      TYPE string,
           lv_topic_name TYPE string.
    lv_topic =  topic.
    SELECT SINGLE  name FROM zcps_endpointgen INTO lv_topic_name WHERE  zalias = lv_topic AND type = 'T'.
    IF sy-subrc = 0 .
      SELECT SINGLE url FROM zcps_topic INTO topic_url WHERE topic_name = lv_topic_name.
    ENDIF.
    CLEAR: lv_topic, lv_topic_name.
  ENDMETHOD.
ENDCLASS.
