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
    DATA : lv_topic TYPE string.
    lv_topic =  topic.
    TRANSLATE lv_topic  TO UPPER CASE.
    SELECT SINGLE url FROM zcps_topic INTO topic_url WHERE topic_name = lv_topic.
    CLEAR lv_topic.
  ENDMETHOD.
ENDCLASS.
