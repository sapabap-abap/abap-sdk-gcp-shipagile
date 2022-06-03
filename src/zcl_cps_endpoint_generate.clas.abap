class ZCL_CPS_ENDPOINT_GENERATE definition
  public
  final
  create public .

public section.

  interfaces ZIF_GENERATE_URL .

  aliases DELETE_URLS
    for ZIF_GENERATE_URL~DELETE_URLS .
  aliases GENERATE_URLS
    for ZIF_GENERATE_URL~GENERATE_URLS .
protected section.
private section.

  aliases C_PUBLISH_URL
    for ZIF_GENERATE_URL~C_PUBLISH_URL .
  aliases C_PULL_URL
    for ZIF_GENERATE_URL~C_PULL_URL .
  aliases ENDPOINT_DETAILS
    for ZIF_GENERATE_URL~ENDPOINT_DETAILS .
  aliases JWT_PROFILE
    for ZIF_GENERATE_URL~JWT_PROFILE .
  aliases GENERATE_ACKNOWLEDGE_URL
    for ZIF_GENERATE_URL~GENERATE_ACKNOWLEDGE_URL .
  aliases GENERATE_PULL_URL
    for ZIF_GENERATE_URL~GENERATE_PULL_URL .
  aliases GENERATE_PUSH_URL
    for ZIF_GENERATE_URL~GENERATE_PUSH_URL .
  aliases SET_DETAILS
    for ZIF_GENERATE_URL~SET_DETAILS .

  class-methods LOCK_TABLE
    importing
      !TABLE type RSTABLE-TABNAME .
  class-methods UNLOCK_TABLE
    importing
      !TABLE type RSTABLE-TABNAME .
ENDCLASS.



CLASS ZCL_CPS_ENDPOINT_GENERATE IMPLEMENTATION.


  METHOD lock_table.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname        = table
*       VARKEY         =
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.


  METHOD unlock_table.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
*       MODE_RSTABLE       = 'E'
        tabname = table.
  ENDMETHOD.


  METHOD zif_generate_url~delete_urls.

    lock_table('ZCPS_TOPIC').
    DELETE  FROM zcps_topic WHERE  topic_name = endpoint_details-name.
    unlock_table('ZCPS_TOPIC').

    lock_table('ZCPS_SUBS').
    DELETE FROM zcps_subs  WHERE  sub_name = endpoint_details-name.
    unlock_table('ZCPS_SUBS').

    COMMIT WORK.
  ENDMETHOD.


  method ZIF_GENERATE_URL~GENERATE_ACKNOWLEDGE_URL.
  endmethod.


  METHOD zif_generate_url~generate_pull_url.


    DATA : ls_zcps_subs TYPE zcps_subs.
    DATA : url TYPE zurl.
    url  = c_pull_url.
    REPLACE '&1' IN  url WITH  jwt_profile-project.
    REPLACE '&2' IN  url WITH  endpoint_details-name.

    ls_zcps_subs-sub_name = endpoint_details-name.
    ls_zcps_subs-url = url.

    lock_table('ZCPS_SUBS').
    INSERT zcps_subs FROM ls_zcps_subs.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    unlock_table('ZCPS_SUBS').

  ENDMETHOD.


  METHOD zif_generate_url~generate_push_url.

    DATA : ls_zcps_topic TYPE zcps_topic.
    DATA : url TYPE zurl.
    url  = c_publish_url.
    REPLACE '&1' IN  url WITH  jwt_profile-project.
    REPLACE '&2' IN  url WITH  endpoint_details-name.

    ls_zcps_topic-topic_name = endpoint_details-name.
    ls_zcps_topic-url = url.

    lock_table('ZCPS_TOPIC').
    INSERT zcps_topic FROM ls_zcps_topic.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    unlock_table('ZCPS_TOPIC').



  ENDMETHOD.


  METHOD zif_generate_url~generate_urls.

    set_details( endpoint_details ).

    CASE  endpoint_details-type.
      WHEN 'S'.
        generate_pull_url( ).
      WHEN 'T'.
        generate_push_url( ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_generate_url~set_details.
    endpoint_details  = in_endpoint_details.
    SELECT SINGLE * FROM zjwt_profile INTO jwt_profile WHERE profile_name =  in_endpoint_details-profile.
  ENDMETHOD.
ENDCLASS.
