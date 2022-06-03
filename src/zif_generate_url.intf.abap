INTERFACE zif_generate_url
  PUBLIC .


  CLASS-DATA endpoint_details TYPE zcps_endpointgen .
  CLASS-DATA jwt_profile TYPE zjwt_profile .
  CONSTANTS : c_pull_url TYPE string VALUE 'HTTPS://PUBSUB.GOOGLEAPIS.COM/V1/PROJECTS/&1/SUBSCRIPTIONS/&2:PULL'.
  CONSTANTS :  c_publish_url TYPE string VALUE 'HTTPS://PUBSUB.GOOGLEAPIS.COM/V1/PROJECTS/&1/TOPICS/&2:PUBLISH'.

  CLASS-METHODS generate_pull_url .
  CLASS-METHODS generate_push_url .
  CLASS-METHODS generate_acknowledge_url .
  CLASS-METHODS generate_urls
    IMPORTING
      !endpoint_details TYPE zcps_endpointgen .
  CLASS-METHODS delete_urls
    IMPORTING
      !endpoint_details TYPE zcps_endpointgen .
  CLASS-METHODS set_details
    IMPORTING
      !in_endpoint_details TYPE zcps_endpointgen .
ENDINTERFACE.
