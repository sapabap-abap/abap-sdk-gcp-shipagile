interface ZIF_MESSAGE
  public .


  data MESSAGE_JSON type STRING .
  data PUBLISH_JSON type STRING .
  data URL type STRING .
  data LS_LOG type BAL_S_MSG .
  data SAP_DOCUMENT type ZDOCUMENT_HEADER .
  data PROFILE type STRING .
  data TOPIC type STRING .
  data SUBSCRIPTION type STRING .

  methods SET_QUEUE_OF_PULLED_MESSAGES
    importing
      !IN_LT_MESSAGES type ZSUBS_PULL_MSG_TABLE .
  methods CONVERT_TO_JSON
    importing
      !MESSAGE_STRUCTURE type ANY
    returning
      value(JSON) type STRING .
  methods ENCODE_BASE64
    importing
      !JSON type STRING
    returning
      value(BASE_64_MSG) type STRING .
  methods DECODE_BASE64
    importing
      !BASE_64_MSG type STRING
    returning
      value(MESSAGE_JSON) type STRING .
  methods CONVERT_TO_STRUCTURE
    importing
      !MESSAGE_JSON type STRING
    exporting
      value(MESSAGE_STRUCTURE) type ref to DATA .
  methods GET_MESSAGE
    returning
      value(OUT_PUBLISH_JSON) type STRING .
  methods SET_MESSAGE
    importing
      !IN_PUBLISH_JSON type STRING .
  methods ACKNOWLEDGE
    importing
      !SUBSCRIPTION type STRING
      !ACK_ID type STRING optional
      !PROFILE type STRING .
  methods GET_AUTH
    returning
      value(LO_AUTH) type ref to ZIF_AUTH .
  methods GET_LOGGER
    returning
      value(LO_LOG) type ref to ZIF_LOGGER .
  methods WRAP_MESSAGE_INTO_CONTAINER
    importing
      !IN_JSON type STRING .
  methods GET_DOCUMENT_DETAILS
    returning
      value(R_SAP_DOCUMENT) type ZDOCUMENT_HEADER .
  methods SET_DOCUMENT_DETAILS
    importing
      !IN_SAP_DOCUMENT type ZDOCUMENT_HEADER .
  methods GET_ACKNOWLEDGED_MESSAGE_LIST
    returning
      value(R_LT_ACK_MESSAGE) type ZSUBS_PULL_MSG_TABLE .
  methods SET_PROFILE
    importing
      !IM_PROFILE type STRING .
  methods SET_TOPIC
    importing
      !IM_TOPIC type STRING .
  methods SET_SUBSCRIPTION
    importing
      !IM_SUBSCRIPTION type STRING .
  methods GET_PULLED_MESSAGES
    returning
      value(EX_LT_MESSAGES) type ZSUBS_PULL_MSG_TABLE .
endinterface.
