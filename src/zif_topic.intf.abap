interface ZIF_TOPIC
  public .


  data LS_LOG type BAL_S_MSG .

  methods GET_AUTH
    returning
      value(LO_AUTH) type ref to ZIF_AUTH .
  methods GET_LOGGER
    returning
      value(LO_LOG) type ref to ZIF_LOGGER .
  methods PUBLISH
    importing
      !LO_MESSAGE type ref to ZIF_MESSAGE
      !TOPIC type STRING
      !PROFILE type STRING
    returning
      value(MESSAGE_ID) type STRING .
  methods GET_CONFIG
    returning
      value(LO_CONFIG) type ref to ZIF_TOPIC_CONFIG .
endinterface.
