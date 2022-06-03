interface ZIF_CPS_FACADE
  public .


  data LO_LOG type ref to ZIF_LOGGER .
  data LO_AUTH type ref to ZIF_AUTH .
  data LO_TOPIC type ref to ZIF_TOPIC .
  data LO_SUBSCRIPTION type ref to ZIF_SUBSCRIPTION .
  data LT_PULLED_MESSAGES type ZSUBS_PULL_MSG_TABLE .
  data LT_MESSAGE_ACK_SUCCESS type ZSUBS_PULL_MSG_TABLE .

  methods PUBLISH
    importing
      value(LO_MESSAGE) type ref to ZIF_MESSAGE
    returning
      value(MESSAGE_ID) type STRING .
  methods SUBSCRIBE
    importing
      value(LO_MESSAGE) type ref to ZIF_MESSAGE .
  methods CREATE_MESSAGE
    returning
      value(LO_MESSAGE) type ref to ZIF_MESSAGE .
  methods ACKNOWLEDGE
    importing
      !LO_MESSAGE type ref to ZIF_MESSAGE .
endinterface.
