interface ZIF_SUBSCRIPTION
  public .


  data LS_LOG type BAL_S_MSG .

  methods GET_LOGGER
    returning
      value(LO_LOG) type ref to ZIF_LOGGER .
  methods GET_CONFIG
    returning
      value(LO_CONFIG) type ref to ZIF_SUBSCRIPTION_CONFIG .
  methods GET_AUTH
    returning
      value(LO_AUTH) type ref to ZIF_AUTH .
  methods PULL
    importing
      !SUBSCRIPTION type STRING
      !PROFILE type STRING
    returning
      value(LT_MESSAGE) type ZSUBS_PULL_MSG_TABLE .
endinterface.
