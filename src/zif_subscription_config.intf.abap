interface ZIF_SUBSCRIPTION_CONFIG
  public .


  methods GET_ENDPOINT
    importing
      !TOPIC_TO_SUBSCRIBE type STRING
    returning
      value(SUBSCRIPTION_URL) type STRING .
endinterface.
