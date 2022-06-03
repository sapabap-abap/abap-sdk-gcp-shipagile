interface ZIF_TOPIC_CONFIG
  public .


  methods GET_ENDPOINT
    importing
      !TOPIC type STRING
    returning
      value(TOPIC_URL) type STRING .
endinterface.
