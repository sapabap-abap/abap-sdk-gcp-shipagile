interface ZIF_AUTH
  public .


  data JWT_PROFILE type ZJWT_PROFILE .
  data PROFILE_NAME type STRING .

  methods GET_CONFIG .
  methods GET_TOKEN
    importing
      !DISPLAY_TOKEN type FLAG optional
    returning
      value(TOKEN) type STRING .
  methods REFRESH_TOKEN
    exporting
      !TOKEN type STRING .
  methods SET_PROFILE
    importing
      !PROFILE type STRING .
  methods GET_LOGGER
    returning
      value(LO_LOG) type ref to ZIF_LOGGER .
  methods GET_PROFILE
    returning
      value(JWT_PROFILE) type ZJWT_PROFILE .
endinterface.
