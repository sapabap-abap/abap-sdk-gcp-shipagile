class ZCL_CONFIG_HANDLER definition
  public
  final
  create public .

public section.

  interfaces ZIF_CONFIG_HANDLER .

  aliases UPLOAD_CONFIG_JSON
    for ZIF_CONFIG_HANDLER~UPLOAD_CONFIG_JSON .
protected section.
private section.

  aliases UPLOAD_CERTIFICATES
    for ZIF_CONFIG_HANDLER~UPLOAD_CERTIFICATES .
  aliases UPLOAD_JWT_PROFILE
    for ZIF_CONFIG_HANDLER~UPLOAD_JWT_PROFILE .
  aliases UPLOAD_URL
    for ZIF_CONFIG_HANDLER~UPLOAD_URL .
ENDCLASS.



CLASS ZCL_CONFIG_HANDLER IMPLEMENTATION.


  method UPLOAD_CERTIFICATES.
  endmethod.


  method UPLOAD_CONFIG_JSON.
  endmethod.


  method UPLOAD_JWT_PROFILE.
  endmethod.


  method UPLOAD_URL.
  endmethod.
ENDCLASS.
