class ZCL_SHIPMENT_DETAILS definition
  public
  final
  create public .

public section.

  class-methods GET_SHIPMENT_PAYLOAD
    importing
      !I_TKNUM type TKNUM
    returning
      value(R_SHIPMENT_PAYLOAD) type FLAG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SHIPMENT_DETAILS IMPLEMENTATION.


  method GET_SHIPMENT_PAYLOAD.
  endmethod.
ENDCLASS.
