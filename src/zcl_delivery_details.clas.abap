class ZCL_DELIVERY_DETAILS definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !DELIVERY_NO type VBELN_VL .
  methods PUSH_DELIVERY_TO_TOPIC
    importing
      !PROFILE type STRING
      !TOPIC type STRING .
  methods PUSH_PGI_DETAILS_TO_TOPIC
    importing
      !DELIVERY type VBELN_VL
      !PROFILE type STRING
      !TOPIC type STRING .
  methods GET_TEST_DELIVERY_PAYLOAD
    returning
      value(DELIVERY_PAYLOAD) type ZDELIVERY_PAYLOAD .
  methods GET_DELIVERY_NO
    returning
      value(DELIVERY) type VBELN_VL .
protected section.
private section.

  types:
    tt_zcps_delivery TYPE STANDARD TABLE OF zcps_delivery .

  data LO_MESSAGE type ref to ZIF_MESSAGE .
  data DELIVERY_PAYLOAD type ZDELIVERY_PAYLOAD .
  data DELIVERY type VBELN_VL .
  data LO_PUBLISH type ref to ZIF_TOPIC .
  data SAP_DOCUMENT type ZDOCUMENT_HEADER .
  data LT_DELIVERY type TT_ZCPS_DELIVERY .

  methods GET_DELIVERY_PAYLOAD
    returning
      value(DELIVERY_PAYLOAD) type ZDELIVERY_PAYLOAD .
  methods GET_PGI_DETAILS
    importing
      !DELIVERY type VBELN_VL
    returning
      value(PGI_PAYLOAD) type ZDELIVERY_PGI_PAYLOAD .
  methods GET_HEADER
    importing
      !DELIVERY type VBELN_VL
    returning
      value(R_HEADER) type LIKP .
  methods GET_ITEMS
    importing
      !DELIVERY type VBELN_VL
    returning
      value(R_ITEMS) type TAB_LIPS .
  methods GET_PLANT_DETAILS
    importing
      !PLANT type WERKS_D
    returning
      value(R_T001W) type T001W .
  methods GET_ADDRESS_DETAILS
    importing
      !ADDRNUMBER type AD_ADDRNUM
    returning
      value(LS_ADRC) type ADRC .
ENDCLASS.



CLASS ZCL_DELIVERY_DETAILS IMPLEMENTATION.


  METHOD constructor.
    DATA : lv_vbeln TYPE vbeln_vl.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = delivery_no
      IMPORTING
        output = lv_vbeln.

    me->delivery = lv_vbeln.
*Pass document no
    sap_document-document = lv_vbeln.
    sap_document-type = 'D'.
    CREATE OBJECT lo_message TYPE zcl_cps_message.
    CREATE OBJECT lo_publish TYPE zcl_cps_topic.
  ENDMETHOD.


  METHOD get_address_details.
    SELECT SINGLE * FROM adrc INTO ls_adrc WHERE addrnumber  = addrnumber.
  ENDMETHOD.


  METHOD get_delivery_no.
    delivery  = me->delivery.
  ENDMETHOD.


  METHOD get_delivery_payload.

    IF delivery EQ '1234567890'.
      me->delivery_payload = delivery_payload  = me->get_test_delivery_payload( ).
    ELSE.

      DATA : ls_likp       TYPE likp,
             lt_lips       TYPE STANDARD TABLE OF lips,
             ls_t001w      TYPE t001w,
             ls_adrc_plant TYPE adrc,
             ls_adrc_sloc  TYPE adrc.

      FIELD-SYMBOLS : <fs_item> TYPE lips.

      DATA : lt_line_items TYPE zitems_delivery_shipagile.
      DATA : wa_line_items TYPE zline_items_delivery_shipagile.

      ls_likp = get_header( delivery  = delivery ).
      lt_lips = get_items(  delivery  = delivery ).

      ls_t001w  = get_plant_details( plant = ls_likp-werks ).
      ls_adrc_plant   = get_address_details( addrnumber =  ls_t001w-adrnr ).


      delivery_payload-delivery_id = delivery.
      delivery_payload-status = 'NEW'.
      delivery_payload-priority_id = '10'.
      delivery_payload-priorityname = 'Standard Shipping'.

      delivery_payload-dangerous_goods = 'false'.

      delivery_payload-created_date = '2021-02-18T16:56:00'.
      delivery_payload-created_by  = 'Manishusa88@gmail.com'.
      delivery_payload-delivery_date  = '2021-02-18T16:56:00'.
      delivery_payload-plan_pgi_date = '2021-02-18T16:56:00'.


*Plant address
      delivery_payload-plant_id  = ls_likp-werks.
      delivery_payload-plant_name = 'Home dist. center'.
      delivery_payload-plant_address-name  = 'New Home Address'.
      delivery_payload-plant_address-company  = 'Home Address'.
      delivery_payload-plant_address-street1  = '19601 Kennemer Dr'.
      delivery_payload-plant_address-city = 'Pflugerville'.
      delivery_payload-plant_address-state  = 'TX'.
      delivery_payload-plant_address-zip = '78660'.
      delivery_payload-plant_address-country  = ls_t001w-land1.

*Storage location details
      delivery_payload-storage_loc_id  = 'FG209'.
      delivery_payload-storage_loc_name = 'Home Storage Loc.'.
      delivery_payload-storage_loc_address-name = 'New Home Address'.
      delivery_payload-storage_loc_address-company = 'Home Address'.
      delivery_payload-storage_loc_address-street1 = '19601 Kennemer Dr'.
      delivery_payload-storage_loc_address-city = 'Pflugerville'.
      delivery_payload-storage_loc_address-state = 'TX'.
      delivery_payload-storage_loc_address-zip  = '78660'.
      delivery_payload-storage_loc_address-country = 'US'.



      delivery_payload-shipping_point_id = 'R29'.
      delivery_payload-shipping_point_name = 'Home carriers only'.
      delivery_payload-shipping_point_address-name = 'New Home Address'.
      delivery_payload-shipping_point_address-company = '"Home Address'.
      delivery_payload-shipping_point_address-street1 = '19601 Kennemer Dr'.
      delivery_payload-shipping_point_address-city = 'Pflugerville'.
      delivery_payload-shipping_point_address-state = 'TX'.
      delivery_payload-shipping_point_address-zip = '78660'.
      delivery_payload-shipping_point_address-country = 'US'.



      delivery_payload-sold_to_id = '310047'.
      delivery_payload-sold_to_name = 'NY Power Corp'.
      delivery_payload-sold_to_address-name = 'NY Power Corp'.
      delivery_payload-sold_to_address-company = 'NY Power Corp'.
      delivery_payload-sold_to_address-street1 = '75 Franklin St'.
      delivery_payload-sold_to_address-city = 'New York'.
      delivery_payload-sold_to_address-state = 'NY'.
      delivery_payload-sold_to_address-zip  = '10029'.
      delivery_payload-sold_to_address-country = 'US'.

*fill items
      LOOP AT lt_lips ASSIGNING <fs_item>.
        CHECK <fs_item> IS ASSIGNED.
        wa_line_items-item_no  = '100'.
        wa_line_items-product_id = 'FT1034'.
        wa_line_items-product_name  = 'Floor Tile'.
        wa_line_items-length  = 32.
        wa_line_items-width   = 32.
        wa_line_items-height  = 5.
        wa_line_items-distance_unit  = 'in'.
        wa_line_items-weight   = 20.
        wa_line_items-mass_unit  = 'lb'.
        wa_line_items-qty = 1.
        wa_line_items-qty_unit  = 'EA'.
        APPEND wa_line_items TO lt_line_items.
      ENDLOOP.

      delivery_payload-items = lt_line_items.

    ENDIF.

  ENDMETHOD.


  METHOD get_header.
    SELECT SINGLE  * FROM likp INTO r_header WHERE vbeln  = delivery.
  ENDMETHOD.


  METHOD get_items.
    SELECT * FROM lips INTO TABLE r_items WHERE vbeln = delivery .
  ENDMETHOD.


  method GET_PGI_DETAILS.
  endmethod.


  METHOD get_plant_details.
    SELECT SINGLE * FROM t001w INTO r_t001w WHERE werks = plant.
  ENDMETHOD.


  METHOD get_test_delivery_payload.

    DATA : lt_line_items TYPE zitems_delivery_shipagile.
    DATA : wa_line_items TYPE zline_items_delivery_shipagile.

    delivery_payload-delivery_id = '1234567890'.
    delivery_payload-created_date = '2021-02-18T16:56:00'.
    delivery_payload-created_by  = 'Manishusa88@gmail.com'.
    delivery_payload-status = 'NEW'.
    delivery_payload-priority_id = '10'.
    delivery_payload-priorityname = 'Standard Shipping'.
    delivery_payload-delivery_date  = '2021-02-18T16:56:00'.
    delivery_payload-plan_pgi_date = '2021-02-18T16:56:00'.
    delivery_payload-dangerous_goods = 'false'.
    delivery_payload-plant_id  = 'R209'.
    delivery_payload-plant_name = 'Home dist. center'.
*    Plant address
    delivery_payload-plant_address-name  = 'New Home Address'.
    delivery_payload-plant_address-company  = 'Home Address'.
    delivery_payload-plant_address-street1  = '19601 Kennemer Dr'.
    delivery_payload-plant_address-city = 'Pflugerville'.
    delivery_payload-plant_address-state  = 'TX'.
    delivery_payload-plant_address-zip = '78660'.
    delivery_payload-plant_address-country  = 'US'.


    delivery_payload-storage_loc_id  = 'FG209'.
    delivery_payload-storage_loc_name = 'Home Storage Loc.'.
    delivery_payload-storage_loc_address-name = 'New Home Address'.
    delivery_payload-storage_loc_address-company = 'Home Address'.
    delivery_payload-storage_loc_address-street1 = '19601 Kennemer Dr'.
    delivery_payload-storage_loc_address-city = 'Pflugerville'.
    delivery_payload-storage_loc_address-state = 'TX'.
    delivery_payload-storage_loc_address-zip  = '78660'.
    delivery_payload-storage_loc_address-country = 'US'.



    delivery_payload-shipping_point_id = 'R29'.
    delivery_payload-shipping_point_name = 'Home carriers only'.
    delivery_payload-shipping_point_address-name = 'New Home Address'.
    delivery_payload-shipping_point_address-company = '"Home Address'.
    delivery_payload-shipping_point_address-street1 = '19601 Kennemer Dr'.
    delivery_payload-shipping_point_address-city = 'Pflugerville'.
    delivery_payload-shipping_point_address-state = 'TX'.
    delivery_payload-shipping_point_address-zip = '78660'.
    delivery_payload-shipping_point_address-country = 'US'.



    delivery_payload-sold_to_id = '310047'.
    delivery_payload-sold_to_name = 'NY Power Corp'.
    delivery_payload-sold_to_address-name = 'NY Power Corp'.
    delivery_payload-sold_to_address-company = 'NY Power Corp'.
    delivery_payload-sold_to_address-street1 = '75 Franklin St'.
    delivery_payload-sold_to_address-city = 'New York'.
    delivery_payload-sold_to_address-state = 'NY'.
    delivery_payload-sold_to_address-zip  = '10029'.
    delivery_payload-sold_to_address-country = 'US'.

*fill items
    wa_line_items-item_no  = '100'.
    wa_line_items-product_id = 'FT1034'.
    wa_line_items-product_name  = 'Floor Tile'.
    wa_line_items-length  = 32.
    wa_line_items-width   = 32.
    wa_line_items-height  = 5.
    wa_line_items-distance_unit  = 'in'.
    wa_line_items-weight   = 20.
    wa_line_items-mass_unit  = 'lb'.
    wa_line_items-qty = 1.
    wa_line_items-qty_unit  = 'EA'.
    APPEND wa_line_items TO lt_line_items.

    delivery_payload-items = lt_line_items.

  ENDMETHOD.


  METHOD push_delivery_to_topic.

    DATA : lv_json         TYPE string,
           message_wrapper TYPE zmessage_wrapper,
           message_object  TYPE zmessage_object.

*Convert delivery payload to JSON  and then base 64 using lo_message
    lv_json =  lo_message->convert_to_json( me->get_delivery_payload( ) ).
    REPLACE ALL OCCURRENCES OF '"dangerousGoods":"false",' IN  lv_json  WITH '"dangerousGoods": false'.

*Create  final delivery message  in pub  sub format

**Data should be  base64 url encode
**{
**  "messages": [
**    {
**      "data": "ewogICJTYWxlc09yZGVyIjoiMTIzNCIKfQ=="
**    }
**  ]
**}
    message_object-data = lo_message->encode_base64( lv_json ).
    message_object-attributes-client_id = sy-mandt.
    message_object-attributes-created_by = ''.
    APPEND message_object  TO  message_wrapper-messages.

*Set Document Details
   lo_message->set_document_details( in_sap_document  = sap_document ).
*Set final delivery message
   lo_message->set_message( lo_message->convert_to_json(  message_wrapper ) ).
*Publish message
   lo_publish->publish( EXPORTING
                                    lo_message = lo_message
                                    topic = topic
                                    profile = profile  ).

  ENDMETHOD.


  METHOD push_pgi_details_to_topic.

*    DATA : zdelivery_pgi_payload        TYPE zdelivery_pgi_payload,
*           zdelivery_pgi_payload_json   TYPE string,
*           zdelivery_pgi_payload_base64 TYPE string,
*           lo_publish                   TYPE REF TO zcl_cps_topic,
*           sap_document                 TYPE zdocument_header.


*    zdelivery_pgi_payload = me->get_pgi_details( delivery ).
*    zdelivery_pgi_payload_json  = me->pgi_details_to_json( zdelivery_pgi_payload ).
*    zdelivery_pgi_payload_base64 = me->pgi_details_to_base64( zdelivery_pgi_payload_json ).

*    lo_publish->publish( EXPORTING
*                                    lo_message = lo_message
*                                    topic = topic
*                                    profile = profile
*                                    sap_document  = sap_document ).

  ENDMETHOD.
ENDCLASS.
