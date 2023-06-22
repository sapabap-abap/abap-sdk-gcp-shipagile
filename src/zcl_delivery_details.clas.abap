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
  types:
    BEGIN of ty_header,
           created_date     TYPE string,
           created_by       TYPE LIKP-ERNAM,
           status           TYPE string,
           priorityID       TYPE LIKP-LPRIO,
           priority_name    TYPE TPRIT-BEZEI,
           del_date         TYPE string,
           planned_pgi_date TYPE string,
           dangerous_goods  TYPE string,
         END OF ty_header .
  types:
    BEGIN OF ty_plant,
            plant_id     TYPE LIPS-WERKS,
            plant_name   TYPE T001W-NAME1,
            NAME         TYPE string,
            COMPANY      TYPE string,
            STREET1      TYPE string,
            CITY         TYPE string,
            STATE        TYPE string,
            ZIP          TYPE string,
            COUNTRY      TYPE string,
            ADRNR        TYPE adrc-addrnumber,
           END OF ty_plant .
  types:
    BEGIN OF ty_storage,
             str_loc_id     TYPE LIPS-LGORT,
             str_loc_name   TYPE T001L-LGOBE,
             name           TYPE ADRC-name1,
             company        TYPE ADRC-name2,
             street1        TYPE ADRC-STREET,
             city           TYPE ADRC-CITY1,
             state          TYPE ADRC-REGION,
             zip            TYPE ADRC-POST_CODE1,
             country        TYPE ADRC-COUNTRY,
             adrnr          TYPE adrc-addrnumber,
           END OF ty_storage .
  types:
    BEGIN OF ty_shipping,
             shipping_id      TYPE LIKP-VSTEL,
             shipping_name    TYPE TVSTT-VTEXT,
             name             TYPE ADRC-name1,
             company          TYPE ADRC-name2,
             street1          TYPE ADRC-STREET,
             city             TYPE ADRC-CITY1,
             state            TYPE ADRC-REGION,
             zip              TYPE ADRC-POST_CODE1,
             country          TYPE ADRC-COUNTRY,
           END OF ty_shipping .
  types:
    BEGIN of ty_sold,
             sold_to_id       TYPE LIKP-KUNAG,
             sold_to_name     TYPE KNA1-NAME1,
             name             TYPE ADRC-name1,
             company          TYPE ADRC-name2,
             street1          TYPE KNA1-STRAS,
             city             TYPE KNA1-ORT01,
             state            TYPE KNA1-REGIO,
             zip              TYPE KNA1-PSTLZ,
             country          TYPE KNA1-LAND1,
             adrnr            TYPE KNA1-ADRNR,
         END OF ty_sold .
  types:
    BEGIN OF ty_items,
     item_no       TYPE LIPS-posnr,
     product_id    TYPE LIPS-matnr,
     product_name  TYPE MAKT-MAKTX,
     length        TYPE MARA-laeng,
     width         TYPE MARA-breit,
     height        TYPE MARA-hoehe,
     weight        TYPE LIPS-ntgew,
     mass_unit     TYPE LIPS-gewei,
     qty           TYPE LIPS-lfimg,
     qty_unit      TYPE LIPS-meins,
    END OF ty_items .
  types:
*      BEGIN OF ty_items,
*     item_no       TYPE string,
*     product_id    TYPE string,
*     product_name  TYPE string,
*     length        TYPE DF16_DEC,
*     width         TYPE df16_dec,
*     height        TYPE df16_dec,
*     weight        TYPE df16_dec,
*     mass_unit     TYPE string,
*     qty           TYPE df16_dec,
*     qty_unit      TYPE string,
*    END OF ty_items .
    tt_items TYPE TABLE OF ty_items .

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
    exporting
      !EX_HEADER type TY_HEADER .
  methods GET_ITEMS
    exporting
      !ET_ITEMS type TT_ITEMS .
  methods GET_PLANT_DETAILS
    exporting
      !EX_PLANT type TY_PLANT .
  methods GET_STORAGE_ADDRESS_DETAILS
    exporting
      !EX_STORAGE type TY_STORAGE .
  methods GET_SHIPPING_DETAILS
    exporting
      !EX_SHIPPING type TY_SHIPPING .
  methods GET_SOLD_TO
    exporting
      !EX_SOLD type TY_SOLD .
  methods PGI_DETAILS_TO_JSON
    importing
      !DELIVERY_PGI_PAYLOAD type ZDELIVERY_PGI_PAYLOAD
    returning
      value(DELIVERY_PGI_PAYLOAD_JSON) type STRING .
  methods PGI_DETAILS_TO_BASE64
    importing
      !DELIVERY_PGI_PAYLOAD_JSON type STRING
    returning
      value(DELIVERY_PGI_PAYLOAD_BASE64) type STRING .
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


  METHOD get_delivery_no.
    delivery  = me->delivery.
  ENDMETHOD.


  METHOD get_delivery_payload.


*      me->delivery_payload = delivery_payload  = me->get_test_delivery_payload( ).
*
*
*      DATA : ls_likp       TYPE likp,
*             lt_lips       TYPE STANDARD TABLE OF lips,
*             ls_t001w      TYPE t001w,
*             ls_adrc_plant TYPE adrc,
*             ls_adrc_sloc  TYPE adrc.
*
*      FIELD-SYMBOLS : <fs_item> TYPE lips.
*
*      DATA : lt_line_items TYPE zitems_delivery_shipagile.
*      DATA : wa_line_items TYPE zline_items_delivery_shipagile.
*
*      ls_likp = get_header( delivery  = delivery ).
*      lt_lips = get_items(  delivery  = delivery ).
*
*      ls_t001w  = get_plant_details( plant = ls_likp-werks ).
*      ls_adrc_plant   = get_address_details( addrnumber =  ls_t001w-adrnr ).
*
*
*      delivery_payload-delivery_id = delivery.
*      delivery_payload-status = 'NEW'.
*      delivery_payload-priority_id = '10'.
*      delivery_payload-priorityname = 'Standard Shipping'.
*
*      delivery_payload-dangerous_goods = 'false'.
*
*      delivery_payload-created_date = '2021-02-18T16:56:00'.
*      delivery_payload-created_by  = 'Manishusa88@gmail.com'.
*      delivery_payload-delivery_date  = '2021-02-18T16:56:00'.
*      delivery_payload-plan_pgi_date = '2021-02-18T16:56:00'.
*
*
**Plant address
*      delivery_payload-plant_id  = ls_likp-werks.
*      delivery_payload-plant_name = 'Home dist. center'.
*      delivery_payload-plant_address-name  = 'New Home Address'.
*      delivery_payload-plant_address-company  = 'Home Address'.
*      delivery_payload-plant_address-street1  = '19601 Kennemer Dr'.
*      delivery_payload-plant_address-city = 'Pflugerville'.
*      delivery_payload-plant_address-state  = 'TX'.
*      delivery_payload-plant_address-zip = '78660'.
*      delivery_payload-plant_address-country  = ls_t001w-land1.
*
**Storage location details
*      delivery_payload-storage_loc_id  = 'FG209'.
*      delivery_payload-storage_loc_name = 'Home Storage Loc.'.
*      delivery_payload-storage_loc_address-name = 'New Home Address'.
*      delivery_payload-storage_loc_address-company = 'Home Address'.
*      delivery_payload-storage_loc_address-street1 = '19601 Kennemer Dr'.
*      delivery_payload-storage_loc_address-city = 'Pflugerville'.
*      delivery_payload-storage_loc_address-state = 'TX'.
*      delivery_payload-storage_loc_address-zip  = '78660'.
*      delivery_payload-storage_loc_address-country = 'US'.
*
*
*
*      delivery_payload-shipping_point_id = 'R29'.
*      delivery_payload-shipping_point_name = 'Home carriers only'.
*      delivery_payload-shipping_point_address-name = 'New Home Address'.
*      delivery_payload-shipping_point_address-company = '"Home Address'.
*      delivery_payload-shipping_point_address-street1 = '19601 Kennemer Dr'.
*      delivery_payload-shipping_point_address-city = 'Pflugerville'.
*      delivery_payload-shipping_point_address-state = 'TX'.
*      delivery_payload-shipping_point_address-zip = '78660'.
*      delivery_payload-shipping_point_address-country = 'US'.
*
*
*
*      delivery_payload-sold_to_id = '310047'.
*      delivery_payload-sold_to_name = 'NY Power Corp'.
*      delivery_payload-sold_to_address-name = 'NY Power Corp'.
*      delivery_payload-sold_to_address-company = 'NY Power Corp'.
*      delivery_payload-sold_to_address-street1 = '75 Franklin St'.
*      delivery_payload-sold_to_address-city = 'New York'.
*      delivery_payload-sold_to_address-state = 'NY'.
*      delivery_payload-sold_to_address-zip  = '10029'.
*      delivery_payload-sold_to_address-country = 'US'.
*
**fill items
*      LOOP AT lt_lips ASSIGNING <fs_item>.
*        CHECK <fs_item> IS ASSIGNED.
*        wa_line_items-item_no  = '100'.
*        wa_line_items-product_id = 'FT1034'.
*        wa_line_items-product_name  = 'Floor Tile'.
*        wa_line_items-length  = 32.
*        wa_line_items-width   = 32.
*        wa_line_items-height  = 5.
*        wa_line_items-distance_unit  = 'in'.
*        wa_line_items-weight   = 20.
*        wa_line_items-mass_unit  = 'lb'.
*        wa_line_items-qty = 1.
*        wa_line_items-qty_unit  = 'EA'.
*        APPEND wa_line_items TO lt_line_items.
*      ENDLOOP.
*
*      delivery_payload-items = lt_line_items.
*
  CALL METHOD me->GET_HEADER
       IMPORTING
         EX_HEADER = DATA(ls_header).

  delivery_payload-DELIVERY_ID = me->DELIVERY.
  delivery_payload-CREATED_DATE = ls_header-CREATED_DATE.
  delivery_payload-CREATED_BY = ls_header-CREATED_BY.
  delivery_payload-DANGEROUS_GOODS = ls_header-DANGEROUS_GOODS.
  delivery_payload-DELIVERY_DATE = ls_header-DEL_DATE.
  delivery_payload-STATUS = ls_header-STATUS.
  delivery_payload-PRIORITY_ID = ls_header-PRIORITYID.
  delivery_payload-PRIORITYNAME = ls_header-PRIORITY_NAME.
  delivery_payload-PLAN_PGI_DATE = ls_header-PLANNED_PGI_DATE.

  CALL METHOD me->GET_PLANT_DETAILS
        IMPORTING
          EX_PLANT = DATA(ls_plant).
  delivery_payload-PLANT_ID = ls_plant-PLANT_ID.
  delivery_payload-PLANT_NAME = ls_plant-PLANT_NAME.
  delivery_payload-PLANT_ADDRESS-NAME = ls_plant-NAME.
  delivery_payload-PLANT_ADDRESS-COMPANY = ls_plant-COMPANY.
  delivery_payload-PLANT_ADDRESS-STREET1 = ls_plant-STREET1.
  delivery_payload-PLANT_ADDRESS-CITY = ls_plant-CITY.
  delivery_payload-PLANT_ADDRESS-STATE = ls_plant-STATE.
  delivery_payload-PLANT_ADDRESS-COUNTRY = ls_plant-COUNTRY.
  delivery_payload-PLANT_ADDRESS-ZIP = ls_plant-ZIP.

  CALL METHOD me->GET_STORAGE_ADDRESS_DETAILS
        IMPORTING
          ex_storage = DATA(ls_storage).

  delivery_payload-STORAGE_LOC_ID = ls_storage-STR_LOC_ID.
  delivery_payload-STORAGE_LOC_NAME = ls_storage-STR_LOC_NAME.
  delivery_payload-STORAGE_LOC_ADDRESS-NAME = ls_storage-NAME.
  delivery_payload-STORAGE_LOC_ADDRESS-COMPANY = ls_storage-COMPANY.
  delivery_payload-STORAGE_LOC_ADDRESS-STREET1 = ls_storage-STREET1.
  delivery_payload-STORAGE_LOC_ADDRESS-CITY = ls_storage-CITY.
  delivery_payload-STORAGE_LOC_ADDRESS-STATE = ls_storage-STATE.
  delivery_payload-STORAGE_LOC_ADDRESS-ZIP = ls_storage-ZIP.
  delivery_payload-STORAGE_LOC_ADDRESS-COUNTRY = ls_storage-COUNTRY.

  CALL METHOD me->GET_SHIPPING_DETAILS
        IMPORTING
          EX_SHIPPING = DATA(ls_shipping).

  delivery_payload-SHIPPING_POINT_ID = ls_shipping-SHIPPING_ID.
  delivery_payload-SHIPPING_POINT_NAME = ls_shipping-SHIPPING_NAME.
  delivery_payload-SHIPPING_POINT_ADDRESS-NAME = ls_shipping-NAME.
  delivery_payload-SHIPPING_POINT_ADDRESS-COMPANY = ls_shipping-COMPANY.
  delivery_payload-SHIPPING_POINT_ADDRESS-STREET1 = ls_shipping-STREET1.
  delivery_payload-SHIPPING_POINT_ADDRESS-CITY = ls_shipping-CITY.
  delivery_payload-SHIPPING_POINT_ADDRESS-STATE = ls_shipping-STATE.
  delivery_payload-SHIPPING_POINT_ADDRESS-ZIP = ls_shipping-ZIP.
  delivery_payload-SHIPPING_POINT_ADDRESS-COUNTRY = ls_shipping-COUNTRY.

  CALL METHOD me->GET_SOLD_TO
        IMPORTING
          EX_SOLD = DATA(ls_sold).

  delivery_payload-SOLD_TO_ID = ls_sold-SOLD_TO_ID.
  delivery_payload-SOLD_TO_NAME = ls_sold-SOLD_TO_NAME.
  delivery_payload-SOLD_TO_ADDRESS-NAME = ls_sold-NAME.
  delivery_payload-SOLD_TO_ADDRESS-COMPANY = ls_sold-COMPANY.
  delivery_payload-SOLD_TO_ADDRESS-STREET1 = ls_sold-STREET1.
  delivery_payload-SOLD_TO_ADDRESS-CITY = ls_sold-CITY.
  delivery_payload-SOLD_TO_ADDRESS-STATE = ls_sold-STATE.
  delivery_payload-SOLD_TO_ADDRESS-ZIP = ls_sold-ZIP.
  delivery_payload-SOLD_TO_ADDRESS-COUNTRY = ls_sold-COUNTRY.

  CALL METHOD me->GET_ITEMS
       IMPORTING
         ET_ITEMS = DATA(lt_items).

  DATA: ls_del_items TYPE ZLINE_ITEMS_DELIVERY_SHIPAGILE.

  LOOP AT lt_items INTO DATA(ls_items).
    ls_del_items-PRODUCT_ID = ls_items-PRODUCT_ID.
    ls_del_items-LENGTH = ls_items-LENGTH.
    ls_del_items-ITEM_NO = ls_items-ITEM_NO.
    ls_del_items-MASS_UNIT = ls_items-MASS_UNIT.
    ls_del_items-PRODUCT_NAME = ls_items-PRODUCT_NAME.
    ls_del_items-QTY = ls_items-QTY.
    ls_del_items-QTY_UNIT = ls_items-QTY_UNIT.
    ls_del_items-WEIGHT = ls_items-WEIGHT.
    ls_del_items-WIDTH = ls_items-WIDTH.
    ls_del_items-HEIGHT = ls_items-HEIGHT.
    APPEND ls_del_items TO  delivery_payload-ITEMS[].
    CLEAR:ls_items,ls_del_items.
  ENDLOOP.

  ENDMETHOD.


  METHOD get_header.
*    SELECT SINGLE  * FROM likp INTO r_header WHERE vbeln  = delivery.
     SELECT SINGLE CONCAT( erdat,erzet ) as created_date,
            ernam as created_by,
            'NEW' as status,
            lprio as priorityID,
            CONCAT( wadat,lfuhr ) as del_date,
            CONCAT( lfdat,SPE_WAUHR_IST ) as planned_pgi_date,
            'FALSE' as dangerous_goods
            FROM LIKP
            WHERE vbeln EQ @me->delivery INTO CORRESPONDING FIELDS OF @ex_header.

     IF sy-subrc EQ 0.
       SELECT SINGLE bezei FROM TPRIT INTO @ex_header-priority_name WHERE spras EQ 'EN' AND lprio EQ @ex_header-priorityID.

     ENDIF.

  ENDMETHOD.


  METHOD get_items.
*    SELECT * FROM lips INTO TABLE r_items WHERE vbeln = delivery .
    DATA: ls_items TYPE me->ty_items.

    SELECT posnr,
           matnr,
           ntgew,
           gewei,
           lfimg,
           meins FROM LIPS INTO TABLE @DATA(lt_lips) WHERE vbeln EQ @me->DELIVERY.

    SELECT a~laeng,
           a~matnr,
           a~breit,
           a~hoehe,
           b~maktx FROM MARA as a INNER JOIN MAKT as b ON a~matnr EQ b~matnr INTO TABLE @DATA(lt_item_detail)
           FOR ALL ENTRIES IN @lt_lips WHERE a~matnr EQ @lt_lips-matnr.

    LOOP AT lt_lips INTO DATA(ls_lips).
      ls_items-ITEM_NO = ls_lips-posnr.
      ls_items-PRODUCT_ID = ls_lips-matnr.
      ls_items-WEIGHT = ls_lips-ntgew.
      ls_items-QTY = ls_lips-lfimg.
      ls_items-QTY_UNIT = ls_lips-meins.
      ls_items-MASS_UNIT = ls_lips-gewei.
      READ TABLE lt_item_detail INTO DATA(ls_item_details) WITH KEY matnr = ls_lips-matnr.
      IF sy-subrc EQ 0.
        ls_items-length = ls_item_details-LAENG.
        ls_items-WIDTH = ls_item_details-breit.
        ls_items-height = ls_item_details-hoehe.
        ls_items-PRODUCT_NAME = ls_item_details-maktx.
        APPEND ls_items TO et_items.
        CLEAR ls_items.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  method GET_PGI_DETAILS.
  endmethod.


  METHOD get_plant_details.
*    SELECT SINGLE * FROM t001w INTO r_t001w WHERE werks = plant.
     SELECT SINGLE werks FROM LIPS INTO @ex_plant-plant_id WHERE vbeln EQ @me->DELIVERY.
     SELECT  SINGLE name1 as plant_name,
                    stras as street1,
                    ort01 as city,
                    regio as state,
                    pstlz as zip,
                    land1 as country,
                    adrnr as adrnr
                    FROM T001W  INTO CORRESPONDING FIELDS OF @ex_plant WHERE werks EQ @ex_plant-plant_id.

     SELECT SINGLE Name1 as Name,
                   name2 as company
                   FROM ADRC INTO CORRESPONDING FIELDS OF @ex_plant WHERE addrnumber EQ @ex_plant-adrnr.
  ENDMETHOD.


  method GET_SHIPPING_DETAILS.
    SELECT SINGLE vstel FROM LIKP INTO @ex_shipping-SHIPPING_ID WHERE vbeln EQ @me->DELIVERY.

    SELECT SINGLE vtext FROM TVSTT INTO @ex_shipping-SHIPPING_NAME WHERE vstel EQ @ex_shipping-SHIPPING_ID.

    SELECT SINGLE adrnr FROM TVST INTO @DATA(lv_ardnr) WHERE vstel EQ @ex_shipping-shipping_id.

    SELECT SINGLE name1      AS name,
                  name2      AS company,
                  street     AS street1,
                  city1      AS city,
                  region     AS state,
                  post_code1 AS zip,
                  country    AS country FROM ADRC INTO CORRESPONDING FIELDS OF @ex_shipping WHERE addrnumber EQ @lv_ardnr.
  endmethod.


  method GET_SOLD_TO.
    SELECT SINGLE kunag FROM LIKP INTO @ex_sold-SOLD_TO_ID WHERE vbeln EQ @me->DELIVERY.

    SELECT SINGLE name1 as sold_to_name,
                  stras as street1,
                  ort01 as city,
                  regio as state,
                  pstlz as zip,
                  land1 as country,
                  adrnr as adrnr FROM KNA1 INTO CORRESPONDING FIELDS OF @ex_sold WHERE kunnr EQ @ex_sold-sold_to_id.

    SELECT SINGLE name1 as name,
                  name2 as company FROM ADRC INTO CORRESPONDING FIELDS OF @ex_sold WHERE addrnumber EQ @ex_sold-adrnr.


  endmethod.


  METHOD GET_STORAGE_ADDRESS_DETAILS.
*    SELECT SINGLE * FROM adrc INTO ls_adrc WHERE addrnumber  = addrnumber.
     SELECT SINGLE LGORT,WERKS FROM LIPS INTO @DATA(ls_str) WHERE vbeln EQ @me->DELIVERY.
     IF sy-subrc EQ 0.
       ex_storage-STR_LOC_ID = ls_str-lgort.
     ENDIF.

     SELECT SINGLE lgobe FROM T001l INTO @DATA(lv_lgobe) WHERE lgort EQ @ls_str-lgort.
     IF sy-subrc EQ 0.
       ex_storage-STR_LOC_NAME = lv_lgobe.
       ex_storage-NAME = lv_lgobe.
     ENDIF.

     SELECT SINGLE lfdnr,adrnr FROM TWLAD INTO @DATA(ls1) WHERE lgort EQ @ls_str-lgort AND werks EQ @ls_str-WERKS.

     SELECT SINGLE name1 as name,
                   name2 as company
                   FROM ADRC INTO CORRESPONDING FIELDS OF @ex_storage WHERE addrnumber EQ @ls1-adrnr.

     SELECT SINGLE street     AS street1,
                   city1      AS city,
                   region     AS state,
                   post_code1 AS zip,
                   country    AS country
                   FROM ADRC INTO CORRESPONDING FIELDS OF @ex_storage WHERE addrnumber EQ @ls1-lfdnr.

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
*    wa_line_items-distance_unit  = 'in'.
    wa_line_items-weight   = 20.
    wa_line_items-mass_unit  = 'lb'.
    wa_line_items-qty = 1.
    wa_line_items-qty_unit  = 'EA'.
    APPEND wa_line_items TO lt_line_items.

    delivery_payload-items = lt_line_items.

  ENDMETHOD.


  method PGI_DETAILS_TO_BASE64.
  DELIVERY_PGI_PAYLOAD_BASE64 =    me->lo_message->ENCODE_BASE64( DELIVERY_PGI_PAYLOAD_JSON ).
  endmethod.


  method PGI_DETAILS_TO_JSON.

  DELIVERY_PGI_PAYLOAD_JSON = me->LO_MESSAGE->CONVERT_TO_JSON( DELIVERY_PGI_PAYLOAD ).

  endmethod.


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

    DATA : zdelivery_pgi_payload        TYPE zdelivery_pgi_payload,
           zdelivery_pgi_payload_json   TYPE string,
           zdelivery_pgi_payload_base64 TYPE string,
           lo_publish                   TYPE REF TO zcl_cps_topic,
           sap_document                 TYPE zdocument_header.


    zdelivery_pgi_payload = me->get_pgi_details( delivery ).
    zdelivery_pgi_payload_json  = me->pgi_details_to_json( zdelivery_pgi_payload ).
    zdelivery_pgi_payload_base64 = me->pgi_details_to_base64( zdelivery_pgi_payload_json ).

   sap_document = lo_publish->publish( EXPORTING
                                    lo_message = lo_message
                                    topic = topic
                                    profile = profile ).

  ENDMETHOD.
ENDCLASS.
