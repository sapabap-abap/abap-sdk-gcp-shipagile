*----------------------------------------------------------------------*
***INCLUDE LZFG_CPS_MAINTI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_PROFILE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_profile INPUT.
  DATA : lt_profile_list TYPE STANDARD TABLE OF zjwt_profile,
         BEGIN OF  wa_profile,
           profile_name TYPE  zprofile,
         END OF wa_profile,
         lt_profile LIKE TABLE OF wa_profile.
  FIELD-SYMBOLS : <wa_profile> LIKE  wa_profile.

  SELECT *
  FROM zjwt_profile
  INTO  CORRESPONDING FIELDS OF TABLE  lt_profile.

  IF  sy-subrc = 0.

    LOOP AT lt_profile ASSIGNING <wa_profile>.
      CHECK <wa_profile> IS ASSIGNED.
      TRANSLATE <wa_profile>-profile_name TO LOWER CASE.
    ENDLOOP.

  ENDIF..
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PROFILE_NAME'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZCPS_ENDPOINTGEN-PROFILE '
      value_org       = 'S'
    TABLES
      value_tab       = lt_profile
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.

  ENDIF. " IF SY-SUBRC EQ 0
  CLEAR : lt_profile,lt_profile_list.

ENDMODULE.
