class ZCL_CPS_AUTH definition
  public
  final
  create public .

public section.

  interfaces ZIF_AUTH .

  aliases GET_PROFILE
    for ZIF_AUTH~GET_TOKEN .
  PROTECTED SECTION.

private section.

  aliases JWT_PROFILE
    for ZIF_AUTH~JWT_PROFILE .
  aliases PROFILE_NAME
    for ZIF_AUTH~PROFILE_NAME .
  aliases GET_LOGGER
    for ZIF_AUTH~GET_LOGGER .

  types:
    BEGIN OF ty_jwt_header,
        alg TYPE char255,
        typ TYPE char3,
      END OF ty_jwt_header .
  types:
    BEGIN OF ty_jwt_claim,
        iss   TYPE char255, "Issuer
        scope TYPE char255, "
        sub   TYPE char255, "Subject
        aud   TYPE char255, "Audience
        exp   TYPE i, "string, "Expiration Time
        iat   TYPE i, " string, "Issued At
      END OF ty_jwt_claim .

  data LO_LOG type ref to ZIF_LOGGER .
  data LS_LOG type BAL_S_MSG .

  methods GENERATE_JWT
    returning
      value(JWT) type STRING
    raising
      ZCX_JWT_GENERATOR .
  methods CHECK_FOR_TOKEN_IN_DB
    returning
      value(LS_TOKEN) type ZCPS_TOKEN .
  methods STORE_TOKEN
    importing
      !IM_LS_TOKEN type ZCPS_TOKEN .
  methods POST_CALL
    returning
      value(RESPONSE) type STRING .
ENDCLASS.



CLASS ZCL_CPS_AUTH IMPLEMENTATION.


  METHOD check_for_token_in_db.

    DATA : timestamp_current TYPE timestamp,
           ls_token_db       TYPE zcps_token,
           result            TYPE i.

    SELECT SINGLE * FROM zcps_token INTO ls_token_db   WHERE zdate = sy-datum.

    GET TIME STAMP FIELD  timestamp_current.

*  checktime difference betweenn timestamps
    TRY.
        CALL METHOD cl_abap_tstmp=>subtract
          EXPORTING
            tstmp1 = timestamp_current
            tstmp2 = ls_token_db-time_stamp
          RECEIVING
            r_secs = result.
      CATCH cx_parameter_invalid_range .
      CATCH cx_parameter_invalid_type .
    ENDTRY.

    IF result > 3500.
      CLEAR ls_token.
    ELSE.
      ls_token = ls_token_db  .
    ENDIF.

    CLEAR :timestamp_current,ls_token_db,result.


  ENDMETHOD.


  METHOD generate_jwt.

    DATA input_bins TYPE STANDARD TABLE OF ssfbin.
    DATA output_bins TYPE STANDARD TABLE OF ssfbin.
    DATA input_length TYPE ssflen.
    DATA output_length TYPE ssflen.
    DATA output_crc TYPE ssfreturn.
    DATA signers TYPE STANDARD TABLE OF ssfinfo.
    DATA: jwt_claim_json       TYPE string,
          jwt_header_json      TYPE string,
          jwt_header_base64url TYPE string,
          jwt_claim_base64url  TYPE string.
    DATA input_base64url TYPE string.
    DATA: signature           TYPE string,
          signature_base64url TYPE string.

    DATA : jwt_header	       TYPE ty_jwt_header,
           jwt_claim         TYPE ty_jwt_claim,
           ssfinfo           TYPE ssfinfo,
           current_timestamp TYPE timestamp,
           timestamp_iat     TYPE timestamp,
           timestamp_exp     TYPE timestamp.

    DATA  : date              TYPE sydate,
            time              TYPE syuzeit,
            lv_unix_timestamp TYPE string.


*   ssf profile
    ssfinfo-id        = jwt_profile-ssf_id.
    ssfinfo-profile = jwt_profile-ssf_profile.
    ssfinfo-result = 28.
*   header
    jwt_header-alg  = jwt_profile-alg.
    jwt_header-typ  = jwt_profile-typ.
*   claim
    jwt_claim-iss   =  jwt_profile-iss.
    jwt_claim-scope =  jwt_profile-scope.
    jwt_claim-aud   =  jwt_profile-aud.


    GET TIME STAMP FIELD timestamp_iat.

    CONVERT TIME STAMP timestamp_iat TIME ZONE 'UTC' INTO DATE date TIME time.

    cl_pco_utility=>convert_abap_timestamp_to_java(
        EXPORTING
            iv_date = date
            iv_time = time
        IMPORTING
            ev_timestamp = lv_unix_timestamp ).

    IF strlen( lv_unix_timestamp ) GT 10.
      jwt_claim-iat = lv_unix_timestamp(10).
    ELSE.
      jwt_claim-iat = lv_unix_timestamp.
    ENDIF.


    CALL FUNCTION 'TIMESTAMP_DURATION_ADD'
      EXPORTING
        timestamp_in  = timestamp_iat
        timezone      = 'UTC'
        duration      = 1
        unit          = 'H'
      IMPORTING
        timestamp_out = timestamp_exp.

    CONVERT TIME STAMP timestamp_exp TIME ZONE 'UTC' INTO DATE date TIME time.

    cl_pco_utility=>convert_abap_timestamp_to_java(
           EXPORTING
               iv_date = date
               iv_time = time
           IMPORTING
               ev_timestamp = lv_unix_timestamp ).

    jwt_claim-exp =
       COND #(
           WHEN strlen( lv_unix_timestamp ) > 10
               THEN lv_unix_timestamp(10)
               ELSE lv_unix_timestamp ).


    jwt_header_json = /ui2/cl_json=>serialize(
      compress = abap_true
      data  = jwt_header
      pretty_name = /ui2/cl_json=>pretty_mode-low_case    ).

    jwt_claim_json = /ui2/cl_json=>serialize(
      compress = abap_true
      data  = jwt_claim
      pretty_name = /ui2/cl_json=>pretty_mode-low_case  ).

*encode base64
    jwt_header_base64url =  cl_http_utility=>encode_base64( unencoded = jwt_header_json ).
    jwt_claim_base64url  =  cl_http_utility=>encode_base64( unencoded = jwt_claim_json ).
*format base  64

    REPLACE ALL OCCURRENCES OF '=' IN jwt_header_base64url WITH ''.
    REPLACE ALL OCCURRENCES OF '+' IN jwt_header_base64url WITH '-'.
    REPLACE ALL OCCURRENCES OF '/' IN jwt_header_base64url WITH '_'.

    REPLACE ALL OCCURRENCES OF '=' IN jwt_claim_base64url WITH ''.
    REPLACE ALL OCCURRENCES OF '+' IN jwt_claim_base64url WITH '-'.
    REPLACE ALL OCCURRENCES OF '/' IN jwt_claim_base64url WITH '_'.


    input_base64url = |{ jwt_header_base64url }.{ jwt_claim_base64url }|.
    input_length = strlen( input_base64url ).

* convert to binary data
    DATA lv_xstring TYPE xstring.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text     = input_base64url
        encoding = '4110'
      IMPORTING
        buffer   = lv_xstring
      EXCEPTIONS
        failed   = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      zcx_jwt_generator=>raise_system( ).
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = lv_xstring
      TABLES
        binary_tab = input_bins.
    IF sy-subrc <> 0.
      zcx_jwt_generator=>raise_system( ).
    ENDIF.

    CLEAR : lv_xstring .
    APPEND ssfinfo TO signers.

    CALL FUNCTION 'SSF_KRN_SIGN'
      EXPORTING
        str_format                   = 'PKCS1-V1.5'
        b_inc_certs                  = abap_false
        b_detached                   = abap_false
        b_inenc                      = abap_false
        ostr_input_data_l            = input_length
        str_hashalg                  = 'SHA256'
      IMPORTING
        ostr_signed_data_l           = output_length
        crc                          = output_crc    " SSF Return code
      TABLES
        ostr_input_data              = input_bins
        signer                       = signers
        ostr_signed_data             = output_bins
      EXCEPTIONS
        ssf_krn_error                = 1
        ssf_krn_noop                 = 2
        ssf_krn_nomemory             = 3
        ssf_krn_opinv                = 4
        ssf_krn_nossflib             = 5
        ssf_krn_signer_list_error    = 6
        ssf_krn_input_data_error     = 7
        ssf_krn_invalid_par          = 8
        ssf_krn_invalid_parlen       = 9
        ssf_fb_input_parameter_error = 10.
    IF sy-subrc <> 0.
      zcx_jwt_generator=>raise_system( ).
    ENDIF.


    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length = output_length
        encoding     = '4110'
      IMPORTING
        text_buffer  = signature
      TABLES
        binary_tab   = output_bins
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      zcx_jwt_generator=>raise_system( ).
    ENDIF.


    signature_base64url =  cl_http_utility=>encode_base64( unencoded = signature ).

    jwt = |{ input_base64url }.{ signature_base64url }|.

    IF jwt IS INITIAL.
      ls_log-msgid = 'ZCPS_MESSAGE'.
      ls_log-msgty = 'E'.
      ls_log-msgno = '011'.
      lo_log->add_msg( ls_log ).
    ELSE.
      ls_log-msgid = 'ZCPS_MESSAGE'.
      ls_log-msgty = 'S'.
      ls_log-msgno = '012'.
      ls_log-msgv1 = jwt.
      lo_log->add_msg( ls_log ).
    ENDIF.


  ENDMETHOD.


  METHOD post_call.

    DATA:
      BEGIN OF ls_token,
        access_token TYPE string,
        expires_in   TYPE i,
        token_type   TYPE string,
      END OF ls_token,
      BEGIN OF ls_error,
        error             TYPE string,
        error_description TYPE string,
      END OF ls_error,
      lv_url           TYPE string,
      lo_exception     TYPE REF TO zcx_jwt_generator,
      lo_rest_client   TYPE REF TO cl_rest_http_client,
      lo_response      TYPE REF TO if_rest_entity,
      lo_request       TYPE REF TO if_rest_entity,
      lv_body          TYPE string,
      lo_http_client   TYPE REF TO if_http_client,
      lr_root          TYPE REF TO cx_root,
      jwt              TYPE string,
      reason           TYPE  string,
      http_status      TYPE  string,
      content_length   TYPE  string,
      location         TYPE  string,
      content_type     TYPE  string,
      l_response       TYPE  string,
      display_response TYPE flag.
    TRY.
        jwt = generate_jwt( ).

        IF jwt IS INITIAL.
          RETURN.
        ENDIF.


        lv_url = jwt_profile-aud.



        CREATE OBJECT lo_exception.

        cl_http_client=>create_by_url(
          EXPORTING
            url                = lv_url   " URL
            ssl_id             = 'ANONYM'    " SSL Identity
          IMPORTING
            client             = lo_http_client   " HTTP Client Abstraction
          EXCEPTIONS
            argument_not_found = 1
            plugin_not_active  = 2
            internal_error     = 3
            OTHERS             = 4 ).
        IF sy-subrc <> 0.
          zcx_jwt_generator=>raise_system( ).
        ENDIF.


        IF lo_http_client IS NOT BOUND.
          zcx_jwt_generator=>raise_system( ).
        ENDIF.

        lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
        CREATE OBJECT lo_rest_client
          EXPORTING
            io_http_client = lo_http_client.
        lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
*       lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
        lo_http_client->request->set_formfield_encoding( formfield_encoding = if_http_entity=>co_formfield_encoding_encoded ).
        lo_request = lo_rest_client->if_rest_client~create_request_entity( ).

        lo_request->set_content_type( 'application/x-www-form-urlencoded').
        lo_request->set_string_data( lv_body ).

        lo_http_client->request->set_form_field(
     EXPORTING
       name  = 'grant_type'
       value = 'urn:ietf:params:oauth:grant-type:jwt-bearer'
       ).

        lo_http_client->request->set_form_field(
        EXPORTING
          name  = 'assertion'
          value = jwt
          ).

        TRY.
            lo_rest_client->if_rest_resource~post( lo_request ).
          CATCH cx_rest_client_exception INTO lr_root.
            l_response = lr_root->get_longtext( ).
            CALL METHOD cl_demo_output=>display_text( l_response ).
        ENDTRY.
** Collect response
        lo_response = lo_rest_client->if_rest_client~get_response_entity( ).
*http_status = lo_response->get_header_field( 'status' ).
        http_status =  lo_rest_client->if_rest_client~get_status( ).
        reason = lo_response->get_header_field( '~status_reason' ).
        content_length = lo_response->get_header_field( 'content-length' ).
        location = lo_response->get_header_field( 'location' ).
        content_type = lo_response->get_header_field( 'content-type' ).
        response = lo_response->get_string_data( ).


        GET PARAMETER ID 'ZDISPLAY_RESPONSE' FIELD display_response .
        IF display_response  EQ 'X'.
          CALL METHOD cl_demo_output=>display_json( response ).
        ENDIF.
        lo_http_client->close( ).

        IF sy-subrc <> 0.
          zcx_jwt_generator=>raise_system( ).
        ENDIF.

      CATCH zcx_jwt_generator INTO lo_exception.

    ENDTRY.
  ENDMETHOD.


  METHOD store_token.

    DATA : ls_token TYPE zcps_token.

    ls_token-mandt = sy-mandt.
    ls_token-zdate = sy-datum.
    ls_token-token = im_ls_token-token.

    GET TIME STAMP FIELD ls_token-time_stamp .

*    delete previous entry .

    DELETE  FROM zcps_token.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
*

    INSERT zcps_token FROM ls_token.

    IF sy-subrc = 0.
      COMMIT WORK .
    ELSE.
      ls_log-msgty = 'E'.
      ls_log-msgid = '014'. "Error Saving token to database
      ls_log-msgno = 'ZCPS_MESSAGE'.
      lo_log->add_msg( ls_log ).
    ENDIF.

    CLEAR ls_token.
  ENDMETHOD.


  METHOD zif_auth~get_config.

    SELECT SINGLE *
    FROM zjwt_profile
    INTO CORRESPONDING FIELDS OF jwt_profile
    WHERE profile_name =  profile_name.
    IF sy-subrc = 0.
* convert  to lowercase
      TRANSLATE jwt_profile-aud    TO LOWER CASE.
      TRANSLATE jwt_profile-sub    TO LOWER CASE.
      TRANSLATE jwt_profile-iss    TO LOWER CASE.
      TRANSLATE jwt_profile-scope  TO LOWER CASE.
    ENDIF.
  ENDMETHOD.


  METHOD zif_auth~get_logger.
    CREATE OBJECT lo_log TYPE zcl_cps_logger.
  ENDMETHOD.


  METHOD zif_auth~get_profile.
    jwt_profile = me->jwt_profile.
  ENDMETHOD.


  METHOD zif_auth~get_token.

    DATA : response TYPE string,
           BEGIN OF ls_token,
             access_token TYPE string,
             expires_in   TYPE i,
             token_type   TYPE string,
           END OF ls_token,
           BEGIN OF ls_idtoken,
             id_token TYPE string,
           END OF ls_idtoken,
           BEGIN OF ls_error,
             error             TYPE string,
             error_description TYPE string,
           END OF ls_error,

           ls_token_db TYPE zcps_token.


*get logger object
    TRY.
        lo_log ?= get_logger( ).


*check if valid token exist  otherwise request for token

        ls_token_db = me->check_for_token_in_db( ).

        token =  ls_token_db-token.

        IF  token IS INITIAL.

********************************************************************
** POST CALL
********************************************************************
          response  = me->post_call(  ).


********************************************************************
** extract token  from json
********************************************************************


          SEARCH response FOR '"id_token"'.

          IF sy-subrc NE 0.

            /ui2/cl_json=>deserialize(
              EXPORTING
                json = response
              CHANGING
                data = ls_token ).

          ELSE.

            /ui2/cl_json=>deserialize(
                    EXPORTING
                      json = response
                    CHANGING
                      data = ls_idtoken ).

          ENDIF.

          CLEAR ls_error.

          SEARCH  response FOR 'Error type :'.

          IF sy-subrc = 0.
            /ui2/cl_json=>deserialize(
              EXPORTING
                json = response
              CHANGING
                data = ls_error ).

            IF ls_error-error  IS NOT INITIAL.
              CONCATENATE 'Error type :' ls_error-error 'Error Description:' ls_error-error_description INTO ls_error-error_description SEPARATED BY space.
              lo_log->add_errortext( ls_error-error_description ).
            ENDIF.

          ENDIF.

          IF ls_token-access_token IS NOT INITIAL.
            ls_token_db-token  =  token = ls_token-access_token.
          ELSE.
            ls_token_db-token  = token = ls_idtoken-id_token.
          ENDIF.
* Store token
        me->store_token( ls_token_db ).
        ENDIF.

      CATCH cx_bal_exception.

    ENDTRY.

  ENDMETHOD.


  METHOD zif_auth~refresh_token.

  ENDMETHOD.


  METHOD zif_auth~set_profile.
    profile_name  = PROFILE.
  ENDMETHOD.
ENDCLASS.
