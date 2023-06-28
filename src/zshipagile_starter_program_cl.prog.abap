*&---------------------------------------------------------------------*
*&  Include           ZSHIPAGILE_STARTER_PROGRAM_CL
*&---------------------------------------------------------------------*

INCLUDE :
trustman_dekl,
trustman_form,
trustman_psem,
trustman_dist,
trustman_ssls,
trustman_ctrl,
trustman_dynp,
trustman_prog,
trustman_lps.


CLASS lcx_pse DEFINITION INHERITING FROM cx_static_check FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    DATA:
      mf_errortext TYPE string,
      mf_html      TYPE string,
      t100key      TYPE scx_t100key.

    METHODS constructor
      IMPORTING
        !errortext TYPE string OPTIONAL
        !html      TYPE string OPTIONAL
        !textid    TYPE scx_t100key OPTIONAL
        !previous  LIKE previous OPTIONAL.

ENDCLASS.                    "lcx_pse DEFINITION

***********************************************************************
* CLASS lcx_pse IMPLEMENTATION
************************************************************************

CLASS lcx_pse IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    mf_errortext = errortext.
    mf_html = html.
    t100key = textid.
  ENDMETHOD.                    "constructor

ENDCLASS..

*INCLUDE zgcp_test_class.
CLASS  lcl_driver DEFINITION FINAL .
  PUBLIC SECTION.

    TYPES: tt_lines TYPE STANDARD TABLE OF string.
    METHODS:   handle_ok_code IMPORTING ok_code TYPE sy-ucomm.
    METHODS:   initialization.
    METHODS:   selection_screen_output.
    METHODS:   f4_file  IMPORTING program_name  TYPE  sycprog
                                  dynpro_number TYPE sydynnr.

  PRIVATE SECTION.


    TYPES  : BEGIN OF ty_url ##NEEDED,
               checkbox TYPE flag  ##NEEDED,
               endpoint TYPE zendpoint_name,
               url      TYPE zurl ##NEEDED,
             END OF ty_url  ##NEEDED,

             BEGIN OF ty_ack ##NEEDED ,
               check   TYPE char1,
               srno    TYPE i,
               ack_id  TYPE string,
               message TYPE char20,
               status  TYPE char4,
             END OF ty_ack  ##NEEDED.

*   Secure Login Server Group Profile
*
    TYPES:
      BEGIN OF sls_profile,
        name TYPE string,
        type TYPE string,
        url  TYPE string,
        prot TYPE i,
      END OF sls_profile.

    TYPES:
      BEGIN OF sls_sid,
        sid TYPE string,
      END OF sls_sid.

    TYPES:
      BEGIN OF sls_root,
        cert TYPE xstring,
      END OF sls_root.

    TYPES:
      BEGIN OF sls_group,
        id          TYPE string,
        name        TYPE string,
        description TYPE string,
        sids        TYPE STANDARD TABLE OF sls_sid WITH KEY sid,
        profiles    TYPE STANDARD TABLE OF sls_profile WITH KEY name,
        ssl_roots   TYPE STANDARD TABLE OF sls_root WITH KEY cert,
      END OF sls_group.
    DATA altnames TYPE TABLE OF string.

    TYPES:
      BEGIN OF sls_auth,
        status  TYPE string,
        salt    TYPE string,
        subject TYPE string,
        keysize TYPE ssfkeylen,
      END OF sls_auth.

    CONSTANTS:
      c_x                TYPE char1            VALUE 'X',
      c_b1               TYPE char2            VALUE 'B1',
      c_b2               TYPE char2            VALUE 'B2',
      c_b3               TYPE char2            VALUE 'B3',
      c_b4               TYPE char2            VALUE 'B4',
      c_subs             TYPE string           VALUE 'ZCPS_SUBS',
      c_topic            TYPE string           VALUE 'ZCPS_TOPIC',
      c_profile          TYPE string           VALUE 'SHIPAGILE',
      c_experimental     TYPE string           VALUE 'EXPERIMENTAL',
      c_subscription     TYPE string           VALUE 'EXPERIMENTAL',
      c_zjwt_profile     TYPE string           VALUE 'ZJWT_PROFILE',
      c_config_file_name TYPE string           VALUE 'CONFIG.JSON',
      c_key_file_name    TYPE string           VALUE 'KEY.P12',
      c_pub_sub          TYPE string           VALUE 'PUBSUB.CRT',
      c_object           TYPE balobj_d         VALUE 'ZCPS_LOG',
      c_sub_object       TYPE balobj_d         VALUE 'ZCPS_LOG',
      c_program_name     TYPE rsvar-report     VALUE 'ZCPS_DELIVERY_SEND'.


    DATA: BEGIN OF  wa_url,
            name     TYPE zendpoint_name,
            type     TYPE ztype_sub_or_topic,
            endpoint TYPE string,
            alias    TYPE string,
          END OF wa_url,
          BEGIN OF  wa_p12,
            name            TYPE string,
            privatekeytype  TYPE string,
            privatekeydata  TYPE string,
            validaftertime  TYPE  string,
            validbeforetime TYPE  string,
            keyalgorithm    TYPE  string,
            keyorigin       TYPE  string,
            keytype         TYPE  string,
          END OF wa_p12,
          BEGIN OF wa_config,
            jwt_profile TYPE zjwt_profile,
            url         LIKE TABLE OF wa_url,
            p12         LIKE wa_p12,
          END OF  wa_config,

          lt_file_contents TYPE tt_lines,
          selected_folder  TYPE string,
          lv_filecount     TYPE i,
          lt_files         TYPE TABLE OF file_info,
          success          TYPE flag,
          lo_subs          TYPE  REF TO zcl_cps_subscription,
          lo_topic         TYPE  REF TO zcl_cps_topic,
          profile          TYPE string,
          lv_success       TYPE flag,
          lt_messages      TYPE  zsubs_pull_msg_table,
          lo_message       TYPE  REF TO zif_message,
          mr_exception     TYPE REF TO lcx_pse,
          mo_client        TYPE REF TO if_http_client,
          mr_reader        TYPE REF TO if_sxml_reader,
          mf_profile       TYPE localfile,
          mf_psename       TYPE ssfpsename,
          mf_pab           TYPE ssfpab,
          mf_cred          TYPE ssfpw,
          mf_subject       TYPE string,                 "Subject SLS PSE
          mf_issuer        TYPE string,                 "Issuer SLS PSE
          mf_altname       TYPE string,
          ms_t100key       TYPE scx_t100key,            "Exception Last error
          mf_errortext     TYPE string,                 "Exception Last error
          mf_html          TYPE string,
          mf_statuscode    TYPE string,                 "HTTP Statuscode
          mf_statuscodei   TYPE i,
          mf_statustext    TYPE string,                 "HTTP Statustext
          mf_ecode         TYPE sysubrc,                "CL_HTTP_CLIENT~GET_LAST_ERROR
          mf_emessage      TYPE string,
          mf_response      TYPE xstring,                "HTTP Response body
          ms_sls_group     TYPE sls_group,
          ms_sid           TYPE sls_sid,
          ms_profile       TYPE sls_profile,
          ms_root          TYPE sls_root,
          mr_ra_profile    TYPE REF TO sls_profile,   "RA Profile in ms_sls_group
          ms_auth          TYPE sls_auth.

    DATA: separator    TYPE  flag,
          but1_pressed TYPE flag,
          but2_pressed TYPE flag,
          but3_pressed TYPE flag.

    DATA: lo_text_edit TYPE REF TO cl_gui_textedit,
          lo_dock      TYPE REF TO cl_gui_docking_container,
          lt_text      TYPE TABLE OF char255.




* Secure Login Server Authenticate
*

    METHODS: progress_indicator IMPORTING text TYPE string,
      update_config_db_tables       RETURNING VALUE(success) TYPE flag   EXCEPTIONS zcx_,
      extract_config                IMPORTING lt_lines TYPE tt_lines,
      read_directory,
      test_connection,
      upload_certification,
      upload_configuration,
      update_alias_table,
      check_anonymous_pse,
      update_zcps_subs,
      update_zcps_topic,
      lock_table                    IMPORTING name TYPE rstable-tabname,
      unlock_table                  IMPORTING name TYPE rstable-tabname,
      create_ssf_application,
      create_ssf_application_jwt_si,
      create_ssf_pse_node,
      create_message ,
      subscribe,
      publish,
      process_message,
      get_user_desktop_directory,
      update_profile_table,
      get_server_file_separator     RETURNING VALUE(f_separator) TYPE char1,
      upload_file                   IMPORTING filename  TYPE  string CHANGING lt_lines TYPE tt_lines,
      internal_error                RAISING lcx_pse,
      store_pse                     IMPORTING fname TYPE localfile psename TYPE ssfpsename RAISING lcx_pse,
      notify_icm                    IMPORTING psename TYPE icm_credname,

      check_directory_existence     IMPORTING directory TYPE string,
      create_pse_upload_json_key,
      create_log_object,
      schedule_jobs,
      show_text_editor,
      set_error IMPORTING error TYPE char255,
      upload_pubsub_cert_ssl.

ENDCLASS.


CLASS lcl_driver IMPLEMENTATION.
  METHOD handle_ok_code.
    IF p_file IS INITIAL   .
      MESSAGE  'Please Provide Directory Path' TYPE 'E'.
    ELSE.

      CASE ok_code.
        WHEN  c_b1.
          me->upload_configuration( ).                   "#EC CI_CALLTA
          but1_pressed  =  abap_true.
        WHEN c_b2.
          IF   but1_pressed  =  abap_true.
            me->upload_certification( ).                 "#EC CI_CALLTA
            but2_pressed  =  abap_true.
          ELSE.
            MESSAGE 'Please follow sequence !' TYPE 'E'.
          ENDIF.
        WHEN c_b3.
          IF   but2_pressed  =  abap_true.
            me->test_connection( ).                      "#EC CI_CALLTA
          ELSE.
            MESSAGE 'Please follow sequence !' TYPE 'E'.
          ENDIF.
        WHEN c_b4.
          me->schedule_jobs( ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.
  METHOD initialization.
    me->show_text_editor( ).
    but1 = TEXT-100.
    but2 = TEXT-101.
    but3 = TEXT-102.
    but4 = TEXT-103.
    comm01 = '1.Enter the file path where config zip folder was downloaded to your'.
    comm02 = '  local machine from your Shipagile account.'.
    comm03 ='2.To download the file ,log into your Shipagile account and go to '.
    comm04 ='  integration and select SAP on-prem and follow the on screen instructions.'.
    CLEAR :  me->but1_pressed,
             me->but2_pressed,
             me->but3_pressed .
    me->get_user_desktop_directory( ).
  ENDMETHOD.
  METHOD upload_certification.
    IF p_file IS NOT  INITIAL.
      me->check_directory_existence( CONV string( p_file ) ).
    ENDIF.
    me->progress_indicator('Check Existence of Cryptolib').
    me->check_anonymous_pse( ).
    me->progress_indicator('Create Shipagile Key Node').
    me->create_ssf_application( ) .
    me->create_pse_upload_json_key( ).
    me->progress_indicator('Upload SSL Certificate').
    me->upload_pubsub_cert_ssl( ).
    me->progress_indicator('Creating SSF Profile').
    p_but2 = icon_green_light.
  ENDMETHOD.
  METHOD upload_configuration.

    IF p_file  IS NOT  INITIAL.
      me->selected_folder  = to_lower( p_file ).
      me->check_directory_existence( CONV string( p_file ) ).
    ENDIF.
    me->read_directory( ).
    me->progress_indicator('Reading Directory').

    CONCATENATE me->selected_folder me->separator  c_config_file_name INTO DATA(filename).
    me->upload_file( EXPORTING filename = filename CHANGING lt_lines = me->lt_file_contents ).
    me->progress_indicator('Reading Config File').
    me->extract_config( me->lt_file_contents ).
    me->progress_indicator('Uploading Configuration File Data to SAP').
    IF me->update_config_db_tables( ) EQ  abap_true.
      p_but1 = icon_green_light.
    ENDIF.
  ENDMETHOD.
  METHOD f4_file.
    CALL METHOD cl_gui_frontend_services=>directory_browse
      CHANGING
        selected_folder      = me->selected_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    p_file = to_lower( me->selected_folder ).
  ENDMETHOD.
  METHOD upload_file.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = filename
        filetype                = 'ASC'
        codepage                = '4110'
      CHANGING
        data_tab                = lt_lines
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD read_directory.
    CALL METHOD cl_gui_frontend_services=>directory_list_files
      EXPORTING
        directory                   = me->selected_folder
      CHANGING
        file_table                  = me->lt_files
        count                       = me->lv_filecount
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6.
    IF sy-subrc NE 0.
      MESSAGE |Error Reading Directoy  | TYPE 'E'.
    ELSE.
      LOOP AT  lt_files ASSIGNING FIELD-SYMBOL(<wa_file>).
        <wa_file>-filename = to_upper( <wa_file>-filename ) .
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD extract_config.

    DATA: lv_json TYPE string.

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs>) FROM 1   TO  lines( lt_lines ).
      CONCATENATE lv_json <fs> INTO lv_json.
    ENDLOOP.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = lv_json
      CHANGING
        data = me->wa_config.
  ENDMETHOD.
  METHOD progress_indicator .
    cl_progress_indicator=>progress_indicate( i_text = text i_processed = sy-index  i_total = 1 i_output_immediately = abap_true ).
    WAIT UP TO 1 SECONDS.
  ENDMETHOD.
  METHOD update_config_db_tables.
    TRY.
*    UPDATE ZJWT_PROFILE  TABLE
        me->update_profile_table( ).
*    UPDATE TOPIC URL TABLE
        me->update_zcps_topic( ).
*    UPDATE SUBSCRIPTION URL TABLE
        me->update_zcps_subs( ).
*    UPDATE ALIAS TABLE.
        me->update_alias_table( ).
*    Create SAP Log
        create_log_object( ).

        success   = abap_true.
      CATCH lcx_pse INTO DATA(lo_excp).
        success   = abap_false.
    ENDTRY.
  ENDMETHOD.
  METHOD test_connection.
*  get test delivery payload .

*  publish test delivery payload to test topic

*  pull  message and acknowledge

*
    p_but3 = icon_green_light.
  ENDMETHOD.
  METHOD check_anonymous_pse.

    DATA:
      lf_profile_temp TYPE localfile,
      lf_tempname     TYPE trfile,
      lf_psename      TYPE ssfpsename,
      lf_answer       TYPE c,
      ptab            TYPE abap_func_parmbind_tab,
      ptab_line       TYPE abap_func_parmbind,
      etab            TYPE abap_func_excpbind_tab,
      etab_line       TYPE abap_func_excpbind,
      dn              TYPE certsubjct,
      cclalg          TYPE string,
      lf_alg          TYPE ssfflag,
      lf_keylen       TYPE ssfkeylen,
      lf_credname     TYPE icm_credname,
      lo_lcx_pse      TYPE REF TO lcx_pse.

*...Set RC = Error
    sy-subrc = 1.
    TRY.
*...Check SSL Client Anonym
        CALL FUNCTION 'SSFPSE_PARAMETER'
          EXPORTING
            context       = 'SSLC'
            applic        = 'ANONYM'
          EXCEPTIONS
            pse_not_found = 1
            OTHERS        = 2.
        IF sy-subrc > 1. internal_error( ). ENDIF.
        IF sy-subrc = 1.
*...PSE is missing - create it
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Create PSE SSL client Anonymous'(034)
              text_question         = 'PSE SSL client Anonymous is missing. Create it?'(033)
              display_cancel_button = ' '
              default_button        = '2'
            IMPORTING
              answer                = lf_answer
            EXCEPTIONS
              OTHERS                = 1.
          IF sy-subrc <> 0. internal_error( ). ENDIF.
          IF lf_answer <> '1'. RETURN. ENDIF.
          dn = 'CN=anonymous'.
          cclalg = 'RSA:2048:SHA1'.
          ptab_line-name = 'DN'.
          ptab_line-kind = abap_func_exporting.
          GET REFERENCE OF dn INTO ptab_line-value.
          INSERT ptab_line INTO TABLE ptab.
          IF sy-saprl > '740'.
            ptab_line-name = 'CCLALG'.
            ptab_line-kind = abap_func_exporting.
            GET REFERENCE OF cclalg INTO ptab_line-value.
            INSERT ptab_line INTO TABLE ptab.
          ELSE.
            lf_alg = 'R'.
            lf_keylen = 1024.
            ptab_line-name = 'ALG'.
            ptab_line-kind = abap_func_exporting.
            GET REFERENCE OF lf_alg INTO ptab_line-value.
            INSERT ptab_line INTO TABLE ptab.
            ptab_line-name = 'KEYLEN'.
            ptab_line-kind = abap_func_exporting.
            GET REFERENCE OF lf_keylen INTO ptab_line-value.
            INSERT ptab_line INTO TABLE ptab.
          ENDIF.
          ptab_line-name = 'PSEPATH'.
          ptab_line-kind = abap_func_importing.
          GET REFERENCE OF lf_tempname INTO ptab_line-value.
          INSERT ptab_line INTO TABLE ptab.
          etab_line-name = 'OTHERS'.
          etab_line-value = 1.
          INSERT etab_line INTO TABLE etab.
          CALL FUNCTION 'SSFPSE_CREATE'
            PARAMETER-TABLE ptab
            EXCEPTION-TABLE etab.
          IF sy-subrc <> 0. internal_error( ). ENDIF.
          lf_profile_temp = lf_tempname.

          CALL FUNCTION 'SSFPSE_FILENAME'
            EXPORTING
              context = 'SSLC'
              applic  = 'ANONYM'
            IMPORTING
              psename = lf_psename
            EXCEPTIONS
              OTHERS  = 0.

          store_pse( fname = lf_profile_temp psename = lf_psename ).


          lf_credname = lf_psename.
          notify_icm( lf_credname ).
        ENDIF.
      CATCH lcx_pse INTO lo_lcx_pse.
    ENDTRY.
  ENDMETHOD.
  METHOD lock_table.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = name
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2.
  ENDMETHOD.

  METHOD unlock_table.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        mode_rstable = 'E'
        tabname      = name.

  ENDMETHOD.
  METHOD  update_zcps_subs.

    DATA : lt_cps_subs TYPE STANDARD TABLE OF zcps_subs,
           cps_subs    TYPE zcps_subs.

    LOOP  AT me->wa_config-url INTO DATA(wa_url)  WHERE type = 'S'.
      cps_subs-sub_name  = wa_url-name.
      cps_subs-url  = wa_url-endpoint.
      APPEND cps_subs TO lt_cps_subs.
      CLEAR:cps_subs.
    ENDLOOP.

    me->lock_table( 'ZCPS_SUBS').
*    MODIFY zcps_subs FROM TABLE lt_cps_subs.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.
    me->unlock_table('ZCPS_SUBS').
  ENDMETHOD.
  METHOD publish.

    CREATE OBJECT lo_topic.

    create_message( ).

    CALL METHOD lo_topic->publish
      EXPORTING
        lo_message = lo_message
        topic      = c_experimental
        profile    = c_profile.

    FREE : lo_message,profile.
  ENDMETHOD.
  METHOD subscribe.

    CREATE OBJECT lo_subs.

    CALL METHOD lo_subs->pull
      EXPORTING
        subscription = c_subscription
        profile      = c_profile
      RECEIVING
        lt_message   = lt_messages.

    process_message(  ).

    FREE:lo_subs,profile.
  ENDMETHOD.
  METHOD process_message.
    DATA :
      lo_cx_salv_msg       TYPE  REF TO cx_salv_msg,
      lo_cx_salv_not_found TYPE  REF TO cx_salv_not_found,
      lo_functions         TYPE  REF TO cl_salv_functions_list,
      lo_columns           TYPE  REF TO cl_salv_columns_table,
      lo_column            TYPE  REF TO cl_salv_column_table,
      lo_events            TYPE  REF TO cl_salv_events_table,
      lines                TYPE i,
      lv_index             TYPE c,
      wa_ack               TYPE ty_ack,
      wa_message           TYPE zsubs_pull_msg_object,
      lt_url               TYPE  TABLE  OF ty_url ##NEEDED,
      lt_ack_alv           TYPE  TABLE  OF ty_ack  ##NEEDED,
      lo_alv               TYPE  REF TO cl_salv_table ##NEEDED,
      lo_alv2              TYPE  REF TO cl_salv_table ##NEEDED.

    DESCRIBE             TABLE lt_messages LINES lines.

    IF lines IS INITIAL.
      lines = 5.
    ENDIF.

    DO lines TIMES.
      wa_ack-srno = sy-index.
      wa_ack-check = abap_false.
      lv_index =  sy-index.
      wa_ack-status = icon_red_light.
      READ TABLE lt_messages INTO wa_message INDEX sy-index.
      wa_ack-ack_id  = wa_message-ack_id.
      CONCATENATE TEXT-t11 lv_index INTO wa_ack-message.
      CONDENSE wa_ack-message.
      APPEND wa_ack TO lt_ack_alv.
      CLEAR :wa_ack,lv_index,wa_message.
    ENDDO.



    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_alv2
          CHANGING
            t_table      = lt_ack_alv.

        lo_columns = lo_alv2->get_columns( ).
        lo_column ?= lo_columns->get_column( 'CHECK' )  ##NO_TEXT.
        lo_column->set_short_text( TEXT-tc7  )  ##NO_TEXT.
        lo_column->set_medium_text( TEXT-tc6 ) ##NO_TEXT.
        lo_column->set_long_text( TEXT-tc6  ) ##NO_TEXT.
        lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ) ##NO_TEXT.
        lo_column ?= lo_columns->get_column( 'SRNO' ) ##NO_TEXT.
        lo_column->set_short_text( TEXT-tc8  ) ##NO_TEXT.
        lo_column ?= lo_columns->get_column( 'ACK_ID' )  ##NO_TEXT.
        lo_column->set_short_text( TEXT-tc9  )  ##NO_TEXT.
        lo_column ?= lo_columns->get_column( 'STATUS' )  ##NO_TEXT.
        lo_column->set_short_text( TEXT-t10 ) ##NO_TEXT.
        lo_column ?= lo_columns->get_column( 'MESSAGE' ) ##NO_TEXT.
        lo_column->set_short_text( TEXT-t11  ) ##NO_TEXT.
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ) ##NO_TEXT.
        lo_functions  = lo_alv2->get_functions( ).
        lo_functions->set_all('X').

*-- pf_status
        lo_alv2->set_screen_status(
           pfstatus      =  'STANDARD'
           report       = sy-repid
           set_functions = lo_alv2->c_functions_all ).

      CATCH cx_salv_not_found  INTO  lo_cx_salv_not_found.
        WRITE lo_cx_salv_not_found->get_text( ).
      CATCH cx_salv_msg INTO lo_cx_salv_msg.
        WRITE lo_cx_salv_msg->get_text( ).
    ENDTRY.
*   all events
    lo_events = lo_alv2->get_event( ).
*
*   Event handler
*    SET HANDLER  lo_handler->handle_checkbox_for_ack  FOR lo_events.
*    lo_handler->set_messages( lt_messages ).
*    SET HANDLER  lo_handler->on_user_command   FOR lo_events.
*    SET HANDLER  lo_handler->handle_hot_spot   FOR lo_events.


    lo_alv2->set_screen_popup(
                  start_column = 1    ##NUMBER_OK
                  end_column   = 100  ##NUMBER_OK
                  start_line   = 1    ##NUMBER_OK
                  end_line     = 20   ##NUMBER_OK
                  ).

    lo_alv2->display( ).
  ENDMETHOD.
  METHOD create_message.
    DATA:lv_json TYPE string.
    lv_json =  '{"deliveryId":"1234567890","createdDate":"2021-02-18T16:56:00","createdBy":"test@shipagile.com","status":"NEW","priorityId":"10"}'.

    CONCATENATE '{' lv_json '}' INTO lv_json.

    CALL METHOD lo_message->wrap_message_into_container
      EXPORTING
        in_json = lv_json.

  ENDMETHOD.
  METHOD create_ssf_application.
*Create new SSF Application
*Create ssf profile
    create_ssf_application_jwt_si( ).
    IF  me->lv_success EQ  c_x.
      CLEAR me->lv_success.
*Create ssf pse node
      create_ssf_pse_node( ).
      IF  me->lv_success EQ  c_x.
        MESSAGE TEXT-021 TYPE 'S'.
      ELSE.
        MESSAGE TEXT-022 TYPE  'E'.
      ENDIF.
    ELSE.
      MESSAGE TEXT-023 TYPE  'E'.
    ENDIF.

  ENDMETHOD.
  METHOD create_ssf_application_jwt_si.
    DATA : ssf_application TYPE ssfapplic,
           ssfapplict      TYPE ssfapplict.

    CONSTANTS: c_jwt_si TYPE char7 VALUE 'JWT_SI'.
*Check if profile exist
    SELECT  SINGLE applic FROM ssfapplic INTO ssf_application-applic WHERE applic EQ  c_jwt_si.
    IF  sy-subrc NE 0 .
      ssf_application-applic  = c_jwt_si.
      ssf_application-b_toolkit  =  c_x.
      ssf_application-b_format = c_x.
      ssf_application-b_pab =  c_x.
      ssf_application-b_profid =  c_x.
      ssf_application-b_profile =  c_x.
      ssf_application-b_hashalg =  c_x.
      ssf_application-b_encralg =  c_x.
      ssf_application-b_inccerts   =  ' '.
      ssf_application-b_detached   =  ' '.
      ssf_application-b_askpwd   =  ' '.
      ssf_application-b_distrib =  c_x.
* Description
      ssfapplict-sprsl = 'E'.
      ssfapplict-applic  = c_jwt_si.
      ssfapplict-descript = TEXT-018."'JWT Signature'.

      lock_table('SSFAPPLIC') .
      INSERT  ssfapplic FROM ssf_application .

      IF sy-subrc = 0  .
        INSERT  ssfapplict FROM ssfapplict.
        COMMIT WORK.
        lv_success = c_x.
      ENDIF.
      unlock_table('SSFAPPLIC').
      IF lv_success IS NOT INITIAL.
        MESSAGE TEXT-017 TYPE  'S'.  "'SSF APP Created Successfully'
      ENDIF.
    ELSE.
      lv_success = abap_true.
      MESSAGE TEXT-020 TYPE'S'.  "SSF APP Exists'
    ENDIF.
  ENDMETHOD.
  METHOD create_ssf_pse_node  .
    DATA : ssf_ssfargs TYPE ssfargs,
           lv_profile  TYPE ssfpab.

    SELECT SINGLE  * FROM  ssfargs INTO ssf_ssfargs WHERE applic  = 'JWT_SI'.
    IF sy-subrc NE 0 .
*create profile name
      lv_profile  = 'SAPJWT_SI&1.pse' ##NO_TEXT.
      REPLACE '&1' IN lv_profile  WITH sy-mandt.
      CONDENSE lv_profile.

      ssf_ssfargs-applic = 'JWT_SI'.
      ssf_ssfargs-ssftoolkit = 'SAPSECULIB'.
      ssf_ssfargs-ssfformat = 'PKCS1-V1.5'.
      ssf_ssfargs-pab  =  ssf_ssfargs-profile  = lv_profile.
      ssf_ssfargs-hashalg = 'SHA256'.
      ssf_ssfargs-encralg  = 'AES128-CBC'.
      ssf_ssfargs-distrib  = c_x.
      ssf_ssfargs-explicit  = c_x.

      lock_table('SSFARGS') .
      INSERT  ssfargs FROM ssf_ssfargs .
      IF sy-subrc = 0  .
        COMMIT WORK.
        me->lv_success = c_x.
      ENDIF.
      unlock_table('SSFARGS').
    ELSE.
      me->lv_success = c_x.
    ENDIF.
  ENDMETHOD.

  METHOD get_server_file_separator.

    CASE sy-opsys.
      WHEN 'Windows NT'.
        f_separator = '\'.
      WHEN 'Linux'.
        f_separator = '/'.
      WHEN 'HP-UX'.
        f_separator = '/'.
      WHEN 'OS400'.
        f_separator = '/'.
      WHEN OTHERS.
        f_separator = '/'.
    ENDCASE.
  ENDMETHOD.
  METHOD update_profile_table.

    DATA : jwt_profile TYPE zjwt_profile.

    MOVE-CORRESPONDING wa_config-jwt_profile TO jwt_profile.

    me->lock_table( 'ZJWT_PROFILE').
    MODIFY zjwt_profile FROM jwt_profile.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
    me->unlock_table('ZJWT_PROFILE').

  ENDMETHOD.
  METHOD update_zcps_topic.
    DATA : lt_cps_topic TYPE STANDARD TABLE OF zcps_topic,
           cps_topic    TYPE zcps_topic.


    LOOP  AT me->wa_config-url INTO DATA(wa_url)  WHERE type = 'T'.
      cps_topic-topic_name  = wa_url-name.
      cps_topic-url  = wa_url-endpoint.
      APPEND cps_topic TO lt_cps_topic.
      CLEAR:cps_topic.
    ENDLOOP.

    me->lock_table( 'ZCPS_TOPIC').
*    MODIFY zcps_topic FROM TABLE lt_cps_topic.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.
    me->unlock_table('ZCPS_TOPIC').
  ENDMETHOD.
  METHOD  update_alias_table.

    DATA : lt_endpoint TYPE STANDARD TABLE OF zcps_endpointgen,
           ls_endpoint TYPE zcps_endpointgen.

    LOOP  AT me->wa_config-url INTO DATA(wa_url).
      ls_endpoint-name  = wa_url-name.
      ls_endpoint-type  = wa_url-type.
      ls_endpoint-zalias  = wa_url-alias.
      APPEND ls_endpoint TO lt_endpoint.
      CLEAR:ls_endpoint.
    ENDLOOP.

    me->lock_table('ZCPS_ENDPOINTGEN').
*    MODIFY ZCPS_ENDPOINTGEN FROM TABLE lt_endpoint.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.
    me->unlock_table('ZCPS_ENDPOINTGEN').
  ENDMETHOD.
  METHOD internal_error.
    DATA: ms_t100key      TYPE scx_t100key.
    CLEAR: ms_t100key.
    ms_t100key-msgid = sy-msgid.
    ms_t100key-msgno = sy-msgno.
    ms_t100key-attr1 = sy-msgv1.
    ms_t100key-attr2 = sy-msgv2.
    ms_t100key-attr3 = sy-msgv3.
    ms_t100key-attr4 = sy-msgv4.
    CREATE OBJECT mr_exception
      EXPORTING
        textid = ms_t100key.
    RAISE EXCEPTION mr_exception.

  ENDMETHOD.
  METHOD store_pse.

    CALL FUNCTION 'SSFPSE_STORE'
      EXPORTING
        fname        = fname
        psename      = psename
        psepin       = mf_cred
        b_distribute = 'X'
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0. internal_error( ). ENDIF.

  ENDMETHOD.

  METHOD notify_icm.

    CALL FUNCTION 'ICM_SSL_PSE_CHANGED'
      EXPORTING
        global    = 1
        cred_name = psename
      EXCEPTIONS
        OTHERS    = 0.

  ENDMETHOD.
  METHOD get_user_desktop_directory.

    DATA desktop_directory TYPE string.

    me->separator = me->get_server_file_separator( ).

    cl_gui_frontend_services=>get_desktop_directory(
      CHANGING
        desktop_directory    = desktop_directory
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
           ).
    CALL METHOD cl_gui_cfw=>update_view.

*    check existance of Directory .
    IF sy-subrc = 0.
      CONCATENATE desktop_directory me->separator 'config_shipagile' INTO  desktop_directory.
      p_file = desktop_directory.
    ELSE.

* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.
  METHOD check_directory_existence.
    DATA:  directory_exists TYPE abap_bool.
    me->progress_indicator('Check if Config Directory Exists').
    cl_gui_frontend_services=>directory_exist(
     EXPORTING
       directory            = directory
     RECEIVING
       result               =  directory_exists
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
          ).
    IF sy-subrc = 0.
      IF directory_exists EQ abap_false.
        MESSAGE 'Please  Download Config Folder from Shipagile Portal' TYPE 'E'.
      ENDIF.
    ELSE.
*     Implement suitable error handling here
    ENDIF.
  ENDMETHOD.
  METHOD create_pse_upload_json_key.
    DATA:
      lf_bindata        TYPE xstring,
      lf_psedata        TYPE xstring,
*  create  ssf application
      lo_abap_pse_app   TYPE REF TO cl_abap_pse_application,
      lo_abap_pse       TYPE REF TO cl_abap_pse,
      lo_cx_abap_pse    TYPE REF TO cx_abap_pse,
      lo_cx_asn1_parser TYPE REF TO cx_asn1_parser,
      lo_cx_pkcs        TYPE REF TO cx_pkcs,
      password          TYPE ssfp12pw,
      filename          TYPE string.

    password = wa_config-jwt_profile-password.


**************************************************************************************************


    DATA : lv_base64_decoded_string TYPE string.

*    lv_base64_decoded_string = cl_http_utility=>decode_base64( encoded = me->wa_config-p12-privatekeydata ).

****approach1
*
**
*    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*      EXPORTING
*        text   = lv_base64_decoded_string
**       MIMETYPE       = ' '
**       ENCODING       = ENCODING
*      IMPORTING
*        buffer = lf_bindata
*      EXCEPTIONS
*        failed = 1.
*******************************************approach  3******************************************************
    DATA: lr_cutf8   TYPE REF TO cl_abap_conv_obj,
          lf_xstring TYPE xstring,
          lt_xstring TYPE STANDARD TABLE OF xstring,
          lf_lines   TYPE i,
          lf_idx     TYPE i,
          lf_string  TYPE string.
*. Base64 coded data
    CREATE OBJECT lr_cutf8 EXPORTING incode = '4110'.
*    lr_cutf8->convert( EXPORTING inbuff = lf_bindata outbufflg = 0 IMPORTING outbuff = lf_string ).

    TRY.
        lf_string =  me->wa_config-p12-privatekeydata .
        CALL FUNCTION 'SSFC_BASE64_DECODE'
          EXPORTING
            b64data = lf_string
          IMPORTING
            bindata = lf_xstring
          EXCEPTIONS
            OTHERS  = 1.
        IF sy-subrc = 0.
          lf_bindata = lf_xstring.
        ENDIF.

        DATA : lo_parser TYPE REF TO cl_asn1_parser.
        CREATE OBJECT lo_parser EXPORTING if_blob = lf_xstring if_sloppy = abap_true.




************************************************************************************************************
*    DATA:lt_content_binary TYPE TABLE OF x, "sdokcntbin,
*         wa_content_binary TYPE x. "sdokcntbin.
*
*    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*      EXPORTING
*        buffer     = buffer
**       APPEND_TO_TABLE       = ' '
** IMPORTING
**       OUTPUT_LENGTH         = OUTPUT_LENGTH
*      TABLES
*        binary_tab = lt_content_binary.
**
*    LOOP AT  lt_content_binary INTO  wa_content_binary .
*      CONCATENATE lf_bindata wa_content_binary  INTO lf_bindata IN BYTE MODE.
*    ENDLOOP.
**************


******Approach2

*    CALL FUNCTION 'SSFC_BASE64_DECODE'
*      EXPORTING
*        b64data                  = me->wa_config-p12-privatekeydata
**       B64LENG                  = B64LENG
**       B_CHECK                  = B_CHECK
*      IMPORTING
*        bindata                  = lf_bindata
*      EXCEPTIONS
*        ssf_krn_error            = 1
*        ssf_krn_noop             = 2
*        ssf_krn_nomemory         = 3
*        ssf_krn_opinv            = 4
*        ssf_krn_input_data_error = 5
*        ssf_krn_invalid_par      = 6
*        ssf_krn_invalid_parlen   = 7.
*
*
*    DATA :input_length TYPE  i .
*    input_length = xstrlen( lf_bindata ).
*
*    DATA:lt_content_binary TYPE TABLE OF x, "sdokcntbin,
*         wa_content_binary TYPE raw.
*
*
*    wa_content_binary-line = lf_bindata.
*    APPEND wa_content_binary TO lt_content_binary .
*
*    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*      EXPORTING
*        input_length = input_length
**       FIRST_LINE   = 0
**       LAST_LINE    = 0
** IMPORTING
**       BUFFER       = BUFFER
*      TABLES
*        binary_tab   = lt_content_binary
** EXCEPTIONS
**       FAILED       = 1
*      .
*    LOOP AT  lt_content_binary INTO  wa_content_binary .
*      CONCATENATE lf_bindata wa_content_binary  INTO lf_bindata IN BYTE MODE.
*    ENDLOOP.
*
*    lf_psedata = lf_bindata.
*
*    lf_bindata  =  wa_content_binary-line.
*    lf_bindata = me->wa_config-p12-privatekeydata.
**************************************************************************************************
*
*    CONCATENATE me->selected_folder me->separator c_key_file_name INTO filename.
*    TRANSLATE filename TO LOWER CASE.
**
*    CALL METHOD cl_secxml_helper=>upload_file(
*      EXPORTING
*        filename = filename
*      IMPORTING
*        bindata  = lf_bindata ).
*

        DATA: lt_binary TYPE TABLE OF solix,
              ls_binary TYPE xstring. "solix.
        ls_binary = cl_http_utility=>decode_base64( me->wa_config-p12-privatekeydata  ).
        DATA base64_key  TYPE string.
        CALL FUNCTION 'SSFC_BASE64_ENCODE'
          EXPORTING
            bindata = ls_binary
*           BINLENG =
          IMPORTING
            b64data = base64_key
*   EXCEPTIONS
*           SSF_KRN_ERROR                  = 1
*           SSF_KRN_NOOP                   = 2
*           SSF_KRN_NOMEMORY               = 3
*           SSF_KRN_OPINV                  = 4
*           SSF_KRN_INPUT_DATA_ERROR       = 5
*           SSF_KRN_INVALID_PAR            = 6
*           SSF_KRN_INVALID_PARLEN         = 7
*           OTHERS  = 8
          .
        IF sy-subrc <> 0.
          IF base64_key NE me->wa_config-p12-privatekeydata .
            MESSAGE 'error in key ' TYPE 'E'.
          ENDIF.
* Implement suitable error handling here
        ENDIF.


***************************************************************************
        password  = 'notasecret'.
        lf_bindata  = cl_pkcs=>get_octet_string( lo_parser ).
        cl_pkcs=>import_p12(
               EXPORTING
                         p12pw = password
                         p12keynumber = 1
                         p12data = lf_bindata
               IMPORTING psedata = lf_psedata ).

        CREATE OBJECT lo_abap_pse
          EXPORTING
            iv_pse = lf_psedata.

*    lo_abap_pse->save( iv_context  = 'SSFA' iv_application = 'SHPAGL').
        lo_abap_pse->save( iv_context  = 'SSFA' iv_application = 'JWT_SI').
      CATCH cx_abap_pse INTO lo_cx_abap_pse.
      CATCH cx_pkcs     INTO lo_cx_pkcs.
      CATCH  cx_asn1_parser INTO lo_cx_asn1_parser.
    ENDTRY.
  ENDMETHOD.
  METHOD upload_pubsub_cert_ssl.
    DATA : bincert      TYPE xstring,
           lv_filename  TYPE string,
           lo_abap_pse  TYPE REF TO cl_abap_pse,
           lox_abap_pse TYPE REF TO cx_abap_pse.

    CONCATENATE me->selected_folder me->separator c_pub_sub INTO lv_filename.
    TRANSLATE lv_filename TO LOWER CASE.
    TRY.
        CALL FUNCTION 'SSFC_CERTIFICATE_IMPORT'
          EXPORTING
            filename                   = lv_filename
            encoding                   = '?'
          IMPORTING
            certificate                = bincert
*           SUBJECT                    =
*           ISSUER                     =
*           SERIALNO                   =
*           VALIDFROM                  =
*           VALIDTO                    =
*           ALGID                      =
*           FINGERPRINT                =
          EXCEPTIONS
            sapgui_required            = 1
            no_upload_authorization    = 2
            data_transmission_error    = 3
            file_open_error            = 4
            file_access_denied         = 5
            file_read_error            = 6
            unknown_error              = 7
            import_parameter_missing   = 8
            encoding_not_supported     = 9
            invalid_certificate_format = 10
            OTHERS                     = 11.

        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  INTO DATA(message) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        DATA : refcert TYPE cert_struct,
               rc      TYPE i.

        PERFORM  parse_cert_struct  IN PROGRAM   s_trustmanager
           USING    bincert
           CHANGING refcert
                    rc   .

        CREATE OBJECT lo_abap_pse
          EXPORTING
            iv_context      = 'SSLC'
            iv_application  = 'ANONYM'
            iv_load_from_db = abap_true.

        CALL METHOD lo_abap_pse->add_trusted_certificate
          EXPORTING
            iv_certificate = refcert-bindata
            iv_add_to_cab  = abap_false.

        lo_abap_pse->save( iv_context  = 'SSLC' iv_application = 'ANONYM').
      CATCH cx_abap_pse INTO lox_abap_pse .
    ENDTRY.
  ENDMETHOD.
  METHOD  create_log_object.

    DATA : ls_balobj  TYPE balobj.

    SELECT SINGLE * FROM  balobj INTO ls_balobj WHERE object = c_object.

    IF sy-subrc NE 0.
      ls_balobj-object = c_object.
      lock_table( 'BALOBJ' ).
      INSERT balobj  FROM ls_balobj.
      IF sy-subrc = 0.
        COMMIT WORK.
        WAIT UP TO 2 SECONDS.
        unlock_table( 'BALOBJ' ).
        SET PARAMETER ID 'BALOBJ'  FIELD c_sub_object.

        CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
          EXPORTING
            action                       = 'U'
            view_name                    = 'V_BALSUB'
          EXCEPTIONS
            client_reference             = 1 ##NUMBER_OK
            foreign_lock                 = 2 ##NUMBER_OK
            invalid_action               = 3 ##NUMBER_OK
            no_clientindependent_auth    = 4 ##NUMBER_OK
            no_database_function         = 5 ##NUMBER_OK
            no_editor_function           = 6 ##NUMBER_OK
            no_show_auth                 = 7 ##NUMBER_OK
            no_tvdir_entry               = 8 ##NUMBER_OK
            no_upd_auth                  = 9 ##NUMBER_OK
            only_show_allowed            = 10 ##NUMBER_OK
            system_failure               = 11 ##NUMBER_OK
            unknown_field_in_dba_sellist = 12 ##NUMBER_OK
            view_not_found               = 13 ##NUMBER_OK
            maintenance_prohibited       = 14 ##NUMBER_OK
            OTHERS                       = 15 ##NUMBER_OK.
        IF sy-subrc <> 0.
          MESSAGE  TEXT-e02 TYPE 'E' .
        ENDIF.

      ELSE.
        MESSAGE  TEXT-015 TYPE 'E'."'Error Creating Log Object'
      ENDIF.
    ELSE.
      MESSAGE TEXT-014 TYPE  'S'."'Log object ZCPS_LOG already exists in SLG0'
    ENDIF.
  ENDMETHOD.
  METHOD schedule_jobs.
    CALL FUNCTION 'BP_JOBVARIANT_SCHEDULE'
      EXPORTING
        title_name     = 'Schedule Background Job' " Displayed as title of of scheduling screens
        job_name       = 'Shipagile_job' " Name of background processing job
        prog_name      = c_program_name " Name of ABAP " report that is to be run -- used also to select variants
      EXCEPTIONS
        no_such_report = 01. " PROG_NAME program  not found.
  ENDMETHOD.
  METHOD selection_screen_output.
    LOOP AT SCREEN.
      IF screen-group1 = 'P1'.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.
  METHOD show_text_editor.


    IF  me->lo_dock IS NOT BOUND.

      CREATE OBJECT lo_dock
        EXPORTING
*         parent                      = parent
          repid                       = sy-repid
          dynnr                       = sy-dynnr
          side                        = cl_gui_docking_container=>dock_at_right
          extension                   = 500
*         style                       = style
*         lifetime                    = lifetime_default
          caption                     = 'Error'
*         metric                      = 0
          ratio                       = 50
*         no_autodef_progid_dynnr     = no_autodef_progid_dynnr
*         name                        = name
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


    ENDIF.

    IF lo_text_edit IS NOT BOUND.

      CREATE OBJECT lo_text_edit
        EXPORTING
          wordwrap_mode     = 2 " 0: OFF; 1: wrap a window border; 2: wrap at fixed pos
          wordwrap_position = 254   " pos of wordwrap, only makes sense with wordwrap_mode=2
          parent            = me->lo_dock.     "Parent Container
    ENDIF.

    lo_text_edit->set_readonly_mode( 1 ).

    APPEND 'Error will be displayed here !' TO lt_text.

    CALL METHOD lo_text_edit->set_text_as_r3table
      EXPORTING
        table = lt_text.   " text as R/3 tabl


  ENDMETHOD.
  METHOD set_error.
    APPEND error TO me->lt_text.
    CALL METHOD me->lo_text_edit->set_text_as_r3table
      EXPORTING
        table = me->lt_text.   " text as R/3 tabl
  ENDMETHOD.
ENDCLASS.
