*&---------------------------------------------------------------------*
*&  Include           ZTEST_GCP_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_handler DEFINITION DEFERRED.
CLASS  lcl_driver DEFINITION FINAL .
  PUBLIC SECTION.

    TYPES  : BEGIN OF ty_url ##NEEDED,
               checkbox TYPE flag  ##NEEDED,
               endpoint TYPE zendpoint_name,
               url      TYPE zurl ##NEEDED,
             END OF ty_url  ##NEEDED,
             BEGIN OF ty_ack ##NEEDED ,
               check      TYPE char1,
               srno       TYPE i,
               ack_id     TYPE string,
               message    TYPE char20,
               message_id TYPE char50,
               status     TYPE char4,
             END OF ty_ack  ##NEEDED.

    DATA: lt_url       TYPE  TABLE  OF ty_url ##NEEDED,
          lt_ack_alv   TYPE  TABLE  OF ty_ack  ##NEEDED,
          lo_alv       TYPE  REF TO cl_salv_table ##NEEDED,
          lo_alv2      TYPE  REF TO cl_salv_table ##NEEDED,
          lo_handler   TYPE  REF TO lcl_handler,
          lo_message   TYPE  REF TO zif_message,
          subscription TYPE string ##NEEDED,
          topic        TYPE string ##NEEDED.



    CONSTANTS: c_rad        TYPE char6  VALUE 'RAD',
               c_x          TYPE char1  VALUE 'X',
               c_b1         TYPE char2  VALUE 'B1',
               c_b2         TYPE char2  VALUE 'B2',
               c_b3         TYPE char2  VALUE 'B3',
               c_b4         TYPE char2  VALUE 'B4',
               c_b5         TYPE char2  VALUE 'B5',
               c_b6         TYPE char2  VALUE 'B6',
               c_b7         TYPE char2  VALUE 'B7',
               c_b8         TYPE char2  VALUE 'B8',
               c_b9         TYPE char2  VALUE 'B9',
               c_b10        TYPE char3  VALUE 'B10',
               c_b11        TYPE char3  VALUE 'B11',


               tcode1       TYPE char20 VALUE 'ZCPS_TOPIC_MAINT',
               tcode2       TYPE char20 VALUE 'ZCPS_SUBS_MAINT',
               tcode3       TYPE char20 VALUE 'ZCPS_LOG',
               tcode4       TYPE char20 VALUE 'ZCPS_UTIL',
               tcode6       TYPE char20 VALUE 'STRUST',
               tcode7       TYPE char20 VALUE 'ZJWT_PROFILE',
               tcode8       TYPE char20 VALUE 'ZCPS_ENDPOINT_GEN',
               c_object     TYPE balobj_d VALUE 'ZCPS_LOG',
               c_sub_object TYPE balobj_d VALUE 'ZCPS_LOG',
               c_disp       TYPE char4 VALUE 'DISP'.

    METHODS:
      constructor  IMPORTING screen TYPE sydynnr
                             repid  TYPE syrepid,
      set_parameterid IMPORTING parameter_id TYPE char20
                                im_value     TYPE flag,
      unlock_table IMPORTING table TYPE rstable-tabname,
      lock_table   IMPORTING table TYPE rstable-tabname,
      handle_ok_code IMPORTING ok_code TYPE sy-ucomm,
      call_screen ,
      initialization ,
      f4_help_profile RETURNING VALUE(profile)  TYPE zjwt_profile-profile_name,
      display_alv_url,
      get_topic,
      get_subs,
      create_message ,
      subscribe,
      publish,
      process_message,
      get_selections,
      execute_test,
      create_ssf_pse_node ,
      create_ssf_application_jwt_si,
      create_ssf_application ,
      create_log_object ,
      check_github_connection,
      run_git,
      get_endpoints.

  PRIVATE SECTION.
    DATA:
      screen      TYPE sy-dynnr,
      repid       TYPE sy-repid,
      lt_seltab   TYPE TABLE OF rsparams,
      wa_seltab   TYPE rsparams,
      lo_subs     TYPE  REF TO zcl_cps_subscription,
      lo_topic    TYPE  REF TO zcl_cps_topic,
      profile     TYPE string,
      lv_success  TYPE flag,
      lt_messages TYPE  zsubs_pull_msg_table.
ENDCLASS.
CLASS lcl_handler DEFINITION FINAL .
  PUBLIC SECTION.
    METHODS :
      constructor  IMPORTING screen    TYPE sydynnr
                             repid     TYPE syrepid
                             lo_driver TYPE REF TO lcl_driver,

      handle_checkbox         FOR EVENT link_click OF  cl_salv_events_table   IMPORTING row,
      handle_checkbox_for_ack FOR EVENT link_click OF  cl_salv_events_table   IMPORTING row column,
      handle_hot_spot         FOR EVENT link_click OF  cl_salv_events_table   IMPORTING row column,

      on_user_command          FOR EVENT added_function OF cl_salv_events      IMPORTING e_salv_function,
      set_messages             IMPORTING im_lt_messages TYPE  zsubs_pull_msg_table,
      get_selection,
      refresh IMPORTING ref_alv TYPE REF TO cl_salv_table,
      acknowledge .
  PRIVATE SECTION.

    DATA :
      lt_messages            TYPE zsubs_pull_msg_table,
      lt_message_to_ack      TYPE zsubs_pull_msg_table,
      lt_message_ack_success TYPE zsubs_pull_msg_table,
      lt_seltab              TYPE TABLE OF rsparams,
      wa_seltab              TYPE rsparams,
      screen                 TYPE sy-dynnr ##NEEDED,
      repid                  TYPE sy-repid,
      lo_driver              TYPE REF TO lcl_driver.

ENDCLASS.

CLASS lcl_driver IMPLEMENTATION.
  METHOD constructor.
    me->screen = screen.
    me->repid = repid.
    CREATE OBJECT lo_handler EXPORTING screen = me->screen repid = me->repid lo_driver = me .
    CREATE OBJECT lo_message TYPE zcl_cps_message.
  ENDMETHOD.
  METHOD call_screen.
    initialization( ).

*    CALL SCREEN 100.
  ENDMETHOD.
  METHOD initialization.
    PERFORM initialization.
    DATA  :  r_s TYPE flag .
    SET PARAMETER ID 'ZDISPLAY_RESPONSE' FIELD 'X'.
    GET PARAMETER ID 'ZRADIO_BUTTON' FIELD r_s.
    me->display_alv_url( ).
    IF r_s NE abap_true.
      get_topic( ).
    ELSE.
      get_subs( ).
    ENDIF.
  ENDMETHOD.
  METHOD get_topic.
    DATA : lt_topic TYPE STANDARD TABLE OF zcps_topic,
           wa_topic TYPE zcps_topic,
           wa_url   TYPE ty_url.

    SELECT * FROM zcps_topic  INTO TABLE lt_topic .    "# CI_NOWHERE

    LOOP AT lt_topic INTO wa_topic.
      wa_url-endpoint = wa_topic-topic_name.
      wa_url-url = wa_topic-url.
      APPEND wa_url TO lt_url.
    ENDLOOP.

    CLEAR :wa_url,lt_topic,wa_topic.

    lo_alv->refresh( ).
  ENDMETHOD.
  METHOD get_subs.
    DATA : lt_subs TYPE STANDARD TABLE OF zcps_subs,
           wa_subs TYPE zcps_subs,
           wa_url  TYPE ty_url.
    SELECT * FROM zcps_subs  INTO TABLE lt_subs.        "#EC CI_NOWHERE

    LOOP AT lt_subs INTO wa_subs.
      wa_url-endpoint = wa_subs-sub_name.
      wa_url-url = wa_subs-url.
      APPEND wa_url TO lt_url.
    ENDLOOP.

    CLEAR :wa_url,lt_subs,wa_subs.
    lo_alv->refresh( ).
  ENDMETHOD.
  METHOD  handle_ok_code.
    CASE ok_code.
      WHEN  c_b1."ZCPS_TOPIC_MAINT
        CALL TRANSACTION tcode1.                         "#EC CI_CALLTA
      WHEN c_b2."ZCPS_SUBS_MAINT
        CALL TRANSACTION tcode2.                         "#EC CI_CALLTA
      WHEN c_b3."ZCPS_LOG
        CALL TRANSACTION tcode3.                         "#EC CI_CALLTA
      WHEN c_b4."util
        CALL TRANSACTION tcode4.                         "#EC CI_CALLTA
      WHEN c_b6."strust
        CALL TRANSACTION tcode6.                         "#EC CI_CALLTA
      WHEN c_b7."maint jwt
        CALL TRANSACTION tcode7.                         "#EC CI_CALLTA
      WHEN c_b11."ZCPS_ENDPOINT_GEN
        CALL TRANSACTION tcode8.                         "#EC CI_CALLTA
      WHEN c_b5."ssl
        create_ssf_application( ) .                      "#EC CI_CALLTA
      WHEN c_b8."test  git
        check_github_connection( ).
      WHEN c_b9."run git
        run_git( ).
      WHEN c_b10."create log object slg01
        create_log_object( ) .
      WHEN c_rad.
        get_endpoints( ).
      WHEN c_disp.
        IF  p_chk EQ abap_true.
          SET PARAMETER ID  'ZDISPLAY_RESPONSE' FIELD  abap_true.
        ELSE.
          SET PARAMETER ID  'ZDISPLAY_RESPONSE' FIELD  abap_false.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD execute_test.
    get_selections( ).
    READ TABLE lt_seltab INTO wa_seltab WITH  KEY selname = 'R_P'.
    IF wa_seltab-low EQ abap_true.
      set_parameterid( parameter_id = 'ZRADIO_BUTTON'  im_value = ' ' ).
      publish( ) .
    ELSE.
      set_parameterid( parameter_id = 'ZRADIO_BUTTON'  im_value = 'X' ).
      subscribe( ).
    ENDIF.
  ENDMETHOD.
  METHOD f4_help_profile.
    DATA :
      lt_return TYPE STANDARD TABLE OF ddshretval,
      wa_return LIKE LINE OF lt_return,
      BEGIN OF wa_profile,
        profile_name TYPE zjwt_profile-profile_name,
      END OF  wa_profile,
      lt_profile LIKE STANDARD TABLE OF wa_profile.

    SELECT profile_name FROM zjwt_profile INTO TABLE lt_profile. "#EC CI_NOWHERE

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = TEXT-013  "'PROFILE_NAME'
        window_title    = TEXT-012  "'Profiles Stored in System'
        value_org       = 'S'
      TABLES
        value_tab       = lt_profile
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc NE 0.
      MESSAGE TEXT-019 TYPE  'E'.
    ENDIF.

    READ TABLE lt_return INTO wa_return INDEX 1.

    profile = wa_return-fieldval.
  ENDMETHOD.
  METHOD set_parameterid.
    SET PARAMETER ID parameter_id  FIELD im_value.
  ENDMETHOD.
  METHOD display_alv_url.

    DATA : lo_cx_salv_msg       TYPE  REF TO cx_salv_msg,
           lo_cx_salv_not_found TYPE  REF TO cx_salv_not_found,
           lo_functions         TYPE  REF TO cl_salv_functions_list,
           docking_at_bottom    TYPE  REF TO cl_gui_docking_container,
           lo_columns           TYPE  REF TO cl_salv_columns_table,
           lo_column            TYPE  REF TO cl_salv_column_table,
           lo_display           TYPE  REF TO cl_salv_display_settings,
           lo_events            TYPE  REF TO cl_salv_events_table,
           lt_column_ref TYPE salv_t_column_ref,
           ls_column_ref TYPE salv_s_column_ref.


    CREATE OBJECT docking_at_bottom
      EXPORTING
        repid     = me->repid
        dynnr     = me->screen
        side      = docking_at_bottom->dock_at_bottom
        extension = 40
        ratio     = 60 ##NUMBER_OK.

*    docking_at_bottom->set_height( 50 ).

    TRY.
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            r_container  = docking_at_bottom
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_url.
      CATCH cx_salv_msg INTO lo_cx_salv_msg.
        WRITE lo_cx_salv_msg->get_text( ).
    ENDTRY.

*   all events
    lo_events = lo_alv->get_event( ).
*   event handler
    SET HANDLER  lo_handler->handle_checkbox   FOR lo_events.
    lo_columns = lo_alv->get_columns( ).
*    lo_columns->set_optimize('X').
    lo_functions  =  lo_alv->get_functions( ).
    lo_functions->set_all('X').


    TRY.
        lo_column ?= lo_columns->get_column( TEXT-tc1 )."Checkbox
        lo_column->set_short_text( TEXT-tc5  )."Select
        lo_column->set_long_text( TEXT-tc4 ).
        lo_column->set_medium_text( TEXT-tc4 ).
        lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
        lo_display = lo_alv->get_display_settings( ).
        lo_display->set_list_header( TEXT-tc3 )."'Select Endpoint to Test '
        lo_alv->display( ).
      CATCH cx_salv_not_found INTO lo_cx_salv_not_found .
        WRITE lo_cx_salv_not_found->get_text( ).
    ENDTRY.
    lt_column_ref = lo_columns->get( ).
    LOOP AT lt_column_ref INTO ls_column_ref.
      lo_column ?= ls_column_ref-r_column.
      CASE ls_column_ref-columnname.
        WHEN 'ENDPOINT' or 'URL'.
          lo_column->set_lowercase(   value = if_salv_c_bool_sap=>true ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD unlock_table.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        tabname = table.
  ENDMETHOD.


  METHOD lock_table.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname        = table
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE  TEXT-e01 TYPE 'E'.
    ENDIF.
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

  METHOD create_message .
    DATA:lv_profile TYPE string.

    CREATE OBJECT lo_message TYPE zcl_cps_message.


    READ TABLE lt_seltab INTO wa_seltab WITH  KEY selname = 'P_PROF'.
    lv_profile  = wa_seltab-low .
    lo_message->set_attributes(  EXPORTING profile = lv_profile ).

    DATA : lt_text TYPE STANDARD TABLE OF tline.
    DATA : wa_text TYPE tline.
    DATA : lv_json TYPE string.


*   get message in josn and publish
    CALL FUNCTION 'TERM_CONTROL_EDIT'
      EXPORTING
        titel          = TEXT-016 "'Input JSON Message'
        langu          = sy-langu
      TABLES
        textlines      = lt_text[]
      EXCEPTIONS
        user_cancelled = 1
        OTHERS         = 2.

    IF sy-subrc EQ 0.
      LOOP AT lt_text INTO wa_text.
        CONCATENATE lv_json wa_text-tdline INTO lv_json.
      ENDLOOP.
    ENDIF.

    CONCATENATE '{' lv_json '}' INTO lv_json.

    CALL METHOD lo_message->wrap_message_into_container
      EXPORTING
        in_json = lv_json.

  ENDMETHOD.
  METHOD subscribe.

    CREATE OBJECT lo_subs.

    READ TABLE lt_seltab INTO wa_seltab WITH  KEY selname = 'P_PROF'.
    profile  = wa_seltab-low .


    CALL METHOD lo_subs->pull
      EXPORTING
        subscription = subscription
        profile      = profile
      RECEIVING
        lt_message   = lt_messages.

    process_message(  ).

    FREE:lo_subs,profile.
  ENDMETHOD.

  METHOD publish.
    DATA  :
      lv_profile TYPE string.

    CREATE OBJECT lo_topic.

    create_message( ).

    READ TABLE lt_seltab INTO wa_seltab WITH  KEY selname = 'P_PROF'.
    lv_profile  = wa_seltab-low .

    CALL METHOD lo_topic->publish
      EXPORTING
        lo_message = lo_message
        topic      = topic
        profile    = lv_profile.

    FREE : lo_message,profile.
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
      wa_message           TYPE zsubs_pull_msg_object.

    DESCRIBE TABLE lt_messages LINES lines.

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
      wa_ack-message_id = wa_message-message-message_id.
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
        lo_columns->set_optimize('X').
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
          lo_column ?= lo_columns->get_column( 'MESSAGE_ID' ) ##NO_TEXT.
        lo_column->set_long_text( TEXT-t12  ) ##NO_TEXT.
        lo_column->set_short_text( TEXT-t13  ) ##NO_TEXT.
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
*   event handler
    SET HANDLER  lo_handler->handle_checkbox_for_ack  FOR lo_events.
    lo_handler->set_messages( lt_messages ).
    SET HANDLER  lo_handler->on_user_command   FOR lo_events.
    SET HANDLER  lo_handler->handle_hot_spot   FOR lo_events.


    lo_alv2->set_screen_popup(
                  start_column = 1  ##NUMBER_OK
                  end_column   = 100  ##NUMBER_OK
                  start_line   = 1  ##NUMBER_OK
                  end_line     = 20 ##NUMBER_OK
                  ).

    lo_alv2->display( ).
  ENDMETHOD.
  METHOD  get_selections.

    CLEAR :  lt_seltab.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = repid
      TABLES
        selection_table = lt_seltab[]
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
  ENDMETHOD.
  METHOD create_ssf_pse_node  .
    DATA : ssf_ssfargs TYPE ssfargs,
           lv_profile  TYPE ssfpab.

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
      lv_success = c_x.
    ENDIF.
    unlock_table('SSFARGS').
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
      lv_success = abap_false.
      MESSAGE TEXT-020 TYPE'S'.  "SSF APP Exists'
    ENDIF.
  ENDMETHOD.
  METHOD create_ssf_application .
* Create new SSF Application
    DATA : lv_success      TYPE flag.
*create ssf profile
    create_ssf_application_jwt_si( ).
    IF lv_success = c_x.
      CLEAR  lv_success.
*create ssf pse node
      create_ssf_pse_node( ).
      IF lv_success  EQ  c_x.
        MESSAGE TEXT-021 TYPE 'S'.
      ELSE.
        MESSAGE TEXT-022 TYPE  'E'.
      ENDIF.
    ELSE.
      MESSAGE TEXT-023 TYPE  'E'.
    ENDIF.
  ENDMETHOD.
  METHOD check_github_connection.
    SUBMIT  zabapgit_test_ssl AND RETURN.
  ENDMETHOD.
  METHOD  run_git.
    SUBMIT zabapgit_standalone AND RETURN.
  ENDMETHOD.
  METHOD     get_endpoints.
    CLEAR: lt_seltab,wa_seltab,lt_url.
    get_selections( ).
    READ TABLE lt_seltab INTO wa_seltab WITH  KEY selname = 'R_P'.
    IF wa_seltab-low EQ abap_true.
      get_topic( ).
    ELSE.
      get_subs( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
CLASS lcl_handler  IMPLEMENTATION.

  METHOD constructor.
    me->screen =  screen.
    me->repid = repid.
    me->lo_driver =  lo_driver.
  ENDMETHOD.
  METHOD  get_selection.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = repid
      TABLES
        selection_table = lt_seltab[]
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD handle_checkbox.

    FIELD-SYMBOLS : <fs> TYPE  lo_driver->ty_url.

    get_selection( ).

    READ TABLE lo_driver->lt_url ASSIGNING <fs> INDEX row.
    IF <fs> IS ASSIGNED.
      IF  <fs>-checkbox   =  abap_true.
        <fs>-checkbox  =  abap_false.
        CLEAR : lo_driver->subscription ,
                lo_driver->topic.
      ELSE.
        <fs>-checkbox  =  abap_true.
        READ TABLE lt_seltab INTO wa_seltab WITH  KEY selname = 'R_P'.
        IF wa_seltab-low = abap_true.
          lo_driver->topic  = <fs>-endpoint.
        ELSE.
          lo_driver->subscription = 'def-A5j6M0YWpz6CB4K51w4a-experimental'."<fs>-endpoint.
        ENDIF.
      ENDIF.
    ENDIF.

    refresh(  lo_driver->lo_alv ).

  ENDMETHOD.
  METHOD handle_checkbox_for_ack .
    FIELD-SYMBOLS:  <fs_ack_alv>  TYPE lo_driver->ty_ack.
    FIELD-SYMBOLS:  <fs_ack_message> LIKE LINE OF lt_message_to_ack.

    IF column EQ 'CHECK'.
      READ TABLE  lo_driver->lt_ack_alv  ASSIGNING <fs_ack_alv> INDEX row .
      IF <fs_ack_alv> IS ASSIGNED.
        IF  <fs_ack_alv>-check  =  abap_true.
          <fs_ack_alv>-check  =  abap_false.
        ELSE.
          <fs_ack_alv>-check  =  abap_true.
          READ TABLE lt_messages ASSIGNING <fs_ack_message> WITH KEY ack_id = <fs_ack_alv>-ack_id  BINARY SEARCH.
          IF  <fs_ack_message> IS ASSIGNED.
            APPEND <fs_ack_message> TO lt_message_to_ack.
          ENDIF.
        ENDIF.
      ENDIF.
      refresh( me->lo_driver->lo_alv2 ).
    ENDIF.

  ENDMETHOD.
  METHOD acknowledge.
    DATA : profile        TYPE string.

    FIELD-SYMBOLS:  <fs_ack_alv> TYPE lo_driver->ty_ack.
    FIELD-SYMBOLS:  <fs_ack_message> LIKE LINE OF lt_message_to_ack.

    lo_driver->lo_message->set_queue_of_pulled_messages( lt_message_to_ack ).

    READ TABLE lt_seltab INTO wa_seltab WITH  KEY selname = 'P_PROF'.
    profile  = wa_seltab-low .

    lo_driver->lo_message->acknowledge(  EXPORTING subscription = lo_driver->subscription profile  = profile ).
    lt_message_ack_success = lo_driver->lo_message->get_acknowledged_message_list( ).

    LOOP AT lo_driver->lt_ack_alv ASSIGNING <fs_ack_alv>.
      READ TABLE lt_message_ack_success ASSIGNING <fs_ack_message> WITH KEY ack_id = <fs_ack_alv>-ack_id  BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_ack_alv>-status  = icon_green_light.
      ENDIF.
    ENDLOOP.

    refresh( me->lo_driver->lo_alv2 ).

  ENDMETHOD.
  METHOD set_messages.
    lt_messages =  im_lt_messages.
    SORT lt_messages BY ack_id.
  ENDMETHOD.
  METHOD on_user_command.
    CASE e_salv_function.
**When Acknowledge Button is selected
      WHEN '&ACK'.
        acknowledge( ).
      WHEN  '&CANCEL'.
        lo_driver->call_screen( ).
    ENDCASE.
  ENDMETHOD.
  METHOD handle_hot_spot .

    FIELD-SYMBOLS:  <fs_ack_alv> TYPE lo_driver->ty_ack.
    FIELD-SYMBOLS:  <fs_message> LIKE LINE OF lt_messages.

    DATA :  lv_data TYPE string.
    IF column EQ  'MESSAGE'.
      READ TABLE lo_driver->lt_ack_alv  ASSIGNING <fs_ack_alv> INDEX row.
      IF <fs_ack_alv> IS ASSIGNED.
        READ TABLE lt_messages   ASSIGNING <fs_message> WITH KEY ack_id = <fs_ack_alv>-ack_id.
        IF sy-subrc = 0.
          lv_data = <fs_message>-message-data.
          cl_demo_output=>display_json( lo_driver->lo_message->decode_base64( lv_data ) ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD refresh.
    ref_alv->refresh( ).
  ENDMETHOD.

ENDCLASS.

FORM initialization .
  but1  = TEXT-b01.
  but2  = TEXT-b02.
  but3  = TEXT-b03.
*  but5  = TEXT-b05.
*  but4  = TEXT-b04.
*  but6  = TEXT-b06.
*  but7  = TEXT-b07.
*  but8  = TEXT-b08.
*  but9  = TEXT-b09.
*  but10 = TEXT-b10.
*  but11 = TEXT-b11.
ENDFORM.
