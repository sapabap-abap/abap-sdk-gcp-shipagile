class ZCL_CPS_LOGGER definition
  public
  final
  create public .

public section.

  interfaces ZIF_LOGGER
      data values LOG_OBJECT = 'ZCPS_LOG'
                  DEFAULT_SUBOBJECT = 'ZCPS_LOG' .

  aliases ADD_ERRORTEXT
    for ZIF_LOGGER~ADD_ERRORTEXT .
  aliases ADD_EXCEPTION
    for ZIF_LOGGER~ADD_EXCEPTION .
  aliases ADD_MSG
    for ZIF_LOGGER~ADD_MSG .
  aliases ADD_STATUSTEXT
    for ZIF_LOGGER~ADD_STATUSTEXT .
  aliases LOG_MESSAGE_DETAILS
    for ZIF_LOGGER~LOG_MESSAGE_DETAILS .
  aliases SAVE
    for ZIF_LOGGER~SAVE .
  aliases UPDATE_PROGRESS
    for ZIF_LOGGER~UPDATE_PROGRESS .

  data DEBUG type BOOLEAN .
  data MESSAGE_COUNTER type I read-only .
  data GUI_UPDATE type ABAP_BOOL .
  data SUMMARY_COUNTER type I read-only .
  data DETLEVEL type BAL_S_MSG-DETLEVEL read-only .

  events MAX_MEMORY_LIMIT_REACHED
    exporting
      value(E_NUMBER_OF_MESSAGES) type I optional .

  methods CONSTRUCTOR
    importing
      !I_LOG_OBJECT type BALOBJ_D default 'ZCPS_LOG'
      !I_DEFAULT_SUBOBJECT type BALSUBOBJ default 'ZCPS_LOG'
      !I_DEBUG_SUBOBJECT type BALSUBOBJ optional
      !I_REORG_IN_DAYS type ALCCMPARAM default '3'
      !I_EXTNUMBER type BALNREXT optional
      !I_LOGHANDLE type BALLOGHNDL optional
      !I_MAX_MSG_MEMORY type I default 100
    raising
      CX_BAL_EXCEPTION .
protected section.
private section.

  aliases STRING_TO_MSGV
    for ZIF_LOGGER~STRING_TO_MSGV .

  data M_HANDLE type BALLOGHNDL .
  data M_BATCH_MODE type ABAP_BOOL .
  data M_MAX_MESSAGE_MEMORY type I value 100 ##NO_TEXT.
  data M_PROGRESS_PERCENTAGE type I .
  data M_S_LOG type BAL_S_LOG .
ENDCLASS.



CLASS ZCL_CPS_LOGGER IMPLEMENTATION.


  method CONSTRUCTOR.
   data: lv_days type int1.

    IF i_loghandle IS SUPPLIED.
      m_handle = i_loghandle.
    ELSE.

   IF i_extnumber IS not INITIAL.
       m_s_log-extnumber = i_extnumber.
   ENDIF.
    m_s_log-object    = i_log_object.
    m_s_log-subobject = i_default_subobject.
    m_s_log-aluser    = sy-uname.
    m_s_log-alprog    = sy-repid.
    m_s_log-altcode   = sy-tcode.
      m_max_message_memory = i_max_msg_memory.

*   Delete logs after the days customized in ALCCMCUST
    lv_days = i_reorg_in_days.
    m_s_log-aldate_del = sy-datum + lv_days.
    detlevel = 1.

*   Get the log handle.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = m_s_log
      IMPORTING
        e_log_handle            = m_handle
      EXCEPTIONS
          OTHERS       = 1.
    IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_bal_exception
          MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
    IF sy-batch IS NOT INITIAL.
      m_batch_mode = abap_true.
    endif.

endmethod.


  method zif_logger~ADD_ERRORTEXT.
    DATA: msg1(50)      TYPE c,
        msg2(50)      TYPE c,
        msg3(50)      TYPE c,
        msg4(50)      TYPE c,
        /dev/null     TYPE string ##NEEDED,
        lv_gui_active TYPE c.

  string_to_msgv( EXPORTING i_text = i_errortext
                  IMPORTING e_msgv1 = msg1
                            e_msgv2 = msg2
                            e_msgv3 = msg3
                            e_msgv4 = msg4 ).

  IF gui_update = abap_true.
    CALL FUNCTION 'RFC_IS_GUI_ON'
      EXPORTING
        login_check = ' '
      IMPORTING
        on          = lv_gui_active.

     IF lv_gui_active = 'Y'.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = m_progress_percentage
          text       = i_errortext.
    ENDIF.
  ENDIF.
  MESSAGE e899(smoiws) WITH msg1 msg2 msg3 msg4 INTO /dev/null.

  endmethod.


  method zif_logger~ADD_EXCEPTION.
    DATA: lv_text       TYPE string,
          ls_exc        TYPE bal_s_exc,
          lv_gui_active TYPE c.

  ASSERT ID smoiws CONDITION NOT i_exception IS INITIAL.

  IF gui_update = abap_true.
*   double check in case we are running in async
    CALL FUNCTION 'RFC_IS_GUI_ON'
      EXPORTING
        login_check = ' '
      IMPORTING
        on          = lv_gui_active.

    IF lv_gui_active = 'Y'.

      lv_text = i_exception->get_text( ).
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = m_progress_percentage
          text       = lv_text.
    ENDIF.

  ENDIF.
  IF m_batch_mode = abap_true.
    lv_text = i_exception->get_text( ).
    WRITE: lv_text.
    NEW-LINE.
  ENDIF.

  ls_exc-exception = i_exception.
  ls_exc-msg_count = message_counter.
  ls_exc-msgty = 'E'.
  ls_exc-probclass = '4'.

  CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
    EXPORTING
      i_log_handle     = m_handle
      i_s_exc          = ls_exc
*   IMPORTING
*     E_S_MSG_HANDLE   =
*     E_MSG_WAS_LOGGED =
*     E_MSG_WAS_DISPLAYED       =
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc = 0.
    ADD 1 TO summary_counter.
    ADD 1 TO message_counter.
    IF message_counter >= m_max_message_memory.
*   Store log to DB now.
      save( ).
      CLEAR message_counter.
    ENDIF.
  Else.
    RAISE EXCEPTION TYPE cx_bal_exception
          MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  endif.

  endmethod.


  METHOD zif_logger~add_msg.
    DATA: l_s_msg       TYPE bal_s_msg,
          /dev/null     TYPE string,
          lv_gui_active TYPE c.


* define data of message for Application Log
    l_s_msg  = message.

    MOVE-CORRESPONDING l_s_msg to sy.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO /dev/null.


    IF gui_update = abap_true.
*   double check in case we are running in async
      CALL FUNCTION 'RFC_IS_GUI_ON'
        EXPORTING
          login_check = ' '
        IMPORTING
          on          = lv_gui_active.

      IF lv_gui_active = 'Y'.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = m_progress_percentage
            text       = /dev/null.
      ENDIF.

    ENDIF.

    IF m_batch_mode = abap_true.
      WRITE: /dev/null.
      NEW-LINE.
    ENDIF.

* ... see structure BAL_S_LOG or report SBAL_DEMO_02 for ...
* ... further data which can be added to a message       ...

* add this message to log file
* we do not specify I_LOG_HANDLE since we want to add this message
* to the default log. If it does not exist we do not care
* (EXCEPTIONS log_not_found = 0).
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg      = l_s_msg
        i_log_handle = m_handle
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_bal_exception
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    ADD 1 TO summary_counter.
    ADD 1 TO message_counter.
    IF message_counter >= m_max_message_memory.
*  Raise event the number was reached
      RAISE EVENT max_memory_limit_reached EXPORTING e_number_of_messages = message_counter.
      CLEAR message_counter.
    ENDIF.
  ENDMETHOD.


  method zif_logger~ADD_STATUSTEXT.
      DATA: msg1(50)      TYPE c,
        msg2(50)      TYPE c,
        msg3(50)      TYPE c,
        msg4(50)      TYPE c,
        /dev/null     TYPE string ##NEEDED,
        lv_gui_active TYPE c.


   string_to_msgv( EXPORTING i_text = i_statustext
                  IMPORTING e_msgv1 = msg1
                            e_msgv2 = msg2
                            e_msgv3 = msg3
                            e_msgv4 = msg4 ).

  IF gui_update = abap_true.
    CALL FUNCTION 'RFC_IS_GUI_ON'
      EXPORTING
        login_check = ' '
      IMPORTING
        on          = lv_gui_active.

    IF lv_gui_active = 'Y'.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = m_progress_percentage
          text       = i_statustext.
    ENDIF.
  ENDIF.

  MESSAGE s899(smoiws) WITH msg1 msg2 msg3 msg4 INTO /dev/null.

  endmethod.


  method zif_logger~DISPLAY.
    DATA: l_handle_tab    TYPE bal_t_logh,
        ls_profile type bal_s_prof,
        lv_display_type type c.

  append m_handle to l_handle_tab.

  if i_display_type CS 'PS'.
    lv_display_type = 'P'.
  else.
    lv_display_type = i_display_type.
  endif.

  case lv_display_type. "display as popup
    when 'P'.

      IF i_disp_profile IS INITIAL.
        CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
          IMPORTING
            e_s_display_profile = ls_profile.

        ls_profile-end_col    = 130.
        ls_profile-pop_adjst  = 'X'.
        ls_profile-use_grid   = abap_true.
        ls_profile-cwidth_opt = abap_true.
        ls_profile-tree_ontop = abap_false.
      ELSE.
        ls_profile = i_disp_profile.
      ENDIF.

*  get a prepared profile
      call function 'BAL_DSP_LOG_DISPLAY'
        exporting
          i_t_log_handle      = l_handle_tab
          i_s_display_profile = ls_profile
        exceptions
          others              = 1.

      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    when 'S'. "Display in subscreen area
*
      call function 'BAL_DSP_OUTPUT_SET_DATA'
        exporting
          i_t_log_handle = l_handle_tab
        exceptions
          others         = 1.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
  endcase.
  endmethod.


  method zif_logger~GET_HANDLE.
    r_handle = m_handle.
  endmethod.


  METHOD zif_logger~log_message_details.
    DATA:lv_log_details TYPE zshipagile_log.

    lv_log_details-document = document-document.
    lv_log_details-document_type  = document-type.
    lv_log_details-message_id   = message_id.

    IF lv_log_details-message_id IS INITIAL.
      lv_log_details-status = icon_red_light.
    ELSE.
      lv_log_details-status = icon_green_light.
    ENDIF.

    lv_log_details-mandt  = sy-mandt.
    lv_log_details-time  = sy-uzeit.
    lv_log_details-user_name  = sy-uname.
    lv_log_details-zdate = sy-datum.
    TRY.
        lv_log_details-guid = cl_uuid_factory=>create_system_uuid( )->create_uuid_c32( ).
        INSERT  zshipagile_log  FROM lv_log_details.
        IF sy-subrc = 0.
          COMMIT WORK.
          CLEAR :lv_log_details.
        ELSE.

        ENDIF.
      CATCH   cx_uuid_error.

    ENDTRY.
  ENDMETHOD.


  method zif_logger~REFRESH.

  CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
    EXPORTING
      i_log_handle  = m_handle
    EXCEPTIONS
       OTHERS        = 1.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_bal_exception
          MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  detlevel = 1.
  endmethod.


  METHOD zif_logger~save.
*  DATA: appinfo(50) TYPE c VALUE 'Method SAVE_TO_DB; Count=',"#EC NOTEXT
*        text(12)    TYPE c.

    IF m_handle IS NOT INITIAL.
*    WRITE summary_counter TO text LEFT-JUSTIFIED.
*    CONCATENATE appinfo text INTO appinfo.

*   Save logs to DB
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_client             = sy-mandt
          i_in_update_task     = i_in_update_task
          i_save_all           = 'X'
          i_2th_connection     = i_2th_connection
          i_2th_connect_commit = i_2th_connect_commit
          i_link2job           = i_link2job
        IMPORTING
          e_new_lognumbers     = et_lognumbers
        EXCEPTIONS
          OTHERS               = 1.
      COMMIT WORK.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_bal_exception
          MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  method zif_logger~STRING_TO_MSGV.
     DATA: lv_offset     TYPE i,
          lv_temp       TYPE string,
          lv_len        TYPE i,
          lv_varnam     TYPE string.

          field-symbols:<fs_msg> type c.

    lv_offset = 0.
    lv_len = strlen( i_text ).

    WHILE lv_len > 0 AND sy-index <= 4 .
      lv_temp = sy-index.
      CONCATENATE 'E_MSGV' lv_temp INTO lv_varnam.
      ASSIGN (lv_varnam) TO <fs_msg>.
      IF lv_len >= 50.
        <fs_msg> = i_text+lv_offset(50).
      ELSE.
        <fs_msg> = i_text+lv_offset(lv_len).
      ENDIF.
      lv_offset = lv_offset + 50.
      lv_len = lv_len - 50.
    ENDWHILE.
  endmethod.


  method zif_logger~UPDATE_PROGRESS.
*  avoid that no progress clock is displayed
    CASE i_percentage.
      WHEN 0.
        m_progress_percentage = 1.
      WHEN 100.
        m_progress_percentage = 99.
      WHEN OTHERS.
        m_progress_percentage = i_percentage.
    ENDCASE.

  endmethod.
ENDCLASS.
