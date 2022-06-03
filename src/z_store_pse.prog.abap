*&---------------------------------------------------------------------*
*& Report  SIPT_SAFT_STORE_PSE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_store_pse  MESSAGE-ID sipt.

PARAMETERS: file         TYPE string  LOWER CASE OBLIGATORY.
PARAMETERS: p_prof    TYPE char30 obligatory.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.
  PERFORM file_f4.

START-OF-SELECTION.

  DATA:
    gv_print_char  TYPE sipt_print_char,
    gv_sesam       TYPE ssfpabpw VALUE '##SAFTPortugal##',
    gv_subject     TYPE ssfid,
    gv_psename     TYPE ssfpsename,
    gs_sipt_cert   TYPE sipt_cert,
    gv_client_role TYPE cccategory,
    gv_serial_no   TYPE char40,
    pseleng        TYPE i,
    psedata        TYPE TABLE OF ssfbin,
    psefile        TYPE localfile,
    psetemp        TYPE localfile,
    guid_32        TYPE guid_32.


  DATA:
    g_log_handle TYPE balloghndl,
    gv_message   TYPE c.

  CONSTANTS:
    gc_test_serial_no(40) TYPE c VALUE '20101118125703',
    gc_test_print_char    TYPE sipt_ref_print_char VALUE 'r4sJ'.
*
*  AUTHORITY-CHECK OBJECT 'S_RZL_ADM'
*     ID 'ACTVT' FIELD '01'.

*  IF sy-subrc NE 0.
*    MESSAGE e108. "You are not authorized to run this program
*  ENDIF.


* upload PSE from frontend
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = file
      filetype                = 'BIN'
*     HAS_FIELD_SEPARATOR     = ' '
*     HEADER_LENGTH           = 0
*     READ_BY_LINE            = 'X'
*     DAT_MODE                = ' '
*     CODEPAGE                = ' '
*     IGNORE_CERR             = ABAP_TRUE
*     REPLACEMENT             = '#'
*     CHECK_BOM               = ' '
*     VIRUS_SCAN_PROFILE      =
*     NO_AUTH_CHECK           = ' '
    IMPORTING
      filelength              = pseleng
*     HEADER                  =
    TABLES
      data_tab                = psedata
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
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO gv_message.
    PERFORM add_message.
    PERFORM show_log.
    RETURN.
  ENDIF.

* create temporary PSE name
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
*     EV_GUID_16 =
*     EV_GUID_22 =
      ev_guid_32 = guid_32.

  CONCATENATE 'TEMP_' guid_32 '.pse' INTO psefile.
  PERFORM get_pse_localpath
    USING    psefile
    CHANGING psetemp.

* load PSE to temporary PSE file
  CALL FUNCTION 'SSFPSE_UPLOAD'
    EXPORTING
      fname             = psetemp
      pselen            = pseleng
    TABLES
      pse               = psedata
    EXCEPTIONS
      file_open_failed  = 1
      file_write_failed = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO gv_message.
    PERFORM add_message.
    PERFORM show_log.
    RETURN.
  ENDIF.


  CALL FUNCTION 'SIPT_CERT_CHECK'
    EXPORTING
      iv_psename     = psefile
    IMPORTING
      ev_client_role = gv_client_role
      ev_serial_no   = gv_serial_no
      ev_subject     = gv_subject
*     EV_PROFILE_PAB =
      es_sipt_cert   = gs_sipt_cert
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    DELETE DATASET psetemp.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO gv_message.
    PERFORM add_message.
    PERFORM show_log.
    RETURN.
  ENDIF.


  MESSAGE i109 WITH gv_subject gv_serial_no INTO gv_message.
  PERFORM add_message.

  IF gv_client_role = 'P'
    AND gv_serial_no = gc_test_serial_no.
    MESSAGE w105 WITH gv_serial_no gs_sipt_cert-serial_no
    INTO gv_message.
    PERFORM add_message.
  ENDIF.


* store PSE to database and distribute it
  gv_psename = gs_sipt_cert-psename.

  CALL FUNCTION 'SSFPSE_STORE'
    EXPORTING
      fname             = psetemp
      psepin            = gv_sesam
      psename           = gv_psename
      id                = gv_subject
*     HOST              = ' '
*     INSTANCEID        = '00'
*     TYPE              = 'PSE'
*     FORMAT            = 'RAW'
      b_newdn           = 'X'
*     B_CLEANUP         = 'X'
      b_distribute      = 'X'
    EXCEPTIONS
      file_load_failed  = 1
      storing_failed    = 2
      authority_missing = 3
      OTHERS            = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO gv_message.
    PERFORM add_message.
    DELETE DATASET psetemp.
    PERFORM show_log.
    RETURN.
  ELSE.
    MESSAGE s112 WITH gs_sipt_cert-psename INTO gv_message.
    PERFORM add_message.
  ENDIF.

* Perform test signature
  MESSAGE i113 INTO gv_message.
  PERFORM add_message.

  CALL FUNCTION 'SIPT_SIGN'
    EXPORTING
      iv_inv_date       = 'A'
      iv_sy_date        = 'A'
      iv_inv_no         = 'A'
      iv_gross_total    = 'A'
      iv_prev_signature = 'A'
    IMPORTING
*     ev_signature      =
      ev_print_char     = gv_print_char
*     ev_cert_id        =
*     ev_key_vers       =
*     ev_serialno       =
*     ev_subject        =
    EXCEPTIONS
      error             = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO gv_message.
    PERFORM add_message.
    DELETE DATASET psetemp.
    PERFORM show_log.
    RETURN.
  ELSE.
    IF ( gv_serial_no = gs_sipt_cert-serial_no
      AND gv_print_char <>  gs_sipt_cert-ref_print_char )
      OR ( gv_serial_no = gc_test_serial_no
      AND gv_print_char <> gc_test_print_char ).
      MESSAGE e114 INTO gv_message.
      PERFORM add_message.
      DELETE DATASET psetemp.
      PERFORM show_log.
      RETURN.
    ENDIF.
  ENDIF.

  MESSAGE s111 INTO gv_message.
  PERFORM add_message.

  DELETE DATASET psetemp.
  PERFORM show_log.
  RETURN.


************************************************************************
* F4-Help for Filename
************************************************************************
FORM file_f4.

  DATA: lt_filetable TYPE filetable,
        lf_rc        TYPE i,
        ls_file      TYPE file_table.

* Call file selector at frontend
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      multiselection          = abap_false
    CHANGING
      file_table              = lt_filetable
      rc                      = lf_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
               DISPLAY LIKE 'E'
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

* Number of selected filed must be equal to one.
  CHECK lf_rc = 1.

* Access selected file
  READ TABLE lt_filetable INTO ls_file INDEX 1.
  CHECK sy-subrc = 0.

  file = ls_file-filename.

ENDFORM.                                                    "FILE_F4

*---------------------- GET PSE LOCAL FILEPATH ------------------------*
FORM get_pse_localpath
     USING    filename TYPE c
     CHANGING path     TYPE c.

  DATA: dir_instance(256) TYPE c,
        dir_sec(256)      TYPE c.

  PERFORM get_profile_param
      USING     p_prof
      CHANGING  dir_instance.

  PERFORM build_file_path
      USING    dir_instance 'sec'
      CHANGING dir_sec.

  PERFORM build_file_path
      USING    dir_sec filename
      CHANGING path.

ENDFORM.                    "GET_PSE_LOCALPATH

*------------------------ GET VALUE OF PROFILE PARAM ------------------*
FORM get_profile_param
     USING    name  TYPE c
     CHANGING value TYPE c.   "error: value initial

  CALL 'C_SAPGPARAM'
         ID 'NAME' FIELD name
         ID 'VALUE' FIELD value.                          "#EC CI_CCALL

ENDFORM.                    "GET_PROFILE_PARAM

*------------------------ BUILD UP FILE PATH --------------------------*
FORM build_file_path
     USING    path     TYPE c
              filename TYPE c
     CHANGING result   TYPE c.

  CALL 'BUILD_DS_SPEC' ID 'PATH'     FIELD path
                       ID 'FILENAME' FIELD filename
                       ID 'OPSYS'    FIELD sy-opsys
                       ID 'RESULT'   FIELD result.

ENDFORM.                    "BUILD_FILE_PATH

*&---------------------------------------------------------------------*
*&      Form  add_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_message.
  DATA ls_msg TYPE bal_s_msg.

  IF g_log_handle IS INITIAL.
    PERFORM create_log.
  ENDIF.

  ls_msg-msgty = sy-msgty.
  ls_msg-msgid = sy-msgid.
  ls_msg-msgno = sy-msgno.
  ls_msg-msgv1 = sy-msgv1.
  ls_msg-msgv2 = sy-msgv2.
  ls_msg-msgv3 = sy-msgv3.
  ls_msg-msgv4 = sy-msgv4.

  CASE sy-msgty.
    WHEN 'E'.
      ls_msg-probclass = '1'.
    WHEN 'W'.
      ls_msg-probclass = '2'.
    WHEN 'I'.
      ls_msg-probclass = '3'.
    WHEN OTHERS.
      ls_msg-probclass = '4'.
  ENDCASE.

* add message to log
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
*     i_log_handle     = g_log_handle
      i_s_msg          = ls_msg
*      IMPORTING
*     E_S_MSG_HANDLE   =
*     E_MSG_WAS_LOGGED =
*     E_MSG_WAS_DISPLAYED =
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "add_message


*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LOG_HANDLE  text
*----------------------------------------------------------------------*
FORM create_log.
  DATA: ls_log       TYPE bal_s_log.

  ls_log-extnumber       = 'SIPT_VBRK'.
  ls_log-alprog          = sy-cprog.
  ls_log-aluser          = sy-uname.
*  ls_log-object          = 'SIPT'.
*  ls_log-subobject       = 'VBRK'.

* Create log and get handle
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = g_log_handle
    EXCEPTIONS
      OTHERS       = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "create_log


*&---------------------------------------------------------------------*
*&      Form  show_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_log.
  DATA    gs_log_filter TYPE bal_s_lfil.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
*      EXPORTING
*           I_S_LOG_FILTER         =
*           I_T_LOG_CONTEXT_FILTER =
*           I_S_MSG_FILTER         =
*           I_T_MSG_CONTEXT_FILTER =
*           I_T_LOG_HANDLE         =
*           I_T_MSG_HANDLE         =
*           I_S_DISPLAY_PROFILE    =
*           I_AMODAL               = ' '
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "show_log
