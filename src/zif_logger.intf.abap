interface ZIF_LOGGER
  public .


  data LOG_OBJECT type BALOBJ_D .
  data DEFAULT_SUBOBJECT type BALSUBOBJ .
  data DEBUG_SUBOBJECT type BALSUBOBJ .
  data REORG_IN_DAYS type ALCCMPARAM .
  constants C_PROBCLASS_LOW type BAL_S_MSG-PROBCLASS value '4' ##NO_TEXT.
  constants C_PROBCLASS_MEDIUM type BAL_S_MSG-PROBCLASS value '3' ##NO_TEXT.
  constants C_PROBCLASS_HIGH type BAL_S_MSG-PROBCLASS value '2' ##NO_TEXT.
  constants C_PROBCLASS_VHIGH type BAL_S_MSG-PROBCLASS value '1' ##NO_TEXT.
  constants C_PROBCLASS_NONE type BAL_S_MSG-PROBCLASS value SPACE ##NO_TEXT.

  methods ADD_EXCEPTION
    importing
      !I_EXCEPTION type ref to CX_ROOT
    raising
      CX_BAL_EXCEPTION .
  methods ADD_MSG
    importing
      !MESSAGE type BAL_S_MSG
    raising
      CX_BAL_EXCEPTION .
  methods ADD_ERRORTEXT
    importing
      !I_ERRORTEXT type STRING
    raising
      CX_BAL_EXCEPTION .
  methods ADD_STATUSTEXT
    importing
      !I_STATUSTEXT type STRING
    raising
      CX_BAL_EXCEPTION .
  methods REFRESH
    raising
      CX_BAL_EXCEPTION .
  methods SAVE
    importing
      !I_CLIENT type SY-MANDT optional
      !I_IN_UPDATE_TASK type BOOLEAN optional
      !I_SAVE_ALL type BOOLEAN default 'X'
      !I_2TH_CONNECTION type BOOLEAN optional
      !I_2TH_CONNECT_COMMIT type BOOLEAN optional
      !I_LINK2JOB type BOOLEAN default 'X'
    exporting
      !ET_LOGNUMBERS type BAL_T_LGNM
    raising
      CX_BAL_EXCEPTION .
  methods DISPLAY
    importing
      !I_DISPLAY_TYPE type C default 'P'
      !I_DISP_PROFILE type BAL_S_PROF optional .
  methods GET_HANDLE
    returning
      value(R_HANDLE) type BALLOGHNDL .
  methods UPDATE_PROGRESS
    importing
      !I_PERCENTAGE type I .
  methods STRING_TO_MSGV
    importing
      !I_TEXT type STRING
    exporting
      !E_MSGV1 type SYMSGV
      !E_MSGV2 type SYMSGV
      !E_MSGV3 type SYMSGV
      !E_MSGV4 type SYMSGV .
  methods LOG_MESSAGE_DETAILS
    importing
      !DOCUMENT type ZDOCUMENT_HEADER
      !MESSAGE_ID type STRING .
endinterface.
