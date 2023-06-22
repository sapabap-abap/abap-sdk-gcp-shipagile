*&---------------------------------------------------------------------*
*& Report ZCPS_DELIVERY_SEND
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCPS_DELIVERY_SEND.

SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  DATA:lv_vbeln Type likp-vbeln.
  DATA:lv_dats  TYPE datum.
  SELECT-OPTIONS: S_DN FOR lv_vbeln,
                  S_Date FOR lv_dats .
SELECTION-SCREEN : END OF BLOCK B1.



DATA: lt_vbuk TYPE TABLE OF vbuk,
      ls_vbuk TYPE vbuk,
      lt_likp TYPE TABLE OF likp,
      ls_likp TYPE likp,
      profile TYPE string VALUE '&1',
      topic   TYPE string value '&1',
      lo_del_details TYPE REF TO ZCL_DELIVERY_DETAILS.

SELECT vbeln erdat from likp into CORRESPONDING FIELDS OF TABLE lt_likp where ERDAT in S_Date.


IF  lt_likp IS  NOT INITIAL.
 SELECT VBELN WBSTK from vbuk into CORRESPONDING FIELDS OF TABLE lt_vbuk
    FOR ALL ENTRIES IN lt_likp where vbeln = lt_likp-vbeln.
  LOOP AT lt_vbuk into ls_vbuk.
              CREATE OBJECT lo_del_details EXPORTING DELIVERY_NO = ls_vbuk-vbeln.
    IF ls_vbuk-wbstk = 'C'.
      CALL METHOD lo_del_details->PUSH_PGI_DETAILS_TO_TOPIC EXPORTING  DELIVERY = LS_VBUK-VBELN
                                                                       PROFILE  = profile
                                                                       TOPIC    = topic.

    ENDIF.
    IF ls_vbuk-wbstk = 'A'.
        CALL METHOD lo_del_details->PUSH_DELIVERY_TO_TOPIC EXPORTING profile = profile
                                                                     topic   = topic.
    ENDIF.
    clear: ls_vbuk.

  ENDLOOP.
ENDIF.
