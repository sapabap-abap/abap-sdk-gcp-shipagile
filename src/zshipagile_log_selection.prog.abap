*&---------------------------------------------------------------------*
*& Include          ZSHIPAGILE_LOG_SELECTION
*&---------------------------------------------------------------------*
DATA : log TYPE zshipagile_log.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : s_date   FOR log-zdate ,
                 s_uname  FOR log-user_name,
                 s_doc    FOR log-document NO INTERVALS,
                 s_doctyp FOR log-document_type   NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN PUSHBUTTON  1(24) but1
 USER-COMMAND b1.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
but1 = text-006.

AT SELECTION-SCREEN.
  PERFORM handle_ok_code.

START-OF-SELECTION .

  PERFORM main.
