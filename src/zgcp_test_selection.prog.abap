*&---------------------------------------------------------------------*
*& Include          ZTEST_GCP_SELECTION
*&---------------------------------------------------------------------*


*SELECTION-SCREEN  BEGIN OF SCREEN 100 .
SELECTION-SCREEN BEGIN OF BLOCK outer NO INTERVALS.
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_prof   TYPE zjwt_profile-profile_name  DEFAULT 'SHIPAGILE'.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN  : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-006.
PARAMETERS : r_p RADIOBUTTON GROUP g2 USER-COMMAND rad ,
             r_s RADIOBUTTON GROUP g2.
PARAMETERS: p_chk AS CHECKBOX DEFAULT 'X' USER-COMMAND disp .
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK  b3.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 1(20)  but1 USER-COMMAND b1."1(20)
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 1(20) but2 USER-COMMAND b2."24(20)
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 1(20) but3 USER-COMMAND b3."48(20)
*SELECTION-SCREEN SKIP 1.
*SELECTION-SCREEN PUSHBUTTON 1(20) but4 USER-COMMAND b4."70(20)
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN : END OF  BLOCK b2.
*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN  BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-007 ."config
"SSL
*SELECTION-SCREEN BEGIN OF BLOCK  b5 WITH FRAME TITLE TEXT-008.
*SELECTION-SCREEN PUSHBUTTON 1(20) but5 USER-COMMAND b5 .
*SELECTION-SCREEN SKIP  1.
*SELECTION-SCREEN PUSHBUTTON 1(20) but6 USER-COMMAND b6.
*SELECTION-SCREEN SKIP  1.
*SELECTION-SCREEN PUSHBUTTON 1(20) but7 USER-COMMAND b7.
*SELECTION-SCREEN END OF BLOCK b5.
*SELECTION-SCREEN SKIP  1.
*SELECTION-SCREEN : BEGIN OF BLOCK b7 WITH FRAME TITLE TEXT-011.
*SELECTION-SCREEN PUSHBUTTON 1(20) but11 USER-COMMAND b11.
*SELECTION-SCREEN : END OF  BLOCK b7.
*SELECTION-SCREEN SKIP 1.
"GIT
*SELECTION-SCREEN BEGIN OF BLOCK  b6  WITH FRAME TITLE TEXT-009.
*SELECTION-SCREEN PUSHBUTTON 1(20) but8 USER-COMMAND b8.
*SELECTION-SCREEN PUSHBUTTON 24(20) but9 USER-COMMAND b9.
*SELECTION-SCREEN END OF BLOCK b6.
*SELECTION-SCREEN SKIP  1.
"LOG
*SELECTION-SCREEN BEGIN OF BLOCK b8 WITH FRAME TITLE TEXT-010.
*SELECTION-SCREEN PUSHBUTTON 1(27) but10 USER-COMMAND b10.
*SELECTION-SCREEN END OF BLOCK b8.
SELECTION-SCREEN SKIP  1.
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN END OF BLOCK outer.
*SELECTION-SCREEN END OF SCREEN 100.
