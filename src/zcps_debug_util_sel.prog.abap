*&---------------------------------------------------------------------*
*& Include          ZSD_SHIPAGILE_SELECTION
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*Report Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN  BEGIN OF SCREEN 2000 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001."report
SELECT-OPTIONS: s_delv FOR likp-vbeln,
                s_tknum FOR vttk-tknum.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 2000.


*&---------------------------------------------------------------------*
*Tab block
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF TABBED BLOCK tab FOR 40 LINES,
TAB (20) tab1 USER-COMMAND push1 ,
END OF BLOCK tab.
*&---------------------------------------------------------------------*
*Events
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM  initialization.


AT SELECTION-SCREEN .
  PERFORM handle_tabstrip USING sy-ucomm.
