*&---------------------------------------------------------------------*
*& Include          ZSD_SHIPAGILE_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.


  ok_code  = sy-ucomm.
  CASE ok_code.
    WHEN c_enter.
    WHEN c_cancel OR c_exit.
      LEAVE PROGRAM.
    WHEN c_back.
      LEAVE TO  SCREEN 0.
    WHEN  c_save.
    WHEN c_display.
      PERFORM display_document.
    WHEN c_log.
      perform display_single_log.
  ENDCASE.

  CLEAR ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.


  ok_code  = sy-ucomm.
  CASE ok_code.
    WHEN c_enter.

    WHEN c_cancel OR c_exit.
      LEAVE PROGRAM.
    WHEN c_back.
      LEAVE TO  SCREEN 0.
    WHEN  c_save.

  ENDCASE.

  CLEAR ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  ok_code  = sy-ucomm.
  CASE ok_code.
    WHEN c_enter.

    WHEN c_cancel OR c_exit.
      LEAVE PROGRAM.
    WHEN c_back.
      LEAVE TO  SCREEN 0.
    WHEN  c_save.

  ENDCASE.

  CLEAR ok_code.
ENDMODULE.
