*&---------------------------------------------------------------------*
*& Report ZSHIPAGILE_CERTI_UPLD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshipagile_starter_program MESSAGE-ID TRUST .

INCLUDE zshipagile_starter_program_sel.
INCLUDE zshipagile_starter_program_cl.

LOAD-OF-PROGRAM.
DATA(lo_driver) = NEW lcl_driver( ).

INITIALIZATION.
  lo_driver->initialization( ).

AT SELECTION-SCREEN OUTPUT.
  lo_driver->selection_screen_output( ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR  p_file.
  lo_driver->f4_file( dynpro_number =  sy-dynnr program_name = sy-cprog ).


AT SELECTION-SCREEN.
lo_driver->handle_ok_code( sy-ucomm ).
