*&---------------------------------------------------------------------*
*& Report ZGCP_TEST
*&---------------------------------------------------------------------*
*&
REPORT zgcp_test.

INCLUDE zgcp_test_selection.
INCLUDE zgcp_test_class.

LOAD-OF-PROGRAM .
  DATA : lo_driver    TYPE REF TO  lcl_driver ##NEEDED.
  CREATE OBJECT lo_driver EXPORTING screen = sy-dynnr repid = sy-repid .
INITIALIZATION.
  lo_driver->initialization( ).
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_prof.
  p_prof  = lo_driver->f4_help_profile(  ).
AT SELECTION-SCREEN ON p_chk.
  lo_driver->set_parameterid( parameter_id = 'ZDISPLAY_RESPONSE' im_value = p_chk ).
AT SELECTION-SCREEN .
  lo_driver->handle_ok_code( sy-ucomm ).
START-OF-SELECTION.
  lo_driver->execute_test( ).
