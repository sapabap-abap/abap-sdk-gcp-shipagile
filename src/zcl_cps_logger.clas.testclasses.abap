
CLASS cl_test_bal_logobj DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>cl_Test_Bal_Logobj
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>CL_BAL_LOGOBJ
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO cl_bal_logobj,  "class under test
      handler_raised type abap_bool value abap_false.

    METHODS: general_test FOR TESTING.
    methods: event_handler for event max_memory_limit_reached of cl_bal_logobj.
ENDCLASS.       "cl_Test_Bal_Logobj


CLASS cl_test_bal_logobj IMPLEMENTATION.

  METHOD general_test.
    DATA: lr_log      TYPE REF TO cl_bal_logobj,
          /dev/null   TYPE string,
          lt_lognrnew TYPE bal_t_lgnm,
          ls_lognrnew TYPE bal_s_lgnm,
          lt_lognrs   TYPE bal_t_logn.

    TRY.
        CREATE OBJECT lr_log
          EXPORTING
            i_log_object        = 'CCMS'
            i_default_subobject = 'PROC'
            i_debug_subobject   = 'DEBUG'
            I_MAX_MSG_MEMORY    = 3.
        set handler event_handler for lr_log.
        cl_aunit_assert=>assert_bound( act = lr_log ).
        cl_aunit_assert=>assert_not_initial( act = lr_log->get_handle( ) ).
        lr_log->add_errortext( i_errortext = 'Test errortext' ).
        lr_log->add_statustext( i_statustext = 'Statustext' ).
        MESSAGE s009(bl) INTO /dev/null.
        lr_log->add_msg( i_probclass = if_sbal_logger_config=>c_probclass_none ).
        lr_log->increment_detlevel( ).
        lr_log->add_statustext( i_statustext = 'Statustext' ).
        lr_log->decrement_detlevel( ).
        TRY.
            RAISE EXCEPTION TYPE cx_demo_constructor
              EXPORTING
                my_text = sy-repid.

          CATCH cx_demo_constructor INTO DATA(r_demo).
            lr_log->add_exception( i_exception = r_demo ).
        ENDTRY.
        cl_aunit_assert=>assert_equals( act = handler_raised
                                        exp = abap_true ).
        lr_log->add_debug_msg( EXPORTING i_headertext = 'Debugmessage'
                                                 i_msgv1 = 'Var1'
                                                 i_msgv2 = 'Var2'
                                                 i_msgv3 = 'Var3'
                                                 i_msgv4 = 'Var4'
                                                 i_detlevel = '2' ).
        lr_log->update_progress( i_percentage = 50 ).
        lr_log->save( IMPORTING
                                      et_lognumbers = lt_lognrnew ).
        lr_log->display( ).
        lr_log->refresh( ).
        lr_log->display( ).

      CATCH cx_bal_exception INTO DATA(r_bal).
        cl_aunit_assert=>fail( msg = r_bal->get_longtext( ) ).
    ENDTRY.

  ENDMETHOD.

  method event_handler.
    handler_raised = abap_true.
  ENDMETHOD.


ENDCLASS.
