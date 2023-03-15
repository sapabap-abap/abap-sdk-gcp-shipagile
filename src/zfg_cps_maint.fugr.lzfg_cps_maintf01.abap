*----------------------------------------------------------------------*
***INCLUDE LZFG_CPS_MAINTF01.
*----------------------------------------------------------------------*
*FORM zcreate_endpoint_url.
*  DATA : ls_endpoint TYPE zcps_endpointgen.
*  ls_endpoint  = <vim_total_struc>.
*  CASE <action>.
*    WHEN  'N'.
*      PERFORM zvalidate_input  USING ls_endpoint.
*      zcl_cps_endpoint_generate=>generate_urls( ls_endpoint  ).
*    WHEN 'D'.
*      zcl_cps_endpoint_generate=>delete_urls( ls_endpoint  ).
*  ENDCASE.
*  CLEAR :   ls_endpoint.
*ENDFORM.
*FORM zvalidate_input USING ls_endpoint TYPE zcps_endpointgen .
*  DATA : lf_error TYPE flag .
*  CASE abap_false.
*    WHEN  ls_endpoint-name.
*      lf_error = abap_true.
*      MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
*    WHEN  ls_endpoint-type.
*      lf_error = abap_true.
*      MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
*    WHEN  ls_endpoint-profile.
*      lf_error = abap_true.
*      MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
*  ENDCASE.
*
*  IF lf_error EQ abap_true.
*    vim_abort_saving = abap_true.
*    sy-subrc = 4.
*    EXIT.
*  ELSE.
*    vim_abort_saving = abap_false.
*    sy-subrc = 0.
*  ENDIF.
*ENDFORM.
