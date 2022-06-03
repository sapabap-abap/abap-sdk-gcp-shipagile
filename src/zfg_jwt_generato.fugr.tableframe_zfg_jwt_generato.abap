*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFG_JWT_GENERATO
*   generation date: 28.02.2022 at 21:52:19
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFG_JWT_GENERATO   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
