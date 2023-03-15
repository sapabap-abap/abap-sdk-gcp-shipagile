*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGCP_URL........................................*
DATA:  BEGIN OF STATUS_ZGCP_URL                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGCP_URL                      .
CONTROLS: TCTRL_ZGCP_URL
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZJWT_PROFILE....................................*
DATA:  BEGIN OF STATUS_ZJWT_PROFILE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZJWT_PROFILE                  .
CONTROLS: TCTRL_ZJWT_PROFILE
            TYPE TABLEVIEW USING SCREEN '0004'.
*.........table declarations:.................................*
TABLES: *ZGCP_URL                      .
TABLES: *ZJWT_PROFILE                  .
TABLES: ZGCP_URL                       .
TABLES: ZJWT_PROFILE                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
