*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCPS_ENDPOINTGEN................................*
DATA:  BEGIN OF STATUS_ZCPS_ENDPOINTGEN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCPS_ENDPOINTGEN              .
CONTROLS: TCTRL_ZCPS_ENDPOINTGEN
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: ZCPS_SUBS.......................................*
DATA:  BEGIN OF STATUS_ZCPS_SUBS                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCPS_SUBS                     .
CONTROLS: TCTRL_ZCPS_SUBS
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZCPS_TOPIC......................................*
DATA:  BEGIN OF STATUS_ZCPS_TOPIC                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCPS_TOPIC                    .
CONTROLS: TCTRL_ZCPS_TOPIC
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZJWT_PROFILE....................................*
DATA:  BEGIN OF STATUS_ZJWT_PROFILE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZJWT_PROFILE                  .
CONTROLS: TCTRL_ZJWT_PROFILE
            TYPE TABLEVIEW USING SCREEN '0009'.
*.........table declarations:.................................*
TABLES: *ZCPS_ENDPOINTGEN              .
TABLES: *ZCPS_SUBS                     .
TABLES: *ZCPS_TOPIC                    .
TABLES: *ZJWT_PROFILE                  .
TABLES: ZCPS_ENDPOINTGEN               .
TABLES: ZCPS_SUBS                      .
TABLES: ZCPS_TOPIC                     .
TABLES: ZJWT_PROFILE                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
