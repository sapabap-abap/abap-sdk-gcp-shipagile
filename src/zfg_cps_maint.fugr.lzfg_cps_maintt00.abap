*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 20.05.2022 at 08:14:41
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCPS_ENDPOINTGEN................................*
DATA:  BEGIN OF STATUS_ZCPS_ENDPOINTGEN              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCPS_ENDPOINTGEN              .
CONTROLS: TCTRL_ZCPS_ENDPOINTGEN
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZCPS_SUBS.......................................*
DATA:  BEGIN OF STATUS_ZCPS_SUBS                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCPS_SUBS                     .
CONTROLS: TCTRL_ZCPS_SUBS
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZCPS_TOPIC......................................*
DATA:  BEGIN OF STATUS_ZCPS_TOPIC                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCPS_TOPIC                    .
CONTROLS: TCTRL_ZCPS_TOPIC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCPS_ENDPOINTGEN              .
TABLES: *ZCPS_SUBS                     .
TABLES: *ZCPS_TOPIC                    .
TABLES: ZCPS_ENDPOINTGEN               .
TABLES: ZCPS_SUBS                      .
TABLES: ZCPS_TOPIC                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
