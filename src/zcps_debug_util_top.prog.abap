*&---------------------------------------------------------------------*
*& Include          ZSD_SHIPAGILE_TOP
*&---------------------------------------------------------------------*
  TYPE-POOLS: abap.
  TABLES : vttk,likp,sscrfields.
  TYPES : BEGIN OF  ty_final,
            checkbox TYPE c,
            vbeln    TYPE vbeln_vl,
            tknum    TYPE tknum,
          END OF    ty_final,

          BEGIN OF ty_details,
            vbeln TYPE vbeln_vl,
          END OF   ty_details.

  TYPES : BEGIN OF ty_payload,
            component TYPE char200,
            value     TYPE char200,
          END  OF ty_payload.


  CLASS lcl_handle_events DEFINITION DEFERRED.
  DATA: o_event_handler TYPE REF TO lcl_handle_events.
  DATA: o_columns TYPE REF TO cl_salv_columns_table,
        o_column  TYPE REF TO cl_salv_column_table.
  DATA: o_alv_events TYPE REF TO cl_salv_events_table.
  DATA: o_alv   TYPE REF TO cl_salv_table.
  DATA: o_alv2   TYPE REF TO cl_salv_table.
  DATA: o_container TYPE REF TO cl_gui_docking_container.
  DATA: o_container2 TYPE REF TO cl_gui_docking_container.
  DATA : log TYPE zshipagile_log.
*Functions
  DATA: o_functions TYPE REF TO cl_salv_functions_list.
  DATA :gt_final   TYPE STANDARD TABLE OF ty_final.
  DATA :gt_details TYPE STANDARD TABLE OF ty_details.
  DATA : gt_payload TYPE STANDARD TABLE OF ty_payload.
  DATA: gs_components TYPE abap_compdescr.
  DATA: gt_components_delivery  TYPE STANDARD TABLE OF abap_compdescr.
  DATA: gt_components_shipment TYPE STANDARD TABLE OF abap_compdescr.
  DATA: gt_components_package  TYPE STANDARD TABLE OF abap_compdescr.
  DATA: gt_components_booking  TYPE STANDARD TABLE OF abap_compdescr.
  DATA: gt_components_gi  TYPE STANDARD TABLE OF abap_compdescr.
  DATA: gt_components_label  TYPE STANDARD TABLE OF abap_compdescr.
  DATA: go_strucdescr TYPE REF TO cl_abap_structdescr.
  DATA: data_payload TYPE zdelivery_payload.
  DATA: profile TYPE  zjwt_profile.
  DATA : ok_code TYPE sy-ucomm.
  DATA :gt_log     TYPE  STANDARD TABLE OF zshipagile_log.
  DATA : gt_config TYPE STANDARD TABLE OF zshipagile_cnfg.

  DATA : answer TYPE c.

  CONSTANTS: c_enter       TYPE  char5 VALUE  'ENTER',
             c_save        TYPE  char4 VALUE  'SAVE',
             c_cancel      TYPE  char6 VALUE  'CANCEL',
             c_back        TYPE  char4 VALUE  'BACK',
             c_exit        TYPE  char4 VALUE  'EXIT',
             c_title       TYPE  char20 VALUE  'Shipagile Cockpit',
             c_display     TYPE  char8 VALUE '&DISPLAY',
             c_profile     TYPE  char8 VALUE '&PROFILE',
             c_create      TYPE  char7 VALUE '&CREATE',
             c_delete      TYPE  char7 VALUE '&DELETE',
             c_update      TYPE  char6 VALUE '&UPDATE',
             c_display_ssf TYPE  char7 VALUE '&DISPLAY_SF',
             c_log         TYPE char5 VALUE '&LOG',

             c_config_tab  TYPE char20 VALUE 'CONFIG',
             c_report_tab  TYPE char20 VALUE 'REPORT',
             c_log_tab     TYPE char20 VALUE 'LOG',
             c_tab1        TYPE char20 VALUE 'TAB1',
             c_tab2        TYPE char20 VALUE 'TAB2',
             c_tab3        TYPE char20 VALUE 'TAB3'.

  CONSTANTS : c_exe1  TYPE char5 VALUE 'EXEC1',
              c_exe2  TYPE char5 VALUE 'EXEC2',
              c_exe3  TYPE char5 VALUE 'EXEC3',
              c_exe4  TYPE char5 VALUE 'EXEC4',
              c_exe5  TYPE char5 VALUE 'EXEC5',
              c_exe6  TYPE char5 VALUE 'EXEC6',
              c_exe7  TYPE char5 VALUE 'EXEC7',
              c_exe8  TYPE char5 VALUE 'EXEC8',
              c_exe9  TYPE char5 VALUE 'EXEC9',
              c_exe10 TYPE char6 VALUE 'EXEC10',
              c_exe11 TYPE char6 VALUE 'EXEC11'.
  CONSTANTS : c_x TYPE c VALUE 'X'.
  CONSTANTS : c_f8 TYPE char5 VALUE 'ONLI'.
  CONSTANTS : c_push1  TYPE char5 VALUE 'PUSH1'.
  CONSTANTS : c_push2  TYPE char5 VALUE 'PUSH2'.
  CONSTANTS : c_push3  TYPE char5 VALUE 'PUSH3'.
