*&---------------------------------------------------------------------*
*& Include          ZSD_SHIPAGILE_PBO
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZSTANDARD'.
  SET TITLEBAR c_title.

  PERFORM display_grid.
  PERFORM alv_grid_delivery.
ENDMODULE.
