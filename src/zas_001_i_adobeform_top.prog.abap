*&---------------------------------------------------------------------*
*& Include          ZAS_001_I_ADOBEFORM_TOP
*&---------------------------------------------------------------------*

CLASS lcl_main DEFINITION DEFERRED.
DATA: go_main TYPE REF TO lcl_main.

TABLES: lips,likp.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_rad1 RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND radio .
  PARAMETERS: p_rad2 RADIOBUTTON GROUP 1.
  select-options s_vbeln for lips-vbeln MODIF ID abc.
  SELECT-OPTIONS s_posnr FOR lips-posnr MODIF ID abd.
  SELECT-OPTIONS s_matnr FOR lips-matnr MODIF ID abe.
  SELECT-OPTIONS s_kunnr FOR likp-kunnr MODIF ID abf.
  PARAMETERS  p_file TYPE localfile OBLIGATORY MODIF ID aca.

SELECTION-SCREEN END OF BLOCK blk1.
