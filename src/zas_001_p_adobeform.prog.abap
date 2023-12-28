*&---------------------------------------------------------------------*
*& Report ZAS_001_P_ADOBEFORM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zas_001_p_adobeform.

INCLUDE ZAS_001_I_ADOBEFORM_top.
INCLUDE ZAS_001_I_ADOBEFORM_cls.
INCLUDE ZAS_001_I_ADOBEFORM_mdl.


INITIALIZATION.
  go_main = lcl_main=>create_instance( ).

AT SELECTION-SCREEN OUTPUT.
  go_main->at_selection_screen_output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  go_main->at_selection_value_request_for( ).

START-OF-SELECTION.
  go_main->start_of_selection( ).

END-OF-SELECTION.
  go_main->end_of_selection( ).
