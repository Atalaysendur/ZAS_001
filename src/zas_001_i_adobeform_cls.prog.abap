*&---------------------------------------------------------------------*
*& Include          ZAS_001_I_ADOBEFORM_CLS
*&---------------------------------------------------------------------*



CLASS lcl_main DEFINITION CREATE PRIVATE FINAL.

  PUBLIC SECTION.
    METHODS: at_selection_screen_output,
      at_selection_value_request_for,
      get_data,
      get_excel,
      insert_excl,
      form_print,
      a5_form_print,
      start_of_selection,
      end_of_selection,
      display_alv,
*      smartforms,
      set_excluding,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.

    CLASS-METHODS: create_instance RETURNING VALUE(ro_main) TYPE REF TO lcl_main.

  PRIVATE SECTION.
    DATA: lt_row_no TYPE lvc_t_roid.
    DATA: lv_fcat TYPE  dd02l-tabname.
    DATA: lt_teslimat TYPE TABLE OF zas_001_s_teslimat.
    DATA: lt_excluding TYPE ui_functions,
          lv_excluding TYPE ui_func.
    CLASS-DATA: mo_main TYPE REF TO lcl_main,
                mo_alv  TYPE REF TO cl_gui_alv_grid,
                mo_cont TYPE REF TO cl_gui_custom_container.
    METHODS fill_main_fieldcat RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.
    METHODS fill_main_layout   RETURNING VALUE(rs_layo) TYPE lvc_s_layo.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD create_instance.
    IF mo_main IS INITIAL.
      mo_main = NEW #( ).
    ENDIF.
    ro_main = mo_main.
  ENDMETHOD.

  METHOD at_selection_screen_output.

    LOOP AT SCREEN.

      screen-active = COND #( WHEN p_rad1 EQ abap_true THEN SWITCH #( screen-group1 WHEN 'ACA' THEN 0 )
                              WHEN p_rad2 EQ abap_true THEN SWITCH #( screen-group1 WHEN 'ABC' THEN 0
                                                                                    WHEN 'ABD' THEN 0
                                                                                    WHEN 'ABE' THEN 0
                                                                                    WHEN 'ABF' THEN 0 )  ).

      screen-required = COND #( WHEN screen-name = 'P_FILE' THEN 2 ).
      MODIFY SCREEN.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_data.

    SELECT likp~kunnr,
           lips~vbeln,
           lips~posnr,
           lips~pstyv,
           lips~ernam,
           likp~wadat_ist,
           lips~matnr,
           lips~ntgew,
           lips~gewei
      FROM likp
      INNER JOIN lips ON likp~vbeln EQ lips~vbeln
      WHERE lips~vbeln IN @s_vbeln AND
            lips~posnr IN @s_posnr AND
            lips~matnr IN @s_matnr AND
            likp~kunnr IN @s_kunnr
      INTO TABLE @lt_teslimat.

  ENDMETHOD.

  METHOD at_selection_value_request_for.
    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        field_name = 'P_FILE'
      IMPORTING
        file_name  = p_file.

  ENDMETHOD.

  METHOD get_excel.

    DATA : lt_raw_data TYPE truxs_t_text_data.

    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = 'X'
        i_tab_raw_data       = lt_raw_data
        i_filename           = p_file
      TABLES
        i_tab_converted_data = lt_teslimat.

  ENDMETHOD.

  METHOD start_of_selection.

    IF p_rad1 EQ abap_true.
      mo_main->get_data( ).
    ELSEIF p_rad2 EQ abap_true AND p_file IS NOT INITIAL.
      mo_main->get_excel( ).
    ELSE.
*      MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
      MESSAGE s000(zas_0001) DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD end_of_selection.

    IF lt_teslimat IS INITIAL.
      MESSAGE 'Veri Bulunamadı.' TYPE 'I'.
      MESSAGE i001(zas_0001).
    ELSE.
      CALL SCREEN 0100.
    ENDIF.

  ENDMETHOD.

  METHOD fill_main_layout.

    rs_layo = VALUE lvc_s_layo(     zebra       = abap_true
                                    sel_mode    = 'A'
                                    cwidth_opt  = abap_true
                                   ).
  ENDMETHOD.

  METHOD fill_main_fieldcat.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZAS_001_S_TESLIMAT'
      CHANGING
        ct_fieldcat      = rt_fcat.

  ENDMETHOD.

  METHOD set_excluding.
    lt_excluding = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_delete_row )  ).
  ENDMETHOD.


  METHOD display_alv.

    IF mo_alv IS INITIAL.


      mo_cont  = NEW #( container_name = 'CC_0100' ).

      mo_alv   = NEW #( i_parent = mo_cont  ).

      DATA(lt_fcat_main) = me->fill_main_fieldcat( ).

      set_excluding( ).

      SET HANDLER: mo_main->handle_user_command FOR mo_alv,
                   mo_main->handle_toolbar FOR mo_alv.

      CALL METHOD mo_alv->set_table_for_first_display
        EXPORTING
          is_layout                     = me->fill_main_layout( )
          it_toolbar_excluding          = lt_excluding
        CHANGING
          it_outtab                     = lt_teslimat
          it_fieldcatalog               = lt_fcat_main
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

    ELSE.
      mo_alv->refresh_table_display( is_stable = VALUE #( col = abap_true row = abap_true ) ).

    ENDIF.

  ENDMETHOD.

  METHOD handle_toolbar.

    APPEND VALUE stb_button( butn_type = 0
                            function  = '&ETYAZDIR'
                            icon      = icon_positive
                            text      = 'Etiket Yazdır'
                            quickinfo = 'Etiket Yazdır'
                            disabled  = ''
                           ) TO e_object->mt_toolbar.

    APPEND VALUE stb_button( butn_type = 0
                       function  = '&FYAZDIR'
                       icon      = icon_system_save
                       text      = 'Form Yazdır'
                       quickinfo = 'Form Yazdır'
                       disabled  = ''
                      ) TO e_object->mt_toolbar.

    IF p_rad2 EQ abap_true..
      APPEND VALUE stb_button( butn_type = 0
                           function  = '&KAYDET'
                           icon      = icon_system_save
                           text      = 'Kaydet'
                           quickinfo = 'Kaydet'
                           disabled  = ''
                          ) TO e_object->mt_toolbar.
    ENDIF.



  ENDMETHOD.

  METHOD handle_user_command.

    CASE    e_ucomm.
      WHEN '&KAYDET'.
        mo_main->insert_excl( ).
      WHEN '&FYAZDIR'.
        mo_main->form_print( ).
      WHEN '&ETYAZDIR'.
        mo_main->a5_form_print( ).
    ENDCASE.

    mo_alv->refresh_table_display( ).
  ENDMETHOD.

  METHOD insert_excl.

    DATA: lt_tslmt_log TYPE TABLE OF zas_001_t_tslmt.

    CALL METHOD mo_alv->get_selected_rows
      IMPORTING
        et_row_no = lt_row_no.

    IF lt_row_no IS NOT INITIAL.

      LOOP AT lt_row_no INTO DATA(wa_rows) .

        READ TABLE lt_teslimat INTO DATA(ls_teslimat) INDEX wa_rows-row_id.

        IF sy-subrc EQ 0.

          APPEND VALUE #( mandt      = sy-mandt
                          vbeln     = ls_teslimat-vbeln
                          posnr     = ls_teslimat-posnr
                          kunnr     = ls_teslimat-kunnr
                          pstyv     = ls_teslimat-pstyv
                          ernam     = ls_teslimat-ernam
                          wadat_ist = ls_teslimat-wadat_ist
                          matnr     = ls_teslimat-matnr
                          ntgew     = ls_teslimat-ntgew
                          gewei     = ls_teslimat-gewei
                          log_name  = sy-uname
                          log_date  = sy-datum
                          log_time  = sy-uzeit           ) TO lt_tslmt_log.

        ENDIF.

      ENDLOOP.

      INSERT zas_001_t_tslmt FROM TABLE lt_tslmt_log.
      IF sy-subrc EQ 0.
        COMMIT WORK.
*        MESSAGE 'Kayıt İşlemi Tamamlandı.' TYPE 'I'.
        MESSAGE i002(zas_0001).
      ELSE.
        ROLLBACK WORK.
*        MESSAGE 'Kayıt İşlemi Başarısız Oldu..' TYPE 'I'.
        MESSAGE i003(zas_0001).
      ENDIF.

    ELSE.
*      MESSAGE 'Lütfen Bir Satır Seçiniz !' TYPE 'I'.
      MESSAGE i004(zas_0001).
    ENDIF.
  ENDMETHOD.

  METHOD form_print.

    DATA: ls_outputparams TYPE sfpoutputparams,
          lv_name         TYPE  fpname,
          lv_funcname     TYPE  funcname,
          ls_docparams    TYPE sfpdocparams,
          ls_formoutput   TYPE fpformoutput.

    TYPES: BEGIN OF lty_collect_item,
             kunnr TYPE likp-kunnr,
             vbeln TYPE likp-vbeln,
           END OF lty_collect_item.

    DATA: lt_collect_item TYPE TABLE OF lty_collect_item.
    DATA: ls_collect_item TYPE  lty_collect_item.

    DATA: lt_item      TYPE SORTED TABLE OF  zas_001_s_teslimat WITH NON-UNIQUE KEY kunnr vbeln posnr,
          lt_item_fltr TYPE zas_001_tt_tslmtform,
          ls_item      LIKE LINE OF lt_item,
          ls_header    TYPE zas_001_s_tslmtform.


    DATA: lr_item_range_knr TYPE RANGE OF likp-kunnr.
    DATA: lr_item_range_vbln TYPE RANGE OF likp-vbeln.


    CALL METHOD mo_alv->get_selected_rows
      IMPORTING
        et_row_no = lt_row_no.

    IF lt_row_no IS NOT INITIAL.

      LOOP AT lt_row_no INTO DATA(wa_rows) .
        READ TABLE lt_teslimat INTO DATA(ls_teslimat2) INDEX wa_rows-row_id.
        ls_collect_item-kunnr = ls_teslimat2-kunnr.
        ls_collect_item-vbeln = ls_teslimat2-vbeln.
        COLLECT ls_collect_item INTO  lt_collect_item.
        IF line_exists( lr_item_range_knr[ table_line = ls_teslimat2-kunnr ] ) AND line_exists( lr_item_range_vbln[ table_line = ls_teslimat2-vbeln ] ).
          CONTINUE.
        ELSE.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_teslimat2-kunnr ) TO lr_item_range_knr.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_teslimat2-vbeln ) TO lr_item_range_vbln.
        ENDIF.
      ENDLOOP.
    ELSE.
*      MESSAGE 'Lütfen Bir Satır Seçiniz !' TYPE 'E'.
      MESSAGE e004(zas_0001).
    ENDIF.

    SELECT lips~vbeln,
           lips~posnr,
           likp~kunnr,
           lips~pstyv,
           lips~ernam,
           likp~wadat_ist,
           lips~matnr,
           lips~ntgew,
           lips~gewei,
           likp~route
      FROM likp
      INNER JOIN lips ON likp~vbeln EQ lips~vbeln
      INTO CORRESPONDING FIELDS OF TABLE @lt_item
      WHERE likp~kunnr IN @lr_item_range_knr AND
            likp~vbeln IN @lr_item_range_vbln.


    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    lv_name = 'ZAS_001_AF_TSLMTFORM'.

    LOOP AT lt_collect_item INTO ls_collect_item.

      READ TABLE lt_item INTO ls_item WITH KEY kunnr = ls_collect_item-kunnr vbeln = ls_collect_item-vbeln.
      MOVE-CORRESPONDING ls_item TO ls_header.


      lt_item_fltr = FILTER #( lt_item  WHERE kunnr = ls_collect_item-kunnr AND vbeln =  ls_collect_item-vbeln  ) .

      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = lv_name
        IMPORTING
          e_funcname = lv_funcname.

      CALL FUNCTION lv_funcname
        EXPORTING
          /1bcdwb/docparams  = ls_docparams
          is_header          = ls_header
          it_item            = lt_item_fltr
        IMPORTING
          /1bcdwb/formoutput = ls_formoutput
        EXCEPTIONS
          usage_error        = 1
          system_error       = 2
          internal_error     = 3
          OTHERS             = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    CLEAR: ls_header.
    REFRESH lt_item_fltr.



  ENDMETHOD.

  METHOD a5_form_print.

    DATA: ls_outputparams TYPE sfpoutputparams,
          lv_name         TYPE  fpname,
          lv_funcname     TYPE  funcname,
          ls_docparams    TYPE sfpdocparams,
          ls_formoutput   TYPE fpformoutput.

    DATA: ls_header_a5 TYPE zas_001_s_teslimata5frm.

    CALL METHOD mo_alv->get_selected_rows
      IMPORTING
        et_row_no = lt_row_no.


    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    lv_name = 'ZAS_001_AF_TSLMTA5FORM'.

    IF lt_row_no IS NOT INITIAL.

      LOOP AT lt_row_no INTO DATA(wa_rows) .
        READ TABLE lt_teslimat INTO DATA(ls_teslimat3) INDEX wa_rows-row_id.
        ls_header_a5-qrcode = ls_teslimat3-matnr.
        MOVE-CORRESPONDING ls_teslimat3 TO ls_header_a5.
        CONDENSE ls_header_a5-qrcode.
        CONCATENATE '00002' ls_header_a5-matnr INTO ls_header_a5-qrcode.
        CONDENSE: ls_header_a5-vbeln,ls_header_a5-posnr,ls_header_a5-matnr.

        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = lv_name
          IMPORTING
            e_funcname = lv_funcname.

        CALL FUNCTION lv_funcname
          EXPORTING
            /1bcdwb/docparams  = ls_docparams
            is_header          = ls_header_a5
          IMPORTING
            /1bcdwb/formoutput = ls_formoutput
          EXCEPTIONS
            usage_error        = 1
            system_error       = 2
            internal_error     = 3
            OTHERS             = 4.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

      ENDLOOP.
    ELSE.
*      MESSAGE 'Lütfen Bir Satır Seçiniz !' TYPE 'E'.
      MESSAGE e004(zas_0001).
    ENDIF.

    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    CLEAR: ls_header_a5.



  ENDMETHOD.

ENDCLASS.
