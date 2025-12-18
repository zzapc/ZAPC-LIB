*&---------------------------------------------------------------------*
*& Report ZAP_FIORI_BORRAR_CACHES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zap_fiori_borrar_caches.

PARAMETERS: smicm AS CHECKBOX DEFAULT ' ',
            chip  AS CHECKBOX DEFAULT '',
            fiori AS CHECKBOX DEFAULT '',
            segw  AS CHECKBOX DEFAULT 'X'.

START-OF-SELECTION.


  DATA: o_bi      TYPE REF TO zcl_ap_batch_input,
        l_mensaje TYPE bapireturn1-message.


  IF smicm = 'X'.
    CREATE OBJECT o_bi.

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120').
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=CACHE_INVL').

* popup_to_confirm_step
    o_bi->dynpro( program = 'SAPLSPO1' dynpro = '0100').
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=YES').

    o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120').
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=CACHE_INVG').

* popup_to_confirm_step
    o_bi->dynpro( program = 'SAPLSPO1' dynpro = '0100').
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=YES').

    o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120').
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=&F03').

    l_mensaje = o_bi->llamar_transaccion( tcode = 'SMICM' modo = 'E').
  ENDIF.

  IF chip = 'X'.
    SUBMIT /ui2/chip_synchronize_cache
       AND RETURN.
  ENDIF.

  IF fiori = 'X'.
    SUBMIT /ui2/invalidate_global_caches
      AND RETURN
      WITH gv_test = ''
      WITH gv_exe = 'X'.
  ENDIF.

  IF segw = 'X'.
    SUBMIT /iwbep/r_mgw_med_cache_cleanup
      AND RETURN
      WITH allmodel = 'X'.

    SUBMIT /iwfnd/r_med_cache_cleanup
      AND RETURN.
  ENDIF.
