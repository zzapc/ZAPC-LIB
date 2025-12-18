FUNCTION Z_POPUP_EDIT_TEXT.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(TITULO) TYPE  ANY DEFAULT 'Edición de texto'
*"     REFERENCE(TEXTO) TYPE  ANY DEFAULT ''
*"     REFERENCE(INI_X) TYPE  I DEFAULT 5
*"     REFERENCE(INI_Y) TYPE  I DEFAULT 3
*"     REFERENCE(FIN_X) TYPE  I DEFAULT 140
*"     REFERENCE(FIN_Y) TYPE  I DEFAULT 24
*"     REFERENCE(MAX_COLS) TYPE  I DEFAULT 132
*"     REFERENCE(DISPLAY_MODE) TYPE  C DEFAULT ''
*"     REFERENCE(TITULO_TEXTO_CORTO) TYPE  STRING DEFAULT ''
*"     REFERENCE(HTML) TYPE  ABAP_BOOL DEFAULT ''
*"  EXPORTING
*"     REFERENCE(MODIFICADO) TYPE  ABAP_BOOL
*"     REFERENCE(CANCELADO) TYPE  ABAP_BOOL
*"  CHANGING
*"     REFERENCE(STRING) TYPE  ANY
*"     REFERENCE(TEXTO_CORTO) TYPE  ANY OPTIONAL
*"----------------------------------------------------------------------
CLEAR: o_texto, v_cancel, modificado, ztemp.
  v_texto  = texto.
  v_titulo = titulo.
  v_string = string.
  v_display_mode = display_mode.
  v_max_cols = max_cols.
  v_texto_corto = titulo_texto_corto.
  v_HTML = html.
  v_html_op = html.
  ztemp-texto = texto_corto.

  CALL SCREEN 0500 STARTING AT ini_x ini_y ENDING AT fin_x fin_y.

  cancelado = v_cancel.
  IF v_cancel IS INITIAL and DISPLAY_MODE is initial.
    IF string NE v_string.
      modificado = 'X'.
      string = v_string.
      texto_corto = ztemp-texto.
    ENDIF.
  ENDIF.

  o_texto->destroy( ).





ENDFUNCTION.
