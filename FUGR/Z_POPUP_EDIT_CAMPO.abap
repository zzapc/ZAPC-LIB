FUNCTION Z_POPUP_EDIT_CAMPO.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(TITULO) TYPE  ANY
*"     REFERENCE(NOMBRE_CAMPO) TYPE  ANY
*"     REFERENCE(EDITABLE) TYPE  ABAP_BOOL DEFAULT 'X'
*"     REFERENCE(VISIBLE_INICIAL) TYPE  ABAP_BOOL DEFAULT ''
*"     REFERENCE(OBLIGATORIO) TYPE  ABAP_BOOL DEFAULT ''
*"     REFERENCE(X) TYPE  INT1 DEFAULT 10
*"     REFERENCE(Y) TYPE  INT1 DEFAULT 10
*"  EXPORTING
*"     REFERENCE(MODIFICADO) TYPE  ABAP_BOOL
*"  CHANGING
*"     REFERENCE(VALOR) TYPE  ANY
*"  EXCEPTIONS
*"      CANCELADO
*"----------------------------------------------------------------------


  CLEAR: tvarvc, v_cancelado, v_modificado, v_edit, modificado.
  v_nombre_campo = nombre_campo.
  v_titulo = titulo.
  v_editable = editable.
  IF visible_inicial = 'X'.
    CLEAR v_edit.
  ELSE.
    v_edit = v_editable.
  ENDIF.
  tvarvc-low = valor.

  PERFORM guardar_botones_v2 IN PROGRAM zap_status.
  PERFORM clear_botones IN PROGRAM zap_status.
  CALL SCREEN 0200 STARTING AT x y.
  PERFORM restaurar_botones_v2 IN PROGRAM zap_status.

  IF v_cancelado = 'X'.
    RAISE cancelado.
  else.
    if valor ne tvarvc-low.
      modificado = 'X'.
      valor = tvarvc-low.
    endif.
  ENDIF.


 ENDFUNCTION.
