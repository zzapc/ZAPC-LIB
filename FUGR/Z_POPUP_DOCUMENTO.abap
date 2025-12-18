FUNCTION Z_POPUP_DOCUMENTO.
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(OPERACION) TYPE  CHAR1 DEFAULT 'V'
*"  CHANGING
*"     REFERENCE(DOCUMENTO) TYPE  ZEST_DOCUMENTO
*"--------------------------------------------------------------------
CLEAR zest_documento.
  zest_documento = documento.
   *zest_documento = zest_documento.
  v_operacion = operacion.

  CALL SCREEN 0100 STARTING AT 2 2 ENDING AT 120 8.





ENDFUNCTION.
