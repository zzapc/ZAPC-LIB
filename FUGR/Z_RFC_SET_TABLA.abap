FUNCTION Z_RFC_SET_TABLA.
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(TABLA) TYPE  TABNAME
*"     VALUE(CLAVE) TYPE  STRING OPTIONAL
*"     VALUE(CLAVE2) TYPE  STRING OPTIONAL
*"     VALUE(CAMPO_CLAVE) TYPE  STRING OPTIONAL
*"     VALUE(INICIO_CLAVE) TYPE  STRING DEFAULT ''
*"     VALUE(BEGDA) TYPE  BEGDA OPTIONAL
*"     VALUE(ENDDA) TYPE  ENDDA OPTIONAL
*"     VALUE(WHERE) TYPE  STRING DEFAULT ''
*"  EXPORTING
*"     VALUE(CONTENIDO) TYPE  XSTRING
*"     VALUE(MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      I_CLAVES STRUCTURE  AGR_TXT OPTIONAL
*"--------------------------------------------------------------------
DATA: i_where TYPE TABLE OF afx_str_where_clause WITH HEADER LINE.

  DATA: g_it_tabla       TYPE REF TO data,
        g_wa_tabla       TYPE REF TO data,
        o_conv_no_nomber TYPE REF TO cx_sy_conversion_no_number,
        o_root           TYPE REF TO cx_root.

  FIELD-SYMBOLS: <g_it_tabla> TYPE STANDARD TABLE,
                 <g_wa_tabla>.

  CREATE DATA g_wa_tabla TYPE (tabla).
  ASSIGN g_wa_tabla->* TO <g_wa_tabla>.
  CREATE DATA g_it_tabla TYPE STANDARD TABLE OF (tabla).
  ASSIGN g_it_tabla->* TO <g_it_tabla>.

  IF clave IS INITIAL AND i_claves[] IS INITIAL AND begda IS INITIAL and where is initial.
    SELECT * FROM (tabla)
      INTO TABLE <g_it_tabla>.
  ELSE.
    CALL FUNCTION 'Z_GET_WHERE'
      EXPORTING
        tabla        = tabla
        clave        = clave
        clave2       = clave2
        campo_clave  = campo_clave
        inicio_clave = inicio_clave
        begda        = begda
        endda        = endda
        where        = where
      TABLES
        i_claves     = i_claves
        i_where      = i_where.

    SELECT * FROM (tabla)
      INTO TABLE <g_it_tabla>
     WHERE (i_where).
  ENDIF.

  DATA ixml TYPE xstring.
  DATA xml TYPE string.

  TRY.
      CALL TRANSFORMATION id
        SOURCE <g_wa_tabla> = <g_it_tabla>
        RESULT XML  contenido.
    CATCH cx_sy_conversion_no_number INTO o_conv_no_nomber.
      message = o_conv_no_nomber->get_text( ).
      CONCATENATE 'Error conv.' message 'en tabla' tabla INTO message SEPARATED BY space.
    CATCH cx_root INTO o_root.
      message = o_root->get_text( ).
      CONCATENATE 'Error' message 'en tabla' tabla INTO message SEPARATED BY space.
  ENDTRY.





ENDFUNCTION.
