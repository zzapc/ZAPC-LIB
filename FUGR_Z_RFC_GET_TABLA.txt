FUNCTION Z_RFC_GET_TABLA.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(TABLA) TYPE  TABNAME
*"     VALUE(CLAVE) TYPE  STRING OPTIONAL
*"     VALUE(CLAVE2) TYPE  STRING OPTIONAL
*"     VALUE(ACTUALIZAR) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(SISTEMA) TYPE  RFCDEST DEFAULT ZCL_C=>RFC_PRODUCCION
*"     VALUE(CAMPO_CLAVE) TYPE  STRING OPTIONAL
*"     VALUE(INICIO_CLAVE) TYPE  STRING DEFAULT ''
*"     VALUE(BEGDA) TYPE  BEGDA OPTIONAL
*"     VALUE(ENDDA) TYPE  ENDDA OPTIONAL
*"     VALUE(PREVISUALIZAR) TYPE  XFELD DEFAULT ''
*"     VALUE(WHERE) TYPE  STRING DEFAULT ''
*"     VALUE(GET_CONTENIDO) TYPE  XFELD DEFAULT ''
*"  EXPORTING
*"     VALUE(MESSAGE) TYPE  BAPI_MSG
*"     VALUE(CONTENIDO) TYPE  XSTRING
*"  TABLES
*"      I_CLAVES STRUCTURE  AGR_TXT OPTIONAL
*"----------------------------------------------------------------------
  DATA: g_wa_tabla TYPE REF TO data,
        g_it_tabla TYPE REF TO data,
        l_string   TYPE string,
        l_string2  TYPE string,
        i_where    TYPE TABLE OF afx_str_where_clause WITH HEADER LINE.

  DATA l_cont TYPE xstring.
  DATA ltext  TYPE string.
  DATA o_alv  TYPE REF TO zcl_ap_alv.

  FIELD-SYMBOLS: <g_wa_tabla>,
                 <g_it_tabla> TYPE STANDARD TABLE.

  CREATE DATA g_wa_tabla TYPE (tabla).
  ASSIGN g_wa_tabla->* TO <g_wa_tabla>.
  CREATE DATA g_it_tabla TYPE STANDARD TABLE OF (tabla).
  ASSIGN g_it_tabla->* TO <g_it_tabla>.

  l_string = clave.
  l_string2 = clave2.
  CALL FUNCTION 'Z_RFC_SET_TABLA'
    DESTINATION sistema
    EXPORTING
      tabla        = tabla
      clave        = l_string
      clave2       = l_string2
      campo_clave  = campo_clave
      inicio_clave = inicio_clave
      begda        = begda
      endda        = endda
      where        = where
    IMPORTING
      contenido    = l_cont
      message      = message
    TABLES
      i_claves     = i_claves
    EXCEPTIONS
      communication_failure       = 1
      system_failure              = 2
      OTHERS = 99.
    if sy-subrc <> 0.
      message = 'Error en la llamada remota'.
      RETURN.
    endif.


  IF get_contenido = 'X' AND previsualizar = ''.
    contenido = l_cont.
  ELSE.
    CALL TRANSFORMATION id
         SOURCE XML l_cont
         RESULT <g_wa_tabla> = <g_it_tabla>.

    IF previsualizar = 'X'.
      o_alv = NEW #(
          tabla = '' ).
      o_alv->constructor_tabla( CHANGING t_tabla = <g_it_tabla> ).
      o_alv->show( ).
    ELSE.
      IF NOT actualizar IS INITIAL.
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

        DELETE FROM (tabla)
         WHERE (i_where).

        LOOP AT <g_it_tabla> INTO <g_wa_tabla>.
          MODIFY (tabla) FROM <g_wa_tabla>.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
