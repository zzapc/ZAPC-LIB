TYPES: BEGIN OF t_param_rangos,
         campo  TYPE rstt_t_range_string,
         valor  TYPE rstt_t_range_string,
         valor2 TYPE rstt_t_range_string,
         atributo1 TYPE zparametros-atributo1,
         atributo2 TYPE zparametros-atributo2,
         atributo3 TYPE zparametros-atributo3,
       END OF t_param_rangos,
       tt_param_rangos type table of t_param_rangos.
CLASS zcl_ap_parametros DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA i_par         TYPE zt_parametros.
    DATA par           TYPE zparametros.
    DATA error         TYPE bapireturn1-message.
    DATA rangos        TYPE abap_bool.
    DATA ztclave_param TYPE ztclave_param.

    METHODS constructor
      IMPORTING clave         TYPE any
                mostrar_error TYPE abap_bool DEFAULT ''
                campo         TYPE any       DEFAULT ''
                rangos        TYPE abap_bool DEFAULT ''
                fichero_json  TYPE string    DEFAULT ''.

    CLASS-METHODS existe
      IMPORTING clave        TYPE any
                campo        TYPE any
                valor        TYPE any       DEFAULT ''
                valor2       TYPE any       OPTIONAL
                valor3       TYPE any       OPTIONAL
                valor4       TYPE any       OPTIONAL
                quitar_ceros TYPE abap_bool DEFAULT ''
      RETURNING VALUE(si)    TYPE abap_bool.

    METHODS mantenimiento
      IMPORTING campo   TYPE any       OPTIONAL
                !action TYPE c         DEFAULT 'U'
                valor   TYPE any       DEFAULT ''
                alv     TYPE abap_bool DEFAULT zcl_c=>zparametros_alv
      PREFERRED PARAMETER campo.

    CLASS-METHODS get_atributo1
      IMPORTING clave              TYPE any
                campo              TYPE any
                valor              TYPE any       DEFAULT ''
                valor2             TYPE any       DEFAULT ''
                valor3             TYPE any       DEFAULT ''
                valor4             TYPE any       DEFAULT ''
                n10                TYPE abap_bool DEFAULT ''
                ctd                TYPE any       DEFAULT ''
                quitar_ceros       TYPE abap_bool DEFAULT ''
                error_si_no_Existe TYPE abap_bool DEFAULT ''
                atributo           TYPE string    DEFAULT 'ATRIBUTO1'
      RETURNING VALUE(atributo1)   TYPE zatrib_param.

    METHODS get_atr1
      IMPORTING campo            TYPE any
                valor            TYPE any       OPTIONAL
                valor2           TYPE any       OPTIONAL
                n10              TYPE abap_bool DEFAULT ''
                quitar_ceros     TYPE abap_bool DEFAULT ''
                ctd              TYPE any       DEFAULT ''
                mostrar_error    TYPE abap_bool DEFAULT ''
      RETURNING VALUE(atributo1) TYPE zatrib_param.

    METHODS get_tabla_campo
      IMPORTING campo          TYPE any
                valor          TYPE any OPTIONAL
                valor2         TYPE any OPTIONAL
      RETURNING VALUE(i_tabla) TYPE zt_parametros.

    CLASS-METHODS mantenimiento_st
      IMPORTING clave   TYPE any
                campo   TYPE any       DEFAULT ''
                !action TYPE c         DEFAULT 'U'
                valor   TYPE any       DEFAULT ''
                alv     TYPE abap_bool DEFAULT zcl_c=>zparametros_alv.

    CLASS-METHODS get_atributo1_usr
      IMPORTING clave            TYPE any
                campo            TYPE any
                valor            TYPE any      DEFAULT ''
                !uname           TYPE sy-uname OPTIONAL
      RETURNING VALUE(atributo1) TYPE zatrib_param.

    METHODS exist
      IMPORTING campo     TYPE any
                valor     TYPE any DEFAULT ''
                valor2    TYPE any DEFAULT ''
                valor3    TYPE any DEFAULT ''
                valor4    TYPE any DEFAULT ''
      RETURNING VALUE(si) TYPE abap_bool.

    METHODS get_rango_tabla_campo
      IMPORTING campo                  TYPE any       DEFAULT ''
                valor                  TYPE any       DEFAULT ''
                valor2                 TYPE any       DEFAULT ''
                atributo1              TYPE any       DEFAULT ''
                campo_rango            TYPE any       DEFAULT 'ATRIBUTO1'
                !option                TYPE any       DEFAULT 'EQ'
                !sign                  TYPE any       DEFAULT 'I'
                todo                   TYPE abap_bool DEFAULT ''
                error_si_vacio         TYPE any       DEFAULT ''
                filtro_por_campo       TYPE any       DEFAULT ''
                valor_filtro_por_campo TYPE any       DEFAULT ''
      PREFERRED PARAMETER campo
      RETURNING VALUE(r_rango)         TYPE rstt_t_range_string.

    METHODS get_atr2
      IMPORTING campo            TYPE any
                valor            TYPE any OPTIONAL
                valor2           TYPE any OPTIONAL
      RETURNING VALUE(atributo2) TYPE zatrib_param.

    METHODS send_mail
      IMPORTING subject              TYPE any       OPTIONAL
                texto                TYPE any
                campo                TYPE any
                urgente              TYPE abap_bool DEFAULT 'X'
                html                 TYPE abap_bool DEFAULT ''
                !commit              TYPE abap_bool DEFAULT 'X'
                forzar_mail_externo  TYPE abap_bool DEFAULT ''
                confirmacion_lectura TYPE bcs_rqst  DEFAULT 'A'
                emisor               TYPE any       DEFAULT ''.

    CLASS-METHODS exit_activa
      IMPORTING clave         TYPE any      DEFAULT 'EXITS'
                usuario       TYPE sy-uname DEFAULT sy-uname
                !exit         TYPE any      OPTIONAL
      PREFERRED PARAMETER exit
      RETURNING VALUE(activa) TYPE abap_bool.

    METHODS get_atr_rango
      IMPORTING campo     TYPE any
                valor     TYPE any OPTIONAL
                valor2    TYPE any OPTIONAL
      CHANGING  atributo1 TYPE any
                atributo2 TYPE any
                atributo3 TYPE any.

    METHODS add_destinatarios
      IMPORTING campo               TYPE any       DEFAULT 'EMAIL'
                o_mail              TYPE REF TO zcl_ap_envio_mail
                forzar_mail_externo TYPE abap_bool DEFAULT ''
      RETURNING VALUE(error)        TYPE abap_bool.

    METHODS get_atr1_usr
      IMPORTING campo            TYPE any
                valor            TYPE any      OPTIONAL
                !uname           TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(atributo1) TYPE zatrib_param.

    CLASS-METHODS matchcode
      IMPORTING clave        TYPE any
                campo        TYPE any
                atributo2    TYPE any DEFAULT ''
      RETURNING VALUE(valor) TYPE zparametros-valor.

    METHODS inicio
      IMPORTING fichero_json TYPE string DEFAULT ''.

    METHODS get_rango_tabla_campo_n10
      IMPORTING campo                  TYPE any       DEFAULT ''
                valor                  TYPE any       DEFAULT ''
                valor2                 TYPE any       DEFAULT ''
                campo_rango            TYPE any       DEFAULT 'ATRIBUTO1'
                !option                TYPE any       DEFAULT 'EQ'
                !sign                  TYPE any       DEFAULT 'I'
                todo                   TYPE abap_bool DEFAULT ''
                error_si_vacio         TYPE any       DEFAULT ''
                filtro_por_campo       TYPE any       DEFAULT ''
                valor_filtro_por_campo TYPE any       DEFAULT ''
      PREFERRED PARAMETER campo
      RETURNING VALUE(r_rango_n10)     TYPE lxhme_range_n10_t.

    METHODS existe_i
      IMPORTING campo        TYPE any
                valor        TYPE any       DEFAULT ''
                valor2       TYPE any       DEFAULT ''
                quitar_ceros TYPE abap_bool DEFAULT ''
      RETURNING VALUE(si)    TYPE abap_bool.

    METHODS get_rango_tabla_campo_n18
      IMPORTING campo                  TYPE any       DEFAULT ''
                valor                  TYPE any       DEFAULT ''
                valor2                 TYPE any       DEFAULT ''
                campo_rango            TYPE any       DEFAULT 'ATRIBUTO1'
                !option                TYPE any       DEFAULT 'EQ'
                !sign                  TYPE any       DEFAULT 'I'
                todo                   TYPE abap_bool DEFAULT ''
                error_si_vacio         TYPE any       DEFAULT ''
                filtro_por_campo       TYPE any       DEFAULT ''
                valor_filtro_por_campo TYPE any       DEFAULT ''
      PREFERRED PARAMETER campo
      RETURNING VALUE(r_rango_n18)     TYPE range_t_matnr.

    METHODS get_atr3
      IMPORTING campo            TYPE any
                valor            TYPE any       OPTIONAL
                valor2           TYPE any       OPTIONAL
                n10              TYPE abap_bool DEFAULT ''
                quitar_ceros     TYPE abap_bool DEFAULT ''
                ctd              TYPE any       DEFAULT ''
                mostrar_error    TYPE abap_bool DEFAULT ''
      RETURNING VALUE(atributo3) TYPE zatrib_param.

    CLASS-METHODS get_atributo2
      IMPORTING clave              TYPE any
                campo              TYPE any
                valor              TYPE any       DEFAULT ''
                valor2             TYPE any       DEFAULT ''
                valor3             TYPE any       DEFAULT ''
                valor4             TYPE any       DEFAULT ''
                n10                TYPE abap_bool DEFAULT ''
                ctd                TYPE any       DEFAULT ''
                quitar_ceros       TYPE abap_bool DEFAULT ''
                error_si_no_Existe TYPE abap_bool DEFAULT ''
      RETURNING VALUE(atributo2)   TYPE zatrib_param.

    METHODS get_lista_atr1
      IMPORTING campo         TYPE any
                mostrar_error TYPE abap_bool DEFAULT ''
      RETURNING VALUE(lista)  TYPE string.

    METHODS get_lista
      IMPORTING campo         TYPE any
                mostrar_error TYPE abap_bool DEFAULT ''
                columna       TYPE string    DEFAULT 'VALOR'
                valor         TYPE string    DEFAULT ''
                valor2        TYPE string    DEFAULT ''
                valor3        TYPE string    DEFAULT ''
                valor4        TYPE string    DEFAULT ''
      RETURNING VALUE(lista)  TYPE string.

    METHODS get_tabla_formateada EXPORTING !message TYPE bapi_msg
                                 CHANGING  tabla    TYPE table.

    CLASS-METHODS claves_sin_ceros
      IMPORTING par           TYPE zparametros
      RETURNING VALUE(par_sc) TYPE zparametros.

    CLASS-METHODS get_parametro
      IMPORTING clave              TYPE any
                campo              TYPE any
                valor              TYPE any       DEFAULT ''
                valor2             TYPE any       DEFAULT ''
                valor3             TYPE any       DEFAULT ''
                valor4             TYPE any       DEFAULT ''
                quitar_ceros       TYPE abap_bool DEFAULT ''
                error_si_no_Existe TYPE abap_bool DEFAULT ''
      RETURNING VALUE(parametros)  TYPE zparametros.


  PRIVATE SECTION.
    DATA clave         TYPE zclave_param.
    DATA mostrar_error TYPE abap_bool.
    DATA campo         TYPE zcampo_param.
    DATA i_par_rangos  TYPE tt_param_rangos.
endclass. "ZCL_AP_PARAMETROS definition
class ZCL_AP_PARAMETROS implementation.
  METHOD add_destinatarios.
    error = 'X'.
    LOOP AT i_par INTO par WHERE campo = campo.
      IF par-atributo1 IS INITIAL.
        o_mail->add_destinatario( destinatario = par-valor forzar_mail_externo = forzar_mail_externo ).
        CLEAR error.
      ELSEIF NOT par-atributo1 IS INITIAL.
        o_mail->add_destinatario( destinatario = par-atributo1 forzar_mail_externo = forzar_mail_externo ).
        CLEAR error.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD claves_sin_ceros.
    par_sc = par.
    __quitar_ceros: par_sc-campo, par_sc-valor, par_sc-valor2, par_sc-valor3, par_sc-valor4.
  ENDMETHOD.
  METHOD constructor.
    me->clave         = clave.
    me->campo         = campo.
    me->mostrar_error = mostrar_error.
    me->rangos        = rangos.

    inicio( fichero_json = fichero_json ).
  ENDMETHOD.
  METHOD exist.
    CLEAR si.
    SELECT SINGLE @abap_true FROM  zparametros
      INTO @si
     WHERE clave  = @clave
       AND campo  = @campo
       AND valor  = @valor
       AND valor2 = @valor2
       AND valor3 = @valor3
       AND valor4 = @valor4.
  ENDMETHOD.
  METHOD existe.
    DATA l_param TYPE zparametros.

    CLEAR si.

    IF quitar_ceros = 'X'.
      l_param-campo  = campo.
      l_param-valor  = valor.
      l_param-valor2 = valor2.
      l_param-valor3 = valor3.
      l_param-valor4 = valor4.
      DATA(l_par_sc) = claves_sin_ceros(  l_param ).
    ENDIF.

    IF valor2 IS SUPPLIED.
      SELECT SINGLE clave FROM  zparametros
        INTO l_param-clave
       WHERE clave  = clave
         AND campo  = campo
         AND valor  = valor
         AND valor2 = valor2
         AND valor3 = valor3
         AND valor4 = valor4.
      IF sy-subrc = 0.
        si = 'X'.
      ELSE.
        IF quitar_ceros = 'X'.
          SELECT SINGLE clave FROM  zparametros
            INTO l_param-clave
           WHERE clave  = clave
             AND campo  = l_par_sc-campo
             AND valor  = l_par_sc-valor
             AND valor2 = l_par_sc-valor2
             AND valor3 = l_par_sc-valor3
             AND valor4 = l_par_sc-valor4.
          IF sy-subrc = 0.
            si = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      SELECT SINGLE clave FROM  zparametros
        INTO l_param-clave
       WHERE clave = clave
         AND campo = campo
         AND valor = valor.
      IF sy-subrc = 0.
        si = 'X'.
      ELSE.
        IF quitar_ceros = 'X'.
          SELECT SINGLE clave FROM  zparametros
            INTO l_param-clave
           WHERE clave = clave
             AND campo = l_par_sc-campo
             AND valor = l_par_sc-valor.
          IF sy-subrc = 0.
            si = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD existe_i.
    DATA: l_campo_sin_ceros  TYPE zparametros-campo,
          l_valor_sin_ceros  TYPE zparametros-valor,
          l_valor2_sin_ceros TYPE zparametros-valor2.

    CLEAR: si, par.

    IF valor2 IS SUPPLIED.
      READ TABLE i_par INTO par WITH KEY campo = campo
                                         valor = valor
                                         valor2 = valor2.
      IF sy-subrc <> 0.
        IF quitar_ceros = 'X'.
          zcl_ap_string=>quitar_ceros( EXPORTING cadena = campo  CHANGING  salida = l_campo_sin_ceros ).
          zcl_ap_string=>quitar_ceros( EXPORTING cadena = valor  CHANGING  salida = l_valor_sin_ceros ).
          zcl_ap_string=>quitar_ceros( EXPORTING cadena = valor2 CHANGING  salida = l_valor2_sin_ceros ).
          READ TABLE i_par INTO par WITH KEY campo = l_campo_sin_ceros
                                             valor = l_valor_sin_ceros
                                             valor2 = l_valor2_sin_ceros.
        ENDIF.
      ENDIF.
    ELSE.
      READ TABLE i_par INTO par WITH KEY campo = campo
                                         valor = valor.
      IF sy-subrc <> 0.
        IF quitar_ceros = 'X'.
          zcl_ap_string=>quitar_ceros( EXPORTING cadena = campo  CHANGING  salida = l_campo_sin_ceros ).
          zcl_ap_string=>quitar_ceros( EXPORTING cadena = valor  CHANGING  salida = l_valor_sin_ceros ).
          READ TABLE i_par INTO par WITH KEY campo = l_campo_sin_ceros
                                             valor = l_valor_sin_ceros.
        ENDIF.
      ENDIF.
    ENDIF.
    IF NOT par IS INITIAL.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD exit_activa.
    DATA l_param TYPE zparametros.

    CLEAR activa.
    SELECT atributo1
      FROM zparametros
      WHERE clave  = @clave
        AND campo  = @exit
        AND valor  = @usuario
        AND valor2 = ''
      ORDER BY PRIMARY KEY
      INTO @l_param-atributo1
      UP TO 1 ROWS.
    ENDSELECT.
    IF sy-subrc = 0.
      activa = l_param-atributo1.
    ELSE.
      SELECT atributo1
        FROM zparametros
        WHERE clave  = @clave
          AND campo  = @exit
          AND valor  = ''
          AND valor2 = ''
        ORDER BY PRIMARY KEY
        INTO @l_param-atributo1
        UP TO 1 ROWS.
      ENDSELECT.
      IF sy-subrc = 0.
        activa = l_param-atributo1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_atr1.
    DATA: l_campo_sin_ceros  TYPE zparametros-campo,
          l_valor_sin_ceros  TYPE zparametros-valor,
          l_valor2_sin_ceros TYPE zparametros-valor2,
          l_n10              TYPE n LENGTH 10,
          l_ctd              TYPE mengev,
          l_mensaje          TYPE bapi_msg,
          error              TYPE string.

    CLEAR par.
    READ TABLE i_par INTO par WITH KEY campo = campo
                                       valor = valor
                                       valor2 = valor2.
    IF sy-subrc <> 0.
      IF quitar_ceros = 'X'.
        zcl_ap_string=>quitar_ceros( EXPORTING cadena = campo  CHANGING  salida = l_campo_sin_ceros ).
        zcl_ap_string=>quitar_ceros( EXPORTING cadena = valor  CHANGING  salida = l_valor_sin_ceros ).
        zcl_ap_string=>quitar_ceros( EXPORTING cadena = valor2 CHANGING  salida = l_valor2_sin_ceros ).
        READ TABLE i_par INTO par WITH KEY campo = l_campo_sin_ceros
                                           valor = l_valor_sin_ceros
                                           valor2 = l_valor2_sin_ceros.
      ENDIF.
    ENDIF.
    IF NOT par IS INITIAL.
      atributo1 = par-atributo1.

      IF n10 = 'X'.
        l_n10 = atributo1.
        zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_n10 ).
        atributo1 = l_n10.
      ELSEIF ctd = 'X'.
        zcl_ap_string=>string2ctd( EXPORTING ctd_texto = atributo1
                                   IMPORTING cantidad  = l_ctd
                                             mensaje   = l_mensaje ).
        IF l_mensaje IS INITIAL.
          atributo1 = l_ctd.
        ELSE.
          CONCATENATE 'Valor no es cantidad en' campo valor l_mensaje INTO error SEPARATED BY space.
          IF mostrar_error = 'X'.
            MESSAGE error TYPE 'E'.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      CONCATENATE 'No existe entrada ZPARAMETROS' campo valor INTO error SEPARATED BY space.
      IF mostrar_error = 'X'.
        MESSAGE error TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_atr1_usr.
    CLEAR error.
    READ TABLE i_par INTO par WITH KEY campo = campo
                                       valor = valor
                                       valor2 = uname.
    IF sy-subrc = 0.
      atributo1 = par-atributo1.
    ELSE.
      READ TABLE i_par INTO par WITH KEY campo = campo
                                         valor = valor
                                         valor2 = ''.
      IF sy-subrc = 0.
        atributo1 = par-atributo1.
      ELSE.
        IF mostrar_error = 'X'.
          MESSAGE e398(00) WITH 'No existe entrada ZPARAMETROS' campo valor ''.
        ELSE.
          CONCATENATE 'No existe entrada ZPARAMETROS' campo valor INTO error SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_atr2.
    CLEAR error.
    READ TABLE i_par INTO par WITH KEY campo = campo
                                       valor = valor
                                       valor2 = valor2.
    IF sy-subrc = 0.
      atributo2 = par-atributo2.
    ELSE.
      IF mostrar_error = 'X'.
        MESSAGE e398(00) WITH 'No existe entrada ZPARAMETROS' campo valor ''.
      ELSE.
        CONCATENATE 'No existe entrada ZPARAMETROS' campo valor INTO error SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_atr3.
    DATA: l_campo_sin_ceros  TYPE zparametros-campo,
          l_valor_sin_ceros  TYPE zparametros-valor,
          l_valor2_sin_ceros TYPE zparametros-valor2,
          l_n10              TYPE n LENGTH 10,
          l_ctd              TYPE mengev,
          l_mensaje          TYPE bapi_msg,
          error              TYPE string.

    CLEAR par.
    READ TABLE i_par INTO par WITH KEY campo = campo
                                       valor = valor
                                       valor2 = valor2.
    IF sy-subrc <> 0.
      IF quitar_ceros = 'X'.
        zcl_ap_string=>quitar_ceros( EXPORTING cadena = campo  CHANGING  salida = l_campo_sin_ceros ).
        zcl_ap_string=>quitar_ceros( EXPORTING cadena = valor  CHANGING  salida = l_valor_sin_ceros ).
        zcl_ap_string=>quitar_ceros( EXPORTING cadena = valor2 CHANGING  salida = l_valor2_sin_ceros ).
        READ TABLE i_par INTO par WITH KEY campo = l_campo_sin_ceros
                                           valor = l_valor_sin_ceros
                                           valor2 = l_valor2_sin_ceros.
      ENDIF.
    ENDIF.
    IF NOT par IS INITIAL.
      atributo3 = par-atributo3.

      IF n10 = 'X'.
        l_n10 = atributo3.
        zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_n10 ).
        atributo3 = l_n10.
      ELSEIF ctd = 'X'.
        zcl_ap_string=>string2ctd( EXPORTING ctd_texto = atributo3
                                   IMPORTING cantidad  = l_ctd
                                             mensaje   = l_mensaje ).
        IF l_mensaje IS INITIAL.
          atributo3 = l_ctd.
        ELSE.
          CONCATENATE 'Valor no es cantidad en' campo valor l_mensaje INTO error SEPARATED BY space.
          IF mostrar_error = 'X'.
            MESSAGE error TYPE 'E'.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      CONCATENATE 'No existe entrada ZPARAMETROS' campo valor INTO error SEPARATED BY space.
      IF mostrar_error = 'X'.
        MESSAGE error TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_atr_rango.
    LOOP AT i_par_rangos ASSIGNING FIELD-SYMBOL(<param_rango>).
      IF     campo  IN <param_rango>-campo
         AND valor  IN <param_rango>-valor
         AND valor2 IN <param_rango>-valor2.
        atributo1 = <param_rango>-atributo1.
        atributo2 = <param_rango>-atributo2.
        atributo3 = <param_rango>-atributo3.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_atributo1.
    DATA: l_ok      TYPE abap_bool,
          l_param   TYPE zparametros,
          l_n10     TYPE n LENGTH 10,
          l_ctd     TYPE mengev,
          l_mensaje TYPE bapi_msg.

    SELECT SINGLE (atributo) FROM  zparametros
      INTO atributo1
     WHERE clave  = clave
       AND campo  = campo
       AND valor  = valor
       AND valor2 = valor2
       AND valor3 = valor3
       AND valor4 = valor4.
    IF sy-subrc = 0.
      l_ok = 'X'.
    ELSEIF quitar_ceros = 'X'.
      IF quitar_ceros = 'X'.
        l_param-campo  = campo.
        l_param-valor  = valor.
        l_param-valor2 = valor2.
        l_param-valor3 = valor3.
        l_param-valor4 = valor4.
        DATA(l_par_sc) = claves_sin_ceros(  l_param ).
      ENDIF.
      SELECT SINGLE (atributo) FROM  zparametros
        INTO atributo1
       WHERE clave  = clave
         AND campo  = l_par_sc-campo
         AND valor  = l_par_sc-valor
         AND valor2 = l_par_sc-valor2
         AND valor3 = l_par_sc-valor3
         AND valor4 = l_par_sc-valor4.
      IF sy-subrc = 0.
        l_ok = 'X'.
      ENDIF.
    ENDIF.

    IF l_ok = 'X'.
      IF n10 = 'X'.
        l_n10 = atributo1.
        zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_n10 ).
        atributo1 = l_n10.
      ELSEIF ctd = 'X'.
        zcl_ap_string=>string2ctd( EXPORTING ctd_texto = atributo1
                                   IMPORTING cantidad  = l_ctd
                                             mensaje   = l_mensaje ).
        IF l_mensaje IS INITIAL.
          atributo1 = l_ctd.
          CONDENSE atributo1 NO-GAPS.
        ENDIF.
      ENDIF.
    ELSEIF error_si_no_existe = abap_true.
      MESSAGE |No hay definido parámetro para { clave } { campo } { valor } { valor2 } { valor3 } { valor4 }| TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD get_atributo1_usr.
    SELECT atributo1
      FROM zparametros
      WHERE clave  = @clave
        AND campo  = @campo
        AND valor  = @valor
        AND valor2 = @uname
      ORDER BY PRIMARY KEY
      INTO @atributo1
      UP TO 1 ROWS.
    ENDSELECT.
    IF sy-subrc <> 0.
      SELECT atributo1
        FROM zparametros
        WHERE clave  = @clave
          AND campo  = @campo
          AND valor  = @valor
          AND valor2 = ''
        ORDER BY PRIMARY KEY
        INTO @atributo1
        UP TO 1 ROWS.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.
  METHOD get_atributo2.
    atributo2 = get_atributo1(
        clave              = clave
        campo              = campo
        valor              = valor
        valor2             = valor2
        valor3             = valor3
        valor4             = valor4
        n10                = n10
        ctd                = ctd
        quitar_ceros       = quitar_ceros
        error_si_no_existe = error_si_no_existe
        atributo           = 'ATRIBUTO2' ).
  ENDMETHOD.
  METHOD get_lista.
    DATA l_ok TYPE c LENGTH 1.

    CLEAR: error, par, lista.
    CASE columna.
      WHEN 'VALOR'.
        LOOP AT i_par INTO par WHERE campo = campo.
          l_ok = 'X'.
          __add_lista lista par-valor.
        ENDLOOP.
      WHEN 'VALOR2'.
        LOOP AT i_par INTO par WHERE campo = campo AND valor = valor.
          l_ok = 'X'.
          __add_lista lista par-valor2.
        ENDLOOP.
      WHEN 'VALOR3'.
        LOOP AT i_par INTO par WHERE campo = campo AND valor = valor AND valor2 = valor2.
          l_ok = 'X'.
          __add_lista lista par-valor3.
        ENDLOOP.
      WHEN 'VALOR4'.
        LOOP AT i_par INTO par WHERE campo = campo AND valor = valor AND valor2 = valor2 AND valor3 = valor3.
          l_ok = 'X'.
          __add_lista lista par-valor4.
        ENDLOOP.
      WHEN 'ATRIBUTO1'.
        LOOP AT i_par INTO par WHERE campo = campo AND valor = valor AND valor2 = valor2 AND valor3 = valor3 AND valor4 = valor4.
          l_ok = 'X'.
          __add_lista lista par-atributo1.
        ENDLOOP.
    ENDCASE.
    IF l_ok IS INITIAL.
      CONCATENATE 'No existe entrada ZPARAMETROS' campo INTO error SEPARATED BY space.
      IF mostrar_error = 'X'.
        MESSAGE error TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_lista_atr1.
    DATA: error              TYPE string,
          l_n10              TYPE n LENGTH 10,
          l_campo_sin_ceros  TYPE zparametros-campo,
          l_valor_sin_ceros  TYPE zparametros-valor,
          l_valor2_sin_ceros TYPE zparametros-valor2,
          l_mensaje          TYPE bapi_msg,
          l_ctd              TYPE mengev.

    CLEAR: error, par, lista.
    LOOP AT i_par INTO par WHERE campo = campo.
      __add_lista lista par-atributo1.
    ENDLOOP.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe entrada ZPARAMETROS' campo INTO error SEPARATED BY space.
      IF mostrar_error = 'X'.
        MESSAGE error TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_parametro.
    DATA: l_ok    TYPE abap_bool,
          l_param TYPE zparametros.

    CLEAR parametros.
    SELECT SINGLE * FROM  zparametros
      INTO parametros
     WHERE clave  = clave
       AND campo  = campo
       AND valor  = valor
       AND valor2 = valor2
       AND valor3 = valor3
       AND valor4 = valor4.
    IF sy-subrc = 0.
      l_ok = 'X'.
    ELSEIF quitar_ceros = 'X'.
      IF quitar_ceros = 'X'.
        l_param-campo  = campo.
        l_param-valor  = valor.
        l_param-valor2 = valor2.
        l_param-valor3 = valor3.
        l_param-valor4 = valor4.
        DATA(l_par_sc) = claves_sin_ceros(  l_param ).
      ENDIF.
      SELECT SINGLE * FROM  zparametros
        INTO parametros
         WHERE clave  = clave
           AND campo  = l_par_sc-campo
           AND valor  = l_par_sc-valor
           AND valor2 = l_par_sc-valor2
           AND valor3 = l_par_sc-valor3
           AND valor4 = l_par_sc-valor4.
      IF sy-subrc = 0.
        l_ok = 'X'.
      ENDIF.
    ENDIF.

    IF error_si_no_existe = abap_true AND l_ok = abap_false.
      MESSAGE |No hay definido parámetro para { clave } { campo } { valor } { valor2 } { valor3 } { valor4 }| TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD get_rango_tabla_campo.
    DATA: l_campo TYPE c LENGTH 40,
          l_linea TYPE rstt_s_range_string.

    FIELD-SYMBOLS <fs> TYPE any.

    CLEAR r_rango.
    LOOP AT i_par INTO par.
      IF NOT ( par-campo = campo OR todo = 'X' ).
        CONTINUE.
      ENDIF.

      IF valor IS SUPPLIED.
        IF valor <> par-valor.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF valor2 IS SUPPLIED.
        IF valor2 <> par-valor2.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF atributo1 IS SUPPLIED.
        IF atributo1 <> par-atributo1.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF NOT filtro_por_campo IS INITIAL.
        CONCATENATE 'PAR-' filtro_por_campo INTO l_campo.
        ASSIGN (l_campo) TO <fs>.
        IF sy-subrc = 0.
          IF <fs> <> valor_filtro_por_campo.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR l_linea.

      CASE campo_rango.
        WHEN 'CAMPO'.
          l_linea-low = par-campo.
        WHEN 'ATRIBUTO1'.
          l_linea-low = par-atributo1.
        WHEN 'ATRIBUTO2'.
          l_linea-low = par-atributo2.
        WHEN 'VALOR'.
          l_linea-low = par-valor.
        WHEN 'VALOR2'.
          l_linea-low = par-valor2.
      ENDCASE.

      IF option = '*'.
        IF l_linea-low CS '*'.
          l_linea-option = 'CP'.
        ELSE.
          l_linea-option = 'EQ'.
        ENDIF.
      ELSE.
        l_linea-option = option.
      ENDIF.

      l_linea-sign = sign.

      COLLECT l_linea INTO r_rango.
    ENDLOOP.

    IF r_rango IS INITIAL AND error_si_vacio <> ''.
      CLEAR l_linea.
      l_linea-option = 'EQ'.
      l_linea-sign   = 'I'.
      IF error_si_vacio = 'X'.
        l_linea-low = '?'.
      ELSE.
        IF option = '*' AND error_si_vacio CS '*'.
          l_linea-option = 'CP'.
        ENDIF.

        l_linea-low = error_si_vacio.
      ENDIF.
      COLLECT l_linea INTO r_rango.
    ENDIF.
  ENDMETHOD.
  METHOD get_rango_tabla_campo_n10.
    DATA: r_rango     TYPE rstt_t_range_string,
          l_linea_n10 TYPE lxhme_range_n10.

    IF valor IS SUPPLIED.
      IF valor2 IS SUPPLIED.
        r_rango = get_rango_tabla_campo( campo = campo
                                         valor = valor
                                         valor2 = valor2
                                         campo_rango = campo_rango
                                         option = option
                                         sign   = sign
                                         todo   = todo
                                         error_si_vacio = error_si_vacio
                                         filtro_por_campo = filtro_por_campo
                                         valor_filtro_por_campo = valor_filtro_por_campo ).
      ELSE.
        r_rango = get_rango_tabla_campo( campo = campo
                                         valor = valor
                                         campo_rango = campo_rango
                                         option = option
                                         sign   = sign
                                         todo   = todo
                                         error_si_vacio = error_si_vacio
                                         filtro_por_campo = filtro_por_campo
                                         valor_filtro_por_campo = valor_filtro_por_campo ).
      ENDIF.
    ELSE.
      r_rango = get_rango_tabla_campo( campo = campo
                                       campo_rango = campo_rango
                                       option = option
                                       sign   = sign
                                       todo   = todo
                                       error_si_vacio = error_si_vacio
                                       filtro_por_campo = filtro_por_campo
                                       valor_filtro_por_campo = valor_filtro_por_campo ).
    ENDIF.

    LOOP AT r_rango ASSIGNING FIELD-SYMBOL(<linea>).
      CLEAR l_linea_n10.
      MOVE-CORRESPONDING <linea> TO l_linea_n10.
      IF NOT <linea>-low IS INITIAL.
        zcl_ap_string=>poner_ceros_c( CHANGING  cadena =  l_linea_n10-low ).
      ENDIF.
      IF NOT <linea>-high IS INITIAL.
        zcl_ap_string=>poner_ceros_c( CHANGING  cadena =  l_linea_n10-high ).
      ENDIF.
      APPEND l_linea_n10 TO r_rango_n10.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_rango_tabla_campo_n18.
    DATA: r_rango     TYPE rstt_t_range_string,
          l_linea_n18 TYPE range_s_matnr.

    IF valor IS SUPPLIED.
      IF valor2 IS SUPPLIED.
        r_rango = get_rango_tabla_campo( campo = campo
                                         valor = valor
                                         valor2 = valor2
                                         campo_rango = campo_rango
                                         option = option
                                         sign   = sign
                                         todo   = todo
                                         error_si_vacio = error_si_vacio
                                         filtro_por_campo = filtro_por_campo
                                         valor_filtro_por_campo = valor_filtro_por_campo ).
      ELSE.
        r_rango = get_rango_tabla_campo( campo = campo
                                         valor = valor
                                         campo_rango = campo_rango
                                         option = option
                                         sign   = sign
                                         todo   = todo
                                         error_si_vacio = error_si_vacio
                                         filtro_por_campo = filtro_por_campo
                                         valor_filtro_por_campo = valor_filtro_por_campo ).
      ENDIF.
    ELSE.
      r_rango = get_rango_tabla_campo( campo = campo
                                       campo_rango = campo_rango
                                       option = option
                                       sign   = sign
                                       todo   = todo
                                       error_si_vacio = error_si_vacio
                                       filtro_por_campo = filtro_por_campo
                                       valor_filtro_por_campo = valor_filtro_por_campo ).
    ENDIF.

    LOOP AT r_rango ASSIGNING FIELD-SYMBOL(<linea>).
      CLEAR l_linea_n18.
      MOVE-CORRESPONDING <linea> TO l_linea_n18.
      IF NOT <linea>-low IS INITIAL.
        zcl_ap_string=>poner_ceros_c( CHANGING  cadena =  l_linea_n18-low ).
      ENDIF.
      IF NOT <linea>-high IS INITIAL.
        zcl_ap_string=>poner_ceros_c( CHANGING  cadena =  l_linea_n18-high ).
      ENDIF.
      APPEND l_linea_n18 TO r_rango_n18.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_tabla_campo.
    CLEAR i_tabla.
    LOOP AT i_par INTO par WHERE campo = campo.

      IF valor IS SUPPLIED.
        IF valor <> par-valor.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF valor2 IS SUPPLIED.
        IF valor2 <> par-valor2.
          CONTINUE.
        ENDIF.
      ENDIF.

      APPEND par TO i_tabla.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_tabla_formateada.
    DATA l_campo TYPE string.

    CLEAR: message, tabla.
    IF me->ztclave_param IS INITIAL.
      SELECT SINGLE * FROM ztclave_param
        INTO me->ztclave_param
       WHERE clave_param = clave.
      IF sy-subrc <> 0.
        message = 'Debe parametrizar campos primero'.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT fieldname FROM dd03l
      INTO TABLE @DATA(i_campos)
     WHERE     tabname    = 'ZPARAMETROS'
       AND     position  >= '0003'  " Sólo queremos los campos útiles
       AND NOT fieldname IN ( 'AEDAT', 'AENAM', 'AEZET' )
     ORDER BY position.

    DATA(i_campos_final) = zcl_ap_dev=>get_fieldcatalog_tabla_alv( tabla ).

    LOOP AT i_par ASSIGNING FIELD-SYMBOL(<param>).
      APPEND INITIAL LINE TO tabla ASSIGNING FIELD-SYMBOL(<tabla>).
      LOOP AT i_campos ASSIGNING FIELD-SYMBOL(<campo>).
        ASSIGN COMPONENT <campo> OF STRUCTURE ztclave_param TO FIELD-SYMBOL(<columna>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <columna> IS INITIAL OR <columna> = 'INVISIBLE'.
          l_campo = <campo>.
        ELSE.
          l_campo = to_upper( <columna> ).
          CONDENSE l_campo.
          REPLACE ALL OCCURRENCES OF ` ` IN l_campo WITH '_'.
        ENDIF.
        ASSIGN COMPONENT l_campo OF STRUCTURE <tabla> TO FIELD-SYMBOL(<dato>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT <campo> OF STRUCTURE <param> TO FIELD-SYMBOL(<valor>).

* Si el campo destino tiene rutina de conversión la aplicamos
        ASSIGN i_campos_final[ fieldname = l_campo ]-edit_mask TO FIELD-SYMBOL(<mask>).
        IF <mask>(2) = '=='.
          DATA(l_funcion) = |CONVERSION_EXIT_{ <mask>+2 }_INPUT|.
          TRY.
              CALL FUNCTION l_funcion
                EXPORTING
                  input  = <valor>
                IMPORTING
                  output = <dato>.
            CATCH cx_root INTO DATA(o_root). "#EC *
              message = |Error formateando campo { l_campo } { o_root->get_text( ) }|.
          ENDTRY.
        ELSEIF i_campos_final[ fieldname = l_campo ]-datatype = 'DATS'.
          <dato> = zcl_ap_fechas=>string2fecha( <valor> ).
        ELSE.
          <dato> = <valor>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD inicio.
    DATA l_param_rango TYPE t_param_rangos.

    IF NOT fichero_json IS INITIAL.
      zcl_ap_ficheros=>leer_xstring( EXPORTING fichero = fichero_json get_string = 'X'
                                     IMPORTING string = DATA(l_json) ).
      zcl_ap_segw=>set_json( EXPORTING json = l_json
                              IMPORTING datos = i_par ).
    ENDIF.
    IF i_par IS INITIAL.
      IF campo IS INITIAL.
        SELECT * FROM zparametros
          INTO TABLE i_par
         WHERE clave = clave
          ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM zparametros
          INTO TABLE i_par
         WHERE clave = clave
           AND campo = campo
          ORDER BY PRIMARY KEY.
      ENDIF.
    ENDIF.

    IF rangos = 'X'.
      CLEAR i_par_rangos.
      LOOP AT i_par ASSIGNING FIELD-SYMBOL(<par>).
        CLEAR l_param_rango.
        l_param_rango-campo  = zcl_ap_string=>lista2rango( <par>-campo ).
        l_param_rango-valor  = zcl_ap_string=>lista2rango( <par>-valor ).
        l_param_rango-valor2 = zcl_ap_string=>lista2rango( <par>-valor2 ).
        DELETE l_param_rango-valor2 WHERE low = '¿¿¿¿????'.

        l_param_rango-atributo1 = <par>-atributo1.
        l_param_rango-atributo2 = <par>-atributo2.
        l_param_rango-atributo3 = <par>-atributo3.
        APPEND l_param_rango TO i_par_rangos.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD mantenimiento.
    DATA: l_campo TYPE zparametros-campo,
          l_valor TYPE zparametros-valor.

    IF campo IS INITIAL.
      l_campo = me->campo.
    ELSE.
      l_campo = campo.
    ENDIF.

    l_valor = valor.

    IF alv IS INITIAL.

      SET PARAMETER ID 'ZPA' FIELD clave.
      SET PARAMETER ID 'ZP2' FIELD l_campo.
      SET PARAMETER ID 'ZP3' FIELD l_valor.

      IF l_campo IS INITIAL.
        IF l_valor IS INITIAL.
          zcl_ap_utils=>mantener_tabla( tabla = 'ZV_PARAMETROS'
                                        action = action
                                        campo_filtro = 'CLAVE'
                                        op_filtro = 'EQ'
                                        valor_filtro = clave ).
        ELSE.
          zcl_ap_utils=>mantener_tabla( tabla = 'ZV_PARAMETROS'
                                        action = action
                                        campo_filtro = 'CLAVE'
                                        op_filtro = 'EQ'
                                        valor_filtro = clave
                                        campo_filtro2 = 'VALOR'
                                        op_filtro2 = 'EQ'
                                        valor_filtro2 = l_valor ).
        ENDIF.
      ELSE.
        IF l_valor IS INITIAL.
          zcl_ap_utils=>mantener_tabla( tabla = 'ZV_PARAMETROS'
                                        action = action
                                        campo_filtro = 'CLAVE'
                                        op_filtro = 'EQ'
                                        valor_filtro = clave
                                        campo_filtro2 = 'CAMPO'
                                        op_filtro2 = 'EQ'
                                        valor_filtro2 = l_campo ).
        ELSE.
          zcl_ap_utils=>mantener_tabla( tabla = 'ZV_PARAMETROS'
                                        action = action
                                        campo_filtro = 'CLAVE'
                                        op_filtro = 'EQ'
                                        valor_filtro = clave
                                        campo_filtro2 = 'CAMPO'
                                        op_filtro2 = 'EQ'
                                        valor_filtro2 = l_campo
                                        campo_filtro3 = 'VALOR'
                                        op_filtro3 = 'EQ'
                                        valor_filtro3 = l_valor ).
        ENDIF.
      ENDIF.

      SET PARAMETER ID 'ZPA' FIELD ''.
      SET PARAMETER ID 'ZP2' FIELD ''.
      SET PARAMETER ID 'ZP3' FIELD ''.
      CLEAR sy-ucomm.
    ELSE.
      IF action = 'U'.
        SUBMIT zparametros
               AND RETURN
               WITH p_clave = clave
               WITH p_campo = l_campo
               WITH p_valor = l_valor
               WITH p_vis = ''.
      ELSEIF action = 'S'.
        SUBMIT zparametros
               AND RETURN
               WITH p_clave = clave
               WITH p_campo = l_campo
               WITH p_valor = l_valor
               WITH p_vis = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD mantenimiento_st.
    IF alv IS INITIAL.
      SET PARAMETER ID 'ZPA' FIELD clave.
      SET PARAMETER ID 'ZP2' FIELD campo.
      SET PARAMETER ID 'ZP3' FIELD valor.

      IF campo IS INITIAL AND valor IS INITIAL.
        zcl_ap_utils=>mantener_tabla( tabla = 'ZV_PARAMETROS'
                                      campo_filtro = 'CLAVE'
                                      op_filtro = 'EQ'
                                      valor_filtro = clave
                                      action = action ).
      ELSEIF valor IS INITIAL.
        zcl_ap_utils=>mantener_tabla( tabla = 'ZV_PARAMETROS'
                                      campo_filtro = 'CLAVE'
                                      op_filtro = 'EQ'
                                      valor_filtro = clave
                                      campo_filtro2 = 'CAMPO'
                                      op_filtro2 = 'EQ'
                                      valor_filtro2 = campo
                                      action = action ).
      ELSEIF campo IS INITIAL.
        zcl_ap_utils=>mantener_tabla( tabla = 'ZV_PARAMETROS'
                                      campo_filtro = 'CLAVE'
                                      op_filtro = 'EQ'
                                      valor_filtro = clave
                                      campo_filtro2 = 'VALOR'
                                      op_filtro2 = 'EQ'
                                      valor_filtro2 = valor
                                      action = action ).
      ENDIF.

      SET PARAMETER ID 'ZPA' FIELD ''.
      SET PARAMETER ID 'ZP2' FIELD ''.
      SET PARAMETER ID 'ZP3' FIELD ''.
    ELSE.
      IF action = 'U'.
        SUBMIT zparametros
               AND RETURN
               WITH p_clave = clave
               WITH p_campo = campo
               WITH p_valor = valor
               WITH p_vis = ''.
      ELSEIF action = 'S'.
        SUBMIT zparametros
               AND RETURN
               WITH p_clave = clave
               WITH p_campo = campo
               WITH p_valor = valor
               WITH p_vis = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD matchcode.
    DATA: o_popup TYPE REF TO zcl_ap_matchcode_z,
          l_param TYPE zparametros.

    o_popup = NEW #(
        tabname = 'ZPARAMETROS' ).

    o_popup->add_field( field = 'VALOR' selectflag = 'X' ).
    o_popup->add_field( field = 'ATRIBUTO1' ).

    SELECT valor atributo1 atributo2 FROM  zparametros
      INTO CORRESPONDING FIELDS OF l_param
     WHERE clave = clave
       AND campo = campo.

      IF atributo2 = '' OR atributo2 = l_param-atributo2.
        o_popup->add_valor( l_param-valor ).
        o_popup->add_valor( l_param-atributo1 ).
      ENDIF.
    ENDSELECT.

    o_popup->matchcode( EXPORTING field   = 'VALOR'
                        CHANGING  valor   = valor ).
  ENDMETHOD.
  METHOD send_mail.
    DATA: o_mail    TYPE REF TO zcl_ap_envio_mail,
          l_subject TYPE string,
          l_ok      TYPE c LENGTH 1.

    IF NOT line_exists( i_par[ campo = campo ] ).
      RETURN.
    ENDIF.

    o_mail = NEW #(
        usar_clases = 'X' ).

    IF subject IS INITIAL.
      l_subject = texto.
    ELSE.
      l_subject = subject.
    ENDIF.

    o_mail->set_text( texto ).

    LOOP AT i_par INTO par WHERE campo = campo.
      IF par-atributo1 IS INITIAL.
        o_mail->add_destinatario( destinatario = par-valor forzar_mail_externo = forzar_mail_externo ).
        l_ok = 'X'.
      ELSEIF NOT par-atributo1 IS INITIAL.
        o_mail->add_destinatario( destinatario = par-atributo1 forzar_mail_externo = forzar_mail_externo ).
        l_ok = 'X'.
      ENDIF.
    ENDLOOP.

    IF l_ok = 'X'.
      o_mail->envio_mail( subject = l_subject
                          urgente = urgente
                          html    = html
                          commit  = commit
                          forzar_mail_externo = forzar_mail_externo
                          confirmacion_lectura = confirmacion_lectura
                          emisor  = emisor ).
    ENDIF.

    o_mail->free( ).
  ENDMETHOD.