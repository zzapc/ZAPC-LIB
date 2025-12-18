
class ZCL_AP_EXITS definition
  public
  create public .

public section.

  data O_LOG type ref to ZCL_AP_LOG .
  data ID type ZAP_EXITS-ID .
  data GUARDAR_LOG type ABAP_BOOL .
  data ZAP_EXITS type ZAP_EXITS .
  data I_CONDICIONES type ZAP_EXITS_PAR_T .
  data I_VARIABLES type ZAP_EXITS_PAR_T .
  data I_CONDICIONES_AGRUP type ZAP_EXITS_PAR_T .
  data UNAME type SY-UNAME .
  data CLAVE type STRING .
  data COND_OR type XFELD .

  methods CONSTRUCTOR
    importing
      !EXIT type ANY
      !INCLUDE type ANY default ''
      !CLAVE type ANY default ''
      !FORM type ANY default ''
      !UNAME type SY-UNAME default SY-UNAME .
  methods ACTIVA
    importing
      !CONDICION type ANY default ''
      !VALOR_CLAVE2 type ANY default ''
    returning
      value(ACTIVA) type ABAP_BOOL .
  methods GET_VARIABLE
    importing
      !CLAVE type ANY
      !CLAVE2 type ANY optional
      !CLAVE3 type ANY optional
      !CTD type ABAP_BOOL default ''
      !OBLIGATORIO type ABAP_BOOL default 'X'
    returning
      value(VALOR) type ZAP_EXITS_PAR-VALOR .
  class-methods EXIT_ACTIVA
    importing
      !EXIT type ANY optional
      !UNAME type SY-UNAME default SY-UNAME
      !CLAVE type ANY default ''
      !CONDICION type ANY default ''
      !VALOR_CLAVE2 type ANY default ''
    preferred parameter EXIT
    returning
      value(ACTIVA) type ABAP_BOOL .
  methods LOG
    importing
      !P1 type ANY
      !P2 type ANY optional
      !P3 type ANY optional
      !P4 type ANY optional
      !P5 type ANY optional
      !P6 type ANY optional
      !P7 type ANY optional
      !P8 type ANY optional
      !P9 type ANY optional
      !P10 type ANY optional
      !P11 type ANY optional
      !P12 type ANY optional
      !P13 type ANY optional
      !P14 type ANY optional
      !SIEMPRE type ABAP_BOOL default ''
      !MSGTY type SY-MSGTY default 'E'
      !MSGID type ANY default ''
      !MSGNO type ANY default ''
      !MSGV1 type ANY default ''
      !MSGV2 type ANY default ''
      !MSGV3 type ANY default ''
      !MSGV4 type ANY default ''
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods LOG_ST
    importing
      !EXIT type ANY optional
      !UNAME type SY-UNAME default SY-UNAME
      !CLAVE type ANY default ''
      !P1 type ANY optional
      !P2 type ANY optional
      !P3 type ANY optional
      !P4 type ANY optional
      !P5 type ANY optional
      !P6 type ANY optional
      !MSGTY type ANY default 'E'
      !MSGID type ANY default ''
      !MSGNO type ANY default ''
      !MSGV1 type ANY default ''
      !MSGV2 type ANY default ''
      !MSGV3 type ANY default ''
      !MSGV4 type ANY default ''
    preferred parameter EXIT
    returning
      value(MESSAGE) type BAPI_MSG .
  methods GET_LISTA_VARIABLE
    importing
      !CLAVE type ANY
      !OBLIGATORIO type ABAP_BOOL default 'X'
    returning
      value(LISTA) type STRING .
  methods CUMPLE_COND
    returning
      value(SI) type ABAP_BOOL .
  methods EVAL_COND
    importing
      !VALOR type ANY
      !CLAVE type ANY
    returning
      value(CUMPLE) type ABAP_BOOL .
  class-methods GET_RANGO_VARIABLES_ST
    importing
      !EXIT type ANY
      !CLAVE type ANY default ''
      !CLAVE2 type ANY default ''
      !CLAVE3 type ANY default ''
      !CAMPO_RANGO type ANY default 'CLAVE2'
      !OPTION type ANY default 'EQ'
      !SIGN type ANY default 'I'
      !TODO type ABAP_BOOL default ''
      !ERROR_SI_VACIO type ANY default ''
      !FILTRO_POR_CAMPO type ANY default ''
      !VALOR_FILTRO_POR_CAMPO type ANY default ''
    returning
      value(R_RANGO) type RSTT_T_RANGE_STRING .
  class-methods CUMPLE_COND_CLAVE2
    importing
      !EXIT type ANY
      !CONDICION type ANY
      !VALOR type ANY
      !SIN_CEROS type ANY default ''
    returning
      value(CUMPLE) type ABAP_BOOL .
  methods EXISTE_VARIABLE
    importing
      !CLAVE type ANY
      !CLAVE2 type ANY optional
      !CLAVE3 type ANY optional
      !QUITAR_CEROS type ABAP_BOOL default ''
    returning
      value(SI) type ABAP_BOOL .
  methods EXISTE_CONDICION
    importing
      !CLAVE type ANY
      !CLAVE2 type ANY optional
      !CLAVE3 type ANY optional
      !QUITAR_CEROS type ABAP_BOOL default ''
    returning
      value(SI) type ABAP_BOOL .
  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_EXITS definition
class ZCL_AP_EXITS implementation.
  METHOD activa.
    IF zap_exits-activa = 'X'.
      activa = zap_exits-activa.
    ELSEIF zap_exits-activa = 'U'.
      IF zap_exits-usuarios_activos IS INITIAL.
        log( p1 = 'Exit'(exi) p2 = id p3 = 'activa dependiente de usuario, pero lista usuarios vacía'(adu) msgty = 'W' ).
      ELSEIF zcl_ap_lista=>es_elemento( lista = zap_exits-usuarios_activos elemento = uname ) = 'X'.
        activa = 'X'.
        log( p1 = 'Exit'(exi) p2 = id p3 = 'activa para los usuarios'(apu) p4 = zap_exits-usuarios_activos msgty = 'S' ).
      ELSE.
        log( p1 = 'Exit'(exi) p2 = id p3 = 'activa SÓLO para los usuarios'(asu) p4 = zap_exits-usuarios_activos msgty = 'E' ).
      ENDIF.
    ELSE.
      log( p1 = 'Exit'(exi) p2 = id p3 = 'inactiva'(ina) msgty = 'W' ).
    ENDIF.

    IF activa = 'X' AND NOT zap_exits-usuarios_no_exit IS INITIAL.
      IF zcl_ap_lista=>es_elemento( lista = zap_exits-usuarios_no_exit elemento = uname ) = 'X'.
        CLEAR activa.
        log( p1 = 'Exit'(exi) p2 = id p3 = 'NO activa para usuarios'(nau) p4 = zap_exits-usuarios_no_exit  msgty = 'E' ).
      ENDIF.
    ENDIF.

    IF activa = 'X' AND NOT zap_exits-ffin IS INITIAL.
      IF NOT ( zap_exits-ffin >= sy-datum AND zap_exits-finicio <= sy-datum ).
        log( p1 = 'Exit'(exi) p2 = id p3 = 'no activa a fecha de hoy' p4 = 'Sólo desde' p5 = zap_exits-finicio p6 = 'a' p7 = zap_exits-ffin msgty = 'S' ).
        CLEAR activa.
      ENDIF.
    ENDIF.

    IF activa = 'X'.
      IF NOT condicion IS INITIAL.
        IF NOT line_exists( i_condiciones[ clave = condicion clave2 = valor_clave2 ] ).
          IF line_exists( i_condiciones[ clave = condicion  ] ). " Sólo desactivamos si hay alguna entrada para la condición
            CLEAR activa.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF activa = 'X' AND NOT zap_exits-usuarios_break IS INITIAL.
      IF zcl_ap_lista=>es_elemento( lista = zap_exits-usuarios_break elemento = uname ) = 'X'.
        BREAK-POINT.                                          "#EC *
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    DATA l_report TYPE sy-cprog.

    l_report = exit.

    IF NOT include IS INITIAL.
      CONCATENATE l_report include INTO l_report SEPARATED BY '_INC_'.
    ENDIF.

    IF NOT form IS INITIAL.
      CONCATENATE l_report form INTO l_report SEPARATED BY '_FORM_'.
    ENDIF.
    o_log = NEW #(
      object                = 'EXITS'
      report                = l_report
      clave                 = clave
      guardar_tabla_interna = 'X' ).

    SELECT SINGLE * FROM zap_exits
    INTO zap_exits
   WHERE id = exit.
    IF sy-subrc <> 0.
      o_log->log( p1 = 'Exit'(exi) p2 = id p3 = 'no definida'(nod) ).
    ENDIF.

    id = exit.
    guardar_log = zap_exits-grabar_log.
    me->uname = uname.
    me->clave = clave.

    SELECT * FROM zap_exits_par
    INTO TABLE i_condiciones
   WHERE id   = id
     AND tipo = 'C'
    ORDER BY PRIMARY KEY.

    i_condiciones_agrup = i_condiciones.
    DELETE ADJACENT DUPLICATES FROM i_condiciones_agrup COMPARING clave.

    SELECT * FROM zap_exits_par
    INTO TABLE i_variables
   WHERE id   = id
     AND tipo = 'V'
    ORDER BY PRIMARY KEY.

    IF line_exists( i_variables[ clave = 'COND_OR' ] ).
      cond_or = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD cumple_cond.
    DATA: l_cond       TYPE zap_exits_par,
          l_valor_cond TYPE string,
          l_cumple     TYPE c LENGTH 1.

    FIELD-SYMBOLS <fs> TYPE any.

    IF i_condiciones IS INITIAL.
      si = 'X'.
    ELSE.
      LOOP AT i_condiciones_agrup INTO l_cond.
        CASE l_cond-clave.
          WHEN OTHERS.
            ASSIGN (l_cond-clave) TO <fs>.
            IF sy-subrc = 0.
              l_valor_cond = <fs>.
            ELSE.
              log( p1 = 'Condición' p2 = l_cond-clave p3 = 'desconocida' ).
            ENDIF.
        ENDCASE.

        l_cumple = eval_cond( clave = l_cond-clave valor = l_valor_cond ).
        IF l_cumple IS INITIAL AND cond_or IS INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF l_cumple = 'X'.
        si = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD cumple_cond_clave2.
    DATA l_valor_sin_ceros TYPE zap_exits_par-clave2.

    CLEAR cumple.
    l_valor_sin_ceros = valor.
    IF sin_ceros = 'X'.
      __quitar_ceros l_valor_sin_ceros.
    ENDIF.

    SELECT SINGLE clave2 FROM zap_exits_par
    INTO l_valor_sin_ceros
   WHERE id    = exit
     AND tipo  = 'C'
     AND clave = condicion
     AND ( clave2 = valor OR clave2 = l_valor_sin_ceros ).
    IF sy-subrc = 0.
      cumple = 'X'.
    ELSE.
      SELECT SINGLE clave2 FROM zap_exits_par " Si no existe la condición, también lo doy por buena
      INTO l_valor_sin_ceros
     WHERE id    = exit
       AND tipo  = 'C'
       AND clave = condicion.
      IF sy-subrc <> 0.
        cumple = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD eval_cond.
    DATA: l_cumple              TYPE c LENGTH 1,
          l_cond2               TYPE zap_exits_par,
          l_string              TYPE string,
          l_no_cumple           TYPE c LENGTH 1,
          l_excluido            TYPE c LENGTH 1,
          l_cumple_no_exclusion TYPE c LENGTH 1.

    IF line_exists( i_condiciones[ clave  = clave
                                   clave2 = valor
                                   clave3 = '' ] ).
      l_cumple = 'X'.
      log( p1 = 'Documento cumple condicion'(dcc) p2 = clave p3 = valor msgty = 'S' ).
    ELSE.
      LOOP AT i_condiciones INTO l_cond2 WHERE clave = clave AND clave3 = ''.
        __add_lista l_string l_cond2-clave2.
      ENDLOOP.
      IF sy-subrc = 0.
        l_no_cumple = 'X'.
        IF guardar_log = 'X'.
          log( p1 = 'Documento no cumple condicion'(dnc) p2 = clave p3 = l_string p4 = 'Valor'(val) p5 = valor msgty = 'W' ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF line_exists( i_condiciones[ clave  = clave
                                   clave2 = valor
                                   clave3 = 'EXCLUIR' ] ).
      l_excluido = 'X'.
      IF guardar_log = 'X'.
        log( p1 = 'Documento excluido por condicion'(dec) p2 = clave p3 = valor msgty = 'S' ).
      ENDIF.
    ELSE.
      IF line_exists( i_condiciones[ clave  = clave
                                     clave3 = 'EXCLUIR' ] ).
        l_cumple_no_exclusion = 'X'.
        l_cumple = l_cumple_no_exclusion.
      ENDIF.
    ENDIF.
    IF l_cumple = 'X' AND l_no_cumple = '' AND l_excluido IS INITIAL.
      cumple = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD existe_condicion.
    DATA: l_clave2    TYPE zap_exits_par-clave2,
          l_condicion TYPE zap_exits_par.

    l_clave2 = clave2.
    IF quitar_ceros = 'X'.
      __quitar_ceros l_clave2.
    ENDIF.

    CLEAR: l_condicion, si.
    IF clave3 IS INITIAL.
      LOOP AT i_condiciones INTO l_condicion WHERE     clave = clave
                                                   AND ( clave2 = clave2 OR clave2 = l_clave2 )
                                                   AND clave3 = clave3.
        EXIT.
      ENDLOOP.
    ELSE.
      LOOP AT i_condiciones INTO l_condicion WHERE clave = clave AND clave2 = clave2 AND ( clave3 = '' OR clave3 = sy-sysid ).
        EXIT.
      ENDLOOP.
    ENDIF.
    IF NOT l_condicion IS INITIAL.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD existe_variable.
    DATA: l_clave2   TYPE zap_exits_par-clave2,
          l_variable TYPE zap_exits_par.

    l_clave2 = clave2.
    IF quitar_ceros = 'X'.
      __quitar_ceros l_clave2.
    ENDIF.

    CLEAR: l_variable, si.
    IF clave3 IS INITIAL.
      LOOP AT i_variables INTO l_variable WHERE     clave = clave
                                                AND ( clave2 = clave2 OR clave2 = l_clave2 )
                                                AND clave3 = clave3.
        EXIT.
      ENDLOOP.
    ELSE.
      LOOP AT i_variables INTO l_variable WHERE clave = clave AND clave2 = clave2 AND ( clave3 = '' OR clave3 = sy-sysid ).
        EXIT.
      ENDLOOP.
    ENDIF.
    IF NOT l_variable IS INITIAL.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD exit_activa.
    DATA zap_exits TYPE zap_exits.

    SELECT SINGLE activa usuarios_activos usuarios_no_exit usuarios_break FROM zap_exits
    INTO CORRESPONDING FIELDS OF zap_exits
   WHERE id = exit.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF zap_exits-activa = 'X'.
      activa = zap_exits-activa.
    ELSEIF zap_exits-activa = 'U' AND NOT zap_exits-usuarios_activos IS INITIAL.
      activa = zcl_ap_lista=>es_elemento( lista = zap_exits-usuarios_activos elemento = uname ).
    ENDIF.

    IF activa = 'X' AND NOT zap_exits-usuarios_no_exit IS INITIAL.
      IF zcl_ap_lista=>es_elemento( lista = zap_exits-usuarios_no_exit elemento = uname ) = 'X'.
        CLEAR activa.
      ENDIF.
    ENDIF.

    IF activa = 'X' AND NOT zap_exits-ffin IS INITIAL.
      IF NOT ( zap_exits-ffin >= sy-datum AND zap_exits-finicio <= sy-datum ).
        CLEAR activa.
      ENDIF.
    ENDIF.

    IF activa = 'X'.
      IF NOT condicion IS INITIAL.
        SELECT id
          FROM zap_exits_par
          WHERE id     = @exit
            AND tipo   = 'C'
            AND clave  = @condicion
            AND clave2 = @valor_clave2
          ORDER BY PRIMARY KEY
          INTO @zap_exits-id
          UP TO 1 ROWS.
        ENDSELECT.
        IF sy-subrc <> 0.
          SELECT id
            FROM zap_exits_par
            WHERE id    = @exit
              AND tipo  = 'C'
              AND clave = @condicion
            ORDER BY PRIMARY KEY
            INTO @zap_exits-id
            UP TO 1 ROWS.
          ENDSELECT.
          IF sy-subrc = 0.
            CLEAR activa.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF activa = 'X' AND NOT zap_exits-usuarios_break IS INITIAL.
      IF zcl_ap_lista=>es_elemento( lista = zap_exits-usuarios_break elemento = uname ) = 'X'.
        BREAK-POINT.                                          "#EC *
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_lista_variable.
    DATA l_variable TYPE zap_exits_par.

    LOOP AT i_variables INTO l_variable WHERE clave = clave AND ( clave3 = '' OR clave3 = sy-sysid ).
      __add_lista lista l_variable-clave2.
    ENDLOOP.

    IF obligatorio = 'X' AND lista IS INITIAL.
      log( p1 = 'Parametro'(par) p2 = clave p5 = 'sin informar'(sin) msgty = 'E' siempre = 'X'  ).
    ENDIF.
  ENDMETHOD.
  METHOD get_rango_variables_st.
    DATA: i_par   TYPE TABLE OF zap_exits_par,
          par     TYPE zap_exits_par,
          l_clave TYPE c LENGTH 40,
          l_linea TYPE rstt_s_range_string.

    FIELD-SYMBOLS <fs> TYPE any.

    SELECT * FROM zap_exits_par
      INTO TABLE i_par
     WHERE id   = exit
       AND tipo = 'V'.

    CLEAR r_rango.
    LOOP AT i_par INTO par.
      IF NOT ( par-clave = clave OR todo = 'X' ).
        CONTINUE.
      ENDIF.

      IF clave2 IS SUPPLIED.
        IF clave2 <> par-clave2.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF clave3 IS SUPPLIED.
        IF clave3 <> par-clave3.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF NOT filtro_por_campo IS INITIAL.
        CONCATENATE 'PAR-' filtro_por_campo INTO l_clave.
        ASSIGN (l_clave) TO <fs>.
        IF sy-subrc = 0.
          IF <fs> <> valor_filtro_por_campo.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR l_linea.

      CASE campo_rango.
        WHEN 'CLAVE'.
          l_linea-low = par-clave.
        WHEN 'VALOR'.
          l_linea-low = par-valor.
        WHEN 'VALOR2'.
          l_linea-low = par-valor2.
        WHEN 'CLAVE2'.
          l_linea-low = par-clave2.
        WHEN 'CLAVE3'.
          l_linea-low = par-clave3.
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
  METHOD get_variable.
    DATA: l_variable TYPE zap_exits_par,
          l_ctd      TYPE mengev,
          l_mensaje  TYPE bapi_msg.

    IF clave3 IS INITIAL.
      READ TABLE i_variables INTO l_variable WITH KEY clave = clave clave2 = clave2 clave3 = clave3.
    ELSE.
      LOOP AT i_variables INTO l_variable WHERE clave = clave AND clave2 = clave2 AND ( clave3 = '' OR clave3 = sy-sysid ).
        EXIT.
      ENDLOOP.
    ENDIF.
    IF NOT l_variable IS INITIAL.
      valor = l_variable-valor.

      IF ctd = 'X'.
        zcl_ap_string=>string2ctd( EXPORTING ctd_texto = valor
                                 IMPORTING cantidad  = l_ctd
                                           mensaje   = l_mensaje ).
        IF l_mensaje IS INITIAL.
          valor = l_ctd.
        ELSE.
          log( p1 = 'Valor no es cantidad en'(vnc) p2 = clave p3 = l_variable-valor p4 = l_mensaje  siempre = 'X' ).
        ENDIF.
      ENDIF.
    ELSE.
      IF obligatorio = 'X'.
        log( p1 = 'Parametro'(par) p2 = clave p3 = clave2 p4 = clave3 p5 = 'sin informar'(sin) msgty = 'E' siempre = 'X'  ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD log.
    IF guardar_log = 'X' OR siempre = 'X'.
      DATA(l_zlog) = o_log->log( p1 = p1 p2 = p2 p3 = p3 p4 = p4 p5 = p5 p6 = p6 p7 = p7 p8 = p8 p9 = p9 p10 = p10
                  p11 = p11 p12 = p12 p13 = p13 p14 = p14
                msgty = msgty msgid = msgid msgno = msgno
                msgv1 = msgv1 msgv2 = msgv2 msgv3 = msgv3 msgv4 = msgv4 ).
      message = l_zlog-message.
    ENDIF.
  ENDMETHOD.
  METHOD log_st.
    DATA(o_exit) = NEW zcl_ap_exits( exit = exit uname = uname clave = clave ).

    message = o_exit->log( p1 = p1 p2 = p2 p3 = p3 p4 = p4 p5 = p5 p6 = p6
                           msgty = msgty msgid = msgid msgno = msgno
                           msgv1 = msgv1 msgv2 = msgv2 msgv3 = msgv3 msgv4 = msgv4 ).
  ENDMETHOD.