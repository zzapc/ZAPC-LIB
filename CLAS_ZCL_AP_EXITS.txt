CLASS zcl_ap_exits DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA o_log               TYPE REF TO zcl_ap_log.
    DATA id                  TYPE zap_exits-id.
    DATA guardar_log         TYPE abap_bool.
    DATA zap_exits           TYPE zap_exits.
    DATA i_condiciones       TYPE zap_exits_par_t.
    DATA i_variables         TYPE zap_exits_par_t.
    DATA i_condiciones_agrup TYPE zap_exits_par_t.
    DATA uname               TYPE sy-uname.
    DATA clave               TYPE string.
    DATA cond_or             TYPE xfeld.

    METHODS constructor
      IMPORTING !exit    TYPE any
                !include TYPE any      DEFAULT ''
                clave    TYPE any      DEFAULT ''
                !form    TYPE any      DEFAULT ''
                !uname   TYPE sy-uname DEFAULT sy-uname.

    METHODS activa
      IMPORTING condicion     TYPE any DEFAULT ''
                valor_clave2  TYPE any DEFAULT ''
      RETURNING VALUE(activa) TYPE abap_bool.

    METHODS get_variable
      IMPORTING clave        TYPE any
                clave2       TYPE any       OPTIONAL
                clave3       TYPE any       OPTIONAL
                ctd          TYPE abap_bool DEFAULT ''
                obligatorio  TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(valor) TYPE zap_exits_par-valor.

    CLASS-METHODS exit_activa
      IMPORTING !exit         TYPE any      OPTIONAL
                !uname        TYPE sy-uname DEFAULT sy-uname
                clave         TYPE any      DEFAULT ''
                condicion     TYPE any      DEFAULT ''
                valor_clave2  TYPE any      DEFAULT ''
    PREFERRED PARAMETER exit
      RETURNING VALUE(activa) TYPE abap_bool.

    METHODS log
      IMPORTING p1             TYPE any
                p2             TYPE any       OPTIONAL
                p3             TYPE any       OPTIONAL
                p4             TYPE any       OPTIONAL
                p5             TYPE any       OPTIONAL
                p6             TYPE any       OPTIONAL
                p7             TYPE any       OPTIONAL
                p8             TYPE any       OPTIONAL
                p9             TYPE any       OPTIONAL
                p10            TYPE any       OPTIONAL
                p11            TYPE any       OPTIONAL
                p12            TYPE any       OPTIONAL
                p13            TYPE any       OPTIONAL
                p14            TYPE any       OPTIONAL
                siempre        TYPE abap_bool DEFAULT ''
                msgty          TYPE sy-msgty  DEFAULT 'E'
                msgid          TYPE any       DEFAULT ''
                msgno          TYPE any       DEFAULT ''
                msgv1          TYPE any       DEFAULT ''
                msgv2          TYPE any       DEFAULT ''
                msgv3          TYPE any       DEFAULT ''
                msgv4          TYPE any       DEFAULT ''
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS log_st
      IMPORTING !exit          TYPE any      OPTIONAL
                !uname         TYPE sy-uname DEFAULT sy-uname
                clave          TYPE any      DEFAULT ''
                p1             TYPE any      OPTIONAL
                p2             TYPE any      OPTIONAL
                p3             TYPE any      OPTIONAL
                p4             TYPE any      OPTIONAL
                p5             TYPE any      OPTIONAL
                p6             TYPE any      OPTIONAL
                msgty          TYPE any      DEFAULT 'E'
                msgid          TYPE any      DEFAULT ''
                msgno          TYPE any      DEFAULT ''
                msgv1          TYPE any      DEFAULT ''
                msgv2          TYPE any      DEFAULT ''
                msgv3          TYPE any      DEFAULT ''
                msgv4          TYPE any      DEFAULT ''
    PREFERRED PARAMETER exit
      RETURNING VALUE(message) TYPE bapi_msg.

    METHODS get_lista_variable
      IMPORTING clave        TYPE any
                obligatorio  TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(lista) TYPE string.

    METHODS cumple_cond
      RETURNING VALUE(si) TYPE abap_bool.

    METHODS eval_cond
      IMPORTING valor         TYPE any
                clave         TYPE any
      RETURNING VALUE(cumple) TYPE abap_bool.

    CLASS-METHODS get_rango_variables_st
      IMPORTING !exit                  TYPE any
                clave                  TYPE any       DEFAULT ''
                clave2                 TYPE any       DEFAULT ''
                clave3                 TYPE any       DEFAULT ''
                campo_rango            TYPE any       DEFAULT 'CLAVE2'
                !option                TYPE any       DEFAULT 'EQ'
                !sign                  TYPE any       DEFAULT 'I'
                todo                   TYPE abap_bool DEFAULT ''
                error_si_vacio         TYPE any       DEFAULT ''
                filtro_por_campo       TYPE any       DEFAULT ''
                valor_filtro_por_campo TYPE any       DEFAULT ''
      RETURNING VALUE(r_rango)         TYPE rstt_t_range_string.

    CLASS-METHODS cumple_cond_clave2
      IMPORTING !exit         TYPE any
                condicion     TYPE any
                valor         TYPE any
                sin_ceros     TYPE any DEFAULT ''
      RETURNING VALUE(cumple) TYPE abap_bool.

    METHODS existe_variable
      IMPORTING clave        TYPE any
                clave2       TYPE any       OPTIONAL
                clave3       TYPE any       OPTIONAL
                quitar_ceros TYPE abap_bool DEFAULT ''
      RETURNING VALUE(si)    TYPE abap_bool.

    METHODS existe_condicion
      IMPORTING clave        TYPE any
                clave2       TYPE any       OPTIONAL
                clave3       TYPE any       OPTIONAL
                quitar_ceros TYPE abap_bool DEFAULT ''
      RETURNING VALUE(si)    TYPE abap_bool.

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
          WHERE id = @exit
            AND tipo = 'C'
            AND clave = @condicion
            AND clave2 = @valor_clave2
          ORDER BY PRIMARY KEY
          INTO @zap_exits-id
          UP TO 1 ROWS.
        ENDSELECT.
        IF sy-subrc <> 0.
          SELECT id
            FROM zap_exits_par
            WHERE id = @exit
              AND tipo = 'C'
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
