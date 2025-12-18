class ZCL_AP_FECHAS definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF t_fechas,
        fecha       TYPE scdatum,
        dia         TYPE ktag,
        numero_dia  TYPE wotnr,
        nombre_diac TYPE wotnr,
        nombre_dia  TYPE langt,
      END OF t_fechas .
  types:
    tt_fechas TYPE SORTED TABLE OF t_fechas WITH UNIQUE KEY fecha .
  types:
    BEGIN OF t_fecha_cal,
        fecha             TYPE scdatum,
        dia               TYPE ktag,
        numero_dia        TYPE wotnr,
        nombre_diac       TYPE wotnr,
        nombre_dia        TYPE langt,
        festivo           TYPE feiertagkz,
        categoria_festivo TYPE kklass,
        descr_corta       TYPE textk,
        descr_larga       TYPE textl_d,
      END OF t_fecha_cal .
  types:
    tt_fecha_cal TYPE SORTED TABLE OF t_fecha_cal WITH UNIQUE KEY fecha .
  types:
    BEGIN OF t_dia_horas,
        dia   TYPE dats,
        beguz TYPE beguz,
        enduz TYPE enduz,
        orden TYPE n LENGTH 3,
      END OF t_dia_horas .
  types:
    tt_dia_horas TYPE TABLE OF t_dia_horas .
  types:
    BEGIN OF t_meses,
        perio TYPE spmon,
        begda TYPE begda,
        endda TYPE endda,
      END OF t_meses .
  types:
    tt_meses TYPE TABLE OF t_meses WITH KEY perio .

  class-methods GET_NOMBRE_MES
    importing
      !MES type T247-MNR
      !IDIOMA type SY-LANGU default SY-LANGU
    returning
      value(NOMBRE) type T247-LTX .
  class-methods GET_NOMBRE_MES_CORTO
    importing
      !MES type T247-MNR
      !SPRAS type SY-LANGU default SY-LANGU
    returning
      value(NOMBRE) type T247-LTX .
  class-methods GET_SEMANA
    importing
      !FECHA type SCAL-DATE
    returning
      value(SEMANA) type SCAL-WEEK .
  class-methods GET_NUMERO_DIA
    importing
      !FECHA type DATUM
    returning
      value(NUM_DIA) type NUMC1 .
  class-methods GET_PRIMER_DIA_SEMANA
    importing
      !SEMANA type SCAL-WEEK optional
      !FECHA_SEL type DATS optional
      !SUMA_SEMANAS type I default 0
    preferred parameter SEMANA
    returning
      value(FECHA) type SCAL-DATE .
  class-methods GET_ULTIMO_DIA_SEMANA
    importing
      !SEMANA type SCAL-WEEK
    returning
      value(FECHA) type SCAL-DATE .
  class-methods GET_PERIODO_ANTERIOR
    importing
      !PER type SPMON optional
      !FECHA type DATUM default SY-DATUM
    returning
      value(PERIODO) type SPMON .
  class-methods SUMA_MESES
    importing
      !MESES type I
      !FECHA type DATUM
    returning
      value(NUEVA_FECHA) type DATUM .
  class-methods GET_PRIMER_DIA_MES
    importing
      !FECHA type DATUM default SY-DATUM
    returning
      value(NUEVA_FECHA) type DATUM .
  class-methods GET_ULTIMO_DIA_MES
    importing
      !FECHA type DATUM default SY-DATUM
    returning
      value(NUEVA_FECHA) type DATUM .
  class-methods HAY_SOLAPAMIENTO_FECHAS
    importing
      !FINI1 type D
      !FFIN1 type D
      !HINI1 type T default '000000'
      !HFIN1 type T default '240000'
      !FINI2 type D
      !FFIN2 type D
      !HINI2 type T default '000000'
      !HFIN2 type T default '240000'
    returning
      value(HAY_SOLAPAMIENTO) type ABAP_BOOL .
  class-methods INTERSECCION_FECHAS
    importing
      !FINI1 type D
      !FFIN1 type D
      !HINI1 type T default '000000'
      !HFIN1 type T default '240000'
      !FINI2 type D
      !FFIN2 type D
      !HINI2 type T default '000000'
      !HFIN2 type T default '240000'
    exporting
      value(RHINI) type T
      value(RHFIN) type T
      value(RFINI) type D
      value(RFFIN) type D .
  class-methods UNION_FECHAS
    importing
      !FINI1 type D
      !FFIN1 type D
      !HINI1 type T default '000000'
      !HFIN1 type T default '240000'
      !FINI2 type D
      !FFIN2 type D
      !HINI2 type T default '000000'
      !HFIN2 type T default '240000'
    exporting
      value(RHINI) type T
      value(RHFIN) type T
      value(RFINI) type D
      value(RFFIN) type D .
  class-methods RESTA_FECHAS
    importing
      !FINI1 type D
      !FFIN1 type D
      !HINI1 type T default '000000'
      !HFIN1 type T default '240000'
      !FINI2 type D
      !FFIN2 type D
      !HINI2 type T default '000000'
      !HFIN2 type T default '240000'
    exporting
      value(RHINI) type T
      value(RHFIN) type T
      value(RFINI) type D
      value(RFFIN) type D .
  class-methods PRIMER_DIA_LAB_DESDE_FECHA
    importing
      !FECHA type DATUM optional
      !CALID type SCAL-FCALID default 'ES'
      !CORRECT_OPTION type SCAL-INDICATOR default '+'
    preferred parameter FECHA
    returning
      value(FECHA_SALIDA) type DATUM .
  class-methods PERIODOS_RESTA_FECHAS
    importing
      !FECHA1 type D
      !FECHA2 type D
      !OUTPUT_FORMAT type CHAR2 default '05'
    exporting
      !ANYOS type P0347-SCRYY
      !MESES type P0347-SCRMM
      !DIAS type P0347-SCRDD .
  class-methods RANGO2FECHAS
    importing
      !R_FECHAS type TPMY_R_DATE
    exporting
      !FECHA_DESDE type BEGDA
      !FECHA_HASTA type ENDDA .
  class-methods STRING2FECHA
    importing
      !CADENA type ANY optional
      !CORREGIR_ERRORES type ABAP_BOOL default ''
    preferred parameter CADENA
    returning
      value(FECHA) type D .
  class-methods GET_ULTIMO_DIA_MES_ANTERIOR
    importing
      !FECHA type DATUM default SY-DATUM
    returning
      value(NUEVA_FECHA) type DATUM .
  class-methods GET_PERIODO_SIGUIENTE
    importing
      !PER type SPMON optional
      !FECHA type DATUM default SY-DATUM
    returning
      value(PERIODO) type SPMON .
  class-methods ES_VALIDA
    importing
      !FECHA type SY-DATUM
    returning
      value(OK) type ABAP_BOOL .
  class-methods FECHA_A_LETRAS
    importing
      !FECHA type DATS default SY-DATUM
      !IDIOMA type SY-LANGU default SY-LANGU
    preferred parameter FECHA
    returning
      value(LETRAS) type STRING .
  class-methods STRING2HORA
    importing
      !STRING type ANY
    returning
      value(HORA) type SY-UZEIT .
  class-methods GET_DURACION_INTERVALO
    importing
      !FINI type D
      !FFIN type D
      !HINI type T default '000000'
      !HFIN type T default '240000'
      !VER_NEG type ABAP_BOOL default ''
    returning
      value(HORAS) type HOURS .
  class-methods AAAAMMDD_2_DDMMAAAA
    importing
      !FECHA_AAAAMMDD type D
    returning
      value(FECHA_DDMMAAAA) type D .
  class-methods FECHA_HORA_EN_RANGO
    importing
      !R_FECHAS type TPMY_R_DATE
      !R_HORAS type JIT_ITIME_RANGE_TT
      !FECHA type SY-DATUM
      !HORA type SY-UZEIT
    returning
      value(SI) type ABAP_BOOL .
  class-methods RANGOSEM_2_RANGOFECHAS
    importing
      !R_SEMANAS type RSARCH_RT_CALWEEK
    returning
      value(R_FECHAS) type TPMY_R_DATE .
  class-methods ES_FESTIVO
    importing
      !FECHA type DATUM optional
      !CALID type ANY default 'ES'
    preferred parameter FECHA
    returning
      value(FESTIVO) type ABAP_BOOL .
  class-methods CONDENSAR_FECHAS
    importing
      !CLAVE type ANY
      !CLAVE2 type ANY optional
    changing
      !TABLA type TABLE .
  class-methods RANGOPERIO_2_RANGOFECHAS
    importing
      !R_PERIODOS type ZT_RANGO_PERIODOS
    returning
      value(R_FECHAS) type TPMY_R_DATE .
  class-methods GET_LETRA_NUMERO_DIA
    importing
      !FECHA type DATUM
    returning
      value(LETRA_DIA) type CHAR1 .
  class-methods GET_DURACION_INTERVALO_MIN
    importing
      !FINI type D
      !FFIN type D
      !HINI type T default '000000'
      !HFIN type T default '240000'
      !VER_NEG type ABAP_BOOL default ''
    returning
      value(HORAS) type EAUSZT .
  class-methods RANGO2HORAS
    importing
      !R_HORAS type EMMA_TIME_RANGE_TAB
    exporting
      !HORA_DESDE type BEGUZ
      !HORA_HASTA type ENDUZ .
  class-methods LISTA_DIAS
    importing
      !FINI type BEGDA
      !FFIN type ENDDA
      !FCAL type SCAL-FCALID default 'ES'
      !HCAL type SCAL-FCALID default 'ES'
    returning
      value(I_DIAS) type RMXTTY_MTP_DATE_ATTRIBUTES .
  class-methods GET_LETRA_NUMERO_DIA3
    importing
      !FECHA type DATUM
    returning
      value(LETRA_DIA) type CHAR3 .
  class-methods GET_NOMBRE_NUMERO_DIA
    importing
      !FECHA type DATUM
    returning
      value(DIA) type STRING .
  class-methods GET_DURACION_INTERVALO_SEC
    importing
      !FINI type D
      !FFIN type D
      !HINI type T default '000000'
      !HFIN type T default '240000'
    returning
      value(SEGUNDOS) type EAUSZT .
  class-methods GET_FECHA_WS
    importing
      !FECHA type SY-DATUM default SY-DATUM
      !HORA type SY-UZEIT default '000000'
      !TZ type ABAP_BOOL default 'X'
    preferred parameter FECHA
    returning
      value(FECHA_WS) type STRING .
  class-methods DATETIMEWS_2_FECHAHORA
    importing
      !FECHA_WS type ANY
    exporting
      !FECHA type SY-DATUM
      !HORA type SY-UZEIT .
  class-methods FECHAHORA_2_TIMESTAMP
    importing
      !FECHA type SY-DATUM default SY-DATUM
      !HORA type SY-UZEIT default '000000'
      !TZONE type SY-ZONLO default SY-ZONLO
    preferred parameter FECHA
    returning
      value(TIMESTAMP) type TZONREF-TSTAMPS .
  class-methods TIMESTAMP_2_FECHAHORA
    importing
      !TZONE type SY-ZONLO default SY-ZONLO
      !TIMESTAMP type ANY
    exporting
      !FECHA type SY-DATUM
      !HORA type SY-UZEIT .
  class-methods RANGOPERIO_2_TABLAPERIO
    importing
      !R_PERIODOS type ZT_RANGO_PERIODOS
    returning
      value(I_PERIODOS) type ZSPMON_T .
  class-methods GET_INTERVALOS_HORAS
    importing
      !DIA type BEGDA default SY-DATUM
      !DIA_FIN type ENDDA optional
      !BEGUZ type BEGUZ default '000000'
      !ENDUZ type ENDUZ default '240000'
      !MINUTOS type I
    exporting
      !INTERVALOS type TT_DIA_HORAS
      !MENSAJE type BAPI_MSG .
  class-methods RANGO2TABLA_SEMANAS
    importing
      !R_FECHAS type TPMY_R_DATE
      !MAX_SEMANAS type I default 99
      !FECHA_INICIO_SI_VACIA type DATS default SY-DATUM
    exporting
      !I_SEMANAS type ZAP_ALV_COLUMNAS3_T
      !MENSAJE type BAPI_MSG .
  class-methods GET_FECHA_FORMATEADA
    importing
      !FECHA type DATUM
      !FORMATO type ANY
    returning
      value(DIA) type STRING .
  class-methods SUMA_SEMANAS
    importing
      !SEMANA type SCAL-WEEK
      !SEMANAS type I default 0
      !DIAS type I default 0
    returning
      value(SEMANA_FIN) type SCAL-WEEK .
  class-methods RANGO2STRING
    importing
      !R_FECHAS type TPMY_R_DATE
    returning
      value(STRING) type STRING .
  class-methods CONTIENE_MES
    importing
      !FINI type DATS
      !FFIN type DATS
      !MES type NUMC2
    returning
      value(SI) type ABAP_BOOL .
  class-methods POPUP_FECHA
    importing
      !TITULO type ANY default 'Fecha'
      !LINEA1 type ANY default ''
      !LINEA2 type ANY default ''
      !FECHA type DATS default SY-DATUM
    returning
      value(FECHA_SALIDA) type DATS .
  class-methods GET_NUM_DIAS_LABORALES
    importing
      !FINI type BEGDA
      !FFIN type ENDDA
      !FABKL type FABKL
    returning
      value(DIAS_LABORABLES) type INT4 .
  class-methods SUMA_DIAS_LABORABLES
    importing
      !FECHA type DATS
      !DIAS type I
      !CALID type TFACD-IDENT
    returning
      value(FECHA_SALIDA) type DATS .
  class-methods CL_RECA_DATE .  "#EC EMPTY_PROCEDURE
  class-methods RANGOFECHAS_2_RANGOPERIO
    importing
      !R_FECHAS type TPMY_R_DATE
    returning
      value(R_PERIODOS) type ZT_RANGO_PERIODOS .
  class-methods SUMA_HORAS
    importing
      !FECHA_INI type SY-DATUM
      !HORA_INI type SY-UZEIT
      !HORAS type ANY
    exporting
      !FECHA_FIN type SY-DATUM
      !HORA_FIN type SY-UZEIT .
  class-methods GET_TRIMESTRE
    importing
      !FECHA type DATS
    returning
      value(TRIMESTRE) type CHAR2 .
  class-methods GET_FECHAS_CALENDARIO
    importing
      !FINI type BEGDA
      !FFIN type ENDDA
      !CALENDARIO type TFACD-IDENT
    exporting
      !I_FECHAS type TT_FECHA_CAL .
  class-methods ES_DIA_LABORABLE
    importing
      !FECHA type DATS
      !CALENDARIO type SCAL-FCALID
    returning
      value(LABORABLE) type XFELD .
  class-methods GET_FECHAS
    importing
      !FINI type BEGDA
      !FFIN type ENDDA
    exporting
      !I_FECHAS type TT_FECHAS .
  class-methods GET_MESES
    importing
      !FINI type BEGDA
      !FFIN type ENDDA
    exporting
      !I_MESES type TT_MESES .
  class-methods RANGOFECHAS_2_RANGOSEM
    importing
      !R_FECHAS type TPMY_R_DATE
    returning
      value(R_SEMANAS) type RSARCH_RT_CALWEEK .


endclass. "ZCL_AP_FECHAS definition
class ZCL_AP_FECHAS implementation.
  METHOD aaaammdd_2_ddmmaaaa.
    fecha_ddmmaaaa(2) = fecha_aaaammdd+6(2).
    fecha_ddmmaaaa+2(2) = fecha_aaaammdd+4(2).
    fecha_ddmmaaaa+4(4) = fecha_aaaammdd(4).
  ENDMETHOD.
  METHOD cl_reca_date. "#EC EMPTY_PROCEDURE
* Recordatorio de que existe esta clase estándar con utilidades
  ENDMETHOD.
  METHOD condensar_fechas.
    TYPES: BEGIN OF t_fechas_cond,
             clave     TYPE c LENGTH 40,
             clave2    TYPE c LENGTH 40,
             begda     TYPE begda,
             endda     TYPE endda,
             registros TYPE c LENGTH 1000,
           END OF t_fechas_cond.

    DATA: l_tabix       TYPE c LENGTH 5,
          l_fechas      TYPE t_fechas_cond,
          i_fechas      TYPE TABLE OF t_fechas_cond,
          l_fechas_cond TYPE t_fechas_cond,
          l_new         TYPE c LENGTH 1,
          l_end         TYPE c LENGTH 1,
          l_last        TYPE c LENGTH 1,
          i_fechas_cond TYPE TABLE OF t_fechas_cond,
          l_endda       TYPE d,
          l_nbor        TYPE i,
          i_registros   TYPE TABLE OF string,
          l_registro    TYPE string,
          l_first       TYPE c LENGTH 1,
          l_reg         TYPE sy-tabix.

    DATA l_campo TYPE c LENGTH 40.

    FIELD-SYMBOLS: <dyn_wa> TYPE any,
                   <fs>     TYPE any.

    LOOP AT tabla ASSIGNING <dyn_wa>.        " New record
      l_tabix = sy-tabix.

      CLEAR l_fechas.

      CONCATENATE '<dyn_wa>-' 'BEGDA' INTO l_campo.
      ASSIGN (l_campo) TO <fs>.
      IF sy-subrc = 0.
        l_fechas-begda = <fs>.
      ENDIF.
      CONCATENATE '<dyn_wa>-' 'ENDDA' INTO l_campo.
      ASSIGN (l_campo) TO <fs>.
      IF sy-subrc = 0.
        l_fechas-endda = <fs>.
      ENDIF.
      CONCATENATE '<dyn_wa>-' clave INTO l_campo.
      ASSIGN (l_campo) TO <fs>.
      IF sy-subrc = 0.
        l_fechas-clave = <fs>.
      ENDIF.
      IF NOT clave2 IS INITIAL.
        CONCATENATE '<dyn_wa>-' clave2 INTO l_campo.
        ASSIGN (l_campo) TO <fs>.
        IF sy-subrc = 0.
          l_fechas-clave2 = <fs>.
        ENDIF.
      ENDIF.

      l_fechas-registros = l_tabix.
      APPEND l_fechas TO i_fechas.
    ENDLOOP.

    SORT i_fechas.

    CLEAR l_fechas_cond.
    LOOP AT i_fechas INTO l_fechas.
      CLEAR: l_new, l_end, l_last.
      AT NEW clave2.
        l_new = 'X'.
      ENDAT.
      AT END OF clave2.
        l_end = 'X'.
      ENDAT.
      AT LAST.
        l_last = 'X'.
      ENDAT.

* Si no hay registro previo.
      IF l_fechas_cond IS INITIAL.
        l_fechas_cond-clave     = l_fechas-clave.
        l_fechas_cond-clave2    = l_fechas-clave2.
        l_fechas_cond-begda     = l_fechas-begda.
        l_fechas_cond-endda     = l_fechas-endda.
        l_fechas_cond-registros = l_fechas-registros.

        IF l_end = 'X'.
*     Si es el final lo guardamos
          APPEND l_fechas_cond TO i_fechas_cond.
          CLEAR: l_fechas_cond, l_endda.
        ELSE.
*     Si no, esperamos si el siguiente registro continua
          l_endda = l_fechas-endda + 1.
        ENDIF.
      ELSE.
        IF l_end = 'X'.

*     Si es el final y continua, lo guardamos
          IF l_fechas-begda = l_endda.
            l_fechas_cond-endda = l_fechas-endda.
            CONCATENATE l_fechas_cond-registros ','
                        l_fechas-registros INTO l_fechas_cond-registros.
            CONDENSE l_fechas_cond-registros NO-GAPS.
            APPEND l_fechas_cond TO i_fechas_cond.
          ELSE.
*     Si es el final y no consecutivo, guardamos dos treamso.
            APPEND l_fechas_cond TO i_fechas_cond.
            APPEND l_fechas TO i_fechas_cond.
          ENDIF.
          CLEAR: l_fechas_cond, l_endda.
        ELSE.
*     Si no, esperamos si el siguiente registro continua
          IF l_fechas-begda = l_endda.
            l_fechas_cond-endda = l_fechas-endda.
            CONCATENATE l_fechas_cond-registros ','
                        l_fechas-registros INTO l_fechas_cond-registros.
            CONDENSE l_fechas_cond-registros NO-GAPS.
          ELSE.
*     Si es el final y no consecutivo, guardamos dos treamso.
            APPEND l_fechas_cond TO i_fechas_cond.
            l_fechas_cond = l_fechas.
          ENDIF.
          l_endda = l_fechas-endda + 1.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CLEAR l_nbor.
    SORT i_fechas_cond BY registros.
    LOOP AT i_fechas_cond INTO l_fechas.
      SPLIT l_fechas-registros AT ',' INTO TABLE i_registros.
      LOOP AT i_registros INTO l_registro.

        CLEAR l_first.
        l_reg = l_registro - l_nbor.

        AT FIRST.
          l_first = 'X'.
          ASSIGN tabla[ l_reg ] TO <dyn_wa>.
          IF sy-subrc = 0.
            CONCATENATE '<dyn_wa>-' 'ENDDA' INTO l_campo.
            ASSIGN (l_campo) TO <fs>.
            IF sy-subrc = 0.
              <fs> = l_fechas-endda.
            ENDIF.
          ENDIF.
        ENDAT.

        IF l_first IS INITIAL.
          DELETE tabla INDEX l_reg.
          l_nbor = l_nbor + 1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD contiene_mes.
    DATA: l_f1    TYPE d,
          l_f2    TYPE d,
          l_meses TYPE pea_scrmm,
          l_dif   TYPE i.

    CONCATENATE fini(6) '01' INTO l_f1.
    CONCATENATE ffin(6) '01' INTO l_f2.

    CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES' ##FM_SUBRC_OK
      EXPORTING
        date1                   = l_f2
        date2                   = l_f1
        output_format           = '08'
      IMPORTING
        months                  = l_meses
      EXCEPTIONS
        invalid_dates_specified = 1
        OTHERS                  = 2.

    IF l_meses >= 11.
      si = 'X'.
    ELSEIF l_f2+4(2) >= mes.
      l_dif = l_f2+4(2) - mes.
      IF l_dif <= l_meses.
        si = 'X'.
      ENDIF.
    ELSEIF l_f1(4) < l_f2(4). " Son años diferentes
      IF l_f1+4(2) <= mes.
        si = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD datetimews_2_fechahora.
    DATA: l_cadena TYPE c LENGTH 40,
          l_fecha  TYPE c LENGTH 10,
          l_hora   TYPE c LENGTH 8.

    TRY.
        cl_bs_soa_convert_date_time=>map_utc_date_time_in( EXPORTING iv_utc_datetime = fecha_ws iv_timezone = sy-zonlo
                                                           IMPORTING ev_date = fecha ev_time = hora ).
      CATCH cx_bs_soa_exception.
        l_cadena = fecha_ws.

        IF l_cadena CS 'T'.
          SPLIT l_cadena AT 'T' INTO l_fecha l_hora.
        ELSE.
          l_fecha = l_cadena(10).
          l_hora  = l_cadena+11(8).
        ENDIF.
        fecha = string2fecha( l_fecha ).
        hora  = string2hora( l_hora ).
    ENDTRY.
  ENDMETHOD.
  METHOD es_dia_laborable.
    CLEAR laborable.
    CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
      EXPORTING
        date                       = fecha
        factory_calendar_id        = calendario
        message_type               = 'I'
      EXCEPTIONS
        date_after_range           = 1
        date_before_range          = 2
        date_invalid               = 3
        date_no_workingday         = 4
        factory_calendar_not_found = 5
        message_type_invalid       = 6
        OTHERS                     = 7.
    IF sy-subrc = 0.
      laborable = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD es_festivo.
    DATA: l_calid     TYPE scal-fcalid,
          l_sig_fecha TYPE d.

    CLEAR festivo.
    l_calid = calid.
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
      EXPORTING
        correct_option               = '+'
        date                         = fecha
        factory_calendar_id          = l_calid
      IMPORTING
        date                         = l_sig_fecha
      EXCEPTIONS
        calendar_buffer_not_loadable = 1
        correct_option_invalid       = 2
        date_after_range             = 3
        date_before_range            = 4
        date_invalid                 = 5
        factory_calendar_not_found   = 6
        OTHERS                       = 7.

    IF sy-subrc = 0.
      IF fecha <> l_sig_fecha.
        festivo = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD es_valida.
    CLEAR ok.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = fecha
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc = 0.
      ok = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD fecha_a_letras.
    IF NOT fecha IS INITIAL.
      letras = get_nombre_mes( mes = fecha+4(2) idioma = idioma ).
      IF idioma = 'S'.
        CONCATENATE fecha+6(2) 'de' letras ' de' fecha(4) INTO letras SEPARATED BY space.
      ELSE.
        CONCATENATE fecha+6(2) letras fecha(4) INTO letras SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD fecha_hora_en_rango.
    DATA: l_hini  TYPE sy-uzeit,
          l_hfin  TYPE sy-uzeit,
          l_rhora TYPE jit_itime_range,
          l_fini  TYPE d,
          l_ffin  TYPE d.

    IF r_horas IS INITIAL.
      l_hini = '000000'.
      l_hfin = '235959'.
    ELSE.
      READ TABLE r_horas INTO l_rhora INDEX 1.
      l_hini = l_rhora-low.
      IF l_rhora-high IS INITIAL.
        l_hfin = l_rhora-low.
      ELSE.
        l_hfin = l_rhora-high.
      ENDIF.
    ENDIF.

    rango2fechas( EXPORTING r_fechas = r_fechas
                  IMPORTING fecha_desde = l_fini
                            fecha_hasta = l_ffin ).

    si = hay_solapamiento_fechas( fini1 = l_fini
                                  ffin1 = l_ffin
                                  hini1 = l_hini
                                  hfin1 = l_hfin
                                  fini2 = fecha
                                  ffin2 = fecha
                                  hini2 = hora
                                  hfin2 = hora ).
  ENDMETHOD.
  METHOD fechahora_2_timestamp.
    CALL FUNCTION 'IB_CONVERT_INTO_TIMESTAMP'
      EXPORTING
        i_datlo     = fecha
        i_timlo     = hora
        i_tzone     = tzone
      IMPORTING
        e_timestamp = timestamp.
  ENDMETHOD.
  METHOD get_duracion_intervalo.
    DATA: l_dias  TYPE i,
          l_horas TYPE p LENGTH 8 DECIMALS 4.

    IF    fini IS INITIAL
       OR ffin IS INITIAL.
      RETURN.
    ENDIF.

    l_dias = ffin - fini.
    IF l_dias > 69000.
      horas = 99999.
    ELSE.
      IF l_dias >= 0.
        l_horas = ( hfin - hini ) / 3600.
        IF l_horas > 0 OR ver_neg = 'X' OR l_dias > 0.
          TRY.
              horas = 24 * l_dias + l_horas.
            CATCH cx_sy_arithmetic_overflow.
              horas = 99999.
          ENDTRY.
        ENDIF.
      ELSEIF ver_neg = 'X'.
        l_horas = ( hfin - hini ) / 3600.
        IF l_horas > 0 OR l_dias > 0.
          TRY.
              horas = 24 * l_dias + l_horas.
            CATCH cx_sy_arithmetic_overflow.
              horas = 99999.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_duracion_intervalo_min.
    DATA: l_dias  TYPE i,
          l_horas TYPE p LENGTH 8 DECIMALS 2.

    IF    fini IS INITIAL
       OR ffin IS INITIAL.
      RETURN.
    ENDIF.

    l_dias = ffin - fini.
    IF l_dias > 69000.
      l_horas = 99999.
    ELSE.
      IF l_dias >= 0 OR ver_neg = 'X'.
        l_horas = ( hfin - hini ) / 60.
        horas = l_horas.
        IF l_horas > 0 OR l_dias > 0.
          horas = ( 24 * 60 ) * l_dias + l_horas.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_duracion_intervalo_sec.
    DATA: l_dias     TYPE i,
          l_segundos TYPE p LENGTH 8 DECIMALS 2.

    IF    fini IS INITIAL
       OR ffin IS INITIAL.
      RETURN.
    ENDIF.

    l_dias = ffin - fini.
    IF l_dias > 69000.
      l_segundos = 999999.
    ELSE.
      IF l_dias >= 0.
        l_segundos = ( hfin - hini ).
        IF l_segundos > 0 OR l_dias > 0.
          segundos = ( 24 * 60 * 60 ) * l_dias + l_segundos.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_fecha_formateada.
    DATA: l_nombre_dia TYPE c LENGTH 30,
          l_long       TYPE i,
          l_nombre_mes TYPE c LENGTH 30.

    IF fecha IS INITIAL.
      RETURN.
    ENDIF.

    dia = formato.
*  'Nd, DD de nm de AAAA' -> Sábado, 25 de enero de 2014
    IF dia CS 'ND' OR dia CS 'Nd' OR dia CS 'nd'.
      l_nombre_dia = get_nombre_numero_dia( fecha ).
      l_long = strlen( l_nombre_dia ).
      FIND FIRST OCCURRENCE OF 'Nd' IN dia.
      IF sy-subrc = 0.
        l_nombre_dia+1 = to_lower( l_nombre_dia+1 ).
        REPLACE 'Nd' WITH l_nombre_dia(l_long) INTO dia.
      ELSE.
        FIND FIRST OCCURRENCE OF 'nd' IN dia.
        IF sy-subrc = 0.
          l_nombre_dia = to_lower( l_nombre_dia ).
          REPLACE 'nd' WITH l_nombre_dia(l_long) INTO dia.
        ELSE.
          REPLACE 'ND' WITH l_nombre_dia(l_long) INTO dia.
        ENDIF.
      ENDIF.
    ENDIF.

    IF dia CS 'NM' OR dia CS 'Nm' OR dia CS 'nm'.
      l_nombre_mes = get_nombre_mes( fecha+4(2) ).
      l_long = strlen( l_nombre_mes ).
      FIND FIRST OCCURRENCE OF 'Nm' IN dia.
      IF sy-subrc = 0.
        l_nombre_mes+1 = to_lower( l_nombre_mes+1 ).
        REPLACE 'Nm' WITH l_nombre_mes(l_long) INTO dia.
      ELSE.
        FIND FIRST OCCURRENCE OF 'nm' IN dia.
        IF sy-subrc = 0.
          l_nombre_mes = to_lower( l_nombre_mes ).
          REPLACE 'nm' WITH l_nombre_mes(l_long) INTO dia.
        ELSE.
          REPLACE 'NM' WITH l_nombre_mes(l_long) INTO dia.
        ENDIF.
      ENDIF.
    ENDIF.

    IF dia CS 'DD'.
      REPLACE 'DD' WITH fecha+6(2) INTO dia.
    ENDIF.

    IF dia CS 'AAAA'.
      REPLACE 'AAAA' WITH fecha(4) INTO dia.
    ENDIF.
  ENDMETHOD.
  METHOD get_fecha_ws.
    DATA l_sep  TYPE char1.
    DATA l_text TYPE c LENGTH 50.

    IF tz <> 'N'.
      l_sep = 'T'.
    ENDIF.

    CLEAR fecha_ws.
    CONCATENATE fecha(4) '-' fecha+4(2) '-' fecha+6(2)
               l_sep hora(2) ':' hora+2(2) ':' hora+4(2)
                INTO fecha_ws RESPECTING BLANKS.
    IF tz = 'Z'.
      CONCATENATE fecha_ws 'Z' INTO fecha_ws.
    ELSEIF tz = 'N' OR tz = 'M'. "#EC EMPTY_IF_BRANCH
* En este caso no queremos añadir nada a la hora
    ELSE.
      CONCATENATE fecha_ws '+01:00' INTO fecha_ws.

      IF tz IS INITIAL.
        l_text = fecha_ws.
        CLEAR l_text+10(1).
        CLEAR l_text+19.
        fecha_ws = l_text.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_fechas.
    DATA t_fechas TYPE t_fechas.

    CLEAR i_fechas.

    DATA(i_atributos) = lista_dias( fini = fini ffin = ffin ).

    LOOP AT i_atributos ASSIGNING FIELD-SYMBOL(<atributos>).
      CLEAR t_fechas.
      t_fechas-fecha       = <atributos>-date.
      t_fechas-dia         = <atributos>-date+6(2).

      t_fechas-numero_dia  = <atributos>-weekday.
      t_fechas-nombre_diac = <atributos>-weekday_s.
      IF <atributos>-weekday = 3.
        t_fechas-nombre_diac = 'X'.
      ENDIF.
      t_fechas-nombre_dia = <atributos>-weekday_l.

      APPEND t_fechas TO i_fechas.

    ENDLOOP.
  ENDMETHOD.
  METHOD get_fechas_calendario.
    DATA: l_hocid  TYPE tfacd-hocid,
          l_fecha  TYPE d,
          t_fechas TYPE t_fecha_cal.

    CLEAR i_fechas.

    SELECT SINGLE hocid FROM tfacd
      INTO l_hocid
     WHERE ident = calendario.

    l_fecha = fini.
    DO.
      IF l_fecha > ffin.
        EXIT.
      ENDIF.

      CLEAR t_fechas.
      t_fechas-fecha = l_fecha.
      t_fechas-dia   = l_fecha+6(2).

      IF NOT es_dia_laborable( fecha = l_fecha calendario = calendario ).
        t_fechas-festivo = 'X'.
      ENDIF.

      DATA(i_atributos) = lista_dias( fcal = calendario hcal = l_hocid fini = l_fecha ffin = l_fecha ).
      ASSIGN i_atributos[ 1 ] TO FIELD-SYMBOL(<atributos>).
      IF sy-subrc = 0.
        t_fechas-numero_dia  = <atributos>-weekday.
        t_fechas-nombre_diac = <atributos>-weekday_s.
        IF <atributos>-weekday = 3.
          t_fechas-nombre_diac = 'X'.
        ENDIF.
        t_fechas-nombre_dia  = <atributos>-weekday_l.
        t_fechas-descr_corta = <atributos>-txt_short.
        t_fechas-descr_larga = <atributos>-txt_long.

        SELECT SINGLE klass FROM thol
          INTO t_fechas-categoria_festivo
         WHERE ftgid = <atributos>-holiday_id.

      ENDIF.

*      SELECT SINGLE * FROM tfain
*        INTO l_tfain
*       WHERE ident = calendario
*         AND jahr  = t_fechas-fecha(4)
*         AND von  <= t_fechas-fecha
*         AND bis  >= t_fechas-fecha
*         AND wert  = 1.
*      IF sy-subrc = 0.
*        t_fechas-laborable = 'X'.
*      ENDIF.

      APPEND t_fechas TO i_fechas.

      l_fecha = l_fecha + 1.
    ENDDO.
  ENDMETHOD.
  METHOD get_intervalos_horas.
    DATA: l_dia_fin TYPE d,
          l_fecha   TYPE d,
          l_horas   TYPE t_dia_horas,
          l_hora    TYPE sy-uzeit,
          l_orden   TYPE n LENGTH 3.

    IF dia_fin IS INITIAL.
      l_dia_fin = dia.
    ELSE.
      IF dia_fin < dia.
        mensaje = 'Día fin no puede ser menor que día inicial'(dfe).
        RETURN.
      ELSE.
        l_dia_fin = dia_fin.
      ENDIF.
    ENDIF.

    IF enduz < beguz.
      mensaje = 'Hora fin no puede ser menor que hora inicio'(hfe).
      RETURN.
    ENDIF.

    l_fecha = dia.
    WHILE l_fecha <= l_dia_fin.
      CLEAR l_horas.
      l_horas-dia = l_fecha.

      l_hora = beguz.
      CLEAR l_orden.
      WHILE l_hora <= enduz.
        l_horas-beguz = l_hora.
        l_hora = l_hora + minutos * 60.
        l_horas-enduz = l_hora - 1.

        l_orden = l_orden + 1.
        l_horas-orden = l_orden.
        APPEND l_horas TO intervalos.

        IF l_hora = '000000' AND l_hora < l_horas-beguz.
          l_hora = '240000'.
        ENDIF.

        IF l_hora >= enduz.
          EXIT.
        ENDIF.
      ENDWHILE.
      l_fecha = l_fecha + 1.
    ENDWHILE.
  ENDMETHOD.
  METHOD get_letra_numero_dia.
    DATA l_wotnr TYPE p LENGTH 8 DECIMALS 0.

    l_wotnr = get_numero_dia( fecha ).
    CASE l_wotnr.
      WHEN 1. letra_dia = 'L'.
      WHEN 2. letra_dia = 'M'.
      WHEN 3. letra_dia = 'X'.
      WHEN 4. letra_dia = 'J'.
      WHEN 5. letra_dia = 'V'.
      WHEN 6. letra_dia = 'S'.
      WHEN 7. letra_dia = 'D'.
    ENDCASE.
  ENDMETHOD.
  METHOD get_letra_numero_dia3.
    DATA l_wotnr TYPE p LENGTH 8 DECIMALS 0.

    l_wotnr = get_numero_dia( fecha ).
    CASE l_wotnr.
      WHEN 1. letra_dia = 'LUN'.
      WHEN 2. letra_dia = 'MAR'.
      WHEN 3. letra_dia = 'MIE'.
      WHEN 4. letra_dia = 'JUE'.
      WHEN 5. letra_dia = 'VIE'.
      WHEN 6. letra_dia = 'SAB'.
      WHEN 7. letra_dia = 'DOM'.
    ENDCASE.
  ENDMETHOD.
  METHOD get_meses.
    DATA: lv_current_date TYPE d,
          ls_date_range   TYPE t_meses,
          lv_month_end    TYPE d.

    CLEAR i_meses.

    " Verificar rango de fechas válido
    IF fini IS INITIAL OR ffin IS INITIAL OR fini > ffin.
      RETURN.
    ENDIF.

    " Comenzar con la fecha inicial
    lv_current_date = fini.

    " Procesar cada mes hasta alcanzar o superar la fecha final
    WHILE lv_current_date <= ffin.
      " Establecer fecha de inicio para este segmento
      ls_date_range-begda = lv_current_date.

      " Calcular último día del mes actual
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = lv_current_date
        IMPORTING
          last_day_of_month = lv_month_end.

      " Si el fin de mes supera la fecha final general, limitarlo
      IF lv_month_end > ffin.
        lv_month_end = ffin.
      ENDIF.

      " Establecer fecha final para este segmento
      ls_date_range-endda = lv_month_end.

      ls_date_range-perio = lv_current_date(6).
      " Agregar este rango de fechas a la tabla de resultados
      APPEND ls_date_range TO i_meses.

      " Moverse al primer día del siguiente mes
      lv_current_date = lv_month_end + 1.
    ENDWHILE.
  ENDMETHOD.
  METHOD get_nombre_mes.
    CLEAR nombre.
    SELECT SINGLE ltx FROM t247
      INTO nombre
    WHERE spras = idioma
      AND mnr   = mes.
  ENDMETHOD.
  METHOD get_nombre_mes_corto.
    CLEAR nombre.
    SELECT SINGLE ktx FROM t247
      INTO nombre
    WHERE spras = spras
      AND mnr   = mes.
  ENDMETHOD.
  METHOD get_nombre_numero_dia.
    DATA l_wotnr TYPE p LENGTH 8 DECIMALS 0.

    l_wotnr = get_numero_dia( fecha ).
    CASE l_wotnr.
      WHEN 1. dia = 'LUNES'.
      WHEN 2. dia = 'MARTES'.
      WHEN 3. dia = 'MIERCOLES'.
      WHEN 4. dia = 'JUEVES'.
      WHEN 5. dia = 'VIERNES'.
      WHEN 6. dia = 'SABADO'.
      WHEN 7. dia = 'DOMINGO'.
    ENDCASE.
  ENDMETHOD.
  METHOD get_num_dias_laborales.
    DATA it_days TYPE TABLE OF rke_dat.

    IF fini > ffin.
      RETURN.
    ELSE.
      CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
        EXPORTING
          i_datab               = fini
          i_datbi               = ffin
          i_factid              = fabkl
        TABLES
          eth_dats              = it_days
        EXCEPTIONS
          date_conversion_error = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      dias_laborables = lines( it_days ).
    ENDIF.
  ENDMETHOD.
  METHOD get_numero_dia.
    DATA l_wotnr TYPE p LENGTH 8 DECIMALS 0.

    CALL FUNCTION 'DAY_IN_WEEK'
      EXPORTING
        datum = fecha
      IMPORTING
        wotnr = l_wotnr.
    num_dia = l_wotnr.
  ENDMETHOD.
  METHOD get_periodo_anterior.
    DATA l_fecha TYPE d.

    IF NOT per IS INITIAL.
      CONCATENATE per '01' INTO l_fecha.
    ELSE.
      l_fecha = fecha.
    ENDIF.
    l_fecha = suma_meses( meses = -1 fecha = l_fecha ).

    periodo = l_fecha(6).
  ENDMETHOD.
  METHOD get_periodo_siguiente.
    DATA l_fecha TYPE d.

    IF NOT per IS INITIAL.
      CONCATENATE per '01' INTO l_fecha.
    ELSE.
      l_fecha = fecha.
    ENDIF.
    l_fecha = suma_meses( meses = 1 fecha = l_fecha ).

    periodo = l_fecha(6).
  ENDMETHOD.
  METHOD get_primer_dia_mes.
    nueva_fecha = fecha.
    nueva_fecha+6(2) = '01'.
  ENDMETHOD.
  METHOD get_primer_dia_semana.
    DATA: l_week  TYPE scal-week,
          l_fecha TYPE dats.

    IF NOT semana IS INITIAL.
      l_week = semana.
    ELSE.
      IF suma_semanas IS INITIAL.
        l_fecha = fecha_sel.
      ELSE.
        l_fecha = fecha_sel + 7 * suma_semanas.
      ENDIF.
      l_week = get_semana( l_fecha ).
    ENDIF.

    CALL FUNCTION 'WEEK_GET_FIRST_DAY' ##FM_SUBRC_OK
      EXPORTING
        week         = l_week
      IMPORTING
        date         = fecha
      EXCEPTIONS
        week_invalid = 1
        OTHERS       = 2.
  ENDMETHOD.
  METHOD get_semana.
    CALL FUNCTION 'DATE_GET_WEEK' ##FM_SUBRC_OK
      EXPORTING
        date         = fecha
      IMPORTING
        week         = semana
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
  ENDMETHOD.
  METHOD get_trimestre.
    CASE fecha+4(2).
      WHEN '01' OR '02' OR '03'. trimestre = 'T1'.
      WHEN '04' OR '05' OR '06'. trimestre = 'T2'.
      WHEN '07' OR '08' OR '09'. trimestre = 'T3'.
      WHEN '10' OR '11' OR '12'. trimestre = 'T4'.
    ENDCASE.
  ENDMETHOD.
  METHOD get_ultimo_dia_mes.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS' ##FM_SUBRC_OK
      EXPORTING
        day_in            = fecha
      IMPORTING
        last_day_of_month = nueva_fecha
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
  ENDMETHOD.
  METHOD get_ultimo_dia_mes_anterior.
    DATA l_fecha TYPE d.

    CONCATENATE fecha(6) '01' INTO l_fecha.
    nueva_fecha = l_fecha - 1.
  ENDMETHOD.
  METHOD get_ultimo_dia_semana.
    fecha = get_primer_dia_semana( semana ).
    fecha = fecha + 6.
  ENDMETHOD.
  METHOD hay_solapamiento_fechas.
    DATA: l_ini1 TYPE c LENGTH 14,
          l_ini2 TYPE c LENGTH 14,
          l_fin1 TYPE c LENGTH 14,
          l_fin2 TYPE c LENGTH 14.

    CONCATENATE fini1 hini1 INTO l_ini1.
    CONCATENATE fini2 hini2 INTO l_ini2.
    CONCATENATE ffin1 hfin1 INTO l_fin1.
    CONCATENATE ffin2 hfin2 INTO l_fin2.

    CLEAR hay_solapamiento.
    IF     l_ini1 <= l_fin2
       AND l_fin1 >= l_ini2.

      hay_solapamiento = 'X'.

    ENDIF.
  ENDMETHOD.
  METHOD interseccion_fechas.
    CLEAR: rfini, rffin, rhini, rhfin.

    IF hay_solapamiento_fechas( fini1 = fini1 ffin1 = ffin1
                                fini2 = fini2 ffin2 = ffin2
                                hini1 = hini1 hfin1 = hfin1
                                hini2 = hini2 hfin2 = hfin2 ) <> 'X'.
      RETURN.
    ENDIF.

    IF fini1 = fini2.
      rfini = fini1.
      IF hini1 >= hini2.
        rhini = hini1.
      ELSE.
        rhini = hini2.
      ENDIF.
    ELSEIF fini1 < fini2.
      rfini = fini2.
      rhini = hini2.
    ELSE.
      rfini = fini1.
      rhini = hini1.
    ENDIF.

    IF ffin1 = ffin2.
      rffin = ffin1.
      IF hfin1 >= hfin2.
        rhfin = hfin2.
      ELSE.
        rhfin = hfin1.
      ENDIF.
    ELSEIF ffin1 < ffin2.
      rffin = ffin1.
      rhfin = hfin1.
    ELSE.
      rffin = ffin2.
      rhfin = hfin2.
    ENDIF.
  ENDMETHOD.
  METHOD lista_dias.
    CLEAR i_dias.
    CALL FUNCTION 'DAY_ATTRIBUTES_GET' ##FM_SUBRC_OK
      EXPORTING
        factory_calendar           = fcal
        holiday_calendar           = hcal
        date_from                  = fini
        date_to                    = ffin
      TABLES
        day_attributes             = i_dias
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.
  ENDMETHOD.
  METHOD periodos_resta_fechas.

    CLEAR: anyos, meses, dias.
    CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES' ##FM_SUBRC_OK
      EXPORTING
        date1                   = fecha1
        date2                   = fecha2
        output_format           = output_format
      IMPORTING
        years                   = anyos
        months                  = meses
        days                    = dias
      EXCEPTIONS
        invalid_dates_specified = 1
        OTHERS                  = 2.
    if sy-subrc ne 0.
      CLEAR: anyos, meses, dias.
    endif.

  ENDMETHOD.
  METHOD popup_fecha.
    DATA l_linea1 TYPE c LENGTH 132.

    IF linea1 IS INITIAL.
      l_linea1 = titulo.
    ELSE.
      l_linea1 = linea1.
    ENDIF.

    CALL FUNCTION 'TR_POPUP_INPUT_DATE'
      EXPORTING
        iv_title               = titulo
        iv_description1        = l_linea1
        iv_description2        = linea2
        iv_date                = fecha
      IMPORTING
        ev_date                = fecha_salida
      EXCEPTIONS
        action_aborted_by_user = 1
        value_not_changed      = 2.

    IF sy-subrc <> 0.
      CLEAR fecha_salida.
    ENDIF.
  ENDMETHOD.
  METHOD primer_dia_lab_desde_fecha.
*       Si la fecha dada es laborable devuelve esa fecha,
*       sino devuelve el primer día laborable a partir de esa fecha

    CLEAR fecha_salida.
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE' ##FM_SUBRC_OK
      EXPORTING
        correct_option               = correct_option
        date                         = fecha
        factory_calendar_id          = calid
      IMPORTING
        date                         = fecha_salida
      EXCEPTIONS
        calendar_buffer_not_loadable = 1
        correct_option_invalid       = 2
        date_after_range             = 3
        date_before_range            = 4
        date_invalid                 = 5
        factory_calendar_not_found   = 6
        OTHERS                       = 7.
  ENDMETHOD.
  METHOD rango2fechas.
    DATA: l_fecha      TYPE tpms_r_date,
          r_fechas_aux TYPE tpmy_r_date.

    DESCRIBE TABLE r_fechas LINES sy-tfill.

    IF sy-tfill = 0.
      fecha_desde = '00000000'.
      fecha_hasta = '99991231'.
    ELSEIF sy-tfill = 1.
      READ TABLE r_fechas INTO l_fecha INDEX 1.
      fecha_desde = l_fecha-low.
      IF l_fecha-high IS INITIAL.
        IF l_fecha-option(1) = 'G'.
          fecha_hasta = '99991231'.
        ELSE.
          fecha_hasta = fecha_desde.
        ENDIF.
      ELSE.
        fecha_hasta = l_fecha-high.
      ENDIF.
    ELSE.
      r_fechas_aux = r_fechas.
      SORT r_fechas_aux BY low.
      READ TABLE r_fechas_aux INTO l_fecha INDEX 1.
      fecha_desde = l_fecha-low.
      DESCRIBE TABLE r_fechas LINES sy-tfill.
      READ TABLE r_fechas_aux INTO l_fecha INDEX sy-tfill.
      IF l_fecha-high IS INITIAL.
        fecha_hasta = l_fecha-low.
        IF l_fecha-option(1) = 'G'.
          fecha_hasta = '99991231'.
        ELSE.
          fecha_hasta = l_fecha-low.
        ENDIF.
      ELSE.
        fecha_hasta = l_fecha-high.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD rango2horas.
    DATA: l_hora      TYPE emma_time_range,
          r_horas_aux TYPE emma_time_range_tab.

    DESCRIBE TABLE r_horas LINES sy-tfill.

    IF sy-tfill = 0.
      hora_desde = '000000'.
      hora_hasta = '235959'.
    ELSEIF sy-tfill = 1.
      READ TABLE r_horas INTO l_hora INDEX 1.
      hora_desde = l_hora-low.
      IF l_hora-high IS INITIAL.
        hora_hasta = hora_desde.
      ELSE.
        hora_hasta = l_hora-high.
      ENDIF.
    ELSE.
      r_horas_aux = r_horas.
      SORT r_horas_aux BY low.
      READ TABLE r_horas_aux INTO l_hora INDEX 1.
      hora_desde = l_hora-low.
      DESCRIBE TABLE r_horas LINES sy-tfill.
      READ TABLE r_horas_aux INTO l_hora INDEX sy-tfill.
      IF l_hora-high IS INITIAL.
        hora_hasta = l_hora-low.
      ELSE.
        hora_hasta = l_hora-high.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD rango2string.
    DATA: l_f1     TYPE c LENGTH 10,
          l_string TYPE string,
          l_f2     TYPE c LENGTH 10.

    FIELD-SYMBOLS <fechas> TYPE tpms_r_date.

    CLEAR string.
    LOOP AT r_fechas ASSIGNING <fechas>.
      IF NOT <fechas>-low IS INITIAL.
        WRITE <fechas>-low TO l_f1.
        IF <fechas>-high IS INITIAL OR <fechas>-low = <fechas>-high.
          l_string = l_f1.
        ELSE.
          WRITE <fechas>-high TO l_f2.
          CONCATENATE l_f1 'a' l_f2 INTO l_string SEPARATED BY space.
        ENDIF.
      ELSEIF NOT <fechas>-high IS INITIAL.
        WRITE <fechas>-high TO l_f1.
        l_string = l_f1.
      ENDIF.

      IF <fechas>-sign = 'E' OR <fechas>-option = 'NE'.
        CONCATENATE 'EXCLUIR' l_string INTO l_string.
      ENDIF.

      IF string IS INITIAL.
        string = l_string.
      ELSE.
        CONCATENATE string l_string INTO string SEPARATED BY ','.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD rango2tabla_semanas.
    DATA: l_begda  TYPE begda,
          l_endda  TYPE endda,
          l_fecha  TYPE d,
          l_cont   TYPE n LENGTH 3,
          l_semana TYPE zap_alv_columnas3,
          l_week   TYPE scal-week.

    rango2fechas( EXPORTING r_fechas = r_fechas
                  IMPORTING fecha_desde = l_begda
                            fecha_hasta = l_endda ).

    IF l_begda IS INITIAL.
      l_begda = fecha_inicio_si_vacia.
    ENDIF.

    l_fecha = l_begda + 1 - get_numero_dia( l_begda ).

    WHILE l_fecha <= l_endda.
      IF l_cont >= max_semanas.
        l_cont = max_semanas.
        CONCATENATE 'Se utilizará un máximo de' l_cont 'semanas' INTO mensaje SEPARATED BY space.
        EXIT.
      ENDIF.
      l_cont = l_cont + 1.
      CLEAR l_semana.
      l_semana-colum = l_cont.
      l_week = get_semana( l_fecha ).
      l_semana-aux1  = l_week.
      l_semana-begda = l_fecha.
      l_semana-endda = l_semana-begda + 6.
      APPEND l_semana TO i_semanas.
      l_fecha = l_fecha + 7.
    ENDWHILE.
  ENDMETHOD.
  METHOD rangofechas_2_rangoperio.
    DATA: l_fecha   TYPE tpms_r_date,
          l_periodo TYPE spmonr.

    CLEAR r_periodos.
    DESCRIBE TABLE r_fechas LINES sy-tfill.

    IF sy-tfill <= 0.
      RETURN.
    ENDIF.

    LOOP AT r_fechas INTO l_fecha.
      l_periodo-option = l_fecha-option.
      l_periodo-sign   = l_fecha-sign.
      l_periodo-low    = l_fecha-low(6).
      IF NOT l_fecha-high IS INITIAL.
        l_periodo-high = l_fecha-high(6).
      ENDIF.
      APPEND l_periodo TO r_periodos.
    ENDLOOP.
  ENDMETHOD.
  METHOD rangofechas_2_rangosem.
    DATA: l_fecha TYPE tpms_r_date,
          l_week  TYPE rsarch_rs_calweek.

    CLEAR r_semanas.
    IF r_fechas IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT r_fechas INTO l_fecha.
      l_week-option = 'BT'.
      l_week-sign   = l_fecha-sign.
      l_week-low    = get_semana( l_fecha-low ).
      IF l_fecha-high IS INITIAL.
        l_week-high = l_week-low.
      ELSE.
        l_week-high = get_semana( l_fecha-high ).
      ENDIF.
      APPEND l_week TO r_semanas.
    ENDLOOP.
  ENDMETHOD.
  METHOD rangoperio_2_rangofechas.
    DATA: l_periodo TYPE spmonr,
          l_fecha   TYPE tpms_r_date.

    CLEAR r_fechas.
    DESCRIBE TABLE r_periodos LINES sy-tfill.

    IF sy-tfill <= 0.
      RETURN.
    ENDIF.

    LOOP AT r_periodos INTO l_periodo.
      l_fecha-option = 'BT'.
      l_fecha-sign   = l_periodo-sign.
      CONCATENATE l_periodo-low '01' INTO l_fecha-low.
      IF l_periodo-high IS INITIAL.
        l_fecha-high = get_ultimo_dia_mes( l_fecha-low ).
      ELSE.
        CONCATENATE l_periodo-high '01' INTO l_fecha-high.
        l_fecha-high = get_ultimo_dia_mes( l_fecha-high ).
      ENDIF.
      APPEND l_fecha TO r_fechas.
    ENDLOOP.
  ENDMETHOD.
  METHOD rangoperio_2_tablaperio.
    DATA: l_periodo TYPE spmonr,
          l_fecha   TYPE tpms_r_date.

    CLEAR i_periodos.
    DESCRIBE TABLE r_periodos LINES sy-tfill.

    IF sy-tfill <= 0.
      RETURN.
    ENDIF.

    LOOP AT r_periodos INTO l_periodo.
      IF l_periodo-option = 'EQ'.
        APPEND l_periodo-low TO i_periodos.
      ELSE.
        CONCATENATE l_periodo-low '01' INTO l_fecha-low.
        IF l_periodo-high IS INITIAL.
          l_fecha-high = get_ultimo_dia_mes( l_fecha-low ).
        ELSE.
          CONCATENATE l_periodo-high '01' INTO l_fecha-high.
          l_fecha-high = get_ultimo_dia_mes( l_fecha-high ).
        ENDIF.
        WHILE l_fecha-low(6) <= l_fecha-high(6).
          APPEND l_fecha-low(6) TO i_periodos.
          l_fecha-low = suma_meses( fecha = l_fecha-low meses = 1 ).
        ENDWHILE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD rangosem_2_rangofechas.
    DATA: l_week  TYPE rsarch_rs_calweek,
          l_fecha TYPE tpms_r_date.

    CLEAR r_fechas.
    DESCRIBE TABLE r_semanas LINES sy-tfill.

    IF sy-tfill <= 0.
      RETURN.
    ENDIF.

    LOOP AT r_semanas INTO l_week.
      l_fecha-option = 'BT'.
      l_fecha-sign   = l_week-sign.
      l_fecha-low    = get_primer_dia_semana( l_week-low ).
      IF l_week-high IS INITIAL.
        l_fecha-high = get_ultimo_dia_semana( l_week-low ).
      ELSE.
        IF l_week-high+4(2) > '53'.
          l_fecha-high = |{ l_week-high(4) }1231|.
        ELSE.
          l_fecha-high = get_ultimo_dia_semana( l_week-high ).
          IF l_fecha-high(3) = '000'.
            l_fecha-high = |{ l_week-high(4) }1231|.
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND l_fecha TO r_fechas.
    ENDLOOP.
  ENDMETHOD.
  METHOD resta_fechas.
    DATA: l_ini1 TYPE c LENGTH 14,
          l_ini2 TYPE c LENGTH 14,
          l_fin1 TYPE c LENGTH 14,
          l_fin2 TYPE c LENGTH 14.

    CONCATENATE fini1 hini1 INTO l_ini1.
    CONCATENATE fini2 hini2 INTO l_ini2.
    CONCATENATE ffin1 hfin1 INTO l_fin1.
    CONCATENATE ffin2 hfin2 INTO l_fin2.

    rfini = fini1. rffin = ffin1.
    rhini = hini1. rhfin = hfin1.

    IF     l_ini1 <= l_fin2
       AND l_fin1 >= l_ini2.

      IF l_ini1 <= l_fin2.
        rfini = fini1.
        rhini = hini1.
      ELSE.
        rfini = ffin2.
        rhini = hfin2.
      ENDIF.

      IF l_fin1 >= l_ini2.
        rffin = ffin2.
        rhfin = hfin2.
      ELSE.
        rffin = fini2.
        rhfin = hini2.
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD string2fecha.
    DATA: l_cadena TYPE c LENGTH 20,
          l_long   TYPE i,
          l_aux1   TYPE c LENGTH 4,
          l_aux2   TYPE c LENGTH 4,
          l_aux3   TYPE c LENGTH 4,
          l_dia    TYPE c LENGTH 2,
          l_aux_a  TYPE c LENGTH 10,
          l_aux_b  TYPE c LENGTH 10,
          l_aux_c  TYPE c LENGTH 10,
          l_mes    TYPE c LENGTH 20,
          l_mesn   TYPE c LENGTH 2.

    l_cadena = cadena.
    DO 14 TIMES.
      IF l_cadena(1) = ''.
        l_cadena = l_cadena+1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    l_cadena = to_upper( l_cadena ).
    l_long = strlen( l_cadena ).

    IF l_cadena CS ` `.
      SPLIT l_cadena AT ` ` INTO l_aux1 l_aux2 l_aux3.
      l_aux2 = to_upper( l_aux2 ).
      IF strlen( l_aux1 ) = 1.
        l_dia = l_aux1.
        __poner_ceros l_dia.
        l_aux1 = l_dia.
      ENDIF.
    ENDIF.

    IF l_cadena CS '/'.
      IF corregir_errores = 'X'.
        SPLIT l_cadena AT '/' INTO l_aux_a l_aux_b l_aux_c.
* Sólo una barra.
        IF l_aux_c IS INITIAL.
          IF l_cadena CS '.'.
            REPLACE '.' WITH '/' INTO l_cadena.
            SPLIT l_cadena AT '/' INTO l_aux_a l_aux_b l_aux_c.
          ENDIF.
        ENDIF.
        IF l_aux_c IS INITIAL.
          IF strlen( l_aux_a ) = 5.
            l_cadena+2(1) = '/'.
          ENDIF.
        ENDIF.
      ENDIF.

      SPLIT l_cadena AT '/' INTO fecha+6(2) fecha+4(2) fecha(4).
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = fecha+6(2) ).
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = fecha+4(2) ).
      IF strlen( condense( fecha(4) ) ) = 2.
        fecha(4) = |20{ condense( fecha(4) ) }|.
      ENDIF.

    ELSEIF l_cadena CS '-'.
      SPLIT l_cadena AT '-' INTO l_aux1 l_aux2 l_aux3.
      IF strlen( l_aux1 ) = 2 AND strlen( l_aux2 ) = 3.
        fecha(4) = |20{ l_aux3 }|.
        fecha+6(2) = l_aux1.
        CASE l_aux2.
          WHEN 'ENE' OR 'JAN'. fecha+4(2) = '01'.
          WHEN 'FEB'. fecha+4(2) = '02'.
          WHEN 'MAR'. fecha+4(2) = '03'.
          WHEN 'ABR' OR 'APR'. fecha+4(2) = '04'.
          WHEN 'MAY'. fecha+4(2) = '05'.
          WHEN 'JUN'. fecha+4(2) = '06'.
          WHEN 'JUL'. fecha+4(2) = '07'.
          WHEN 'AGO' OR 'AUG'. fecha+4(2) = '08'.
          WHEN 'SEP'. fecha+4(2) = '09'.
          WHEN 'OCT'. fecha+4(2) = '10'.
          WHEN 'NOV'. fecha+4(2) = '11'.
          WHEN 'DIC' OR 'DEC'. fecha+4(2) = '12'.
        ENDCASE.
      ELSE.
        l_long = strlen( l_aux1 ).
        IF l_long = 4.
          fecha(4) = l_aux1.
          fecha+4(2) = l_aux2.
          fecha+6(2) = l_aux3.
        ELSEIF strlen( l_aux3 ) = 4.
          fecha(4) = l_aux3.
          fecha+4(2) = l_aux2.
          fecha+6(2) = l_aux1.
        ELSEIF strlen( l_aux3 ) = 2 AND strlen( l_aux1 ) = 2.
          fecha(4) = |20{ l_aux3 }|.
          fecha+4(2) = l_aux2.
          fecha+6(2) = l_aux1.
        ENDIF.
      ENDIF.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = fecha+6(2) ).
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = fecha+4(2) ).
    ELSEIF    l_cadena(3) = 'LUN' OR l_cadena(3) = 'MAR' OR l_cadena(3) = 'MIÉ' OR l_cadena(4) = 'JUE'
           OR l_cadena(3) = 'VIE' OR l_cadena(3) = 'SÁB' OR l_cadena(3) = 'DOM'.
      l_cadena = l_cadena+4.
      SPLIT l_cadena AT ' ' INTO l_dia l_mes.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_dia ).
      SELECT mnr FROM t247
        INTO l_mesn
        UP TO 1 ROWS
      WHERE spras = sy-langu
        AND ktx   = l_mes
       ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        CONCATENATE sy-datum(4) l_mesn l_dia INTO fecha.
      ENDIF.
    ELSEIF    l_aux2 = 'JAN' OR l_aux2 = 'FEB' OR l_aux2 = 'MAR' OR l_aux2 = 'APR'
           OR l_aux2 = 'MAY' OR l_aux2 = 'JUN' OR l_aux2 = 'JUL' OR l_aux2 = 'AUG'
           OR l_aux2 = 'SEP' OR l_aux2 = 'OCT' OR l_aux2 = 'NOV' OR l_aux2 = 'DEC'.
      IF strlen( l_aux1 ) = 2 AND strlen( l_aux2 ) = 3.
        fecha(4) = l_aux3.
        fecha+6(2) = l_aux1.
        CASE l_aux2.
          WHEN 'JAN'. fecha+4(2) = '01'.
          WHEN 'FEB'. fecha+4(2) = '02'.
          WHEN 'MAR'. fecha+4(2) = '03'.
          WHEN 'APR'. fecha+4(2) = '04'.
          WHEN 'MAY'. fecha+4(2) = '05'.
          WHEN 'JUN'. fecha+4(2) = '06'.
          WHEN 'JUL'. fecha+4(2) = '07'.
          WHEN 'AUG'. fecha+4(2) = '08'.
          WHEN 'SEP'. fecha+4(2) = '09'.
          WHEN 'OCT'. fecha+4(2) = '10'.
          WHEN 'NOV'. fecha+4(2) = '11'.
          WHEN 'DEC'. fecha+4(2) = '12'.
        ENDCASE.
      ELSE.
        l_long = strlen( l_aux1 ).
        IF l_long = 4.
          fecha(4) = l_aux1.
          fecha+4(2) = l_aux2.
          fecha+6(2) = l_aux3.
        ELSEIF strlen( l_aux3 ) = 4.
          fecha(4) = l_aux3.
          fecha+4(2) = l_aux2.
          fecha+6(2) = l_aux1.
        ELSEIF strlen( l_aux3 ) = 2 AND strlen( l_aux1 ) = 2.
          fecha(4) = |20{ l_aux3 }|.
          fecha+4(2) = l_aux2.
          fecha+6(2) = l_aux1.
        ENDIF.
      ENDIF.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = fecha+6(2) ).
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = fecha+4(2) ).
    ELSE.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = l_cadena
          accept_initial_date      = 'X'
        IMPORTING
          date_internal            = fecha
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2.
      IF sy-subrc <> 0.
        sy-tfill = strlen( l_cadena ).
        IF sy-tfill = 8.
          IF l_cadena(8) CO '0123456789'.
            fecha = l_cadena.
          ELSEIF l_cadena+6(2) CO '0123456789' AND l_cadena+3(2) CO '0123456789' AND l_cadena(2) CO '0123456789'.
            IF l_cadena+2(1) = '-' AND l_cadena+5(1) = '-'.
              CONCATENATE l_cadena+6(2) l_cadena+3(2) '20' l_cadena(2) INTO fecha.
            ELSEIF l_cadena+2(1) = ' ' AND l_cadena+5(1) = ' '.
              CONCATENATE l_cadena(2) l_cadena+3(2) '20' l_cadena+6(2) INTO fecha.
            ENDIF.
          ENDIF.
        ELSEIF sy-tfill = 10.
          IF l_cadena+2(1) = '-' AND l_cadena+5(1) = '-'.
            CONCATENATE l_cadena+6(4) l_cadena+3(2) l_cadena(2) INTO fecha.
          ENDIF.
        ENDIF.
        IF NOT fecha IS INITIAL.
          IF es_valida( fecha ) = ''.
            CLEAR fecha.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF fecha IS INITIAL.
      IF cadena CO '0123456789'.
        l_long = strlen( cadena ).
        TRY.
            IF cadena = 0.
              fecha = '00000000'.
            ELSEIF cadena <= 2958465.
              fecha = '19000101'.
              fecha = fecha + cadena - 2.
            ENDIF.
          CATCH cx_root. "#EC *
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD string2hora.
    DATA: l_hh TYPE c LENGTH 10,
          l_mm TYPE c LENGTH 10,
          l_ss TYPE c LENGTH 10,
          l_m  TYPE n LENGTH 2,
          l_s  TYPE n LENGTH 2,
          l_h  TYPE n LENGTH 2.

    IF string CS ':'.
      SPLIT string AT ':' INTO l_hh l_mm.
      IF l_mm CS ':'.
        SPLIT l_mm AT ':' INTO l_mm l_ss.
        l_m = l_mm(2).
        l_s = l_ss(2).
      ELSE.
        l_m = l_mm(2).
        l_s = '00'.
      ENDIF.
      l_h = l_hh(2).

      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_h ).
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_m ).
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_s ).
      CONCATENATE l_h l_m l_s INTO hora.
    ELSEIF string CS ','.
      l_hh = string.
      REPLACE ',' WITH '.' INTO l_hh.
      hora = l_hh * 24 * 3600.
    ELSE.
      IF strlen( string ) = 4.
        CONCATENATE string '00' INTO hora.
      ELSEIF strlen( string ) = 2.
        CONCATENATE string '0000' INTO hora.
      ELSEIF strlen( string ) = 1.
        CONCATENATE '0' string '0000' INTO hora.
      ELSE.
        hora = string.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD suma_dias_laborables.
    IF dias IS INITIAL.
      fecha_salida = fecha.
    ELSE.
      CALL FUNCTION 'BKK_ADD_WORKINGDAY'
        EXPORTING
          i_date      = fecha
          i_days      = dias
          i_calendar1 = calid
        IMPORTING
          e_date      = fecha_salida.
    ENDIF.
  ENDMETHOD.
  METHOD suma_horas.
    DATA l_horas TYPE p2012-anzhl.

    l_horas = horas.
    CALL FUNCTION 'CATT_ADD_TO_TIME'
      EXPORTING
        idate = fecha_ini
        itime = hora_ini
        stdaz = l_horas
      IMPORTING
        edate = fecha_fin
        etime = hora_fin.
  ENDMETHOD.
  METHOD suma_meses.
    CALL FUNCTION 'MONTH_PLUS_DETERMINE'
      EXPORTING
        months  = meses
        olddate = fecha
      IMPORTING
        newdate = nueva_fecha.
  ENDMETHOD.
  METHOD suma_semanas.
    DATA: l_fecha TYPE d,
          l_dias  TYPE i.

    l_fecha = get_primer_dia_semana( semana ).
    l_dias = dias + semanas * 7.
    l_fecha = l_fecha + l_dias.
    semana_fin = get_semana( l_fecha ).
  ENDMETHOD.
  METHOD timestamp_2_fechahora.
  DATA:
    l_tzone     TYPE sy-zonlo,
    l_timestamp TYPE tzntstmps,
    l_char(20).

CONSTANTS:
* high_value - 1 day
  gc_timestamp_high_minus TYPE tzntstmps VALUE '99991201000000',
* 2. January of the year 1.A lower value result in an error
  gc_timestamp_low_plus   TYPE tzntstmps VALUE '00010102000000',
* initial value date
  gc_date_low             LIKE sy-datum VALUE '00000000',
  gc_date_low_plus        LIKE sy-datum VALUE '00010102',
  gc_date_high_minus      LIKE sy-datum VALUE '99991201',
  gc_time_low             LIKE sy-uzeit VALUE '000000'.

*    DATA l_timestamp TYPE tzonref-tstamps.

    l_timestamp = timestamp.

*    CALL FUNCTION 'IB_CONVERT_FROM_TIMESTAMP'
*      EXPORTING
*        i_timestamp = l_timestamp
*        i_tzone     = tzone
*      IMPORTING
*        e_datlo     = fecha
*        e_timlo     = hora.


*  l_timestamp = i_timestamp.

  l_tzone = tzone.
  IF l_tzone = space.
    l_tzone = 'CET   '.
  ENDIF.
* the whole day first day of time calculation is set to 0
  IF l_timestamp < gc_timestamp_low_plus.
    l_timestamp = ibxx_utc_low.
  ENDIF.
* the whole day at the end of the year 9999 is set to the
* end of SAP time calculation.
  IF l_timestamp > gc_timestamp_high_minus.
    l_timestamp = ibxx_utc_high.
  ENDIF.

  CASE l_timestamp.
    WHEN ibxx_utc_low.
      fecha = gc_date_low.           "'00000000'.
      hora = gc_time_low.                                "'000000'.
    WHEN ibxx_utc_high.
      fecha = ibxx_date_high.        "'99991231'.
      hora = ibxx_time_high.                             "'235959'.
    WHEN OTHERS.                       "usual time
      CONVERT TIME STAMP l_timestamp TIME ZONE l_tzone
       INTO DATE fecha TIME hora.

      IF NOT sy-subrc IS INITIAL.
        l_char = l_timestamp.
        fecha = l_char(8).
        hora  = l_char+8(6).
      ENDIF.
  ENDCASE.

  ENDMETHOD.
  METHOD union_fechas.
    CLEAR: rfini, rffin, rhini, rhfin.

    IF fini1 = fini2.
      rfini = fini1.
      IF hini1 <= hini2.
        rhini = hini1.
      ELSE.
        rhini = hini2.
      ENDIF.
    ELSEIF fini1 > fini2.
      rfini = fini2.
      rhini = hini2.
    ELSE.
      rfini = fini1.
      rhini = hini1.
    ENDIF.

    IF ffin1 = ffin2.
      rffin = ffin1.
      IF hfin1 <= hfin2.
        rhfin = hfin2.
      ELSE.
        rhfin = hfin1.
      ENDIF.
    ELSEIF ffin1 > ffin2.
      rffin = ffin1.
      rhfin = hfin1.
    ELSE.
      rffin = ffin2.
      rhfin = hfin2.
    ENDIF.
  ENDMETHOD.