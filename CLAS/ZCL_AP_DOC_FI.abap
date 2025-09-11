CLASS zcl_ap_doc_fi DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA bkpf TYPE bkpf.

    CLASS-METHODS get_ebeln_simple
      IMPORTING bukrs        TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                belnr        TYPE bkpf-belnr
                gjahr        TYPE bkpf-gjahr
      EXPORTING VALUE(ebeln) TYPE bseg-ebeln
                VALUE(ebelp) TYPE bseg-ebelp.

    CLASS-METHODS get_bkpf
      IMPORTING bukrs       TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                belnr       TYPE bkpf-belnr
                gjahr       TYPE bkpf-gjahr
      RETURNING VALUE(bkpf) TYPE bkpf.

    CLASS-METHODS es_anulado
      IMPORTING bukrs          TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                belnr          TYPE bkpf-belnr
                gjahr          TYPE bkpf-gjahr
      RETURNING VALUE(anulado) TYPE bkpf-xnetb.

    CLASS-METHODS get_importe_por_cuenta
      IMPORTING bukrs          TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                hkont          TYPE bseg-hkont
                zuonr          TYPE bseg-zuonr OPTIONAL
                blart          TYPE bkpf-blart OPTIONAL
                xref3          TYPE bseg-xref3 OPTIONAL
                aufnr          TYPE bseg-aufnr OPTIONAL
                ebeln          TYPE bseg-ebeln OPTIONAL
                ebelp          TYPE bseg-ebelp OPTIONAL
      RETURNING VALUE(importe) TYPE dmbtrv.

    CLASS-METHODS get_importe_por_cuenta_doc
      IMPORTING bukrs          TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                belnr          TYPE bkpf-belnr
                gjahr          TYPE bkpf-gjahr
                koart          TYPE bseg-koart DEFAULT 'S'
                hkont          TYPE bseg-hkont
      RETURNING VALUE(importe) TYPE dmbtrv.

    CLASS-METHODS insertar_url_gos_st
      IMPORTING bukrs        TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                belnr        TYPE bkpf-belnr
                gjahr        TYPE bkpf-gjahr
                url          TYPE string
                titulo       TYPE string
                posiciones   TYPE abap_bool  DEFAULT ''
      RETURNING VALUE(error) TYPE ekko-loekz.

    CLASS-METHODS get_url_por_titulo_st
      IMPORTING bukrs      TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                belnr      TYPE bkpf-belnr
                gjahr      TYPE bkpf-gjahr
                titulo     TYPE string
      RETURNING VALUE(url) TYPE string.

    CLASS-METHODS get_ult_doc_por_cuenta
      IMPORTING bukrs TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                hkont TYPE bseg-hkont
                zuonr TYPE bseg-zuonr OPTIONAL
                blart TYPE bkpf-blart OPTIONAL
                xref3 TYPE bseg-xref3 OPTIONAL
                aufnr TYPE bseg-aufnr OPTIONAL
                ebeln TYPE bseg-ebeln OPTIONAL
                ebelp TYPE bseg-ebelp OPTIONAL
      CHANGING  belnr TYPE bkpf-belnr
                gjahr TYPE bkpf-gjahr.

    CLASS-METHODS fb03
      IMPORTING bukrs TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                belnr TYPE bkpf-belnr
                gjahr TYPE bkpf-gjahr
                buzei TYPE bseg-buzei OPTIONAL.

    CLASS-METHODS cuenta_tiene_imputacion
      IMPORTING bukrs     TYPE bkpf-bukrs
                saknr     TYPE skb1-saknr
                campo     TYPE any
      RETURNING VALUE(si) TYPE bkpf-xnetb.

    CLASS-METHODS convertir_moneda_eur
      IMPORTING importe            TYPE any
                moneda             TYPE waers
                wkurs              TYPE wkurs OPTIONAL
                fecha              TYPE dats  DEFAULT sy-datum
                type_of_rate       TYPE any   DEFAULT 'M'
                moneda_local       TYPE waers DEFAULT 'EUR'
      RETURNING VALUE(importe_eur) TYPE netwr.

    CLASS-METHODS get_importe_iva
      IMPORTING bukrs          TYPE bukrs DEFAULT zcl_c=>bukrs
                mwskz          TYPE mwskz
                waers          TYPE waers DEFAULT zcl_c=>waers
                wrbtr          TYPE any
      RETURNING VALUE(importe) TYPE bseg-wrbtr.

    CLASS-METHODS actualizar_campo_bseg
      IMPORTING campo TYPE string
                valor TYPE any
                bukrs TYPE bukrs
                belnr TYPE belnr_d
                gjahr TYPE gjahr
                buzei TYPE buzei.

    CLASS-METHODS es_periodo_abierto
      IMPORTING bukrs             TYPE bukrs       DEFAULT zcl_c=>bukrs
                gjahr             TYPE gjahr       OPTIONAL
                monat             TYPE monat       OPTIONAL
                budat             TYPE budat       OPTIONAL
                koart             TYPE t001b-mkoar DEFAULT '+'
                konto             TYPE t001b-vkont DEFAULT '+'
                glvor             TYPE glvor       DEFAULT 'RFBU'
      RETURNING VALUE(es_abierto) TYPE abap_bool.

    CLASS-METHODS get_datos_iva
      IMPORTING bukrs          TYPE bukrs DEFAULT zcl_c=>bukrs
                fecha          TYPE dats  DEFAULT sy-datum
                mwskz          TYPE mwskz
                waers          TYPE waers DEFAULT zcl_c=>waers
                wrbtr          TYPE any
      RETURNING VALUE(importe) TYPE bseg-wrbtr.

    CLASS-METHODS get_imp_iva_desde_bruto
      IMPORTING bukrs          TYPE bukrs DEFAULT zcl_c=>bukrs
                mwskz          TYPE mwskz
                waers          TYPE waers DEFAULT zcl_c=>waers
                wrbtr          TYPE any
      RETURNING VALUE(importe) TYPE bseg-wrbtr.

    CLASS-METHODS get_utilizacion_pago
      IMPORTING bukrs         TYPE bukrs
                augbl         TYPE augbl
                augdt         TYPE augdt
                cuentas       TYPE abap_bool DEFAULT 'X'
                proveedor     TYPE abap_bool DEFAULT ''
                cliente       TYPE abap_bool DEFAULT ''
                blart         TYPE blart     DEFAULT ''
      RETURNING VALUE(i_bseg) TYPE bseg_t.

    CLASS-METHODS get_nombre_cuenta
      IMPORTING saknr        TYPE saknr    OPTIONAL
                ktopl        TYPE ktopl    DEFAULT zcl_c=>plan_cuentas
                spras        TYPE sy-langu DEFAULT sy-langu
      PREFERRED PARAMETER saknr
      RETURNING VALUE(txt20) TYPE txt20_skat.

    CLASS-METHODS get_fecha_pago
      IMPORTING fecha             TYPE dats
                zterm             TYPE dzterm
      RETURNING VALUE(fecha_pago) TYPE dats.

    CLASS-METHODS get_tipo_cambio
      IMPORTING fecha       TYPE sy-datum    DEFAULT sy-datum
                mon_ext     TYPE waers
                mon_local   TYPE waers       DEFAULT 'EUR'
                tipo_cambio TYPE tcurr-kurst DEFAULT 'M'
      RETURNING VALUE(tipo) TYPE tcurr-ukurs.

    CLASS-METHODS get_bschl_anul
      IMPORTING bschl             TYPE bschl
      RETURNING VALUE(bschl_anul) TYPE bschl.

    CLASS-METHODS get_saldo_cuenta
      IMPORTING hkont        TYPE hkont
                fecha        TYPE dats  DEFAULT sy-datum
                bukrs        TYPE bukrs DEFAULT zcl_c=>bukrs
      RETURNING VALUE(saldo) TYPE cfsaldo.

    CLASS-METHODS visualizar
      IMPORTING bukrs TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                belnr TYPE bkpf-belnr
                gjahr TYPE bkpf-gjahr.

    CLASS-METHODS convertir_moneda_ext
      IMPORTING importe_local  TYPE any
                moneda         TYPE waers
                wkurs          TYPE wkurs OPTIONAL
                fecha          TYPE dats  DEFAULT sy-datum
                type_of_rate   TYPE any   DEFAULT 'M'
                moneda_local   TYPE waers DEFAULT 'EUR'
      RETURNING VALUE(importe) TYPE netwr.

    CLASS-METHODS anular_documento
      IMPORTING bukrs      TYPE bkpf-bukrs DEFAULT zcl_c=>bukrs
                belnr      TYPE bkpf-belnr
                gjahr      TYPE bkpf-gjahr
                budat      TYPE budat      OPTIONAL
                modo       TYPE bdc_amod   DEFAULT 'N'
      EXPORTING gjahr_anul TYPE bkpf-gjahr
                belnr_anul TYPE bkpf-belnr
                !message   TYPE bapi_msg.

    CLASS-METHODS modificacion_campo_pos
      IMPORTING bseg            TYPE bseg
                campo           TYPE any
                segundos_espera TYPE int2      DEFAULT 0
                !commit         TYPE abap_bool DEFAULT ''
      RETURNING VALUE(message)  TYPE bapi_msg.

    CLASS-METHODS anular_compensacion
      IMPORTING bukrs            TYPE bkpf-bukrs  DEFAULT zcl_c=>bukrs
                belnr            TYPE bkpf-belnr
                gjahr            TYPE bkpf-gjahr
                budat            TYPE budat       OPTIONAL
                anular_documento TYPE abap_bool   DEFAULT ''
                modo             TYPE bdc_amod    DEFAULT 'N'
                stgrd            TYPE rf05r-stgrd DEFAULT '01'
      EXPORTING gjahr_anul       TYPE bkpf-gjahr
                !message         TYPE bapi_msg
                belnr_anul       TYPE bkpf-belnr
                anul_ok          TYPE abap_bool.

    CLASS-METHODS get_saldo_cuenta_glt0
      IMPORTING hkont          TYPE hkont
                fecha          TYPE dats      DEFAULT sy-datum
                bukrs          TYPE bukrs     DEFAULT zcl_c=>bukrs
                periodo        TYPE spmon     OPTIONAL
                solo_valor_mes TYPE abap_bool DEFAULT ''
      RETURNING VALUE(saldo)   TYPE cfsaldo.

    CLASS-METHODS info_partida
      IMPORTING fecha     TYPE dats DEFAULT sy-datum
      CHANGING  rfposxext TYPE rfposxext
                t001      TYPE t001 OPTIONAL.

    CLASS-METHODS declarado_sii
      IMPORTING bukrs            TYPE bkpf-bukrs
                belnr            TYPE bkpf-belnr
                gjahr            TYPE bkpf-gjahr
      RETURNING VALUE(declarado) TYPE abap_bool.

    CLASS-METHODS compensar_por_documento
      IMPORTING kunnr                TYPE kunnr
                budat                TYPE budat     DEFAULT sy-datum
                bukrs                TYPE bukrs     DEFAULT zcl_c=>bukrs
                waers                TYPE waers     DEFAULT 'EUR'
                documentos           TYPE tcm_t_co_belnr
                modo_ct              TYPE bdcmode   DEFAULT 'N'
                cmes                 TYPE any
                parar_si_diferencias TYPE abap_bool DEFAULT ''
      EXPORTING !message             TYPE bapi_msg
                belnr                TYPE bkpf-belnr
                gjahr                TYPE bkpf-gjahr.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_DOC_FI definition
class ZCL_AP_DOC_FI implementation.
  METHOD actualizar_campo_bseg.
    DATA: l_aux   TYPE sgtxt,
          l_set   TYPE string,
          l_koart TYPE koart,
          l_kunnr TYPE kunnr,
          l_lifnr TYPE lifnr,
          l_hkont TYPE hkont,
          l_augbl TYPE augbl,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_dd03l TYPE dd03l,
          l_tabla TYPE string.

    WRITE valor TO l_aux. " EC *
    CONCATENATE campo '=' l_aux INTO l_set SEPARATED BY space.

    SELECT SINGLE koart kunnr lifnr hkont augbl FROM bseg
      INTO (l_koart, l_kunnr, l_lifnr, l_hkont, l_augbl)
       WHERE bukrs = bukrs
         AND belnr = belnr
         AND gjahr = gjahr
         AND buzei = buzei.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    UPDATE bseg
       SET (l_set)
     WHERE bukrs = bukrs
       AND belnr = belnr
       AND gjahr = gjahr
       AND buzei = buzei.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE l_koart.
      WHEN 'D'.
        SELECT SINGLE fieldname FROM dd03l
          INTO l_dd03l-fieldname
         WHERE tabname   = 'BSID'
           AND fieldname = campo.
        IF sy-subrc = 0.
          IF l_augbl IS INITIAL.
            l_tabla = 'BSID'.
          ELSE.
            l_tabla = 'BSAD'.
          ENDIF.
          UPDATE (l_tabla)
           SET (l_set)
         WHERE bukrs = bukrs
           AND kunnr = l_kunnr
           AND belnr = belnr
           AND gjahr = gjahr
           AND buzei = buzei.
        ENDIF.
      WHEN 'K'.
        SELECT SINGLE fieldname FROM dd03l
          INTO l_dd03l-fieldname
         WHERE tabname   = 'BSIK'
           AND fieldname = campo.
        IF sy-subrc = 0.
          IF l_augbl IS INITIAL.
            l_tabla = 'BSIK'.
          ELSE.
            l_tabla = 'BSAK'.
          ENDIF.
          UPDATE (l_tabla)
           SET (l_set)
         WHERE bukrs = bukrs
           AND lifnr = l_lifnr
           AND belnr = belnr
           AND gjahr = gjahr
           AND buzei = buzei.
        ENDIF.

      WHEN 'S'.
        SELECT SINGLE fieldname FROM dd03l
          INTO l_dd03l-fieldname
         WHERE tabname   = 'BSIS'
           AND fieldname = campo.
        IF sy-subrc = 0.
          IF l_augbl IS INITIAL.
            l_tabla = 'BSIS'.
          ELSE.
            l_tabla = 'BSAS'.
          ENDIF.
          UPDATE (l_tabla)
           SET (l_set)
         WHERE bukrs = bukrs
           AND hkont = l_hkont
           AND belnr = belnr
           AND gjahr = gjahr
           AND buzei = buzei.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD anular_compensacion.
    DATA o_bi TYPE REF TO zcl_ap_batch_input.

    CLEAR: anul_ok, gjahr_anul, belnr_anul, message.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Imagen de selecc. anulación de conciliación
    o_bi->dynpro( program = 'SAPMF05R' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=RAGL' ).
    o_bi->campos( campo = 'RF05R-AUGBL' valor = belnr ). " Número del documento de compensación
    o_bi->campos( campo = 'RF05R-BUKRS' valor = bukrs ). " Sociedad
    o_bi->campos( campo = 'RF05R-GJAHR' valor = gjahr ). " Ejercicio
    IF NOT stgrd IS INITIAL.
      o_bi->campos( campo = 'RF05R-STGRD' valor = stgrd ).  " Motivo de anulación
    ENDIF.
    message = o_bi->llamar_transaccion( tcode = 'FBRA' modo = modo ).

    IF o_bi->msgty <> 'S'.
      RETURN.
    ELSE.
      IF o_bi->msgid = 'F5' AND o_bi->msgno = '539'.
        anul_ok = 'X'.
      ENDIF.
      CLEAR message.
      IF anular_documento = 'X'.
        SELECT COUNT( * ) FROM bseg
          INTO sy-tfill
         WHERE bukrs = bukrs
           AND belnr = belnr
           AND gjahr = gjahr.
        IF sy-tfill > 0.
          anular_documento( EXPORTING bukrs = bukrs belnr = belnr gjahr = gjahr budat = budat modo = modo
                            IMPORTING belnr_anul = belnr_anul gjahr_anul = gjahr_anul message = message ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD anular_documento.
    DATA: l_bkpf  LIKE bkpf,
          l_stgrd TYPE uf05a-stgrd,
          o_bi    TYPE REF TO zcl_ap_batch_input.

    SELECT SINGLE budat gjahr stblg FROM bkpf
      INTO CORRESPONDING FIELDS OF l_bkpf
     WHERE bukrs = bukrs
       AND belnr = belnr
       AND gjahr = gjahr.
    IF sy-subrc = 0 AND NOT l_bkpf-stblg IS INITIAL.
      message = 'Documento ya anulado'(dya).
      belnr_anul = l_bkpf-stblg.
      gjahr_anul = l_bkpf-gjahr.
    ELSE.
      IF     NOT budat        IS INITIAL
         AND     l_bkpf-budat <> budat.
        l_stgrd = '02'.
      ELSE.
        l_stgrd = '01'.
      ENDIF.

      o_bi = NEW #( ).

      o_bi->inicio( ).

      o_bi->dynpro( program = 'SAPMF05A' dynpro = '0105' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).
      o_bi->campos( campo = 'RF05A-BELNS' valor = belnr ). " Documento a anular
      o_bi->campos( campo = 'BKPF-BUKRS' valor = bukrs ). " Sociedad
      o_bi->campos( campo = 'RF05A-GJAHS' valor = gjahr ). " Ejercicio
      o_bi->campos( campo = 'UF05A-STGRD' valor = l_stgrd ). " Motivo de anulación o contabilización inversa

      IF NOT budat IS INITIAL.
        o_bi->campos( campo = 'BSIS-BUDAT' valor = budat ).
      ENDIF.

      message = o_bi->llamar_transaccion( tcode = 'FB08' modo = modo ).

      IF     o_bi->msgty = 'S' AND o_bi->msgid = 'F5'
         AND o_bi->msgno = '312'.
        belnr_anul = o_bi->msgv1.
        zcl_ap_string=>poner_ceros_c( CHANGING cadena = belnr_anul ).
        IF budat IS INITIAL.
          gjahr_anul = l_bkpf-gjahr.
        ELSE.
          gjahr_anul = budat(4).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD compensar_por_documento.
    DATA: l_tcode   TYPE sy-tcode,
          o_bi      TYPE REF TO zcl_ap_batch_input,
          l_ind     TYPE numc2,
          l_mensaje TYPE bapireturn1-message.

    IF NOT kunnr IS INITIAL.
      l_tcode = 'F-32'.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMF05A' dynpro = '0131' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'RF05A-AGKON' valor = kunnr ). " Número de cuenta o clave de un pool de trabajo
    o_bi->campos( campo = 'BKPF-BUDAT' valor = budat ). " Fecha de contabilización en el documento
    o_bi->campos( campo = 'BKPF-BUKRS' valor = bukrs ). " Sociedad
    o_bi->campos( campo = 'BKPF-WAERS' valor = waers ). " Clave de moneda
    o_bi->campos( campo = 'RF05A-XNOPS' valor = 'X' ). " Indicador: ¿seleccionar sólo PAs que no sean oper.CME?
    o_bi->campos( campo = 'RF05A-XPOS1(01)' valor = '' ). " Campo para marcar
    o_bi->campos( campo = 'RF05A-XPOS1(03)' valor = 'X' ). " Campo para marcar
    o_bi->campos( campo = 'RF05A-AGUMS' valor = cmes ).

    o_bi->dynpro( program = 'SAPMF05A' dynpro = '0731' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).

    LOOP AT documentos ASSIGNING FIELD-SYMBOL(<belnr>).
      l_ind = sy-tabix.
      o_bi->campos( campo = 'RF05A-SEL01' ind = l_ind valor = <belnr> ). " Campo entrada p.concepto búsqueda p.selección part.abiertas
    ENDLOOP.

    message = o_bi->llamar_transaccion( tcode = 'F-32' modo = modo_ct ).

    IF o_bi->msgid = 'F5' AND o_bi->msgno = '312' AND o_bi->msgty = 'S'.
      belnr = sy-msgv1.
      __poner_ceros belnr.
      gjahr = budat(4).
      CLEAR message.
    ELSE.
* Si falla con este mensaje es porque hay diferencias.
      IF message CS 'SAPMF05A 0700'.
        IF parar_si_diferencias = 'X' AND modo_ct = 'N'.
          message = o_bi->llamar_transaccion( tcode = 'F-32' modo = 'E' ).
        ELSE.
          message = 'No es posible compensar por existir diferencias'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD convertir_moneda_eur.
    CLEAR importe_eur.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date                    = fecha
        foreign_amount          = importe
        foreign_currency        = moneda
        local_currency          = moneda_local
        rate                    = wkurs
        type_of_rate            = type_of_rate
*     READ_TCURR              = 'X'
      IMPORTING
*     EXCHANGE_RATE           =
*     FOREIGN_FACTOR          =
        local_amount            = importe_eur
*     LOCAL_FACTOR            =
*     EXCHANGE_RATEX          =
*     FIXED_RATE              =
*     DERIVED_RATE_TYPE       =
     EXCEPTIONS
       no_rate_found           = 1
       overflow                = 2
       no_factors_found        = 3
       no_spread_found         = 4
       derived_2_times         = 5
       OTHERS                  = 6.
  ENDMETHOD.
  METHOD convertir_moneda_ext.
    CLEAR importe.

    CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
      EXPORTING
*   CLIENT                  = SY-MANDT
        date                    = fecha
        foreign_currency        = moneda
        local_amount            = importe_local
        local_currency          = moneda_local
        rate                    = wkurs
        type_of_rate            = type_of_rate
*   READ_TCURR              = 'X'
      IMPORTING
*   EXCHANGE_RATE           = EXCHANGE_RATE
        foreign_amount          = importe
*   FOREIGN_FACTOR          = FOREIGN_FACTOR
*   LOCAL_FACTOR            = LOCAL_FACTOR
*   EXCHANGE_RATEX          = EXCHANGE_RATEX
*   DERIVED_RATE_TYPE       = DERIVED_RATE_TYPE
*   FIXED_RATE              = FIXED_RATE
     EXCEPTIONS
       no_rate_found           = 1
       overflow                = 2
       no_factors_found        = 3
       no_spread_found         = 4
       derived_2_times         = 5.
  ENDMETHOD.
  METHOD cuenta_tiene_imputacion.
    TYPES: BEGIN OF t_cobs,
             bukrs TYPE bseg-bukrs,
             saknr TYPE bseg-saknr,
             field TYPE tcobf-field,
             mwskz TYPE bseg-mwskz,
             xintb TYPE skb1-xintb,
           END OF t_cobs.

    DATA: xcobf  TYPE TABLE OF tcobf,
          l_cobf TYPE tcobf,
          l_cobs TYPE t_cobs,
          xcobs  TYPE TABLE OF t_cobs.
    DATA: t169o TYPE t169o,
          skb1  TYPE skb1,
          t004f TYPE t004f.

    DATA: feldauswahl TYPE c LENGTH 140,
          i           TYPE i.

    CLEAR si.

    SELECT * FROM tcobf
      INTO TABLE xcobf
      ORDER BY PRIMARY KEY.

*------ Entfernen der Felder aus T169O
    LOOP AT xcobf INTO l_cobf.
      SELECT SINGLE * FROM t169o
      INTO t169o
       WHERE field = l_cobf-field.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      DELETE xcobf.
    ENDLOOP.

*-----  Kundendefinierte Felder gefüllt?
*------ Sachkonto lesen -----------------------------------------------*
    CALL FUNCTION 'READ_SACHKONTO'
      EXPORTING
        buchungskreis = bukrs
        sachkonto     = saknr
      IMPORTING
        sachkonto_wa  = skb1.

    IF campo = 'MWSKZ'.
      IF NOT skb1-mwskz IS INITIAL.
        si = 'X'.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM t004f INTO t004f WHERE bukrs = bukrs
                                            AND fstag = skb1-fstag.

*------ Steuerkennzeichen setzen -------------------------------------*
    IF skb1-mwskz = '< ' OR skb1-mwskz = '> '.
      l_cobs-mwskz = skb1-mwskz.
    ENDIF.

*------ Gegebenenfalls Flag "nur automatisch bebuchbar" setzen -------*
    IF skb1-xintb <> space OR skb1-mitkz <> space.
      l_cobs-xintb = 'X'.
    ENDIF.

*------ "Keyfelder" von XCOBS füllen ---------------------------------*
    l_cobs-bukrs = bukrs.
    l_cobs-saknr = saknr.
    APPEND l_cobs TO xcobs.

*------ Entfernen der Felder aufgrund T004F-Steuerung -----------------*
    LOOP AT xcobf INTO l_cobf.
      feldauswahl(50) = t004f-faus1.
      feldauswahl+90(50) = t004f-faus2.
      i = l_cobf-group1 - 1.
      SHIFT feldauswahl BY i PLACES.
      IF NOT ( feldauswahl(1) = '+' OR feldauswahl(1) = '.' ).
        CONTINUE.
      ENDIF.
      l_cobs-field = l_cobf-field.
      APPEND l_cobs TO xcobs.
    ENDLOOP.

    IF line_exists( xcobs[ field = campo ] ).
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD declarado_sii.
    DATA l_source_key TYPE edocument-source_key.

    CONCATENATE bukrs belnr gjahr INTO l_source_key.

    SELECT SINGLE source_key FROM edocument
      INTO l_source_key
     WHERE source_type = 'FI_INVOICE'
       AND source_key  = l_source_key.
    IF sy-subrc = 0.
      declarado = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD es_anulado.
    DATA l_bkpf TYPE bkpf.

    CLEAR anulado.
* Compruebo si tiene documento de anulaciÃ³n
    l_bkpf = get_bkpf( bukrs = bukrs belnr = belnr gjahr = gjahr ).
    IF NOT l_bkpf-stblg IS INITIAL.
      anulado = 'X'.
* Por si acaso, verificamos que estÃ© anulado, el documento anulaciÃ³n
      l_bkpf = get_bkpf( bukrs = bukrs
                         belnr = l_bkpf-stblg
                         gjahr = l_bkpf-stjah ).
      IF l_bkpf-stblg IS INITIAL.
        CLEAR anulado.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD es_periodo_abierto.
    DATA: l_gjahr  TYPE gjahr,
          l_monat  TYPE monat,
          l_period TYPE t001b-frpe1.

    IF NOT budat IS INITIAL.
      CALL FUNCTION 'FI_PERIOD_DETERMINE'
        EXPORTING
          i_budat        = budat
          i_bukrs        = bukrs
*       I_PERIV        = ' '
*       I_GJAHR        = 0000
*       I_MONAT        = 00
*       X_XMO16        = ' '
        IMPORTING
          e_gjahr        = l_gjahr
          e_monat        = l_monat
*       E_POPER        =
        EXCEPTIONS
          fiscal_year    = 1
          period         = 2
          period_version = 3
          posting_period = 4
          special_period = 5
          version        = 6
          posting_date   = 7
          OTHERS         = 8.
    ELSE.
      l_gjahr = gjahr.
      l_monat = monat.
    ENDIF.

    l_period = l_monat.

    CALL FUNCTION 'FI_PERIOD_CHECK'
      EXPORTING
        i_bukrs          = bukrs
*     I_OPVAR          = ' '
        i_gjahr          = l_gjahr
        i_koart          = koart
        i_konto          = konto
        i_monat          = l_period
        i_glvor          = glvor
*     I_SPERI          =
* IMPORTING
*     E_OPER           =
      EXCEPTIONS
        error_period     = 1
        error_period_acc = 2
        OTHERS           = 3.

    IF sy-subrc = 0.
      es_abierto = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD fb03.
    SET PARAMETER ID: 'BLN' FIELD belnr,
                      'BUK' FIELD bukrs,
                      'GJR' FIELD gjahr.

    IF buzei IS INITIAL.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.       "#EC CI_CALLTA
    ELSE.
      SET PARAMETER ID 'BUZ' FIELD buzei.
      CALL TRANSACTION 'FB09' AND SKIP FIRST SCREEN.       "#EC CI_CALLTA
    ENDIF.
  ENDMETHOD.
  METHOD get_bkpf.
    SELECT SINGLE * FROM bkpf
      INTO bkpf
     WHERE bukrs = bukrs
       AND belnr = belnr
       AND gjahr = gjahr.
  ENDMETHOD.
  METHOD get_bschl_anul.
    SELECT SINGLE stbsl FROM tbsl
      INTO bschl_anul
     WHERE bschl = bschl.
  ENDMETHOD.
  METHOD get_datos_iva.
    DATA taxcom TYPE taxcom.

    CLEAR importe.
    taxcom-bukrs = bukrs.
    taxcom-budat = fecha.
    taxcom-waers = waers.
    taxcom-kposn = '999999'.
    taxcom-mwskz = mwskz.
    taxcom-koart = space.
    taxcom-shkzg = 'H'.
    taxcom-wrbtr = wrbtr.
    taxcom-wmwst = 0.
    taxcom-xmwst = 'X'.
    taxcom-wskto = 0.
    taxcom-skfbt = 0.
    taxcom-zbd1p = 0.

    CALL FUNCTION 'CALCULATE_TAX_ITEM'
      EXPORTING
*      dialog     = 'N'
*      inklusive  = space
        i_taxcom   = taxcom
*      pruefen    = space
*     reset      = space
      IMPORTING
*      e_navfw    = e_navfw
        e_taxcom   = taxcom.
*      e_xstvr    = e_xstvr
*      nav_anteil = nav_anteil.

    importe = taxcom-wmwst.
  ENDMETHOD.
  METHOD get_ebeln_simple.
    DATA l_bkpf TYPE bkpf.

    l_bkpf = get_bkpf( bukrs = bukrs belnr = belnr gjahr = gjahr ).

    IF l_bkpf-awtyp = 'RMRP'.
      l_bkpf-gjahr = l_bkpf-awkey+10(4).
      zcl_ap_factura_mm=>get_ebeln_simple(
        EXPORTING
          belnr = l_bkpf-awkey(10)
          gjahr = l_bkpf-gjahr
        IMPORTING
          ebeln = ebeln
          ebelp = ebelp ).
    ENDIF.
  ENDMETHOD.
  METHOD get_fecha_pago.
    DATA: l_count TYPE i,
          l_t052  TYPE t052.

    CLEAR fecha_pago.
    SELECT COUNT( * ) FROM  t052                           "#EC CI_BYPASS
      INTO l_count
     WHERE zterm = zterm.

    IF l_count = 1.
      SELECT  * FROM  t052
        INTO l_t052
        UP TO 1 ROWS
       WHERE zterm = zterm
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      IF l_t052-zmona IS INITIAL AND l_t052-ztag2 IS INITIAL.
        IF l_t052-ztag1 = '030'.
          fecha_pago = zcl_ap_fechas=>suma_meses( meses = 1 fecha = fecha ).
        ELSE.
          fecha_pago = fecha + l_t052-ztag1.
        ENDIF.
        RETURN.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
      EXPORTING
        i_bldat         = fecha
        i_budat         = fecha
*     I_CPUDT         = SY-DATUM
*     I_ZFBDT         =
        i_zterm         = zterm
*     I_REINDAT       =
*     I_LIFNR         =
*     I_BUKRS         =
      IMPORTING
*     E_ZBD1T         =
*     E_ZBD1P         =
*     E_ZBD2T         =
*     E_ZBD2P         =
*     E_ZBD3T         =
        e_zfbdt         = fecha_pago
*     E_SPLIT         =
*     E_ZSCHF         =
*     E_ZLSCH         =
*     E_T052          =
      EXCEPTIONS
        terms_not_found = 1
        OTHERS          = 2.
  ENDMETHOD.
  METHOD get_imp_iva_desde_bruto.
    DATA: l_wrbtr TYPE bseg-wrbtr,
          i_mwdat TYPE TABLE OF rtax1u15.

    l_wrbtr = wrbtr.

    CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
      EXPORTING
          i_bukrs                 = bukrs
          i_mwskz                 = mwskz
*   I_TXJCD                 = ' '
          i_waers                 = waers
          i_wrbtr                 = l_wrbtr
*   I_ZBD1P                       = 0
*   I_PRSDT                       =
*   I_PROTOKOLL                   =
*   I_TAXPS                       =
*   I_ACCNT_EXT                   =
*   I_ACCDATA                     =
*   IS_ENHANCEMENT                =
      IMPORTING
*   E_FWNAV                       =
*   E_FWNVV                       =
          e_fwste                 = importe
*   E_FWAST                       =
      TABLES
          t_mwdat                 = i_mwdat
     EXCEPTIONS
       bukrs_not_found               = 1
       country_not_found             = 2
       mwskz_not_defined             = 3
       mwskz_not_valid               = 4
       account_not_found             = 5
       different_discount_base       = 6
       different_tax_base            = 7
       txjcd_not_valid               = 8
       not_found                     = 9
       ktosl_not_found               = 10
       kalsm_not_found               = 11
       parameter_error               = 12
       knumh_not_found               = 13
       kschl_not_found               = 14
       unknown_error                 = 15
       OTHERS                        = 16.
  ENDMETHOD.
  METHOD get_importe_iva.
    DATA: l_wrbtr TYPE bseg-wrbtr,
          i_mwdat TYPE TABLE OF rtax1u15.

    l_wrbtr = wrbtr.
    CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
      EXPORTING
        i_bukrs                 = bukrs
        i_mwskz                 = mwskz
*   I_TXJCD                 = ' '
        i_waers                 = waers
        i_wrbtr                 = l_wrbtr
*   I_ZBD1P                 = 0
*   I_PRSDT                 =
*   I_PROTOKOLL             =
*   I_TAXPS                 =
*   I_ACCNT_EXT             =
*   I_ACCDATA               =
      IMPORTING
*   E_FWNAV                 =
*   E_FWNVV                 =
        e_fwste                 = importe
*   E_FWAST                 =
      TABLES
        t_mwdat                 = i_mwdat
     EXCEPTIONS
       bukrs_not_found         = 1
       country_not_found       = 2
       mwskz_not_defined       = 3
       mwskz_not_valid         = 4
       ktosl_not_found         = 5
       kalsm_not_found         = 6
       parameter_error         = 7
       knumh_not_found         = 8
       kschl_not_found         = 9
       unknown_error           = 10
       account_not_found       = 11
       txjcd_not_valid         = 12
       OTHERS                  = 13.
  ENDMETHOD.
  METHOD get_importe_por_cuenta.
    " TODO: parameter EBELN is never used (ABAP cleaner)
    " TODO: parameter EBELP is never used (ABAP cleaner)

    DATA: r_zuonr TYPE RANGE OF bsas-zuonr,
          r_blart TYPE RANGE OF bsas-blart,
          r_xref3 TYPE RANGE OF bsas-xref3,
          r_aufnr TYPE RANGE OF bsas-aufnr,
          l_zuonr LIKE LINE OF r_zuonr,
          l_blart LIKE LINE OF r_blart,
          l_xref3 LIKE LINE OF r_xref3,
          l_aufnr LIKE LINE OF r_aufnr,
          i_bsas  TYPE TABLE OF bsas,
          l_bsas  TYPE bsas.

    CLEAR importe.

    IF NOT zuonr IS INITIAL.
      l_zuonr-option = 'EQ'.
      l_zuonr-sign   = 'I'.
      l_zuonr-low    = zuonr.
      APPEND l_zuonr TO r_zuonr.
    ENDIF.

    IF NOT blart IS INITIAL.
      l_blart-option = 'EQ'.
      l_blart-sign   = 'I'.
      l_blart-low    = blart.
      APPEND l_blart TO r_blart.
    ENDIF.

    IF NOT xref3 IS INITIAL.
      l_xref3-option = 'EQ'.
      l_xref3-sign   = 'I'.
      l_xref3-low    = xref3.
      APPEND l_xref3 TO r_xref3.
    ENDIF.

    IF NOT aufnr IS INITIAL.
      l_aufnr-option = 'EQ'.
      l_aufnr-sign   = 'I'.
      l_aufnr-low    = aufnr.
      APPEND l_aufnr TO r_aufnr.
    ENDIF.

    SELECT dmbtr shkzg FROM bsas
      INTO CORRESPONDING FIELDS OF TABLE i_bsas
     WHERE bukrs  = bukrs
       AND hkont  = hkont
       AND blart IN r_blart
       AND zuonr IN r_zuonr
       AND xref3 IN r_xref3
       AND aufnr IN r_aufnr.

    SELECT dmbtr shkzg FROM bsis
      APPENDING CORRESPONDING FIELDS OF TABLE i_bsas
     WHERE bukrs  = bukrs
       AND hkont  = hkont
       AND blart IN r_blart
       AND zuonr IN r_zuonr
       AND xref3 IN r_xref3
       AND aufnr IN r_aufnr.

    LOOP AT i_bsas INTO l_bsas.
      IF l_bsas-shkzg = 'S'.
        importe = importe + l_bsas-dmbtr.
      ELSE.
        importe = importe - l_bsas-dmbtr.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_importe_por_cuenta_doc.
    DATA: i_bseg TYPE TABLE OF bseg,
          l_bseg TYPE bseg.

    CLEAR importe.
    IF es_anulado( bukrs = bukrs belnr = belnr  gjahr = gjahr )
       IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT dmbtr shkzg FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE i_bseg
     WHERE bukrs = bukrs
       AND belnr = belnr
       AND gjahr = gjahr
       AND koart = koart
       AND hkont = hkont
      ORDER BY PRIMARY KEY.

    LOOP AT i_bseg INTO l_bseg.
      IF l_bseg-shkzg = 'S'.
        importe = importe + l_bseg-dmbtr.
      ELSE.
        importe = importe - l_bseg-dmbtr.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_nombre_cuenta.
    CLEAR txt20.
    IF NOT saknr IS INITIAL.
      SELECT SINGLE txt20 FROM  skat
        INTO txt20
       WHERE spras = spras
         AND ktopl = ktopl
         AND saknr = saknr.
    ENDIF.
  ENDMETHOD.
  METHOD get_saldo_cuenta.
    TYPES: BEGIN OF t_partidas,
             dmbtr TYPE bsas-dmbtr,
             shkzg TYPE bsas-shkzg,
           END OF t_partidas.

    DATA i_partidas TYPE TABLE OF t_partidas.

    FIELD-SYMBOLS <partida> TYPE t_partidas.

    SELECT SUM( dmbtr ) shkzg FROM bsis
      INTO TABLE i_partidas
     WHERE bukrs  = bukrs
       AND hkont  = hkont
       AND budat <= fecha
     GROUP BY shkzg.

    CLEAR saldo.
    LOOP AT i_partidas ASSIGNING <partida>.
      IF <partida>-shkzg = 'H'.
        saldo = saldo + <partida>-dmbtr.
      ELSE.
        saldo = saldo - <partida>-dmbtr.
      ENDIF.
    ENDLOOP.

    IF fecha < sy-datum.
      SELECT SUM( dmbtr ) shkzg FROM bsas
        INTO TABLE i_partidas
       WHERE bukrs = bukrs
         AND hkont = hkont
         AND augdt > fecha
       GROUP BY shkzg.

      LOOP AT i_partidas ASSIGNING <partida>.
        IF <partida>-shkzg = 'H'.
          saldo = saldo + <partida>-dmbtr.
        ELSE.
          saldo = saldo - <partida>-dmbtr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_saldo_cuenta_glt0.
    DATA: l_perio TYPE spmon,
          l_glt0  TYPE glt0,
          l_hsl   TYPE glt0-hsl01,
          l_mes   TYPE n LENGTH 2.

    IF NOT periodo IS INITIAL.
      l_perio = periodo.
    ELSE.
      l_perio = fecha(6).
    ENDIF.

    CLEAR saldo.
    SELECT * FROM glt0                          "#EC CI_ALL_FIELDS_NEEDED
      INTO l_glt0
     WHERE rldnr = '00'
       AND rrcty = '0'
       AND rvers = '001'
       AND bukrs = bukrs
       AND ryear = l_perio(4)
       AND racct = hkont.

      IF solo_valor_mes IS INITIAL.
        saldo = saldo + l_glt0-hslvt.
      ENDIF.

      DO VARYING l_hsl FROM l_glt0-hsl01 NEXT l_glt0-hsl02.
        l_mes = sy-index.

        IF solo_valor_mes IS INITIAL OR l_mes = l_perio+4(2).
          saldo = saldo + l_hsl.
        ENDIF.

        IF l_mes = 12 OR l_mes = l_perio+4(2).
          EXIT.
        ENDIF.
      ENDDO.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_tipo_cambio.
    CALL FUNCTION 'READ_EXCHANGE_RATE'
      EXPORTING
        date             = fecha
        foreign_currency = mon_ext
        local_currency   = mon_local
        type_of_rate     = tipo_cambio
      IMPORTING
        exchange_rate    = tipo
      EXCEPTIONS
        error_message    = 1.
  ENDMETHOD.
  METHOD get_ult_doc_por_cuenta.
    " TODO: parameter EBELN is never used (ABAP cleaner)
    " TODO: parameter EBELP is never used (ABAP cleaner)

    DATA: r_zuonr TYPE RANGE OF bsas-zuonr,
          r_blart TYPE RANGE OF bsas-blart,
          r_xref3 TYPE RANGE OF bsas-xref3,
          r_aufnr TYPE RANGE OF bsas-aufnr,
          l_zuonr LIKE LINE OF r_zuonr,
          l_blart LIKE LINE OF r_blart,
          l_xref3 LIKE LINE OF r_xref3,
          l_aufnr LIKE LINE OF r_aufnr,
          i_bsas  TYPE TABLE OF bsas,
          l_bsas  TYPE bsas.

    IF NOT zuonr IS INITIAL.
      l_zuonr-option = 'EQ'.
      l_zuonr-sign   = 'I'.
      l_zuonr-low    = zuonr.
      APPEND l_zuonr TO r_zuonr.
    ENDIF.

    IF NOT blart IS INITIAL.
      l_blart-option = 'EQ'.
      l_blart-sign   = 'I'.
      l_blart-low    = blart.
      APPEND l_blart TO r_blart.
    ENDIF.

    IF NOT xref3 IS INITIAL.
      l_xref3-option = 'EQ'.
      l_xref3-sign   = 'I'.
      l_xref3-low    = xref3.
      APPEND l_xref3 TO r_xref3.
    ENDIF.

    IF NOT aufnr IS INITIAL.
      l_aufnr-option = 'EQ'.
      l_aufnr-sign   = 'I'.
      l_aufnr-low    = aufnr.
      APPEND l_aufnr TO r_aufnr.
    ENDIF.

    SELECT belnr bukrs gjahr FROM bsas
      INTO CORRESPONDING FIELDS OF TABLE i_bsas
     WHERE bukrs  = bukrs
       AND hkont  = hkont
       AND blart IN r_blart
       AND zuonr IN r_zuonr
       AND xref3 IN r_xref3
       AND aufnr IN r_aufnr.

    SELECT belnr bukrs gjahr FROM bsis
      APPENDING CORRESPONDING FIELDS OF TABLE i_bsas
     WHERE bukrs  = bukrs
       AND hkont  = hkont
       AND blart IN r_blart
       AND zuonr IN r_zuonr
       AND xref3 IN r_xref3
       AND aufnr IN r_aufnr.

    SORT i_bsas BY gjahr DESCENDING belnr DESCENDING.

    CLEAR: belnr, gjahr.
    LOOP AT i_bsas INTO l_bsas.
      IF es_anulado( bukrs = l_bsas-bukrs
                     belnr = l_bsas-belnr
                     gjahr = l_bsas-gjahr ) = ''.
        belnr = l_bsas-belnr.
        gjahr = l_bsas-gjahr.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_url_por_titulo_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    IF NOT belnr IS INITIAL.
      CONCATENATE bukrs belnr gjahr INTO l_clave.
      url = zcl_ap_gos=>get_url_por_titulo_st( tipo   = 'BKPF'
                                            clave  = l_clave
                                            titulo = titulo ).
    ENDIF.
  ENDMETHOD.
  METHOD get_utilizacion_pago.
    FIELD-SYMBOLS: <bsas> TYPE bsas,
                   <bsak> TYPE bsak,
                   <bsad> TYPE bsad.

    DATA: r_blart  TYPE RANGE OF blart,
          lr_blart LIKE LINE OF r_blart,
          i_bsas   TYPE TABLE OF bsas,
          l_bseg   TYPE bseg,
          i_bsak   TYPE TABLE OF bsak,
          i_bsad   TYPE TABLE OF bsad.

    IF NOT blart IS INITIAL.
      CLEAR lr_blart.
      lr_blart-option = 'EQ'.
      lr_blart-sign   = 'I'.
      lr_blart-low    = blart.
      APPEND lr_blart TO r_blart.
    ENDIF.

    CLEAR i_bseg.
    IF cuentas = 'X'.
      SELECT * FROM bsas
        INTO TABLE i_bsas
       WHERE bukrs  = bukrs
         AND augdt  = augdt
         AND augbl  = augbl
         AND blart IN r_blart.
      LOOP AT i_bsas ASSIGNING <bsas>.
        MOVE-CORRESPONDING <bsas> TO l_bseg.
        SELECT SINGLE koart lifnr kunnr FROM bseg
          INTO (l_bseg-koart, l_bseg-lifnr, l_bseg-kunnr)
         WHERE bukrs = l_bseg-bukrs
           AND belnr = l_bseg-belnr
           AND gjahr = l_bseg-gjahr
           AND buzei = l_bseg-buzei.
        APPEND l_bseg TO i_bseg.
      ENDLOOP.
    ENDIF.

    IF proveedor = 'X'.
      SELECT * FROM bsak
        INTO CORRESPONDING FIELDS OF TABLE i_bsak
       WHERE bukrs  = bukrs
         AND augdt  = augdt
         AND augbl  = augbl
         AND blart IN r_blart.
      LOOP AT i_bsak ASSIGNING <bsak>.
        MOVE-CORRESPONDING <bsak> TO l_bseg.
        l_bseg-koart = 'K'.
        APPEND l_bseg TO i_bseg.
      ENDLOOP.
    ENDIF.

    IF cliente = 'X'.
      SELECT * FROM bsad
        INTO CORRESPONDING FIELDS OF TABLE i_bseg
       WHERE bukrs  = bukrs
         AND augdt  = augdt
         AND augbl  = augbl
         AND blart IN r_blart.

      LOOP AT i_bsad ASSIGNING <bsad>.
        MOVE-CORRESPONDING <bsad> TO l_bseg.
        l_bseg-koart = 'D'.
        APPEND l_bseg TO i_bseg.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD info_partida.
    IF t001-bukrs <> rfposxext-bukrs.
      SELECT SINGLE * FROM t001 INTO t001 WHERE bukrs = rfposxext-bukrs.
    ENDIF.
    CALL FUNCTION 'ITEM_DERIVE_FIELDS'
      EXPORTING
        s_t001    = t001
*       S_BSEGP   =
        key_date  = fecha
*       XOPVW     = 'X'
*       X_ICONS_ONLY       = ' '
*       I_KALSM   =
      CHANGING
        s_item    = rfposxext
      EXCEPTIONS
        bad_input = 1
        OTHERS    = 2.
  ENDMETHOD.
  METHOD insertar_url_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.
    DATA: i_bseg TYPE TABLE OF bseg,
          l_bseg TYPE bseg.

    CONCATENATE bukrs belnr gjahr INTO l_clave.
    error = zcl_ap_gos=>insertar_url_gos_st( tipo   = 'BKPF'
                                          clave  = l_clave
                                          titulo = titulo
                                          url    = url ).

    IF posiciones <> 'X'.
      RETURN.
    ENDIF.

    SELECT buzei FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE i_bseg
     WHERE bukrs = bukrs
       AND belnr = belnr
       AND gjahr = gjahr
      ORDER BY PRIMARY KEY.
    LOOP AT i_bseg INTO l_bseg.
      CONCATENATE bukrs belnr gjahr l_bseg-buzei INTO l_clave.
      error = zcl_ap_gos=>insertar_url_gos_st( tipo   = 'BSEG'
                                            clave  = l_clave
                                            titulo = titulo
                                            url    = url ).
    ENDLOOP.
  ENDMETHOD.
  METHOD modificacion_campo_pos.
    DATA: l_bseg    TYPE bseg,
          l_campo   TYPE c LENGTH 40,
          it_buztab TYPE tpit_t_buztab,
          l_buztab  TYPE tpit_buztab,
          l_fname   TYPE tpit_fname,
          it_fldtab TYPE tpit_t_fname,
          errtab    TYPE tpit_t_errdoc.

    FIELD-SYMBOLS: <bseg_old> TYPE any,
                   <bseg_new> TYPE any.

    TYPE-POOLS tpit.

    IF bseg IS INITIAL.
      message = 'Informe datos de posicion'(idp).
      RETURN.
    ENDIF.

    IF campo IS INITIAL.
      message = 'Informe campo'(ica).
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM  bseg                  "#EC CI_ALL_FIELDS_NEEDED
      INTO l_bseg
     WHERE bukrs = bseg-bukrs
       AND belnr = bseg-belnr
       AND gjahr = bseg-gjahr
       AND buzei = bseg-buzei.
    IF sy-subrc <> 0.
      message = 'Documento/posicion no existe'(dne).
      RETURN.
    ENDIF.

    CONCATENATE 'L_BSEG-' campo INTO l_campo.
    ASSIGN (l_campo) TO <bseg_old>.
    IF sy-subrc <> 0.
      CONCATENATE 'Campo'(cam) campo 'no existe en BSEG'(neb) INTO message SEPARATED BY space.
      RETURN.
    ELSE.
      CONCATENATE 'BSEG-' campo INTO l_campo.
      ASSIGN (l_campo) TO <bseg_new>.
      IF <bseg_new> = <bseg_old>.
        " No hace falta modificar nada, salidmos
        RETURN.
      ENDIF.
    ENDIF.

    CLEAR it_buztab.
    MOVE-CORRESPONDING bseg TO l_buztab.
    APPEND l_buztab TO it_buztab.

    l_fname-fname = campo.
    APPEND l_fname TO it_fldtab.

    CALL FUNCTION 'FI_ITEMS_MASS_CHANGE'
      EXPORTING
        s_bseg     = bseg
      IMPORTING
        errtab     = errtab
      TABLES
        it_buztab  = it_buztab
        it_fldtab  = it_fldtab
      EXCEPTIONS
        bdc_errors = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ELSE.
      IF commit = 'X'.
        COMMIT WORK AND WAIT.
      ENDIF.
      IF segundos_espera > 0.
        WAIT UP TO segundos_espera SECONDS.
      ENDIF.

      DO 2 TIMES.
        SELECT SINGLE * FROM  bseg              "#EC CI_ALL_FIELDS_NEEDED
          INTO l_bseg
         WHERE bukrs = bseg-bukrs
           AND belnr = bseg-belnr
           AND gjahr = bseg-gjahr
           AND buzei = bseg-buzei.
        CONCATENATE 'L_BSEG-' campo INTO l_campo.
        ASSIGN (l_campo) TO <bseg_old>.
        IF <bseg_new> = <bseg_old> OR segundos_espera > 0.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS. " La actualización parece que se hacen en fondo, a veces hay que esperar
        ENDIF.
      ENDDO.
      IF <bseg_new> <> <bseg_old>.
        CONCATENATE 'No se ha modificado el valor del campo'(nmv) campo INTO message SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar.
    DATA: l_bkpf   TYPE bkpf,
          l_dialog TYPE boole VALUE 'X'.

    SELECT SINGLE awkey awsys awtyp FROM bkpf
      INTO CORRESPONDING FIELDS OF l_bkpf
     WHERE bukrs = bukrs
       AND belnr = belnr
       AND gjahr = gjahr.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'AC_DOCUMENT_RECORD'
      EXPORTING
        i_awtyp      = l_bkpf-awtyp
        i_awref      = l_bkpf-awkey+00(10)
        i_aworg      = l_bkpf-awkey+10(10)
        i_awsys      = l_bkpf-awsys
*       i_awtyp_incl = 'BKPF'
        i_awtyp_excl = ' '
        i_bukrs      = bukrs
        i_valutyp    = '0'
        x_dialog     = l_dialog.
  ENDMETHOD.
