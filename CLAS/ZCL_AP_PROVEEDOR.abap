CLASS zcl_ap_proveedor DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS urls_gos_st
      IMPORTING lifnr        TYPE lfa1-lifnr
      RETURNING VALUE(tabla) TYPE ztab_url_gos.

    CLASS-METHODS insertar_url_gos_st
      IMPORTING lifnr  TYPE lfa1-lifnr
                url    TYPE string
                titulo TYPE string.

    CLASS-METHODS visualizar_proveedor_st
      IMPORTING lifnr TYPE lfa1-lifnr.

    CLASS-METHODS get_url_por_titulo_st
      IMPORTING lifnr      TYPE lfa1-lifnr
                titulo     TYPE string
      RETURNING VALUE(url) TYPE string.

    CLASS-METHODS get_emails
      IMPORTING lifnr         TYPE lifnr OPTIONAL
                parnr         TYPE parnr OPTIONAL
                remark        TYPE any   DEFAULT ''
      PREFERRED PARAMETER lifnr
      RETURNING VALUE(emails) TYPE isu_adsmtp_tab.

    CLASS-METHODS get_email
      IMPORTING lifnr        TYPE lifnr     OPTIONAL
                parnr        TYPE parnr     OPTIONAL
                remark       TYPE any       DEFAULT ''
                lista        TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER lifnr
      RETURNING VALUE(email) TYPE ad_smtpadr.

    CLASS-METHODS get_nombre
      IMPORTING lifnr        TYPE lifnr
      RETURNING VALUE(name1) TYPE name1.

    CLASS-METHODS get_volumen_negocio
      IMPORTING lifnr        TYPE lifnr
                bukrs        TYPE bukrs       OPTIONAL
                fechas       TYPE tpmy_r_date OPTIONAL
      RETURNING VALUE(valor) TYPE cf005-umsaz.

    CLASS-METHODS get_direccion
      IMPORTING lifnr            TYPE lifnr OPTIONAL
                adrnr            TYPE adrnr OPTIONAL
      RETURNING VALUE(direccion) TYPE addr1_data.

    CLASS-METHODS get_direccion_string
      IMPORTING adrnr            TYPE adrc-addrnumber OPTIONAL
                lifnr            TYPE lifnr           OPTIONAL
      RETURNING VALUE(direccion) TYPE szad_field-addr_short.

    CLASS-METHODS get_datos_banco
      IMPORTING lifnr   TYPE any
                banks   TYPE any OPTIONAL
                bankl   TYPE any OPTIONAL
                bankn   TYPE any OPTIONAL
      EXPORTING lfbk    TYPE lfbk
                tiban   TYPE tiban
                bnka    TYPE bnka
                mensaje TYPE bapi_msg
                iban    TYPE tiban-iban
                swift   TYPE bnka-swift.

    CLASS-METHODS popup_emails
      IMPORTING lifnr     TYPE lifnr
                operacion TYPE any DEFAULT 'MAINTAIN'.

  PROTECTED SECTION.
  PRIVATE SECTION.
endclass. "ZCL_AP_PROVEEDOR definition
class ZCL_AP_PROVEEDOR implementation.
  METHOD get_datos_banco.
    DATA: or_banks TYPE REF TO zcl_ap_rango,
          or_bankn TYPE REF TO zcl_ap_rango,
          or_bankl TYPE REF TO zcl_ap_rango.

    or_banks = NEW #( valor_inicial = banks ).
    or_bankn = NEW #( valor_inicial = bankn ).
    or_bankl = NEW #( valor_inicial = bankl ).

    SELECT COUNT( * ) FROM lfbk
      INTO sy-tfill
     WHERE lifnr  = lifnr
       AND banks IN or_banks->rango
       AND bankl IN or_bankl->rango
       AND bankn IN or_bankn->rango.
    IF sy-tfill = 0.
      mensaje = 'No existe cuenta bancaria'(ncb).
    ELSEIF sy-tfill > 1.
      mensaje = 'Existe más de una cuenta. Especifique'(muc).
    ELSE.
      SELECT * FROM lfbk
        INTO lfbk
        UP TO 1 ROWS
       WHERE lifnr  = lifnr
         AND banks IN or_banks->rango
         AND bankl IN or_bankl->rango
         AND bankn IN or_bankn->rango
       ORDER BY PRIMARY KEY.
      ENDSELECT.

      SELECT SINGLE * FROM  tiban
        INTO tiban
       WHERE banks = lfbk-banks
         AND bankl = lfbk-bankl
         AND bankn = lfbk-bankn
         AND bkont = lfbk-bkont.
      iban = tiban-iban.

      SELECT SINGLE * FROM  bnka
        INTO bnka
       WHERE banks = lfbk-banks
         AND bankl = lfbk-bankl.
      swift = bnka-swift.
    ENDIF.
  ENDMETHOD.
  METHOD get_direccion.
    DATA l_adrnr TYPE adrnr.

    IF adrnr IS INITIAL.
      SELECT SINGLE adrnr FROM lfa1
        INTO l_adrnr
       WHERE lifnr = lifnr.
    ELSE.
      l_adrnr = adrnr.
    ENDIF.

    direccion = zcl_ap_direcciones=>get_direccion2( l_adrnr ).
  ENDMETHOD.
  METHOD get_direccion_string.
    DATA l_adrnr TYPE adrnr.

    IF adrnr IS INITIAL.
      SELECT SINGLE adrnr FROM lfa1
        INTO l_adrnr
       WHERE lifnr = lifnr.
    ELSE.
      l_adrnr = adrnr.
    ENDIF.

    direccion = zcl_ap_direcciones=>get_direccion_string( l_adrnr ).
  ENDMETHOD.
  METHOD get_email.
    DATA: i_emails TYPE isu_adsmtp_tab,
          l_email  TYPE adsmtp.

    CLEAR email.
    i_emails = get_emails( lifnr = lifnr parnr = parnr remark = remark ).

    IF lista IS INITIAL.
      READ TABLE i_emails INTO l_email WITH KEY flgdefault = 'X'.
      IF sy-subrc = 0.
        email = l_email-smtp_addr.
      ELSE.
        READ TABLE i_emails INTO l_email INDEX 1. "#EC CI_NOORDER
        IF sy-subrc = 0.
          email = l_email-smtp_addr.
        ENDIF.
      ENDIF.
    ELSE.
      LOOP AT i_emails INTO l_email.
        __add_lista email l_email-smtp_addr.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_emails.
    DATA l_adrnr TYPE lfa1-adrnr.

    SELECT SINGLE adrnr FROM lfa1
      INTO l_adrnr
     WHERE lifnr = lifnr.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF parnr IS INITIAL.
      emails = zcl_ap_direcciones=>get_emails( adrnr = l_adrnr remark = remark ).
    ELSE.
      SELECT SINGLE prsnr FROM knvk
        INTO @DATA(l_prsnr)
       WHERE parnr = @parnr.
      IF sy-subrc = 0.
        SELECT * FROM adr6
          APPENDING CORRESPONDING FIELDS OF TABLE emails  ##TOO_MANY_ITAB_FIELDS
         WHERE addrnumber = l_adrnr
           AND persnumber = l_prsnr.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_nombre.
    CLEAR name1.
    SELECT SINGLE name1 FROM lfa1
      INTO name1
     WHERE lifnr = lifnr.
  ENDMETHOD.
  METHOD get_url_por_titulo_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = lifnr.
    url = zcl_ap_gos=>get_url_por_titulo_st( tipo   = 'LFA1'
                                          clave  = l_clave
                                          titulo = titulo ).
  ENDMETHOD.
  METHOD get_volumen_negocio.
    TYPES: BEGIN OF t_partidas,
             shkzg TYPE shkzg,
             dmbtr TYPE dmbtr,
           END OF t_partidas.

    FIELD-SYMBOLS <partida> TYPE t_partidas.

    DATA: r_bukrs    TYPE RANGE OF bukrs,
          lr_bukrs   LIKE LINE OF r_bukrs,
          i_partidas TYPE TABLE OF t_partidas.

    IF NOT bukrs IS INITIAL.
      lr_bukrs-option = 'EQ'.
      lr_bukrs-sign   = 'I'.
      lr_bukrs-low    = bukrs.
      APPEND lr_bukrs TO r_bukrs.
    ENDIF.

    SELECT shkzg SUM( dmbtr ) FROM bsik
      INTO TABLE i_partidas
     WHERE lifnr  = lifnr
       AND bukrs IN r_bukrs
       AND budat IN fechas
       AND bschl IN ( '21', '22', '31', '32' )
     GROUP BY shkzg.

    SELECT shkzg SUM( dmbtr ) FROM bsak
      APPENDING TABLE i_partidas
     WHERE lifnr  = lifnr
       AND budat IN fechas
       AND bukrs IN r_bukrs
       AND bschl IN ( '21', '22', '31', '32' )
     GROUP BY shkzg.

    CLEAR valor.
    LOOP AT i_partidas ASSIGNING <partida>.
      IF <partida>-shkzg = 'H'.
        valor = valor + <partida>-dmbtr.
      ELSE.
        valor = valor - <partida>-dmbtr.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD insertar_url_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = lifnr.
    zcl_ap_gos=>insertar_url_gos_st( tipo   = 'LFA1'
                                  clave  = l_clave
                                  titulo = titulo
                                  url    = url ).
  ENDMETHOD.
  METHOD popup_emails.
    DATA l_adrnr TYPE adrnr.

    SELECT SINGLE adrnr FROM lfa1
      INTO l_adrnr
     WHERE lifnr = lifnr.

    IF NOT l_adrnr IS INITIAL.
      zcl_ap_direcciones=>popup_emails( adrnr = l_adrnr operacion = operacion ).
    ENDIF.
  ENDMETHOD.
  METHOD urls_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = lifnr.
    tabla = zcl_ap_gos=>urls_gos_st( tipo = 'LFA1' clave = l_clave ).
  ENDMETHOD.
  METHOD visualizar_proveedor_st.
* Para los parámetros se puede usar (include MF02KO00 linea
*  IF l_rc = 4.
**------- SET/GET-Parameter KDY einlesen -------------------------------
*    GET PARAMETER ID 'KDY' FIELD kred-auswl.
*  ENDIF.
**\AT Ende
* SET PARAMETER ID 'KDY' FIELD '/111'.  "Dirección
* SET PARAMETER ID 'KDY' FIELD '/380'.  "Persona contacto
*
    SET PARAMETER ID 'LIF' FIELD lifnr.
    CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN.       "#EC CI_CALLTA
  ENDMETHOD.
