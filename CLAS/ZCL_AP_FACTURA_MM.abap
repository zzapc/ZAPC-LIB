CLASS zcl_ap_factura_mm DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA rbkp TYPE rbkp .

    CLASS-METHODS get_rbkp
      IMPORTING
        !belnr      TYPE bkpf-belnr
        !gjahr      TYPE bkpf-gjahr
      RETURNING
        VALUE(rbkp) TYPE rbkp .
    CLASS-METHODS get_ebeln_simple
      IMPORTING
        !belnr TYPE bkpf-belnr
        !gjahr TYPE bkpf-gjahr
      EXPORTING
        !ebeln TYPE bseg-ebeln
        !ebelp TYPE bseg-ebelp .
    CLASS-METHODS insertar_url_gos_st
      IMPORTING
        !belnr       TYPE bkpf-belnr
        !gjahr       TYPE bkpf-gjahr
        !url         TYPE string
        !titulo      TYPE string
      RETURNING
        VALUE(error) TYPE ekko-loekz .
    CLASS-METHODS get_url_por_titulo_st
      IMPORTING
        !belnr     TYPE bkpf-belnr
        !gjahr     TYPE bkpf-gjahr
        !titulo    TYPE string
      RETURNING
        VALUE(url) TYPE string .
    CLASS-METHODS borrar_url_gos_st
      IMPORTING
        !belnr       TYPE bkpf-belnr
        !gjahr       TYPE bkpf-gjahr
        !url         TYPE string
        !titulo      TYPE string
      RETURNING
        VALUE(error) TYPE ekko-loekz .
    CLASS-METHODS visualizar
      IMPORTING
        !belnr TYPE bkpf-belnr
        !gjahr TYPE bkpf-gjahr .
    CLASS-METHODS get_doc_contable
      IMPORTING
        !belnr    TYPE rbkp-belnr
        !gjahr    TYPE rbkp-gjahr
      EXPORTING
        !belnr_fi TYPE bkpf-belnr
        !gjahr_fi TYPE bkpf-gjahr
        !bukrs_fi TYPE bkpf-bukrs .
    CLASS-METHODS modificar
      IMPORTING
        !belnr TYPE bkpf-belnr
        !gjahr TYPE bkpf-gjahr .
    CLASS-METHODS es_periodo_abierto
      IMPORTING
        !bukrs    TYPE bukrs
        !budat    TYPE budat
      RETURNING
        VALUE(si) TYPE abap_bool .
    CLASS-METHODS anular_factura
      IMPORTING
        !belnr      TYPE rbkp-belnr
        !gjahr      TYPE rbkp-gjahr
        !budat      TYPE budat OPTIONAL
        !stgrd      TYPE rf05r-stgrd DEFAULT '01'
        !commit     TYPE abap_bool DEFAULT 'X'
      EXPORTING
        !belnr_anul TYPE bkpf-belnr
        !gjahr_anul TYPE bkpf-gjahr
        !message    TYPE bapi_msg
        !i_return   TYPE bapirettab .
  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_FACTURA_MM definition
class ZCL_AP_FACTURA_MM implementation.
  METHOD anular_factura.
    DATA: l_rbkp  LIKE rbkp,
          l_stgrd TYPE uf05a-stgrd.

    CLEAR: message, belnr_anul, gjahr_anul.
    SELECT SINGLE budat gjahr stblg FROM rbkp
      INTO CORRESPONDING FIELDS OF l_rbkp
     WHERE belnr = belnr
       AND gjahr = gjahr.
    IF sy-subrc = 0 AND NOT l_rbkp-stblg IS INITIAL.
      message = 'Documento ya anulado'(dya).
      belnr_anul = l_rbkp-stblg.
      gjahr_anul = l_rbkp-gjahr.
    ELSE.
      IF NOT stgrd IS INITIAL.
        l_stgrd = stgrd.
      ELSE.
        IF     NOT budat        IS INITIAL
           AND     l_rbkp-budat <> budat.
          l_stgrd = '02'.
        ELSE.
          l_stgrd = '01'.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
        EXPORTING
          invoicedocnumber          = belnr
          fiscalyear                = gjahr
          reasonreversal            = l_stgrd
          postingdate               = budat
        IMPORTING
          invoicedocnumber_reversal = belnr_anul
          fiscalyear_reversal       = gjahr_anul
        TABLES
          return                    = i_return.

      IF belnr_anul IS INITIAL.
        LOOP AT i_return ASSIGNING FIELD-SYMBOL(<return>) WHERE type = 'E'.
          IF message IS INITIAL.
            message = <return>-message.
          ELSE.
            message = message && ` ` && <return>-message.
          ENDIF.
        ENDLOOP.
      ELSEIF commit = 'X'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD borrar_url_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    CONCATENATE belnr gjahr INTO l_clave.
    error = zcl_ap_gos=>borrar_url_gos_st( tipo   = 'BUS2081'
                                          clave  = l_clave
                                          titulo = titulo
                                          url    = url ).
  ENDMETHOD.
  METHOD es_periodo_abierto.
    CLEAR si.
    CALL FUNCTION 'MR_PERIOD_DETERMINE'
      EXPORTING
        i_bukrs                = bukrs
        i_budat                = budat
*       I_KZRFB                =
*     IMPORTING
*       E_MONAT                =
*       E_GJAHR                =
*       E_XRUEJ                =
*       E_XRUEM                =
*       E_LFGJA                =
*       E_LFMON                =
      EXCEPTIONS
        invalid_posting_period = 1
        marv_no_entry          = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD get_doc_contable.
    DATA l_key TYPE bkpf-awkey.

    CLEAR: bukrs_fi, belnr_fi, gjahr_fi.
    CONCATENATE belnr gjahr INTO l_key.
    SELECT bukrs, belnr, gjahr
      FROM bkpf
      INTO (@bukrs_fi, @belnr_fi, @gjahr_fi)
      UP TO 1 ROWS
      WHERE awtyp = 'RMRP'
        AND awkey = @l_key
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_ebeln_simple.
    SELECT ebeln, ebelp
      FROM ekbe
      INTO (@ebeln, @ebelp)
      UP TO 1 ROWS
      WHERE belnr = @belnr
        AND gjahr = @gjahr
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_rbkp.
    CLEAR rbkp.
    SELECT SINGLE * FROM rbkp
      INTO rbkp
     WHERE belnr = belnr
       AND gjahr = gjahr.
  ENDMETHOD.
  METHOD get_url_por_titulo_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    IF NOT belnr IS INITIAL.
      CONCATENATE belnr gjahr INTO l_clave.
      url = zcl_ap_gos=>get_url_por_titulo_st( tipo   = 'BUS2081'
                                            clave  = l_clave
                                            titulo = titulo ).
    ENDIF.
  ENDMETHOD.
  METHOD insertar_url_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    CONCATENATE belnr gjahr INTO l_clave.
    error = zcl_ap_gos=>insertar_url_gos_st( tipo   = 'BUS2081'
                                          clave  = l_clave
                                          titulo = titulo
                                          url    = url ).
  ENDMETHOD.
  METHOD modificar.
    SELECT SINGLE bukrs, rbstat FROM rbkp
      INTO (@DATA(l_bukrs), @DATA(l_rbstat))
     WHERE belnr = @belnr
       AND gjahr = @gjahr.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SET PARAMETER ID: 'RBN' FIELD belnr,
                      'GJR' FIELD gjahr,
                      'BUK' FIELD l_bukrs,
                      'RBS' FIELD l_rbstat,
                      'CHG' FIELD 'X'.

    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

    SET PARAMETER ID: 'RBS' FIELD '',
                      'CHG' FIELD ''.
  ENDMETHOD.
  METHOD visualizar.
    SET PARAMETER ID: 'RBN' FIELD belnr,
                      'GJR' FIELD gjahr.
    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
  ENDMETHOD.
