CLASS zcl_ap_factura_sd DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_objeto TYPE srgbtbrel-typeid_a VALUE 'BUS2037' ##NO_TEXT.

    CLASS-METHODS es_anulada
      IMPORTING vbeln          TYPE vbrk-vbeln
      RETURNING VALUE(anulada) TYPE ekko-loekz.

    CLASS-METHODS visualizar
      IMPORTING vbeln TYPE any.

    CLASS-METHODS get_valor_condicion
      IMPORTING vbeln        TYPE vbrp-vbeln
                posnr        TYPE vbrp-posnr OPTIONAL
                kschl        TYPE kschl
                campo        TYPE string     DEFAULT 'KWERT'
      RETURNING VALUE(valor) TYPE komv-kwert.

    CLASS-METHODS get_valor_condicion_char
      IMPORTING vbeln        TYPE vbrp-vbeln
                posnr        TYPE vbrp-posnr OPTIONAL
                kschl        TYPE kschl
                campo        TYPE string     OPTIONAL
      RETURNING VALUE(valor) TYPE string.

    CLASS-METHODS get_texto_string
      IMPORTING vbeln         TYPE vbeln_vf
                posnr         TYPE posnr_vf OPTIONAL
                !id           TYPE stxh-tdid
                spras         TYPE spras    DEFAULT ''
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS get_adrc_entrega
      IMPORTING vbeln       TYPE vbeln_vf
      RETURNING VALUE(adrc) TYPE adrc.

    CLASS-METHODS get_doc_fi
      IMPORTING vbeln       TYPE vbeln_vf
      RETURNING VALUE(bkpf) TYPE bkpf.

    CLASS-METHODS get_fecha_vto
      IMPORTING vbeln            TYPE vbeln_vf
                zterm            TYPE vbrk-zterm OPTIONAL
      RETURNING VALUE(fecha_vto) TYPE datum.

    CLASS-METHODS crea_factura
      IMPORTING entrega                     TYPE vbeln_vl
                o_log                       TYPE REF TO zcl_ap_log OPTIONAL
                modoct                      TYPE char1             DEFAULT 'N'
                segundos_espera_si_bloqueos TYPE int2              DEFAULT 10
                fkdat                       TYPE fkdat             DEFAULT sy-datum
                fkart                       TYPE fkart             DEFAULT ''
      EXPORTING !message                    TYPE bapi_msg
                factura                     TYPE vbeln_vf.

    CLASS-METHODS get_nped_cliente
      IMPORTING vbeln        TYPE vbeln_vf
      RETURNING VALUE(bstkd) TYPE bstkd.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_FACTURA_SD definition
class ZCL_AP_FACTURA_SD implementation.
  METHOD crea_factura.
    DATA: l_pedido      TYPE vbeln_va,
          l_auart       TYPE vbak-auart,
          l_fkart       TYPE fkart,
          o_bi          TYPE REF TO zcl_ap_batch_input,
          xvbfs_factura TYPE TABLE OF vbfs,
          string        TYPE string.

    IF fkart IS INITIAL.
      l_pedido = zcl_ap_entregas=>get_pedido( entrega ).
      IF NOT l_pedido IS INITIAL.
        SELECT SINGLE auart FROM vbak
          INTO l_auart
         WHERE vbeln = l_pedido.
        IF sy-subrc = 0.
          SELECT SINGLE fkarv FROM tvak
            INTO l_fkart
           WHERE auart = l_auart.
        ENDIF.
      ENDIF.
    ELSE.
      l_fkart = fkart.
    ENDIF.

    CLEAR message.

    message = zcl_ap_entregas=>espera_si_bloqueada( vbeln = entrega segundos_espera = segundos_espera_si_bloqueos ).

    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMV60A' dynpro = '0102' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).
    IF NOT l_fkart IS INITIAL.
      o_bi->campos( campo = 'RV60A-FKART' valor = l_fkart ).
    ENDIF.
    IF NOT fkdat IS INITIAL.
      o_bi->campos( campo = 'RV60A-FKDAT' valor = fkdat ).
    ENDIF.
    o_bi->campos( campo = 'KOMFK-VBELN(01)' valor = entrega ). " Número de documento comercial

    FREE MEMORY ID 'ZLOG_FACTURA'.
    message = o_bi->llamar_transaccion( tcode = 'VF01' modo = modoct ).

    IF o_bi->msgno = '311' AND o_bi->msgid = 'VF'.
      factura = o_bi->msgv1.
      __poner_ceros factura.

      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'Se ha creado factura' p2 = factura msgty = 'S' ).
      ENDIF.
    ELSE.
* Para poder recuperar el log, tenemos que poner enhancement en RV_INVOICE_CREATE
**---------------------------------------------------------------------*
**       E N D E                                                       *
**---------------------------------------------------------------------*
*"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(1) Módulo funciones RV_INVOICE_CREATE, Final                                                                                                         A
**$*$-Start: (1)---------------------------------------------------------------------------------$*$*
*ENHANCEMENT 1  ZGSD_RECUPERAR_LOG_FACTURA.    "active version
*  IF zcl_exits=>exit_activa( 'FACT_RECUPERAR_LOG' ) = 'X'.
*    export XVBFS_factura from XVBFS[] to MEMORY id 'ZLOG_FACTURA'.
*  endif.
*ENDENHANCEMENT.
**$*$-End:   (1)---------------------------------------------------------------------------------$*$*
*ENDFUNCTION.

      IMPORT xvbfs_factura TO xvbfs_factura FROM MEMORY ID 'ZLOG_FACTURA'.
      FREE MEMORY ID 'ZLOG_FACTURA'.

      LOOP AT xvbfs_factura ASSIGNING FIELD-SYMBOL(<vbfs>) WHERE msgty = 'E'.
        MESSAGE ID <vbfs>-msgid TYPE 'S' NUMBER <vbfs>-msgno
                WITH <vbfs>-msgv1 <vbfs>-msgv2 <vbfs>-msgv3 <vbfs>-msgv4
                INTO string.
        IF NOT string IS INITIAL AND message CS 'dynpro'.
          CLEAR message.
        ENDIF.
        __concat_a message string.
      ENDLOOP.

      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'Error creando factura desde entrega' p2 = entrega p3 = message msgty = 'E' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD es_anulada.
    DATA: t_vbfa TYPE TABLE OF vbfa,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_vbfa TYPE vbfa.

    CLEAR anulada.

    SELECT vbeln posnn FROM vbfa
      INTO CORRESPONDING FIELDS OF TABLE t_vbfa
     WHERE vbelv    = vbeln
       AND vbtyp_n IN ( 'N', 'O' )
     ORDER BY PRIMARY KEY.

    LOOP AT t_vbfa INTO l_vbfa.
      anulada = 'X'.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_adrc_entrega.
    DATA l_adrnr TYPE vbpa-adrnr.

    CLEAR adrc.
    SELECT adrnr FROM vbpa
      INTO l_Adrnr
      UP TO 1 ROWS
     WHERE vbeln = vbeln
       AND parvw = 'WE'
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      SELECT * FROM adrc
        INTO adrc
      UP TO 1 ROWS
       WHERE addrnumber  = l_adrnr
         AND date_from  <= sy-datum
         AND date_to    >= sy-datum
      ORDER BY PRIMARY KEY.
      ENDSELECT.

    ENDIF.
  ENDMETHOD.
  METHOD get_doc_fi.
    CLEAR bkpf.
    SELECT * FROM bkpf
      INTO bkpf
      UP TO 1 ROWS
     WHERE awtyp = 'VBRK'
       AND awkey = vbeln
     ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_fecha_vto.
    DATA l_zterm TYPE bseg-zterm.

    IF zterm IS INITIAL.
      SELECT SINGLE zterm FROM vbrk
        INTO l_zterm
       WHERE vbeln = vbeln.
    ELSE.
      l_zterm = zterm.
    ENDIF.

    CLEAR fecha_vto.
    CALL FUNCTION 'J_1A_SD_CI_DUEDATE_GET'
      EXPORTING
        iv_vbeln                 = vbeln
        iv_zterm                 = l_zterm
      IMPORTING
        ev_netdate               = fecha_vto
      EXCEPTIONS
        fi_document_not_found    = 1
        payment_terms_incomplete = 2
        invoice_not_found        = 3
        OTHERS                   = 4.
    IF sy-subrc = 0.
      IF fecha_vto IS INITIAL.
        SELECT SINGLE fkdat FROM vbrk
          INTO fecha_vto
         WHERE vbeln = vbeln.

        fecha_vto = zcl_ap_doc_fi=>get_fecha_pago( fecha = fecha_vto
                                                 zterm = l_zterm ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_nped_cliente.
    CLEAR bstkd.
    SELECT SINGLE bstnk_vf FROM vbrk
      INTO bstkd
     WHERE vbeln = vbeln.

    IF NOT bstkd IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT aubel FROM vbrp
      INTO TABLE @DATA(i_pedidos)
     WHERE vbeln  = @vbeln
       AND aubel <> ''
      order by aubel.

    LOOP AT i_pedidos ASSIGNING FIELD-SYMBOL(<ped>). "#EC CI_NOORDER
      SELECT SINGLE bstkd FROM vbkd
        INTO bstkd
       WHERE vbeln  = <ped>-aubel
         AND posnr  = '000000'
         AND bstkd <> ''.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_texto_string.
    IF posnr IS INITIAL.
      string = zcl_ap_textos=>get_texto_string( id = id object = 'VBBK' name = vbeln spras = spras ).
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_condicion.
    FIELD-SYMBOLS <valor> TYPE any.

    DATA: r_kposn TYPE RANGE OF konv-kposn,
          l_knumv TYPE vbrk-knumv,
          l_kposn LIKE LINE OF r_kposn,
          l_tabla TYPE tabname VALUE 'KONV',
          i_konv  TYPE TABLE OF konv,
          l_campo TYPE string,
          l_konv  TYPE konv.

    SELECT SINGLE knumv FROM vbrk
      INTO l_knumv
     WHERE vbeln = vbeln.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF NOT posnr IS INITIAL.
      l_kposn-option = 'EQ'.
      l_kposn-sign   = 'I'.
      l_kposn-low    = posnr.
      APPEND l_kposn TO r_kposn.
    ENDIF.

    IF zcl_c=>hana = 'X'.
      l_tabla = 'V_KONV_CDS'.
    ENDIF.

    SELECT * FROM (l_tabla)
      INTO CORRESPONDING FIELDS OF TABLE i_konv
     WHERE knumv  = l_knumv
       AND kposn IN r_kposn
       AND kschl  = kschl
     ORDER BY PRIMARY KEY.

    CONCATENATE 'L_KONV-' campo INTO l_campo.
    ASSIGN (l_campo) TO <valor>.
    IF sy-subrc = 0.
      LOOP AT i_konv INTO l_konv.
        valor = valor + <valor>.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_condicion_char.
    FIELD-SYMBOLS <valor> TYPE any.

    DATA: r_kposn TYPE RANGE OF konv-kposn,
          l_knumv TYPE vbrk-knumv,
          l_kposn LIKE LINE OF r_kposn,
          i_konv  TYPE TABLE OF konv,
          l_campo TYPE string,
          l_konv  TYPE konv.

    SELECT SINGLE knumv FROM vbrk
      INTO l_knumv
     WHERE vbeln = vbeln.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF NOT posnr IS INITIAL.
      l_kposn-option = 'EQ'.
      l_kposn-sign   = 'I'.
      l_kposn-low    = posnr.
      APPEND l_kposn TO r_kposn.
    ENDIF.

    SELECT * FROM konv                        "#EC CI_ALL_FIELDS_NEEDED
      INTO TABLE i_konv
     WHERE knumv  = l_knumv
       AND kposn IN r_kposn
       AND kschl  = kschl
     ORDER BY PRIMARY KEY.

    CONCATENATE 'L_KONV-' campo INTO l_campo.
    ASSIGN (l_campo) TO <valor>.
    IF sy-subrc = 0.
      LOOP AT i_konv INTO l_konv.
        valor = <valor>.
        EXIT.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar.
    TYPES: BEGIN OF t_ent,
             vbeln TYPE vbrk-vbeln,
             fkdat TYPE vbrk-fkdat,
           END OF t_ent,
           BEGIN OF t_vbeln,
             vbeln TYPE vbak-vbeln,
           END OF t_vbeln.

    DATA: l_entrada  TYPE string,
          l_lista    TYPE string,
          l_vbeln    TYPE vbeln_va,
          i_facturas TYPE TABLE OF t_vbeln,
          i_ent      TYPE TABLE OF t_ent,
          l_ent      TYPE t_ent,
          l_ok       TYPE c LENGTH 1,
          l_fila     TYPE i,
          l_vbtyp    TYPE vbak-vbtyp,
          l_tcode    TYPE sy-tcode,
          l_par      TYPE c LENGTH 3.

    l_entrada = vbeln.
    IF l_entrada(1) = '@'.
      REPLACE ALL OCCURRENCES OF '@' IN l_entrada WITH ''.
      l_entrada = l_entrada+5.
    ENDIF.

    IF l_entrada CS ','.
      l_lista = l_entrada.
      CLEAR l_vbeln.
    ELSE.
      l_vbeln = l_entrada.
      __poner_ceros l_vbeln.
    ENDIF.

    IF NOT l_vbeln IS INITIAL.
      CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
        EXPORTING
          vbeln = l_vbeln.
    ELSE.
      i_facturas = zcl_ap_lista=>lista2tabla_n10( lista = l_lista ).
      DESCRIBE TABLE i_facturas LINES sy-tfill.
      IF sy-tfill = 1.
        READ TABLE i_facturas INTO l_vbeln INDEX 1.
        CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
          EXPORTING
            vbeln = l_vbeln.
      ELSEIF sy-tfill > 1.
        SELECT * FROM vbrk
          INTO CORRESPONDING FIELDS OF TABLE i_ent
          FOR ALL ENTRIES IN i_facturas
         WHERE vbeln = i_facturas-vbeln
         ORDER BY PRIMARY KEY.
        DESCRIBE TABLE i_ent LINES sy-tfill.
        IF sy-tfill = 1.
          READ TABLE i_ent INTO l_ent INDEX 1.
          IF sy-subrc = 0.
            CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
              EXPORTING
                vbeln = l_ent-vbeln.
          ENDIF.
        ELSEIF sy-tfill > 1.
          CALL FUNCTION 'Z_POPUP_ALV_AP'
            EXPORTING
              titulo         = 'Seleccione factura a visualizar'(spv)
              campos_hotspot = 'VBELN'
              ancho          = 40
              botones        = 'CANCEL'
            TABLES
              t_datos        = i_ent.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    " handle_double_click
