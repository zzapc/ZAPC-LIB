
CLASS zcl_ap_registro_info DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_escalas TYPE STANDARD TABLE OF ekomd WITH NON-UNIQUE KEY klfn1.

    CONSTANTS c_tcode_crear     TYPE sy-tcode VALUE 'ME11' ##NO_TEXT.
    CONSTANTS c_tcode_modificar TYPE sy-tcode VALUE 'ME12' ##NO_TEXT.

    DATA msgty   TYPE sy-msgty.
    DATA mensaje TYPE bapireturn1-message.
    DATA lights  TYPE sy-msgty.
    DATA eina    TYPE eina.
    DATA eine    TYPE eine.
    DATA a017    TYPE a017.

    METHODS crear
      IMPORTING p_eina       TYPE eina
                p_eine       TYPE eine       OPTIONAL
                modo         TYPE bdc_mode   DEFAULT 'N'
                i_escalas    TYPE tt_escalas OPTIONAL
      RETURNING VALUE(infnr) TYPE eina-infnr.

    METHODS existe
      IMPORTING lifnr        TYPE eina-lifnr
                matnr        TYPE eina-matnr
                ekorg        TYPE eine-ekorg OPTIONAL
                werks        TYPE eine-werks OPTIONAL
                esokz        TYPE eine-esokz DEFAULT '0'
      RETURNING VALUE(infnr) TYPE eina-infnr.

    CLASS-METHODS visualizar_st
      IMPORTING lifnr TYPE eina-lifnr OPTIONAL
                matnr TYPE eina-matnr OPTIONAL
                ekorg TYPE eine-ekorg OPTIONAL
                werks TYPE eine-werks OPTIONAL
                esokz TYPE eine-esokz DEFAULT '0'
                infnr TYPE eina-infnr OPTIONAL.

    METHODS modificar_cond_a017
      IMPORTING datab          TYPE rv13a-datab
                datbi          TYPE rv13a-datbi DEFAULT '99991231'
                kbetr          TYPE konp-kbetr
                konwa          TYPE konp-konwa  DEFAULT 'EUR'
                kpein          TYPE konp-kpein  OPTIONAL
                kmein          TYPE konp-kmein  DEFAULT 'ST'
                bpumz          TYPE eine-bpumz  OPTIONAL
                bpumn          TYPE eine-bpumn  OPTIONAL
                modo           TYPE bdc_mode    DEFAULT 'N'
      RETURNING VALUE(mensaje) TYPE bapireturn1-message.

    METHODS get_cond_a017_fecha
      IMPORTING fecha       TYPE a017-datbi
                kschl       TYPE a017-kschl DEFAULT 'PB00'
      RETURNING VALUE(konp) TYPE konp.

    METHODS get_cond_a018_fecha
      IMPORTING fecha       TYPE a017-datbi
                kschl       TYPE a017-kschl DEFAULT 'PB00'
      RETURNING VALUE(konp) TYPE konp.

    METHODS modificar_precio
      IMPORTING p_eine         TYPE eine
                modo           TYPE bdc_mode   DEFAULT 'N'
                p_eina         TYPE eina
                i_escalas      TYPE tt_escalas OPTIONAL
      RETURNING VALUE(message) TYPE bapi_msg.



endclass. "ZCL_AP_REGISTRO_INFO definition
class ZCL_AP_REGISTRO_INFO implementation.
  METHOD crear.
    DATA o_bi  TYPE REF TO zcl_ap_batch_input.
    DATA l_ind TYPE numc2.

    o_bi = NEW #( ).

* Pantalla de llamada info de compras
    o_bi->dynpro( program = 'SAPMM06I' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'EINA-LIFNR'
                  valor = p_eina-lifnr ). " Número de cuenta del proveedor
    o_bi->campos( campo = 'EINA-MATNR'
                  valor = p_eina-matnr ). " Número de material
    o_bi->campos( campo = 'EINE-EKORG'
                  valor = p_eine-ekorg ). " Organización de compras
    o_bi->campos( campo = 'EINE-WERKS'
                   valor = p_eine-werks ). " Centro
    o_bi->campos( campo = 'EINA-INFNR' valor = '' ).
    CASE p_eine-esokz.
      WHEN '0' OR ''.  " Normal
        o_bi->campos( campo = 'RM06I-NORMB'
                      valor = 'X' ). " Indicador: registro info normal
      WHEN '1'.        " Contabilidad
        MESSAGE 'Error tipo contabilidad' TYPE 'A'.
      WHEN '2'.        " Consignación
        o_bi->campos( campo = 'RM06I-KONSI'
                      valor = 'X' ).
      WHEN '3'.        " Subcontratación
        o_bi->campos( campo = 'RM06I-LOHNB'
                      valor = 'X' ).
      WHEN 'P'.        " Pipeline
        o_bi->campos( campo = 'RM06I-PIPEL'
                      valor = 'X' ).
      WHEN OTHERS.        " Otrod
        MESSAGE 'Error tipo' TYPE 'A'.
    ENDCASE.

* Datos generales del info de compras
    o_bi->dynpro( program = 'SAPMM06I' dynpro = '0101' ).
    IF p_eine IS INITIAL.
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).
    ELSE.
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=EINE' ).
    ENDIF.

    IF NOT p_eina-meins IS INITIAL.
      o_bi->campos( campo = 'EINA-MEINS' valor = p_eina-meins ).
    ENDIF.
    IF NOT p_eina-idnlf IS INITIAL.
      o_bi->campos( campo = 'EINA-IDNLF' valor = p_eina-idnlf ).
    ENDIF.
    IF NOT p_eina-umrez IS INITIAL OR NOT p_eina-umrez IS INITIAL.
      o_bi->campos( campo = 'EINA-UMREZ' valor = p_eina-umrez ).
      o_bi->campos( campo = 'EINA-UMREN' valor = p_eina-umren ).
    ENDIF.

    IF NOT p_eine IS INITIAL.

* Datos de organización de compras del info-compras parte 1
      o_bi->dynpro( program = 'SAPMM06I' dynpro = '0102' ).
      o_bi->campos( campo = 'EINE-NORBM' valor = p_eine-norbm ). " Cantidad de pedido estándar
      o_bi->campos( campo = 'EINE-MWSKZ' valor = p_eine-mwskz ). " Indicador IVA
      o_bi->campos( campo = 'EINE-NETPR' valor = p_eine-netpr ). " Precio neto en info de compras

      IF NOT p_eine-ekgrp IS INITIAL.
        o_bi->campos( campo = 'EINE-EKGRP' valor = p_eine-ekgrp ).
      ENDIF.

      IF NOT p_eine-peinh IS INITIAL.
        o_bi->campos( campo = 'EINE-PEINH' valor = p_eine-peinh ).
      ENDIF.

      IF p_eine-bprme <> p_eina-meins.
        IF NOT p_eine-bpumz IS INITIAL.
          o_bi->campos( campo = 'EINE-BPUMZ' valor = p_eine-bpumz ).
        ENDIF.
        IF NOT p_eine-bpumn IS INITIAL.
          o_bi->campos( campo = 'EINE-BPUMN' valor = p_eine-bpumn ).
        ENDIF.
      ENDIF.

      IF NOT p_eine-bprme IS INITIAL.
        o_bi->campos( campo = 'EINE-BPRME' valor = p_eine-bprme ).
      ENDIF.

      IF NOT p_eine-aplfz IS INITIAL.
        o_bi->campos( campo = 'EINE-APLFZ' valor = p_eine-aplfz ).
      ENDIF.

      IF NOT p_eine-minbm IS INITIAL.
        o_bi->campos( campo = 'EINE-MINBM' valor = p_eine-minbm ).
      ENDIF.

      IF NOT p_eine-norbm IS INITIAL.
        o_bi->campos( campo = 'EINE-NORBM' valor = p_eine-norbm ).
      ENDIF.
    ENDIF.

    IF i_escalas IS INITIAL.
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).
    ELSE.
* Datos de organización de compras del info-compras parte 1
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=KO' ).

* Condición: condiciones adicionales
      o_bi->dynpro( program = 'SAPMV13A' dynpro = '0201' okcode = '=PSTF' ).
      o_bi->campos( campo = 'RV13A-DATAB' valor = sy-datum ). " Inicio de validez de la condición
      o_bi->campos( campo = 'RV13A-DATBI' valor = '31.12.9999' ). " Fin de validez del registro de condición
      o_bi->campos( campo = 'RV130-SELKZ(01)' valor = 'X' ). " Indicador de selección en dynpros de lista
      o_bi->campos( campo = 'KONP-KBETR(01)' valor = p_eine-netpr ). " Importe/porcentaje de condición si no existe escala
      o_bi->campos( campo = 'KONP-KONWA(01)' valor = p_eine-waers  ).
      o_bi->campos( campo = 'KONP-KPEIN(01)' valor = p_eine-peinh ).
      o_bi->campos( campo = 'KONP-KMEIN(01)' valor = p_eine-bprme ).

* Condiciones: Escalas de cantidades
      o_bi->dynpro( program = 'SAPMV13A' dynpro = '0303' okcode = '=SICH' ).
      LOOP AT i_escalas ASSIGNING FIELD-SYMBOL(<escala>).
        l_ind = sy-tabix.
        o_bi->campos( campo = 'KONM-KSTBM' ind = l_ind valor = <escala>-kstbm ). " Cantidad de la condición de base de escala
        o_bi->campos( campo = 'KONM-KBETR' ind = l_ind valor = <escala>-kbetr ). " Importe o porcentaje de la condición
        IF l_ind = '01'.
          o_bi->campos( campo = 'RV13A-SKONWA' ind = l_ind valor = <escala>-koein  ).
          o_bi->campos( campo = 'RV13A-KPEIN' ind = l_ind valor = <escala>-kpein ).
          o_bi->campos( campo = 'RV13A-KMEIN' ind = l_ind valor = <escala>-kmein ).
        ENDIF.
      ENDLOOP.
*    ADD 1 TO l_ind.
*    WHILE l_ind < 8.
*      o_bi->campos( campo = 'KONM-KSTBM' ind = l_ind valor = '' ). " Cantidad de la condición de base de escala
*      o_bi->campos( campo = 'KONM-KBETR' ind = l_ind valor = '' ). " Importe o porcentaje de la condición
*      ADD 1 TO l_ind.
*    ENDWHILE.
    ENDIF.

    mensaje = o_bi->llamar_transaccion( tcode = c_tcode_crear
                        modo = modo ).
    msgty = o_bi->msgty.
    IF msgty = 'S'.
      lights = '3'.
      infnr = o_bi->msgv1.
    ELSE.
      lights = '1'.
    ENDIF.
  ENDMETHOD.
  METHOD existe.
    CLEAR: infnr, eina, eine.

    SELECT * FROM eina
      INTO eina
      UP TO 1 ROWS
     WHERE lifnr = lifnr
       AND matnr = matnr
       AND loekz = ''
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      infnr = eina-infnr.
      SELECT SINGLE * FROM eine
        INTO eine
       WHERE infnr = infnr
         AND ekorg = ekorg
         AND esokz = esokz
         AND werks = werks
         AND loekz = ''.
    ENDIF.
  ENDMETHOD.
  METHOD get_cond_a017_fecha.
    CLEAR: konp, a017.

    IF NOT eine-werks IS INITIAL.
      SELECT  * FROM a017
        INTO a017
        UP TO 1 ROWS
       WHERE kappl  = 'M'
         AND kschl  = kschl
         AND ekorg  = eine-ekorg
         AND lifnr  = eina-lifnr
         AND werks  = eine-werks
         AND matnr  = eina-matnr
         AND datbi >= fecha
         AND datab <= fecha
       ORDER BY PRIMARY KEY.
      ENDSELECT.
    ELSE.
      SELECT * FROM a018
        INTO CORRESPONDING FIELDS OF a017
        UP TO 1 ROWS
       WHERE kappl  = 'M'
         AND kschl  = kschl
         AND ekorg  = eine-ekorg
         AND lifnr  = eina-lifnr
         AND matnr  = eina-matnr
         AND datbi >= fecha
         AND datab <= fecha
       ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.

    SELECT * FROM konp
      INTO konp
      UP TO 1 ROWS
     WHERE knumh    = a017-knumh
       AND loevm_ko = ''
     ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_cond_a018_fecha.
    CLEAR: konp, a017.

    SELECT * FROM a018
      INTO CORRESPONDING FIELDS OF a017
      UP TO 1 ROWS
     WHERE kappl  = 'M'
       AND kschl  = kschl
       AND ekorg  = eine-ekorg
       AND lifnr  = eina-lifnr
       AND matnr  = eina-matnr
       AND datbi >= fecha
       AND datab <= fecha
     ORDER BY PRIMARY KEY.
    ENDSELECT.

    SELECT * FROM konp
      INTO konp
      UP TO 1 ROWS
     WHERE knumh    = a017-knumh
       AND loevm_ko = ''
     ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD modificar_cond_a017.
    DATA: o_bi    TYPE REF TO zcl_ap_batch_input,
          l_konp  TYPE konp,
          l_meins TYPE meins.

    o_bi = NEW #( ).

* Pantalla de llamada info de compras
    o_bi->dynpro( program = 'SAPMM06I' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'EINA-LIFNR'
                  valor = eina-lifnr ). " Número de cuenta del proveedor
    o_bi->campos( campo = 'EINA-MATNR'
                  valor = eina-matnr ). " Número de material
    o_bi->campos( campo = 'EINE-EKORG'
                  valor = eine-ekorg ). " Organización de compras
    o_bi->campos( campo = 'EINE-WERKS'
                  valor = eine-werks ). " Centro
    CASE eine-esokz.
      WHEN '0' OR ''.  " Normal
        o_bi->campos( campo = 'RM06I-NORMB'
                      valor = 'X' ). " Indicador: registro info normal
      WHEN '2'.        " Consignación
        o_bi->campos( campo = 'RM06I-KONSI'
                      valor = 'X' ).
      WHEN '3'.        " Subcontratación
        o_bi->campos( campo = 'RM06I-LOHNB'
                      valor = 'X' ).
      WHEN 'P'.        " Pipeline
        o_bi->campos( campo = 'RM06I-PIPEL'
                      valor = 'X' ).
    ENDCASE.

* Datos generales del info de compras
    o_bi->dynpro( program = 'SAPMM06I' dynpro = '0101' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=KO' ).

    l_konp = get_cond_a017_fecha( fecha = datab ).
    IF l_konp IS INITIAL.
      l_konp = get_cond_a018_fecha( fecha = datab ).
    ENDIF.
    IF NOT l_konp IS INITIAL OR datab < sy-datum.
* Visual. de periodos de validez
      o_bi->dynpro( program = 'SAPLV14A' dynpro = '0102' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=NEWD' ).
    ENDIF.

* Condición: condiciones adicionales
    o_bi->dynpro( program = 'SAPMV13A' dynpro = '0201' ).
    o_bi->campos( campo = 'BDC_CURSOR' valor = 'KONP-KMEIN(01)' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).
    o_bi->campos( campo = 'RV13A-DATAB'
                  valor = datab ). " Inicio de validez de la condición
    o_bi->campos( campo = 'RV13A-DATBI'
                  valor = datbi ). " Fin validez del registro condición
    o_bi->campos( campo = 'KONP-KBETR(01)'
                  valor = kbetr ).
    IF NOT kpein IS INITIAL.
      o_bi->campos( campo = 'KONP-KPEIN(01)'
                    valor = kpein ). " Ctd.Base
    ENDIF.
    " Importe/porcentaje de condición si no existe escala
    o_bi->campos( campo = 'KONP-KONWA(01)'
                  valor = konwa ). " Unidad de condición
    o_bi->campos( campo = 'KONP-KMEIN(01)'
                  valor = kmein ). " Unidad de medida para la condición

    l_meins = zcl_ap_material=>get_unidad_base( eina-matnr ).
    IF l_meins <> kmein.
      o_bi->dynpro( program = 'SAPMV13A' dynpro = '0122' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
      IF NOT bpumz IS INITIAL.
        o_bi->campos( campo = 'KONP-KUMNE' valor = bpumz ).
      ENDIF.
      IF NOT bpumn IS INITIAL.
        o_bi->campos( campo = 'KONP-KUMZA' valor = bpumn ).
      ENDIF.
    ENDIF.

    mensaje = o_bi->llamar_transaccion( tcode = c_tcode_modificar
                                          modo = modo ).
    me->mensaje = mensaje.
    msgty = o_bi->msgty.
    lights = o_bi->lights.
  ENDMETHOD.
  METHOD modificar_precio.
    DATA o_bi  TYPE REF TO zcl_ap_batch_input.
    DATA l_ind TYPE numc2.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Pantalla de llamada info de compras
    o_bi->dynpro( program = 'SAPMM06I' dynpro = '0100' okcode = '/00' ).
    o_bi->campos( campo = 'EINA-LIFNR' valor = p_eina-lifnr ). " Número de cuenta del proveedor
    o_bi->campos( campo = 'EINA-MATNR' valor = p_eina-matnr ). " Número de material
    o_bi->campos( campo = 'EINE-EKORG' valor = p_eine-ekorg ). " Organización de compras
    o_bi->campos( campo = 'EINE-WERKS' valor = p_eine-werks ). " Centro
    o_bi->campos( campo = 'EINA-INFNR' valor = p_eine-infnr ). " Número del registro info de compras
    IF p_eine-esokz = '3'.
      o_bi->campos( campo = 'RM06I-LOHNB' valor = 'X' ). " Indicador: registro info de subcontratación
      o_bi->campos( campo = 'RM06I-NORMB' valor = '' ).
    ENDIF.

* Datos generales del info de compras
    o_bi->dynpro( program = 'SAPMM06I' dynpro = '0101' okcode = '=EINE' ).

* Datos de organización de compras del info-compras parte 1
    o_bi->dynpro( program = 'SAPMM06I' dynpro = '0102' okcode = '=KO' ).

* Visual. de periodos de validez
    o_bi->dynpro( program = 'SAPLV14A' dynpro = '0102' okcode = '=NEWD' ).

    IF i_escalas IS INITIAL.
* Condición: condiciones adicionales
      o_bi->dynpro( program = 'SAPMV13A' dynpro = '0201' okcode = '=SICH' ).
      o_bi->campos( campo = 'RV13A-DATAB' valor = sy-datum ). " Inicio de validez de la condición
      o_bi->campos( campo = 'RV13A-DATBI' valor = '31.12.9999' ). " Fin de validez del registro de condición
      o_bi->campos( campo = 'KONP-KBETR(01)' valor = p_eine-netpr ). " Importe/porcentaje de condición si no existe escala
      o_bi->campos( campo = 'KONP-KONWA(01)' valor = p_eine-waers  ).
      o_bi->campos( campo = 'KONP-KPEIN(01)' valor = p_eine-peinh ).
      o_bi->campos( campo = 'KONP-KMEIN(01)' valor = p_eine-bprme ).
    ELSE.
* Condición: condiciones adicionales
      o_bi->dynpro( program = 'SAPMV13A' dynpro = '0201' okcode = '=PSTF' ).
      o_bi->campos( campo = 'RV13A-DATAB' valor = sy-datum ). " Inicio de validez de la condición
      o_bi->campos( campo = 'RV13A-DATBI' valor = '31.12.9999' ). " Fin de validez del registro de condición
      o_bi->campos( campo = 'RV130-SELKZ(01)' valor = 'X' ). " Indicador de selección en dynpros de lista
      o_bi->campos( campo = 'KONP-KBETR(01)' valor = p_eine-netpr ). " Importe/porcentaje de condición si no existe escala
      o_bi->campos( campo = 'KONP-KONWA(01)' valor = p_eine-waers  ).
      o_bi->campos( campo = 'KONP-KPEIN(01)' valor = p_eine-peinh ).
      o_bi->campos( campo = 'KONP-KMEIN(01)' valor = p_eine-bprme ).

* Condiciones: Escalas de cantidades
      o_bi->dynpro( program = 'SAPMV13A' dynpro = '0303' okcode = '=SICH' ).
      LOOP AT i_escalas ASSIGNING FIELD-SYMBOL(<escala>).
        l_ind = sy-tabix.
        o_bi->campos( campo = 'KONM-KSTBM' ind = l_ind valor = <escala>-kstbm ). " Cantidad de la condición de base de escala
        o_bi->campos( campo = 'KONM-KBETR' ind = l_ind valor = <escala>-kbetr ). " Importe o porcentaje de la condición
        IF l_ind = '01'.
          o_bi->campos( campo = 'RV13A-SKONWA' ind = l_ind valor = <escala>-koein  ).
          o_bi->campos( campo = 'RV13A-KPEIN' ind = l_ind valor = <escala>-kpein ).
          o_bi->campos( campo = 'RV13A-KMEIN' ind = l_ind valor = <escala>-kmein ).
        ENDIF.
      ENDLOOP.
*    ADD 1 TO l_ind.
*    WHILE l_ind < 8.
*      o_bi->campos( campo = 'KONM-KSTBM' ind = l_ind valor = '' ). " Cantidad de la condición de base de escala
*      o_bi->campos( campo = 'KONM-KBETR' ind = l_ind valor = '' ). " Importe o porcentaje de la condición
*      ADD 1 TO l_ind.
*    ENDWHILE.
    ENDIF.

    mensaje = o_bi->llamar_transaccion( tcode = c_tcode_modificar
                        modo = modo ).
    msgty = o_bi->msgty.
    IF msgty = 'S'.
      lights = '3'.
    ELSE.
      lights = '1'.
      message = mensaje.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar_st.
    DATA o_bi      TYPE REF TO zcl_ap_batch_input.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mensaje TYPE bapireturn1-message.

    o_bi = NEW #( ).

* Pantalla de llamada info de compras
    o_bi->dynpro( program = 'SAPMM06I' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'EINA-LIFNR' valor = lifnr ). " Número de cuenta del proveedor
    o_bi->campos( campo = 'EINA-MATNR' valor = matnr ). " Número de material
    o_bi->campos( campo = 'EINE-EKORG' valor = ekorg ). " Organización de compras
    o_bi->campos( campo = 'EINE-WERKS' valor = werks ). " Centro
    o_bi->campos( campo = 'EINA-INFNR' valor = infnr ).

    CASE esokz.
      WHEN '0' OR ''.  " Normal
        o_bi->campos( campo = 'RM06I-NORMB'
                      valor = 'X' ). " Indicador: registro info normal
      WHEN '2'.        " Consignación
        o_bi->campos( campo = 'RM06I-KONSI'
                      valor = 'X' ).
      WHEN '3'.        " Subcontratación
        o_bi->campos( campo = 'RM06I-LOHNB'
                      valor = 'X' ).
      WHEN 'P'.        " Pipeline
        o_bi->campos( campo = 'RM06I-PIPEL'
                      valor = 'X' ).
    ENDCASE.

    l_mensaje = o_bi->llamar_transaccion( tcode = 'ME13' modo = 'E' ).
  ENDMETHOD.
