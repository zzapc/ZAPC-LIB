class ZCL_AP_TRANSPORTE definition
  public
  create public .

public section.

  types:
    BEGIN OF t_entregas,
        vbeln TYPE vbeln_vl,
        wbstk TYPE vbuk-wbstk,
      END OF t_entregas .
  types:
    tt_entregas TYPE TABLE OF t_entregas .

  class-methods VISUALIZAR
    importing
      !TKNUM type ANY .
  class-methods MODIFICAR
    importing
      !TKNUM type ANY .
  class-methods GET_TEXTO_STRING
    importing
      !TKNUM type TKNUM
      !ID type STXH-TDID
      !SPRAS type SPRAS default SY-LANGU
    returning
      value(STRING) type STRING .
  class-methods MODIFICAR_COSTE
    importing
      !TKNUM type TKNUM
      !O_LOG type ref to ZCL_AP_LOG optional
      !COMMIT type ABAP_BOOL default 'X'
      !COSTE type ANY
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods MODIFICAR_STATUS
    importing
      !TKNUM type TKNUM
      !O_LOG type ref to ZCL_AP_LOG optional
      !COMMIT type ABAP_BOOL default 'X'
      !STATUS type STTRG
      !SOLO_SUPERIOR type ABAP_BOOL default 'X'
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods GET_N_ENTREGAS
    importing
      !TKNUM type TKNUM optional
      !SOLO_SIN_SM type ABAP_BOOL default ''
    preferred parameter TKNUM
    returning
      value(NENTREGAS) type INT4 .
  class-methods GET_ENTREGAS
    importing
      !TKNUM type TKNUM
    exporting
      !ENTREGAS type TT_ENTREGAS .
  class-methods CONTABILIZAR_GASTOS
    importing
      !TKNUM type TKNUM
      !O_LOG type ref to ZCL_AP_LOG
      !MODOCT type CHAR1 default 'N'
      !SEGUNDOS_ESPERA_SI_BLOQUEOS type INT2 default 10
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods ESPERA_SI_BLOQUEADO
    importing
      !TKNUM type TKNUM
      !SEGUNDOS_ESPERA type INT2 default 10
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods ESTA_BLOQUEADO
    importing
      !TKNUM type TKNUM
    returning
      value(BLOQUEADA) type ABAP_BOOL .
  class-methods MODIFICAR_FECHA_HORA
    importing
      !TKNUM type TKNUM
      !O_LOG type ref to ZCL_AP_LOG optional
      !COMMIT type ABAP_BOOL default 'X'
      !FECHA type VTTK-DPREG
      !HORA type VTTK-UPREG
      !CAMPO_FECHA type ANY
      !TIME_ZONE type TZNZONE default 'UTC'
      !ANULAR type ABAP_BOOL default ''
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods SIMULAR_GASTOS_TRANSPORTE
    importing
      !TKNUM type TKNUM
      !POPUP type ABAP_BOOL default ''
      !ESPERA_ACTIVA type ABAP_BOOL default ''
    exporting
      !FKNUM type VFKP-FKNUM
      !GASTO type KOMV-KBETR
      !MESSAGE type BAPI_MSG
      !I_MSG type BAPIRET2_T
      !E_FREIGHT_COSTS type V54A0_SCDD_TAB
      !E_ERRORS_OCCURED type C
      !E_WARNINGS_OCCURED type C
      !E_CREATED_FREIGHT_COSTS type I
      !I_KOMV type KOMV_T
      !MESSAGE_OK type BAPI_MSG
      !WAERS type WAERS .
  class-methods BORRAR_GASTO_TRANSPORTE
    importing
      !FKNUM type FKNUM
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods BORRAR_TRANSPORTE
    importing
      !TKNUM type TKNUM
      !O_LOG type ref to ZCL_AP_LOG optional
      !COMMIT type ABAP_BOOL default 'X'
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods DESVINCULAR_ENTREGA
    importing
      !TKNUM type TKNUM
      !VBELN type VBELN_VL
      !O_LOG type ref to ZCL_AP_LOG optional
      !COMMIT type ABAP_BOOL default 'X'
      !FORZAR type ABAP_BOOL default ''
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods ADD_ENTREGA
    importing
      !VBELN type VBELN_VL optional
      !TKNUM type TKNUM
      !COMMIT type ABAP_BOOL default 'X'
      !I_VBELN type TT_VBELN optional
    exporting
      !I_RETURN type BAPIRET2_T
    returning
      value(MESSAGE) type BAPI_MSG .
protected section.
private section.
endclass. "ZCL_AP_TRANSPORTE definition
class ZCL_AP_TRANSPORTE implementation.
METHOD add_entrega.
    DATA: headerdata       TYPE bapishipmentheader,
          headerdataaction TYPE bapishipmentheaderaction,
          itemdata         TYPE STANDARD TABLE OF bapishipmentitem,
          itemdataaction   TYPE STANDARD TABLE OF bapishipmentitemaction,
          l_return         TYPE bapiret2,
          l_vttk           TYPE vttk,
          i_vbelnl         TYPE tt_vbeln.

    CLEAR: i_return, message.

    IF vbeln IS INITIAL AND i_vbeln IS INITIAL.
      message = 'No ha informado entrega'.
      RETURN.
    ELSEIF NOT i_vbeln IS INITIAL.
      i_vbelnl = i_vbeln.
    ELSE.
      APPEND vbeln TO i_vbelnl.
    ENDIF.

    SELECT SINGLE tknum FROM vttk
      INTO l_vttk-tknum
     WHERE tknum = tknum.
    IF sy-subrc NE 0.
      __concat2 message 'No existe el transporte'(net) tknum.
    ELSE.
      LOOP AT i_vbelnl ASSIGNING FIELD-SYMBOL(<vbeln>).
        SELECT SINGLE tknum FROM vttp
          INTO l_vttk-tknum
         WHERE tknum = tknum
           AND vbeln = <vbeln>.
        IF sy-subrc = 0.
          DELETE i_vbelnl.
        ENDIF.
      ENDLOOP.

      IF i_vbelnl IS INITIAL.
        RETURN.
      ELSE.
        SELECT MAX( tpnum ) FROM vttp
          INTO @DATA(l_tpnum)
         WHERE tknum = @tknum.

        LOOP AT i_vbelnl ASSIGNING <vbeln>.
          ADD 1 TO l_tpnum.
          APPEND VALUE #( delivery = <vbeln> itenerary = l_tpnum ) TO itemdata.
          APPEND VALUE #( delivery = 'A' itenerary = 'A' ) TO itemdataaction.
        ENDLOOP.

        headerdata-shipment_num = tknum.
        CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
          EXPORTING
            headerdata       = headerdata
            headerdataaction = headerdataaction
          TABLES
            itemdata         = itemdata
            itemdataaction   = itemdataaction
            return           = i_return.


        LOOP AT i_return INTO l_return WHERE type = 'E'. "#EC CI_STDSEQ
          __add_lista message l_return-message.
        ENDLOOP.
        IF message IS INITIAL AND commit = 'X'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD borrar_gasto_transporte.
    DATA: l_shp_tab TYPE v54a0_refobj_tab,
          l_scd_tab TYPE v54a0_scdd_tab,
          l_scd_wa  TYPE v54a0_scdd,
          l_item_wa TYPE v54a0_scd_item,
          l_t180    TYPE t180,
          l_retcode TYPE sy-subrc.

* Código basado en ROIGFQS1

    CLEAR message.

*   Lock SCD
    PERFORM scd_enqueue IN PROGRAM saplv54u USING fknum sy-subrc.
    IF NOT ( sy-subrc IS INITIAL ).
      message = 'Gasto de transporte bloqueado'.
      RETURN.
    ENDIF.

*   To create missing doc. item qty assignments
    l_t180-trtyp = 'H'.

*   Read SCD and create missing doc. item qty assignments
    CALL FUNCTION 'SD_SCD_VIEW'
      EXPORTING
        i_fknum             = fknum
        i_t180              = l_t180
        i_langu             = sy-langu
        i_opt_auth_check    = ' '
        i_opt_tvtft         = ' '
        i_opt_refobj        = 'X'
        i_opt_refobj_lock   = 'X'
*        importing
*       e_tvtft             =
      CHANGING
        c_scd               = l_scd_wa
        c_refobj_tab        = l_shp_tab
      EXCEPTIONS
        scd_not_found       = 1
*       no_authority        = 2
        tvtf_type_not_valid = 3
*       tvft_type_not_valid = 4
*       refobj_lock         = 5
*       refobj_not_found    = 6
*       delivery_missing    = 7
        error_message       = 98
        OTHERS              = 99.

    IF sy-subrc <> 0.
      message = 'Error leyendo gasto de transporte'.
      RETURN.
    ENDIF.

*   Create y-component
*   (not done within SD_SCD_VIEW due to l_t180-aktyp = h)
    l_scd_wa-y-vfkk = l_scd_wa-x-vfkk.
    LOOP AT l_scd_wa-x-item INTO l_item_wa.
*     refresh not used components
      CLEAR:   l_item_wa-konv_changed,
               l_item_wa-tvft.
      REFRESH: l_item_wa-calc_base,
               l_item_wa-komk,
               l_item_wa-komp,
               l_item_wa-komv.
      APPEND l_item_wa TO l_scd_wa-y-item.
    ENDLOOP.

    APPEND l_scd_wa TO l_scd_tab.

*   Prepare SCD for deletion
    CALL FUNCTION 'SD_SCD_DELETE'
      CHANGING
        c_scd_wa           = l_scd_wa
      EXCEPTIONS
        item_not_deletable = 1
        scd_not_deletable  = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      message = 'El gasto de transporte no puede ser borrado'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

*   Carry out changes
    CALL FUNCTION 'SD_SCD_CHANGE'
      CHANGING
        c_scd_wa  = l_scd_wa
        c_scd_tab = l_scd_tab.

    l_t180-trtyp = 'V'.

*   Delete SCD
    CALL FUNCTION 'SD_SCDS_SAVE'
      EXPORTING
        i_t180            = l_t180
        i_opt_update_task = 'X'
        i_refobj_tab      = l_shp_tab
      CHANGING
        c_scd_tab         = l_scd_tab
      EXCEPTIONS
        no_change         = 1
        OTHERS            = 2.

    l_retcode = sy-subrc.
    IF ( l_retcode = 2 ).
      message = 'Error actualizando gasto de transporte'.
      ROLLBACK WORK.
    ENDIF.

    IF ( l_retcode <> 1 ).
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.
METHOD borrar_transporte.
  DATA: headerdata       TYPE bapishipmentheader,
        headerdataaction TYPE bapishipmentheaderaction,
        i_itemdata       TYPE TABLE OF bapishipmentitem,
        i_itemdataaction TYPE TABLE OF bapishipmentitemaction,
        i_return         TYPE TABLE OF bapiret2,
        l_return         TYPE bapiret2,
        l_vttk           TYPE vttk.

  SELECT SINGLE tndr_actp FROM vttk
    INTO l_vttk-tndr_actp
   WHERE tknum = tknum.
  IF sy-subrc NE 0.
    __concat2 message 'No existe el transporte'(net) tknum.
  ELSE.
    CLEAR headerdata.
    headerdata-shipment_num = tknum.
    headerdataaction-shipment_num  = 'D'.

    SELECT vbeln FROM vttp
      INTO @DATA(l_vbeln)
     WHERE tknum = @tknum.
      APPEND VALUE #( delivery = l_vbeln ) TO i_itemdata.
      APPEND VALUE #( delivery = 'D' itenerary = 'D' ) TO i_itemdataaction.
    ENDSELECT.

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = headerdata
        headerdataaction = headerdataaction
*       TECHNCONTROL     = TECHNCONTROL
      TABLES
*       HEADERDEADLINE   =
*       HEADERDEADLINEACTION       =
        itemdata         = i_itemdata
        itemdataaction   = i_itemdataaction
*       STAGEDATA        =
*       STAGEDATAACTION  =
*       STAGEDEADLINE    =
*       STAGEDEADLINEACTION        =
*       ITEMONSTAGE      =
*       ITEMONSTAGEACTION          =
*       ADDRESS          =
*       ADDRESSACTION    =
*       HDUNHEADER       =
*       HDUNHEADERACTION =
*       HDUNITEM         =
*       HDUNITEMACTION   =
        return           = i_return.


    LOOP AT i_return INTO l_return WHERE type = 'E'      "#EC CI_STDSEQ
                                     AND id NE 'VTBAPI'
                                     AND NOT ( id = 'VW' AND number = '514' ).
      __add_lista message l_return-message.
    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT i_return INTO l_return WHERE type = 'E'.   "#EC CI_STDSEQ
        __add_lista message l_return-message.
      ENDLOOP.
    ENDIF.

    IF NOT o_log IS INITIAL.
      IF message IS INITIAL.
        o_log->log( p1 = 'Se borrado el transporte' p2 = tknum msgty = 'S' ).
      ELSE.
        o_log->set_tabla_log_from_bapiret2_t( i_return ).
      ENDIF.
    ENDIF.
  ENDIF.


ENDMETHOD.
METHOD contabilizar_gastos.
    DATA: l_fbgst TYPE vttk-fbgst,
          o_bi    TYPE REF TO zcl_ap_batch_input.

    CLEAR message.

    SELECT SINGLE fbgst FROM vttk
      INTO l_fbgst
     WHERE tknum = tknum.
    IF sy-subrc NE 0.
      __concat2 message 'No existe el transporte'(NET) tknum.
    ELSE.
      IF l_fbgst = 'C'.
        IF NOT o_log IS INITIAL.
          o_log->log( p1 = 'Transporte'(TRA) p2 = tknum p3 = 'ya tenía efectuado contabilización de gastos'(YCG) msgty = 'I' ).
        ENDIF.
      ELSE.
        message = espera_si_bloqueado( tknum = tknum segundos_espera = segundos_espera_si_bloqueos ).

        IF message IS INITIAL.
          CREATE OBJECT o_bi.

          o_bi->inicio( ).

          o_bi->dynpro( program = 'SAPMV54A' dynpro = '0010').
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=UEBP').
          o_bi->campos( campo = 'VTTK-TKNUM' valor = tknum ). " Entrega


          o_bi->dynpro( program = 'SAPMV54A' dynpro = '0030').
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH').


          message = o_bi->llamar_transaccion( tcode = 'VI01' modo = modoct ). "#EC CI_USAGE_OK[2270199]

          IF message CS 'ya están calculados'(YEC) OR message CS 'Se graban gastos'(SGG).
            CLEAR message.
          ENDIF.

          espera_si_bloqueado( tknum = tknum segundos_espera = segundos_espera_si_bloqueos ).

*    FGC. Quitamos comprobación status gastos de transporte. Nueca es 'C'.
*        CLEAR l_fbgst.
*        SELECT SINGLE fbgst FROM vttk
*          INTO l_fbgst
*         WHERE tknum = tknum.
*        IF l_fbgst = 'C'.
*          IF NOT o_log IS INITIAL.
*            o_log->log( p1 = 'Se ha modificado contabilización de gastos del transporte' p2 = tknum msgty = 'S' ).
*          ENDIF.
*          CLEAR message.
*        ELSE.
*          IF NOT o_log IS INITIAL.
*            o_log->log( p1 = 'Error efectuando contabilización de gastos del transporte' p2 = tknum p3 = message msgty = 'E' ).
*          ENDIF.
*        ENDIF.
        ELSE.
          IF NOT o_log IS INITIAL.
            o_log->log( p1 = message msgty = 'E' ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD desvincular_entrega.
  DATA: headerdata       TYPE bapishipmentheader,
        headerdataaction TYPE bapishipmentheaderaction,
        i_itemdata       TYPE TABLE OF bapishipmentitem,
        i_itemdataaction TYPE TABLE OF bapishipmentitemaction,
        i_return         TYPE TABLE OF bapiret2,
        l_return         TYPE bapiret2,
        l_vttk           TYPE vttk.

  SELECT SINGLE tndr_actp stdis FROM vttk
    INTO CORRESPONDING FIELDS OF l_vttk
   WHERE tknum = tknum.
  IF sy-subrc NE 0.
    __concat2 message 'No existe el transporte'(net) tknum.
  ELSE.
    CLEAR headerdata.
    headerdata-shipment_num = tknum.

    APPEND VALUE #( delivery = vbeln ) TO i_itemdata.
    APPEND VALUE #( delivery = 'D' itenerary = 'D' ) TO i_itemdataaction.

    IF l_vttk-stdis = 'X' AND forzar = 'X'.
      UPDATE vttk
         SET stdis = ''
       WHERE tknum = tknum.
    ENDIF.

    CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
      EXPORTING
        headerdata       = headerdata
        headerdataaction = headerdataaction
*       TECHNCONTROL     = TECHNCONTROL
      TABLES
*       HEADERDEADLINE   =
*       HEADERDEADLINEACTION       =
        itemdata         = i_itemdata
        itemdataaction   = i_itemdataaction
*       STAGEDATA        =
*       STAGEDATAACTION  =
*       STAGEDEADLINE    =
*       STAGEDEADLINEACTION        =
*       ITEMONSTAGE      =
*       ITEMONSTAGEACTION          =
*       ADDRESS          =
*       ADDRESSACTION    =
*       HDUNHEADER       =
*       HDUNHEADERACTION =
*       HDUNITEM         =
*       HDUNITEMACTION   =
        return           = i_return.

    IF l_vttk-stdis = 'X' AND forzar = 'X'.
      UPDATE vttk
         SET stdis = 'X'
       WHERE tknum = tknum.
    ENDIF.

    LOOP AT i_return INTO l_return WHERE type = 'E'.     "#EC CI_STDSEQ
      __add_lista message l_return-message.
    ENDLOOP.

    IF NOT o_log IS INITIAL.
      IF message IS INITIAL.
        o_log->log( p1 = 'Se borrado el transporte' p2 = tknum msgty = 'S' ).
      ELSE.
        o_log->set_tabla_log_from_bapiret2_t( i_return ).
      ENDIF.
    ENDIF.
  ENDIF.


ENDMETHOD.
METHOD espera_si_bloqueado.

    DATA bloqueada.

    DO segundos_espera TIMES.
      bloqueada = esta_bloqueado( tknum ).
      IF bloqueada = 'X'.
        __concat4 message 'Transporte'(TRA) tknum 'bloqueada por'(BLP) sy-msgv1.
        WAIT UP TO 1 SECONDS.
      ELSE.
        CLEAR message.
        EXIT.
      ENDIF.
    ENDDO.


  ENDMETHOD.
METHOD esta_bloqueado.

    bloqueada = zcl_ap_utils=>comprobar_bloqueo( tabla = 'VTTK'
                                                 clave = sy-mandt
                                                 clave2 = tknum ).

  ENDMETHOD.
METHOD get_entregas.
  DATA: i_vbeln    TYPE TABLE OF vbeln_vl,
        l_vbeln    TYPE vbeln_vl,
        l_entregas TYPE t_entregas,
        l_tabla    TYPE string VALUE 'VBUK'. "En HANA debería ser LIKP

  IF zcl_c=>hana = 'X'.
    l_tabla = 'LIKP'.
  ENDIF.

  SELECT DISTINCT vbeln FROM vttp
    INTO TABLE i_vbeln
   WHERE tknum = tknum.

  LOOP AT i_vbeln INTO l_vbeln.
    CLEAR l_entregas.
    l_entregas-vbeln = l_vbeln.
    SELECT SINGLE wbstk FROM (l_tabla)
     INTO l_entregas-wbstk
     WHERE vbeln = l_vbeln.
    APPEND l_entregas TO entregas.
  ENDLOOP.


ENDMETHOD.
METHOD get_n_entregas.
    DATA: i_vbeln TYPE TABLE OF vbeln_vl,
          l_vbeln TYPE vbeln_vl.

    SELECT DISTINCT vbeln FROM vttp
      INTO TABLE i_vbeln
     WHERE tknum = tknum.

    IF solo_sin_sm = 'X'.
      LOOP AT i_vbeln INTO l_vbeln.
        IF zcl_ap_entregas=>tiene_sm( l_vbeln ) = 'X'.
          DELETE i_vbeln.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DESCRIBE TABLE i_vbeln LINES nentregas.

  ENDMETHOD.
METHOD get_texto_string.

    string = zcl_ap_textos=>get_texto_string( id = id object = 'VTTK' name = tknum spras = spras ).

  ENDMETHOD.
METHOD modificar.

    SET PARAMETER ID 'TNR' FIELD tknum.
    CALL TRANSACTION 'VT02N' AND SKIP FIRST SCREEN.      "#EC CI_CALLTA

  ENDMETHOD.
METHOD modificar_coste.
    DATA: headerdata       TYPE bapishipmentheader,
          headerdataaction TYPE bapishipmentheaderaction,
          i_return         TYPE TABLE OF bapiret2,
          l_return         TYPE bapiret2,
          l_vttk           TYPE vttk.

    SELECT SINGLE tndr_actp FROM vttk
      INTO l_vttk-tndr_actp
     WHERE tknum = tknum.
    IF sy-subrc NE 0.
      __concat2 message 'No existe el transporte'(NET) tknum.
    ELSE.
      IF l_vttk-tndr_actp = coste.
        IF NOT o_log IS INITIAL.
          o_log->log( p1 = 'El coste de transporte'(ECT) p2 = coste  p3 = 'no varía'(NVA) msgty = 'W' ).
        ENDIF.
      ELSE.
        CLEAR headerdata.
        headerdata-shipment_num = tknum.
        headerdata-tendering_act_price = coste.
        headerdataaction-tendering_act_price = 'C'.

        CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
          EXPORTING
            headerdata       = headerdata
            headerdataaction = headerdataaction
*           TECHNCONTROL     = TECHNCONTROL
          TABLES
*           HEADERDEADLINE   =
*           HEADERDEADLINEACTION       =
*           ITEMDATA         =
*           ITEMDATAACTION   =
*           STAGEDATA        =
*           STAGEDATAACTION  =
*           STAGEDEADLINE    =
*           STAGEDEADLINEACTION        =
*           ITEMONSTAGE      =
*           ITEMONSTAGEACTION          =
*           ADDRESS          =
*           ADDRESSACTION    =
*           HDUNHEADER       =
*           HDUNHEADERACTION =
*           HDUNITEM         =
*           HDUNITEMACTION   =
            return           = i_return.


        LOOP AT i_return INTO l_return WHERE type = 'E'. "#EC CI_STDSEQ
          __add_lista message l_return-message.
        ENDLOOP.

        IF NOT o_log IS INITIAL.
          IF message IS INITIAL.
            o_log->log( p1 = 'Se modificado el coste del transporte'(SMC) p2 = tknum p3 = 'a' p4 = coste p5 = 'valor anterior'(VAN) p6 = l_vttk-tndr_actp msgty = 'S' ).
          ELSE.
            o_log->set_tabla_log_from_bapiret2_t( i_return ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD modificar_fecha_hora.
  DATA: headerdata             TYPE bapishipmentheader,
        headerdataaction       TYPE bapishipmentheaderaction,
        i_return               TYPE TABLE OF bapiret2,
        l_return               TYPE bapiret2,
        l_vttk                 TYPE vttk,
        l_campo                TYPE string,
        i_headerdeadline       TYPE TABLE OF bapishipmentheaderdeadline,
        l_headerdeadline       TYPE bapishipmentheaderdeadline,
        i_headerdeadlineaction TYPE TABLE OF bapishipmentheaderdeadlineact,
        l_headerdeadlineaction TYPE bapishipmentheaderdeadlineact,
        l_time_utc(16)         TYPE c,
        l_campo_hora           TYPE string,
        l_time_type            TYPE string,
        l_cambios.


  FIELD-SYMBOLS: <fecha> TYPE vttk-dpreg,
                 <hora>  TYPE vttk-upreg.

  SELECT SINGLE * FROM vttk "#EC CI_ALL_FIELDS_NEEDED
    INTO corresponding fields of l_vttk
   WHERE tknum = tknum.
  IF sy-subrc NE 0.
    __concat2 message 'No existe el transporte' tknum.
  ELSE.
    CASE campo_fecha.
      WHEN 'DTDIS'. "Fecha planificación
        l_campo_hora = 'UZDIS'.
        l_time_type  = 'HDRSTPLDT'. "*Ver códigos equivalencia en include LV56I_BAPIF03 linea 3900.
      WHEN 'DPREG'. "Fecha llegada plan
        l_campo_hora = 'UPREG'.
        l_time_type  = 'HDRSTCIPDT'. "*Ver códigos equivalencia en include LV56I_BAPIF03 linea 3900.
      WHEN 'DAREG'. "Fecha llegada confirmada
        l_campo_hora = 'UAREG'.
        l_time_type  = 'HDRSTCIADT'.
      WHEN 'DPTEN'.
        l_campo_hora = 'UPTEN'.
        l_time_type  = 'HDRSTSEPDT'. "*Ver códigos equivalencia en include LV56I_BAPIF03 linea 3900.
      WHEN 'DPLEN'. "Fin Carga plan
        l_campo_hora = 'UPLEN'.
        l_time_type  = 'HDRSTLEPDT'. "*Ver códigos equivalencia en include LV56I_BAPIF03 linea 3900.
      WHEN 'DALEN'. "Fin Carga real
        l_campo_hora = 'UALEN'.
        l_time_type  = 'HDRSTLEADT'. "*Ver códigos equivalencia en include LV56I_BAPIF03 linea 3900.
      WHEN 'DTABF'. "Despacho expdición real
        l_campo_hora = 'UZABF'.
        l_time_type  = 'HDRSTCADT'. "*Ver códigos equivalencia en include LV56I_BAPIF03 linea 3900.
      WHEN 'DPLBG'. "Inicio de carga planificado
        l_campo_hora = 'UPLBG'.
        l_time_type  = 'HDRSTLSPDT'. "*Ver códigos equivalencia en include LV56I_BAPIF03 linea 3900.
      WHEN 'DPABF'. "Despacho expedición plan
        l_campo_hora = 'UPABF'.
        l_time_type  = 'HDRSTCPDT'. "Status copmlete act
      WHEN 'DATEN'. "Fin transporte real
        l_campo_hora = 'UATEN'.
        l_time_type  = 'HDRSTSEADT'.
      WHEN OTHERS.
        message = 'Campo fecha no contemplado'.
    ENDCASE.

    IF message IS INITIAL.
      CONCATENATE 'L_VTTK-' campo_fecha INTO l_campo.
      ASSIGN (l_campo) TO <fecha>.
      IF sy-subrc = 0.
        IF <fecha> NE fecha.
          l_cambios = 'X'.
        ENDIF.
      ENDIF.

      IF l_cambios IS INITIAL.
        CONCATENATE 'L_VTTK-' l_campo_hora INTO l_campo.
        ASSIGN (l_campo) TO <hora>.
        IF sy-subrc = 0.
          IF <hora> NE hora.
            l_cambios = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      IF l_cambios IS INITIAL.
        IF NOT o_log IS INITIAL.
          o_log->log( p1 = 'El valor de fecha' p2 = campo_fecha p3 = fecha p4 = hora p5 = 'no varía' msgty = 'W' ).
        ENDIF.
      ELSE.
        CLEAR headerdata.
        headerdata-shipment_num = tknum.
        headerdataaction-shipment_num = 'X'.

        IF campo_fecha = 'DTDIS' AND l_vttk-dtdis IS INITIAL.
          headerdata-status_plan = 'X'.
          headerdataaction-status_plan = 'C'.
        ENDIF.

        IF campo_fecha = 'DAREG'.
          IF l_vttk-streg IS INITIAL.
            headerdata-status_checkin = 'X'.
            headerdataaction-status_checkin = 'C'.
          ELSEIF anular = 'X'.
            headerdata-status_checkin = ''.
            headerdataaction-status_checkin = 'D'.
          ENDIF.
        ENDIF.

        IF campo_fecha = 'DALEN' AND l_vttk-stlad IS INITIAL.
          headerdata-status_load_end = 'X'.
          headerdataaction-status_load_end = 'C'.
        ENDIF.

        IF campo_fecha = 'DTABF' AND l_vttk-stabf IS INITIAL.
          headerdata-status_compl = 'X'.
          headerdataaction-status_compl = 'C'.
        ENDIF.

        IF campo_fecha = 'DPLBG' AND l_vttk-stlbg IS INITIAL. "Inicio de carga planificado
          headerdata-status_load_start = 'X'.
          headerdataaction-status_load_start = 'C'.
        ENDIF.

        IF campo_fecha = 'DATEN' AND l_vttk-stten IS INITIAL. "Fin de transporte
          headerdata-status_shpmnt_end = 'X'.
          headerdataaction-status_shpmnt_end = 'C'.
        ENDIF.

        IF hora IS INITIAL.
          CONCATENATE fecha '000001' INTO l_time_utc.
        ELSE.
          CONCATENATE fecha hora INTO l_time_utc.
        ENDIF.
        l_headerdeadline-time_type = l_time_type.
        l_headerdeadline-time_stamp_utc = l_time_utc.
        l_headerdeadline-time_zone = time_zone.
        APPEND l_headerdeadline TO i_headerdeadline.

        l_headerdeadlineaction-time_type = 'C'.
        l_headerdeadlineaction-time_stamp_utc = 'C'.
        l_headerdeadlineaction-time_zone = 'C'.
        APPEND l_headerdeadlineaction TO i_headerdeadlineaction.


        CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
          EXPORTING
            headerdata           = headerdata
            headerdataaction     = headerdataaction
*           TECHNCONTROL         = TECHNCONTROL
          TABLES
            headerdeadline       = i_headerdeadline
            headerdeadlineaction = i_headerdeadlineaction
*           ITEMDATA             =
*           ITEMDATAACTION       =
*           STAGEDATA            =
*           STAGEDATAACTION      =
*           STAGEDEADLINE        =
*           STAGEDEADLINEACTION  =
*           ITEMONSTAGE          =
*           ITEMONSTAGEACTION    =
*           ADDRESS              =
*           ADDRESSACTION        =
*           HDUNHEADER           =
*           HDUNHEADERACTION     =
*           HDUNITEM             =
*           HDUNITEMACTION       =
            return               = i_return.


        LOOP AT i_return INTO l_return WHERE type = 'E'.
          __add_lista message l_return-message.
        ENDLOOP.

        IF message IS INITIAL AND commit = 'X'.
          zcl_ap_dev=>commit(  dequeue_all = 'X' ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT o_log IS INITIAL.
      IF message IS INITIAL.
*              o_log->log( p1 = 'Se modificado el coste del transporte' p2 = tknum p3 = 'a' p4 = coste p5 = 'valor anterior' p6 = l_vttk-tndr_actp msgty = 'S' ).
      ELSE.
        o_log->set_tabla_log_from_bapiret2_t( i_return ).
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.
METHOD modificar_status.
  DATA: headerdata             TYPE bapishipmentheader,
        headerdataaction       TYPE bapishipmentheaderaction,
        i_headerdeadline       TYPE TABLE OF bapishipmentheaderdeadline,
        i_headerdeadlineaction TYPE TABLE OF bapishipmentheaderdeadlineact,
        i_return               TYPE TABLE OF bapiret2,
        l_return               TYPE bapiret2,
        l_vttk                 TYPE vttk.
  SELECT SINGLE sttrg FROM vttk
    INTO l_vttk-sttrg
   WHERE tknum = tknum.
  IF sy-subrc NE 0.
    __concat2 message 'No existe el transporte'(net) tknum.
  ELSE.
    IF l_vttk-sttrg = status.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'El status del transporte'(est) p2 = tknum p3 = status p4 = 'no varía'(nva) msgty = 'W' ).
      ENDIF.
    ELSEIF l_vttk-sttrg > status AND solo_superior = 'X'.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'El status del transporte'(est) p2 = tknum p3 = l_vttk-sttrg p4 = 'es superior al propuesto'(esp) p5 = status msgty = 'W' ).
      ENDIF.
    ELSE.
      CLEAR headerdata.
      headerdata-shipment_num = tknum.

      CASE status.
        WHEN '1'.
*STATUS_CHECKIN STREG CHAR  1 0 Status del registro
          headerdata-status_plan  = 'X'.
          headerdataaction-status_checkin  = 'C'.

*STATUS_CHECKIN STREG CHAR  1 0 Status del registro
          headerdata-status_checkin  = ''.
          headerdataaction-status_checkin  = 'D'.
*STATUS_LOAD_START  STLBG CHAR  1 0 Status para inicio de carga
          headerdata-status_load_start = ''.
          headerdataaction-status_load_start = 'D'.
*STATUS_LOAD_END  STLAD CHAR  1 0 Status para finalización de carga
          headerdata-status_load_end = ''.
          headerdataaction-status_load_end = 'D'.

        WHEN '2'.
*STATUS_CHECKIN STREG CHAR  1 0 Status del registro
          headerdata-status_checkin  = 'X'.
          headerdataaction-status_checkin  = 'C'.
*STATUS_LOAD_START  STLBG CHAR  1 0 Status para inicio de carga
          headerdata-status_load_start = ''.
          headerdataaction-status_load_start = 'D'.
*STATUS_LOAD_END  STLAD CHAR  1 0 Status para finalización de carga
          headerdata-status_load_end = ''.
          headerdataaction-status_load_end = 'D'.
        WHEN '3'.
*STATUS_LOAD_START  STLBG CHAR  1 0 Status para inicio de carga
          headerdata-status_load_start = 'X'.
          headerdataaction-status_load_start = 'C'.
*STATUS_LOAD_END  STLAD CHAR  1 0 Status para finalización de carga
          headerdata-status_load_end = ''.
          headerdataaction-status_load_end = 'D'.
        WHEN '4'. "Fin de carga
*STATUS_LOAD_END  STLAD CHAR  1 0 Status para finalización de carga
          headerdata-status_load_end = 'X'.
          headerdataaction-status_load_end = 'C'.

        WHEN '5'.  "Despacho expedición
          headerdata-status_compl      = 'X'.
          headerdataaction-status_compl = 'C'.

        WHEN '6'.  "Inicio de transporte
          headerdata-status_shpmnt_start      = 'X'.
          headerdataaction-status_shpmnt_start = 'C'.

        WHEN '7'.  "Fin de transporte
          headerdata-status_shpmnt_end      = 'X'.
          headerdataaction-status_shpmnt_end = 'C'.

      ENDCASE.
*STATUS_LOAD_START  STLBG CHAR  1 0 Status para inicio de carga
*STATUS_LOAD_END  STLAD CHAR  1 0 Status para finalización de carga
*STATUS_COMPL STABF CHAR  1 0 Status de despacho de expedición
*STATUS_SHPMNT_START  STTBG CHAR  1 0 Status para inicio de transporte
*STATUS_SHPMNT_END  STTEN CHAR  1 0 Status para finalización de transporte

*      CONCATENATE sy-datum sy-uzeit INTO v_time_utc.
*      headerdeadline-time_type = ttype.
*      headerdeadline-time_stamp_utc = v_time_utc.
*      headerdeadline-time_zone = sy-tzone.
*      APPEND headerdeadline TO i_headerdeadline.
*
*      headerdeadlineaction-time_type = 'C'.
*      headerdeadlineaction-time_stamp_utc = 'C'.
*      headerdeadlineaction-time_zone = 'C'.
*      APPEND headerdeadlineaction TO i_headerdeadlineaction.

      espera_si_bloqueado( tknum = tknum segundos_espera = 3 ).

      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
        EXPORTING
          headerdata           = headerdata
          headerdataaction     = headerdataaction
*         TECHNCONTROL         = TECHNCONTROL
        TABLES
          headerdeadline       = i_headerdeadline
          headerdeadlineaction = i_headerdeadlineaction
*         ITEMDATA             =
*         ITEMDATAACTION       =
*         STAGEDATA            =
*         STAGEDATAACTION      =
*         STAGEDEADLINE        =
*         STAGEDEADLINEACTION  =
*         ITEMONSTAGE          =
*         ITEMONSTAGEACTION    =
*         ADDRESS              =
*         ADDRESSACTION        =
*         HDUNHEADER           =
*         HDUNHEADERACTION     =
*         HDUNITEM             =
*         HDUNITEMACTION       =
          return               = i_return.


      LOOP AT i_return INTO l_return WHERE type = 'E'.   "#EC CI_STDSEQ
        __add_lista message l_return-message.
      ENDLOOP.

      IF message IS INITIAL AND commit = 'X'.
        zcl_ap_dev=>commit( ).
      ELSE.
        IF NOT o_log IS INITIAL.
          IF message IS INITIAL.
*          o_log->log( p1 = 'Se modificado el coste del transporte' p2 = tknum p3 = 'a' p4 = coste p5 = 'valor anterior' p6 = l_vttk-tndr_actp msgty = 'S' ).
          ELSE.
            o_log->set_tabla_log_from_bapiret2_t( i_return ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.
METHOD simular_gastos_transporte.

    DATA: f_vttkvb           TYPE vttkvb,
          f_tvtk             TYPE tvtk,
          f_tvtkt            TYPE tvtkt,
          f_ttds             TYPE ttds,
          f_ttdst            TYPE ttdst,
          e_delivery_missing TYPE rv56a-selkz,
          f_vttp             TYPE STANDARD TABLE OF vttpvb,
          f_trlk             TYPE STANDARD TABLE OF vtrlk,
          f_trlp             TYPE STANDARD TABLE OF vtrlp,
          f_vtts             TYPE STANDARD TABLE OF vttsvb,
          f_vtsp             TYPE STANDARD TABLE OF vtspvb,
          f_vbpa             TYPE STANDARD TABLE OF vbpavb,
          f_vbadr            TYPE STANDARD TABLE OF sadrvb,
          f_vbplk            TYPE STANDARD TABLE OF vbplk,
          f_vbplp            TYPE STANDARD TABLE OF vbplp,
          f_vbpls            TYPE STANDARD TABLE OF vbpls,
          lt_shipments       TYPE v54a0_refobj_tab,
          ls_shipments       TYPE v54a0_refobj,
          ls_freight_costs   TYPE v54a0_scdd.

    CLEAR: fknum, gasto, message, i_msg, e_freight_costs, e_errors_occured, e_warnings_occured, e_created_freight_costs, i_komv, message_ok, waers.

    IF espera_activa = 'X'.
      DO 5 TIMES.
        SELECT SINGLE tknum FROM vttk
          INTO @DATA(l_tknum)
         WHERE tknum = @tknum.
        IF sy-subrc = 0.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.
      IF l_tknum IS INITIAL.
        message = 'No existe el transporte'.
        DATA(l_error) = 'X'.
      ENDIF.
    ENDIF.

    IF l_error IS INITIAL.
      SELECT VBELN FROM VTFA
        INTO FKNUM UP TO 1 ROWS
       WHERE TKNUM EQ SPACE AND VBELV EQ TKNUM AND VBTYP_V EQ '8' AND VBTYP_N EQ 'a'
       ORDER BY PRIMARY KEY .
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        SELECT SUM( netwr ) FROM vfkp
          INTO gasto
         WHERE rebel = tknum.

        SELECT WAERS FROM VFKP
          INTO WAERS UP TO 1 ROWS WHERE REBEL = TKNUM
         ORDER BY PRIMARY KEY .
        ENDSELECT.

        message_ok = |Gastos de transporte REALES =  { gasto } { waers }|.
      ENDIF.
    ENDIF.

    IF fknum IS INITIAL AND l_error IS INITIAL.
      TRY.
          CALL FUNCTION 'RV_SHIPMENT_VIEW'
            EXPORTING
              shipment_number             = tknum
              option_tvtk                 = abap_true
              option_ttds                 = abap_true
              language                    = sy-langu
              option_items                = abap_true
              option_minimized_item_data  = abap_false
              option_sales_orders         = abap_false
              option_export_data          = abap_false
              option_stawn_read           = abap_false
              option_segments             = abap_true
              option_partners             = abap_true
              option_messages             = abap_true
              option_packages             = abap_true
              option_flow                 = abap_true
              option_delivery_lock        = abap_false
              option_authority_check      = abap_false
              activity                    = 'A' "Display
              option_no_refresh           = abap_true
              option_ignore_missing_deliv = abap_true
              i_filter_type               = 'F' "Transportation Relevance
            IMPORTING
              f_vttkvb                    = f_vttkvb
              f_tvtk                      = f_tvtk
              f_tvtkt                     = f_tvtkt
              f_ttds                      = f_ttds
              f_ttdst                     = f_ttdst
              e_delivery_missing          = e_delivery_missing
            TABLES
              f_vttp                      = f_vttp
              f_trlk                      = f_trlk
              f_trlp                      = f_trlp
              f_vtts                      = f_vtts
              f_vtsp                      = f_vtsp
              f_vbpa                      = f_vbpa
              f_vbadr                     = f_vbadr
              f_vbplk                     = f_vbplk
              f_vbplp                     = f_vbplp
              f_vbpls                     = f_vbpls
            EXCEPTIONS
              not_found                   = 1
              no_authority                = 2
              delivery_missing            = 3
              delivery_lock               = 4
              OTHERS                      = 5.
          IF sy-subrc <> 0.
* Implement Suitable Exceptions
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_msg).
            message = |Error leyendo transporte { l_msg }|.
            l_error = 'X'.
          ENDIF.
        CATCH cx_root INTO DATA(o_root).
          message = |Error leyendo Transporte { o_root->get_text( ) }|.
          l_error = 'X'.
      ENDTRY.

      IF l_error IS INITIAL.
        ls_shipments-vttkf   = f_vttkvb.
        CLEAR: ls_shipments-vttkf-streg.
        ls_shipments-vttkf-sttrg = '1'.
        ls_shipments-vttkf-tseginit = 'X'.

        ls_shipments-vttp[]  = f_vttp[].
        ls_shipments-vtrlk[] = f_trlk[].
        ls_shipments-vtrlp[] = f_trlp[].
        ls_shipments-vtrlp_c[] = f_trlp[].
        ls_shipments-vtrlp_s[] = f_trlp[].
        ls_shipments-vttsf[] = f_vtts[].
        ls_shipments-vtsp[]  = f_vtsp[].
        ls_shipments-vbpa[]  = f_vbpa[].
        ls_shipments-vbplk[] = f_vbplk[].
        ls_shipments-vbplp[] = f_vbplp[].
*        ls_shipments-vbadr[] = f_vbadr[].
        ls_shipments-tvtk    = f_tvtk.
        ls_shipments-ttds    = f_ttds.

        CALL FUNCTION 'SD_SCD_REFOBJ_PREPARE'
          CHANGING
            c_refobj = ls_shipments.

        APPEND ls_shipments TO lt_shipments.
        CLEAR: ls_shipments.

        TRY.
            DATA: i_log      TYPE TABLE OF sprot_u,
                  l_bapiret2 TYPE bapiret2.
            CALL FUNCTION 'SD_SCD_SIMULATE_FREIGHT_COSTS'
              EXPORTING
                i_shipments             = lt_shipments
                i_scd_sim               = 3
                i_run                   = '00000000'
              IMPORTING
                e_freight_costs         = e_freight_costs
                e_errors_occured        = e_errors_occured
                e_warnings_occured      = e_warnings_occured
                e_created_freight_costs = e_created_freight_costs
              TABLES
                c_log_file              = i_log.

            LOOP AT i_log ASSIGNING FIELD-SYMBOL(<log>).
              l_bapiret2-type = <log>-severity.
              l_bapiret2-id = <log>-ag.
              l_bapiret2-number = <log>-msgnr.
              l_bapiret2-message_v1 = <log>-var1.
              l_bapiret2-message_v2 = <log>-var2.
              l_bapiret2-message_v3 = <log>-var3.
              l_bapiret2-message_v4 = <log>-var4.
              MESSAGE ID l_bapiret2-id TYPE 'S' NUMBER l_bapiret2-number WITH l_bapiret2-message_v1 l_bapiret2-message_v2 l_bapiret2-message_v3 l_bapiret2-message_v4 INTO l_bapiret2-message.
              APPEND l_bapiret2 TO i_msg.
            ENDLOOP.

            IF NOT e_freight_costs[] IS INITIAL.
              LOOP AT e_freight_costs INTO ls_freight_costs.
                LOOP AT ls_freight_costs-x-item INTO DATA(ls_item).
                  ADD ls_item-vfkp-netwr TO gasto.
                  waers = ls_item-vfkp-waers.
                  LOOP AT ls_item-komv INTO DATA(ls_komv).
                    APPEND ls_komv TO i_komv.
                  ENDLOOP.
                ENDLOOP.
              ENDLOOP.
              IF i_komv IS INITIAL.
                message = 'No se han calculado gastos de transporte'.
              ELSE.
                message_ok = |Gastos de transporte simulados =  { gasto } { waers }|.
              ENDIF.
            ELSE.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_msg.
              message = |Error simulando gastos { l_msg }|.
            ENDIF.
          CATCH cx_root INTO o_root.
            message = |Error simulando Gastos { o_root->get_text( ) }|.
        ENDTRY.
      ENDIF.
    ENDIF.

    IF popup = 'X'.
      TYPES: BEGIN OF t_msg_popup,
               lights  TYPE string,
               message TYPE bapi_msg,
             END OF t_msg_popup.
      DATA: l_texto     TYPE string,
            i_msg_popup TYPE TABLE OF t_msg_popup,
            l_msg_popup TYPE t_msg_popup.
      IF NOT message IS INITIAL.
        l_texto = 'Se ha producido un error calculando gastos de transporte'.
      ELSE.
        l_texto = message_ok.
      ENDIF.

      LOOP AT i_msg ASSIGNING FIELD-SYMBOL(<msg>).
        l_msg_popup-lights = zcl_ap_alv=>set_icono( icono = <msg>-type mensaje = <msg>-message ).
        l_msg_popup-message = <msg>-message.
        APPEND l_msg_popup TO i_msg_popup.
      ENDLOOP.
      CALL FUNCTION 'Z_POPUP_ALV_AP'
        EXPORTING
          titulo  = |Simulación gastos transporte { tknum ALPHA = OUT }|
          texto   = l_texto
          texto2  = message
          botones = 'OK'
        TABLES
          t_datos = i_msg_popup.
    ENDIF.

  ENDMETHOD.
METHOD visualizar.

    SET PARAMETER ID 'TNR' FIELD tknum.
    CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.      "#EC CI_CALLTA

  ENDMETHOD.
