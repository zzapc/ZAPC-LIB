CLASS zcl_ap_entregas DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_embalaje,
        posnr TYPE posnr,
        matnr TYPE matnr,
        charg TYPE charg_d,
        lfimg TYPE lfimg,
        vrkme TYPE vrkme,
        exidv TYPE exidv,
        vhilm TYPE vhilm,
      END OF t_embalaje.
    TYPES tt_embalaje TYPE TABLE OF t_embalaje.
    TYPES:
      BEGIN OF t_vlpod,
        posnr TYPE posnr,
        grund TYPE reacd,
        diff  TYPE lfimg_diff,
      END OF t_vlpod.
    TYPES tt_vlpod TYPE TABLE OF t_vlpod.

    CONSTANTS c_objectclas TYPE cdobjectcl         VALUE 'LIEFERUNG' ##NO_TEXT.
    CONSTANTS c_tipo_gos   TYPE srgbtbrel-typeid_a VALUE 'LIKP' ##NO_TEXT.

    CLASS-METHODS visualizar
      IMPORTING vbeln TYPE any.

    CLASS-METHODS get_texto_string
      IMPORTING vbeln         TYPE vbeln_vl
                posnr         TYPE posnr_vl OPTIONAL
                !id           TYPE stxh-tdid
                spras         TYPE spras    DEFAULT ''
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS picking
      IMPORTING vbeln           TYPE likp-vbeln
                i_lips          TYPE tab_lips
                modoct          TYPE char1       DEFAULT 'N'
                cambiar_ctd     TYPE abap_bool   DEFAULT ''
                solo_part_lotes TYPE abap_bool   DEFAULT ''
                ajustar_ctd     TYPE abap_bool   DEFAULT ''
                i_embalaje      TYPE tt_embalaje OPTIONAL
      RETURNING VALUE(mensaje)  TYPE bapiret2-message.

    CLASS-METHODS salida_mercancias
      IMPORTING vbeln          TYPE likp-vbeln
                modoct         TYPE char1             DEFAULT 'N'
                o_log          TYPE REF TO zcl_ap_log OPTIONAL
      RETURNING VALUE(mensaje) TYPE bapiret2-message.

    CLASS-METHODS deshacer_picking
      IMPORTING vbeln           TYPE likp-vbeln
                posnr           TYPE lips-posnr        OPTIONAL
                modoct          TYPE char1             DEFAULT 'N'
                mod_ctd_picking TYPE abap_bool         DEFAULT 'X'
                o_log           TYPE REF TO zcl_ap_log OPTIONAL
      RETURNING VALUE(mensaje)  TYPE bapiret2-message.

    CLASS-METHODS desembalar_entrega
      IMPORTING vbeln          TYPE likp-vbeln
                modoct         TYPE char1 DEFAULT 'N'
      RETURNING VALUE(mensaje) TYPE bapiret2-message.

    CLASS-METHODS get_direccion
      IMPORTING kunnr            TYPE kunnr     OPTIONAL
                vbeln            TYPE vbeln_vl  OPTIONAL
                ampliada         TYPE abap_bool DEFAULT ''
      RETURNING VALUE(direccion) TYPE string.

    CLASS-METHODS picking_y_embalaje
      IMPORTING vbeln              TYPE likp-vbeln
                posnr              TYPE lips-posnr
                charg              TYPE charg_d
                lfimg              TYPE any
                vrkme              TYPE lips-vrkme OPTIONAL
                exidv              TYPE exidv      OPTIONAL
                modoct             TYPE char1      DEFAULT 'N'
                lstel              TYPE likp-lstel OPTIONAL
                almacen_hu         TYPE abap_bool  DEFAULT ''
                convertir_a_un_pos TYPE abap_bool  DEFAULT 'X'
      RETURNING VALUE(mensaje)     TYPE bapiret2-message.

    CLASS-METHODS desembalaje
      IMPORTING vbeln          TYPE likp-vbeln
                posnr          TYPE lips-posnr OPTIONAL
                charg          TYPE charg_d    OPTIONAL
                lfimg          TYPE any        OPTIONAL
                vrkme          TYPE lips-vrkme OPTIONAL
                exidv          TYPE exidv
                modoct         TYPE char1      DEFAULT 'N'
      RETURNING VALUE(mensaje) TYPE bapiret2-message.

    CLASS-METHODS buscar_part_lote_sin_hu
      IMPORTING vbeln               TYPE vbeln_vl
                posnr               TYPE posnr
      RETURNING VALUE(posnr_sin_hu) TYPE posnr.

    CLASS-METHODS borrar_part_lote
      IMPORTING vbeln          TYPE likp-vbeln
                posnr          TYPE lips-posnr OPTIONAL
                posnr_part     TYPE lips-posnr OPTIONAL
                modoct         TYPE char1      DEFAULT 'N'
      RETURNING VALUE(mensaje) TYPE bapiret2-message.

    CLASS-METHODS set_texto_string
      IMPORTING vbeln   TYPE vbeln_vl
                posnr   TYPE posnr_vl OPTIONAL
                !id     TYPE stxh-tdid
                spras   TYPE spras    DEFAULT ''
                !string TYPE string.

    CLASS-METHODS get_part_lote_sin_hu
      IMPORTING vbeln         TYPE vbeln_vl
      RETURNING VALUE(i_lips) TYPE tab_lips.

    CLASS-METHODS get_ctd_entrega
      IMPORTING vbeln           TYPE any
                posnr           TYPE any DEFAULT ''
                unidad          TYPE any DEFAULT ''
      RETURNING VALUE(cantidad) TYPE mengv13.

    CLASS-METHODS get_ctd_sm
      IMPORTING vbeln           TYPE any
                posnr           TYPE any DEFAULT ''
                unidad          TYPE any DEFAULT ''
      RETURNING VALUE(cantidad) TYPE mengv13.

    CLASS-METHODS hay_mod
      IMPORTING vbeln             TYPE any
                username          TYPE cdhdr-username DEFAULT sy-uname
                tcode             TYPE cdhdr-tcode    DEFAULT 'VL02N'
                segundos          TYPE i              DEFAULT 5
      RETURNING VALUE(change_ind) TYPE cdhdr-change_ind.

    CLASS-METHODS get_ctd_picking
      IMPORTING vbeln        TYPE vbeln_vl
                posnr        TYPE posnr
                add_subpos   TYPE abap_bool DEFAULT 'X'
                !buffer      TYPE abap_bool DEFAULT ''
                !select      TYPE abap_bool DEFAULT ''
      RETURNING VALUE(pikmg) TYPE pikmg.

    CLASS-METHODS get_ctd_pos
      IMPORTING vbeln        TYPE vbeln_vl
                posnr        TYPE posnr
                add_subpos   TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(lfimg) TYPE lfimg.

    CLASS-METHODS modificar
      IMPORTING vbeln      TYPE vbeln
                i_lips_mod TYPE tab_lips          OPTIONAL
                i_lips_new TYPE tab_lips          OPTIONAL
                i_lips_del TYPE tab_lips          OPTIONAL
                !commit    TYPE abap_bool         DEFAULT 'X'
                modo_bi    TYPE char1             DEFAULT 'N'
                o_log      TYPE REF TO zcl_ap_log OPTIONAL
                mod_pesos  TYPE abap_bool         DEFAULT ''
      EXPORTING i_return   TYPE bapiret2_t
                !message   TYPE bapi_msg.

    CLASS-METHODS insertar_nueva_posicion
      IMPORTING lips     TYPE lips
      EXPORTING !message TYPE bapi_msg
                posnr    TYPE posnr
                i_return TYPE bapiret2_t.

    CLASS-METHODS insertar_nueva_posicion_ct
      IMPORTING lips     TYPE lips
                modo_bi  TYPE char1 DEFAULT 'N'
      EXPORTING !message TYPE bapi_msg
                posnr    TYPE posnr.

    CLASS-METHODS get_solicitantep
      IMPORTING vbeln        TYPE vbeln_vl
      RETURNING VALUE(kunnr) TYPE kunnr.

    CLASS-METHODS get_pedido
      IMPORTING vbeln         TYPE vbeln_vl
      RETURNING VALUE(pedido) TYPE vbeln_va.

    CLASS-METHODS embalar
      IMPORTING i_pos          TYPE tt_embalaje
                vbeln          TYPE vbeln_vl
                o_log          TYPE REF TO zcl_ap_log
                modoct         TYPE char1 DEFAULT 'N'
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS tiene_sm
      IMPORTING vbeln     TYPE any
      RETURNING VALUE(sm) TYPE abap_bool.

    CLASS-METHODS anyadir_pos_pedido
      IMPORTING vbeln     TYPE vbeln_vl
                pedido    TYPE vbeln_va
                posnr     TYPE posnr
                o_log     TYPE REF TO zcl_ap_log OPTIONAL
                !commit   TYPE abap_bool         DEFAULT ''
                modoct    TYPE char1             DEFAULT 'N'
                forzar    TYPE abap_bool         DEFAULT ''
      EXPORTING posnr_new TYPE posnr
                !message  TYPE bapi_msg.

    CLASS-METHODS esta_bloqueada
      IMPORTING vbeln            TYPE vbeln_vl
      RETURNING VALUE(bloqueada) TYPE abap_bool.

    CLASS-METHODS modificar_fecha_sm
      IMPORTING vbeln                       TYPE vbeln_vl
                o_log                       TYPE REF TO zcl_ap_log OPTIONAL
                modoct                      TYPE bdcmode           DEFAULT 'N'
                forzar                      TYPE abap_bool         DEFAULT ''
                wadat                       TYPE likp-wadat_ist
                segundos_espera_si_bloqueos TYPE int2              DEFAULT 10
                !tab                        TYPE any               DEFAULT ''
      RETURNING VALUE(message)              TYPE bapi_msg.

    CLASS-METHODS espera_si_bloqueada
      IMPORTING vbeln           TYPE vbeln_vl
                segundos_espera TYPE int2    DEFAULT 10
                tiempo_espera   TYPE mengv13 DEFAULT 1
      RETURNING VALUE(message)  TYPE bapi_msg.

    CLASS-METHODS efectuar_sm
      IMPORTING vbeln                       TYPE vbeln_vl
                o_log                       TYPE REF TO zcl_ap_log OPTIONAL
                modoct                      TYPE char1             DEFAULT 'N'
                segundos_espera_si_bloqueos TYPE int2              DEFAULT 10
      RETURNING VALUE(message)              TYPE bapi_msg.

    CLASS-METHODS get_transporte
      IMPORTING vbeln        TYPE any
      RETURNING VALUE(tknum) TYPE tknum.

    CLASS-METHODS crear_desde_vl10b
      IMPORTING ebeln    TYPE ebeln
                o_log    TYPE REF TO zcl_ap_log OPTIONAL
                modoct   TYPE bdcmode           DEFAULT 'N'
                dias     TYPE int2              DEFAULT -1
                log_exit TYPE any               DEFAULT ''
                bapi     TYPE abap_bool         DEFAULT ''
      EXPORTING vbeln    TYPE vbeln
                !message TYPE bapi_msg
                warning  TYPE bapi_msg.

    CLASS-METHODS vlpod
      IMPORTING i_pos          TYPE tt_vlpod
                vbeln          TYPE vbeln_vl
                podat          TYPE likp-podat        OPTIONAL
                potim          TYPE likp-potim        OPTIONAL
                o_log          TYPE REF TO zcl_ap_log OPTIONAL
                modoct         TYPE char1             DEFAULT 'N'
                confirmar_are  TYPE abap_bool         DEFAULT ''
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS get_url_por_titulo_st
      IMPORTING vbeln      TYPE ekko-ebeln
                titulo     TYPE any
      RETURNING VALUE(url) TYPE string.

    CLASS-METHODS insertar_url_gos_st
      IMPORTING vbeln  TYPE vbeln_vl
                url    TYPE any OPTIONAL
                titulo TYPE any OPTIONAL.

    CLASS-METHODS insert_vbfa
      IMPORTING vbeln   TYPE vbeln_vl
                posnr   TYPE posnr
                vbeln_n TYPE any
                posnr_n TYPE any
                mjahr   TYPE any DEFAULT ''
                vbtyp_n TYPE any.

    CLASS-METHODS crear_desde_vl10a
      IMPORTING pedido   TYPE vbeln_va
                o_log    TYPE REF TO zcl_ap_log OPTIONAL
                modoct   TYPE bdcmode           DEFAULT 'N'
                dias     TYPE int2              DEFAULT -1
                log_exit TYPE any               DEFAULT ''
                bapi     TYPE abap_bool         DEFAULT ''
      EXPORTING vbeln    TYPE vbeln_vl
                !message TYPE bapi_msg
                warning  TYPE bapi_msg.

    CLASS-METHODS get_fecha_hora_inicio
      IMPORTING vbeln      TYPE vbeln_vl         OPTIONAL
                !handle    TYPE likp-handle      OPTIONAL
                tipo_fecha TYPE tsege-even       OPTIONAL
                tipo       TYPE tsege-even_verty OPTIONAL
      EXPORTING fecha      TYPE dats
                hora       TYPE uzeit.

    CLASS-METHODS set_fecha_hora_inicio
      IMPORTING vbeln      TYPE vbeln_vl         OPTIONAL
                !handle    TYPE likp-handle      OPTIONAL
                tipo_fecha TYPE tsege-even       OPTIONAL
                tipo       TYPE tsege-even_verty OPTIONAL
                !tzone     TYPE tsege-even_zonfr DEFAULT 'CET'
                fecha      TYPE dats
                hora       TYPE uzeit.

    CLASS-METHODS validar_entrega_modificable
      IMPORTING vbeln          TYPE vbeln_vl
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS crear_entrega_desde_pedido
      IMPORTING pedido   TYPE vbeln_va
                o_log    TYPE REF TO zcl_ap_log OPTIONAL
                modoct   TYPE bdcmode           DEFAULT 'N'
                fecha    TYPE dats              DEFAULT '99991231'
      EXPORTING vbeln    TYPE vbeln_vl
                !message TYPE bapi_msg.

    CLASS-METHODS modificar_ctd_picking
      IMPORTING vbeln    TYPE vbeln
                i_pos    TYPE tt_vbpok          OPTIONAL
                !commit  TYPE abap_bool         DEFAULT 'X'
                modo_bi  TYPE char1             DEFAULT 'N'
                o_log    TYPE REF TO zcl_ap_log OPTIONAL
      EXPORTING i_return TYPE bapiret2_t
                !message TYPE bapi_msg.

    CLASS-METHODS get_factura
      IMPORTING vbeln          TYPE vbeln_vl
      EXPORTING facturas       TYPE string
      RETURNING VALUE(factura) TYPE vbeln_vf.

    CLASS-METHODS get_lote_origen_pedido
      IMPORTING vbeln        TYPE vbeln_vl
                posnr        TYPE posnr
      RETURNING VALUE(charg) TYPE charg_d.

    CLASS-METHODS picking_opt
      IMPORTING vbeln           TYPE likp-vbeln
                i_lips          TYPE tab_lips
                modoct          TYPE char1       DEFAULT 'N'
                cambiar_ctd     TYPE abap_bool   DEFAULT ''
                solo_part_lotes TYPE abap_bool   DEFAULT ''
                ajustar_ctd     TYPE abap_bool   DEFAULT ''
                i_embalaje      TYPE tt_embalaje OPTIONAL
      RETURNING VALUE(mensaje)  TYPE bapiret2-message.

    CLASS-METHODS borrar
      IMPORTING vbeln               TYPE vbeln
                !commit             TYPE abap_bool         DEFAULT 'X'
                o_log               TYPE REF TO zcl_ap_log OPTIONAL
                sacar_de_transporte TYPE abap_bool         DEFAULT ''
      EXPORTING i_return            TYPE bapiret2_t
                !message            TYPE bapi_msg.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_ENTREGAS definition
class ZCL_AP_ENTREGAS implementation.
  METHOD anyadir_pos_pedido.
    DATA: l_tabla TYPE tabname VALUE 'VBUP',
          l_vbap  TYPE vbap,
          l_vbup  TYPE vbup,
          l_vbep  TYPE vbep,
          l_lfimg TYPE lips-lfimg.
    DATA o_bi TYPE REF TO zcl_ap_batch_input.

    IF zcl_c=>hana = 'X'.
      l_tabla = 'LIPS'.
    ENDIF.

    SELECT SINGLE kwmeng FROM vbap  ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO CORRESPONDING FIELDS OF l_vbap
     WHERE vbeln = pedido
       AND posnr = posnr.

    SELECT SINGLE uvvlk FROM (l_tabla)
      INTO l_vbup-uvvlk
     WHERE vbeln = pedido
       AND posnr = posnr.
    IF sy-subrc <> 0.
      __concat3 message 'No existe el pedido' pedido posnr.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message msgty = 'E' ).
      ENDIF.
    ELSE.
      IF forzar = 'X'.
        IF l_vbup-uvvlk = 'A' OR l_vbup-uvvlk = 'B'.
          UPDATE (l_tabla)
            SET uvvlk = 'C'
          WHERE vbeln = pedido
            AND posnr = posnr.
          IF NOT o_log IS INITIAL.
            o_log->log( p1 = 'Marcamos posición pedido' p2 = pedido p3 = posnr p4 = 'como completa. ¡REVISE PEDIDO!' msgty = 'W' ).
          ENDIF.
        ENDIF.

        SELECT SUM( bmeng ) FROM vbep
          INTO l_vbep-bmeng
        WHERE vbeln = pedido
          AND posnr = posnr.
        IF l_vbep-bmeng = 0.
          UPDATE vbep "#EC AOC_STD_TABLE
            SET bmeng = l_vbap-kwmeng
          WHERE vbeln = pedido
            AND posnr = posnr
            AND etenr = '0001'.
          IF NOT o_log IS INITIAL.
            o_log->log( p1 = 'Marcamos posición pedido' p2 = pedido p3 = posnr p4 = 'como completa' msgty = 'W' ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=RAUF_T' ).

* Entrega:     Popup para agrupación de pedidos
    o_bi->dynpro( program = 'SAPMV50A' dynpro = '0105' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENT1' ).
    o_bi->campos( campo = 'LV50C-DATBI' valor = '31.12.9999' ). " Fecha de selección de entrega
    o_bi->campos( campo = 'LV50C-VBELN' valor = pedido ). " Pedido
    o_bi->campos( campo = 'LV50C-ABPOS' valor = posnr ). " Desde posición
    o_bi->campos( campo = 'LV50C-BIPOS' valor = posnr ). " Hasta posición

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

    message = o_bi->llamar_transaccion( tcode = 'VL02N' modo = modoct ).

    SELECT posnr lfimg FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO (posnr_new, l_lfimg)
      UP TO 1 ROWS
     WHERE vgbel = pedido
       AND vgpos = posnr
      ORDER BY posnr DESCENDING.
    ENDSELECT.
    IF sy-subrc = 0.
      CLEAR message.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'En en entrega' p2 = vbeln p3 = 'se ha añadido la posición' p4 = posnr p6 = 'del pedido' p7 = pedido p8 = 'nueva posición entrega' p9 = posnr_new msgty = 'S' ).
      ENDIF.

      IF forzar = 'X'.
* Si la cantidad de la entrega es inferior a la del pedido (por verificación de disponibilidad, modificamos la entrega)
        IF l_lfimg < l_vbap-kwmeng.
          o_bi->inicio( ).

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' okcode = '=WEIT' ).
          o_bi->campos( campo = 'RV50A-POSNR' valor = posnr_new ).

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' okcode = '=SICH_T' ).
          o_bi->campos( campo = 'LIPSD-G_LFIMG(01)' valor = l_vbap-kwmeng ).

          DATA(l_msg) = o_bi->llamar_transaccion( tcode = 'VL02N' modo = modoct ).
          SELECT SINGLE lfimg FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
            INTO @DATA(l_lfimg2)
           WHERE vbeln = @vbeln
             AND posnr = @posnr.
          IF l_lfimg2 = l_vbap-kwmeng.
            IF NOT o_log IS INITIAL.
              o_log->log( p1 = 'La entrega ' p2 = vbeln p3 = posnr p4 = 'se creó por menos' p5 = l_lfimg p6 = 'se cambia a' p7 = l_vbap-kwmeng msgty = 'W' ).
            ENDIF.
          ELSE.
            UPDATE lips "#EC AOC_STD_TABLE
              SET lfimg = l_vbap-kwmeng
             WHERE vbeln = vbeln
               AND posnr = posnr.
            IF NOT o_log IS INITIAL.
              o_log->log( p1 = 'La entrega ' p2 = vbeln p3 = posnr p4 = 'se creó por menos' p5 = l_lfimg p6 = 'se fuerza a' p7 = l_vbap-kwmeng msgty = 'W' ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'Error' p2 = message p3 = 'añadiendo en entrega' p4 = vbeln p5 = 'la posición' p6 = posnr p7 = 'del pedido' p8 = pedido msgty = 'E' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD borrar.
    DATA: header_control TYPE bapiobdlvhdrctrlchg,
          header_data    TYPE bapiobdlvhdrchg,
          return         TYPE bapiret2.

    SELECT SINGLE vbtyp FROM likp
      INTO @DATA(l_vbtyp)
     WHERE vbeln = @vbeln.
    IF sy-subrc <> 0.
      message = 'No existe la entrega'.
      RETURN.
    ENDIF.

    SELECT SINGLE tknum FROM vttp
      INTO @DATA(l_tknum)
     WHERE vbeln = @vbeln.
    IF sy-subrc = 0.
      SELECT SINGLE stdis FROM vttk
        INTO @DATA(l_stdis)
       WHERE tknum = @l_tknum.
      IF sy-subrc = 0.
        IF NOT sacar_de_transporte IS INITIAL.
          IF sacar_de_transporte = 'F'.
            message = zcl_ap_transporte=>desvincular_entrega( tknum = l_tknum vbeln = vbeln forzar = 'X' o_log = o_log commit = commit ).
          ELSE.
            message = zcl_ap_transporte=>desvincular_entrega( tknum = l_tknum vbeln = vbeln o_log = o_log commit = commit ).
          ENDIF.
          IF NOT message IS INITIAL.
            RETURN.
          ENDIF.
        ELSE.
          IF l_stdis = 'X'.
            message = |No se puede borrar la entrega al estar asignada al transporte planificado { l_tknum ALPHA = OUT }|.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    header_control-deliv_numb = vbeln.
    header_data-deliv_numb = header_control-deliv_numb.
    header_control-dlv_del = 'X'.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = header_data
        header_control = header_control
        delivery       = header_data-deliv_numb
*       TECHN_CONTROL  =
*       HEADER_DATA_SPL   =
*       HEADER_CONTROL_SPL            =
*       SENDER_SYSTEM  =
      TABLES
*       HEADER_PARTNER =
*       HEADER_PARTNER_ADDR           =
*       HEADER_DEADLINES  =
*       item_data      = i_item_data
*       item_control   = i_item_control
*       ITEM_SERIAL_NO =
*       SUPPLIER_CONS_DATA            =
*       EXTENSION1     =
*       EXTENSION2     =
        return         = i_return.
*     TOKENREFERENCE =
*     ITEM_DATA_SPL  =
*     COLLECTIVE_CHANGE_ITEMS       =
*     new_item_data  = new_item_data
*     new_item_data_spl = new_item_data_spl.
*     new_item_org   = new_item_org
*     ITEM_DATA_DOCU_BATCH          =

    LOOP AT i_return INTO return WHERE message IS INITIAL.
      MESSAGE ID return-id TYPE return-type NUMBER return-number WITH return-message_v1 return-message_v2
              INTO message.
      return-message = message.
      MODIFY i_return FROM return.
    ENDLOOP.
    LOOP AT i_return INTO return WHERE type = 'E'.
      __add_lista message return-message.
    ENDLOOP.

    IF NOT o_log IS INITIAL.
      o_log->set_tabla_log_from_bapiret2_t( i_return ).
    ENDIF.

    IF message IS INITIAL AND commit = 'X'.
      zcl_ap_dev=>commit( dequeue_all = 'X' ).
    ENDIF.
  ENDMETHOD.
  METHOD borrar_part_lote.
    DATA: l_lips        TYPE lips,
          o_bi          TYPE REF TO zcl_ap_batch_input,
          l_vbfa        TYPE vbfa,
          l_ctd_picking TYPE lipsd-pikmg.

    SELECT SINGLE posnr uecha FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO CORRESPONDING FIELDS OF l_lips
     WHERE vbeln = vbeln
       AND posnr = posnr_part.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF l_lips-uecha <> posnr.
      mensaje = 'Posición de la partición no cuadra con posición principal'.
      RETURN.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

* Pulsamos para seleccionar posición
    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

* Entrega                Ventana   Posicionar    Posición
    o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
    o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-uecha ). " Número de posición del documento comercial

* Marcamos la primera fila y puslsamos sobre selección de lotes
    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHSP_T' ).
    o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).
    o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de selección en dynpros de lista

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
    o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-posnr ).

* Borramos posición
    o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
    o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).
    o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de selección en dynpros de lista
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=POLO_T' ).

* Volvemos a la pantalla inicial (picking)
    o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK_T' ).

* Nos aseguramos que estamos en la pestaÃ±a de picking
    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=T\02' ).

* Seleccionamos la posicion
    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
    o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-uecha ).

* A la cantidad de picking existente, le restamos la que acabamos de borrar
    SELECT SUM( rfmng_flo )  FROM vbfa
      INTO l_vbfa-rfmng_flo
     WHERE vbelv    = vbeln
       AND posnv    = l_lips-uecha
       AND vbeln    = vbeln
       AND posnn   <> l_lips-posnr
       AND vbtyp_n  = 'Q'.

    l_ctd_picking = l_vbfa-rfmng_flo.

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).
    o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = l_ctd_picking ).

    mensaje = o_bi->llamar_transaccion( tcode = 'VL02N' modo = modoct ).

    IF mensaje CS 'grabado'.
      CLEAR mensaje.
    ENDIF.
  ENDMETHOD.
  METHOD buscar_part_lote_sin_hu.
    DATA: l_lips TYPE lips,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_vepo TYPE vepo.

    SELECT posnr FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1] "#EC CI_EXIT_SELECT
      INTO l_lips-posnr
     WHERE vbeln = vbeln
       AND uecha = posnr.
* Verificamos si ya está asignada a entrega
      SELECT SINGLE * FROM vepo JOIN vekp ON vepo~venum = vekp~venum
        INTO CORRESPONDING FIELDS OF l_vepo
       WHERE vbeln     = vbeln
         AND posnr     = l_lips-posnr
         AND vpobj    IN ( '01', '03' )
         AND vpobjkey  = vbeln.
* Si no encuentro equivalencia en HU es que esa particición no tiene HU asignada
      IF sy-subrc <> 0.
        posnr_sin_hu = l_lips-posnr.
        EXIT.
      ENDIF.
    ENDSELECT.
  ENDMETHOD.
  METHOD crear_desde_vl10a.
    DATA: o_bi      TYPE REF TO zcl_ap_batch_input,
          l_fecha   TYPE dats,
          l_hora    TYPE sy-uzeit,
          l_vbsk    TYPE vbsk,
          l_vbfs    TYPE vbfs,
          l_msg     TYPE bapi_msg,
          l_msgc    TYPE bapi_msg,
          l_pos_ped TYPE i.
    DATA: l_request   TYPE bapideliciousrequest,
          tbl_request TYPE TABLE OF bapideliciousrequest,
          tbl_items   TYPE TABLE OF bapideliciouscreateditems,
          tbl_return  TYPE TABLE OF bapiret2,
          l_return    TYPE bapiret2,
          l_items     TYPE bapideliciouscreateditems.

    __data_set_vart vbap.

    IF bapi IS INITIAL OR modoct = 'A'.
      o_bi = NEW #( ).

      o_bi->inicio( ).
      o_bi->dynpro( program = 'RVV50R10C' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '==S0S_TAB2' ).
      IF dias = -1.
        o_bi->campos( campo = 'ST_LEDAT-HIGH' valor = '' ).
      ELSEIF dias > 0.
        l_fecha = sy-datum + dias.
        o_bi->campos( campo = 'ST_LEDAT-HIGH' valor = l_fecha ).
      ENDIF.
      o_bi->dynpro( program = 'RVV50R10C' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ONLI' ).
      o_bi->campos( campo = 'ST_VBELN-LOW' valor = pedido ).

      o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=&ALL' ).

      o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=VL01' ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

      o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=&F03' ).

      o_bi->dynpro( program = 'RVV50R10C' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/EE' ).

      GET TIME.
      l_fecha = sy-datum.
      l_hora  = sy-uzeit.

      message = o_bi->llamar_transaccion( tcode = 'VL10A' modo = modoct ).

      SELECT SINGLE sammg anzlp FROM vbsk               "#EC CI_NOFIELD
        INTO (l_vbsk-sammg, l_vbsk-anzlp)
       WHERE smart  = 'L'
         AND ernam  = sy-uname
         AND erdat  = l_fecha
         AND uzeit >= l_hora.
      IF sy-subrc = 0.
        SELECT * FROM vbfs
          INTO l_vbfs
         WHERE sammg = l_vbsk-sammg
           AND ( msgno <> '544' )
           AND msgid <> ''.
          MESSAGE ID l_vbfs-msgid TYPE 'S' NUMBER l_vbfs-msgno WITH l_vbfs-msgv1 l_vbfs-msgv2 l_vbfs-msgv3 l_vbfs-msgv4 INTO l_msg.
          __add_lista l_msgc l_msg.
        ENDSELECT.
      ENDIF.

      SELECT SINGLE vbeln FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO vbeln
       WHERE vgbel  = pedido
         AND erdat  = l_fecha
         AND erzet >= l_hora.
      IF sy-subrc = 0.
        CLEAR message.
        warning = l_msgc.

        SELECT COUNT( * ) FROM vbap ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
          INTO l_pos_ped
         WHERE vbeln = pedido
           AND abgru = ''.

        IF l_vbsk-anzlp < l_pos_ped.
          l_msg = zcl_ap_utils=>concat( p1 = 'Sólo se han creado' p2 = l_vbsk-anzlp p3 = 'pos.en entrega de las' p4 = l_pos_ped p5 = 'del pedido' ).
          __add_lista warning l_msg.
        ENDIF.
      ELSE.
        IF message CS 'producido un error inesperado'.
          CLEAR message.
        ENDIF.
        IF NOT log_exit IS INITIAL.
          SELECT SINGLE message FROM (zcl_c=>tabla_zlog)
            INTO l_msg
           WHERE proceso   = 'EXITS'
             AND clave     = pedido
             AND progname  = log_exit
             AND fecha     = l_fecha
             AND hora     >= l_hora.
          IF sy-subrc = 0.
            __concat2 message l_msg message.
          ENDIF.
        ENDIF.
        IF NOT l_msgc IS INITIAL.
          __concat_a message l_msgc.
        ENDIF.
        IF message IS INITIAL.
          message = 'No se ha podido crear la entrega'.
        ENDIF.
      ENDIF.
    ELSE.
      SELECT matnr posnr vbeln vrkme werks zmeng FROM vbap ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO CORRESPONDING FIELDS OF TABLE i_vbap
        WHERE vbeln = pedido
          AND abgru = ''.
      IF sy-subrc <> 0.
        message = 'No existen posiciones del pedido'.
      ELSE.
        LOOP AT i_vbap INTO l_vbap.
          l_request-document_numb      = l_vbap-vbeln.
          l_request-document_item      = l_vbap-posnr.
          l_request-quantity_sales_uom = l_vbap-zmeng.
          l_request-quantity_base__uom = l_vbap-zmeng.
          l_request-sales_unit         = l_vbap-vrkme.
          l_request-id                 = 1.
          l_request-document_type      = 'A'.
          l_request-delivery_date      = sy-datum.
          l_request-material           = l_vbap-matnr.
          l_request-plant              = l_vbap-werks.
          l_request-date               = sy-datum.
          l_request-goods_issue_date   = sy-datum.
          l_request-goods_issue_time   = sy-uzeit.
          APPEND l_request TO tbl_request.
        ENDLOOP.

        CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXEC'
          TABLES
            request      = tbl_request
            createditems = tbl_items
            return       = tbl_return.

        IF NOT log_exit IS INITIAL.
          SELECT SINGLE message FROM (zcl_c=>tabla_zlog)
            INTO l_msg
           WHERE proceso   = 'EXITS'
             AND clave     = pedido
             AND progname  = log_exit
             AND fecha     = l_fecha
             AND hora     >= l_hora.
          IF sy-subrc = 0.
            __concat2 message l_msg message.
          ENDIF.
        ENDIF.
        IF NOT l_msgc IS INITIAL.
          __concat_a message l_msgc.
        ENDIF.
        LOOP AT tbl_return INTO l_return WHERE type = 'E'.
          __concat_a message l_return-message.
        ENDLOOP.
        IF tbl_return IS INITIAL AND tbl_items IS INITIAL.
          message = 'No se ha creado entrega.'.
        ENDIF.

        LOOP AT tbl_items INTO l_items WHERE document_numb <> ''.
          vbeln = l_items-document_numb.
          l_vbsk-anzlp = l_vbsk-anzlp + 1.
        ENDLOOP.

        IF l_vbsk-anzlp < l_pos_ped.
          l_msg = zcl_ap_utils=>concat( p1 = 'Sólo se han creado' p2 = l_vbsk-anzlp p3 = 'pos.en entrega de las' p4 = l_pos_ped p5 = 'del pedido' ).
          __add_lista warning l_msg.
        ENDIF.

        IF NOT vbeln IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD crear_desde_vl10b.
    DATA: o_bi      TYPE REF TO zcl_ap_batch_input,
          l_fecha   TYPE dats,
          l_hora    TYPE sy-uzeit,
          l_vbsk    TYPE vbsk,
          l_vbfs    TYPE vbfs,
          l_msg     TYPE bapi_msg,
          l_msgc    TYPE bapi_msg,
          l_pos_ped TYPE i,
          i_ekpo    TYPE STANDARD TABLE OF ekpo.

    DATA: l_request   TYPE bapideliciousrequest,
          tbl_request TYPE TABLE OF bapideliciousrequest,
          tbl_items   TYPE TABLE OF bapideliciouscreateditems,
          tbl_return  TYPE TABLE OF bapiret2,
          l_return    TYPE bapiret2,
          l_items     TYPE bapideliciouscreateditems.

    IF bapi IS INITIAL OR modoct = 'A'.
      o_bi = NEW #( ).

      o_bi->inicio( ).
      o_bi->dynpro( program = 'RVV50R10C' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=S0S_TAB5' ).
      o_bi->campos( campo = 'ST_VSTEL-LOW' valor = '' ).
      IF dias = -1.
        o_bi->campos( campo = 'ST_LEDAT-HIGH' valor = '' ).
      ELSEIF dias > 0.
        l_fecha = sy-datum + dias.
        o_bi->campos( campo = 'ST_LEDAT-HIGH' valor = l_fecha ).
      ENDIF.
      o_bi->dynpro( program = 'RVV50R10C' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ONLI' ).
      o_bi->campos( campo = 'ST_EBELN-LOW' valor = ebeln ).
      o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=&ALL' ).
      o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=SAMD' ).
      o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=&F03' ).
      o_bi->dynpro( program = 'RVV50R10C' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/EE' ).

      GET TIME.
      l_fecha = sy-datum.
      l_hora  = sy-uzeit.

      message = o_bi->llamar_transaccion( tcode = 'VL10B' modo = modoct ).

      SELECT SINGLE sammg anzlp FROM vbsk               "#EC CI_NOFIELD
        INTO (l_vbsk-sammg, l_vbsk-anzlp)
       WHERE smart  = 'L'
         AND ernam  = sy-uname
         AND erdat  = l_fecha
         AND uzeit >= l_hora.
      IF sy-subrc = 0.
        SELECT * FROM vbfs
          INTO l_vbfs
         WHERE sammg = l_vbsk-sammg
           AND ( msgno <> '544' )
           AND msgid <> ''.
          MESSAGE ID l_vbfs-msgid TYPE 'S' NUMBER l_vbfs-msgno WITH l_vbfs-msgv1 l_vbfs-msgv2 l_vbfs-msgv3 l_vbfs-msgv4 INTO l_msg.
          __add_lista l_msgc l_msg.
        ENDSELECT.
      ENDIF.

      SELECT belnr FROM ekbe
        INTO vbeln
        UP TO 1 ROWS
       WHERE ebeln  = ebeln
         AND vgabe  = '8'
         AND bewtp  = 'L'
         AND cpudt  = l_fecha
         AND cputm >= l_hora
       ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        CLEAR message.
        warning = l_msgc.

        SELECT COUNT( * ) FROM ekpo ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
          INTO l_pos_ped
         WHERE ebeln = ebeln
           AND loekz = ''.

        IF l_vbsk-anzlp < l_pos_ped.
          l_msg = zcl_ap_utils=>concat( p1 = 'Sólo se han creado' p2 = l_vbsk-anzlp p3 = 'pos.en entrega de las' p4 = l_pos_ped p5 = 'del pedido' ).
          __add_lista warning l_msg.
        ENDIF.
      ELSE.
        IF message CS 'producido un error inesperado'.
          CLEAR message.
        ENDIF.
        IF NOT log_exit IS INITIAL.
          SELECT SINGLE message FROM (zcl_c=>tabla_zlog)
            INTO l_msg
           WHERE proceso   = 'EXITS'
             AND clave     = ebeln
             AND progname  = log_exit
             AND fecha     = l_fecha
             AND hora     >= l_hora.
          IF sy-subrc = 0.
            __concat2 message l_msg message.
          ENDIF.
        ENDIF.
        IF NOT l_msgc IS INITIAL.
          __concat_a message l_msgc.
        ENDIF.
        IF message IS INITIAL.
          message = 'No se ha podido crear la entrega'.
        ENDIF.
      ENDIF.
    ELSE.
      SELECT  ebeln ebelp matnr meins menge werks FROM ekpo ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO CORRESPONDING FIELDS OF TABLE i_ekpo
        WHERE ebeln = ebeln
          AND loekz = ''.
      IF sy-subrc <> 0.
        message = 'No existen posiciones del pedido'.
      ELSE.
        LOOP AT i_ekpo ASSIGNING FIELD-SYMBOL(<ekpo>).
          l_request-document_numb      = <ekpo>-ebeln.
          l_request-document_item      = <ekpo>-ebelp.
          l_request-plant              = <ekpo>-werks.
          l_request-quantity_sales_uom = <ekpo>-menge.
          l_request-base_uom           = <ekpo>-meins.
          l_request-material           = <ekpo>-matnr.
          l_request-delivery_date      = sy-datum.
          l_request-document_type      = 'B'. " Purchasing Ord
          APPEND l_request TO tbl_request.
          l_pos_ped = l_pos_ped + 1.
        ENDLOOP.

        CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXEC'
          TABLES
            request      = tbl_request
            createditems = tbl_items
            return       = tbl_return.

        IF NOT log_exit IS INITIAL.
          SELECT SINGLE message FROM (zcl_c=>tabla_zlog)
            INTO l_msg
           WHERE proceso   = 'EXITS'
             AND clave     = ebeln
             AND progname  = log_exit
             AND fecha     = l_fecha
             AND hora     >= l_hora.
          IF sy-subrc = 0.
            __concat2 message l_msg message.
          ENDIF.
        ENDIF.
        IF NOT l_msgc IS INITIAL.
          __concat_a message l_msgc.
        ENDIF.
        LOOP AT tbl_return INTO l_return WHERE type = 'E'.
          __concat_a message l_return-message.
        ENDLOOP.

        IF tbl_return IS INITIAL AND tbl_items IS INITIAL.
          message = 'No se ha creado entrega.'.
        ENDIF.

        LOOP AT tbl_items INTO l_items WHERE document_numb <> ''.
          vbeln = l_items-document_numb.
          l_vbsk-anzlp = l_vbsk-anzlp + 1.
        ENDLOOP.

        IF l_vbsk-anzlp < l_pos_ped.
          l_msg = zcl_ap_utils=>concat( p1 = 'Sólo se han creado' p2 = l_vbsk-anzlp p3 = 'pos.en entrega de las' p4 = l_pos_ped p5 = 'del pedido' ).
          __add_lista warning l_msg.
        ENDIF.

        IF NOT vbeln IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD crear_entrega_desde_pedido.
    DATA: l_vstel TYPE vstel,
          o_bi    TYPE REF TO zcl_ap_batch_input.

    SELECT SINGLE vstel FROM vbap ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO l_vstel
     WHERE vbeln  = pedido
       AND vstel <> ''
       AND abgru  = ''.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '4001' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'LIKP-VSTEL' valor = l_vstel ). " Pto.exped./depto.entrada mcía.
    o_bi->campos( campo = 'LV50C-DATBI' valor = fecha ). " Fecha de selección de entrega
    o_bi->campos( campo = 'LV50C-VBELN' valor = pedido ). " Pedido

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

    SET PARAMETER ID 'VL' FIELD ''.
    message = o_bi->llamar_transaccion( tcode = 'VL01N' modo = modoct ).

    GET PARAMETER ID 'VL' FIELD vbeln.
    IF vbeln IS INITIAL.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'Error creando entrega' p2 = message msgty = 'E' ).
      ENDIF.
    ELSE.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'Se ha entrega' p2 = vbeln p3 = 'desde el pedido' p4 = pedido msgty = 'S' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD desembalaje.
    " TODO: parameter CHARG is never used (ABAP cleaner)
    " TODO: parameter LFIMG is only used in commented-out code (ABAP cleaner)
    " TODO: parameter VRKME is never used (ABAP cleaner)

    DATA: l_vekp        TYPE vekp,
          l_vepo        TYPE vepo,
          l_lips        TYPE lips,
          l_exidv       TYPE exidv,
          o_bi          TYPE REF TO zcl_ap_batch_input,
          l_vbfa        TYPE vbfa,
          l_ctd_picking TYPE lipsd-pikmg.

    mensaje = validar_entrega_modificable( vbeln ).
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE venum FROM vekp
      INTO l_vekp-venum
     WHERE exidv = exidv.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe la SSCC' exidv INTO mensaje SEPARATED BY space.
      RETURN.
    ENDIF.

    SELECT SINGLE posnr FROM vepo
      INTO l_vepo-posnr
     WHERE venum  = l_vekp-venum
       AND vbeln  = vbeln
       AND posnr <> ''.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO l_lips
       WHERE vbeln = vbeln
         AND posnr = l_vepo-posnr.
    ENDIF.

    WRITE exidv TO l_exidv.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=VERP_T' ).

*  o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=ALL_INH').
*  o_bi->campos( campo = 'BDC_CURSOR' valor = 'V51VE-VHILM(01)' ).
*
**
*  o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=TRSE').
*  o_bi->campos( campo = 'BDC_CURSOR' valor = '02/03' ).
*
** LIST_DISPLAY: Buscar
*  o_bi->dynpro( program = 'SAPLSEUT' dynpro = '0750').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=TRSE').
*  o_bi->campos( campo = 'SEARCHFOR' valor = l_exidv ).
*  o_bi->campos( campo = 'RADIO_XPANDALL' valor = 'X').
*
**
*  o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=TRMK').
*
**
*  o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=HUENTF').
*
**
*  o_bi->dynpro( program = 'SAPMSSY0' dynpro = '0120').

    o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=HUSUCH' ).

    o_bi->dynpro( program = 'SAPLV51G' dynpro = '6001' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENTR' ).
    o_bi->campos( campo = 'VEKP-EXIDV' valor = l_exidv ).

    o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=HUENTF' ).
    o_bi->campos( campo = 'BDC_CURSOR' valor = 'V51VE-VHILM(01)' ).

    o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=HUENTF' ).

    IF l_lips IS INITIAL.
* Si la HU no tiene asignada ninguna posición de la entrega, grabamos
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).
    ELSE.
* Si no borramos la posición de la entrega
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK' ).

* Pulsamos para seleccionar posición
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

* Entrega                Ventana   Posicionar    Posición
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
      o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-uecha ). " Número de posición del documento comercial

* Marcamos la primera fila y puslsamos sobre selección de lotes
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHSP_T' ).
      o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).
      o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de selección en dynpros de lista

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
      o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-posnr ).

* Borramos posición
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).
      o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de selección en dynpros de lista
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=POLO_T' ).

* Volvemos a la pantalla inicial (picking)
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK_T' ).

* Nos aseguramos que estamos en la pestaÃ±a de picking
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=T\02' ).

* Seleccionamos la posicion
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
      o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-uecha ).

** A la cantidad de picking existente, le restamos la que acabamos de borrar
*    SELECT SINGLE lfimg FROM lips
*      INTO l_ctd_entrega
*     WHERE vbeln = vbeln
*       AND posnr = l_lips-uecha.
*
*    CLEAR l_ctd_picking.
*    DATA i_lips TYPE TABLE OF lips.
*
*    SELECT * FROM lips
*      INTO TABLE i_lips
*     WHERE vbeln = vbeln
*       AND posnr NE l_lips-posnr
*       AND uecha = l_lips-uecha.
*    IF sy-subrc = 0.
*      LOOP AT i_lips INTO l_lips2.
*        SELECT rfmng_flo FROM vbfa
*          INTO l_vbfa-rfmng_flo
*          WHERE vbelv = vbeln
*            AND posnv = l_lips2-posnr
*            AND vbtyp_n = 'Q'.
*          ADD l_vbfa-rfmng_flo TO l_ctd_picking.
*        ENDSELECT.
*      ENDLOOP.
*    ELSE.
      SELECT SUM( rfmng_flo )  FROM vbfa
        INTO l_vbfa-rfmng_flo
       WHERE vbelv    = vbeln
         AND posnv    = l_lips-uecha
         AND vbeln    = vbeln
         AND posnn   <> l_lips-posnr
         AND vbtyp_n  = 'Q'.
      l_ctd_picking = l_vbfa-rfmng_flo.
*    ENDIF.
*    IF l_ctd_entrega > 0.
*      IF l_ctd_picking > l_ctd_entrega.
*        l_ctd_picking = l_ctd_entrega.
*      ENDIF.
*    ENDIF.

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).
      o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = l_ctd_picking ).
    ENDIF.

    mensaje = o_bi->llamar_transaccion( tcode = 'VL02N' modo = modoct ).

    IF mensaje CS 'grabado'.
      CLEAR mensaje.
    ENDIF.
  ENDMETHOD.
  METHOD desembalar_entrega.
    DATA o_bi TYPE REF TO zcl_ap_batch_input.

    mensaje = validar_entrega_modificable( vbeln ).
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE vbtyp FROM likp
      INTO @DATA(l_vbtyp)
     WHERE vbeln = @vbeln.
    IF l_vbtyp = '7'.
      DATA(l_tcode) = 'VL32N'.
      DATA(l_dynnr) = '4104'.
    ELSE.
      l_tcode = 'VL02N'.
      l_dynnr = '4004'.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMV50A' dynpro = CONV #(  l_dynnr ) ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=VERP_T' ).

    o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=HUMARKHU' ).

    o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=HUENTF' ).

    o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).

    mensaje = o_bi->llamar_transaccion( tcode = CONV #( l_tcode ) modo = modoct ).
  ENDMETHOD.
  METHOD deshacer_picking.
    DATA: o_bi    TYPE REF TO zcl_ap_batch_input,
          l_lips  TYPE lips,
          i_lips2 TYPE TABLE OF lips.

    mensaje = validar_entrega_modificable( vbeln ).
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    o_bi = NEW #( ).

    SELECT SINGLE vbtyp FROM likp
      INTO @DATA(l_vbtyp)
     WHERE vbeln = @vbeln.
    IF l_vbtyp = '7'.
      DATA(l_tcode) = 'VL32N'.
      DATA(l_dynnr) = '4104'.
    ELSE.
      l_tcode = 'VL02N'.
      l_dynnr = '4004'.
    ENDIF.

    IF NOT o_log IS INITIAL.
      o_log->log( p1 = 'Deshaciendo picking' msgty = 'I' ).
    ENDIF.

    IF posnr IS INITIAL.
      SELECT posnr FROM lips
        INTO CORRESPONDING FIELDS OF l_lips
        UP TO 1 ROWS
       WHERE vbeln  = vbeln
         AND uecha <> ''.
      ENDSELECT.
      IF sy-subrc <> 0.
        RETURN. " No hay partición de picking.
      ENDIF.

      SELECT posnr FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO CORRESPONDING FIELDS OF TABLE i_lips2
       WHERE vbeln = vbeln
         AND uecha = ''
         AND xchpf = 'X'
        ORDER BY vbeln posnr.

    ELSE.
      SELECT posnr FROM lips
        INTO CORRESPONDING FIELDS OF l_lips
        UP TO 1 ROWS
       WHERE vbeln = vbeln
         AND uecha = posnr.
      ENDSELECT.
      IF sy-subrc <> 0.
        RETURN. " No hay partición de picking.
      ENDIF.

      SELECT posnr FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO CORRESPONDING FIELDS OF TABLE i_lips2
       WHERE vbeln = vbeln
         AND posnr = posnr
        ORDER BY vbeln posnr.
    ENDIF.

    LOOP AT i_lips2 INTO l_lips.
      AT FIRST.
        o_bi->inicio( ).
* Pantalla selcción de entrega
        o_bi->dynpro( program = 'SAPMV50A' dynpro = CONV #( l_dynnr ) ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
        o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega
      ENDAT.

* Pulsamos para seleccionar posición
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

* Entrega                Ventana   Posicionar    Posición
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
      o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-posnr ). " Número de posición del documento comercial

* Marcamos la primera fila y puslsamos sobre selección de lotes
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHSP_T' ).
      o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).
      o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de selección en dynpros de lista

* Marcamos todo
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=MKAL_T' ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=POLO_T' ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK_T' ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      IF mod_ctd_picking = 'X'.
        o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = '' ).
      ENDIF.
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).

      AT LAST.
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

        mensaje = o_bi->llamar_transaccion( tcode = CONV #( l_tcode ) modo = modoct ).

        IF NOT o_log IS INITIAL.
          o_log->log( p1 = mensaje msgty = o_bi->msgty ).
        ENDIF.

        IF o_bi->msgty = 'S' OR mensaje CS 'grabado'.
          CLEAR mensaje.
        ENDIF.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.
  METHOD efectuar_sm.
    DATA: l_tabla TYPE tabname VALUE 'VBUK',
          l_wbstk TYPE vbuk-wbstk,
          o_bi    TYPE REF TO zcl_ap_batch_input.

    IF zcl_c=>hana = 'X'.
      l_tabla = 'LIKP'.
    ENDIF.

    CLEAR message.

    SELECT SINGLE wbstk FROM (l_tabla)
      INTO l_wbstk
     WHERE vbeln = vbeln.
    IF sy-subrc <> 0.
      __concat2 message 'No existe la entrega' vbeln.
    ELSE.
      IF l_wbstk = 'C'.
        IF NOT o_log IS INITIAL.
          o_log->log( p1 = 'Entrega' p2 = vbeln p3 = 'ya tenía efectuada SM' msgty = 'I' ).
        ENDIF.
      ELSE.
        message = espera_si_bloqueada( vbeln = vbeln segundos_espera = segundos_espera_si_bloqueos ).

        IF message IS INITIAL.
          o_bi = NEW #( ).

          o_bi->inicio( ).

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=WABU_T' ).
          o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

          message = o_bi->llamar_transaccion( tcode = 'VL02N' modo = modoct ).

          espera_si_bloqueada( vbeln = vbeln segundos_espera = segundos_espera_si_bloqueos ).

          CLEAR l_wbstk.
          SELECT SINGLE wbstk FROM (l_tabla)
            INTO l_wbstk
           WHERE vbeln = vbeln.
          IF l_wbstk = 'C'.
            IF NOT o_log IS INITIAL.
              o_log->log( p1 = 'Se ha modificado SM de la entrega' p2 = vbeln msgty = 'S' ).
            ENDIF.
            CLEAR message.
          ELSE.
            IF NOT o_log IS INITIAL.
              o_log->log( p1 = 'Error efectuando  SM de la entrega' p2 = vbeln p3 = message msgty = 'E' ).
            ENDIF.
          ENDIF.
        ELSE.
          IF NOT o_log IS INITIAL.
            o_log->log( p1 = message msgty = 'E' ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD embalar.
    DATA: o_bi    TYPE REF TO zcl_ap_batch_input,
          l_vhilm TYPE vekp-vhilm,
          l_cont  TYPE i.

    FIELD-SYMBOLS <hu> TYPE t_embalaje.

    IF i_pos IS INITIAL.
      message = 'No ha informado SSCCs para embalar'.
      RETURN.
    ENDIF.

    SELECT SINGLE vbtyp FROM likp
      INTO @DATA(l_vbtyp)
     WHERE vbeln = @vbeln.
    IF l_vbtyp = '7'.
      DATA(l_tcode) = 'VL32N'.
      DATA(l_dynnr) = '4104'.
    ELSE.
      l_tcode = 'VL02N'.
      l_dynnr = '4004'.
    ENDIF.

    message = validar_entrega_modificable( vbeln ).
    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(i_pos2) = i_pos.
    LOOP AT i_pos2 ASSIGNING <hu>.
      SELECT SINGLE venum FROM vekp
        INTO @DATA(l_venum)
       WHERE exidv = @<hu>-exidv
         AND ( vbeln_gen = @vbeln OR vpobjkey = @vbeln ).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      SELECT SINGLE venum FROM vepo
        INTO l_venum
       WHERE venum = l_venum.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      SELECT SINGLE venum FROM vepo
        INTO l_venum
       WHERE venum = l_venum
         AND matnr = <hu>-matnr
         AND charg = <hu>-charg
         AND vemng = <hu>-lfimg
         AND vemeh = <hu>-vrkme.
      IF sy-subrc = 0.
        DELETE i_pos2.
      ELSE.
        message = |SSCC { <hu>-exidv ALPHA = OUT } ya asignada a entrega, pero no cuadran datos de embalaje|.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF i_pos2 IS INITIAL.
* Todo embalado correctamente.
      RETURN.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMV50A' dynpro = CONV #( l_dynnr ) ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=VERP_T' ). " Embalaje

    o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=UE6VDIR' ). " Entrada individual

    LOOP AT i_pos ASSIGNING <hu>.
      o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENTR' ).
      o_bi->campos( campo = 'VEKP-EXIDV' valor = <hu>-exidv ). " Identificación externa de la unidad de manipulación

      CLEAR l_vhilm.
      SELECT COUNT( * ) FROM vekp
        INTO l_cont
       WHERE exidv = <hu>-exidv.
      IF l_cont = 1.
        SELECT SINGLE venum vhilm FROM vekp
          INTO (l_venum, l_vhilm)
         WHERE exidv = <hu>-exidv.
        SELECT SINGLE venum FROM vepo
          INTO l_venum
         WHERE venum = l_venum.
        IF sy-subrc <> 0.
          CLEAR l_vhilm.
        ENDIF.
      ENDIF.
      IF l_vhilm <> <hu>-vhilm.
        o_bi->campos( campo = 'VEKP-VHILM' valor = <hu>-vhilm ). " Material de embalaje
      ENDIF.
      o_bi->campos( campo = 'HUMV4-MATNR' valor = <hu>-matnr ). " Número de material
      o_bi->campos( campo = 'HUMV4-CHARG' valor = <hu>-charg ). " Número de lote
      o_bi->campos( campo = 'HUMV4-QUANTITY' valor = <hu>-lfimg ). " Cantidad base embalada en posición de unidad manipulación
      o_bi->campos( campo = 'HUMV4-VRKME' valor = <hu>-vrkme ). " Unidad de medida alternativa p.unidad de medida de stock
      o_bi->campos( campo = 'HUMV4-POSNR' valor = <hu>-posnr ).
    ENDLOOP.

    o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).

    message = o_bi->llamar_transaccion( tcode = CONV #( l_tcode ) modo = modoct ).

    IF o_bi->msgty = 'S'.
      CLEAR message.
      LOOP AT o_bi->i_mensajes ASSIGNING FIELD-SYMBOL(<message>) WHERE    ( msgnr = '150' OR msgnr = '157' AND msgid = 'HUDIALOG' )
                                                                       OR msgid = 'HUGENERAL'.
        MESSAGE ID <message>-msgid TYPE <message>-msgtyp NUMBER <message>-msgnr WITH
                <message>-msgv1 <message>-msgv2 <message>-msgv3 <message>-msgv4 INTO DATA(l_msg).
        IF message IS INITIAL.
          message = l_msg.
        ELSE.
          message = |{ message }, { l_msg }|.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF message IS INITIAL.
* Verificamos que todo esté ok.
      LOOP AT i_pos ASSIGNING <hu>.
        SELECT SINGLE venum FROM vekp
          INTO @l_venum
         WHERE exidv = @<hu>-exidv
           AND ( vbeln_gen = @vbeln OR vpobjkey = @vbeln ).
        IF sy-subrc <> 0.
          message = |La SSCC { <hu>-exidv ALPHA = OUT } no está asignada a la entrega. Revise|.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT o_log IS INITIAL.
      IF message IS INITIAL.
        o_log->log( p1 = 'Se ha realizado correctamente el embalaje de entrega' p2 = vbeln msgty = 'S' ).
      ELSE.
        o_log->log( p1 = message ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD espera_si_bloqueada.
    DATA bloqueada TYPE c LENGTH 1.

    DO segundos_espera TIMES.
      bloqueada = esta_bloqueada( vbeln ).
      IF bloqueada = 'X'.
        __concat4 message 'Entrega' vbeln 'bloqueada por' sy-msgv1.
        WAIT UP TO tiempo_espera SECONDS.
      ELSE.
        CLEAR message.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD esta_bloqueada.
    bloqueada = zcl_ap_utils=>comprobar_bloqueo( tabla  = 'LIKP'
                                                 clave  = sy-mandt
                                                 clave2 = vbeln ).
  ENDMETHOD.
  METHOD get_ctd_entrega.
    DATA: l_lips TYPE lips,
          i_lips TYPE TABLE OF lips.

    IF posnr IS INITIAL.
      SELECT matnr SUM( lfimg ) meins FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO (l_lips-matnr, l_lips-lfimg, l_lips-meins)
       WHERE vbeln = vbeln
      GROUP BY matnr meins.
        APPEND l_lips TO i_lips.
      ENDSELECT.
    ELSE.
      SELECT matnr SUM( lfimg ) meins FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO (l_lips-matnr, l_lips-lfimg, l_lips-meins)
       WHERE vbeln = vbeln
         AND ( posnr = posnr OR uecha = posnr )
      GROUP BY matnr meins.
        APPEND l_lips TO i_lips.
      ENDSELECT.
    ENDIF.

    IF NOT unidad IS INITIAL.
      LOOP AT i_lips INTO l_lips.
        IF unidad <> l_lips-meins.
          l_lips-lfimg = zcl_ap_material=>convertir_unidad( matnr          = l_lips-matnr
                                                            unidad_origen  = l_lips-meins
                                                            unidad_destino = unidad
                                                            cantidad       = l_lips-lfimg ).
        ENDIF.
        cantidad = cantidad + l_lips-lfimg.
      ENDLOOP.
    ELSE.
      LOOP AT i_lips INTO l_lips.
        cantidad = cantidad + l_lips-lfimg.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_ctd_picking.
    DATA: l_tabla TYPE tabname VALUE 'VBUP',
          l_kosta TYPE vbup-kosta,
          l_ctd   TYPE pikmg.

    IF zcl_c=>hana = 'X'.
      l_tabla = 'LIPS'.
    ENDIF.

    SELECT SINGLE kosta FROM (l_tabla)
      INTO l_kosta
     WHERE vbeln = vbeln
       AND posnr = posnr.
    IF l_kosta IS INITIAL.
      CLEAR pikmg.
    ELSEIF l_kosta = 'C' AND buffer <> 'N'.
      SELECT SINGLE lfimg FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO pikmg
     WHERE vbeln = vbeln
       AND posnr = posnr.
    ELSE.
      IF select = 'X'.
        SELECT SUM( rfmng ) FROM vbfa
          INTO pikmg
         WHERE vbelv   = vbeln
           AND posnv   = posnr
           AND vbtyp_n = 'Q'.
      ELSE.
        IF buffer IS INITIAL OR buffer = 'N'.
          CALL FUNCTION 'WB2_EXP_READ_DOCFLOW_REFRESH'.
        ENDIF.

        CALL FUNCTION 'WB2_GET_PICK_QUANTITY'
          EXPORTING
            i_vbeln             = vbeln
            i_posnr             = posnr
*           I_MODE              = ' '
          IMPORTING
            e_pikmg             = pikmg
          EXCEPTIONS
            document_read_error = 1
            OTHERS              = 2.
      ENDIF.
      IF sy-subrc <> 0.
        IF zcl_c=>hana IS INITIAL.
          SELECT lfimg FROM lips JOIN vbup ON  lips~vbeln = vbup~vbeln ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
                                           AND lips~posnr = vbup~posnr
             INTO pikmg
            UP TO 1 ROWS
            WHERE lips~vbeln = vbeln
              AND lips~posnr = posnr
              AND vbup~kosta = 'C'
            ORDER BY lips~vbeln lips~posnr.
          ENDSELECT.
        ELSE.
          SELECT lfimg FROM (l_tabla)
             INTO pikmg
            UP TO 1 ROWS
            WHERE vbeln = vbeln
              AND posnr = posnr
              AND kosta = 'C'
            ORDER BY vbeln posnr.
          ENDSELECT.
        ENDIF.
      ENDIF.
    ENDIF.

    IF add_subpos = 'X'.
      IF zcl_c=>hana IS INITIAL.
        SELECT SUM( lfimg ) FROM lips JOIN vbup ON  lips~vbeln = vbup~vbeln ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
                                                AND lips~posnr = vbup~posnr
           INTO l_ctd
          WHERE lips~vbeln = vbeln
            AND uecha      = posnr
            AND vbup~kosta = 'C'.
      ELSE.
        SELECT SUM( lfimg ) FROM (l_tabla)
           INTO l_ctd
          WHERE vbeln = vbeln
            AND uecha = posnr
            AND kosta = 'C'.
      ENDIF.

      pikmg = pikmg + l_ctd.
    ENDIF.
  ENDMETHOD.
  METHOD get_ctd_pos.
    IF add_subpos = 'X'.
      SELECT SUM( lfimg ) FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO lfimg
       WHERE vbeln = vbeln
         AND ( posnr = posnr OR uecha = posnr ).
    ELSE.
      SELECT SUM( lfimg ) FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO lfimg
       WHERE vbeln = vbeln
         AND posnr = posnr.
    ENDIF.
  ENDMETHOD.
  METHOD get_ctd_sm.
    TYPES: BEGIN OF t_lips,
             matnr TYPE matnr,
             lfimg TYPE mengv13,
             meins TYPE meins,
             bwart TYPE bwart,
           END OF t_lips.

    DATA: l_lips TYPE t_lips,
          i_lips TYPE TABLE OF t_lips.

    IF posnr IS INITIAL.
      SELECT matnr SUM( rfmng ) meins bwart FROM vbfa
        INTO (l_lips-matnr, l_lips-lfimg, l_lips-meins, l_lips-bwart)
       WHERE vbelv  = vbeln
         AND bwart IN ( '601', '602' )
      GROUP BY matnr meins bwart.
        IF l_lips-bwart = '602'.
          l_lips-lfimg = - l_lips-lfimg.
        ENDIF.
        APPEND l_lips TO i_lips.
      ENDSELECT.
    ELSE.
      SELECT matnr SUM( rfmng ) meins bwart FROM vbfa
        INTO (l_lips-matnr, l_lips-lfimg, l_lips-meins, l_lips-bwart)
       WHERE vbelv  = vbeln
         AND posnv  = posnr
         AND bwart IN ( '601', '602' )
      GROUP BY matnr meins bwart.
        IF l_lips-bwart = '602'.
          l_lips-lfimg = - l_lips-lfimg.
        ENDIF.
        APPEND l_lips TO i_lips.
      ENDSELECT.
    ENDIF.

    IF NOT unidad IS INITIAL.
      LOOP AT i_lips INTO l_lips.
        IF unidad <> l_lips-meins.
          l_lips-lfimg = zcl_ap_material=>convertir_unidad( matnr          = l_lips-matnr
                                                            unidad_origen  = l_lips-meins
                                                            unidad_destino = unidad
                                                            cantidad       = l_lips-lfimg ).
        ENDIF.
        cantidad = cantidad + l_lips-lfimg.
      ENDLOOP.
    ELSE.
      LOOP AT i_lips INTO l_lips.
        cantidad = cantidad + l_lips-lfimg.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_direccion.
    DATA: l_kunnr  TYPE kunnr,
          ls_kupav TYPE kupav,
          lf_txtpa TYPE ad_line_s.

    IF NOT kunnr IS INITIAL.
      l_kunnr = kunnr.
    ELSE.
      SELECT SINGLE kunnr FROM likp
        INTO l_kunnr
       WHERE vbeln = vbeln.
    ENDIF.

    SELECT SINGLE adrnr FROM kna1
      INTO ls_kupav-adrnr
     WHERE kunnr = l_kunnr.

    IF ampliada IS INITIAL.
      ls_kupav-address_type = '1'.

      CALL FUNCTION 'SD_ADDRESS_BUILD'
        EXPORTING
          i_kupav = ls_kupav
        IMPORTING
          e_txtpa = lf_txtpa.
*    tables
*      xvbadr  = ct_xvbadr.

      direccion = lf_txtpa.
    ELSE.
      direccion = zcl_ap_direcciones=>get_direccion_stringf( adrnr = ls_kupav-adrnr ).
    ENDIF.
  ENDMETHOD.
  METHOD get_factura.
    CLEAR: factura, facturas.

    SELECT vbfa~vbeln, vbfa~erdat, SUM( rfmng_flt ) AS ctd FROM vbfa JOIN vbrk ON vbfa~vbeln = vbrk~vbeln
      INTO TABLE @DATA(i_facturas)
     WHERE vbelv   = @vbeln
       AND vbtyp_n = 'M' " Factura
       AND stufe   = 0
       AND fksto   = ''  " No anulada
     GROUP BY vbfa~vbeln, vbfa~erdat.

    SORT i_facturas BY ctd DESCENDING erdat DESCENDING.
    LOOP AT i_facturas ASSIGNING FIELD-SYMBOL(<fact>).
      IF factura IS INITIAL.
        factura = <fact>-vbeln.
      ENDIF.
      __add_lista facturas <fact>-vbeln.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_fecha_hora_inicio.
    DATA: l_handle     TYPE likp-handle,
          l_even_tstfr TYPE tsege-even_tstfr,
          l_even_zonfr TYPE tsege-even_zonfr.

    IF handle IS INITIAL.
      SELECT SINGLE handle FROM likp
        INTO l_handle
       WHERE vbeln = vbeln.
    ELSE.
      l_handle = handle.
    ENDIF.

    IF NOT l_handle IS INITIAL.
      SELECT SINGLE even_tstfr even_zonfr FROM tsege
        INTO (l_even_tstfr, l_even_zonfr)
       WHERE head_hdl   = l_handle
         AND even       = tipo_fecha
         AND even_verty = tipo.
      IF sy-subrc = 0 AND NOT l_even_tstfr IS INITIAL.
        zcl_ap_fechas=>timestamp_2_fechahora( EXPORTING timestamp = l_even_tstfr
                                                        tzone     = l_even_zonfr
                                              IMPORTING fecha     = fecha
                                                        hora      = hora ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_lote_origen_pedido.
    SELECT SINGLE charg, vgbel, vgpos FROM lips  ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO (@DATA(l_lote_entrega), @DATA(l_vgbel), @DATA(l_vgpos))
     WHERE vbeln = @vbeln
       AND posnr = @posnr.
    IF NOT l_lote_entrega IS INITIAL.
      IF NOT l_vgbel IS INITIAL.
        SELECT SINGLE charg FROM vbap  ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
          INTO @DATA(l_lote_pedido)
         WHERE vbeln = @l_vgbel
           AND posnr = @l_vgpos.
        IF l_lote_pedido = l_lote_entrega.
          charg = l_lote_pedido.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_part_lote_sin_hu.
    DATA: i_lipsp TYPE TABLE OF lips,
          l_lips  TYPE lips,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_vepo  TYPE vepo.

    FIELD-SYMBOLS <lips> TYPE lips.

    CLEAR i_lips.

    SELECT posnr FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO CORRESPONDING FIELDS OF TABLE i_lipsp
     WHERE vbeln = vbeln
       AND uecha = ''.

    LOOP AT i_lipsp ASSIGNING <lips>.
      SELECT * FROM lips                             "#EC CI_SEL_NESTED
        INTO l_lips                                  ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
       WHERE vbeln = vbeln
         AND uecha = <lips>-posnr.

* Verificamos si ya está asignada a entrega
        SELECT SINGLE * FROM vepo JOIN vekp ON vepo~venum = vekp~venum "#EC CI_SEL_NESTED
          INTO CORRESPONDING FIELDS OF l_vepo
         WHERE vbeln     = vbeln
           AND posnr     = l_lips-posnr
           AND vpobj    IN ( '01', '03' )
           AND vpobjkey  = vbeln.
* Si no encuentro equivalencia en HU es que esa particición no tiene HU asignada
        IF sy-subrc <> 0.
          APPEND l_lips TO i_lips.
        ENDIF.
      ENDSELECT.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_pedido.
    SELECT vgbel FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1] "#EC CI_EXIT_SELECT
      INTO pedido
     WHERE vbeln  = vbeln
       AND vgbel <> ''.
      SELECT SINGLE vbeln FROM vbak
        INTO pedido
       WHERE vbeln = pedido.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_solicitantep.
    DATA l_pedido TYPE vbeln_va.

    l_pedido = get_pedido( vbeln ).
    IF NOT l_pedido IS INITIAL.
      SELECT SINGLE kunnr FROM vbak
        INTO kunnr
       WHERE vbeln = l_pedido.
    ENDIF.
  ENDMETHOD.
  METHOD get_texto_string.
    IF posnr IS INITIAL.
      string = zcl_ap_textos=>get_texto_string( id = id object = 'VBBK' name = vbeln spras = spras ).
    ENDIF.
  ENDMETHOD.
  METHOD get_transporte.
    SELECT tknum FROM vttp
      INTO tknum
      UP TO 1 ROWS
     WHERE vbeln = vbeln
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_url_por_titulo_st.
    DATA: l_clave  TYPE srgbtbrel-instid_a,
          l_titulo TYPE string.

    l_clave = vbeln.
    l_titulo = titulo.
    url = zcl_ap_gos=>get_url_por_titulo_st( tipo   = c_tipo_gos
                                             clave  = l_clave
                                             titulo = l_titulo ).
  ENDMETHOD.
  METHOD hay_mod.
    change_ind = zcl_ap_control_cambios=>hay_mod(
                     objectclas = c_objectclas
                     objectid   = vbeln
                     username   = username
                     tcode      = tcode
                     segundos   = segundos ).
  ENDMETHOD.
  METHOD insert_vbfa.
    DATA: l_vbfa TYPE vbfavb,
          i_vbfa TYPE TABLE OF vbfavb.

    SELECT SINGLE vbeln vbtyp FROM likp
      INTO (l_vbfa-vbelv, l_vbfa-vbtyp_v)
     WHERE vbeln = vbeln.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    SELECT SINGLE posnr matnr FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO (l_vbfa-posnv, l_vbfa-matnr)
     WHERE vbeln = vbeln
       AND posnr = posnr.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    l_vbfa-vbeln   = vbeln_n.
    l_vbfa-posnn   = posnr_n.
    l_vbfa-mjahr   = mjahr.
    l_vbfa-vbtyp_n = vbtyp_n.

    CASE vbtyp_n.
      WHEN 'R'.
        SELECT SINGLE menge meins bwart FROM mseg
          INTO (l_vbfa-rfmng, l_vbfa-meins, l_vbfa-bwart)
         WHERE mblnr = vbeln_n
           AND mjahr = mjahr
           AND zeile = posnr_n.
    ENDCASE.
    l_vbfa-rfmng_flt = l_vbfa-rfmng.
    l_vbfa-rfmng_flo = l_vbfa-rfmng_flt.
    l_vbfa-vrkme     = l_vbfa-meins.
    l_vbfa-updkz     = 'I'.
    l_vbfa-erdat     = sy-datum.
    l_vbfa-erzet     = sy-uzeit.
    APPEND l_vbfa TO i_vbfa.

    CALL FUNCTION 'RV_DOCUMENT_FLOW_ADD'
      EXPORTING
        f_struktur = 'VBFA'
        f_vbeln    = vbeln
      TABLES
        fxtabl     = i_vbfa
        fxvbfa     = i_vbfa.
  ENDMETHOD.
  METHOD insertar_nueva_posicion ##NEEDED.
*    DATA: ls_vbkok         TYPE vbkok,
*          ls_delivery_head TYPE likpvb,
*          lt_new_items     TYPE TABLE OF /spe/add_delivery_item,
*          ls_new_items     TYPE /spe/add_delivery_item,
*          ls_return        TYPE bapiret2,
*          is_vbkok         TYPE vbkok,
*          lv_posnr         TYPE lips-posnr,
*          lv_meins         TYPE meins.
*
*
*    ls_delivery_head-vbeln = lips-vbeln.
*    ls_delivery_head-spe_le_scenario = 'S'. " STO (Stock Transfer Order)
*    " Get last item number in delivery
*    SELECT SINGLE MAX( posnr )
*      FROM lips
*      INTO lv_posnr
*      WHERE vbeln = lips-vbeln
*        AND posnr < '900000'.
*    " Set new item number in delivery
*    ADD 10 TO lv_posnr.
*    " get uom for material
*    SELECT SINGLE meins
*      FROM mara
*      INTO lv_meins
*      WHERE matnr = lips-matnr.
*    " Set new item data
**    ls_new_items-rfpos  = lv_posnr.
*    ls_new_items-matnr  = lips-matnr.
*    ls_new_items-pstyv  = lips-pstyv.
*    ls_new_items-lfimg  = lips-lfimg.
*    ls_new_items-vrkme  = lips-vrkme.
*    ls_new_items-werks  = lips-werks.
*    ls_new_items-lgort  = lips-lgort.
*    ls_new_items-vbeln  = lips-vbeln.
*    ls_new_items-posnr  = lv_posnr.
*    ls_new_items-meins  = lv_meins.
*    APPEND ls_new_items TO lt_new_items.
*    " Set new item to delivey
*    CALL FUNCTION '/SPE/OUTB_DLV_CHG_ITEMS_INSERT'
*      EXPORTING
*        if_delivery_number = lips-vbeln
*        is_vbkok           = ls_vbkok
*        is_delivery_head   = ls_delivery_head
*      TABLES
*        it_new_items       = lt_new_items
*        et_return          = i_return.
*
*    READ TABLE i_return WITH KEY type = 'E' INTO ls_return.
*    IF sy-subrc NE 0.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*    ENDIF.
  ENDMETHOD.
  METHOD insertar_nueva_posicion_ct.
    DATA: l_vbtyp     TYPE likp-vbtyp,
          l_tcode     TYPE sy-tcode,
          l_dynnr     TYPE bdcdata-dynpro,
          l_posnr_max TYPE lips-posnr,
          o_bi        TYPE REF TO zcl_ap_batch_input,
          l_vbap      TYPE vbap,
          l_vbep      TYPE vbep.

    SELECT SINGLE vbtyp FROM likp
      INTO l_vbtyp
     WHERE vbeln = lips-vbeln.
    IF l_vbtyp = '7'.
      l_tcode = 'VL32N'.
      l_dynnr = '4104'.
    ELSE.
      l_tcode = 'VL02N'.
      l_dynnr = '4004'.
    ENDIF.

    SELECT MAX( posnr ) FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO l_posnr_max
     WHERE vbeln = lips-vbeln
       AND posnr < '900000'.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    IF NOT lips-vgbel IS INITIAL AND NOT lips-vgpos IS INITIAL.
      SELECT SINGLE * FROM vbap ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        INTO l_vbap
       WHERE vbeln = lips-vgbel
         AND posnr = lips-vgpos.
    ENDIF.

    o_bi->dynpro( program = 'SAPMV50A' dynpro = l_dynnr ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'LIKP-VBELN' valor = lips-vbeln ). " Entrega

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
    IF NOT l_vbap IS INITIAL.
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=RAUF_T' ).
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '0105' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENT1' ).

      SELECT SINGLE edatu FROM vbep
        INTO l_vbep-edatu
       WHERE vbeln = l_vbap-vbeln
         AND posnr = l_vbap-posnr
         AND bmeng > 0
         AND edatu > sy-datum.
      IF sy-subrc = 0.
        o_bi->campos( campo = 'LV50C-DATBI' valor = l_vbep-edatu ). " Pedido
      ENDIF.

      o_bi->campos( campo = 'LV50C-VBELN' valor = l_vbap-vbeln ). " Pedido
      o_bi->campos( campo = 'LV50C-ABPOS' valor = l_vbap-posnr ). " Desde posición
      o_bi->campos( campo = 'LV50C-BIPOS' valor = l_vbap-posnr ). " Desde posición
    ELSE.
* AÃ±adimos una nueva posición a la entrega
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=POAN_T' ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'LIPS-MATNR(02)' valor = lips-matnr ).
      o_bi->campos( campo = 'LIPSD-G_LFIMG(02)' valor = lips-lfimg ).
      o_bi->campos( campo = 'LIPS-VRKME(02)' valor = lips-vrkme ).
      IF NOT lips-werks IS INITIAL.
        o_bi->campos( campo = 'LIPS-WERKS(02)' valor = lips-werks ).
      ENDIF.
      IF NOT lips-lgort IS INITIAL.
        o_bi->campos( campo = 'LIPS-LGORT(02)' valor = lips-lgort ).
      ENDIF.

    ENDIF.

    o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

    message = espera_si_bloqueada( vbeln = lips-vbeln ). " segundos_espera = segundos_espera_si_bloqueos ).

    message = o_bi->llamar_transaccion( tcode = l_tcode modo = modo_bi ).

    SELECT MAX( posnr ) FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO posnr
     WHERE vbeln = lips-vbeln
       AND posnr < '900000'.
    IF posnr = l_posnr_max.
      CLEAR posnr.
    ELSE.
      CLEAR message.
    ENDIF.
  ENDMETHOD.
  METHOD insertar_url_gos_st.
    DATA: l_clave  TYPE srgbtbrel-instid_a,
          l_titulo TYPE string,
          l_url    TYPE string.

    l_clave = vbeln.
    l_titulo = titulo.
    l_url = url.
    zcl_ap_gos=>insertar_url_gos_st( tipo   = c_tipo_gos
                                     clave  = l_clave
                                     titulo = l_titulo
                                     url    = l_url ).
  ENDMETHOD.
  METHOD modificar.
    DATA: lips           TYPE lips,
          return         TYPE bapiret2,
          header_control TYPE bapiobdlvhdrctrlchg,
          header_data    TYPE bapiobdlvhdrchg,
          item_data      TYPE bapiobdlvitemchg,
          item_control   TYPE bapiobdlvitemctrlchg,
          l_lips         TYPE lips,
          i_item_data    TYPE TABLE OF bapiobdlvitemchg,
          i_item_control TYPE TABLE OF bapiobdlvitemctrlchg.

* Primero intentamos insertar, de forma que si falla no hamos el resto!
    LOOP AT i_lips_new INTO lips.
      insertar_nueva_posicion_ct( EXPORTING lips = lips modo_bi = modo_bi IMPORTING message = message ).

      IF NOT message IS INITIAL.
        return-type    = 'E'.
        return-message = message.
        APPEND return TO i_return.
      ENDIF.
    ENDLOOP.

    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.

    header_control-deliv_numb = vbeln.
    header_data-deliv_numb = header_control-deliv_numb.

    LOOP AT i_lips_mod INTO lips.
      CLEAR: item_data, item_control.

      item_data-deliv_numb = header_data-deliv_numb.
      item_data-deliv_item = lips-posnr.
      item_data-dlv_qty    = lips-lfimg.
      item_data-batch      = lips-charg.

      IF mod_pesos = 'X'.
        item_data-gross_wt   = lips-brgew.
        item_data-net_weight = lips-ntgew.
      ENDIF.

      SELECT SINGLE matnr vrkme umvkz umvkn lfimg charg brgew ntgew FROM lips "#EC CI_SEL_NESTED
        INTO (item_data-material, item_data-sales_unit, item_data-fact_unit_nom, item_data-fact_unit_denom,
              l_lips-lfimg, l_lips-charg, l_lips-brgew, l_lips-ntgew)
       WHERE vbeln = header_data-deliv_numb                                                                     ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
         AND posnr = item_data-deliv_item.

      IF mod_pesos = 'X'.
        IF l_lips-brgew <> lips-brgew.
          item_control-gross_wt_flg = 'X'.
        ENDIF.

        IF l_lips-ntgew <> lips-ntgew.
          item_control-net_wt_flg = 'X'.
        ENDIF.
      ENDIF.

      IF    l_lips-lfimg              <> item_data-dlv_qty OR l_lips-charg <> lips-charg
         OR item_control-gross_wt_flg  = 'X'
         OR item_control-net_wt_flg    = 'X'.

        IF l_lips-lfimg <> item_data-dlv_qty.
          SELECT SINGLE isocode
            INTO item_data-sales_unit_iso
            FROM t006
           WHERE msehi = item_data-sales_unit.

          IF item_data-sales_unit_iso = item_data-sales_unit.
            item_data-dlv_qty_imunit = item_data-dlv_qty.
          ELSE.
            item_data-dlv_qty_imunit = zcl_ap_material=>convertir_unidad( matnr          = item_data-material
                                                                          cantidad       = item_data-dlv_qty
                                                                          unidad_origen  = item_data-sales_unit
                                                                          unidad_destino = item_data-sales_unit_iso ).
            IF item_data-dlv_qty_imunit IS INITIAL.
              item_data-dlv_qty_imunit = item_data-dlv_qty.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND item_data TO i_item_data.

        item_control-deliv_numb = header_data-deliv_numb.
        item_control-deliv_item = item_data-deliv_item.
        item_control-chg_delqty = 'X'.
        APPEND item_control TO i_item_control.
      ENDIF.
    ENDLOOP.

    LOOP AT i_lips_del INTO lips.
      item_data-deliv_numb = header_data-deliv_numb.
      item_data-deliv_item = lips-posnr.

      SELECT SINGLE matnr vrkme lfimg FROM lips      "#EC CI_SEL_NESTED
        INTO (item_data-material, item_data-sales_unit, item_data-dlv_qty)  ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
       WHERE vbeln = header_data-deliv_numb
         AND posnr = item_data-deliv_item.

      SELECT SINGLE isocode
        INTO item_data-sales_unit_iso
        FROM t006
       WHERE msehi = item_data-sales_unit.

      IF item_data-sales_unit_iso = item_data-sales_unit.
        item_data-fact_unit_nom   = 1.
        item_data-fact_unit_denom = 1.
        item_data-dlv_qty_imunit  = item_data-dlv_qty.
      ELSE.
        item_data-dlv_qty_imunit  = zcl_ap_material=>convertir_unidad( matnr          = item_data-material
                                                                       cantidad       = item_data-dlv_qty
                                                                       unidad_origen  = item_data-sales_unit
                                                                       unidad_destino = item_data-sales_unit_iso ).
        item_data-fact_unit_nom   = 1.
        item_data-fact_unit_denom = 1.

      ENDIF.

      APPEND item_data TO i_item_data.

      item_control-deliv_numb = header_data-deliv_numb.
      item_control-deliv_item = item_data-deliv_item.
      item_control-del_item   = 'X'.
      APPEND item_control TO i_item_control.
    ENDLOOP.

    IF i_item_data IS INITIAL.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'No hay nada que modificar' msgty = 'W' ).
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
        EXPORTING
          header_data    = header_data
          header_control = header_control
          delivery       = header_data-deliv_numb
*         TECHN_CONTROL  =
*         HEADER_DATA_SPL   =
*         HEADER_CONTROL_SPL            =
*         SENDER_SYSTEM  =
        TABLES
*         HEADER_PARTNER =
*         HEADER_PARTNER_ADDR           =
*         HEADER_DEADLINES  =
          item_data      = i_item_data
          item_control   = i_item_control
*         ITEM_SERIAL_NO =
*         SUPPLIER_CONS_DATA            =
*         EXTENSION1     =
*         EXTENSION2     =
          return         = i_return.
*     TOKENREFERENCE =
*     ITEM_DATA_SPL  =
*     COLLECTIVE_CHANGE_ITEMS       =
*     new_item_data  = new_item_data
*     new_item_data_spl = new_item_data_spl.
*     new_item_org   = new_item_org
*     ITEM_DATA_DOCU_BATCH          =

      LOOP AT i_return INTO return WHERE message IS INITIAL.
        MESSAGE ID return-id TYPE return-type NUMBER return-number WITH return-message_v1 return-message_v2
                INTO message.
        return-message = message.
        MODIFY i_return FROM return.
      ENDLOOP.
      LOOP AT i_return INTO return WHERE type = 'E'.
        __add_lista message return-message.
      ENDLOOP.

      IF NOT o_log IS INITIAL.
        IF i_return IS INITIAL.
          o_log->log( p1 = '¿Se ha modificado la entrega?' msgty = 'W' ).
        ELSE.
          o_log->set_tabla_log_from_bapiret2_t( i_return ).
        ENDIF.
      ENDIF.

      IF message IS INITIAL AND commit = 'X'.
        zcl_ap_dev=>commit( dequeue_all = 'X' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD modificar_ctd_picking ##NEEDED.
    " TODO: parameter MODO_BI is never used (ABAP cleaner)
    " TODO: parameter O_LOG is never used (ABAP cleaner)

    DATA: l_vbkok       TYPE vbkok,
          i_vbpok       TYPE TABLE OF vbpok,
          l_ctd_pick_ot TYPE vbfa-rfmng,
          i_prot        TYPE TABLE OF prott,
          l_return      TYPE bapiret2.
    DATA o_bi TYPE REF TO zcl_ap_batch_input.

    l_vbkok-vbeln_vl = vbeln.
    SELECT SINGLE vbtyp FROM likp
      INTO l_vbkok-vbtyp_vl
     WHERE vbeln = vbeln.
    l_vbkok-vbeln = vbeln.

    i_vbpok = i_pos.

    LOOP AT i_vbpok ASSIGNING FIELD-SYMBOL(<pos>).
      <pos>-vbeln_vl = vbeln.
      IF NOT <pos>-posnn IS INITIAL.
        <pos>-posnr_vl = <pos>-posnn.
      ENDIF.
      <pos>-vbeln = vbeln.
      <pos>-kzfme = 'X'.

      IF NOT <pos>-charg IS INITIAL.
        <pos>-charg = <pos>-charg.
      ENDIF.
      IF NOT <pos>-vrkme IS INITIAL.
        <pos>-vrkme = <pos>-vrkme.
      ELSE.
        SELECT SINGLE vrkme FROM lips
          INTO <pos>-vrkme
         WHERE vbeln = vbeln
           AND posnr = <pos>-posnn.
      ENDIF.
      IF NOT <pos>-lgort IS INITIAL.
        SELECT SINGLE werks lgort FROM lips
          INTO CORRESPONDING FIELDS OF <pos>
         WHERE vbeln = vbeln
           AND posnr = <pos>-posnn.
      ENDIF.
    ENDLOOP.
    DATA(i_vbpok_orig) = i_vbpok.
    LOOP AT i_vbpok ASSIGNING <pos>.
      CLEAR l_ctd_pick_ot.
      SELECT SUM( rfmng ) FROM vbfa
        INTO l_ctd_pick_ot
       WHERE vbelv    = vbeln
         AND posnv    = <pos>-posnr_vl
         AND vbtyp_n  = 'Q'
         AND vbeln   <> vbeln.
      IF l_ctd_pick_ot > 0. " Si hay cantidades de pocking por OT la función no hace el picking correctamente.
        IF l_ctd_pick_ot > <pos>-pikmg.
*        <pos>-pikmg = - ( l_ctd_pick_ot - <pos>-pikmg ).
*        <pos>-plmin = '-'.
          DATA(l_error_ot) = 'X'.
        ELSE.
          <pos>-pikmg = <pos>-pikmg - l_ctd_pick_ot.
          <pos>-plmin = '+'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF l_error_ot = 'X' AND lines( i_vbpok ) = 1.
      o_bi = NEW #( ).

      o_bi->inicio( ).

* Initial Screen: Create/Change Outbound Delivery
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' okcode = '=ENT2' ).
      o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' okcode = '=POPO_T' ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' okcode = '=WEIT' ).
      o_bi->campos( campo = 'RV50A-POSNR' valor = i_vbpok[ 1 ]-posnr_vl ).

* Delivery: Item Overview (Subscreen Container)
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' okcode = '=SICH_T' ).
      o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = i_vbpok[ 1 ]-pikmg ).

      message = o_bi->llamar_transaccion( tcode = 'VL02N' modo = 'N' ).
      DATA(l_pikmg2) = zcl_ap_entregas=>get_ctd_picking( vbeln = vbeln posnr = i_vbpok[ 1 ]-posnr_vl select = 'X' add_subpos = '' buffer = 'N' ).
      IF l_pikmg2 = i_vbpok[ 1 ]-pikmg.
        CLEAR message.
        RETURN.
      ENDIF.
    ELSE.
      CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
        EXPORTING
          vbkok_wa             = l_vbkok
          synchron             = 'X'
          commit               = commit
          delivery             = vbeln
          update_picking       = 'X'
          nicht_sperren_1      = 'X'
          if_confirm_central   = ' '
          if_database_update_1 = '1'
        TABLES
          vbpok_tab            = i_vbpok
          prot                 = i_prot.

      LOOP AT i_prot ASSIGNING FIELD-SYMBOL(<prot>).
        CLEAR l_return.
        MOVE-CORRESPONDING <prot> TO l_return.
        MESSAGE ID <prot>-msgid TYPE 'S' NUMBER <prot>-msgno WITH <prot>-msgv1 <prot>-msgv2 <prot>-msgv3 <prot>-msgv4 INTO l_return-message.
        APPEND l_return TO i_return.

        IF <prot>-msgty = 'E'.
          message = l_return-message.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF message IS INITIAL AND commit = 'X'.
      LOOP AT i_vbpok_orig ASSIGNING <pos>.
        DATA(l_pikmg) = zcl_ap_entregas=>get_ctd_picking( vbeln = vbeln posnr = <pos>-posnr_vl select = 'X' add_subpos = '' buffer = 'N' ).
        IF l_pikmg <> <pos>-pikmg.
          message = |No se ha realizado correctamente el picking en posición { <pos>-posnr_vl ALPHA = OUT }|.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD modificar_fecha_sm.
    DATA: l_wadat     TYPE wadat,
          o_bi        TYPE REF TO zcl_ap_batch_input,
          l_wadat_new TYPE wadat.

    CLEAR message.
    IF wadat IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE wadat_ist FROM likp
      INTO l_wadat
     WHERE vbeln = vbeln.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF l_wadat = wadat.
      RETURN.
    ENDIF.

    message = espera_si_bloqueada( vbeln = vbeln segundos_espera = segundos_espera_si_bloqueos ).

    IF message IS INITIAL.
      o_bi = NEW #( ).

      o_bi->inicio( ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
      o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

      IF NOT tab IS INITIAL.
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = tab ).
      ENDIF.

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).
      o_bi->campos( campo = 'LIKP-WADAT_IST' valor = wadat ). " Fecha prevista para movimiento de mercancías

      message = o_bi->llamar_transaccion( tcode = 'VL02N' modo = modoct ).

      SELECT SINGLE wadat_ist FROM likp
        INTO l_wadat_new
       WHERE vbeln     = vbeln
         AND wadat_ist = wadat.
      IF sy-subrc = 0.
        IF NOT o_log IS INITIAL.
          o_log->log( p1    = 'Se ha modificado la fecha de  SM de la entrega'
                      p2    = vbeln
                      p3    = 'de fecha anterior'
                      p4    = l_wadat
                      p5    = 'a nueva fecha = '
                      p6    = l_wadat_new
                      msgty = 'S' ).
        ENDIF.
        CLEAR message.
      ELSE.
        IF forzar = 'X'.
          UPDATE likp "#EC AOC_STD_TABLE
            SET wadat_ist = wadat
           WHERE vbeln = vbeln.
          IF NOT o_log IS INITIAL.
            o_log->log( p1    = 'Se ha forzado modificación fecha de  SM de la entrega'
                        p2    = vbeln
                        p3    = 'de fecha anterior'
                        p4    = l_wadat
                        p5    = 'a nueva fecha = '
                        p6    = wadat
                        msgty = 'S' ).
          ENDIF.
          CLEAR message.
        ELSE.
          IF NOT o_log IS INITIAL.
            o_log->log( p1 = 'Error modificando la fecha de  SM de la entrega' p2 = vbeln p3 = message msgty = 'E' ).
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message msgty = 'E' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD picking.
    " TODO: parameter I_EMBALAJE is never used (ABAP cleaner)

    DATA: l_vbtyp     TYPE likp-vbtyp,
          l_tcode     TYPE sy-tcode,
          l_dynnr     TYPE bdcdata-dynpro,
          o_bi        TYPE REF TO zcl_ap_batch_input,
          i_lips3     TYPE TABLE OF lips,
          i_lips2     TYPE TABLE OF lips,
          l_lips      TYPE lips,
          i_lips_orig TYPE TABLE OF lips,
          l_lips2     TYPE lips,
          l_total_pos TYPE lips-lfimg,
          l_lips3     TYPE lips,
          l_msgbdc    TYPE bdcmsgcoll,
          l_vbfa      TYPE vbfa.

    FIELD-SYMBOLS <lips> TYPE lips.

    CLEAR mensaje.

    mensaje = validar_entrega_modificable( vbeln ).
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE vbtyp FROM likp
      INTO l_vbtyp
     WHERE vbeln = vbeln.
    IF l_vbtyp = '7'.
      l_tcode = 'VL32N'.
      l_dynnr = '4104'.
    ELSE.
      l_tcode = 'VL02N'.
      l_dynnr = '4004'.
    ENDIF.

    o_bi = NEW #( ).
    i_lips3 = i_lips.
    i_lips2 = i_lips3.
    SORT i_lips2.

* Descartamos aquellas posiciones sobre las que ya tengo el picking
    LOOP AT i_lips2 INTO l_lips.
      SELECT * FROM lips                      "#EC CI_ALL_FIELDS_NEEDED
        INTO l_lips
        UP TO 1 ROWS
       WHERE vbeln = vbeln
         AND uecha = l_lips-posnr
         AND matnr = l_lips-matnr
         AND charg = l_lips-charg
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        DELETE i_lips2.
      ELSE.
        SELECT vbeln posnr charg FROM lips
          APPENDING CORRESPONDING FIELDS OF TABLE i_lips_orig
         WHERE vbeln = vbeln
           AND posnr = l_lips-posnr.
      ENDIF.
    ENDLOOP.
    SORT i_lips_orig.
    DELETE ADJACENT DUPLICATES FROM i_lips_orig.

    IF NOT i_lips2 IS INITIAL.
      o_bi->inicio( ).

* Pantalla selccion de entrega
      o_bi->dynpro( program = 'SAPMV50A' dynpro = l_dynnr ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
      o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

      LOOP AT i_lips2 INTO l_lips.
        l_lips2 = l_lips.
        AT NEW posnr.
* Pulsamos para seleccionar posicion
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

* Entrega                Ventana   Posicionar    Posicion
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
          o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-posnr ). " Número de posicion del documento comercial

* Marcamos la primera fila y puslsamos sobre seleccion de lotes
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          IF solo_part_lotes <> 'N'.
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHSP_T' ).
          ELSE.
            o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          ENDIF.
          o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).

          IF ajustar_ctd = 'X'.
            CLEAR l_total_pos.
            LOOP AT i_lips3 INTO l_lips3 WHERE posnr = l_lips2-posnr.
*              ADD l_lips3-lfimg TO l_total_pos.
              l_total_pos = l_total_pos + l_lips3-lgmng.
            ENDLOOP.
            o_bi->campos( campo = 'LIPSD-G_LFIMG(01)' valor = l_total_pos ).
          ENDIF.

          IF solo_part_lotes <> 'N'.
*APC20150926 Si la posicion de entrega ya tenia lote informado, no me deja ir a la particion de lote.
* Por ello, veo si el lote que tiene es uno de los que voy a informar, y si es asi lo blanqueo
            IF NOT l_lips2-charg IS INITIAL.
              ASSIGN i_lips_orig[ posnr = l_lips2-posnr
                                  charg = l_lips2-charg ] TO <lips>.
              IF sy-subrc = 0.
                IF <lips>-charg <> ''.
                  o_bi->campos( campo = 'LIPS-CHARG(01)' valor = '' ).
                ENDIF.
              ELSE.
                IF ajustar_ctd = 'X'.
                  LOOP AT i_lips_orig ASSIGNING <lips> WHERE     posnr  = l_lips2-posnr
                                                             AND charg <> l_lips2-charg.
                    EXIT.
                  ENDLOOP.
                  IF sy-subrc = 0.
                    IF <lips>-charg <> ''.
                      o_bi->campos( campo = 'LIPS-CHARG(01)' valor = '' ). " Borramos lote si lo hubiera!
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de seleccion en dynpros de lista
          ENDIF.
        ENDAT.

        IF solo_part_lotes <> 'N'.
* Pulsamos para aÃ±adir una nueva línbea
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=POAN_T' ).

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          o_bi->campos( campo = 'LIPS-LGORT(02)' valor = l_lips-lgort ).
          o_bi->campos( campo = 'LIPS-CHARG(02)' valor = l_lips-charg ). " Número de lote
          o_bi->campos( campo = 'LIPS-LFIMG(02)' valor = l_lips-lgmng ). " Cantidad entregada efectivamente en UMV
          IF NOT l_lips-vrkme IS INITIAL.
            o_bi->campos( campo = 'LIPS-VRKME(02)' valor = l_lips-vrkme ).
          ENDIF.
          IF NOT l_lips-lichn IS INITIAL.
            o_bi->campos( campo = 'LIPS-LICHN(2)' valor = l_lips-lichn ).
          ENDIF.

        ENDIF.

        AT END OF posnr.
          IF solo_part_lotes <> 'N'.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK_T' ).
          ENDIF.
        ENDAT.

        AT LAST.
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).
        ENDAT.
      ENDLOOP.

      mensaje = o_bi->llamar_transaccion( tcode = l_tcode modo = modoct ).
      IF mensaje = 'Los datos batch input para el dynpro SAPMV50A 1000 no existen.'.
        READ TABLE o_bi->i_mensajes INTO l_msgbdc WITH KEY msgid = 'VL' msgnr = '221'.
        IF sy-subrc = 0.
          MESSAGE ID l_msgbdc-msgid TYPE 'S' NUMBER l_msgbdc-msgnr WITH l_msgbdc-msgv1 l_msgbdc-msgv2 l_msgbdc-msgv3 l_msgbdc-msgv4 INTO mensaje.
        ENDIF.
      ENDIF.

      IF o_bi->msgty = 'S'.
        CLEAR mensaje.
      ENDIF.
    ENDIF.

    IF solo_part_lotes IS INITIAL.
      IF mensaje IS INITIAL.
        SELECT * FROM lips                    "#EC CI_ALL_FIELDS_NEEDED
          INTO TABLE i_lips2
         WHERE vbeln  = vbeln
           AND uecha <> ''.

        i_lips3 = i_lips.
        LOOP AT i_lips2 ASSIGNING <lips>.
          LOOP AT i_lips3 INTO l_lips WHERE     posnr = <lips>-uecha
                                            AND matnr = <lips>-matnr
                                            AND charg = <lips>-charg
                                            AND lgmng = <lips>-lfimg.
*                                   and abart ne 'X'.
*        l_lips-abart = 'X'. "Para no volverlo a coger
*        modify i_lips3 from l_lips.
            DELETE i_lips3.
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            CLEAR l_vbfa-rfmng.
            SELECT SINGLE rfmng FROM vbfa            "#EC CI_SEL_NESTED
              INTO l_vbfa-rfmng
             WHERE vbelv   = vbeln
               AND posnv   = <lips>-posnr
               AND vbtyp_n = 'Q'.
            IF <lips>-lgort = l_lips-lgort AND <lips>-lgmng = l_vbfa-rfmng.
              DELETE i_lips2.
            ELSE.
              IF <lips>-lgort = l_lips-lgort.
                CLEAR <lips>-lgort.
              ELSE.
                <lips>-lgort = l_lips-lgort.
              ENDIF.
              <lips>-lgmng = l_lips-lgmng.
            ENDIF.
          ELSE.
            DELETE i_lips2.
          ENDIF.
        ENDLOOP.

        LOOP AT i_lips2 ASSIGNING <lips>.
          l_lips = <lips>.
          <lips>-posnr = <lips>-uecha.
          <lips>-uecha = l_lips-posnr.
        ENDLOOP.
        SORT i_lips2.

        LOOP AT i_lips2 INTO l_lips.
          AT FIRST.
            o_bi->inicio( ).
* Pantalla selccion de entrega
            o_bi->dynpro( program = 'SAPMV50A' dynpro = l_dynnr ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
            o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

          ENDAT.

          AT NEW posnr.
* Pulsamos para seleccionar posicion
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

* Entrega                Ventana   Posicionar    Posicion
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
            o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-posnr ). " Número de posicion del documento comercial

* Marcamos la primera fila y puslsamos sobre seleccion de lotes
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHPL_T01' ).
            o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).
            o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de seleccion en dynpros de lista

            CLEAR l_total_pos.
          ENDAT.

* Pulsamos para aÃ±adir una nueva línbea
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
          o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-uecha ). " Número de posicion del documento comercial

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          IF NOT l_lips-lgort IS INITIAL.
            o_bi->campos( campo = 'LIPS-LGORT(01)' valor = l_lips-lgort ). " Número de lote
          ENDIF.
          o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = l_lips-lgmng ). " Cantidad entregada efectivamente en UMV
          l_total_pos = l_total_pos + l_lips-lgmng.

          AT END OF posnr.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK_T' ).

            IF cambiar_ctd = 'X'.
              o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
              o_bi->campos( campo = 'LIPSD-G_LFIMG(01)' valor = l_total_pos ).
              o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
            ENDIF.
          ENDAT.

          AT LAST.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

            mensaje = o_bi->llamar_transaccion( tcode = l_tcode modo = modoct ).

            IF o_bi->msgty = 'S'.
              CLEAR mensaje.
            ENDIF.

          ENDAT.
        ENDLOOP.

* Ha habido errores en el picking!!, deshacemos!
        IF NOT mensaje IS INITIAL.
          deshacer_picking( vbeln = vbeln modoct = modoct ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD picking_opt.
    DATA: l_vbtyp     TYPE likp-vbtyp,
          l_tcode     TYPE sy-tcode,
          l_dynnr     TYPE bdcdata-dynpro,
          o_bi        TYPE REF TO zcl_ap_batch_input,
          i_lips3     TYPE TABLE OF lips,
          i_lips2     TYPE TABLE OF lips,
          l_tabla     TYPE tabname,
          l_max_posnr TYPE posnr,
          l_lips      TYPE lips,
          i_lips_orig TYPE TABLE OF lips,
          l_lips2     TYPE lips,
          l_kosta     TYPE kosta,
          l_total_pos TYPE lips-lfimg,
          l_lips3     TYPE lips,
          l_cont      TYPE i,
          l_msgbdc    TYPE bdcmsgcoll,
          l_vbfa      TYPE vbfa.
    DATA l_vhilm TYPE vekp-vhilm.

    FIELD-SYMBOLS <lips> TYPE lips.

    CLEAR mensaje.

    SET PARAMETER ID 'SHP_PROCESS_VIEW' FIELD 'P'. " Nos posicionamos el tab de picking

    mensaje = validar_entrega_modificable( vbeln ).
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(i_embalaje_l) = i_embalaje.

    SELECT SINGLE vbtyp FROM likp
      INTO l_vbtyp
     WHERE vbeln = vbeln.
    IF l_vbtyp = '7'.
      l_tcode = 'VL32N'.
      l_dynnr = '4104'.
    ELSE.
      l_tcode = 'VL02N'.
      l_dynnr = '4004'.
    ENDIF.

    o_bi = NEW #( ).
    i_lips3 = i_lips.
    i_lips2 = i_lips3.
    SORT i_lips2.

    IF zcl_c=>hana IS INITIAL.
      l_tabla = 'VBUP'.
    ELSE.
      l_tabla = 'LIPS'.
    ENDIF.

    SELECT MAX( posnr ) FROM lips
      INTO l_max_posnr
     WHERE vbeln = vbeln
       AND posnr > '900000'.
    IF l_max_posnr IS INITIAL.
      l_max_posnr = '900000'.
    ENDIF.

* Descartamos aquellas posiciones sobre las que ya tengo el picking
    LOOP AT i_lips2 INTO l_lips.
      SELECT * FROM lips                      "#EC CI_ALL_FIELDS_NEEDED
        INTO l_lips                             ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
        UP TO 1 ROWS
       WHERE vbeln = vbeln
         AND uecha = l_lips-posnr
         AND matnr = l_lips-matnr
         AND charg = l_lips-charg
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        DELETE i_lips2.
      ELSE.
        SELECT vbeln posnr charg FROM lips                 ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
          APPENDING CORRESPONDING FIELDS OF TABLE i_lips_orig
         WHERE vbeln = vbeln
           AND posnr = l_lips-posnr.
      ENDIF.
    ENDLOOP.
    SORT i_lips_orig.
    DELETE ADJACENT DUPLICATES FROM i_lips_orig.

    IF NOT i_lips2 IS INITIAL.
      o_bi->inicio( ).

* Pantalla selccion de entrega
      o_bi->dynpro( program = 'SAPMV50A' dynpro = l_dynnr ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
      o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

      LOOP AT i_lips2 INTO l_lips.
        l_lips2 = l_lips.
        AT NEW posnr.
          SELECT SINGLE kosta FROM (l_tabla)
            INTO l_kosta
           WHERE vbeln = vbeln
             AND posnr = l_lips-posnr.

* Pulsamos para seleccionar posicion
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

* Entrega                Ventana   Posicionar    Posicion
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
          o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-posnr ). " Número de posicion del documento comercial

          DATA(l_n_particiones) = 0.
          LOOP AT i_lips TRANSPORTING NO FIELDS WHERE posnr = l_lips-posnr.
            l_n_particiones = l_n_particiones + 1.
          ENDLOOP.
          IF l_n_particiones = 1.
* Verificamos si no tenemos una partición previa
            SELECT COUNT( * ) FROM lips
              INTO @DATA(l_part_prev)
             WHERE vbeln  = @vbeln
               AND posnr <> @l_lips-posnr
               AND uecha  = @l_lips-posnr.
            l_n_particiones = l_n_particiones + l_part_prev.
          ENDIF.

* Marcamos la primera fila y puslsamos sobre seleccion de lotes
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          IF solo_part_lotes <> 'N' AND l_n_particiones > 1.
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHSP_T' ).
          ELSE.
            o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          ENDIF.
          o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).

          IF ajustar_ctd = 'X'.
            CLEAR l_total_pos.
            LOOP AT i_lips3 INTO l_lips3 WHERE posnr = l_lips2-posnr.
*              ADD l_lips3-lfimg TO l_total_pos.
              l_total_pos = l_total_pos + l_lips3-lgmng.
            ENDLOOP.
            o_bi->campos( campo = 'LIPSD-G_LFIMG(01)' valor = l_total_pos ).
          ENDIF.

          IF solo_part_lotes <> 'N'.
*APC20150926 Si la posicion de entrega ya tenia lote informado, no me deja ir a la particion de lote.
* Por ello, veo si el lote que tiene es uno de los que voy a informar, y si es asi lo blanqueo
            IF NOT l_lips2-charg IS INITIAL.
              ASSIGN i_lips_orig[ posnr = l_lips2-posnr
                                  charg = l_lips2-charg ] TO <lips>.
              IF sy-subrc = 0.
                IF <lips>-charg <> ''.
                  o_bi->campos( campo = 'LIPS-CHARG(01)' valor = '' ).
                ENDIF.
              ELSE.
                IF ajustar_ctd = 'X'.
                  LOOP AT i_lips_orig ASSIGNING <lips> WHERE     posnr  = l_lips2-posnr
                                                             AND charg <> l_lips2-charg.
                    EXIT.
                  ENDLOOP.
                  IF sy-subrc = 0.
                    IF <lips>-charg <> ''.
                      o_bi->campos( campo = 'LIPS-CHARG(01)' valor = '' ). " Borramos lote si lo hubiera!
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de seleccion en dynpros de lista
          ENDIF.
        ENDAT.

        IF l_n_particiones = 1.
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
* Si se necesita cambiar el almacén, pero tenía lote informado, tenemos que borrarlo antes de volver a intentar ponerlo.
          ASSIGN i_lips_orig[ posnr = l_lips2-posnr
                              charg = l_lips2-charg ] TO <lips>.
          IF sy-subrc = 0.
            IF <lips>-charg <> '' AND ( <lips>-lgort <> l_lips-lgort AND l_lips-lgort NE '' ).
              o_bi->campos( campo = 'LIPS-CHARG(01)' valor = '' ).
              o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = '' ).
              o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
              o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
            ENDIF.
          ENDIF.

          IF NOT l_lips-lgort IS INITIAL.
            o_bi->campos( campo = 'LIPS-LGORT(01)' valor = l_lips-lgort ).
          ENDIF.
          o_bi->campos( campo = 'LIPS-CHARG(01)' valor = l_lips-charg ).

          IF l_kosta = 'A' OR l_kosta = 'B'.
            o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = l_lips-lgmng ). " Cantidad entregada efectivamente en UMV
          ENDIF.
        ELSE.
          DATA(l_hay_part_lotes) = 'X'.
          IF solo_part_lotes <> 'N'.
            IF solo_part_lotes = 'E'. " Caso especial para hacer embalaje en un solo paso
              CLEAR l_hay_part_lotes.
              l_max_posnr = l_max_posnr + 1.
              LOOP AT i_embalaje_l ASSIGNING FIELD-SYMBOL(<hu>) WHERE     posnr = l_lips-posnr
                                                                      AND charg = l_lips-charg.
                <hu>-posnr = l_max_posnr.
              ENDLOOP.
            ENDIF.
* Pulsamos para añadir una nueva línea
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=POAN_T' ).

            o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
            IF l_lips-lgort IS INITIAL.
              o_bi->campos( campo = 'LIPS-LGORT(02)' valor = l_lips-lgort ).
            ENDIF.
            o_bi->campos( campo = 'LIPS-CHARG(02)' valor = l_lips-charg ). " Número de lote
            o_bi->campos( campo = 'LIPS-LFIMG(02)' valor = l_lips-lgmng ). " Cantidad entregada efectivamente en UMV
            IF NOT l_lips-vrkme IS INITIAL.
              o_bi->campos( campo = 'LIPS-VRKME(02)' valor = l_lips-vrkme ).
            ENDIF.
          ENDIF.
        ENDIF.

        AT END OF posnr.
          IF solo_part_lotes <> 'N' AND l_n_particiones > 1.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK_T' ).
          ENDIF.
        ENDAT.

        AT LAST.
          IF l_hay_part_lotes IS INITIAL AND NOT i_embalaje IS INITIAL.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=VERP_T' ). " Embalaje

            o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=UE6VDIR' ). " Entrada individual

            LOOP AT i_embalaje_l ASSIGNING <hu>.
              o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
              o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENTR' ).
              o_bi->campos( campo = 'VEKP-EXIDV' valor = <hu>-exidv ). " Identificación externa de la unidad de manipulación

              CLEAR l_vhilm.
              SELECT COUNT( * ) FROM vekp
                INTO l_cont
               WHERE exidv = <hu>-exidv.
              IF l_cont = 1.
                SELECT vhilm
                  FROM vekp
                  WHERE exidv = @<hu>-exidv
                  ORDER BY PRIMARY KEY
                  INTO @l_vhilm
                  UP TO 1 ROWS.
                ENDSELECT.
              ENDIF.
              IF l_vhilm <> <hu>-vhilm.
                o_bi->campos( campo = 'VEKP-VHILM' valor = <hu>-vhilm ). " Material de embalaje
              ENDIF.
              o_bi->campos( campo = 'HUMV4-MATNR' valor = <hu>-matnr ). " Número de material
              o_bi->campos( campo = 'HUMV4-CHARG' valor = <hu>-charg ). " Número de lote
              o_bi->campos( campo = 'HUMV4-QUANTITY' valor = <hu>-lfimg ). " Cantidad base embalada en posición de unidad manipulación
              o_bi->campos( campo = 'HUMV4-VRKME' valor = <hu>-vrkme ). " Unidad de medida alternativa p.unidad de medida de stock
              o_bi->campos( campo = 'HUMV4-POSNR' valor = <hu>-posnr ).
            ENDLOOP.

            o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).
          ELSE.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).
          ENDIF.
        ENDAT.
      ENDLOOP.

      mensaje = o_bi->llamar_transaccion( tcode = l_tcode modo = modoct ).
      IF mensaje = 'Los datos batch input para el dynpro SAPMV50A 1000 no existen.'.
        READ TABLE o_bi->i_mensajes INTO l_msgbdc WITH KEY msgid = 'VL' msgnr = '221'.
        IF sy-subrc = 0.
          MESSAGE ID l_msgbdc-msgid TYPE 'S' NUMBER l_msgbdc-msgnr WITH l_msgbdc-msgv1 l_msgbdc-msgv2 l_msgbdc-msgv3 l_msgbdc-msgv4 INTO mensaje.
        ENDIF.
      ENDIF.

      IF o_bi->msgty = 'S'.
        CLEAR mensaje.
      ENDIF.
    ENDIF.

    IF solo_part_lotes IS INITIAL AND l_hay_part_lotes = 'X'.
      IF mensaje IS INITIAL.
        SELECT * FROM lips                    "#EC CI_ALL_FIELDS_NEEDED
          INTO TABLE i_lips2                    ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
         WHERE vbeln  = vbeln
           AND uecha <> ''.

        i_lips3 = i_lips.
        LOOP AT i_lips2 ASSIGNING <lips>.
          LOOP AT i_lips3 INTO l_lips WHERE     posnr = <lips>-uecha
                                            AND matnr = <lips>-matnr
                                            AND charg = <lips>-charg
                                            AND lgmng = <lips>-lfimg.
*                                   and abart ne 'X'.
*        l_lips-abart = 'X'. "Para no volverlo a coger
*        modify i_lips3 from l_lips.
            DELETE i_lips3.
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            CLEAR l_vbfa-rfmng.
            SELECT rfmng
              FROM vbfa                              "#EC CI_SEL_NESTED
              WHERE vbelv   = @vbeln
                AND posnv   = @<lips>-posnr
                AND vbtyp_n = 'Q'
              ORDER BY PRIMARY KEY
              INTO @l_vbfa-rfmng
              UP TO 1 ROWS.
            ENDSELECT.
            IF <lips>-lgort = l_lips-lgort AND <lips>-lgmng = l_vbfa-rfmng.
              DELETE i_lips2.
            ELSE.
              IF <lips>-lgort = l_lips-lgort.
                CLEAR <lips>-lgort.
              ELSE.
                <lips>-lgort = l_lips-lgort.
              ENDIF.
              <lips>-lgmng = l_lips-lgmng.
            ENDIF.
          ELSE.
            DELETE i_lips2.
          ENDIF.
        ENDLOOP.

        LOOP AT i_lips2 ASSIGNING <lips>.
          l_lips = <lips>.
          <lips>-posnr = <lips>-uecha.
          <lips>-uecha = l_lips-posnr.
        ENDLOOP.
        SORT i_lips2.

        LOOP AT i_lips2 INTO l_lips.
          AT FIRST.
            o_bi->inicio( ).
* Pantalla selccion de entrega
            o_bi->dynpro( program = 'SAPMV50A' dynpro = l_dynnr ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
            o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

          ENDAT.

          AT NEW posnr.
* Pulsamos para seleccionar posicion
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

* Entrega                Ventana   Posicionar    Posicion
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
            o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-posnr ). " Número de posicion del documento comercial

* Marcamos la primera fila y puslsamos sobre seleccion de lotes
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHPL_T01' ).
            o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).
            o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de seleccion en dynpros de lista

            CLEAR l_total_pos.
          ENDAT.

* Pulsamos para aÃ±adir una nueva línbea
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
          o_bi->campos( campo = 'RV50A-POSNR' valor = l_lips-uecha ). " Número de posicion del documento comercial

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          IF NOT l_lips-lgort IS INITIAL.
            o_bi->campos( campo = 'LIPS-LGORT(01)' valor = l_lips-lgort ). " Número de lote
          ENDIF.
          o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = l_lips-lgmng ). " Cantidad entregada efectivamente en UMV
          l_total_pos = l_total_pos + l_lips-lgmng.

          AT END OF posnr.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK_T' ).

            IF cambiar_ctd = 'X'.
              o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
              o_bi->campos( campo = 'LIPSD-G_LFIMG(01)' valor = l_total_pos ).
              o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
            ENDIF.
          ENDAT.

          AT LAST.
            IF NOT i_embalaje IS INITIAL.
              o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
              o_bi->campos( campo = 'BDC_OKCODE' valor = '=VERP_T' ). " Embalaje

              o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
              o_bi->campos( campo = 'BDC_OKCODE' valor = '=UE6VDIR' ). " Entrada individual

              LOOP AT i_embalaje ASSIGNING <hu>.
                o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
                o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENTR' ).
                o_bi->campos( campo = 'VEKP-EXIDV' valor = <hu>-exidv ). " Identificación externa de la unidad de manipulación

                CLEAR l_vhilm.
                SELECT COUNT( * ) FROM vekp
                  INTO l_cont
                 WHERE exidv = <hu>-exidv.
                IF l_cont = 1.
                  SELECT vhilm
                    FROM vekp
                    WHERE exidv = @<hu>-exidv
                    ORDER BY PRIMARY KEY
                    INTO @l_vhilm
                    UP TO 1 ROWS.
                  ENDSELECT.
                ENDIF.
                IF l_vhilm <> <hu>-vhilm.
                  o_bi->campos( campo = 'VEKP-VHILM' valor = <hu>-vhilm ). " Material de embalaje
                ENDIF.
                o_bi->campos( campo = 'HUMV4-MATNR' valor = <hu>-matnr ). " Número de material
                o_bi->campos( campo = 'HUMV4-CHARG' valor = <hu>-charg ). " Número de lote
                o_bi->campos( campo = 'HUMV4-QUANTITY' valor = <hu>-lfimg ). " Cantidad base embalada en posición de unidad manipulación
                o_bi->campos( campo = 'HUMV4-VRKME' valor = <hu>-vrkme ). " Unidad de medida alternativa p.unidad de medida de stock
                o_bi->campos( campo = 'HUMV4-POSNR' valor = <hu>-posnr ).
              ENDLOOP.

              o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
              o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).
            ELSE.
              o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
              o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).
            ENDIF.

            mensaje = o_bi->llamar_transaccion( tcode = l_tcode modo = modoct ).

            IF o_bi->msgty = 'S'.
              CLEAR mensaje.
            ENDIF.

          ENDAT.
        ENDLOOP.

* Ha habido errores en el picking!!, deshacemos!
        IF NOT mensaje IS INITIAL.
          deshacer_picking( vbeln = vbeln modoct = modoct ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD picking_y_embalaje.
    " TODO: parameter CONVERTIR_A_UN_POS is never used (ABAP cleaner)

    DATA: l_tabla            TYPE tabname VALUE 'VBUP',
          l_likp             TYPE likp,
          l_venum            TYPE venum,
          l_lips             TYPE lips,
          l_unidad           TYPE meins,
          l_cantidad         TYPE mengv13,
          o_bi               TYPE REF TO zcl_ap_batch_input,
          l_vepo             TYPE vepo,
          l_particion_sin_hu TYPE c LENGTH 1,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_vbup             TYPE vbup.

    FIELD-SYMBOLS <bdcmsg> TYPE bdcmsgcoll.

    IF zcl_c=>hana = 'X'.
      l_tabla = 'LIPS'.
    ENDIF.

    SELECT SINGLE lstel FROM likp
      INTO l_likp-lstel
     WHERE vbeln = vbeln.
    IF sy-subrc <> 0.
      mensaje = 'No existe la entrega'.
      RETURN.
    ENDIF.

    l_venum = zcl_ap_hu=>get_venum( exidv ).
    IF l_venum IS INITIAL.
      mensaje = 'No existe la HU'.
      RETURN.
    ENDIF.

    SELECT SINGLE matnr posnr vrkme lgort charg lfimg FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      INTO CORRESPONDING FIELDS OF l_lips
     WHERE vbeln = vbeln
       AND posnr = posnr.
    IF sy-subrc <> 0.
      mensaje = 'No existe la posición de la entrega'.
      RETURN.
    ELSE.
      IF vrkme = l_lips-vrkme OR lfimg IS INITIAL.
        l_unidad   = vrkme.
        l_cantidad = lfimg.
      ELSE.
        l_cantidad = zcl_ap_material=>convertir_unidad( matnr          = l_lips-matnr
                                                        cantidad       = lfimg
                                                        unidad_origen  = vrkme
                                                        unidad_destino = l_lips-vrkme ).
        IF l_cantidad IS INITIAL.
          l_unidad   = vrkme.
          l_cantidad = lfimg.
        ELSE.
          l_unidad = l_lips-vrkme.
        ENDIF.
      ENDIF.
    ENDIF.

    mensaje = validar_entrega_modificable( vbeln ).
    IF NOT mensaje IS INITIAL.
      RETURN.
    ENDIF.

    o_bi = NEW #( ).

* Si la HU no esta asignada a la entrega, no continuamos
    SELECT posnr FROM vepo
      INTO l_vepo-posnr
      UP TO 1 ROWS
     WHERE vbeln = vbeln
       AND venum = l_venum
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      o_bi->inicio( ).

      SET PARAMETER ID 'SHP_PROCESS_VIEW' FIELD 'P'. " Nos posicionamos el tab de picking

* Pantalla selccion de entrega
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
      o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

* Buscamos si tenemos alguna posicion de particion de lotes sin asignar a HU

* Buscamos particion de lotes de la posicion
      CLEAR l_particion_sin_hu.
      IF NOT exidv IS INITIAL.
        SELECT posnr FROM lips ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
          INTO l_lips-posnr
         WHERE vbeln = vbeln
           AND uecha = posnr.
* Verificamos si ya esta asignada a entrega
          SELECT * FROM vepo JOIN vekp ON vepo~venum = vekp~venum
            INTO CORRESPONDING FIELDS OF l_vepo
            UP TO 1 ROWS
           WHERE vbeln     = vbeln
             AND posnr     = l_lips-posnr
             AND vpobj    IN ( '01', '03' )
             AND vpobjkey  = vbeln
           ORDER BY vepo~venum vepo~vepos.
          ENDSELECT.
* Si no encuentro equivalencia en HU es que esa particicion no tiene HU asignada
          IF sy-subrc <> 0.
            l_particion_sin_hu = 'X'.
          ENDIF.
        ENDSELECT.
      ENDIF.

      IF l_particion_sin_hu IS INITIAL.
        IF NOT lstel IS INITIAL.
          IF lstel <> l_likp-lstel.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=T\03' ).

            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
            o_bi->campos( campo = 'LIKP-LSTEL' valor = lstel ).
          ENDIF.
        ENDIF.

* Pulsamos para seleccionar posición
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

* Entrega                Ventana   Posicionar    Posición
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
        o_bi->campos( campo = 'RV50A-POSNR' valor = posnr ). " Número de posición del documento comercial

*APC20200422 Verificamos si la entrega tiene lote informado y si viene de pedido
        DATA(l_lote_pedido) = get_lote_origen_pedido( vbeln = vbeln posnr = posnr ).
        IF NOT l_lote_pedido IS INITIAL.
          IF l_lote_pedido <> charg.
            mensaje = |El lote indicado { charg } no coincide con el del pedido { l_lote_pedido }|.
            RETURN.
          ENDIF.
        ELSEIF NOT l_lips-charg IS INITIAL.
          IF l_lips-charg = charg.
*APC20200518 Aunque no tenga el lote del pedido, si el lote de la entrega es el mismo que voy a informar, hacemos el mismo tratamiento
            l_lote_pedido = charg.
          ELSE.
            mensaje = |El lote indicado { charg } no coincide con el que hay en la entrega { l_lips-charg }|.
            RETURN.
          ENDIF.
        ENDIF.

        IF l_lote_pedido IS INITIAL.
* Marcamos la primera fila y pulsamos sobre seleccion de lotes
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHSP_T' ).
          o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).
          o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de seleccion en dynpros de lista

* Pulsamos para añadir una nueva línbea
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=POAN_T' ).
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
          o_bi->campos( campo = 'LIPS-CHARG(02)' valor = charg ). " Numero de lote

          IF almacen_hu = 'X'.
            SELECT SINGLE lgort FROM vepo
              INTO l_vepo-lgort
             WHERE venum = l_venum.
            o_bi->campos( campo = 'LIPS-LGORT(02)' valor = l_vepo-lgort ).
          ENDIF.

          o_bi->campos( campo = 'LIPS-LFIMG(02)' valor = l_cantidad ). " Cantidad entregada efectivamente en UMV
          o_bi->campos( campo = 'LIPS-VRKME(02)' valor = l_unidad ).
        ELSE.
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
* Si tenía un lote informado del pedido, directamente incrementamos la cantidad de picking
          DATA(l_ctd_picking) = get_ctd_picking( vbeln = vbeln posnr = posnr add_subpos = '' select = 'X' ).
          l_cantidad = l_cantidad + l_ctd_picking.

          IF almacen_hu = 'X'.
            SELECT SINGLE lgort FROM vepo
              INTO l_vepo-lgort
             WHERE venum = l_venum.
            IF l_vepo-lgort <> l_lips-lgort.
              IF l_ctd_picking = 0.
                o_bi->campos( campo = 'LIPS-LGORT(01)' valor = l_vepo-lgort ).
              ELSE.
                mensaje = |Almacén de la HU es { l_vepo-lgort } diferente del de la posición { l_lips-lgort } que ya tiene picking|.
                RETURN.
              ENDIF.
            ENDIF.
          ENDIF.

*        o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = l_cantidad ). " Cantidad de picking
        ENDIF.

        IF exidv IS INITIAL.
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).
        ELSE.
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
* Pulsamos para ir a embalaje
          IF l_lote_pedido IS INITIAL.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '3000' ).
          ELSE.
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          ENDIF.
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=VERP_T' ).

* Marcamos todos los lotes para embalajar
          o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=HU_MARKA' ).
*
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=UE6VDIR' ).
* Paso a pestaña Entrada individual

          o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).
          o_bi->campos( campo = 'VEKP-EXIDV' valor = exidv ).
        ENDIF.
      ELSE.
* Pulsamos para ir a embalaje
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=VERP_T' ).

* Marcamos todos los lotes para embalajar
        o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=HU_MARKA' ).
*
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=UE6VDIR' ).
* Paso a pestaña Entrada individual

        o_bi->dynpro( program = 'SAPLV51G' dynpro = '6000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH' ).
        o_bi->campos( campo = 'VEKP-EXIDV' valor = exidv ).
      ENDIF.

      mensaje = o_bi->llamar_transaccion( tcode = 'VL02N' modo = modoct ).

      IF mensaje CS 'grabado'.
        CLEAR mensaje.

        CLEAR l_vepo.
        DO 5 TIMES.
          SELECT * FROM vepo
            INTO l_vepo
            UP TO 1 ROWS
           WHERE vbeln = vbeln
             AND venum = l_venum
            ORDER BY PRIMARY KEY.
          ENDSELECT.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.
      ENDIF.

      IF mensaje IS INITIAL.
* Los mensajes de HU, los devuelve como informativos pero son errores!
        LOOP AT o_bi->i_mensajes ASSIGNING <bdcmsg> WHERE    ( msgid = 'HUDIALOG'    AND msgnr = '157' )
                                                          OR ( msgid = 'HUFUNCTIONS' AND msgnr = '261' ).
          MESSAGE ID <bdcmsg>-msgid TYPE 'S' NUMBER <bdcmsg>-msgnr WITH
                  <bdcmsg>-msgv1 <bdcmsg>-msgv2 <bdcmsg>-msgv3 <bdcmsg>-msgv4 INTO mensaje.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

*  IF l_vepo-posnr IS INITIAL AND sy-uname = 'APICAZO'.
*    l_vepo-posnr = posnr.
*  ENDIF.

    IF l_vepo IS INITIAL.
      mensaje = 'No se encuentra posición de embalaje'.
      RETURN.
    ENDIF.

    SELECT SINGLE vbeln FROM (l_tabla)
      INTO l_vbup-vbeln
     WHERE vbeln = vbeln
       AND posnr = l_vepo-posnr
       AND kosta = 'C'.
    IF sy-subrc = 0.
* Posicion ya embalada!
    ELSE.
      o_bi->inicio( ).
* Pantalla selccion de entrega
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
      o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

* Pulsamos para seleccionar posicion
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

* Entrega                Ventana   Posicionar    Posicion
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
      o_bi->campos( campo = 'RV50A-POSNR' valor = posnr ). " Número de posicion del documento comercial

      IF posnr <> l_vepo-posnr.
* Marcamos la primera fila y puslsamos sobre seleccion de lotes
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHPL_T01' ).
        o_bi->campos( campo = 'BDC_CURSOR' valor = 'LIPS-POSNR(01)' ).
        o_bi->campos( campo = 'RV50A-LIPS_SELKZ(01)' valor = 'X' ). " Indicador de seleccion en dynpros de lista

* Pulsamos para añadir una nueva línbea
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

        o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
        o_bi->campos( campo = 'RV50A-POSNR' valor = l_vepo-posnr ). " Número de posicion del documento comercial

      ENDIF.
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
      o_bi->campos( campo = 'LIPSD-PIKMG(01)' valor = l_cantidad ). " Cantidad entregada efectivamente en UMV

      IF posnr <> l_vepo-posnr.
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK_T' ).
      ENDIF.

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

      mensaje = o_bi->llamar_transaccion( tcode = 'VL02N' modo = modoct ).

      IF o_bi->msgty = 'S'.
        CLEAR mensaje.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD salida_mercancias.
    DATA: l_tabla TYPE tabname VALUE 'VBUK',
          l_wbstk TYPE wbstk,
          o_bi    TYPE REF TO zcl_ap_batch_input,
          l_vbtyp TYPE likp-vbtyp.

    IF zcl_c=>hana = 'X'.
      l_tabla = 'LIKP'.
    ENDIF.

    CLEAR mensaje.

    SELECT SINGLE wbstk FROM (l_tabla)
      INTO l_wbstk
     WHERE vbeln = vbeln.
    IF l_wbstk = 'C'.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'La entrega' p2 = vbeln p3 = 'ya tenía efectuada la salida de mercancías' msgty = 'W' ).
      ENDIF.
      RETURN.
    ELSE.
      o_bi = NEW #( ).

      o_bi->inicio( ).

      SELECT SINGLE vbtyp FROM likp
        INTO l_vbtyp
       WHERE vbeln = vbeln.

      IF l_vbtyp = '7'.
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '4104' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=WABU_T' ).
        o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

        mensaje = o_bi->llamar_transaccion( tcode = 'VL32N' modo = modoct ).
      ELSE.
* Pantalla selcción de entrega
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '4004' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=WABU_T' ).
        o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

        mensaje = o_bi->llamar_transaccion( tcode = 'VL02N' modo = modoct ).
      ENDIF.

      IF o_bi->msgty = 'E' AND mensaje <> ''.
      ELSE.
        SELECT SINGLE wbstk FROM (l_tabla)
          INTO l_wbstk
         WHERE vbeln = vbeln.
        IF l_wbstk = 'C'.
          CLEAR mensaje.
        ENDIF.
      ENDIF.

      IF NOT o_log IS INITIAL.
        IF mensaje IS INITIAL.
          o_log->log( p1 = 'Se ha efectuado la salida de mercancías de la entrega' p2 = vbeln msgty = 'S' ).
        ELSE.
          o_log->log( p1 = 'Error de SM de la entrega' p2 = vbeln p3 = mensaje msgty = 'E' ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_fecha_hora_inicio.
    DATA: l_handle     TYPE likp-handle,
          tsege        TYPE tsege,
          l_even_tstfr TYPE tsege-even_tstfr,
          l_even_zonfr TYPE tsege-even_zonfr,
          l_fecha      TYPE dats,
          l_hora       TYPE uzeit,
          tsegh        TYPE tsegh,
          l_aux        TYPE c LENGTH 22.

    IF handle IS INITIAL.
      SELECT SINGLE handle FROM likp
        INTO l_handle
       WHERE vbeln = vbeln.
    ELSE.
      l_handle = handle.
    ENDIF.

    IF l_handle IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE even_tstfr even_zonfr FROM tsege
      INTO (l_even_tstfr, l_even_zonfr)
     WHERE head_hdl   = l_handle
       AND even       = tipo_fecha
       AND even_verty = tipo.
    IF sy-subrc = 0.
      zcl_ap_fechas=>timestamp_2_fechahora( EXPORTING timestamp = l_even_tstfr
                                                      tzone     = l_even_zonfr
                                            IMPORTING fecha     = l_fecha
                                                      hora      = l_hora ).
      IF NOT ( l_fecha = fecha AND l_hora = hora ).
        l_even_tstfr = zcl_ap_fechas=>fechahora_2_timestamp( fecha = fecha hora = hora tzone = tzone ).
        UPDATE tsege "#EC AOC_STD_TABLE
          SET even_tstfr = l_even_tstfr
              even_tstto = l_even_tstfr
         WHERE head_hdl   = l_handle
           AND even       = tipo_fecha
           AND even_verty = tipo.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM tsegh
        INTO tsegh
      WHERE head_hdl = l_handle.
      IF sy-subrc <> 0.
        tsegh-head_hdl   = l_handle.
        tsegh-head_obj   = 'WSHDRLIKP'.
        tsegh-head_tpl   = 'WSHDRSTDDL'.
        tsegh-head_unacr = sy-uname.
        CONCATENATE sy-datum sy-uzeit INTO l_aux.
        tsegh-head_tstcr = l_aux.
        INSERT tsegh FROM tsegh. "#EC AOC_STD_TABLE
      ENDIF.
      tsege-head_hdl   = l_handle.
      tsege-even       = tipo_fecha.
      tsege-even_sor   = 300.
      tsege-even_zonfr = tzone.
      tsege-even_zonto = tzone.
      IF tipo = '1'.
        tsege-even_verty = '0'.
        INSERT tsege FROM tsege. "#EC AOC_STD_TABLE
      ELSEIF tipo = '0'.
        tsege-even_verty = '1'.
        INSERT tsege FROM tsege. "#EC AOC_STD_TABLE
      ENDIF.

      tsege-even_verty = tipo.
      tsege-even_tstfr = zcl_ap_fechas=>fechahora_2_timestamp( fecha = fecha hora = hora tzone = tzone ).
      tsege-even_tstto = tsege-even_tstfr.
      INSERT tsege FROM tsege.  "#EC AOC_STD_TABLE
    ENDIF.
  ENDMETHOD.
  METHOD set_texto_string.
    DATA l_name TYPE stxh-tdname.

    IF posnr IS INITIAL.
      zcl_ap_textos=>save_texto_string( id = id object = 'VBBK' name = vbeln spras = spras string = string ).
    ELSE.
      CONCATENATE vbeln posnr INTO l_name.
      zcl_ap_textos=>save_texto_string( id = id object = 'VBBP' name = l_name spras = spras string = string ).
    ENDIF.
  ENDMETHOD.
  METHOD tiene_sm.
    DATA: l_tabla TYPE tabname VALUE 'VBUK',
          l_vbeln TYPE vbuk-vbeln,
          l_wbstk TYPE vbuk-wbstk.

    IF zcl_c=>hana = 'X'.
      l_tabla = 'LIKP'.
    ENDIF.

    l_vbeln = vbeln.
    __poner_ceros l_vbeln.
    SELECT SINGLE wbstk FROM (l_tabla)
      INTO l_wbstk
     WHERE vbeln = l_vbeln.
    IF l_wbstk = 'C'.
      sm = 'X'.
    ELSE.
      CLEAR sm.
    ENDIF.
  ENDMETHOD.
  METHOD validar_entrega_modificable.
    DATA: l_tknum TYPE tknum,
          l_sttrg TYPE vttk-sttrg.

    IF tiene_sm( vbeln ) = 'X'.
      message = 'Entrega ya tiene salida de mercancías. No es posible modificarla'.
    ELSE.
      l_tknum = get_transporte( vbeln ).
      IF NOT l_tknum IS INITIAL.
        SELECT SINGLE sttrg FROM vttk
          INTO l_sttrg
         WHERE tknum = l_tknum.
        IF l_sttrg = '7'.
          __concat3 message 'Entrega está incluída en transporte finalizado' l_tknum '. No es posible modificarla'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar.
    TYPES: BEGIN OF t_ent,
             vbeln TYPE likp-vbeln,
             lfdat TYPE likp-lfdat,
           END OF t_ent,
           BEGIN OF t_vbeln,
             vbeln TYPE vbak-vbeln,
           END OF t_vbeln.

    DATA: l_entrada  TYPE string,
          l_lista    TYPE string,
          l_vbeln    TYPE vbeln_va,
          i_entregas TYPE TABLE OF t_vbeln,
          i_ent      TYPE TABLE OF t_ent,
          l_ent      TYPE t_ent.

    CHECK NOT vbeln IS INITIAL.

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
      i_entregas = zcl_ap_lista=>lista2tabla_n10( lista = l_lista ).
      DESCRIBE TABLE i_entregas LINES sy-tfill.
      IF sy-tfill = 1.
        READ TABLE i_entregas INTO l_vbeln INDEX 1.
        CALL FUNCTION 'RV_CALL_DISPLAY_TRANSACTION'
          EXPORTING
            vbeln = l_vbeln.
      ELSEIF sy-tfill > 1.
        SELECT * FROM likp
          INTO CORRESPONDING FIELDS OF TABLE i_ent
          FOR ALL ENTRIES IN i_entregas
         WHERE vbeln = i_entregas-vbeln
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
              titulo         = 'Seleccione entrega a visualizar'(spv)
              campos_hotspot = 'VBELN'
              ancho          = 40
              botones        = 'CANCEL'
            TABLES
              t_datos        = i_ent.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    " handle_double_click
  METHOD vlpod.
    DATA: l_tabla TYPE tabname VALUE 'VBUK',
          o_bi    TYPE REF TO zcl_ap_batch_input,
          l_tvpod TYPE tvpod,
          l_posnr TYPE posnr,
          l_pos   TYPE numc2,
          l_aux   TYPE posnr,
          l_npos  TYPE i,
          l_pdstk TYPE vbuk-pdstk.

    FIELD-SYMBOLS <pos> TYPE t_vlpod.

    IF zcl_c=>hana = 'X'.
      l_tabla = 'LIKP'.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPMV50A' dynpro = '4006' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

    LOOP AT i_pos ASSIGNING <pos>.
      SELECT SINGLE lfimg_diff calcu FROM tvpod "#EC CI_SEL_NESTED "#EC CI_NOFIELD
        INTO CORRESPONDING FIELDS OF l_tvpod
       WHERE vbeln = vbeln
         AND posnr = <pos>-posnr.
      IF sy-subrc = 0.
        IF <pos>-diff = l_tvpod-lfimg_diff.
          IF    ( <pos>-grund = 'DFG1' AND l_tvpod-calcu = '+' )
             OR ( <pos>-grund = 'DFG2' AND l_tvpod-calcu = '-' ).
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      IF <pos>-posnr < '900000'.
        l_posnr = <pos>-posnr.
      ELSE.
        SELECT SINGLE uecha FROM lips                "#EC CI_SEL_NESTED
          INTO l_posnr                                 ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
         WHERE vbeln = vbeln
           AND posnr = <pos>-posnr.
      ENDIF.

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=POPO_T' ).

      o_bi->dynpro( program = 'SAPMV50A' dynpro = '0111' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=WEIT' ).
      o_bi->campos( campo = 'RV50A-POSNR' valor = l_posnr ).

      IF <pos>-posnr < '900000'.
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'TVPODVB-GRUND(01)' valor = <pos>-grund ).
        o_bi->campos( campo = 'TVPODVB-LFIMG_DIFF(01)' valor = <pos>-diff ).
        o_bi->campos( campo = 'TVPODVB-LGMNG_DIFF(01)' valor = '' ind = l_pos ).
      ELSE.
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'BDC_CURSOR' valor = 'RV50A-CHMULT(01)' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=CHPL_T01' ).

        l_pos = 1.
        SELECT posnr FROM lips                       "#EC CI_SEL_NESTED  "#EC CI_EXIT_SELECT
          INTO l_aux                                   ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
         WHERE vbeln = vbeln
           AND uecha = l_posnr
         ORDER BY posnr.
          l_pos = l_pos + 1.
          IF l_aux = <pos>-posnr.
            EXIT.
          ENDIF.
        ENDSELECT.

        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
        o_bi->campos( campo = 'TVPODVB-GRUND' valor = <pos>-grund ind = l_pos ).
        o_bi->campos( campo = 'TVPODVB-LFIMG_DIFF' valor = <pos>-diff ind = l_pos ).

      ENDIF.
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK_T' ).
      l_npos = l_npos + 1.
    ENDLOOP.

    IF l_npos = 0.
      o_log->log( p1 = 'No habia ninguna posición con modificaciones ARE' p2 = vbeln msgty = 'S' ).
      IF confirmar_are = 'X'.
        SELECT SINGLE pdstk FROM (l_tabla)
          INTO l_pdstk
         WHERE vbeln = vbeln.
        IF l_pdstk <> 'C'.
          o_bi->inicio( ).
          o_bi->dynpro( program = 'SAPMV50A' dynpro = '4006' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
*        o_bi->campos( campo = 'BDC_CURSOR' valor = 'TVPODVB-GRUND').
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=PODQ' ).
          IF NOT podat IS INITIAL.
            o_bi->campos( campo = 'LIKP-PODAT' valor = podat ).
          ENDIF.
          IF NOT potim IS INITIAL.
            o_bi->campos( campo = 'LIKP-POTIM' valor = potim ).
          ENDIF.

          o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

          message = o_bi->llamar_transaccion( tcode = 'VLPOD' modo = modoct ).
          SELECT SINGLE pdstk FROM (l_tabla)
            INTO l_pdstk
           WHERE vbeln = vbeln.
          IF l_pdstk = 'C'.
            CLEAR message.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      IF NOT podat IS INITIAL.
        o_bi->campos( campo = 'LIKP-PODAT' valor = podat ).
      ENDIF.
      IF NOT potim IS INITIAL.
        o_bi->campos( campo = 'LIKP-POTIM' valor = potim ).
      ENDIF.
      IF confirmar_are = 'X'.
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=PODQ' ).
        o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
      ENDIF.
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

      message = o_bi->llamar_transaccion( tcode = 'VLPOD' modo = modoct ).

      IF o_bi->msgty = 'S'.
        CLEAR message.
      ENDIF.

      IF message IS INITIAL.
        LOOP AT i_pos ASSIGNING <pos>.
          SELECT lfimg_diff, calcu
            FROM tvpod               "#EC CI_SEL_NESTED "#EC CI_NOFIELD
            WHERE vbeln = @vbeln
              AND posnr = @<pos>-posnr
            ORDER BY PRIMARY KEY
            INTO CORRESPONDING FIELDS OF @l_tvpod
            UP TO 1 ROWS.
          ENDSELECT.
          IF sy-subrc = 0.
            IF <pos>-diff = l_tvpod-lfimg_diff.
              IF    ( <pos>-grund = 'DFG1' AND l_tvpod-calcu = '+' )
                 OR ( <pos>-grund = 'DFG2' AND l_tvpod-calcu = '-' ).
                CONTINUE.
              ENDIF.
            ENDIF.
            message = 'No se han podido realizar todas las modificaciones ARE'.
          ELSE.
            message = 'No se han podido realizar todas las modificaciones ARE'.
          ENDIF.
        ENDLOOP.

        IF l_npos = 0 AND message IS INITIAL AND confirmar_are = 'X'.
          SELECT SINGLE pdstk FROM (l_tabla)
            INTO l_pdstk
           WHERE vbeln = vbeln.
          IF l_pdstk <> 'C'.
            o_bi->inicio( ).
            o_bi->dynpro( program = 'SAPMV50A' dynpro = '4006' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
            o_bi->campos( campo = 'LIKP-VBELN' valor = vbeln ). " Entrega

            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_CURSOR' valor = 'TVPODVB-GRUND' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=PODQ' ).
            IF NOT podat IS INITIAL.
              o_bi->campos( campo = 'LIKP-PODAT' valor = podat ).
            ENDIF.
            IF NOT potim IS INITIAL.
              o_bi->campos( campo = 'LIKP-POTIM' valor = potim ).
            ENDIF.

            o_bi->dynpro( program = 'SAPMV50A' dynpro = '1000' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=SICH_T' ).

            message = o_bi->llamar_transaccion( tcode = 'VLPOD' modo = modoct ).
            SELECT SINGLE pdstk FROM (l_tabla)
              INTO l_pdstk
             WHERE vbeln = vbeln.
            IF l_pdstk = 'C'.
              CLEAR message.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF NOT o_log IS INITIAL.
        IF message IS INITIAL.
          o_log->log( p1 = 'Se ha realizado correctamente el acuse de recibo de la entrega' p2 = vbeln msgty = 'S' ).
        ELSE.
          o_log->log( p1 = message ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
