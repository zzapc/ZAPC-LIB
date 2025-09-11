CLASS zcl_ap_pedido_mm DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_dif_ctd,
        ebelp    TYPE ebelp,
        menge_ok TYPE ekpo-menge,
        menge_bd TYPE ekpo-menge,
        meins    TYPE ekpo-meins,
      END OF t_dif_ctd.
    TYPES tt_dif_ctd           TYPE TABLE OF t_dif_ctd.
    TYPES tt_rel_final         TYPE TABLE OF bapirlcorq.
    TYPES tt_rel_info_gen      TYPE TABLE OF bapirlgnrq.
    TYPES tt_rel_posted        TYPE TABLE OF bapirlcorq.
    TYPES tt_rel_prerequisites TYPE TABLE OF bapirlcorq.

    CONSTANTS c_objectclas TYPE cdhdr-objectclas VALUE 'EINKBELEG' ##NO_TEXT.

    DATA i_errores TYPE wty_return_tab.

    CONSTANTS c_hist_factura TYPE bewtp              VALUE 'Q' ##NO_TEXT.
    CONSTANTS c_hist_entrega TYPE bewtp              VALUE 'E' ##NO_TEXT.
    CONSTANTS c_objeto       TYPE srgbtbrel-typeid_a VALUE 'BUS2012' ##NO_TEXT.
    CONSTANTS c_hist_salidas TYPE bewtp              VALUE 'U' ##NO_TEXT.

    DATA bapiret2  TYPE bapiret2.
    DATA message   TYPE bapi_msg.
    DATA i_dif_ctd TYPE tt_dif_ctd.

    CLASS-METHODS urls_gos_st
      IMPORTING ebeln        TYPE ekko-ebeln
      RETURNING VALUE(tabla) TYPE ztab_url_gos.

    CLASS-METHODS insertar_url_gos_st
      IMPORTING ebeln  TYPE ekko-ebeln
                url    TYPE string
                titulo TYPE string.

    CLASS-METHODS get_estado_liberacion
      IMPORTING ebeln         TYPE ebeln
      RETURNING VALUE(estado) TYPE char1.

    CLASS-METHODS visualizar_pedido_st
      IMPORTING ebeln TYPE ekko-ebeln
                ebelp TYPE ekpo-ebelp OPTIONAL.

    CLASS-METHODS get_url_por_titulo_st
      IMPORTING ebeln      TYPE ekko-ebeln
                titulo     TYPE string
      RETURNING VALUE(url) TYPE string.

    CLASS-METHODS es_borrado
      IMPORTING ebeln          TYPE ebeln
      RETURNING VALUE(borrado) TYPE abap_bool.

    CLASS-METHODS visualizar
      IMPORTING ebeln TYPE any   OPTIONAL
                ebelp TYPE ebelp OPTIONAL
                !edit TYPE xfeld DEFAULT ''
                  PREFERRED PARAMETER ebeln.

    CLASS-METHODS get_estrategias
      IMPORTING ebeln         TYPE ekko-ebeln
                info_usuarios TYPE abap_bool DEFAULT ''
      RETURNING VALUE(t_cod)  TYPE ztab_cod.

    CLASS-METHODS get_texto
      IMPORTING ebeln         TYPE ebeln
                ebelp         TYPE ebelp        OPTIONAL
                !id           TYPE stxh-tdid
                spras         TYPE stxh-tdspras OPTIONAL
      RETURNING VALUE(lineas) TYPE oij_tline.

    CLASS-METHODS save_texto
      IMPORTING ebeln  TYPE ebeln
                ebelp  TYPE ebelp        OPTIONAL
                !id    TYPE stxh-tdid
                spras  TYPE stxh-tdspras OPTIONAL
                lineas TYPE oij_tline.

    METHODS cambiar_pedido
      IMPORTING ebeln            TYPE ebeln
                i_campos         TYPE ztab_campos_cambio_pedido
      RETURNING VALUE(i_errores) TYPE wty_return_tab.

    METHODS get_error_formateado
      RETURNING VALUE(message) TYPE bapireturn1-message.

    CLASS-METHODS get_valor_historial
      IMPORTING ebeln        TYPE ebeln
                ebelp        TYPE ebelp     OPTIONAL
                bewtp        TYPE bewtp
                cantidad     TYPE abap_bool DEFAULT ''
      RETURNING VALUE(valor) TYPE tslvt.

    CLASS-METHODS get_valor_facturas
      IMPORTING ebeln        TYPE ebeln
                ebelp        TYPE ebelp     OPTIONAL
                cantidad     TYPE abap_bool DEFAULT ''
      RETURNING VALUE(valor) TYPE tslvt.

    CLASS-METHODS get_valor_entregas
      IMPORTING ebeln        TYPE ebeln
                ebelp        TYPE ebelp     OPTIONAL
                cantidad     TYPE abap_bool DEFAULT ''
      RETURNING VALUE(valor) TYPE tslvt.

    CLASS-METHODS es_liberado_cod
      IMPORTING t_cod           TYPE ztab_cod
      RETURNING VALUE(liberado) TYPE abap_bool.

    CLASS-METHODS atta_gos_st
      IMPORTING ebeln        TYPE ebeln
      RETURNING VALUE(tabla) TYPE ztab_url_gos.

    CLASS-METHODS get_valor_condicion
      IMPORTING ebeln        TYPE ekpo-ebeln
                ebelp        TYPE ekpo-ebelp
                kschl        TYPE kschl
                campo        TYPE string DEFAULT 'KWERT'
      RETURNING VALUE(valor) TYPE komv-kwert.

    CLASS-METHODS get_valor_salidas
      IMPORTING ebeln        TYPE ebeln
                ebelp        TYPE ebelp     OPTIONAL
                cantidad     TYPE abap_bool DEFAULT ''
      RETURNING VALUE(valor) TYPE tslvt.

    METHODS crear_pedido
      IMPORTING ekko                 TYPE ekko
                i_ekpo               TYPE ekpo_tty
                i_eket               TYPE eket_tt           OPTIONAL
                eikp                 TYPE eikp              OPTIONAL
                i_ekkn               TYPE ty_ekkn           OPTIONAL
                i_ekpa               TYPE meout_t_ekpa      OPTIONAL
                i_ekpv               TYPE mmpr_ekpv         OPTIONAL
                o_log                TYPE REF TO zcl_ap_log OPTIONAL
                no_warnings          TYPE abap_bool         DEFAULT 'X'
                !commit              TYPE abap_bool         DEFAULT 'X'
                verificar_cantidades TYPE abap_bool         DEFAULT ''
      RETURNING VALUE(ebeln)         TYPE ebeln.

    METHODS modificar_pedido
      IMPORTING ekko                   TYPE ekko
                i_ekpo                 TYPE ekpo_tty              OPTIONAL
                i_eket                 TYPE eket_tt               OPTIONAL
                eikp                   TYPE eikp                  OPTIONAL
                i_ekkn                 TYPE ty_ekkn               OPTIONAL
                i_ekpa                 TYPE meout_t_ekpa          OPTIONAL
                i_konv                 TYPE tab_konv              OPTIONAL
                !commit                TYPE abap_bool             DEFAULT 'X'
                o_log                  TYPE REF TO zcl_ap_log     OPTIONAL
                no_warnings            TYPE abap_bool             DEFAULT 'X'
                campos_usuario         TYPE abap_bool             DEFAULT ''
                i_ekpv                 TYPE mmpr_ekpv             OPTIONAL
                no_aut                 TYPE abap_bool             DEFAULT ''
                error_si_no_mod_campos TYPE abap_bool             DEFAULT ''
                i_components           TYPE bapimepo_t_component  OPTIONAL
                i_componentsx          TYPE bapimepo_t_componentx OPTIONAL
      RETURNING VALUE(message)         TYPE bapiret2-message.

    CLASS-METHODS get_texto_string
      IMPORTING ebeln         TYPE ebeln
                ebelp         TYPE ebelp        OPTIONAL
                !id           TYPE stxh-tdid
                spras         TYPE stxh-tdspras OPTIONAL
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS modificar_reparto
      IMPORTING ebeln          TYPE eket-ebeln
                ebelp          TYPE eket-ebelp
                etenr          TYPE eket-etenr        DEFAULT '0001'
                eindt          TYPE eket-eindt        OPTIONAL
                !uzeit         TYPE eket-uzeit        OPTIONAL
                o_log          TYPE REF TO zcl_ap_log OPTIONAL
                borrar         TYPE abap_bool         DEFAULT ''
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS add_pos_pedido
      IMPORTING ebeln    TYPE eket-ebeln
                eindt    TYPE eindt     DEFAULT sy-datum
                !commit  TYPE abap_bool DEFAULT 'X'
      EXPORTING !message TYPE bapi_msg
      CHANGING  ekpo     TYPE ekpo.

    CLASS-METHODS borrar_pos_pedido
      IMPORTING ebeln          TYPE ebeln
                ebelp          TYPE ebelp             OPTIONAL
                o_log          TYPE REF TO zcl_ap_log OPTIONAL
                !commit        TYPE abap_bool         DEFAULT 'X'
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS get_info_lib
      IMPORTING ebeln             TYPE ebeln
                todas             TYPE abap_bool DEFAULT ''
                mostrar_ult_lib   TYPE abap_bool DEFAULT 'X'
      EXPORTING ult_lib           TYPE bapirlcorq-rel_code1
                ult_lib_desc      TYPE bapirlcorq-rel_cd_tx1
                sig_lib           TYPE bapirlcorq-rel_code1
                sig_lib_desc      TYPE bapirlcorq-rel_cd_tx1
                fecha_ult_lib     TYPE dats
                hora_ult_lib      TYPE tims
                usuario_ult_lib   TYPE sy-uname
                liberado          TYPE abap_bool
                rel_info_gen      TYPE tt_rel_info_gen
                rel_prerequisites TYPE tt_rel_prerequisites
                rel_posted        TYPE tt_rel_posted
                rel_final         TYPE tt_rel_final.

    CLASS-METHODS liberar
      IMPORTING ebeln             TYPE ebeln
                rel_code          TYPE bapimmpara-po_rel_cod
                !commit           TYPE abap_bool DEFAULT 'X'
      EXPORTING rel_status_new    TYPE bapimmpara-rel_status
                rel_indicator_new TYPE bapimmpara-po_rel_ind
                !message          TYPE bapi_msg.

    CLASS-METHODS copiar_pedido
      IMPORTING ebeln     TYPE ebeln
                bsart_new TYPE bsart             OPTIONAL
                eindt_new TYPE eindt             OPTIONAL
                o_log     TYPE REF TO zcl_ap_log OPTIONAL
                bedat_new TYPE bedat             OPTIONAL
                lifnr_new TYPE lifnr             OPTIONAL
                pstyp_new TYPE ekpo-pstyp        DEFAULT '!'
                werks_new TYPE werks_d           DEFAULT ''
                lgort_new TYPE lgort_d           DEFAULT ''
                absgr_new TYPE absgr             OPTIONAL
                i_ekpo_i  TYPE ekpo_tty          OPTIONAL
                !commit   TYPE abap_bool         DEFAULT 'X'
      EXPORTING ebeln_new TYPE ebeln
                !message  TYPE bapi_msg
                i_errores TYPE wty_return_tab.

    CLASS-METHODS borrar_pedido
      IMPORTING ebeln     TYPE ebeln
                o_log     TYPE REF TO zcl_ap_log OPTIONAL
      EXPORTING !message  TYPE bapi_msg
                i_errores TYPE wty_return_tab.

    CLASS-METHODS get_datos_pedido_from_objeto
      IMPORTING im_header TYPE REF TO if_purchase_order_mm
                get_xekpo TYPE abap_bool DEFAULT ''
      EXPORTING ekko      TYPE ekko
                i_ekpo    TYPE me_ekpo
                i_ekpv    TYPE mmpr_ekpv
                xekpo     TYPE mmpr_uekpo.

    CLASS-METHODS get_entrega
      IMPORTING ebeln        TYPE ebeln OPTIONAL
                ebelp        TYPE ebelp OPTIONAL
                  PREFERRED PARAMETER ebeln
      RETURNING VALUE(vbeln) TYPE vbeln_vl.

    CLASS-METHODS marca_pos_entrega_final
      IMPORTING ebeln          TYPE ebeln
                ebelp          TYPE ebelp             OPTIONAL
                o_log          TYPE REF TO zcl_ap_log OPTIONAL
                !commit        TYPE abap_bool         DEFAULT 'X'
                elikz          TYPE elikz             DEFAULT 'X'
                eglkz          TYPE eglkz             DEFAULT '?'
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS copiar_posicion
      IMPORTING ebeln             TYPE ebeln
                ebelp             TYPE ebelp
                o_log             TYPE REF TO zcl_ap_log OPTIONAL
                matnr_new         TYPE matnr             OPTIONAL
                !commit           TYPE abap_bool         DEFAULT 'X'
                menge_new         TYPE any               DEFAULT 0
                konnr_new         TYPE konnr             OPTIONAL
                ktpnr_new         TYPE ktpnr             OPTIONAL
                borrar_pos_origen TYPE abap_bool         DEFAULT ''
      EXPORTING ebelp_new         TYPE ebelp
                !message          TYPE bapi_msg
                i_errores         TYPE wty_return_tab.

    CLASS-METHODS esta_bloqueado
      IMPORTING ebeln            TYPE ebeln
                borrar           TYPE abap_bool DEFAULT ''
      RETURNING VALUE(bloqueado) TYPE abap_bool.

    CLASS-METHODS espera_si_bloqueado
      IMPORTING ebeln           TYPE ebeln
                segundos_espera TYPE int2 DEFAULT 10
      RETURNING VALUE(message)  TYPE bapi_msg.

    CLASS-METHODS rechazar
      IMPORTING ebeln          TYPE ebeln
                !commit        TYPE abap_bool DEFAULT 'X'
                anular_rechazo TYPE abap_bool DEFAULT ''
      EXPORTING !message       TYPE bapi_msg.

    CLASS-METHODS get_adjuntos
      IMPORTING ebeln TYPE ebeln
      EXPORTING i_adj TYPE zcl_ap_gos=>tt_adj.

    CLASS-METHODS get_status_header
      IMPORTING ebeln         TYPE ebeln
      RETURNING VALUE(status) TYPE mepo_status.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA i_poheader     TYPE wrf_pohf_bapimepoheader_tty.
    DATA poheader       TYPE bapimepoheader.
    DATA i_poitem       TYPE wrf_pohf_bapimepoitem_tty.
    DATA poitem         TYPE bapimepoitem.
    DATA i_poheaderx    TYPE wrf_pohf_bapimepoheaderx_tty.
    DATA poheaderx      TYPE bapimepoheaderx.
    DATA i_poitemx      TYPE wrf_pohf_bapimepoitemx_tty.
    DATA poitemx        TYPE bapimepoitemx.
    DATA i_pocond       TYPE wspo_t_mepocond.
    DATA i_pocondx      TYPE wspo_t_mepocondx.
    DATA schedule       TYPE bapimeposchedule.
    DATA i_schedule     TYPE wspo_t_meposchedule.
    DATA schedulex      TYPE bapimeposchedulx.
    DATA i_schedulex    TYPE wspo_t_meposchedulex.
    DATA impheader      TYPE bapieikp.
    DATA impheaderx     TYPE bapieikpx.
    DATA account        TYPE bapimepoaccount.
    DATA i_account      TYPE bapimepoaccount_tp.
    DATA accountx       TYPE bapimepoaccountx.
    DATA i_accountx     TYPE bapimepoaccountx_tp.
    DATA shipping       TYPE bapiitemship.
    DATA i_shipping     TYPE bapiitemship_tp.
    DATA shippingx      TYPE bapiitemshipx.
    DATA i_shippingx    TYPE bapiitemshipx_tp.
    DATA address        TYPE bapimepoaddrdelivery.
    DATA i_address      TYPE wspo_t_mepoaddrdelivery.
    DATA i_partners     TYPE bapiekkop_tp.
    DATA i_extension_in TYPE t_bapiparex.
    DATA i_components   TYPE bapimepo_t_component.
    DATA i_componentsx  TYPE bapimepo_t_componentx.

    METHODS informar_tablas_bapi
      IMPORTING ekko           TYPE ekko
                i_ekpo         TYPE ekpo_tty     OPTIONAL
                i_eket         TYPE eket_tt      OPTIONAL
                eikp           TYPE eikp         OPTIONAL
                i_ekkn         TYPE ty_ekkn      OPTIONAL
                i_ekpa         TYPE meout_t_ekpa OPTIONAL
                i_konv         TYPE tab_konv     OPTIONAL
                i_ekpv         TYPE mmpr_ekpv    OPTIONAL
                campos_usuario TYPE abap_bool    DEFAULT ''.
endclass. "ZCL_AP_PEDIDO_MM definition
class ZCL_AP_PEDIDO_MM implementation.
  METHOD add_pos_pedido.
    DATA: l_tabla TYPE tabname,
          ekko    TYPE ekko,
          o_ped   TYPE REF TO zcl_ap_pedido_mm,
          l_ekpo  TYPE ekpo,
          i_ekpo  TYPE TABLE OF ekpo,
          eket    TYPE eket,
          i_eket  TYPE TABLE OF eket.

    l_tabla = 'EKKO'.
    SELECT SINGLE * FROM (l_tabla)
      INTO ekko
     WHERE ebeln = ebeln.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe el pedido'(nep) ebeln INTO message SEPARATED BY space.
    ELSE.
      o_ped = NEW #( ).

      ekpo-ebeln = ebeln.
      SELECT SINGLE ebelp FROM ekpo
        INTO ekpo-ebelp
       WHERE ebeln = ebeln
         AND ebelp = ekpo-ebelp.
      IF sy-subrc = 0 OR ekpo-ebelp IS INITIAL.
        SELECT MAX( ebelp ) FROM ekpo
          INTO ekpo-ebelp
         WHERE ebeln = ebeln.
        ekpo-ebelp = ekpo-ebelp + 10.
      ENDIF.

      l_tabla = 'EKPO'.
      SELECT * FROM (l_tabla)                 "#EC CI_ALL_FIELDS_NEEDED
        INTO l_ekpo
        UP TO 1 ROWS
       WHERE ebeln = ebeln
         AND loekz = ''
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        IF ekpo-werks IS INITIAL.
          ekpo-werks = l_ekpo-werks.
        ENDIF.
        IF ekpo-lgort IS INITIAL.
          ekpo-lgort = l_ekpo-lgort.
        ENDIF.
      ENDIF.

      IF ekpo-txz01 IS INITIAL.
        ekpo-txz01 = zcl_ap_material=>get_descripcion( ekpo-matnr ).
      ENDIF.
      APPEND ekpo TO i_ekpo.

      CLEAR eket.
      MOVE-CORRESPONDING ekpo TO eket.
      eket-etenr = '0001'.
      eket-eindt = eindt.
      APPEND eket TO i_eket.

      message = o_ped->modificar_pedido( ekko   = ekko
                                         i_ekpo = i_ekpo
                                         i_eket = i_eket
                                         commit = commit ).

    ENDIF.
  ENDMETHOD.
  METHOD atta_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = ebeln.
    tabla = zcl_ap_gos=>atta_gos_st( tipo = c_objeto clave = l_clave ).
  ENDMETHOD.
  METHOD borrar_pedido.
    DATA: l_tabla TYPE tabname,
          ekko    TYPE ekko,
          i_ekpo  TYPE TABLE OF ekpo,
          o_ped   TYPE REF TO zcl_ap_pedido_mm.

    FIELD-SYMBOLS: <ekpo>  TYPE ekpo,
                   <error> TYPE bapiret2.

    l_tabla = 'EKKO'.
    SELECT SINGLE * FROM (l_tabla)
      INTO ekko
     WHERE ebeln = ebeln.
    IF sy-subrc <> 0.
      __concat2 message 'No existe el pedido'(nep) ebeln.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message ).
      ENDIF.
      return.
    ENDIF.

    l_tabla = 'EKPO'.
    SELECT * FROM (l_tabla)
      INTO TABLE i_ekpo
     WHERE ebeln = ebeln
       AND loekz = ''.
    IF sy-subrc <> 0.
      __concat3 message 'Pedido'(ped) ebeln 'no tiene posiciones sin borrar'(npb).
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message ).
      ENDIF.
      return.
    ENDIF.

    LOOP AT i_ekpo ASSIGNING <ekpo>.
      <ekpo>-loekz = 'L'.
    ENDLOOP.

    o_ped = NEW #( ).
    o_ped->modificar_pedido( ekko   = ekko
                             i_ekpo = i_ekpo
                             o_log  = o_log ).

    LOOP AT o_ped->i_errores ASSIGNING <error> WHERE type = 'E'.
      __concat_a message <error>-message.
    ENDLOOP.
  ENDMETHOD.
  METHOD borrar_pos_pedido.
    DATA: l_tabla TYPE tabname,
          ekko    TYPE ekko,
          o_ped   TYPE REF TO zcl_ap_pedido_mm,
          l_ekpo  TYPE ekpo,
          i_ekpo  TYPE TABLE OF ekpo,
          i_eket  TYPE TABLE OF eket.

    l_tabla = 'EKKO'.
    SELECT SINGLE * FROM (l_tabla)
      INTO ekko
     WHERE ebeln = ebeln.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe el pedido'(nep) ebeln INTO message SEPARATED BY space.
    ELSE.
      o_ped = NEW #( ).

      l_tabla = 'EKPO'.
      SELECT SINGLE * FROM (l_tabla)
      INTO l_ekpo
     WHERE ebeln = ebeln
       AND ebelp = ebelp.
      IF sy-subrc <> 0.
        message = 'Posición de pedido no existe'(ppn).
      ENDIF.

      IF l_ekpo-loekz IS NOT INITIAL.
        RETURN.
      ENDIF.

      l_ekpo-loekz = 'L'.
      APPEND l_ekpo TO i_ekpo.

      message = o_ped->modificar_pedido( ekko   = ekko
                                         i_ekpo = i_ekpo
                                         i_eket = i_eket ).

    ENDIF.

    IF NOT o_log IS INITIAL.
      IF NOT message IS INITIAL.
        o_log->log( p1 = 'Error borrando pos.'(ebp) p2 = ebelp p3 = 'de pedido'(dpe) p4 = ebeln p5 = message ).
      ELSE.
        o_log->log( p1 = 'Se ha borrado la posicion'(sbp) p2 = ebelp p3 = 'de pedido'(dpe) p4 = ebeln msgty = 'S' ).
      ENDIF.
    ENDIF.

    IF message IS INITIAL AND commit = 'X'.
      zcl_ap_dev=>commit( ).
    ENDIF.
  ENDMETHOD.
  METHOD cambiar_pedido.
    DATA: l_campo TYPE zest_campos_cambio_pedido,
          l_aux   TYPE string,
          l_error TYPE bapiret2.

    FIELD-SYMBOLS <fs> TYPE any.

    CLEAR: i_poheader,
           i_poheaderx,
           i_poitem,
           i_poitemx.

    LOOP AT i_campos INTO l_campo WHERE ebelp IS INITIAL.
      CONCATENATE 'POHEADER-' l_campo-campo INTO l_aux.
      ASSIGN (l_aux) TO <fs>.
      IF <fs> IS ASSIGNED.
        <fs> = l_campo-valor.
      ELSE.
        l_error-type = 'E'.
        CONCATENATE 'Campo'(cam) l_campo-campo
                    'no existe en estructura POHEADER'(neh)
                    INTO l_error-message SEPARATED BY space.
        APPEND l_error TO i_errores.
      ENDIF.

      CONCATENATE 'POHEADERX-' l_campo-campo INTO l_aux.
      ASSIGN (l_aux) TO <fs>.
      IF <fs> IS ASSIGNED.
        <fs> = l_campo-valor.
      ENDIF.

    ENDLOOP.

    LOOP AT i_campos INTO l_campo WHERE NOT ebelp IS INITIAL.
      CLEAR poitem.
      poitem-po_item = l_campo-ebelp.
      IF l_campo-delete_ind = 'X'.
        poitem-delete_ind = l_campo-delete_ind.
        APPEND poitem TO i_poitem.
      ELSE.
        CONCATENATE 'POITEM-' l_campo-campo INTO l_aux.
        ASSIGN (l_aux) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = l_campo-valor.
          APPEND poitem TO i_poitem.
        ELSE.
          l_error-type = 'E'.
          CONCATENATE 'Campo'(cam) l_campo-campo 'no existe en estructura POITEM'(nei)
                      INTO l_error-message SEPARATED BY space.
          APPEND l_error TO i_errores.
        ENDIF.
      ENDIF.

      CLEAR poitemx.
      poitemx-po_item = l_campo-ebelp.
      IF l_campo-delete_ind = 'X'.
        poitemx-delete_ind = l_campo-delete_ind.
        APPEND poitemx TO i_poitemx.
      ELSE.
        CONCATENATE 'POITEMX-' l_campo-campo INTO l_aux.
        ASSIGN (l_aux) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = 'X'.
          APPEND poitemx TO i_poitemx.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF i_errores IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = ebeln
        poheader      = poheader
        poheaderx     = poheaderx
      TABLES
        return        = i_errores
        poitem        = i_poitem
        poitemx       = i_poitemx.

    me->i_errores = i_errores.

    IF NOT line_exists( i_errores[ type = 'E' ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD copiar_pedido.
    DATA: l_tabla TYPE tabname,
          ekko    TYPE ekko,
          i_ekpo  TYPE TABLE OF ekpo,
          i_eket  TYPE TABLE OF eket,
          i_ekkn  TYPE TABLE OF ekkn,
          i_ekpa  TYPE TABLE OF ekpa,
          o_ped   TYPE REF TO zcl_ap_pedido_mm,
          eikp    TYPE eikp.

    FIELD-SYMBOLS: <ekpo>  TYPE ekpo,
                   <eket>  TYPE eket,
                   <ekpa>  TYPE ekpa,
                   <ekkn>  TYPE ekkn,
                   <error> TYPE bapiret2.

    l_tabla = 'EKKO'.
    SELECT SINGLE * FROM (l_tabla)
      INTO ekko
     WHERE ebeln = ebeln.
    IF sy-subrc <> 0.
      __concat2 message 'No existe el pedido'(nep) ebeln.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message ).
      ENDIF.
      return.
    ENDIF.

    IF NOT i_ekpo_i IS INITIAL.
      i_ekpo = i_ekpo_i.
    ELSE.
      l_tabla = 'EKPO'.
      SELECT * FROM (l_tabla)
        INTO TABLE i_ekpo
       WHERE ebeln = ebeln
         AND loekz = ''.
      IF sy-subrc <> 0.
        __concat3 message 'Pedido'(ped) ebeln 'no tiene posiciones sin borrar'(ntp).
        IF NOT o_log IS INITIAL.
          o_log->log( p1 = message ).
        ENDIF.
        return.
      ENDIF.

      LOOP AT i_ekpo ASSIGNING <ekpo>.
        CLEAR <ekpo>-ebeln.
        IF NOT werks_new IS INITIAL.
          IF <ekpo>-werks = <ekpo>-berid.
            <ekpo>-berid = werks_new.
          ENDIF.
          <ekpo>-werks = werks_new.
        ENDIF.
        IF NOT lgort_new IS INITIAL.
          <ekpo>-lgort = lgort_new.
        ENDIF.
        IF pstyp_new <> '!'.
          <ekpo>-pstyp = pstyp_new.
        ENDIF.

        CLEAR: <ekpo>-banfn,
               <ekpo>-bnfpo.
      ENDLOOP.
    ENDIF.

    l_tabla = 'EKET'.
    SELECT * FROM (l_tabla)
      INTO TABLE i_eket
     WHERE ebeln = ebeln.

    IF NOT bsart_new IS INITIAL.
      ekko-bsart = bsart_new.
    ENDIF.

    DATA(l_lifnr) = ekko-lifnr.
    IF NOT lifnr_new IS INITIAL.
      ekko-lifnr = lifnr_new.
      IF ekko-lifre = l_lifnr.
        ekko-lifre = lifnr_new.
      ENDIF.
    ENDIF.

    IF NOT bedat_new IS INITIAL.
      ekko-bedat = bedat_new.
    ENDIF.

    IF NOT absgr_new IS INITIAL.
      ekko-absgr = absgr_new.
    ENDIF.

    IF NOT eindt_new IS INITIAL.
      LOOP AT i_eket ASSIGNING <eket>.
        CLEAR <eket>-ebeln.
        <eket>-eindt = eindt_new.
      ENDLOOP.
    ENDIF.

*    SELECT SINGLE * FROM eikp
*      INTO eikp
*     WHERE ebeln = ebeln.

    l_tabla = 'EKKN'.
    SELECT * FROM (l_tabla)
      INTO TABLE i_ekkn
     WHERE ebeln = ebeln.

    l_tabla = 'EKPA'.
    SELECT * FROM (l_tabla)
      INTO TABLE i_ekpa
     WHERE ebeln = ebeln.
    IF NOT lifnr_new IS INITIAL.
      LOOP AT i_ekpa ASSIGNING <ekpa> WHERE lifn2 = l_lifnr.
        <ekpa>-lifn2 = lifnr_new.
      ENDLOOP.
    ENDIF.

    CLEAR ekko-ebeln.

    LOOP AT i_ekkn ASSIGNING <ekkn>. CLEAR <ekkn>-ebeln. ENDLOOP.
    LOOP AT i_ekpa ASSIGNING <ekpa>. CLEAR <ekpa>-ebeln. ENDLOOP.
    o_ped = NEW #( ).
    ebeln_new = o_ped->crear_pedido( ekko   = ekko
                                     i_ekpo = i_ekpo
                                     i_eket = i_eket
                                     eikp   = eikp
                                     i_ekkn = i_ekkn
                                     i_ekpa = i_ekpa
                                     o_log  = o_log
                                     commit = commit ).

    LOOP AT o_ped->i_errores ASSIGNING <error> WHERE type = 'E'.
      __concat_a message <error>-message.
    ENDLOOP.
  ENDMETHOD.
  METHOD copiar_posicion.
    DATA: l_tabla TYPE tabname,
          ekko    TYPE ekko,
          i_ekpo  TYPE TABLE OF ekpo,
          l_ekpo  TYPE ekpo,
          l_eket  TYPE eket,
          i_eket  TYPE TABLE OF eket,
          i_ekkn  TYPE TABLE OF ekkn,
          i_ekpa  TYPE TABLE OF ekpa,
          o_ped   TYPE REF TO zcl_ap_pedido_mm,
          eikp    TYPE eikp.

    FIELD-SYMBOLS: <ekpo>  TYPE ekpo,
                   <ekkn>  TYPE ekkn,
                   <ekpa>  TYPE ekpa,
                   <error> TYPE bapiret2.

    IF NOT matnr_new IS INITIAL.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = 'Copiamos posicion'(cpo) p2 = ebelp p3 = 'a nueva posición con nuevo material'(npm) p4 = matnr_new p5 = menge_new msgty = 'I' ).
      ENDIF.
    ENDIF.

    l_tabla = 'EKKO'.
    SELECT SINGLE * FROM (l_tabla)
      INTO ekko
     WHERE ebeln = ebeln.
    IF sy-subrc <> 0.
      __concat2 message 'No existe el pedido'(nep) ebeln.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message ).
      ENDIF.
      return.
    ENDIF.

    SELECT * FROM ekpo
      INTO TABLE i_ekpo
     WHERE ebeln = ebeln
       AND ebelp = ebelp.
*     AND loekz = ''.
    IF sy-subrc <> 0.
      __concat2 message 'No existe la posicion'(nps) ebelp.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message ).
      ENDIF.
      return.
    ENDIF.

    LOOP AT i_ekpo ASSIGNING <ekpo>.
      DATA(l_ekpo_orig) = <ekpo>.
      IF NOT matnr_new IS INITIAL.
        <ekpo>-matnr = matnr_new.
        <ekpo>-ematn = matnr_new.
        <ekpo>-txz01 = zcl_ap_material=>get_descripcion( matnr_new ).
      ENDIF.
      IF NOT menge_new IS INITIAL.
        <ekpo>-menge = menge_new.
      ENDIF.
      SELECT MAX( ebelp ) FROM ekpo
        INTO ebelp_new
       WHERE ebeln = ebeln.
      ebelp_new = ebelp_new + 10.
      <ekpo>-ebelp = ebelp_new.
      CLEAR <ekpo>-loekz.
    ENDLOOP.

    IF borrar_pos_origen = 'X' AND l_ekpo_orig-loekz <> 'L'.
      l_ekpo_orig-loekz = 'L'.
      APPEND l_ekpo_orig TO i_ekpo.
    ENDIF.

    IF NOT <ekpo> IS INITIAL.
      IF NOT matnr_new IS INITIAL OR NOT menge_new IS INITIAL.
        l_ekpo = <ekpo>.
        CLEAR <ekpo>.
        <ekpo>-ebeln = l_ekpo-ebeln.
        <ekpo>-ebelp = l_ekpo-ebelp.
        <ekpo>-matnr = l_ekpo-matnr.
        <ekpo>-menge = l_ekpo-menge.
        <ekpo>-meins = l_ekpo-meins.
        <ekpo>-werks = l_ekpo-werks.
        <ekpo>-lgort = l_ekpo-lgort.
      ENDIF.

      IF NOT konnr_new IS INITIAL.
        <ekpo>-konnr = konnr_new.
        <ekpo>-ktpnr = ktpnr_new.
      ENDIF.

      MOVE-CORRESPONDING <ekpo> TO l_eket.
      SELECT eindt FROM eket
        INTO l_eket-eindt
        UP TO 1 ROWS
       WHERE ebeln = ebeln
         AND ebelp = ebelp
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      l_eket-etenr = '0001'.
      l_eket-ebelp = ebelp_new.
      APPEND l_eket TO i_eket.
    ENDIF.

    SELECT * FROM ekkn
      INTO TABLE i_ekkn
     WHERE ebeln = ebeln
       AND ebelp = ebelp.

    LOOP AT i_ekkn ASSIGNING <ekkn>.
      <ekkn>-ebelp = ebelp_new.
    ENDLOOP.

    l_tabla = 'EKPA'.
    SELECT * FROM (l_tabla)
      INTO TABLE i_ekpa
     WHERE ebeln = ebeln
       AND ebelp = ebelp.
    LOOP AT i_ekpa ASSIGNING <ekpa>.
      <ekpa>-ebelp = ebelp_new.
    ENDLOOP.

    o_ped = NEW #( ).
    o_ped->modificar_pedido( ekko   = ekko
                             i_ekpo = i_ekpo
                             i_eket = i_eket
                             eikp   = eikp
                             i_ekkn = i_ekkn
                             i_ekpa = i_ekpa
                             o_log  = o_log
                             commit = commit ).

    LOOP AT o_ped->i_errores ASSIGNING <error> WHERE type = 'E'.
      __concat_a message <error>-message.
    ENDLOOP.
  ENDMETHOD.
  METHOD crear_pedido.
    DATA l_dif_ctd TYPE t_dif_ctd.

    FIELD-SYMBOLS <ekpo> TYPE ekpo.

    CLEAR: ebeln,
           message,
           i_dif_ctd.
    informar_tablas_bapi( ekko   = ekko
                          eikp   = eikp
                          i_ekpo = i_ekpo
                          i_eket = i_eket
                          i_ekkn = i_ekkn
                          i_ekpa = i_ekpa
                          i_ekpv = i_ekpv ).

    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = poheader
        poheaderx        = poheaderx
*       POADDRVENDOR     =
*       TESTRUN          =
*       MEMORY_UNCOMPLETE =
*       MEMORY_COMPLETE  =
        poexpimpheader   = impheader
        poexpimpheaderx  = impheaderx
*       VERSIONS         =
*       NO_MESSAGING     =
*       NO_MESSAGE_REQ   =
*       NO_AUTHORITY     =
*       NO_PRICE_FROM_PO =
      IMPORTING
        exppurchaseorder = ebeln
*       EXPHEADER        =
*       EXPPOEXPIMPHEADER =
      TABLES
        return           = i_errores
        poitem           = i_poitem
        poitemx          = i_poitemx
        poaddrdelivery   = i_address
        pocond           = i_pocond
        pocondx          = i_pocondx
        poschedule       = i_schedule
        poschedulex      = i_schedulex
        poaccount        = i_account
        poaccountx       = i_accountx
        poshipping       = i_shipping
        poshippingx      = i_shippingx
        popartner        = i_partners.

    IF NOT ebeln IS INITIAL.
      IF NOT verificar_cantidades IS INITIAL.
        LOOP AT i_ekpo ASSIGNING <ekpo>.
          READ TABLE i_poitem INTO poitem WITH KEY po_item = <ekpo>-ebelp.
          IF sy-subrc = 0.
            IF <ekpo>-meins <> poitem-po_unit.
*            poitem-quantity = zcl_ap_material=>convertir_unidad( matnr = <ekpo>-matnr unidad_origen = poitem-po_unit unidad_destino = <ekpo>-meins cantidad = poitem-quantity ).
              DATA(l_menge) = zcl_ap_material=>convertir_unidad( matnr          = <ekpo>-matnr
                                                                 unidad_origen  = <ekpo>-meins
                                                                 unidad_destino = poitem-po_unit
                                                                 cantidad       = <ekpo>-menge ).
              DATA(l_meins) = poitem-po_unit.
            ELSE.
              l_menge = <ekpo>-menge.
              l_meins = <ekpo>-meins.
            ENDIF.
            IF l_menge <> poitem-quantity.
* Verificamos si el registro info tiene perfil de redondeo y si es así ignoramos la diferencia
              SELECT rdprf FROM  eine JOIN eina ON eine~infnr = eina~infnr
                INTO @DATA(l_rdprf)
                UP TO 1 ROWS
               WHERE matnr      = @<ekpo>-matnr
                 AND lifnr      = @ekko-lifnr
                 AND eina~loekz = ''
                 AND eine~loekz = ''
                 AND ekorg      = @ekko-ekorg
                 AND esokz      = '0'
                 AND werks      = @<ekpo>-werks.
              ENDSELECT.
              IF sy-subrc = 0 AND NOT l_rdprf IS INITIAL.
                CONTINUE.
              ENDIF.

              CLEAR l_dif_ctd.
              l_dif_ctd-ebelp    = <ekpo>-ebelp.
              l_dif_ctd-menge_ok = l_menge.
              l_dif_ctd-menge_bd = poitem-quantity.
              l_dif_ctd-meins    = l_meins.
              APPEND l_dif_ctd TO i_dif_ctd.
            ENDIF.
          ELSE.
            CLEAR l_dif_ctd.
            l_dif_ctd-ebelp    = <ekpo>-ebelp.
            l_dif_ctd-menge_ok = <ekpo>-menge.
            l_dif_ctd-meins    = <ekpo>-meins.
            APPEND l_dif_ctd TO i_dif_ctd.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF commit = 'X'.
        zcl_ap_dev=>commit( ).
      ENDIF.
    ELSE.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.

* Borramos mensaje de ampliación
    DELETE i_errores WHERE type = 'W' AND message CS 'CI_EK'.
    DELETE i_errores WHERE message = 'No se ha creado ninguna instancia de tipo objeto PurchaseOrder; ref.externa:'(nrf).
    DELETE i_errores WHERE message CS 'ME_PROCESS_PO_CUST'.

    LOOP AT i_errores INTO bapiret2 WHERE type = 'E'.
      __add_lista message bapiret2-message.
    ENDLOOP.

    IF NOT i_dif_ctd IS INITIAL.
      bapiret2-type    = 'E'.
      bapiret2-message = 'Existen diferencias en cantidades creadas'(edc).
      APPEND bapiret2 TO i_errores.
    ENDIF.

    IF NOT o_log IS INITIAL.
      IF no_warnings = 'X'.
        DELETE i_errores WHERE type = 'W'.
      ENDIF.
      o_log->set_tabla_log_from_bapiret2_t( i_errores ).
    ENDIF.
  ENDMETHOD.
  METHOD es_borrado.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_ebeln TYPE ebeln.

    borrado = 'X'.
    SELECT SINGLE ebeln FROM ekpo
      INTO l_ebeln
     WHERE ebeln = ebeln
       AND loekz = ''.
    IF sy-subrc = 0.
      CLEAR borrado.
    ENDIF.
  ENDMETHOD.
  METHOD es_liberado_cod.
    DATA l_cod TYPE zest_cod.

    CLEAR liberado.
    DESCRIBE TABLE t_cod LINES sy-tfill.
    IF sy-tfill <= 0.
      RETURN.
    ENDIF.

    READ TABLE t_cod INTO l_cod INDEX sy-tfill.
    liberado = l_cod-liberada.
  ENDMETHOD.
  METHOD espera_si_bloqueado.
    DATA bloqueado TYPE c LENGTH 1.

    DO segundos_espera TIMES.
      bloqueado = esta_bloqueado( ebeln ).
      IF bloqueado = 'X'.
        __concat4 message 'Pedido' ebeln 'bloqueado por' sy-msgv1.
        WAIT UP TO 1 SECONDS.
      ELSE.
        CLEAR message.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD esta_bloqueado.
    bloqueado = zcl_ap_utils=>comprobar_bloqueo( tabla  = 'EKKO'
                                                 clave  = sy-mandt
                                                 clave2 = ebeln
                                                 borrar = borrar ).
  ENDMETHOD.
  METHOD get_adjuntos.
    zcl_ap_gos=>get_file_list( EXPORTING typeid       = c_objeto
                                         instid       = ebeln
                                         get_adj_mail = 'X'
                               CHANGING  i_adj        = i_adj ).
  ENDMETHOD.
  METHOD get_datos_pedido_from_objeto.
    DATA: header    TYPE mepoheader,
          items     TYPE purchase_order_items,
          line_item TYPE purchase_order_item,
          po_line   TYPE mepoitem,
          l_ekpo    TYPE ekpo,
          l_uekpo   TYPE uekpo,
          l_ekpv    TYPE ekpv.

    header = im_header->get_data( ).
    MOVE-CORRESPONDING header TO ekko.

    items = im_header->get_items( ).
    LOOP AT items INTO line_item.
      po_line = line_item-item->get_data( ).
      MOVE-CORRESPONDING po_line TO l_ekpo.
      APPEND l_ekpo TO i_ekpo.
      IF get_xekpo = 'X'.
        MOVE-CORRESPONDING l_ekpo TO l_uekpo.
        APPEND l_uekpo TO xekpo.
      ENDIF.

      l_ekpv = line_item-item->get_shipping_data( ).
      IF NOT l_ekpv IS INITIAL.
        APPEND l_ekpv TO i_ekpv.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_entrega.
    DATA: i_vbeln TYPE TABLE OF vbeln_vl,
          l_vbeln TYPE vbeln_vl.

    IF ebelp IS INITIAL.
      SELECT DISTINCT belnr FROM ekbe
        INTO TABLE i_vbeln
       WHERE ebeln = ebeln
         AND vgabe = '8'
         AND bewtp = 'L'
      ORDER BY belnr.
    ELSE.
      SELECT DISTINCT belnr FROM ekbe
        INTO TABLE i_vbeln
       WHERE ebeln = ebeln
         AND ebelp = ebelp
         AND vgabe = '8'
         AND bewtp = 'L'
       ORDER BY belnr.
    ENDIF.

    LOOP AT i_vbeln INTO l_vbeln.
      SELECT SINGLE vbeln FROM likp
        INTO l_vbeln
       WHERE vbeln = l_vbeln.
      IF sy-subrc = 0.
        vbeln = l_vbeln.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_error_formateado.
    DATA l_error TYPE bapiret2.
    DATA l_cont  TYPE i.

    CLEAR message.
    LOOP AT i_errores INTO l_error WHERE id = '06' AND number = '022'.
      l_error-type = 'E'.
      MODIFY i_errores FROM l_error.
    ENDLOOP.

    LOOP AT i_errores TRANSPORTING NO FIELDS WHERE type = 'E'.
      l_cont = l_cont + 1.
    ENDLOOP.
    IF l_cont > 1.
      DELETE i_errores WHERE id = 'BAPI'.
    ENDIF.

    LOOP AT i_errores INTO l_error WHERE type = 'E'.
      IF message IS INITIAL.
        message = l_error-message.
      ELSE.
        CONCATENATE message ',' INTO message.
        CONCATENATE message l_error-message INTO message
                    SEPARATED BY space.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_estado_liberacion.
    DATA ekko TYPE ekko.

    SELECT SINGLE frggr frgsx frgke frgzu frgrl FROM ekko
    INTO CORRESPONDING FIELDS OF ekko
    WHERE ebeln = ebeln.

    IF ekko-frggr IS INITIAL. " No hay estrategia
      estado = 'S'. " SIN ESTRATEGIA DE LIBERACION
    ELSE.
      IF ekko-frgrl = 'X'. " Liberación incompleta
        IF ekko-frgzu IS INITIAL. " No se ha efectuado ningún paso de liberación
          estado = 'I'. " LIBERACION NO INICIADA
        ELSE.
          estado = 'P'. " LIBERACION PARCIAL
        ENDIF.
      ELSE.
        estado = 'L'. " LIBERADO
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_estrategias.
    DATA: l_relp TYPE bapirlcopo,
          i_rel  TYPE TABLE OF bapirlcopo,
          l_rel  TYPE bapirlcopo,
          l_cod  TYPE zest_cod.
    DATA: i_cdpos TYPE iscdpos_tab,
          l_cdpos TYPE iscdpos,
          l_ind   TYPE i,
          l_lib   TYPE c LENGTH 10.

    CALL FUNCTION 'BAPI_PO_GETRELINFO'
      EXPORTING
        purchaseorder          = ebeln
*       PO_REL_CODE            =
      IMPORTING
*       GENERAL_RELEASE_INFO   =
*       RELEASE_PREREQUISITES  =
        release_already_posted = l_relp
      TABLES
        release_final          = i_rel.
*       RETURN                 =

    DEFINE get_rel.
      CLEAR l_cod.
      IF NOT l_rel-rel_code&1 IS INITIAL.
        l_cod-frgco = l_rel-rel_code&1.
        IF NOT l_relp-rel_code&1 IS INITIAL.
          l_cod-liberada = 'X'.
        ENDIF.
        APPEND l_cod TO t_cod.
      ENDIF.
    END-OF-DEFINITION.

    LOOP AT i_rel INTO l_rel.
      get_rel: 1, 2, 3, 4, 5, 6, 7, 8.
    ENDLOOP.

    IF info_usuarios = 'X'.
      i_cdpos =
       zcl_ap_control_cambios=>get_cdpos( objectclas = c_objectclas
                                          objectid   = ebeln
                                          tabname    = 'EKKO'
                                          fname      = 'FRGZU' ).
      CLEAR l_cdpos.
      LOOP AT t_cod INTO l_cod WHERE liberada = 'X'.
        l_ind = sy-tabix.
        CLEAR l_lib.
        DO l_ind TIMES.
          CONCATENATE 'X' l_lib INTO l_lib.
        ENDDO.
        READ TABLE i_cdpos INTO l_cdpos WITH KEY value_new = l_lib.
        IF sy-subrc = 0.
          l_cod-liberado_por = l_cdpos-username.
          l_cod-liberado_el  = l_cdpos-udate.
          l_cod-liberado_a   = l_cdpos-utime.
          MODIFY t_cod FROM l_cod.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.
  METHOD get_info_lib.
    DATA l_ekko TYPE ekko.
    DATA: l_cdhdr TYPE cdhdr,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_cdpos TYPE cdpos.

    FIELD-SYMBOLS: <info_gen>   TYPE bapirlgnrq,
                   <rel_posted> TYPE bapirlcorq.

    CLEAR: liberado,
           ult_lib,
           sig_lib,
           fecha_ult_lib,
           usuario_ult_lib,
           ult_lib_desc,
           sig_lib_desc,
           rel_info_gen,
           rel_prerequisites,
           rel_posted,
           rel_final.

    SELECT SINGLE frggr frgke frgsx frgzu FROM ekko
      INTO CORRESPONDING FIELDS OF l_ekko
     WHERE ebeln = ebeln.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF todas = 'X'.
      CLEAR l_ekko-frgzu.
    ENDIF.

    REFRESH rel_info_gen.
    CLEAR rel_info_gen.
    CALL FUNCTION 'ME_REL_INFO'
      EXPORTING
*       I_TITLE           = ' '
*       I_FRGCO           = l_ekko-frgco
        i_frgkz           = l_ekko-frgke
        i_frggr           = l_ekko-frggr
        i_frgst           = l_ekko-frgsx
        i_frgzu           = l_ekko-frgzu
        i_frgot           = '2'
        i_no_dialog       = '2'
*       I_SUBSCREEN       = ' '
      TABLES
        rel_info_gen      = rel_info_gen
        rel_prerequisites = rel_prerequisites
        rel_posted        = rel_posted
        rel_final         = rel_final
      EXCEPTIONS
        not_active        = 1
        OTHERS            = 2.
    IF sy-subrc = 0.
      ASSIGN rel_info_gen[ 1 ] TO <info_gen>.
      IF sy-subrc = 0.
        IF    <info_gen>-rel_ind = 'L' " Liberado
           OR (     <info_gen>-rel_ind   IS INITIAL
                AND <info_gen>-rel_group IS INITIAL
                AND <info_gen>-rel_strat IS INITIAL
                AND <info_gen>-rel_code  IS INITIAL ).
          liberado = 'X'.
        ENDIF.
      ELSE.
        liberado = 'X'.
      ENDIF.
    ENDIF.

    ASSIGN rel_posted[ 1 ] TO <rel_posted>.
    IF sy-subrc = 0.
      IF NOT <rel_posted>-rel_code8 IS INITIAL.  "#EC PREFER_CASE
        ult_lib = <rel_posted>-rel_code8.
        ult_lib_desc = <rel_posted>-rel_cd_tx8.
      ELSEIF NOT <rel_posted>-rel_code7 IS INITIAL.
        ult_lib = <rel_posted>-rel_code7.
        ult_lib_desc = <rel_posted>-rel_cd_tx7.
      ELSEIF NOT <rel_posted>-rel_code6 IS INITIAL.
        ult_lib = <rel_posted>-rel_code6.
        ult_lib_desc = <rel_posted>-rel_cd_tx6.
      ELSEIF NOT <rel_posted>-rel_code5 IS INITIAL.
        ult_lib = <rel_posted>-rel_code5.
        ult_lib_desc = <rel_posted>-rel_cd_tx5.
      ELSEIF NOT <rel_posted>-rel_code4 IS INITIAL.
        ult_lib = <rel_posted>-rel_code4.
        ult_lib_desc = <rel_posted>-rel_cd_tx4.
      ELSEIF NOT <rel_posted>-rel_code3 IS INITIAL.
        ult_lib = <rel_posted>-rel_code3.
        ult_lib_desc = <rel_posted>-rel_cd_tx3.
      ELSEIF NOT <rel_posted>-rel_code2 IS INITIAL.
        ult_lib = <rel_posted>-rel_code2.
        ult_lib_desc = <rel_posted>-rel_cd_tx2.
      ELSEIF NOT <rel_posted>-rel_code1 IS INITIAL.
        ult_lib = <rel_posted>-rel_code1.
        ult_lib_desc = <rel_posted>-rel_cd_tx1.
      ENDIF.
    ENDIF.

    ASSIGN rel_final[ 1 ] TO FIELD-SYMBOL(<rel_final>).
    IF sy-subrc = 0.
      IF ult_lib IS INITIAL.
        sig_lib = <rel_final>-rel_code1.
        sig_lib_desc = <rel_final>-rel_cd_tx1.
      ELSEIF <rel_final>-rel_code1 = ult_lib.
        sig_lib = <rel_final>-rel_code2.
        sig_lib_desc = <rel_final>-rel_cd_tx2.
      ELSEIF <rel_final>-rel_code2 = ult_lib.
        sig_lib = <rel_final>-rel_code3.
        sig_lib_desc = <rel_final>-rel_cd_tx3.
      ELSEIF <rel_final>-rel_code3 = ult_lib.
        sig_lib = <rel_final>-rel_code4.
        sig_lib_desc = <rel_final>-rel_cd_tx4.
      ELSEIF <rel_final>-rel_code4 = ult_lib.
        sig_lib = <rel_final>-rel_code5.
        sig_lib_desc = <rel_final>-rel_cd_tx5.
      ELSEIF <rel_final>-rel_code5 = ult_lib.
        sig_lib = <rel_final>-rel_code6.
        sig_lib_desc = <rel_final>-rel_cd_tx6.
      ELSEIF <rel_final>-rel_code6 = ult_lib.
        sig_lib = <rel_final>-rel_code7.
        sig_lib_desc = <rel_final>-rel_cd_tx7.
      ELSEIF <rel_final>-rel_code7 = ult_lib.
        sig_lib = <rel_final>-rel_code8.
        sig_lib_desc = <rel_final>-rel_cd_tx8.
      ENDIF.
    ENDIF.

    IF NOT mostrar_ult_lib IS INITIAL.
      IF NOT ult_lib IS INITIAL.
        SELECT * FROM  cdhdr
          INTO l_cdhdr
         WHERE objectclas = 'EINKBELEG'
           AND objectid   = ebeln
         ORDER BY udate ASCENDING
                  utime ASCENDING.
          SELECT objectclas FROM  cdpos
            INTO l_cdpos-objectclas
                 WHERE objectclas = l_cdhdr-objectclas
                   AND objectid   = l_cdhdr-objectid
                   AND changenr   = l_cdhdr-changenr
                   AND tabname    = 'EKKO'
                   AND fname      = 'FRGZU'
                   AND chngind    = 'U'
           ORDER BY PRIMARY KEY.
            fecha_ult_lib   = l_cdhdr-udate.
            hora_ult_lib    = l_cdhdr-utime.
            usuario_ult_lib = l_cdhdr-username.
            EXIT.                                  "#EC CI_EXIT_SELECT.
          ENDSELECT.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_status_header.
    DATA: gr_po      TYPE REF TO cl_po_header_handle_mm,
          l_document TYPE mepo_document,
          l_result   TYPE mmpur_bool.

    CLEAR status.

    gr_po = NEW #( ).

    l_document-doc_type    = 'F'.
    l_document-process     = 'PO_PROCESS'.
    l_document-trtyp       = 'A'.
    l_document-doc_key(10) = ebeln.

    gr_po->po_initialize( im_document = l_document ).

    gr_po->po_read( EXPORTING im_tcode     = 'ME23N'
                              im_trtyp     = l_document-trtyp
                              im_aktyp     = l_document-trtyp
                              im_po_number = l_document-doc_key(10)
                              im_document  = l_document
                    IMPORTING ex_result    = l_result ).

    IF l_result <> mmpur_yes.
      RETURN.
    ENDIF.

    PERFORM get_status_header IN PROGRAM saplmepo
            CHANGING status.
  ENDMETHOD.
  METHOD get_texto.
    DATA l_name TYPE stxh-tdname.

    IF ebelp IS INITIAL.
      l_name = ebeln.
      lineas = zcl_ap_textos=>get_texto( id     = id
                                         name   = l_name
                                         spras  = spras
                                         object = 'EKKO' ).
    ELSE.
      CONCATENATE ebeln ebelp INTO l_name.
      lineas = zcl_ap_textos=>get_texto( id     = id
                                         name   = l_name
                                         spras  = spras
                                         object = 'EKPO' ).
    ENDIF.
  ENDMETHOD.
  METHOD get_texto_string.
    DATA l_name TYPE stxh-tdname.

    IF ebelp IS INITIAL.
      l_name = ebeln.
      string = zcl_ap_textos=>get_texto_string( id     = id
                                                name   = l_name
                                                spras  = spras
                                                object = 'EKKO' ).
    ELSE.
      CONCATENATE ebeln ebelp INTO l_name.
      string = zcl_ap_textos=>get_texto_string( id     = id
                                                name   = l_name
                                                spras  = spras
                                                object = 'EKPO' ).
    ENDIF.
  ENDMETHOD.
  METHOD get_url_por_titulo_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = ebeln.
    url = zcl_ap_gos=>get_url_por_titulo_st( tipo   = 'BUS2012'
                                             clave  = l_clave
                                             titulo = titulo ).
    IF url IS INITIAL.
      url = zcl_ap_gos=>get_url_por_titulo_st( tipo   = 'BUS2014'
                                               clave  = l_clave
                                               titulo = titulo ).
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_condicion.
    FIELD-SYMBOLS <valor> TYPE any.

    DATA: r_kposn TYPE RANGE OF konv-kposn,
          l_knumv TYPE ekko-knumv,
          l_kposn LIKE LINE OF r_kposn,
          i_konv  TYPE TABLE OF konv,
          l_campo TYPE string,
          l_konv  TYPE konv,
          l_tabla TYPE tabname     VALUE 'KONV'.

    SELECT SINGLE knumv FROM ekko
      INTO l_knumv
     WHERE ebeln = ebeln.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF NOT ebelp IS INITIAL.
      l_kposn-option = 'EQ'.
      l_kposn-sign   = 'I'.
      l_kposn-low    = ebelp.
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
  METHOD get_valor_entregas.
    valor = get_valor_historial( ebeln    = ebeln
                                 ebelp    = ebelp
                                 bewtp    = c_hist_entrega
                                 cantidad = cantidad ).
  ENDMETHOD.
  METHOD get_valor_facturas.
    valor = get_valor_historial( ebeln    = ebeln
                                 ebelp    = ebelp
                                 bewtp    = c_hist_factura
                                 cantidad = cantidad  ).
  ENDMETHOD.
  METHOD get_valor_historial.
    TYPES: BEGIN OF t_ekbe,
             wrbtr TYPE ekbe-wrbtr,
             menge TYPE ekbe-menge,
             shkzg TYPE ekbe-shkzg,
           END OF t_ekbe.

    DATA: i_ekbe TYPE TABLE OF ekbe,
          l_ekbe TYPE ekbe.

    CLEAR valor.

    IF ebelp IS INITIAL.
      SELECT dmbtr menge shkzg FROM ekbe
        INTO CORRESPONDING FIELDS OF TABLE i_ekbe
       WHERE ebeln = ebeln
         AND bewtp = bewtp.
    ELSE.
      SELECT dmbtr menge shkzg FROM ekbe
        INTO CORRESPONDING FIELDS OF TABLE i_ekbe
       WHERE ebeln = ebeln
         AND ebelp = ebelp
         AND bewtp = bewtp.
    ENDIF.

    LOOP AT i_ekbe INTO l_ekbe.
      IF l_ekbe-shkzg = 'S'.
        IF cantidad IS INITIAL.
          valor = valor + l_ekbe-dmbtr.
        ELSE.
          valor = valor + l_ekbe-menge.
        ENDIF.
      ELSE.
        IF cantidad IS INITIAL.
          valor = valor - l_ekbe-dmbtr.
        ELSE.
          valor = valor - l_ekbe-menge.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_valor_salidas.
    valor = get_valor_historial( ebeln    = ebeln
                                 ebelp    = ebelp
                                 bewtp    = c_hist_salidas
                                 cantidad = cantidad ).
  ENDMETHOD.
  METHOD informar_tablas_bapi.
    DATA: wrf_pohf_data_ekko_sty   TYPE wrf_pohf_data_ekko_sty,
          l_tabla                  TYPE tabname,
          l_ekko                   TYPE ekko,
          l_poheader               TYPE bapimepoheader,
          s_bapi_te_mepoheaderx    TYPE bapi_te_mepoheaderx,
          s_bapi_te_mepoheader     TYPE bapi_te_mepoheader,
          l_extension_in           TYPE bapiparex,
          wrf_pohf_data_eikp_sty   TYPE wrf_pohf_data_eikp_sty,
          l_impheader              TYPE bapieikp,
          l_ekpo                   TYPE ekpo,
          wrf_pohf_data_ekpo_sty   TYPE wrf_pohf_data_ekpo_sty,
          l_eket                   TYPE eket,
          l_adrn2                  TYPE ekpo-adrn2,
          l_poitem                 TYPE bapimepoitem,
          l_ekkn                   TYPE ekkn,
          mepoaccounting           TYPE mepoaccounting,
          l_account                TYPE bapimepoaccount,
          l_borrado                TYPE char1,
          wrf_pohf_data_eket_sty   TYPE wrf_pohf_data_eket_sty,
          l_schedule               TYPE bapimeposchedule,
          l_ekpa                   TYPE ekpa,
          l_wrf_pohf_data_ekpa_sty TYPE wrf_pohf_data_ekpa_sty,
          l_partner                TYPE bapiekkop,
          l_konv                   TYPE konv,
          l_komv                   TYPE komv,
          l_bapimepocond           TYPE bapimepocond,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_knumv                  TYPE ekko-knumv,
          l_bapimepocond_bd        TYPE bapimepocond,
          l_konv_bd                TYPE konv,
          l_komv_bd                TYPE komv,
          l_bapimepocondx          TYPE bapimepocondx,
          l_ekpv                   TYPE ekpv,
          bapimeposhippexp         TYPE bapimeposhippexp,
          l_shipping               TYPE bapiitemship.

    FIELD-SYMBOLS <fs> TYPE any.

    CLEAR: poheader,
           impheader,
           i_errores,
           i_poitem,
           i_poitemx,
           i_schedule,
           i_schedulex,
           i_pocond,
           i_pocondx,
           address,
           i_address,
           i_partners,
           i_extension_in.

    MOVE-CORRESPONDING ekko TO wrf_pohf_data_ekko_sty.
    CALL FUNCTION 'MAP2E_EKKO_TO_MEPOHEADER'
      EXPORTING
        wrf_pohf_data_ekko_sty = wrf_pohf_data_ekko_sty
      CHANGING
        bapimepoheader         = poheader.

    IF NOT ekko-ebeln IS INITIAL.
      l_tabla = 'EKKO'.
      SELECT SINGLE * FROM (l_tabla)          "#EC CI_ALL_FIELDS_NEEDED
        INTO l_ekko
       WHERE ebeln = ekko-ebeln.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING l_ekko TO wrf_pohf_data_ekko_sty.
        CALL FUNCTION 'MAP2E_EKKO_TO_MEPOHEADER'
          EXPORTING
            wrf_pohf_data_ekko_sty = wrf_pohf_data_ekko_sty
          CHANGING
            bapimepoheader         = l_poheader.
      ENDIF.
    ENDIF.

*  zcl_ap_material=>determina_cambios_campos( EXPORTING original     = poheader
*                                                       modificacion = l_poheader
*                                             CHANGING  cambios      = poheaderx ).
    zcl_ap_utils=>busca_cambios_datos( EXPORTING original     = l_poheader
                                                 modificacion = poheader
                                                 ddic         = 'X'
                                       CHANGING  cambios      = poheaderx ).

    poheaderx-item_intvl = 'X'. " Respetar numeración posiciones

    IF campos_usuario = 'X'.
      zcl_ap_utils=>busca_cambios_datos( EXPORTING original     = l_ekko
                                                   modificacion = ekko
                                                   ddic         = 'X'
                                         CHANGING  cambios      = s_bapi_te_mepoheaderx ).
      s_bapi_te_mepoheaderx-po_number = ekko-ebeln.
      s_bapi_te_mepoheader-po_number = ekko-ebeln.
      MOVE-CORRESPONDING ekko TO s_bapi_te_mepoheader.
      l_extension_in-structure = 'BAPI_TE_MEPOHEADER'.
      cl_abap_container_utilities=>fill_container_c( EXPORTING  im_value               = s_bapi_te_mepoheader
                                                     IMPORTING  ex_container           = l_extension_in-valuepart1
                                                     EXCEPTIONS illegal_parameter_type = 1
                                                                OTHERS                 = 2 ).
      IF sy-subrc <> 0.
        MESSAGE 'Error en campos usuario' TYPE 'S'.
      ELSE.
        APPEND l_extension_in TO i_extension_in.
        l_extension_in-structure  = 'BAPI_TE_MEPOHEADERX'.
        l_extension_in-valuepart1 = s_bapi_te_mepoheaderx.
        APPEND l_extension_in TO i_extension_in.
      ENDIF.
    ENDIF.

***************** EIKP DATOS CABECERA IMPORTACIÓN
    MOVE-CORRESPONDING eikp TO wrf_pohf_data_eikp_sty.
    CALL FUNCTION 'MAP2E_EIKP_TO_BAPIEIKP'
      EXPORTING
        wrf_pohf_data_eikp_sty = wrf_pohf_data_eikp_sty
      CHANGING
        bapieikp               = impheader.

*  zcl_ap_material=>determina_cambios_campos( EXPORTING original     = impheader
*                                                       modificacion = l_impheader
*                                             CHANGING  cambios      = impheaderx ).
    zcl_ap_utils=>busca_cambios_datos( EXPORTING original     = l_impheader
                                                 modificacion = impheader
                                                 ddic         = 'X'
                                       CHANGING  cambios      = impheaderx ).

**************** EKPO POSICIONES

    LOOP AT i_ekpo INTO l_ekpo.
      MOVE-CORRESPONDING l_ekpo TO wrf_pohf_data_ekpo_sty.

* Ñapa para poder especificar posiciones gratuitas
      ASSIGN ('L_EKPO-SPE_CRM_FKREL') TO <fs>.
      IF sy-subrc = 0.
        IF <fs> = 'X'.
          wrf_pohf_data_ekpo_sty-umson = 'X'.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'MAP2E_EKPO_TO_MEPOITEM'
        EXPORTING
          wrf_pohf_data_ekpo_sty = wrf_pohf_data_ekpo_sty
        CHANGING
          bapimepoitem           = poitem.

* Como no podemos indicar que no queremos factura final, si detectamos una N en este campo
* luego indicadmos que es posición sin cargo.
      IF poitem-ir_ind = 'N'.
        CLEAR poitem-ir_ind.
        poitem-free_item = 'X'.
      ENDIF.

      poitem-gi_based_gr = l_ekpo-wabwe.

      READ TABLE i_eket INTO l_eket WITH KEY ebelp = l_ekpo-ebelp.
      IF sy-subrc = 0.
        poitem-batch = l_eket-charg.
      ENDIF.

      APPEND poitem TO i_poitem.

      IF NOT l_ekpo-adrn2 IS INITIAL.
        SELECT SINGLE adrn2 FROM ekpo
          INTO l_adrn2
         WHERE ebeln = l_ekpo-ebeln
           AND ebelp = l_ekpo-ebelp.
        IF l_ekpo-adrn2 <> l_adrn2.
          CLEAR address.
          MOVE-CORRESPONDING poitem TO address.
          address-addr_no = l_ekpo-adrn2.
          APPEND address TO i_address.
        ENDIF.
      ENDIF.

      IF NOT ekko-ebeln IS INITIAL.
        l_tabla = 'EKPO'.
        SELECT SINGLE * FROM (l_tabla)        "#EC CI_ALL_FIELDS_NEEDED
          INTO l_ekpo
         WHERE ebeln = ekko-ebeln
           AND ebelp = l_ekpo-ebelp.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING l_ekpo TO wrf_pohf_data_ekpo_sty.
          CALL FUNCTION 'MAP2E_EKPO_TO_MEPOITEM'
            EXPORTING
              wrf_pohf_data_ekpo_sty = wrf_pohf_data_ekpo_sty
            CHANGING
              bapimepoitem           = l_poitem.
          IF l_ekpo-wabwe <> wrf_pohf_data_ekpo_sty-wabwe.
            l_poitem-gi_based_gr = 'X'.
          ENDIF.
        ELSE.
          CLEAR l_poitem.
        ENDIF.
      ENDIF.

*    zcl_ap_material=>determina_cambios_campos( EXPORTING original     = poitem
*                                                         modificacion = l_poitem
*                                                         pos_ini_cambios = 1
*                                               CHANGING  cambios      = poitemx ).
      zcl_ap_utils=>busca_cambios_datos( EXPORTING original     = l_poitem
                                                   modificacion = poitem
                                                   ddic         = 'X'
                                         CHANGING  cambios      = poitemx ).
      poitemx-po_item = poitem-po_item.
      APPEND poitemx TO i_poitemx.
    ENDLOOP.

    LOOP AT i_ekkn INTO l_ekkn.
      MOVE-CORRESPONDING l_ekkn TO mepoaccounting.
      mepoaccounting-zexkn = l_ekkn-zekkn.

      CALL FUNCTION 'MAP2E_ACCOUNT_TO_BAPIACCOUNT'
        EXPORTING
          mepoaccounting  = mepoaccounting
        CHANGING
          bapimepoaccount = account.

      IF NOT l_ekkn-ps_psp_pnr IS INITIAL.
        WRITE l_ekkn-ps_psp_pnr TO account-wbs_element.
      ENDIF.
      APPEND account TO i_account.

      IF NOT ekko-ebeln IS INITIAL.
        SELECT * FROM ekkn
          INTO l_ekkn
          UP TO 1 ROWS
         WHERE ebeln = l_ekkn-ebeln
           AND ebelp = l_ekkn-ebelp
         ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING l_ekkn TO mepoaccounting.

          CALL FUNCTION 'MAP2E_ACCOUNT_TO_BAPIACCOUNT'
            EXPORTING
              mepoaccounting  = mepoaccounting
            CHANGING
              bapimepoaccount = l_account.

          IF NOT l_ekkn-ps_psp_pnr IS INITIAL.
            WRITE l_ekkn-ps_psp_pnr TO l_account-wbs_element.
          ENDIF.
        ELSE.
          CLEAR l_account.
        ENDIF.
      ENDIF.

*    zcl_ap_material=>determina_cambios_campos( EXPORTING original     = account
*                                                         modificacion = l_account
*                                                         pos_ini_cambios = 2
*                                               CHANGING  cambios      = accountx ).
      zcl_ap_utils=>busca_cambios_datos( EXPORTING original     = l_account
                                                   modificacion = account
                                                   ddic         = 'X'
                                         CHANGING  cambios      = accountx ).
      accountx-po_item   = account-po_item.
      accountx-serial_no = account-serial_no.
      APPEND accountx TO i_accountx.
    ENDLOOP.

    LOOP AT i_eket INTO l_eket.
      CLEAR l_borrado.
      IF l_eket-cd_loctype = '!D!'. " Truco para pasar indicador de borrado.
        l_borrado = 'X'.
        CLEAR l_eket-cd_loctype.
      ENDIF.
      MOVE-CORRESPONDING l_eket TO wrf_pohf_data_eket_sty.

      CALL FUNCTION 'MAP2E_EKET_TO_MEPOSCHEDULE'
        EXPORTING
          wrf_pohf_data_eket_sty = wrf_pohf_data_eket_sty
        CHANGING
          bapimeposchedule       = schedule.

      schedule-delete_ind = l_borrado.
      APPEND schedule TO i_schedule.

      IF NOT ekko-ebeln IS INITIAL.
        l_tabla = 'EKET'.
        SELECT SINGLE * FROM (l_tabla)        "#EC CI_ALL_FIELDS_NEEDED
          INTO l_eket
         WHERE ebeln = ekko-ebeln
           AND ebelp = l_eket-ebelp
           AND etenr = l_eket-etenr.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING l_eket TO wrf_pohf_data_eket_sty.

          CALL FUNCTION 'MAP2E_EKET_TO_MEPOSCHEDULE'
            EXPORTING
              wrf_pohf_data_eket_sty = wrf_pohf_data_eket_sty
            CHANGING
              bapimeposchedule       = l_schedule.
        ELSE.
          CLEAR l_schedule.
        ENDIF.
      ENDIF.

      zcl_ap_utils=>busca_cambios_datos( EXPORTING original     = l_schedule
                                                   modificacion = schedule
                                                   ddic         = 'X'
                                         CHANGING  cambios      = schedulex ).

      schedulex-po_item    = schedule-po_item.
      schedulex-sched_line = schedule-sched_line.
      APPEND schedulex TO i_schedulex.
    ENDLOOP.

    LOOP AT i_ekpa INTO l_ekpa.
      MOVE-CORRESPONDING l_ekpa TO l_wrf_pohf_data_ekpa_sty.
      CALL FUNCTION 'MAP2E_EKPA_TO_BAPIEKKOP'
        EXPORTING
          wrf_pohf_data_ekpa_sty = l_wrf_pohf_data_ekpa_sty
        CHANGING
          bapiekkop              = l_partner.

      APPEND l_partner TO i_partners.
    ENDLOOP.

    LOOP AT i_konv INTO l_konv.
      MOVE-CORRESPONDING l_konv TO l_komv.
      CALL FUNCTION 'MAP2E_KOMV_TO_BAPIMEPOCOND'
        EXPORTING
          komv         = l_komv
        CHANGING
          bapimepocond = l_bapimepocond.
      IF NOT l_konv-lifnr IS INITIAL.
        l_bapimepocond-vendor_no = l_konv-lifnr.
      ENDIF.

      IF NOT ekko-ebeln IS INITIAL.
        SELECT SINGLE knumv FROM ekko
          INTO l_knumv
         WHERE ebeln = ekko-ebeln.

        CLEAR l_bapimepocond_bd.
        SELECT SINGLE * FROM konv
          INTO l_konv_bd
         WHERE knumv = l_konv-knumv
           AND kposn = l_konv-kposn
           AND stunr = l_konv-stunr
           AND zaehk = l_konv-zaehk.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING l_konv_bd TO l_komv_bd.
          CALL FUNCTION 'MAP2E_KOMV_TO_BAPIMEPOCOND'
            EXPORTING
              komv         = l_komv_bd
            CHANGING
              bapimepocond = l_bapimepocond_bd.

          IF NOT l_konv_bd-lifnr IS INITIAL.
            l_bapimepocond_bd-vendor_no = l_konv_bd-lifnr.
          ENDIF.
        ENDIF.
      ENDIF.

      IF l_bapimepocond <> l_bapimepocond_bd.
        zcl_ap_utils=>busca_cambios_datos( EXPORTING original     = l_bapimepocond_bd
                                                     modificacion = l_bapimepocond
                                                     ddic         = 'X'
                                           CHANGING  cambios      = l_bapimepocondx ).

        l_bapimepocondx-condition_no = l_konv-knumv.
        l_bapimepocondx-itm_number   = l_konv-kposn.
        l_bapimepocondx-cond_st_no   = l_konv-stunr.
        APPEND l_bapimepocondx TO i_pocondx.
        IF NOT l_konv_bd IS INITIAL.
          l_bapimepocond-change_id = 'U'.
        ENDIF.
        APPEND l_bapimepocond TO i_pocond.
      ENDIF.

    ENDLOOP.

    LOOP AT i_ekpv INTO l_ekpv.
      CLEAR bapimeposhippexp.
      CALL FUNCTION 'MAP2E_EKPV_TO_BAPISHIPPING'
        EXPORTING
          ekpv             = l_ekpv
        CHANGING
          bapimeposhippexp = bapimeposhippexp.

      MOVE-CORRESPONDING bapimeposhippexp TO shipping.
      APPEND shipping TO i_shipping.

      IF NOT ekko-ebeln IS INITIAL.
        l_tabla = 'EKPV'.
        SELECT SINGLE * FROM (l_tabla)        "#EC CI_ALL_FIELDS_NEEDED
          INTO l_ekpv
         WHERE ebeln = ekko-ebeln
           AND ebelp = l_ekpv-ebelp.
        IF sy-subrc = 0.
          CLEAR bapimeposhippexp.
          CALL FUNCTION 'MAP2E_EKPV_TO_BAPISHIPPING'
            EXPORTING
              ekpv             = l_ekpv
            CHANGING
              bapimeposhippexp = bapimeposhippexp.
          MOVE-CORRESPONDING bapimeposhippexp TO l_shipping.
        ELSE.
          CLEAR l_shipping.
        ENDIF.
      ENDIF.

      zcl_ap_utils=>busca_cambios_datos( EXPORTING original     = l_shipping
                                                   modificacion = shipping
                                                   ddic         = 'X'
                                         CHANGING  cambios      = shippingx ).
      shippingx-po_item = shipping-po_item.
      APPEND shippingx TO i_shippingx.
    ENDLOOP.
  ENDMETHOD.
  METHOD insertar_url_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = ebeln.
    zcl_ap_gos=>insertar_url_gos_st( tipo   = 'BUS2012'
                                     clave  = l_clave
                                     titulo = titulo
                                     url    = url ).
  ENDMETHOD.
  METHOD liberar.
    DATA: i_return TYPE TABLE OF bapireturn,
          l_return TYPE bapireturn.

    CALL FUNCTION 'BAPI_PO_RELEASE'
      EXPORTING
        purchaseorder          = ebeln
        po_rel_code            = rel_code
        use_exceptions         = 'X'
      IMPORTING
        rel_status_new         = rel_status_new
        rel_indicator_new      = rel_indicator_new
      TABLES
        return                 = i_return
      EXCEPTIONS
        authority_check_fail   = 1
        document_not_found     = 2
        enqueue_fail           = 3
        prerequisite_fail      = 4
        release_already_posted = 5
        responsibility_fail    = 6
        OTHERS                 = 7.

    IF sy-subrc = 0 AND i_return IS INITIAL.
      IF commit = 'X'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ELSE.
      LOOP AT i_return INTO l_return.
        __concat_a message l_return-message.
      ENDLOOP.
      IF sy-subrc <> 0.
        message = 'Error al liberar el pedido'(elp).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD marca_pos_entrega_final.
    DATA: l_tabla TYPE tabname,
          ekko    TYPE ekko,
          o_ped   TYPE REF TO zcl_ap_pedido_mm,
          l_ekpo  TYPE ekpo,
          i_ekpo  TYPE TABLE OF ekpo,
          i_eket  TYPE TABLE OF eket.

    CLEAR message.
    l_tabla = 'EKKO'.
    SELECT SINGLE * FROM (l_tabla)            "#EC CI_ALL_FIELDS_NEEDED
      INTO ekko
     WHERE ebeln = ebeln.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe el pedido'(nep) ebeln INTO message SEPARATED BY space.
    ELSE.
      o_ped = NEW #( ).

      l_tabla = 'EKPO'.
      SELECT SINGLE * FROM (l_tabla)          "#EC CI_ALL_FIELDS_NEEDED
        INTO l_ekpo
       WHERE ebeln = ebeln
         AND ebelp = ebelp.
      IF sy-subrc <> 0.
        message = 'Posición de pedido no existe'(ppn).
      ENDIF.

* Han solicitado modificar flag de entrega completa
      IF eglkz <> '?'.
        IF l_ekpo-eglkz = eglkz.
          IF NOT o_log IS INITIAL.
            IF eglkz = 'X'.
              o_log->log( p1 = 'La posicion'(lap) p2 = ebelp p3 = 'del pedido'(del) p4 = ebeln p5 = 'ya tenía entrega completa' msgty = 'W' ).
            ELSE.
              o_log->log( p1 = 'La posicion'(lap) p2 = ebelp p3 = 'del pedido'(del) p4 = ebeln p5 = 'no tenía entrega completa' msgty = 'W' ).
            ENDIF.
          ENDIF.
        ELSE.
          DATA(l_mod_eglkz) = 'X'.
        ENDIF.
      ENDIF.

      IF l_ekpo-elikz = elikz.
        IF NOT o_log IS INITIAL.
          IF elikz = 'X'.
            o_log->log( p1 = 'La posicion'(lap) p2 = ebelp p3 = 'del pedido'(del) p4 = ebeln p5 = 'ya tenía entrega final'(yte) msgty = 'W' ).
          ELSE.
            o_log->log( p1 = 'La posicion'(lap) p2 = ebelp p3 = 'del pedido'(del) p4 = ebeln p5 = 'no tenía entrega final'(nef) msgty = 'W' ).
          ENDIF.
        ENDIF.
      ELSE.
        DATA(l_mod_elikz) = 'X'.
      ENDIF.

      IF l_mod_eglkz IS INITIAL AND l_mod_elikz IS INITIAL.
        RETURN.
      ENDIF.

      l_ekpo-elikz = elikz.
      IF eglkz <> '?'.
        l_ekpo-eglkz = eglkz.
      ENDIF.
      APPEND l_ekpo TO i_ekpo.

      message = o_ped->modificar_pedido( ekko   = ekko
                                         i_ekpo = i_ekpo
                                         i_eket = i_eket ).

    ENDIF.

    IF NOT o_log IS INITIAL.
      IF NOT message IS INITIAL.
        o_log->log( p1 = message ).
      ELSE.
        IF l_mod_elikz = 'X'.
          IF NOT o_log IS INITIAL.
            IF elikz = 'X'.
              o_log->log( p1 = 'Se ha marcado la posicion'(smp) p2 = ebelp p3 = 'del pedido'(del) p4 = ebeln p5 = 'con entrega final'(cef) msgty = 'S' ).
            ELSE.
              o_log->log( p1 = 'Se ha desmarcado la posicion'(sdp) p2 = ebelp p3 = 'del pedido'(del) p4 = ebeln p5 = 'de entrega final'(def) msgty = 'S' ).
            ENDIF.
          ENDIF.
        ENDIF.
        IF l_mod_eglkz = 'X'.
          IF NOT o_log IS INITIAL.
            IF eglkz = 'X'.
              o_log->log( p1 = 'Se ha marcado la posicion'(smp) p2 = ebelp p3 = 'del pedido'(del) p4 = ebeln p5 = 'con entrega completa' msgty = 'S' ).
            ELSE.
              o_log->log( p1 = 'Se ha desmarcado la posicion'(sdp) p2 = ebelp p3 = 'del pedido'(del) p4 = ebeln p5 = 'de entrega completa' msgty = 'S' ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF message IS INITIAL AND commit = 'X'.
      zcl_ap_dev=>commit( ).
    ENDIF.
  ENDMETHOD.
  METHOD modificar_pedido.
    DATA: l_contract_item     TYPE bapimeoutitem,
          i_contract_item     TYPE TABLE OF bapimeoutitem,
          l_contract_itemx    TYPE bapimeoutitemx,
          i_contract_itemx    TYPE TABLE OF bapimeoutitemx,
          l_contract_account  TYPE bapimeoutaccount,
          i_contract_account  TYPE TABLE OF bapimeoutaccount,
          l_contract_accountx TYPE bapimeoutaccountx,
          i_contract_accountx TYPE TABLE OF bapimeoutaccountx,
          l_funcion           TYPE string,
          l_contract_header   TYPE bapimeoutheader,
          l_contract_headerx  TYPE bapimeoutheaderx.

    informar_tablas_bapi( ekko           = ekko
                          eikp           = eikp
                          i_ekpo         = i_ekpo
                          i_eket         = i_eket
                          i_ekkn         = i_ekkn
                          i_ekpa         = i_ekpa
                          i_ekpv         = i_ekpv
                          i_konv         = i_konv
                          campos_usuario = campos_usuario ).

    IF i_components IS SUPPLIED.
      me->i_components  = i_components.
      me->i_componentsx = i_componentsx.
    ENDIF.

    IF ekko-bstyp = 'K' OR ekko-bstyp = 'L'.
      LOOP AT i_poitem INTO poitem.
        CLEAR l_contract_item.
        MOVE-CORRESPONDING poitem TO l_contract_item.
        l_contract_item-item_no = poitem-po_item.
        APPEND l_contract_item TO i_contract_item.
      ENDLOOP.
      LOOP AT i_poitemx INTO poitemx.
        CLEAR l_contract_itemx.
        MOVE-CORRESPONDING poitemx TO l_contract_itemx.
        l_contract_itemx-item_no  = poitemx-po_item.
        l_contract_itemx-item_nox = poitemx-po_itemx.
        APPEND l_contract_itemx TO i_contract_itemx.
      ENDLOOP.
      LOOP AT i_account INTO account.
        CLEAR l_contract_account.
        MOVE-CORRESPONDING account TO l_contract_account.
        l_contract_account-item_no = account-po_item.
        APPEND l_contract_account TO i_contract_account.
      ENDLOOP.
      LOOP AT i_accountx INTO accountx.
        CLEAR l_contract_accountx.
        MOVE-CORRESPONDING accountx TO l_contract_accountx.
        l_contract_accountx-item_no  = accountx-po_item.
        l_contract_accountx-item_nox = accountx-po_itemx.
        APPEND l_contract_accountx TO i_contract_accountx.
      ENDLOOP.

      IF ekko-bstyp = 'K'.
        l_funcion = 'BAPI_CONTRACT_CHANGE'.
      ELSEIF ekko-bstyp = 'L'.
        l_funcion = 'BAPI_SAG_CHANGE'.
      ENDIF.

      CALL FUNCTION l_funcion
        EXPORTING
          purchasingdocument = ekko-ebeln
          header             = l_contract_header
          headerx            = l_contract_headerx
*         VENDOR_ADDRESS     = VENDOR_ADDRESS
*         TESTRUN            = TESTRUN
*         TECHNICAL_DATA     = TECHNICAL_DATA
* IMPORTING
*         EXP_HEADER         = EXP_HEADER
        TABLES
          item               = i_contract_item
          itemx              = i_contract_itemx
          account            = i_contract_account
*         ACCOUNTPROFITSEGMENT = ACCOUNTPROFITSEGMENT
          accountx           = i_contract_accountx
*         DELIVERY_ADDRESS   = DELIVERY_ADDRESS
*         ITEM_COND_VALIDITY = ITEM_COND_VALIDITY
*         ITEM_COND_VALIDITYX = ITEM_COND_VALIDITYX
*         item_condition     = i_conditions
*         item_conditionx    = i_conditionsx
*         ITEM_COND_SCALE_VALUE = ITEM_COND_SCALE_VALUE
*         ITEM_COND_SCALE_QUAN = ITEM_COND_SCALE_QUAN
*         ITEM_TEXT          = ITEM_TEXT
*         HEADER_TEXT        = HEADER_TEXT
*         HEAD_COND_VALIDITY = HEAD_COND_VALIDITY
*         HEAD_COND_VALIDITYX = HEAD_COND_VALIDITYX
*         HEAD_CONDITION     = HEAD_CONDITION
*         HEAD_CONDITIONX    = HEAD_CONDITIONX
*         HEAD_COND_SCALE_VAL = HEAD_COND_SCALE_VAL
*         HEAD_COND_SCALE_QUAN = HEAD_COND_SCALE_QUAN
*         PARTNER            = PARTNER
*         PARTNERX           = PARTNERX
*         RELEASE_DOCU       = RELEASE_DOCU
          extensionin        = i_extension_in
*         EXTENSIONOUT       = EXTENSIONOUT
          return             = i_errores.
    ELSE.
      CALL FUNCTION 'BAPI_PO_CHANGE'
        EXPORTING
          purchaseorder   = ekko-ebeln
          poheader        = poheader
          poheaderx       = poheaderx
*         POADDRVENDOR    = POADDRVENDOR
*         TESTRUN         = TESTRUN
*         MEMORY_UNCOMPLETE = MEMORY_UNCOMPLETE
*         MEMORY_COMPLETE = MEMORY_COMPLETE
          poexpimpheader  = impheader
          poexpimpheaderx = impheaderx
*         VERSIONS        = VERSIONS
*         NO_MESSAGING    = NO_MESSAGING
*         NO_MESSAGE_REQ  = NO_MESSAGE_REQ
          no_authority    = no_aut
*         NO_PRICE_FROM_PO = NO_PRICE_FROM_PO
* IMPORTING
*         EXPHEADER       = EXPHEADER
*         EXPPOEXPIMPHEADER = EXPPOEXPIMPHEADER
        TABLES
          return          = i_errores
          poitem          = i_poitem
          poitemx         = i_poitemx
*         POADDRDELIVERY  = POADDRDELIVERY
          poschedule      = i_schedule
          poschedulex     = i_schedulex
          poaccount       = i_account
          poaccountx      = i_accountx
*         POCONDHEADER    = POCONDHEADER
*         POCONDHEADERX   = POCONDHEADERX
          pocond          = i_pocond
          pocondx         = i_pocondx
*         POLIMITS        = POLIMITS
*         POCONTRACTLIMITS = POCONTRACTLIMITS
*         POSERVICES      = POSERVICES
*         POSRVACCESSVALUES = POSRVACCESSVALUES
*         POSERVICESTEXT  = POSERVICESTEXT
          extensionin     = i_extension_in
*         EXTENSIONOUT    = EXTENSIONOUT
*         POEXPIMPITEM    = POEXPIMPITEM
*         POEXPIMPITEMX   = POEXPIMPITEMX
*         POTEXTHEADER    = POTEXTHEADER
*         POTEXTITEM      = POTEXTITEM
*         ALLVERSIONS     = ALLVERSIONS
          popartner       = i_partners
          pocomponents    = me->i_components
          pocomponentsx   = me->i_componentsx
          poshipping      = i_shipping
          poshippingx     = i_shippingx.
*         POSHIPPINGEXP   = POSHIPPINGEXP
*         POHISTORY       = POHISTORY
*         POHISTORY_TOTALS = POHISTORY_TOTALS
*         POCONFIRMATION  = POCONFIRMATION
    ENDIF.

    IF error_si_no_mod_campos = 'X'.
* A veces la BAPI no hace lo que se le indica, pero sólo da una advertencia de la que no nos damos cuenta
      LOOP AT i_errores ASSIGNING FIELD-SYMBOL(<error>) WHERE type = 'I' AND id = 'ME' AND number = '664'. " No se ha podido ejecutar la modificación de XXX
        <error>-type = 'E'.
      ENDLOOP.
    ENDIF.

    READ TABLE i_errores INTO bapiret2 WITH KEY type = 'E'.
    IF sy-subrc <> 0.
      IF commit = 'X'.
        zcl_ap_dev=>commit( ).
      ENDIF.
    ELSE.
      DESCRIBE TABLE i_errores LINES sy-tfill.
      IF sy-tfill > 1.
        DELETE i_errores WHERE    message CS 'Imposible modificar la instancia'(imi)
                               OR message CS 'No se ha creado ninguna instancia'(nni).
      ENDIF.

* Devolvemos el último mensaje de error, que es el más explicativo
      LOOP AT i_errores INTO bapiret2 WHERE type = 'E'.
        message = bapiret2-message.
      ENDLOOP.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.

* Borramos mensaje de ampliación
    DELETE i_errores WHERE type = 'W' AND message CS 'CI_EK'.
    DELETE i_errores WHERE message = 'No se ha creado ninguna instancia de tipo objeto PurchaseOrder; ref.externa:'(nrf).

    IF NOT o_log IS INITIAL.
      IF no_warnings = 'X'.
        DELETE i_errores WHERE type = 'W'.
      ENDIF.
      o_log->set_tabla_log_from_bapiret2_t( i_errores ).
    ENDIF.
  ENDMETHOD.
  METHOD modificar_reparto.
    DATA: l_tabla TYPE tabname,
          l_eket  TYPE eket,
          o_ped   TYPE REF TO zcl_ap_pedido_mm,
          ekko    TYPE ekko,
          i_ekpo  TYPE TABLE OF ekpo,
          i_eket  TYPE TABLE OF eket.

    FIELD-SYMBOLS <eket> TYPE eket.

    l_tabla = 'EKET'.
    SELECT SINGLE * FROM (l_tabla)            "#EC CI_ALL_FIELDS_NEEDED
      INTO l_eket
     WHERE ebeln = ebeln
       AND ebelp = ebelp
       AND etenr = etenr.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe el reparto'(ner) ebeln ebelp etenr INTO message SEPARATED BY space.
      IF NOT o_log IS INITIAL.
        o_log->log( p1 = message ).
      ENDIF.
    ELSE.
      IF    ( NOT eindt IS INITIAL AND l_eket-eindt <> eindt )
         OR ( NOT uzeit IS INITIAL AND l_eket-uzeit <> uzeit )
         OR borrar = 'X'.
        o_ped = NEW #( ).

        l_tabla = 'EKKO'.
        SELECT SINGLE * FROM (l_tabla)        "#EC CI_ALL_FIELDS_NEEDED
          INTO ekko
         WHERE ebeln = ebeln.

        l_tabla = 'EKPO'.
        SELECT * FROM (l_tabla)               "#EC CI_ALL_FIELDS_NEEDED
          INTO TABLE i_ekpo
         WHERE ebeln = ebeln
           AND ebelp = ebelp.

        l_tabla = 'EKET'.
        SELECT * FROM (l_tabla)               "#EC CI_ALL_FIELDS_NEEDED
          INTO TABLE i_eket
         WHERE ebeln = ebeln
           AND ebelp = ebelp
           AND etenr = etenr.

        LOOP AT i_eket ASSIGNING <eket>.
          IF NOT eindt IS INITIAL.
            <eket>-eindt = eindt.
          ENDIF.
          IF NOT uzeit IS INITIAL.
            <eket>-uzeit = uzeit.
          ENDIF.
          IF borrar = 'X'.
            <eket>-cd_loctype = '!D!'.
          ENDIF.
        ENDLOOP.

        message = o_ped->modificar_pedido( ekko   = ekko
                                           i_ekpo = i_ekpo
                                           i_eket = i_eket
                                           o_log  = o_log ).

        IF NOT o_log IS INITIAL.
          IF message IS INITIAL.
            o_log->log( p1    = 'Se modifica la fecha de reparto en el pedido'(smf)
                        p2    = ebeln
                        p3    = ebelp
                        p4    = 'Nueva fecha'(nuf)
                        p5    = eindt
                        p6    = 'F.anterior'(fan)
                        p7    = l_eket-eindt
                        msgty = 'S' ).
          ELSE.
            o_log->log( p1 = 'Error modificando la fecha de reparto en el pedido'(emf)
                        p2 = ebeln
                        p3 = ebelp
                        p4 = 'Nueva fecha'(nuf)
                        p5 = eindt
                        p6 = 'F.anterior'(fan)
                        p7 = l_eket-eindt ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD rechazar.
    DATA: ls_document TYPE mepo_document,
          lr_po       TYPE REF TO cl_po_header_handle_mm,
          l_result    TYPE mmpur_bool,
          ls_bapi     TYPE bapiret2.

    IF esta_bloqueado( ebeln ) = 'X'.
      message = |Pedido bloqueado por { sy-msgv1 }|.
      RETURN.
    ENDIF.

*  prepare creation of PO instance
    ls_document-process     = 'PO_PROCESS'.
    ls_document-trtyp       = 'V'.
    ls_document-doc_key(10) = ebeln.
    IF anular_rechazo IS INITIAL.
      ls_document-initiator-initiator = 'RELEASE'.
    ENDIF.

    lr_po = NEW #( ).
    lr_po->for_bapi = 'X'.
    lr_po->po_initialize( ls_document ).
    lr_po->set_po_number( ebeln ).

    TRY.
        lr_po->po_read( EXPORTING im_tcode     = 'ME29N'
                                  im_trtyp     = ls_document-trtyp
                                  im_aktyp     = ls_document-trtyp
                                  im_po_number = ebeln
                                  im_document  = ls_document
                        IMPORTING ex_result    = l_result ). " 2212877
      CATCH cx_root ##CATCH_ALL.
        l_result = mmpur_no.                                " 2212877
    ENDTRY.

    IF l_result = mmpur_no.
      message = 'Error recuperando datos del pedido'.
      RETURN.
    ENDIF.

    IF    lr_po->if_releasable_mm~is_rejection_allowed( ) = 'X'
       OR anular_rechazo = 'X'.
      lr_po->if_releasable_mm~reject( EXPORTING  im_reset = anular_rechazo
                                      EXCEPTIONS
                                          failed   = 1
                                          OTHERS   = 2 ).
      IF sy-subrc = 0.
        " Success here...
        lr_po->po_post( EXCEPTIONS failure = 1
                                   OTHERS  = 2 ).
        IF sy-subrc > 0.
          lr_po->po_initialize( ).
        ELSE.
          lr_po->po_close( ).
          FREE lr_po.
        ENDIF.
        IF commit = 'X'.
          COMMIT WORK AND WAIT.
        ENDIF.
      ELSE.
        " Error here...
        CALL FUNCTION 'BALW_BAPIRETURN_GET2'
          EXPORTING
            type   = sy-msgty
            cl     = sy-msgid
            number = sy-msgno
            par1   = sy-msgv1
            par2   = sy-msgv2
            par3   = sy-msgv3
          IMPORTING
            return = ls_bapi.
        message = ls_bapi-message.
      ENDIF.
    ELSE.
      message = 'Rechazo no permitido'.
    ENDIF.
  ENDMETHOD.
  METHOD save_texto.
    DATA l_name TYPE stxh-tdname.

    IF ebelp IS INITIAL.
      l_name = ebeln.
      zcl_ap_textos=>save_texto( id     = id
                                 name   = l_name
                                 spras  = spras
                                 object = 'EKKO'
                                 lineas = lineas ).
    ELSE.
      CONCATENATE ebeln ebelp INTO l_name.
      zcl_ap_textos=>save_texto( id     = id
                                 name   = l_name
                                 spras  = spras
                                 object = 'EKPO'
                                 lineas = lineas ).
    ENDIF.
  ENDMETHOD.
  METHOD urls_gos_st.
    DATA l_clave TYPE srgbtbrel-instid_a.

    l_clave = ebeln.
    tabla = zcl_ap_gos=>urls_gos_st( tipo = 'BUS2012' clave = l_clave ).

    DATA(tabla2) = zcl_ap_gos=>urls_gos_st( tipo = 'BUS2014' clave = l_clave ).
    APPEND LINES OF tabla2 TO tabla.
  ENDMETHOD.
  METHOD visualizar.
    CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
      EXPORTING
        i_ebeln              = ebeln
        i_ebelp              = ebelp
        i_enjoy              = 'X'
        i_edit               = edit
      EXCEPTIONS
        not_found            = 1
        no_authority         = 2
        invalid_call         = 3
        preview_not_possible = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      MESSAGE 'Error mostrando pedido' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar_pedido_st.
    CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
      EXPORTING
        i_ebeln              = ebeln
        i_ebelp              = ebelp
        i_enjoy              = 'X'
      EXCEPTIONS
        not_found            = 1
        no_authority         = 2
        invalid_call         = 3
        preview_not_possible = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      MESSAGE 'Error mostrando pedido' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
