TYPES: BEGIN OF t_status_orden,
         aufnr TYPE aufnr,
         vornr TYPE vornr,
         txt04 TYPE j_txt04,
         istat TYPE j_istat,
       END OF t_status_orden,
       tt_status_orden TYPE TABLE OF t_status_orden.
CLASS zcl_ap_orden_pp DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_saldos_liq           TYPE STANDARD TABLE OF kabr_gsum.
    TYPES tt_kapm                 TYPE TABLE OF kapm.
    TYPES t_bapi_order_header1    TYPE TABLE OF bapi_order_header1.
    TYPES t_bapi_order_item       TYPE TABLE OF bapi_order_item.
    TYPES t_bapi_order_phase      TYPE TABLE OF bapi_order_phase.
    TYPES t_bapi_order_component  TYPE TABLE OF bapi_order_component.
    TYPES t_bapi_order_operation1 TYPE TABLE OF bapi_order_operation1.

    DATA aufk            TYPE aufk.
    DATA afko            TYPE afko.
    DATA caufvd          TYPE caufvd.
    DATA i_header        TYPE t_bapi_order_header1.
    DATA header          TYPE bapi_order_header1.
    DATA position        TYPE bapi_order_item.
    DATA i_position      TYPE t_bapi_order_item.
    DATA phase           TYPE bapi_order_phase.
    DATA i_phase         TYPE t_bapi_order_phase.
    DATA component       TYPE bapi_order_component.
    DATA i_component     TYPE t_bapi_order_component.
    DATA operation       TYPE bapi_order_operation1.
    DATA i_operation     TYPE t_bapi_order_operation1.
    DATA i_return        TYPE bapiret1_tab.
    DATA return          TYPE bapiret1.
    DATA afru            TYPE afru.
    DATA i_detail_return TYPE cocf_t_bapi_return.
    DATA g_t_kkbcs_out   TYPE kkbcs_out_t.
    DATA kkbcs_out       TYPE kkbcs_out.
    DATA g_t_kkbcs       TYPE kkbcs_t.
    DATA kkbcs           TYPE kkbcs.

    CLASS-DATA c_status_lib  TYPE jest-stat VALUE 'I0002' ##NO_TEXT.
    CLASS-DATA c_status_ctec TYPE j_istat   VALUE 'I0045' ##NO_TEXT.
    CLASS-DATA c_status_ptbo TYPE j_istat   VALUE 'I0076' ##NO_TEXT.
    CLASS-DATA c_status_cerr TYPE j_istat   VALUE 'I0046' ##NO_TEXT.
    CLASS-DATA c_status_abie TYPE jest-stat VALUE 'I0001' ##NO_TEXT.
    CLASS-DATA c_status_plan TYPE jest-stat VALUE 'I0513' ##NO_TEXT.
    CLASS-DATA c_status_notp TYPE jest-stat VALUE 'I0010' ##NO_TEXT.
    CLASS-DATA c_status_noti TYPE jest-stat VALUE 'I0009' ##NO_TEXT.

    METHODS constructor
      IMPORTING aufnr TYPE aufnr OPTIONAL.

    CLASS-METHODS visualizar
      IMPORTING aufnr     TYPE any       OPTIONAL
                editar    TYPE abap_bool DEFAULT ''
                lista     TYPE any       OPTIONAL
                separador TYPE any       DEFAULT ','
      PREFERRED PARAMETER aufnr.

    CLASS-METHODS get_caufvd
      IMPORTING aufnr         TYPE aufnr
      RETURNING VALUE(caufvd) TYPE caufvd.

    CLASS-METHODS get_material
      IMPORTING aufnr        TYPE aufnr
      RETURNING VALUE(matnr) TYPE matnr.

    CLASS-METHODS contiene_status
      IMPORTING aufnr              TYPE aufnr    OPTIONAL
                !status            TYPE any      OPTIONAL
                spras              TYPE sy-langu DEFAULT sy-langu
                status_excluir     TYPE any      DEFAULT ''
                vornr              TYPE vornr    DEFAULT ''
                status_int         TYPE any      DEFAULT ''
                objnr              TYPE any      DEFAULT ''
      RETURNING VALUE(si_contiene) TYPE abap_bool.

    CLASS-METHODS get_puesto_trabajo
      IMPORTING aufnr        TYPE aufnr
                vornr        TYPE vornr DEFAULT '0010'
                ktsch        TYPE any   DEFAULT ''
      RETURNING VALUE(arbpl) TYPE arbpl.

    CLASS-METHODS ver_md04
      IMPORTING matnr TYPE matnr
                werks TYPE werks_d.

    CLASS-METHODS ver_notificacion
      IMPORTING rueck TYPE co_rueck
                rmzhl TYPE co_rmzhl OPTIONAL.

    METHODS get_detalle
      IMPORTING !header     TYPE abap_bool DEFAULT ''
                positions   TYPE abap_bool DEFAULT ''
                !operations TYPE abap_bool DEFAULT ''
                !components TYPE abap_bool DEFAULT ''.

    METHODS set_orden
      IMPORTING aufnr TYPE aufnr.

    METHODS free.

    CLASS-METHODS get_ctd_movs_mm
      IMPORTING aufnr           TYPE any                OPTIONAL
                matnr           TYPE matnr              OPTIONAL
                charg           TYPE charg_d            OPTIONAL
                bwart           TYPE bwart              OPTIONAL
                bwart2          TYPE bwart              OPTIONAL
                r_bwart         TYPE zt_rangebwart      OPTIONAL
                r_budat         TYPE fagl_range_t_budat OPTIONAL
                werks           TYPE werks_d            OPTIONAL
                l_bwart         TYPE any                DEFAULT ''
                ini_mat         TYPE any                DEFAULT ''
                rspos           TYPE mseg-rspos         OPTIONAL
                lgort           TYPE lgort_d            OPTIONAL
                sgtxt           TYPE any                OPTIONAL
                meins           TYPE meins              DEFAULT ''
      EXPORTING !message        TYPE bapi_msg
      RETURNING VALUE(cantidad) TYPE mengev.

    CLASS-METHODS get_ctd_notif
      IMPORTING aufnr           TYPE aufnr
                vornr           TYPE vornr OPTIONAL
                tipo            TYPE any   DEFAULT 'N'
      RETURNING VALUE(cantidad) TYPE mengv13.

    METHODS notificar
      IMPORTING vornr          TYPE vornr
                cantidad       TYPE any
                rechazo        TYPE any
                meins          TYPE meins
                fecha_ini      TYPE datum     DEFAULT sy-datum
                hora_ini       TYPE uzeit     DEFAULT sy-uzeit
                fecha_fin      TYPE datum     DEFAULT sy-datum
                hora_fin       TYPE uzeit     DEFAULT sy-uzeit
                crear_mov      TYPE abap_bool DEFAULT ''
                parcial_final  TYPE aueru_vs  DEFAULT ''
                !commit        TYPE abap_bool DEFAULT 'X'
                budat          TYPE budat     DEFAULT sy-datum
                arbpl          TYPE arbpl     DEFAULT ''
                texto          TYPE any       DEFAULT ''
                act1_uni       TYPE any       DEFAULT ''
                act1_ctd       TYPE any       OPTIONAL
                act2_uni       TYPE any       DEFAULT ''
                act2_ctd       TYPE any       OPTIONAL
                act3_uni       TYPE any       DEFAULT ''
                act3_ctd       TYPE any       OPTIONAL
                act4_uni       TYPE any       DEFAULT ''
                act4_ctd       TYPE any       OPTIONAL
                act5_uni       TYPE any       DEFAULT ''
                act5_ctd       TYPE any       OPTIONAL
                act6_uni       TYPE any       DEFAULT ''
                act6_ctd       TYPE any       OPTIONAL
      RETURNING VALUE(mensaje) TYPE bapireturn1-message.

    CLASS-METHODS notificar_st
      IMPORTING aufnr           TYPE aufnr
                vornr           TYPE vornr
                cantidad        TYPE any
                rechazo         TYPE any                    OPTIONAL
                meins           TYPE meins
                fecha_ini       TYPE datum                  DEFAULT sy-datum
                hora_ini        TYPE uzeit                  DEFAULT sy-uzeit
                fecha_fin       TYPE datum                  DEFAULT sy-datum
                hora_fin        TYPE uzeit                  DEFAULT sy-uzeit
                crear_mov       TYPE abap_bool              DEFAULT ''
                parcial_final   TYPE aueru_vs               DEFAULT ''
                !commit         TYPE abap_bool              DEFAULT 'X'
                act1_uni        TYPE any                    DEFAULT ''
                act1_ctd        TYPE any                    OPTIONAL
                act2_uni        TYPE any                    DEFAULT ''
                act2_ctd        TYPE any                    OPTIONAL
                act3_uni        TYPE any                    DEFAULT ''
                act3_ctd        TYPE any                    OPTIONAL
                i_consumos      TYPE tab_bapi_goodsmvt_item OPTIONAL
                budat           TYPE budat                  DEFAULT sy-datum
                arbpl           TYPE arbpl                  DEFAULT ''
                texto           TYPE any                    DEFAULT ''
                act5_uni        TYPE any                    DEFAULT ''
                act5_ctd        TYPE any                    OPTIONAL
                act4_uni        TYPE any                    DEFAULT ''
                act4_ctd        TYPE any                    OPTIONAL
                act6_uni        TYPE any                    DEFAULT ''
                act6_ctd        TYPE any                    OPTIONAL
      EXPORTING mensaje         TYPE bapireturn1-message
                i_detail_return TYPE cocf_t_bapi_return
                !return         TYPE bapiret1
                afru            TYPE afru.

    METHODS anular_notificacion
      IMPORTING rueck          TYPE co_rueck
                rmzhl          TYPE co_rmzhl
                postg_date     TYPE bapi_pi_confirm-postg_date DEFAULT sy-datum
                !commit        TYPE abap_bool                  DEFAULT 'X'
      RETURNING VALUE(mensaje) TYPE bapireturn-message.

    CLASS-METHODS anular_notificacion_st
      IMPORTING rueck      TYPE co_rueck
                rmzhl      TYPE co_rmzhl
                postg_date TYPE bapi_pi_confirm-postg_date DEFAULT sy-datum
                !commit    TYPE abap_bool                  DEFAULT 'X'
      EXPORTING mensaje    TYPE bapireturn-message
                afru       TYPE afru.

    CLASS-METHODS get_status_st
      IMPORTING aufnr         TYPE aufnr     OPTIONAL
                objnr         TYPE qmobjnr   OPTIONAL
                usuario       TYPE abap_bool DEFAULT ''
                spras         TYPE sy-langu  DEFAULT sy-langu
                bypass_buffer TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER aufnr
      RETURNING VALUE(status) TYPE bsvx-sttxt.

    CLASS-METHODS esta_bloqueada
      IMPORTING aufnr            TYPE aufnr
      RETURNING VALUE(bloqueado) TYPE abap_bool.

    METHODS contiene_status_orden
      IMPORTING aufnr              TYPE aufnr
                !status            TYPE any
                spras              TYPE sy-langu  DEFAULT sy-langu
                vornr              TYPE vornr     DEFAULT ''
                usuario            TYPE abap_bool DEFAULT ''
      RETURNING VALUE(si_contiene) TYPE abap_bool.

    CLASS-METHODS get_capacidad_puesto_trabajo
      IMPORTING arbpl              TYPE any
                begda              TYPE begda
                endda              TYPE endda
                werks              TYPE werks_d
      RETURNING VALUE(i_capacidad) TYPE zt_crcapacity_exact.

    CLASS-METHODS modificar
      IMPORTING aufnr TYPE aufk-aufnr.

    METHODS contiene_status_orden_rango
      IMPORTING aufnr              TYPE aufnr
                r_status           TYPE /spe/ret_range_c4_t
                spras              TYPE sy-langu  DEFAULT sy-langu
                vornr              TYPE vornr     DEFAULT ''
                !or                TYPE abap_bool DEFAULT ''
      RETURNING VALUE(si_contiene) TYPE abap_bool.

    METHODS get_datos_cabecera.
    METHODS get_datos_costes.

    METHODS notificar_op
      IMPORTING vornr          TYPE vornr
                cantidad       TYPE any
                rechazo        TYPE any
                meins          TYPE meins
                fecha_ini      TYPE datum                  DEFAULT sy-datum
                hora_ini       TYPE uzeit                  DEFAULT sy-uzeit
                fecha_fin      TYPE datum                  DEFAULT sy-datum
                hora_fin       TYPE uzeit                  DEFAULT sy-uzeit
                crear_mov      TYPE abap_bool              DEFAULT ''
                parcial_final  TYPE aueru_vs               DEFAULT '1'
                !commit        TYPE abap_bool              DEFAULT 'X'
                act1_uni       TYPE any                    DEFAULT ''
                act1_ctd       TYPE any                    OPTIONAL
                act2_uni       TYPE any                    DEFAULT ''
                act2_ctd       TYPE any                    OPTIONAL
                act3_uni       TYPE any                    DEFAULT ''
                act3_ctd       TYPE any                    OPTIONAL
                i_consumos     TYPE tab_bapi_goodsmvt_item OPTIONAL
                budat          TYPE budat
                arbpl          TYPE arbpl                  DEFAULT ''
                texto          TYPE any                    DEFAULT ''
                act4_uni       TYPE any                    DEFAULT ''
                act4_ctd       TYPE any                    OPTIONAL
                act5_uni       TYPE any                    DEFAULT ''
                act5_ctd       TYPE any                    OPTIONAL
                act6_uni       TYPE any                    DEFAULT ''
                act6_ctd       TYPE any                    OPTIONAL
      RETURNING VALUE(mensaje) TYPE bapireturn1-message.

    CLASS-METHODS cambia_status_usuario
      IMPORTING aufnr                TYPE aufnr
                !status              TYPE any
                spras                TYPE sy-langu  DEFAULT sy-langu
                !commit              TYPE abap_bool DEFAULT 'X'
                no_permitir_anterior TYPE abap_bool DEFAULT ''
      RETURNING VALUE(mensaje)       TYPE bapi_msg.

    CLASS-METHODS cambia_status
      IMPORTING aufnr          TYPE aufnr
                !status        TYPE any
                spras          TYPE sy-langu  DEFAULT sy-langu
                !commit        TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    METHODS anular_notificacion_op
      IMPORTING rueck          TYPE co_rueck
                rmzhl          TYPE co_rmzhl
                postg_date     TYPE bapi_pi_confirm-postg_date DEFAULT sy-datum
                !commit        TYPE abap_bool                  DEFAULT 'X'
      RETURNING VALUE(mensaje) TYPE bapireturn-message.

    CLASS-METHODS cerrar_tecnicamente
      IMPORTING aufnr          TYPE any
                anular         TYPE abap_bool DEFAULT ''
                modo_ct        TYPE bdcmode   DEFAULT 'E'
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    METHODS contiene_status_us_orden_rango
      IMPORTING aufnr              TYPE aufnr
                r_status           TYPE /spe/ret_range_c4_t
                spras              TYPE sy-langu    DEFAULT sy-langu
                vornr              TYPE vornr       DEFAULT ''
                stsma              TYPE tj30t-stsma DEFAULT ''
      RETURNING VALUE(si_contiene) TYPE abap_bool.

    CLASS-METHODS get_clase
      IMPORTING aufnr        TYPE any
      RETURNING VALUE(auart) TYPE auart.

    CLASS-METHODS get_almacen
      IMPORTING aufnr        TYPE any
      RETURNING VALUE(lgort) TYPE lgort_d.

    CLASS-METHODS cerrar_orden
      IMPORTING aufnr          TYPE aufnr
                !commit        TYPE abap_bool DEFAULT 'X'
                anular         TYPE abap_bool DEFAULT ''
                modo           TYPE char1     DEFAULT 'N'
      RETURNING VALUE(message) TYPE bapiret2-message.

    CLASS-METHODS lista2rango
      IMPORTING lista           TYPE any       OPTIONAL
                separador       TYPE any       DEFAULT ','
                !option         TYPE any       DEFAULT 'EQ'
                permitir_vacias TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER lista
      RETURNING VALUE(rango)    TYPE range_t_aufnr.

    CLASS-METHODS get_status_hist
      IMPORTING aufnr       TYPE aufnr
                !status     TYPE any
                registro    TYPE any   DEFAULT 'U'
                stsma       TYPE any   DEFAULT ''
                spras       TYPE spras DEFAULT sy-langu
      RETURNING VALUE(jcds) TYPE jcds.

    CLASS-METHODS liberar
      IMPORTING aufnr          TYPE any
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    CLASS-METHODS get_recurso
      IMPORTING arbid        TYPE any        OPTIONAL
                objty        TYPE crhd-objty DEFAULT 'A'
      PREFERRED PARAMETER arbid
      RETURNING VALUE(arbpl) TYPE arbpl.

    CLASS-METHODS get_capacidades
      IMPORTING aufnr    TYPE aufnr       OPTIONAL
                aufpl    TYPE caufv-aufpl OPTIONAL
      EXPORTING i_kako   TYPE pdsmaint_kako_t
                i_kbed   TYPE badi_kbed
                i_kapm   TYPE tt_kapm
                !message TYPE bapi_msg.

    CLASS-METHODS visualizar_plan
      IMPORTING warpl  TYPE any       OPTIONAL
                editar TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER warpl.

    CLASS-METHODS get_texto
      IMPORTING aufnr        TYPE aufnr
                vornr        TYPE vornr     OPTIONAL
                largo        TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(texto) TYPE string.

    CLASS-METHODS get_capacidades_equipo
      IMPORTING arbid    TYPE afvc-arbid
                kapar    TYPE kako-kapar DEFAULT ''
      EXPORTING i_kako   TYPE pdsmaint_kako_t
                i_kapm   TYPE tt_kapm
                !message TYPE bapi_msg.

    CLASS-METHODS leer_datos_maestros_pp
      IMPORTING aufnr          TYPE aufnr
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS espera_si_bloqueada
      IMPORTING aufnr           TYPE aufnr
                segundos_espera TYPE int2 DEFAULT 10
      RETURNING VALUE(message)  TYPE bapi_msg.

    CLASS-METHODS get_saldo_liquidacion
      IMPORTING aufnr        TYPE aufnr
                fecha        TYPE dats DEFAULT sy-datum
      EXPORTING i_saldos     TYPE tt_saldos_liq
                !message     TYPE bapi_msg
      RETURNING VALUE(saldo) TYPE wkgxxx.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA i_status_orden TYPE tt_status_orden.
endclass. "ZCL_AP_ORDEN_PP definition
class ZCL_AP_ORDEN_PP implementation.
  METHOD anular_notificacion.
    CLEAR: i_return, return, afru.

    CALL FUNCTION 'BAPI_PRODORDCONF_CANCEL'
      EXPORTING
        confirmation        = rueck
        confirmationcounter = rmzhl
        postg_date          = postg_date
*       CONF_TEXT           =
      IMPORTING
        return              = return
*       LOCKED              =
        created_conf_no     = afru-rueck
        created_conf_count  = afru-rmzhl.

    IF NOT afru IS INITIAL.
      IF commit = 'X'.
        zcl_ap_dev=>commit( ).
      ENDIF.

      SELECT SINGLE * FROM afru
        INTO afru
       WHERE rueck = afru-rueck
         AND rmzhl = afru-rmzhl.
    ELSE.
      mensaje = return-message.
    ENDIF.
  ENDMETHOD.
  METHOD anular_notificacion_op.
    CLEAR: i_return, return, afru.

    CALL FUNCTION 'BAPI_PROCORDCONF_CANCEL'
      EXPORTING
        confirmation        = rueck
        confirmationcounter = rmzhl
        postg_date          = postg_date
*       CONF_TEXT           =
      IMPORTING
        return              = return
*       LOCKED              =
        created_conf_no     = afru-rueck
        created_conf_count  = afru-rmzhl.

    IF NOT afru IS INITIAL.
      IF commit = 'X'.
        zcl_ap_dev=>commit( ).
      ENDIF.

      SELECT SINGLE * FROM afru
        INTO afru
       WHERE rueck = afru-rueck
         AND rmzhl = afru-rmzhl.
      IF sy-subrc <> 0.
        mensaje = return-message.
      ENDIF.
    ELSE.
      mensaje = return-message.
    ENDIF.
  ENDMETHOD.
  METHOD anular_notificacion_st.
    DATA: l_aufnr TYPE aufnr,
          o_orden TYPE REF TO zcl_ap_orden_pp.

    SELECT SINGLE aufnr FROM afru
      INTO l_aufnr
     WHERE rueck = rueck
       AND rmzhl = rmzhl.
    IF sy-subrc <> 0.
      mensaje = 'No existe la notificacion'(nen).
    ELSEIF l_aufnr IS INITIAL.
      mensaje = 'No se ha podido determinar_orden'(ndo).
    ELSE.
      o_orden = NEW #(
          aufnr = l_aufnr ).

      IF o_orden->aufk-autyp = '40'.
        mensaje = o_orden->anular_notificacion_op( rueck = rueck
                                                   rmzhl = rmzhl
                                                   postg_date = postg_date
                                                   commit = commit ).
      ELSE.
        mensaje = o_orden->anular_notificacion( rueck = rueck
                                                rmzhl = rmzhl
                                                postg_date = postg_date
                                                commit = commit ).
      ENDIF.

      afru = o_orden->afru.
    ENDIF.
  ENDMETHOD.
  METHOD cambia_status.
    DATA: i_status TYPE TABLE OF jstat,
          l_error  TYPE abap_bool.

    CLEAR mensaje.

    SELECT SINGLE autyp, objnr FROM aufk
      INTO (@DATA(l_autyp), @DATA(l_objnr))
     WHERE aufnr = @aufnr.
    IF sy-subrc <> 0.
      mensaje = 'No existe la orden'(neo).
      RETURN.
    ENDIF.

    SELECT istat FROM tj02t "#EC CI_GENBUFF
      INTO @DATA(istat)
      UP TO 1 ROWS
     WHERE spras = @spras
       AND txt04 = @status
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      mensaje = |Status { status } no válido|.
      RETURN.
    ENDIF.

    i_status = VALUE #( (  stat = istat ) ).

    CALL FUNCTION 'STATUS_CHANGE_INTERN'
      EXPORTING
*       CHECK_ONLY          = ' '
*       CLIENT              = '100'
        objnr               = l_objnr
*       ZEILE               = ' '
*       SET_CHGKZ           =
      IMPORTING
        error_occurred      = l_error
*       OBJECT_NOT_FOUND    =
*       STATUS_INCONSISTENT =
*       STATUS_NOT_ALLOWED  =
      TABLES
        status              = i_status
      EXCEPTIONS
        object_not_found    = 1
        status_inconsistent = 2
        status_not_allowed  = 3
        OTHERS              = 4.
    IF sy-subrc <> 0 OR l_error = 'X'.
      CASE sy-subrc.
        WHEN 1. mensaje = 'Orden errónea'.
        WHEN 2. mensaje = 'Status inconsistente'.
        WHEN 3. mensaje = 'Status no permitido'.
        WHEN OTHERS. MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mensaje.
      ENDCASE.
    ELSE.
      IF commit = 'X'.
        zcl_ap_dev=>commit( ).
      ENDIF.
      SELECT SINGLE objnr FROM jest
        INTO l_objnr
       WHERE objnr = l_objnr
         AND stat  = istat
         AND inact = ''.
      IF sy-subrc <> 0.
        mensaje = 'No se ha modificado el estado de la orden'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD cambia_status_usuario.
    DATA: l_autyp         TYPE aufk-autyp,
          l_jsto          TYPE jsto,
          i_tj30t         TYPE TABLE OF tj30t,
*        o_bi TYPE REF TO zcl_ap_batch_input,
          " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
          l_nstatus       TYPE i,
          l_tj30t         TYPE tj30t,
          " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
          l_tabix         TYPE n LENGTH 2,
          l_jest          TYPE jest,
          l_tj30_ant      TYPE tj30,
          l_tj30          TYPE tj30,
          i_orders        TYPE TABLE OF bapi_order_key,
          l_status        TYPE bapi_order_func_cntrl-status,
          l_return        TYPE bapiret2,
          i_detail_return TYPE TABLE OF bapi_order_return,
          l_detail_return TYPE bapi_order_return.

    SELECT SINGLE autyp FROM aufk
      INTO l_autyp
     WHERE aufnr = aufnr.
    IF sy-subrc <> 0.
      mensaje = 'No existe la orden'(neo).
      RETURN.
    ENDIF.

    CONCATENATE 'OR' aufnr INTO l_jsto-objnr.

    SELECT SINGLE * FROM jsto
      INTO l_jsto
     WHERE objnr = l_jsto-objnr.
    IF sy-subrc <> 0.
      mensaje = 'Orden no tiene asociado esquema de estatus'(one).
    ELSE.
      SELECT * FROM tj30t
        INTO TABLE i_tj30t
       WHERE stsma = l_jsto-stsma
         AND spras = spras.
      IF sy-subrc <> 0.
        CONCATENATE 'Esquema'(esq) l_jsto-stsma 'no contiene ningún status'(ncn) INTO mensaje SEPARATED BY space.
      ELSE.
        l_nstatus = lines( i_tj30t ).
        READ TABLE i_tj30t INTO l_tj30t WITH KEY txt04 = status.
        IF sy-subrc <> 0.
          CONCATENATE 'Esquema'(esq) l_jsto-stsma 'no contiene status'(ncs) status INTO mensaje SEPARATED BY space.
        ELSE.
          l_tabix = sy-tabix.
          SELECT SINGLE * FROM jest
            INTO l_jest
           WHERE objnr = l_jsto-objnr
             AND stat  = l_tj30t-estat
             AND inact = ''.
          IF sy-subrc = 0.
* Si el status ya esta activo no hacemos nada
            CONCATENATE '>Orden ya está en estado'(oye) status 'No hacemos nada'(nhn) INTO mensaje SEPARATED BY space.
          ELSE.
            IF no_permitir_anterior = 'X'.
              SELECT * FROM jest
                INTO l_jest
                UP TO 1 ROWS
               WHERE objnr    = l_jsto-objnr
                 AND stat  LIKE 'E%' " Sólo estatus de usuario
                 AND inact    = ''
                ORDER BY PRIMARY KEY.
              ENDSELECT.
              IF sy-subrc = 0.
                SELECT SINGLE * FROM tj30
                  INTO l_tj30_ant
                 WHERE stsma = l_jsto-stsma
                   AND estat = l_jest-stat.
                IF sy-subrc = 0.
                  SELECT SINGLE * FROM tj30
                    INTO l_tj30
                   WHERE stsma = l_jsto-stsma
                     AND estat = l_tj30t-estat.
                  IF l_tj30_ant-stonr > l_tj30-stonr.
                    READ TABLE i_tj30t INTO l_tj30t WITH KEY estat = l_jest-stat.
                    CONCATENATE '>Estatus'(esx) l_tj30t-txt04 'mayor que el propuesto'(mqp) status INTO mensaje SEPARATED BY space.
                    RETURN.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
*          CREATE OBJECT o_bi.
*
*          o_bi->inicio( ).
*
** Pantalla de acceso modificar/visualizar orden
*          o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5110').
*          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00').
*          o_bi->campos( campo = 'CAUFVD-AUFNR' valor = aufnr ). " Número de orden
*
*          o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5115').
*          o_bi->campos( campo = 'BDC_OKCODE' valor = '=STAT').
*
*          IF l_nstatus > 5.
*            o_bi->dynpro( program = 'SAPLBSVA' dynpro = '0300').
*            o_bi->campos( campo = 'BDC_OKCODE' valor = '==A-').
*
*            o_bi->dynpro( program = 'SAPLBSVA' dynpro = '0300').
*            o_bi->campos( campo = 'BDC_OKCODE' valor = '==A+').
*            l_tabix = 5 - ( l_nstatus - L_TABIX ).
*          ENDIF.
** Master screen STATUS_MAINTAIN
*          o_bi->dynpro( program = 'SAPLBSVA' dynpro = '0300').
*          o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK').
*          o_bi->campos( campo = 'J_STMAINT-ANWS' ind = l_tabix valor = 'X'). " Indicador "Status activo de momento"
*
*          o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5115').
*          o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU').
*
*          mensaje = o_bi->llamar_transaccion( tcode = 'COR2' modo = 'E').

* Vuelvo a verificar si el estatus se ha grabado con éxito, y si es así, blanqueamos mensaje

            IF l_autyp = '40'.
              APPEND aufnr TO i_orders.
              l_status = status.
              CALL FUNCTION 'BAPI_PROCORD_SETUSERSTATUS'
                EXPORTING
                  status_profile     = l_jsto-stsma
                  status             = l_status
                  work_process_group = 'COWORK_BAPI'
                  work_process_max   = 99
                IMPORTING
                  return             = l_return
                TABLES
                  orders             = i_orders
                  detail_return      = i_detail_return.

              IF l_return-type <> 'E' AND commit = 'X'.
                zcl_ap_dev=>commit( ).
              ENDIF.
            ELSEIF l_autyp = '10'.
              APPEND aufnr TO i_orders.
              l_status = status.
              CALL FUNCTION 'BAPI_PRODORD_SETUSERSTATUS'
                EXPORTING
                  status_profile     = l_jsto-stsma
                  status             = l_status
                  work_process_group = 'COWORK_BAPI'
                  work_process_max   = 99
                IMPORTING
                  return             = l_return
                TABLES
                  orders             = i_orders
                  detail_return      = i_detail_return.

              IF l_return-type <> 'E' AND commit = 'X'.
                zcl_ap_dev=>commit( ).
              ENDIF.
            ELSEIF l_autyp = '30'.
              CALL FUNCTION 'STATUS_CHANGE_EXTERN'
                EXPORTING
                  objnr               = l_jsto-objnr
                  user_status         = l_tj30t-estat
*                 SET_INACT           = ' '
*                 SET_CHGKZ           =
                  no_check            = 'X'
*             IMPORTING
*                 STONR               =
                EXCEPTIONS
                  object_not_found    = 1
                  status_inconsistent = 2
                  status_not_allowed  = 3
                  OTHERS              = 4.
              CASE sy-subrc.
                WHEN 0. IF commit = 'X'. COMMIT WORK AND WAIT. ENDIF.
                WHEN 1. l_return-message = 'Orden no encontrada'.
                WHEN 2. l_return-message = 'Status inconsistente'.
                WHEN 3. l_return-message = 'Status no permitido'.
                WHEN 4. l_return-message = 'Error modificando status de usuario'.
              ENDCASE.
            ELSE.
              l_return-message = 'No implementado el cambio de status'(nic).
            ENDIF.

            SELECT SINGLE objnr FROM jest
              INTO l_jest-objnr
             WHERE objnr = l_jsto-objnr
               AND stat  = l_tj30t-estat
               AND inact = ''.
            IF sy-subrc = 0 OR ( l_return-message IS INITIAL AND commit = '' ).
              CONCATENATE '>Se cambia el estatus de la orden a'(cso) status INTO mensaje SEPARATED BY space.
            ELSE.
              IF NOT l_return-message IS INITIAL.
                mensaje = l_return-message.
              ELSE.
                READ TABLE i_detail_return INTO l_detail_return WITH KEY type = 'E'.
                IF sy-subrc = 0.
                  mensaje = l_detail_return-message.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD cerrar_orden.
    DATA: l_autyp         TYPE aufk-autyp,
          orders          TYPE TABLE OF bapi_order_key,
          detail_return   TYPE TABLE OF bapi_order_return,
          application_log TYPE TABLE OF bapi_order_application_log,
          l_return        TYPE bapiret2,
          o_bi            TYPE REF TO zcl_ap_batch_input.

    SELECT SINGLE autyp FROM aufk
      INTO l_autyp
     WHERE aufnr = aufnr.

    IF anular IS INITIAL.
      APPEND aufnr TO orders.

      IF l_autyp = '40'.
        CALL FUNCTION 'BAPI_PROCORD_COMPLETE_TECH'
* EXPORTING
*   SCOPE_COMPL_TECH         = '1'
*   WORK_PROCESS_GROUP       = 'COWORK_BAPI'
*   WORK_PROCESS_MAX         = 99
* IMPORTING
*   RETURN                   =
          TABLES
            orders          = orders
            detail_return   = detail_return
            application_log = application_log.
      ELSE. " IF l_autyp = '10'.
        CALL FUNCTION 'BAPI_PRODORD_COMPLETE_TECH'
* EXPORTING
*   SCOPE_COMPL_TECH         = '1'
*   WORK_PROCESS_GROUP       = 'COWORK_BAPI'
*   WORK_PROCESS_MAX         = 99
          IMPORTING
            return          = l_return
          TABLES
            orders          = orders
            detail_return   = detail_return
            application_log = application_log.
      ENDIF.
*  IF l_return-type NE 'E'.
*    IF NOT detail_return IS INITIAL.
*      select single * from afko
*        into l_afko
*       where aufnr = aufnr.
*    ENDIF.
*  ENDIF.

      IF l_return-type = 'E'.
        message = l_return-message.
        CALL FUNCTION 'DEQUEUE_ALL'.
      ELSE.
        IF commit = 'X'.
          zcl_ap_dev=>commit( ).
        ENDIF.

        IF contiene_status( aufnr = aufnr status = 'CERR' ) = ''.
          o_bi = NEW #( ).

          o_bi->inicio( ).

          IF l_autyp = '40'.
* Pantalla de acceso modificar/visualizar orden
            o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5110' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
            o_bi->campos( campo = 'CAUFVD-AUFNR' valor = aufnr ). " Número de orden
            o_bi->campos( campo = 'R62CLORD-FLG_COMPL' valor = 'X' ). " Indicador: tratar toda el grafo la orden

            o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5115' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=ABSK' ).

            o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5115' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).

            message = o_bi->llamar_transaccion( tcode = 'COR2' modo = modo ).
            IF contiene_status( aufnr = aufnr status = 'CERR' ) = 'X'.
              CLEAR message.
            ELSEIF message IS INITIAL.
              message = 'Error cerrando orden'(eco).
            ENDIF.
          ELSEIF message IS INITIAL.
            message = 'Error cerrando orden'(eco).
          ENDIF.
        ELSEIF message IS INITIAL.
          CLEAR message.
        ENDIF.
      ENDIF.
    ELSE.
      IF contiene_status( aufnr = aufnr status = 'CERR' ) = 'X'.

        o_bi = NEW #( ).

        o_bi->inicio( ).

        IF l_autyp = '40'.
* Pantalla de acceso modificar/visualizar orden
          o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5110' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          o_bi->campos( campo = 'CAUFVD-AUFNR' valor = aufnr ). " Número de orden
          o_bi->campos( campo = 'R62CLORD-FLG_COMPL' valor = 'X' ). " Indicador: tratar toda el grafo la orden

          o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5115' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=RABK' ).

          o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5115' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).

          message = o_bi->llamar_transaccion( tcode = 'COR2' modo = modo ).
        ELSEIF l_autyp = '20'.
          message = 'No implementado para tipo de órdenes 20'.
        ENDIF.
        IF contiene_status( aufnr = aufnr status = 'CERR' ) = ''.
          CLEAR message.
        ELSEIF message IS INITIAL.
          message = 'Error anulando cierre'(eac).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD cerrar_tecnicamente.
    DATA l_autyp TYPE aufk-autyp.
    DATA o_bi    TYPE REF TO zcl_ap_batch_input.

    SELECT SINGLE autyp FROM aufk
      INTO l_autyp
     WHERE aufnr = aufnr.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe la orden'(neo) aufnr INTO mensaje SEPARATED BY space.
    ELSE.
      IF anular IS INITIAL AND contiene_status( aufnr = aufnr status = 'CTEC' ) = 'X'.
        mensaje = '>Orden ya estaba cerrada técnicamente'(oyc).
      ELSEIF anular = 'X' AND contiene_status( aufnr = aufnr status = 'CTEC' ) = ''.
        mensaje = '>Orden no estaba cerrada técnicamente'(oct).
      ELSE.
        o_bi = NEW #( ).

        o_bi->inicio( ).

        IF l_autyp = '40'.
* Pantalla de acceso modificar/visualizar orden
          o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5110' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          o_bi->campos( campo = 'CAUFVD-AUFNR' valor = aufnr ). " Número de orden

          o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5115' ).
          IF anular IS INITIAL.
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=TABS' ).
          ELSE.
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=TABR' ).
          ENDIF.

          o_bi->dynpro( program = 'SAPLCOKO' dynpro = '5115' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).

          mensaje = o_bi->llamar_transaccion( tcode = 'COR2' modo = modo_ct  ).
        ELSEIF l_autyp = '10'.
* Pantalla de acceso modificar/visualizar orden
          o_bi->dynpro( program = 'SAPLCOKO1' dynpro = '0110' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          o_bi->campos( campo = 'CAUFVD-AUFNR' valor = aufnr ). " Número de orden

          o_bi->dynpro( program = 'SAPLCOKO1' dynpro = '0115' ).
          IF anular IS INITIAL.
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=TABS' ).
          ELSE.
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=TABR' ).
          ENDIF.

          o_bi->dynpro( program = 'SAPLCOKO1' dynpro = '0115' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).

          mensaje = o_bi->llamar_transaccion( tcode = 'CO02' modo = modo_ct ).
        ENDIF.

        IF contiene_status( aufnr = aufnr status = 'CTEC' ) = 'X' AND anular IS INITIAL.
          mensaje = '>Se ha cerrado técnicamente la orden'(sct).
        ELSEIF contiene_status( aufnr = aufnr status = 'CTEC' ) = '' AND anular = 'X'.
          mensaje = '>Se ha anulado cierre técnicamente la orden'(sac).
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    IF NOT aufnr IS INITIAL.
      set_orden( aufnr ).
    ENDIF.
  ENDMETHOD.
  METHOD contiene_status.
    DATA: r_status  TYPE RANGE OF j_status,
          i_status  TYPE TABLE OF string,
          l_txt04   TYPE string,
          l_istat   TYPE j_status,
          lr_status LIKE LINE OF r_status,
          l_objnr   TYPE jest-objnr,
          l_aufpl   TYPE afvc-aufpl,
          l_aplzl   TYPE afvc-aplzl.

    SPLIT status AT ',' INTO TABLE i_status.

    LOOP AT i_status INTO l_txt04.
      SELECT istat FROM tj02t                           "#EC CI_GENBUFF
        INTO l_istat
       WHERE spras = spras
         AND txt04 = l_txt04.
*    IF sy-subrc = 0.
        CLEAR lr_status.
        lr_status-option = 'EQ'.
        lr_status-sign   = 'I'.
        lr_status-low    = l_istat.
        APPEND lr_status TO r_status.
*    ENDIF.
      ENDSELECT.
    ENDLOOP.

    CLEAR i_status.
    SPLIT status_int AT ',' INTO TABLE i_status.
    LOOP AT i_status INTO l_txt04.
      CLEAR lr_status.
      lr_status-option = 'EQ'.
      lr_status-sign   = 'I'.
      lr_status-low    = l_txt04.
      APPEND lr_status TO r_status.
    ENDLOOP.

    IF NOT status_excluir IS INITIAL.
      REFRESH i_status.
      SPLIT status_excluir AT ',' INTO TABLE i_status.
      LOOP AT i_status INTO l_txt04.
        SELECT istat FROM tj02t                         "#EC CI_GENBUFF
          INTO l_istat
          UP TO 1 ROWS
         WHERE spras = spras
           AND txt04 = l_txt04.
        ENDSELECT.
        IF sy-subrc = 0.
          CLEAR lr_status.
          lr_status-option = 'EQ'.
          lr_status-sign   = 'E'.
          lr_status-low    = l_istat.
          APPEND lr_status TO r_status.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF r_status IS INITIAL.
      RETURN.
    ENDIF.

    IF NOT objnr IS INITIAL.
      l_objnr = objnr.
    ELSE.
      IF vornr IS INITIAL.
        CONCATENATE 'OR' aufnr INTO l_objnr.
      ELSE.
        SELECT afvc~aufpl aplzl
          INTO (l_aufpl, l_aplzl)
          UP TO 1 ROWS
          FROM afko JOIN afvc ON afko~aufpl = afvc~aufpl
         WHERE aufnr = aufnr
           AND vornr = vornr.
        ENDSELECT.
        CONCATENATE 'OV' l_aufpl l_aplzl INTO l_objnr.
      ENDIF.
    ENDIF.

    SELECT SINGLE stat FROM  jest
      INTO l_istat
     WHERE objnr  = l_objnr
       AND stat  IN r_status
       AND inact  = ''.
    IF sy-subrc = 0.
      si_contiene = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD contiene_status_orden.
    DATA: l_objnr        TYPE jest-objnr,
          l_aufpl        TYPE afvc-aufpl,
          l_aplzl        TYPE afvc-aplzl,
          l_jsto         TYPE jsto,
          l_status_orden TYPE t_status_orden.

    FIELD-SYMBOLS <status> TYPE t_status_orden.

    READ TABLE i_status_orden TRANSPORTING NO FIELDS  WITH KEY aufnr = aufnr  "#EC PREF_LINE_EX
                                                               vornr = vornr
         BINARY SEARCH.
    IF sy-subrc <> 0.
      IF vornr IS INITIAL.
        CONCATENATE 'OR' aufnr INTO l_objnr.
      ELSE.
        SELECT afvc~aufpl aplzl
          INTO (l_aufpl, l_aplzl)
          UP TO 1 ROWS
          FROM afko JOIN afvc ON afko~aufpl = afvc~aufpl
         WHERE aufnr = aufnr
           AND vornr = vornr.
        ENDSELECT.
        CONCATENATE 'OV' l_aufpl l_aplzl INTO l_objnr.
      ENDIF.

      IF usuario IS INITIAL.
        SELECT  * FROM  jest JOIN tj02t ON jest~stat = tj02t~istat "#EC CI_BUFFJOIN
          APPENDING CORRESPONDING FIELDS OF TABLE i_status_orden
         WHERE objnr = l_objnr
           AND spras = spras
           AND inact = ''.
      ELSE.

        CONCATENATE 'OR' aufnr INTO l_jsto-objnr.
        SELECT SINGLE stsma FROM  jsto
          INTO l_jsto-stsma
         WHERE objnr = l_jsto-objnr.

        SELECT  txt04 estat FROM  jest JOIN tj30t ON jest~stat = tj30t~estat "#EC CI_BUFFJOIN
          INTO (l_status_orden-txt04, l_status_orden-istat)
         WHERE objnr = l_objnr
           AND spras = spras
           AND inact = ''
           AND stsma = l_jsto-stsma.
          APPEND l_status_orden TO i_status_orden.
        ENDSELECT.
      ENDIF.

      LOOP AT i_status_orden ASSIGNING <status> WHERE aufnr IS INITIAL.
        <status>-aufnr = aufnr.
        <status>-vornr = vornr.
      ENDLOOP.

      SORT i_status_orden.
    ENDIF.

    READ TABLE i_status_orden TRANSPORTING NO FIELDS WITH KEY aufnr = aufnr "#EC PREF_LINE_EX
                                                              vornr = vornr
                                                              txt04 = status
         BINARY SEARCH.
    IF sy-subrc = 0.
      si_contiene = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD contiene_status_orden_rango.
    DATA: i_status  TYPE TABLE OF tj02t,
          l_status  TYPE tj02t,
          lr_status TYPE range_c4,
          l_exc     TYPE c LENGTH 1.

    IF r_status IS INITIAL.
      si_contiene = 'X'.
    ELSE.
      SELECT * FROM tj02t                               "#EC CI_GENBUFF
        INTO TABLE i_status
       WHERE txt04 IN r_status
         AND spras  = spras
       ORDER BY PRIMARY KEY.
      IF sy-subrc <> 0.
        si_contiene = 'X'.
      ELSE.
        LOOP AT i_status INTO l_status.
          si_contiene = contiene_status_orden( aufnr  = aufnr
                                               status = l_status-txt04
                                               spras  = spras
                                               vornr  = vornr  ).
          IF or IS INITIAL.
            IF si_contiene IS INITIAL.
              EXIT.
            ENDIF.
          ELSE.
            IF si_contiene = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

        LOOP AT r_status INTO lr_status WHERE    sign = 'E'
                                              OR ( sign = 'I' AND option = 'NE' ).
          LOOP AT i_status_orden TRANSPORTING NO FIELDS WHERE     aufnr = aufnr "#EC PREF_LINE_EX
                                                              AND vornr = vornr
                                                              AND txt04 = lr_status-low.
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            l_exc = 'X'.
            CLEAR si_contiene.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF l_exc IS INITIAL.
          LOOP AT r_status TRANSPORTING NO FIELDS WHERE sign = 'I' AND option <> 'NE'.
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            si_contiene = 'X'.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD contiene_status_us_orden_rango.
    DATA: l_jsto    TYPE jsto,
          i_status  TYPE TABLE OF tj30t,
          l_status  TYPE tj30t,
          lr_status TYPE range_c4,
          l_exc     TYPE c LENGTH 1.

    IF r_status IS INITIAL.
      si_contiene = 'X'.
    ELSE.
      IF stsma IS INITIAL.
        CONCATENATE 'OR' aufnr INTO l_jsto-objnr.
        SELECT SINGLE stsma FROM  jsto
          INTO l_jsto-stsma
         WHERE objnr = l_jsto-objnr.
      ELSE.
        l_jsto-stsma = stsma.
      ENDIF.

      IF l_jsto-stsma IS INITIAL.
        SELECT txt04 FROM tj30t                         "#EC CI_GENBUFF
          INTO CORRESPONDING FIELDS OF TABLE i_status
         WHERE txt04 IN r_status
           AND spras  = spras
          ORDER BY PRIMARY KEY.
      ELSE.
        SELECT txt04 FROM tj30t                         "#EC CI_GENBUFF
          INTO CORRESPONDING FIELDS OF TABLE i_status
         WHERE txt04 IN r_status
           AND spras  = spras
           AND stsma  = l_jsto-stsma
          ORDER BY PRIMARY KEY.
      ENDIF.

      IF sy-subrc <> 0.
        si_contiene = 'X'.
      ELSE.
        LOOP AT i_status INTO l_status.
          si_contiene = contiene_status_orden( aufnr  = aufnr
                                               status = l_status-txt04
                                               spras  = spras
                                               vornr  = vornr
                                               usuario = 'X'  ).
          IF si_contiene IS INITIAL.
            EXIT.
          ENDIF.
        ENDLOOP.

        LOOP AT r_status INTO lr_status WHERE    sign = 'E'
                                              OR ( sign = 'I' AND option = 'NE' ).
          LOOP AT i_status_orden TRANSPORTING NO FIELDS WHERE     aufnr = aufnr "#EC PREF_LINE_EX
                                                              AND vornr = vornr
                                                              AND txt04 = lr_status-low.
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            l_exc = 'X'.
            CLEAR si_contiene.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF l_exc IS INITIAL.
          LOOP AT r_status TRANSPORTING NO FIELDS WHERE sign = 'I' AND option <> 'NE'. "#EC PREF_LINE_EX
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            si_contiene = 'X'.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD espera_si_bloqueada.
    DATA bloqueado TYPE c LENGTH 1.

    DO segundos_espera TIMES.
      bloqueado = esta_bloqueada( aufnr ).
      IF bloqueado = 'X'.
        message = |Orden { aufnr ALPHA = OUT } bloqueada por { sy-msgv1 }|.
        WAIT UP TO 1 SECONDS.
      ELSE.
        CLEAR message.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD esta_bloqueada.
    bloqueado = zcl_ap_utils=>comprobar_bloqueo( tabla = 'AUFK'
                                                 clave = sy-mandt
                                                 clave2 = aufnr ).

    IF bloqueado IS INITIAL.
      SELECT SINGLE rsnum FROM afko
        INTO @DATA(l_rsnum)
       WHERE aufnr = @aufnr.
      IF sy-subrc = 0.
        bloqueado = zcl_ap_utils=>comprobar_bloqueo( tabla = 'RKPF'
                                                     clave = sy-mandt
                                                     clave2 = l_rsnum ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD free.
    CLEAR: aufk, afko, caufvd, i_header, header, i_position, position, i_phase,
           phase, i_component, component.
  ENDMETHOD.
  METHOD get_almacen.
    SELECT lgort FROM afpo
      INTO lgort
      UP TO 1 ROWS
     WHERE aufnr  = aufnr
       AND lgort <> ''
       AND xloek  = ''.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_capacidad_puesto_trabajo.
    DATA l_kapid TYPE crca-kapid.

    SELECT kapid FROM crhd
      INTO l_kapid
      UP TO 1 ROWS
    WHERE objty = 'A'
      AND arbpl = arbpl
      AND werks = werks.
    ENDSELECT.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CR_CAPACITY_AVAILABLE_TIMES'
      EXPORTING
        kapid                     = l_kapid
        datuv                     = begda
        datub                     = endda
      TABLES
        et_capacity_exact         = i_capacidad
      EXCEPTIONS
        startdate_greater_enddate = 1
        not_found                 = 2
        OTHERS                    = 3.
    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando capacidad de puesto de trabajo' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_capacidades.
    DATA: l_aufpl TYPE afko-aufpl,
          l_kbed  TYPE kbed.
    DATA i_kbed2 TYPE TABLE OF kbed.

    CLEAR: i_kapm, message.
    IF aufpl IS INITIAL.
      SELECT SINGLE aufpl FROM afko
        INTO l_aufpl
       WHERE aufnr = aufnr.
    ELSE.
      l_aufpl = aufpl.
    ENDIF.

    IF l_aufpl IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM kbed                                  "#EC CI_NOFIELD
      INTO TABLE i_kbed
    WHERE aufpl = l_aufpl.

    IF i_kbed IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM kako                             "#EC CI_NO_TRANSFORM
      INTO TABLE i_kako
     FOR ALL ENTRIES IN i_kbed
   WHERE kapid = i_kbed-kapid.

    READ TABLE i_kbed INTO l_kbed WITH KEY split  = '0'.

    CALL FUNCTION 'CR_SINGLE_CAPACITIES'
      EXPORTING
        up_kapid             = l_kbed-kapid
      TABLES
        tab_single           = i_kapm
      EXCEPTIONS
        not_found            = 01
        no_single_capacities = 02.
    IF sy-subrc <> 0.
      message = |Error { sy-subrc } leyendo capacidades |.
      RETURN.
    ENDIF.

*Capacidades de ordenes cerradas
    SELECT split kapid FROM afru
      INTO CORRESPONDING FIELDS OF TABLE i_kbed2
     WHERE aufnr = l_aufpl
       AND split > 0
       AND satza = 'B10'
     GROUP BY split kapid.

    LOOP AT i_kbed2 ASSIGNING FIELD-SYMBOL(<kbed>).
      IF NOT line_exists( i_kbed[ kapid = <kbed>-kapid split = <kbed>-split ] ).
        APPEND <kbed> TO i_kbed.
      ENDIF.
    ENDLOOP.

* Borramos de I_KAPM las no asignadas a split
    LOOP AT i_kapm ASSIGNING FIELD-SYMBOL(<kapm>).
      LOOP AT i_kbed TRANSPORTING NO FIELDS WHERE kapid = <kapm>-kapie. "#EC PREF_LINE_EX
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        DELETE i_kapm.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_capacidades_equipo.
    DATA: t_kako TYPE TABLE OF rcrkk,
          t_kapm TYPE TABLE OF kapm.

    CLEAR: i_kako, i_kapm.
    CALL FUNCTION 'CR_CAPACITIES_OF_WORKCENTER'
      EXPORTING
        arbid               = arbid
*       ARBPL               = ' '
*       WERKS               = ' '
      TABLES
        t_kako              = t_kako
*       E_KAKO              =
      EXCEPTIONS
        missing_parameters  = 1
        not_found           = 2
        no_capacities_found = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      message = |Error { sy-subrc } leyendo capacidades del recurso|.
      RETURN.
    ENDIF.

    IF NOT kapar IS INITIAL.
      DELETE t_kako WHERE kapar <> kapar.
    ENDIF.

    MOVE-CORRESPONDING t_kako TO i_kako.

    LOOP AT t_kako ASSIGNING FIELD-SYMBOL(<kako>).
      CLEAR t_kapm.
      CALL FUNCTION 'CR_SINGLE_CAPACITIES'
        EXPORTING
          up_kapid             = <kako>-kapid
        TABLES
          tab_single           = t_kapm
        EXCEPTIONS
          not_found            = 1
          no_single_capacities = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        message = |Error { sy-subrc } leyendo capacidades de la capacidad { <kako>-kapid }|.
        RETURN.
      ELSE.
        MOVE-CORRESPONDING t_kapm TO i_kapm.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_caufvd.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_aufnr TYPE aufnr.

    SELECT SINGLE aufnr FROM afko
      INTO l_aufnr
     WHERE aufnr = aufnr.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'ALM_ME_ORDER_HEADER_GETDETAIL'
      EXPORTING
        i_aufnr          = aufnr
*       I_ENQUEUE_ORDER  =
*       NO_DETAILS       =
      IMPORTING
*       E_AUFPL          =
        e_caufvd         = caufvd
*       E_AFFLD          =
*       E_PMSDO          =
*       E_ORDER_HEADER   =
      EXCEPTIONS
        order_locked     = 1
        order_read_error = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD get_clase.
    SELECT SINGLE auart FROM aufk
      INTO auart
     WHERE aufnr = aufnr.
  ENDMETHOD.
  METHOD get_ctd_movs_mm.
    TYPES: BEGIN OF t_movs,
             matnr TYPE mseg-matnr,
             shkzg TYPE mseg-shkzg,
             menge TYPE mseg-menge,
             meins TYPE mseg-meins,
           END OF t_movs.

    FIELD-SYMBOLS <mov> TYPE t_movs.

    DATA: ri_matnr        TYPE rstt_t_range_string,
          ri_charg        TYPE RANGE OF charg_d,
          ri_aufnr        TYPE RANGE OF aufnr,
          ri_werks        LIKE RANGE OF werks,
          ri_lgort        LIKE RANGE OF lgort,
          ri_sgtxt        TYPE RANGE OF mseg-sgtxt,
          ri_rspos        TYPE RANGE OF mseg-rspos,
          ri_bwart        TYPE RANGE OF bwart,
          r_bwart_string  TYPE RANGE OF string,
          lr_matnr        LIKE LINE OF ri_matnr,
          lr_charg        LIKE LINE OF ri_charg,
          lr_aufnr        LIKE LINE OF ri_aufnr,
          lr_werks        LIKE LINE OF ri_werks,
          lr_lgort        LIKE LINE OF ri_lgort,
          lr_sgtxt        LIKE LINE OF ri_sgtxt,
          lr_rspos        LIKE LINE OF ri_rspos,
          lr_bwart        LIKE LINE OF ri_bwart,
          lr_bwart_string LIKE LINE OF r_bwart_string,
          i_movs          TYPE TABLE OF t_movs.

    CLEAR: cantidad, message.

    IF NOT matnr IS INITIAL.
      CLEAR lr_matnr.
      lr_matnr-option = 'EQ'.
      lr_matnr-sign   = 'I'.
      lr_matnr-low    = matnr.
      APPEND lr_matnr TO ri_matnr.
    ENDIF.

    IF NOT ini_mat IS INITIAL.
      ri_matnr = zcl_ap_string=>lista2rango( lista = ini_mat option = 'LK' ).
    ENDIF.

    IF NOT charg IS INITIAL.
      CLEAR lr_charg.
      lr_charg-option = 'EQ'.
      lr_charg-sign   = 'I'.
      lr_charg-low    = charg.
      APPEND lr_charg TO ri_charg.
    ENDIF.

    IF NOT aufnr IS INITIAL.
      CLEAR lr_aufnr.
      lr_aufnr-option = 'EQ'.
      lr_aufnr-sign   = 'I'.
      lr_aufnr-low    = aufnr.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = lr_aufnr-low ).
      APPEND lr_aufnr TO ri_aufnr.
    ENDIF.

    IF NOT werks IS INITIAL.
      CLEAR lr_werks.
      lr_werks-option = 'EQ'.
      lr_werks-sign   = 'I'.
      lr_werks-low    = werks.
      APPEND lr_werks TO ri_werks.
    ENDIF.

    IF NOT lgort IS INITIAL.
      CLEAR lr_lgort.
      lr_lgort-option = 'EQ'.
      lr_lgort-sign   = 'I'.
      lr_lgort-low    = lgort.
      APPEND lr_lgort TO ri_lgort.
    ENDIF.

    IF NOT sgtxt IS INITIAL.
      CLEAR lr_lgort.
      lr_sgtxt-option = 'EQ'.
      lr_sgtxt-sign   = 'I'.
      lr_sgtxt-low    = lgort.
      APPEND lr_sgtxt TO ri_sgtxt.
    ENDIF.

    IF NOT rspos IS INITIAL.
      CLEAR lr_rspos.
      lr_rspos-option = 'EQ'.
      lr_rspos-sign   = 'I'.
      lr_rspos-low    = rspos.
      APPEND lr_rspos TO ri_rspos.
    ENDIF.

    IF bwart IS INITIAL.
      ri_bwart = r_bwart.
    ELSE.
      CLEAR lr_bwart.
      lr_bwart-option = 'EQ'.
      lr_bwart-sign   = 'I'.
      lr_bwart-low    = bwart.
      APPEND lr_bwart TO ri_bwart.

      IF NOT bwart2 IS INITIAL.
        CLEAR lr_bwart.
        lr_bwart-option = 'EQ'.
        lr_bwart-sign   = 'I'.
        lr_bwart-low    = bwart2.
        APPEND lr_bwart TO ri_bwart.
      ENDIF.
    ENDIF.

    IF NOT l_bwart IS INITIAL.
      r_bwart_string = zcl_ap_string=>lista2rango( l_bwart ).

      LOOP AT r_bwart_string INTO lr_bwart_string.
        MOVE-CORRESPONDING lr_bwart_string TO lr_bwart.
        APPEND lr_bwart TO ri_bwart.
      ENDLOOP.
    ENDIF.

    CLEAR cantidad.

* Busco en los pendientes de procesar
    IF sgtxt IS INITIAL.
      SELECT matnr shkzg SUM( menge ) meins
        INTO TABLE i_movs
        FROM aufm
       WHERE matnr IN ri_matnr
         AND charg IN ri_charg
         AND bwart IN ri_bwart
         AND aufnr IN ri_aufnr
         AND budat IN r_budat
         AND werks IN ri_werks
         AND lgort IN ri_lgort
         AND rspos IN ri_rspos
       GROUP BY matnr shkzg meins.
    ELSE.
      SELECT aufm~matnr aufm~shkzg SUM( aufm~menge ) aufm~meins
        INTO TABLE i_movs
        FROM aufm JOIN mseg ON  aufm~mblnr = mseg~mblnr
                            AND aufm~mjahr = mseg~mjahr
                            AND aufm~zeile = mseg~zeile
       WHERE aufm~matnr IN ri_matnr
         AND aufm~charg IN ri_charg
         AND aufm~bwart IN ri_bwart
         AND aufm~aufnr IN ri_aufnr
         AND aufm~budat IN r_budat
         AND aufm~werks IN ri_werks
         AND aufm~lgort IN ri_lgort
         AND aufm~rspos IN ri_rspos
         AND sgtxt      IN ri_sgtxt
       GROUP BY aufm~matnr aufm~shkzg aufm~meins.
    ENDIF.

    LOOP AT i_movs ASSIGNING <mov>.
      IF meins <> '' AND meins <> <mov>-meins AND <mov>-menge <> 0.
        <mov>-menge = zcl_ap_material=>convertir_unidad( matnr = <mov>-matnr unidad_origen = <mov>-meins unidad_destino = meins cantidad = <mov>-menge ).
        IF <mov>-menge = 0.
          message = |Material { <mov>-matnr ALPHA = OUT } no tiene conversión de { <mov>-meins } a { meins }|.
        ENDIF.
      ENDIF.
      IF <mov>-shkzg = 'S'.
        cantidad = cantidad + <mov>-menge.
      ELSE.
        cantidad = cantidad - <mov>-menge.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_ctd_notif.
    DATA: ri_vornr     TYPE RANGE OF vornr,
          lr_vornr     LIKE LINE OF ri_vornr,
          l_notif      TYPE afru-gmnga,
          l_recha      TYPE afru-xmnga,
          l_notif_anul TYPE afru-gmnga,
          l_recha_anul TYPE afru-xmnga.

    IF NOT vornr IS INITIAL.
      CLEAR lr_vornr.
      lr_vornr-option = 'EQ'.
      lr_vornr-sign   = 'I'.
      lr_vornr-low    = vornr.
      APPEND lr_vornr TO ri_vornr.
    ENDIF.

    SELECT SUM( xmnga ) SUM( xmnga ) FROM afru
      INTO (l_notif, l_recha)
     WHERE aufnr  = aufnr
       AND vornr IN ri_vornr
       AND stokz  = ''.

    SELECT SUM( xmnga ) SUM( xmnga ) FROM afru
      INTO (l_notif_anul, l_recha_anul)
     WHERE aufnr  = aufnr
       AND vornr IN ri_vornr
       AND stokz  = 'X'.

    IF tipo = 'N'.
      cantidad = l_notif - l_notif_anul.
    ELSE.
      cantidad = l_recha - l_recha_anul.
    ENDIF.
  ENDMETHOD.
  METHOD get_datos_cabecera.
    CALL FUNCTION 'ALM_ME_ORDER_HEADER_GETDETAIL'
      EXPORTING
        i_aufnr          = aufk-aufnr
*       I_ENQUEUE_ORDER  =
*       NO_DETAILS       =
      IMPORTING
*       E_AUFPL          =
        e_caufvd         = caufvd
*       E_AFFLD          =
*       E_PMSDO          =
*       E_ORDER_HEADER   =
      EXCEPTIONS
        order_locked     = 1
        order_read_error = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD get_datos_costes.
    DATA l_kkbcs_out TYPE kkbcs_out.

    SET PARAMETER ID 'ZIC' FIELD 'X'.                       "#EC *

    CALL FUNCTION 'CO_COST_SHOW_ORDER_COSTS'
      EXPORTING
        caufvd_imp = caufvd.
    SET PARAMETER ID 'ZIC' FIELD ''.                        "#EC *

    CLEAR: g_t_kkbcs_out, g_t_kkbcs.
    IMPORT g_t_kkbcs_out TO g_t_kkbcs_out
           g_t_kkbcs     TO g_t_kkbcs FROM MEMORY ID 'DATOS_COSTES'.
    FREE MEMORY ID 'DATOS_COSTES'.

    DELETE g_t_kkbcs WHERE wrttp <> '01'.
    LOOP AT g_t_kkbcs_out INTO kkbcs_out.
      CLEAR: kkbcs_out-plankost_g, kkbcs_out-planmeng_g.
      LOOP AT g_t_kkbcs INTO kkbcs WHERE     wrttp = '01'
                                         AND kstar = kkbcs_out-kstar
                                         AND herku = kkbcs_out-herku
                                         AND hkgrp = kkbcs_out-hkgrp
                                         AND beweg = kkbcs_out-beweg.
        kkbcs_out-plankost_g = kkbcs_out-plankost_g + kkbcs-wkg000.
        kkbcs_out-planmeng_g = kkbcs_out-planmeng_g + kkbcs-meg000.
        DELETE g_t_kkbcs.
      ENDLOOP.
      MODIFY g_t_kkbcs_out FROM kkbcs_out.
    ENDLOOP.

    LOOP AT g_t_kkbcs INTO kkbcs.
      CLEAR kkbcs_out.
      MOVE-CORRESPONDING kkbcs TO kkbcs_out.
      kkbcs_out-plankost_g = kkbcs-wkg000.
      kkbcs_out-planmeng_g = kkbcs-meg000.

      READ TABLE g_t_kkbcs_out INTO l_kkbcs_out WITH KEY herku = kkbcs-herku
                                                        pmawr = kkbcs-pmawr
                                                        belar = kkbcs-belar
                                                        kstar = kkbcs-kstar
                                                        herkz = kkbcs-herkz
                                                        beweg = kkbcs-beweg.
      IF sy-subrc = 0.
        IF kkbcs_out-meinh IS INITIAL.
          kkbcs_out-meinh = l_kkbcs_out-meinh.
        ENDIF.
        IF kkbcs_out-hkgrp IS INITIAL.
          kkbcs_out-hkgrp = l_kkbcs_out-hkgrp.
        ENDIF.
        IF kkbcs_out-waers IS INITIAL.
          kkbcs_out-waers = l_kkbcs_out-waers.
        ENDIF.
      ENDIF.
      COLLECT kkbcs_out INTO g_t_kkbcs_out.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_detalle.
    DATA: l_obj_pi TYPE bapi_pi_order_objects,
          l_return TYPE bapiret2,
          l_obj_pp TYPE bapi_pp_order_objects.

    CLEAR: i_header, i_position, i_component, i_operation.

    IF aufk-autyp = '40'. " Orden de proceso
      l_obj_pi-header     = header.
      l_obj_pi-positions  = positions.
      l_obj_pi-sequences  = ''.
      l_obj_pi-phases     = operations.
      l_obj_pi-components = components.

      CALL FUNCTION 'BAPI_PROCORD_GET_DETAIL'
        EXPORTING
          number        = afko-aufnr
*         COLLECTIVE_ORDER       =
          order_objects = l_obj_pi
        IMPORTING
          return        = l_return
        TABLES
          header        = i_header
          position      = i_position
*         SEQUENCE      =
          phase         = i_phase
*         TRIGGER_POINT =
          component     = i_component.
*       PROD_REL_TOOL =
    ELSEIF aufk-autyp = '10'. " Orden de fabricación
      l_obj_pp-header     = header.
      l_obj_pp-positions  = positions.
      l_obj_pp-sequences  = ''.
      l_obj_pp-operations = operations.
      l_obj_pp-components = components.

      CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
        EXPORTING
          number        = afko-aufnr
*         COLLECTIVE_ORDER       =
          order_objects = l_obj_pp
        IMPORTING
          return        = l_return
        TABLES
          header        = i_header
          position      = i_position
          component     = i_component
          operation     = i_operation.
    ENDIF.

    IF l_return-type = 'E'.
      MESSAGE ID l_return-id TYPE l_return-type NUMBER l_return-number
              WITH l_return-message_v1 l_return-message_v2.
    ENDIF.
  ENDMETHOD.
  METHOD get_material.
    SELECT SINGLE plnbez FROM caufv
      INTO matnr
     WHERE aufnr = aufnr.
  ENDMETHOD.
  METHOD get_puesto_trabajo.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_vornr TYPE vornr.

    CLEAR arbpl.
    IF vornr IS INITIAL.
      IF ktsch IS INITIAL.
        SELECT crhd~arbpl vornr FROM afko
          INNER JOIN afvc ON afvc~aufpl = afko~aufpl
          INNER JOIN crhd ON crhd~objid = afvc~arbid
          INTO (arbpl, l_vornr)
          UP TO 1 ROWS
          WHERE afko~aufnr = aufnr
            AND crhd~objty = 'A'
          ORDER BY vornr arbpl.
        ENDSELECT.
      ELSE.
        SELECT crhd~arbpl vornr FROM afko
          INNER JOIN afvc ON afvc~aufpl = afko~aufpl
          INNER JOIN crhd ON crhd~objid = afvc~arbid
          INTO (arbpl, l_vornr)
              UP TO 1 ROWS
          WHERE afko~aufnr = aufnr
            AND afvc~ktsch = ktsch
            AND crhd~objty = 'A'
          ORDER BY vornr arbpl.
        ENDSELECT.
      ENDIF.
    ELSE.
      SELECT crhd~arbpl vornr FROM afko
        INNER JOIN afvc ON afvc~aufpl = afko~aufpl
        INNER JOIN crhd ON crhd~objid = afvc~arbid
        INTO (arbpl, l_vornr)
        UP TO 1 ROWS
        WHERE afko~aufnr = aufnr
          AND afvc~vornr = vornr
          AND crhd~objty = 'A'
          ORDER BY vornr arbpl.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.
  METHOD get_recurso.
    SELECT SINGLE arbpl FROM  crhd
      INTO arbpl
      WHERE objty = objty
        AND objid = arbid.
  ENDMETHOD.
  METHOD get_saldo_liquidacion.
    DATA: l_caufv      TYPE caufv,
          i_auak       TYPE auak,
          i_control    TYPE kabr_control,
          it_setyptab  TYPE kabrt_setyp_table,
          it_objtab    TYPE TABLE OF jsto_pre,
          it_rem_obj   TYPE kabrt_objtab_table,
          it_trace_obj TYPE kabrt_objtab_table,
          ct_set       TYPE kabrt_aufsel_table,
          ct_late_obj  TYPE kabrt_objtab_table,
          ct_add_sdr   TYPE kabrt_objtab_table,
          ct_bel_all   TYPE kabrt_bel_all_table,
          ct_matnr     TYPE kabrt_cose_sel_table,
          ct_wsum      TYPE kabrt_wsum_table.

    CLEAR: saldo, i_saldos, message.

    SELECT SINGLE gltrp gltri werks objnr FROM caufv
      INTO CORRESPONDING FIELDS OF l_caufv
     WHERE aufnr = aufnr.
    IF sy-subrc <> 0.
      message = 'No existe la orden'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXPORTING
        i_identification = sy-uzeit
        check_on_commit  = ' '.

    SELECT kokrs FROM t001w JOIN t001k ON t001k~bwkey = t001w~bwkey  "#EC CI_BUFFJOIN
                            JOIN csks  ON t001k~bukrs = csks~bukrs
      INTO i_auak-kokrs
     UP TO 1 ROWS
      WHERE t001w~werks = l_caufv-werks
     ORDER BY csks~kokrs.
    ENDSELECT.

    i_auak-budat    = fecha.
    i_auak-cpudt    = sy-datum.
    i_auak-wsdat    = fecha.
    i_auak-kurst    = 'M'.
    i_auak-gjahr    = fecha(4).
    i_auak-perio    = |0{ fecha+4(2) }|.
    i_auak-co_vaart = '1'.
    i_auak-buperio  = i_auak-perio.
    i_auak-bugjahr  = i_auak-gjahr.
    i_auak-wrttp    = '04'.

    i_control-wrttp         = '04'.
    i_control-testrun       = 'X'.
    i_control-detaillist    = 'X'.
    i_control-selart        = 'OR1'.
    i_control-ok            = 'AUSF'.
    i_control-afpo_pre_read = 'X'.
    i_control-coslv_read    = 'X'.
    i_control-count_com     = '1'.
    i_control-mess_identif  = sy-uzeit.

    APPEND 'OR' TO it_setyptab.

    APPEND l_caufv-objnr TO it_objtab.

    CALL FUNCTION 'K_SETTLEMENT_GROUP_PROCESS'
      EXPORTING
        i_auak        = i_auak
        i_control     = i_control
        it_rem_obj    = it_rem_obj
        it_trace_obj  = it_trace_obj
        it_setyptab   = it_setyptab
        i_no_commit   = ''
*  IMPORTING
*       E_LST         =
      TABLES
        it_objtab     = it_objtab
      CHANGING
        ct_set        = ct_set
        ct_late_obj   = ct_late_obj
        ct_add_sdr    = ct_add_sdr
        ct_bel_all    = ct_bel_all
        ct_matnr      = ct_matnr
        ct_wsum       = ct_wsum
        ct_gsum       = i_saldos
      EXCEPTIONS
        error_message = 999.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
              sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ENDIF.

    LOOP AT i_saldos ASSIGNING FIELD-SYMBOL(<saldo>).
      saldo = saldo + <saldo>-wkg_0.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_status_hist.
    DATA: r_stsma  TYPE RANGE OF tj30t-stsma,
          lr_stsma LIKE LINE OF r_stsma,
          l_auart  TYPE aufk-auart,
          l_objnr  TYPE jcds-objnr.

    CLEAR lr_stsma.
    lr_stsma-option = 'EQ'.
    lr_stsma-sign   = 'I'.
    IF NOT stsma IS INITIAL.
      lr_stsma-low = stsma.
      APPEND lr_stsma TO r_stsma.
    ELSEIF NOT aufnr IS INITIAL.
      SELECT SINGLE auart FROM aufk
        INTO l_auart
       WHERE aufnr = aufnr.
      IF sy-subrc = 0.
        SELECT SINGLE stsma FROM t003o
          INTO lr_stsma-low
         WHERE auart = l_auart.
        IF NOT lr_stsma-low IS INITIAL.
          APPEND lr_stsma TO r_stsma.
        ENDIF.
      ENDIF.
    ENDIF.

    IF r_stsma IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR jcds.
    CONCATENATE 'OR' aufnr INTO l_objnr.
    IF registro = 'U'.
      SELECT * FROM jcds JOIN tj30t ON jcds~stat = tj30t~estat "#EC CI_BUFFJOIN "#EC CI_EXIT_SELECT
        INTO CORRESPONDING FIELDS OF jcds
        UP TO 1 ROWS
       WHERE objnr  = l_objnr
         AND stsma IN r_stsma
         AND txt04  = status
         AND inact  = ''
         AND spras  = spras
        ORDER BY udate DESCENDING utime DESCENDING.
      ENDSELECT.
    ELSEIF registro = 'P'.
      SELECT * FROM jcds JOIN tj30t ON jcds~stat = tj30t~estat "#EC CI_BUFFJOIN "#EC CI_EXIT_SELECT
        INTO CORRESPONDING FIELDS OF jcds
        UP TO 1 ROWS
       WHERE objnr  = l_objnr
         AND stsma IN r_stsma
         AND txt04  = status
         AND inact  = ''
         AND spras  = spras
        ORDER BY udate ASCENDING utime ASCENDING.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.
  METHOD get_status_st.
    DATA: l_objnr     TYPE jest-objnr,
          l_user_line TYPE bsvx-sttxt.

* Tablas estatus JEST y de cambios de estatus JCDS

    IF NOT objnr IS INITIAL.
      l_objnr = objnr.
    ELSE.
      CONCATENATE 'OR' aufnr INTO l_objnr.
    ENDIF.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
*       CLIENT           = SY-MANDT
        flg_user_stat    = usuario
        objnr            = l_objnr
*       ONLY_ACTIVE      = 'X'
        spras            = spras
        bypass_buffer    = bypass_buffer
      IMPORTING
*       ANW_STAT_EXISTING       =
*       E_STSMA          =
        line             = status
        user_line        = l_user_line
*       STONR            =
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      CLEAR status.
    ENDIF.

    IF usuario = 'X'.
      status = l_user_line.
    ENDIF.
  ENDMETHOD.
  METHOD get_texto.
    DATA: l_ktext TYPE aufk-ktext,
          l_ltext TYPE aufk-ltext,
          l_name  TYPE stxh-tdname,
          l_aufpl TYPE afko-aufpl,
          l_aplzl TYPE afvc-aplzl.

    IF vornr IS INITIAL.
      SELECT SINGLE ktext ltext FROM aufk
        INTO (l_ktext, l_ltext)
       WHERE aufnr = aufnr.
      IF l_ltext <> '' AND largo = 'X'.
        CONCATENATE sy-mandt aufnr INTO l_name.
        texto = zcl_ap_textos=>get_texto_string( object = 'AUFK' name = l_name id = 'KOPF' ).
        IF texto IS INITIAL.
          texto = l_ktext.
        ENDIF.
      ELSE.
        texto = l_ktext.
      ENDIF.
    ELSE.
      SELECT SINGLE aufpl FROM afko
        INTO l_aufpl
       WHERE aufnr = aufnr.
      IF NOT l_aufpl IS INITIAL.
        SELECT aplzl, ltxa1, txtsp
          FROM afvc
          WHERE aufpl = @l_aufpl
            AND vornr = @vornr
          ORDER BY PRIMARY KEY
          INTO (@l_aplzl, @l_ktext, @l_ltext)
          UP TO 1 ROWS.
        ENDSELECT.
        IF sy-subrc = 0.
          IF l_ltext <> '' AND largo = 'X'.
            CONCATENATE sy-mandt l_aufpl l_aplzl INTO l_name.
            texto = zcl_ap_textos=>get_texto_string( object = 'AUFK' name = l_name id = 'AVOT' ).
            IF texto IS INITIAL.
              texto = l_ktext.
            ENDIF.
          ELSE.
            texto = l_ktext.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD leer_datos_maestros_pp.
    DATA o_bi TYPE REF TO zcl_ap_batch_input.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPLCOKO1' dynpro = '0110' okcode = '/00' ).
    o_bi->campos( campo = 'CAUFVD-AUFNR' valor = aufnr ). " Número de orden

    o_bi->dynpro( program = 'SAPLCOKO1' dynpro = '0115' okcode = '=STAK' ).

    o_bi->dynpro( program = 'SAPLCOKO1' dynpro = '0131' okcode = '=ENT1' ).
    o_bi->campos( campo = 'RC62F-REV_SEL' valor = '' ). " Casilla de selección
    o_bi->campos( campo = 'RC62F-NO_SEL' valor = 'X' ). " Casilla de selección
    o_bi->campos( campo = 'RC62F-NEW_ROUT' valor = 'X' ). " Indicador: seleccionar nuevamente hoja de ruta
    o_bi->campos( campo = 'RC62F-PLAUF' valor = '31.12.2999' ). " Fecha de explosión de hoja de ruta
    o_bi->campos( campo = 'RC62F-NEW_BOM' valor = 'X' ). " Indicador: seleccionar nuevamente lista de materiales
    o_bi->campos( campo = 'RC62F-AUFLD' valor = '31.12.2999' ). " Fecha de explosión p. lista materiales y hoja de ruta

    o_bi->dynpro( program = 'SAPLCOKO1' dynpro = '0115' okcode = '=BU' ).

    message = o_bi->llamar_transaccion( tcode = 'CO02' modo = 'E' ).
    IF o_bi->msgty = 'S' AND o_bi->msgid = 'CO' AND o_bi->msgno = '100'. " Se ha grabado la orden.
      CLEAR message.
    ENDIF.
  ENDMETHOD.
  METHOD liberar.
    DATA: l_autyp           TYPE aufk-autyp,
          ls_orders         TYPE bapi_order_key,
          lti_orders        TYPE TABLE OF bapi_order_key,
          lti_detail_return TYPE TABLE OF bapi_order_return,
          ls_detail_return  TYPE bapi_order_return.

    SELECT SINGLE autyp FROM aufk
      INTO l_autyp
     WHERE aufnr = aufnr.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe la orden'(neo) aufnr INTO mensaje SEPARATED BY space.
    ELSE.
      IF contiene_status( aufnr = aufnr status = 'LIB.' spras = 'S' ) = 'X'.
        mensaje = '>Orden ya estaba liberada'(oyl).
      ELSE.
        ls_orders-order_number = aufnr.
        APPEND ls_orders TO lti_orders.

        IF l_autyp = '40'.    " Orden de proceso
          CALL FUNCTION 'BAPI_PROCORD_RELEASE'
            EXPORTING
              release_control = '1'
            TABLES
              orders          = lti_orders
              detail_return   = lti_detail_return.
        ELSEIF l_autyp = '10'. " Orden de fabricación
          CALL FUNCTION 'BAPI_PRODORD_RELEASE'
            EXPORTING
              release_control = '1'
            TABLES
              orders          = lti_orders
              detail_return   = lti_detail_return.
        ENDIF.
        READ TABLE lti_detail_return INTO ls_detail_return WITH KEY type = 'E'.
        IF sy-subrc = 0.
          mensaje = ls_detail_return-message.
        ELSE.
          zcl_ap_dev=>commit( ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD lista2rango.
    DATA: r_rango_string TYPE rstt_t_range_string,
          l_rango        TYPE range_s_aufnr.

    FIELD-SYMBOLS <rango> TYPE rstt_s_range_string.

    r_rango_string = zcl_ap_lista=>lista2rango( lista = lista separador = separador option = option permitir_vacias = permitir_vacias ).

    LOOP AT r_rango_string ASSIGNING <rango>.
      CLEAR l_rango.
      MOVE-CORRESPONDING <rango> TO l_rango.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_rango-low ).
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_rango-high ).
      APPEND l_rango TO rango.
    ENDLOOP.
  ENDMETHOD.
  METHOD modificar.
    DATA l_autyp TYPE aufk-autyp.

    SET PARAMETER ID 'ANR' FIELD aufnr.

    SELECT SINGLE autyp FROM aufk
      INTO l_autyp
     WHERE aufnr = aufnr.
    IF sy-subrc = 0.
      CASE l_autyp.
        WHEN '20'.
          CALL TRANSACTION 'CN22' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        WHEN '40'.
          CALL TRANSACTION 'COR2' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        WHEN OTHERS.
          CALL TRANSACTION 'CO02' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
      ENDCASE.
    ENDIF.
  ENDMETHOD.
  METHOD notificar.
    " TODO: parameter COMMIT is never used (ABAP cleaner)

    DATA: l_timetickets TYPE bapi_pp_timeticket,
          i_timetickets TYPE TABLE OF bapi_pp_timeticket,
          l_propose     TYPE bapi_pp_conf_prop,
          i_goodmov     TYPE TABLE OF bapi2017_gm_item_create,
          i_link        TYPE TABLE OF bapi_link_conf_goodsmov.

    FIELD-SYMBOLS <return> TYPE bapi_coru_return.

    DATA: BEGIN OF ls_afvc,
            vornr TYPE afvc-vornr,
            objnr TYPE afvc-objnr,
            rueck TYPE afvc-rueck,
            steus TYPE afvc-steus,
            matnr TYPE afpo-matnr,
            arbid TYPE afvc-arbid,
            pwerk TYPE afpo-pwerk,
          END OF ls_afvc.

    CLEAR ls_afvc.

* Buscamos la operación a notificar
    SELECT afvc~vornr afvc~objnr afvc~rueck afvc~steus afpo~matnr afvc~arbid afpo~pwerk
    INTO CORRESPONDING FIELDS OF ls_afvc
      UP TO 1 ROWS
    FROM afko INNER JOIN afvc ON afvc~aufpl = afko~aufpl
              INNER JOIN afpo ON afpo~aufnr = afko~aufnr
    WHERE afko~aufnr = aufk-aufnr
      AND afvc~vornr = vornr
     ORDER BY vornr.
    ENDSELECT.
    IF sy-subrc <> 0.
      CONCATENATE 'La orden'(lor) me->aufk-aufnr 'no tiene operación'(nto) vornr INTO mensaje SEPARATED BY space.
      RETURN.
    ENDIF.

* Comprobamos si el material está bloqueado
    CALL FUNCTION 'ENQUEUE_EMMARCS'
      EXPORTING
        mode_marc      = 'S'
        matnr          = ls_afvc-matnr
        werks          = ls_afvc-pwerk
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      IF     ( NOT sy-msgid IS INITIAL )
         AND ( NOT sy-msgty IS INITIAL )
         AND ( NOT sy-msgno IS INITIAL ).
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO mensaje.
      ELSE.
        mensaje = 'Material Bloqueado'(mbl).
      ENDIF.
      RETURN.
    ELSE.
      CALL FUNCTION 'DEQUEUE_EMMARCS'
        EXPORTING
          mode_marc = 'S'
          mandt     = sy-mandt
          matnr     = ls_afvc-matnr
          werks     = ls_afvc-pwerk.
    ENDIF.

* Comprobamos que no esté notificada
    SELECT SINGLE objnr INTO ls_afvc-objnr FROM jest
      WHERE objnr = ls_afvc-objnr
        AND stat  = 'I0009'
        AND inact = ''.
    IF sy-subrc = 0.
      CONCATENATE 'La operación'(lop) ls_afvc-vornr 'de la orden'(dlo) me->aufk-aufnr 'ya está notificada fin'(yen)
                  INTO mensaje SEPARATED BY space.
      RETURN.
    ENDIF.

    l_timetickets-orderid         = aufk-aufnr.
    l_timetickets-operation       = vornr.
    l_timetickets-plant           = ls_afvc-pwerk.
    l_timetickets-postg_date      = budat.
    l_timetickets-fin_conf        = parcial_final.
    l_timetickets-clear_res       = space.
    l_timetickets-yield           = cantidad.
    l_timetickets-scrap           = rechazo.
    l_timetickets-conf_quan_unit  = meins.
    l_timetickets-exec_start_date = fecha_ini.
    l_timetickets-exec_fin_date   = fecha_fin.
    l_timetickets-exec_start_time = hora_ini.
    l_timetickets-exec_fin_time   = hora_fin.
    l_timetickets-conf_text       = texto.
    IF arbpl IS INITIAL.
      SELECT SINGLE arbpl INTO l_timetickets-work_cntr FROM crhd
        WHERE objty = 'A'
          AND objid = ls_afvc-arbid.
    ELSE.
      l_timetickets-work_cntr = arbpl.
    ENDIF.

    IF NOT act1_uni IS INITIAL.
      l_timetickets-conf_acti_unit1 = act1_uni.
      l_timetickets-conf_activity1  = act1_ctd.
    ENDIF.
    IF NOT act2_uni IS INITIAL.
      l_timetickets-conf_acti_unit2 = act2_uni.
      l_timetickets-conf_activity2  = act2_ctd.
    ENDIF.
    IF NOT act3_uni IS INITIAL.
      l_timetickets-conf_acti_unit3 = act3_uni.
      l_timetickets-conf_activity3  = act3_ctd.
    ENDIF.
    IF NOT act4_uni IS INITIAL.
      l_timetickets-conf_acti_unit4 = act4_uni.
      l_timetickets-conf_activity4  = act4_ctd.
    ENDIF.
    IF NOT act5_uni IS INITIAL.
      l_timetickets-conf_acti_unit5 = act5_uni.
      l_timetickets-conf_activity5  = act5_ctd.
    ENDIF.
    IF NOT act6_uni IS INITIAL.
      l_timetickets-conf_acti_unit6 = act6_uni.
      l_timetickets-conf_activity6  = act6_ctd.
    ENDIF.

    APPEND l_timetickets TO i_timetickets.

    IF crear_mov = 'X'.
      CLEAR l_propose.
      l_propose-GoodsMovement = 'X'.
*       Recuperamos los datos de la BAPI que notifica
      CALL FUNCTION 'BAPI_PRODORDCONF_GET_TT_PROP'
        EXPORTING
          propose            = l_propose
        TABLES
          timetickets        = i_timetickets
          goodsmovements     = i_goodmov
          link_conf_goodsmov = i_link.
    ENDIF.

* Llamamos a la función que crea la notificación
    CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
      EXPORTING
        post_wrong_entries = '2'
        testrun            = ' '
      IMPORTING
        return             = return
      TABLES
        timetickets        = i_timetickets
        goodsmovements     = i_goodmov
        link_conf_goodsmov = i_link
        detail_return      = i_detail_return.

* Comprobamos si tenemos errores
    LOOP AT i_detail_return ASSIGNING <return> WHERE type = 'E'.
      MESSAGE ID <return>-id
              TYPE 'S'
              NUMBER <return>-number
              WITH <return>-message_v1
                   <return>-message_v2
                   <return>-message_v3
                   <return>-message_v4
              INTO mensaje.
    ENDLOOP.

    IF mensaje IS INITIAL.
* Hacemos el Commit y esperamos a que se actualice todo
      zcl_ap_dev=>commit( ).

      LOOP AT i_detail_return ASSIGNING <return> WHERE         type     = 'I'
                                                       AND NOT conf_no IS INITIAL.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM afru
          INTO me->afru
         WHERE rueck = <return>-conf_no
           AND rmzhl = <return>-conf_cnt.
*    ELSE.
*      SELECT * FROM afru
*        INTO me->afru
*       WHERE aufnr = me->aufk-aufnr
*         AND vornr = vornr.
*      ENDSELECT.
      ELSE.
        mensaje = 'Ha habido un error durante la notificación'(hen).
      ENDIF.
    ELSE.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.
  ENDMETHOD.
  METHOD notificar_op.
    DATA: l_timetickets TYPE bapi_pi_timeticket1,
          i_timetickets TYPE TABLE OF bapi_pi_timeticket1,
          l_propose     TYPE bapi_pp_conf_prop,
          i_goodmov     TYPE TABLE OF bapi2017_gm_item_create,
          i_link        TYPE TABLE OF bapi_link_conf_goodsmov,
          i_cons        TYPE TABLE OF bapi2017_gm_item_create,
          l_cons        TYPE bapi2017_gm_item_create,
          l_return      TYPE bapi_coru_return.

    FIELD-SYMBOLS: <mov>    TYPE bapi2017_gm_item_create,
                   <return> TYPE bapi_coru_return.

    DATA: BEGIN OF ls_afvc,
            vornr TYPE afvc-vornr,
            objnr TYPE afvc-objnr,
            rueck TYPE afvc-rueck,
            steus TYPE afvc-steus,
            matnr TYPE afpo-matnr,
            arbid TYPE afvc-arbid,
            pwerk TYPE afpo-pwerk,
          END OF ls_afvc.

    CLEAR ls_afvc.

* Buscamos la operación a notificar
    SELECT afvc~vornr afvc~objnr afvc~rueck afvc~steus afpo~matnr afvc~arbid afpo~pwerk
    INTO CORRESPONDING FIELDS OF ls_afvc
      UP TO 1 ROWS
    FROM afko INNER JOIN afvc ON afvc~aufpl = afko~aufpl
              INNER JOIN afpo ON afpo~aufnr = afko~aufnr
    WHERE afko~aufnr = aufk-aufnr
      AND afvc~vornr = vornr
     ORDER BY afvc~vornr.
    ENDSELECT.
    IF sy-subrc <> 0.
      CONCATENATE 'La orden'(lor) me->aufk-aufnr 'no tiene operación'(nto) vornr INTO mensaje SEPARATED BY space.
      RETURN.
    ENDIF.

* Comprobamos si el material está bloqueado
    CALL FUNCTION 'ENQUEUE_EMMARCS'
      EXPORTING
        mode_marc      = 'S'
        matnr          = ls_afvc-matnr
        werks          = ls_afvc-pwerk
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      IF     ( NOT sy-msgid IS INITIAL )
         AND ( NOT sy-msgty IS INITIAL )
         AND ( NOT sy-msgno IS INITIAL ).
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO mensaje.
      ELSE.
        mensaje = 'Material Bloqueado'(mbl).
      ENDIF.
      RETURN.
    ELSE.
      CALL FUNCTION 'DEQUEUE_EMMARCS'
        EXPORTING
          mode_marc = 'S'
          mandt     = sy-mandt
          matnr     = ls_afvc-matnr
          werks     = ls_afvc-pwerk.
    ENDIF.

* Comprobamos que no esté notificada
    SELECT SINGLE objnr INTO ls_afvc-objnr FROM jest
      WHERE objnr = ls_afvc-objnr
        AND stat  = 'I0009'
        AND inact = ''.
    IF sy-subrc = 0.
      CONCATENATE 'La operación'(lop) ls_afvc-vornr 'de la orden'(dlo) me->aufk-aufnr 'ya está notificada fin'(ynf)
                  INTO mensaje SEPARATED BY space.
      RETURN.
    ENDIF.

    l_timetickets-orderid         = aufk-aufnr.
    l_timetickets-phase           = vornr.
    l_timetickets-plant           = ls_afvc-pwerk.
    l_timetickets-postg_date      = budat.
*           Notificación parcial
*           X Notificación final
*           1 Notificación final automática
    l_timetickets-fin_conf        = parcial_final.
    l_timetickets-clear_res       = space.
    l_timetickets-yield           = cantidad.
    l_timetickets-scrap           = rechazo.
    l_timetickets-conf_quan_unit  = meins.
    l_timetickets-exec_start_date = fecha_ini.
    l_timetickets-exec_fin_date   = fecha_fin.
    l_timetickets-exec_start_time = hora_ini.
    l_timetickets-exec_fin_time   = hora_fin.
    l_timetickets-clear_res       = 'X'. " Compensar reservas abiertas
    l_timetickets-conf_text       = texto.
    IF arbpl IS INITIAL.
      SELECT SINGLE arbpl INTO l_timetickets-resource FROM crhd
        WHERE objty = 'A'
          AND objid = ls_afvc-arbid.
    ELSE.
      l_timetickets-resource = arbpl.
    ENDIF.

    IF NOT act1_uni IS INITIAL.
      l_timetickets-conf_acti_unit1 = act1_uni.
      l_timetickets-conf_activity1  = act1_ctd.
    ENDIF.
    IF NOT act2_uni IS INITIAL.
      l_timetickets-conf_acti_unit2 = act2_uni.
      l_timetickets-conf_activity2  = act2_ctd.
    ENDIF.
    IF NOT act3_uni IS INITIAL.
      l_timetickets-conf_acti_unit3 = act3_uni.
      l_timetickets-conf_activity3  = act3_ctd.
    ENDIF.
    IF NOT act4_uni IS INITIAL.
      l_timetickets-conf_acti_unit4 = act4_uni.
      l_timetickets-conf_activity4  = act4_ctd.
    ENDIF.
    IF NOT act5_uni IS INITIAL.
      l_timetickets-conf_acti_unit5 = act5_uni.
      l_timetickets-conf_activity5  = act5_ctd.
    ENDIF.
    IF NOT act6_uni IS INITIAL.
      l_timetickets-conf_acti_unit6 = act6_uni.
      l_timetickets-conf_activity6  = act6_ctd.
    ENDIF.
    APPEND l_timetickets TO i_timetickets.

    IF NOT crear_mov IS INITIAL.
      CLEAR l_propose.
      l_propose-GoodsMovement = 'X'.
*       Recuperamos los datos de la BAPI que notifica
      CALL FUNCTION 'BAPI_PROCORDCONF_GET_TT_PROP'
        EXPORTING
          propose            = l_propose
        TABLES
          timetickets        = i_timetickets
          goodsmovements     = i_goodmov
          link_conf_goodsmov = i_link.

      IF crear_mov = 'N'.
        CLEAR i_goodmov.
      ELSE.
        i_cons = i_consumos.
        LOOP AT i_goodmov ASSIGNING <mov>.
          CLEAR l_cons.
          IF NOT <mov>-batch IS INITIAL.
            READ TABLE i_cons INTO l_cons WITH KEY material = <mov>-material
                                                   batch    = <mov>-batch.
          ELSE.
            READ TABLE i_cons INTO l_cons WITH KEY material = <mov>-material.
          ENDIF.
          IF sy-subrc = 0.
            DELETE i_cons INDEX sy-tabix.
            <mov>-entry_qnt = l_cons-entry_qnt.
            <mov>-entry_uom = l_cons-entry_uom.
          ELSE.
            DELETE i_goodmov.
          ENDIF.
        ENDLOOP.

        IF NOT i_cons IS INITIAL.
          mensaje = 'No se ha podido consumir los componentes indicados'(ncc).
          LOOP AT i_cons INTO l_cons.
            CLEAR l_return.
            l_return-id         = '398'.
            l_return-type       = 'E'.
            l_return-number     = '000'.
            l_return-message_v1 = l_cons-material.
            l_return-message_v2 = l_cons-batch.
            MESSAGE ID l_return-id
                    TYPE 'S'
                    NUMBER l_return-number
                    WITH l_return-message_v1
                         l_return-message_v2
                         l_return-message_v3
                         l_return-message_v4
                    INTO l_return-message.
            APPEND l_return TO i_detail_return.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

* Llamamos a la función que crea la notificación
    CALL FUNCTION 'BAPI_PROCORDCONF_CREATE_TT'
      EXPORTING
        post_wrong_entries = '2'
        testrun            = ' '
      IMPORTING
        return             = return
      TABLES
        timetickets        = i_timetickets
        goodsmovements     = i_goodmov
        link_conf_goodsmov = i_link
        detail_return      = i_detail_return.

* Comprobamos si tenemos errores
    LOOP AT i_detail_return ASSIGNING <return> WHERE type = 'E'.
      MESSAGE ID <return>-id
              TYPE 'S'
              NUMBER <return>-number
              WITH <return>-message_v1
                   <return>-message_v2
                   <return>-message_v3
                   <return>-message_v4
              INTO mensaje.
    ENDLOOP.

    IF mensaje IS INITIAL.
      IF commit = 'X'.
        zcl_ap_dev=>commit( ).
      ENDIF.

      LOOP AT i_detail_return ASSIGNING <return> WHERE         type     = 'I'
                                                       AND NOT conf_no IS INITIAL.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM afru
          INTO me->afru
         WHERE rueck = <return>-conf_no
           AND rmzhl = <return>-conf_cnt.

        IF sy-subrc <> 0 AND commit IS INITIAL.
          afru-rueck = <return>-conf_no.
          afru-rmzhl = <return>-conf_cnt.
        ENDIF.
*    ELSE.
*      SELECT * FROM afru
*        INTO me->afru
*       WHERE aufnr = me->aufk-aufnr
*         AND vornr = vornr.
*      ENDSELECT.
      ELSE.
        mensaje = 'Ha habido un error durante la notificación'(hen).
      ENDIF.
    ELSE.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.
  ENDMETHOD.
  METHOD notificar_st.
    DATA o_orden TYPE REF TO zcl_ap_orden_pp.

    o_orden = NEW #(
        aufnr = aufnr ).

    IF o_orden->aufk-autyp = '40'.
      mensaje = o_orden->notificar_op( vornr    = vornr
                                    cantidad = cantidad
                                    rechazo  = rechazo
                                    meins    = meins
                                    budat     = budat
                                    fecha_ini = fecha_ini
                                    hora_ini  = hora_ini
                                    fecha_fin = fecha_fin
                                    hora_fin  = hora_fin
                                    crear_mov = crear_mov
                                    parcial_final = parcial_final
                                    act1_uni  = act1_uni
                                    act1_ctd  = act1_ctd
                                    act2_uni  = act2_uni
                                    act2_ctd  = act2_ctd
                                    act3_uni  = act3_uni
                                    act3_ctd  = act3_ctd
                                    act4_uni  = act4_uni
                                    act4_ctd  = act4_ctd
                                    act5_uni  = act5_uni
                                    act5_ctd  = act5_ctd
                                    act6_uni  = act6_uni
                                    act6_ctd  = act6_ctd
                                    texto     = texto
                                    arbpl     = arbpl
                                    commit    = commit
                                    i_consumos = i_consumos ).
    ELSE.
      mensaje = o_orden->notificar( vornr    = vornr
                                    cantidad = cantidad
                                    rechazo  = rechazo
                                    meins    = meins
                                    budat     = budat
                                    fecha_ini = fecha_ini
                                    hora_ini  = hora_ini
                                    fecha_fin = fecha_fin
                                    hora_fin  = hora_fin
                                    crear_mov = crear_mov
                                    parcial_final = parcial_final
                                    act1_uni  = act1_uni
                                    act1_ctd  = act1_ctd
                                    act2_uni  = act2_uni
                                    act2_ctd  = act2_ctd
                                    act3_uni  = act3_uni
                                    act3_ctd  = act3_ctd
                                    act4_uni  = act4_uni
                                    act4_ctd  = act4_ctd
                                    act5_uni  = act5_uni
                                    act5_ctd  = act5_ctd
                                    act6_uni  = act6_uni
                                    act6_ctd  = act6_ctd
                                    arbpl     = arbpl
                                    commit    = commit
                                    texto     = texto ).

    ENDIF.

    afru = o_orden->afru.
    i_detail_return = o_orden->i_detail_return.
    return = o_orden->return.
  ENDMETHOD.
  METHOD set_orden.
    free( ).
    CLEAR: aufk, afko.
    SELECT SINGLE * FROM aufk
      INTO me->aufk
     WHERE aufnr = aufnr.
    IF sy-subrc <> 0.
      MESSAGE 'No existe esa orden'(neo) TYPE 'E'.
    ELSE.
      SELECT SINGLE * FROM afko
        INTO me->afko
       WHERE aufnr = aufnr.
    ENDIF.
  ENDMETHOD.
  METHOD ver_md04.
    DATA o_bi      TYPE REF TO zcl_ap_batch_input.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mensaje TYPE bapireturn1-message.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Lista planif. nec. visual. individual: acceso
    o_bi->dynpro( program = 'SAPMM61R' dynpro = '0300' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENTR' ).
    o_bi->campos( campo = 'RM61R-MATNR' valor = matnr ). " Número de material
    o_bi->campos( campo = 'RM61R-WERKS' valor = werks ). " Centro
    o_bi->campos( campo = 'RM61R-DFILT' valor = '' ). " Activar filtro visualización y regla selección
*   o_bi->campos( campo = 'RM61R-ERGBZ' valor = 'ZAP00002'). " Regla selección

    l_mensaje = o_bi->llamar_transaccion( tcode = 'MD04' modo = 'E' ).
  ENDMETHOD.
  METHOD ver_notificacion.
    DATA: l_aufnr TYPE aufnr,
          l_autyp TYPE aufk-autyp.

    SET PARAMETER ID 'RCK' FIELD rueck.
    SET PARAMETER ID 'RZL' FIELD rmzhl.

    SELECT SINGLE aufnr FROM afru
      INTO l_aufnr
     WHERE rueck = rueck
       AND rmzhl = rmzhl.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE autyp FROM aufk
      INTO l_autyp
     WHERE aufnr = l_aufnr.
    IF sy-subrc = 0.
      CASE l_autyp.
        WHEN '20'.
          CALL TRANSACTION 'CN23' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        WHEN '40'.
          CALL TRANSACTION 'CORT' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
        WHEN OTHERS.
          CALL TRANSACTION 'CO14' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
      ENDCASE.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar.
    TYPES: BEGIN OF t_ord,
             aufnr  TYPE caufv-aufnr,
             auart  TYPE caufv-auart,
             gstri  TYPE caufv-gstri,
             plnbez TYPE caufv-plnbez,
           END OF t_ord,
           BEGIN OF t_aufnr,
             aufnr TYPE caufv-aufnr,
           END OF t_aufnr.

    DATA: l_aufnr TYPE aufnr,
          l_autyp TYPE aufk-autyp.

    DATA: i_aufnr TYPE TABLE OF t_aufnr,
          i_ord   TYPE TABLE OF t_ord,
          l_ord   TYPE t_ord,
          l_ok    TYPE c LENGTH 1,
          l_fila  TYPE i.

    IF NOT aufnr IS INITIAL.
      l_aufnr = aufnr.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_aufnr ).

      SET PARAMETER ID 'ANR' FIELD l_aufnr.
      GET PARAMETER ID 'ANR' FIELD l_aufnr.

      SET PARAMETER ID 'BR1' FIELD l_aufnr.
      GET PARAMETER ID 'BR1' FIELD l_aufnr.

      SELECT SINGLE autyp FROM aufk
        INTO l_autyp
       WHERE aufnr = l_aufnr.
      IF sy-subrc = 0.
        CASE l_autyp.
          WHEN '20'.
            CALL TRANSACTION 'CN23' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          WHEN '30'.
            IF editar IS INITIAL.
              CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
            ELSE.
              CALL TRANSACTION 'IW32' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
            ENDIF.
          WHEN '40'.
            IF editar IS INITIAL.
              CALL TRANSACTION 'COR3' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
            ELSE.
              CALL TRANSACTION 'COR2' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
            ENDIF.
          WHEN OTHERS.
            IF editar IS INITIAL.
              CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
            ELSE.
              CALL TRANSACTION 'CO02' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
            ENDIF.
        ENDCASE.
      ENDIF.
    ELSE.
      i_aufnr = zcl_ap_lista=>lista2tabla_n12( lista = lista separador = separador ).
      DESCRIBE TABLE i_aufnr LINES sy-tfill.
      IF sy-tfill = 1.
        READ TABLE i_aufnr INTO l_aufnr INDEX 1.
        visualizar( aufnr = l_aufnr editar = editar ).
      ELSEIF sy-tfill > 1.
        SELECT * FROM caufv
          INTO CORRESPONDING FIELDS OF TABLE i_ord
          FOR ALL ENTRIES IN i_aufnr
         WHERE aufnr = i_aufnr-aufnr
          ORDER BY PRIMARY KEY.
        DESCRIBE TABLE i_ord LINES sy-tfill.
        IF sy-tfill = 1.
          READ TABLE i_ord INTO l_ord INDEX 1.
          visualizar( aufnr = l_ord-aufnr editar = editar ).
        ELSEIF sy-tfill > 1.
          SORT i_ord.
          CALL FUNCTION 'Z_POPUP_ALV'
            EXPORTING
              titulo           = 'Seleccione orden a visualizar'(SOV)
              ancho            = 40
              solo_aceptar     = ''
              seleccionar_fila = 'X'
            IMPORTING
              ok               = l_ok
              fila             = l_fila
            TABLES
              t_datos          = i_ord.

          IF l_ok = 'X'.
            READ TABLE i_ord INTO l_ord INDEX l_fila.
            IF sy-subrc = 0.
              visualizar( aufnr = l_ord-aufnr editar = editar ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar_plan.
    SET PARAMETER ID 'MPL' FIELD warpl.
    IF editar IS INITIAL.
      CALL TRANSACTION 'IP03' AND SKIP FIRST SCREEN.     "#EC CI_CALLTA
    ELSE.
      CALL TRANSACTION 'IP02' AND SKIP FIRST SCREEN.     "#EC CI_CALLTA
    ENDIF.
  ENDMETHOD.
