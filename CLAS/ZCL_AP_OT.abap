CLASS zcl_ap_ot DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES zt_ltap_creat TYPE TABLE OF ltap_creat.

    CLASS-METHODS visualizar
      IMPORTING lgnum TYPE ltak-lgnum DEFAULT '002'
                tanum TYPE any
                tapos TYPE ltap-tapos OPTIONAL.

    CLASS-METHODS crear_ot_multiple
      IMPORTING lgnum               TYPE ltak-lgnum
                bwlvs               TYPE ltak-bwlvs
                ltak                TYPE ltak_vb       OPTIONAL
                betyp               TYPE ltak-betyp    DEFAULT ''
                benum               TYPE ltak-benum    DEFAULT ''
                mostrar_mensaje     TYPE abap_bool     DEFAULT 'X'
                in_update_task      TYPE abap_bool     DEFAULT ''
                espera_a_grabado    TYPE abap_bool     DEFAULT ''
                ausfb               TYPE lvs_ausfb     DEFAULT ''
                commit_work         TYPE abap_bool     DEFAULT 'X'
                dequeue_all         TYPE abap_bool     DEFAULT ''
                solo_verificaciones TYPE abap_bool     DEFAULT ''
                usar_bi             TYPE abap_bool     DEFAULT ''
                lznum               TYPE ltak-lznum    DEFAULT ''
      CHANGING  i_ltap_creat        TYPE zt_ltap_creat
                tanum               TYPE ltak-tanum
                !message            TYPE any
                i_ltak              TYPE pdt_t_ltak_vb OPTIONAL.

    CLASS-METHODS visualizar_stock_material
      IMPORTING matnr TYPE lqua-matnr OPTIONAL
                werks TYPE lqua-werks DEFAULT zcl_c=>werks
                lgnum TYPE lqua-lgnum DEFAULT zcl_c=>lgnum
                lgtyp TYPE lqua-lgtyp OPTIONAL
                lgpla TYPE lqua-lgpla OPTIONAL
                sonum TYPE lqua-sonum DEFAULT ''
                sobkz TYPE lqua-sobkz DEFAULT ''
      PREFERRED PARAMETER matnr.

    CLASS-METHODS crear_ot_desde_nt
      IMPORTING lgnum    TYPE ltak-lgnum
                tbnum    TYPE ltak-tbnum
                t_pos    TYPE l03b_trite_t
                usar_bi  TYPE abap_bool  DEFAULT ''
                squit    TYPE rl03tsquit DEFAULT ''
      CHANGING  tanum    TYPE ltak-tanum
                !message TYPE bapireturn1-message.

    CLASS-METHODS get_stock_ubicacion
      IMPORTING matnr        TYPE lqua-matnr
                werks        TYPE lqua-werks DEFAULT '0002'
                lgnum        TYPE lqua-lgnum DEFAULT '002'
                sonum        TYPE lqua-sonum OPTIONAL
                vbeln        TYPE vbak-vbeln OPTIONAL
                lgtyp        TYPE lqua-lgtyp
                lgpla        TYPE lqua-lgpla
      RETURNING VALUE(verme) TYPE lqua-verme.

    CLASS-METHODS confirmar_ot
      IMPORTING lgnum           TYPE ltak-lgnum
                mostrar_mensaje TYPE abap_bool  DEFAULT 'X'
                in_update_task  TYPE abap_bool  DEFAULT ''
                ausfb           TYPE lvs_ausfb  DEFAULT ''
                commit_work     TYPE abap_bool  DEFAULT 'X'
                dequeue_all     TYPE abap_bool  DEFAULT ''
                tanum           TYPE ltak-tanum
                quknz           TYPE rl03tquknz DEFAULT ''
                squit           TYPE rl03tsquit DEFAULT ''
      CHANGING  i_ltap_conf     TYPE pdt_t_ltap_conf
                !message        TYPE any
                confirmacion_ok TYPE abap_bool.

    CLASS-METHODS confirmar_pos_ot
      IMPORTING lgnum           TYPE ltak-lgnum
                mostrar_mensaje TYPE abap_bool  DEFAULT 'X'
                in_update_task  TYPE abap_bool  DEFAULT ''
                ausfb           TYPE lvs_ausfb  DEFAULT ''
                commit_work     TYPE abap_bool  DEFAULT 'X'
                dequeue_all     TYPE abap_bool  DEFAULT ''
                tanum           TYPE ltak-tanum
                quknz           TYPE rl03tquknz DEFAULT ''
                ctd_conf        TYPE any        OPTIONAL
                tapos           TYPE ltap-tapos
                kzdif           TYPE lvs_kzdif  DEFAULT ''
                nquit           TYPE rl03tnquit DEFAULT ''
                ctd_dif         TYPE any        OPTIONAL
                nlpla           TYPE ltap-nlpla DEFAULT ''
                squit           TYPE rl03tsquit DEFAULT ''
      CHANGING  !message        TYPE any
                confirmacion_ok TYPE abap_bool.

    CLASS-METHODS borra_pos_ot
      IMPORTING lgnum     TYPE ltak-lgnum
                tapos     TYPE ltap-tapos
                tanum     TYPE ltak-tanum
      EXPORTING VALUE(ok) TYPE abap_bool
                !message  TYPE bapireturn1-message.

    CLASS-METHODS get_rango_lgtyp_est
      IMPORTING lgnum          TYPE t334t-lgnum
                kzear          TYPE t334t-kzear
                lgtkz          TYPE t334t-lgtkz
                bestq          TYPE t334t-bestq DEFAULT ''
                sobkz          TYPE t334t-sobkz
                lagkl          TYPE t334t-lagkl DEFAULT ''
                bwref          TYPE t334t-bwref
      RETURNING VALUE(r_lgtyp) TYPE tab_range_c3.

    CLASS-METHODS crear_ubicacion
      IMPORTING lgnum          TYPE lagp-lgnum
                lgtyp          TYPE lagp-lgtyp
                lgpla          TYPE lagp-lgpla
                lgber          TYPE lagp-lgber DEFAULT '001'
                lptyp          TYPE lagp-lptyp DEFAULT ''
      RETURNING VALUE(mensaje) TYPE string.

    CLASS-METHODS ubic_perm_stock_neg
      IMPORTING lgnum        TYPE lgnum DEFAULT zcl_c=>lgnum
                lgtyp        TYPE lgtyp OPTIONAL
      PREFERRED PARAMETER lgtyp
      RETURNING VALUE(negat) TYPE t331-negat.

    CLASS-METHODS anular_ot
      IMPORTING lgnum     TYPE ltak-lgnum
                tanum     TYPE ltak-tanum
      EXPORTING VALUE(ok) TYPE abap_bool
                !message  TYPE bapireturn1-message.

    CLASS-METHODS esta_bloqueada
      IMPORTING lgnum            TYPE lgnum
                tanum            TYPE tanum
      RETURNING VALUE(bloqueado) TYPE abap_bool.

    CLASS-METHODS anular_pos_ot
      IMPORTING lgnum          TYPE ltak-lgnum
                tapos          TYPE ltap-tapos
                tanum          TYPE ltak-tanum
                !commit        TYPE abap_bool DEFAULT ''
      RETURNING VALUE(message) TYPE bapireturn1-message.

    CLASS-METHODS get_datos_usuario
      IMPORTING !uname       TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(datos) TYPE lrf_wkqu.

    CLASS-METHODS get_lgnum_def_usuario
      IMPORTING !uname       TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(lgnum) TYPE lgnum.

    CLASS-METHODS get_unidad_wm
      IMPORTING lgnum        TYPE lgnum
                matnr        TYPE matnr
      RETURNING VALUE(lhme1) TYPE lhmeh1.

    CLASS-METHODS convert_ctd_wm
      IMPORTING lgnum           TYPE lgnum
                matnr           TYPE matnr
                cantidad_origen TYPE mengv13 OPTIONAL
                unidad_origen   TYPE meins   OPTIONAL
      EXPORTING cantidad_wm     TYPE mengv13
                unidad_wm       TYPE lhmeh1.

    CLASS-METHODS crear_ot_simple
      IMPORTING lgnum               TYPE ltak-lgnum
                bwlvs               TYPE ltak-bwlvs
                ltak                TYPE ltak_vb    OPTIONAL
                betyp               TYPE ltak-betyp DEFAULT ''
                benum               TYPE ltak-benum DEFAULT ''
                mostrar_mensaje     TYPE abap_bool  DEFAULT 'X'
                in_update_task      TYPE abap_bool  DEFAULT ''
                espera_a_grabado    TYPE abap_bool  DEFAULT ''
                ausfb               TYPE lvs_ausfb  DEFAULT ''
                commit_work         TYPE abap_bool  DEFAULT 'X'
                dequeue_all         TYPE abap_bool  DEFAULT ''
                solo_verificaciones TYPE abap_bool  DEFAULT ''
                usar_bi             TYPE abap_bool  DEFAULT ''
                matnr               TYPE matnr
                anfme               TYPE any
                altme               TYPE any        DEFAULT ''
                bestq               TYPE bestq      DEFAULT ''
                werks               TYPE werks_d
                lgort               TYPE lgort_d
                sobkz               TYPE any        DEFAULT ''
                sonum               TYPE any        DEFAULT ''
                vltyp               TYPE ltap-vltyp
                vlpla               TYPE ltap-vlpla
                nltyp               TYPE ltap-nltyp
                nlpla               TYPE ltap-nlpla
                vlqnr               TYPE ltap-vlqnr OPTIONAL
                nlqnr               TYPE ltap-nlqnr OPTIONAL
                lznum               TYPE ltak-lznum DEFAULT ''
      EXPORTING tanum               TYPE any
                !message            TYPE any
                i_ltak              TYPE pdt_t_ltak_vb.

    CLASS-METHODS buscar_ots
      IMPORTING lgnum      TYPE lgnum
                nltyp      TYPE lgtyp     OPTIONAL
                nlpla      TYPE lgpla     OPTIONAL
                matnr      TYPE matnr     OPTIONAL
                confirmada TYPE abap_bool DEFAULT 'X'
                anulada    TYPE abap_bool DEFAULT 'X'
                lznum      TYPE any       OPTIONAL
                get_lista  TYPE abap_bool
      EXPORTING i_ltap     TYPE tt_ltap
                lista_ots  TYPE any.

    CLASS-METHODS get_ctd_ubicacion
      IMPORTING lgnum           TYPE lgnum
                lgtyp           TYPE lgtyp
                lgpla           TYPE lgpla
                disponible      TYPE abap_bool DEFAULT 'X'
                entradas        TYPE abap_bool DEFAULT ''
                salidas         TYPE abap_bool DEFAULT ''
                unidad_salida   TYPE meins
      RETURNING VALUE(cantidad) TYPE mengv13.

    CLASS-METHODS get_lqua
      IMPORTING matnr         TYPE lqua-matnr OPTIONAL
                werks         TYPE lqua-werks DEFAULT zcl_c=>werks
                lgnum         TYPE lqua-lgnum DEFAULT zcl_c=>lgnum
                sonum         TYPE lqua-sonum OPTIONAL
                vbeln         TYPE vbak-vbeln OPTIONAL
                lgtyp         TYPE lqua-lgtyp OPTIONAL
                lgpla         TYPE lqua-lgpla OPTIONAL
                posnr         TYPE vbap-posnr OPTIONAL
                bestq         TYPE lqua-bestq DEFAULT '*'
                solo_disp     TYPE abap_bool  DEFAULT ''
                sobkz         TYPE sobkz      OPTIONAL
                new_meins     TYPE meins      DEFAULT ''
      RETURNING VALUE(i_lqua) TYPE lqua_t.

    CLASS-METHODS ls03n
      IMPORTING lgnum TYPE lgnum DEFAULT ''
                lgtyp TYPE lgtyp
                lgpla TYPE lgpla.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_OT definition
class ZCL_AP_OT implementation.
  METHOD anular_ot.
    DATA l_ltak TYPE ltak.
    DATA o_bi   TYPE REF TO zcl_ap_batch_input.

    SELECT SINGLE kquit FROM ltak
      INTO l_ltak-kquit
     WHERE lgnum = lgnum
       AND tanum = tanum.
    IF sy-subrc <> 0.
      message = 'OT no existe'(one).
    ELSE.
      IF l_ltak-kquit = 'X'.
        message = 'OT ya estaba borrada'(oyb).
        ok = 'X'.
      ELSE.
        o_bi = NEW #( ).

        o_bi->inicio( ).

* Anular OT: pantalla inicial
        o_bi->dynpro( program = 'SAPML03T' dynpro = '0118' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
        o_bi->campos( campo = 'LTAK-TANUM' valor = tanum ). " Número de orden de transporte
        o_bi->campos( campo = 'RL03T-TAPOS' valor = '' ). " Posición de orden de transporte
        o_bi->campos( campo = 'LTAK-LGNUM' valor = lgnum ). " Núm.almacén/Complejo alm.
        o_bi->campos( campo = 'RL03T-RHELL' valor = '' ). " Proceso visible de la transacción
        o_bi->campos( campo = 'RL03T-RDNKL' valor = 'X' ). " Proceso no visible de la transacción

        message = o_bi->llamar_transaccion( tcode = 'LT15' modo = 'N' ).

        SELECT SINGLE kquit FROM ltak
          INTO l_ltak-kquit
         WHERE lgnum = lgnum
           AND tanum = tanum.

        IF l_ltak-kquit = 'X'.
          ok = 'X'.
          CLEAR message.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD anular_pos_ot.
    DATA: l_ltap_cancel TYPE ltap_cancl,
          i_ltap_cancel TYPE TABLE OF ltap_cancl.

    CLEAR l_ltap_cancel.
    l_ltap_cancel-tanum = tanum.
    l_ltap_cancel-tapos = tapos.
    APPEND l_ltap_cancel TO i_ltap_cancel.

    CALL FUNCTION 'L_TO_CANCEL'
      EXPORTING
        i_lgnum                      = lgnum
        i_tanum                      = tanum
*     I_SOLEX                      = 0
        i_cancl                      = 'X'
*     I_SUBST                      = ' '
*     I_QNAME                      = SY-UNAME
*     I_UPDATE_TASK                = ' '
        i_commit_work                = commit
      TABLES
        t_ltap_cancl                 = i_ltap_cancel
      EXCEPTIONS
        to_confirmed                 = 1
        to_doesnt_exist              = 2
        item_confirmed               = 3
        item_doesnt_exist            = 4
        foreign_lock                 = 5
        double_lines                 = 6
        nothing_to_do                = 7
        xfeld_wrong                  = 8
        su_movement_partly_confirmed = 9
        update_without_commit        = 10
        no_authority                 = 11
        error_message                = 999 " Cualquier otra excepción!
        OTHERS                       = 12.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
              sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ENDIF.
  ENDMETHOD.
  METHOD borra_pos_ot.
    DATA l_ltap TYPE ltap.
    DATA o_bi   TYPE REF TO zcl_ap_batch_input.

*  CALL FUNCTION 'L_TO_DELETE_ITEM_INT'
*    EXPORTING
*      i_lgnum = lgnum
*      i_tanum = tanum
*      i_tapos = tapos.

    SELECT SINGLE pquit vdifm vsola FROM ltap
      INTO CORRESPONDING FIELDS OF l_ltap
     WHERE lgnum = lgnum
       AND tanum = tanum
       AND tapos = tapos.
    IF sy-subrc <> 0.
      message = 'OT no existe'(one).
    ELSE.
      l_ltap-vdifm = abs( l_ltap-vdifm ).
      IF l_ltap-pquit = 'X' AND l_ltap-vdifm = l_ltap-vsola.
        message = 'OT ya estaba borrada'(oyb).
        ok = 'X'.
      ELSE.
        o_bi = NEW #( ).

        o_bi->inicio( ).

* Anular OT: pantalla inicial
        o_bi->dynpro( program = 'SAPML03T' dynpro = '0118' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
        o_bi->campos( campo = 'LTAK-TANUM' valor = tanum ). " Número de orden de transporte
        o_bi->campos( campo = 'RL03T-TAPOS' valor = tapos ). " Posición de orden de transporte
        o_bi->campos( campo = 'LTAK-LGNUM' valor = lgnum ). " Núm.almacén/Complejo alm.
        o_bi->campos( campo = 'RL03T-RHELL' valor = '' ). " Proceso visible de la transacción
        o_bi->campos( campo = 'RL03T-RDNKL' valor = 'X' ). " Proceso no visible de la transacción

        message = o_bi->llamar_transaccion( tcode = 'LT15' modo = 'N' ).

        SELECT SINGLE pquit vdifm vsola FROM ltap
          INTO CORRESPONDING FIELDS OF l_ltap
         WHERE lgnum = lgnum
           AND tanum = tanum
           AND tapos = tapos.

        l_ltap-vdifm = abs( l_ltap-vdifm ).
        IF l_ltap-pquit = 'X' AND l_ltap-vdifm = l_ltap-vsola.
          ok = 'X'.
          CLEAR message.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD buscar_ots.
    FIELD-SYMBOLS <ltap> TYPE ltap.

    DATA l_tanum TYPE tanum.

    DEFINE declara_rango.
      DATA: r_&1  TYPE RANGE OF &2-&1,
            lr_&1 LIKE LINE OF r_&1.
    END-OF-DEFINITION.

    DEFINE rango_ltap.
      declara_rango &1 lTAP.
      IF NOT &1 IS INITIAL.
        __rangoc_eq r_&1 &1.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE rango_ltak.
      declara_rango &1 ltak.
      IF NOT &1 IS INITIAL.
        __rangoc_eq r_&1 &1.
      ENDIF.
    END-OF-DEFINITION.

    rango_ltap: nltyp, nlpla, matnr.
    rango_ltak lznum.

    declara_rango: pquit ltap,
                   vorga ltap.

    IF anulada = 'X'.
      __rangoc_eq: r_pquit 'X',
                   r_vorga 'ST'.
    ELSE.
      IF anulada = ''.
        __rangoc_ne r_vorga 'ST'.
      ENDIF.

      IF confirmada = 'X'.
        __rangoc_eq r_pquit 'X'.
        __rangoc_ne r_vorga 'ST'.
      ELSEIF confirmada = ''.
        __rangoc_eq r_pquit ''.
      ENDIF.
    ENDIF.

    SELECT * FROM ltap JOIN ltak ON  ltak~lgnum = ltap~lgnum
                                 AND ltak~tanum = ltap~tanum
      INTO CORRESPONDING FIELDS OF TABLE i_ltap
     WHERE ltak~lgnum  = lgnum
       AND lznum      IN r_lznum
       AND matnr      IN r_matnr
       AND nltyp      IN r_nltyp
       AND nlpla      IN r_nlpla
       AND pquit      IN r_pquit
       AND vorga      IN r_vorga.

    IF get_lista = 'X'.
      LOOP AT i_ltap ASSIGNING <ltap>.
        __add_lista_no0 lista_ots <ltap>-tanum.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD confirmar_ot.
    DATA: l_ltap_conf TYPE ltap_conf,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_ltap      TYPE ltap.

    CLEAR message.

    LOOP AT i_ltap_conf INTO l_ltap_conf.
      SELECT SINGLE tanum FROM ltap
        INTO l_ltap-tanum
       WHERE lgnum = lgnum
         AND tanum = l_ltap_conf-tanum
         AND tapos = l_ltap_conf-tapos.
      IF sy-subrc <> 0.
        CONCATENATE 'No existe la posición'(nep) l_ltap_conf-tapos 'de la OT'(dot) l_ltap_conf-tanum
                    INTO message SEPARATED BY space.
      ENDIF.
    ENDLOOP.

*QUKNZ
*           Confirmar toma y transporte de material
*1  Sólo confirmar toma
*2  Sólo confirmar transporte de material
*3  Sólo información
    IF message IS INITIAL.
      CALL FUNCTION 'L_TO_CONFIRM'
        EXPORTING
          i_lgnum                        = lgnum
          i_tanum                        = tanum
          i_squit                        = squit  " Posición de orden de transporte completa
          i_quknz                        = quknz  " Confirmación separada
*       I_SUBST                        = ' '
*       I_QNAME                        = SY-UNAME
*       I_ENAME                        = SY-UNAME
*       I_SOLEX                        = 0
*       I_PERNR                        = 0
*       I_STDAT                        = INIT_DATUM
*       I_STUZT                        = 0
*       I_ENDAT                        = INIT_DATUM
*       I_ENUZT                        = 0
*       I_ISTWM                        = 0
*       I_KOMIM                        = ' '
*       I_EINLM                        = ' '
*       I_TBELI                        = ' '
          i_update_task                  = in_update_task
          i_commit_work                  = commit_work
          i_ausfb                        = ausfb
        TABLES
          t_ltap_conf                    = i_ltap_conf
*       T_LTAP_CONF_HU                 = T_LTAP_CONF_HU
*       T_LTAP_CONF_HU_SERIAL          = T_LTAP_CONF_HU_SERIAL
        EXCEPTIONS
          to_confirmed                   = 1
          to_doesnt_exist                = 2
          item_confirmed                 = 3
          item_subsystem                 = 4
          item_doesnt_exist              = 5
          item_without_zero_stock_check  = 6
          item_with_zero_stock_check     = 7
          one_item_with_zero_stock_check = 8
          item_su_bulk_storage           = 9
          item_no_su_bulk_storage        = 10
          one_item_su_bulk_storage       = 11
          foreign_lock                   = 12
          squit_or_quantities            = 13
          vquit_or_quantities            = 14
          bquit_or_quantities            = 15
          quantity_wrong                 = 16
          double_lines                   = 17
          kzdif_wrong                    = 18
          no_difference                  = 19
          no_negative_quantities         = 20
          wrong_zero_stock_check         = 21
          su_not_found                   = 22
          no_stock_on_su                 = 23
          su_wrong                       = 24
          too_many_su                    = 25
          nothing_to_do                  = 26
          no_unit_of_measure             = 27
          xfeld_wrong                    = 28
          update_without_commit          = 29
          no_authority                   = 30
          lqnum_missing                  = 31
          charg_missing                  = 32
          no_sobkz                       = 33
          no_charg                       = 34
          nlpla_wrong                    = 35
          two_step_confirmation_required = 36
          two_step_conf_not_allowed      = 37
          pick_confirmation_missing      = 38
          quknz_wrong                    = 39
          hu_data_wrong                  = 40
          no_hu_data_required            = 41
          hu_data_missing                = 42
          hu_not_found                   = 43
          picking_of_hu_not_possible     = 44
          not_enough_stock_in_hu         = 45
          serial_number_data_wrong       = 46
          serial_numbers_not_required    = 47
          no_differences_allowed         = 48
          serial_number_not_available    = 49
          serial_number_data_missing     = 50
          to_item_split_not_allowed      = 51
          input_wrong                    = 52
          error_message                  = 53.       " Cualquier otra excepción!

      IF sy-subrc = 3 OR sy-subrc = 0. " Item confirmed
        confirmacion_ok = 'X'.
      ELSE.
        IF sy-subrc <> 0.
          IF dequeue_all = 'X'.
            CALL FUNCTION 'DEQUEUE_ALL'.
          ENDIF.
        ENDIF.

        IF NOT sy-msgty IS INITIAL.
          IF mostrar_mensaje = 'X' OR sy-msgty <> 'I' OR sy-msgty = 'S'.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
                    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
          ENDIF.
        ELSE.
          message = '¡Se ha producido un error inesperado!'(sei).
        ENDIF.
      ENDIF.
    ELSE.
      sy-msgty = 'E'.
      sy-msgid = '00'.
      sy-msgno = '398'.
      sy-msgv1 = message.
    ENDIF.
  ENDMETHOD.
  METHOD confirmar_pos_ot.
    " TODO: parameter MOSTRAR_MENSAJE is never used (ABAP cleaner)
    " TODO: parameter IN_UPDATE_TASK is never used (ABAP cleaner)
    " TODO: parameter AUSFB is never used (ABAP cleaner)

    DATA: i_ltap_conf TYPE TABLE OF ltap_conf,
          l_ltap      TYPE ltap.

    FIELD-SYMBOLS <ltap_conf> TYPE ltap_conf.

    CLEAR confirmacion_ok.

    SELECT * FROM ltap
      INTO CORRESPONDING FIELDS OF TABLE i_ltap_conf
     WHERE lgnum = lgnum
       AND tanum = tanum
       AND tapos = tapos.
    IF sy-subrc <> 0.
      CLEAR confirmacion_ok.
      CONCATENATE 'No existe la OT'(neo) tanum tapos INTO message SEPARATED BY space.
    ELSE.
      IF quknz = '2'.
        SELECT SINGLE pquit FROM ltap
          INTO l_ltap-pquit
         WHERE lgnum = lgnum
           AND tanum = tanum
           AND tapos = tapos.
        IF l_ltap-pquit = 'X'.
* Entrega ya confirmada
          confirmacion_ok = 'X'.
        ENDIF.
      ENDIF.

      IF confirmacion_ok IS INITIAL.

        LOOP AT i_ltap_conf ASSIGNING <ltap_conf>.
          CLEAR <ltap_conf>-nlpla.
          <ltap_conf>-nista = ctd_conf.
          <ltap_conf>-ndifa = ctd_dif.
          <ltap_conf>-kzdif = kzdif.
          <ltap_conf>-nquit = nquit.
          <ltap_conf>-squit = squit.
          IF NOT nlpla IS INITIAL.
            <ltap_conf>-nlpla = nlpla.
          ENDIF.
        ENDLOOP.

        confirmar_ot( EXPORTING lgnum           = lgnum
                                tanum           = tanum
                                quknz           = quknz
                                squit           = ''  " Confirmar posición única
                                commit_work     = commit_work
                                dequeue_all     = dequeue_all
                      CHANGING  i_ltap_conf     = i_ltap_conf
                                message         = message
                                confirmacion_ok = confirmacion_ok ).

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD convert_ctd_wm.
    DATA: l_unidad_origen TYPE meins,
          l_lhmg1         TYPE mlgn-lhmg1,
          l_ctd_origen    TYPE mengv13.

    l_unidad_origen = unidad_origen.
    SELECT SINGLE lhme1 lhmg1 FROM  mlgn
      INTO (unidad_wm, l_lhmg1)
     WHERE matnr = matnr
       AND lgnum = lgnum.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    l_ctd_origen = cantidad_origen.
    IF l_unidad_origen = '*'.
      l_unidad_origen = zcl_ap_material=>get_unidad_base( matnr ).
      l_ctd_origen = l_lhmg1.
    ENDIF.

    IF l_ctd_origen = 0.
      RETURN.
    ENDIF.

    IF l_unidad_origen IS INITIAL.
      l_unidad_origen = zcl_ap_material=>get_unidad_base( matnr ).
    ENDIF.

    IF l_unidad_origen = unidad_wm.
      RETURN.
    ENDIF.

    cantidad_wm = zcl_ap_material=>convertir_unidad( matnr          = matnr
                                                     cantidad       = l_ctd_origen
                                                     unidad_origen  = l_unidad_origen
                                                     unidad_destino = unidad_wm ).
  ENDMETHOD.
  METHOD crear_ot_desde_nt.
    DATA: l_pos   TYPE l03b_trite,
          l_lagp  TYPE lagp,
          l_fehlt TYPE t333-fehlt,
          i_mens  TYPE TABLE OF wmgrp_msg,
          l_subrc TYPE sy-subrc.
    DATA l_mensaje TYPE bapireturn1-message.
    DATA o_bi      TYPE REF TO zcl_ap_batch_input.

    CLEAR message.

    LOOP AT t_pos INTO l_pos.
      SELECT SINGLE skzsa skzse skzsi skzua skzue FROM lagp
        INTO CORRESPONDING FIELDS OF l_lagp
        WHERE lgnum = lgnum
          AND lgtyp = l_pos-vltyp
          AND lgpla = l_pos-vlpla.
      IF sy-subrc = 0.
        IF l_lagp-skzua = 'X' OR l_lagp-skzue = 'X'.
          CONCATENATE 'Ubicación'(ubi) l_pos-vltyp l_pos-vlpla
                      'bloqueada'(blq)
                      INTO message SEPARATED BY space.
          EXIT.
        ENDIF.
      ELSE.
        CONCATENATE 'Ubicación'(ubi) l_pos-vltyp l_pos-vlpla 'no existe'(noe)
                    INTO message SEPARATED BY space.
        EXIT.
      ENDIF.

      SELECT SINGLE skzsa skzse skzsi skzua skzue FROM lagp
        INTO CORRESPONDING FIELDS OF l_lagp
        WHERE lgnum = lgnum
          AND lgtyp = l_pos-nltyp
          AND lgpla = l_pos-nlpla.
      IF sy-subrc = 0.
        IF    l_lagp-skzua = 'X' OR l_lagp-skzue = 'X'
           OR l_lagp-skzsa = 'X' OR l_lagp-skzse = 'X'
           OR l_lagp-skzsi = 'X'.
          CONCATENATE 'Ubicación'(ubi) l_pos-nltyp l_pos-nlpla
                      'bloqueada'(blq)
                      INTO message SEPARATED BY space.
          EXIT.
        ENDIF.
      ELSE.
        CONCATENATE 'Ubicación'(ubi) l_pos-nltyp l_pos-nlpla 'no existe'(noe)
                    INTO message SEPARATED BY space.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE fehlt FROM t333
      INTO l_fehlt
     WHERE lgnum = lgnum
       AND bwlvs = '101'.
    IF l_fehlt = 'X'.
      UPDATE t333
         SET fehlt = ''
       WHERE lgnum = lgnum
         AND bwlvs = '101'.
    ENDIF.

    DESCRIBE TABLE t_pos LINES sy-tfill.
    IF usar_bi IS INITIAL OR sy-tfill > 1.
      CALL FUNCTION 'L_TO_CREATE_TR'
        EXPORTING
          i_lgnum                        = lgnum
          i_tbnum                        = tbnum
          it_trite                       = t_pos
          i_squit                        = squit
        IMPORTING
          e_tanum                        = tanum
        TABLES
          t_wmgrp_msg                    = i_mens
        EXCEPTIONS
          foreign_lock                   = 1
          qm_relevant                    = 2
          tr_completed                   = 3
          xfeld_wrong                    = 4
          ldest_wrong                    = 5
          drukz_wrong                    = 6
          tr_wrong                       = 7
          squit_forbidden                = 8
          no_to_created                  = 9
          update_without_commit          = 10
          no_authority                   = 11
          preallocated_stock             = 12
          partial_transfer_req_forbidden = 13
          input_error                    = 14
          error_message                  = 999 " Cualquier otra excepción!
          OTHERS                         = 15.
      l_subrc = sy-subrc.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH
              sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mensaje.
    ELSE.

      READ TABLE t_pos INTO l_pos INDEX 1.
      o_bi = NEW #( ).

      o_bi->inicio( ).

* Crear OT a partir de NT: pantalla inicial
      o_bi->dynpro( program = 'SAPML03T' dynpro = '0131' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
      o_bi->campos( campo = 'LTAK-LGNUM' valor = lgnum ). " Núm.almacén/Complejo alm.
      o_bi->campos( campo = 'LTBK-TBNUM' valor = tbnum ). " Número de necesidad de transporte
      o_bi->campos( campo = '*LTBP-TBPOS' valor = l_pos-tbpos ).
      o_bi->campos( campo = 'RL03T-ALAKT' valor = 'X' ). " Marcar todas las posiciones

* Crear orden transp.: pantalla preparatoria para entrada mat.
      o_bi->dynpro( program = 'SAPML03T' dynpro = '0104' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).
      o_bi->campos( campo = 'RL03T-ANZL2' valor = '' ). " Número de unidades de almacén a entrar
      o_bi->campos( campo = 'LTAPE-ANFME(01)' valor = l_pos-anfme ). " Ctd teórica 'hacia' en unidad de medida alternativa
      o_bi->campos( campo = 'LTAPE-NLTYP(01)' valor = l_pos-nltyp ). " Tipo almacén destino
      o_bi->campos( campo = 'LTAPE-NLPLA(01)' valor = l_pos-nlpla ). " Ubicación de destino

      IF usar_bi = 'X'.
        l_mensaje = o_bi->llamar_transaccion( tcode = 'LT04' modo = 'N' ).
      ELSE.
        l_mensaje = o_bi->llamar_transaccion( tcode = 'LT04' modo = usar_bi ).
      ENDIF.

      IF o_bi->msgty = 'S' AND o_bi->msgid = 'L3' AND o_bi->msgno = '016'.
        tanum = o_bi->msgv1.
        zcl_ap_string=>poner_ceros_c( CHANGING cadena = tanum ).
      ENDIF.
    ENDIF.

    IF tanum IS INITIAL.
      IF l_mensaje IS INITIAL.
        CASE L_subrc.
          WHEN 1.
            message = 'Datos bloqueados'(dbl).
          WHEN 3.
            message = 'La OT ya estaba creada'(oyc).
            READ TABLE t_pos INTO l_pos INDEX 1.
            SELECT SINGLE tanum FROM ltbp
              INTO tanum
             WHERE lgnum = lgnum
               AND tbnum = tbnum
               AND tbpos = l_pos-tbpos.
          WHEN 12.
            message = 'Stock preadjudicado'(stp).
          WHEN 14.
            message = 'Error en parámetros llamada función'(epf).
          WHEN OTHERS.
            IF message IS INITIAL.
              message(4) = sy-subrc.
              CONCATENATE 'Error'(err) message 'al intentar crear OT'(ico)
                          INTO message SEPARATED BY space.
            ENDIF.
        ENDCASE.
      ELSE.
        message = l_mensaje.
      ENDIF.
    ELSE.
      CONCATENATE 'Se ha creado OT'(sco) tanum INTO message
                  SEPARATED BY space.
    ENDIF.

    IF l_fehlt = 'X'.
      UPDATE t333
         SET fehlt = 'X'
       WHERE lgnum = lgnum
         AND bwlvs = '101'.
    ENDIF.
  ENDMETHOD.
  METHOD crear_ot_multiple.
    DATA: l_ltap  TYPE ltap_creat,
          l_lagp  TYPE lagp,
          l_t333  TYPE t333,
          l_ltak  TYPE ltak_vb,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_tanum TYPE tanum.

    DATA o_bi      TYPE REF TO zcl_ap_batch_input.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mensaje TYPE bapireturn1-message.

    CLEAR: tanum, message.

    IF ltak IS SUPPLIED.
      APPEND ltak TO i_ltak.
    ENDIF.

    LOOP AT i_ltap_creat INTO l_ltap.
      SELECT SINGLE skzse skzsi skzua skzue FROM lagp
        INTO CORRESPONDING FIELDS OF l_lagp
        WHERE lgnum = lgnum
          AND lgtyp = l_ltap-vltyp
          AND lgpla = l_ltap-vlpla.
      IF sy-subrc = 0.
        IF l_lagp-skzua = 'X' OR l_lagp-skzsi = 'X'.
          CONCATENATE 'Ubicación'(ubi) l_ltap-vltyp l_ltap-vlpla
                      'bloqueada para salidas'(bps)
                      INTO message SEPARATED BY space.
          EXIT.
        ENDIF.
      ELSE.
        CONCATENATE 'Ubicación'(ubi) l_ltap-vltyp l_ltap-vlpla 'no existe'(noe)
                    INTO message SEPARATED BY space.
        EXIT.
      ENDIF.

      IF l_ltap-nlpla = ''.
        CONTINUE.
      ENDIF.

      SELECT SINGLE skzse skzsi skzue FROM lagp
        INTO CORRESPONDING FIELDS OF l_lagp
        WHERE lgnum = lgnum
          AND lgtyp = l_ltap-nltyp
          AND lgpla = l_ltap-nlpla.
      IF sy-subrc = 0.
        IF l_lagp-skzue = 'X' OR l_lagp-skzse = 'X' OR l_lagp-skzsi = 'X'.
          CONCATENATE 'Ubicación'(ubi) l_ltap-nltyp l_ltap-nlpla
                      'bloqueada para entradas'(bpe)
                      INTO message SEPARATED BY space.
          EXIT.
        ENDIF.
      ELSE.

*--------------------------------------------------------------------
* En el caso de que la clase de movimiento tenga ubicación de destino
* DINÁMICA, no se debe enviar este mensaje.
        CLEAR l_t333.
        SELECT SINGLE * FROM t333
          INTO l_t333
         WHERE lgnum = lgnum
           AND bwlvs = bwlvs.
        IF l_t333-nkdyn IS INITIAL.
          CONCATENATE 'Ubicación'(ubi) l_ltap-nltyp l_ltap-nlpla 'no existe'(noe)
                      INTO message SEPARATED BY space.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF message IS INITIAL.

      IF solo_verificaciones IS INITIAL.
        DESCRIBE TABLE i_ltap_creat LINES sy-tfill.
        IF usar_bi IS INITIAL OR sy-tfill > 1.
          CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
            EXPORTING
              i_lgnum                = lgnum
              i_bwlvs                = bwlvs
              i_betyp                = betyp
              i_benum                = benum
              i_lznum                = lznum
              i_update_task          = in_update_task
              i_commit_work          = commit_work
              i_ausfb                = ausfb
            IMPORTING
              e_tanum                = tanum
            TABLES
              t_ltap_creat           = i_ltap_creat
              t_ltak                 = i_ltak
            EXCEPTIONS
              no_to_created          = 1
              bwlvs_wrong            = 2
              betyp_wrong            = 3
              benum_missing          = 4
              betyp_missing          = 5
              foreign_lock           = 6
              vltyp_wrong            = 7
              vlpla_wrong            = 8
              vltyp_missing          = 9
              nltyp_wrong            = 10
              nlpla_wrong            = 11
              nltyp_missing          = 12
              rltyp_wrong            = 13
              rlpla_wrong            = 14
              rltyp_missing          = 15
              squit_forbidden        = 16
              manual_to_forbidden    = 17
              letyp_wrong            = 18
              vlpla_missing          = 19
              nlpla_missing          = 20
              sobkz_wrong            = 21
              sobkz_missing          = 22
              sonum_missing          = 23
              bestq_wrong            = 24
              lgber_wrong            = 25
              xfeld_wrong            = 26
              date_wrong             = 27
              drukz_wrong            = 28
              ldest_wrong            = 29
              update_without_commit  = 30
              no_authority           = 31
              material_not_found     = 32
              lenum_wrong            = 33
              matnr_missing          = 34
              werks_missing          = 35
              anfme_missing          = 36
              altme_missing          = 37
              lgort_wrong_or_missing = 38
              error_message          = 999 " Cualquier otra excepción!
              OTHERS                 = 39.
          IF sy-subrc <> 0.
            DATA(l_sy) = sy.
          ENDIF.
        ELSE.

          READ TABLE i_ltap_creat INTO l_ltap INDEX 1.

          o_bi = NEW #( ).

          o_bi->inicio( ).

* Anlegen Transportauftrag: Anforderdynpro
          o_bi->dynpro( program = 'SAPML03T' dynpro = '0101' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
          o_bi->campos( campo = 'LTAK-LGNUM' valor = lgnum ). " Núm.almacén/Complejo alm.
          o_bi->campos( campo = 'LTAK-BENUM' valor = benum ). " Número de necesidad
          o_bi->campos( campo = 'LTAK-BETYP' valor = betyp ). " Tipo de necesidad
          o_bi->campos( campo = 'LTAK-BWLVS' valor = bwlvs ). " Cl.movim.gestión almacenes
          o_bi->campos( campo = 'LTAP-MATNR' valor = l_ltap-matnr ). " Número de material
          o_bi->campos( campo = 'RL03T-ANFME' valor = l_ltap-anfme ). " Cantidad solicitada en unidad medida alternativa
          o_bi->campos( campo = 'LTAP-ALTME' valor = l_ltap-altme ). " Unidad de medida alternativa
          o_bi->campos( campo = 'LTAP-BESTQ' valor = l_ltap-bestq ). " Diferenciación de stock en sistema de gestión de almacenes
          o_bi->campos( campo = 'LTAP-WERKS' valor = l_ltap-werks ). " Centro
          o_bi->campos( campo = 'LTAP-LGORT' valor = l_ltap-lgort ). " Almacén
          o_bi->campos( campo = 'LTAP-SOBKZ' valor = l_ltap-sobkz ). " Indicador de stock especial
          o_bi->campos( campo = 'RL03T-LSONR' valor = l_ltap-sonum ). " Número de stock especial

* Crear orden transp.: pantalla preparatoria p. salida stock
          IF bwlvs = '999'.
* Crear orden transporte: pantalla de posición individual
            o_bi->dynpro( program = 'SAPML03T' dynpro = '0102' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).

            o_bi->campos( campo = 'LTAP-VLTYP' valor = l_ltap-vltyp ). " Tipo alm.procedencia
            o_bi->campos( campo = 'LTAP-VLPLA' valor = l_ltap-vlpla ). " Ubic.proced.
            o_bi->campos( campo = 'LTAP-VLQNR' valor = l_ltap-vlqnr ). " Cuanto
            o_bi->campos( campo = 'LTAP-NLTYP' valor = l_ltap-nltyp ). " Tipo alm.procedencia
            o_bi->campos( campo = 'LTAP-NLPLA' valor = l_ltap-nlpla ). " Ubicación de destino
            o_bi->campos( campo = 'LTAP-NLQNR' valor = l_ltap-nlqnr ). " Cuanto
          ELSE.

            o_bi->dynpro( program = 'SAPML03T' dynpro = '0105' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=TAH2' ).
            o_bi->campos( campo = 'LTAPA-ANFME(01)' valor = l_ltap-anfme ). " Cantidad teórica 'desde' en unidad-medida alternativa
            o_bi->campos( campo = 'LTAPA-VLTYP(01)' valor = l_ltap-vltyp ). " Tipo alm.procedencia
            o_bi->campos( campo = 'LTAPA-VLBER(01)' valor = '' ). " Área de almacén de procedencia
            o_bi->campos( campo = 'LTAPA-VLPLA(01)' valor = l_ltap-vlpla ). " Ubic.proced.

* Crear orden transporte: pantalla de posición individual
            o_bi->dynpro( program = 'SAPML03T' dynpro = '0102' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).

            o_bi->campos( campo = 'LTAP-VLQNR' valor = l_ltap-vlqnr ). " Cuanto
            o_bi->campos( campo = 'LTAP-NLPLA' valor = l_ltap-nlpla ). " Ubicación de destino
            o_bi->campos( campo = 'LTAP-NLQNR' valor = l_ltap-nlqnr ). " Cuanto

* Crear orden transp.: pantalla preparatoria p. salida stock
            o_bi->dynpro( program = 'SAPML03T' dynpro = '0105' ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).
          ENDIF.

          IF usar_bi = 'X'.
            l_mensaje = o_bi->llamar_transaccion( tcode = 'LT01' modo = 'N' ).
          ELSE.
            l_mensaje = o_bi->llamar_transaccion( tcode = 'LT01' modo = usar_bi ).
          ENDIF.

          IF o_bi->msgty = 'S' AND o_bi->msgid = 'L3' AND o_bi->msgno = '016'.
            tanum = o_bi->msgv1.
            zcl_ap_string=>poner_ceros_c( CHANGING cadena = tanum ).
          ENDIF.

        ENDIF.

* Caso de tener más de una OT, no informa TANUM
        IF tanum IS INITIAL AND NOT i_ltak IS INITIAL.
          READ TABLE i_ltak INTO l_ltak INDEX 1.
          IF sy-subrc = 0.
            tanum = l_ltak-tanum.
          ENDIF.
        ENDIF.

        IF NOT tanum IS INITIAL.
          IF espera_a_grabado = 'X' AND commit_work = 'X'.
            DO 5 TIMES.
              SELECT SINGLE tanum FROM ltak
                INTO l_tanum
               WHERE lgnum = lgnum
                 AND tanum = tanum.
              IF sy-subrc = 0.
                EXIT.
              ELSE.
                WAIT UP TO 1 SECONDS.
              ENDIF.
            ENDDO.
          ENDIF.
        ELSE.
          IF dequeue_all = 'X'.
            CALL FUNCTION 'DEQUEUE_ALL'.
          ENDIF.
        ENDIF.

        IF NOT l_sy-msgty IS INITIAL.
          IF mostrar_mensaje = 'X' OR l_sy-msgty <> 'I' OR l_sy-msgty = 'S'.
            MESSAGE ID l_sy-msgid TYPE l_sy-msgty NUMBER l_sy-msgno WITH
                    l_sy-msgv1 l_sy-msgv2 l_sy-msgv3 l_sy-msgv4 INTO message.
          ENDIF.
        ELSE.
          message = '¡Se ha producido un error inesperado!'(sei).
        ENDIF.

        IF tanum IS INITIAL AND message IS INITIAL.
          message = 'Error al crear OT'(eco).
        ENDIF.
      ENDIF.
    ELSE.
      sy-msgty = 'E'.
      sy-msgid = '00'.
      sy-msgno = '398'.
      sy-msgv1 = message.
    ENDIF.
  ENDMETHOD.
  METHOD crear_ot_simple.
    DATA: l_ltap TYPE ltap_creat,
          i_ltap TYPE TABLE OF ltap_creat.

    l_ltap-matnr = matnr.
    l_ltap-anfme = anfme.
    IF altme IS INITIAL.
      l_ltap-altme = zcl_ap_material=>get_unidad_base( matnr ).
    ELSE.
      l_ltap-altme = altme.
    ENDIF.
    l_ltap-bestq = bestq.
    l_ltap-werks = werks.
    l_ltap-lgort = lgort.
    l_ltap-sobkz = sobkz.
    l_ltap-sonum = sonum.
    l_ltap-vltyp = vltyp.
    l_ltap-vlpla = vlpla.
    l_ltap-nltyp = nltyp.
    l_ltap-nlpla = nlpla.
    l_ltap-vlqnr = vlqnr.
    l_ltap-nlqnr = nlqnr.
    APPEND l_ltap TO i_ltap.

    crear_ot_multiple(
      EXPORTING
        lgnum               = lgnum
        bwlvs               = bwlvs
        ltak                = ltak
        betyp               = betyp
        benum               = benum
        lznum               = lznum
        mostrar_mensaje     = mostrar_mensaje
        in_update_task      = in_update_task
        espera_a_grabado    = espera_a_grabado
        ausfb               = ausfb
        commit_work         = commit_work
        dequeue_all         = dequeue_all
        solo_verificaciones = solo_verificaciones
        usar_bi             = usar_bi
      CHANGING
        i_ltap_creat        = i_ltap
        i_ltak              = i_ltak
        tanum               = tanum
        message             = message ).
  ENDMETHOD.
  METHOD crear_ubicacion.
    DATA: l_lagp TYPE lagp,
          " TODO: variable is assigned but never used (ABAP cleaner)
          i_lagp TYPE TABLE OF lagp.

    CLEAR mensaje.
    SELECT SINGLE lgnum FROM lagp
      INTO l_lagp-lgnum
     WHERE lgnum = lgnum
       AND lgtyp = lgtyp
       AND lgpla = lgpla.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    CLEAR l_lagp.
    l_lagp-lgnum = lgnum.
    l_lagp-lgtyp = lgtyp.
    l_lagp-lgpla = lgpla.
    l_lagp-lgber = lgber.
    l_lagp-lptyp = lptyp.
    APPEND l_lagp TO i_lagp.

    TRY.
        CALL FUNCTION 'L_LAGP_HINZUFUEGEN'
          EXPORTING
            xlagp = l_lagp.

      CATCH cx_sy_dyn_call_illegal_type.
        CONCATENATE 'Error al intentar crear ubicación'(ecu) lgnum lgtyp lgpla INTO mensaje SEPARATED BY space.
    ENDTRY.

    SELECT SINGLE lgpla FROM lagp
      INTO l_lagp-lgpla
     WHERE lgnum = lgnum
       AND lgtyp = lgtyp
       AND lgpla = lgpla.
    IF sy-subrc <> 0.
      CONCATENATE 'No se ha podido crear ubicación'(ncu) lgnum lgtyp lgpla INTO mensaje SEPARATED BY space.
    ENDIF.
  ENDMETHOD.
  METHOD esta_bloqueada.
    bloqueado = zcl_ap_utils=>comprobar_bloqueo( tabla = 'LTAK'
                                                 clave = sy-mandt
                                                 clave2 = lgnum
                                                 clave3 = tanum ).
  ENDMETHOD.
  METHOD get_ctd_ubicacion.
    __data_set_vart lqua.
    CLEAR cantidad.
    SELECT ausme einme matnr meins verme FROM lqua
      INTO CORRESPONDING FIELDS OF TABLE i_lqua
     WHERE lgnum = lgnum
       AND lgtyp = lgtyp
       AND lgpla = lgpla.

    LOOP AT i_lqua ASSIGNING <lqua>.
      IF disponible = 'X' AND <lqua>-verme <> 0.
        IF <lqua>-meins <> unidad_salida.
          <lqua>-verme = zcl_ap_material=>convertir_unidad( matnr = <lqua>-matnr unidad_origen = <lqua>-meins unidad_destino = unidad_salida cantidad = <lqua>-verme ).
        ENDIF.
        cantidad = cantidad + <lqua>-verme.
      ENDIF.

      IF entradas = 'X' AND <lqua>-einme <> 0.
        IF <lqua>-meins <> unidad_salida.
          <lqua>-einme = zcl_ap_material=>convertir_unidad( matnr = <lqua>-matnr unidad_origen = <lqua>-meins unidad_destino = unidad_salida cantidad = <lqua>-einme ).
        ENDIF.
        cantidad = cantidad + <lqua>-einme.
      ENDIF.

      IF salidas = 'X' AND <lqua>-ausme <> 0.
        IF <lqua>-meins <> unidad_salida.
          <lqua>-ausme = zcl_ap_material=>convertir_unidad( matnr = <lqua>-matnr unidad_origen = <lqua>-meins unidad_destino = unidad_salida cantidad = <lqua>-ausme ).
        ENDIF.
        cantidad = cantidad + <lqua>-ausme.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
  METHOD get_datos_usuario.
    DATA lt_xuser TYPE TABLE OF lrf_wkqu.

    CLEAR datos.

    CALL FUNCTION 'L_USER_DATA_GET'
      EXPORTING
        i_uname        = uname
      TABLES
        t_xuser        = lt_xuser
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.

    IF sy-subrc = 0.
      READ TABLE lt_xuser INTO datos WITH KEY statu = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD get_lgnum_def_usuario.
    DATA lrf_wkqu TYPE lrf_wkqu.

    lrf_wkqu = get_datos_usuario( uname ).
    lgnum = lrf_wkqu-lgnum.
  ENDMETHOD.
  METHOD get_lqua.
    DATA l_sonum TYPE lqua-sonum.

    FIELD-SYMBOLS <lqua> TYPE lqua.

    __def_rangoc: bestq, lqua_verme, lgtyp, lgpla, werks_d, matnr, sobkz, lvs_sonum.

    IF bestq <> '*'.
      __rangoc_eq r_bestq bestq.
    ENDIF.

    IF solo_disp = 'X'.
      CLEAR lr_lqua_verme.
      lr_lqua_verme-option = 'GT'.
      lr_lqua_verme-sign   = 'I'.
      APPEND lr_lqua_verme TO r_lqua_verme.
    ENDIF.

    IF NOT sonum IS INITIAL.
      l_sonum = sonum.
    ELSEIF NOT vbeln IS INITIAL.
      IF posnr IS INITIAL.
        CONCATENATE vbeln '000010' INTO l_sonum.
      ELSE.
        CONCATENATE vbeln posnr INTO l_sonum.
      ENDIF.
    ENDIF.
    IF NOT l_sonum IS INITIAL.
      __rangoc_eq r_lvs_sonum l_sonum.
    ENDIF.

    DEFINE rango.
      IF NOT &1 IS INITIAL.
        __rangoc_eq r_&1 &1.
      ENDIF.
    END-OF-DEFINITION.

    rango: matnr, lgtyp, lgpla, sobkz.
    IF NOT werks IS INITIAL.
      __rangoc_eq r_werks_d werks.
    ENDIF.

    SELECT * FROM lqua
      INTO TABLE i_lqua
     WHERE lgnum  = lgnum
       AND matnr IN r_matnr
       AND werks IN r_werks_d
       AND sobkz IN r_sobkz
       AND sonum IN r_lvs_sonum
       AND lgtyp IN r_lgtyp
       AND lgpla IN r_lgpla
       AND bestq IN r_bestq
       AND verme IN r_lqua_verme.

    IF NOT new_meins IS INITIAL.
      LOOP AT i_lqua ASSIGNING <lqua> WHERE verme <> 0 AND meins <> new_meins.
        <lqua>-verme = zcl_ap_material=>convertir_unidad( matnr = <lqua>-matnr cantidad = <lqua>-verme unidad_origen = <lqua>-meins unidad_destino = new_meins ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_rango_lgtyp_est.
    DATA: l_t334t  TYPE t334t,
          lr_lgtyp TYPE range_c3,
          l_tip    TYPE t334t-lgty0.

    SELECT SINGLE * FROM t334t
      INTO l_t334t
     WHERE lgnum = lgnum
       AND kzear = kzear
       AND lgtkz = lgtkz
       AND bestq = bestq
       AND sobkz = sobkz
       AND lagkl = lagkl
       AND bwref = bwref.
    IF sy-subrc <> 0.
      CLEAR lr_lgtyp.
      lr_lgtyp-option = 'EQ'.
      lr_lgtyp-sign   = 'I'.
      lr_lgtyp-low    = '?'.
      COLLECT lr_lgtyp INTO r_lgtyp.
    ELSE.
      DO 9 TIMES VARYING l_tip FROM l_t334t-lgty0 NEXT l_t334t-lgty1.
        IF NOT l_tip IS INITIAL.
          CLEAR lr_lgtyp.
          lr_lgtyp-option = 'EQ'.
          lr_lgtyp-sign   = 'I'.
          lr_lgtyp-low    = l_tip.
          COLLECT lr_lgtyp INTO r_lgtyp.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.
  METHOD get_stock_ubicacion.
    DATA l_sonum TYPE lqua-sonum.

    IF NOT sonum IS INITIAL.
      l_sonum = sonum.
    ELSEIF NOT vbeln IS INITIAL.
      CONCATENATE vbeln '000010' INTO l_sonum.
    ENDIF.

    IF NOT l_sonum IS INITIAL.
      SELECT SUM( verme ) FROM lqua
        INTO verme
       WHERE lgnum = lgnum
         AND matnr = matnr
         AND werks = werks
         AND sonum = l_sonum
         AND lgtyp = lgtyp
         AND lgpla = lgpla
         AND bestq = ''.
    ELSE.
      SELECT SUM( verme ) FROM lqua
        INTO verme
       WHERE lgnum = lgnum
         AND matnr = matnr
         AND werks = werks
         AND lgtyp = lgtyp
         AND lgpla = lgpla
         AND bestq = ''.
    ENDIF.
  ENDMETHOD.
  METHOD get_unidad_wm.
    SELECT SINGLE lhme1 FROM  mlgn
      INTO (lhme1)
     WHERE matnr = matnr
       AND lgnum = lgnum.
  ENDMETHOD.
  METHOD ls03n.
    DATA(l_lgnum) = lgnum.
    IF l_lgnum IS INITIAL.
      SELECT lgnum  "#EC CI_NOFIRST.
        FROM lagp
        WHERE lgtyp = @lgtyp
          AND lgpla = @lgpla
        ORDER BY PRIMARY KEY
        INTO @l_lgnum
        UP TO 1 ROWS.
      ENDSELECT.
    ENDIF.

    SET PARAMETER ID 'LGN' FIELD l_lgnum.
    SET PARAMETER ID 'LGT' FIELD lgtyp.
    SET PARAMETER ID 'LGP' FIELD lgpla.
    CALL TRANSACTION 'LS03N' AND SKIP FIRST SCREEN.
  ENDMETHOD.
  METHOD ubic_perm_stock_neg.
    SELECT SINGLE negat FROM t331
      INTO negat
     WHERE lgnum = lgnum
       AND lgtyp = lgtyp.
  ENDMETHOD.
  METHOD visualizar.
    TYPES: BEGIN OF t_ot,
             tanum TYPE ltak-tanum,
           END OF t_ot,
           BEGIN OF t_tanum,
             tanum TYPE tanum,
           END OF t_tanum.

    DATA: i_tanum TYPE TABLE OF t_tanum,
          l_tanum TYPE tanum,
          i_ot    TYPE TABLE OF t_ot,
          l_ot    TYPE t_ot,
          l_ok    TYPE c LENGTH 1,
          l_fila  TYPE i.

    i_tanum = zcl_ap_lista=>lista2tabla_n10( lista = tanum ).
    DESCRIBE TABLE i_tanum LINES sy-tfill.
    IF sy-tfill = 1.
      READ TABLE i_tanum INTO l_tanum INDEX 1.

      SET PARAMETER ID 'LGN' FIELD lgnum.
      SET PARAMETER ID 'TAN' FIELD l_tanum.
      SET PARAMETER ID 'TAP' FIELD tapos.
      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
    ELSEIF sy-tfill > 1.
      SELECT * FROM ltak
        INTO CORRESPONDING FIELDS OF TABLE i_ot
        FOR ALL ENTRIES IN i_tanum
       WHERE lgnum = lgnum
         AND tanum = i_tanum-tanum
       ORDER BY PRIMARY KEY.
      DESCRIBE TABLE i_ot LINES sy-tfill.
      IF sy-tfill = 1.
        READ TABLE i_ot INTO l_ot INDEX 1.
        SET PARAMETER ID 'LGN' FIELD lgnum.
        SET PARAMETER ID 'TAN' FIELD l_tanum.
        SET PARAMETER ID 'TAP' FIELD tapos.
        CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.     "#EC CI_CALLTA
      ELSEIF sy-tfill > 1.
        CALL FUNCTION 'Z_POPUP_ALV'
          EXPORTING
            titulo           = 'Seleccione OT a visualizar'(sov)
            ancho            = 40
            solo_aceptar     = ''
            seleccionar_fila = 'X'
          IMPORTING
            ok               = l_ok
            fila             = l_fila
          TABLES
            t_datos          = i_ot.

        IF l_ok = 'X'.
          READ TABLE i_ot INTO l_ot INDEX l_fila.
          IF sy-subrc = 0.
            SET PARAMETER ID 'LGN' FIELD lgnum.
            SET PARAMETER ID 'TAN' FIELD l_ot-tanum.
            SET PARAMETER ID 'TAP' FIELD tapos.
            CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar_stock_material.
    DATA o_bi      TYPE REF TO zcl_ap_batch_input.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mensaje TYPE bapireturn1-message.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Visualizar cuantos del material: pantalla inicial
    o_bi->dynpro( program = 'SAPML01S' dynpro = '0209' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'RL01S-LGNUM'
                  valor = lgnum ). " Núm.almacén/Complejo alm.
    o_bi->campos( campo = 'RL01S-MATNR'
                  valor = matnr ). " Número de material
    o_bi->campos( campo = 'RL01S-WERKS'
                  valor = werks ). " Centro
    o_bi->campos( campo = 'RL01S-BESTQ'
                  valor = '*' ). " Diferenciación de stock en sistema
    IF sonum IS INITIAL.
      o_bi->campos( campo = 'RL01S-SOBKZ'
                    valor = '*' ). " Indicador de stock especial
    ELSE.
      o_bi->campos( campo = 'RL01S-SOBKZ' valor = sobkz ).
      o_bi->campos( campo = 'RL01S-LSONR' valor = sonum ).
    ENDIF.

    IF NOT lgtyp IS INITIAL.
      o_bi->campos( campo = 'RL01S-LGTYP' valor = lgtyp ).
    ENDIF.
    IF NOT lgpla IS INITIAL.
      o_bi->campos( campo = 'RL01S-LGPLA' valor = lgpla ).
    ENDIF.

    l_mensaje = o_bi->llamar_transaccion( tcode = 'LS24'
                        modo = 'E' ).
  ENDMETHOD.
