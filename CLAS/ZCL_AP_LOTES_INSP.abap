CLASS zcl_ap_lotes_insp DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_bapi2045l2 TYPE TABLE OF bapi2045l2.
    TYPES tt_bapi2045l4 TYPE TABLE OF bapi2045l4.
    TYPES tt_bapi2045d1 TYPE TABLE OF bapi2045d1.
    TYPES tt_bapi2045d2 TYPE TABLE OF bapi2045d2.
    TYPES tt_bapi2045d3 TYPE TABLE OF bapi2045d3.

    DATA return1          TYPE bapireturn1.
    DATA i_operaciones    TYPE tt_bapi2045l2.
    DATA return2          TYPE bapiret2.
    DATA operacion        TYPE bapi2045l2.
    DATA requerimientos   TYPE bapi2045d5.
    DATA i_requerimientos TYPE tt_bapi2045d1.
    DATA i_resultados     TYPE tt_bapi2045d2.
    DATA i_return         TYPE bapiret2_t.
    DATA i_puntos_insp    TYPE tt_bapi2045d3.
    DATA i_puntos         TYPE tt_bapi2045l4.
    DATA message          TYPE bapi_msg.

    METHODS constructor
      IMPORTING lote_insp TYPE qals-prueflos OPTIONAL.

    METHODS get_operaciones.

    METHODS set_lote_insp
      IMPORTING lote_insp TYPE qals-prueflos.

    METHODS get_detalle_operacion
      IMPORTING n_oper TYPE bapi2045l2-inspoper.

    METHODS get_resultados_lote_insp
      IMPORTING r_noper      TYPE lmbp_range_c4_t  OPTIONAL
                no_car       TYPE abap_bool        DEFAULT ''
                r_userc1     TYPE range_t_matnr    OPTIONAL
                r_usern2     TYPE lxhme_range_n3_t OPTIONAL
      RETURNING VALUE(i_res) TYPE zres_lote_insp_t.

    CLASS-METHODS get_resultados_lote_insp_st
      IMPORTING lote_insp       TYPE qals-prueflos
                r_noper         TYPE lmbp_range_c4_t OPTIONAL
                noper           TYPE vornr           DEFAULT ''
                caracteristica  TYPE any             DEFAULT ''
                solo_ultimo_pto TYPE abap_bool       DEFAULT ''
                solo_evaluadas  TYPE abap_bool       DEFAULT ''
      RETURNING VALUE(i_res)    TYPE zres_lote_insp_t.

    CLASS-METHODS ver_resultados
      IMPORTING VALUE(lote_insp) TYPE qals-prueflos
                n_oper           TYPE qaqee-vornr DEFAULT '0010'
                editar           TYPE abap_bool   DEFAULT ''
                datos_punto_insp TYPE bapi2045l4  OPTIONAL
                modus            TYPE any         DEFAULT ''.

    METHODS set_resultados
      IMPORTING n_oper            TYPE bapi2045l2-inspoper
                i_res             TYPE zres_lote_insp_t
                dequeue_all       TYPE abap_bool  DEFAULT 'X'
                punto_insp        TYPE qibpprobe  OPTIONAL
                datos_punto_insp  TYPE bapi2045l4 OPTIONAL
                aux1              TYPE any        DEFAULT ''
                no_muestra_fisica TYPE abap_bool  DEFAULT 'X'
      RETURNING VALUE(mensaje)    TYPE bapireturn1-message.

    CLASS-METHODS get_evaluacion
      IMPORTING res               TYPE bapi2045d2
                katalgart         TYPE qkatausw DEFAULT '1'
                werks             TYPE werks_d
      RETURNING VALUE(evaluation) TYPE bapi2045d2-evaluation.

    CLASS-METHODS get_status_st
      IMPORTING lote_insp     TYPE qals-prueflos OPTIONAL
                spras         TYPE sy-langu      DEFAULT sy-langu
                bypass_buffer TYPE abap_bool     DEFAULT ''
                objnr         TYPE jest-objnr    OPTIONAL
                usuario       TYPE abap_bool     DEFAULT ''
      PREFERRED PARAMETER lote_insp
      RETURNING VALUE(status) TYPE bsvx-sttxt.

    CLASS-METHODS ver_lote_insp_st
      IMPORTING lote_insp TYPE qals-prueflos
                editar    TYPE abap_bool DEFAULT ''.

    CLASS-METHODS validar_resultado
      IMPORTING res            TYPE bapi2045d2
                katalgart      TYPE qkatausw   DEFAULT '1'
                werks          TYPE werks_d
                mstr_char      TYPE qmstr_char OPTIONAL
      RETURNING VALUE(mensaje) TYPE bapireturn1-message.

    METHODS forzar_recalculo_caract
      IMPORTING n_oper         TYPE bapi2045l2-inspoper OPTIONAL
                caract         TYPE qibpmerknr
                werks          TYPE werks_d
      RETURNING VALUE(mensaje) TYPE bapireturn1-message.

    CLASS-METHODS contiene_status
      IMPORTING lote_insp          TYPE qals-prueflos
                !status            TYPE any                        OPTIONAL
                spras              TYPE sy-langu                   DEFAULT sy-langu
                status_excluir     TYPE any                        DEFAULT ''
                s_status           TYPE zcl_ap_rango=>tab_range_c4 OPTIONAL
      RETURNING VALUE(si_contiene) TYPE abap_bool.

    CLASS-METHODS set_decision_empleo
      IMPORTING lote_insp             TYPE qals-prueflos
                werks                 TYPE werks_d
                code_group            TYPE qvgruppe
                !code                 TYPE qvcode
                force_completion      TYPE qinsp_can  DEFAULT 'X'
                stock_posting         TYPE qstock_pst DEFAULT 'X'
                vemenge               TYPE qvemenge
                usar_call_transaction TYPE abap_bool  DEFAULT ''
                bdc_mode              TYPE bdc_mode   DEFAULT 'N'
                status_fijos          TYPE string     DEFAULT ''
      RETURNING VALUE(mensaje)        TYPE bapireturn1-message.

    CLASS-METHODS crear_lote_insp
      IMPORTING qals            TYPE qals          OPTIONAL
                ktextlos        TYPE qals-ktextlos OPTIONAL
      PREFERRED PARAMETER qals
      RETURNING VALUE(prueflos) TYPE qplos.

    METHODS copiar_lote_insp
      IMPORTING prueflos            TYPE qplos
                ktextlos            TYPE qals-ktextlos OPTIONAL
                copiar_usern1       TYPE abap_bool     DEFAULT 'X'
      RETURNING VALUE(new_prueflos) TYPE qplos.

    METHODS crear_punto_inspeccion
      IMPORTING datos            TYPE bapi2045l4
      RETURNING VALUE(insppoint) TYPE qprobenrpp.

    METHODS get_list_pto_insp
      IMPORTING prueflos       TYPE qplos
                vornr          TYPE vornr OPTIONAL
      RETURNING VALUE(i_lista) TYPE qapptab.

    CLASS-METHODS anular
      IMPORTING lote_insp      TYPE qals-prueflos
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    CLASS-METHODS crear_lote_insp2
      IMPORTING qals     TYPE qals
                ktextlos TYPE qals-ktextlos DEFAULT ''
      EXPORTING prueflos TYPE qplos
                !message TYPE bapi_msg.

    CLASS-METHODS get_value_lote_insp_st
      IMPORTING lote_insp      TYPE qals-prueflos
                r_noper        TYPE lmbp_range_c4_t OPTIONAL
                noper          TYPE vornr           DEFAULT ''
                caracteristica TYPE any             OPTIONAL
                tipo           TYPE any             DEFAULT 'U'
      RETURNING VALUE(valor)   TYPE qmean_val.

    CLASS-METHODS crear_punto_insp_ct
      IMPORTING lote_insp TYPE qals-prueflos
                noper     TYPE vornr DEFAULT ''.

    CLASS-METHODS lanzar_qe71
      IMPORTING prueflos        TYPE qals-prueflos   OPTIONAL
                r_vornr         TYPE lmbp_range_c4_t OPTIONAL
                vornr           TYPE vornr           DEFAULT ''
                aufnr           TYPE aufnr           OPTIONAL
                prplatz_excluir TYPE string          DEFAULT ''.

    CLASS-METHODS tiene_decision_empleo
      IMPORTING lote_insp TYPE qals-prueflos
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS mov_stock
      IMPORTING lote_insp TYPE qals-prueflos
                menge     TYPE any
                meins     TYPE meins      DEFAULT ''
                werks     TYPE werks_d    DEFAULT ''
                lgort     TYPE lgort_d    DEFAULT ''
                bldat     TYPE bldat      DEFAULT sy-datum
                budat     TYPE budat      DEFAULT sy-datum
                bktxt     TYPE any        DEFAULT ''
                tcode     TYPE tcode      DEFAULT 'QA11'
                bestq     TYPE bestq      DEFAULT ''
                bwart     TYPE bwart      DEFAULT ''
                grund     TYPE mseg-grund OPTIONAL
                sgtxt     TYPE sgtxt      DEFAULT ''
                xblnr     TYPE xblnr      DEFAULT ''
      EXPORTING emkpf     TYPE emkpf
                !message  TYPE bapi_msg.

    CLASS-METHODS cambiar_status
      IMPORTING lote_insp TYPE qals-prueflos
                stat      TYPE any
                inact     TYPE any DEFAULT ''.

    CLASS-METHODS visualizar
      IMPORTING prueflos TYPE qals-prueflos.

    CLASS-METHODS get_resultados_simple.

  PROTECTED SECTION.
    DATA lote_insp TYPE qals-prueflos.

  PRIVATE SECTION.
endclass. "ZCL_AP_LOTES_INSP definition
class ZCL_AP_LOTES_INSP implementation.
  METHOD anular.
    DATA: l_status TYPE bsvx-sttxt,
          o_bi     TYPE REF TO zcl_ap_batch_input.

    l_status = zcl_ap_lotes_insp=>get_status_st( lote_insp ).
    IF l_status CS 'LOTA'.
      EXIT.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Pantalla acceso: tratamiento lote insp.
    o_bi->dynpro( program = 'SAPLQPL1' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'QALS-PRUEFLOS' valor = lote_insp ). " Nº lote inspección

    o_bi->dynpro( program = 'SAPLQPL1' dynpro = '0200' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=LSTO' ).

    o_bi->dynpro( program = 'SAPLQPL1' dynpro = '0200' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).

    mensaje = o_bi->llamar_transaccion( tcode = 'QA02' modo = 'E' ).
    l_status = zcl_ap_lotes_insp=>get_status_st( lote_insp ).
    IF l_status CS 'LOTA'.
      CLEAR mensaje.
    ELSEIF mensaje IS INITIAL.
      CONCATENATE 'No se ha anulado el lote de inspección'(NAD) lote_insp INTO mensaje SEPARATED BY space.
    ENDIF.
  ENDMETHOD.
  METHOD cambiar_status.
    DATA: l_objnr TYPE j_objnr,
          jest    TYPE jest.

    SELECT SINGLE objnr FROM qals
      INTO l_objnr
     WHERE prueflos = lote_insp.

    IF l_objnr IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM  jest
      INTO jest
     WHERE objnr = l_objnr
       AND stat  = stat.
    IF sy-subrc = 0.
      IF jest-inact <> inact.
        UPDATE jest
          SET inact = inact
         WHERE objnr = l_objnr
           AND stat  = stat.
      ENDIF.
    ELSE.
      CLEAR jest.
      jest-objnr = l_objnr.
      jest-stat  = stat.
      jest-inact = inact.
      MODIFY jest FROM jest.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    IF NOT lote_insp IS INITIAL.
      set_lote_insp( lote_insp ).
    ENDIF.
  ENDMETHOD.
  METHOD contiene_status.
    DATA: r_status  TYPE RANGE OF j_status,
          i_status  TYPE TABLE OF j_txt04,
          l_txt04   TYPE j_txt04,
          l_istat   TYPE j_status,
          lr_status LIKE LINE OF r_status,
          lr_stat   TYPE lxhme_range_c4,
          l_objnr   TYPE jest-objnr.

    IF NOT status IS INITIAL.
      SPLIT status AT ',' INTO TABLE i_status.

      LOOP AT i_status INTO l_txt04.
        SELECT istat FROM tj02t                               "#EC *
          INTO l_istat
          UP TO 1 ROWS
         WHERE spras = spras
           AND txt04 = l_txt04
         ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          CLEAR lr_status.
          lr_status-option = 'EQ'.
          lr_status-sign   = 'I'.
          lr_status-low    = l_istat.
          APPEND lr_status TO r_status.
        ENDIF.
      ENDLOOP.

      IF NOT status_excluir IS INITIAL.
        REFRESH i_status.
        SPLIT status_excluir AT ',' INTO TABLE i_status.
        LOOP AT i_status INTO l_txt04.
          SELECT istat FROM tj02t                             "#EC *
            INTO l_istat
            UP TO 1 ROWS
           WHERE spras = spras
             AND txt04 = l_txt04
            ORDER BY PRIMARY KEY.
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
    ELSEIF NOT s_status IS INITIAL.
      LOOP AT s_status INTO lr_stat.
        SELECT istat FROM tj02t                               "#EC *
          INTO l_istat
          UP TO 1 ROWS
         WHERE spras = spras
           AND txt04 = lr_stat-low
          ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          CLEAR lr_status.
          lr_status-option = lr_stat-option.
          lr_status-sign   = lr_stat-sign.
          lr_status-low    = l_istat.
          APPEND lr_status TO r_status.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF r_status IS INITIAL.
      EXIT.
    ENDIF.

    CONCATENATE 'QL' lote_insp INTO l_objnr.

    SELECT SINGLE stat FROM  jest                             "#EC *
      INTO l_istat
     WHERE objnr  = l_objnr
       AND stat  IN r_status
       AND inact  = ''.
    IF sy-subrc = 0.
      si_contiene = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD copiar_lote_insp.
    DATA: l_qals      TYPE qals,
          l_vornr     TYPE vornr,
          i_lista     TYPE TABLE OF qapp,
          l_lista     TYPE qapp,
          l_datos     TYPE bapi2045l4,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_insppoint TYPE qprobenrpp.

    SELECT SINGLE * FROM qals                   "#EC CI_ALL_FIELDS_NEEDED
      INTO l_qals
     WHERE prueflos = prueflos.

    new_prueflos = crear_lote_insp( qals = l_qals ktextlos  = ktextlos ).

    IF new_prueflos IS INITIAL.
      RETURN.
    ENDIF.

    SELECT vornr
      INTO l_vornr
      UP TO 1 ROWS
     FROM qals JOIN v_qapo ON qals~aufpl = v_qapo~aufpl
     WHERE prueflos = prueflos
     ORDER BY prueflos vornr.
    ENDSELECT.

    i_lista = get_list_pto_insp( prueflos = prueflos vornr = l_vornr ).
    LOOP AT i_lista INTO l_lista.
      CLEAR l_datos.
      MOVE-CORRESPONDING l_lista TO l_datos.
      l_datos-insplot  = new_prueflos.
      l_datos-inspoper = l_vornr.
      IF copiar_usern1 IS INITIAL.
        CLEAR l_datos-usern1.
      ENDIF.
      l_insppoint = crear_punto_inspeccion( datos = l_datos ).
    ENDLOOP.
  ENDMETHOD.
  METHOD crear_lote_insp.
    crear_lote_insp2( EXPORTING qals     = qals
                                ktextlos = ktextlos
                      IMPORTING prueflos = prueflos ).
  ENDMETHOD.
  METHOD crear_lote_insp2.
    DATA: l_qals  TYPE qals,
          l_aktiv TYPE qmat-aktiv,
          l_aux   TYPE c LENGTH 3.
    DATA ls_rmqed_imp     TYPE rmqed.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_kzskiplot     TYPE qvinsmk.
    DATA ls_qals          TYPE qals.
    DATA lv_subrc         TYPE sysubrc.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_skip_to_stock TYPE qkzskiplot.

    CLEAR message.

    IF NOT l_qals-matnr IS INITIAL.
      SELECT SINGLE aktiv FROM qmat
        INTO l_aktiv
       WHERE art   = l_qals-art
         AND matnr = l_qals-matnr
         AND werks = l_qals-werk.
      IF sy-subrc <> 0.
        message = zcl_ap_utils=>concat( p1 = 'El material'(EMA) p2 = l_qals-matnr p3 = 'no tiene la clase insp'(NCI) p4 = l_qals-art
                                        p5 = 'en el centro'(ECE) p6 = l_qals-werk ).
      ELSEIF l_aktiv IS INITIAL.
        message = zcl_ap_utils=>concat( p1 = 'El material'(EMA) p2 = l_qals-matnr p3 = 'tiene la clase insp'(TCI) p4 = l_qals-art
                                        p5 = 'inactiva'(INA) ).
      ENDIF.
    ENDIF.

    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.

    ls_rmqed_imp-dbs_steuer = '01'.
    ls_rmqed_imp-dbs_flag   = 'X'.
    ls_rmqed_imp-dbs_edunk  = 'X'.
    ls_rmqed_imp-dbs_fdunk  = 'X'.
    ls_rmqed_imp-dbs_noerr  = 'X'.
    ls_rmqed_imp-dbs_nowrn  = 'X'.
    ls_rmqed_imp-dbs_nochg  = 'X'.
    ls_rmqed_imp-dbs_noauf  = 'X'.
    ls_rmqed_imp-dbs_subrc  = 'X'.

    CALL FUNCTION 'QPL1_INITIALIZE'
      EXPORTING
        i_lot_data_only = 'X'.

    l_qals = qals.

    CLEAR l_qals.
    l_qals-werk       = qals-werk.
    l_qals-lagortchrg = qals-lagortchrg.
    l_qals-art        = qals-art.
    l_qals-herkunft   = qals-herkunft.
    l_qals-matnr      = qals-matnr.
    l_qals-losmenge   = qals-losmenge.
    l_qals-pastrterm  = sy-datum.
    l_qals-aufnr      = qals-aufnr.
    l_qals-charg      = qals-charg.
    l_qals-ebeln      = qals-ebeln.
    l_qals-ebelp      = qals-ebelp.
    IF ktextlos IS INITIAL.
      l_qals-ktextlos = qals-ktextlos.
    ELSE.
      l_qals-ktextlos = ktextlos.
    ENDIF.

    CALL FUNCTION 'QPL1_INSPECTION_LOT_CREATE'
      EXPORTING
        qals_imp        = l_qals
        rmqed_imp       = ls_rmqed_imp
      IMPORTING
        e_kzskiplot     = lv_kzskiplot
        e_prueflos      = prueflos
        e_qals          = ls_qals
        subrc           = lv_subrc
        e_skip_to_stock = lv_skip_to_stock.
    IF NOT prueflos IS INITIAL.
      CALL FUNCTION 'QPL1_UPDATE_MEMORY'
        EXPORTING
          i_qals  = ls_qals
          i_updkz = 'I'.

      CALL FUNCTION 'QPL1_INSPECTION_LOTS_POSTING'.

      zcl_ap_dev=>commit( ).
    ELSE.
      IF sy-msgty IS INITIAL.
        CASE lv_subrc.
          WHEN 8.
            message = zcl_ap_utils=>concat( p1 = 'Material'(MAT) p2 = l_qals-matnr p3 = 'Centro'(CEN) p4 = l_qals-werk p5 = 'no actualizado para clase insp'(NAC) p6 = qals-art ).
          WHEN OTHERS.
            l_aux = lv_subrc.
            CONCATENATE 'Error'(ERR) l_aux 'al crear lote inspección'(CLI) INTO message SEPARATED BY space.
        ENDCASE.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD crear_punto_insp_ct.
    DATA o_bi      TYPE REF TO zcl_ap_batch_input.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mensaje TYPE bapireturn1-message.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Entrada lote insp. entrada resultados
    o_bi->dynpro( program = 'SAPMQEEA' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENT0' ).
    o_bi->campos( campo = 'QALS-PRUEFLOS' valor = lote_insp ). " Nº lote inspección
    o_bi->campos( campo = 'QAQEE-VORNR' valor = noper ). " Número de operación
    o_bi->campos( campo = 'QAQEE-MODUS' valor = '1' ). " Filtro caract.(modo lectura) p.caract.insp.
    l_mensaje = o_bi->llamar_transaccion( tcode = 'QE01' modo = 'E' ).
  ENDMETHOD.
  METHOD crear_punto_inspeccion.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_insplot  TYPE qplos.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_inspoper TYPE vornr.

    CALL FUNCTION 'QIBP_INSPPOINT_CREATEFROMDATA' ##FM_SUBRC_OK
      EXPORTING
        data                 = datos
        external_commit      = ''
      IMPORTING
        insplot              = lv_insplot
        inspoper             = lv_inspoper
        insppoint            = insppoint
        return2              = return2
      TABLES
*     qmidi_errors         = lt_qmidi_errors
        returntable          = i_return
      EXCEPTIONS
        wrong_inspection_lot = 1
        wrong_operation      = 2
        qm_idi_error         = 3
        OTHERS               = 4.
  ENDMETHOD.
  METHOD forzar_recalculo_caract.
    DATA: l_pos_char TYPE i,
          o_bi       TYPE REF TO zcl_ap_batch_input,
          l_req      TYPE bapi2045d1.

    get_detalle_operacion( n_oper = n_oper ).

    IF i_requerimientos IS INITIAL.
      mensaje = 'Error buscando características lote de inspección'(EBC).
    ELSE.
      LOOP AT i_requerimientos TRANSPORTING NO FIELDS WHERE inspchar <= caract.
        l_pos_char = l_pos_char + 1.
      ENDLOOP.
      IF l_pos_char = 0.
        CONCATENATE 'Caract.'(CAR) caract ' no existe en lote insp'(NEL) INTO mensaje SEPARATED BY space.
      ELSE.
        o_bi = NEW #( ).

        o_bi->inicio( ).

* Entrada lote insp. entrada resultados
        o_bi->dynpro( program = 'SAPMQEEA' dynpro = '0100' ).
        o_bi->campos( campo = 'QALS-PRUEFLOS' valor = lote_insp ). " Nº lote inspección
        o_bi->campos( campo = 'QAQEE-VORNR' valor = n_oper ). " Número de operación
        o_bi->campos( campo = 'QAQEE-PRPLATZWRK' valor = werks ). " Centro
        o_bi->campos( campo = 'QAQEE-MODUS' valor = '1' ). " Filtro caract.(modo lectura) p.caract.insp.

        o_bi->dynpro( program = 'SAPLQEEM' dynpro = '1110' ).

        IF l_pos_char > 0.

          o_bi->campos( campo = 'BDC_OKCODE' valor = '=MKPR_PUSH' ).

          LOOP AT i_requerimientos INTO l_req WHERE inspchar < caract.
* Pantalla indiv. caract. cualitativa códigos de partic.
            IF l_req-char_type = '02'.
              o_bi->dynpro( program = 'SAPLQEEM' dynpro = '0118' ).
            ELSE.
              o_bi->dynpro( program = 'SAPLQEEM' dynpro = '0116' ).
            ENDIF.

            o_bi->campos( campo = 'BDC_OKCODE' valor = '=NMKP' ).
          ENDLOOP.
          IF l_req-char_type = '02'.
            o_bi->dynpro( program = 'SAPLQEEM' dynpro = '0118' ).
          ELSE.
            o_bi->dynpro( program = 'SAPLQEEM' dynpro = '0116' ).
          ENDIF.
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=FOCA' ).

* Pantalla individual caract.cuantitativa
          o_bi->dynpro( program = 'SAPLQEEM' dynpro = '0116' ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).
        ENDIF.

        mensaje = o_bi->llamar_transaccion( tcode = 'QE01' modo = 'N' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_detalle_operacion.
    CLEAR: operacion, requerimientos, return2.

    IF n_oper IS INITIAL.
      READ TABLE i_operaciones INTO operacion INDEX 1.
    ELSE.
      operacion-inspoper = n_oper.
    ENDIF.

    CALL FUNCTION 'BAPI_INSPOPER_GETDETAIL'                       "#EC *
      EXPORTING
        insplot                            = lote_insp
        inspoper                           = operacion-inspoper
        read_insppoints                    = 'X'
        read_char_requirements             = 'X'
        read_char_results                  = 'X'
        read_sample_results                = 'X'
        read_single_results                = 'X'
        read_chars_with_classes            = 'X'
        read_chars_without_recording       = 'X'
*        RES_ORG                            = ' '
*        CHAR_FILTER_NO                     = '1   '
*        CHAR_FILTER_TCODE                  = 'QE11'
*        MAX_INSPPOINTS                     = 100
*        INSPPOINT_FROM                     = 0
*        HANDHELD_APPLICATION               = ' '
      IMPORTING
        operation                          = operacion
        insppoint_requirements             = requerimientos
        return                             = return2
      TABLES
        insppoints                         = i_puntos
        char_requirements                  = i_requerimientos
        char_results                       = i_resultados
        sample_results                     = i_puntos_insp
*        single_results                     =
      EXCEPTIONS
        OTHERS                             = 1.
  ENDMETHOD.
  METHOD get_evaluacion.
    " TODO: parameter KATALGART is never used (ABAP cleaner)

    DATA: l_qabwr TYPE qabwr,
          i_qaspr TYPE TABLE OF qaspr.

    l_qabwr-katalgart = '1'.
    l_qabwr-auswmenge = res-code_grp1. "??? Tabla QPAC
    l_qabwr-auswmgwrk = werks.
    l_qabwr-gruppe    = res-code_grp1.
    l_qabwr-code      = res-code1.

    CALL FUNCTION 'QEBR_CODE_VALUATION'
      EXPORTING
        dynprocall         = space
        qabwr              = l_qabwr
      IMPORTING
*        DBEWERTG           =
*        FEHLKLAS           =
        mbewertg           = evaluation
*        FECODSELE          =
*        FEGRPSELE          =
*        E_QABWR_EX         =
      TABLES
        qasptab            = i_qaspr
      EXCEPTIONS
        other_error        = 1
        system_error       = 2
        user_error         = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      evaluation = 'F'.
    ENDIF.
  ENDMETHOD.
  METHOD get_list_pto_insp.
    DATA: l_vornr TYPE qapo-vornr,
          i_qalt  TYPE TABLE OF qalt.

    IF vornr IS INITIAL.
      SELECT SINGLE vornr
        INTO l_vornr
       FROM qals JOIN v_qapo ON qals~aufpl = v_qapo~aufpl
       WHERE prueflos = prueflos.
    ELSE.
      l_vornr = vornr.
    ENDIF.

    IF l_vornr IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'QIBP_INSPPOINT_GETLIST'   ##FM_SUBRC_OK
      EXPORTING
        i_insp_lot               = prueflos
        i_insp_lot_operation     = l_vornr
        i_insp_point_from        = '000001'
        i_insp_point_to          = '999999'
      TABLES
        t_qapp_tab               = i_lista
        t_qalt_tab               = i_qalt
      EXCEPTIONS
        wrong_insp_lot           = 1
        wrong_insp_lot_operation = 2
        no_insp_point            = 3
        OTHERS                   = 4.
  ENDMETHOD.
  METHOD get_operaciones.
    CALL FUNCTION 'BAPI_INSPLOT_GETOPERATIONS'
      EXPORTING
        number        = lote_insp
      IMPORTING
        return        = return1
      TABLES
        inspoper_list = i_operaciones.
  ENDMETHOD.
  METHOD get_resultados_lote_insp.
    " TODO: parameter NO_CAR is never used (ABAP cleaner)

    DATA: l_aufpl    TYPE qals-aufpl,
          l_vorglfnr TYPE qapp-vorglfnr,
          l_userc1   TYPE qapp-userc1,
          l_usern2   TYPE qapp-usern2,
          l_res      TYPE zres_lote_insp.

    FIELD-SYMBOLS: <oper> TYPE bapi2045l2,
                   <pto>  TYPE bapi2045d3,
                   <req>  TYPE bapi2045d1,
                   <p>    TYPE bapi2045l4,
                   <res>  TYPE bapi2045d2.

    CLEAR i_res.
    get_operaciones( ).

    LOOP AT i_operaciones ASSIGNING <oper> WHERE inspoper IN r_noper.
      get_detalle_operacion( <oper>-inspoper ).

      SELECT SINGLE aufpl FROM qals
        INTO l_aufpl
       WHERE prueflos = <oper>-insplot.

      SELECT vorglfnr FROM v_qapo
        INTO l_vorglfnr
        UP TO 1 ROWS
       WHERE aufpl = l_aufpl
         AND vornr = <oper>-inspoper
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      LOOP AT i_puntos_insp ASSIGNING <pto>.
        IF NOT r_userc1 IS INITIAL OR NOT r_usern2 IS INITIAL.
          SELECT SINGLE userc1 usern2 FROM qapp
            INTO (l_userc1, l_usern2)
           WHERE prueflos = <pto>-insplot
             AND vorglfnr = l_vorglfnr
             AND probenr  = <pto>-inspsample.
          IF NOT ( l_userc1 IN r_userc1 AND l_usern2 IN r_usern2 ).
            CONTINUE.
          ENDIF.
        ENDIF.

        CLEAR l_res.
        ASSIGN i_requerimientos[ inspoper = <pto>-inspoper
                                 inspchar = <pto>-inspchar ] TO <req>.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <req> TO l_res.                  "#EC *
          ASSIGN i_puntos[ inspoper  = <pto>-inspoper
                           insppoint = <pto>-inspsample ] TO <p>.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <p> TO l_res.                  "#EC *
          ENDIF.

          MOVE-CORRESPONDING <pto> TO l_res.                  "#EC *
          l_res-txt_oper = <oper>-txt_oper.
          APPEND l_res TO i_res.
        ENDIF.
      ENDLOOP.
      IF sy-subrc <> 0. " Si tiene puntos de inspección, no queremos resultados
        LOOP AT i_requerimientos ASSIGNING <req>.
          CLEAR l_res.
          MOVE-CORRESPONDING <req> TO l_res.                  "#EC *
          l_res-txt_oper = <oper>-txt_oper.
          ASSIGN i_resultados[ insplot  = <req>-insplot
                               inspoper = <req>-inspoper
                               inspchar = <req>-inspchar ] TO <res>.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <res> TO l_res.                "#EC *
          ENDIF.
          APPEND l_res TO i_res.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_resultados_lote_insp_st.
    DATA: o_li      TYPE REF TO zcl_ap_lotes_insp,
          rr_noper  TYPE lmbp_range_c4_t,
          lr_noper  TYPE lmbp_range_c4,
          l_res     TYPE zres_lote_insp,
          l_max_pto TYPE qibpprobe.

    o_li = NEW #(
        lote_insp = lote_insp ).

    IF NOT r_noper IS INITIAL.
      rr_noper = r_noper.
    ELSEIF NOT noper IS INITIAL.
      CLEAR lr_noper.
      lr_noper-option = 'EQ'.
      lr_noper-sign   = 'I'.
      lr_noper-low    = noper.
      APPEND lr_noper TO rr_noper.
    ENDIF.

    i_res = o_li->get_resultados_lote_insp( r_noper = rr_noper ).

    IF NOT caracteristica IS INITIAL.
      DELETE i_res WHERE mstr_char <> caracteristica.
    ENDIF.

    IF solo_ultimo_pto = 'X'.
      LOOP AT i_res INTO l_res.
        IF l_res-inspsample > l_max_pto.
          l_max_pto = l_res-inspsample.
        ENDIF.
      ENDLOOP.
      DELETE i_res WHERE inspsample <> l_max_pto.
    ENDIF.

    IF solo_evaluadas = 'X'.
      DELETE i_res WHERE closed = '' AND evaluated = ''.
    ENDIF.
  ENDMETHOD.
  METHOD get_resultados_simple.
* Puede ser también con qasr en lugar de QAMR
    SELECT qals~prueflos, v_qapo~vornr, plmk~verwmerkm, plmk~kurztext, qamr~mittelwert,
           qamr~katalgart1, qamr~gruppe1, qamr~code1, qamr~version1
      FROM qals JOIN v_qapo ON qals~aufpl = v_qapo~aufpl
                JOIN qamr   ON qamr~prueflos = qals~prueflos
                           AND qamr~vorglfnr = v_qapo~vorglfnr
                JOIN plmk   ON  plmk~plnty  = v_qapo~plnty
                            AND plmk~plnnr  = v_qapo~plnnr
                            AND plmk~plnkn  = v_qapo~pplnkn
                            AND plmk~merknr = qamr~merknr
                            AND plmk~loekz  = ''
      INTO TABLE @DATA(i_res)
     WHERE qals~prueflos = '010000010313'.

    cl_demo_output=>display( i_res ).
  ENDMETHOD.
  METHOD get_status_st.
    DATA: l_objnr     TYPE jest-objnr,
          l_user_line TYPE bsvx-sttxt.

    IF objnr IS INITIAL.
      CONCATENATE 'QL' lote_insp INTO l_objnr.
    ELSE.
      l_objnr = objnr.
    ENDIF.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
*     CLIENT           = SY-MANDT
        flg_user_stat    = usuario
        objnr            = l_objnr
*     ONLY_ACTIVE      = 'X'
        spras            = spras
        bypass_buffer    = bypass_buffer
      IMPORTING
*     ANW_STAT_EXISTING       =
*     E_STSMA          =
        line             = status
        user_line        = l_user_line
*     STONR            =
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.                                         "#EC *
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF usuario = 'X'.
      status = l_user_line.
    ENDIF.
  ENDMETHOD.
  METHOD get_value_lote_insp_st.
    DATA: i_res   TYPE zres_lote_insp_t,
          l_valor TYPE mengv13,
          l_cont  TYPE i.

    FIELD-SYMBOLS <res> TYPE zres_lote_insp.

    i_res = get_resultados_lote_insp_st( lote_insp = lote_insp r_noper = r_noper noper = noper caracteristica = caracteristica ).

    IF i_res IS INITIAL.
      valor = '????'.
    ELSE.
      CASE tipo.
        WHEN 'U'.
          LOOP AT i_res ASSIGNING <res>.
            valor = <res>-mean_value.
          ENDLOOP.
        WHEN 'P'.
          LOOP AT i_res ASSIGNING <res>.
            valor = <res>-mean_value.
            EXIT.
          ENDLOOP.
        WHEN 'M'.
          LOOP AT i_res ASSIGNING <res>.
            l_valor = <res>-mean_value.
            valor = valor + l_valor.
            l_cont = l_cont + 1.
          ENDLOOP.
          valor = valor / l_cont.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
  METHOD lanzar_qe71.
    DATA: i_qals     TYPE TABLE OF qals,
          l_qals     TYPE qals,
          i_qapo     TYPE TABLE OF qapo,
          l_qapo     TYPE qapo,
          i_qapo_sel TYPE TABLE OF qapo,
          r_prplatz  TYPE RANGE OF string,
          r_prplz    TYPE RANGE OF qapo-prplatz.

    IF NOT prueflos IS INITIAL.
      SELECT prueflos FROM qals
        INTO CORRESPONDING FIELDS OF TABLE i_qals
       WHERE prueflos = prueflos.
    ELSEIF NOT aufnr IS INITIAL.
      SELECT prueflos FROM qals                           "#EC CI_NOFIRST
        INTO CORRESPONDING FIELDS OF TABLE i_qals
       WHERE aufnr = aufnr.
    ENDIF.

    LOOP AT i_qals INTO l_qals.
      CALL FUNCTION 'QIBP_INSPOPER_GETLIST' ##FM_SUBRC_OK
        EXPORTING
          i_insp_lot        = l_qals-prueflos
          i_insp_oper       = vornr
*       I_HANDHELD_APPLICATION       =
        TABLES
          t_qapo_tab        = i_qapo
*       T_QEWL_TAB        =
        EXCEPTIONS
          no_inspection_lot = 1
          no_operations     = 2
          OTHERS            = 3.

      LOOP AT i_qapo INTO l_qapo WHERE vornr IN r_vornr.
        APPEND l_qapo TO i_qapo_sel.
      ENDLOOP.
    ENDLOOP.

    IF NOT prplatz_excluir IS INITIAL.
      r_prplatz = zcl_ap_lista=>lista2rango( lista = prplatz_excluir ).
      DELETE i_qapo_sel WHERE prplatz IN r_prplatz.
    ENDIF.

    IF i_qapo_sel IS INITIAL.
      RETURN.
    ENDIF.

    EXPORT i_qapo_sel FROM i_qapo_sel TO MEMORY ID 'ZQE71_SEL'.

    SUBMIT rqetbm10                                        "#EC CI_SUBMIT
       AND RETURN
           WITH ql_matnr = ''
           WITH ql_charg = '?'
           WITH ql_prplz IN r_prplz.

****** Para que funcione, en RQETBI10 incluir este enhancement
*form fill_fieldgroups_l.
*"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(1) Forma FILL_FIELDGROUPS_L, Inicio                                                                                                                  A
**$*$-Start: (1)---------------------------------------------------------------------------------$*$*
*enhancement 1  zqe71_sel.    "active version
*  data: i_qapo_sel type table of qapo,
*        l_qapo_sel type qapo,
*        l_qals type qals.
*
*  import i_qapo_sel to i_qapo_sel from memory id 'ZQE71_SEL'.
*  if sy-subrc = 0.
*    if not i_qapo_sel is initial.
*      sort i_qapo_sel.
*      loop at i_qapo_sel into l_qapo_sel.
*        at new prueflos.
*          select single * from qals
*            into l_qals
*           where prueflos = l_qapo_sel-prueflos.
*        endat.
*
*        read table object_tab with key prueflos = l_qapo_sel-prueflos
*                                       vornr    = l_qapo_sel-vornr.
*        if sy-subrc ne 0.
*          clear object_tab.
*          move-corresponding l_qals to object_tab.
*          move-corresponding l_qapo_sel to object_tab.
*          if object_tab-objnr  is initial.
*          object_tab-objnr = l_qals-objnr.
*          endif.
*
*          call function 'STATUS_TEXT_EDIT'
*            exporting
*              objnr                   = object_tab-objnr
*              spras                   = sy-langu
*            importing
*              line                    = object_tab-sttxt
*            exceptions
*              object_not_found        = 1
*              others                  = 2.
*          append object_tab.
*        endif.
*      endloop.
*    endif.
*    free memory id 'ZQE71_SEL'.
*  endif.
*
*
*endenhancement.
**$*$-End:   (1)---------------------------------------------------------------------------------$*$*
  ENDMETHOD.
  METHOD mov_stock.
    DATA: l_qals       TYPE qals,
          g_imkpf      TYPE imkpf,
          g_imsegtab   TYPE TABLE OF imseg,
          g_emsegtab   TYPE TABLE OF emseg,
          l_imseg      TYPE imseg,
          l_bestq      TYPE c LENGTH 1,
          l_bestq_orig TYPE c LENGTH 1,
          " TODO: variable is assigned but never used (ABAP cleaner)
          zes_mkpf     TYPE mkpf,
          l_updkz      TYPE c LENGTH 1.

    SELECT SINGLE * FROM qals
      INTO l_qals
     WHERE prueflos = lote_insp.
    IF sy-subrc <> 0.
      message = 'No existe el lote de inspección'(NLI).
      EXIT.
    ENDIF.

    IF tiene_decision_empleo( lote_insp ) = 'X'.
      message = 'Decisión de empleo tomada'(DET).
      EXIT.
    ENDIF.

    CLEAR g_imkpf.
    CLEAR g_imsegtab.
    REFRESH g_imsegtab.
    CLEAR g_emsegtab.
    REFRESH g_emsegtab.

    g_imkpf-bldat = bldat.
    g_imkpf-budat = budat.
    g_imkpf-bktxt = bktxt.
    g_imkpf-xblnr = xblnr.

    MOVE-CORRESPONDING l_qals TO l_imseg.
    CLEAR l_imseg-kzvbr.
    l_imseg-erfme = l_qals-mengeneinh.
    IF NOT meins IS INITIAL AND meins <> l_qals-mengeneinh.
      l_imseg-erfmg = zcl_ap_material=>convertir_unidad( matnr = l_qals-matnr unidad_origen = meins unidad_destino = l_qals-mengeneinh cantidad = menge ).
    ELSE.
      l_imseg-erfmg = menge.
    ENDIF.

    IF werks IS INITIAL.
      l_imseg-werks = l_qals-werkvorg.
    ELSE.
      l_imseg-werks = werks.
    ENDIF.
    IF lgort IS INITIAL.
      l_imseg-lgort = l_qals-lagortvorg.
    ELSE.
      l_imseg-lgort = lgort.
    ENDIF.
    l_imseg-qplos = l_qals-prueflos.

    l_imseg-grund = grund.
    l_imseg-sgtxt = sgtxt.

    IF NOT bwart IS INITIAL.
      l_imseg-bwart = bwart.
      IF l_imseg-bwart = '350'.
        l_bestq = 'S'.
      ELSEIF l_imseg-bwart = '349'.
        l_bestq = 'Q'.
        l_bestq_orig = 'S'.
      ENDIF.
    ELSEIF bestq = ''.
      l_bestq = bestq.
      l_imseg-bwart = '321'. " Calidad a libre
    ELSEIF bestq = 'S'.
      l_bestq = bestq.
      l_imseg-bwart = '350'. " Calidad a bloqueado
    ELSE.
      CONCATENATE 'No es posible determinar tipo de movimiento a stock'(NDT) bestq INTO message SEPARATED BY space.
      EXIT.
    ENDIF.

    APPEND l_imseg TO g_imsegtab.

    CALL FUNCTION 'QPL1_UPDATE_MEMORY'
      EXPORTING
        i_qals = l_qals.

    CALL FUNCTION 'QAAT_QM_ACTIVE_INACTIVE'
      EXPORTING
        aktiv = space.

    CALL FUNCTION 'MB_CREATE_GOODS_MOVEMENT'
      EXPORTING
        imkpf     = g_imkpf
        xallp     = 'X' " c_kreuz
*     XALLB     = ' '
        xallr     = ''
        ctcod     = tcode
*     XQMCL     = ' '
        old_subrc = 0
*     IPKCOM    =
*     X_AUTHORITY           = ' '
*     XLISU     = 'X'
*     XQMSR     = ' '
*     MSR_MB_DATA           =
      IMPORTING
        emkpf     = emkpf
*     E_LVS_TAFKZ           =
        es_mkpf   = zes_mkpf
      TABLES
        imseg     = g_imsegtab
        emseg     = g_emsegtab
*     IMSEG_CSL_TOKEN       =
*     ET_MSEG   =
*     IACCOUNTING           =
*     IACCOUNTING_CR        =
      .

    IF emkpf-mblnr IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      EXIT.
    ENDIF.

    CALL FUNCTION 'QAMB_COLLECT_RECORD'
      EXPORTING
        lotnumber   = l_qals-prueflos " ti_datos-prueflos
        docyear     = emkpf-mjahr
        docnumber   = emkpf-mblnr
        docposition = l_qals-zeile " ti_datos-zeile
        type        = '3'. " 3  Documentos de material p.traspasar una decisión de empleo

    IF l_bestq = 'Q'.
      l_qals-lmengezub = l_qals-lmengezub + l_imseg-erfmg.
      IF l_bestq_orig = 'S'.
        l_qals-lmenge04 = l_qals-lmenge04 - l_imseg-erfmg.
      ELSE.
        l_qals-lmenge01 = l_qals-lmenge01 - l_imseg-erfmg.
      ENDIF.
    ELSE.
      l_qals-lmengezub = l_qals-lmengezub - l_imseg-erfmg.
      IF l_bestq IS INITIAL.
        l_qals-lmenge01 = l_qals-lmenge01 + l_imseg-erfmg.
      ELSEIF l_bestq = 'S'.
        l_qals-lmenge04 = l_qals-lmenge04 + l_imseg-erfmg.
      ENDIF.
    ENDIF.

*           Doc.de material de contabilización DE o corrección de ctd.
*1  Documentos de material p.crear lotes
*2  Documentos de material p.corregir una cantidad real
*3  Documentos de material p.traspasar una decisión de empleo
*4  Doc.material p.trasladar stock cal.de un lote insp.
*5  Doc.material p.entregas siguientes tras descargo de stock
*6  Doc.material p.creación lotes o entregas sig.anulados
*7  Doc.material anulados p.traspasar una decisión de empleo
*X  Documentos de material para el inventario QM
* DEL MOVIMIENTO
    CALL FUNCTION 'MB_POST_GOODS_MOVEMENT'
      IMPORTING
        emkpf = emkpf.

    CALL FUNCTION 'QAAT_QM_ACTIVE_INACTIVE'
      EXPORTING
        aktiv = 'X'.

    l_qals-mblnr = emkpf-mblnr.
    l_qals-mjahr = emkpf-mjahr.
    l_updkz = 'U'.
    CALL FUNCTION 'QPL1_UPDATE_MEMORY'
      EXPORTING
        i_qals  = l_qals
        i_updkz = l_updkz.

    WAIT UP TO 1 SECONDS.

* Post lot to DataBase...
    CALL FUNCTION 'QPL1_INSPECTION_LOTS_POSTING'
      EXPORTING
        i_mode = '1'.

    CALL FUNCTION 'STATUS_UPDATE_ON_COMMIT'.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*/QAMB initialisieren
    CALL FUNCTION 'QAMB_REFRESH_DATA'.
  ENDMETHOD.
  METHOD set_decision_empleo.
    DATA: l_status  TYPE bsvx-sttxt,
          l_data    TYPE bapi2045ud,
          l_return  TYPE bapireturn1,
          o_bi      TYPE REF TO zcl_ap_batch_input,
          i_jstatus TYPE TABLE OF j_status,
          l_jstatus TYPE j_status.

    CLEAR mensaje.
    l_status = zcl_ap_lotes_insp=>get_status_st( lote_insp ).
    IF l_status CS 'LOTA'.
      mensaje = 'Lote de inspección anulado. No se puede dar DE'(NPD).
    ENDIF.

    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF usar_call_transaction IS INITIAL.
      l_data-insplot         = lote_insp.
      l_data-ud_plant        = werks.
      l_data-ud_code_group   = code_group.
      l_data-ud_selected_set = vemenge.
      IF l_data-ud_selected_set IS INITIAL.
        l_data-ud_selected_set = code_group.
      ENDIF.
      l_data-ud_code             = code.
      l_data-ud_force_completion = force_completion.
      l_data-ud_stock_posting    = stock_posting.

      CALL FUNCTION 'BAPI_INSPLOT_SETUSAGEDECISION'
        EXPORTING
          number  = lote_insp
          ud_data = l_data
*       LANGUAGE             =
        IMPORTING
*       UD_RETURN_DATA       =
*       STOCK_DATA           =
          return  = l_return
* TABLES
*       SYSTEM_STATUS        =
*       USER_STATUS          =
        .

      IF l_return-type = 'E'.
        mensaje = l_return-message.
        ROLLBACK WORK.                                   "#EC CI_ROLLBACK
      ELSE.
        zcl_ap_dev=>commit( ).
      ENDIF.
    ELSE.
      IF NOT l_status CS `DE  `.
        o_bi = NEW #( ).
        o_bi->inicio( ).

* Pantalla inicial decisión de empleo
        o_bi->dynpro( program = 'SAPMQEVA' dynpro = '0100' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
        o_bi->campos( campo = 'QALS-PRUEFLOS' valor = lote_insp ). " Nº lote inspección

        o_bi->dynpro( program = 'SAPMQEVA' dynpro = '0200' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).
        o_bi->campos( campo = 'RQEVA-VCODE' valor = code ). " Código de decisión de empleo
        o_bi->campos( campo = 'RQEVA-VCODEGRP' valor = code_group ). " Grupo de códigos de decisión de empleo

        mensaje = o_bi->llamar_transaccion( tcode = 'QA11' modo = bdc_mode ).

        IF mensaje CS 'Se ha grabado la decisión de empleo'(SGD).
          CLEAR mensaje.
        ELSEIF tiene_decision_empleo( lote_insp ) = 'X'.
          CLEAR mensaje.
        ELSEIF mensaje IS INITIAL.
          mensaje = 'No se ha tomado DE'(NDE).
        ENDIF.
      ENDIF.
    ENDIF.

    IF mensaje IS INITIAL AND NOT status_fijos IS INITIAL.
      DO 4 TIMES.
        SELECT SINGLE prueflos FROM qave
           INTO l_data-insplot
          WHERE prueflos = lote_insp.
        IF sy-subrc = 0.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.

* Nos aseguramos que el lote de inspección tiene estos status
      SPLIT status_fijos AT ',' INTO TABLE i_jstatus.
      LOOP AT i_jstatus INTO l_jstatus.
        cambiar_status( lote_insp = lote_insp stat = l_jstatus ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD set_lote_insp.
    me->lote_insp = lote_insp.
  ENDMETHOD.
  METHOD set_resultados.
    " TODO: parameter AUX1 is never used (ABAP cleaner)

    DATA: l_res                         TYPE bapi2045d2,
          l_punto                       TYPE bapi2045l4,
          l_pto                         TYPE bapi2045d3,
          l_pto_insp                    TYPE qibpprobe,
          l_return2                     TYPE bapiret2,
          l_err_evaluar_formula_ignorar TYPE c LENGTH 1,
          l_mensaje_formula             TYPE string,
          l_error_formula               TYPE c LENGTH 1,
          l_error_formula_op            TYPE c LENGTH 1.

    FIELD-SYMBOLS: <req>     TYPE bapi2045d1,
                   <res_new> TYPE zres_lote_insp,
                   <res>     TYPE bapi2045d2,
                   <pto>     TYPE bapi2045d3.

    get_detalle_operacion( n_oper ).

    DELETE i_puntos_insp WHERE inspsample <> punto_insp.

    IF i_resultados IS INITIAL.
      LOOP AT i_requerimientos ASSIGNING <req>.
        CLEAR l_res.
        MOVE-CORRESPONDING <req> TO l_res.                    "#EC *

        ASSIGN i_res[ insplot  = l_res-insplot
                      inspoper = l_res-inspoper
                      inspchar = l_res-inspchar ] TO <res_new>.
        IF sy-subrc = 0.
          l_res-code_grp1 = <res_new>-sel_set1.
        ENDIF.
        APPEND l_res TO i_resultados.
      ENDLOOP.
    ENDIF.

    LOOP AT i_resultados ASSIGNING <res>.
      ASSIGN i_res[ insplot  = <res>-insplot
                    inspoper = <res>-inspoper
                    inspchar = <res>-inspchar ] TO <res_new>.
      IF sy-subrc = 0.
        IF    <res>-code1       <> <res_new>-code1
           OR <res>-mean_value  <> <res_new>-mean_value
           OR <res_new>-formula <> ''.
          IF <res_new>-char_type = '02'.
            <res>-code1      = <res_new>-code1.
            <res>-code_grp1  = <res_new>-sel_set1.
            <res>-evaluation = get_evaluacion( res       = <res>
                                               katalgart = <res_new>-cat_type1
                                               werks     = <res_new>-psel_set1 ).
          ELSE.
            <res>-mean_value = <res_new>-mean_value.
            <res>-evaluation = 'A'.
          ENDIF.
          <res>-remark    = <res_new>-remark.
          <res>-evaluated = 'X'.
          <res>-closed    = 'X'.
          CLEAR <res>-evaluation. "???
        ENDIF.
      ENDIF.
      CLEAR <res>-err_class. "??
    ENDLOOP.

    IF NOT punto_insp IS INITIAL OR NOT datos_punto_insp IS INITIAL.

      READ TABLE i_puntos INTO l_punto WITH KEY insppoint = punto_insp.
      IF sy-subrc <> 0.
        CLEAR l_punto.
        l_punto = datos_punto_insp.
        ASSIGN i_requerimientos[ 1 ] TO <req>.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING <req> TO l_punto.                "#EC *
        ENDIF.
        l_punto-insppoint = punto_insp.
      ENDIF.
      IF NOT datos_punto_insp IS INITIAL.
        l_punto-userc1    = datos_punto_insp-userc1.
        l_punto-userc2    = datos_punto_insp-userc2.
        l_punto-usern1    = datos_punto_insp-usern1.
        l_punto-usern2    = datos_punto_insp-usern2.
        l_punto-userd1    = datos_punto_insp-userd1.
        l_punto-usert1    = datos_punto_insp-usert1.
        l_punto-inspector = datos_punto_insp-inspector.
      ENDIF.

      CLEAR i_resultados.

      IF i_puntos_insp IS INITIAL.
        LOOP AT i_requerimientos ASSIGNING <req>.
          CLEAR l_pto.
          MOVE-CORRESPONDING <req> TO l_pto.                  "#EC *

          ASSIGN i_res[ insplot  = l_pto-insplot
                        inspoper = l_pto-inspoper
                        inspchar = l_pto-inspchar ] TO <res_new>.
          IF sy-subrc = 0.
            l_pto-code_grp1  = <res_new>-sel_set1.
            l_pto-inspsample = punto_insp.
            APPEND l_pto TO i_puntos_insp.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF punto_insp > '000001'.
        l_pto_insp = '000001'.
      ELSE.
        l_pto_insp = '000000'.
      ENDIF.

      LOOP AT i_puntos_insp ASSIGNING <pto> WHERE inspsample = punto_insp.
        ASSIGN i_res[ insplot    = <pto>-insplot
                      inspoper   = <pto>-inspoper
                      inspchar   = <pto>-inspchar
                      inspsample = <pto>-inspsample ] TO <res_new>.
        IF sy-subrc = 0.
          IF    <pto>-code1       <> <res_new>-code1
             OR <pto>-mean_value  <> <res_new>-mean_value
             OR <res_new>-formula <> ''.
            IF <res_new>-char_type = '02'.
              <pto>-code1     = <res_new>-code1.
              <pto>-code_grp1 = <res_new>-sel_set1.
              MOVE-CORRESPONDING <pto> TO l_res.              "#EC *
              <pto>-evaluation = get_evaluacion( res       = l_res
                                                 katalgart = <res_new>-cat_type1
                                                 werks     = <res_new>-psel_set1 ).
            ELSE.
              <pto>-mean_value = <res_new>-mean_value.
              <pto>-evaluation = 'A'.
            ENDIF.
            <pto>-remark    = <res_new>-remark.
            <pto>-evaluated = 'X'.
            <pto>-closed    = 'X'.
            CLEAR <pto>-evaluation. "???
          ENDIF.
        ELSE.
          ASSIGN i_res[ insplot    = <pto>-insplot
                        inspoper   = <pto>-inspoper
                        inspchar   = <pto>-inspchar
                        inspsample = l_pto_insp ] TO <res_new>.
          IF sy-subrc = 0.
            IF    <pto>-code1       <> <res_new>-code1
               OR <pto>-mean_value  <> <res_new>-mean_value
               OR <res_new>-formula <> ''.
              IF <res_new>-char_type = '02'.
                <pto>-code1     = <res_new>-code1.
                <pto>-code_grp1 = <res_new>-sel_set1.
                MOVE-CORRESPONDING <pto> TO l_res.            "#EC *
                <pto>-evaluation = get_evaluacion( res       = l_res
                                                   katalgart = <res_new>-cat_type1
                                                   werks     = <res_new>-psel_set1 ).
              ELSE.
                <pto>-mean_value = <res_new>-mean_value.
                <pto>-evaluation = 'A'.
              ENDIF.
              <pto>-evaluated = 'X'.
              <pto>-closed    = 'X'.
              CLEAR <pto>-evaluation. "???
            ENDIF.
          ENDIF.
        ENDIF.

        IF <pto>-code1 IS INITIAL.
          CLEAR <pto>-code_grp1.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR: return2, i_return.

* APC11022013 Algunas fórmulas presentan errores de evalución y no sabemos por qué!!!
    LOOP AT i_requerimientos ASSIGNING <req> WHERE formula <> ''.
      LOOP AT i_puntos_insp ASSIGNING <pto> WHERE     inspoper   = <req>-inspoper
                                                  AND inspchar   = <req>-inspchar
                                                  AND smpl_inval = 'X'.
        CLEAR: <pto>-smpl_attr, <pto>-smpl_inval, <pto>-evaluated, <pto>-evaluation.
      ENDLOOP.
    ENDLOOP.

    IF no_muestra_fisica = 'X'.
      CLEAR l_punto-phys_smpl.
    ENDIF.

    CALL FUNCTION 'BAPI_INSPOPER_RECORDRESULTS'
      EXPORTING
        insplot                    = lote_insp
        inspoper                   = n_oper
        insppointdata              = l_punto
*     HANDHELD_APPLICATION       = ' '
      IMPORTING
        return                     = return2
      TABLES
        char_results               = i_resultados
        sample_results             = i_puntos_insp
*     SINGLE_RESULTS             =
        returntable                = i_return.

************************************************************************

    IF return2-type = 'E'.
      READ TABLE i_return INTO l_return2 WITH KEY type = 'E'.
      IF sy-subrc = 0.
* Por si es un error al evaluar fórmulas, volvemos a intentar grabar sin evaluar esos valores
        IF    ( l_return2-id = 'QE' AND (    l_return2-number = '388'    " Al evaluar la fórmula han surgido errores
                                          OR l_return2-number = '362'    " La media no está definida. Imposible la valoración
                                          OR l_return2-number = '456' ) ) " El tamaño de muestreo inspeccionado no concuerda con el planificado
           OR ( l_return2-id = 'QI' AND ( l_return2-number = '127' ) ). " No es válida la muestra

          IF l_return2-id = 'QE' AND l_return2-number = '388'.
            l_err_evaluar_formula_ignorar = 'X'.
          ENDIF.

          READ TABLE i_return INTO l_return2 WITH KEY type = 'E'.
          IF sy-subrc = 0.
            l_mensaje_formula = l_return2-message.
          ELSE.
            l_mensaje_formula = return2-message.
          ENDIF.

          LOOP AT i_requerimientos ASSIGNING <req> WHERE formula <> ''.
            LOOP AT i_resultados ASSIGNING <res> WHERE     inspoper = <req>-inspoper
                                                       AND inspchar = <req>-inspchar.
              CLEAR: <res>-closed, <res>-evaluated.
            ENDLOOP.
            l_error_formula = 'X'.
          ENDLOOP.

          LOOP AT i_requerimientos ASSIGNING <req> WHERE formula <> ''.
            LOOP AT i_puntos_insp ASSIGNING <pto> WHERE     inspoper = <req>-inspoper
                                                        AND inspchar = <req>-inspchar.
              CLEAR: <pto>-closed, <pto>-evaluated.
            ENDLOOP.
            l_error_formula_op = 'X'.
          ENDLOOP.

          CLEAR: return2, i_return.

          CALL FUNCTION 'BAPI_INSPOPER_RECORDRESULTS'
            EXPORTING
              insplot                    = lote_insp
              inspoper                   = n_oper
              insppointdata              = l_punto
*     HANDHELD_APPLICATION       = ' '
            IMPORTING
              return                     = return2
            TABLES
              char_results               = i_resultados
              sample_results             = i_puntos_insp
*     SINGLE_RESULTS             =
              returntable                = i_return.
        ENDIF.
      ENDIF.
    ENDIF.

    IF return2-type = 'E'.
      READ TABLE i_return INTO l_return2 WITH KEY type = 'E'.
      IF sy-subrc = 0.
        mensaje = l_return2-message.
      ELSE.
        mensaje = return2-message.
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      IF dequeue_all = 'X'.
        CALL FUNCTION 'DEQUEUE_ALL'.
      ENDIF.

      IF l_err_evaluar_formula_ignorar IS INITIAL.
        IF l_error_formula_op = 'X' OR l_error_formula = 'X'.
          CONCATENATE 'Error al evaluar fórmula.'(EEF) l_mensaje_formula INTO mensaje SEPARATED BY space.

* Si hay formulas, mostramos al operario el punto de inspección para que pulse en calcular
          COMMIT WORK AND WAIT.
          WAIT UP TO 1 SECONDS.
          zcl_ap_lotes_insp=>ver_resultados( lote_insp = lote_insp
                                             n_oper = n_oper
                                             datos_punto_insp = datos_punto_insp
                                             editar = 'X' ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD tiene_decision_empleo.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: l_qave                 TYPE qave,
          i_objnr                TYPE jsto-objnr,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_warning_occurred     TYPE c LENGTH 1,
          l_activity_not_allowed TYPE c LENGTH 1,
          l_status_not_allowed   TYPE c LENGTH 1.

    SELECT SINGLE prueflos FROM qave
      INTO l_qave-prueflos
     WHERE prueflos = lote_insp.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CONCATENATE 'QL' lote_insp INTO i_objnr.
    CALL FUNCTION 'STATUS_CHANGE_FOR_ACTIVITY'
      EXPORTING
        check_only           = 'X'
        objnr                = i_objnr
        vrgng                = 'QM20'
      IMPORTING
        warning_occurred     = l_warning_occurred
        activity_not_allowed = l_activity_not_allowed
        status_not_allowed   = l_status_not_allowed
      EXCEPTIONS
        activity_not_allowed = 01
        warning_occured      = 02
        status_not_allowed   = 01.
*
    IF    sy-subrc                = 1
       OR l_activity_not_allowed <> space
       OR l_status_not_allowed   <> space.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD validar_resultado.
    DATA: l_tabla       TYPE tabname VALUE 'QAMV',
          l_qabwr       TYPE qabwr,
          l_valor_float TYPE qabwr-mittelwert,
          i_qaspr       TYPE TABLE OF qaspr,
          l_evaluation  TYPE bapi2045d2-evaluation.

    IF mstr_char IS INITIAL.
      SELECT SINGLE * FROM (l_tabla)
      INTO CORRESPONDING FIELDS OF l_qabwr
      WHERE prueflos = res-insplot
        AND vorglfnr = '00000001'
        AND merknr   = res-inspchar.
    ELSE.
      SELECT * FROM (l_tabla)
        UP TO 1 ROWS
      INTO CORRESPONDING FIELDS OF l_qabwr
      WHERE prueflos  = res-insplot
        AND merknr    = res-inspchar
        AND verwmerkm = mstr_char
       ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.

    IF sy-subrc <> 0.
      CONCATENATE 'Característica de inspección'(CDI) res-inspchar 'incorrecta'(INC) INTO mensaje SEPARATED BY space.
    ENDIF.

    IF katalgart IS INITIAL.
      l_valor_float = zcl_ap_string=>string2float( res-mean_value ).
      IF l_valor_float = zcl_ap_string=>c_error_float.
        mensaje = 'Nº incorrecto'(NIN).
      ELSE.
        CLEAR i_qaspr.
        l_qabwr-mittelwert = l_valor_float.
        l_qabwr-mittelwni  = 'X'.
        CALL FUNCTION 'QEBR_MEAN_VALUE_TOLERANCE_INSP'
          EXPORTING
            dynprocall   = space
            qabwr        = l_qabwr
*        IMPORTING
*         DBEWERTG     =
*         FEHLKLAS     =
*         mbewertg     = l_mbewertg
*         FECODSELE    =
*         FEGRPSELE    =
*         E_QABWR_EX   =
          TABLES
            qasptab      = i_qaspr
          EXCEPTIONS
            other_error  = 1
            system_error = 2
            user_error   = 3
            OTHERS       = 4.
        IF sy-subrc <> 0.
          mensaje = 'Valor incorrecto'(VIN).
        ENDIF.
      ENDIF.
    ELSE.
      l_evaluation = get_evaluacion( res       = res
                                     katalgart = katalgart
                                     werks     = werks ).
      IF l_evaluation = 'F'.
        mensaje = 'Valor no válido'(VNV).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD ver_lote_insp_st.
    SET PARAMETER ID 'QLS' FIELD lote_insp.

    IF editar IS INITIAL.
      CALL TRANSACTION 'QA03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
    ELSE.
      CALL TRANSACTION 'QA02' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
    ENDIF.
  ENDMETHOD.
  METHOD ver_resultados.
    DATA: l_qals TYPE qals, "<listado>-slwbez
          l_hora TYPE c LENGTH 8.
    DATA o_bi      TYPE REF TO zcl_ap_batch_input.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mensaje TYPE bapireturn1-message. "#EC *

    SELECT SINGLE slwbez werk FROM qals
      INTO CORRESPONDING FIELDS OF l_qals
     WHERE prueflos = lote_insp.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SET PARAMETER ID 'QLS' FIELD lote_insp.
    SET PARAMETER ID 'QVO' FIELD n_oper.
    SET PARAMETER ID 'QMO' FIELD '1'.
    SET PARAMETER ID 'QBK' FIELD l_qals-slwbez.

    IF l_qals-slwbez IS INITIAL.
      IF editar = 'X'.
        CALL TRANSACTION 'QE01' AND SKIP FIRST SCREEN.     "#EC CI_CALLTA
      ELSE.
        CALL TRANSACTION 'QE03' AND SKIP FIRST SCREEN.     "#EC CI_CALLTA
      ENDIF.
    ELSE.
      IF datos_punto_insp IS INITIAL.
        IF editar = 'X'.
          CALL TRANSACTION 'QE12' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
        ELSE.
          CALL TRANSACTION 'QE13' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
        ENDIF.
      ELSE.
        o_bi = NEW #( ).

        o_bi->inicio( ).

* Entrada lote insp. entrada resultados
        o_bi->dynpro( program = 'SAPMQEEA' dynpro = '0100' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
        o_bi->campos( campo = 'QALS-PRUEFLOS' valor = lote_insp ).
        o_bi->campos( campo = 'QAQEE-VORNR' valor = n_oper  ).
        o_bi->campos( campo = 'QAQEE-PRPLATZWRK' valor = l_qals-werk ). " Centro

        IF NOT modus IS INITIAL.
          o_bi->campos( campo = 'QAQEE-MODUS' valor = modus  ).
        ENDIF.

        IF NOT datos_punto_insp-userc1 IS INITIAL.
          o_bi->campos( campo = 'QAPPD-USERC1' valor = datos_punto_insp-userc1 ). " Campo de usuario para diez caracteres
        ENDIF.
        IF NOT datos_punto_insp-userc2 IS INITIAL.
          o_bi->campos( campo = 'QAPPD-USERC2' valor = datos_punto_insp-userc2 ). " Campo de usuario para diez caracteres
        ENDIF.
        IF NOT datos_punto_insp-usern1 IS INITIAL.
          o_bi->campos( campo = 'QAPPD-USERN1' valor = datos_punto_insp-usern1 ). " Campo usuario para diez caracteres numéricos
        ENDIF.
        o_bi->campos( campo = 'QAPPD-USERD1' valor = datos_punto_insp-userd1 ). " Campo de usuario para fecha
        WRITE datos_punto_insp-usert1 TO l_hora.
        o_bi->campos( campo = 'QAPPD-USERT1' valor = l_hora ). " Campo de usuario para hora

        IF editar = 'X'.
          l_mensaje = o_bi->llamar_transaccion( tcode = 'QE12' modo = 'E' ). "#EC *
        ELSE.
          l_mensaje = o_bi->llamar_transaccion( tcode = 'QE13' modo = 'E' ). "#EC *
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar.
    SET PARAMETER ID 'QLS' FIELD prueflos.
    CALL TRANSACTION 'QA03' AND SKIP FIRST SCREEN. "#EC CI_CALLTA
  ENDMETHOD.
