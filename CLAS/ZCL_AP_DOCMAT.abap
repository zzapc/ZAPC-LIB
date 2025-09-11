CLASS zcl_ap_docmat DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_matnr TYPE matnr18.

    DATA i_items  TYPE tab_bapi_goodsmvt_item.
    DATA i_return TYPE bapiret2_t.
    DATA return   TYPE bapiret2.
    DATA mensaje  TYPE bapi_msg.
    DATA lights   TYPE lights.
    DATA xblnr    TYPE mkpf-xblnr.
    DATA bktxt    TYPE mkpf-bktxt.
    DATA budat    TYPE mkpf-budat.
    DATA mblnr    TYPE mkpf-mblnr.
    DATA mjahr    TYPE mkpf-mjahr.

    CLASS-METHODS visualizar
      IMPORTING mjahr TYPE mseg-mjahr OPTIONAL
                mblnr TYPE mseg-mblnr OPTIONAL
                zeile TYPE mseg-zeile DEFAULT '0001'
      PREFERRED PARAMETER mblnr.

    METHODS set_cabecera
      IMPORTING budat TYPE budat    DEFAULT sy-datum
                bldat TYPE bldat    DEFAULT sy-datum
                xblnr TYPE any      DEFAULT ''
                bktxt TYPE any      DEFAULT ''
                tcode TYPE sy-tcode DEFAULT 'MB11'.

    METHODS set_posicion
      IMPORTING bwart       TYPE bwart
                matnr       TYPE any
                menge       TYPE any
                meins       TYPE meins
                werks       TYPE werks_d    OPTIONAL
                lgort       TYPE any        OPTIONAL
                sakto       TYPE sakto      OPTIONAL
                kostl       TYPE kostl      OPTIONAL
                sgtxt       TYPE any        OPTIONAL
                sobkz       TYPE sobkz      OPTIONAL
                val_kdauf   TYPE kdauf      OPTIONAL
                val_kdpos   TYPE kdpos      OPTIONAL
                charg       TYPE charg_d    OPTIONAL
                aufnr       TYPE aufnr      OPTIONAL
                rsnum       TYPE rsnum      OPTIONAL
                rspos       TYPE rspos      OPTIONAL
                bsskz       TYPE lvs_bsskz  OPTIONAL
                grund       TYPE mb_grbew   OPTIONAL
                ummat       TYPE ummat      OPTIONAL
                umcha       TYPE umcha      OPTIONAL
                ebeln       TYPE ebeln      OPTIONAL
                ebelp       TYPE ebelp      OPTIONAL
                kzbew       TYPE kzbew      OPTIONAL
                umwrk       TYPE umwrk      OPTIONAL
                umlgo       TYPE any        OPTIONAL
                insmk       TYPE mb_insmk   OPTIONAL
                lvs_letyp   TYPE lvs_letyp  OPTIONAL
                lvs_anzle   TYPE lvs_anzle  OPTIONAL
                lvs_lemen   TYPE lvs_lemen  OPTIONAL
                expirydate  TYPE vfdat      OPTIONAL
                prod_date   TYPE hsdat      OPTIONAL
                lifnr       TYPE lifnr      OPTIONAL
                elikz       TYPE elikz      OPTIONAL
                kdauf       TYPE kdauf      OPTIONAL
                kdpos       TYPE kdpos      OPTIONAL
                vfdat       TYPE vfdat      OPTIONAL
                posid       TYPE ps_posid   OPTIONAL
                hsdat       TYPE hsdat      OPTIONAL
                lfbnr       TYPE mseg-lfbnr OPTIONAL
                lfpos       TYPE mseg-lfpos OPTIONAL
                lfbja       TYPE mseg-lfbja OPTIONAL
                vbeln       TYPE vbeln_vl   OPTIONAL
                posnr       TYPE posnr_vl   OPTIONAL
                bwtar       TYPE mseg-bwtar OPTIONAL
                opc         TYPE string     OPTIONAL
                lgtyp       TYPE lgtyp      OPTIONAL
                paobjnr     TYPE rkeobjnr   OPTIONAL
                ablad       TYPE mseg-ablad OPTIONAL
                dmbtr       TYPE dmbtr      OPTIONAL
      RETURNING VALUE(item) TYPE bapi2017_gm_item_create.

    METHODS crea_doc
      IMPORTING espera_a_grabado TYPE abap_bool DEFAULT ''
                dequeue_all      TYPE abap_bool DEFAULT ''
                !commit          TYPE abap_bool DEFAULT 'X'
      EXPORTING mblnr            TYPE mblnr
                mjahr            TYPE mjahr.

    METHODS get_error
      RETURNING VALUE(mensaje) TYPE bapiret2-message.

    METHODS anular_doc
      IMPORTING espera_a_grabado TYPE abap_bool      DEFAULT ''
                mblnr            TYPE mblnr
                mjahr            TYPE mjahr
                budat            TYPE budat          OPTIONAL
                i_posiciones     TYPE wb2_mblpo_stab OPTIONAL
                !commit          TYPE abap_bool      DEFAULT 'X'
                popup_fecha      TYPE abap_bool      DEFAULT ''
                opciones         TYPE any            DEFAULT ''
      EXPORTING smbln            TYPE mblnr
                sjahr            TYPE mjahr
                salida           TYPE any.

    CLASS-METHODS esta_anulado
      IMPORTING mblnr          TYPE mblnr
                mjahr          TYPE mjahr
                zeile          TYPE mseg-zeile OPTIONAL
      RETURNING VALUE(anulado) TYPE abap_bool.

    CLASS-METHODS get_valor_movs_mm
      IMPORTING matnr           TYPE matnr
                bwart           TYPE bwart              OPTIONAL
                bwart2          TYPE bwart              OPTIONAL
                r_bwart         TYPE zt_rangebwart      OPTIONAL
                r_budat         TYPE fagl_range_t_budat OPTIONAL
                werks           TYPE werks_d            OPTIONAL
                shkzg           TYPE shkzg              OPTIONAL
                charg           TYPE charg_d            OPTIONAL
      RETURNING VALUE(cantidad) TYPE mengev.

    CLASS-METHODS ctd_anulada
      IMPORTING mblnr        TYPE mblnr
                mjahr        TYPE mjahr
                zeile        TYPE mseg-zeile OPTIONAL
      RETURNING VALUE(menge) TYPE mengev.

    CLASS-METHODS get_periodo_abierto
      IMPORTING fecha                TYPE budat DEFAULT sy-datum
                bukrs                TYPE bukrs DEFAULT zcl_c=>bukrs
      RETURNING VALUE(fecha_abierta) TYPE budat.

    CLASS-METHODS mi10_simple
      IMPORTING fecha      TYPE dats
                werks      TYPE werks_d
                lgort      TYPE lgort_d
                cantidad   TYPE mengv13  OPTIONAL
                meins      TYPE meins    DEFAULT ''
                diferencia TYPE mengv13  OPTIONAL
                modo       TYPE bdc_mode DEFAULT 'N'
                matnr      TYPE matnr
                charg      TYPE charg_d  DEFAULT ''
      EXPORTING !message   TYPE bapi_msg
                mkpf       TYPE mkpf.

    CLASS-METHODS mov_simple
      IMPORTING budat            TYPE budat      DEFAULT sy-datum
                bldat            TYPE bldat      DEFAULT sy-datum
                xblnr            TYPE any        DEFAULT ''
                bktxt            TYPE any        DEFAULT ''
                tcode            TYPE sy-tcode   DEFAULT 'MB11'
                bwart            TYPE bwart
                matnr            TYPE matnr
                menge            TYPE any
                meins            TYPE meins
                werks            TYPE werks_d    OPTIONAL
                lgort            TYPE any        OPTIONAL
                sakto            TYPE sakto      OPTIONAL
                kostl            TYPE kostl      OPTIONAL
                sgtxt            TYPE sgtxt      OPTIONAL
                sobkz            TYPE sobkz      OPTIONAL
                val_kdauf        TYPE kdauf      OPTIONAL
                val_kdpos        TYPE kdpos      OPTIONAL
                charg            TYPE charg_d    OPTIONAL
                aufnr            TYPE aufnr      OPTIONAL
                rsnum            TYPE rsnum      OPTIONAL
                rspos            TYPE rspos      OPTIONAL
                bsskz            TYPE lvs_bsskz  OPTIONAL
                grund            TYPE mb_grbew   OPTIONAL
                ummat            TYPE ummat      OPTIONAL
                umcha            TYPE umcha      OPTIONAL
                ebeln            TYPE ebeln      OPTIONAL
                ebelp            TYPE ebelp      OPTIONAL
                kzbew            TYPE kzbew      OPTIONAL
                espera_a_grabado TYPE abap_bool  DEFAULT 'X'
                dequeue_all      TYPE abap_bool  DEFAULT 'X'
                !commit          TYPE abap_bool  DEFAULT 'X'
                umwrk            TYPE umwrk      OPTIONAL
                umlgo            TYPE any        OPTIONAL
                insmk            TYPE mb_insmk   OPTIONAL
                kdauf            TYPE kdauf      OPTIONAL
                kdpos            TYPE kdpos      OPTIONAL
                vfdat            TYPE vfdat      OPTIONAL
                posid            TYPE ps_posid   OPTIONAL
                lfbnr            TYPE lfbnr      OPTIONAL
                lfpos            TYPE lfpos      OPTIONAL
                hsdat            TYPE hsdat      OPTIONAL
                vbeln            TYPE vbeln_vl   OPTIONAL
                posnr            TYPE posnr      OPTIONAL
                bwtar            TYPE mseg-bwtar OPTIONAL
                lifnr            type lifnr      OPTIONAL
                opc              TYPE string     OPTIONAL
                lgtyp            TYPE lgtyp      OPTIONAL
                paobjnr          TYPE rkeobjnr   OPTIONAL
      EXPORTING mblnr            TYPE mblnr
                mjahr            TYPE mjahr
                mensaje          TYPE bapi_msg.

    CLASS-METHODS es_periodo_abierto
      IMPORTING bukrs     TYPE bukrs
                fecha     TYPE budat
      EXPORTING !message  TYPE bapi_msg
      RETURNING VALUE(si) TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA header  TYPE bapi2017_gm_head_01.
    DATA code    TYPE bapi2017_gm_code.
    DATA headret TYPE bapi2017_gm_head_ret.
endclass. "ZCL_AP_DOCMAT definition
class ZCL_AP_DOCMAT implementation.
  METHOD anular_doc.
    " TODO: parameter OPCIONES is never used (ABAP cleaner)

    DATA: l_budat      TYPE budat,
          l_matdocitem TYPE TABLE OF bapi2017_gm_item_04.

    CLEAR: i_return,
           smbln,
           sjahr,
           headret,
           salida.

    IF popup_fecha = 'X'.
      l_budat = zcl_ap_fechas=>popup_fecha( titulo = 'Fecha anulación' ) ##NO_TEXT.
      IF l_budat IS INITIAL.
        mensaje = 'Debe indicar fecha de anulación' ##NO_TEXT.
        RETURN.
      ENDIF.
    ELSEIF budat IS INITIAL.
      SELECT SINGLE budat FROM mkpf
        INTO l_budat
       WHERE mblnr = mblnr
         AND mjahr = mjahr.
    ELSE.
      l_budat = budat.
    ENDIF.

    l_matdocitem = i_posiciones.
    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING materialdocument    = mblnr
                matdocumentyear     = mjahr
                goodsmvt_pstng_date = l_budat
*                GOODSMVT_PR_UNAME   =
      IMPORTING goodsmvt_headret    = headret
      TABLES    return              = i_return
                goodsmvt_matdocitem = l_matdocitem.

    IF NOT headret IS INITIAL.
      IF commit = 'X'.
        zcl_ap_dev=>commit( ).
      ENDIF.

      smbln = headret-mat_doc.
      sjahr = headret-doc_year.

      IF espera_a_grabado = 'X'.
        DO 5 TIMES.
          SELECT SINGLE mblnr FROM mkpf
            INTO @data(l_mblnr)
           WHERE mblnr = @smbln
             AND mjahr = @sjahr.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.
      ENDIF.
    ELSE.
      LOOP AT i_return INTO return WHERE type = 'E'.
        mensaje = return-message.
      ENDLOOP.

      IF ( mensaje CS 'period' OR mensaje CS 'períod' ) AND popup_fecha = 'Y'.
        anular_doc( EXPORTING espera_a_grabado = espera_a_grabado
                              mblnr            = mblnr
                              mjahr            = mjahr
                              i_posiciones     = i_posiciones
                              commit           = commit
                              popup_fecha      = 'X'
                    IMPORTING smbln            = smbln
                              sjahr            = sjahr ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD crea_doc.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mkpf TYPE mkpf.

    CLEAR: i_return,
           mblnr,
           mjahr.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING goodsmvt_header  = header
                goodsmvt_code    = code
                testrun          = ' '
      IMPORTING goodsmvt_headret = headret
                materialdocument = mblnr
                matdocumentyear  = mjahr
      TABLES    goodsmvt_item    = i_items
                return           = i_return.

    IF NOT mblnr IS INITIAL.
      IF commit = 'X'.
        zcl_ap_dev=>commit( ).

        lights = zcl_ap_alv=>c_sem_verde.
        IF espera_a_grabado = 'X'.
          DO 5 TIMES.
            SELECT SINGLE mblnr FROM mkpf
              INTO l_mkpf-mblnr
             WHERE mblnr = mblnr
               AND mjahr = mjahr.
            IF sy-subrc = 0.
              EXIT.
            ELSE.
              WAIT UP TO 1 SECONDS.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDIF.
    ELSE.
      lights = zcl_ap_alv=>c_sem_rojo.
      IF dequeue_all = 'X'.
        CALL FUNCTION 'DEQUEUE_ALL'.
      ENDIF.
    ENDIF.
    LOOP AT i_return INTO return.
      mensaje = return-message.
    ENDLOOP.
  ENDMETHOD.
  METHOD ctd_anulada.
    DATA: i_mseg TYPE TABLE OF mseg,
          l_mseg TYPE mseg.

    CLEAR menge.

    IF zeile IS INITIAL.
      SELECT menge shkzg FROM mseg
        INTO CORRESPONDING FIELDS OF TABLE i_mseg
       WHERE sjahr = mjahr
         AND smbln = mblnr.
    ELSE.
      SELECT menge shkzg FROM mseg
        INTO CORRESPONDING FIELDS OF TABLE i_mseg
       WHERE sjahr = mjahr
         AND smbln = mblnr
         AND smblp = zeile.
    ENDIF.

    LOOP AT i_mseg INTO l_mseg.
      IF l_mseg-shkzg = 'S'.
        menge = menge + l_mseg-menge.
      ELSE.
        menge = menge - l_mseg-menge.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD es_periodo_abierto.
    CLEAR: message,
           si.
    CALL FUNCTION 'MR_PERIOD_DETERMINE'
      EXPORTING  i_bukrs                = bukrs
                 i_budat                = fecha
*                 I_KZRFB                =
*     IMPORTING
*                 E_MONAT                =
*                 E_GJAHR                =
*                 E_XRUEJ                =
*                 E_XRUEM                =
*                 E_LFGJA                =
*                 E_LFMON                =
      EXCEPTIONS invalid_posting_period = 1
                 marv_no_entry          = 2
                 OTHERS                 = 3.

    IF sy-subrc = 0.
      si = 'X'.
    ELSE.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ENDIF.
  ENDMETHOD.
  METHOD esta_anulado.
    DATA l_mseg TYPE mseg.

    CLEAR anulado.
    IF zeile IS INITIAL.
      SELECT mblnr FROM mseg
        INTO l_mseg-mblnr
        UP TO 1 ROWS
       WHERE sjahr = mjahr
         AND smbln = mblnr
       ORDER BY PRIMARY KEY.
      ENDSELECT.
    ELSE.
      SELECT mblnr FROM mseg
        INTO l_mseg-mblnr
        UP TO 1 ROWS
       WHERE sjahr = mjahr
         AND smbln = mblnr
         AND smblp = zeile
       ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.
    IF NOT l_mseg IS INITIAL.
      anulado = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD get_error.
    CLEAR mensaje.
    READ TABLE i_return INTO return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      mensaje = return-message.
    ENDIF.
  ENDMETHOD.
  METHOD get_periodo_abierto.
    DATA l_marv TYPE marv.

    fecha_abierta = fecha.

* Hay que verificar que el periodo actual esté abierto. En caso contra-
* rio, se contabiliza con fecha 01 del periodo abierto
    SELECT SINGLE * FROM marv
      INTO l_marv
      WHERE bukrs = bukrs.

    IF l_marv-lfgja = fecha(4).
      IF l_marv-lfmon <> fecha+4(2).
        fecha_abierta+4(2) = l_marv-lfmon.
        fecha_abierta+6(2) = '01'.
      ENDIF.
    ELSE.
      fecha_abierta(4) = l_marv-lfgja.
      fecha_abierta+4(2) = l_marv-lfmon.
      fecha_abierta+6(2) = '01'.
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_movs_mm.
    TYPES: BEGIN OF t_movs,
             shkzg TYPE mseg-shkzg,
             menge TYPE mseg-menge,
           END OF t_movs.

    FIELD-SYMBOLS <mov> TYPE t_movs.

    DATA: ri_matnr TYPE RANGE OF matnr,
          ri_charg TYPE RANGE OF charg_d,
          ri_werks TYPE RANGE OF werks_d,
          ri_bwart TYPE RANGE OF bwart,
          ri_shkzg TYPE RANGE OF shkzg,
          lr_matnr LIKE LINE OF ri_matnr,
          lr_charg LIKE LINE OF ri_charg,
          lr_werks LIKE LINE OF ri_werks,
          lr_bwart LIKE LINE OF ri_bwart,
          lr_shkzg LIKE LINE OF ri_shkzg,
          i_movs   TYPE TABLE OF t_movs.

    IF NOT matnr IS INITIAL.
      CLEAR lr_matnr.
      lr_matnr-option = 'EQ'.
      lr_matnr-sign   = 'I'.
      lr_matnr-low    = matnr.
      APPEND lr_matnr TO ri_matnr.
    ENDIF.

    IF NOT charg IS INITIAL.
      CLEAR lr_charg.
      lr_charg-option = 'EQ'.
      lr_charg-sign   = 'I'.
      lr_charg-low    = charg.
      APPEND lr_charg TO ri_charg.
    ENDIF.

    IF NOT werks IS INITIAL.
      CLEAR lr_werks.
      lr_werks-option = 'EQ'.
      lr_werks-sign   = 'I'.
      lr_werks-low    = werks.
      APPEND lr_werks TO ri_werks.
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

    IF NOT shkzg IS INITIAL.
      CLEAR lr_shkzg.
      lr_shkzg-option = 'EQ'.
      lr_shkzg-sign   = 'I'.
      lr_shkzg-low    = shkzg.
      APPEND lr_shkzg TO ri_shkzg.
    ENDIF.

    CLEAR cantidad.

* Busco en los pendientes de procesar
    SELECT shkzg SUM( menge )
      INTO TABLE i_movs
      FROM mkpf JOIN mseg ON  mkpf~mblnr = mseg~mblnr
                          AND mkpf~mjahr = mseg~mjahr
     WHERE budat IN r_budat
       AND werks IN ri_werks
       AND matnr IN ri_matnr
       AND charg IN ri_charg
       AND bwart IN ri_bwart
       AND shkzg IN ri_shkzg
     GROUP BY shkzg.

    LOOP AT i_movs ASSIGNING <mov>.
      IF <mov>-shkzg = 'S'.
        cantidad = cantidad + <mov>-menge.
      ELSE.
        cantidad = cantidad - <mov>-menge.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD mi10_simple.
    DATA: o_bi    TYPE REF TO zcl_ap_batch_input,
          l_meins TYPE meins,
          l_ctd   TYPE mengv13,
          l_fecha TYPE d,
          l_hora  TYPE sy-uzeit.

    CLEAR: message,
           mkpf.

    o_bi = NEW #( ).

    IF meins IS INITIAL.
      l_meins = zcl_ap_material=>get_unidad_base( matnr ).
    ELSE.
      l_meins = meins.
    ENDIF.

    IF NOT diferencia IS INITIAL.
      IF charg IS INITIAL.
        SELECT SUM( labst ) FROM mard
          INTO l_ctd
         WHERE matnr = matnr
           AND werks = werks
           AND lgort = lgort.
      ELSE.
        SELECT SUM( clabs ) FROM mchb
          INTO l_ctd
         WHERE matnr = matnr
           AND charg = charg
           AND werks = werks
           AND lgort = lgort.
      ENDIF.
      l_ctd = l_ctd + diferencia.
    ELSE.
      l_ctd = cantidad.
    ENDIF.
    o_bi->inicio( ).

* Pantalla inicial: crear documento de invent.
    o_bi->dynpro( program = 'SAPMM07I' dynpro = '0700' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00' ).
    o_bi->campos( campo = 'RM07I-ZLDAT' valor = fecha ). " Fecha del último recuento
    o_bi->campos( campo = 'RM07I-BLDAT' valor = fecha ). " Fecha de documento en documento
    o_bi->campos( campo = 'IKPF-WERKS' valor = werks ). " Centro
    o_bi->campos( campo = 'IKPF-LGORT' valor = lgort ). " Almacén

* Posiciones: crear recuento de inventario
    o_bi->dynpro( program = 'SAPMM07I' dynpro = '0731' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).
    o_bi->campos( campo = 'ISEG-MATNR(01)' valor = matnr ). " Número de material
    o_bi->campos( campo = 'ISEG-CHARG(01)' valor = charg ).  " Número de lote
    o_bi->campos( campo = 'ISEG-ERFMG(01)' valor = l_ctd ). " Ctd.en unidad de medida de entrada (inventario)
    o_bi->campos( campo = 'ISEG-ERFME(01)' valor = l_meins ). " Unidad medida de entrada (inventario)

    l_fecha = sy-datum.
    l_hora  = sy-uzeit.
    message = o_bi->llamar_transaccion( tcode = 'MI10' modo = modo ).
    SELECT mblnr mjahr FROM mkpf                          "#EC CI_NOFIELD
      INTO CORRESPONDING FIELDS OF mkpf
      UP TO 1 ROWS
     WHERE tcode  = 'MI10'
       AND cpudt  = l_fecha
       AND cputm >= l_hora
       AND usnam  = sy-uname
     ORDER BY PRIMARY KEY.
    ENDSELECT.

    IF sy-subrc = 0.
      CLEAR message.
    ENDIF.
  ENDMETHOD.
  METHOD mov_simple.
    DATA o_mov TYPE REF TO zcl_ap_docmat.

    CLEAR: mblnr,
           mjahr,
           mensaje.

    o_mov = NEW #( ).
    o_mov->set_cabecera( budat = budat
                         bldat = bldat
                         xblnr = xblnr
                         bktxt = bktxt
                         tcode = tcode ).

    o_mov->set_posicion( bwart     = bwart
                         matnr     = matnr
                         menge     = menge
                         meins     = meins
                         werks     = werks
                         lgort     = lgort
                         sakto     = sakto
                         kostl     = kostl
                         sgtxt     = sgtxt
                         sobkz     = sobkz
                         val_kdauf = val_kdauf
                         val_kdpos = val_kdpos
                         charg     = charg
                         aufnr     = aufnr
                         rsnum     = rsnum
                         rspos     = rspos
                         bsskz     = bsskz
                         grund     = grund
                         ummat     = ummat
                         umcha     = umcha
                         ebeln     = ebeln
                         ebelp     = ebelp
                         kzbew     = kzbew
                         umwrk     = umwrk
                         umlgo     = umlgo
                         insmk     = insmk
                         kdauf     = kdauf
                         kdpos     = kdpos
                         vfdat     = vfdat
                         hsdat     = hsdat
                         posid     = posid
                         lfbnr     = lfbnr
                         lfpos     = lfpos
                         vbeln     = vbeln
                         posnr     = posnr
                         bwtar     = bwtar
                         lifnr     = lifnr
                         lgtyp     = lgtyp
                         paobjnr   = paobjnr
                         opc       = opc ).

    o_mov->crea_doc( EXPORTING espera_a_grabado = espera_a_grabado
                               dequeue_all      = dequeue_all
                               commit           = commit
                     IMPORTING mblnr            = mblnr
                               mjahr            = mjahr ).

    IF mblnr IS INITIAL.
      mensaje = o_mov->mensaje.
    ENDIF.
  ENDMETHOD.
  METHOD set_cabecera.
    CLEAR header.
    header-pstng_date = budat.
    header-doc_date   = bldat.
    header-pr_uname   = sy-uname.
    header-header_txt = bktxt.
    header-ref_doc_no = xblnr.

    DATA(l_tcode) = tcode.
    IF strlen( tcode ) = 3.
      SELECT tcode
        FROM t158b
        WHERE bwart    = @tcode
          AND tcode LIKE 'MB%'
        ORDER BY PRIMARY KEY
        INTO @l_tcode
        UP TO 1 ROWS.
      ENDSELECT.
    ENDIF.
** Tipo de movimiento
    SELECT gmcode FROM t158g
      INTO code
      UP TO 1 ROWS
     WHERE tcode = l_tcode
     ORDER BY PRIMARY KEY.
    ENDSELECT.

    CLEAR: i_items,
           i_return.
  ENDMETHOD.
  METHOD set_posicion.
    CLEAR item.
    item-move_type            = bwart.
    item-material             = matnr.
    item-plant                = werks.
    item-stge_loc             = lgort.
    item-entry_qnt            = menge.
    item-entry_uom            = meins.
    item-gl_account           = sakto.
    item-costcenter           = kostl.
    item-item_text            = sgtxt.
    item-spec_stock           = sobkz.
    item-sales_ord            = kdauf.
    item-s_ord_item           = kdpos.
    item-val_sales_ord        = val_kdauf.
    item-val_s_ord_item       = val_kdpos.
    item-batch                = charg.
    item-expirydate           = expirydate.
    item-prod_date            = prod_date.
    item-orderid              = aufnr.
    item-reserv_no            = rsnum.
    item-res_item             = rspos.
    item-spec_mvmt            = bsskz.
    item-move_reas            = grund.
    item-move_mat             = ummat.
    item-move_batch           = umcha.
    item-po_number            = ebeln.
    item-po_item              = ebelp.
    item-mvt_ind              = kzbew.
    item-move_plant           = umwrk.
    item-move_stloc           = umlgo.
    item-stck_type            = insmk.
    item-unittype_1           = lvs_letyp.
    item-su_pl_stck_1         = lvs_anzle.
    item-st_un_qtyy_1         = lvs_lemen.
    item-vendor               = lifnr.
    item-no_more_gr           = elikz.
    item-expirydate           = vfdat.
    item-prod_date            = hsdat.
    item-wbs_elem             = posid.
    item-ref_doc              = lfbnr.
    item-ref_doc_it           = lfpos.
    item-ref_doc_yr           = lfbja.
    item-deliv_numb_to_search = vbeln.
    item-deliv_item_to_search = posnr.
    item-deliv_item_to_search = lgtyp.
*  item-deliv_numb = vbeln.
*  item-deliv_item = posnr.
    item-val_type             = bwtar.
    item-stge_type            = lgtyp.
    item-profit_segm_no       = paobjnr.
    item-unload_pt            = ablad.
    item-amount_lc            = dmbtr.

    IF opc CS 'QUANTITY'.
      item-quantity = menge.
      item-base_uom = meins.
    ENDIF.

    APPEND item TO i_items.
  ENDMETHOD.
  METHOD visualizar.
    DATA l_mseg TYPE mseg.

    IF mjahr IS INITIAL.
      SELECT mblnr mjahr zeile FROM mseg
        INTO CORRESPONDING FIELDS OF l_mseg
        UP TO 1 ROWS
       WHERE mblnr = mblnr
         AND zeile = zeile
        ORDER BY mjahr DESCENDING.
      ENDSELECT.
    ELSE.
      SELECT SINGLE mblnr mjahr zeile FROM mseg
        INTO CORRESPONDING FIELDS OF l_mseg
       WHERE mjahr = mjahr
         AND mblnr = mblnr
         AND zeile = zeile.
    ENDIF.

    IF l_mseg IS INITIAL.
      RETURN.
    ENDIF.

    SET PARAMETER ID 'MBN' FIELD l_mseg-mblnr.
    SET PARAMETER ID 'POS' FIELD l_mseg-zeile.
    SET PARAMETER ID 'MJA' FIELD l_mseg-mjahr.
    CALL FUNCTION 'MIGO_DIALOG'
      EXPORTING i_action            = 'A04'
                i_refdoc            = 'R02'
                i_notree            = 'X'
                i_no_auth_check     = ' '
                i_deadend           = 'X'
                i_skip_first_screen = 'X'
                i_okcode            = 'OK_GO'
                i_mblnr             = l_mseg-mblnr
                i_mjahr             = l_mseg-mjahr
                i_zeile             = l_mseg-zeile.
  ENDMETHOD.
