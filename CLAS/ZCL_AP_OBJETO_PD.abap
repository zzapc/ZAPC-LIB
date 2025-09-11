class ZCL_AP_OBJETO_PD definition
  public
  create public .

public section.

  constants C_PLVAR type HRP1000-PLVAR value '01' ##NO_TEXT.
  data OBJETO type HRP1000 .
  data TIPO_OBJETO type HRP1000-OBJID .

  methods CONSTRUCTOR
    importing
      !OTYPE type HRP1000-OTYPE optional
      !OBJID type HRP1000-OBJID optional
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231'
      !ISTAT type HRP1000-ISTAT default '1' .
  class-methods GET_DESCRIPCION
    importing
      !OTYPE type HRP1000-OTYPE optional
      !OBJID type ANY
      !BEGDA type HRP1000-BEGDA default SY-DATUM
      !ENDDA type HRP1000-ENDDA default SY-DATUM
      !ISTAT type HRP1000-ISTAT default '1'
      !LANGU type HRP1000-LANGU default SY-LANGU
      !SHORT type ABAP_BOOL default ''
    returning
      value(STEXT) type HRP1000-STEXT .
  methods GET_RELACION_SIMPLE
    importing
      !SCLAS type HRP1001-SCLAS
      !RSIGN type HRP1001-RSIGN default 'A'
      !RELAT type HRP1001-RELAT
      !BEGDA type HRP1000-BEGDA optional
      !ENDDA type HRP1000-ENDDA optional
    returning
      value(OBJ_RELACIONADO) type HRP1000-OBJID .
  class-methods SET_OBJETO
    importing
      !HRP1000 type HRP1000
    returning
      value(OBJ) type ref to ZCL_AP_OBJETO_PD .
  class-methods LEE_TEXTO
    importing
      !OTYPE type HRP1000-OTYPE
      !OBJID type HRP1000-OBJID
      !SUBTY type HRP1002-SUBTY
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231'
    returning
      value(TEXTOS) type TTTEXT .
  methods GET_TEXTO
    importing
      !SUBTY type HRP1002-SUBTY
    returning
      value(TEXTOS) type TTTEXT .
  methods GET_SUPERIOR
    changing
      !OTYPE type HRP1000-OTYPE
      !OBJID type HRP1000-OBJID .
  class-methods LEE_NOTA
    importing
      !OTYPE type HRP1000-OTYPE
      !OBJID type HRP1000-OBJID
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231'
      !SCLAS type HRP1001-SCLAS
      !SOBID type ANY
      !TIPO_TEXTO type STXH-TDOBJECT default 'HR_TEMNOTE'
      !ID_TEXTO type STXH-TDID optional
      !QUITAR_LINEAS_COMENTARIOS type ABAP_BOOL default ''
    returning
      value(I_TEXTO) type TTTEXT .
  class-methods PANTALLA
    importing
      !ACCION type SY-UCOMM default 'DISP'
      !HRP1000 type HRP1000 optional
    preferred parameter ACCION .
  class-methods GET_ABREVIATURA
    importing
      !OTYPE type HRP1000-OTYPE optional
      !OBJID type HRP1000-OBJID
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231'
      !ISTAT type HRP1000-ISTAT default '1'
      !LANGU type HRP1000-LANGU default SY-LANGU
    returning
      value(SHORT) type HRP1000-SHORT .
  class-methods PP01
    importing
      !OTYPE type HRP1000-OTYPE
      !OBJID type HRP1000-OBJID
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231' .
  class-methods CREA_INFOTIPO
    importing
      !INFTY type INFTY
      !SUBTY type SUBTY optional
      !OTYPE type OTYPE
      !OBJID type HRP1000-OBJID optional
      !SCLAS type SCLAS optional
      !SOBID type ANY optional
      !RESOU type RESOU optional
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default '99991231'
      !ISTAT type PPHDR-ISTAT default '2'
      !MODOBI type CHAR1 default 'E'
      !SHORT type HRP1000-SHORT optional
      !STEXT type HRP1000-STEXT optional
      !KAPZ1 type KAPZ1 optional
      !KAPZ2 type KAPZ2 optional
      !KAPZ3 type KAPZ3 optional
      !BEGUZ type BEGUZ optional
      !ENDUZ type ENDUZ optional
      !RETID type RETID optional
      !HORARIO type PIQHRVSCHED_T optional
    exporting
      !MENSAJE type BAPIRETURN1-MESSAGE
      !NEW_OBJID type HRP1000-OBJID .
  class-methods BORRA_INFOTIPO
    importing
      !INFTY type INFTY
      !SUBTY type SUBTY optional
      !OTYPE type OTYPE
      !OBJID type HRP1000-OBJID optional
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default '99991231'
      !ISTAT type PPHDR-ISTAT default '2'
      !MODOBI type CHAR1 default 'E'
      !SCLAS type SCLAS optional
      !SOBID type ANY optional
    exporting
      !MENSAJE type BAPIRETURN1-MESSAGE .
  class-methods GET_RELACION_ST
    importing
      !OTYPE type HRP1001-OTYPE
      !OBJID type HRP1001-OBJID
      !SCLAS type HRP1001-SCLAS
      !RSIGN type HRP1001-RSIGN default 'A'
      !RELAT type HRP1001-RELAT
      !BEGDA type HRP1000-BEGDA optional
      !ENDDA type HRP1000-ENDDA optional
    returning
      value(I_REL) type PIQ_HRP1001_TAB .
  class-methods GET_RECURSIVO_ST
    importing
      !OTYPE type HRP1000-OTYPE
      !OBJID type HRP1000-OBJID
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231'
      !ISTAT type HRP1000-ISTAT default '1'
      !RSIGN type HRP1001-RSIGN default 'A'
      !RELAT type HRP1001-RELAT
    returning
      value(R_OBJID) type LSO_RANGE_OBJID_TAB .
  class-methods MATCHCODE_OBJETO
    importing
      !OTYPE type HRP1000-OTYPE
    returning
      value(OBJID) type HRP1000-OBJID .
  class-methods MODIFICA_INFOTIPO
    importing
      !INFTY type INFTY
      !SUBTY type SUBTY optional
      !OTYPE type OTYPE
      !OBJID type HRP1000-OBJID optional
      !SCLAS type SCLAS optional
      !SOBID type ANY optional
      !RESOU type RESOU optional
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default '99991231'
      !ISTAT type PPHDR-ISTAT default '2'
      !MODOBI type CHAR1 default 'E'
      !SHORT type HRP1000-SHORT optional
      !STEXT type HRP1000-STEXT optional
      !KAPZ1 type KAPZ1 optional
      !KAPZ2 type KAPZ2 optional
      !KAPZ3 type KAPZ3 optional
      !BEGUZ type BEGUZ optional
      !ENDUZ type ENDUZ optional
      !RETID type RETID optional
      !HORARIO type PIQHRVSCHED_T optional
      !LIMITAR type ABAP_BOOL default ''
    exporting
      !MENSAJE type BAPIRETURN1-MESSAGE
      !NEW_OBJID type HRP1000-OBJID .
  class-methods GRABA_TEXTO
    importing
      !OTYPE type HRP1000-OTYPE
      !OBJID type HRP1000-OBJID
      !SUBTY type HRP1002-SUBTY
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231'
      !I_LINEAS type RMXTTY_OTPT_TRSI_LTX_LINES
    returning
      value(TEXTOS) type TTTEXT .
  class-methods MANTENER_INFOTIPO
    importing
      !INFTY type INFTY
      !SUBTY type SUBTY optional
      !OTYPE type OTYPE
      !OBJID type HRP1000-OBJID optional
      !BEGDA type BEGDA default SY-DATUM
      !ENDDA type ENDDA default '99991231'
      !ISTAT type PPHDR-ISTAT default '2'
      !ACCION type T77FC-FCODE
      !PLVAR type PLVAR default C_PLVAR
      !PNNNN type ANY optional
    exporting
      !MENSAJE type BAPIRETURN1-MESSAGE
      !PNNNN_NEW type ANY
      !FCODE_OUT type T77FC-FCODE .
  class-methods CREA_RELACION
    importing
      !REL type HRI1001
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods BORRA_RELACION
    importing
      !REL type P1001
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods LEE_NOTA_STRING
    importing
      !OTYPE type HRP1000-OTYPE
      !OBJID type HRP1000-OBJID
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231'
      !SCLAS type HRP1001-SCLAS
      !SOBID type ANY
      !TIPO_TEXTO type STXH-TDOBJECT default 'HR_TEMNOTE'
      !ID_TEXTO type STXH-TDID optional
      !QUITAR_LINEAS_COMENTARIOS type ABAP_BOOL default ''
    returning
      value(STRING) type STRING .
  class-methods LEE_TEXTO_STRING
    importing
      !OTYPE type HRP1000-OTYPE
      !OBJID type HRP1000-OBJID
      !SUBTY type HRP1002-SUBTY
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231'
    returning
      value(STRING) type STRING .
  class-methods PP02
    importing
      !OTYPE type HRP1000-OTYPE
      !OBJID type HRP1000-OBJID
      !BEGDA type HRP1000-BEGDA default '00000000'
      !ENDDA type HRP1000-ENDDA default '99991231'
      !INFTY type HRP1001-INFTY
      !SUBTY type HRP1001-SUBTY default ''
    returning
      value(MESSAGE) type BAPI_MSG .
protected section.
private section.
endclass. "ZCL_AP_OBJETO_PD definition
class ZCL_AP_OBJETO_PD implementation.
METHOD borra_infotipo.
  DATA: l_seark(12),
        i_hrp1001   TYPE TABLE OF hrp1001,
        l_char5(5),
        l_tabix     TYPE i.

  DATA o_bi TYPE REF TO zcl_ap_batch_input.

  CREATE OBJECT o_bi.

  o_bi->inicio( ).

  IF infty NE '1000'.
    l_seark = objid.
  ENDIF.

  o_bi->dynpro( program = 'SAPMH5A0' dynpro = '1000').
  IF infty = '1001'.
    o_bi->campos( campo = 'BDC_OKCODE'
                  valor = 'LIST').
  ELSE.
    o_bi->campos( campo = 'BDC_OKCODE'
                  valor = '=DEL').
  ENDIF.
  o_bi->campos( campo = 'PPHDR-PLVAR'
                valor = c_plvar ). " Variante de plan
  o_bi->campos( campo = 'PPHDR-OTYPE'
                valor = otype ). " Tp.objeto
  o_bi->campos( campo = 'PM0D1-SEARK'
                valor = l_seark ). " Concepto de búsqueda
  o_bi->campos( campo = 'PPHDR-BEGDA'
                valor = begda ). " Fecha de inicio
  o_bi->campos( campo = 'PPHDR-ENDDA'
                valor = endda ). " Fecha final
  o_bi->campos( campo = 'PPHDR-ENDDA'
                valor = endda ). " Fecha final
  o_bi->campos( campo = 'PPHDR-INFTY'
                valor = infty ). " Fecha final
  o_bi->campos( campo = 'PPHDR-SUBTY'
                valor = subty ). " Fecha final
  o_bi->campos( campo = 'PPHDR-ISTAT'
                valor = istat ). " Fecha final

  CASE infty.
    WHEN '1000'.
      o_bi->dynpro( program = 'MP100000' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE'
                    valor = '=DEL').
    WHEN '1001'.
      SELECT * FROM hrp1001
        INTO CORRESPONDING FIELDS OF TABLE i_hrp1001
       WHERE otype = otype
         AND objid = objid
         AND subty = subty
         AND begda <= endda
         AND endda >= begda
         AND sclas = sclas
         AND istat = istat.


      IF lines( i_hrp1001 ) > 1.
* Si hay más de un enlace a veces falla porque no se posiciona bien
        READ TABLE i_hrp1001 ASSIGNING FIELD-SYMBOL(<hrp1001>) WITH KEY sobid = sobid.
        IF sy-subrc = 0.
          DATA: wa_hrp1001key TYPE hripkey,
                l_dialog      TYPE pppar-dsupr.

          IF modobi = 'A'. l_dialog = '0'. ELSE. l_dialog = '2'. ENDIF.
          MOVE-CORRESPONDING  <hrp1001> TO wa_hrp1001key.
          CALL FUNCTION 'RH_PNNNN_MAINTAIN'
            EXPORTING
              act_fcode           = 'DEL'
              act_plvar           = <hrp1001>-plvar
              act_otype           = <hrp1001>-otype
              act_objid           = <hrp1001>-objid
              act_infty           = '1001'
              act_istat           = <hrp1001>-istat
              act_subty           = <hrp1001>-subty
              act_begda           = <hrp1001>-begda
              act_endda           = <hrp1001>-endda
              act_infotypekey     = wa_hrp1001key
              suppress_dialog     = l_dialog
            EXCEPTIONS
              infty_not_valid     = 1
              no_plvar            = 2
              object_not_defined  = 3
              otype_not_valid     = 4
              no_authority        = 5
              action_rejected     = 6
              no_gdate            = 7
              fcode_not_supported = 8
              OTHERS              = 9.

          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mensaje.
          RETURN.
        ENDIF.
      ELSE.
        CASE subty. "Participantes
          WHEN 'A025' OR 'A023'.
            READ TABLE i_hrp1001 TRANSPORTING NO FIELDS
                    WITH KEY sobid = sobid.
            IF sy-subrc = 0.
              l_tabix = sy-tabix.
              WRITE l_tabix TO l_char5.
*            IF sy-tabix > 1.
*              o_bi->dynpro( program = 'MP100100' dynpro = '2000').
*              o_bi->campos( campo = 'PPHDX-RECORD_NR'
*                            valor = l_char5 ).
*            ENDIF.
              o_bi->dynpro( program = 'MP100100' dynpro = '3000').
              o_bi->campos( campo = 'BDC_OKCODE' valor = '/00').
              IF l_tabix > 1.
                o_bi->campos( campo = 'PPHDX-PAGE_NR' valor = l_char5 ).
              ENDIF.

              o_bi->dynpro( program = 'MP100100' dynpro = '3000').
              o_bi->campos( campo = 'BDC_OKCODE' valor = '=DEL').
              o_bi->campos( campo = 'LINE_SEL(01)' valor = 'X' ).

              o_bi->dynpro( program = 'MP100100' dynpro = '2000').
              o_bi->campos( campo = 'BDC_OKCODE' valor = '=DEL').

              IF l_tabix > 1.
                o_bi->dynpro( program = 'MP100100' dynpro = '3000').
                o_bi->campos( campo = 'BDC_OKCODE' valor = '=RET').
              ENDIF.
            ELSE.
              mensaje = 'No existe objeto vinculado a borrar'(neb).
              RETURN.
            ENDIF.
          WHEN OTHERS.
            MESSAGE 'Subtipo no definido'(snd) TYPE 'A'.
*        WHEN 'A023'. "Recurso
*          o_bi->dynpro( program = 'MP100100' dynpro = '4023').
*          o_bi->campos( campo = 'PAD23-BEGUZ' valor = beguz ).
*          o_bi->campos( campo = 'PAD23-ENDUZ' valor = enduz ).
*          o_bi->campos( campo = 'Q1001-RESID' valor = retid ).
*          o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
        ENDCASE.
      ENDIF.
    WHEN '1024'.
      o_bi->dynpro( program = 'MP102400' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=DEL').
    WHEN '1035'.
      o_bi->dynpro( program = 'MP103500' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=DEL').
    WHEN '9901'.
      o_bi->dynpro( program = 'MP990100' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE'
                    valor = '=DEL').
  ENDCASE.

  o_bi->dynpro( program = 'SAPMH5A0' dynpro = '1000').
  o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK').

  mensaje = o_bi->llamar_transaccion( tcode = 'PP02' modo = modobi ).


ENDMETHOD.
METHOD borra_relacion.
    DATA i1001 TYPE TABLE OF p1001.

    APPEND rel TO i1001.
    CALL FUNCTION 'RH_DELETE_INFTY'
      EXPORTING
        vtask               = 'B'
      TABLES
        innnn               = i1001
      EXCEPTIONS
        error_during_delete = 1
        no_authorization    = 2
        delete_first_record = 3
        corr_exit           = 4
        OTHERS              = 5.
    IF sy-subrc NE 0.
      IF sy-msgty = 'E'.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ELSE.
        message = 'Error borrando relación'(ebr).
      ENDIF.
    ELSE.
      CALL FUNCTION 'HRCM_BTDB_UPDATE_DB'
        EXPORTING
          vtask        = 'D'
        EXCEPTIONS
          update_error = 1
          OTHERS       = 2.
      IF sy-subrc NE 0.
        MESSAGE 'Error en UPDATE borrando relación'(eub) TYPE 'E'.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD constructor.

  SELECT * FROM  hrp1000
    INTO objeto
    UP TO 1 ROWS
   WHERE plvar  = c_plvar
     AND otype  = otype
     AND objid  = objid
     AND istat  = istat
     AND begda  <= endda
     AND endda  >= begda
    ORDER BY PRIMARY KEY.
  ENDSELECT.

  tipo_objeto = otype.

ENDMETHOD.
METHOD crea_infotipo.
  DATA: l_seark(12),
        l_fecha TYPE d.

  DATA o_bi TYPE REF TO zcl_ap_batch_input.

  CLEAR new_objid.

  CREATE OBJECT o_bi.

  o_bi->inicio( ).

  IF infty NE '1000'.
    l_seark = objid.
  ENDIF.

  o_bi->dynpro( program = 'SAPMH5A0' dynpro = '1000').
  o_bi->campos( campo = 'BDC_OKCODE'
                valor = '=INSE').
  o_bi->campos( campo = 'PPHDR-PLVAR'
                valor = c_plvar ). " Variante de plan
  o_bi->campos( campo = 'PPHDR-OTYPE'
                valor = otype ). " Tp.objeto
  o_bi->campos( campo = 'PM0D1-SEARK'
                valor = l_seark ). " Concepto de búsqueda
  o_bi->campos( campo = 'PPHDR-BEGDA'
                valor = begda ). " Fecha de inicio
  o_bi->campos( campo = 'PPHDR-ENDDA'
                valor = endda ). " Fecha final
  o_bi->campos( campo = 'PPHDR-ENDDA'
                valor = endda ). " Fecha final
  o_bi->campos( campo = 'PPHDR-INFTY'
                valor = infty ). " Fecha final
  o_bi->campos( campo = 'PPHDR-SUBTY'
                valor = subty ). " Fecha final
  o_bi->campos( campo = 'PPHDR-ISTAT'
                valor = istat ). " Fecha final

  CASE infty.
    WHEN '1000'.
      o_bi->dynpro( program = 'MP100000' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE'
                    valor = '=UPD').
      IF short IS INITIAL.
        o_bi->campos( campo = 'P1000-SHORT'
                      valor = stext(12) ).
      ELSE.
        o_bi->campos( campo = 'P1000-SHORT'
                      valor = short ).
      ENDIF.
      o_bi->campos( campo = 'P1000-STEXT'
                    valor = stext ).
    WHEN '1001'.
      o_bi->dynpro( program = 'MP100100' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE'
                    valor = '=UPD').
      o_bi->campos( campo = 'P1001-SCLAS'
                    valor = sclas ).
      o_bi->campos( campo = 'P1001-SOBID'
                    valor = sobid ).

      CASE subty. "Participantes
        WHEN 'A025'.
          o_bi->campos( campo = 'P1001-PRIOX'
                        valor = '50' ).
          o_bi->dynpro( program = 'MP100100' dynpro = '4025').
          o_bi->campos( campo = 'BDC_OKCODE'
                        valor = '=UPD').
        WHEN 'A023'. "Recurso
          o_bi->dynpro( program = 'MP100100' dynpro = '4023').
          o_bi->campos( campo = 'PAD23-BEGUZ' valor = beguz ).
          o_bi->campos( campo = 'PAD23-ENDUZ' valor = enduz ).
          o_bi->campos( campo = 'Q1001-RESID' valor = retid ).
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
        WHEN 'A034'.
          o_bi->campos( campo = 'P1001-PRIOX'
                        valor = '50' ).
          o_bi->dynpro( program = 'MP100100' dynpro = '4034').
          o_bi->campos( campo = 'BDC_OKCODE'
                        valor = '=UPD').
        WHEN 'B049'.
          o_bi->dynpro( program = 'MP100100' dynpro = '4077').
          o_bi->campos( campo = 'BDC_OKCODE'
                        valor = '=UPD').
          o_bi->campos( campo = 'PAD77-STATE_ID'
                        valor = '03' ). "Finalizado
      ENDCASE.
    WHEN '1021'.
      o_bi->dynpro( program = 'MP102100' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').

    WHEN '1026'.
      o_bi->dynpro( program = 'MP102600' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
      o_bi->campos( campo = 'P1026-LANGU' valor =  'S' ).

    WHEN '1024'.
      o_bi->dynpro( program = 'MP102400' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
      o_bi->campos( campo = 'P1024-KAPZ1'
                    valor =  kapz1 ).
      o_bi->campos( campo = 'P1024-KAPZ2'
                    valor =  kapz2 ).
      o_bi->campos( campo = 'P1024-KAPZ3'
                    valor =  kapz3 ).

    WHEN '1031'.
      o_bi->dynpro( program = 'MP103100' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
      o_bi->campos( campo = 'P1031-VKIND' valor = '$MEETING').
      o_bi->campos( campo = 'P1031-TLCNT' valor = '1').
    WHEN '1035'.
      DATA: l_hor       TYPE hrvsched, l_beguz(10), l_enduz(10),
            l_hor_ant   TYPE hrvsched.
      o_bi->dynpro( program = 'MP103500' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=NEWE').
      IF horario IS INITIAL.
        o_bi->dynpro( program = 'MP103500' dynpro = '2000').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=NEW1').
        WRITE beguz TO l_beguz.
        WRITE enduz TO l_enduz.
        o_bi->campos( campo = 'PT1035-EVDAT(01)' valor = begda ).
        o_bi->campos( campo = 'PT1035-BEGUZ(01)' valor = l_beguz ).
        o_bi->campos( campo = 'PT1035-ENDUZ(01)' valor = l_enduz ).
      ELSE.
* Borramos todas las líneas previas
        o_bi->dynpro( program = 'MP103500' dynpro = '2000').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=ADEL').
* Creamos las nuevas
        CLEAR l_hor_ant.
        LOOP AT horario INTO l_hor.
          IF l_hor-beguz = l_hor_ant-enduz.
            ADD 1 TO l_hor-beguz.
          ENDIF.
          o_bi->dynpro( program = 'MP103500' dynpro = '2000').
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00').
          WRITE l_hor-beguz TO l_beguz.
          WRITE l_hor-enduz TO l_enduz.
          IF l_hor-evdat IS INITIAL.
            l_fecha = l_hor-begda.
          ELSE.
            l_fecha = l_hor-evdat.
          ENDIF.
          o_bi->campos( campo = 'PT1035-EVDAT(01)' valor = l_fecha ).
          o_bi->campos( campo = 'PT1035-BEGUZ(01)' valor = l_beguz ).
          o_bi->campos( campo = 'PT1035-ENDUZ(01)' valor = l_enduz ).
          o_bi->dynpro( program = 'MP103500' dynpro = '2000').
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=NEW1').
          o_bi->campos( campo = 'BDC_CURSOR' valor = 'PT1035-EVDAT(01)' ).
          l_hor_ant = l_hor.
        ENDLOOP.
      ENDIF.
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
  ENDCASE.

  o_bi->dynpro( program = 'SAPMH5A0' dynpro = '1000').
  o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK').

  mensaje = o_bi->llamar_transaccion( tcode = 'PP02' modo = modobi ).

  CASE infty.
    WHEN '1000'.
      GET PARAMETER ID 'PON' FIELD new_objid.
  ENDCASE.

ENDMETHOD.
METHOD crea_relacion.
    DATA i_hri1001 TYPE TABLE OF hri1001.

    APPEND rel TO i_hri1001.
    CALL FUNCTION 'HRCM_BTDB_RELAT_CREATE'
      EXPORTING
        check_budget_rela    = space
*       VTASK                = 'D'
      TABLES
        relation             = i_hri1001
*       ERR_RELATION         =
      EXCEPTIONS
        relation_not_allowed = 1
        object_not_found     = 2
        wrong_date_format    = 3
        time_not_valid       = 4
        no_authority         = 5
        error_during_insert  = 6
        undefined            = 7
        OTHERS               = 8.
    IF sy-subrc NE 0.
      IF sy-msgty = 'E'.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ELSE.
        IF sy-subrc = 2.
          message = 'Alguno de los objetos no está activo en esas fechas'(aoa).
        ELSEIF sy-subrc = 5.
          message = 'No autorizado a crear la relación'(nac).
        ELSE.
          message = 'Error creando relación'(ecr).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD get_abreviatura.

  SELECT short FROM  hrp1000
    INTO short
    UP TO 1 ROWS
   WHERE plvar  = c_plvar
     AND otype  = otype
     AND objid  = objid
     AND istat  = istat
     AND begda  <= endda
     AND endda  >= begda
     AND langu  = langu
    ORDER BY PRIMARY KEY.
  ENDSELECT.

ENDMETHOD.
METHOD get_descripcion.
  DATA l_campo TYPE string.

  CLEAR stext.

  IF short IS INITIAL.
    l_campo = 'STEXT'.
  ELSE.
    l_campo = 'SHORT'.
  ENDIF.

  data r_otype type range of hrp1000-otype.
  if not otype is initial.
    append value #( option = 'EQ' SIGN = 'I' LOW = OTYPE ) TO R_OTYPE.
  endif.


  SELECT (l_campo) FROM  hrp1000
    INTO stext
    up to 1 rows
   WHERE plvar  = c_plvar
     AND otype  IN R_otype
     AND objid  = objid
     AND istat  = istat
     AND begda  <= endda
     AND endda  >= begda
     AND langu  = langu
    order by endda descending.
  endselect.
  IF sy-subrc NE 0.
    SELECT (l_campo) FROM  hrp1000
      INTO stext
      up to 1 rows
     WHERE plvar  = c_plvar
       AND otype  IN R_otype
       AND objid  = objid
       AND begda  <= endda
       AND endda  >= begda
       AND langu  = langu
    order by endda descending.
  endselect.
    IF sy-subrc NE 0.
      SELECT (l_campo) FROM  hrp1000
        INTO stext
        UP TO 1 ROWS
       WHERE plvar  = c_plvar
         AND otype  IN R_otype
         AND objid  = objid
         AND langu  = langu
        order by endda descending.
      ENDSELECT.
    ENDIF.
  ENDIF.

ENDMETHOD.
method GET_RECURSIVO_ST.
  DATA: i_objid TYPE lso_range_objid_tab,
        l_objid TYPE lso_range_objid,
        i_hrp1001 TYPE TABLE OF hrp1001,
        l_hrp1001 TYPE hrp1001.

  CLEAR r_objid.

  i_hrp1001 = get_relacion_st( otype = otype
                            objid = objid
                            begda = begda
                            endda = endda
                            rsign = rsign
                            relat = relat
                            sclas = otype ).

  LOOP AT i_hrp1001 INTO l_hrp1001.
    l_hrp1001-objid = l_hrp1001-sobid.
    l_objid-sign = 'I'.
    l_objid-option = 'EQ'.
    l_objid-low  = l_hrp1001-objid.
    APPEND l_objid TO r_objid.

    i_objid = get_recursivo_st( otype = otype
                            objid = l_hrp1001-objid
                            begda = l_hrp1001-begda
                            endda = l_hrp1001-endda
                            rsign = rsign
                            relat = relat ).
    LOOP AT i_objid INTO l_objid.
      APPEND l_objid TO r_objid.
    ENDLOOP.
*    IF sy-subrc NE 0.
*      l_objid-sign = 'I'.
*      l_objid-option = 'EQ'.
*      l_objid-low  = l_hrp1001-objid.
*      APPEND l_objid TO r_objid.
*    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    l_objid-sign = 'I'.
    l_objid-option = 'EQ'.
    l_objid-low  = objid.
    APPEND l_objid TO r_objid.
  ENDIF.

  sort r_objid.
  delete ADJACENT DUPLICATES FROM r_objid.

endmethod.
METHOD get_relacion_simple.
  DATA: l_sobid TYPE hrp1001-sobid,
        l_begda TYPE hrp1001-begda,
        l_endda TYPE hrp1001-endda.

  IF begda IS INITIAL.
    l_begda = objeto-begda.
  ELSE.
    l_begda = begda.
  ENDIF.

  IF endda IS INITIAL.
    l_endda = objeto-endda.
  ELSE.
    l_endda = endda.
  ENDIF.

  CLEAR obj_relacionado.
  SELECT sobid FROM hrp1001
    INTO l_sobid
    UP TO 1 ROWS
   WHERE otype = objeto-otype
     AND objid = objeto-objid
     AND plvar = objeto-plvar
     AND rsign = rsign
     AND relat = relat
     AND begda <= l_endda
     AND endda >= l_begda
     AND sclas = sclas
    ORDER BY PRIMARY KEY.
  ENDSELECT.

  IF sy-subrc = 0.
    obj_relacionado = l_sobid.
  ENDIF.

ENDMETHOD.
method GET_RELACION_ST.
  DATA: l_sobid TYPE hrp1001-sobid.


  CLEAR i_rel.
  SELECT * FROM hrp1001
    INTO TABLE i_rel
   WHERE otype = otype
     AND objid = objid
     AND plvar = c_plvar
     AND rsign = rsign
     AND relat = relat
     AND begda <= endda
     AND endda >= begda
     AND sclas = sclas.

endmethod.
method GET_SUPERIOR.
  DATA: l_sobid TYPE hrp1001-sobid,
        i_leading_pos TYPE TABLE OF hrobject,
        l_leading_pos type hrobject,
        l_fecha type d.

  if objeto-begda <= sy-datum and objeto-endda >= sy-datum.
    l_fecha = sy-datum.
  else.
    l_fecha = objeto-begda.
  endif.


  l_sobid = objeto-objid.
  CALL FUNCTION 'RH_GET_LEADING_POSITION'
    EXPORTING
      plvar                   = c_plvar
      otype                   = objeto-otype
      sobid                   = l_sobid
      date                    = l_fecha
*   AUTH                    = 'X'
*   BUFFER_MODE             = ' '
*   CONSIDER_VAC_POS        = ' '
    TABLES
      leading_pos             = i_leading_pos
   EXCEPTIONS
     no_lead_pos_found       = 1
     OTHERS                  = 2.
  IF sy-subrc = 0.
    read table i_leading_pos into l_leading_pos index 1.
    if sy-subrc = 0.
      otype = l_leading_pos-otype.
      objid = l_leading_pos-objid.
    endif.
  ENDIF.

endmethod.
METHOD get_texto.

  IF objeto-begda <= sy-datum AND objeto-endda >= sy-datum.
    DATA(l_fecha) = sy-datum.
  ELSE.
    l_fecha = objeto-endda.
  ENDIF.

  textos = lee_texto( otype = objeto-otype
                                     objid = objeto-objid
                                     subty = subty
                                     begda = l_fecha
                                     endda = l_fecha ).


ENDMETHOD.
METHOD graba_texto.
  DATA: i_1002    TYPE TABLE OF p1002,
        l_1002    TYPE p1002,
        i_hrt1002 TYPE TABLE OF hrt1002,
        hrt1002   TYPE hrt1002,
        l_line    TYPE tdline,
        l_tabla   TYPE tabname.

  REFRESH textos.

  IF begda IS INITIAL.
    SELECT tabnr FROM hrp1002
      INTO CORRESPONDING FIELDS OF TABLE i_1002
     WHERE plvar = c_plvar
       AND otype = otype
       AND objid = objid
       AND subty = subty.
  ELSE.
    SELECT tabnr FROM hrp1002
      INTO CORRESPONDING FIELDS OF TABLE i_1002
     WHERE plvar = c_plvar
       AND otype = otype
       AND objid = objid
       AND subty = subty
       AND begda = begda
       AND endda = endda.
  ENDIF.

  IF sy-subrc = 0.
    LOOP AT i_1002 INTO l_1002.
      DELETE FROM hrt1002
       WHERE tabnr = l_1002-tabnr.
      LOOP AT i_lineas INTO l_line.
        CLEAR hrt1002.
        hrt1002-tabnr = l_1002-tabnr.
        hrt1002-tabseqnr = sy-tabix.
        hrt1002-tline = l_line.
        MODIFY hrt1002 FROM hrt1002.
      ENDLOOP.
    ENDLOOP.
  ELSE.
    DATA: plog_tab    TYPE TABLE OF hrdbtab,
          l_plog      TYPE hrdbtab,
          tb_plog_tab TYPE TABLE OF hrtbuffer,
          l_buffer    TYPE hrtbuffer.

    l_tabla = 'HRP1000'.
    SELECT * FROM (l_tabla)
    INTO CORRESPONDING FIELDS OF l_plog
      UP TO 1 ROWS
     WHERE plvar = c_plvar
       AND otype = otype
       AND objid = objid
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      l_plog-infty = '1002'.
      l_plog-subty = subty.
      l_plog-opera = 'I'.
      l_plog-varyf = 'S'.
      l_plog-vdata = '           $00000000000000000001'.

      IF NOT begda IS INITIAL.
        l_plog-begda = begda.
        l_plog-endda = endda.
      ENDIF.
      APPEND l_plog TO plog_tab.

      LOOP AT i_lineas INTO l_line.
        CLEAR l_buffer.
        MOVE-CORRESPONDING l_plog TO l_buffer.
        l_buffer-tabnr = l_plog-vdata.
        l_buffer-tabseqnr = sy-tabix.
        l_buffer-tdata = l_line.
        l_buffer-opera = 'I'.
        APPEND l_buffer TO tb_plog_tab.
      ENDLOOP.
      CALL FUNCTION 'RH_BASE_UPDATE_DB_DIALOG'
*     EXPORTING
*       CLIENT                 = SY-MANDT
*       ORDER_FLG              = 'X'
*       TRKORR                 =
*       MAN_FLG                =
*       ACT_FLG                =
*       KEEP_LUPD              =
*       WORKF_ACTV             = 'X'
*       ALE_FLG                =
        TABLES
          upd_plog_tab    = plog_tab
          upd_tb_plog_tab = tb_plog_tab.
*       UPD_BEFORE_IMAGE       =
*       UPD_AFTER_IMAGE        =
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.
ENDMETHOD.
METHOD lee_nota.
  DATA: l_stxh TYPE stxh,
        BEGIN OF l_clave,
          plvar TYPE hrp1001-plvar,
          otype TYPE hrp1001-otype,
          objid TYPE hrp1001-objid,
          infty TYPE hrp1001-infty,
          rsign TYPE hrp1001-rsign,
          relat TYPE hrp1001-relat,
          sclas TYPE hrp1001-sclas,
          sobid TYPE hrp1001-sobid,
        END OF l_clave.

  SELECT plvar otype objid infty rsign relat sclas sobid FROM hrp1001
    INTO CORRESPONDING FIELDS OF l_clave
   WHERE plvar = c_plvar
     AND otype = otype
     AND objid = objid
     AND begda <= endda
     AND endda >= begda
     AND sclas = sclas
     AND sobid = sobid
   ORDER BY begda.
  ENDSELECT.

  IF id_texto IS INITIAL.
    SELECT tdid tdname tdobject tdspras FROM stxh
      UP TO 1 ROWS
       INTO CORRESPONDING FIELDS OF l_stxh
      WHERE tdobject = tipo_texto
       AND tdname   = l_clave
    ORDER BY PRIMARY KEY.
    ENDSELECT.
  ELSE.
    SELECT tdid tdname tdobject tdspras FROM stxh
       INTO CORRESPONDING FIELDS OF l_stxh
      UP TO 1 ROWS
     WHERE tdobject = tipo_texto
       AND tdname   = l_clave
       AND tdid     = id_texto
    ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDIF.

  REFRESH i_texto.
  IF sy-subrc = 0.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = l_stxh-tdid
        language                = l_stxh-tdspras
        name                    = l_stxh-tdname
        object                  = l_stxh-tdobject
      TABLES
        lines                   = i_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc NE 0.
*      message.
    ELSE.
      IF quitar_lineas_comentarios = 'X'.
        DELETE i_texto WHERE tdformat = '>X'.
      ENDIF.
    ENDIF.
  ENDIF.



ENDMETHOD.
METHOD lee_nota_string.
  DATA i_texto TYPE tttext.

  i_texto = lee_nota(
      otype      = otype
      objid      = objid
      begda      = begda
      endda      = endda
      sclas      = sclas
      sobid      = sobid
      tipo_texto = tipo_texto
      id_texto   = id_texto
      quitar_lineas_comentarios = quitar_lineas_comentarios
         ).

  string = zcl_ap_textos=>tline2string( lineas = i_texto separar_con_espacio = '' ).

ENDMETHOD.
METHOD lee_texto.
  DATA: i_1002    TYPE TABLE OF p1002,
        i_hrt1002 TYPE TABLE OF hrt1002,
        hrt1002   TYPE hrt1002,
        l_line    TYPE tline,
        l_tabla   TYPE tabname VALUE 'HRP1002'.

  REFRESH textos.

  SELECT * FROM (l_tabla)
    INTO CORRESPONDING FIELDS OF TABLE i_1002
   WHERE plvar = c_plvar
     AND otype = otype
     AND objid = objid
     AND subty = subty
     AND endda = begda
     AND begda = endda.
  IF sy-subrc NE 0.
    SELECT * FROM (l_tabla)
      INTO CORRESPONDING FIELDS OF TABLE i_1002
     WHERE plvar = c_plvar
       AND otype = otype
       AND objid = objid
       AND subty = subty
       AND endda >= begda
       AND begda <= endda.
  ENDIF.

  IF sy-subrc = 0.
    CALL FUNCTION 'RH_READ_INFTY_TABDATA'
      EXPORTING
        infty          = '1002'
      TABLES
        innnn          = i_1002
        hrtnnnn        = i_hrt1002
      EXCEPTIONS
        no_table_infty = 1
        innnn_empty    = 2
        nothing_found  = 3
        OTHERS         = 4.
    IF sy-subrc NE 0.
*  message.
    ENDIF.

    LOOP AT i_hrt1002 INTO hrt1002.
      l_line-tdformat = hrt1002-tformat.
      l_line-tdline   = hrt1002-tline.
      APPEND l_line TO textos.
    ENDLOOP.
  ENDIF.

ENDMETHOD.
METHOD lee_texto_string.

  DATA(i_textos) = lee_texto( otype  = otype
                              objid  = objid
                              subty  = subty
                              begda  = begda
                              endda  = endda ).

  string = zcl_ap_textos=>tline2string( lineas = i_textos separar_con_espacio = '' ).

ENDMETHOD.
METHOD mantener_infotipo.
  DATA: l_msg TYPE hrrhad_msg.

  CLEAR mensaje.

  CALL FUNCTION 'RH_PNNNN_MAINTAIN'
    EXPORTING
      act_fcode           = accion
      act_plvar           = plvar
      act_otype           = otype
      act_objid           = objid
      act_infty           = infty
      act_subty           = subty
      act_istat           = istat
      act_begda           = begda
      act_endda           = endda
*     ACT_NEW_LANGU       =
      act_pnnnn           = pnnnn
*     ACT_INFOTYPEKEY     =
*     ACT_DPATT           =
*     ACT_GDATE           =
*     ACT_NEW_HISTO       =
*     ACT_ENQUEUE         = 'X'
*     SUPPRESS_DIALOG     =
*     ACT_VTASK           = 'D'
*     ACT_COMMIT_FLG      = 'X'
*     ACT_MAINT           = 'X'
*     CLEAR_BUFFER_PLOG_TAB       = 'X'
*     SUPPRESS_INTEGRATION        =
*     ACT_PPPAR_EXEP      =
    IMPORTING
      act_ok_code         = fcode_out
      act_pnnnn_out       = pnnnn_new
      act_mess_info       = l_msg
*     TABLES
*     ACT_HRTNNNN         =
    EXCEPTIONS
      infty_not_valid     = 1
      no_plvar            = 2
      object_not_defined  = 3
      otype_not_valid     = 4
      no_authority        = 5
      action_rejected     = 6
      no_gdate            = 7
      fcode_not_supported = 8
      OTHERS              = 9.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mensaje.
  ELSEIF l_msg-msgty = 'E'.
    MESSAGE ID l_msg-msgid TYPE 'S' NUMBER l_msg-msgno WITH l_msg-msgv1 l_msg-msgv2 l_msg-msgv3 l_msg-msgv4 INTO mensaje.
  ENDIF.


ENDMETHOD.
method MATCHCODE_OBJETO.
  data : f4_objec   type objec.

  call function 'RH_OBJID_REQUEST'
    exporting
      plvar             = c_plvar
      otype             = otype
      dynpro_repid      = sy-cprog
      dynpro_dynnr      = sy-dynnr
      dynpro_plvarfield = 'PPHDR-PLVAR'
      dynpro_otypefield = 'PPHDR-OTYPE'
      dynpro_searkfield = 'PM0D1-SEARK'
    importing
      sel_object        = f4_objec
    exceptions
      cancelled         = 1
      wrong_condition   = 2
      nothing_found     = 3
      illegal_mode      = 4
      internal_error    = 5
      others            = 6.

  if sy-subrc = 0.
    objid = f4_objec.
  endif.

endmethod.
METHOD modifica_infotipo.
  DATA: l_seark(12).

  DATA o_bi TYPE REF TO zcl_ap_batch_input.

  CLEAR new_objid.

  CREATE OBJECT o_bi.

  o_bi->inicio( ).

  l_seark = objid.


  o_bi->dynpro( program = 'SAPMH5A0' dynpro = '1000').
  o_bi->campos( campo = 'BDC_OKCODE'
                valor = '=AEND').
  o_bi->campos( campo = 'PPHDR-PLVAR'
                valor = c_plvar ). " Variante de plan
  o_bi->campos( campo = 'PPHDR-OTYPE'
                valor = otype ). " Tp.objeto
  o_bi->campos( campo = 'PM0D1-SEARK'
                valor = l_seark ). " Concepto de búsqueda
  o_bi->campos( campo = 'PPHDR-BEGDA'
                valor = begda ). " Fecha de inicio
  o_bi->campos( campo = 'PPHDR-ENDDA'
                valor = endda ). " Fecha final
  o_bi->campos( campo = 'PPHDR-ENDDA'
                valor = endda ). " Fecha final
  o_bi->campos( campo = 'PPHDR-INFTY'
                valor = infty ). " Fecha final
  o_bi->campos( campo = 'PPHDR-SUBTY'
                valor = subty ). " Fecha final
  o_bi->campos( campo = 'PPHDR-ISTAT'
                valor = istat ). " Fecha final

  CASE infty.
    WHEN '1000'.
      o_bi->dynpro( program = 'MP100000' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE'
                    valor = '=UPD').
      IF short IS INITIAL.
        o_bi->campos( campo = 'P1000-SHORT'
                      valor = stext(12) ).
      ELSE.
        o_bi->campos( campo = 'P1000-SHORT'
                      valor = short ).
      ENDIF.
      o_bi->campos( campo = 'P1000-STEXT'
                    valor = stext ).
    WHEN '1001'.
      o_bi->dynpro( program = 'MP100100' dynpro = '2000').

      o_bi->campos( campo = 'P1001-BEGDA' valor = begda ).
      o_bi->campos( campo = 'P1001-ENDDA' valor = endda ).
      o_bi->campos( campo = 'P1001-SCLAS'
                    valor = sclas ).
      o_bi->campos( campo = 'P1001-SOBID'
                    valor = sobid ).

      o_bi->campos( campo = 'BDC_OKCODE'
                    valor = '=UPD').
      IF limitar NE ''.
        CASE subty. "Participantes
          WHEN 'A025'.
            o_bi->campos( campo = 'P1001-PRIOX'
                          valor = '50' ).
            o_bi->dynpro( program = 'MP100100' dynpro = '4025').
            o_bi->campos( campo = 'BDC_OKCODE'
                          valor = '=UPD').
          WHEN 'A023'. "Recurso
            o_bi->dynpro( program = 'MP100100' dynpro = '4023').
            o_bi->campos( campo = 'PAD23-BEGUZ' valor = beguz ).
            o_bi->campos( campo = 'PAD23-ENDUZ' valor = enduz ).
            o_bi->campos( campo = 'Q1001-RESID' valor = retid ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').

          WHEN 'B049'.
            o_bi->dynpro( program = 'MP100100' dynpro = '4077').
            o_bi->campos( campo = 'BDC_OKCODE'
                          valor = '=UPD').
            o_bi->campos( campo = 'PAD77-STATE_ID'
                          valor = '03' ). "Finalizado

          WHEN 'A011'. "Centro de coste
            o_bi->dynpro( program = 'MP100100' dynpro = '5010').
            o_bi->campos( campo = 'PKEYK-KOSTL' valor = sobid ).
            o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
        ENDCASE.
      ENDIF.
    WHEN '1021'.
      o_bi->dynpro( program = 'MP102100' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').

    WHEN '1026'.
      o_bi->dynpro( program = 'MP102600' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
      o_bi->campos( campo = 'P1026-LANGU' valor =  'S' ).

    WHEN '1024'.
      o_bi->dynpro( program = 'MP102400' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
      o_bi->campos( campo = 'P1024-KAPZ1'
                    valor =  kapz1 ).
      o_bi->campos( campo = 'P1024-KAPZ2'
                    valor =  kapz2 ).
      o_bi->campos( campo = 'P1024-KAPZ3'
                    valor =  kapz3 ).
    WHEN '1035'.
      DATA: l_hor       TYPE hrvsched, l_beguz(10), l_enduz(10),
            l_hor_ant   TYPE hrvsched.
      o_bi->dynpro( program = 'MP103500' dynpro = '2000').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=NEWE').
      IF horario IS INITIAL.
        o_bi->dynpro( program = 'MP103500' dynpro = '2000').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=NEW1').
        WRITE beguz TO l_beguz.
        WRITE enduz TO l_enduz.
        o_bi->campos( campo = 'PT1035-EVDAT(01)' valor = begda ).
        o_bi->campos( campo = 'PT1035-BEGUZ(01)' valor = l_beguz ).
        o_bi->campos( campo = 'PT1035-ENDUZ(01)' valor = l_enduz ).
      ELSE.
* Borramos todas las líneas previas
        o_bi->dynpro( program = 'MP103500' dynpro = '2000').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=ADEL').
* Creamos las nuevas
        CLEAR l_hor_ant.
        LOOP AT horario INTO l_hor.
          IF l_hor-beguz = l_hor_ant-enduz.
            ADD 1 TO l_hor-beguz.
          ENDIF.
          o_bi->dynpro( program = 'MP103500' dynpro = '2000').
          o_bi->campos( campo = 'BDC_OKCODE' valor = '/00').
          WRITE l_hor-beguz TO l_beguz.
          WRITE l_hor-enduz TO l_enduz.
          o_bi->campos( campo = 'PT1035-EVDAT(01)' valor = l_hor-begda ).
          o_bi->campos( campo = 'PT1035-BEGUZ(01)' valor = l_beguz ).
          o_bi->campos( campo = 'PT1035-ENDUZ(01)' valor = l_enduz ).
          o_bi->dynpro( program = 'MP103500' dynpro = '2000').
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=NEW1').
          o_bi->campos( campo = 'BDC_CURSOR' valor = 'PT1035-EVDAT(01)' ).
          l_hor_ant = l_hor.
        ENDLOOP.
      ENDIF.
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPD').
  ENDCASE.

  o_bi->dynpro( program = 'SAPMH5A0' dynpro = '1000').
  o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK').

  mensaje = o_bi->llamar_transaccion( tcode = 'PP02' modo = modobi ).

  CASE infty.
    WHEN '1000'.
      GET PARAMETER ID 'PON' FIELD new_objid.
  ENDCASE.

ENDMETHOD.
method PANTALLA.

  DATA: pphdr TYPE pphdr,
        pm0d1 TYPE pm0d1.
  DATA: pfstatus       TYPE sy-pfkey,
        transaction    TYPE tstct-ttext,
        aktion(25).
  CONSTANTS:
        yes                          VALUE '1',
        no                           VALUE '0',
        on                           VALUE '1',
        off                          VALUE '0'.
  DATA: subrc LIKE sy-subrc,                                "VWMA104906
        dummy.
  DATA: imp_infty TYPE wplog.         "Imported INFTY from Dialog
  DATA: imp_ok    TYPE sy-ucomm.      "Imported OK-CODE from Dialog
  DATA: infty_subrc(4).               "Return-code from update

  DATA: vtask TYPE t777d-vtask.       "Updatetask

  CLEAR pphdr.
  MOVE-CORRESPONDING hrp1000 TO pphdr.

  PERFORM rh_infty_dialog IN PROGRAM sapfh5ad USING
          accion     pphdr-plvar pphdr-otype pphdr-objid
          pphdr-infty pphdr-subty pphdr-istat pphdr-varyf
          pphdr-begda pphdr-endda dummy       pm0d1-dpatt
          vtask       on          on          on
          off         off         off         dummy
          dummy       off         imp_infty   imp_ok
          infty_subrc.

endmethod.
METHOD pp01.
  DATA l_hrp1000 TYPE hrp1000.

  SELECT begda endda FROM hrp1000
    INTO CORRESPONDING FIELDS OF l_hrp1000
    UP TO 1 ROWS
  WHERE plvar = c_plvar
    AND otype = otype
    AND objid = objid
    AND begda <= endda
    AND endda >= begda
    ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  DATA o_bi TYPE REF TO zcl_ap_batch_input.
  DATA l_mensaje TYPE bapireturn1-message.
  CREATE OBJECT o_bi.

  o_bi->inicio( ).

  o_bi->dynpro( program = 'SAPMH5A0' dynpro = '5000').
  o_bi->campos( campo = 'BDC_OKCODE'
                valor = '/00').
  o_bi->campos( campo = 'PPHDR-PLVAR'
                valor = c_plvar ). " Variante de plan
  o_bi->campos( campo = 'PPHDR-OTYPE'
                valor = otype ). " Tp.objeto
  o_bi->campos( campo = 'PM0D1-SEARK'
                valor = objid ). " Concepto de búsqueda
  o_bi->dynpro( program = 'SAPMH5A0' dynpro = '5000').
  o_bi->campos( campo = 'BDC_OKCODE'
                valor = '/00').
  o_bi->campos( campo = 'PPHDR-BEGDA'
                valor = l_hrp1000-begda ). " Fecha de inicio
  o_bi->campos( campo = 'PPHDR-ENDDA'
                valor = l_hrp1000-endda ). " Fecha final

  l_mensaje = o_bi->llamar_transaccion( tcode = 'PP01' modo = 'E' ).

ENDMETHOD.
METHOD pp02.
  DATA: l_hrp1000 TYPE hrp1000,
        l_hrp1001 TYPE hrp1001,
        l_tabla   TYPE tabname.

  SELECT istat begda endda FROM hrp1000
    INTO CORRESPONDING FIELDS OF l_hrp1000
    UP TO 1 ROWS
  WHERE plvar = c_plvar
    AND otype = otype
    AND objid = objid
    AND begda <= endda
    AND endda >= begda
    ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc NE 0.
    message = |No existe el objeto { otype } { objid }|.
    RETURN.
  ENDIF.

  CONCATENATE 'HRP' infty INTO l_tabla.
  TRY.
      SELECT istat begda endda FROM (l_tabla)
        INTO CORRESPONDING FIELDS OF l_hrp1001
        UP TO 1 ROWS
      WHERE plvar = c_plvar
        AND otype = otype
        AND objid = objid
        AND begda <= endda
        AND endda >= begda
        AND infty = infty
        AND subty = subty
        ORDER BY PRIMARY KEY.
      ENDSELECT.
    CATCH cx_root INTO DATA(cx_root).
  ENDTRY.

  DATA: o_bi      TYPE REF TO zcl_ap_batch_input,
        l_mensaje TYPE bapireturn1-message.
  CREATE OBJECT o_bi.

  o_bi->inicio( ).

  o_bi->dynpro( program = 'SAPMH5A0' dynpro = '1000').
  o_bi->campos( campo = 'PPHDR-PLVAR' valor = c_plvar ). " Variante de plan
  o_bi->campos( campo = 'PPHDR-OTYPE' valor = otype ). " Tp.objeto
  o_bi->campos( campo = 'PM0D1-SEARK' valor = objid ). " Concepto de búsqueda
  o_bi->campos( campo = 'PPHDR-INFTY' valor = infty ). " Infotipo
  o_bi->campos( campo = 'PPHDR-subty' valor = subty ). " Infotipo
  o_bi->campos( campo = 'PPHDR-ISTAT' valor = l_hrp1000-istat ). " Status de planificación
  IF l_hrp1001 IS INITIAL.
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=INSE').

    IF begda IS INITIAL.
      o_bi->campos( campo = 'PPHDR-BEGDA' valor = l_hrp1000-begda ). " Fecha de inicio
      o_bi->campos( campo = 'PPHDR-ENDDA' valor = l_hrp1000-endda ). " Fecha final
    ELSE.
      o_bi->campos( campo = 'PPHDR-BEGDA' valor = begda ). " Fecha de inicio
      o_bi->campos( campo = 'PPHDR-ENDDA' valor = endda ). " Fecha final
    ENDIF.
  ELSE.
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=AEND').

    o_bi->campos( campo = 'PPHDR-BEGDA' valor = l_hrp1001-begda ). " Fecha de inicio
    o_bi->campos( campo = 'PPHDR-ENDDA' valor = l_hrp1001-endda ). " Fecha final
  ENDIF.

  l_mensaje = o_bi->llamar_transaccion( tcode = 'PP02' modo = 'E').

ENDMETHOD.
method SET_OBJETO.

  create object obj.
  obj->objeto = hrp1000.

endmethod.
