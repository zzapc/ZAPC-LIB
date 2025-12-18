CLASS zcl_ap_aviso_qm DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA cabecera         TYPE bapi2080_nothdre.
    DATA i_return         TYPE bapiret2_t.
    DATA return           TYPE bapiret2.
    DATA i_actividad      TYPE alm_me_bapi2080_notactve_t.
    DATA i_interlocutores TYPE alm_me_bapi2080_notpartnre_t.
    DATA i_textos         TYPE alm_me_bapi2080_notfulltxte_t.
    DATA indabschl        TYPE riwo00-selkt.
    DATA viqmel           TYPE viqmel.
    DATA riwo02           TYPE riwo02.
    DATA riwo03           TYPE riwo03.

    CONSTANTS c_classtype TYPE klassenart VALUE '015' ##NO_TEXT.
    CONSTANTS c_tipo_gos  TYPE sibftypeid VALUE 'BUS2078' ##NO_TEXT.

    DATA i_notifhdtext TYPE bapi2080_nothdtxte.

    METHODS leer_aviso
      IMPORTING qmnum          TYPE bapi2080_nothdre-notif_no
      RETURNING VALUE(mensaje) TYPE bapiret2-message.

    METHODS crear_aviso
      IMPORTING tipo             TYPE bapi2080-notif_type
                !header          TYPE bapi2080_nothdri              OPTIONAL
                i_actividad      TYPE alm_me_bapi2080_notactvi_t    OPTIONAL
                i_interlocutores TYPE alm_me_bapi2080_notpartnri_t  OPTIONAL
                i_textos         TYPE alm_me_bapi2080_notfulltxti_t OPTIONAL
                viqmel           TYPE viqmel                        OPTIONAL
      RETURNING VALUE(num_aviso) TYPE bapi2080_nothdre-notif_no.

    METHODS borra_datos_aviso.

    METHODS modificar_datos_aviso
      IMPORTING  !header          TYPE bapi2080_nothdri
                 i_actividad      TYPE alm_me_bapi2080_notactvi_t    OPTIONAL
                 i_interlocutores TYPE alm_me_bapi2080_notpartnri_t  OPTIONAL
                 i_textos         TYPE alm_me_bapi2080_notfulltxti_t OPTIONAL
      RETURNING  VALUE(mensaje)   TYPE bapiret2-message
      EXCEPTIONS error.

    METHODS cerrar_aviso.

    METHODS get_text_error
      RETURNING VALUE(mensaje) TYPE bapiret2-message.

    CLASS-METHODS visualizar_st
      IMPORTING qmnum TYPE qmel-qmnum.

    CLASS-METHODS get_status_st
      IMPORTING qmnum         TYPE qmnum     OPTIONAL
                objnr         TYPE qmobjnr   OPTIONAL
                usuario       TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER qmnum
      RETURNING VALUE(status) TYPE bsvx-sttxt.

    METHODS get_aviso
      IMPORTING qmnum TYPE qmnum.

    CLASS-METHODS editar_st
      IMPORTING qmnum TYPE qmel-qmnum.

    CLASS-METHODS get_avisos
      IMPORTING r_qmnum         TYPE range_qmnum_tab            OPTIONAL
                r_qmart         TYPE diacl_qmart_range_t        OPTIONAL
                r_status        TYPE zcl_ap_rango=>tab_range_c4 OPTIONAL
                r_matnr         TYPE fre_range_t_matnr          OPTIONAL
                r_deviceid      TYPE wtysc_deviceid_ranges_tab  OPTIONAL
                matnr           TYPE matnr                      OPTIONAL
                deviceid        TYPE deviceid                   OPTIONAL
                qmnum           TYPE qmnum                      OPTIONAL
                r_lifnr         TYPE fagl_range_t_lifnr         OPTIONAL
                r_qmcod         TYPE zcl_ap_rango=>tab_range_c4 OPTIONAL
                r_erdat         TYPE range_date_t               OPTIONAL
                r_strmn         TYPE range_date_t               OPTIONAL
      RETURNING VALUE(i_viqmel) TYPE iqstt_viqmel.

    CLASS-METHODS get_text_cod
      IMPORTING catalogo     TYPE qpcd-katalogart
                grupo        TYPE qpcd-codegruppe
                codigo       TYPE qpcd-code
                idioma       TYPE qpct-sprache   DEFAULT sy-langu
                fecha        TYPE qpcd-gueltigab DEFAULT sy-datum
      RETURNING VALUE(texto) TYPE qpct-kurztext.

    CLASS-METHODS contiene_status
      IMPORTING s_status  TYPE zcl_ap_rango=>tab_range_c4 OPTIONAL
                objnr     TYPE qmobjnr                    OPTIONAL
                qmnum     TYPE qmnum                      OPTIONAL
                !status   TYPE any                        OPTIONAL
                usuario   TYPE abap_bool                  DEFAULT ''
      RETURNING VALUE(ok) TYPE abap_bool.

    CLASS-METHODS f4_cod
      IMPORTING catalogo      TYPE qpgr-katalogart
                grupo         TYPE qpgr-codegruppe
                pickup        TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(codigo) TYPE qpk1cd-code.

    CLASS-METHODS get_status_usuario_st
      IMPORTING qmnum         TYPE qmnum   OPTIONAL
                objnr         TYPE qmobjnr OPTIONAL
      PREFERRED PARAMETER qmnum
      RETURNING VALUE(status) TYPE bsvx-sttxt.

    CLASS-METHODS set_valor_clas
      IMPORTING classnum       TYPE any
                qmnum          TYPE qmfe-qmnum
                fenum          TYPE qmfe-fenum OPTIONAL
                !commit        TYPE abap_bool  DEFAULT 'X'
                caract         TYPE any
                valor          TYPE any
                classtype      TYPE any        DEFAULT c_classtype
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    CLASS-METHODS crear_nota
      IMPORTING qmnum           TYPE any
                descripcion     TYPE any DEFAULT ''
                contenido       TYPE any DEFAULT ''
      RETURNING VALUE(i_return) TYPE bapirettab.

    CLASS-METHODS leer_nota
      IMPORTING qmnum       TYPE any
                descripcion TYPE any DEFAULT ''
      EXPORTING contenido   TYPE any
                i_return    TYPE bapirettab.

    CLASS-METHODS borrar_nota
      IMPORTING qmnum       TYPE any
                descripcion TYPE any DEFAULT ''
      EXPORTING i_return    TYPE bapirettab.

    CLASS-METHODS get_texto_string
      IMPORTING qmnum         TYPE qmnum
                !id           TYPE stxh-tdid    DEFAULT 'LTQM'
                spras         TYPE stxh-tdspras DEFAULT sy-langu
      RETURNING VALUE(string) TYPE string.

    METHODS crear_aviso_qm
      IMPORTING !commit   TYPE abap_bool     DEFAULT 'X'
      EXPORTING num_aviso TYPE bapi2080_nothdre-notif_no
                !message  TYPE bapi_msg
      CHANGING  i_viqmma  TYPE tt_rfc_viqmma OPTIONAL
                i_ihpa    TYPE tt_rfc_ihpa   OPTIONAL
                viqmel    TYPE viqmel        OPTIONAL
                i_viqmur  TYPE tt_rfc_viqmur OPTIONAL.

    METHODS modificar_aviso_qm
      IMPORTING !commit  TYPE abap_bool     DEFAULT 'X'
      EXPORTING !message TYPE bapi_msg
      CHANGING  i_viqmma TYPE tt_rfc_viqmma OPTIONAL
                i_ihpa   TYPE tt_rfc_ihpa_m OPTIONAL
                viqmel   TYPE viqmel        OPTIONAL.

    CLASS-METHODS set_status_usuario
      IMPORTING qmnum          TYPE any
                txt04          TYPE tj30t-txt04 OPTIONAL
                estat          TYPE tj30t-estat OPTIONAL
                spras          TYPE sy-langu    DEFAULT sy-langu
                cambio_directo TYPE abap_bool   DEFAULT ''
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS set_texto_string
      IMPORTING qmnum   TYPE qmnum
                !id     TYPE stxh-tdid    DEFAULT 'LTQM'
                spras   TYPE stxh-tdspras DEFAULT sy-langu
                !string TYPE string.

    CLASS-METHODS get_descripcion_aviso
      IMPORTING qmnum        TYPE qmnum
      RETURNING VALUE(texto) TYPE string.

    CLASS-METHODS f4_grp
      IMPORTING catalogo     TYPE qpgr-katalogart
                pickup       TYPE abap_bool DEFAULT 'X'
                qmart        TYPE qmart
      CHANGING  codigo       TYPE qpk1cd-code
                VALUE(grupo) TYPE qpgr-codegruppe.

    CLASS-METHODS get_texto_pos_string
      IMPORTING qmnum         TYPE qmnum
                fenum         TYPE qmfe-fenum
                !id           TYPE stxh-tdid    DEFAULT 'LTQM'
                spras         TYPE stxh-tdspras DEFAULT sy-langu
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS get_texto_acc_string
      IMPORTING qmnum         TYPE qmnum
                manum         TYPE qmsm-manum
                !id           TYPE stxh-tdid    DEFAULT 'LTQM'
                spras         TYPE stxh-tdspras DEFAULT sy-langu
      RETURNING VALUE(string) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS grabar_aviso_interno.
endclass. "ZCL_AP_AVISO_QM definition
class ZCL_AP_AVISO_QM implementation.
  METHOD borra_datos_aviso.
    CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_DELETE'
      EXPORTING number = cabecera-notif_no
      TABLES
*                NOTITEM =
*                NOTIFCAUS =
*                NOTIFACTV =
*                NOTIFTASK =
*                NOTIFPARTNR =
                return = i_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = 'X'.
  ENDMETHOD.
  METHOD borrar_nota.
    zcl_ap_gos=>borrar_nota( EXPORTING clave       = qmnum
                                       tipo        = c_tipo_gos
                                       descripcion = descripcion
                             IMPORTING i_return    = i_return ).
  ENDMETHOD.
  METHOD cerrar_aviso.
    DATA l_syststat TYPE bapi2080_notsti.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
      EXPORTING number   = cabecera-notif_no
*                SYSTSTAT = 'XXXX'
                syststat = l_syststat
*                TESTRUN  = ' '
* IMPORTING
*                SYSTEMSTATUS =
*                USERSTATUS =
      TABLES    return   = i_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = 'X'.
  ENDMETHOD.
  METHOD contiene_status.
    DATA: r_stat_si TYPE RANGE OF j_status,
          l_objnr   TYPE jest-objnr,
          lr_status TYPE lxhme_range_c4,
          r_status  TYPE zcl_ap_rango=>tab_range_c4,
          l_istat   TYPE j_status,
          lr_stat   LIKE LINE OF r_stat_si,
          r_stat_no TYPE RANGE OF j_status.

    IF NOT objnr IS INITIAL.
      l_objnr = objnr.
    ELSE.
      CONCATENATE 'QM' qmnum INTO l_objnr.
    ENDIF.

    IF s_status IS INITIAL.
      CLEAR lr_status.
      lr_status-option = 'EQ'.
      lr_status-sign   = 'I'.
      lr_status-low    = status.
      APPEND lr_status TO r_status.
    ELSE.
      r_status = s_status.
    ENDIF.

    ok = 'X'.

* Filtramos valores en función del status
    LOOP AT r_status INTO lr_status.
      IF usuario IS INITIAL.
        SELECT SINGLE istat FROM tj02t                        "#EC *
          INTO l_istat
          WHERE spras = sy-langu
            AND txt04 = lr_status-low.
      ELSE.
        SELECT estat FROM tj30t                               "#EC *
          INTO l_istat
          UP TO 1 ROWS
          WHERE spras = sy-langu
            AND txt04 = lr_status-low
          ORDER BY PRIMARY KEY.
        ENDSELECT.
      ENDIF.
      IF sy-subrc = 0.
        CLEAR lr_stat.
        IF lr_status-sign = 'I' AND lr_status-option = 'EQ'.
          lr_stat-option = lr_status-option.
          lr_stat-sign   = lr_status-sign.
          lr_stat-low    = l_istat.
          APPEND lr_stat TO r_stat_si.
        ELSE.
          lr_stat-option = 'EQ'.
          lr_stat-sign   = 'I'.
          lr_stat-low    = l_istat.
          APPEND lr_stat TO r_stat_no.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF NOT r_stat_si IS INITIAL.
      SELECT SINGLE stat FROM jest                           "#EC *
        INTO l_istat
        WHERE objnr  = l_objnr
          AND stat  IN r_stat_si
          AND inact  = ''.
      IF sy-subrc <> 0.
        CLEAR ok.
        return.
      ENDIF.
    ENDIF.

    IF NOT r_stat_no IS INITIAL.
      SELECT SINGLE stat FROM jest                           "#EC *
        INTO l_istat
        WHERE objnr  = l_objnr
          AND stat  IN r_stat_no
          AND inact  = ''.
      IF sy-subrc = 0.
        CLEAR ok.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD crear_aviso.
    DATA l_header TYPE bapi2080_nothdri.

    IF NOT header IS INITIAL.
      l_header = header.
    ELSE.
      CALL FUNCTION 'MAP2E_VIQMEL_TO_BAPI2080_NOTHI'
        EXPORTING viqmel           = viqmel
        CHANGING  bapi2080_nothdri = l_header.
    ENDIF.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE'
      EXPORTING
*                EXTERNAL_NUMBER    =
                notif_type         = tipo
                notifheader        = l_header
*                TASK_DETERMINATION = ' '
*                SENDER             =
*                ORDERID            =
      IMPORTING notifheader_export = cabecera
      TABLES
*                notitem            =
*                NOTIFCAUS          =
                notifactv          = i_actividad
*                NOTIFTASK          =
                notifpartnr        = i_interlocutores
                longtexts          = i_textos
*                KEY_RELATIONSHIPS  =
                return             = i_return.

    IF i_return IS INITIAL.
      grabar_aviso_interno( ).
      num_aviso = cabecera-notif_no.
    ELSE.
      READ TABLE i_return INTO return WITH KEY type = 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD crear_aviso_qm.
    DATA riqs5 TYPE riqs5.

    MOVE-CORRESPONDING viqmel TO riqs5.

    CALL FUNCTION 'IQS4_CREATE_NOTIFICATION'
      EXPORTING
        i_riqs5    = riqs5
        i_commit   = commit
        i_wait     = 'X'
      IMPORTING
        e_viqmel   = viqmel
      TABLES
*       i_inlines_t = i_inlines
*       i_viqmfe_t = i_viqmfe
        i_viqmma_t = i_viqmma
        i_viqmur_t = i_viqmur
        i_ihpa_t   = i_ihpa
        return     = i_return.

    IF viqmel-qmnum IS INITIAL.
      READ TABLE i_return INTO return WITH KEY type = 'E'.
      message = return-message.
    ELSE.
      num_aviso = viqmel-qmnum.

      IF viqmel-charg IS INITIAL AND NOT riqs5-charg IS INITIAL.
        UPDATE qmel "#EC AOC_STD_TABLE
           SET charg = riqs5-charg
         WHERE qmnum = viqmel-qmnum.
      ENDIF.

      IF viqmel-qmtxt IS INITIAL AND NOT riqs5-qmtxt IS INITIAL.
        UPDATE qmel "#EC AOC_STD_TABLE
           SET qmtxt = riqs5-qmtxt
         WHERE qmnum = viqmel-qmnum.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD crear_nota.
    i_return = zcl_ap_gos=>crear_nota( contenido   = contenido
                                       clave       = qmnum
                                       tipo        = c_tipo_gos
                                       descripcion = descripcion ).
  ENDMETHOD.
  METHOD editar_st.
    IF NOT qmnum IS INITIAL.
      SET PARAMETER ID 'IQM' FIELD qmnum.
      CALL TRANSACTION 'QM02' AND SKIP FIRST SCREEN.       "#EC CI_CALLTA
    ENDIF.
  ENDMETHOD.
  METHOD f4_cod.
    DATA: i_seleccion TYPE TABLE OF qpk1cd,
          l_seleccion TYPE qpk1cd.

    CALL FUNCTION 'QPK1_GP_CODE_SELECTION'
      EXPORTING  i_katalogart           = catalogo
                 i_codegruppe           = grupo
*                 I_CODE                 = '*'
*                 I_SPRACHE              = SY-LANGU
*                 I_WINX1                = 10
*                 I_WINX2                = 70
*                 I_WINY1                = 5
*                 I_WINY2                = 25
*                 I_DISPLAY_MODE         =
*                 I_RETURN_IF_ONE        = 'X'
                 i_pickup_mode          = pickup
*                 I_RETURN_IF_MANY       =
*                 I_NO_USAGEINDICATION   =
*                 I_NO_AUTHORITY_CHECK   =
      TABLES     t_qpk1cdtab            = i_seleccion
*                 T_CODEGRPTAB           =
      EXCEPTIONS no_match_in_range      = 1
                 no_user_selection      = 2
                 no_authorization       = 3
                 no_selection_specified = 4
                 object_locked          = 5
                 lock_error             = 6
                 object_missing         = 7
                 OTHERS                 = 8.

    IF sy-subrc = 0.
      READ TABLE i_seleccion INTO l_seleccion INDEX 1.
      IF sy-subrc = 0.
        codigo = l_seleccion-code.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD f4_grp.
    DATA: g_riwo020tab TYPE TABLE OF riwo020,
          l_riwo020tab TYPE riwo020.
    DATA l_rnbr TYPE t352b-rbnr.
    DATA: g_codegrptab TYPE TABLE OF qpk1codegrp,
          l_codegrptab TYPE qpk1codegrp.
    DATA: i_seleccion TYPE TABLE OF qpk1cd,
          l_seleccion TYPE qpk1cd.

*--- Katalogtabelle leer -> nachlesen
    IF g_riwo020tab IS INITIAL.
      SELECT SINGLE rbnr FROM tq80
        INTO l_rnbr
        WHERE qmart = qmart.

      CALL FUNCTION 'CATALOGUE_SELECTION'
        EXPORTING  rbnr                    = l_rnbr
                   qmart                   = qmart
        TABLES     t_riwo020tab            = g_riwo020tab
        EXCEPTIONS cdgrp_not_valid_pattern = 1
                   OTHERS                  = 2.
      if sy-subrc ne 0.
      message 'Error recuperando catálogo' type 'E'.
      return.
      endif.
    ENDIF.

    CLEAR g_codegrptab.
    LOOP AT g_riwo020tab INTO l_riwo020tab WHERE qkatart = catalogo.
      l_codegrptab-codegruppe = l_riwo020tab-qcodegrp.
      APPEND l_codegrptab TO g_codegrptab.
    ENDLOOP.

    CALL FUNCTION 'QPK1_GP_CODE_SELECTION'
      EXPORTING  i_katalogart           = catalogo
                 i_codegruppe           = grupo
*                 I_CODE                 = '*'
*                 I_SPRACHE              = SY-LANGU
*                 I_WINX1                = 10
*                 I_WINX2                = 70
*                 I_WINY1                = 5
*                 I_WINY2                = 25
*                 I_DISPLAY_MODE         =
*                 I_RETURN_IF_ONE        = 'X'
                 i_pickup_mode          = pickup
*                 I_RETURN_IF_MANY       =
*                 I_NO_USAGEINDICATION   =
*                 I_NO_AUTHORITY_CHECK   =
      TABLES     t_qpk1cdtab            = i_seleccion
                 t_codegrptab           = g_codegrptab
      EXCEPTIONS no_match_in_range      = 1
                 no_user_selection      = 2
                 no_authorization       = 3
                 no_selection_specified = 4
                 object_locked          = 5
                 lock_error             = 6
                 object_missing         = 7
                 OTHERS                 = 8.

    IF sy-subrc = 0.
      READ TABLE i_seleccion INTO l_seleccion INDEX 1.
      IF sy-subrc = 0.
        codigo = l_seleccion-code.
        grupo  = l_seleccion-codegruppe.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_aviso.
    CALL FUNCTION 'READ_NOTIFICATION'
      EXPORTING  qmnum          = qmnum
      IMPORTING  indabschl      = indabschl
                 iviqmel        = viqmel
                 iriwo02        = riwo02
                 eriwo03        = riwo03
*    tables
*                 iviqmfe        = w_qmfe
*                 iviqmma        = w_qmma
*                 iviqmsm        = w_qmsm
*                 iviqmur        = w_qmur
      EXCEPTIONS invalid_number = 1
                 OTHERS         = 2.
  ENDMETHOD.
  METHOD get_avisos.
    FIELD-SYMBOLS <viqmel> TYPE viqmel.

    DATA: r_stat_si  TYPE RANGE OF j_status,
          o_qmnum    TYPE REF TO zcl_ap_rango,
          o_matnr    TYPE REF TO zcl_ap_rango,
          o_deviceid TYPE REF TO zcl_ap_rango,
          lr_status  TYPE lxhme_range_c4,
          l_istat    TYPE j_status,
          lr_stat    LIKE LINE OF r_stat_si,
          r_stat_no  TYPE RANGE OF j_status,
          l_objnr    TYPE jest-objnr.

    o_qmnum = NEW #( ).
    o_matnr = NEW #( ).
    o_deviceid = NEW #( ).

    IF NOT qmnum IS INITIAL.
      o_qmnum->set_eq( qmnum ).
    ENDIF.

    IF NOT matnr IS INITIAL.
      o_matnr->set_eq( matnr ).
    ENDIF.
    IF NOT deviceid IS INITIAL.
      o_deviceid->set_eq( deviceid ).
    ENDIF.

    CLEAR i_viqmel.
    SELECT * FROM viqmel
      INTO TABLE i_viqmel
      WHERE qmnum    IN r_qmnum
        AND qmnum    IN o_qmnum->rango
        AND qmart    IN r_qmart
        AND matnr    IN r_matnr
        AND matnr    IN o_matnr->rango
        AND deviceid IN r_deviceid
        AND deviceid IN o_deviceid->rango
        AND lifnum   IN r_lifnr
        AND qmcod    IN r_qmcod
        AND erdat    IN r_erdat
        AND strmn    IN r_strmn.

* Y filtramos valores en función del status
    IF r_status IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT r_status INTO lr_status.
      SELECT istat FROM tj02t                               "#EC *
        INTO l_istat
        UP TO 1 ROWS
        WHERE spras = sy-langu
          AND txt04 = lr_status-low
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR lr_stat.
      IF lr_status-sign = 'I' AND lr_status-option = 'EQ'.
        lr_stat-option = lr_status-option.
        lr_stat-sign   = lr_status-sign.
        lr_stat-low    = l_istat.
        APPEND lr_stat TO r_stat_si.
      ELSE.
        lr_stat-option = 'EQ'.
        lr_stat-sign   = 'I'.
        lr_stat-low    = l_istat.
        APPEND lr_stat TO r_stat_no.
      ENDIF.
    ENDLOOP.

    LOOP AT i_viqmel ASSIGNING <viqmel>.
      CONCATENATE 'QM' <viqmel>-qmnum INTO l_objnr.
      IF NOT r_stat_si IS INITIAL.
        SELECT SINGLE stat FROM jest                       "#EC *
          INTO l_istat
          WHERE objnr  = l_objnr
            AND stat  IN r_stat_si
            AND inact  = ''.
        IF sy-subrc <> 0.
          DELETE i_viqmel.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF NOT r_stat_no IS INITIAL.
        SELECT SINGLE stat FROM jest                       "#EC *
          INTO l_istat
          WHERE objnr  = l_objnr
            AND stat  IN r_stat_no
            AND inact  = ''.
        IF sy-subrc = 0.
          DELETE i_viqmel.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_descripcion_aviso.
    DATA l_qmel TYPE qmel.

    SELECT SINGLE qmtxt indtx FROM qmel
      INTO ( l_qmel-qmtxt, l_qmel-indtx )
      WHERE qmnum = qmnum.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF l_qmel-indtx = 'X'.
      texto = get_texto_string( qmnum = qmnum
                                id    = 'LTXT' ).
    ELSE.
      texto = l_qmel-qmtxt.
    ENDIF.
  ENDMETHOD.
  METHOD get_status_st.
    DATA: l_objnr     TYPE jest-objnr,
          l_user_line TYPE bsvx-sttxt.

    IF NOT objnr IS INITIAL.
      l_objnr = objnr.
    ELSE.
      CONCATENATE 'QM' qmnum INTO l_objnr.
    ENDIF.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
*                 CLIENT           = SY-MANDT
                 flg_user_stat    = usuario
                 objnr            = l_objnr
*                 ONLY_ACTIVE      = 'X'
                 spras            = sy-langu
                 bypass_buffer    = 'X'
      IMPORTING
*                 ANW_STAT_EXISTING =
*                 E_STSMA          =
                 line             = status
                 user_line        = l_user_line
*                 STONR            =
      EXCEPTIONS object_not_found = 1
                 OTHERS           = 2.

      if sy-subrc ne 0.
      message 'Error recuperando status' type 'S'.
      return.
      endif.
    IF usuario = 'X'.
      status = l_user_line.
    ENDIF.
  ENDMETHOD.
  METHOD get_status_usuario_st.
    DATA l_objnr TYPE jest-objnr.

    IF NOT objnr IS INITIAL.
      l_objnr = objnr.
    ELSE.
      CONCATENATE 'QM' qmnum INTO l_objnr.
    ENDIF.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
*                 CLIENT           = SY-MANDT
                 flg_user_stat    = 'X'
                 objnr            = l_objnr
*                 ONLY_ACTIVE      = 'X'
                 spras            = sy-langu
*                 BYPASS_BUFFER    = ' '
      IMPORTING
*                 ANW_STAT_EXISTING =
*                 E_STSMA          =
*                 line             = status
                 user_line        = status
*                 STONR            =
      EXCEPTIONS object_not_found = 1
                 OTHERS           = 2.

                       if sy-subrc ne 0.
      message 'Error recuperando status' type 'S'.
      endif.
  ENDMETHOD.
  METHOD get_text_cod.
    SELECT kurztext FROM qpct
      INTO texto
      UP TO 1 ROWS
      WHERE katalogart  = catalogo
        AND codegruppe  = grupo
        AND code        = codigo
        AND sprache     = idioma
        AND gueltigab  <= fecha
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_text_error.
    CLEAR mensaje.
    READ TABLE i_return INTO return INDEX 1.
    IF sy-subrc = 0.
      mensaje = return-message.
    ENDIF.
  ENDMETHOD.
  METHOD get_texto_acc_string.
    DATA l_name TYPE stxh-tdname.

    CONCATENATE qmnum manum INTO l_name.
    string = zcl_ap_textos=>get_texto_string( id     = id
                                              name   = l_name
                                              spras  = spras
                                              object = 'QMSM' ).
  ENDMETHOD.
  METHOD get_texto_pos_string.
    DATA l_name TYPE stxh-tdname.

    CONCATENATE qmnum fenum INTO l_name.
    string = zcl_ap_textos=>get_texto_string( id     = id
                                              name   = l_name
                                              spras  = spras
                                              object = 'QMFE' ).
  ENDMETHOD.
  METHOD get_texto_string.
    DATA l_name TYPE stxh-tdname.

    l_name = qmnum.
    string = zcl_ap_textos=>get_texto_string( id          = id
                                              name        = l_name
                                              spras       = spras
                                              object      = 'QMEL'
                                              memory      = 'X'
                                              elim_regexp = ':.()' ).
  ENDMETHOD.
  METHOD grabar_aviso_interno.
    IF i_return IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
      EXPORTING number      = cabecera-notif_no
      IMPORTING notifheader = cabecera
      TABLES    return      = i_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = 'X'.
  ENDMETHOD.
  METHOD leer_aviso.
    CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
      EXPORTING number             = qmnum
      IMPORTING notifheader_export = cabecera
                notifhdtext        = i_notifhdtext
      TABLES    notlongtxt         = i_textos
*                NOTITEM            =
*                NOTIFCAUS          =
                notifactv          = i_actividad
*                NOTIFTASK          =
                notifpartnr        = i_interlocutores
                return             = i_return.

    IF NOT i_return IS INITIAL.
      mensaje = get_text_error( ).
    ENDIF.
  ENDMETHOD.
  METHOD leer_nota.
    zcl_ap_gos=>leer_nota( EXPORTING clave       = qmnum
                                     tipo        = c_tipo_gos
                                     descripcion = descripcion
                           IMPORTING contenido   = contenido
                                     i_return    = i_return ).
  ENDMETHOD.
  METHOD modificar_aviso_qm.
    DATA: riqs5        TYPE riqs5,
          i_viqmma_new TYPE tt_rfc_viqmma,
          i_viqmma_mod TYPE tt_rfc_viqmma,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_viqmel_old TYPE viqmel.

    FIELD-SYMBOLS <qmma> TYPE rfc_viqmma.

    MOVE-CORRESPONDING viqmel TO riqs5.

    LOOP AT i_viqmma ASSIGNING <qmma>.
      IF <qmma>-manum IS INITIAL.
        CLEAR <qmma>-fenum.
        SELECT MAX( manum ) FROM qmma
          INTO <qmma>-manum
          WHERE qmnum = viqmel-qmnum.
        <qmma>-manum  = <qmma>-manum + 1.
        <qmma>-qmanum = <qmma>-manum.

        APPEND <qmma> TO i_viqmma_new.
      ELSE.
        APPEND <qmma> TO i_viqmma_mod.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'IQS4_MODIFY_NOTIFICATION'
      EXPORTING i_qmnum      = viqmel-qmnum
                i_riqs5_new  = riqs5
                i_commit     = commit
                i_wait       = 'X'
      IMPORTING e_viqmel_old = l_viqmel_old
                e_viqmel_new = viqmel
      TABLES
*                i_inlines_t  = i_inlines
*                i_viqmfe_t   = i_viqmfe
                i_viqmma_t   = i_viqmma_mod
                i_ihpa_t     = i_ihpa
                return       = i_return.

    READ TABLE i_return INTO return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      message = return-message.
    ELSE.
      IF NOT i_viqmma IS INITIAL.
        CALL FUNCTION 'IQS0_CREATE_VIQMMA'
          EXPORTING  i_post        = commit
                     i_qmnum       = viqmel-qmnum
                     i_reset       = space
          TABLES     e_viqmma      = i_viqmma_new
          EXCEPTIONS show_messages = 1
                     error_message = 2
                     OTHERS        = 3.

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD modificar_datos_aviso.
    " TODO: parameter I_TEXTOS is never used (ABAP cleaner)

    DATA: l_header_aux     TYPE bapi2080_nothdri,
          notifheader_x    TYPE bapi2080_nothdri_x,
          l_actividad      TYPE bapi2080_notactvi,
          l_actividad_x    TYPE bapi2080_notactvi_x,
          l_new            TYPE c LENGTH 1,
          l_actividad_orig TYPE bapi2080_notactve,
          i_actividad_new2 TYPE TABLE OF bapi2080_notactvi,
          i_actividad_new  TYPE TABLE OF bapi2080_notactvi,
          i_actividad_x    TYPE TABLE OF bapi2080_notactvi_x,
          l_qmnum          TYPE viqmel-qmnum,
          l_interlocutor   TYPE bapi2080_notpartnri.
* Como los interlocutores no se actualizan correctamente con la
* BAPI los actualizadmos directamente en tablas
    DATA: l_objnr TYPE ihpa-objnr,
          l_ihpa  TYPE ihpa.

    FIELD-SYMBOLS: <original>          TYPE any,
                   <modificacion>      TYPE any,
                   <campo_x>           TYPE any,
                   <original_comp>     TYPE any,
                   <modificacion_comp> TYPE any,
                   <campo_x_comp>      TYPE any.

    IF cabecera IS INITIAL.
      return.
    ELSE.
      MOVE-CORRESPONDING cabecera TO l_header_aux.
    ENDIF.

    ASSIGN: header TO <original>,
            l_header_aux TO <modificacion>,
            notifheader_x TO <campo_x>.

    WHILE sy-subrc = 0.
      ASSIGN COMPONENT sy-index OF STRUCTURE <original> TO <original_comp>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      ASSIGN COMPONENT sy-index OF STRUCTURE <modificacion> TO <modificacion_comp>.
      ASSIGN COMPONENT sy-index OF STRUCTURE <campo_x> TO <campo_x_comp>.
      IF <original_comp> <> <modificacion_comp>.
        <campo_x_comp> = 'X'.
      ENDIF.
    ENDWHILE.

*
    LOOP AT i_actividad INTO l_actividad.
      CLEAR: l_actividad_x,
             l_new.
      READ TABLE me->i_actividad INTO l_actividad_orig WITH KEY act_code = l_actividad-act_code.
      IF sy-subrc = 0.
        l_actividad_x-act_key = l_actividad_orig-act_key.
        IF l_actividad_orig-act_sort_no <> l_actividad-act_sort_no.
          l_actividad_x-act_sort_no = 'X'.
        ENDIF.
        IF l_actividad_orig-acttext <> l_actividad-acttext.
          l_actividad_x-acttext = 'X'.
        ENDIF.
        IF l_actividad_orig-act_codegrp <> l_actividad-act_codegrp.
          l_actividad_x-act_codegrp = 'X'.
        ENDIF.
        IF l_actividad_orig-act_code <> l_actividad-act_code.
          l_actividad_x-act_code = 'X'.
        ENDIF.
        IF l_actividad_orig-delete_flag = 'X'.
          l_actividad_x-act_sort_no = 'X'.
        ENDIF.
      ELSE.
        l_new = 'X'.
        l_actividad_x-act_key     = l_actividad-act_key.
        l_actividad_x-act_sort_no = 'X'.
        l_actividad_x-acttext     = 'X'.
        l_actividad_x-act_codegrp = 'X'.
        l_actividad_x-act_code    = 'X'.
      ENDIF.

      IF    l_actividad_x-act_sort_no <> ''
         OR l_actividad_x-acttext     <> ''
         OR l_actividad_x-act_codegrp <> ''
         OR l_actividad_x-act_code    <> ''.
        l_actividad-refobjectkey = cabecera-notif_no.

        IF l_new = 'X'.
          APPEND l_actividad TO i_actividad_new2.
        ELSE.
          APPEND l_actividad TO i_actividad_new.
          APPEND l_actividad_x TO i_actividad_x.
        ENDIF.
      ENDIF.
    ENDLOOP.

    l_qmnum = cabecera-notif_no.
    CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
      EXPORTING number        = l_qmnum
                notifheader   = header
                notifheader_x = notifheader_x
*    IMPORTING
*                notifheader_export = cabecera
      TABLES
*                NOTIFITEM     =
*                NOTIFITEM_X   =
*                NOTIFCAUS     =
*                NOTIFCAUS_X   =
                notifactv     = i_actividad_new
                notifactv_x   = i_actividad_x
*                NOTIFTASK     =
*                NOTIFTASK_X   =
*                NOTIFPARTNR   =
*                NOTIFPARTNR_X =
                return        = i_return.

    IF NOT i_actividad_new2 IS INITIAL.
      DELETE i_return WHERE number = '407'.

      l_qmnum = cabecera-notif_no.
      REFRESH i_return.
      CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
        EXPORTING number      = l_qmnum
                  notifheader = header
*                  TASK_DETERMINATION = ' '
*                  SENDER      =
*                  ORDERID     =
* IMPORTING
*                  NOTIFHDTEXT =
*                  NOTIFHEADER_EXPORT =
        TABLES
*                  NOTFULLTXT  =
*                  NOTITEM     =
*                  NOTIFCAUS   =
                  notifactv   = i_actividad_new2
*                  NOTIFTASK   =
*                  NOTIFPARTNR =
*                  KEY_RELATIONSHIPS =
                  return      = i_return.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = 'X'.

    IF NOT i_return IS INITIAL.
      CONCATENATE 'QM' cabecera-notif_no INTO l_objnr.
      DELETE FROM ihpa "#EC AOC_STD_TABLE
       WHERE objnr = l_objnr.

      LOOP AT i_interlocutores INTO l_interlocutor.
        CLEAR l_ihpa.
        MOVE-CORRESPONDING l_interlocutor TO l_ihpa.
        l_ihpa-objnr  = l_objnr.
        l_ihpa-obtyp  = 'QMI'.
        l_ihpa-parvw  = l_interlocutor-partn_role.
        l_ihpa-parnr  = l_interlocutor-partner.
        l_ihpa-aedat  = sy-datum.
        l_ihpa-erdat  = l_ihpa-aedat.
        l_ihpa-aezeit = sy-uzeit.
        l_ihpa-erzeit = l_ihpa-aezeit.
        l_ihpa-aenam  = sy-uname.
        l_ihpa-ernam  = l_ihpa-aenam.
        INSERT ihpa FROM l_ihpa. "#EC AOC_STD_TABLE
      ENDLOOP.

      grabar_aviso_interno( ).
    ELSE.
      mensaje = get_text_error( ).
    ENDIF.
  ENDMETHOD.
  METHOD set_status_usuario.
    DATA: l_stsma      TYPE tj30t-stsma,
          l_estat      TYPE tj30t-estat,
          l_jest       TYPE jest,
          l_usr_status TYPE bapi2078_notusrstati,
          i_return     TYPE TABLE OF bapiret2,
          l_return     TYPE bapiret2.

    IF estat IS INITIAL AND txt04 IS INITIAL.
      message = 'Informe nuevo status'(ins).
      return.
    ENDIF.

    SELECT stsma
      FROM                            tq80 JOIN qmel
           ON tq80~qmart = qmel~qmart "#EC CI_BUFFJOIN
      INTO l_stsma
      UP TO 1 ROWS
      WHERE qmnum = qmnum
      ORDER BY stsma.
    ENDSELECT.
    IF l_stsma IS INITIAL.
      message = 'No encuentro esquema'(nee).
      return.
    ENDIF.

    IF NOT estat IS INITIAL.
      l_estat = estat.
    ELSE.
      SELECT estat FROM tj30t
        INTO l_estat
        UP TO 1 ROWS
        WHERE stsma = l_stsma
          AND spras = spras
          AND txt04 = txt04
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc <> 0.
        message = 'Estatus no válido'(env).
      ENDIF.
    ENDIF.
    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE 'QM' qmnum INTO l_jest-objnr.
    SELECT SINGLE * FROM jest
      INTO l_jest
      WHERE objnr = l_jest-objnr
        AND stat  = l_estat
        AND inact = ''.
    IF sy-subrc = 0. " Si ya tiene el status no seguimos
      RETURN.
    ENDIF.

    l_usr_status-status_int = l_estat.
    SELECT SINGLE txt04 FROM tj30t
      INTO l_usr_status-status_ext
      WHERE stsma = l_stsma
        AND spras = spras
        AND estat = l_estat.
    l_usr_status-langu_iso = sy-langu.
    l_usr_status-langu     = l_usr_status-langu_iso.

    IF cambio_directo IS INITIAL.
      CALL FUNCTION 'BAPI_QUALNOT_CHANGEUSRSTAT'
        EXPORTING number     = qmnum
                  usr_status = l_usr_status
*                  SET_INACTIVE = ' '
*                  TESTRUN    = ' '
* IMPORTING
*                  SYSTEMSTATUS =
*                  USERSTATUS =
        TABLES    return     = i_return.

      READ TABLE i_return INTO l_return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        message = l_return-message.
      ELSE.
        zcl_ap_dev=>commit( ).
      ENDIF.
    ELSE.
      UPDATE jest "#EC AOC_STD_TABLE
        SET inact = 'X'
      WHERE objnr    = l_jest-objnr
        AND stat  LIKE 'E%'
        AND inact    = ''.

      l_jest-stat = l_estat.
      CLEAR l_jest-inact.
      MODIFY jest FROM l_jest. "#EC AOC_STD_TABLE
    ENDIF.
  ENDMETHOD.
  METHOD set_texto_string.
    DATA l_name TYPE stxh-tdname.

    l_name = qmnum.
    zcl_ap_textos=>save_texto_string( id     = id
                                      name   = l_name
                                      object = 'QMEL'
                                      string = string
                                      spras  = spras ).

    IF id = 'LTQM' AND NOT string IS INITIAL.
      SELECT SINGLE indtx FROM qmel
        INTO @DATA(indtx)
       WHERE qmnum = @qmnum.
      IF sy-subrc = 0 AND indtx IS INITIAL.
        UPDATE qmel "#EC AOC_STD_TABLE
           SET indtx = 'X'
         WHERE qmnum = qmnum.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_valor_clas.
    DATA: l_tabla TYPE bapi1003_key-objecttable,
          l_key   TYPE bapi1003_key-object,
          o_cld   TYPE REF TO zcl_ap_clasificacion.

    IF NOT fenum IS INITIAL.
      l_tabla = 'QMFE'.
      CONCATENATE qmnum fenum INTO l_key.
    ELSE.
      l_tabla = 'QMEL'.
      l_key = qmnum.
    ENDIF.

    o_cld = NEW #( objecttable = l_tabla
                   classnum    = classnum
                   classtype   = classtype ).

    o_cld->get_datos( objectkey        = l_key
                      unvaluated_chars = 'X' ).

    READ TABLE o_cld->i_return INTO o_cld->return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      mensaje = o_cld->return-message.
    ELSE.
      READ TABLE o_cld->i_return INTO o_cld->return WITH KEY id     = 'CL'
                                                             number = '732'.
      IF sy-subrc = 0.
        mensaje = o_cld->return-message.
      ENDIF.
    ENDIF.

    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    DELETE o_cld->i_car_char WHERE charact <> caract AND value_char IS INITIAL.
    DELETE o_cld->i_car_num WHERE charact <> caract AND value_from IS INITIAL AND value_to IS INITIAL.
    DELETE o_cld->i_car_curr WHERE charact <> caract AND value_from IS INITIAL AND value_to IS INITIAL.

    READ TABLE o_cld->i_car_char INTO o_cld->car_char WITH KEY charact = caract.
    IF sy-subrc = 0.
      o_cld->set_char( charact       = caract
                       value_char    = valor
                       value_neutral = valor ).
    ELSE.
      READ TABLE o_cld->i_car_num INTO o_cld->car_num WITH KEY charact = caract.
      IF sy-subrc = 0.
        o_cld->set_num( charact    = caract
                        value_from = valor ).
      ELSE.
        READ TABLE o_cld->i_car_curr INTO o_cld->car_curr WITH KEY charact = caract.
        IF sy-subrc = 0.
          o_cld->set_curr( charact    = caract
                           value_from = valor ).
        ENDIF.
      ENDIF.
    ENDIF.

    o_cld->set_datos( objectkey   = l_key
                      commit      = commit
                      dequeue_all = 'X' ).

    LOOP AT o_cld->i_return INTO o_cld->return WHERE type = 'E' AND number <> '117'.
      IF mensaje IS INITIAL.
        mensaje = o_cld->return-message.
      ELSE.
        CONCATENATE mensaje o_cld->return-message INTO mensaje SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF mensaje IS INITIAL.
      READ TABLE o_cld->i_return INTO o_cld->return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        mensaje = o_cld->return-message.
      ENDIF.
    ENDIF.

    o_cld->free( ).
  ENDMETHOD.
  METHOD visualizar_st.
    IF NOT qmnum IS INITIAL.
      SET PARAMETER ID 'IQM' FIELD qmnum.
      CALL TRANSACTION 'QM03' AND SKIP FIRST SCREEN.       "#EC CI_CALLTA
    ENDIF.
  ENDMETHOD.
