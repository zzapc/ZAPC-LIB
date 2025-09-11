class ZCL_AP_AVISO_PM definition
  public
  create public .

public section.

  data CABECERA type BAPI2080_NOTHDRE .
  data I_RETURN type BAPIRET2_T .
  data RETURN type BAPIRET2 .
  data I_ACTIVIDAD type ALM_ME_BAPI2080_NOTACTVE_T .
  data I_INTERLOCUTORES type ALM_ME_BAPI2080_NOTPARTNRE_T .
  data I_TEXTOS type ALM_ME_BAPI2080_NOTFULLTXTE_T .
  data I_CAUSAS type ALM_ME_BAPI2080_NOTCAUSE_T .
  data I_NOTIFHDTEXT type BAPI2080_NOTHDTXTE .

  methods LEER_AVISO
    importing
      !QMNUM type BAPI2080_NOTHDRE-NOTIF_NO
    returning
      value(MENSAJE) type BAPIRET2-MESSAGE .
  methods CREAR_AVISO
    importing
      !TIPO type BAPI2080-NOTIF_TYPE
      !HEADER type BAPI2080_NOTHDRI
      !I_ACTIVIDAD type ALM_ME_BAPI2080_NOTACTVI_T optional
      !I_INTERLOCUTORES type ALM_ME_BAPI2080_NOTPARTNRI_T optional
      !I_TEXTOS type ALM_ME_BAPI2080_NOTFULLTXTI_T optional
      !I_ITEMS type ALM_ME_BAPI2080_NOTITEMI_T optional
      !I_CAUSAS type ALM_ME_BAPI2080_NOTCAUSI_T optional
    returning
      value(NUM_AVISO) type BAPI2080_NOTHDRE-NOTIF_NO .
  methods BORRA_DATOS_AVISO .
  methods MODIFICAR_DATOS_AVISO
    importing
      !HEADER type BAPI2080_NOTHDRI
      !I_ACTIVIDAD type ALM_ME_BAPI2080_NOTACTVI_T optional
      !I_INTERLOCUTORES type ALM_ME_BAPI2080_NOTPARTNRI_T optional
      !I_TEXTOS type ALM_ME_BAPI2080_NOTFULLTXTI_T optional
      !I_CAUSAS type ALM_ME_BAPI2080_NOTCAUSI_T optional
    returning
      value(MENSAJE) type BAPIRET2-MESSAGE
    exceptions
      ERROR .
  methods CERRAR_AVISO
    importing
      !FECHA_CIERRE type SY-DATUM default SY-DATUM
      !HORA_CIERRE type SY-UZEIT default SY-UZEIT
      !QMNUM type QMNUM optional
      !COMMIT type ABAP_BOOL default 'X'
    returning
      value(MENSAJE) type BAPIRET2-MESSAGE .
  methods GET_TEXT_ERROR
    returning
      value(MENSAJE) type BAPIRET2-MESSAGE .
  class-methods VISUALIZAR_ST
    importing
      !QMNUM type QMEL-QMNUM .
  class-methods GET_STATUS_ST
    importing
      !QMNUM type VIQMEL-QMNUM optional
      !OBJNR type QMOBJNR optional
      !USUARIO type FLAG default ''
      !BYPASS_BUFFER type ABAP_BOOL default ''
      !SPRAS type SY-LANGU default SY-LANGU
    preferred parameter QMNUM
    returning
      value(STATUS) type BSVX-STTXT .
  class-methods IMPRIMIR_FORMULARIO
    importing
      !QMNUM type QMNUM
      !WORKPAPER type WWORKPAPER-WORKPAPER
      !DEVICE type ITCPP-TDDEVICE default 'SCREEN'
      !TDDEST type RSPOPNAME default ''
      !GET_AREA_OTF type ANY default ''
      !GET_AREA_PDF type ANY default ''
      !LANGU type SY-LANGU default SY-LANGU
    exporting
      !I_OTFDATA type TT_ITCOO
      !PDF type FPCONTENT .
  class-methods SAVE_TEXTO
    importing
      !QMNUM type QMNUM
      !TEXTO type ANY
      !SPRAS type SPRAS default SY-LANGU
      !ID type STXH-TDID default 'LTXT'
      !QMTXT type ANY default '' .
  class-methods GET_INTERLOCUTOR
    importing
      !QMNUM type QMNUM
      !PARVW type ANY
    exporting
      value(PARNR) type ANY
      !NOMBRE type ANY .
  class-methods GET_TEXTO_STRING
    importing
      !QMNUM type QMNUM
      !ID type STXH-TDID default 'LTXT'
      !SPRAS type STXH-TDSPRAS default SY-LANGU
    returning
      value(STRING) type STRING .
  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS grabar_aviso_interno.
endclass. "ZCL_AP_AVISO_PM definition
class ZCL_AP_AVISO_PM implementation.
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
  METHOD cerrar_aviso.
    DATA: l_qmnum    TYPE qmnum,
          l_syststat TYPE bapi2080_notsti.

    CLEAR: i_return,
           mensaje.

    IF qmnum IS INITIAL.
      l_qmnum = cabecera-notif_no.
    ELSE.
      l_qmnum = qmnum.
    ENDIF.

    l_syststat-langu   = sy-langu.
    l_syststat-refdate = fecha_cierre.
    l_syststat-reftime = hora_cierre.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
      EXPORTING number   = l_qmnum
                syststat = l_syststat
*                TESTRUN  = ' '
* IMPORTING
*                SYSTEMSTATUS =
*                USERSTATUS =
      TABLES    return   = i_return.

    mensaje = get_text_error( ).

    IF mensaje IS INITIAL AND commit = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING wait = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD crear_aviso.
    DATA: i_act TYPE alm_me_bapi2080_notactvi_t,
          i_int TYPE alm_me_bapi2080_notpartnri_t,
          i_ite TYPE alm_me_bapi2080_notitemi_t,
          i_cau TYPE alm_me_bapi2080_notcausi_t.

    CLEAR i_return.
    i_act = i_actividad.
    i_int = i_interlocutores.
    i_ite = i_items.
    i_cau = i_causas.

    CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE'
      EXPORTING
*                EXTERNAL_NUMBER    =
                notif_type         = tipo
                notifheader        = header
*                TASK_DETERMINATION = ' '
*                SENDER             =
*                ORDERID            =
      IMPORTING notifheader_export = cabecera
      TABLES    notitem            = i_ite
                notifcaus          = i_cau
                notifactv          = i_act
*                NOTIFTASK          =
                notifpartnr        = i_int
                longtexts          = i_textos
*                KEY_RELATIONSHIPS  =
                return             = i_return.

    IF i_return IS INITIAL.
      grabar_aviso_interno( ).
      num_aviso = cabecera-notif_no.
    ENDIF.
  ENDMETHOD.
  METHOD get_interlocutor.
    DATA: l_ihpa   TYPE ihpa,
          l_pernr  TYPE persno,
          l_nombre TYPE string.

    CONCATENATE 'QM' qmnum INTO l_ihpa-objnr.
    SELECT SINGLE obtyp parnr adrnr
      FROM ihpa
      INTO CORRESPONDING FIELDS OF l_ihpa
      WHERE objnr    = l_ihpa-objnr
        AND parvw    = parvw
        AND kzloesch = ''.
    IF sy-subrc <> 0 OR l_ihpa-parnr IS INITIAL.
      RETURN.
    ENDIF.

    parnr = l_ihpa-parnr.
    IF l_ihpa-adrnr IS INITIAL.
      l_pernr = l_ihpa-parnr.
      __poner_ceros l_pernr.
      SELECT SINGLE ename FROM pa0001
        INTO l_nombre
        WHERE pernr  = l_pernr
          AND endda >= sy-datum
          AND begda <= sy-datum.
      IF sy-subrc = 0.
        nombre = l_nombre.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_status_st.
    DATA: l_objnr TYPE jest-objnr,
          l_sttxt TYPE bsvx-sttxt.

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
                 spras            = spras
                 bypass_buffer    = bypass_buffer
      IMPORTING
*                 ANW_STAT_EXISTING =
*                 E_STSMA          =
                 line             = status
                 user_line        = l_sttxt
*                 STONR            =
      EXCEPTIONS object_not_found = 1
                 OTHERS           = 2.

    IF usuario = 'X'.
      status = l_sttxt.
    ENDIF.
  ENDMETHOD.
  METHOD get_text_error.
    CLEAR mensaje.
    READ TABLE i_return INTO return INDEX 1.
    IF sy-subrc = 0.
      mensaje = return-message.
    ENDIF.
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
  METHOD imprimir_formulario.
    DATA: l_tabla    TYPE tabname,
          viqmel     TYPE viqmel,
          i_qpk1cd   TYPE TABLE OF qpk1cd,
          l_qpk1cd   TYPE qpk1cd,
          l_iqkat    TYPE qkat,
          iqkat      TYPE TABLE OF qkat,
          iviqmfe    TYPE TABLE OF wqmfe,
          wqmfe      TYPE wqmfe,
          iviqmma    TYPE TABLE OF wqmma,
          iviqmsm    TYPE TABLE OF wqmsm,
          iviqmur    TYPE TABLE OF wqmur,
          ihpad_tab  TYPE TABLE OF ihpad,
          ihpad      TYPE ihpad,
          l_pernr    TYPE persno,
          wworkpaper TYPE wworkpaper,
          iworkpaper TYPE TABLE OF wworkpaper,
          riwo1      TYPE riwo1,
          iloa       TYPE iloa,
          riwo00     TYPE riwo00,
          wqmur      TYPE wqmur,
          " TODO: variable is assigned but never used (ABAP cleaner)
          riwo02     TYPE riwo02,
          riwo03     TYPE riwo03,
          i_tq80     TYPE tq80,
          tq15t      TYPE tq15t.

    l_tabla = 'VIQMEL'.
    SELECT SINGLE *
      FROM (l_tabla)
      INTO viqmel
      WHERE qmnum = qmnum.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'QPK1_GP_CODE_ARRAY_SELECTION'
      EXPORTING  i_katalogart           = 'A'
                 i_codegruppe           = '*'
*                 I_CODE                 = '*'
*                 I_SPRACHE              = SY-LANGU
                 i_no_usageindication   = 'X'
                 i_no_authority_check   = 'X'
      TABLES     t_qpk1cdtab            = i_qpk1cd
*                 T_CODEGRPTAB           =
      EXCEPTIONS no_match_in_range      = 1
                 no_authorization       = 2
                 no_selection_specified = 3
                 object_locked          = 4
                 lock_error             = 5
                 object_missing         = 6
                 OTHERS                 = 7.
    IF sy-subrc = 0.
      LOOP AT i_qpk1cd INTO l_qpk1cd.
        MOVE-CORRESPONDING l_qpk1cd TO l_iqkat.
        l_iqkat-versioncd = l_qpk1cd-version.
        l_iqkat-gueltigcd = l_qpk1cd-gueltigab.
        APPEND l_iqkat TO iqkat.
      ENDLOOP.
    ENDIF.

    l_tabla = 'QMFE'.
    SELECT *
      FROM (l_tabla)
      INTO CORRESPONDING FIELDS OF TABLE iviqmfe
      WHERE qmnum = viqmel-qmnum
      ORDER BY PRIMARY KEY.
    LOOP AT iviqmfe INTO wqmfe WHERE prtkz = ''.
      wqmfe-prtkz = 'X'.
      MODIFY iviqmfe FROM wqmfe.
    ENDLOOP.

    l_tabla = 'QMMA'.
    SELECT *
      FROM (l_tabla)
      INTO CORRESPONDING FIELDS OF TABLE iviqmma
      WHERE qmnum = viqmel-qmnum
      ORDER BY PRIMARY KEY.

    l_tabla = 'QMSM'.
    SELECT *
      FROM (l_tabla)
      INTO CORRESPONDING FIELDS OF TABLE iviqmsm
      WHERE qmnum = viqmel-qmnum
      ORDER BY PRIMARY KEY.

    l_tabla = 'QMUR'.
    SELECT *
      FROM (l_tabla)
      INTO CORRESPONDING FIELDS OF TABLE iviqmur
      WHERE qmnum = viqmel-qmnum
      ORDER BY PRIMARY KEY.

    l_tabla = 'IHPA'.
    SELECT *
      FROM (l_tabla)
      INTO CORRESPONDING FIELDS OF TABLE ihpad_tab
      WHERE objnr = viqmel-objnr
      ORDER BY PRIMARY KEY.

    LOOP AT ihpad_tab INTO ihpad.
      l_pernr = ihpad-parnr.
      ihpad-name2 = zcl_ap_empleado=>get_nombre( l_pernr ).
      MODIFY ihpad_tab FROM ihpad.
    ENDLOOP.

    IF tddest IS INITIAL.
      wworkpaper-tddest = zcl_ap_usuario=>get_impresora( ).
    ELSE.
      SELECT SINGLE padest FROM tsp03
        INTO wworkpaper-tddest
        WHERE padest = tddest.
      IF sy-subrc <> 0.
        wworkpaper-tddest = zcl_ap_usuario=>get_impresora( ).
      ENDIF.
    ENDIF.
    wworkpaper-pm_appl    = 'N'.
    wworkpaper-workpaper  = workpaper.
    wworkpaper-selected   = 'X'.
    wworkpaper-print_lang = langu.
    APPEND wworkpaper TO iworkpaper.

    MOVE-CORRESPONDING viqmel TO riwo1.

    l_tabla = 'ILOA'.
    SELECT SINGLE *
      FROM (l_tabla)
      INTO iloa
      WHERE iloan = viqmel-iloan.

    READ TABLE iviqmfe INTO wqmfe INDEX 1.
    IF sy-subrc = 0.
      SELECT kurztext FROM qpct
        INTO riwo00-txtcdfe
        UP TO 1 ROWS
        WHERE katalogart = 'C'
          AND codegruppe = wqmfe-fegrp
          AND code       = wqmfe-fecod
          AND sprache    = sy-langu
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      riwo00-txtfe = wqmfe-fecod.
      wqmfe-txtcdgr = riwo00-txtcdfe.
      MODIFY iviqmfe FROM wqmfe INDEX 1.
    ENDIF.

    READ TABLE iviqmur INTO wqmur INDEX 1.
    IF sy-subrc = 0.
      SELECT kurztext FROM qpct
        INTO riwo00-txtcdur
        UP TO 1 ROWS
        WHERE katalogart = '5'
          AND codegruppe = wqmur-urgrp
          AND code       = wqmur-urcod
          AND sprache    = sy-langu
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      riwo00-txtur = wqmur-urcod.
      wqmur-txtcd = riwo00-txtcdur.
      MODIFY iviqmur FROM wqmur INDEX 1.
    ENDIF.

    MOVE-CORRESPONDING viqmel TO riwo00.
    CALL FUNCTION 'READ_NOTIFICATION'
      EXPORTING  qmnum          = riwo00-qmnum
                 i_delete       = 'X'
      IMPORTING  ibautx         = riwo1-bautx
                 ieqkt          = riwo1-eqtxt
                 ipltxt         = riwo1-pltxt
                 iriwo02        = riwo02
                 eriwo03        = riwo03
      EXCEPTIONS invalid_number = 1
                 OTHERS         = 2.

    riwo00-gewrk   = riwo03-arbpl.
    riwo00-swerk   = viqmel-arbplwerk.
    riwo00-ktext   = riwo03-arbpltx.
    riwo00-matktx  = riwo03-matktx.
    riwo00-kuname1 = riwo03-kuname1.
    riwo00-lfname1 = riwo03-lfname1.
    riwo00-buname  = riwo03-buname.
    riwo00-txtsa   = riwo03-txtcdsa.
    riwo00-qmart   = viqmel-qmart.
    riwo1-sdate = sy-datum.
    riwo1-szeit = sy-uzeit.
    riwo1-iloak = '1'.
    riwo1-sinit = 'X'.
    IF     riwo1-equnr = space
       AND riwo1-tplnr = space
       AND riwo1-iloai = 'X'.
      riwo1-sinit = 'X'.
    ENDIF.
    riwo1-chkat = 'X'.
    riwo1-vrgng = 'PMOC'.
    riwo1-nrobj = viqmel-qmnum.
    riwo1-aktyp = 'A'.
    SELECT SINGLE * FROM tq80
      INTO i_tq80
      WHERE qmart = viqmel-qmart.
    riwo1-info_wind = i_tq80-info_wind.
    IF i_tq80-qmtyp = '03'.
      riwo1-service = 'X'.
    ENDIF.
    CALL FUNCTION 'READ_CATALOGUE_TEXT'
      EXPORTING iq80     = i_tq80
      IMPORTING txtkatfe = riwo00-txtktfe
                txtkatma = riwo00-txtktma
                txtkatot = riwo00-txtktot
                txtkatur = riwo00-txtktur
                txtkatsa = riwo00-txtktsa
                txtkatsm = riwo00-txtktsm.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
*                 CLIENT            = SY-MANDT
                 flg_user_stat     = 'X'
                 objnr             = viqmel-objnr
*                 ONLY_ACTIVE       = 'X'
                 spras             = sy-langu
      IMPORTING  anw_stat_existing = riwo00-astex
                 line              = riwo00-sttxt
                 user_line         = riwo00-astxt
      EXCEPTIONS object_not_found  = 1
                 OTHERS            = 2.
    IF i_tq80-otkat <> space.
      SELECT SINGLE * FROM tq15t
        INTO tq15t
        WHERE sprache    = sy-langu
          AND katalogart = i_tq80-otkat.
      IF         sy-subrc          = 0
         AND NOT tq15t-schlagwort IS INITIAL.
        riwo00-otswortpk = tq15t-schlagwort.
        riwo00-otrahmen  = tq15t-schlagwort.
        riwo00-otrahmen  = tq15t-schlagwort.
        riwo00-otwortkat = tq15t-schlagwort.
        riwo00-otwortgrp = tq15t-schlagwort.
      ENDIF.
    ENDIF.
    IF i_tq80-fekat <> space.
      SELECT SINGLE * FROM tq15t
        INTO tq15t
        WHERE sprache    = sy-langu
          AND katalogart = i_tq80-fekat.
      IF         sy-subrc          = 0
         AND NOT tq15t-schlagwort IS INITIAL.
        riwo00-feswortpk = tq15t-schlagwort.
        riwo00-ferahmen  = tq15t-schlagwort.
        riwo00-fewortkat = tq15t-schlagwort.
        riwo00-fewortgrp = tq15t-schlagwort.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'PM_NOTIFICATION_PRINT_CONTROL'
      EXPORTING  device               = device
                 iviqmel              = viqmel
                 print_language       = langu
                 qmnum                = viqmel-qmnum
                 riwo00               = riwo00
                 riwo1                = riwo1
*                 CAUFVD               = ' '
                 iloa                 = iloa
*                 O_RIWO1              = ' '
*                 RQM00                = ' '
*                 IT_AOBJECT_UI        =
*                 IV_SESS_TZONE        =
      TABLES     iqkat                = iqkat
                 iviqmfe              = iviqmfe
                 iviqmma              = iviqmma
                 iviqmsm              = iviqmsm
                 iviqmur              = iviqmur
                 iworkpaper           = iworkpaper
                 ihpad_tab            = ihpad_tab
*                 IAFFHD               =
*                 IAFVGD               =
*                 IRESBD               =
*                 IRIPW0               =
*                 O_IHPAD_TAB          =
*                 IHSG_TAB             =
*                 IHGNS_TAB            =
*                 KBEDP_TAB            =
*                 OP_PRINT_TAB         =
      EXCEPTIONS no_workpapers_passed = 1
                 error_message        = 999 " Cualquier otra excepción!
                 OTHERS               = 2.

    IF device = 'PDF'.
      IF NOT get_area_otf IS INITIAL.
        IMPORT i_otfdata TO i_otfdata FROM MEMORY ID get_area_otf.
        FREE MEMORY ID get_area_otf.
      ENDIF.

      IF NOT get_area_pdf IS INITIAL.
        IMPORT pdf TO pdf FROM MEMORY ID get_area_pdf.
        FREE MEMORY ID get_area_pdf.
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD leer_aviso.
    CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
      EXPORTING number             = qmnum
      IMPORTING notifheader_export = cabecera
                notifhdtext        = i_notifhdtext
      TABLES    notlongtxt         = i_textos
*                NOTITEM            =
                notifcaus          = i_causas
                notifactv          = i_actividad
*                NOTIFTASK          =
                notifpartnr        = i_interlocutores
                return             = i_return.

    IF NOT i_return IS INITIAL.
      mensaje = get_text_error( ).
    ENDIF.
  ENDMETHOD.
  METHOD modificar_datos_aviso.
    DATA: l_header_aux     TYPE bapi2080_nothdri,
          notifheader_x    TYPE bapi2080_nothdri_x,
          l_actividad      TYPE bapi2080_notactvi,
          l_actividad_x    TYPE bapi2080_notactvi_x,
          l_new            TYPE c LENGTH 1,
          l_actividad_orig TYPE bapi2080_notactve,
          i_actividad_new2 TYPE TABLE OF bapi2080_notactvi,
          i_actividad_new  TYPE TABLE OF bapi2080_notactvi,
          i_actividad_x    TYPE TABLE OF bapi2080_notactvi_x,
          l_causas         TYPE bapi2080_notcausi,
          l_causas_x       TYPE bapi2080_notcausi_x,
          l_causas_orig    TYPE bapi2080_notcausi,
          i_causas_new2    TYPE TABLE OF bapi2080_notcausi,
          i_causas_new     TYPE TABLE OF bapi2080_notcausi,
          i_causas_x       TYPE TABLE OF bapi2080_notcausi_x,
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
      RETURN.
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

    LOOP AT i_causas INTO l_causas.
      CLEAR: l_causas_x,
             l_new.
      READ TABLE me->i_causas INTO l_causas_orig WITH KEY cause_key = l_causas-cause_key.
      IF sy-subrc = 0.
        l_causas_x-cause_key = l_causas_orig-cause_key.
        IF l_causas_orig-cause_codegrp <> l_causas-cause_codegrp.
          l_causas_x-cause_codegrp = 'X'.
        ENDIF.
        IF l_causas_orig-cause_code <> l_causas-cause_code.
          l_causas_x-cause_code = 'X'.
        ENDIF.
      ELSE.
        l_new = 'X'.
        l_causas_x-cause_key     = l_causas_x-cause_key.
        l_causas_x-cause_sort_no = 'X'.
        l_causas_x-cause_codegrp = 'X'.
        l_causas_x-cause_code    = 'X'.
      ENDIF.

      IF     l_causas_x-cause_sort_no = ''
         AND l_causas_x-cause_codegrp = ''
         AND l_causas_x-cause_code    = ''.
        CONTINUE.
      ENDIF.

*      l_causas_x-CAUSE_KEY = cabecera-notif_no.

      IF l_new = 'X'.
        IF l_causas-cause_key IS INITIAL.
          l_causas-cause_key = '0001'.
        ENDIF.
        IF l_causas-cause_sort_no IS INITIAL.
          l_causas-cause_sort_no = '0001'.
        ENDIF.
        IF l_causas-item_key IS INITIAL.
          l_causas-item_key = '0001'.
        ENDIF.
        IF l_causas-item_sort_no IS INITIAL.
          l_causas-item_sort_no = '0001'.
        ENDIF.

        APPEND l_causas TO i_causas_new2.
      ELSE.
        APPEND l_causas TO i_causas_new.
        APPEND l_causas_x TO i_causas_x.
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
                notifcaus     = i_causas_new
                notifcaus_x   = i_causas_x
                notifactv     = i_actividad_new
                notifactv_x   = i_actividad_x
*                NOTIFTASK     =
*                NOTIFTASK_X   =
*                NOTIFPARTNR   =
*                NOTIFPARTNR_X =
                return        = i_return.

    IF NOT i_actividad_new2[] IS INITIAL OR NOT i_causas_new2[] IS INITIAL.
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
                  notifcaus   = i_causas_new2
                  notifactv   = i_actividad_new2
*                  NOTIFTASK   =
*                  NOTIFPARTNR =
*                  KEY_RELATIONSHIPS =
                  return      = i_return.
    ENDIF.

    IF i_return[] IS INITIAL.
      CONCATENATE 'QM' cabecera-notif_no INTO l_objnr.
      DELETE FROM ihpa
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
        INSERT ihpa FROM l_ihpa.
      ENDLOOP.

      grabar_aviso_interno( ).
    ELSE.
      mensaje = get_text_error( ).
    ENDIF.
  ENDMETHOD.
  METHOD save_texto.
    DATA: l_string TYPE string,
          l_qmtxt  TYPE qmtxt.

    l_string = texto.
    zcl_ap_textos=>save_texto_string( id     = id
                                      name   = qmnum
                                      spras  = spras
                                      object = 'QMEL'
                                      string = l_string ).

    IF id = 'LTXT'.
      IF qmtxt IS INITIAL.
        l_qmtxt = texto.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN l_qmtxt WITH ` `.
      ELSE.
        l_qmtxt = qmtxt.
      ENDIF.
      UPDATE qmel
         SET qmtxt = l_qmtxt
             indtx = 'X'
       WHERE qmnum = qmnum.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar_st.
    IF NOT qmnum IS INITIAL.
      SET PARAMETER ID 'IQM' FIELD qmnum.
      CALL TRANSACTION 'IW23' AND SKIP FIRST SCREEN.       "#EC CI_CALLTA
    ENDIF.
  ENDMETHOD.
