CLASS zcl_ap_empleado DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
* Estructura importación report rplpay00
      BEGIN OF t_devengos,
        pernr             TYPE pernr-pernr,
        name              TYPE c LENGTH 35,
        job_title         TYPE c LENGTH 15,
*tarif
        trfar             TYPE p0008-trfar,
        trfgb             TYPE p0008-trfgb,
        trfgr             TYPE p0008-trfgr,
        trfst             TYPE p0008-trfst,
* act_pay
        infotype          TYPE pskey-infty,
        infotype_text     TYPE t582s-itext,
        subty             TYPE pskey-subty,
        subty_text        TYPE t591s-stext,
* subty_text(20),
        objps             TYPE pskey-objps,
        begda             TYPE pskey-begda,
        endda             TYPE pskey-endda,
        reason            TYPE pshd1-preas, " Reason for changes
        reason_text       TYPE t530f-rtext,
*--- record for pay lines in 0008
        wage_type         TYPE p0008-lga01,
        wage_type_text    TYPE t512t-lgtxt,
        wage_type_amount  TYPE p0008-bet01,
        currency          TYPE sy-waers,
        op_ind_wage_types TYPE p0008-opk01,
        number            TYPE p0008-anz01,
        time_meas_unit    TYPE c LENGTH 7,
*--- record for pay of employee
*            indbw,
        perc_diffr        TYPE p LENGTH 8 DECIMALS 2,
        color             TYPE c LENGTH 3,
      END OF t_devengos.
    TYPES tt_devengos   TYPE TABLE OF t_devengos.
    TYPES tt_bapi7013_1 TYPE TABLE OF bapi7013_1.
    TYPES:
      BEGIN OF t_cond_fechas,
        clave1 TYPE c LENGTH 40,
        clave2 TYPE c LENGTH 40,
        clave3 TYPE c LENGTH 40,
        clave4 TYPE c LENGTH 40,
        clave5 TYPE c LENGTH 40,
        begda  TYPE begda,
        endda  TYPE endda,
        linea  TYPE string,
      END OF t_cond_fechas.
    TYPES tt_cond_fechas TYPE TABLE OF t_cond_fechas.

    DATA datos_basicos TYPE zest_empleado.

    CONSTANTS op_create TYPE pspar-actio VALUE 'INS' ##NO_TEXT.
    CONSTANTS op_change TYPE pspar-actio VALUE 'MOD' ##NO_TEXT.

    DATA return TYPE bapireturn1.

    CONSTANTS c_plvar TYPE plvar VALUE '01' ##NO_TEXT.
    CONSTANTS c_molga TYPE molga VALUE '04' ##NO_TEXT.

    METHODS constructor
      IMPORTING pernr TYPE pa0000-pernr OPTIONAL
                begda TYPE pa0000-begda DEFAULT sy-datum
                endda TYPE pa0000-endda DEFAULT sy-datum.

    METHODS get_datos_maestros
      IMPORTING begda TYPE pa0000-begda DEFAULT sy-datum
                endda TYPE pa0000-endda DEFAULT sy-datum.

    CLASS-METHODS get_nombre
      IMPORTING pernr         TYPE pa0001-pernr
                begda         TYPE pa0001-begda DEFAULT sy-datum
                endda         TYPE pa0001-endda DEFAULT sy-datum
      RETURNING VALUE(nombre) TYPE pa0001-ename.

    CLASS-METHODS get_datos_basicos
      IMPORTING pernr                TYPE pa0000-pernr        OPTIONAL
                begda                TYPE pa0000-begda        DEFAULT sy-datum
                endda                TYPE pa0000-endda        DEFAULT sy-datum
                o_cache              TYPE REF TO zcl_ap_cache OPTIONAL
      PREFERRED PARAMETER pernr
      RETURNING VALUE(datos_basicos) TYPE zest_empleado.

    CLASS-METHODS visualizar_st
      IMPORTING pernr          TYPE pa0001-pernr
                begda          TYPE pa0001-begda DEFAULT sy-datum
                endda          TYPE pa0001-endda DEFAULT sy-datum
                infty          TYPE any          DEFAULT ''
                subty          TYPE any          DEFAULT ''
                resumen        TYPE abap_bool    DEFAULT ''
                actualizar     TYPE abap_bool    DEFAULT ''
                objps          TYPE pa0001-objps OPTIONAL
                seqnr          TYPE pa0001-seqnr OPTIONAL
      RETURNING VALUE(mensaje) TYPE bapireturn1-message.

    CLASS-METHODS get_comunicacion_st
      IMPORTING pernr        TYPE pa0000-pernr OPTIONAL
                subty        TYPE pa0105-subty OPTIONAL
                begda        TYPE pa0000-begda DEFAULT sy-datum
                endda        TYPE pa0000-endda DEFAULT sy-datum
      PREFERRED PARAMETER pernr
      RETURNING VALUE(usrid) TYPE pa0105-usrid_long.

    METHODS crear_cualificacion
      IMPORTING otype         TYPE hrp1000-otype
                objid         TYPE hrp1000-objid
                begda         TYPE hrp1000-begda
                endda         TYPE hrp1000-endda
                rating        TYPE bapiqualific_tab-rating
      RETURNING VALUE(return) TYPE bapireturn1.

    METHODS inicio.

    METHODS cambiar_empleado
      IMPORTING pernr TYPE pa0001-pernr OPTIONAL
                begda TYPE pa0001-begda DEFAULT sy-datum
                endda TYPE pa0001-endda DEFAULT sy-datum
      PREFERRED PARAMETER pernr.

    CLASS-METHODS bloquear_empleado
      IMPORTING pernr         TYPE pa0000-pernr
      RETURNING VALUE(return) TYPE bapireturn1.

    CLASS-METHODS desbloquear_empleado
      IMPORTING pernr         TYPE pa0000-pernr
      RETURNING VALUE(return) TYPE bapireturn1.

    CLASS-METHODS get_pernr_from_uname
      IMPORTING !uname       TYPE uname DEFAULT sy-uname
                begda        TYPE begda DEFAULT sy-datum
                endda        TYPE endda DEFAULT sy-datum
      PREFERRED PARAMETER uname
      RETURNING VALUE(pernr) TYPE persno.

    CLASS-METHODS get_uname_from_pernr
      IMPORTING begda        TYPE begda  DEFAULT sy-datum
                endda        TYPE endda  DEFAULT sy-datum
                VALUE(pernr) TYPE persno OPTIONAL
      PREFERRED PARAMETER pernr
      RETURNING VALUE(uname) TYPE uname.

    METHODS borrar_cualificacion
      IMPORTING otype         TYPE hrp1000-otype
                objid         TYPE hrp1000-objid
                begda         TYPE hrp1000-begda
                endda         TYPE hrp1000-endda
                rating        TYPE bapiqualific_tab-rating
      RETURNING VALUE(return) TYPE bapireturn1.

    CLASS-METHODS get_email
      IMPORTING pernr        TYPE pa0001-pernr OPTIONAL
                !uname       TYPE sy-uname     OPTIONAL
      PREFERRED PARAMETER pernr
      RETURNING VALUE(email) TYPE string.

    CLASS-METHODS get_edad
      IMPORTING pernr       TYPE persno
                fecha       TYPE dats DEFAULT sy-datum
      RETURNING VALUE(edad) TYPE empl_age.

    CLASS-METHODS get_tipo_contrato
      IMPORTING begda TYPE begda DEFAULT sy-datum
                endda TYPE endda DEFAULT sy-datum
                pernr TYPE persno
      EXPORTING idseg TYPE pes_idseg
                ttext TYPE t5e2t-ttext
                fijo  TYPE abap_bool.

    CLASS-METHODS get_disponibilidad
      IMPORTING pernr         TYPE persno
                begda         TYPE begda
                endda         TYPE endda
      RETURNING VALUE(i_disp) TYPE hrtem_timelist.

    CLASS-METHODS es_disponible_en_horario
      IMPORTING pernr             TYPE persno
                begda             TYPE begda
                beguz             TYPE beguz
                enduz             TYPE enduz
      RETURNING VALUE(disponible) TYPE abap_bool.

    CLASS-METHODS get_horario_trabajo
      IMPORTING pernr          TYPE pa0001-pernr
                begda          TYPE pa0001-begda
                endda          TYPE pa0001-endda
      RETURNING VALUE(t_ptpsp) TYPE ptpsp_tab.

    CLASS-METHODS borrar_infotipo
      IMPORTING pernr          TYPE pa0001-pernr
                begda          TYPE pa0001-begda DEFAULT sy-datum
                endda          TYPE pa0001-endda DEFAULT sy-datum
                infty          TYPE p0001-infty  OPTIONAL
                subty          TYPE p0001-subty  OPTIONAL
      RETURNING VALUE(mensaje) TYPE bapireturn1-message.

    CLASS-METHODS get_num_candidato
      IMPORTING pernr        TYPE persno
                fecha        TYPE d DEFAULT sy-datum
      RETURNING VALUE(aplno) TYPE aplno.

    CLASS-METHODS get_calificaciones
      IMPORTING pernr                   TYPE persno
                begda                   TYPE begda DEFAULT '19000101'
                endda                   TYPE endda DEFAULT '99991231'
      RETURNING VALUE(i_calificaciones) TYPE zt_calificaciones.

    CLASS-METHODS get_datos_ampliados
      IMPORTING pernr        TYPE pa0000-pernr        OPTIONAL
                begda        TYPE pa0000-begda        DEFAULT sy-datum
                endda        TYPE pa0000-endda        DEFAULT sy-datum
                o_cache      TYPE REF TO zcl_ap_cache OPTIONAL
      PREFERRED PARAMETER pernr
      RETURNING VALUE(datos) TYPE zest_empl_mas.

    CLASS-METHODS get_cualificaciones
      IMPORTING pernr                    TYPE persno
                begda                    TYPE begda DEFAULT '19000101'
                endda                    TYPE endda DEFAULT '99991231'
      RETURNING VALUE(i_cualificaciones) TYPE hrpd_profq_tab.

    CLASS-METHODS tiene_cualificacion
      IMPORTING pernr     TYPE persno
                begda     TYPE begda DEFAULT sy-datum
                endda     TYPE endda DEFAULT sy-datum
                cualif    TYPE zcualif
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS condensar_fechas
      CHANGING fechas TYPE tt_cond_fechas.

    CLASS-METHODS get_superiores
      IMPORTING pernr             TYPE persno    OPTIONAL
                fecha             TYPE dats      DEFAULT sy-datum
                consider_vac_pos  TYPE abap_bool DEFAULT 'X'
                otype             TYPE otype     DEFAULT 'P'
                valido_hoy        TYPE abap_bool OPTIONAL
      RETURNING VALUE(superiores) TYPE hrobject_t.

    CLASS-METHODS get_superior
      IMPORTING pernr           TYPE persno
                fecha           TYPE dats DEFAULT sy-datum
      RETURNING VALUE(superior) TYPE persno.

    CLASS-METHODS get_pernr_from_perid
      IMPORTING perid        TYPE any   OPTIONAL
                begda        TYPE begda DEFAULT sy-datum
                endda        TYPE endda DEFAULT sy-datum
      EXPORTING VALUE(pernr) TYPE persno
                !message     TYPE bapi_msg.

    CLASS-METHODS eliminar_bloqueo
      IMPORTING pernr            TYPE persno
      RETURNING VALUE(bloqueado) TYPE abap_bool.

    CLASS-METHODS get_texto_infotipo
      IMPORTING pernr       TYPE persno
                infty       TYPE infty
                fecha       TYPE dats  DEFAULT sy-datum
                subty       TYPE subty DEFAULT ''
                endda       TYPE dats  DEFAULT '00000000'
      RETURNING VALUE(text) TYPE it_notes.

    CLASS-METHODS get_texto_infotipo_string
      IMPORTING pernr         TYPE persno
                infty         TYPE infty
                fecha         TYPE dats  DEFAULT sy-datum
                subty         TYPE subty DEFAULT ''
                endda         TYPE dats  DEFAULT '00000000'
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS get_horario_trabajo_detallado
      IMPORTING pernr            TYPE pa0001-pernr
                begda            TYPE pa0001-begda
                endda            TYPE pa0001-endda
      RETURNING VALUE(i_horario) TYPE pwsdayint_tab.

    CLASS-METHODS set_texto_infotipo_string
      IMPORTING pernr          TYPE persno
                infty          TYPE infty
                fecha          TYPE dats   DEFAULT sy-datum
                endda          TYPE dats   DEFAULT '00000000'
                subty          TYPE subty  DEFAULT ''
                !string        TYPE string DEFAULT ''
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS set_texto_infotipo
      IMPORTING pernr          TYPE persno
                infty          TYPE infty
                fecha          TYPE dats  DEFAULT sy-datum
                endda          TYPE dats  DEFAULT '00000000'
                subty          TYPE subty DEFAULT ''
                !text          TYPE it_notes
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS get_timeoverview
      IMPORTING pernr     TYPE pa0001-pernr
                begda     TYPE pa0001-begda
                endda     TYPE pa0001-endda
      EXPORTING i_return  TYPE bapiret2_t
                i_tiempos TYPE tt_bapi7013_1.

    CLASS-METHODS set_fichaje
      IMPORTING pernr          TYPE persno
                fecha          TYPE dats
                hora           TYPE uzeit
                satza          TYPE any DEFAULT ''
                terid          TYPE any DEFAULT ''
                dallf          TYPE any DEFAULT '='
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS del_fichaje
      IMPORTING pernr          TYPE persno
                fecha          TYPE dats
                hora           TYPE uzeit
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS get_devengos
      IMPORTING pernr      TYPE persno
                fecha      TYPE dats      DEFAULT sy-datum
                lista      TYPE abap_bool DEFAULT ''
      EXPORTING i_devengos TYPE tt_devengos.

    CLASS-METHODS get_fecha_it0041
      IMPORTING pernr               TYPE persno
                fecha               TYPE dats  DEFAULT sy-datum
                clase_fecha         TYPE datar
                endda               TYPE endda OPTIONAL
      EXPORTING pa0041              TYPE pa0041
                indice              TYPE numc2
      RETURNING VALUE(fecha_salida) TYPE dats.

    CLASS-METHODS set_fecha_it0041
      IMPORTING pernr       TYPE persno
                fecha       TYPE dats DEFAULT sy-datum
                clase_fecha TYPE datar
                fecha_mod   TYPE dats
      EXPORTING indice      TYPE numc2
                pa0041      TYPE pa0041
                !message    TYPE bapi_msg.

    CLASS-METHODS actualizar_infotipo
      IMPORTING datos     TYPE any
                infty     TYPE infty
                operation TYPE pspar-actio
                !visible  TYPE abap_bool DEFAULT ''
      EXPORTING !message  TYPE bapi_msg
                !return   TYPE bapireturn1
                !key      TYPE bapipakey.

    CLASS-METHODS condensar
      CHANGING it TYPE table.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA o_empleado TYPE REF TO cl_pt_employee.
    DATA i_p0000    TYPE tim_p0000_tab.
    DATA i_p0001    TYPE tim_p0001_tab.
    DATA i_p0002    TYPE tim_p0002_tab.
    DATA i_p0007    TYPE tim_p0007_tab.
    DATA i_p0008    TYPE tim_p0008_tab.
    DATA pernr      TYPE pa0000-pernr.
    DATA begda      TYPE pa0000-begda.
    DATA endda      TYPE pa0000-endda.
endclass. "ZCL_AP_EMPLEADO definition
class ZCL_AP_EMPLEADO implementation.
  METHOD actualizar_infotipo.
    CLEAR: key, message, return.

    ASSIGN COMPONENT 'PERNR' OF STRUCTURE datos TO FIELD-SYMBOL(<pernr>).
    IF sy-subrc <> 0.
      message = 'Estructura de entrada incorrecta'.
      RETURN.
    ENDIF.
    DATA(l_infty) = infty.
    ASSIGN COMPONENT 'INFTY' OF STRUCTURE datos TO FIELD-SYMBOL(<infty>).
    IF sy-subrc = 0.
      IF l_infty IS INITIAL.
        l_infty = <infty>.
      ELSEIF l_infty <> <infty> AND <infty> <> ''.
        mESSAGE = 'Incoherencia en infotipos'.
        RETURN.
      ENDIF.
    ELSE.
      message = 'Estructura de entrada incorrecta'.
      RETURN.
    ENDIF.
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE datos TO FIELD-SYMBOL(<subty>).
    IF sy-subrc <> 0.
      message = 'Estructura de entrada incorrecta'.
      RETURN.
    ENDIF.
    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE datos TO FIELD-SYMBOL(<begda>).
    IF sy-subrc <> 0.
      message = 'Estructura de entrada incorrecta'.
      RETURN.
    ENDIF.
    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE datos TO FIELD-SYMBOL(<endda>).
    IF sy-subrc <> 0.
      message = 'Estructura de entrada incorrecta'.
      RETURN.
    ENDIF.
    ASSIGN COMPONENT 'OBJPS' OF STRUCTURE datos TO FIELD-SYMBOL(<objps>).
    IF sy-subrc <> 0.
      message = 'Estructura de entrada incorrecta'.
      RETURN.
    ENDIF.
    ASSIGN COMPONENT 'SEQNR' OF STRUCTURE datos TO FIELD-SYMBOL(<seqnr>).
    IF sy-subrc <> 0.
      message = 'Estructura de entrada incorrecta'.
      RETURN.
    ENDIF.

    IF <pernr> IS INITIAL.
      message = 'Informe nº de empleado'.
      RETURN.
    ENDIF.

    DATA(l_return) = zcl_ap_empleado=>bloquear_empleado( <pernr> ).
    IF l_return-type = 'E'.
      message = l_return-message.
      return = l_return.
      RETURN.
    ENDIF.

    IF visible = 'X'.
      DATA(dialog_mode) = '2'.
    ENDIF.

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = l_infty
        number        = <pernr>
        subtype       = <subty>
        validityend   = <endda>
        validitybegin = <begda>
        objectid      = <objps>
        recordnumber  = <seqnr>
        record        = datos
        operation     = operation
        nocommit      = ''
        dialog_mode   = dialog_mode
      IMPORTING
        key           = key
        return        = l_return
      EXCEPTIONS
        error_message = 999
        OTHERS        = 1.
    IF sy-subrc <> 0.
      IF l_return-type <> 'E'.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ENDIF.
    ENDIF.
    IF l_return-type = 'E'.
      message = l_return-message.
      return = l_return.
    ENDIF.
    zcl_ap_empleado=>desbloquear_empleado( <pernr> ).
  ENDMETHOD.
  METHOD bloquear_empleado.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = pernr
      IMPORTING
        return = return.
  ENDMETHOD.
  METHOD borrar_cualificacion.
* Estructura BAPI cualificac.(tabla perfiles)

    DATA: l_profile        TYPE bapiqualific_tab,
* Estructura BAPI cualificac.(tabla perfiles)
          i_profile_delete TYPE STANDARD TABLE OF bapiqualific_tab,
          i_profile_add    TYPE STANDARD TABLE OF bapiqualific_tab.
    DATA l_sobid TYPE bapiqualific-sobid. " ID del objeto vinculado

    CLEAR l_profile.
    l_profile-obj_id    = objid.
    l_profile-begda     = begda.
    l_profile-endda     = endda.
    l_profile-rating    = rating.
    l_profile-user_name = sy-uname.
    APPEND l_profile TO i_profile_delete.

    l_sobid = pernr.

    CALL FUNCTION 'BAPI_QUALIPROF_CHANGE'
      EXPORTING
        plvar          = '01'
        otype          = 'P'
        sobid          = l_sobid
      IMPORTING
        return         = return
      TABLES
        profile_add    = i_profile_add
        profile_delete = i_profile_delete.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.
  METHOD borrar_infotipo.
    DATA: l_tabla    TYPE c LENGTH 20,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_pernr    TYPE persno,
          o_bi       TYPE REF TO zcl_ap_batch_input,
          l_pantalla TYPE bdc_prog,
          l_mensaje  TYPE bapireturn1-message.

    CONCATENATE 'PA' infty INTO l_tabla.
    SELECT SINGLE pernr FROM (l_tabla)
      INTO l_pernr
     WHERE pernr = pernr
       AND subty = subty
       AND endda = endda
       AND begda = begda.
    IF sy-subrc <> 0.
      CLEAR mensaje.
    ELSE.
      o_bi = NEW #( ).

      o_bi->inicio( ).

* Pantalla acceso PA30
      o_bi->dynpro( program = 'SAPMP50A' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=DEL' ).
      o_bi->campos( campo = 'RP50G-PERNR' valor = pernr ).
      o_bi->campos( campo = 'RP50G-BEGDA' valor = begda ).
      o_bi->campos( campo = 'RP50G-ENDDA' valor = endda ).
      o_bi->campos( campo = 'RP50G-CHOIC' valor = infty ).
      o_bi->campos( campo = 'RP50G-SUBTY' valor = subty ).

      IF infty = '2001'.
        l_pantalla = 'MP200000'.
      ELSE.
        CONCATENATE 'MP' infty '00' INTO l_pantalla.
      ENDIF.
      o_bi->dynpro( program = l_pantalla dynpro = '2000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=UPDL' ).

      l_mensaje = o_bi->llamar_transaccion( tcode = 'PA30' modo = 'E' ).

      SELECT SINGLE pernr FROM (l_tabla)
        INTO l_pernr
       WHERE pernr = pernr
         AND subty = subty
         AND endda = endda
         AND begda = begda.
      IF sy-subrc <> 0.
        CLEAR mensaje.
      ELSE.
        mensaje = l_mensaje.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD cambiar_empleado.
    inicio( ).

    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
  ENDMETHOD.
  METHOD condensar.
    DATA: l_cont   TYPE char1,
          i_key    TYPE apb_lpd_t_key_value,
          l_fechas TYPE t_cond_fechas,
          i_fechas TYPE tt_cond_fechas.

    DATA(i_campos) = zcl_ap_dev=>get_fieldcatalog( tabla = it ).
    LOOP AT i_campos ASSIGNING FIELD-SYMBOL(<campo>)
         WHERE name <> 'BEGDA' AND name <> 'ENDDA'.
      l_cont = l_cont + 1.
      IF l_cont > 5.
        MESSAGE 'Sólo es posible condensar 5 campos' TYPE 'E'.
      ENDIF.
      APPEND VALUE #( key = |CLAVE{ l_cont }| value = <campo>-name  ) TO i_key.
    ENDLOOP.
    LOOP AT it ASSIGNING FIELD-SYMBOL(<it>).
      CLEAR l_fechas.
      MOVE-CORRESPONDING <it> TO l_fechas.
      LOOP AT i_key ASSIGNING FIELD-SYMBOL(<key>).
        ASSIGN COMPONENT <key>-key OF STRUCTURE l_fechas TO FIELD-SYMBOL(<destino>).
        ASSIGN COMPONENT <key>-value OF STRUCTURE <it> TO FIELD-SYMBOL(<origen>).
        <destino> = <origen>.
      ENDLOOP.
      APPEND l_fechas TO i_fechas.
    ENDLOOP.
    zcl_ap_empleado=>condensar_fechas( CHANGING fechas = i_fechas ).
    CLEAR it.
    LOOP AT i_fechas ASSIGNING FIELD-SYMBOL(<fechas>).
      APPEND INITIAL LINE TO it ASSIGNING <it>.
      MOVE-CORRESPONDING <fechas> TO <it>.
      LOOP AT i_key ASSIGNING <key>.
        ASSIGN COMPONENT <key>-key OF STRUCTURE <fechas> TO <origen>.
        ASSIGN COMPONENT <key>-value OF STRUCTURE <it> TO <destino>.
        <destino> = <origen>.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD condensar_fechas.
    DATA: i_fechas    TYPE tt_cond_fechas,
          l_fechas    TYPE t_cond_fechas,
          l_fecha_ant TYPE t_cond_fechas,
          l_endda     TYPE endda.

    i_fechas = fechas.
    CLEAR fechas.

    LOOP AT i_fechas INTO l_fechas.
      IF l_fecha_ant IS INITIAL.
        l_fecha_ant = l_fechas.
      ELSE.
        l_endda = l_fecha_ant-endda + 1.
        IF    l_endda            <> l_fechas-begda
           OR l_fecha_ant-clave1 <> l_fechas-clave1
           OR l_fecha_ant-clave2 <> l_fechas-clave2
           OR l_fecha_ant-clave3 <> l_fechas-clave3
           OR l_fecha_ant-clave4 <> l_fechas-clave4
           OR l_fecha_ant-clave5 <> l_fechas-clave5.
          APPEND l_fecha_ant TO fechas.
          l_fecha_ant = l_fechas.
        ELSE.
          l_fecha_ant-endda = l_fechas-endda.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF NOT l_fecha_ant IS INITIAL.
      APPEND l_fecha_ant TO fechas.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    cambiar_empleado( pernr = pernr
                      begda = begda
                      endda = endda ).
  ENDMETHOD.
  METHOD crear_cualificacion.
* Estructura BAPI cualificac.(tabla perfiles)

    DATA: l_profile        TYPE bapiqualific_tab,
          i_profile_add    TYPE STANDARD TABLE OF bapiqualific_tab,
* Estructura BAPI cualificac.(tabla perfiles)
          i_profile_delete TYPE STANDARD TABLE OF bapiqualific_tab.
    DATA l_sobid TYPE bapiqualific-sobid. " ID del objeto vinculado

    CLEAR l_profile.
    l_profile-obj_id    = objid.
    l_profile-begda     = begda.
    l_profile-endda     = endda.
    l_profile-rating    = rating.
    l_profile-user_name = sy-uname.
    APPEND l_profile TO i_profile_add.

    l_sobid = pernr.

    CALL FUNCTION 'BAPI_QUALIPROF_CHANGE'
      EXPORTING
        plvar          = '01'
        otype          = 'P'
        sobid          = l_sobid
      IMPORTING
        return         = return
      TABLES
        profile_add    = i_profile_add
        profile_delete = i_profile_delete.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.
  METHOD del_fichaje.
    DATA: i_del_teven      TYPE TABLE OF teven,
          i_del_teven_more TYPE TABLE OF teven_more,
          ins_teven        TYPE teven,
          i_ins_teven      TYPE TABLE OF teven,
          del_teven_more   TYPE teven_more,
          ins_teven_more   TYPE teven_more,
          i_ins_teven_more TYPE TABLE OF teven_more.

    CLEAR message.

    SELECT * FROM ('TEVEN')
      INTO CORRESPONDING FIELDS OF TABLE i_del_teven
     WHERE pernr = pernr
       AND ldate = fecha
       AND ltime = hora
       AND stokz = ''.
    IF sy-subrc <> 0.
      message = |No existe el fichaje { fecha DATE = USER } { hora TIME = USER }|.
    ENDIF.

    SELECT * FROM ('TEVEN')
      INTO CORRESPONDING FIELDS OF TABLE i_del_teven_more
     WHERE pernr = pernr
       AND ldate = fecha
       AND ltime = hora
       AND stokz = ''.

    CLEAR: sy-msgid, sy-msgno, sy-msgty, sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.

    CALL FUNCTION 'HR_TMW_DB_UPDATE_TEVENT'
      TABLES
        del_teven      = i_del_teven
        ins_teven      = i_ins_teven
        del_teven_more = i_del_teven_more
        ins_teven_more = i_ins_teven_more
      EXCEPTIONS
        insert_failed  = 1
        update_failed  = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      IF sy-msgty IS INITIAL.
        message = |Error borrando fichaje { fecha DATE = USER } { hora TIME = USER }|.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ENDIF.
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
    ENDIF.
  ENDMETHOD.
  METHOD desbloquear_empleado.
    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = pernr
      IMPORTING
        return = return.
  ENDMETHOD.
  METHOD eliminar_bloqueo.
    DATA hrpersonee TYPE hrpersonee_s.

    bloqueado = zcl_ap_utils=>comprobar_bloqueo( tabla = 'PREL'
                                                 clave = sy-mandt
                                                 clave2 = pernr
                                                 clave3 = '*'
                                                 borrar = 'X' ).
    CALL FUNCTION 'HR_PERSONEE_GETPERSON'
      EXPORTING
        iv_employee_id    = pernr
        iv_with_authority = space
      IMPORTING
        es_hrpersonee     = hrpersonee
      EXCEPTIONS
        error_message     = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando empleado'(ere) TYPE 'S'.
    ENDIF.

    IF NOT hrpersonee-personid IS INITIAL.
      bloqueado = zcl_ap_utils=>comprobar_bloqueo( tabla = 'PLOGI'
                                                   clave = sy-mandt
                                                   clave2 = '01'
                                                   clave3 = 'CP'
                                                   clave4 = hrpersonee-personid
                                                   borrar = 'X' ).
    ENDIF.
  ENDMETHOD.
  METHOD es_disponible_en_horario.
    DATA: i_disp         TYPE TABLE OF timelist,
          l_abs_finicio  TYPE c LENGTH 14,
          l_abs_ffin     TYPE c LENGTH 14,
          l_f1           TYPE d,
          l_time         TYPE timelist,
          l_ini          TYPE c LENGTH 1,
          l_fin          TYPE c LENGTH 1,
          l_disp_finicio TYPE c LENGTH 14,
          l_hfin         TYPE beguz,
          l_disp_ffin    TYPE c LENGTH 14,
          l_f2           TYPE d.

    i_disp = get_disponibilidad( pernr = pernr
                                 begda = begda
                                 endda = begda ).
    DELETE i_disp WHERE avail <> 1.

    IF i_disp IS INITIAL.
      RETURN.
    ENDIF.

    l_abs_finicio(8) = begda.
    l_abs_finicio+8(6) = beguz.
    IF enduz >= beguz.
      l_abs_ffin(8) = begda.
    ELSE.
      l_f1 = begda + 1.
      l_abs_ffin(8) = l_f1.
    ENDIF.
    l_abs_ffin+8(6) = enduz.

    LOOP AT i_disp INTO l_time.
      CLEAR: l_ini, l_fin.
      AT FIRST.
        l_ini = 'X'.
      ENDAT.
      AT LAST.
        l_fin = 'X'.
      ENDAT.
      IF l_ini = 'X'.
        l_disp_finicio(8) = l_time-date.
        l_disp_finicio+8(6) = l_time-btime.
      ENDIF.

      IF l_fin = 'X'.
        l_hfin = l_hfin + l_time-btime + l_time-hours * 3600 ##NUMBER_OK.
        IF l_hfin >= l_disp_finicio+8(6).
          l_disp_ffin(8) = l_disp_finicio(8).
        ELSE.
          l_f1 = l_disp_finicio(8).
          l_f2 = l_f1 + 1.
          l_disp_ffin(8) = l_f2.
        ENDIF.
        l_disp_ffin+8(6) = l_hfin.
      ENDIF.
    ENDLOOP.

    IF NOT (    l_abs_finicio < l_disp_finicio
             OR l_abs_ffin    > l_disp_ffin ).
      disponible = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD get_calificaciones.
    DATA: l_empleado  TYPE hrsobid,
          i_empleados TYPE TABLE OF hrsobid.

    CLEAR l_empleado.
    l_empleado-plvar = c_plvar.
    l_empleado-otype = 'P'.
    l_empleado-sobid = pernr.
    APPEND l_empleado TO i_empleados.

    CALL FUNCTION 'RHPA_APPRAISEES_APP_READ'
      EXPORTING
        begda        = begda
        endda        = endda
*       WITH_STEXT   = 'X'
*       WITH_ADD_INFO       = 'X'
      TABLES
        appraisees   = i_empleados
        appraisals   = i_calificaciones
      EXCEPTIONS
        no_authority = 1
        undefined    = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD get_comunicacion_st.
    DATA l_usrid TYPE pa0105-usrid.

    CLEAR usrid.
    SELECT usrid, usrid_long
      FROM pa0105
      INTO (@l_usrid, @usrid)
      UP TO 1 ROWS
      WHERE pernr  = @pernr
        AND subty  = @subty
        AND begda <= @endda
        AND endda >= @begda
      ORDER BY endda DESCENDING.
    ENDSELECT.

    IF usrid IS INITIAL AND NOT l_usrid IS INITIAL.
      usrid = l_usrid.
    ENDIF.
  ENDMETHOD.
  METHOD get_cualificaciones.
    DATA: l_empleado  TYPE hrsobid,
          i_empleados TYPE TABLE OF hrsobid.

    CLEAR l_empleado.
    l_empleado-plvar = c_plvar.
    l_empleado-otype = 'P'.
    l_empleado-sobid = pernr.
    APPEND l_empleado TO i_empleados.

    CALL FUNCTION 'RHPP_Q_PROFILE_READ'
      EXPORTING
        begda            = begda
        endda            = endda
*       WITH_STEXT       = 'X'
*       WITH_QK_INFO     = 'X'
        check_note       = 'X'
      TABLES
        objects          = i_empleados
*       ERR_OBJECTS      =
        profile          = i_cualificaciones
      EXCEPTIONS
        no_authority     = 1
        wrong_otype      = 2
        object_not_found = 3
        undefined        = 4
        OTHERS           = 5.

    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando cualificaciones'(erc) TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_datos_ampliados.
    DATA: l_datos_basicos TYPE zest_empleado,
          l_pa0000        TYPE pa0000,
          l_pa0001        TYPE pa0001,
          l_pa0002        TYPE pa0002,
          l_pa0006        TYPE pa0006,
          l_pa0007        TYPE pa0007,
          l_pa0016        TYPE pa0016,
          l_pa0061        TYPE pa0061.

    IF NOT o_cache IS INITIAL.
      o_cache->get_cache_mem( EXPORTING tabla = 'DATOS_AMPL' clave = pernr clave2 = begda clave3 = endda
                              IMPORTING xstring = datos encontrado = o_cache->enc ).
      IF NOT o_cache->enc IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    l_datos_basicos = get_datos_basicos( pernr = pernr
                                         begda = begda
                                         endda = endda
                                         o_cache = o_cache ).
    CLEAR datos.
    MOVE-CORRESPONDING l_datos_basicos TO datos.

    SELECT massg massn stat2 FROM pa0000
    INTO CORRESPONDING FIELDS OF l_pa0000
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc = 0.
      datos-massn = l_pa0000-massn.
      SELECT SINGLE mntxt FROM  t529t
        INTO datos-mntxt
       WHERE sprsl = sy-langu
         AND massn = datos-massn.

      datos-massg = l_pa0000-massg.
      SELECT SINGLE mgtxt FROM  t530t
        INTO datos-mgtxt
       WHERE sprsl = sy-langu
         AND massn = datos-massn
         AND massg = datos-massg.

      datos-stat2 = l_pa0000-stat2.
      SELECT SINGLE text1 FROM  t529u
        INTO datos-sttxt
       WHERE sprsl = sy-langu
         AND statn = '2'
         AND statv = datos-stat2.
    ENDIF.

    SELECT ansvh FROM pa0001
    INTO CORRESPONDING FIELDS OF l_pa0001
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc = 0.
      datos-ansvh = l_pa0001-ansvh.
      SELECT SINGLE atx FROM t542t
        INTO datos-atx
       WHERE spras = sy-langu
         AND molga = c_molga
         AND ansvh = l_pa0001-ansvh.
    ENDIF.

    SELECT famst gbdat gesch nach2 nachn natio perid vorna aedtm FROM pa0002
    INTO CORRESPONDING FIELDS OF l_pa0002
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc = 0.
      IF l_pa0002-aedtm > datos-aedtm.
        datos-aedtm = l_pa0002-aedtm.
      ENDIF.
      datos-id_type = l_pa0002-perid+19(1).
      IF l_pa0002-perid(2) = 'ES'.
        datos-nif = l_pa0002-perid+2(13).
      ELSE.
        datos-nif = l_pa0002-perid(13).
      ENDIF.
      datos-gbdat = l_pa0002-gbdat.
      datos-natio = l_pa0002-natio.
      SELECT SINGLE natio FROM  t005t
        INTO datos-natio_t
       WHERE spras = sy-langu
         AND land1 = l_pa0002-natio.
      datos-famst = l_pa0002-famst.

      SELECT SINGLE ftext FROM t502t
        INTO datos-fatxt
       WHERE sprsl = sy-langu
         AND famst = datos-famst.
      CASE datos-fatxt.
        WHEN 'sol.'. datos-fatxt = 'Soltero'(sol).
        WHEN 'cas.'. datos-fatxt = 'Casado'(cas).
        WHEN 'viu.'. datos-fatxt = 'Viudo'(viu).
        WHEN 'div.'. datos-fatxt = 'Divorciado'(div).
        WHEN 'sep.'. datos-fatxt = 'Separado'(sep).
        WHEN 'Par.H.'. datos-fatxt = 'Pareja de hecho'(phe).
      ENDCASE.
      datos-gesch   = l_pa0002-gesch.
      datos-gesch_t = zcl_ap_utils=>get_texto_dominio( dominio = 'GESCH' valor = datos-gesch ).

      datos-vorna   = l_pa0002-vorna.
      datos-nachn   = l_pa0002-nachn.
      datos-nach2   = l_pa0002-nach2.

    ENDIF.

    SELECT telnr FROM pa0006
    INTO CORRESPONDING FIELDS OF l_pa0006
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc = 0.
      datos-telnr = l_pa0006-telnr.
    ENDIF.

    SELECT empct FROM pa0007
    INTO CORRESPONDING FIELDS OF l_pa0007
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc = 0.
      datos-empct = l_pa0007-empct.
    ENDIF.

    SELECT ctedt cttyp aedtm FROM pa0016
    INTO CORRESPONDING FIELDS OF l_pa0016
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc = 0.
      IF l_pa0016-aedtm > datos-aedtm.
        datos-aedtm = l_pa0016-aedtm.
      ENDIF.
      datos-cttyp = l_pa0016-cttyp.
      datos-cttxt = zcl_ap_dev=>get_descripcion( tabla = 'T547S' valor = datos-cttyp ).
      datos-ctedt = l_pa0016-ctedt.
    ENDIF.

    SELECT berkt grcot natss pesoc aedtm FROM pa0061
    INTO CORRESPONDING FIELDS OF l_pa0061
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc = 0.
      datos-berkt = l_pa0061-berkt.
      IF l_pa0061-aedtm > datos-aedtm.
        datos-aedtm = l_pa0061-aedtm.
      ENDIF.

      SELECT SINGLE ctext FROM  t5e8s
        INTO datos-berkt_t
       WHERE sprsl = sy-langu
         AND mocat = '01'
         AND categ = datos-berkt.
      IF sy-subrc <> 0.
        SELECT ctext FROM  t5e8s
          INTO datos-berkt_t
          UP TO 1 ROWS
         WHERE sprsl = sy-langu
           AND categ = datos-berkt
          ORDER BY PRIMARY KEY.
        ENDSELECT.
      ENDIF.

      datos-pesoc = l_pa0061-pesoc.
      SELECT SINGLE ttext FROM t5e2t
        INTO datos-pesoc_t
       WHERE sprsl = sy-langu
         AND ticon = datos-pesoc
         AND modal = 'N'.
      IF sy-subrc <> 0.
        SELECT ttext FROM t5e2t
          INTO datos-pesoc_t
          UP TO 1 ROWS
         WHERE sprsl = sy-langu
           AND ticon = datos-pesoc
         ORDER BY PRIMARY KEY.
        ENDSELECT.
      ENDIF.

      datos-grcot = l_pa0061-grcot.
      SELECT SINGLE text FROM  t5e4jt
        INTO datos-grcot_t
       WHERE spras = sy-langu
         AND tpccc = datos-pesoc
         AND grcot = datos-grcot.
      IF sy-subrc <> 0.
        SELECT text FROM  t5e4jt
          UP TO 1 ROWS
          INTO datos-grcot_t
         WHERE spras = sy-langu
           AND grcot = datos-grcot
         ORDER BY PRIMARY KEY.
        ENDSELECT.
      ENDIF.

      CONCATENATE l_pa0061-natss(2) '/' l_pa0061-natss+2(8) '/'
                  l_pa0061-natss+10(2) INTO datos-ss SEPARATED BY space.
    ENDIF.

    datos-aplno = get_num_candidato( pernr = pernr ).

    IF NOT o_cache IS INITIAL.
      o_cache->set_cache_mem( tabla = 'DATOS_AMPL' clave = pernr clave2 = begda clave3 = endda xstring = datos ).
    ENDIF.
  ENDMETHOD.
  METHOD get_datos_basicos.
    DATA: l_enc   TYPE c LENGTH 1,
          l_aux   TYPE c LENGTH 40,
          l_aedtm TYPE aedtm.

    CLEAR datos_basicos.

    IF NOT o_cache IS INITIAL.
      o_cache->get_cache_mem( EXPORTING tabla = 'DATOS_BASICOS' clave = pernr clave2 = begda clave3 = endda
                              IMPORTING xstring = datos_basicos encontrado = o_cache->enc ).
      IF NOT o_cache->enc IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT * FROM pa0001
    INTO CORRESPONDING FIELDS OF datos_basicos
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF o_cache IS INITIAL.
      SELECT SINGLE name1 FROM  t500p
        INTO datos_basicos-pbtxt
           WHERE persa = datos_basicos-werks.
    ELSE.
      o_cache->get_cache_mem( EXPORTING tabla = 'T500P' clave = datos_basicos-werks nombre_campo = 'NAME1' buscar_auto = 'X' IMPORTING valor = datos_basicos-pbtxt ).
    ENDIF.

    IF NOT datos_basicos-orgeh IS INITIAL.
      CLEAR l_enc.
      IF NOT o_cache IS INITIAL.
        o_cache->get_cache_mem( EXPORTING tabla = 'T527X' clave = datos_basicos-orgeh clave2 = begda clave3 = endda
                                  IMPORTING valor = datos_basicos-orgtx encontrado = l_enc ).
      ENDIF.

      IF l_enc IS INITIAL.
        SELECT orgtx FROM  t527x
          INTO datos_basicos-orgtx
          UP TO 1 ROWS
         WHERE sprsl  = sy-langu
           AND orgeh  = datos_basicos-orgeh
           AND endda >= begda
           AND begda <= endda
         ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF NOT o_cache IS INITIAL.
          o_cache->set_cache_mem( tabla = 'T527X' clave = datos_basicos-orgeh clave2 = begda clave3 = endda valor = datos_basicos-orgtx ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT datos_basicos-plans IS INITIAL.
      CLEAR l_enc.
      IF NOT o_cache IS INITIAL.
        o_cache->get_cache_mem( EXPORTING tabla = 'T528T' clave = datos_basicos-plans clave2 = begda clave3 = endda
                                  IMPORTING valor = datos_basicos-plstx encontrado = l_enc ).
      ENDIF.
      IF l_enc IS INITIAL.
        SELECT plstx FROM  t528t
          INTO datos_basicos-plstx
          UP TO 1 ROWS
         WHERE sprsl  = sy-langu
           AND otype  = 'S'
           AND plans  = datos_basicos-plans
           AND endda >= begda
           AND begda <= endda
          ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF NOT o_cache IS INITIAL.
          o_cache->set_cache_mem( tabla = 'T528T' clave = datos_basicos-plans clave2 = begda clave3 = endda valor = datos_basicos-plstx ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT datos_basicos-stell IS INITIAL.
      CLEAR l_enc.
      IF NOT o_cache IS INITIAL.
        o_cache->get_cache_mem( EXPORTING tabla = 'T513S' clave = datos_basicos-stell clave2 = begda clave3 = endda
                                  IMPORTING valor = datos_basicos-stltx encontrado = l_enc ).
      ENDIF.

      IF l_enc IS INITIAL.
        IF zcl_c=>cliente_tasks = 'VOS'.
          SELECT stext FROM hrp1000
            INTO datos_basicos-stltx
            UP TO 1 ROWS
           WHERE plvar = '01'
             AND otype = 'C'
             AND objid = datos_basicos-stell
           ORDER BY endda DESCENDING.
          ENDSELECT.
          IF sy-subrc <> 0.
            SELECT stltx FROM  t513s "#EC CI_BYPASS
              INTO datos_basicos-stltx
              UP TO 1 ROWS
             WHERE sprsl  = sy-langu
               AND stell  = datos_basicos-stell
               AND endda >= begda
               AND begda <= endda
              ORDER BY endda DESCENDING.
            ENDSELECT.
          ENDIF.
        ELSE.
          SELECT stltx FROM  t513s "#EC CI_BYPASS
            INTO datos_basicos-stltx
            UP TO 1 ROWS
           WHERE sprsl  = sy-langu
             AND stell  = datos_basicos-stell
             AND endda >= begda
             AND begda <= endda
            ORDER BY endda DESCENDING.
          ENDSELECT.
          IF sy-subrc <> 0.
            SELECT stltx
              FROM t513s
              INTO @datos_basicos-stltx
              UP TO 1 ROWS
              WHERE sprsl = @sy-langu
                AND stell = @datos_basicos-stell
              ORDER BY PRIMARY KEY.
            ENDSELECT.
            IF sy-subrc <> 0.
              SELECT stext FROM hrp1000
                INTO datos_basicos-stltx
                UP TO 1 ROWS
               WHERE plvar = '01'
                 AND otype = 'C'
                 AND objid = datos_basicos-stell
               ORDER BY endda DESCENDING.
              ENDSELECT.
            ENDIF.
          ENDIF.
        ENDIF.

        IF NOT o_cache IS INITIAL.
          o_cache->set_cache_mem( tabla = 'T513S' clave = datos_basicos-stell clave2 = begda clave3 = endda valor = datos_basicos-stltx ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF o_cache IS INITIAL.
      SELECT SINGLE ptext FROM t503t
        INTO datos_basicos-ptext
       WHERE sprsl = sy-langu
         AND persk = datos_basicos-persk.
    ELSE.
      o_cache->get_cache_mem( EXPORTING tabla = 'T503T' clave = datos_basicos-persk nombre_campo = 'PTEXT' buscar_auto = 'X' IMPORTING valor = datos_basicos-ptext ).
    ENDIF.

    IF o_cache IS INITIAL.
      SELECT SINGLE ptext FROM  t501t
        INTO datos_basicos-pgtxt
       WHERE sprsl = sy-langu
         AND persg = datos_basicos-persg.
    ELSE.
      o_cache->get_cache_mem( EXPORTING tabla = 'T501T' clave = datos_basicos-persg nombre_campo = 'PTEXT' buscar_auto = 'X' IMPORTING valor = datos_basicos-pgtxt ).
    ENDIF.

    IF NOT datos_basicos-kostl IS INITIAL.
      IF o_cache IS INITIAL.
        SELECT ktext FROM  cskt                          "#EC CI_BYPASS
          INTO datos_basicos-ktext
          UP TO 1 ROWS
         WHERE spras = sy-langu
           AND kokrs = zcl_c=>sociedad_co
           AND kostl = datos_basicos-kostl
         ORDER BY datbi DESCENDING.
        ENDSELECT.
      ELSE.
        o_cache->get_cache_mem( EXPORTING tabla = 'CSKT' clave = datos_basicos-kostl buscar_auto = 'X' IMPORTING valor = datos_basicos-ktext ).
      ENDIF.
    ENDIF.

    IF NOT datos_basicos-btrtl IS INITIAL.
      CLEAR l_enc.
      IF NOT o_cache IS INITIAL.
        CONCATENATE datos_basicos-werks datos_basicos-btrtl INTO l_aux.
        o_cache->get_cache_mem( EXPORTING tabla = 'T001P' clave = l_aux clave2 = begda clave3 = endda
                                  IMPORTING valor = datos_basicos-btext encontrado = l_enc ).
      ENDIF.
      IF l_enc IS INITIAL.
        SELECT SINGLE btext FROM  t001p
          INTO datos_basicos-btext
         WHERE werks = datos_basicos-werks
           AND btrtl = datos_basicos-btrtl.
        IF NOT o_cache IS INITIAL.
          o_cache->set_cache_mem( tabla = 'T001P' clave = l_aux clave2 = begda clave3 = endda valor = datos_basicos-btext ).
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT stat2 aedtm FROM pa0000
    INTO (datos_basicos-stat2, l_aedtm)
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
      ORDER BY endda DESCENDING.
    ENDSELECT.
    IF l_aedtm > datos_basicos-aedtm.
      datos_basicos-aedtm = l_aedtm.
    ENDIF.
    IF NOT datos_basicos-stat2 IS INITIAL.
      CLEAR l_enc.
      IF NOT o_cache IS INITIAL.
        CONCATENATE datos_basicos-werks datos_basicos-btrtl INTO l_aux.
        o_cache->get_cache_mem( EXPORTING tabla = 'STAT2' clave = datos_basicos-stat2
                                  IMPORTING valor = datos_basicos-sttxt encontrado = l_enc ).
      ENDIF.
      IF l_enc IS INITIAL.
        SELECT SINGLE text1 FROM t529u
          INTO datos_basicos-sttxt
        WHERE sprsl = syst-langu
          AND statn = '2'
          AND statv = datos_basicos-stat2.
        IF NOT o_cache IS INITIAL.
          o_cache->set_cache_mem( tabla = 'STAT2' clave = datos_basicos-stat2 valor = datos_basicos-sttxt ).
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT empct aedtm FROM pa0007
      INTO (datos_basicos-empct, l_aedtm)
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
      ORDER BY endda DESCENDING.
    ENDSELECT.
    IF l_aedtm > datos_basicos-aedtm.
      datos_basicos-aedtm = l_aedtm.
    ENDIF.

    IF NOT o_cache IS INITIAL.
      o_cache->set_cache_mem( tabla = 'DATOS_BASICOS' clave = pernr clave2 = begda clave3 = endda xstring = datos_basicos ).
    ENDIF.
  ENDMETHOD.
  METHOD get_datos_maestros.
    o_empleado->get_master_data(
      EXPORTING
        im_begda = begda
        im_endda = endda
      IMPORTING
        ex_i0000 = i_p0000
        ex_i0001 = i_p0001
        ex_i0002 = i_p0002
        ex_i0007 = i_p0007
        ex_i0008 = i_p0008 ).
  ENDMETHOD.
  METHOD get_devengos.
    DATA r_stat2 TYPE RANGE OF pa0000-stat2.

    CLEAR i_devengos.

    IF lista IS INITIAL.
      zcl_ap_utils=>set_runtime_info( ).
      SUBMIT rplpay00
             AND RETURN
*VIA SELECTION-SCREEN
             WITH pnppernr = pernr
             WITH pnpstat2 IN r_stat2
             WITH pnpbegda = fecha
             WITH pnpendda = fecha
             WITH alv_list = 'X'.

      zcl_ap_utils=>get_runtime_info( IMPORTING tabla = i_devengos ).

    ELSE.
      SUBMIT rplpay00
             AND RETURN
             WITH pnppernr = pernr
             WITH pnpstat2 IN r_stat2
             WITH pnpbegda = fecha
             WITH pnpendda = fecha
             WITH alv_list = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD get_disponibilidad.
    DATA i_pernr TYPE TABLE OF pernrtab.

    APPEND pernr TO i_pernr.

    CALL FUNCTION 'HR_PERSONAL_TIME_LIST'
      EXPORTING
        begindate              = begda
        enddate                = endda
*       ORG_REQUIRED           = 'X'
      TABLES
        pernrtable             = i_pernr
        time_list              = i_disp
      EXCEPTIONS
        error_work_schedule    = 1
        error_infty            = 2
        error_permission_infty = 3
        error_break            = 4
        OTHERS                 = 5.

    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando time list'(etl) TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_edad.
    DATA: l_gbdat TYPE gbdat,
          l_anyos TYPE char4.

    SELECT gbdat FROM pa0002
      INTO l_gbdat
      UP TO 1 ROWS
     WHERE pernr = pernr
    ORDER BY endda DESCENDING.
    ENDSELECT.

    IF NOT l_gbdat IS INITIAL.
      CALL FUNCTION 'EHS_CALC_YEARS_BETWEEN_DATES'
        EXPORTING
          first_date                  = l_gbdat
*         MODIFY_INTERVAL             = ' '
          second_date                 = fecha
        IMPORTING
          years_between_dates         = l_anyos
        EXCEPTIONS
          sequence_of_dates_not_valid = 1
          OTHERS                      = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Error calculando diferencia de años'(eda) TYPE 'S'.
      ENDIF.
    ENDIF.

    edad = l_anyos.
  ENDMETHOD.
  METHOD get_email.
    DATA l_pernr TYPE pa0001-pernr.

    IF NOT pernr IS INITIAL.
      l_pernr = pernr.
    ELSEIF NOT uname IS INITIAL.
      SELECT pernr FROM pa0105
        INTO l_pernr
        UP TO 1 ROWS
       WHERE usrty  = '0001'
         AND usrid  = uname
         AND endda >= sy-datum
         AND begda <= sy-datum
       ORDER BY endda DESCENDING.
      ENDSELECT.
    ENDIF.

    IF NOT l_pernr IS INITIAL.
      email = get_comunicacion_st( pernr = l_pernr subty = '0010' ).
    ENDIF.

* Si el empleado no tuviera mail, compruebo si existe un usuario
* de SAP que sí tenga
    IF email IS INITIAL.
      SELECT adr6~smtp_addr INTO email
        UP TO 1 ROWS
       FROM usr21 JOIN adr6 ON usr21~persnumber = adr6~persnumber
       WHERE usr21~bname = uname
        ORDER BY usr21~bname usr21~persnumber.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.
  METHOD get_fecha_it0041.
    DATA l_ind TYPE numc2.

    CLEAR: pa0041, indice, fecha_salida.

    IF endda > fecha.
      DATA(l_endda) = endda.
    ELSE.
      l_endda = fecha.
    ENDIF.
    SELECT * FROM pa0041
      INTO pa0041
     UP TO 1 ROWS
     WHERE pernr  = pernr
       AND endda >= fecha
       AND begda <= l_endda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DO 24 TIMES.
      l_ind = sy-index.
      ASSIGN COMPONENT |DAR{ l_ind }| OF STRUCTURE pa0041 TO FIELD-SYMBOL(<tipo>).
      IF sy-subrc = 0.
        IF <tipo> = clase_fecha.
          ASSIGN COMPONENT |DAT{ l_ind }| OF STRUCTURE pa0041 TO FIELD-SYMBOL(<fecha>).
          IF sy-subrc = 0.
            indice = l_ind.
            fecha_salida = <fecha>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD get_horario_trabajo.
    CLEAR t_ptpsp.
    CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
      EXPORTING
        pernr         = pernr
        begda         = begda
        endda         = endda
        switch_activ  = 1
        read_cluster  = 'X'
      TABLES
        perws         = t_ptpsp
      EXCEPTIONS
        error_occured = 1
        abort_occured = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando horario de trabajo'(erh) TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_horario_trabajo_detallado.
    DATA: i_perws TYPE ptpsp_tab,
          daygen  TYPE STANDARD TABLE OF pwsdaygen.

    i_perws = get_horario_trabajo( pernr = pernr begda = begda endda = endda ).

    IF i_perws IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'HR_WORK_SCHEDULE_TIMES'
      EXPORTING
        pernr         = pernr
        begda         = begda
        endda         = endda
*       KUG           = ' '
*       BREAK_OVERTIME                = '1'
*       REFRESH_INFOTYPE_BUFFER       = 'X'
* IMPORTING
*       WARNING_OCCURED               = WARNING_OCCURED
      TABLES
*       I0001         = I0001
*       I0007         = I0007
*       I2003         = I2003
*       I0049         = I0049
        perws         = i_perws
        daygen        = daygen
        dayint        = i_horario
      EXCEPTIONS
        error_occured = 1
        perws_error   = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Error recupeando horario' TYPE 'I'.
    ENDIF.
  ENDMETHOD.
  METHOD get_nombre.
    SELECT ename FROM pa0001
      INTO nombre
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
     ORDER BY endda DESCENDING.
    ENDSELECT.
    IF sy-subrc <> 0.
      SELECT ename FROM pa0001
        INTO nombre
        UP TO 1 ROWS
       WHERE pernr = pernr
       ORDER BY endda DESCENDING.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.
  METHOD get_num_candidato.
    CLEAR aplno.
    SELECT aplno FROM pa0139
      INTO aplno
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= fecha
       AND endda >= fecha
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_pernr_from_perid.
    DATA: r_perid  TYPE RANGE OF pa0002-perid,
          l_perid  TYPE pa0002-perid,
          lr_perid LIKE LINE OF r_perid,
          i_pa0002 TYPE TABLE OF pa0002,
          l_pa0002 TYPE pa0002,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_pa0000 TYPE pa0000,
          l_cont   TYPE c LENGTH 5.

    l_perid = perid.
    l_perid = to_upper( l_perid ).

    CLEAR lr_perid.
    lr_perid-option    = 'CP'.
    lr_perid-sign      = 'I'.
    lr_perid-low       = l_perid.
    lr_perid-low+19(1) = '*'.
    APPEND lr_perid TO r_perid.

    CONCATENATE '0' l_perid INTO lr_perid-low.
    lr_perid-low+19(1) = '*'.
    APPEND lr_perid TO r_perid.

    CONCATENATE 'ES' l_perid INTO lr_perid-low.
    lr_perid-low+19(1) = '*'.
    APPEND lr_perid TO r_perid.

    CONCATENATE 'ES0' l_perid INTO lr_perid-low.
    lr_perid-low+19(1) = '*'.
    APPEND lr_perid TO r_perid.

    CLEAR: pernr, message.

    SELECT pernr FROM pa0002
      INTO CORRESPONDING FIELDS OF TABLE i_pa0002
     WHERE begda <= endda
       AND endda >= begda
       AND perid IN r_perid.

    READ TABLE i_pa0002 INTO l_pa0002 INDEX 1.          "#EC CI_NOORDER
    IF sy-subrc = 0.
      pernr = l_pa0002-pernr.
    ENDIF.

    LOOP AT i_pa0002 INTO l_pa0002.
      SELECT SINGLE pernr FROM pa0000
        INTO l_pa0000-pernr
       WHERE pernr  = l_pa0002-pernr
         AND begda <= endda
         AND endda >= begda
         AND stat2  = '3'. " Activo
      IF sy-subrc <> 0.
* Si no está activo, no lo queremos
        DELETE i_pa0002.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE i_pa0002 LINES sy-tfill.
    IF sy-tfill = 1.
      READ TABLE i_pa0002 INTO l_pa0002 INDEX 1.        "#EC CI_NOORDER
      IF sy-subrc = 0.
        pernr = l_pa0002-pernr.
      ENDIF.
    ELSEIF sy-tfill = 0.
      message = 'No hay ningún empleado activo con este identificador'(nea).
    ELSEIF sy-tfill > 1.
      l_cont = sy-tfill.
      READ TABLE i_pa0002 INTO l_pa0002 INDEX 1.        "#EC CI_NOORDER
      IF sy-subrc = 0.
        pernr = l_pa0002-pernr.
      ENDIF.

      CONCATENATE 'Existen'(exi) l_cont 'empleados activos con este identificador'(aai) INTO message SEPARATED BY space.
    ENDIF.
  ENDMETHOD.
  METHOD get_pernr_from_uname.
    CLEAR pernr.
    SELECT pernr FROM pa0105
      INTO pernr
      UP TO 1 ROWS
     WHERE usrty  = '0001'
       AND usrid  = uname
       AND begda <= endda
       AND endda >= begda
   ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_superior.
    DATA superiores TYPE hrobject_t.

    FIELD-SYMBOLS <pos> TYPE hrobject.

    superiores = get_superiores( pernr = pernr fecha = fecha ).
    ASSIGN superiores[ otype = 'P' ] TO <pos>.
    IF sy-subrc = 0.
      superior = <pos>-objid.
    ENDIF.
  ENDMETHOD.
  METHOD get_superiores.
    DATA: l_plvar   TYPE objec-plvar,
          l_sobid   TYPE hrsobid-sobid,
          l_otype   TYPE hrsobid-otype,
          i_hrp1001 TYPE TABLE OF hrp1001,
          lines     TYPE i,
          l_hrp1001 TYPE hrp1001.

    FIELD-SYMBOLS <pos> TYPE hrobject.

    CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
      IMPORTING
        act_plvar       = l_plvar
      EXCEPTIONS
        no_active_plvar = 1.

    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando plvar'(plv) TYPE 'S'.
    ENDIF.
* Inicializar l_sobid y l_otype
    l_sobid = pernr.
    l_otype = otype.
* Comprobar si el empleado está asignado a varias posiciones.
* Si es así, se coge la que tiene más % de asignación (PROZT).
    CLEAR i_hrp1001.
    SELECT prozt sclas sobid FROM hrp1001
      INTO CORRESPONDING FIELDS OF TABLE i_hrp1001
      WHERE otype  = otype
        AND objid  = pernr
        AND plvar  = l_plvar
        AND rsign  = 'B'
        AND relat  = '008'
        AND begda <= fecha
        AND endda >= fecha.
    SORT i_hrp1001 BY prozt DESCENDING sobid DESCENDING.
    lines = lines( i_hrp1001 ).
    IF lines > 1.
      READ TABLE i_hrp1001 INDEX 1 INTO l_hrp1001.
      l_sobid = l_hrp1001-sobid.
      l_otype = l_hrp1001-sclas.
    ENDIF.

    CALL FUNCTION 'RH_GET_LEADING_POSITION'
      EXPORTING
        plvar             = l_plvar
        otype             = l_otype
        sobid             = l_sobid
        date              = fecha
        auth              = ' '
*       BUFFER_MODE       = ' '
        consider_vac_pos  = consider_vac_pos
      TABLES
        leading_pos       = superiores
      EXCEPTIONS
        no_lead_pos_found = 1
        OTHERS            = 2.
    IF sy-subrc = 0.
      CLEAR l_hrp1001.
      LOOP AT superiores ASSIGNING <pos>.
        SELECT sclas sobid FROM hrp1001
          INTO CORRESPONDING FIELDS OF l_hrp1001
          UP TO 1 ROWS
         WHERE otype  = <pos>-otype
           AND objid  = <pos>-objid
           AND plvar  = <pos>-plvar
           AND rsign  = 'A'
           AND relat  = '008'
           AND begda <= fecha
           AND endda >= fecha
           AND sclas  = 'P'
         ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          <pos>-otype = l_hrp1001-sclas.
          <pos>-objid = l_hrp1001-sobid.
        ENDIF.
        IF valido_hoy = abap_true.
          " TODO: variable is assigned but never used (ABAP cleaner)
          SELECT SINGLE pernr INTO @DATA(l_pernr)
            FROM pa0000
           WHERE pernr  = @l_hrp1001-sobid
             AND stat2  = '3'
             AND begda <= @sy-datum
             AND endda >= @sy-datum.
          IF sy-subrc <> 0.
            CLEAR <pos>.
          ENDIF.
        ENDIF.
      ENDLOOP.
      DELETE superiores WHERE objid IS INITIAL.
    ENDIF.
  ENDMETHOD.
  METHOD get_texto_infotipo.
    DATA: l_tabla TYPE tabname,
          key     TYPE pskey.

* Leer notas del infotipo Medidas
    CLEAR text.

    CONCATENATE 'PA' infty INTO l_tabla.
    IF endda IS INITIAL.
      SELECT SINGLE * FROM (l_tabla)
        INTO CORRESPONDING FIELDS OF key
       WHERE pernr  = pernr
         AND endda >= fecha
         AND begda <= fecha
         AND subty  = subty.
      IF sy-subrc <> 0.
        SELECT SINGLE * FROM (l_tabla)
          INTO CORRESPONDING FIELDS OF key
         WHERE pernr  = pernr
           AND endda >= fecha
           AND begda <= fecha.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM (l_tabla)
        INTO CORRESPONDING FIELDS OF key
       WHERE pernr = pernr
         AND endda = endda
         AND begda = fecha
         AND subty = subty.
    ENDIF.

    IF key IS INITIAL.
      RETURN.
    ENDIF.

    key-infty = infty.

    CALL FUNCTION 'HR_READ_INFTY_NOTE'
      EXPORTING
        key       = key
        tclas     = 'A'
      TABLES
        text      = text
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando nota' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_texto_infotipo_string.
    DATA text TYPE it_notes.

    text = get_texto_infotipo( pernr = pernr fecha = fecha endda = endda infty = infty subty = subty ).
    string = zcl_ap_string=>tabla2string( text ).
  ENDMETHOD.
  METHOD get_timeoverview.
    DATA employees      TYPE STANDARD TABLE OF bapi7013_4.
    DATA extapplication TYPE bapi7013_3-extapplication.
    DATA timetypes      TYPE STANDARD TABLE OF bapi7013_5.
    DATA timetypevalues TYPE STANDARD TABLE OF bapi7013_2.

    CLEAR: i_tiempos, i_return.
    APPEND pernr TO employees.
    CALL FUNCTION 'BAPI_PTIMEOVERVIEW_GET'
      EXPORTING
        fromdate                = begda
        todate                  = endda
        extapplication          = extapplication
        refresh_infotype_buffer = 'X'
      TABLES
        employees               = employees
        timetypes               = timetypes
        timeoverview            = i_tiempos
        timetypevalues          = timetypevalues
        return                  = i_return.
  ENDMETHOD.
  METHOD get_tipo_contrato.
    CLEAR: idseg, ttext, fijo.

    SELECT pesoc FROM pa0061
      INTO idseg
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND begda <= endda
       AND endda >= begda
      ORDER BY PRIMARY KEY.
    ENDSELECT.

    SELECT ttext FROM  t5e2t
      INTO ttext
      UP TO 1 ROWS
     WHERE sprsl = sy-langu
       AND ticon = idseg
     ORDER BY PRIMARY KEY.
    ENDSELECT.

    IF idseg(1) >= '1' AND idseg(1) <= '3'.
      fijo = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD get_uname_from_pernr.
    CLEAR uname.
    SELECT usrid FROM pa0105
      INTO uname
      UP TO 1 ROWS
     WHERE pernr  = pernr
       AND subty  = '0001'
       AND begda <= endda
       AND endda >= begda
     ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD inicio.
    CLEAR: o_empleado,
           i_p0000,
           i_p0001,
           i_p0002,
           i_p0007,
           i_p0008,
           datos_basicos,
           pernr,
           begda,
           endda.
  ENDMETHOD.
  METHOD set_fecha_it0041.
    DATA l_p0041 TYPE p0041.

    CLEAR message.
    DATA(l_fecha) = get_fecha_it0041( EXPORTING pernr = pernr
                                                fecha = fecha
                                                clase_fecha = clase_fecha
                                      IMPORTING pa0041 = pa0041
                                                indice = indice ).
    IF l_fecha = fecha_mod.
      RETURN.
    ENDIF.

    IF NOT pa0041 IS INITIAL.
      IF NOT indice IS INITIAL.
        DATA(l_indice) = indice.
      ELSE.
        DO 24 TIMES.
          indice = sy-index.
          ASSIGN COMPONENT |DAR{ indice }| OF STRUCTURE pa0041 TO FIELD-SYMBOL(<tipo>).
          IF sy-subrc = 0.
            IF <tipo> IS INITIAL.
              ASSIGN COMPONENT |DAT{ indice }| OF STRUCTURE pa0041 TO FIELD-SYMBOL(<fecha>).
              IF sy-subrc = 0.
                IF <fecha> IS INITIAL.
                  l_indice = indice.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDDO.
        IF l_indice IS INITIAL.
          message = 'No se ha encontrado hueco para poner la fecha'.
          RETURN.
        ELSE.
          ASSIGN COMPONENT |DAR{ l_indice }| OF STRUCTURE pa0041 TO FIELD-SYMBOL(<clase>).
          <clase> = clase_fecha.
        ENDIF.
      ENDIF.
      ASSIGN COMPONENT |DAT{ l_indice }| OF STRUCTURE pa0041 TO <fecha>.
      <fecha> = fecha_mod.
      MODIFY pa0041 FROM pa0041.  "#EC AOC_STD_TABLE
    ELSE.
      l_p0041-pernr = pernr.
      l_p0041-begda = fecha.
      l_p0041-endda = '99991231'.
      l_p0041-dar01 = clase_fecha.
      l_p0041-dat01 = fecha_mod.
      actualizar_infotipo( datos = l_p0041
                           infty = '0041'
                           operation = 'INS' ).

*      DATA(l_return) = zcl_ap_empleado=>bloquear_empleado( pernr ).
*      IF l_return-type = 'E'.
*        message = l_return-message.
*        RETURN.
*      ENDIF.
*
*      l_p0041-pernr = pernr.
*      l_p0041-begda = fecha.
*      l_p0041-endda = '99991231'.
*      l_p0041-dar01 = clase_fecha.
*      l_p0041-dat01 = fecha_mod.
*      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
*        EXPORTING
*          infty         = '0041'
*          number        = pernr
**         subtype       = ''
*          validityend   = '9991231'
*          validitybegin = sy-datum
*          record        = l_p0041
*          operation     = 'INS'
*          nocommit      = ''
*        IMPORTING
*          key           = l_key
*          return        = l_return
*        EXCEPTIONS
*          OTHERS        = 1.
*
*      IF l_return-type = 'E'.
*        message = l_return-message.
*      ENDIF.
*      zcl_ap_empleado=>desbloquear_empleado( pernr ).
    ENDIF.
  ENDMETHOD.
  METHOD set_fichaje.
    DATA: ins_teven_more   TYPE teven_more,
          ins_teven        TYPE teven,
          i_ins_teven      TYPE TABLE OF teven,
          i_ins_teven_more TYPE TABLE OF teven_more,
          i_del_teven      TYPE TABLE OF teven,
          del_teven_more   TYPE teven_more,
          i_del_teven_more TYPE TABLE OF teven_more.

    CLEAR message.

    ins_teven_more-pernr = pernr.
    ins_teven-pernr = ins_teven_more-pernr.
    ins_teven_more-ldate = fecha.
    ins_teven-ldate = ins_teven_more-ldate.
    ins_teven_more-erdat = sy-datum.
    ins_teven-erdat = ins_teven_more-erdat.
    ins_teven_more-ltime = hora.
    ins_teven-ltime = ins_teven_more-ltime.
    ins_teven_more-ertim = sy-uzeit.
    ins_teven-ertim = ins_teven_more-ertim.
    ins_teven_more-satza = satza.
    ins_teven-satza = ins_teven_more-satza.
    ins_teven_more-terid = terid.
    ins_teven-terid = ins_teven_more-terid.
    ins_teven_more-dallf = dallf.
    ins_teven-dallf = ins_teven_more-dallf.

    DO 30 TIMES.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'PD_SEQ_NR'
          quantity    = '00000000000000000001'
        IMPORTING
          number      = ins_teven-pdsnr.
      SELECT SINGLE pdsnr FROM teven
        " TODO: variable is assigned but never used (ABAP cleaner)
        INTO @DATA(l_pdsnr)
       WHERE pdsnr = @ins_teven-pdsnr.
      IF sy-subrc <> 0.
        EXIT.
      ELSE.
        CLEAR ins_teven-pdsnr.
      ENDIF.
    ENDDO.

    IF ins_teven-pdsnr IS INITIAL.
      message = 'Error determinando nº de fichaje'.
      RETURN.
    ENDIF.

    ins_teven_more-pdsnr = ins_teven-pdsnr.
    APPEND ins_teven TO i_ins_teven.
    APPEND ins_teven_more TO i_ins_teven_more.

    CLEAR: sy-msgid, sy-msgno, sy-msgty, sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.

    CALL FUNCTION 'HR_TMW_DB_UPDATE_TEVENT'
      TABLES
        del_teven      = i_del_teven
        ins_teven      = i_ins_teven
        del_teven_more = i_del_teven_more
        ins_teven_more = i_ins_teven_more
      EXCEPTIONS
        insert_failed  = 1
        update_failed  = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      IF sy-msgty IS INITIAL.
        message = |Error insertando fichaje { fecha DATE = USER } { hora TIME = USER }|.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ENDIF.
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
    ENDIF.
  ENDMETHOD.
  METHOD set_texto_infotipo.
    DATA: l_tabla TYPE tabname,
          key     TYPE pskey,
          l_text  LIKE LINE OF text,
          l_hrpad TYPE hrpad_text,
          i_hrpad TYPE hrpad_text_tab.

* Leer notas del infotipo Medidas

    CONCATENATE 'PA' infty INTO l_tabla.
    IF endda IS INITIAL.
      SELECT SINGLE * FROM (l_tabla)
        INTO CORRESPONDING FIELDS OF key
       WHERE pernr  = pernr
         AND endda >= fecha
         AND begda <= fecha
         AND subty  = subty.
      IF sy-subrc <> 0.
        SELECT SINGLE * FROM (l_tabla)
          INTO CORRESPONDING FIELDS OF key
         WHERE pernr  = pernr
           AND endda >= fecha
           AND begda <= fecha.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM (l_tabla)
        INTO CORRESPONDING FIELDS OF key
       WHERE pernr = pernr
         AND endda = endda
         AND begda = fecha
         AND subty = subty.
    ENDIF.

    IF key IS INITIAL.
      RETURN.
    ENDIF.

    key-infty = infty.

*    EXPORT text FROM text[] TO DATABASE pcl1(tx) ID key.
*    IF sy-subrc = 0.
*      UPDATE (l_tabla)
*         SET itxex = 'X'
*       WHERE pernr = pernr
*         and subty = key-subty
*         and endda = key-endda
*         and begda = key-begda.
*      COMMIT WORK AND WAIT.
*    ENDIF.

    LOOP AT text INTO l_text.
      l_hrpad = l_text.
      APPEND l_hrpad TO i_hrpad.
    ENDLOOP.

    TRY.
        cl_hrpa_text_cluster=>update(
            tclas         = 'A'
            pskey         = key
            histo         = 'X'
            uname         = sy-uname
            aedtm         = sy-datum
            pgmid         = ''
            text_tab      = i_hrpad
            no_auth_check = 'X' ).
      CATCH cx_hrpa_missing_authorization INTO DATA(o_hrpa_missing_authorization).
        message = o_hrpa_missing_authorization->get_longtext( ).
        IF message IS INITIAL.
          message = o_hrpa_missing_authorization->get_text( ).
        ENDIF.
      CATCH cx_hrpa_violated_assertion INTO DATA(cx_hrpa_violated_assertion).
        message = cx_hrpa_violated_assertion->get_longtext( ).
        IF message IS INITIAL.
          message = cx_hrpa_violated_assertion->get_text( ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD set_texto_infotipo_string.
    DATA text TYPE it_notes.

    zcl_ap_string=>string2tabla( EXPORTING string = string longitud = 72 CHANGING tabla = text ).
    message = set_texto_infotipo( pernr = pernr fecha = fecha endda = endda infty = infty subty = subty text = text ).
  ENDMETHOD.
  METHOD tiene_cualificacion.
    DATA i_cualif TYPE hrpd_profq_tab.

    CLEAR si.
    i_cualif = get_cualificaciones( pernr = pernr
                                    begda = begda
                                    endda = endda ).

    IF line_exists( i_cualif[ ttype = 'Q'
                              tbjid = cualif ] ).
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar_st.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: l_pernr TYPE persno,
          o_bi    TYPE REF TO zcl_ap_batch_input.
    DATA l_tabla TYPE tabname.
    DATA r_objps TYPE RANGE OF pa0001-objps.
    DATA r_seqnr TYPE RANGE OF pa0001-seqnr.
    DATA: l_objps TYPE pa0000-objps,
          l_seqnr TYPE pa0000-seqnr.

    IF pernr IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE pernr FROM pa0000
      INTO l_pernr
     WHERE pernr = pernr.
    IF sy-subrc = 0.

      o_bi = NEW #( ).

      o_bi->inicio( ).

* Pantalla acceso PA30
      o_bi->dynpro( program = 'SAPMP50A' dynpro = '1000' ).
      IF resumen IS INITIAL.
        IF actualizar IS INITIAL.
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=DIS' ).
        ELSE.
          o_bi->campos( campo = 'BDC_OKCODE' valor = '=MOD' ).
        ENDIF.
      ELSE.
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=LIST' ).
      ENDIF.
      o_bi->campos( campo = 'RP50G-PERNR' valor = pernr ).
      o_bi->campos( campo = 'RP50G-BEGDA' valor = begda ).
      o_bi->campos( campo = 'RP50G-ENDDA' valor = endda ).
      o_bi->campos( campo = 'RP50G-CHOIC' valor = infty ).
      o_bi->campos( campo = 'RP50G-SUBTY' valor = subty ).

      l_tabla = |PA{ infty }|.

      SELECT tabname
        FROM dd02l
        WHERE tabname = @l_tabla
        ORDER BY PRIMARY KEY
        INTO @l_tabla
        UP TO 1 ROWS.
      ENDSELECT.
      IF sy-subrc = 0.
        IF NOT objps IS SUPPLIED.
          r_objps = VALUE #( ( option = 'EQ' sign = 'I' low = objps ) ).
        ENDIF.
        IF NOT seqnr IS SUPPLIED.
          r_seqnr = VALUE #( ( option = 'EQ' sign = 'I' low = seqnr ) ).
        ENDIF.
        SELECT SINGLE objps seqnr FROM (l_tabla)
          INTO (l_objps, l_seqnr)
         WHERE pernr  = pernr
           AND subty  = subty
           AND begda  = begda
           AND endda  = endda
           AND objps IN r_objps
           AND seqnr IN r_seqnr.
        IF sy-subrc = 0.
          o_bi->campos( campo = 'RP50G-OBJPS' valor = l_objps ).
          o_bi->campos( campo = 'RP50G-SEQNR' valor = l_seqnr ).
        ENDIF.
      ENDIF.

      IF actualizar IS INITIAL.
        mensaje = o_bi->llamar_transaccion( tcode = 'PA20' modo = 'E' ).
      ELSE.
        mensaje = o_bi->llamar_transaccion( tcode = 'PA30' modo = 'E' ).
      ENDIF.

      o_bi->inicio( ).
    ELSE.
      TRY.
          CALL METHOD ('ZCL_AP_CANDIDATO')=>('VISUALIZAR_ST')
            EXPORTING
              aplno = pernr
              begda = begda
              endda = endda
              infotipo = infty
              subtipo  = subty
              resumen = resumen
            RECEIVING
              message = mensaje.
        CATCH cx_root INTO DATA(o_root). "#EC * " TODO: variable is assigned but never used (ABAP cleaner)
          MESSAGE 'No implementada la opción de visualizar candidatos' TYPE 'I'.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
