CLASS zcl_ap_batch_input DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA i_bdcdata        TYPE tab_bdcdata.
    DATA bdcdata          TYPE bdcdata.
    DATA est_mensaje      TYPE bdcmsgcoll          READ-ONLY.
    DATA i_mensajes       TYPE tab_bdcmsgcoll      READ-ONLY.
    DATA subrc            TYPE sy-subrc            READ-ONLY.
    DATA msgid            TYPE sy-msgid            READ-ONLY.
    DATA msgno            TYPE sy-msgno            READ-ONLY.
    DATA msgty            TYPE sy-msgty.
    DATA msgv1            TYPE sy-msgv1            READ-ONLY.
    DATA msgv2            TYPE sy-msgv2            READ-ONLY.
    DATA msgv3            TYPE sy-msgv3            READ-ONLY.
    DATA msgv4            TYPE sy-msgv4            READ-ONLY.
    DATA texto            TYPE bapireturn1-message READ-ONLY.
    DATA lights           TYPE c LENGTH 1          READ-ONLY.
    DATA i_bdcdata_backup TYPE tab_bdcdata.

    METHODS constructor.

    METHODS crear_juego_datos
      IMPORTING groupid  TYPE apqi-groupid
                !user    TYPE apqi-userid    DEFAULT sy-uname
                !keep    TYPE apqi-qerase    DEFAULT ''
                holddata TYPE apqi-startdate DEFAULT sy-datum.

    METHODS cierra_juego_datos
      IMPORTING tcode TYPE sy-tcode.

    METHODS dynpro
      IMPORTING !program TYPE bdcdata-program
                !dynpro  TYPE bdcdata-dynpro
                okcode   TYPE any OPTIONAL.

    METHODS campos
      IMPORTING campo      TYPE bdcdata-fnam
                valor      TYPE any
                ind        TYPE numc2     OPTIONAL
                no_cero    TYPE abap_bool OPTIONAL
                hora_larga TYPE abap_bool DEFAULT ''.

    METHODS llamar_transaccion
      IMPORTING tcode          TYPE sy-tcode
                modo           TYPE bdc_mode   DEFAULT 'N'
                capturar_msg   TYPE abap_bool  DEFAULT 'X'
                defsize        TYPE ctu_defsze DEFAULT ''
                nobinpt        TYPE ctu_nobim  DEFAULT ''
                racommit       TYPE ctu_rafc   DEFAULT ''
                updmode        TYPE ctu_update DEFAULT 'S'
      RETURNING VALUE(mensaje) TYPE bapireturn1-message.

    METHODS convertir_codigo_bdc
      IMPORTING gen_cursor        TYPE abap_bool DEFAULT ''
                gen_subscr        TYPE abap_bool DEFAULT ''
                lin_largas        TYPE abap_bool DEFAULT 'X'
                okcode_con_dynpro TYPE abap_bool DEFAULT 'X'.

    METHODS inicio.

    METHODS get_descripcion_campo
      IMPORTING campo         TYPE bdcdata-fnam
      RETURNING VALUE(ddtext) TYPE dd04t-ddtext.

    METHODS append_bdcdata
      IMPORTING t_bdcdata TYPE tab_bdcdata.

    METHODS llamar_transaccion2
      IMPORTING tcode             TYPE sy-tcode
                modo              TYPE bdc_mode DEFAULT 'N'
      RETURNING VALUE(t_mensajes) TYPE tab_bdcmsgcoll.

    CLASS-METHODS cambiar_modo
      CHANGING modo TYPE bdc_mode OPTIONAL.


  PRIVATE SECTION.
    DATA v_bdc_iniciado          TYPE string.
    DATA v_bdc_numlineas_bdcdata TYPE i.

    METHODS formatear_mensaje.

    METHODS get_atributos_campo
      IMPORTING campo        TYPE bdcdata-fnam
      RETURNING VALUE(dd03l) TYPE dd03l.
endclass. "ZCL_AP_BATCH_INPUT definition
class ZCL_AP_BATCH_INPUT implementation.
  METHOD append_bdcdata.
    LOOP AT t_bdcdata INTO bdcdata.
      APPEND bdcdata TO i_bdcdata.
    ENDLOOP.
  ENDMETHOD.
  METHOD cambiar_modo.
    IF modo IS INITIAL OR modo = 'A'.
      modo = 'N'.
      MESSAGE 'Se cambia modo ejecución call transactión a NO VISIBLE'(nov) TYPE 'S'.
    ELSEIF modo = 'N' OR modo = 'E'.
      modo = 'A'.
      MESSAGE 'Se cambia modo ejecución call transactión a VISIBLE'(vis) TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD campos.
    DATA: l_tipo  TYPE c LENGTH 1,
          l_dd03l TYPE dd03l.
    DATA l_meins TYPE meins.

    IF no_cero = 'X'.
      IF valor IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    DESCRIBE FIELD valor TYPE l_tipo.
    l_dd03l = get_atributos_campo( campo ).

    CLEAR bdcdata.
    IF ind IS SUPPLIED.
      CONCATENATE campo '(' ind ')' INTO bdcdata-fnam.
    ELSE.
      bdcdata-fnam = campo.
    ENDIF.

    CASE l_tipo.
      WHEN 'P'.
        IF l_dd03l-leng IS INITIAL.
          WRITE valor TO bdcdata-fval(10).
        ELSE.
          WRITE valor TO bdcdata-fval(l_dd03l-leng).
        ENDIF.
      WHEN 'T'.
        WRITE valor TO bdcdata-fval(8).
        IF hora_larga IS INITIAL.
          CLEAR bdcdata-fval+5.
        ENDIF.
      WHEN 'I'.
        bdcdata-fval = valor.
        CONDENSE bdcdata-fval NO-GAPS.
      WHEN 'D'.
        IF valor IS INITIAL.
          CLEAR bdcdata-fval.
        ELSE.
          WRITE valor TO bdcdata-fval(10).
        ENDIF.
      WHEN OTHERS.
        IF l_dd03l-datatype = 'UNIT'.
          l_meins = valor.
          WRITE l_meins TO bdcdata-fval(3).
        ELSE.
          bdcdata-fval = valor.
        ENDIF.
    ENDCASE.
    APPEND bdcdata TO i_bdcdata.
    v_bdc_numlineas_bdcdata = v_bdc_numlineas_bdcdata + 1.
  ENDMETHOD.
  METHOD cierra_juego_datos.
    IF v_bdc_iniciado <> 'X'.
      RETURN.
    ENDIF.

    IF NOT tcode IS INITIAL.
      CALL FUNCTION 'BDC_INSERT'
        EXPORTING
          tcode            = tcode
        TABLES
          dynprotab        = i_bdcdata
        EXCEPTIONS
          internal_error   = 1
          not_open         = 2
          queue_error      = 3
          tcode_invalid    = 4
          printing_invalid = 5
          posting_invalid  = 6
          OTHERS           = 7.
      IF sy-subrc <> 0.
        MESSAGE 'Error en BDC_INSERT'(ebi) TYPE 'E'.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'BDC_CLOSE_GROUP'.
    CLEAR v_bdc_iniciado.
    CLEAR v_bdc_numlineas_bdcdata.
  ENDMETHOD.
  METHOD constructor.
    inicio( ).
  ENDMETHOD.
  METHOD convertir_codigo_bdc.
    TYPES t_linea TYPE c LENGTH 1000.

    DATA: i_tabla  TYPE TABLE OF t_linea,
          l_linea  TYPE t_linea,
          " TODO: variable is assigned but never used (ABAP cleaner)
          i_codigo TYPE TABLE OF t_linea,
          l_linea2 TYPE t_linea,
          l_rc     TYPE i.
    DATA: bdc     TYPE bdcdata,
          i_bdc   TYPE tab_bdcdata,
          l_fin   TYPE abap_bool,
          l_tcode TYPE sy-tcode.
    DATA i_tabix TYPE TABLE OF sy-tabix.

    cl_gui_frontend_services=>clipboard_import(
      IMPORTING
        data                 = i_tabla
*       length               =
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando datos del portapapeles' TYPE 'I'.
      RETURN.
    ENDIF.

    LOOP AT i_tabla INTO l_linea.

      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = l_linea
          replacement       = 64
        IMPORTING
          outtext           = l_linea
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
* message.
      ENDIF.

      SPLIT l_linea AT '@' INTO bdc-program bdc-dynpro bdc-dynbegin
                                bdc-fnam bdc-fval.
      APPEND bdc TO i_bdc.
    ENDLOOP.

    IF okcode_con_dynpro = 'X'.
      DATA(i_bdc_aux) = i_bdc.
      LOOP AT i_bdc ASSIGNING FIELD-SYMBOL(<bdc>) WHERE program <> ''. "#EC CI_STDSEQ
        DATA(l_tabix) = sy-tabix + 1.
        LOOP AT i_bdc_aux ASSIGNING FIELD-SYMBOL(<bdc_aux>) FROM l_tabix.
          IF <bdc_aux>-program IS INITIAL.
            IF <bdc_aux>-fnam = 'BDC_OKCODE'.
              DATA(l_ok_code) = <bdc_aux>-fval.
              APPEND sy-tabix TO i_tabix.
              EXIT.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF NOT l_ok_code IS INITIAL.
          <bdc>-fnam = 'BDC_OKCODE'.
          <bdc>-fval = l_ok_code.
        ENDIF.
      ENDLOOP.
      l_tabix = 0.
      LOOP AT i_bdc ASSIGNING <bdc>.
        l_tabix = l_tabix + 1.
        IF line_exists( i_tabix[ table_line = l_tabix ] ). "#EC CI_STDSEQ
          DELETE i_bdc.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT i_bdc INTO bdc.
      AT FIRST.
        APPEND '   data: o_bi type ref to zcl_ap_batch_input,' TO i_codigo.
        APPEND '         l_mensaje type bapireturn1-message.' TO i_codigo.
        APPEND '   create object o_bi.' TO i_codigo.
        APPEND '' TO i_codigo.
        APPEND '   o_bi->inicio( ).' TO i_codigo.
      ENDAT.
      AT LAST.
        l_fin = 'X'.
      ENDAT.

      IF bdc-dynbegin = 'T'.
        l_tcode = bdc-fnam.
        CONTINUE.
      ENDIF.

      IF gen_cursor IS INITIAL AND bdc-fnam = 'BDC_CURSOR'.
        IF l_fin IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF gen_subscr IS INITIAL AND bdc-fnam = 'BDC_SUBSCR'.
        IF l_fin IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      CLEAR l_linea.
      IF bdc-program IS INITIAL.
        IF lin_largas = 'X'.
          CONCATENATE '   o_bi->campos( campo = ''' bdc-fnam
                      ''' valor = ''' bdc-fval ''').' INTO l_linea.
          IF bdc-fnam CS '-'.
            l_linea2 = get_descripcion_campo( bdc-fnam ).
            IF NOT l_linea2 IS INITIAL.
              CONCATENATE l_linea '"' l_linea2 INTO l_linea
                          SEPARATED BY space.
            ENDIF.
          ENDIF.
        ELSE.
          CONCATENATE '   o_bi->campos( campo = ''' bdc-fnam ''''
                      INTO l_linea.
          APPEND l_linea TO i_codigo.
          CLEAR l_linea.
          CONCATENATE '                 valor = '''
              bdc-fval ''').' INTO l_linea.
          IF bdc-fnam CS '-'.
            l_linea2 = get_descripcion_campo( bdc-fnam ).
            IF NOT l_linea2 IS INITIAL.
              CONCATENATE l_linea '"' l_linea2 INTO l_linea
                          SEPARATED BY space.
              l_linea = l_linea(72).
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        APPEND '' TO i_codigo.
        SELECT SINGLE dtxt FROM d020t
          INTO l_linea
         WHERE prog = bdc-program
           AND dynr = bdc-dynpro
           AND lang = sy-langu.
        IF sy-subrc = 0.
          CONCATENATE '* ' l_linea INTO l_linea SEPARATED BY space.
          APPEND l_linea TO i_codigo.
        ENDIF.

        IF bdc-fnam = 'BDC_OKCODE'.
          CONCATENATE '   o_bi->dynpro( program = ''' bdc-program ''' dynpro = ''' bdc-dynpro ''' okcode = ''' bdc-fval ''').' INTO l_linea.
        ELSE.
          CONCATENATE '   o_bi->dynpro( program = ''' bdc-program ''' dynpro = ''' bdc-dynpro ''').' INTO l_linea.
        ENDIF.
      ENDIF.
      APPEND l_linea TO i_codigo.

      IF l_fin = 'X'.
        APPEND '' TO i_codigo.

        CONCATENATE '    l_mensaje = o_bi->llamar_transaccion( tcode = ''' ##NO_TEXT
               l_tcode ''' modo = ''E'').' INTO l_linea.
        APPEND l_linea TO i_codigo.
      ENDIF.
    ENDLOOP.

    cl_gui_frontend_services=>clipboard_export(
      IMPORTING
        data                 = i_codigo
      CHANGING
        rc                   = l_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).

    IF sy-subrc <> 0.
      MESSAGE 'Error exportando datos al portapapeles' TYPE 'I'.
    ENDIF.
  ENDMETHOD.
  METHOD crear_juego_datos.
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        client              = sy-mandt
        group               = groupid
        user                = user
        keep                = keep
        holddate            = holddata
      EXCEPTIONS
        client_invalid      = 1
        destination_invalid = 2
        group_invalid       = 3
        group_is_locked     = 4
        holddate_invalid    = 5
        internal_error      = 6
        queue_error         = 7
        running             = 8
        system_lock_error   = 9
        user_invalid        = 10  ##NUMBER_OK
        OTHERS              = 11 ##NUMBER_OK.

    IF sy-subrc = 0.
      v_bdc_iniciado = 'X'.
    ELSE.
      CLEAR v_bdc_iniciado.
    ENDIF.
  ENDMETHOD.
  METHOD dynpro.
    CLEAR bdcdata.
    bdcdata-program  = program.
    bdcdata-dynpro   = dynpro.
    bdcdata-dynbegin = 'X'.
    APPEND bdcdata TO i_bdcdata.
    v_bdc_numlineas_bdcdata = v_bdc_numlineas_bdcdata + 1.

    IF NOT okcode IS INITIAL.
      campos( campo = 'BDC_OKCODE' valor = okcode ).
    ENDIF.
  ENDMETHOD.
  METHOD formatear_mensaje.
** Comprobamos si hubo errores.
*  DESCRIBE TABLE i_mensajes LINES sy-tfill.
*  IF SUBRC = 0 and sy-tfill > 1.
*    READ TABLE i_mensajes into est_mensaje INDEX sy-tfill.
*    IF SY-SUBRC = 0.
*
*      MSGID = est_mensaje-MSGID.
*      msgno = est_mensaje-MSGNR.
*      MSGTY = est_mensaje-MSGTYP.
*      MSGV1 = est_mensaje-MSGV1.
*      MSGV2 = est_mensaje-MSGV2.
*      MSGV3 = est_mensaje-MSGV3.
*      MSGV4 = est_mensaje-MSGV4.
*    ENDIF.
*  ENDIF.
* Obtenemos el texto con el mensaje de error
    IF NOT msgty IS INITIAL.
      MESSAGE ID msgid TYPE msgty NUMBER msgno WITH
              msgv1 msgv2 msgv3 msgv4 INTO texto.
    ELSE.
      msgty = 'E'.
      texto = '¡Se ha producido un error inesperado!'(spe).
    ENDIF.
  ENDMETHOD.
  METHOD get_atributos_campo.
    DATA: l_tabla TYPE string,
          l_campo TYPE string,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_aux   TYPE string.

    CLEAR dd03l.
    IF campo NS '-'.
      RETURN.
    ENDIF.

    SPLIT campo AT '-' INTO l_tabla l_campo.
    IF l_campo CS '('.
      SPLIT l_campo AT '(' INTO l_campo l_aux.
    ENDIF.
    SELECT * FROM dd03l
      INTO dd03l
      UP TO 1 ROWS
     WHERE tabname   = l_tabla
       AND fieldname = l_campo
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_descripcion_campo.
    DATA l_dd03l TYPE dd03l.

    l_dd03l = get_atributos_campo( campo ).
    IF NOT l_dd03l IS INITIAL.
      SELECT ddtext FROM dd04t
        INTO ddtext
        UP TO 1 ROWS
       WHERE ddlanguage = sy-langu
         AND rollname   = l_dd03l-rollname
        ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.
  METHOD inicio.
    CLEAR: i_bdcdata, bdcdata.
    REFRESH i_bdcdata.

    CLEAR: v_bdc_iniciado, v_bdc_numlineas_bdcdata, est_mensaje,
           i_mensajes, subrc, msgid, msgno, msgty, msgv1, msgv2,
           msgv3, msgv4, texto, lights.
  ENDMETHOD.
  METHOD llamar_transaccion.
    " TODO: parameter UPDMODE is never used (ABAP cleaner)

    DATA l_opt TYPE ctu_params.

    CLEAR i_mensajes.
    CLEAR: subrc, msgid, msgno, msgty, msgv1, msgv2, msgv3, msgv4,
           mensaje.

    l_opt-defsize  = defsize.
    l_opt-nobinpt  = nobinpt.
    l_opt-racommit = racommit.

    IF l_opt IS INITIAL.
      CALL TRANSACTION tcode                             "#EC CI_CALLTA
           USING i_bdcdata
           MODE  modo
           UPDATE 'S'
           MESSAGES INTO i_mensajes. "#EC *

    ELSE.
      l_opt-dismode = modo.
      l_opt-updmode = 'S'.
      CALL TRANSACTION tcode                             "#EC CI_CALLTA
           USING i_bdcdata
           OPTIONS FROM l_opt
           MESSAGES INTO i_mensajes. "#EC *
    ENDIF.

    IF capturar_msg = 'X'.
      IF sy-msgid = '00' AND sy-msgno = '344' AND sy-msgty = 'S'.
        sy-msgty = 'E'.
      ENDIF.

      msgid = sy-msgid.
      msgno = sy-msgno.
      msgty = sy-msgty.
      msgv1 = sy-msgv1.
      msgv2 = sy-msgv2.
      msgv3 = sy-msgv3.
      msgv4 = sy-msgv4.

      formatear_mensaje( ).

      mensaje = texto.

      IF msgty = 'S'.
        IF sy-msgno = '347' AND sy-msgid = '00'.
          msgty = 'E'.
          lights = '1'.
        ELSE.
          lights = '3'.
        ENDIF.
      ELSE.
        lights = '1'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD llamar_transaccion2.

    CLEAR i_mensajes.
    CLEAR: subrc, msgid, msgno, msgty, msgv1, msgv2, msgv3, msgv4, t_mensajes.

    CALL TRANSACTION tcode                               "#EC CI_CALLTA
         USING i_bdcdata
         MODE  modo
         UPDATE 'S'
         MESSAGES INTO i_mensajes.
    t_mensajes = i_mensajes.
  ENDMETHOD.
