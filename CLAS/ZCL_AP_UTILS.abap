  CLASS lcl_event DEFINITION.
    PUBLIC SECTION.
      data padre type ref to zcl_ap_utils.
      METHODS m_timer_finished FOR EVENT finished OF cl_gui_timer.
  ENDCLASS.                    "lcl_event DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
  CLASS lcl_event IMPLEMENTATION.
    METHOD m_timer_finished.
*   refresh selection
*    get_data_somewhere( ).
*   trigger PAI to force refresh of overview
*      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*        EXCEPTIONS
*          OTHERS = 0.
      padre->accion_refresco( ).
    ENDMETHOD.                    "handle_finished
  ENDCLASS.                    "lcl_event IMPLEMENTATION
CLASS zcl_ap_utils DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA o_timer              TYPE REF TO cl_gui_timer.
    DATA mensaje_timer        TYPE string.
    DATA msgty                TYPE sy-msgty.
    DATA commit               TYPE abap_bool.
    DATA segundos             TYPE int4.
    DATA hora_inicio_medicion TYPE uzeit.

    CLASS-METHODS comprobar_bloqueo
      IMPORTING clave            TYPE any
                clave2           TYPE any       DEFAULT ''
                clave3           TYPE any       DEFAULT ''
                clave4           TYPE any       DEFAULT ''
                !uname           TYPE sy-uname  DEFAULT '*'
                borrar           TYPE abap_bool DEFAULT ''
                tabla            TYPE any       DEFAULT ''
      EXPORTING datos_bloqueo    TYPE seqg3
      RETURNING VALUE(bloqueado) TYPE abap_bool.

    CLASS-METHODS mantener_tabla
      IMPORTING tabla         TYPE any
                filtro        TYPE any            DEFAULT ''
                campo_filtro  TYPE any            DEFAULT ''
                op_filtro     TYPE any            DEFAULT ''
                valor_filtro  TYPE any            DEFAULT ''
                campo_filtro2 TYPE any            DEFAULT ''
                op_filtro2    TYPE any            DEFAULT ''
                valor_filtro2 TYPE any            DEFAULT ''
                !action       TYPE c              DEFAULT 'U'
                se16          TYPE abap_bool      DEFAULT ''
                i_filtros     TYPE scprvimsellist OPTIONAL
                campo_filtro3 TYPE any            DEFAULT ''
                op_filtro3    TYPE any            DEFAULT ''
                valor_filtro3 TYPE any            DEFAULT ''
                and_or        TYPE any            DEFAULT 'AND'
                fecha         TYPE dats           DEFAULT '00000000'.

    CLASS-METHODS get_texto_dominio
      IMPORTING dominio      TYPE any
                valor        TYPE any
                idioma       TYPE sy-langu DEFAULT sy-langu
      RETURNING VALUE(texto) TYPE dd07t-ddtext.

    CLASS-METHODS comprobar_bloqueo_time
      IMPORTING clave            TYPE any
                clave2           TYPE any       DEFAULT ''
                clave3           TYPE any       DEFAULT ''
                clave4           TYPE any       DEFAULT ''
                !uname           TYPE sy-uname  DEFAULT '*'
                borrar           TYPE abap_bool DEFAULT ''
                segundos         TYPE i         DEFAULT 60
                tabla            TYPE any       DEFAULT ''
      RETURNING VALUE(bloqueado) TYPE abap_bool.

    CLASS-METHODS get_valor_param
      IMPORTING parametro    TYPE any
      RETURNING VALUE(valor) TYPE text40.

    CLASS-METHODS break_param
      IMPORTING parametro TYPE any DEFAULT 'ZBP'.

    CLASS-METHODS usuario_bloqueador
      IMPORTING clave          TYPE any
                clave2         TYPE any DEFAULT ''
                clave3         TYPE any DEFAULT ''
                clave4         TYPE any DEFAULT ''
                tabla          TYPE any DEFAULT ''
      RETURNING VALUE(usuario) TYPE sy-uname.

    CLASS-METHODS parar_fondo
      IMPORTING !uname       TYPE sy-uname DEFAULT zcl_c=>usuario_ap
                intentos     TYPE i        DEFAULT 100
                max_segundos TYPE i        DEFAULT 300
      PREFERRED PARAMETER uname.

    CLASS-METHODS call_se16n
      IMPORTING tabla    TYPE any
                sap_edit TYPE abap_bool DEFAULT ''
                variante TYPE any       DEFAULT ''.

    CLASS-METHODS bloquear_programa
      IMPORTING cprog        TYPE sy-cprog DEFAULT sy-cprog
                intentos     TYPE i        DEFAULT 10
                espera       TYPE i        DEFAULT 2
      RETURNING VALUE(error) TYPE abap_bool.

    CLASS-METHODS mantener_cluster
      IMPORTING tabla         TYPE any
                filtro        TYPE any            DEFAULT ''
                campo_filtro  TYPE any            DEFAULT ''
                op_filtro     TYPE any            DEFAULT ''
                valor_filtro  TYPE any            DEFAULT ''
                campo_filtro2 TYPE any            DEFAULT ''
                op_filtro2    TYPE any            DEFAULT ''
                valor_filtro2 TYPE any            DEFAULT ''
                !action       TYPE c              DEFAULT 'U'
                se16          TYPE abap_bool      DEFAULT ''
                i_filtros     TYPE scprvimsellist OPTIONAL.

    CLASS-METHODS beep
      IMPORTING repeticiones TYPE i DEFAULT 1.

    CLASS-METHODS reproducir_sonido
      IMPORTING fichero        TYPE any DEFAULT 'C:\WINDOWS\media\tada.wav'
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    CLASS-METHODS reproducir_voz
      IMPORTING palabras       TYPE any OPTIONAL
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    CLASS-METHODS concat
      IMPORTING p1            TYPE any       OPTIONAL
                p2            TYPE any       OPTIONAL
                p3            TYPE any       OPTIONAL
                p4            TYPE any       OPTIONAL
                p5            TYPE any       OPTIONAL
                p6            TYPE any       OPTIONAL
                p7            TYPE any       OPTIONAL
                meins         TYPE meins     DEFAULT 'ST'
                parent1       TYPE any       DEFAULT ''
                parent2       TYPE any       DEFAULT ''
                parent3       TYPE any       DEFAULT ''
                parent4       TYPE any       DEFAULT ''
                parent5       TYPE any       DEFAULT ''
                parent6       TYPE any       DEFAULT ''
                parent7       TYPE any       DEFAULT ''
                p8            TYPE any       OPTIONAL
                p9            TYPE any       OPTIONAL
                parent8       TYPE any       DEFAULT ''
                parent9       TYPE any       DEFAULT ''
                separador     TYPE any       DEFAULT space
                p10           TYPE any       OPTIONAL
                parent10      TYPE any       DEFAULT ''
                p11           TYPE any       OPTIONAL
                parent11      TYPE any       DEFAULT ''
                hora_larga    TYPE abap_bool DEFAULT ''
                p12           TYPE any       OPTIONAL
                parent12      TYPE any       DEFAULT ''
                p13           TYPE any       OPTIONAL
                parent13      TYPE any       DEFAULT ''
                p14           TYPE any       OPTIONAL
                parent14      TYPE any       DEFAULT ''
                p15           TYPE any       OPTIONAL
                parent15      TYPE any       DEFAULT ''
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS format
      IMPORTING valor         TYPE any       OPTIONAL
                no_cero       TYPE abap_bool DEFAULT 'X'
                hora_larga    TYPE abap_bool DEFAULT ''
                meins         TYPE meins     DEFAULT ''
                parentesis    TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER valor
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS message
      IMPORTING !type                          TYPE msgty     DEFAULT 'E'
                p1                             TYPE any       DEFAULT ''
                p2                             TYPE any       DEFAULT ''
                p3                             TYPE any       DEFAULT ''
                p4                             TYPE any       DEFAULT ''
                p5                             TYPE any       DEFAULT ''
                p6                             TYPE any       DEFAULT ''
                p7                             TYPE any       DEFAULT ''
                meins                          TYPE meins     DEFAULT 'ST'
                guardar_log                    TYPE abap_bool DEFAULT ''
                clave_log                      TYPE any       DEFAULT ''
                proceso_log                    TYPE any       DEFAULT ''
                p8                             TYPE any       DEFAULT ''
                p9                             TYPE any       DEFAULT ''
                !mail                          TYPE any       DEFAULT ''
                solo_log                       TYPE abap_bool DEFAULT ''
                no_break                       TYPE abap_bool DEFAULT ''
                usuarios_cambio_error_x_warnin TYPE any       DEFAULT ''
                solo_primera_vez               TYPE any       DEFAULT ''
                msgv1                          TYPE any       DEFAULT ''
                msgv2                          TYPE any       DEFAULT ''
                msgv3                          TYPE any       DEFAULT ''
                msgv4                          TYPE any       DEFAULT ''
                p10                            TYPE any       DEFAULT ''
                p11                            TYPE any       DEFAULT ''
      RETURNING VALUE(message)                 TYPE string.

    CLASS-METHODS ejecutar_comando
      IMPORTING comando    TYPE any
                parametros TYPE any DEFAULT ''
      EXPORTING !message   TYPE bapi_msg
                !log       TYPE lca_tracefile_tab.

    CLASS-METHODS get_valor_dominio
      IMPORTING dominio      TYPE any
                idioma       TYPE sy-langu DEFAULT sy-langu
                VALUE(texto) TYPE any
      RETURNING VALUE(valor) TYPE dd07t-domvalue_l.

    CLASS-METHODS get_codepage
      IMPORTING db              TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER db
      RETURNING VALUE(codepage) TYPE tcp00-cpcodepage.

    CLASS-METHODS set_runtime_info.

    CLASS-METHODS get_runtime_info
      IMPORTING get_xml TYPE abap_bool DEFAULT ''
      EXPORTING tabla   TYPE table
                !xml    TYPE xstring.

    METHODS set_timer
      IMPORTING segundos      TYPE i         DEFAULT 60
                mensaje       TYPE any       DEFAULT 'Refrescando'(REF)
                msgty         TYPE any       DEFAULT 'X'
                !commit       TYPE abap_bool DEFAULT 'X'
                reintentos    TYPE int4      DEFAULT -1
                cancel_previo TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER segundos.

    METHODS accion_refresco.

    CLASS-METHODS busca_cambios_datos
      IMPORTING original     TYPE any
                modificacion TYPE any       DEFAULT ''
                ddic         TYPE abap_bool DEFAULT ''
      CHANGING  cambios      TYPE any.

    CLASS-METHODS get_system_info
      RETURNING VALUE(info) TYPE rfcsi.

    CLASS-METHODS crear_estructura
      IMPORTING tabla      TYPE table
                estructura TYPE ddobjname.

    CLASS-METHODS existe_en_pila
      IMPORTING evento    TYPE any OPTIONAL
                programa  TYPE any OPTIONAL
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS get_tcode_real
      RETURNING VALUE(tcode) TYPE sy-tcode.

    METHODS cancel_timer.

    CLASS-METHODS grabar_tabla_en_ot
      IMPORTING tabla  TYPE tabname
                i_keys TYPE table_of_strings.

    METHODS inicio_medicion.
    METHODS fin_medicion.

    CLASS-METHODS es_in_update_task
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS desbloquear_programa
      IMPORTING cprog        TYPE sy-cprog DEFAULT sy-cprog
      RETURNING VALUE(error) TYPE abap_bool.


  PRIVATE SECTION.
    DATA reintentos TYPE int4 VALUE -999 ##NO_TEXT.
endclass. "ZCL_AP_UTILS definition
class ZCL_AP_UTILS implementation.
  METHOD accion_refresco.
    IF msgty = 'X'.
      zcl_ap_sgpi=>text( texto = mensaje_timer ).
      MESSAGE mensaje_timer TYPE 'S'.
    ELSEIF msgty CO 'SIWE'.
      MESSAGE mensaje_timer TYPE msgty.
    ENDIF.

    IF commit = 'X'.
      COMMIT WORK AND WAIT.
    ENDIF.

    IF reintentos = -1.
      o_timer->run( ).
    ELSE.
      IF me->reintentos > 0.
        o_timer->run( ).
      ENDIF.
      me->reintentos = me->reintentos - 1.
    ENDIF.
  ENDMETHOD.
  METHOD beep.
    DATA wordbasic  TYPE ole2_object.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA objectvar1 TYPE ole2_object.

    INCLUDE ole2incl ##INCL_OK.

    CREATE OBJECT wordbasic 'Word.Basic'.

    DO repeticiones TIMES.
      CALL METHOD OF wordbasic 'Beep' = objectvar1
        EXPORTING #1 = '1'.
    ENDDO.
  ENDMETHOD.
  METHOD bloquear_programa.
    DATA prg TYPE indx-srtfd.

    prg = cprog.

    error = 'X'.

    DO intentos TIMES.
      CALL FUNCTION 'ENQUEUE_ESINDX'
        EXPORTING
          relid          = 'ZZ'
          srtfd          = prg
          srtf2          = 0
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2.

      IF sy-subrc <> 0.
        IF espera IS INITIAL.
          MESSAGE 'El programa ya se está ejecutando. Salimos' TYPE 'S'.
        ELSE.
          MESSAGE 'El programa ya se está ejecutando. Espere'(pee) TYPE 'S'.
          WAIT UP TO espera SECONDS.
        ENDIF.
      ELSE.
        CLEAR error.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD break_param.
    IF get_valor_param( parametro ) = 'X'.
      BREAK-POINT.                                            "#EC *
    ENDIF.
  ENDMETHOD.
  METHOD busca_cambios_datos.
    DATA: lr_data         TYPE REF TO data,
          lo_tabdescr     TYPE REF TO cl_abap_structdescr,
          campos          TYPE ddfields,
          l_campo         TYPE c LENGTH 100,
          o_campos_origen TYPE REF TO cl_abap_structdescr,
          i_campos_origen TYPE cl_abap_structdescr=>component_table,
          i_campos        TYPE abap_component_tab.

    FIELD-SYMBOLS: <campo>         TYPE dfies,
                   <origen>        TYPE any,
                   <cambios>       TYPE any,
                   <modificacion>  TYPE any,
                   <campos_origen> TYPE cl_abap_structdescr=>component.

    IF ddic = 'X'.
      CREATE DATA lr_data LIKE original.
      lo_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data  ).

      lo_tabdescr->get_ddic_field_list(
*    EXPORTING
*      p_langu                  = SY-LANGU
*      p_including_substructres = ABAP_FALSE
        RECEIVING
          p_field_list = campos
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3 ).

      LOOP AT campos ASSIGNING <campo>.
        ASSIGN COMPONENT <campo>-fieldname OF STRUCTURE original TO <origen>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        CONCATENATE 'CAMBIOS-' <campo>-fieldname INTO l_campo.
        ASSIGN (l_campo) TO <cambios>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF modificacion IS INITIAL.
          IF NOT <origen> IS INITIAL.
            <cambios> = 'X'.
          ENDIF.
        ELSE.
          CONCATENATE 'MODIFICACION-' <campo>-fieldname INTO l_campo.
          ASSIGN (l_campo) TO <modificacion>.
          IF sy-subrc = 0.
            IF <modificacion> <> <origen>.
              <cambios> = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      o_campos_origen ?= cl_abap_structdescr=>describe_by_data( original ).
      LOOP AT i_campos_origen ASSIGNING <campos_origen>.
        ASSIGN COMPONENT <campos_origen>-name OF STRUCTURE original TO <origen>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        CONCATENATE 'CAMBIOS-' <campos_origen>-name INTO l_campo.
        ASSIGN (l_campo) TO <cambios>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF modificacion IS INITIAL.
          IF NOT <origen> IS INITIAL.
            <cambios> = 'X'.
          ENDIF.
        ELSE.
          CONCATENATE 'MODIFICACION-' <campos_origen>-name INTO l_campo.
          ASSIGN (l_campo) TO <modificacion>.
          IF sy-subrc = 0.
            IF <modificacion> <> <origen>.
              <cambios> = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD call_se16n.
    DATA: l_tabla   TYPE se16n_tab,
          l_variant TYPE slis_vari.

    l_tabla = tabla.

*  IF SAP_EDIT IS INITIAL.
*  CALL FUNCTION 'SE16N_START'
*   EXPORTING
*     i_tab                    = l_tabla
*     i_display                = 'X'
**   I_EXIT_SELFIELD_FB       = ' '
*            .
*  ELSE.
    l_variant = variante.
    CALL FUNCTION 'SE16N_INTERFACE'                "#EC FM_SUBRC_OK
      EXPORTING
        i_tab     = l_tabla
        i_edit    = sap_edit
        i_sapedit = sap_edit
*     I_NO_TXT  = ' '
*     I_MAX_LINES              = 500
*     I_LINE_DET               = ' '
*     I_DISPLAY = 'X'
*     I_CLNT_SPEZ              = ' '
*     I_CLNT_DEP               = ' '
        i_variant = l_variant
*     I_OLD_ALV = ' '
*     I_CHECKKEY               = ' '
*     I_TECH_NAMES             = ' '
*     I_CWIDTH_OPT_OFF         = ' '
*     I_SCROLL  = ' '
*     I_NO_CONVEXIT            = ' '
*     I_LAYOUT_GET             = ' '
*     IMPORTING
*     E_LINE_NR =
*     E_DREF    =
*     TABLES
*     IT_SELFIELDS             =
*     IT_OUTPUT_FIELDS         =
*     IT_OR_SELFIELDS          =
*     IT_CALLBACK_EVENTS       =
      EXCEPTIONS
        no_values = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Error llamando SE16N'(e16) TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD cancel_timer.
    IF NOT o_timer IS INITIAL.
      o_timer->cancel( ).
      CLEAR o_timer.
    ENDIF.
  ENDMETHOD.
  METHOD comprobar_bloqueo.
    DATA: l_garg   TYPE seqg3-garg,
          l_number TYPE i VALUE 0,
          i_seqg3  TYPE TABLE OF seqg3.

    CLEAR bloqueado.

    CONCATENATE clave clave2 clave3 clave4 INTO l_garg.

    CLEAR sy-msgv1.
    CALL FUNCTION 'ENQUEUE_READ'                              "#EC *
      EXPORTING
        gclient               = sy-mandt
        gname                 = tabla
        garg                  = l_garg
        guname                = uname
      IMPORTING
        number                = l_number
      TABLES
        enq                   = i_seqg3
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.
    IF l_number <> 0 OR sy-subrc <> 0.
      bloqueado = 'X'.
      READ TABLE i_seqg3 INTO datos_bloqueo INDEX 1.
      IF sy-msgv1 IS INITIAL AND sy-subrc = 0.
        sy-msgv1 = datos_bloqueo-guname.
      ENDIF.
    ENDIF.

    IF l_number <> 0 AND borrar = 'X'.
      DO 3 TIMES.
        COMMIT WORK AND WAIT.
        CALL FUNCTION 'ENQUEUE_READ'                          "#EC *
          EXPORTING
            gclient               = sy-mandt
            gname                 = tabla
            garg                  = l_garg
            guname                = uname
          IMPORTING
            number                = l_number
          TABLES
            enq                   = i_seqg3
          EXCEPTIONS
            communication_failure = 1
            system_failure        = 2
            OTHERS                = 3.
        IF l_number <> 0.
          WAIT UP TO 1 SECONDS.
        ELSE.
          EXIT.

        ENDIF.
      ENDDO.
    ENDIF.

    IF l_number <> 0 AND borrar = 'X'.
      CALL FUNCTION 'ENQUE_DELETE'
        EXPORTING
*       CHECK_UPD_REQUESTS    = 0
          suppress_syslog_entry = 'X'
*     IMPORTING
*       SUBRC                 =
        TABLES
          enq                   = i_seqg3.
    ENDIF.
  ENDMETHOD.
  METHOD comprobar_bloqueo_time.
    " TODO: parameter BORRAR is never used (ABAP cleaner)

* Verificamos bloqueos.
    DO segundos TIMES.
      IF zcl_ap_utils=>comprobar_bloqueo( tabla  = tabla
                                          clave  = clave
                                          clave2 = clave2
                                          clave3 = clave3
                                          clave4 = clave4
                                          uname  = uname ) = ''.
        EXIT.
      ELSE.
* Si el aviso estuviera bloqueado, esperamos....
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD concat.
    DATA: l_p1  TYPE string,
          l_p2  TYPE string,
          l_p3  TYPE string,
          l_p4  TYPE string,
          l_p5  TYPE string,
          l_p6  TYPE string,
          l_p7  TYPE string,
          l_p8  TYPE string,
          l_p9  TYPE string,
          l_p10 TYPE string,
          l_p11 TYPE string,
          l_p12 TYPE string,
          l_p13 TYPE string,
          l_p14 TYPE string,
          l_p15 TYPE string.

    IF NOT p1 IS INITIAL. l_p1 = format( valor = p1 meins = meins parentesis = parent1 hora_larga = hora_larga ). ENDIF.
    IF p2 IS INITIAL AND p3 IS INITIAL AND p4 IS INITIAL AND p5 IS INITIAL. " Optimizáción para no perder tiempo
      string = l_p1.
      RETURN.
    ENDIF.

    IF NOT p2 IS INITIAL. l_p2 = format( valor = p2 meins = meins parentesis = parent2 hora_larga = hora_larga ). ENDIF.
    IF p3 IS INITIAL AND p4 IS INITIAL AND p5 IS INITIAL AND p6 IS INITIAL. " Optimizáción para no perder tiempo
      CONCATENATE l_p1 l_p2 INTO string SEPARATED BY separador.
      RETURN.
    ENDIF.

    IF NOT p3 IS INITIAL. l_p3 = format( valor = p3 meins = meins parentesis = parent3 hora_larga = hora_larga ). ENDIF.
    IF NOT p4 IS INITIAL. l_p4 = format( valor = p4 meins = meins parentesis = parent4 hora_larga = hora_larga ). ENDIF.
    IF NOT p5 IS INITIAL. l_p5 = format( valor = p5 meins = meins parentesis = parent5 hora_larga = hora_larga ). ENDIF.
    IF NOT p6 IS INITIAL. l_p6 = format( valor = p6 meins = meins parentesis = parent6 hora_larga = hora_larga ). ENDIF.
    IF NOT p7 IS INITIAL. l_p7 = format( valor = p7 meins = meins parentesis = parent7 hora_larga = hora_larga ). ENDIF.
    IF NOT p8 IS INITIAL. l_p8 = format( valor = p8 meins = meins parentesis = parent8 hora_larga = hora_larga ). ENDIF.
    IF NOT p9 IS INITIAL. l_p9 = format( valor = p9 meins = meins parentesis = parent9 hora_larga = hora_larga ). ENDIF.
    IF NOT p10 IS INITIAL. l_p10 = format( valor = p10 meins = meins parentesis = parent10 hora_larga = hora_larga ). ENDIF.
    IF NOT p11 IS INITIAL. l_p11 = format( valor = p11 meins = meins parentesis = parent11 hora_larga = hora_larga ). ENDIF.
    IF NOT p12 IS INITIAL. l_p12 = format( valor = p12 meins = meins parentesis = parent12 hora_larga = hora_larga ). ENDIF.
    IF NOT p13 IS INITIAL. l_p13 = format( valor = p13 meins = meins parentesis = parent13 hora_larga = hora_larga ). ENDIF.
    IF NOT p14 IS INITIAL. l_p14 = format( valor = p14 meins = meins parentesis = parent14 hora_larga = hora_larga ). ENDIF.
    IF NOT p15 IS INITIAL. l_p15 = format( valor = p15 meins = meins parentesis = parent15 hora_larga = hora_larga ). ENDIF.

    CONCATENATE l_p1 l_p2 l_p3 l_p4 l_p5 l_p6 l_p7 l_p8 l_p9 l_p10 l_p11 l_p12 l_p13 l_p14 l_p15 INTO string SEPARATED BY separador.
  ENDMETHOD.
  METHOD crear_estructura.
    DATA: dd02v    TYPE dd02v,
          string   TYPE string,
          i_campos TYPE lvc_t_fcat.

    FIELD-SYMBOLS <campos> TYPE lvc_s_fcat.

    __data_set_vart dd03p.

    SELECT tabname FROM dd02l
      INTO dd02v-tabname
      UP TO 1 ROWS
     WHERE tabname = estructura
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      CONCATENATE 'Estructura' estructura 'ya existe' INTO string SEPARATED BY space.
      IF zcl_ap_popup=>confirmar( texto = string texto2 = '¿Está seguro de querer actualizarla?' opcion = 'N' ) = ''.
        RETURN.
      ENDIF.
    ENDIF.

    i_campos = zcl_ap_dev=>get_fieldcatalog_tabla_alv( tabla ).

    dd02v-tabclass  = 'INTTAB'.
    dd02v-tabname   = estructura.
    dd02v-as4user   = sy-uname.
    dd02v-as4time   = sy-uzeit.
    dd02v-as4date   = sy-datum.
    dd02v-applclass = ''.
    dd02v-authclass = ''.

    LOOP AT i_campos ASSIGNING <campos>.
      CLEAR l_dd03p.
      MOVE-CORRESPONDING <campos> TO l_dd03p.
      l_dd03p-tabname  = estructura.
      l_dd03p-position = sy-tabix.
      APPEND l_dd03p TO i_dd03p.
    ENDLOOP.

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name      = estructura
        dd02v_wa  = dd02v
      TABLES
        dd03p_tab = i_dd03p.

    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name = estructura.

    MESSAGE i398(00) WITH 'Se ha creado estructura' estructura.
  ENDMETHOD.
  METHOD desbloquear_programa.
    DATA prg TYPE indx-srtfd.

    prg = cprog.

    CALL FUNCTION 'DEQUEUE_ESINDX'
      EXPORTING
        mode_indx = 'E'
        mandt     = sy-mandt
        relid     = 'ZZ'
        srtfd     = prg
        srtf2     = 0.
*     X_RELID   = ' '
*     X_SRTFD   = ' '
*     X_SRTF2   = ' '
*     _SCOPE    = '3'
*     _SYNCHRON = ' '
*     _COLLECT  = ' '

    IF sy-subrc = 0.
      error = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD ejecutar_comando.
    DATA: parameters TYPE sxpgcolist-parameters,
          status     TYPE extcmdexex-status,
          exitcode   TYPE extcmdexex-exitcode.
    DATA sxpgcostab TYPE sxpgcostab.

    CLEAR message.

    SELECT name parameters FROM  sxpgcostab
      INTO CORRESPONDING FIELDS OF sxpgcostab
      UP TO 1 ROWS
     WHERE name = comando
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      CONCATENATE 'Defina entrada'(dfe) comando 'en SM69'(s69) INTO message SEPARATED BY space.
    ENDIF.

    sxpgcostab-parameters = parametros.
    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = sxpgcostab-name
        additional_parameters         = sxpgcostab-parameters
*     OPERATINGSYSTEM               = SY-OPSYS
*     TARGETSYSTEM                  = SY-HOST
*     DESTINATION                   =
*     STDOUT                        = 'X'
*     STDERR                        = 'X'
*     TERMINATIONWAIT               = 'X'
*     TRACE                         =
*     DIALOG                        =
      IMPORTING
        status                        = status
        exitcode                      = exitcode
      TABLES
        exec_protocol                 = log
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.

    IF sy-subrc <> 0.
      IF sy-msgty IS INITIAL.
        CONCATENATE 'Error llamando'(erl) comando parametros INTO message SEPARATED BY space.
      ELSE.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO message.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD es_in_update_task.
    DATA lv_update_process TYPE sy-subrc.

    CALL FUNCTION 'TH_IN_UPDATE_TASK'
      IMPORTING
        in_update_task = lv_update_process.
    IF lv_update_process = 1.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD existe_en_pila.
    DATA i_callstack TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        et_callstack = i_callstack.

    IF NOT programa IS INITIAL AND NOT evento IS INITIAL.
      IF line_exists( i_callstack[ progname  = programa
                                   eventname = evento ] ).
        si = 'X'.
      ENDIF.
    ELSE.
      IF NOT programa IS INITIAL.
        IF line_exists( i_callstack[ progname = programa ] ).
          si = 'X'.
        ENDIF.
      ENDIF.
      IF NOT evento IS INITIAL.
        IF line_exists( i_callstack[ eventname = evento ] ).
          si = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD fin_medicion.
    GET TIME.
    segundos = sy-uzeit - hora_inicio_medicion.
  ENDMETHOD.
  METHOD format.
    " TODO: parameter NO_CERO is never used (ABAP cleaner)

    DATA: l_tipo   TYPE c LENGTH 1,
          l_string TYPE c LENGTH 255,
          l_hora   TYPE c LENGTH 8.

    DESCRIBE FIELD valor TYPE l_tipo.

    CASE l_tipo.
      WHEN 'P'.
        IF meins IS INITIAL.
          WRITE valor TO l_string.
        ELSE.
          WRITE valor TO l_string UNIT meins.
        ENDIF.
      WHEN 'T'.
        WRITE valor TO l_hora.
        IF hora_larga IS INITIAL.
          CLEAR l_hora+5.
        ENDIF.
        l_string = l_hora.
      WHEN OTHERS.
        WRITE valor TO l_string.
    ENDCASE.
    CONDENSE l_string.
    IF parentesis = 'X'.
      CONCATENATE '(' l_string ')' INTO l_string.
    ENDIF.
    string = l_string.
  ENDMETHOD.
  METHOD get_codepage.
    DATA l_db TYPE tcp00-cpcodepage.

    CALL FUNCTION 'SCP_GET_CODEPAGE_NUMBER'
      EXPORTING
        database_also     = ''
      IMPORTING
*     start_appl_codepage       = start_appl_codepage
*     APPL_CODEPAGE     = APPL_CODEPAGE
        gui_codepage      = codepage
        database_codepage = l_db
*     DATABASE_NONUNIQ  = DATABASE_NONUNIQ
*     APPL_FOR_DISPLAY  = APPL_FOR_DISPLAY
*     APPL_FOR_PROPOSE  = APPL_FOR_PROPOSE
*     APPL_FOR_INPUT    = APPL_FOR_INPUT
*     USER_LOGIN_CODEPAGE       = USER_LOGIN_CODEPAGE
*     USER_EMODE_CODEPAGE       = USER_EMODE_CODEPAGE
*   TABLES
*     DATABASE_CODEPAGES        = DATABASE_CODEPAGES
      EXCEPTIONS
        internal_error    = 1.

    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando codepage'(erc) TYPE 'S'.
    ENDIF.

    IF db = 'X'.
      codepage = l_db.
    ENDIF.
  ENDMETHOD.
  METHOD get_runtime_info.
    DATA : gr_table    TYPE REF TO cl_salv_table,
           lv_xml_type TYPE salv_bs_constant.

    FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.

    DATA:
      rspar       TYPE TABLE OF rsparams,
      lt_data_ref TYPE REF TO data.

    CLEAR: tabla, xml.

    TRY.
        cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING
            r_data            = lt_data_ref ).
      CATCH cx_salv_bs_sc_runtime_info.
        MESSAGE 'Data retrieval error'(dre) TYPE 'E'.
    ENDTRY.

    cl_salv_bs_runtime_info=>clear_all( ).
    ASSIGN lt_data_ref->* TO <fs_table>.
    IF sy-subrc = 0.
      tabla = <fs_table>.
    ENDIF.

    IF get_xml = 'X'.
      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = gr_table
            CHANGING
              t_table  = <fs_table> ).
        CATCH cx_salv_msg.
      ENDTRY.

***--  Convert the output the internal XML format
      lv_xml_type = if_salv_bs_xml=>c_type_mhtml.
      xml = gr_table->to_xml( xml_type = lv_xml_type ).
    ENDIF.
  ENDMETHOD.
  METHOD get_system_info.
    CALL FUNCTION 'RFC_GET_SYSTEM_INFO'
      EXPORTING
        destination             = 'NONE'
      IMPORTING
        rfcsi_export            = info
*     RFC_LOGIN_COMPLETE      =
*     DIALOG_USER_TYPE        =
*     CURRENT_RESOURCES       =
*     MAXIMAL_RESOURCES       =
*     RECOMMENDED_DELAY       =
*     DEST_COMMUNICATION_MESSAGE       =
*     DEST_SYSTEM_MESSAGE     =
      EXCEPTIONS
        authority_not_available = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Error llamando RFC_GET_SYSTEM_INFO'(elr) TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_tcode_real.
    CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD tcode.         "#EC CI_CCALL

    IF sy-tcode = tcode OR tcode IS INITIAL.
      tcode = cl_abap_syst=>get_transaction_code( ).
    ENDIF.
  ENDMETHOD.
  METHOD get_texto_dominio.
    CLEAR texto.
    SELECT ddtext FROM dd07t
      INTO texto
      UP TO 1 ROWS
     WHERE domname    = dominio
       AND ddlanguage = idioma
       AND domvalue_l = valor
     ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_valor_dominio.
    CLEAR valor.
    SELECT domvalue_l FROM dd07t
      INTO valor
      UP TO 1 ROWS
     WHERE domname    = dominio
       AND ddlanguage = idioma
       AND ddtext     = texto
    ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_valor_param.
    GET PARAMETER ID parametro FIELD valor.
  ENDMETHOD.
  METHOD grabar_tabla_en_ot.
    DATA: ls_e071  TYPE e071,
          lt_e071  TYPE tr_objects,
          ls_e071k TYPE e071k,
          lt_e071k TYPE tr_keys.

    ls_e071-as4pos   = 1.
    ls_e071-pgmid    = 'R3TR'.
    ls_e071-object   = 'TABU'.
    ls_e071-obj_name = tabla.
    ls_e071-objfunc  = 'K'.
    ls_e071-lang     = sy-langu.
    INSERT ls_e071 INTO TABLE lt_e071.

    LOOP AT i_keys ASSIGNING FIELD-SYMBOL(<key>).
      ls_e071k-pgmid      = 'R3TR'.
      ls_e071k-object     = 'TABU'.
      ls_e071k-objname    = ls_e071-obj_name.
      ls_e071k-mastertype = 'TABU'.
      ls_e071k-mastername = ls_e071-obj_name.
      ls_e071k-lang       = sy-langu.

      ls_e071k-tabkey     = <key>.

      INSERT ls_e071k INTO TABLE lt_e071k.
    ENDLOOP.
    IF lt_e071k IS INITIAL.
      MESSAGE 'No ha seleccionado ninguna línea' TYPE 'I'.
    ELSE.
      CALL FUNCTION 'TR_REQUEST_CHOICE'
        EXPORTING
*         IV_REQUEST_TYPES     =
*         IV_CLI_DEP           = ' '
*         IV_REQUEST           = ' '
          it_e071  = lt_e071
          it_e071k = lt_e071k
*         IV_LOCK_OBJECTS      = ' '
*         IV_NO_OWNER_CHECK    = ' '
        EXCEPTIONS
          OTHERS   = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD inicio_medicion.
    GET TIME.
    hora_inicio_medicion = sy-uzeit.
  ENDMETHOD.
  METHOD mantener_cluster.
    " TODO: parameter SE16 is never used (ABAP cleaner)

    DATA: l_filtro1 TYPE string,
          l_filtro2 TYPE string,
          l_seltab  TYPE vimsellist,
          i_seltab  TYPE TABLE OF vimsellist,
          l_vista   TYPE vcldir-vclname.

    IF i_filtros IS INITIAL.
      IF NOT filtro IS INITIAL.
        SPLIT filtro AT ' AND ' INTO l_filtro1 l_filtro2.
        IF l_filtro1 CS '='.
          CLEAR l_seltab.
          SPLIT l_filtro1 AT '=' INTO l_seltab-viewfield l_seltab-value.
          l_seltab-operator = 'EQ'.
          IF NOT l_filtro2 IS INITIAL.
            l_seltab-and_or = 'AND'.
          ENDIF.
          APPEND l_seltab TO i_seltab.
        ENDIF.
        IF l_filtro2 CS '='.
          CLEAR l_seltab.
          SPLIT l_filtro2 AT '=' INTO l_seltab-viewfield l_seltab-value.
          l_seltab-operator = 'EQ'.
          APPEND l_seltab TO i_seltab.
        ENDIF.
      ENDIF.

      IF NOT campo_filtro IS INITIAL.
        CLEAR l_seltab.
        l_seltab-viewfield = campo_filtro.
        l_seltab-value     = valor_filtro.
        l_seltab-operator  = op_filtro.
        IF NOT campo_filtro2 IS INITIAL.
          l_seltab-and_or = 'AND'.
        ENDIF.
        APPEND l_seltab TO i_seltab.

        IF NOT campo_filtro2 IS INITIAL.
          CLEAR l_seltab.
          l_seltab-viewfield = campo_filtro2.
          l_seltab-value     = valor_filtro2.
          l_seltab-operator  = op_filtro2.
          APPEND l_seltab TO i_seltab.
        ENDIF.
      ENDIF.
    ELSE.
      i_seltab = i_filtros.
    ENDIF.

    l_vista = tabla.

    CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'  ##NUMBER_OK
      EXPORTING
        viewcluster_name             = l_vista
*     START_OBJECT                 = '          '
        maintenance_action           = action
*     READ_KIND                    = ' '
*     SHOW_SELECTION_POPUP         = ' '
*     CORR_NUMBER                  = ' '
*     NO_WARNING_FOR_CLIENTINDEP   = ' '
*     RFC_DESTINATION              = ' '
*     SUPPRESS_WA_POPUP            = ' '
      TABLES
        dba_sellist                  = i_seltab
*     DBA_SELLIST_CLUSTER          =
*     EXCL_CUA_FUNCT_ALL_OBJECTS   =
*     EXCL_CUA_FUNCT_CLUSTER       =
*     DPL_SELLIST_FOR_START_OBJECT =
      EXCEPTIONS
        client_reference             = 1
        foreign_lock                 = 2
        viewcluster_not_found        = 3
        viewcluster_is_inconsistent  = 4
        missing_generated_function   = 5
        no_upd_auth                  = 6
        no_show_auth                 = 7
        object_not_found             = 8
        no_tvdir_entry               = 9
        no_clientindep_auth          = 10
        invalid_action               = 11   ##NUMBER_OK
        saving_correction_failed     = 12   ##NUMBER_OK
        system_failure               = 13   ##NUMBER_OK
        unknown_field_in_dba_sellist = 14   ##NUMBER_OK
        missing_corr_number          = 15   ##NUMBER_OK
        OTHERS                       = 16 ##NUMBER_OK.

    IF sy-subrc <> 0.
      MESSAGE 'Error en mantenimiento cluster'(emc) TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD mantener_tabla.
    DATA: l_filtro1        TYPE string,
          l_filtro2        TYPE string,
          l_seltab         TYPE vimsellist,
          i_seltab         TYPE TABLE OF vimsellist,
          i_excl_cua_funct TYPE TABLE OF vimexclfun,
          l_vista          TYPE dd02v-tabname.

    IF i_filtros IS INITIAL.
      IF NOT filtro IS INITIAL.
        SPLIT filtro AT ' AND ' INTO l_filtro1 l_filtro2.
        IF l_filtro1 CS '='.
          CLEAR l_seltab.
          SPLIT l_filtro1 AT '=' INTO l_seltab-viewfield l_seltab-value.
          l_seltab-operator = 'EQ'.
          IF NOT l_filtro2 IS INITIAL.
            l_seltab-and_or = and_or.
          ENDIF.
          APPEND l_seltab TO i_seltab.
        ENDIF.
        IF l_filtro2 CS '='.
          CLEAR l_seltab.
          SPLIT l_filtro2 AT '=' INTO l_seltab-viewfield l_seltab-value.
          l_seltab-operator = 'EQ'.
          APPEND l_seltab TO i_seltab.
        ENDIF.
      ENDIF.

      IF NOT campo_filtro IS INITIAL.
        CLEAR l_seltab.
        l_seltab-viewfield = campo_filtro.
        l_seltab-value     = valor_filtro.
        l_seltab-operator  = op_filtro.
        IF NOT campo_filtro2 IS INITIAL OR NOT fecha IS INITIAL.
          l_seltab-and_or = and_or.
        ENDIF.
        APPEND l_seltab TO i_seltab.

        IF NOT campo_filtro2 IS INITIAL.
          CLEAR l_seltab.
          l_seltab-viewfield = campo_filtro2.
          l_seltab-value     = valor_filtro2.
          l_seltab-operator  = op_filtro2.

          IF NOT campo_filtro3 IS INITIAL OR NOT fecha IS INITIAL.
            l_seltab-and_or = and_or.
          ENDIF.
          APPEND l_seltab TO i_seltab.

          IF NOT campo_filtro3 IS INITIAL.
            CLEAR l_seltab.
            l_seltab-viewfield = campo_filtro3.
            l_seltab-value     = valor_filtro3.
            l_seltab-operator  = op_filtro3.
            IF NOT fecha IS INITIAL.
              l_seltab-and_or = and_or.
            ENDIF.

            APPEND l_seltab TO i_seltab.
          ENDIF.
        ENDIF.
      ENDIF.

      IF NOT fecha IS INITIAL.
        CLEAR l_seltab.
        l_seltab-viewfield = 'BEGDA'.
        WRITE fecha TO l_seltab-value.
        l_seltab-operator = 'LE'.
        l_seltab-and_or   = 'AND'.
        APPEND l_seltab TO i_seltab.

        CLEAR l_seltab.
        l_seltab-viewfield = 'ENDDA'.
        WRITE fecha TO l_seltab-value.
        l_seltab-operator = 'GE'.
        APPEND l_seltab TO i_seltab.
      ENDIF.

    ELSE.
      i_seltab = i_filtros.
    ENDIF.

    IF action = 'S'.
      APPEND 'AEND' TO i_excl_cua_funct.
    ENDIF.

    l_vista = tabla.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action         = action
        view_name      = l_vista
      TABLES
        dba_sellist    = i_seltab
        excl_cua_funct = i_excl_cua_funct
                         EXCEPTIONS
                         client_reference
                         foreign_lock
                         invalid_action
                         no_clientindependent_auth
                         no_database_function
                         no_editor_function
                         no_show_auth
                         no_tvdir_entry
                         no_upd_auth
                         only_show_allowed
                         system_failure
                         unknown_field_in_dba_sellist
                         view_not_found
                         maintenance_prohibited.

    IF sy-subrc <> 0.
      IF se16 = 'X'.
        zcl_ap_utils=>call_se16n( l_vista ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD message.
    DATA l_type TYPE msgty.

    l_type = type.
    message = concat( p1 = p1 p2 = p2 p3 = p3 p4 = p4 p5 = p5 p6 = p6 p7 = p7 p8 = p8 p9 = p9 p10 = p10 p11 = p11 meins = meins ).

    IF guardar_log = 'X'.
      IF type = 'Y'.
        l_type = 'E'.
      ENDIF.
      zcl_ap_log=>set_log( proceso = proceso_log clave = clave_log msgty = type message = message mail = mail solo_primera_vez = solo_primera_vez msgv1 = msgv1 msgv2 = msgv2 msgv3 = msgv3 msgv4 = msgv4 ).
    ENDIF.

    IF no_break IS INITIAL.
      zcl_ap_dev=>break_condicional( texto = message ).
    ENDIF.

    IF solo_log IS INITIAL.
      IF type = 'Y'.
        l_type = 'I'.
      ENDIF.

      IF type = 'E' AND NOT usuarios_cambio_error_x_warnin IS INITIAL.
        IF zcl_ap_lista=>es_elemento( lista = usuarios_cambio_error_x_warnin elemento = sy-uname ) = 'X'.
          MESSAGE message TYPE 'W'.
        ELSE.
          MESSAGE message TYPE l_type.
        ENDIF.
      ELSE.
        MESSAGE message TYPE l_type.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD parar_fondo.
    DATA: l_fecha       TYPE sy-datum,
          l_hora        TYPE sy-uzeit,
          l_hora_actual TYPE sy-uzeit,
          l_segundos    TYPE eauszt,
          salir         TYPE c LENGTH 1.
*        mara TYPE TABLE OF mara.

    GET TIME.
    l_fecha = sy-datum.
    l_hora  = sy-uzeit.

    IF sy-uname <> uname.
      RETURN.
    ENDIF.

    DO intentos TIMES.
*      SELECT matnr FROM mara ""#EC CI_NOFIELD
*         UP TO 500 ROWS
*        INTO CORRESPONDING FIELDS OF TABLE mara
*       WHERE matnr NE ''.
      WAIT UP TO 1 SECONDS.

      GET TIME.
      GET TIME FIELD l_hora_actual.
      l_segundos = zcl_ap_fechas=>get_duracion_intervalo_sec( fini = l_fecha  hini = l_hora
                                                              ffin = sy-datum hfin = l_hora_actual ).
      IF l_segundos > max_segundos.
        salir = 'X'.
      ENDIF.

      IF salir = 'X'.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD reproducir_sonido.
    DATA: owmp TYPE ole2_object,
          ocnt TYPE ole2_object.

    CHECK NOT fichero IS INITIAL.

* This code works on Windows XP, if you are using Windows 7
* you must replace WMPlayer.ocx with WMPlayer.ocx.7
    CREATE OBJECT owmp 'WMPlayer.ocx'.
    IF sy-subrc = 0.
      CREATE OBJECT owmp 'WMPlayer.ocx.7'.
      IF sy-subrc <> 0.
        mensaje = 'No encontramos WMPlayer en el equipo'(nwm).
        RETURN.
      ENDIF.
    ENDIF.

    SET PROPERTY OF owmp 'Url' = fichero.
    GET PROPERTY OF owmp 'Controls' = ocnt.
    IF ocnt IS INITIAL.
      mensaje = 'Error al llamar al reproductor'(erp).
    ELSE.
      CALL METHOD OF ocnt 'Play'.
*    WAIT UP TO 1 SECONDS.  "This is neccesary, if not the program ends
    ENDIF.
  ENDMETHOD.
  METHOD reproducir_voz.
    DATA ovoice TYPE ole2_object.

    CHECK NOT palabras IS INITIAL.

    CREATE OBJECT ovoice 'SAPI.SpVoice'.
    IF sy-subrc = 0.
      CALL METHOD OF ovoice 'Speak'
        EXPORTING #1 = palabras.
    ELSE.
      mensaje = 'SAP Voice no instalado'(svn).
    ENDIF.
  ENDMETHOD.
  METHOD set_runtime_info.
    cl_salv_bs_runtime_info=>clear_all( ).

    cl_salv_bs_runtime_info=>set(
        display        = abap_false
        metadata       = abap_false
        data           = abap_true ).

*    cl_salv_bs_runtime_info=>clear_all( ).
*
*    cl_salv_bs_runtime_info=>set(
*        display        = abap_false
*        metadata       = abap_false
*        data           = abap_true ).
*
*    SUBMIT aqnwgref-facturazcom0001======
*      AND RETURN
*     WITH sp$00006 IN sp$00006
*     WITH sp$00007 IN sp$00007
*     WITH sp$00011 IN sp$00011
*     WITH sp$00010 IN sp$00010
*     WITH sp$00020 IN sp$00020
*     WITH sp$00008 IN sp$00008
*     WITH sp$00009 IN sp$00009
*     WITH sp$00016 IN sp$00016
*     WITH sp$00017 IN sp$00017
*     WITH sp$00015 IN sp$00015
*     WITH sp$00022 IN sp$00022
*     WITH sp$00021 IN sp$00021
*     WITH sp$00014 IN sp$00014
*     WITH sp$00012 IN sp$00012
*     WITH sp$00018 IN sp$00018
*     WITH sp$00019 IN sp$00019
*     WITH sp$00013 IN sp$00013
*     WITH sp$00023 IN sp$00023
*     WITH sp$00024 IN sp$00024
*     "WITH sp$00025 IN sp$00025 "F.Contable" LSP- 02.04.2019 - ID.13219
*     WITH %convdat =  %convdat
*     WITH %ratetyp =  %ratetyp
*     WITH %pcurr   =  %pcurr
*     WITH %alv     = 'X'
*     WITH %alvl    = ''.
*
*    SET PARAMETER ID 'ZNP' FIELD ''.
*
*    DATA: lt_data_ref   TYPE REF TO data.
*
*    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.
*
*    TRY.
*        cl_salv_bs_runtime_info=>get_data_ref(
*          IMPORTING
*            r_data            = lt_data_ref ).
*      CATCH cx_salv_bs_sc_runtime_info.
*        MESSAGE 'Error recuperando datos de MMBE' TYPE 'E'.
*    ENDTRY.
*
*    cl_salv_bs_runtime_info=>clear_all( ).
*    ASSIGN lt_data_ref->* TO  <fs_table>.
*    IF NOT <fs_table> IS ASSIGNED.
*      MESSAGE 'No se han seleccionado datos desde la query ZCOM0001' TYPE 'I'.
*      RETURN.
*    ELSE.
*      LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs>).
*        CLEAR %g00.
*        MOVE-CORRESPONDING <fs> TO l_query.
*        APPEND l_query TO %g00.
*      ENDLOOP.
*    ENDIF.

*report zemail_excel_dynamic.
*
*" by: John Eswin Nizar
*
*constants: c_tab      type c value cl_abap_char_utilities=>horizontal_tab,
*           c_cret     type c value cl_abap_char_utilities=>cr_lf,
*           c_mimetype type char64 value 'APPLICATION/MSEXCEL;charset=utf-16le'.
*data: v_xattach        type xstring, it_binary_attach type solix_tab.
**---------------------------------------------------------------------------------* Convert the internal data to XString*----------------------------------------------------------------------------------
*data: lc_descr_ref type ref to cl_abap_structdescr,
*      lv_value     type char128, lv_temp type string,
*      lv_mid       type string, lv_mid2 type string,
*      lv_tabix     type sy-tabix.
*field-symbols: <fs_intable> type any.
*field-symbols: <intable_wa> type abap_compdescr.
*
*tables: mara.
*
*data:
*  rspar         type table of rsparams,
*  wa_rspar      like line of rspar,
*  lt_data_ref   type ref to data,
*  go_salv_table type ref to cl_salv_table.
*
*field-symbols: <fs_table> type standard table.
*
*data : lv_pgmna type tstc-pgmna.
*
*selection-screen begin of block bl1 with frame title text-001.
*parameters : p_tcode(20) type c.
*parameters : p_var(10) type c obligatory.
*selection-screen end of block bl1.
*
*selection-screen begin of block bl2 with frame title text-002.
*parameter : p_pvw radiobutton group gr1 default 'X',
*p_eml radiobutton group gr1.
*selection-screen end of block bl2.
*
*select single pgmna into lv_pgmna from tstc where tcode eq p_tcode.
*
*cl_salv_bs_runtime_info=>set(
*    display        = abap_false
*    metadata       = abap_false
*    data           = abap_true ).
*
*submit (lv_pgmna) using selection-set p_var and return.
*
*try.
*    cl_salv_bs_runtime_info=>get_data_ref(
*      importing
*        r_data            = lt_data_ref ).
*  catch cx_salv_bs_sc_runtime_info.
*    message 'Data retrieval error' type 'E'.
*endtry.
*
*cl_salv_bs_runtime_info=>clear_all( ).
*assign lt_data_ref->* to <fs_table>.
*
*data : gr_table type ref to cl_salv_table,
*       lv_xml_type type salv_bs_constant,
*       lv_xml type xstring,
*       gt_outtab type standard table of sflight,
*       lv_smtpadr type ad_smtpadr.
*
*try.
*    cl_salv_table=>factory(
*      importing
*        r_salv_table = gr_table
*      changing
*        t_table  = <fs_table> ).
*  catch cx_salv_msg.
*endtry.
*
*data : lr_functions type ref to cl_salv_functions_list,
*       lr_layout type ref to cl_salv_layout,
*       ls_key type salv_s_layout_key.
*
*lr_functions = gr_table->get_functions( ).
**... #3.1 activate ALV generic Functions
*lr_functions->set_all( abap_true ).
*
*lr_layout = gr_table->get_layout( ).
*
**... $4.1 set the Layout Key
*ls_key-report = sy-repid.
*lr_layout->set_key( ls_key ).
*
**... #4.3 set Layout save restrictions
*lr_layout->set_save_restriction( if_salv_c_layout=>restrict_user_independant ).
*
**... Top of List, End of List
*data: lr_content type ref to cl_salv_form_element.
*
*if p_pvw eq 'X'.
**" Call to Convert ALV Output as internal XML Format
*  gr_table->display( ).
*else.
****--  Convert the output the internal XML format
*  lv_xml_type = if_salv_bs_xml=>c_type_mhtml.
*  lv_xml      = gr_table->to_xml( xml_type = lv_xml_type ).
*
**" Email Data Declaration
****-- BCS data
*  data : send_request   type ref to cl_bcs,
*         main_text      type bcsy_text,
*         document       type ref to cl_document_bcs,
*         recipient      type ref to if_recipient_bcs,
*         bcs_exception  type ref to cx_bcs,
*         sent_to_all    type os_boolean,
*         xls_content    type solix_tab,
*         lp_xls_size    type so_obj_len.
*
*
*  " Email - Content Conversion / Body / Attachment Creation
*  try.
**  " ------ Create persistent send request -----------
*      send_request = cl_bcs=>create_persistent( ).
*
** " ------------ add document --------------------
** get xls xstring and convert it to BCS format
*
*      lp_xls_size = xstrlen( lv_xml ).
*
*      xls_content = cl_document_bcs=>xstring_to_solix( ip_xstring = lv_xml ).
*
*      append 'TestAttachment' to main_text.
*      document = cl_document_bcs=>create_document(
*           i_type  = 'RAW'
*           i_text  = main_text
*           i_subject = 'Test ExcelAttachment' ).
*
*      document->add_attachment(
*              i_attachment_type  = 'xls'
*              i_attachment_subject = 'ExampleSpeadSheet'
*              i_attachment_size  = lp_xls_size
*              i_att_content_hex  = xls_content ).
*
*      " add document to send request
*      send_request->set_document( document ).
*
*      " Send E-Mail
*      data l_c_address type ad_smtpadr.
*      "  ----- add recipient (e-mail address) -------------
**   LOOP AT s_email.
*      l_c_address = 'apicazo@gmail.com'. " s_email-low.
*      recipient = cl_cam_address_bcs=>create_internet_address(
*            i_address_string = l_c_address ).
*
*      "  add recipient to send request
*      send_request->add_recipient( i_recipient = recipient ).
**   ENDLOOP.
*
*      "  ------------  send document ------------------
*      sent_to_all = send_request->send( i_with_error_screen = 'X' ).
*
*      if sent_to_all = 'X'.
*        message i022(so).
*      endif.
*
*      commit work.
*
*      " ---------------------------------------
*      " * exception handling
*    catch cx_bcs into bcs_exception.
*      write : text-001.
*      write : text-002, bcs_exception->error_type.
*      exit.
*
*  endtry.
*endif.
  ENDMETHOD.
  METHOD set_timer.
    DATA o_event TYPE REF TO lcl_event.

    CHECK sy-batch IS INITIAL.

    IF NOT o_timer IS INITIAL AND cancel_previo = 'X'.
      cancel_timer( ).
    ENDIF.

*https://blogs.sap.com/2017/04/11/refresh-the-display-using-class-cl_gui_timer/
    IF o_timer IS INITIAL.

*   create timer
      o_timer = NEW #( ).

**   event handler
      o_event = NEW #( ).
      o_event->padre = me.
      SET HANDLER o_event->m_timer_finished FOR o_timer.

*   set interval in seconds
      o_timer->interval = segundos.

    ENDIF.

    mensaje_timer = mensaje.
    me->msgty      = msgty.
    me->commit     = commit.
    me->reintentos = reintentos.

    o_timer->run( ).
  ENDMETHOD.
  METHOD usuario_bloqueador.
    DATA: l_garg   TYPE seqg3-garg,
          l_number TYPE i VALUE 0,
          i_seqg3  TYPE TABLE OF seqg3,
          l_SEQG3  TYPE seqg3.

    CLEAR usuario.

    CONCATENATE clave clave2 clave3 clave4 INTO l_garg.

    CALL FUNCTION 'ENQUEUE_READ'                              "#EC *
      EXPORTING
        gclient               = sy-mandt
        gname                 = tabla
        garg                  = l_garg
        guname                = ''
      IMPORTING
        number                = l_number
      TABLES
        enq                   = i_seqg3
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.
    IF l_number <> 0 OR sy-subrc <> 0.
      READ TABLE i_seqg3 INDEX 1 INTO l_SEQG3.
      IF sy-subrc = 0.
        usuario = l_seqg3-guname.
      ENDIF.
    ENDIF.
  ENDMETHOD.
