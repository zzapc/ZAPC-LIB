**********************************************************************
* APLICACION  : BC                                                    *
* TIPO        : Job
* TITULO      : Control de jobs
*                                                                      *
* DESCRIPCION :
*
* AUTOR: Andrés Picazo                          FECHA: 30/27/2007     *
************************************************************************
REPORT zctrljobs.


*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: zctrljobs_job_pl,
        zctrljobs_param,
        zctrljobs_doc,
        zctrljobs_resp,
        dd07t,
        snap,
        tadir,
        zestadisticas.

DATA: v_sel_fecha TYPE c LENGTH 1,
      l_mensaje   TYPE c LENGTH 80.

DATA o_prog TYPE REF TO zcl_ap_dev.

DEFINE fechas.
  IF     NOT (     p_fini IS INITIAL AND p_ffin IS INITIAL )
     AND NOT ( NOT p_hini IS INITIAL AND p_hfin IS INITIAL ).
    v_sel_fecha = 'X'.
    v_fini = p_fini.
    v_hini = p_hini.
    v_ffin = p_ffin.
    v_hfin = p_hfin.
  ELSE.
    CLEAR v_sel_fecha.
* Recuperamos la fecha y de hora de última ejecución
    SELECT SINGLE *
      FROM zctrljobs_job_pl
     WHERE proceso = &1.

    IF sy-subrc <> 0.
      CLEAR zctrljobs_job_pl.
      zctrljobs_job_pl-proceso = &1.
      zctrljobs_job_pl-fecha   = sy-datum.
      zctrljobs_job_pl-hora    = '000000'.
      INSERT zctrljobs_job_pl.
    ENDIF.

    v_fini = zctrljobs_job_pl-fecha.
    v_hini = zctrljobs_job_pl-hora.

  ENDIF.
END-OF-DEFINITION.

DEFINE graba_fechas.
  IF v_sel_fecha IS INITIAL.
    IF p_ejec = 'X'.
      zctrljobs_job_pl-proceso = &1.
      zctrljobs_job_pl-fecha   = v_ffin.
      zctrljobs_job_pl-hora    = v_hfin + 1.
      MODIFY zctrljobs_job_pl.
      COMMIT WORK.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.

CONSTANTS: c_report TYPE c LENGTH 20              VALUE 'ZCTRLJOBS',
           c_id     LIKE zctrljobs_job_pl-proceso VALUE 'CONTROLJOB'.


*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check.
  PUBLIC SECTION.
    METHODS handle_double_click REDEFINITION.
    METHODS handle_user_command REDEFINITION.
ENDCLASS.


*------TABLAS INTERNAS-------------------------------------------------*
DATA i_zctrljobs_param LIKE zctrljobs_param OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF i_listado OCCURS 100,
        check       TYPE c LENGTH 1,
        lights      TYPE c LENGTH 1,
        tipocontrol LIKE zctrljobs_param-tipocontrol,
        jobname     LIKE zctrljobs_param-jobname,
        status      TYPE c LENGTH 10,
        sin_regla   TYPE string,
        uname       LIKE snap-uname,
        progname    LIKE zctrljobs_param-report,
        cnam        LIKE trdir-cnam,
        unam        LIKE trdir-unam,
        status2     TYPE c LENGTH 16,
        strtdate    LIKE tbtcjob_bk-strtdate,
        strttime    LIKE tbtcjob_bk-strttime,
        regla       LIKE zctrljobs_param-jobname,
        report      LIKE zctrljobs_param-report,
        textobreve  LIKE zctrljobs_param-textobreve,
        casoaviso   LIKE zctrljobs_param-casoaviso,
        jobcount    LIKE tbtcjob_bk-jobcount,
        joblogid    LIKE tbtcjob_bk-joblog,
        tabla       TYPE tabname,
        typegroup   TYPE rsrd1-tyma_val,
        usuario     LIKE zctrljobs_resp-usuario,
        email       LIKE zctrljobs_resp-email,
        ejec_report LIKE zctrljobs_param-ejec_report,
        msgv1       LIKE zctrljobs_param-msgv1,
      END OF i_listado,
      l_listado LIKE i_listado.


*------VARIABLES-------------------------------------------------------*
RANGES: r_fecha FOR sy-datum,
        r_hora  FOR sy-uzeit.

DATA:: v_fini LIKE sy-datum,
       v_hini LIKE sy-uzeit,
       v_ffin LIKE sy-datum,
       v_hfin LIKE sy-uzeit.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK parma WITH FRAME.

PARAMETERS: p_fini LIKE sy-datum, " Fecha inicio
            p_hini LIKE sy-uzeit, " Hora inicio
            p_ffin LIKE sy-datum, " Fecha fin
            p_hfin LIKE sy-uzeit.

SELECTION-SCREEN SKIP.

PARAMETERS: p_ejec  AS CHECKBOX,
            p_dumps AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK parma.



*----------------------------------------------------------------------*
* CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.
  METHOD handle_double_click.
    READ TABLE i_listado INDEX row INTO l_listado.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE l_listado-tipocontrol.
      WHEN 'J'.
        CALL FUNCTION 'BP_JOBLOG_SHOW_SM37B'
          EXPORTING
            jobcount                  = l_listado-jobcount
            joblogid                  = l_listado-joblogid
            jobname                   = l_listado-jobname
          EXCEPTIONS
            error_reading_jobdata     = 1
            error_reading_joblog_data = 2
            jobcount_missing          = 3
            joblog_does_not_exist     = 4
            joblog_is_empty           = 5
            joblog_show_canceled      = 6
            jobname_missing           = 7
            job_does_not_exist        = 8
            no_joblog_there_yet       = 9
            no_show_privilege_given   = 10
            OTHERS                    = 11.
        IF sy-subrc <> 0.
          MESSAGE 'Error visualizando job' TYPE 'I'.
        ENDIF.
      WHEN 'D'.
        LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
          IF sy-tabix = row.
            <listado>-check = 'X'.
          ELSE.
            CLEAR <listado>-check.
          ENDIF.
        ENDLOOP.
        PERFORM enviar_mail USING 'X'.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_salv_function.
      WHEN 'F01'.
        get_seleccion( ).
        PERFORM enviar_mail USING ''.
        PERFORM ejecutar_report.

      WHEN 'F02'.
        get_seleccion( ).
        LOOP AT i_listado INTO l_listado WHERE     check       = 'X'
                                               AND tipocontrol = 'D'.
          DELETE FROM snap                           "#EC AOC_STD_TABLE
           WHERE datum = l_listado-strtdate
             AND uzeit = l_listado-strttime.
        ENDLOOP.
        MESSAGE 'Se han borrado los datos de Dumps' TYPE 'I'.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
************************************************************************
*
*                  LOGICA DEL PROGRAMA
*
************************************************************************

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  o_prog = NEW #( ).

  v_ffin = sy-datum.
  v_hfin = sy-uzeit.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
** Si el proceso arranca entre las 00:00:00 y las 05:00:00 no se arranca
*  IF sy-uzeit >= '000000' AND sy-uzeit <= '050000'.
*    MESSAGE s000(zs) WITH 'No se arranca por hora de ejecución'.
*    LEAVE PROGRAM.
*  ENDIF.

* Marcamos programa para evitar ejecuciones simultáneas
  PERFORM bloquear.
* Determina las fecha a utilizar
  fechas c_id.

  SELECT * FROM zctrljobs_param                         "#EC CI_NOWHERE
    INTO TABLE i_zctrljobs_param.
  SORT i_zctrljobs_param.

  IF p_dumps IS INITIAL.
    PERFORM seleccionar_jobs.

    PERFORM seleccionar_batchinputs.
  ENDIF.

  PERFORM seleccionar_dumps.

  PERFORM agrupar_listado.

* Graba última fecha/hora ejecución en tabla ZCTRLJOBS_JOB_PL
  graba_fechas c_id.

  IF p_ejec = 'X' AND p_dumps IS INITIAL.
    PERFORM enviar_mail USING ''.
    PERFORM ejecutar_report.

    SELECT SINGLE * FROM tadir
     WHERE pgmid    = 'R3TR'
       AND object   = 'PROG'
       AND obj_name = 'ZARCHIVADO_TABLAS_TEMP'.
    IF sy-subrc = 0.
      SUBMIT (tadir-obj_name)
        AND RETURN.
    ENDIF.

    IF sy-batch = 'X'.
      DATA l_jobname TYPE tbtcm-jobname.
      CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
        IMPORTING
          jobname = l_jobname.
      IF NOT l_jobname IS INITIAL.
        zcl_ap_jobs=>borrar_jobs( fecha = sy-datum jobname = l_jobname solo_finalizados = 'X' solo_sin_spool = 'X' ).
      ENDIF.
    ENDIF.
  ENDIF.

  PERFORM listado.

*  zcl_ap_jobs=>borrar_jobs( fecha = sy-datum jobname = 'ZHRR016_AGENDA' solo_finalizados = 'X' solo_sin_spool = 'X' ).

*&---------------------------------------------------------------------*
*&      Form  BLOQUEAR
*&---------------------------------------------------------------------*
*       Comprueba si tiene que bloquear la ejecución del programa
*----------------------------------------------------------------------*
FORM bloquear.
  o_prog->sgpi_texto( 'Verificando que no haya otra ejecución en curso' ).
  SELECT COUNT( * ) FROM tbtco
    INTO sy-tfill
   WHERE     jobname  = 'CONTROL_JOBS'
     AND NOT status  IN ( 'F', 'A', 'S' ).
  IF sy-tfill > 1.
    MESSAGE 'Parado por existir otro job en ejecución' TYPE 'I'.
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  seleccionar_jobs
*&---------------------------------------------------------------------*
FORM seleccionar_jobs.
  DATA: l_btcselect LIKE btcselect,
        i_joblist   LIKE tbtcjob_bk OCCURS 0 WITH HEADER LINE,
        l_status    TYPE c LENGTH 1,
        l_msgv1     LIKE zctrljobs_param-msgv1.

  o_prog->sgpi_texto( 'Seleccionando jobs' ).

* Seleccionar Jobs
  CLEAR l_btcselect.
  l_btcselect-jobname   = '*'.
  l_btcselect-username  = '*'.
  l_btcselect-from_date = v_fini.
  l_btcselect-from_time = v_hini.
  l_btcselect-to_date   = v_ffin.
  l_btcselect-to_time   = v_hfin.
  l_btcselect-schedul   = ''.
  l_btcselect-ready     = ''.
  l_btcselect-running   = ''.
  l_btcselect-finished  = 'X'.
  l_btcselect-aborted   = 'X'.

  CALL FUNCTION 'BP_JOB_SELECT_SM37B'
    EXPORTING
      jobselect_dialog    = 'N'
      jobsel_param_in     = l_btcselect
*     ENDDATE             = '        '
*     ENDTIME             = '      '
* IMPORTING
*     JOBSEL_PARAM_OUT    =
    TABLES
      jobselect_joblist_b = i_joblist
    EXCEPTIONS
      invalid_dialog_type = 1
      jobname_missing     = 2
      no_jobs_found       = 3
      selection_canceled  = 4
      username_missing    = 5
      OTHERS              = 6.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Comprobamos primeros los jobs con error
  LOOP AT i_joblist WHERE status = 'A'.
    LOOP AT i_zctrljobs_param WHERE     tipocontrol = 'J'
                                    AND (    casoaviso = 'E'
                                          OR casoaviso = 'T'
                                          OR casoaviso = 'D' ).
      IF i_joblist-jobname NP i_zctrljobs_param-jobname.
        CONTINUE.
      ENDIF.

* Si habían informado el report,
      IF NOT i_zctrljobs_param-report IS INITIAL.
        IF i_joblist-progname CP i_zctrljobs_param-report.
* Intentamos averiguar en qué punto del job falló
          PERFORM control_paso USING i_joblist-jobname
                                     i_joblist-jobcount
                                     i_joblist-joblog
                                     i_joblist-progname
                  CHANGING l_status.
          IF l_status <> 'S'.
            CLEAR i_listado.
            MOVE-CORRESPONDING i_zctrljobs_param TO i_listado.
            i_listado-regla = i_zctrljobs_param-jobname.
            MOVE-CORRESPONDING i_joblist TO i_listado.
            i_listado-status2 = l_status.
            APPEND i_listado.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR i_listado.
        MOVE-CORRESPONDING i_zctrljobs_param TO i_listado.
        i_listado-regla = i_zctrljobs_param-jobname.
        MOVE-CORRESPONDING i_joblist TO i_listado.
        i_listado-progname = '*'.
        APPEND i_listado.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  LOOP AT i_joblist WHERE status <> 'A'.
    LOOP AT i_zctrljobs_param WHERE     tipocontrol = 'J'
                                    AND (    casoaviso = 'T'
                                          OR casoaviso = 'D' ).
      IF i_joblist-jobname NP i_zctrljobs_param-jobname.
        CONTINUE.
      ENDIF.

      IF i_zctrljobs_param-casoaviso = 'D'.
        PERFORM comprobar_variables USING i_joblist-jobname
                                          i_joblist-jobcount
                                          i_joblist-joblog
                                          i_zctrljobs_param-msgid
                                          i_zctrljobs_param-msgno
                                          i_zctrljobs_param-msgv1
                                          i_zctrljobs_param-distinto
                CHANGING l_status l_msgv1.
        IF l_status IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF NOT i_zctrljobs_param-report IS INITIAL.
        IF i_joblist-progname CP i_zctrljobs_param-report.
          CLEAR i_listado.
          MOVE-CORRESPONDING i_zctrljobs_param TO i_listado.
          i_listado-regla = i_zctrljobs_param-jobname.
          MOVE-CORRESPONDING i_joblist TO i_listado.
          i_listado-status2 = l_status.
          i_listado-msgv1   = l_msgv1.
          APPEND i_listado.
        ENDIF.
      ELSE.
        CLEAR i_listado.
        MOVE-CORRESPONDING i_zctrljobs_param TO i_listado.
        i_listado-regla = i_zctrljobs_param-jobname.
        MOVE-CORRESPONDING i_joblist TO i_listado.
        i_listado-progname = '*'.
        i_listado-status2  = l_status.
        i_listado-msgv1    = l_msgv1.
        APPEND i_listado.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  listado
*&---------------------------------------------------------------------*
FORM listado.
  DATA o_alv TYPE REF TO lcl_alv.

  IF sy-batch = 'X'.
    RETURN.
  ENDIF.

  o_alv = NEW #( status           = 'STANDARD_ALV_DYN'
                         status_prog        = 'ZAP_STATUS' ).
*    EXPORTING
*      status = 'STANDARD'.

  o_alv->add_button( button = 'F01' text = 'Ejecutar'(eje)  icon = icon_execute_object   ).
  o_alv->add_button( button = 'F02' qinfo = 'Borra dump'  icon = icon_delete  ).

  o_alv->set_field_noout( 'JOBCOUNT,JOBLOGID' ).
  o_alv->set_field_text( campo = 'STATUS' valor = 'Estatus JOB' ).

  o_alv->set_field_text( campo = 'STATUS2' valor = 'Estatus paso' ).
  o_alv->show( ).
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  control_paso
*&---------------------------------------------------------------------*
FORM control_paso
  USING    pe_jobname  TYPE any
           pe_jobcount TYPE any
           pe_joblog   TYPE any
           pe_report   TYPE any
  CHANGING ps_status   TYPE any.

  DATA: i_joblog        LIKE tbtc5 OCCURS 0 WITH HEADER LINE,
        i_joblog2       LIKE tbtc5 OCCURS 0 WITH HEADER LINE,
        i_tbtcp         LIKE tbtcp OCCURS 0 WITH HEADER LINE,
        l_tabix         LIKE sy-tabix,
        " TODO: variable is assigned but never used (ABAP cleaner)
        l_no            TYPE c LENGTH 1,
        l_string        TYPE string,
        i_no_enviar(40) OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'BP_JOBLOG_READ'
    EXPORTING
      jobcount              = pe_jobcount
      joblog                = pe_joblog
      jobname               = pe_jobname
    TABLES
      joblogtbl             = i_joblog
    EXCEPTIONS
      joblog_does_not_exist = 1
      joblog_is_empty       = 2
      job_does_not_exist    = 3
      OTHERS                = 99.
  IF sy-subrc <> 0.
    ps_status = 'E'.
  ELSE.
    i_joblog2[] = i_joblog[].

    SELECT stepcount FROM tbtcp
      INTO CORRESPONDING FIELDS OF TABLE i_tbtcp
     WHERE jobname  = pe_jobname
       AND jobcount = pe_jobcount.

    ps_status = 'S'.
    LOOP AT i_tbtcp WHERE progname = pe_report.
      CLEAR ps_status.

      IF ps_status IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      LOOP AT i_joblog WHERE     msgid = '00'
                             AND msgno = '550'
                             AND msgv1 = i_tbtcp-stepcount.
        l_tabix = sy-tabix + 1.
        IF ps_status IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        LOOP AT i_joblog2 FROM l_tabix.
          IF ps_status IS NOT INITIAL.
            CONTINUE.
          ENDIF.

* Si encuentro el siguiente paso, es que ha acabado bien.
          IF i_joblog2-msgid = '00' AND i_joblog2-msgno = '550'.
            ps_status = 'S'.
* Si encuentro el mensaje de job cancelado, es que ha sido el
* causante del error.
          ELSEIF i_joblog2-msgid = '00' AND i_joblog2-msgno = '518'.
            ps_status = 'E'.
            EXIT.
          ELSE.
            CLEAR l_no.
            SELECT SINGLE string FROM zctrljobs_paramt
              INTO @DATA(no_enviar)
             WHERE tipocontrol = @i_zctrljobs_param-tipocontrol
               AND jobname = @i_zctrljobs_param-jobname
               AND report = @i_zctrljobs_param-report
               AND tipo = 'NO_ENVIAR'.

            IF no_enviar <> ''.
              SPLIT no_enviar AT ',' INTO TABLE i_no_enviar.
              LOOP AT i_no_enviar.
                IF i_joblog2-text CS i_no_enviar.
                  ps_status = 'S'.
                  EXIT.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      IF sy-subrc <> 0.
        ps_status = 'N'.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  coMPROBAR_VARIABLES
*&---------------------------------------------------------------------*

FORM comprobar_variables
  USING    pe_jobname  TYPE any
           pe_jobcount TYPE any
           pe_joblog   TYPE any
           pe_msgid    TYPE any
           pe_msgno    TYPE any
           pe_msgv1    TYPE any
           pe_distinto TYPE any
  CHANGING ps_status   TYPE any
           ps_msgv1    TYPE any.

  DATA i_joblog LIKE tbtc5 OCCURS 0 WITH HEADER LINE.

  CLEAR ps_status.
  CALL FUNCTION 'BP_JOBLOG_READ'
    EXPORTING
      jobcount              = pe_jobcount
      joblog                = pe_joblog
      jobname               = pe_jobname
    TABLES
      joblogtbl             = i_joblog
    EXCEPTIONS
      joblog_does_not_exist = 1
      joblog_is_empty       = 2
      job_does_not_exist    = 3
      OTHERS                = 99.
  IF sy-subrc <> 0.
    ps_status = 'E'.
  ELSE.
    READ TABLE i_joblog WITH KEY msgid = pe_msgid
                                 msgno = pe_msgno.
    IF sy-subrc = 0.
      IF    ( i_joblog-msgv1  = pe_msgv1 AND pe_distinto IS INITIAL )
         OR ( i_joblog-msgv1 <> pe_msgv1 AND pe_distinto  = 'X' ).
        ps_status = 'X'.
        ps_msgv1 = i_joblog-msgv1.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " control_paso

*&---------------------------------------------------------------------*
*&      Form  enviar_mail
*&---------------------------------------------------------------------*
FORM enviar_mail USING pe_popup.
  DATA: i_list      LIKE i_listado OCCURS 0 WITH HEADER LINE,
        o_mail      TYPE REF TO zcl_ap_envio_mail,
        l_titulo    TYPE c LENGTH 80,
        l_direccion TYPE c LENGTH 80,
        i_texto     LIKE solisti1 OCCURS 0 WITH HEADER LINE.
  DATA i_joblog LIKE tbtc5 OCCURS 0 WITH HEADER LINE.
  DATA: i_infotab  TYPE rsdumptab,
        l_dumpinfo LIKE rsdumpinfo.
  " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
  DATA i_ft TYPE rsdump_ft_it.
*        l_ft LIKE rsdump_ft.
  DATA: l_shorttext       TYPE string,
        l_explanation     TYPE string,
        l_userhints       TYPE string,
        l_description     TYPE string,
        l_correctionhints TYPE string,
        l_internalhints   TYPE string.

  FIELD-SYMBOLS <texto> TYPE solisti1.

  o_prog->sgpi_texto( 'Enviando mails' ).

  LOOP AT i_listado WHERE     check = 'X'
                          AND ( usuario <> '' OR email <> '' ).
    CLEAR: i_listado-strtdate, i_listado-strttime.
    COLLECT i_listado INTO i_list.
  ENDLOOP.
  LOOP AT i_list.
    READ TABLE i_listado WITH KEY tipocontrol  = i_list-tipocontrol
                                  jobname      = i_list-jobname
                                  progname     = i_list-progname
                                  tabla        = i_list-tabla.
    IF sy-subrc = 0.
      i_list-strtdate = i_listado-strtdate.
      i_list-strttime = i_listado-strttime.
      MODIFY i_list.
    ENDIF.
  ENDLOOP.

  LOOP AT i_list INTO i_listado WHERE     check = 'X'
                                      AND ( usuario <> '' OR email <> '' ).

    o_mail = NEW #( ).

    CASE i_listado-tipocontrol.
      WHEN 'J'.
        CONCATENATE '!Comprobacion Job' i_listado-jobname INTO l_titulo
                    SEPARATED BY space.
        IF i_listado-status = 'Cancelado'.
          CONCATENATE l_titulo 'cancelado.' INTO l_titulo
                      SEPARATED BY space.
        ELSE.
          CONCATENATE l_titulo 'Finalizado' INTO l_titulo
                      SEPARATED BY space.
          IF i_listado-casoaviso = 'D'.
            CONCATENATE l_titulo 'con status erróneo.' INTO l_titulo
                        SEPARATED BY space.
          ELSE.
            CONCATENATE l_titulo 'correctamente.' INTO l_titulo
                        SEPARATED BY space.
          ENDIF.
        ENDIF.
      WHEN 'B'.
        CONCATENATE 'Batch Input' i_listado-jobname INTO l_titulo
                    SEPARATED BY space.
        IF i_listado-status = 'Errores'.
          CONCATENATE l_titulo 'acabado con errores.' INTO l_titulo
                      SEPARATED BY space.
        ELSE.
          CONCATENATE l_titulo 'finalizado con éxito' INTO l_titulo
                      SEPARATED BY space.
        ENDIF.
      WHEN 'D'.
        CONCATENATE 'Dump' i_listado-jobname
                    'en report' i_listado-progname
                    INTO l_titulo SEPARATED BY space.
    ENDCASE.

    IF i_listado-email IS INITIAL.
      l_direccion = i_listado-usuario.
    ELSE.
      l_direccion = i_listado-email.
    ENDIF.

    REFRESH i_texto.
    i_texto = i_listado-textobreve.
    o_mail->add_parrafo_html( i_texto ).
*    APPEND i_texto.
*    CLEAR i_texto. APPEND i_texto.
*    i_texto =
*    'Se le envía este correo porque está dado de alta en tabla avisos'.
*    APPEND i_texto.
*    i_texto = 'con los siguientes parámetros'.
*    APPEND i_texto.
    o_mail->add_parrafo_html(
     '<P>Se le envía este correo porque está dado de alta en tabla' ).
    o_mail->add_parrafo_html(
     'avisos con los siguientes parámetros:</P>' ).

    CASE i_listado-tipocontrol.
      WHEN 'J'.
        CONCATENATE 'Job:' i_listado-regla INTO i_texto
                    SEPARATED BY space.
*        APPEND i_texto.
        o_mail->add_parrafo_html( i_texto ).
      WHEN 'B'.
        CONCATENATE 'Batch Input:' i_listado-regla INTO i_texto
                    SEPARATED BY space.
*        APPEND i_texto.
        o_mail->add_parrafo_html( i_texto ).
      WHEN 'D'.
        CONCATENATE 'Tipo Dump:' i_listado-regla INTO i_texto
                    SEPARATED BY space.
*        APPEND i_texto.
        o_mail->add_parrafo_html( i_texto ).
    ENDCASE.

    CONCATENATE 'Report:' i_listado-report INTO i_texto
                SEPARATED BY space.
*    APPEND i_texto.
    o_mail->add_parrafo_html( i_texto ).
    SELECT SINGLE * FROM dd07t
     WHERE domname    = 'ZCASOAVISO'
       AND ddlanguage = sy-langu
       AND domvalue_l = i_listado-casoaviso.

    CONCATENATE 'Caso:' dd07t-ddtext INTO i_texto
                SEPARATED BY space.
*    APPEND i_texto.
    o_mail->add_parrafo_html( i_texto ).
*    CLEAR i_texto. APPEND i_texto.
    o_mail->set_text( '</br>' ).
    i_texto = 'Documentación:'.
*    APPEND i_texto.
    o_mail->add_parrafo_html( i_texto ).

    SELECT comentarios FROM zctrljobs_doc
      INTO zctrljobs_doc-comentarios
     WHERE tipocontrol = i_listado-tipocontrol
       AND jobname     = i_listado-regla
       AND report      = i_listado-report.
      i_texto = zctrljobs_doc-comentarios.
*      APPEND i_texto.
      o_mail->add_parrafo_html( i_texto ).
    ENDSELECT.

    IF i_listado-tipocontrol = 'J'.
*      CLEAR i_texto. APPEND i_texto.
*      i_texto = 'Log del job:'.
*      APPEND i_texto.
      o_mail->set_text( '<h1>Log del job</h1>' ).

      CALL FUNCTION 'BP_JOBLOG_READ'
        EXPORTING
          jobcount              = i_listado-jobcount
          joblog                = i_listado-joblogid
          jobname               = i_listado-jobname
        TABLES
          joblogtbl             = i_joblog
        EXCEPTIONS
          joblog_does_not_exist = 1
          joblog_is_empty       = 2
          job_does_not_exist    = 3
          OTHERS                = 99.
      IF sy-subrc <> 0.
        MESSAGE 'Error leyendo job' TYPE 'I'.
      ENDIF.

      LOOP AT i_joblog.
        i_texto = i_joblog-text.
*        APPEND i_texto.
        o_mail->add_parrafo_html( i_texto ).
      ENDLOOP.
    ENDIF.

    IF i_listado-tipocontrol = 'D'.
      CALL FUNCTION 'RS_ST22_GET_DUMPS'
        EXPORTING
          p_day     = i_listado-strtdate
        IMPORTING
          p_infotab = i_infotab.
      READ TABLE i_infotab INTO l_dumpinfo
           WITH KEY sytime = i_listado-strttime.

*      CLEAR i_texto. APPEND i_texto.
*      i_texto = 'Datos del dump:'.
*      APPEND i_texto.
      o_mail->set_text( '<h1>Datos del dump</h1>' ).
      CONCATENATE 'Usuario:' l_dumpinfo-syuser INTO i_texto.
*       APPEND i_texto.
      o_mail->add_parrafo_html( i_texto ).

      IF NOT i_listado-unam IS INITIAL.
        SELECT SINGLE udat FROM trdir
          INTO @DATA(l_udat)
         WHERE name = @i_listado-progname.
        IF sy-subrc = 0.
          i_texto-line = |Última modificación: { l_udat DATE = USER } por { i_listado-unam }|.
          o_mail->add_parrafo_html( i_texto ).
        ENDIF.
      ENDIF.


      CONCATENATE 'Programa:' l_dumpinfo-programname INTO i_texto.
*      APPEND i_texto.
      o_mail->add_parrafo_html( i_texto ).
      CONCATENATE 'Include:' l_dumpinfo-includename INTO i_texto.
*      APPEND i_texto.
      o_mail->add_parrafo_html( i_texto ).

      SELECT SINGLE * FROM snap
       WHERE datum = i_listado-strtdate
         AND uzeit = i_listado-strttime.
      IF sy-subrc = 0.
        CALL FUNCTION 'RS_ST22_GET_FT'
          EXPORTING
            datum = snap-datum
            uzeit = snap-uzeit
            uname = snap-uname
            ahost = snap-ahost
            modno = snap-modno
            mandt = snap-mandt
          IMPORTING
            ft    = i_ft.

        CALL FUNCTION 'RS_ST22_READ_SNAPT'
          IMPORTING
            shorttext       = l_shorttext
            explanation     = l_explanation
            userhints       = l_userhints
            description     = l_description
            correctionhints = l_correctionhints
            internalhints   = l_internalhints.

        o_mail->set_text( '<h1>Descripción breve</h1>' ).
        o_mail->add_parrafo_html( l_shorttext ).
        o_mail->set_text( '<h1>Explicación</h1>' ).
        o_mail->add_parrafo_html( l_explanation ).
        o_mail->set_text( '<h1>Consejos</h1>' ).
        o_mail->add_parrafo_html( l_userhints ).
        o_mail->set_text( '<h1>Descripción</h1>' ).
        o_mail->add_parrafo_html( l_description ).
        o_mail->set_text( '<h1>Consejos para corrección</h1>' ).
        o_mail->add_parrafo_html( l_correctionhints ).
        o_mail->set_text( '<h1>Consejos internos</h1>' ).
        o_mail->add_parrafo_html( l_internalhints ).

*        CLEAR i_texto. APPEND i_texto.
*        i_texto = 'Detalles:'.
*        APPEND i_texto.
*
*        LOOP AT i_ft INTO l_ft WHERE id NE 'ZS'
*                       AND id NE 'TH'
*                       AND id NE 'CO'
*                       AND id NE 'PG'.
*          i_texto = l_ft-value.
*          APPEND i_texto.
*        ENDLOOP.

      ENDIF.
    ENDIF.

*    CALL FUNCTION 'Z_ENVIO_MAIL'
*      EXPORTING
*        subject   = l_titulo
*        direccion = l_direccion
*        urgente   = 'X'
**       commit    = 'X'
*      TABLES
*        texto     = i_texto.

    IF NOT i_texto[] IS INITIAL.
      o_mail->set_text( tabla = i_texto[] ).
    ENDIF.

    WRITE: / 'Mail:', l_titulo, 'a', l_direccion.
    IF zcl_c=>cliente_tasks = 'GRE' OR pe_popup = 'X'.
      LOOP AT o_mail->i_texto_mail ASSIGNING <texto>.
        REPLACE ALL OCCURRENCES OF '<P>' IN <texto> WITH ''.
        REPLACE ALL OCCURRENCES OF '</P>' IN <texto> WITH ''.
        REPLACE ALL OCCURRENCES OF '<h1>' IN <texto> WITH ''.
        REPLACE ALL OCCURRENCES OF '</h1>' IN <texto> WITH ''.
        REPLACE ALL OCCURRENCES OF '</br>' IN <texto> WITH ''.
      ENDLOOP.

      o_mail->envio_mail( subject = l_titulo
                          direccion = l_direccion
                          html    = ''
                          popup = pe_popup ).
    ELSE.
      o_mail->envio_mail( subject = l_titulo
                          direccion = l_direccion
                          html    = 'X'
                          popup = pe_popup ).
    ENDIF.
    o_mail->free( ).
    CLEAR o_mail.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ejecutar_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ejecutar_report.
  DATA: i_list     LIKE i_listado OCCURS 0 WITH HEADER LINE,
        o_bi       TYPE REF TO zcl_ap_batch_input,
        l_hini     TYPE sy-uzeit,
        l_hfin     TYPE sy-uzeit,
        l_sec      TYPE i,
        l_typename LIKE ddtypes-typename,
        " TODO: variable is assigned but never used (ABAP cleaner)
        l_typekind LIKE ddtypes-typekind,
        l_eutype   TYPE c LENGTH 1,
        " TODO: variable is assigned but never used (ABAP cleaner)
        l_ok       TYPE c LENGTH 1.

  LOOP AT i_listado WHERE     check        = 'X'
                          AND ejec_report <> ''
                          AND ( tabla <> '' OR typegroup <> '' ).
    CLEAR: i_listado-strtdate, i_listado-strttime, l_listado-usuario, l_listado-email.
    IF i_listado-ejec_report = 'ACTIVAR_DICCIONAR'.
      CLEAR i_listado-report.
    ENDIF.
    COLLECT i_listado INTO i_list.
  ENDLOOP.
  o_prog->sgpi_texto( 'Ejecutando reports' ).

  o_bi = NEW #( ).

  LOOP AT i_list.
    MESSAGE |Ejecutando report { i_list-ejec_report }| TYPE 'S'.
    CASE i_list-ejec_report.
      WHEN 'ACTIVAR_DICCIONARIO'.

        GET TIME.
        l_hini = sy-uzeit.
        o_bi->inicio( ).

        o_bi->dynpro( program = 'SAPLSD_ENTRY' dynpro = '1000' ).
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=WB_ACTIVATE' ).
        IF NOT i_list-tabla IS INITIAL.
          MESSAGE s398(00) WITH 'Estructura' i_list-tabla.
          o_bi->campos( campo = 'RSRD1-TBMA' valor = 'X' ). " Clase de objetos tablas
          o_bi->campos( campo = 'RSRD1-TBMA_VAL' valor = i_list-tabla ). " Nombre de tabla de 16 posiciones
        ELSE.
          MESSAGE s398(00) WITH 'Type-group' i_list-typegroup.
          o_bi->campos( campo = 'RSRD1-TBMA' valor = '' ). " Clase de objetos tablas
          o_bi->campos( campo = 'RSRD1-TYMA' valor = 'X' ). " Clase de objetos tipo
          o_bi->campos( campo = 'RSRD1-TYMA_VAL' valor = i_list-typegroup ). " Nombre de tabla de 16 posiciones
        ENDIF.

        l_mensaje = o_bi->llamar_transaccion( tcode = 'SE11' modo = 'N' ).
        MESSAGE s398(00) WITH l_mensaje(40) l_mensaje+40(40).

        GET TIME.
        l_hfin = sy-uzeit.

        l_sec = l_hfin - l_hini.
        IF l_sec < 5.
          l_typename = i_list-tabla.
          CALL FUNCTION 'INTERN_TYPE_KIND'
            EXPORTING
              typename = l_typename
*             STATUS   = 'A'
            IMPORTING
              typekind = l_typekind
              eutype   = l_eutype.

          CALL FUNCTION 'RS_DD_ACTIVATE'
            EXPORTING
              objname = l_typename
              objtype = l_eutype
*             P_WB_MANAGER       =
            IMPORTING
              ok      = l_ok.
        ENDIF.

    ENDCASE.
  ENDLOOP.
ENDFORM.                    " enviar_mail

*&---------------------------------------------------------------------*
*&      Form  seleccionar_batchinputs
*&---------------------------------------------------------------------*
FORM seleccionar_batchinputs.
  DATA i_apqi LIKE apqi OCCURS 0 WITH HEADER LINE.

  o_prog->sgpi_texto( 'Seleccionando batch inputs' ).

  SELECT  getdate gettime groupid progid FROM apqi      "#EC CI_NOFIELD
    INTO CORRESPONDING FIELDS OF TABLE i_apqi
   WHERE getdate >= v_fini
     AND getdate <= v_ffin.

  IF NOT v_hini IS INITIAL OR NOT v_hfin IS INITIAL.
    DELETE i_apqi WHERE     getdate = v_fini
                        AND gettime < v_hini.
    DELETE i_apqi WHERE     getdate = v_ffin
                        AND gettime > v_hfin.
  ENDIF.

* Comprobamos primeros los batchinputs con error
  LOOP AT i_apqi.
    LOOP AT i_zctrljobs_param WHERE tipocontrol = 'B'.
      IF i_apqi-groupid NP i_zctrljobs_param-jobname.
        CONTINUE.
      ENDIF.

      IF NOT (    i_zctrljobs_param-casoaviso = 'T'
               OR ( (    i_zctrljobs_param-casoaviso = 'E'
                      OR i_zctrljobs_param-casoaviso = 'D' ) AND i_apqi-qstate =
                         'E' ) ).
        CONTINUE.
      ENDIF.

* Si habían informado el report,
      IF NOT i_zctrljobs_param-report IS INITIAL.
        IF i_zctrljobs_param-report = i_apqi-progid.
          CLEAR i_listado.
          MOVE-CORRESPONDING i_zctrljobs_param TO i_listado.
          i_listado-regla    = i_zctrljobs_param-jobname.
          i_listado-jobname  = i_apqi-groupid.
          i_listado-status   = i_apqi-qstate.
          i_listado-strtdate = i_apqi-getdate.
          i_listado-strttime = i_apqi-gettime.
          i_listado-progname = i_apqi-progid.
          APPEND i_listado.
        ENDIF.
      ELSE.
        CLEAR i_listado.
        MOVE-CORRESPONDING i_zctrljobs_param TO i_listado.
        i_listado-regla    = i_zctrljobs_param-jobname.
        i_listado-jobname  = i_apqi-groupid.
        i_listado-strtdate = i_apqi-getdate.
        i_listado-strttime = i_apqi-gettime.
        i_listado-status   = i_apqi-qstate.
        i_listado-progname = i_apqi-progid.
        APPEND i_listado.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  seleccionar_dumps
*&---------------------------------------------------------------------*
FORM seleccionar_dumps.
  DATA: i_snap          LIKE snap OCCURS 0 WITH HEADER LINE,
        l_report        TYPE c LENGTH 40,
        l_dump          TYPE c LENGTH 40,
        l_tabla         TYPE c LENGTH 40,
        l_typegroup     TYPE c LENGTH 40,
        r_uname         TYPE RANGE OF string,
        r_uname_no      TYPE RANGE OF string,
        r_uname_cr      TYPE RANGE OF string,
        l_string        TYPE string,
        l_no            TYPE c LENGTH 1,
        i_no_enviar(40) OCCURS 0 WITH HEADER LINE,
        l_cnam          TYPE trdir-cnam,
        l_unam          TYPE trdir-unam,
        l_sin_regla     TYPE string.

  o_prog->sgpi_texto( 'Seleccionando dumps' ).

  SELECT * FROM snap
    INTO TABLE i_snap
   WHERE datum >= v_fini
     AND datum <= v_ffin
     AND seqno  = '000'.

  IF NOT v_hini IS INITIAL OR NOT v_hfin IS INITIAL.
    DELETE i_snap WHERE     datum = v_fini
                        AND uzeit < v_hini.
    DELETE i_snap WHERE     datum = v_ffin
                        AND uzeit > v_hfin.
  ENDIF.

* Comprobamos primeros los batchinputs con error
  LOOP AT i_snap.
    CLEAR l_report.
    PERFORM buscar_report_dump USING i_snap
            CHANGING l_report
                     l_dump
                     l_tabla
                     l_typegroup
                     l_cnam
                     l_unam.


    LOOP AT i_zctrljobs_param WHERE tipocontrol = 'D'.
      REFRESH: r_uname, r_uname_no.

      IF NOT (    i_zctrljobs_param-casoaviso = 'T'
               OR i_zctrljobs_param-casoaviso = 'E'
               OR i_zctrljobs_param-casoaviso = 'D' ).
        IF p_dumps IS INITIAL.
          CONTINUE.
        ELSE.
          l_sin_regla = 'CASOAVISO'.
        ENDIF.
      ENDIF.

      IF NOT (     l_dump   CP i_zctrljobs_param-jobname
               AND l_report CP i_zctrljobs_param-report ).
        IF p_dumps IS INITIAL.
          CONTINUE.
        ELSE.
          l_sin_regla = 'REPORT'.
        ENDIF.
      ENDIF.

      SELECT SINGLE string FROM zctrljobs_paramt
        INTO @DATA(solo_usuarios)
       WHERE tipocontrol = @i_zctrljobs_param-tipocontrol
         AND jobname = @i_zctrljobs_param-jobname
         AND report = @i_zctrljobs_param-report
         AND tipo = 'SOLO_USUARIOS'.

      IF NOT solo_usuarios IS INITIAL.
        r_uname[] = zcl_ap_lista=>lista2rango( solo_usuarios ).
        IF NOT i_snap-uname IN r_uname.
          IF p_dumps IS INITIAL.
            CONTINUE.
          ELSE.
            l_sin_regla = 'SOLO_USUARIOS'.
          ENDIF.
        ENDIF.
      ENDIF.

      SELECT SINGLE string FROM zctrljobs_paramt
        INTO @DATA(no_usuarios)
       WHERE tipocontrol = @i_zctrljobs_param-tipocontrol
         AND jobname = @i_zctrljobs_param-jobname
         AND report = @i_zctrljobs_param-report
         AND tipo = 'NO_USUARIOS'.
      IF NOT no_usuarios IS INITIAL.
        r_uname_no[] = zcl_ap_lista=>lista2rango( lista = no_usuarios ).
        IF i_snap-uname IN r_uname_no.
          IF p_dumps IS INITIAL.
            CONTINUE.
          ELSE.
            l_sin_regla = 'NO_USUARIOS'.
          ENDIF.
        ENDIF.
      ENDIF.

      SELECT SINGLE string FROM zctrljobs_paramt
        INTO @DATA(creado_por)
       WHERE tipocontrol = @i_zctrljobs_param-tipocontrol
         AND jobname = @i_zctrljobs_param-jobname
         AND report = @i_zctrljobs_param-report
         AND tipo = 'CREADO_POR'.
      IF NOT creado_por IS INITIAL.
        r_uname_cr[] = zcl_ap_lista=>lista2rango( lista = creado_por ).
        IF NOT ( l_cnam IN r_uname_cr OR l_unam IN r_uname_cr ).
          IF p_dumps IS INITIAL.
            CONTINUE.
          ELSE.
            l_sin_regla = 'CREADO_POR'.
          ENDIF.
        ENDIF.
      ENDIF.



* Si habían informado el report,
      IF NOT i_zctrljobs_param-report IS INITIAL.
        IF l_report CP i_zctrljobs_param-report.
          CLEAR i_listado.
          MOVE-CORRESPONDING i_zctrljobs_param TO i_listado.
          i_listado-regla     = i_zctrljobs_param-jobname.
          i_listado-jobname   = l_dump.
          i_listado-status    = 'D'.
          i_listado-uname     = i_snap-uname.
          i_listado-strtdate  = i_snap-datum.
          i_listado-strttime  = i_snap-uzeit.
          i_listado-progname  = l_report.
          i_listado-tabla     = l_tabla.
          i_listado-typegroup = l_typegroup.
          i_listado-cnam     = l_cnam.
          i_listado-unam     = l_unam.
          i_listado-sin_regla = l_sin_regla.


          CLEAR l_no.
          SELECT SINGLE string FROM zctrljobs_paramt
            INTO @DATA(no_enviar)
           WHERE tipocontrol = @i_zctrljobs_param-tipocontrol
             AND jobname = @i_zctrljobs_param-jobname
             AND report = @i_zctrljobs_param-report
             AND tipo = 'NO_ENVIAR'.

          IF no_enviar <> ''.
            SPLIT no_enviar AT ',' INTO TABLE i_no_enviar.
            LOOP AT i_no_enviar.
              IF l_dump CS i_no_enviar.
                l_no = 'X'.
                EXIT.
              ENDIF.
              IF NOT l_report IS INITIAL.
                IF l_report CS i_no_enviar.
                  l_no = 'X'.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF l_no = ''.
            APPEND i_listado.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR i_listado.
        MOVE-CORRESPONDING i_zctrljobs_param TO i_listado.
        i_listado-regla    = i_zctrljobs_param-jobname.
        i_listado-jobname  = l_dump.
        i_listado-uname    = i_snap-uname.
        i_listado-status   = 'D'.
        i_listado-strtdate = i_snap-datum.
        i_listado-strttime = i_snap-uzeit.
        i_listado-progname = l_report.
        i_listado-cnam     = l_cnam.
        i_listado-unam     = l_unam.
        i_listado-sin_regla = l_sin_regla.

        CLEAR l_no.
        SELECT SINGLE string FROM zctrljobs_paramt
          INTO @no_enviar
         WHERE tipocontrol = @i_zctrljobs_param-tipocontrol
           AND jobname = @i_zctrljobs_param-jobname
           AND report = @i_zctrljobs_param-report
           AND tipo = 'NO_ENVIAR'.
        IF no_enviar <> ''.
          SPLIT no_enviar AT ',' INTO TABLE i_no_enviar.

          LOOP AT i_no_enviar.
            IF l_dump CS i_no_enviar.
              l_no = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF l_no = ''.
          APPEND i_listado.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  agrupar_listado
*&---------------------------------------------------------------------*
FORM agrupar_listado.
  DATA i_list LIKE i_listado OCCURS 0 WITH HEADER LINE.

  o_prog->sgpi_texto( 'Agrupando listado' ).

* Filtramos aquellos valores que hayamos dicho de no mostrar
  LOOP AT i_zctrljobs_param WHERE casoaviso = 'N'.
    DELETE i_listado WHERE     tipocontrol  = i_zctrljobs_param-tipocontrol
                           AND jobname     CP i_zctrljobs_param-jobname
                           AND progname    CP i_zctrljobs_param-report.

  ENDLOOP.

  LOOP AT i_listado.
    IF i_listado-status2 = 'N' OR i_listado-status2 = 'X'.
      i_listado-lights = zcl_ap_alv=>c_sem_ambar.
    ELSE.
      IF    i_listado-status = 'A'
         OR i_listado-status = 'E'
         OR i_listado-status = 'D'.
        i_listado-lights = zcl_ap_alv=>c_sem_rojo.
      ELSE.
        i_listado-lights = zcl_ap_alv=>c_sem_verde.
      ENDIF.
    ENDIF.
    CASE i_listado-status.
      WHEN 'A'.
        i_listado-status = 'Cancelado'.
      WHEN 'F'.
        i_listado-status = 'Finalizado'.
      WHEN 'E'.
        i_listado-status = 'Errores'.
      WHEN 'D'.
        i_listado-status = 'Dump'.
    ENDCASE.

    CASE i_listado-status2.
      WHEN 'X'.
        i_listado-status2 = 'Cumple condición'.
      WHEN 'N'.
        i_listado-status2 = 'No ejecutado'.
      WHEN 'E'.
        i_listado-status2 = 'Error'.
    ENDCASE.

    MODIFY i_listado.
  ENDLOOP.

  i_list[] = i_listado[].
  REFRESH i_listado.
  LOOP AT i_list WHERE sin_regla IS INITIAL.
    SELECT * FROM zctrljobs_resp
     WHERE tipocontrol = i_list-tipocontrol
       AND jobname     = i_list-regla
       AND report      = i_list-report.
      i_listado = i_list.
      i_listado-usuario = zctrljobs_resp-usuario.
      i_listado-email   = zctrljobs_resp-email.
      i_listado-check   = p_ejec.
      APPEND i_listado.
    ENDSELECT.
    IF sy-subrc <> 0.
      APPEND i_list TO i_listado.
    ENDIF.
  ENDLOOP.

  SORT i_listado.
  DELETE ADJACENT DUPLICATES FROM i_listado.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  buscar_report_dump
*&---------------------------------------------------------------------*
FORM buscar_report_dump
  USING    pe_snap      STRUCTURE snap
  CHANGING ps_report    TYPE any
           ps_dump      TYPE any
           ps_tabla     TYPE any
           ps_typegroup TYPE any
           ps_cnam      TYPE trdir-cnam
           ps_unam      TYPE trdir-unam.


  DATA: i_infotab  TYPE rsdumptab,
        l_dumpinfo LIKE rsdumpinfo,
        " TODO: variable is assigned but never used (ABAP cleaner)
        l_aux1     TYPE c LENGTH 255,
        l_aux2     TYPE c LENGTH 255,
        l_aux      TYPE c LENGTH 100.
  DATA i_ft TYPE rsdump_ft_it.
  DATA: l_shorttext       TYPE string,
        " TODO: variable is assigned but never used (ABAP cleaner)
        l_explanation     TYPE string,
        " TODO: variable is assigned but never used (ABAP cleaner)
        l_userhints       TYPE string,
        l_description     TYPE string,
        " TODO: variable is assigned but never used (ABAP cleaner)
        l_correctionhints TYPE string,
        " TODO: variable is assigned but never used (ABAP cleaner)
        l_internalhints   TYPE string.

  CLEAR: ps_tabla, ps_typegroup.

  CALL FUNCTION 'RS_ST22_GET_DUMPS'
    EXPORTING
      p_day     = pe_snap-datum
    IMPORTING
      p_infotab = i_infotab.
  READ TABLE i_infotab INTO l_dumpinfo
       WITH KEY sytime = pe_snap-uzeit
                syuser = pe_snap-uname.
  IF sy-subrc = 0.
    ps_dump = l_dumpinfo-dumpid.
  ENDIF.

  CALL FUNCTION 'RS_ST22_GET_FT'
    EXPORTING
      datum = pe_snap-datum
      uzeit = pe_snap-uzeit
      uname = pe_snap-uname
      ahost = pe_snap-ahost
      modno = pe_snap-modno
      mandt = pe_snap-mandt
    IMPORTING
      ft    = i_ft.

  CALL FUNCTION 'RS_ST22_READ_SNAPT'
    IMPORTING
      shorttext       = l_shorttext
      explanation     = l_explanation
      userhints       = l_userhints
      description     = l_description
      correctionhints = l_correctionhints
      internalhints   = l_internalhints.

  IF l_shorttext CS 'A newer version of data type "'.
    SPLIT l_shorttext AT 'A newer version of data type "' INTO l_aux1 l_aux2.
    SPLIT l_aux2 AT '"' INTO ps_tabla l_aux1.
  ENDIF.
  IF ps_tabla IS INITIAL.
    IF l_description CS 'discovered that Dictionary type "'.
      SPLIT l_description AT 'discovered that Dictionary type "' INTO l_aux1 l_aux2.
      SPLIT l_aux2 AT '"' INTO ps_tabla l_aux1.
    ENDIF.
  ENDIF.
  IF ps_tabla IS INITIAL.
    IF l_shorttext CS 'Type group changed at runtime.'.
      SPLIT l_description AT 'Type group "' INTO l_aux1 l_aux2.
      SPLIT l_aux2 AT '"' INTO ps_typegroup l_aux1.
    ENDIF.
  ENDIF.

  CASE pe_snap-flist(5).
    WHEN 'FC015'.
      ps_report = l_dumpinfo-programname.
    WHEN 'FC023'.
      SELECT  * FROM snap
      UP TO 1 ROWS
       WHERE datum = pe_snap-datum
         AND uzeit = pe_snap-uzeit
         AND modno = pe_snap-modno
         AND seqno = '001'
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      SPLIT snap-flist02 AT 'D044' INTO ps_report l_aux.
      IF NOT l_aux IS INITIAL.
        ps_report = l_aux(20).
      ENDIF.
    WHEN OTHERS.
      CASE l_dumpinfo-dumpid.
        WHEN 'LOAD_PROGRAM_NOT_FOUND'.
          ps_report = l_dumpinfo-includename.
        WHEN OTHERS.
          ps_report = l_dumpinfo-programname.
      ENDCASE.
  ENDCASE.

  IF ps_report CS '='.
    SPLIT ps_report AT '=' INTO ps_report l_aux.
  ENDIF.
  DATA l_report TYPE string.
  l_report = ps_report.
  CLEAR ps_report.

*  IF ps_report = 'SAPLSKBH' OR ps_report = 'CL_SALV_TABLE' OR ps_report = 'SAPMSSY0' OR ps_report CS ':' OR ps_report CS '?'.
*    PERFORM get_prog USING i_ft 'TC' CHANGING ps_report ps_cnam ps_unam.
*    IF ps_cnam IS INITIAL.
*      PERFORM get_prog USING i_ft 'P4' CHANGING ps_report ps_cnam ps_unam.
*    ENDIF.
*    IF ps_cnam IS INITIAL.
*      PERFORM get_prog USING i_ft 'AM' CHANGING ps_report ps_cnam ps_unam.
*    ENDIF.
*    IF ps_cnam IS INITIAL.
*      PERFORM get_prog USING i_ft 'AP' CHANGING ps_report ps_cnam ps_unam.
*    ENDIF.
*
*  ELSEIF ps_report CS '/IWBEP'.
*    PERFORM get_prog USING i_ft 'P1' CHANGING ps_report ps_cnam ps_unam.
*  ENDIF.
  LOOP AT i_ft ASSIGNING FIELD-SYMBOL(<ft>) WHERE value(1) = 'Z'.
    IF strlen( <ft>-value ) > 5 and strlen( <ft>-value ) <= 40.
      PERFORM get_progd USING <ft>-value CHANGING ps_cnam ps_unam.
      IF NOT ps_cnam IS INITIAL.
        ps_report = <ft>-value.
        RETURN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF ps_report IS INITIAL.
    ps_report = l_report.
    PERFORM get_progd USING ps_report CHANGING ps_cnam ps_unam.
  ENDIF.

ENDFORM.

FORM get_prog USING i_ft TYPE rsdump_ft_it pe_tipo
            CHANGING ps_report ps_cnam ps_unam.

  ASSIGN i_ft[ id = pe_tipo ] TO FIELD-SYMBOL(<ft>).
  IF sy-subrc = 0.
    ps_report = <ft>-value.
    PERFORM get_progd USING <ft>-value CHANGING ps_cnam ps_unam.
  ENDIF.

ENDFORM.

FORM get_progd USING report
            CHANGING ps_cnam ps_unam.

  CHECK report NE ''.

  SELECT SINGLE cnam unam FROM trdir
    INTO (ps_cnam, ps_unam)
   WHERE name = report.
  IF sy-subrc NE 0.
    DATA(l_rep) = report && '%'.
    SELECT SINGLE cnam unam FROM trdir
      INTO (ps_cnam, ps_unam)
     WHERE name LIKE l_rep.
  ENDIF.



ENDFORM.
