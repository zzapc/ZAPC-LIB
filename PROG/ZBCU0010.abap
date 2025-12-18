************************************************************************
*
* MÓDULO      : BC                                                     *
* TIPO        : REPORT                                                 *
* TITULO      : Información detallada modificaciones objetos
* DESCRIPCION :                                                        *
* ...                                                                  *
* ...                                                                  *
* AUTOR: Andres Picazo                          FECHA: 10/11/2007
* MODIFICACIONES                                                       *
* --------------                                                       *
* FECHA        NOMBRE            RUTA               ORDEN              *
* -------------------------------------------------------------------- *
************************************************************************
REPORT zbcu010
      NO STANDARD PAGE HEADING
      LINE-COUNT 090
      LINE-SIZE 120.

INCLUDE zbcialv. "nclude rutinas comunes listados ALV
INCLUDE zbcimac.
INCLUDE zbcimail.

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: trdir,
        e070,
        e071,
        zbc004,
        swfdevena.


*------TABLAS INTERNAS-------------------------------------------------*
DATA: BEGIN OF i_listado OCCURS 100,
        check,
        udat    LIKE zbc004-udat,
        tarea   LIKE zbc004-tarea,
        object  LIKE zbc004-object,
        name    LIKE zbc004-name,
        vern    LIKE zbc004-vern,
        text    LIKE trdirt-text,
        unam    LIKE zbc004-unam,
        stime   LIKE zbc004-stime,
        funcion LIKE rs38l-name,
        grupo   LIKE rs38l-area,
        trkorr    LIKE zbc004-trkorr,
        as4text LIKE e07t-as4text,
        trfunction  LIKE e070-trfunction,
        trstatus  LIKE e070-trstatus,
        tarsystem	LIKE e070-tarsystem,
        korrdev	LIKE e070-korrdev,
        as4user	LIKE e070-as4user,
        as4date	LIKE e070-as4date,
        as4time	LIKE e070-as4time,
        strkorr	LIKE e070-strkorr,
        comentario LIKE zbc004-comentario,
      END OF i_listado.
DATA: i_trdir LIKE trdir OCCURS 0 WITH HEADER LINE,
      i_dd02l LIKE dd02l OCCURS 0 WITH HEADER LINE,
      i_dd04l LIKE dd04l OCCURS 0 WITH HEADER LINE,
      i_stxh  LIKE stxh  OCCURS 0 WITH HEADER LINE,
      i_stxfadm LIKE stxfadm OCCURS 0 WITH HEADER LINE,
      i_hrs1000 LIKE hrs1000 OCCURS 0 WITH HEADER LINE,
      i_zbc004 LIKE zbc004 OCCURS 0  WITH HEADER LINE.

*------VARIABLES-------------------------------------------------------*
DATA: l_include LIKE rs38l-include,
      v_directorio TYPE string,
      v_nomail.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk_par WITH FRAME TITLE text-002. "Pará

SELECT-OPTIONS: s_fechas FOR sy-datum DEFAULT sy-datum OBLIGATORY,
                s_uname FOR sy-uname.

SELECTION-SCREEN END OF BLOCK blk_par.

SELECTION-SCREEN BEGIN OF BLOCK blk_opc WITH FRAME TITLE text-003.
PARAMETERS: p_list RADIOBUTTON GROUP r,
            p_actu RADIOBUTTON GROUP r.

PARAMETERS: p_email(80).
SELECTION-SCREEN END OF BLOCK blk_opc.
************************************************************************
*
*                  LOGICA DEL PROGRAMA
*
************************************************************************

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_list = 'X'.
    PERFORM seleccion_datos.

    PERFORM listado.
  ELSEIF p_actu = 'X'.
    PERFORM actualizar_datos.
  ENDIF.

  IF NOT p_email IS INITIAL AND v_nomail IS INITIAL.
    PERFORM seleccion_datos.
    PERFORM enviar_mail.
  ENDIF.

************************************************************************
*
*                  FORMS ADICIONALES
*
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  SELECCION_DATOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM seleccion_datos.

  SELECT * FROM zbc004
    INTO CORRESPONDING FIELDS OF TABLE i_listado
   WHERE udat IN s_fechas
     AND unam IN s_uname.
  LOOP AT i_listado WHERE NOT trkorr IS INITIAL.
    SELECT SINGLE * FROM e070
      INTO CORRESPONDING FIELDS OF i_listado
     WHERE trkorr = i_listado-trkorr.

    SELECT SINGLE as4text FROM e07t
      INTO i_listado-as4text
     WHERE langu = sy-langu
       AND trkorr = i_listado-trkorr.
    MODIFY i_listado.
  ENDLOOP.

  LOOP AT i_listado WHERE object = 'PROG'.
    SELECT SINGLE text FROM trdirt
      INTO i_listado-text
     WHERE sprsl = sy-langu
       AND name  = i_listado-name.
    IF sy-subrc NE 0.
      SELECT SINGLE text FROM trdirt
        INTO i_listado-text
       WHERE sprsl = 'E'
         AND name  = i_listado-name.
      IF sy-subrc NE 0.
        SELECT SINGLE text FROM trdirt
          INTO i_listado-text
         WHERE name  = i_listado-name.
      ENDIF.
    ENDIF.
    MODIFY i_listado.
  ENDLOOP.

  LOOP AT i_listado WHERE object(2) = 'PD'.
    SELECT SINGLE stext FROM hrs1000
      INTO i_listado-text
     WHERE otype = i_listado-object+2
       AND objid = i_listado-name
       AND langu = sy-langu.
    MODIFY i_listado.
  ENDLOOP.

  LOOP AT i_listado WHERE object = 'TABL'.
    SELECT SINGLE ddtext FROM  dd02t
      INTO i_listado-text
     WHERE tabname     = i_listado-name
    AND ddlanguage  = sy-langu.
    MODIFY i_listado.
  ENDLOOP.

  LOOP AT i_listado WHERE object = 'SSFO'.
    SELECT SINGLE caption FROM  stxfadmt
      INTO i_listado-text
           WHERE  langu     = sy-langu
           AND    formname  = i_listado-name.
    MODIFY i_listado.
  ENDLOOP.


  LOOP AT i_listado WHERE name(1) = 'L'
                      AND object = 'PROG'.
    l_include = i_listado-name.
    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
      CHANGING
        funcname            = i_listado-funcion
        group               = i_listado-grupo
        include             = l_include
      EXCEPTIONS
        function_not_exists = 1
        include_not_exists  = 2
        group_not_exists    = 3
        no_selections       = 4
        no_function_include = 5
        OTHERS              = 6.
    IF sy-subrc = 0.
      IF i_listado-text IS INITIAL.
        SELECT SINGLE stext FROM  tftit
          INTO i_listado-text
               WHERE  spras     = sy-langu
               AND    funcname  = i_listado-funcion.
      ENDIF.
      MODIFY i_listado.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " SELECCIONAR_DATOS

*&---------------------------------------------------------------------*
*&      Form  ACTUALIZAR_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM actualizar_datos.

************** REPORTS ************************
  SELECT * FROM trdir
    INTO TABLE i_trdir
   WHERE edtx = ''
      AND ( cdat IN s_fechas
         OR udat IN s_fechas )
      AND ( cnam IN s_uname
         OR unam IN s_uname )
      AND NOT unam IN ('SAP', 'DDIC').

  LOOP AT i_trdir.
    IF i_trdir-name(1) = '/' OR
       i_trdir-name = 'RSTXFPR2' OR
       i_trdir-name = 'RSORAVDZ' OR
       i_trdir-name = 'UNICTOOL' OR
       i_trdir-name(4) = 'TRZ9' OR
       i_trdir-name(4) = 'RKOO' OR
       i_trdir-name(4) = 'RV13' OR
       i_trdir-name(4) = 'PCAS' OR
       i_trdir-name(5) = 'RPCAS' OR
       i_trdir-name(1) = '!' OR
       i_trdir-name(2) = 'AQ' OR
       i_trdir-name(2) = 'GP' OR
       i_trdir-name(2) = 'AL'.

      CONTINUE.
    ENDIF.
    CLEAR i_zbc004.
    MOVE-CORRESPONDING i_trdir TO i_zbc004.
    i_zbc004-object = 'PROG'.
    IF i_zbc004-unam IS INITIAL.
      i_zbc004-unam = i_trdir-cnam.
      i_zbc004-udat = i_trdir-cdat.
    ENDIF.
    IF NOT ( i_zbc004-unam = 'SAP' OR
             i_zbc004-unam = 'DDIC').
      APPEND i_zbc004.
    ENDIF.
  ENDLOOP.

***** BUSCO CAMBIOS EN MODIFICACIOENS AL ESTANDAR!!!!!!
  DATA: i_zbc003 TYPE TABLE OF zbc003 WITH HEADER LINE.

  SELECT * FROM zbc003
   INTO TABLE i_zbc003
  WHERE estado = 'M'.
  IF sy-subrc = 0.
    SELECT * FROM trdir
      INTO TABLE i_trdir
      FOR ALL ENTRIES IN i_zbc003
     WHERE name = i_zbc003-name
        AND ( cdat IN s_fechas
           OR udat IN s_fechas )
        AND ( udat IN s_uname
           OR unam IN s_uname ).

    LOOP AT i_trdir.
      CLEAR i_zbc004.
      MOVE-CORRESPONDING i_trdir TO i_zbc004.
      i_zbc004-object = 'PROG'.
      IF i_zbc004-unam IS INITIAL.
        i_zbc004-unam = i_trdir-cnam.
        i_zbc004-udat = i_trdir-cdat.
      ENDIF.
      i_zbc004-modstandard = 'X'.
      i_zbc004-comentario = '¡¡¡CAMBIO EN MODIFICACIÓN AL ESTÁNDAR!!!'.
      APPEND i_zbc004.

      t 'Se ha modificado el objecto'.
      t i_trdir-name.
      t zcl_c=>empresa.

      CALL FUNCTION 'Z_ENVIO_MAIL'
        EXPORTING
          subject   = i_zbc004-comentario
          direccion = 'andres@andrespicazo.com'
          urgente   = 'X'
        TABLES
          texto     = i_texto_mail.

      CALL FUNCTION 'Z_ENVIO_MAIL'
        EXPORTING
          subject   = i_zbc004-comentario
          direccion = 'DESARROLLO'
          urgente   = 'X'
        TABLES
          texto     = i_texto_mail.
    ENDLOOP.

**** Buscamos WF desactivados que queramos controlar
    LOOP AT i_zbc003 WHERE tcode = 'WF'.
      SELECT SINGLE * FROM swfdevena
       WHERE objcateg = 'BO'
         AND rectype  = i_zbc003-name
         AND event NE 'RELEASED'
         AND enabled  = 'X'.
      IF sy-subrc NE 0.
        REFRESH i_texto_mail.
        t 'Se ha desactivado el WF' .
        t i_zbc003-name.

        CALL FUNCTION 'Z_ENVIO_MAIL'
          EXPORTING
            subject   = 'Se ha desactivado el WF'
            direccion = 'andres@andrespicazo.com'
            urgente   = 'X'
          TABLES
            texto     = i_texto_mail.

        CALL FUNCTION 'Z_ENVIO_MAIL'
          EXPORTING
            subject   = 'Se ha desactivado el WF'
            direccion = 'APICAZO'
            urgente   = 'X'
          TABLES
            texto     = i_texto_mail.
      ENDIF.
    ENDLOOP.
  ENDIF.
****

************** TABLAS ************************
  SELECT * FROM dd02l
    INTO TABLE i_dd02l
   WHERE as4date IN s_fechas
      AND NOT as4user  IN ('SAP', 'DDIC')
      AND as4user IN s_uname.

  LOOP AT i_dd02l WHERE tabname(1) NE '/'.
    CLEAR i_zbc004.
    MOVE-CORRESPONDING i_dd02l TO i_zbc004.
    i_zbc004-object = 'TABL'.
    i_zbc004-name = i_dd02l-tabname.
    i_zbc004-vern = i_dd02l-as4vers.
    i_zbc004-unam = i_dd02l-as4user.
    i_zbc004-udat = i_dd02l-as4date.
    i_zbc004-stime = i_dd02l-as4time.
    APPEND i_zbc004.
  ENDLOOP.

************** ELEMENTOS DE DATOS ************************
  SELECT * FROM dd04l
    INTO TABLE i_dd04l
   WHERE as4date IN s_fechas
      AND NOT as4user  IN ('SAP', 'DDIC')
      AND as4user IN s_uname.

  LOOP AT i_dd04l.
    CLEAR i_zbc004.
    MOVE-CORRESPONDING i_dd04l TO i_zbc004.
    i_zbc004-object = 'DTEL'.
    i_zbc004-name = i_dd04l-rollname.
    i_zbc004-vern = i_dd04l-as4vers.
    i_zbc004-unam = i_dd04l-as4user.
    i_zbc004-udat = i_dd04l-as4date.
    i_zbc004-stime = i_dd04l-as4time.
    APPEND i_zbc004.
  ENDLOOP.

************** FORMULARIOS SAPSCRIP ************************
  SELECT * FROM stxh
    INTO TABLE i_stxh
   WHERE tdobject = 'FORM'
     AND ( tdfdate IN s_fechas OR
           tdldate IN s_fechas )
      AND NOT ( tdfuser  IN ('SAP', 'DDIC') OR
                tdluser  IN ('SAP', 'DDIC') )
      AND ( tdfuser IN s_uname OR
            tdluser IN s_uname ).

  LOOP AT i_stxh.
    CLEAR i_zbc004.
    MOVE-CORRESPONDING i_stxh TO i_zbc004.
    i_zbc004-object = 'FORM'.
    i_zbc004-name = i_stxh-tdname.
    i_zbc004-vern = i_stxh-tdversion.
    IF NOT i_stxh-tdluser IS INITIAL.
      i_zbc004-unam = i_stxh-tdluser.
      i_zbc004-udat = i_stxh-tdldate.
      i_zbc004-stime = i_stxh-tdltime.
    ELSE.
      i_zbc004-unam = i_stxh-tdfuser.
      i_zbc004-udat = i_stxh-tdfdate.
      i_zbc004-stime = i_stxh-tdftime.
    ENDIF.
    APPEND i_zbc004.
  ENDLOOP.

************** SMARTFORMS ************************
  SELECT * FROM stxfadm
    INTO TABLE i_stxfadm
   WHERE ( firstdate IN s_fechas OR
           lastdate IN s_fechas )
      AND NOT ( firstuser  IN ('SAP', 'DDIC') OR
                lastuser  IN ('SAP', 'DDIC') )
      AND ( firstuser IN s_uname OR
            lastuser IN s_uname ).

  LOOP AT i_stxfadm.
    CLEAR i_zbc004.
    MOVE-CORRESPONDING i_stxfadm TO i_zbc004.
    i_zbc004-object = 'SSFO'.
    i_zbc004-name = i_stxfadm-formname.
    i_zbc004-vern = i_stxfadm-version.
    IF NOT i_stxfadm-lastuser IS INITIAL.
      i_zbc004-unam = i_stxfadm-lastuser.
      i_zbc004-udat = i_stxfadm-lastdate.
      i_zbc004-stime = i_stxfadm-lasttime.
    ELSE.
      i_zbc004-unam = i_stxfadm-firstuser.
      i_zbc004-udat = i_stxfadm-firstdate.
      i_zbc004-stime = i_stxfadm-firsttime.
    ENDIF.
    APPEND i_zbc004.
  ENDLOOP.

************* OBJETOS WORKFLOW  *******************************
  SELECT * FROM hrs1000
    INTO TABLE i_hrs1000
   WHERE aedtm IN s_fechas
     AND uname IN s_uname.

  LOOP AT i_hrs1000.
    CLEAR i_zbc004.
    MOVE-CORRESPONDING i_hrs1000 TO i_zbc004.
    i_zbc004-object = 'PD'.
    i_zbc004-object+2 = i_hrs1000-otype.
    i_zbc004-name = i_hrs1000-objid.
    i_zbc004-udat = i_hrs1000-aedtm.
    i_zbc004-unam = i_hrs1000-uname.
    APPEND i_zbc004.
  ENDLOOP.

************* ORDENES DE TRANSPORTE **************************
  RANGES r_object FOR e071-object.
  LOOP AT i_zbc004.
    FREE r_object.
    IF i_zbc004-object = 'PROG'.
      addrango_eq: r_object 'PROG',
                   r_object 'REPS',
                   r_object 'REPT'.
    ENDIF.
    SELECT e071~trkorr as4date
      FROM e071 JOIN e070 ON e071~trkorr = e070~trkorr
      INTO (i_zbc004-trkorr, e070-as4date)
      UP TO 1 ROWS
     WHERE pgmid IN ('R3TR', 'LIMU')
       AND object IN r_object
       AND obj_name = i_zbc004-name
     ORDER BY as4date DESCENDING.
    ENDSELECT.
    IF sy-subrc NE 0 AND i_zbc004-name(1) = 'L'.
      l_include = i_zbc004-name.
      CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
        CHANGING
          funcname            = i_listado-funcion
          group               = i_listado-grupo
          include             = l_include
        EXCEPTIONS
          function_not_exists = 1
          include_not_exists  = 2
          group_not_exists    = 3
          no_selections       = 4
          no_function_include = 5
          OTHERS              = 6.
      IF sy-subrc = 0.
        SELECT e071~trkorr as4date
          FROM e071 JOIN e070 ON e071~trkorr = e070~trkorr
          INTO (i_zbc004-trkorr, e070-as4date)
          UP TO 1 ROWS
         WHERE object   = 'FUGR'
           AND obj_name = i_listado-grupo
         ORDER BY as4date DESCENDING.
        ENDSELECT.
      ENDIF.
    ENDIF.
    MODIFY i_zbc004.
  ENDLOOP.

************* ACTUALIZAR TABLA  *******************************
  LOOP AT i_zbc004 INTO zbc004.
    MODIFY zbc004.
  ENDLOOP.



ENDFORM.                               " ACTUALIZAR_DATOS


*---------------------------------------------------------------------*
*       FORM initialize_fieldcat1                                     *
*---------------------------------------------------------------------*
FORM initialize_fieldcat USING p_fieldtab TYPE slis_t_fieldcat_alv.

  fieldcat_no_out: 'CHECK', 'TRFUNCTION', 'TRSTATUS', 'TARSYSTEM',
                   'KORRDEV', 'AS4USER', 'AS4DATE', 'AS4TIME',
                   'STRKORR'.
  fieldcat_text_simple_input 'TAREA' '__TAREA___'.

ENDFORM.                               " INITIALIZE_FIELDCAT

*---------------------------------------------------------------------*
*       FORM alv_sortinfo                                             *
*---------------------------------------------------------------------*
FORM alv_sortinfo TABLES p_alv_sort TYPE slis_t_sortinfo_alv.

  DATA sortinfo_alv TYPE slis_sortinfo_alv.

  alv_sort_up_group_subrayado: 'UDAT'  1,
                               'TAREA' 2.
  alv_sort_up: 'OBJECT' 3,
               'NAME'   4.

ENDFORM.                    " ALV_SORTINFO
*&---------------------------------------------------------------------*
*&      Form  listado
*&---------------------------------------------------------------------*
FORM listado .
  alv_new_fields_det = 'X'.
  v_colwidth_optimize = 'X'.
  alv_use_grid = ''.
*  v_alv_status = ''.

  IF p_list = 'X'.
    v_alv_tabla = 'I_LISTADO'.
    v_alv_checkbox = 'CHECK'.
  ELSEIF p_actu = 'X'.
    v_alv_tabla = 'I_ZBC004'.
  ENDIF.

  PERFORM initialize_fieldcat USING alv_fieldtab.
  PERFORM alv_sortinfo TABLES alv_sort.

  IF p_list = 'X'.
    PERFORM listado_tabla_alv TABLES i_listado
                               USING v_alv_tabla
                                       alv_fieldtab.
  ELSEIF p_actu = 'X'.
    PERFORM listado_tabla_alv TABLES i_zbc004
                               USING v_alv_tabla
                                     alv_fieldtab.
  ENDIF.


ENDFORM.                    " listado

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       Procesa las acciones de usuario
*---------------------------------------------------------------------*
FORM user_command
  USING pu_okcode
        pu_selfield TYPE slis_selfield.
  CASE pu_okcode.
    WHEN 'VERSION'.
      READ TABLE i_listado INDEX pu_selfield-tabindex.
      IF sy-subrc = 0.
        CALL FUNCTION 'RS_PROGRAM_VERSIONS'
          EXPORTING
            progname         = i_listado-name
          EXCEPTIONS
            function_include = 1
            OTHERS           = 2.
      ENDIF.
    WHEN 'GRABAR'.
      LOOP AT i_listado.
        CLEAR zbc004.
        MOVE-CORRESPONDING i_listado TO zbc004.
        MODIFY zbc004.
      ENDLOOP.
      MESSAGE 'Se han actualizado los registros' TYPE 'I'.
    WHEN 'BORRAR'.
      LOOP AT i_listado WHERE check = 'X'.
        CLEAR zbc004.
        MOVE-CORRESPONDING i_listado TO zbc004.
        DELETE zbc004.
      ENDLOOP.
      MESSAGE 'Se han borrado los registros seleccionados' TYPE 'I'.

    WHEN 'DESCARGAR'.
      PERFORM descargar USING ''.

    WHEN 'MAIL_COD'.
      PERFORM descargar USING 'X'.

    WHEN 'WIKI'.
      DATA: i_buffer(200) OCCURS 0 WITH HEADER LINE,
            l_first,
            l_name(40),
            l_funcion(40),
            l_tarea(40).
      DEFINE copyf.
        i_buffer = &1.
        append i_buffer.
      END-OF-DEFINITION.

      l_first = 'X'.
      LOOP AT i_listado WHERE check = 'X'.
        IF l_first = 'X'.
          copyf '{| border="1" cellpadding="2"'.
          CLEAR l_first.
        ELSE.
          copyf '|-'.
        ENDIF.
        CONCATENATE '[[' i_listado-name ']]' INTO l_name.
        CLEAR l_funcion.
        IF NOT i_listado-funcion IS INITIAL.
          CONCATENATE '[[' i_listado-funcion ']]' INTO l_funcion.
        ENDIF.
        CLEAR l_tarea.
        IF NOT i_listado-tarea IS INITIAL.
          CONCATENATE '[[' i_listado-tarea ']]' INTO l_tarea.
        ENDIF.
        CONCATENATE '|' l_tarea '||'
                        i_listado-object '||'
                        l_name '||'
                        i_listado-text '||'
                        l_funcion '||'
                        i_listado-trkorr '||'
                        i_listado-as4text
              INTO i_buffer SEPARATED BY space.
        APPEND i_buffer.
      ENDLOOP.
      copyf '|}'.

      CALL FUNCTION 'CLPB_EXPORT'
        TABLES
          data_tab   = i_buffer
        EXCEPTIONS
          clpb_error = 1
          OTHERS     = 2.

      MESSAGE 'Se ha exportado el texto al portapapeles' TYPE 'I'.

* Cuando se haga doble click sobre una fila, se muestra el desglose
    WHEN alv_f2code.
      READ TABLE i_listado INDEX pu_selfield-tabindex.
      IF sy-subrc = 0.
        CASE pu_selfield-fieldname.
          WHEN 'TAREA'.
            PERFORM editar_nota.
            MODIFY i_listado INDEX pu_selfield-tabindex.
            pu_selfield-refresh = 'X'.
            pu_selfield-row_stable = 'X'.

          WHEN OTHERS.
            CASE i_listado-object.
              WHEN 'PROG'.
                IF i_listado-funcion IS INITIAL.
                  SET PARAMETER ID 'RID' FIELD i_listado-name.
                  CALL TRANSACTION 'SE38' AND SKIP FIRST SCREEN.
                ELSE.
                  SET PARAMETER ID 'LIB' FIELD i_listado-funcion.
                  CALL TRANSACTION 'SE37' AND SKIP FIRST SCREEN.
                ENDIF.
            ENDCASE.
        ENDCASE.
      ENDIF.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_MAIL
*&---------------------------------------------------------------------*
FORM enviar_mail .
  DATA l_fecha(10).
  DATA i_texto LIKE txw_note OCCURS 0 WITH HEADER LINE.

  READ TABLE s_fechas INDEX 1.
  WRITE s_fechas-low TO l_fecha.
  CONCATENATE 'Modificaciones objetos' zcl_c=>empresa l_fecha
         INTO v_titulo_mail SEPARATED BY space.

  inicio_mail_html v_titulo_mail.

  SORT i_listado.
  LOOP AT i_listado.
    AT NEW udat.
      WRITE i_listado-udat TO l_fecha.
      t3 '<H3>Fecha:' l_fecha '</H3>'.
      t '<br>'.
      t '<table width="1024" border="0">'.
      t '  <tr bgcolor="#6699FF">'.
      v_letra_mail = 'font-size:10px;'.
      col_mail: 'Tarea',
                'Objeto',
                'Nombre',
                'Texto',
                'Función',
                'Orden',
                'Descripción'.

      t '  </tr>'.
    ENDAT.

    t '<tr>'.
    celda_mail: i_listado-tarea,
                i_listado-object,
                i_listado-name,
                i_listado-text,
                i_listado-funcion,
                i_listado-trkorr,
                i_listado-as4text.
    t '  </tr>'.

    IF NOT i_listado-comentario IS INITIAL.
      CALL FUNCTION 'Z_PARTIR_LINEA'
        EXPORTING
          linea    = i_listado-comentario
*          longitud = 72
        TABLES
          lineas   = i_texto.
      LOOP AT i_texto.
        t '<tr><td></td><td></td><td></td><td style=font-size:12px;>'.
        t i_texto.
        t '  </td></tr>'.
      ENDLOOP.
    ENDIF.

    AT END OF udat.
      t '</table>'.
      t '<br>'.
    ENDAT.

    AT LAST.
      t '</body>'.
      enviar_mail p_email ''.
    ENDAT.
  ENDLOOP.


ENDFORM.                    " ENVIAR_MAIL
*&---------------------------------------------------------------------*
*&      Form  EDITAR_NOTA
*&---------------------------------------------------------------------*
FORM editar_nota .
  DATA i_texto LIKE txw_note OCCURS 0 WITH HEADER LINE.

  REFRESH i_texto.
  CALL FUNCTION 'Z_PARTIR_LINEA'
    EXPORTING
      linea    = i_listado-comentario
      longitud = 72
    TABLES
      lineas   = i_texto.

  CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
* EXPORTING
*   EDIT_MODE       = 'X'
    TABLES
      t_txwnote       = i_texto.

  CALL FUNCTION 'Z_UNIR_LINEA'
    IMPORTING
      linea  = i_listado-comentario
    TABLES
      lineas = i_texto.

ENDFORM.                    " EDITAR_NOTA
*&---------------------------------------------------------------------*
*&      Form  DESCARGAR
*&---------------------------------------------------------------------*
FORM descargar USING pe_mail.
  DATA:  i_tabla(255) OCCURS 0 WITH HEADER LINE,
         l_fichero TYPE string,
         l_extension(4),
         l_aux TYPE string,
         l_fecha(10).

  DATA: BEGIN OF i_ficheros OCCURS 100,
          funcion LIKE rs38l-name,
          grupo   LIKE rs38l-area,
          object  LIKE zbc004-object,
          name    LIKE zbc004-name,
          text    LIKE trdirt-text,
        END OF i_ficheros.

  LOOP AT i_listado WHERE check = 'X'
                      AND object = 'PROG'.
    CLEAR i_ficheros.
    MOVE-CORRESPONDING i_listado TO i_ficheros.
    COLLECT i_ficheros.
  ENDLOOP.

  IF pe_mail = 'X'.
    CLEAR: v_directorio, l_extension.
    v_nomail = 'X'.
    WRITE sy-datum TO l_fecha.
*    concatenate '(' c_cliente ')[' l_fecha ']' into l_aux.
    CONCATENATE '(' zcl_c=>empresa ')' INTO l_aux.
  ELSE.
    l_extension = '.ABP'.
    IF v_directorio IS INITIAL.
      CALL METHOD cl_gui_frontend_services=>directory_browse
        EXPORTING
          window_title         = 'Selecciona un directorio'
          initial_folder       = 'C:\TEMP\'
        CHANGING
          selected_folder      = v_directorio
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      CONCATENATE v_directorio '\' INTO v_directorio.
    ENDIF.
  ENDIF.

  LOOP AT i_ficheros.
    READ REPORT i_ficheros-name INTO  i_tabla.
    IF sy-subrc = 0.
      REPLACE ':' WITH '' INTO i_ficheros-text.
      REPLACE '.' WITH '' INTO i_ficheros-text.
      REPLACE '/' WITH '' INTO i_ficheros-text.
      REPLACE '\' WITH '' INTO i_ficheros-text.

      IF i_ficheros-funcion IS INITIAL.
        CONCATENATE v_directorio
                  i_ficheros-name '-' i_ficheros-text
                  l_extension INTO l_fichero.
      ELSE.
        CONCATENATE v_directorio i_ficheros-funcion '-'
                  i_ficheros-grupo '-'
                  i_ficheros-name '-' i_ficheros-text
                  l_extension INTO l_fichero.
      ENDIF.

      IF pe_mail IS INITIAL.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename                = l_fichero
          TABLES
            data_tab                = i_tabla
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            OTHERS                  = 22.
      ELSE.
        inicio_mail_html l_fichero.
        t4 '<h3>Programa:' i_ficheros-name i_ficheros-text '</h3>'.
        IF NOT i_ficheros-funcion IS INITIAL.
          t3 '<h3>Función:' i_ficheros-funcion '</h3>'.
        ENDIF.
        IF NOT i_ficheros-grupo IS INITIAL.
          t3 '<h3>Grupo de funciones:' i_ficheros-grupo '</h3>'.
        ENDIF.
        t3 '<h4>Fecha:' l_fecha '</h4>'.
        t '<br>'.
        t '<pre>'.
        LOOP AT i_tabla.
          t i_tabla.
        ENDLOOP.
        t '</pre></body>'.
        CONCATENATE l_aux l_fichero INTO l_fichero.
        CALL FUNCTION 'Z_ENVIO_MAIL'
          EXPORTING
            subject   = l_fichero
            direccion = p_email
            urgente   = 'X'
            html      = 'X'
          TABLES
            texto     = i_texto_mail.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DESCARGAR
