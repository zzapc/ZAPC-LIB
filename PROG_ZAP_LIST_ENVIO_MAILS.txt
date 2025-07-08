***********************************************************************
* TIPO : LISTADO
* TITULO : Listado mails enviados
* DESCRIPCION : Listado mails enviados
*
* AUTOR: Andrés Picazo                                FECHA: 28/04/2017
*
* Doc. Tecnica: http://sap4.com/tareas?=&cliente=CCC&objeto=OOO
**->MANUAL:http://sap4.com,http://sap4.com/edicion
**->PLANTILLA:,http://sap4.com
*
* MODIFICACIONES
* 28/04/2017 Tarea inicial: http://sap4.com/tareas?&ntask=TTT
*
***********************************************************************
REPORT zap_list_envio_mails.

*------INCLUDES--------------------------------------------------------*

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: zsost.

*------TABLAS INTERNAS-------------------------------------------------*
DATA: i_sndrecs TYPE soxsp2tab,
      l_sndrec  TYPE soxsp2.

*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check.
  PUBLIC SECTION.
    METHODS: handle_double_click REDEFINITION.
    METHODS: handle_user_command REDEFINITION.
ENDCLASS.                    "lcl_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_listado,
             check        TYPE xfeld,
*             lights  type zico_estado_mensaje,
             status       TYPE zsost-status,
             fecha_envio  TYPE zsost-fecha_envio,
             hora_envio   TYPE zsost-hora_envio,
             titulo       TYPE zsost-titulo,
             emisor       TYPE zsost-emisor,
             destinatario TYPE zsost-destinatario,
             forma_envio  TYPE zsost-forma_envio,
             sndreq	      TYPE os_guid,
*             message type bapi_msg,
           END OF t_listado,
           tt_listado TYPE TABLE OF t_listado.
    DATA: i_listado TYPE tt_listado,
          l_listado TYPE t_listado.

    METHODS: main.

    METHODS:  listado,
      seleccionar_datos.

ENDCLASS.                    "REPORT DEFINITION

*------VARIABLES-------------------------------------------------------*
DATA: o_prog TYPE REF TO zcl_report,
      o_alv  TYPE REF TO lcl_alv.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-sel.
SELECT-OPTIONS: s_fecha  FOR zsost-fecha_envio OBLIGATORY DEFAULT sy-datum,
                s_hora   FOR zsost-hora_envio,
                s_titulo FOR zsost-titulo,
                s_emisor FOR sy-uname DEFAULT sy-uname MODIF ID noe,
                s_emimai FOR zsost-emisor NO-DISPLAY,
                s_destin FOR zsost-destinatario,
                s_forma  FOR zsost-forma_envio.
SELECTION-SCREEN: SKIP 1.
PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK 001.
__botones_plantilla.


************************************************************************
*
* LOGICA DEL PROGRAMA
*
************************************************************************

*----------------------------------------------------------------------*
* CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.
  METHOD handle_double_click.
    DATA: cur_wa TYPE soxsp2,
          tab    TYPE soxsp2tab.
    DATA lx_bcs TYPE REF TO cx_bcs.

    READ TABLE o_prog->i_listado INTO o_prog->l_listado INDEX row.
    IF sy-subrc = 0.
      READ TABLE i_sndrecs INTO cur_wa WITH KEY sndreq = o_prog->l_listado-sndreq.
      IF sy-subrc = 0.
        APPEND cur_wa TO tab.
        TRY.
            cl_sndrec_bcs=>display( tab ).
          CATCH cx_bcs INTO lx_bcs.
            MESSAGE ID lx_bcs->msgid TYPE 'S' NUMBER lx_bcs->msgno
              WITH TEXT-018 space space space.
        ENDTRY.
*
*        PERFORM show_document IN PROGRAM rssosoststat
*                              USING cur_wa IF FOUND.
      ENDIF.
    ENDIF.
  ENDMETHOD. "handle_double_click
  METHOD handle_user_command.
    DATA: l_row  TYPE i,
          cur_wa TYPE soxsp2.
    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    CASE e_salv_function.
      WHEN 'EXCEL'.
        exportar_excel( ).
      WHEN 'M01'.
        get_seleccion( ).
        LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
          READ TABLE i_sndrecs INTO cur_wa WITH KEY sndreq = <listado>-sndreq.
          IF sy-subrc = 0.
            TRY.
                cl_sndrec_bcs=>delete(
                  i_sndrec       = cur_wa
                  i_not_in_queue = 'X' ).
              CATCH cx_bcs .
                CONTINUE.
            ENDTRY.
            DELETE o_prog->i_listado.
          ENDIF.
        ENDLOOP.
        IF sy-subrc NE 0.
          MESSAGE 'Seleccione algún registro' TYPE 'I'.
        ELSE.
          refresh( ).
        ENDIF.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD. "handle_USER_COMMAND
ENDCLASS. "lcl_alv IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.
  METHOD main.
    seleccionar_datos( ).
    listado( ).
  ENDMETHOD.                    "REPORT

  METHOD seleccionar_datos.
    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    sgpi_texto( 'Seleccionando datos' ).
*    i_sost = zcl_envio_mail=>consulta_sost( r_fecha = s_fecha[] ).

    DATA:  l_status  TYPE  soststatus.
    l_status = 'XXXXXXXX '.
    IF lines( s_emisor ) <= 1.
      DATA(r_emisor) = s_emisor[].
    ENDIF.

    CALL FUNCTION 'SX_SNDREC_SELECT'
      EXPORTING
*       SND_ART       =
        snd_date      = s_fecha[]
        snd_time      = s_hora[]
*       DEL_DATE      =
*       DEL_TIME      =
        status        = l_status
        notifications = 'X'
        sender        = r_emisor[]
*       MAXSEL        =
        all_waiting   = 'X'
      IMPORTING
        sndrecs       = i_sndrecs.

    o_prog->o_sgpi->get_filas_tabla( i_listado[] ).
    LOOP AT i_sndrecs INTO l_sndrec WHERE titel IN s_titulo
                                      AND stat_date IN s_fecha "No sé porqué en la función no filtra bien
                                      AND usernam IN s_emisor
                                      AND usernam IN s_emimai.
      sgpi_texto( texto1 = 'Procesando datos' cant_porc = 100 ).
      CLEAR l_listado.

      IF l_sndrec-msgty = 'E'.
        l_listado-status = '@5C@'.
      ELSE.
        l_listado-status = '@5B@'.
      ENDIF.

      CASE l_sndrec-sndart.
        WHEN 'INT'.
          l_listado-forma_envio = 'Email'.
        WHEN 'PAG'.
          l_listado-forma_envio = 'SMS'.
        WHEN OTHERS.
          l_listado-forma_envio =  l_sndrec-sndart.
      ENDCASE.
      IF l_listado-forma_envio IN s_forma.

        l_listado-titulo = l_sndrec-titel.
        l_listado-emisor = l_sndrec-usernam.
        l_listado-destinatario = zcl_ap_envio_mail=>get_dir_envio_from_adrnr( l_sndrec-adrnr ).
        IF l_listado-destinatario IN s_destin.
          l_listado-fecha_envio = l_sndrec-stat_date.
          l_listado-hora_envio = l_sndrec-stat_time.
          l_listado-sndreq = l_sndrec-sndreq.

          APPEND l_listado TO i_listado.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.

    sgpi_texto( 'Generando informe' ).

    o_alv->add_button( button = 'M01' text = 'Borrar'  icon = icon_delete ).

    o_alv->set_layout( p_vari ).

    o_alv->set_top_of_page( ).

*    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_noout( 'CHECK,SNDREQ' ).
    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    o_alv->show( ).


  ENDMETHOD.                    "

ENDCLASS.                    "REPORT IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT o_prog
    EXPORTING
      status        = 'INICIO_DYN'
      get_nombre_pc = 'X'
      no_param      = 'X'
      status_prog   = 'ZAP_STATUS'.

  CREATE OBJECT o_alv
    EXPORTING
      status             = 'STANDARD_ALV_DYN'
*     lights             = 'LIGHTS'
      top_of_page_auto   = 'X'
      top_of_page_titulo = 'X'
      status_prog        = 'ZAP_STATUS'.

  p_vari = o_alv->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  p_vari = o_alv->get_f4_layout( ).

AT SELECTION-SCREEN OUTPUT.

*  IF zcl_ap_autorizacion=>es_sapall( ) = ''.
*    REFRESH s_emisor.
*    __rango_eq s_emisor sy-uname.
*    zcl_ap_dynpro=>screen_input( group1 = 'NOE' variable = '' ).
*  ENDIF.

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  o_prog->at_selection( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  o_prog->main( ).
