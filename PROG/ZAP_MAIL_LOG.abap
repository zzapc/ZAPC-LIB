***********************************************************************
* TIPO : LISTADO
* TITULO : Log mail enviados
* DESCRIPCION : Log mail enviados
*
* AUTOR: Andrés Picazo                                FECHA: 20/01/2020
*
***********************************************************************
REPORT zap_mail_log.

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: zap_mail_log.

*------TABLAS INTERNAS-------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check FINAL.
  PUBLIC SECTION.
    METHODS: handle_user_command REDEFINITION.
    METHODS: visualizar_objeto REDEFINITION.
ENDCLASS.                    "lcl_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_listado,
             check   TYPE xfeld,
             lights  TYPE zico_estado_mensaje,
             fecha   TYPE oia_ship , " Fecha envío
             hora    TYPE so_tim_ou , " Hora salida
             clave   TYPE objkey , " Clave de objeto
             destino TYPE text128,
             grupo   TYPE group , " Grupo características
             codigo  TYPE zap_mail_log-codigo,
             version TYPE finb_tr_version , " Versión
             spras   TYPE spras , " Clave de idioma
             html    TYPE mgwpicturehtml , " Viewer HTML
             asunto  TYPE text255 , " Texto
             emisor  TYPE text128,
             copia   TYPE text128,
             message TYPE bapi_msg , " Texto de mensaje
             status  TYPE zap_mail_log-status,
             tcode   TYPE zap_mail_log-tcode,
             cprog   TYPE zap_mail_log-cprog,
           END OF t_listado,
           tt_listado TYPE STANDARD TABLE OF t_listado.
    DATA: i_listado TYPE tt_listado,
          o_alv     TYPE REF TO lcl_alv.                    "#EC NEEDED

    METHODS: main.

    METHODS:  listado,
      seleccionar_datos,
      get_info IMPORTING list        TYPE t_listado
                         display     TYPE abap_bool DEFAULT ''
               RETURNING VALUE(info) TYPE soxsp2.

ENDCLASS.                    "REPORT DEFINITION

*------VARIABLES-------------------------------------------------------*
DATA: o_prog TYPE REF TO zcl_report.


*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-sel.
SELECT-OPTIONS: s_grupo  FOR zap_mail_log-grupo,
                s_codigo FOR zap_mail_log-codigo,
                s_clave  FOR zap_mail_log-clave,
                s_fecha  FOR zap_mail_log-fecha DEFAULT sy-datum,
                s_hora   FOR zap_mail_log-hora,
                s_cprog  FOR zap_mail_log-cprog.
SELECTION-SCREEN: SKIP 1.
PARAMETERS p_ampli AS CHECKBOX.
SELECTION-SCREEN: SKIP 1.
PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b01.
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

  METHOD visualizar_objeto.
    DATA l_list TYPE o_prog->t_listado.
    l_list = list.


    CASE column.
      WHEN OTHERS.
        o_prog->get_info( list = l_list display = 'X' ).
    ENDCASE.
  ENDMETHOD. "handle_double_click


  METHOD handle_user_command.
    check_ucomm_sel = 'F01'.

    super->handle_user_command( e_salv_function ).

    CASE ucomm.
      WHEN 'F01'.
        LOOP AT o_prog->i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE check = 'X'. "#EC NEEDED
* TO DO!
        ENDLOOP.
        IF sy-subrc = 0.
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

    sgpi_texto( 'Seleccionando datos'(sda) ).
    SELECT * FROM zap_mail_log
      INTO CORRESPONDING FIELDS OF TABLE i_listado
     WHERE grupo IN s_grupo
       AND codigo IN s_codigo
       AND clave IN s_clave
       AND fecha IN s_fecha
       AND hora IN s_hora
       AND cprog IN s_cprog.

    o_prog->o_sgpi->get_filas_tabla( i_listado[] ).
    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
      sgpi_texto( texto1 = 'Procesando datos'(pda) cant_porc = 100 ).

      IF <listado>-destino CS '@' AND <listado>-status NE 'S'.
        DATA(info) = get_info( list = <listado> ).
        IF NOT info-msgid IS INITIAL AND info-status CO ' 0123456789'.
          MESSAGE ID info-msgid TYPE 'S' NUMBER info-status WITH info-msgv1 info-msgv2 info-msgv3 info-msgv4 INTO <listado>-message.
        ENDIF.
        UPDATE zap_mail_log
          SET message  = <listado>-message
              status   = info-status
        WHERE fecha    = <listado>-fecha
          AND hora     = <listado>-hora
          AND clave    = <listado>-clave
          AND destino  = <listado>-destino.
      ENDIF.

      IF <listado>-status = 'W'.
        <listado>-message = 'Mail en cola de salida'.
        set_status_list( EXPORTING icono = icon_yellow_light CHANGING list = <listado> ).
      ELSE.
        REPLACE ALL OCCURRENCES OF '@' IN <listado>-message WITH '#'.
        set_status_list( EXPORTING message = <listado>-message icono = icon_green_light CHANGING list = <listado> ).
      ENDIF.
    ENDLOOP.

    SORT i_listado BY fecha hora grupo codigo.

  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.

    sgpi_texto( 'Generando informe'(gin) ).

*    o_alv->add_button( button = 'F01' text = 'Ejecutar'(eje)  icon = icon_execute_object ).
*    o_alv->add_button( button = 'F01' text = 'Excel'  icon = icon_xls ).

    o_alv->set_layout( p_vari ).
    o_alv->get_datos_layout( EXPORTING reordenar_tabla = 'X' CHANGING t_tabla = i_listado ).
    o_alv->set_top_of_page( ).

    IF p_ampli IS INITIAL.
      o_alv->set_field_noout( 'GRUPO,CODIGO,VERSION,SPRAS,HTML,STATUS,TCODE,CPROG' ).
    ENDIF.

    o_alv->set_field_text( 'DESTINO,CODIGO,ASUNTO,EMISOR,COPIA' ).
    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_quitar( 'CHECK' ).
    o_alv->set_field_quitar( 'MESSAGE' ).
    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    o_alv->show( ).


  ENDMETHOD.                    "

  METHOD get_info.
    DATA: r_hora    TYPE RANGE OF sy-uzeit,
          r_fecha   TYPE RANGE OF sy-datum,
          lr_hora   LIKE LINE OF r_hora,
          i_sndrecs TYPE soxsp2tab.

    APPEND VALUE #( option = 'EQ' sign = 'I' low = list-fecha ) TO r_fecha.
    lr_hora-option = 'BT'.
    lr_hora-sign = 'I'.
    lr_hora-low = list-hora.
    lr_hora-high = list-hora + 10.
    APPEND lr_hora TO r_hora.

    DATA:  l_status  TYPE  soststatus.
    l_status = 'XXXXXXXX '.
    CLEAR: i_sndrecs.
    CALL FUNCTION 'SX_SNDREC_SELECT'
      EXPORTING
*       SND_ART       =
        snd_date      = r_fecha
        snd_time      = r_hora[]
*       DEL_DATE      =
*       DEL_TIME      =
        status        = l_status
        notifications = 'X'
*       MAXSEL        =
        all_waiting   = 'X'
      IMPORTING
        sndrecs       = i_sndrecs.
    DELETE i_sndrecs WHERE titel NE list-asunto.
    IF NOT i_sndrecs IS INITIAL.
      READ TABLE i_sndrecs INTO info INDEX 1.
      IF display = 'X'.
        TRY.
            cl_sndrec_bcs=>display( i_sndrecs ).
          CATCH cx_bcs INTO DATA(lx_bcs).
            MESSAGE ID lx_bcs->msgid TYPE 'S' NUMBER lx_bcs->msgno
              WITH TEXT-018 space space space.
        ENDTRY.
      ENDIF.
    ENDIF.
  ENDMETHOD.



ENDCLASS.                    "REPORT IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  o_prog = NEW #( status       = 'INICIO_DYN'
                  status_prog  = 'ZAP_STATUS'
                  no_param     = 'X'
                  guardar_logz = '' ).

*  PERFORM add_button IN PROGRAM zap_status USING 'M01' 'Log'(log) '' ''.

  o_prog->o_alv =  NEW #( status             = 'STANDARD_ALV_DYN'
                          status_prog        = 'ZAP_STATUS'
                          top_of_page_auto   = 'X'
                          top_of_page_titulo = 'X'
                          o_dev              = o_prog ).


  p_vari = o_prog->o_alv->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  p_vari = o_prog->o_alv->get_f4_layout( ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'ONLI'.
      o_prog->validar_seleccion_obligatoria( campos_or = '*' msgty = 'W' ).
    WHEN OTHERS.
      o_prog->at_selection( ).
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  o_prog->main( ).
