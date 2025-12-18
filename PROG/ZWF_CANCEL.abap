***********************************************************************
* TIPO : LISTADO
* TITULO : Chequeo WF finalizados
* DESCRIPCION : Chequeo WF finalizados
*
* AUTOR: Andrés Picazo                                FECHA: 26/05/2017
*
***********************************************************************
REPORT zZWF_CANCEL.

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: swwuserwi, swwwihead.

*------TABLAS INTERNAS-------------------------------------------------*
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
             check      TYPE xfeld,
             lights     TYPE zico_estado_mensaje,
             user_id    TYPE xubname,
             nombre     TYPE adrp-name_text,
             wi_id      TYPE sww_wiid,
             task_obj   TYPE otjid,
             wi_rhtext  TYPE swwwihead-wi_text,
             wi_text    TYPE swwwihead-wi_text,
             wi_cd      TYPE swwwihead-wi_cd,
             wi_ct      TYPE swwwihead-wi_ct,
             wi_stat    TYPE swwwihead-wi_stat,
             top_task   TYPE swwwihead-top_task,
             instid     TYPE sww_wi2obj-instid,
             typeid     TYPE sww_wi2obj-typeid,
             pernr      TYPE persno,
             ldate      TYPE ldate,
             accion(20),
             analisis   TYPE string,
             message    TYPE bapi_msg,
           END OF t_listado,
           tt_listado TYPE TABLE OF t_listado.
    DATA: i_listado           TYPE tt_listado,
          l_listado           TYPE t_listado,
          acciones_realizadas TYPE i.

    METHODS: main.

    METHODS:  listado,
      seleccionar_datos,
      ejecutar,
      cancelar.

ENDCLASS.                    "REPORT DEFINITION

*------VARIABLES-------------------------------------------------------*
DATA: o_prog TYPE REF TO zcl_report,
      o_alv  TYPE REF TO lcl_alv.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-sel.
  SELECT-OPTIONS: s_fecha FOR sy-datum DEFAULT sy-datum,
                  s_uname FOR sy-uname,
                  s_wi_id FOR swwwihead-wi_id,
                  s_task  FOR swwuserwi-task_obj,
                  s_status FOR swwwihead-wi_stat DEFAULT 'STARTED',
                  s_ttask FOR swwwihead-top_task DEFAULT 'TS9*' OPTION CP.
  SELECTION-SCREEN: SKIP 1.
  PARAMETERS: p_auto AS CHECKBOX.
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
    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    READ TABLE o_prog->i_listado ASSIGNING <listado> INDEX row.
    IF sy-subrc = 0.
      zcl_ap_wf=>visualizar( <listado>-wi_id ).
    ENDIF.
  ENDMETHOD. "handle_double_click
  METHOD handle_user_command.
    DATA: l_row         TYPE i,
          l_return_code TYPE sy-subrc,
          l_new_status  TYPE  sww_wistat.

    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    CASE e_salv_function.
      WHEN 'EXCEL'.
        exportar_excel( ).
      WHEN 'BORRAR'.
        get_seleccion( CHANGING t_tabla = o_prog->i_listado ).
        READ TABLE o_prog->i_listado TRANSPORTING NO FIELDS WITH KEY check = 'X'.
        IF sy-subrc NE 0.
          MESSAGE 'Seleccione algún registro' TYPE 'I'.
        ELSE.
          o_prog->ejecutar( ).
          o_alv->refresh( ).
        ENDIF.
      WHEN 'CANCELAR'.
        get_seleccion( CHANGING t_tabla = o_prog->i_listado ).
        READ TABLE o_prog->i_listado TRANSPORTING NO FIELDS WITH KEY check = 'X'.
        IF sy-subrc NE 0.
          MESSAGE 'Seleccione algún registro' TYPE 'I'.
        ELSE.
          o_prog->cancelar( ).
          o_alv->refresh( ).
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

    IF p_auto = 'X'.
      ejecutar( ).
    ENDIF.

    IF p_auto IS INITIAL OR acciones_realizadas > 0.
      listado( ).
    ENDIF.

  ENDMETHOD.                    "REPORT

  METHOD ejecutar.
    DATA: l_row          TYPE i,
          l_return_code  TYPE sy-subrc,
          l_new_status   TYPE  sww_wistat,
          l_new_status_c TYPE  swr_wistat.

    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X' AND accion NE ''.
      acciones_realizadas += 1.

      CASE <listado>-accion.
        WHEN 'CANCELAR'.
          CALL FUNCTION 'SAP_WAPI_ADM_WORKFLOW_CANCEL'
            EXPORTING
              workitem_id = <listado>-wi_id
            IMPORTING
              return_code = l_return_code
              new_status  = l_new_status_c.

          l_new_status = l_new_status_c-status.
        WHEN 'FINALIZAR'.
          CALL FUNCTION 'SAP_WAPI_WORKITEM_COMPLETE'
            EXPORTING
              workitem_id = <listado>-wi_id
*             ACTUAL_AGENT                    = SY-UNAME
*             LANGUAGE    = SY-LANGU
*             SET_OBSOLET = ' '
*             DO_COMMIT   = 'X'
*             DO_CALLBACK_IN_BACKGROUND       = 'X'
*             IFS_XML_CONTAINER               =
*             CHECK_INBOX_RESTRICTION         = ' '
            IMPORTING
              return_code = l_return_code
              new_status  = l_new_status.
*           TABLES
*             SIMPLE_CONTAINER                =
*             MESSAGE_LINES                   =
*             MESSAGE_STRUCT                  =
          .

      ENDCASE.

      IF l_return_code NE 0.
        __concat2 <listado>-message 'Error' l_return_code.
        message( p1 = 'Error tratando WI' p2 = <listado>-wi_id p3 = 'de usuario' p4 = <listado>-user_id p5 = <listado>-typeid p6 = <listado>-instid solo_log = 'X' ).
        set_status_list_stop( CHANGING list = <listado> ).
      ELSE.
        commit WORK AND WAIT.
        __concat2 <listado>-message 'Se ha pasado a nuevo estado' l_new_status.
        set_status_list( EXPORTING color = 'V' icono = zcl_ap_alv=>c_ico_verde CHANGING list = <listado> ).
        message( p1 = 'Se ha marcado como completo WI' p2 = <listado>-wi_id p3 = 'de usuario' p4 = <listado>-user_id p5 = <listado>-typeid p6 = <listado>-instid solo_log = 'X' type = 'S' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD cancelar.
    DATA: l_row         TYPE i,
          l_return_code TYPE sy-subrc,
          l_new_status  TYPE  swr_wistat.

    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
      CALL FUNCTION 'SAP_WAPI_ADM_WORKFLOW_CANCEL'
        EXPORTING
          workitem_id = <listado>-wi_id
*         ACTUAL_AGENT         = SY-UNAME
*         LANGUAGE    = SY-LANGU
*         DO_COMMIT   = 'X'
        IMPORTING
          return_code = l_return_code
          new_status  = l_new_status.
* TABLES
*   MESSAGE_LINES        =
*   MESSAGE_STRUCT       =
      .

      .

      IF l_return_code NE 0.
        __concat2 <listado>-message 'Error' l_return_code.
        message( p1 = 'Error tratando WI' p2 = <listado>-wi_id p3 = 'de usuario' p4 = <listado>-user_id p5 = <listado>-typeid p6 = <listado>-instid solo_log = 'X' ).
        set_status_list_stop( CHANGING list = <listado> ).
      ELSE.
        __concat2 <listado>-message 'Se ha pasado a nuevo estado' l_new_status.
        set_status_list( EXPORTING color = 'V' icono = zcl_ap_alv=>c_ico_verde CHANGING list = <listado> ).
        message( p1 = 'Se ha marcado como completo WI' p2 = <listado>-wi_id p3 = 'de usuario' p4 = <listado>-user_id p5 = <listado>-typeid p6 = <listado>-instid solo_log = 'X' type = 'S' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD seleccionar_datos.
    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.
    DATA: rbkp   TYPE rbkp,
          l_dias TYPE i.

    sgpi_texto( 'Seleccionando datos' ).
    IF s_uname[] IS INITIAL.
      SELECT * FROM swwwihead
     INTO CORRESPONDING FIELDS OF TABLE i_listado
    WHERE wi_id IN s_wi_id
      AND top_task IN s_ttask
     AND wi_stat IN s_status
      AND wi_cd IN s_fecha.
    ELSE.
      SELECT * FROM swwuserwi JOIN swwwihead ON swwuserwi~wi_id = swwwihead~wi_id
        INTO CORRESPONDING FIELDS OF TABLE i_listado
       WHERE swwwihead~wi_id IN s_wi_id
         AND user_id  IN s_uname
         AND task_obj IN s_task
         AND top_task IN s_ttask
        AND wi_stat IN s_status
         AND no_sel = ''
         AND wi_cd IN s_fecha.
    ENDIF.

    o_prog->o_sgpi->get_filas_tabla( i_listado[] ).
    LOOP AT i_listado ASSIGNING <listado>.
      sgpi_texto( texto1 = 'Procesando datos' cant_porc = 100 ).

      <listado>-nombre = zcl_ap_usuario=>get_nombre( <listado>-user_id ).

      SELECT SINGLE instid typeid FROM sww_wi2obj
        INTO (<listado>-instid, <listado>-typeid)
       WHERE wi_id = <listado>-wi_id.

      IF <listado>-wi_stat = 'STARTED' AND <listado>-top_task(3) = 'TS9'.
        IF s_wi_id[] IS INITIAL.
          DATA(l_segundos) = zcl_ap_fechas=>get_duracion_intervalo_sec( fini = <listado>-wi_cd
                                                                        hini = <listado>-wi_ct
                                                                        ffin = sy-datum
                                                                        hfin = sy-uzeit ).
          IF l_segundos > 300.
            <listado>-accion = 'FINALIZAR'.
          ENDIF.
        ELSE.
          <listado>-accion = 'FINALIZAR'.
        ENDIF.
      ENDIF.


      IF NOT <listado>-accion IS INITIAL.
        <listado>-check = p_auto.
        set_status_list( EXPORTING icono = zcl_ap_alv=>c_sem_ambar CHANGING list = <listado> ).
      ELSE.
        IF p_auto = 'X'.
          DELETE i_listado.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.

    sgpi_texto( 'Generando informe' ).

    o_alv->set_layout( p_vari ).

    o_alv->set_top_of_page( ).

    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_noout( 'CHECK' ).
    o_alv->set_field_text( 'ANALISIS,ACCION' ).
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
      status        = 'INICIO'
      get_nombre_pc = 'X'
      no_param      = 'X'
      guardar_logz  = 'X'.

  CREATE OBJECT o_alv
    EXPORTING
      status             = 'STANDARD'
      top_of_page_auto   = 'X'
      top_of_page_titulo = 'X'.

  p_vari = o_alv->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  p_vari = o_alv->get_f4_layout( ).

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
