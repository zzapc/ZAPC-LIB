***********************************************************************
* TIPO : LISTADO
* TITULO : Listado modificaciones desarrollos
* DESCRIPCION : Listado modificaciones desarrollos
*
* AUTOR: Andrés Picazo                                FECHA: 25/09/2017
*
* Doc. Tecnica: http://sap4.com/tareas?=&cliente=CCC&objeto=OOO
**->MANUAL:http://sap4.com,http://sap4.com/edicion
**->PLANTILLA:,http://sap4.com
*
* MODIFICACIONES
* 18/04/2017 Tarea inicial: http://sap4.com/tareas?&ntask=TTT
*
***********************************************************************
REPORT zap_dev_list.

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: zap_dev_est, ztemp.

*------TABLAS INTERNAS-------------------------------------------------*
DATA: v_fechas TYPE string.

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
             check   TYPE xfeld,
             lights  TYPE zico_estado_mensaje,
             cliente TYPE string,
             sysid   TYPE sy-sysid,
             ernam   TYPE ernam , " Creado por
             erdat   TYPE erdat , " Creado el
             objeto  TYPE zap_dev-objeto,
             tipo    TYPE zap_dev-tipo,
             text    TYPE trdirt-text,
             valor1  TYPE text20 , " Denominación
             valor2  TYPE text20 , " Denominación
             valor3  TYPE text20 , " Denominación
             texto   TYPE text255 , " Texto
             string  TYPE zstring , " String
             trkorr  TYPE trkorr , " Orden/Tarea
             message TYPE bapi_msg,
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
SELECT-OPTIONS: s_ernam FOR ztemp-ernam,
                s_erdat FOR ztemp-erdat.

SELECTION-SCREEN: SKIP 1.
PARAMETERS: p_mail TYPE ztemp-texto2.
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
    ENDIF.
  ENDMETHOD. "handle_double_click
  METHOD handle_user_command.
    DATA: l_row     TYPE i,
          l_fichero TYPE string,
          l_ruta    TYPE string.
    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    CASE e_salv_function.
      WHEN 'EXCEL'.
*        exportar_excel( ).
        exportar_xlsx( fichero = 'desarrollos.xlsx' abrir_excel = 'X' dir_temp = 'X' ).
      WHEN 'F01'.
        l_ruta = zcl_ap_ficheros=>buscar_ruta_fichero( rutas =
           'C:\Users\Andres\Google Drive\LIB\,D:\GD_LIB\LIB\' ).


        __concat2 l_fichero zcl_c=>cliente_tasks sy-sysid.
        CONCATENATE l_fichero sy-datum v_fechas INTO l_fichero SEPARATED BY space.

        IF NOT l_ruta IS INITIAL.
          DATA: v_aux1 TYPE string, v_aux2 TYPE string.
          SPLIT zcl_c=>ruta_descarga_slnk AT '\CODIGO\Reports slnk\' INTO v_aux1 v_aux2.
          CONCATENATE '\CODIGO\Reports slnk\' v_aux2 INTO v_aux1.
          REPLACE '\LIB\' IN l_ruta WITH v_aux1.
          CONCATENATE l_ruta l_fichero INTO l_fichero.
        ENDIF.
        exportar_xlsx( fichero = l_fichero abrir_excel = 'X'   ).
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

    CHECK NOT i_listado[] IS INITIAL.

    listado( ).

  ENDMETHOD.                    "REPORT

  METHOD seleccionar_datos.
    FIELD-SYMBOLS <listado> TYPE t_listado.

    sgpi_texto( 'Seleccionando datos' ).
    SELECT * FROM zap_dev                               "#EC CI_NOFIRST
      INTO CORRESPONDING FIELDS OF TABLE i_listado
     WHERE ernam IN s_ernam
       AND erdat IN s_erdat.

    o_prog->o_sgpi->get_filas_tabla( i_listado[] ).
    LOOP AT i_listado ASSIGNING <listado>.
      sgpi_texto( texto1 = 'Procesando datos' cant_porc = 100 ).

      PERFORM get_text IN PROGRAM zap_dev using o_cache CHANGING <listado>-tipo <listado>-objeto <listado>-text.

      set_status_list( EXPORTING message = <listado>-message criterio = 'V' CHANGING list = <listado> ).
    ENDLOOP.

  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.
    DATA lr_erdat LIKE LINE OF s_erdat.

    sgpi_texto( 'Generando informe' ).

    o_alv->set_layout( p_vari ).

    o_alv->set_top_of_page( ).
    o_alv->set_field_text( campo = 'STRING' valor = 'Comentarios' ).

    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_noout( 'CHECK' ).
    o_alv->set_orden( campo = 'ERDAT' down = 'X' ).
    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    IF p_mail IS INITIAL.
      o_alv->show( ).
    ELSE.
      SET PARAMETER ID 'ZMAIL' FIELD 'X'.                   "#EC EXISTS
      IF s_erdat[] IS INITIAL.
        zcl_ap_envio_mail=>mail( EXPORTING subject = 'Modificaciones desarrollos' direccion = p_mail conversion_sap = 'X' CHANGING i_tabla = i_listado ).
      ELSE.
        __concat2 string 'Modificaciones desarrollos' v_fechas.
        zcl_ap_envio_mail=>mail( EXPORTING subject = string direccion = p_mail  o_alv_origen = o_alv nombre_fichero = 'Desarrollos.xls' CHANGING i_tabla = i_listado ).
      ENDIF.
    ENDIF.


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
      status_prog   = 'ZAP_STATUS'.

  CREATE OBJECT o_alv
    EXPORTING
      status             = 'STANDARD_ALV_DYN'
      top_of_page_auto   = 'X'
      top_of_page_titulo = 'X'
      status_prog        = 'ZAP_STATUS'.

  o_alv->add_button( button = 'F01' text = 'Exportación'  icon = icon_xls qinfo = 'Exportación datos' ).
*  o_alv->add_button( button = 'M01' text = 'LOG'  icon = '@15@' qinfo = 'Log' ).

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

  v_fechas = zcl_ap_fechas=>rango2string( s_erdat[] ).


  o_prog->main( ).
