***********************************************************************
* TIPO : LISTADO
* TITULO : ??
* DESCRIPCION : ??
*
* AUTOR: Andrés Picazo                                FECHA: 03/03/2017
* ANALISTA: ??
*
* Doc. Tecnica: http://sap4.com/tareas?=&cliente=CCC&objeto=OOO
**->MANUAL:http://sap4.com,http://sap4.com/edicion
**->PLANTILLA:,http://sap4.com
*
* MODIFICACIONES
* 03/03/2017 Tarea inicial: http://sap4.com/tareas?&ntask=TTT
*
***********************************************************************
REPORT zplantilla_alv8d.

*------INCLUDES--------------------------------------------------------*

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: vbrk, vbrp, t001.

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

ENDCLASS. "lcl_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_listado,
             check     TYPE xfeld,
             lights    TYPE zico_estado_mensaje,
             clave(20),

             message   TYPE bapi_msg,
           END OF t_listado,
           tt_listado TYPE TABLE OF t_listado.
    DATA: i_listado TYPE tt_listado,
          l_listado TYPE t_listado.

    TYPES: BEGIN OF t_detalle,
             check     TYPE xfeld,
             clave(20),
           END OF t_detalle,
           tt_detalle TYPE TABLE OF t_detalle.
    DATA: i_detalle TYPE tt_detalle,
          l_detalle TYPE t_detalle.
    DATA: i_detalle_todo TYPE tt_detalle,
          v_detalle_init.

    METHODS: main.

    METHODS:  listado,
      listado_detalle IMPORTING clave TYPE any OPTIONAL,
      agrupar_datos,
      seleccionar_datos.

ENDCLASS.                    "REPORT DEFINITION


*------VARIABLES-------------------------------------------------------*
DATA: o_prog TYPE REF TO zcl_report,
      o_alv  TYPE REF TO lcl_alv,
      o_alvd TYPE REF TO lcl_alv.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE text-sel.
SELECT-OPTIONS: s_vbeln FOR vbrk-vbeln.
SELECTION-SCREEN: SKIP 1.
PARAMETERS: p_list RADIOBUTTON GROUP g USER-COMMAND g,
            p_deta RADIOBUTTON GROUP g.
SELECTION-SCREEN: SKIP 1.
PARAMETERS: p_varil LIKE disvariant-variant MODIF ID lis,
            p_varid LIKE disvariant-variant MODIF ID det.
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
    IF nombre_tabla CS 'LISTADO'.
      READ TABLE o_prog->i_listado INTO o_prog->l_listado INDEX row.
      IF sy-subrc = 0.
        o_prog->listado_detalle( o_prog->l_listado-clave ).
      ENDIF.
    ELSE.
      READ TABLE o_prog->i_detalle INTO o_prog->l_detalle INDEX row.
      IF sy-subrc = 0.
      ENDIF.
    ENDIF.
  ENDMETHOD. "handle_double_click
  METHOD handle_user_command.
    DATA: l_row TYPE i.
    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    CASE e_salv_function.
      WHEN 'EXCEL'.
        exportar_excel( ).
      WHEN 'CREAR'.
        get_seleccion( CHANGING t_tabla = o_prog->i_listado ).
        LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
          <listado>-lights = zcl_ap_alv=>c_ico_rojo.
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

    IF p_list = 'X'.
      agrupar_datos( ).

      listado( ).
    ELSE.
      listado_detalle( ).
    ENDIF.

  ENDMETHOD.                    "REPORT

  METHOD seleccionar_datos.
    FIELD-SYMBOLS <detalle> TYPE t_detalle.

    sgpi_texto( 'Seleccionando datos' ).

    o_prog->o_sgpi->get_filas_tabla( i_listado[] ).
    LOOP AT i_detalle ASSIGNING <detalle>.
      sgpi_texto( texto1 = 'Procesando datos' cant_porc = 100 ).


    ENDLOOP.

    i_detalle_todo = i_detalle.

  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.

    sgpi_texto( 'Generando informe' ).

    o_alv->set_layout( p_varil ).

    o_alv->set_top_of_page( ).

    o_alv->set_field_noout( 'CLAVE' ).
    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).

    o_alv->show( ).


  ENDMETHOD.                    "listado

  METHOD listado_detalle.
    FIELD-SYMBOLS <detalle> TYPE t_detalle.

    sgpi_texto( 'Generando informe' ).

    CLEAR i_detalle.
    IF clave IS INITIAL.
      i_detalle = i_detalle_todo.
    ELSE.
      LOOP AT i_detalle_todo ASSIGNING <detalle> WHERE clave = clave.
        APPEND <detalle> TO i_detalle.
      ENDLOOP.
    ENDIF.

    IF v_detalle_init IS INITIAL.
      v_detalle_init = 'X'.
      o_alvd->set_layout( p_varid ).
      o_alvd->set_top_of_page( ).

      o_alvd->set_field_noout( 'CLAVE' ).
    ENDIF.
    o_alvd->show( ).


  ENDMETHOD.                    "

  METHOD agrupar_datos.
    FIELD-SYMBOLS: <detalle> TYPE t_detalle,
                   <listado> TYPE t_listado.

    sgpi_texto( 'Agrupando datos' ).
    CLEAR i_listado.
    LOOP AT i_detalle_todo ASSIGNING <detalle>.
      CLEAR l_listado.
      MOVE-CORRESPONDING <detalle> TO l_listado.
      COLLECT l_listado INTO i_listado.
    ENDLOOP.

    LOOP AT i_listado ASSIGNING <listado>.
      IF <listado>-message IS INITIAL.
        <listado>-lights = zcl_ap_alv=>set_icono( icono = zcl_ap_alv=>c_ico_verde mensaje = <listado>-message ).
      ELSE.
        <listado>-lights = zcl_ap_alv=>set_icono( icono = zcl_ap_alv=>c_ico_rojo mensaje = <listado>-message ).
      ENDIF.
    ENDLOOP.


  ENDMETHOD.                    "agrupar_datos

ENDCLASS.                    "REPORT IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT o_prog
    EXPORTING
      status        = 'INICIO'
      get_nombre_pc = 'X'
      no_param      = 'X'.


  CREATE OBJECT o_alv
    EXPORTING
      status             = 'STANDARD'
*     lights             = 'LIGHTS'
      top_of_page_auto   = 'X'
      top_of_page_titulo = 'X'.

  p_varil = o_alv->get_default_layout( ).

  CREATE OBJECT o_alvd
    EXPORTING
      status             = 'STANDARD'
      tabla              = 'I_DETALLE'
      top_of_page_auto   = 'X'
      top_of_page_titulo = 'X'.

  p_varid = o_alvd->get_default_layout( ).


  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).
  zcl_ap_dynpro=>set_primer_radiobutton( campos = 'P_LIST,P_DETA' ).

AT SELECTION-SCREEN OUTPUT.

  zcl_ap_dynpro=>screen_visible( group1 = 'LIS' variable = p_list ).
  zcl_ap_dynpro=>screen_visible( group1 = 'DET' variable = p_deta ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varil.
  p_varil = o_alv->get_f4_layout( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varid.
  p_varid = o_alvd->get_f4_layout( ).

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
