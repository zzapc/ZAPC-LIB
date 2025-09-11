***********************************************************************
* TIPO : LISTADO
* TITULO : ??
* DESCRIPCION : ??
*
* AUTOR: Andrés Picazo                                FECHA: 03/03/2017
* ANALISTA: ??
*
***********************************************************************
REPORT zplantilla_alv2d.

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
    METHODS: visualizar_objeto REDEFINITION.
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
             check   TYPE xfeld,
             lights  TYPE zico_estado_mensaje,
             clave   TYPE string,

             message TYPE bapi_msg,
           END OF t_listado,
           tt_listado TYPE TABLE OF t_listado.
    DATA: i_listado TYPE tt_listado.

    TYPES: BEGIN OF t_detalle,
             check TYPE xfeld,
             clave TYPE string,
           END OF t_detalle,
           tt_detalle TYPE TABLE OF t_detalle.
    DATA: i_detalle TYPE tt_detalle.
    DATA: i_detalle_todo TYPE tt_detalle,
          v_detalle_init.

    DATA: o_alv  TYPE REF TO lcl_alv,
          o_alvd TYPE REF TO lcl_alv.

    METHODS: main.

    METHODS:  listado,
      listado_detalle IMPORTING clave TYPE any OPTIONAL,
      agrupar_datos,
      seleccionar_datos.

ENDCLASS.                    "REPORT DEFINITION


*------VARIABLES-------------------------------------------------------*
DATA: o_prog TYPE REF TO zcl_report.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-sel.
SELECT-OPTIONS: s_vbeln FOR vbrk-vbeln.
SELECTION-SCREEN: SKIP 1.
PARAMETERS: p_list RADIOBUTTON GROUP g USER-COMMAND g DEFAULT 'X',
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
  METHOD visualizar_objeto.
    DATA: l_list TYPE o_prog->t_listado,
          l_deta TYPE o_prog->t_detalle.


    IF nombre_tabla CS 'LISTADO'.
      l_list = list.
      o_prog->listado_detalle( l_list-clave ).
    ELSE.
      l_deta = list.

    ENDIF.
  ENDMETHOD. "handle_double_click
  METHOD handle_user_command.
    check_ucomm_sel = 'PROCESAR'.

    super->handle_user_command( e_salv_function ).


    CASE ucomm.
      WHEN 'PROCESAR'.
        get_seleccion( CHANGING t_tabla = o_prog->i_listado ).
        LOOP AT o_prog->i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE check = 'X'. "#EC CI_STDSEQ
          <listado>-lights = zcl_ap_alv=>c_ico_rojo.
        ENDLOOP.
        IF sy-subrc NE 0.
          MESSAGE i104(dlcn). "Seleccione por lo menos un registro
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

    sgpi_texto( 'Seleccionando datos' ).

    o_prog->o_sgpi->get_filas_tabla( i_detalle[] ).
    LOOP AT i_detalle ASSIGNING FIELD-SYMBOL(<detalle>).
      sgpi_texto( texto1 = 'Procesando datos' cant_porc = 100 ).

    ENDLOOP.

    i_detalle_todo = i_detalle.

  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.

    sgpi_texto( 'Generando informe' ).

    o_alv->add_button( button = 'F01' text = 'Procesar'  icon = icon_xls qinfo = 'Procesar' ucomm = 'PROCESAR' ).

    o_alv->set_layout( p_varil ).
    o_alv->get_datos_layout( EXPORTING reordenar_tabla = 'X' CHANGING t_tabla = i_listado ).
    o_alv->set_top_of_page( ).

    o_alv->set_field_quitar( 'CLAVE,CHECK' ).
    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).

    o_alv->show( ).


  ENDMETHOD.                    "listado

  METHOD listado_detalle.


    sgpi_texto( 'Generando informe' ).
    o_alvd->add_button( button = 'F01' text = 'Excel'  icon = icon_xls qinfo = 'Generar Excel' ).

    CLEAR i_detalle.
    IF clave IS INITIAL.
      i_detalle = i_detalle_todo.
    ELSE.
      LOOP AT i_detalle_todo ASSIGNING FIELD-SYMBOL(<detalle>) WHERE clave = clave. "#EC CI_STDSEQ
        APPEND <detalle> TO i_detalle.
      ENDLOOP.
    ENDIF.

    IF v_detalle_init IS INITIAL.
      v_detalle_init = 'X'.
      o_alvd->set_layout( p_varid ).
      o_alvd->get_datos_layout( EXPORTING reordenar_tabla = 'X' CHANGING t_tabla = i_detalle ).
      o_alvd->set_top_of_page( ).

      o_alvd->set_field_quitar( 'CLAVE,CHECK' ).
    ENDIF.
    o_alvd->show( ).


  ENDMETHOD.                    "

  METHOD agrupar_datos.
    DATA l_listado TYPE t_listado.

    sgpi_texto( 'Agrupando datos' ).
    CLEAR i_listado.
    LOOP AT i_detalle_todo ASSIGNING FIELD-SYMBOL(<detalle>).
      CLEAR l_listado.
      MOVE-CORRESPONDING <detalle> TO l_listado.
      COLLECT l_listado INTO i_listado.
    ENDLOOP.

    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
      set_status_list( EXPORTING message = <listado>-message criterio = 'V' CHANGING list = <listado> ).
    ENDLOOP.


  ENDMETHOD.                    "agrupar_datos

ENDCLASS.                    "REPORT IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  o_prog = NEW #( status   = 'INICIO_DYN' status_prog  = 'ZAP_STATUS'
                  no_param = 'X'          guardar_logz = '' ).

*  PERFORM add_button IN PROGRAM zap_status USING 'M01' 'Log' '' ''.

  o_prog->o_alv =  NEW #( status           = 'STANDARD_ALV_DYN' status_prog        = 'ZAP_STATUS'
                          top_of_page_auto = 'X'                top_of_page_titulo = 'X'  ).

  p_varil = o_prog->o_alv->get_default_layout( ).

  o_prog->o_alvd =  NEW #( status           = 'STANDARD_ALV_DYN' status_prog        = 'ZAP_STATUS'
                           tabla            = 'I_DETALLE'
                           top_of_page_auto = 'X'                top_of_page_titulo = 'X'  ).


  p_varid = o_prog->o_alvd->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).
  zcl_ap_dynpro=>set_primer_radiobutton( campos = 'P_LIST,P_DETA' ).

AT SELECTION-SCREEN OUTPUT.

  zcl_ap_dynpro=>screen_visible( group1 = 'LIS' variable = p_list ).
  zcl_ap_dynpro=>screen_visible( group1 = 'DET' variable = p_deta ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varil.
  p_varil = o_prog->o_alv->get_f4_layout( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varid.
  p_varid = o_prog->o_alvd->get_f4_layout( ).

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
