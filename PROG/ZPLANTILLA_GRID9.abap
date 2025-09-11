***********************************************************************
* TIPO : LISTADO
* TITULO : ??
* DESCRIPCION : ??
*
* AUTOR: Andrés Picazo                                FECHA: 18/04/2017
* ANALISTA: ??
*
* Doc. Tecnica: http://sap4.com/tareas?=&cliente=CCC&objeto=OOO
**->MANUAL:,http://sap4.com
**->PLANTILLA:,http://sap4.com
*
* MODIFICACIONES
* 18/04/2017 Tarea inicial: http://sap4.com/tareas?&ntask=TTT
*
***********************************************************************
REPORT zplantilla_grid9.


*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: vbrk.


*------TABLAS INTERNAS-------------------------------------------------*


*------VARIABLES-------------------------------------------------------*


*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*

CLASS lcl_event_grid DEFINITION INHERITING FROM zcl_ap_alv_grid_eventos FINAL.
  PUBLIC SECTION.
    METHODS: double_click REDEFINITION,
      data_changed REDEFINITION,
      data_changed_finished REDEFINITION,
      toolbar      REDEFINITION,
      user_command REDEFINITION.
ENDCLASS.                    "lcl_event_grid DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF  t_listado,
             check   TYPE xfeld,
             lights  TYPE zico_estado_mensaje,
             bukrs   TYPE t001-bukrs,
             message TYPE bapi_msg,
             style   TYPE lvc_t_styl,
             color   TYPE lvc_t_scol,
             tabix   TYPE sy-tabix,
           END OF t_listado,
           tt_listado TYPE STANDARD TABLE OF t_listado.
    DATA: i_listado     TYPE tt_listado,
          i_listado_ini TYPE tt_listado.

    DATA: o_alv   TYPE REF TO zcl_ap_alv_grid,
          o_event TYPE REF TO lcl_event_grid.

    METHODS:  buscar_datos REDEFINITION,
      validaciones IMPORTING mod TYPE abap_bool DEFAULT '' CHANGING listado TYPE t_listado, "#EC NEEDED
      status_dynpro_0100,
      command_dynpro_0100.

ENDCLASS.                    "REPORT DEFINITION

DATA: o_prog  TYPE REF TO zcl_report.                       "#EC NEEDED

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME.
SELECT-OPTIONS: s_vbeln FOR vbrk-vbeln.
SELECTION-SCREEN SKIP.
PARAMETERS: p_vari LIKE disvariant-variant.                ".
SELECTION-SCREEN END OF BLOCK b01.
__botones_plantilla.

************************************************************************
*
* LOGICA DEL PROGRAMA
*
************************************************************************
CLASS lcl_event_grid IMPLEMENTATION.

  METHOD double_click.
    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    READ TABLE o_prog->i_listado ASSIGNING <listado>  INDEX e_row-index.
    IF sy-subrc = 0.
      CASE e_column.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

  ENDMETHOD.                                               "double_click


  METHOD toolbar.

    super->toolbar( e_object = e_object e_interactive = e_interactive ).

  ENDMETHOD.                                               "toolbar

  METHOD user_command.

    CASE e_ucomm.
      WHEN OTHERS.
        super->user_command( e_ucomm = e_ucomm ).
    ENDCASE.

  ENDMETHOD.                                               "USER_COMMAND


  METHOD data_changed.
    DATA: l_listado TYPE o_prog->t_listado.

    ini_data_changed( cambios = er_data_changed->mt_good_cells  ).

    LOOP AT i_cambios_celda INTO cambio_celda.
      AT NEW row_id.
        CLEAR l_listado.
        READ TABLE o_prog->i_listado INTO l_listado INDEX cambio_celda-row_id. "#EC CI_SUBRC
      ENDAT.

      set_valor_mod( CHANGING datos = l_listado ).

      AT END OF row_id.
        o_prog->validaciones( EXPORTING mod = 'X' CHANGING listado = l_listado ).
        MODIFY o_prog->i_listado FROM l_listado INDEX cambio_celda-row_id.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.                                               "data_changed

  METHOD data_changed_finished.

    IF NOT tabla_data_changed IS INITIAL.
      o_alv->refrescar_grid( ).
      CLEAR tabla_data_changed.
    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "lcl_event_grid IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.

  METHOD buscar_datos.
    FIELD-SYMBOLS <listado> TYPE t_listado.

    sgpi_texto( 'Seleccionando datos'(sda) ).
    CLEAR i_listado.
    SELECT * FROM t001                                      "#EC WARNOK
      INTO CORRESPONDING FIELDS OF TABLE i_listado.

    o_prog->o_sgpi->get_filas_tabla( i_listado[] ).
    LOOP AT i_listado ASSIGNING <listado>.
      <listado>-tabix = sy-tabix.
      sgpi_texto( texto1 = 'Procesando datos'(pda) cant_porc = 100 ).

      validaciones( CHANGING listado = <listado> ).
    ENDLOOP.

    i_listado_ini = i_listado.

  ENDMETHOD.                                               "seleccionar_datos

  METHOD status_dynpro_0100.

    o_alv->registrar_mod( ).
    status_dynpro( EXPORTING o_alv = o_alv variant = p_vari style = 'STYLE' color = 'COLOR' cprog = 'ZAP_STATUS' status = 'ST_DYN' CHANGING i_listado = i_listado ).

*    status_dynpro( EXPORTING cprog = 'ZAP_STATUS' status = 'ST_DYN' CHANGING i_listado = i_listado ).
*    IF inicio IS INITIAL.
*      inicio = 'X'.
*      o_alv->variant-variant = p_vari.
*      o_alv->set_layout( no_rowmove = 'X' no_rowins = 'X' style = 'STYLE' colort = 'COLOR' ).
*      o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_refresh ).
*      o_alv->set_campos_tabint( i_listado[] ).
*
*      sgpi_texto( 'Generando informe' ).
*      o_alv->show( CHANGING tabla = i_listado ).
*      o_alv->actualiza_campos_grid( campos_borrar = 'CHECK,LIGHTS' ).
*      o_alv->set_seleccion( CHANGING t_tabla = i_listado ).
*    ENDIF.
  ENDMETHOD.


  METHOD command_dynpro_0100.
    DATA l_hay_sel.

    command_dynpro( EXPORTING o_alv = o_alv seleccion = 'F01'
                            CHANGING i_listado = i_listado i_listado_ini = i_listado_ini hay_sel = l_hay_sel ).

    IF ucomm = 'F01'.
      o_alv->exportar_xlsx( fichero = 'ALV.xlsx' abrir_excel = 'X' dir_temp = 'X' ).
    ENDIF.

  ENDMETHOD.


  METHOD validaciones.

    CLEAR: listado-message, listado-style, listado-color.

    set_status_list( EXPORTING message = listado-message criterio = 'V' CHANGING list = listado ).

  ENDMETHOD.

ENDCLASS.                    "REPORT IMPLEMENTATION
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT o_prog
    EXPORTING
      status       = 'INICIO'
      guardar_logz = 'X'
      status_prog  = 'ZAP_STATUS'.

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

  IF sy-batch IS INITIAL.
    CREATE OBJECT o_prog->o_event
      EXPORTING
        boton_refrescar = 'X'
        boton_excel     = 'X'
        o_prog          = o_prog.

    CREATE OBJECT o_prog->o_alv
      EXPORTING
        estructura = ''
        o_event    = o_prog->o_event.

    o_prog->o_alv->add_button( button = 'F01' text = 'Ejecutar'(eje)  icon = icon_execute_object ).

    p_vari = o_prog->o_alv->get_default_layout( ).
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  IF NOT o_prog->o_alv IS INITIAL.
    p_vari = o_prog->o_alv->get_f4_layout( ).
  ENDIF.

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.

  o_prog->at_selection( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).

AT SELECTION-SCREEN OUTPUT.
*  zcl_ap_dynpro=>screen_visible( group1 = 'PED' variable = p_pedid ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  o_prog->buscar_datos( ).

  IF sy-batch IS INITIAL.
    CALL SCREEN 0100.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  o_prog->status_dynpro_0100( ).

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  o_prog->command_dynpro_0100( ).


ENDMODULE.                 " USER_COMMAND_0100  INPUT
