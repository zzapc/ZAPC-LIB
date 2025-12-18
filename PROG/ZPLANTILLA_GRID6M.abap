***********************************************************************
* TIPO : MANTENIMIENTO
* TITULO : ??
* DESCRIPCION : ??
*
* AUTOR: Andrés Picazo                                FECHA: 16/09/2015
* ANALISTA: ??
*
* Doc. Tecnica: http://sap4.com/tareas?=&cliente=CCC&objeto=OOO
**->MANUAL:,http://sap4.com
**->PLANTILLA:,http://sap4.com
*
* MODIFICACIONES
* 16/09/2015 Tarea inicial: http://sap4.com/tareas?&ntask=TTT
*
***********************************************************************
REPORT zplantilla_grid6m.

*------INCLUDES--------------------------------------------------------*

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: ztemp.

*------CONSTANTES------------------------------------------------------*
CONSTANTS: c_campos_clave TYPE string VALUE 'CHECK,CLAVE,SUBCLAVE,INDICE'.

*------TABLAS INTERNAS-------------------------------------------------*
TYPES: t_listado TYPE ztemp_est.
__data_set_var listado.

DATA: i_listado_aux TYPE tt_listado,
      i_listado_ini TYPE tt_listado,
      i_listado_borrados TYPE tt_listado.

*------VARIABLES-------------------------------------------------------*
DATA: v_inicio.

*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_grid DEFINITION INHERITING FROM zcl_ap_alv_grid_eventos.
  PUBLIC SECTION.
    METHODS: double_click REDEFINITION,
             data_changed REDEFINITION,
             toolbar      REDEFINITION,
             user_command REDEFINITION.
ENDCLASS.                    "lcl_event_grid DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev.
  PUBLIC SECTION.
    METHODS:  buscar_datos REDEFINITION,
              verificar_datos.

ENDCLASS.                    "REPORT DEFINITION

DATA: o_alv TYPE REF TO zcl_ap_alv_grid,
      o_event TYPE REF TO lcl_event_grid,
      o_prog TYPE REF TO zcl_report.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME.
SELECT-OPTIONS: s_clave FOR ztemp-clave,
                s_subcl FOR ztemp-subclave.
SELECTION-SCREEN SKIP.
PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK 001.
PARAMETER: p_defpa DEFAULT 'X' NO-DISPLAY.
__botones_plantilla.

************************************************************************
*
* LOGICA DEL PROGRAMA
*
************************************************************************
CLASS lcl_event_grid IMPLEMENTATION.
  METHOD double_click.
    READ TABLE i_listado INTO l_listado INDEX e_row-index.
    IF sy-subrc = 0.
      IF e_column CS '_ENT'.
      ELSE.
*          zcl_pedido_sd=>visualizar( l_listado_color-vbeln ).
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "double_click


  METHOD toolbar.

    super->toolbar( EXPORTING e_object = e_object e_interactive = e_interactive ).


  ENDMETHOD.                    "toolbar

  METHOD user_command.
    DATA: l_cont(7), l_hay_sel.

    CASE e_ucomm .
      when 'BORRAR'.
        o_alv->set_marca_filas_sel( EXPORTING validar_seleccion = 'X' CHANGING t_tabla = i_listado hay_sel = l_hay_sel ).
        IF l_hay_sel = 'X'.
          o_alv->comprobar_cambios( ).
          o_alv->set_marca_filas_sel( CHANGING t_tabla = i_listado ).
          l_cont = 0.
          LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
            ADD 1 TO l_cont.
            APPEND <listado> TO i_listado_borrados.

            DELETE i_listado_ini WHERE clave    = <listado>-clave
                                   AND subclave = <listado>-subclave
                                   AND indice   = <listado>-indice.
            DELETE i_listado.
          ENDLOOP.
          MESSAGE s398(00) WITH 'Se han borrado' l_cont 'registros'.
          o_alv->refrescar_grid( ).
        ENDIF.
      WHEN OTHERS.
        super->user_command( EXPORTING e_ucomm = e_ucomm ).
    ENDCASE .

  ENDMETHOD.                    "USER_COMMAND

  METHOD data_changed.
    DATA: ls_good TYPE lvc_s_modi,
          i_cambios TYPE lvc_t_modi,
          l_campo(40).
    FIELD-SYMBOLS <fs> TYPE ANY.

    i_cambios = er_data_changed->mt_good_cells.
    SORT i_cambios BY row_id.

    LOOP AT i_cambios INTO ls_good.
      AT NEW row_id.
        CLEAR l_listado.
        READ TABLE i_listado INTO l_listado INDEX ls_good-row_id.
      ENDAT.
      IF sy-subrc = 0.
        CONCATENATE 'L_LISTADO-' ls_good-fieldname INTO l_campo.
        ASSIGN (l_campo) TO <fs>.
        IF sy-subrc = 0.
          <fs> = ls_good-value.
        ENDIF.
      ENDIF.
      AT END OF row_id.
        IF l_listado-ind IS INITIAL.
          l_listado-ind = 'U'.
        ENDIF.
        l_listado-mensaje = ''.
        MODIFY i_listado FROM l_listado INDEX ls_good-row_id.
      ENDAT.
    ENDLOOP.

    o_prog->verificar_datos( ).

    o_alv->refrescar_grid( ).
  ENDMETHOD.                    "data_changed
ENDCLASS.                    "lcl_event_grid IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.

  METHOD buscar_datos.

    sgpi_texto( 'Leyendo datos' ).

    SELECT * FROM ztemp
      INTO CORRESPONDING FIELDS OF TABLE i_listado
     WHERE clave    IN s_clave
       AND subclave IN s_subcl.

    SORT i_listado.

    LOOP AT i_listado ASSIGNING <listado>.
      zcl_ap_alv_grid=>append_style_no_edit( EXPORTING campo = c_campos_clave
                                             CHANGING  tabla_style = <listado>-style ).
    ENDLOOP.

    i_listado_ini = i_listado.
  ENDMETHOD.                                               "seleccionar_datos

  METHOD verificar_datos.

  ENDMETHOD.                    "VERIFICAR_DATOS

ENDCLASS.                    "REPORT IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT o_prog
    EXPORTING
      status = 'INICIO'.

  o_prog->initialization_i(  EXPORTING get_default_param = p_defpa CHANGING sscrfields = sscrfields ).

  IF sy-batch IS INITIAL.
    CREATE OBJECT o_event
      EXPORTING
        boton_refrescar = 'X'
        boton_excel     = 'X'
        boton_borrar    = 'X'
        o_prog          = o_prog.

    CREATE OBJECT o_alv
      EXPORTING
        estructura = 'ZTEMP_EST'
        o_event    = o_event.

    p_vari = o_alv->get_default_layout( ).
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  IF NOT o_alv IS INITIAL.
    p_vari = o_alv->get_f4_layout( ).
  ENDIF.


************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.

  o_prog->at_selection(  ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  o_prog->buscar_datos( ).

  IF sy-batch IS INITIAL.
    CALL SCREEN 0100.
  ENDIF.

************************************************************************
*
* FORMS ADICIONALES
*
************************************************************************

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  o_prog->set_status( status = 'ST_0100' excluir = '' ).
  SET TITLEBAR '100' WITH o_prog->titulo.

  IF v_inicio IS INITIAL.
    v_inicio = 'X'.

    o_alv->variant-variant = p_vari.
    o_alv->set_layout( style = 'STYLE'
                       edit  = 'X'
                       no_rowmove = 'X'
*                       no_rowins = 'X'
                       ancho_optimizado = '' ).

    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_copy_row ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_delete_row ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_cut ).

    o_alv->show( CHANGING tabla = i_listado ).
    o_alv->actualiza_campos_grid( campos_borrar = 'CHECK,IND' ).
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm .
    WHEN 'BACK'.
      o_alv->comprobar_cambios( ).
      o_prog->salir_si_no_cambios( t1 = i_listado
                                   t2 = i_listado_ini ).
    WHEN 'GRABAR'.

      o_alv->comprobar_cambios( ).
      LOOP AT i_listado ASSIGNING <listado>.
        READ TABLE i_listado_ini INTO l_listado WITH KEY clave    = <listado>-clave
                                                         subclave = <listado>-subclave
                                                         indice   = <listado>-indice.
        IF sy-subrc = 0.
          IF <listado> NE l_listado.
            UPDATE ztemp
               SET valor1       = <listado>-valor1
                   valor2       = <listado>-valor2
                   valor3       = <listado>-valor3
                   texto        = <listado>-texto
                   permanente   = <listado>-permanente
             WHERE clave    = <listado>-clave
               AND subclave = <listado>-subclave
               AND indice   = <listado>-indice.
            IF sy-subrc = 0.
              MESSAGE 'Se han grabado los cambios' TYPE 'S'.
            ENDIF.
          ENDIF.
        ELSE.
          CLEAR ztemp.
          MOVE-CORRESPONDING <listado> TO ztemp.
          ztemp-erdat = sy-datum.
          ztemp-ernam = sy-uname.
          MODIFY ztemp.

          zcl_ap_alv_grid=>append_style_no_edit( EXPORTING campo = c_campos_clave
                                                 CHANGING  tabla_style = <listado>-style ).
        ENDIF.
      ENDLOOP.
      i_listado_ini = i_listado.

      LOOP AT i_listado_borrados ASSIGNING <listado> WHERE check = 'X'.
        DELETE FROM ztemp
         WHERE clave = <listado>-clave.
      ENDLOOP.
      o_alv->refrescar_grid( ).
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
