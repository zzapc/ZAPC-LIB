***********************************************************************
* TIPO : LISTADO
* TITULO : Test ODATA
* DESCRIPCION : Test ODATA
*
* AUTOR: Andrés Picazo                                FECHA: 02/11/2015
*
* Doc. Tecnica: http://sap4.com/tareas?=&cliente=CCC&objeto=OOO
**->MANUAL:,http://sap4.com
**->PLANTILLA:,http://sap4.com
*
* MODIFICACIONES
* 02/11/2015 Tarea inicial: http://sap4.com/tareas?&ntask=TTT
*
***********************************************************************
REPORT ztest_odata.

*------INCLUDES--------------------------------------------------------*

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: /iwbep/i_sbd_pr,
        */iwbep/i_sbd_pr,
        /iwbep/i_sbo_es,
        */iwbep/i_sbo_es,
        zal_files,
        altypes,
        zparametros,
        ztemp.
DATA: /iwbep/i_sbo_pr  TYPE /iwbep/i_sbo_pr,
      */iwbep/i_sbo_pr TYPE /iwbep/i_sbo_pr.

*------TABLAS INTERNAS-------------------------------------------------*
TYPES: BEGIN OF  t_listado,
         check,
         erdat    TYPE ztemp-erdat,
         erzet    TYPE ztemp-erzet,
         subclave TYPE ztemp-subclave,
         texto2   TYPE ztemp-texto2,
         texto    TYPE ztemp-texto2,
       END OF t_listado.

__data_set_var listado.

DATA: i_listado_aux TYPE tt_listado,
      i_listado_ini TYPE tt_listado.

*------VARIABLES-------------------------------------------------------*
DATA: v_inicio,
      i_entidades TYPE TABLE OF tpda_vrm_value WITH HEADER LINE,
      i_atributos TYPE TABLE OF tpda_vrm_value WITH HEADER LINE,
      i_operador  TYPE TABLE OF tpda_vrm_value WITH HEADER LINE.

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
      crear_url.

ENDCLASS.                    "REPORT DEFINITION

DATA: o_alv   TYPE REF TO zcl_ap_alv_grid,
      o_event TYPE REF TO lcl_event_grid,
      o_prog  TYPE REF TO zcl_report,
      comando TYPE string.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME.
PARAMETER: p_projec LIKE /iwbep/i_sbd_pr-project,
           p_url TYPE zal_files-fichero,
           p_modif TYPE ztemp-valor1.
SELECTION-SCREEN SKIP.
PARAMETERS: p_vari LIKE disvariant-variant.                ".
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
      /iwbep/i_sbd_pr-project = l_listado-subclave.
      zal_files-descripcion = l_listado-texto.
      altypes-url = l_listado-texto2.
      cl_gui_cfw=>set_new_ok_code( '/00' ).
    ENDIF.

  ENDMETHOD.                                               "double_click


  METHOD toolbar.

    super->toolbar( EXPORTING e_object = e_object e_interactive = e_interactive ).

  ENDMETHOD.                                               "toolbar

  METHOD user_command.

    CASE e_ucomm .
      WHEN 'BORRAR'.
        o_alv->comprobar_cambios( ).
        o_alv->set_marca_filas_sel( CHANGING t_tabla = i_listado ).
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          DELETE FROM ztemp
           WHERE clave    = 'ODATA_TEST'
             AND subclave = <listado>-subclave
             AND texto2   = <listado>-texto2.

          DELETE i_listado.
        ENDLOOP.
        o_alv->refrescar_grid( ).
      WHEN OTHERS.
        super->user_command( EXPORTING e_ucomm = e_ucomm ).
    ENDCASE .

  ENDMETHOD.                                               "USER_COMMAND

  METHOD data_changed.
  ENDMETHOD.                                               "data_changed
ENDCLASS.                    "lcl_event_grid IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.

  METHOD buscar_datos.
    DATA: r_subclave  TYPE RANGE OF ztemp-subclave,
          lr_subclave LIKE LINE OF r_subclave.

    IF NOT /iwbep/i_sbd_pr-project IS INITIAL.
      __rangoc_eq r_subclave /iwbep/i_sbd_pr-project.
    ENDIF.

    sgpi_texto( 'Seleccionando datos' ).
    SELECT * FROM ztemp
      INTO CORRESPONDING FIELDS OF TABLE i_listado
     WHERE clave = 'ODATA_TEST'
       AND subclave IN r_subclave
     ORDER BY erdat DESCENDING erzet DESCENDING.

  ENDMETHOD.                                               "seleccionar_datos


  METHOD crear_url.
    CONCATENATE zal_files-fichero /iwbep/i_sbd_pr-project '_SRV/' /iwbep/i_sbo_es-name INTO altypes-url.
    IF ztemp-valor1 NE ''.
      CONCATENATE altypes-url '?' ztemp-valor1 INTO altypes-url.
    ENDIF.
    IF NOT ztemp-valor2 IS INITIAL.
      IF ztemp-valor1 IS INITIAL.
        CONCATENATE altypes-url '?' INTO altypes-url.
      ELSE.
        CONCATENATE altypes-url '&' INTO altypes-url.
      ENDIF.
      IF zparametros-valor = 'substringof'.
        CONCATENATE altypes-url '$filter=substringof(''' ztemp-valor3 ''',' ztemp-valor2 ')' INTO altypes-url.
      ELSE.
        CONCATENATE altypes-url '$filter=' ztemp-valor2 INTO altypes-url.
        CONCATENATE altypes-url zparametros-valor '''' INTO altypes-url SEPARATED BY space.
        CONCATENATE altypes-url ztemp-valor3 '''' INTO altypes-url.

      ENDIF.
    ENDIF.
  ENDMETHOD.                    "crear_url


ENDCLASS.                    "REPORT IMPLEMENTATION
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT o_prog
    EXPORTING
      status       = 'INICIO'
      guardar_logz = 'X'.

  p_url = zcl_segw=>get_ruta_odata( ).

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
        estructura = ''
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

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection(  ).

AT SELECTION-SCREEN OUTPUT.
*  zcl_ap_dynpro=>screen_visible( group1 = 'PED' variable = p_pedid ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  /iwbep/i_sbd_pr-project = p_projec.
  zal_files-fichero = p_url.
  ztemp-valor1 = p_modif.
  /iwbep/i_sbo_es-project = p_projec.

  o_prog->crear_url( ).
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
    o_alv->set_layout( no_rowmove = 'X'
                       no_rowins = 'X'
*                      lights = 'LIGHTS'
                      ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_refresh ).

    o_alv->set_campos_tabint( i_listado[] ).

    o_alv->set_field_noout( 'CHECK,SUBCLAVE,ERDAT,ERZET' ).
    o_alv->set_field_text( campo = 'TEXTO2' valor = 'ODATA' ).
    o_alv->set_field_text( campo = 'TEXTO' valor = 'Comentario' ).

    o_prog->sgpi_texto( 'Generando informe' ).
    o_alv->show( CHANGING tabla = i_listado ).
    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    REFRESH i_operador.
    i_operador-key = i_operador-text = 'eq'. APPEND i_operador.
    i_operador-key = i_operador-text = 'substringof'. APPEND i_operador.
    zcl_ap_popup=>list_dynpro( campo   = 'ZPARAMETROS-VALOR'
                               valores = i_operador[] ).
    zparametros-campo = 'eq'.
  ENDIF.

  IF */iwbep/i_sbd_pr NE /iwbep/i_sbd_pr AND /iwbep/i_sbd_pr-project NE ''.

    o_prog->buscar_datos( ).
    o_alv->refrescar_grid( ).

    REFRESH i_entidades.
    APPEND i_entidades.
    SELECT name project FROM /iwbep/i_sbo_es
      INTO CORRESPONDING FIELDS OF /iwbep/i_sbo_es
      WHERE project = /iwbep/i_sbd_pr-project.
      i_entidades-key = /iwbep/i_sbo_es-name.
      i_entidades-text = /iwbep/i_sbo_es-name.
      APPEND i_entidades.
    ENDSELECT.

    zcl_ap_popup=>list_dynpro( campo   = '/IWBEP/I_SBO_ES-NAME'
                               valores = i_entidades[] ).

    REFRESH i_atributos.
    APPEND i_atributos.
    SELECT name project FROM /iwbep/i_sbo_pr
      INTO CORRESPONDING FIELDS OF /iwbep/i_sbo_pr
      WHERE project = /iwbep/i_sbd_pr-project.
      i_atributos-key = /iwbep/i_sbo_pr-name.
      i_atributos-text = /iwbep/i_sbo_pr-name.
      APPEND i_atributos.
    ENDSELECT.

    zcl_ap_popup=>list_dynpro( campo   = 'ZTEMP-VALOR2'
                               valores = i_atributos[] ).
  ENDIF.

*  IF */iwbep/i_sbo_es NE /iwbep/i_sbo_es AND /iwbep/i_sbo_es-name NE ''.
*    SELECT SINGLE * FROM /iwbep/i_sbo_es
*      WHERE project = /iwbep/i_sbd_pr
*        AND name = /iwbep/i_sbo_es-name.
*    REFRESH i_atributos.
*    APPEND i_atributos.
*    SELECT * FROM /iwbep/i_sbo_pr
*      WHERE project = /iwbep/i_sbd_pr-project
*        AND parent_uuid = /iwbep/i_sbo_es-node_uuid.
*      i_atributos-key = /iwbep/i_sbo_es-name.
*      i_atributos-text = /iwbep/i_sbo_es-name.
*      APPEND i_atributos.
*    ENDSELECT.
*
*    zcl_ap_popup=>list_dynpro( campo   = 'ZTEMP-VALOR2'
*                               valores = i_atributos[] ).
*  ENDIF.

  */iwbep/i_sbd_pr = /iwbep/i_sbd_pr.
  */iwbep/i_sbo_es = /iwbep/i_sbo_es.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA v_hora TYPE sy-uzeit.

  IF NOT l_listado IS INITIAL.
    /iwbep/i_sbd_pr-project = l_listado-subclave.
    zal_files-descripcion = l_listado-texto.
    altypes-url = l_listado-texto2.
    CLEAR l_listado.
  ENDIF.
  /iwbep/i_sbo_es-project = /iwbep/i_sbd_pr-project.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CREAR'.
      o_prog->crear_url( ).
    WHEN 'EJECUTAR'.
      v_hora = sy-uzeit.
      zcl_ap_gos=>visualizar_fichero_st( altypes-url ).

      DELETE FROM ztemp
       WHERE clave    = 'ODATA_TEST'
         AND subclave = /iwbep/i_sbd_pr-project
         AND texto2   = altypes-url.

      zcl_ap_temp=>set_st( clave    = 'ODATA_TEST' subclave = /iwbep/i_sbd_pr-project texto = zal_files-descripcion texto2 = altypes-url permanente = 'X' indice_auto = 'X' ).
      o_prog->buscar_datos( ).
      o_alv->refrescar_grid( ).

    WHEN 'GRABAR'.
      o_alv->comprobar_cambios( ).
      o_prog->message( p1 = 'Se han guardado los cambios' type = 'S' no_log = '' ).
      o_alv->refrescar_grid( ).
      i_listado_ini = i_listado.

    WHEN 'LOG'.
      SUBMIT zlog
       AND RETURN
       WITH s_proces = 'ODATA'
       WITH s_progra = /iwbep/i_sbd_pr-project
       WITH s_hora BETWEEN v_hora AND '235959'.


  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
