***********************************************************************
* TIPO : LISTADO
* TITULO : ??
* DESCRIPCION : ??
*
* AUTOR: Andrés Picazo                                FECHA: 10/12/2019
* ANALISTA: ??
*
* Doc. Tecnica: http://sap4.com/tareas?=&cliente=CCC&objeto=OOO
**->MANUAL:http://sap4.com,http://sap4.com/edicion
**->PLANTILLA:,http://sap4.com
*
***********************************************************************
REPORT zfivv052.

*------TABLAS/ESTRUCTURAS----------------------------------------------*

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
             bukrs   TYPE t001-bukrs,
             butxt   TYPE t001-butxt,
             message TYPE bapi_msg,
           END OF t_listado,
           tt_listado TYPE STANDARD TABLE OF t_listado.
    DATA: i_listado TYPE tt_listado,
          o_alv     TYPE REF TO lcl_alv.                    "#EC NEEDED

    METHODS: main.

    METHODS:  listado,
      cargar_fichero.

ENDCLASS.                    "REPORT DEFINITION

*------VARIABLES-------------------------------------------------------*
DATA: o_prog TYPE REF TO zcl_report. "#EC NEEDED


*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-sel.
PARAMETERS: p_file TYPE localfile OBLIGATORY.
SELECTION-SCREEN: SKIP 1.
PARAMETERS: p_vari type disvariant-variant.
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
      WHEN 'XXX'.
      WHEN OTHERS. message = 'No implementado'.
    ENDCASE.
  ENDMETHOD. "handle_double_click

  METHOD handle_user_command.
    super->handle_user_command( e_salv_function ).

    CASE UCOMM.
      WHEN 'CARGA'.
        get_seleccion( CHANGING t_tabla = o_prog->i_listado ).
        LOOP AT o_prog->i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE check = 'X'. "#EC NEEDED
        ENDLOOP.
        IF sy-subrc <> 0.
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
    cargar_fichero( ).
    listado( ).
  ENDMETHOD.                    "REPORT

  METHOD cargar_fichero.
    TYPES: BEGIN OF t_fichero,
             id(10),
             fecha type dats,
           END OF t_fichero.
    DATA: i_fichero TYPE TABLE OF t_fichero,
          l_listado TYPE t_listado,
          o_excel   TYPE REF TO zcl_ap_abap2xls.

    FIELD-SYMBOLS: <fichero> TYPE t_fichero.

    o_prog->sgpi_texto( 'Importando fichero'(imf) ).
    CREATE OBJECT o_excel.
    o_excel->lee_fichero( EXPORTING fichero = p_file
*                                    hoja    = p_hojm_1
                                    get_datos = 'X'
                                    huge      = ''
                                    mostrar_error = 'X'
                                    VALIDAR_MAESTROS = 'X'
                          IMPORTING datos   = i_fichero ).

    o_prog->o_sgpi->get_filas_tabla( i_fichero[] ).
    LOOP AT i_fichero ASSIGNING <fichero>.
      data(l_row) = sy-tabix + 1.
      CLEAR l_listado.
      o_prog->o_sgpi->porcentaje( texto = 'Procesando fichero'(prf) cantidad = 1000 ).
      MOVE-CORRESPONDING <fichero> TO l_listado.
     l_listado-message = o_excel->get_msg( row = l_row ).

      set_status_list( EXPORTING message = l_listado-message criterio = 'A' CHANGING list = l_listado ).
      APPEND L_LISTADO TO I_LISTADO.
    ENDLOOP.


  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.

    sgpi_texto( 'Generando informe'(gin) ).

    o_alv->add_button( button = 'F01' text = 'Ejecutar'(eje)  icon = icon_execute_object UCOMM = 'CARGA').

    o_alv->set_layout( p_vari ).
    o_alv->get_datos_layout( EXPORTING reordenar_tabla = 'X' CHANGING t_tabla = i_listado ).
    o_alv->set_top_of_page( ).

*    o_alv->set_field_hotspot( campo = 'EBELN' auto = 'X' ).

    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_quitar( 'CHECK' ).
    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    o_alv->show( ).


  ENDMETHOD.                    "

ENDCLASS.                    "REPORT IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  o_prog = NEW #( status   = 'INICIO_DYN' status_prog  = 'ZAP_STATUS'
                  no_param = 'X'          guardar_logz = '' ).

  PERFORM add_button IN PROGRAM zap_status USING 'M01' 'Log'(log) '' ''.

  o_prog->o_alv =  NEW #( status = 'STANDARD_ALV_DYN' status_prog = 'ZAP_STATUS'
                          top_of_page_auto = 'X'  top_of_page_titulo = 'X'
                          o_dev            = o_prog ).


  p_vari = o_prog->o_alv->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  p_vari = o_prog->o_alv->get_f4_layout( ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  o_prog->at_selection( ).

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  p_file = zcl_ap_ficheros=>popup_select_fichero( file_filter = zcl_ap_ficheros=>C_FILTRO_XLSX ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  o_prog->main( ).
