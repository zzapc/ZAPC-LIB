***********************************************************************
* TIPO : LISTADO
* TITULO : ??
* DESCRIPCION : ??
*
* AUTOR: Andres Picazo                                FECHA: 24/09/2024
* ANALISTA: ??
*
***********************************************************************
REPORT zplantilla_alvf_2024.

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: vbrk, mara.

*------TABLAS INTERNAS-------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check.
  PUBLIC SECTION.
    METHODS handle_user_command REDEFINITION.
    METHODS visualizar_objeto   REDEFINITION.
ENDCLASS.


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
             message TYPE bapi_msg,
           END OF t_listado,
           tt_listado TYPE TABLE OF t_listado.

    DATA: i_listado TYPE tt_listado,
          o_alv     TYPE REF TO lcl_alv ##NEEDED.

    METHODS  main.

    METHODS: listado,
      cargar_fichero.

ENDCLASS.

*------VARIABLES-------------------------------------------------------*
DATA o_prog TYPE REF TO zcl_report.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-sel.
PARAMETERS p_file TYPE localfile OBLIGATORY.
SELECTION-SCREEN SKIP 1.
PARAMETERS p_vari LIKE disvariant-variant.
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
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    DATA l_list TYPE o_prog->t_listado.

    l_list = list.
    CASE column.
*      WHEN 'BUKRS'.
*        MESSAGE l_list-bukrs TYPE 'I'.
      WHEN OTHERS. message = 'No implementado'.
    ENDCASE.
  ENDMETHOD. " handle_double_click

  METHOD handle_user_command.
    check_ucomm_sel = 'IMPORTAR'.

    super->handle_user_command( e_salv_function ).

    CASE ucomm.
      WHEN 'IMPORTAR'.
        LOOP AT o_prog->i_listado TRANSPORTING NO FIELDS WHERE check = 'X' AND lights(3) = icon_red_light(3).
          MESSAGE 'No seleccione registros con errores' TYPE 'I'.
          RETURN.
        ENDLOOP.
        LOOP AT o_prog->i_listado TRANSPORTING NO FIELDS WHERE check = 'X' AND lights(3) = icon_green_light(3).
          MESSAGE 'No seleccione registros ya importados' TYPE 'I'.
          RETURN.
        ENDLOOP.

        LOOP AT o_prog->i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE check = 'X'.
* TO DO!
        ENDLOOP.
        IF sy-subrc = 0.
          refresh( ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.
  METHOD main.
    cargar_fichero( ).
    listado( ).
  ENDMETHOD.                    " REPORT

  METHOD cargar_fichero.
    TYPES: BEGIN OF t_fichero,
             id             TYPE c LENGTH 10,
             charg          TYPE c LENGTH 10,
             nserie         TYPE c LENGTH 10,
             maktx          TYPE c LENGTH 80,
             cal_funcional  TYPE c LENGTH 10,
             calidad_visual TYPE c LENGTH 10,
             potencia       TYPE c LENGTH 20,
             orden          TYPE c LENGTH 10,
           END OF t_fichero.

    DATA: o_excel   TYPE REF TO zcl_ap_abap2xls,
          i_fichero TYPE TABLE OF t_fichero,
          l_row     TYPE i,
          l_listado TYPE t_listado.

    FIELD-SYMBOLS <fichero> TYPE t_fichero.

    o_prog->sgpi_texto( 'Importando fichero'(imf) ).
    o_excel = NEW #( ).
    o_excel->lee_fichero( EXPORTING fichero = p_file
*                                    hoja    = p_hojm_1
                                    get_datos = 'X'
                                    huge      = 'X'
                                    mostrar_error = 'X'
                                    validar_maestros = 'X'
                          IMPORTING datos   = i_fichero ).

    o_prog->o_sgpi->get_filas_tabla( i_fichero[] ).
    LOOP AT i_fichero ASSIGNING <fichero>.
      l_row = sy-tabix + 1.
      CLEAR l_listado.
      o_prog->o_sgpi->porcentaje( texto = 'Procesando fichero'(prf) cantidad = 1000 ).
      MOVE-CORRESPONDING <fichero> TO l_listado.
      l_listado-message = o_excel->get_msg( row = l_row ).

      set_status_list( EXPORTING message = l_listado-message criterio = 'V' CHANGING list = l_listado ).
      APPEND l_listado TO i_listado.
    ENDLOOP.
  ENDMETHOD.                    " seleccionar_datos

  METHOD listado.
    sgpi_texto( 'Generando informe'(gin) ).

    o_alv->add_button( button = 'F01' text = 'Importar registros'(ire)  icon = icon_execute_object ucomm = 'IMPORTAR' ).

    o_alv->set_layout( p_vari ).
    o_alv->get_datos_layout( EXPORTING reordenar_tabla = 'X' CHANGING t_tabla = i_listado ).
    o_alv->set_top_of_page( ).

*    o_alv->set_field_hotspot( campo = 'EBELN' auto = 'X' ).

    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_quitar( 'CHECK' ).
    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    o_alv->show( ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  o_prog = NEW #( status   = 'INICIO_DYN' status_prog  = 'ZAP_STATUS'
                  no_param = 'X'          guardar_logz = '' ).

  PERFORM add_button IN PROGRAM zap_status USING 'M01' 'Log'(log) '' ''.

  o_prog->o_alv = NEW #( status           = 'STANDARD_ALV_DYN' status_prog        = 'ZAP_STATUS'
                  top_of_page_auto = 'X'                top_of_page_titulo = 'X'
                  o_dev = o_prog ).

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
  p_file = zcl_ap_ficheros=>popup_select_fichero( file_filter = zcl_ap_ficheros=>c_filtro_xls ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  o_prog->main( ).
