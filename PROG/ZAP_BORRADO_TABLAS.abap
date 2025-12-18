***********************************************************************
* TIPO : LISTADO
* TITULO : Borrado tablas
* DESCRIPCION : Borrado tablas
*
* AUTOR: Andrés Picazo                                FECHA: 23/09/2025
*
***********************************************************************
REPORT zap_borrado_tablas.

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES dd02t.

*------TABLAS INTERNAS-------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check FINAL.
  PUBLIC SECTION.
    METHODS handle_user_command REDEFINITION.
    METHODS visualizar_objeto   REDEFINITION.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_listado,
             check       TYPE xfeld,
             lights      TYPE zico_estado_mensaje,
             tabla       TYPE tabname,
             ddtext      TYPE dd02t-ddtext,
             dias_txt    TYPE zparametros-atributo1,
             dias        TYPE int4,
             where_cond  TYPE string,
             campo_fecha TYPE string,
             registros   TYPE int4,
             where       TYPE string,
             message     TYPE bapi_msg,
           END OF t_listado,
           tt_listado TYPE STANDARD TABLE OF t_listado.

    DATA: i_listado TYPE tt_listado,
          o_alv     TYPE REF TO lcl_alv ##NEEDED.

    METHODS  main.

    METHODS: listado,
             seleccionar_datos.

ENDCLASS.

*------VARIABLES-------------------------------------------------------*
DATA o_prog TYPE REF TO zcl_report.


*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-sel.
  SELECT-OPTIONS s_tabla FOR dd02t-tabname.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_ejec AS CHECKBOX.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_vari LIKE disvariant-variant.
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
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    DATA l_list TYPE o_prog->t_listado.

    l_list = list.
    CASE column.
*      WHEN 'BUKRS'.
*        MESSAGE l_list-bukrs TYPE 'I'.
      WHEN OTHERS. message = '?'.
    ENDCASE.
  ENDMETHOD. " handle_double_click

  METHOD handle_user_command.
    check_ucomm_sel = 'EJEC'.

    super->handle_user_command( e_salv_function ).

    CASE ucomm.
      WHEN 'EJEC'.
        LOOP AT o_prog->i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE check = 'X' AND message IS INITIAL AND registros > 0.
          TRY.
              DELETE FROM (<listado>-tabla)
               WHERE (<listado>-where).
               o_prog->message( p1 = |Se han borrado { <listado>-registros } de tabla {  <listado>-tabla } { <listado>-where }| type = 'I' SOLO_LOG = 'X' ).
            CATCH cx_root INTO DATA(o_root).
              <listado>-message = o_root->get_text( ).
          ENDTRY.
          IF <listado>-message IS INITIAL.
            o_prog->set_status_list( EXPORTING message = <listado>-message icono = icon_green_light CHANGING list = <listado> ).
          ELSE.
            o_prog->set_status_list( EXPORTING message = <listado>-message icono = icon_red_light CHANGING list = <listado> ).
          ENDIF.

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
    seleccionar_datos( ).
    listado( ).
  ENDMETHOD.                    " REPORT

  METHOD seleccionar_datos.
    DATA l_fecha_minima TYPE dats.

    sgpi_texto( 'Seleccionando datos'(sda) ).
    SELECT campo AS tabla, valor AS where_cond, atributo1 AS dias_txt, atributo2 AS campo_fecha FROM zparametros
      INTO CORRESPONDING FIELDS OF TABLE @i_listado
     WHERE clave  = 'BOR_TABLAS'
       AND campo IN @s_tabla.

    o_prog->o_sgpi->get_filas_tabla( i_listado[] ).
    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
      sgpi_texto( texto1 = 'Procesando datos'(pda) cant_porc = 100 ).

      SELECT ddtext FROM dd02t
      INTO <listado>-ddtext
      UP TO 1 ROWS
        WHERE tabname    = <listado>-tabla
          AND ddlanguage = sy-langu.
      ENDSELECT.
      IF sy-subrc <> 0.
        SELECT ddtext FROM dd02t
        INTO <listado>-ddtext
        UP TO 1 ROWS
          WHERE tabname = <listado>-tabla.
        ENDSELECT.
        IF sy-subrc <> 0.
          <listado>-message = 'No existe la tabla'(NEL).
        ENDIF.
      ENDIF.

      TRY.
          <listado>-dias = <listado>-dias_txt.
        CATCH cx_root INTO DATA(o_root).
          IF <listado>-message IS INITIAL.
            <listado>-message = o_root->get_text( ).
          ENDIF.
      ENDTRY.

      CLEAR l_fecha_minima.
      IF <listado>-message IS INITIAL.
        IF <listado>-dias = 0.
          <listado>-message = 'No indicado días a considerar'(NID).
        ELSE.
          l_fecha_minima = sy-datum - <listado>-dias.
        ENDIF.
      ENDIF.

      IF <listado>-message IS INITIAL.
        IF <listado>-campo_fecha IS INITIAL.
          <listado>-message = 'No ha informado campo fecha'(NHI).
        ELSE.
          <listado>-where = |{ <listado>-campo_fecha } < '{ l_fecha_minima }'|.
        ENDIF.
      ENDIF.
      IF NOT <listado>-where_cond IS INITIAL.
        <listado>-where = |{ <listado>-where } AND { <listado>-where_cond }|.
      ENDIF.

      IF not <listado>-message IS INITIAL.
        DATA(l_icono) = icon_red_light.
      ELSE.
        TRY.
            SELECT COUNT( * ) FROM (<listado>-tabla)
              INTO <listado>-registros
             WHERE (<listado>-where).

            IF <listado>-registros > 0.
              IF p_ejec = 'X'.
                DELETE FROM (<listado>-tabla)
                 WHERE (<listado>-where).
                IF sy-subrc = 0.
                  l_icono = icon_delete.
                  message( p1 = |Se han borrado { <listado>-registros } de tabla {  <listado>-tabla } { <listado>-where }| type = 'I' SOLO_LOG = 'X' ).
                ENDIF.
              ELSE.
                l_icono = icon_yellow_light.
              ENDIF.
            ELSE.
              l_icono = icon_dummy.
            ENDIF.
          CATCH cx_root INTO o_root.
            <listado>-message = o_root->get_text( ).
            l_icono = icon_message_critical.
            message( p1 = <listado>-tabla p2 = <listado>-message type = 'E' SOLO_LOG = 'X' ).
        ENDTRY.
      ENDIF.

      set_status_list( EXPORTING message = <listado>-message icono = l_icono CHANGING list = <listado> ).
    ENDLOOP.

    SORT i_listado.
  ENDMETHOD.

  METHOD listado.
    sgpi_texto( 'Generando informe'(gin) ).

    o_alv->add_button( button = 'F01' text = 'Borrar registros'(BR1) icon = icon_delete ucomm = 'EJEC' ).

    o_alv->set_layout( p_vari ).

    o_alv->set_top_of_page( ).

    o_alv->set_field_text( 'DIAS,WHERE_COND,CAMPO_FECHA,REGISTROS' ).
    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_quitar( 'CHECK,DIAS_TXT,WHERE' ).

*    o_alv->set_orden( '' ).
    o_alv->get_datos_layout( EXPORTING reordenar_tabla = 'X' tabla_ref = 'X' CHANGING t_tabla = i_listado ).
    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    o_alv->show( ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  o_prog = NEW #( status       = 'INICIO_DYN'
                  status_prog  = 'ZAP_STATUS'
                  no_param     = 'X'
                  guardar_logz = 'X'
                  prog         = 'BOR_TABLAS' ).

  PERFORM add_button IN PROGRAM zap_status USING 'M01' 'Parámetros'(PAR) '' ''.
  PERFORM add_button IN PROGRAM zap_status USING 'M02' 'Log'(LOG) '' ''.

  o_prog->o_alv = NEW #( status             = 'STANDARD_ALV_DYN'
                         status_prog        = 'ZAP_STATUS'
                         top_of_page_auto   = 'X'
                         top_of_page_titulo = 'X'
                         o_dev              = o_prog ).

  p_vari = o_prog->o_alv->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN OUTPUT.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  p_vari = o_prog->o_alv->get_f4_layout( ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  CASE sy-ucomm.
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