***********************************************************************
* TIPO : LISTADO
* TITULO : Textos estándar
* DESCRIPCION : Textos estándar
*
* AUTOR: Andrés Picazo                                FECHA: 18/09/2025
*
***********************************************************************
REPORT zap_textos.

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES stxh.

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
             check    TYPE xfeld,
             lights   TYPE zico_estado_mensaje,
             tdobject TYPE stxh-tdobject,
             tdname   TYPE stxh-tdname,
             tdid     TYPE stxh-tdid,
             tdspras  TYPE stxh-tdspras,
             string   TYPE string,
             color    TYPE lvc_t_scol,
             message  TYPE bapi_msg,
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
SELECTION-SCREEN BEGIN OF BLOCK zpse WITH FRAME TITLE TEXT-sel.
  SELECT-OPTIONS: s_object FOR stxh-tdobject,
                  s_name   FOR stxh-tdname,
                  s_id     FOR stxh-tdid,
                  s_spras  FOR stxh-tdspras.
SELECTION-SCREEN END OF BLOCK zpse.
SELECTION-SCREEN BEGIN OF BLOCK zps2 WITH FRAME TITLE TEXT-bus.
  PARAMETERS: p_busqu1 TYPE tline-tdline,
              p_busqu2 TYPE tline-tdline,
              p_busqu3 TYPE tline-tdline.
  SELECTION-SCREEN SKIP.
  PARAMETERS p_case AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK zps2.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-var.
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
    DATA l_list TYPE o_prog->t_listado.

    l_list = list.
    CASE column.
      WHEN OTHERS.
        zcl_ap_textos=>editar_texto( id      = l_list-tdid
                                     name    = l_list-tdname
                                     spras   = l_list-tdspras
                                     object  = l_list-tdobject
                                     display = '' ).
        o_prog->i_listado[ tdid     = l_list-tdid
                           tdname   = l_list-tdname
                           tdspras  = l_list-tdspras
                           tdobject = l_list-tdobject ]-string = zcl_ap_textos=>get_texto_string(
                                               id     = l_list-tdid
                                               name   = l_list-tdname
                                               spras  = l_list-tdspras
                                               object = l_list-tdobject ).
        o_alv->refresh( ).

    ENDCASE.
  ENDMETHOD. " handle_double_click

  METHOD handle_user_command.
    DATA: l_return    TYPE c LENGTH 1,
          l_reemplazo TYPE hap_s_text_subst-substitution_text,
          l_sytabix   LIKE sy-tabix,
          l_ok        TYPE abap_bool.

    check_ucomm_sel = 'REEMPLAZAR,BORRAR,IDIOMA'.

    super->handle_user_command( e_salv_function ).

    CASE ucomm.
      WHEN 'REEMPLAZAR'.
        zcl_ap_popup=>popup_usuario( EXPORTING campo1 = 'HAP_S_TEXT_SUBST-SUBSTITUTION_TEXT'
                                               titulo = 'Texto de reeemplazo'
                                     IMPORTING return = l_return
                                     CHANGING  valor1 = l_reemplazo ).
        IF l_return = ''.
          LOOP AT o_prog->i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE check = 'X'.
            CLEAR <listado>-color.
            IF NOT p_busqu1 IS INITIAL.
              IF <listado>-string CS p_busqu1.
                l_return =
                  zcl_ap_textos=>sustituir_texto(
                  id              = <listado>-tdid
                  name            = <listado>-tdname
                  spras           = <listado>-tdspras
                  object          = <listado>-tdobject
                  texto_original  = p_busqu1
                  texto_reemplazo = l_reemplazo
                  case            = p_case ).
              ENDIF.
            ENDIF.
            IF NOT p_busqu2 IS INITIAL.
              IF <listado>-string CS p_busqu2.
                l_return =
                  zcl_ap_textos=>sustituir_texto(
                  id              = <listado>-tdid
                  name            = <listado>-tdname
                  spras           = <listado>-tdspras
                  object          = <listado>-tdobject
                  texto_original  = p_busqu2
                  texto_reemplazo = l_reemplazo
                  case            = p_case ).
              ENDIF.
            ENDIF.
            IF NOT p_busqu3 IS INITIAL.
              IF <listado>-string CS p_busqu3.
                l_return = zcl_ap_textos=>sustituir_texto(
                  id              = <listado>-tdid
                  name            = <listado>-tdname
                  spras           = <listado>-tdspras
                  object          = <listado>-tdobject
                  texto_original  = p_busqu3
                  texto_reemplazo = l_reemplazo
                  case            = p_case ).
              ENDIF.
            ENDIF.

            <listado>-string = zcl_ap_textos=>get_texto_string(
              id     = <listado>-tdid
              name   = <listado>-tdname
              spras  = <listado>-tdspras
              object = <listado>-tdobject ).
          ENDLOOP.

          IF l_return <> '' AND l_return <> '0'.
            append_color( EXPORTING colorc      = 'ROJO'
                          CHANGING  tabla_color = <listado>-color ).
          ENDIF.
          refresh( ).
        ENDIF.
      WHEN 'BORRAR'.

        l_return = zcl_ap_popup=>confirmar(
          titulo = 'Textos a borrar'
          texto  = '¿Está seguro de que desea'
          texto2 = 'borrar los textos seleccionados?' ).

        IF l_return = 'X'.
          LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
            l_sytabix = sy-tabix.
            l_return =
              zcl_ap_textos=>borrar_texto(
              id     = <listado>-tdid
              name   = <listado>-tdname
              spras  = <listado>-tdspras
              object = <listado>-tdobject ).
            IF l_return = 0.
              append_color( EXPORTING colorc      = 'ROJO'
                            CHANGING  tabla_color = <listado>-color ).
            ELSE.
              DELETE o_prog->i_listado INDEX l_sytabix.
            ENDIF.

          ENDLOOP.

          refresh( ).
        ENDIF.
      WHEN 'IDIOMA'.
        CLEAR stxh-tdspras.
        zcl_ap_popup=>popup_usuario( EXPORTING campo1 = 'STXH-TDSPRAS'
                                               titulo = 'Indique nuevo idioma'
                                     IMPORTING return = l_ok
                                     CHANGING  valor1 = stxh-tdspras ).
        IF l_ok <> 'A' AND NOT stxh-tdspras IS INITIAL.
          LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
            IF <listado>-tdspras = stxh-tdspras.
              MESSAGE 'No seleccione registros con el idioma a modificar' TYPE 'I'.
              RETURN.
            ENDIF.
            IF line_exists( o_prog->i_listado[ tdid     = <listado>-tdid
                                               tdname   = <listado>-tdname
                                               tdspras  = stxh-tdspras
                                               tdobject = <listado>-tdobject ] ).
              MESSAGE |El registro { <listado>-tdid } { <listado>-tdname } ya tiene el nuevo idioma. Borre primero| TYPE 'I'.
              RETURN.
            ENDIF.
          ENDLOOP.

          LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
            data(l_tabix) = sy-tabix.
            zcl_ap_textos=>save_texto_string(
              id     = <listado>-tdid
              name   = <listado>-tdname
              spras  = stxh-tdspras
              object = <listado>-tdobject
              string = <listado>-string ).
* Verificamos si se ha grabado realmente
            DATA(l_string) = zcl_ap_textos=>get_texto_string(
              id     = <listado>-tdid
              name   = <listado>-tdname
              spras  = stxh-tdspras
              object = <listado>-tdobject ).
            IF not l_string is initial.
              IF zcl_ap_textos=>borrar_texto(
                id     = <listado>-tdid
                name   = <listado>-tdname
                spras  = <listado>-tdspras
                object = <listado>-tdobject ) = 1.
                <listado>-lights  = zcl_ap_alv=>set_icono( icono = icon_green_light mensaje = 'Se ha modificado el idioma' ).
                <listado>-tdspras = stxh-tdspras.
              ELSE.
                <listado>-lights  = zcl_ap_alv=>set_icono( icono = icon_yellow_light mensaje = 'Se ha copiado el texto al nuevo idioma pero no se ha podido borrar el original' ).
                data(l_listado) = <listado>.
                insert l_listado into o_prog->i_listado index l_tabix.
              ENDIF.
            ELSE.
              <listado>-lights = zcl_ap_alv=>set_icono( icono = icon_red_light mensaje = 'Error modificando el idioma' ).
            ENDIF.
          ENDLOOP.
          refresh( ).
        ENDIF.
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

    sgpi_texto( 'Seleccionando datos' ).
    SELECT * FROM stxh                        "#EC CI_ALL_FIELDS_NEEDED
      INTO CORRESPONDING FIELDS OF TABLE i_listado
     WHERE tdobject IN s_object
       AND tdname   IN s_name
       AND tdid     IN s_id
       AND tdspras  IN s_spras.

    o_sgpi->get_filas_tabla( tabla = i_listado ).

    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
      o_sgpi->porcentaje( texto    = 'Leyendo textos'
                          cantidad = 500 ).

      <listado>-string = zcl_ap_textos=>get_texto_string(
        id     = <listado>-tdid
        name   = <listado>-tdname
        spras  = <listado>-tdspras
        object = <listado>-tdobject ).

      IF    NOT p_busqu1 IS INITIAL OR NOT p_busqu2 IS INITIAL
         OR NOT p_busqu3 IS INITIAL.
        DATA(l_ok) = ''.
        IF NOT p_busqu1 IS INITIAL.
*        IF <listado>-string CS p_busqu1.
          IF p_case = 'X'.
            FIND FIRST OCCURRENCE OF p_busqu1 IN <listado>-string RESPECTING CASE.
          ELSE.
            FIND FIRST OCCURRENCE OF p_busqu1 IN <listado>-string IGNORING CASE.
          ENDIF.
          IF sy-subrc = 0.
            l_ok = 'X'.
          ENDIF.
        ENDIF.
        IF NOT p_busqu2 IS INITIAL.
          IF <listado>-string CS p_busqu2.
            l_ok = 'X'.
          ENDIF.
        ENDIF.
        IF NOT p_busqu3 IS INITIAL.
          IF <listado>-string CS p_busqu3.
            l_ok = 'X'.
          ENDIF.
        ENDIF.
        IF l_ok IS INITIAL.
          DELETE i_listado.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT i_listado.
  ENDMETHOD.

  METHOD listado.
    sgpi_texto( 'Generando informe' ).

    o_alv->add_button( button = 'F01' text = 'Reemplazar' icon = icon_replace ucomm = 'REEMPLAZAR' ).
    o_alv->add_button( button = 'F02' text = 'Borrar' icon = icon_delete ucomm = 'BORRAR' ).
    o_alv->add_button( button = 'F03' text = 'Cambiar idioma' icon = icon_change ucomm = 'IDIOMA' ).

    o_alv->set_layout( p_vari ).

    o_alv->set_top_of_page( ).

    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_quitar( 'CHECK,MESSAGE' ).

    o_alv->set_orden( 'TDOBJECT,TDNAME,TDID,TDSPRAS' ).
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
                  guardar_logz = '' ).

  PERFORM add_button IN PROGRAM zap_status USING 'M01' 'Log'(log) '' ''.

  o_prog->o_alv = NEW #( status             = 'STANDARD_ALV_DYN'
                         status_prog        = 'ZAP_STATUS'
                         top_of_page_auto   = 'X'
                         top_of_page_titulo = 'X'
                         o_dev              = o_prog ).

  p_vari = o_prog->o_alv->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN OUTPUT.
  o_prog->handle = ''.
  IF o_prog->o_alv IS INITIAL OR o_prog->handle <> o_prog->aux1.
    o_prog->aux1 = o_prog->handle.
    IF NOT o_prog->o_alv IS INITIAL.
      o_prog->o_alv->free( ).
      CLEAR o_prog->o_alv.
    ENDIF.
    o_prog->o_alv = NEW #( status             = 'STANDARD_ALV_DYN'
                           status_prog        = 'ZAP_STATUS'
                           top_of_page_auto   = 'X'
                           top_of_page_titulo = 'X'
                           handle             = o_prog->handle
                           o_dev              = o_prog ).
    p_vari = o_prog->o_alv->get_default_layout( ).
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  p_vari = o_prog->o_alv->get_f4_layout( ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'ONLI'.
      o_prog->validar_seleccion_obligatoria( campos_or = '*' msgty = 'W' ).
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
