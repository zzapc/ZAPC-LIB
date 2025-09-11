***********************************************************************
* TIPO : LISTADO
* TITULO : Mantenimiento textos mail
* DESCRIPCION : Mantenimiento textos mail
*
* AUTOR: Andrés Picazo                                FECHA: 15/01/2020
*
***********************************************************************
REPORT zap_textos_mail.


*------TABLAS/ESTRUCTURAS----------------------------------------------*

*------TABLAS INTERNAS-------------------------------------------------*


*------VARIABLES-------------------------------------------------------*


*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*-------------------------------------------------------------------7---*

CLASS lcl_event_grid DEFINITION INHERITING FROM zcl_ap_alv_grid_eventos FINAL.
  PUBLIC SECTION.
    METHODS: data_changed          REDEFINITION,
      data_changed_finished REDEFINITION,
      toolbar               REDEFINITION,
      user_command          REDEFINITION,
      visualizar_objeto     REDEFINITION.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF  t_listado,
             check           TYPE xfeld,
             lights          TYPE zico_estado_mensaje,
             grupo           TYPE zap_textos_mail-grupo,
             codigo          TYPE zap_textos_mail-codigo,
             version         TYPE zap_textos_mail-version,
             spras           TYPE zap_textos_mail-spras,
             descripcion     TYPE zap_textos_mail-descripcion,
             html            TYPE zap_textos_mail-html,
             defecto         TYPE zap_textos_mail-defecto,
             asunto          TYPE zap_textos_mail-asunto,
             emisor          TYPE zap_textos_mail-emisor,
             email_pruebas   TYPE zap_textos_mail-email_pruebas,
             aviso_pruebas   TYPE zap_textos_mail-aviso_pruebas,
             destino         TYPE zap_textos_mail-destino,
             texto           TYPE zap_textos_mail-texto,
             adobe           TYPE zap_textos_mail-adobe,
             nombre_adjunto  TYPE zap_textos_mail-nombre_adjunto,
             ico_adj         TYPE icon_d,
             binario         TYPE zap_textos_mail-binario,
             nombre_adjunto2 TYPE zap_textos_mail-nombre_adjunto,
             ico_adj2        TYPE icon_d,
             binario2        TYPE zap_textos_mail-binario,
             updkz           TYPE cdpos-chngind,
             message         TYPE bapi_msg,
             style           TYPE lvc_t_styl,
             color           TYPE lvc_t_scol,
           END OF t_listado,
           tt_listado TYPE STANDARD TABLE OF t_listado.

    DATA: i_listado     TYPE tt_listado,
          i_listado_ini TYPE tt_listado.

    DATA: o_alv   TYPE REF TO zcl_ap_alv_grid,
          o_event TYPE REF TO lcl_event_grid.

    METHODS: buscar_datos REDEFINITION,

      validaciones IMPORTING !mod    TYPE abap_bool DEFAULT ''
                   CHANGING  listado TYPE t_listado ##NEEDED,

      status_dynpro_0100,
      command_dynpro_0100,
      enviar_mail IMPORTING direccion TYPE any.

ENDCLASS.

DATA o_prog TYPE REF TO zcl_report ##NEEDED.

DATA: grupo  TYPE zap_textos_mail-grupo,
      codigo TYPE zap_textos_mail-codigo.
*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-sel.
SELECT-OPTIONS: s_grupo  FOR grupo,
                s_codigo FOR codigo,
                s_spras  FOR sy-langu.
SELECTION-SCREEN SKIP 1.
PARAMETERS p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b01.
__botones_plantilla.

************************************************************************
*
* LOGICA DEL PROGRAMA
*
************************************************************************
CLASS lcl_event_grid IMPLEMENTATION.
  METHOD visualizar_objeto.
    DATA l_list TYPE o_prog->t_listado.

    l_list = list.
    CASE column.
      WHEN 'ICO_ADJ' OR 'NOMBRE_ADJUNTO'.
        IF NOT l_list-binario IS INITIAL.
          zcl_ap_ficheros=>visualizar( fichero = l_list-nombre_adjunto xstring = l_list-binario ).
        ENDIF.
      WHEN 'ICO_ADJ2' OR 'NOMBRE_ADJUNTO2'.
        IF NOT l_list-binario2 IS INITIAL.
          zcl_ap_ficheros=>visualizar( fichero = l_list-nombre_adjunto2 xstring = l_list-binario2 ).
        ENDIF.
      WHEN OTHERS. message = 'No implementado'.
    ENDCASE.
  ENDMETHOD. " handle_double_click

  METHOD toolbar.
    super->toolbar( e_object = e_object e_interactive = e_interactive ).
  ENDMETHOD.

  METHOD user_command.
    DATA l_listado TYPE o_prog->t_listado.

    CASE e_ucomm.
      WHEN 'NUEVO'.
        zcl_ap_alv_grid=>append_style_edit( EXPORTING campo = 'GRUPO,CODIGO,VERSION,SPRAS' CHANGING  tabla_style = l_listado-style ).
        l_listado-updkz = 'I'.
        APPEND l_listado TO o_prog->i_listado.
        o_alv->refrescar_grid( soft_refresh = '' ).
      WHEN 'BORRAR'.
        o_alv->set_marca_filas_sel( EXPORTING validar_seleccion = 'X'  CHANGING t_tabla = o_prog->i_listado ).
        LOOP AT o_prog->i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE check = 'X'.
          <listado>-updkz = 'D'.
        ENDLOOP.
        o_alv->refrescar_grid( soft_refresh = '' ).
      WHEN OTHERS.
        super->user_command( e_ucomm = e_ucomm ).
    ENDCASE.
  ENDMETHOD.

  METHOD data_changed.
    ini_data_changed( cambios = er_data_changed->mt_good_cells  ).

    LOOP AT i_cambios_celda INTO cambio_celda.
      AT NEW row_id.
        READ TABLE o_prog->i_listado INTO DATA(l_listado_ini) INDEX cambio_celda-row_id. "#EC CI_SUBRC
        DATA(l_listado) = l_listado_ini.
      ENDAT.

      set_valor_mod( CHANGING datos = l_listado ).

      AT END OF row_id.
        o_prog->validaciones( EXPORTING mod = 'X' CHANGING listado = l_listado ).
        MODIFY o_prog->i_listado FROM l_listado INDEX cambio_celda-row_id.
        actualizar_fila( fila_ini = l_listado_ini fila_fin = l_listado er_data_changed = er_data_changed fila = cambio_celda-row_id ).
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD data_changed_finished.
    IF NOT tabla_data_changed IS INITIAL.
*      o_alv->refrescar_grid( ).
      CLEAR tabla_data_changed.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.
  METHOD buscar_datos.
    sgpi_texto( 'Seleccionando datos'(sda) ).
    CLEAR i_listado.
    SELECT * FROM zap_textos_mail
      INTO CORRESPONDING FIELDS OF TABLE i_listado
     WHERE grupo  IN s_grupo
       AND codigo IN s_codigo
       AND spras  IN s_spras
     ORDER BY PRIMARY KEY.

    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
      validaciones( CHANGING listado = <listado> ).
    ENDLOOP.

    i_listado_ini = i_listado.
  ENDMETHOD.                                               " seleccionar_datos

  METHOD status_dynpro_0100.
    status_dynpro( EXPORTING cprog = 'ZAP_STATUS' status = 'ST_DYN' CHANGING i_listado = i_listado ).
    IF inicio IS NOT INITIAL.
      RETURN.
    ENDIF.

    inicio = 'X'.
    o_alv->add_button( button = 'F01' text = 'Grabar' icon = icon_system_save ucomm = 'GRABAR' ).
    o_alv->add_button( button = 'F02' text = 'Test' icon = icon_test ucomm = 'TEST' ).
    o_alv->add_button( button = 'F03' text = 'Subir adjunto' icon = icon_pdf ucomm = 'ADJ' ).
    o_alv->add_button( button = 'F04' qinfo = 'Subir adjunto 2' icon = icon_pdf ucomm = 'ADJ2' ).
    o_alv->add_button( button = 'M01' text = 'Borrar adjunto' icon = icon_delete ucomm = 'BOR_ADJ' ).
    o_alv->add_button( button = 'M02' text = 'Borrar adjunto 2' icon = icon_delete ucomm = 'BOR_ADJ2' ).
    IF sy-sysid <> zcl_c=>entorno_produccion.
      o_alv->add_button( button = 'F05' text = 'Transporte' icon = icon_ws_truck ucomm = 'OT' ).
    ENDIF.

    o_alv->registrar_mod( ).
    o_alv->set_layout( no_rowmove = 'X' no_rowins = 'X' style = 'STYLE' colort = 'COLOR' ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_refresh ).
    o_alv->set_campos_tabint( i_listado[] ).
    o_alv->set_field_quitar( 'CHECK,LIGHTS,MESSAGE,UPDKZ,BINARIO,BINARIO2' ).
*      o_alv->set_field_noout( 'EMAIL_PRUEBAS' ).
    o_alv->set_field_hotspot( campo = 'TEXTO' valor = 'TEXT_EDT' ).
    o_alv->set_field_input( 'DESCRIPCION,HTML,DEFECTO,ASUNTO,EMISOR,DESTINO,ADOBE,NOMBRE_ADJUNTO,NOMBRE_ADJUNTO2,EMAIL_PRUEBAS,AVISO_PRUEBAS' ).
    o_alv->set_field( campo = 'HTML,DEFECTO,AVISO_PRUEBAS' op = 'CHECKBOX' ).
    o_alv->set_field_text( 'GRUPO,CODIGO,ASUNTO,EMISOR,DESTINO,NOMBRE_ADJUNTO,NOMBRE_ADJUNTO2' ).
    o_alv->set_field_text( campo = 'TEXTO' valor = 'Cuerpo mail' ).
    o_alv->set_field_text( campo = 'ICO_ADJ' valor = 'Adj' valor2 = 'Adjunto' ).
    o_alv->set_field_text( campo = 'ICO_ADJ2' valor = 'Adj2' valor2 = 'Adjunto 2' ).
    o_alv->set_field_text( campo = 'EMAIL_PRUEBAS' valor = 'Email pruebas' valor2 = 'Email para pruebas en entorno no productivo' ).
    o_alv->set_field_text( campo = 'AVISO_PRUEBAS' valor = 'Aviso' valor2 = 'Aviso entorno no productivo' ).

    o_alv->add_filtro( campo = 'UPDKZ' valor = 'D' sign = 'E'  ).
    sgpi_texto( 'Generando informe' ).
    o_alv->show( CHANGING tabla = i_listado ).

    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).
  ENDMETHOD.

  METHOD command_dynpro_0100.
    DATA: l_hay_sel       TYPE c LENGTH 1,
          zap_textos_mail TYPE zap_textos_mail,
          l_xstring       TYPE xstring.
    DATA: l_key  TYPE c LENGTH 120,
          i_keys TYPE TABLE OF string.

    o_alv->comprobar_cambios( ).
    command_dynpro( EXPORTING o_alv = o_alv seleccion = 'TEST,TEST_PDF,OT,ADJ,ADJ2,BOR_ADJ,BOR_ADJ2'
                            CHANGING i_listado = i_listado i_listado_ini = i_listado_ini hay_sel = l_hay_sel ).

    CASE ucomm.
      WHEN 'GRABAR'.
        LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE updkz = 'D'.
          DELETE FROM zap_textos_mail WHERE grupo = <listado>-grupo AND codigo = <listado>-codigo AND version = <listado>-version AND spras = <listado>-spras.
          DELETE i_listado.
        ENDLOOP.
        LOOP AT i_listado ASSIGNING <listado> WHERE updkz <> ''.
          CLEAR zap_textos_mail.
          MOVE-CORRESPONDING <listado> TO zap_textos_mail.
          IF <listado>-updkz = 'U'.
            MODIFY zap_textos_mail FROM zap_textos_mail.
          ELSE.
            CLEAR <listado>-style.
            INSERT zap_textos_mail FROM zap_textos_mail.
          ENDIF.
        ENDLOOP.
        i_listado_ini = i_listado.
        MESSAGE 'Se han guardado los datos' TYPE 'S'.
        o_alv->refrescar_grid( soft_refresh = '' ).
      WHEN 'TEST'.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          zcl_ap_envio_mail=>mail( grupo = <listado>-grupo codigo = <listado>-codigo version = <listado>-version spras = <listado>-spras direccion = sy-uname popup = 'X' ).
        ENDLOOP.

      WHEN 'ADJ'.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          zcl_ap_ficheros=>leer_xstring( EXPORTING popup_select_fichero = 'X'
                                         IMPORTING xstring = l_xstring
                                                   fichero_salida = string ).
          IF NOT string IS INITIAL.
            <listado>-updkz          = 'U'.
            <listado>-binario        = l_xstring.
            <listado>-nombre_adjunto = zcl_ap_ficheros=>get_nombre_fichero( fichero = string con_extension = 'X' ).
            o_prog->validaciones( EXPORTING mod = 'X' CHANGING listado = <listado> ).
          ENDIF.
        ENDLOOP.
        o_alv->refrescar_grid( ).

      WHEN 'ADJ2'.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          zcl_ap_ficheros=>leer_xstring( EXPORTING popup_select_fichero = 'X'
                                         IMPORTING xstring = l_xstring
                                                   fichero_salida = string ).
          IF NOT string IS INITIAL.
            <listado>-updkz           = 'U'.
            <listado>-binario2        = l_xstring.
            <listado>-nombre_adjunto2 = zcl_ap_ficheros=>get_nombre_fichero( fichero = string con_extension = 'X' ).
            o_prog->validaciones( EXPORTING mod = 'X' CHANGING listado = <listado> ).
          ENDIF.
        ENDLOOP.
        o_alv->refrescar_grid( ).

      WHEN 'BOR_ADJ'.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          <listado>-updkz = 'U'.
          CLEAR: <listado>-binario, <listado>-nombre_adjunto, <listado>-ico_adj.
        ENDLOOP.
        o_alv->refrescar_grid( ).

      WHEN 'BOR_ADJ2'.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          <listado>-updkz = 'U'.
          CLEAR: <listado>-binario2, <listado>-nombre_adjunto2, <listado>-ico_adj2.
        ENDLOOP.
        o_alv->refrescar_grid( ).

      WHEN 'OT'.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          l_key(3) = sy-mandt.
          l_key+3(30) = <listado>-grupo.
          l_key+33(15) = <listado>-codigo.
          l_key+48(2) = <listado>-version.
          l_key+50(1) = <listado>-spras.
          APPEND l_key TO i_keys.
        ENDLOOP.

        zcl_ap_utils=>grabar_tabla_en_ot( tabla = 'ZAP_TEXTOS_MAIL' i_keys = i_keys ).
    ENDCASE.
  ENDMETHOD.

  METHOD validaciones.
    CLEAR: listado-message, listado-style, listado-color, listado-lights.

    IF mod = 'X'.
      IF listado-updkz IS INITIAL.
        listado-updkz = 'U'.
      ENDIF.
    ENDIF.

    IF NOT listado-binario IS INITIAL.
      aux1 = zcl_ap_ficheros=>get_extension( fichero = listado-nombre_adjunto ).
      aux1 = to_upper( aux1 ).
      CASE aux1(3).
        WHEN 'PDF'. listado-ico_adj = icon_pdf.
        WHEN 'DOC'. listado-ico_adj = icon_doc.
        WHEN OTHERS. listado-ico_adj = icon_jpg.
      ENDCASE.
    ENDIF.

    IF NOT listado-binario2 IS INITIAL.
      aux1 = zcl_ap_ficheros=>get_extension( fichero = listado-nombre_adjunto2 ).
      aux1 = to_upper( aux1 ).
      CASE aux1(3).
        WHEN 'PDF'. listado-ico_adj2 = icon_pdf.
        WHEN 'DOC'. listado-ico_adj2 = icon_doc.
        WHEN OTHERS. listado-ico_adj2 = icon_jpg.
      ENDCASE.
    ENDIF.

    set_status_list( EXPORTING message = listado-message criterio = 'V' CHANGING list = listado ).
  ENDMETHOD.

  METHOD enviar_mail.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  o_prog = NEW #( status       = 'INICIO'
                  guardar_logz = 'X'
                  status_prog  = 'ZAP_STATUS' ).

  o_prog->initialization_i( ).

  IF sy-batch IS INITIAL.
    o_prog->o_event = NEW #( boton_refrescar = 'X'
                             boton_excel     = 'Y'
                             boton_nuevo     = 'X'
                             boton_borrar    = 'X'
                             o_prog          = o_prog ).
    o_prog->o_event->campo_updkz = 'UPDKZ'.
    o_prog->o_alv = NEW #( estructura = ''
                           o_event    = o_prog->o_event ).

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_grupo-low.
  DATA(o_mc_g) = NEW zcl_ap_matchcode_z( tabname = 'ZAP_TEXTOS_MAIL' ).
  o_mc_g->add_field( field = 'GRUPO' selectflag = 'X' ).
  SELECT DISTINCT grupo FROM zap_textos_mail                "#EC *
    INTO TABLE @DATA(i_grupos).

  LOOP AT i_grupos ASSIGNING FIELD-SYMBOL(<grupo>).
    o_mc_g->add_valor( <grupo> ).
  ENDLOOP.

  o_mc_g->matchcode( EXPORTING field   = 'GRUPO'
                      CHANGING  valor   = s_grupo-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_codigo-low.
  DATA(o_mc_c) = NEW zcl_ap_matchcode_z( tabname = 'ZAP_TEXTOS_MAIL' ).
  o_mc_c->add_field( field = 'CODIGO' selectflag = 'X' ).
  o_mc_c->add_field( field = 'DESCRIPCION' ).
  SELECT DISTINCT codigo, descripcion FROM zap_textos_mail
    INTO TABLE @DATA(i_cod)
   WHERE grupo IN @s_grupo.

  LOOP AT i_cod ASSIGNING FIELD-SYMBOL(<cod>).
    o_mc_c->add_valor( <cod>-codigo ).
    o_mc_c->add_valor( <cod>-descripcion ).
  ENDLOOP.

  o_mc_c->matchcode( EXPORTING field   = 'CODIGO'
                      CHANGING  valor   = s_codigo-low ).

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'ONLI'.
      o_prog->validar_seleccion_obligatoria( campos_or = '*' msgty = 'W' ).
    WHEN OTHERS.
      o_prog->at_selection( ).
  ENDCASE.


*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  o_prog->buscar_datos( ).

  IF sy-batch IS INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE 'Este programa no se puede ejecutar en fondo'(pnf) TYPE 'E'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  o_prog->status_dynpro_0100( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  o_prog->command_dynpro_0100( ).


ENDMODULE.
