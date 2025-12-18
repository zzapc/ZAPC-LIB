*----------------------------------------------------------------------*
***INCLUDE LZAP_POPUPO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: i_bot TYPE TABLE OF string WITH HEADER LINE,
        i_opc TYPE TABLE OF string WITH HEADER LINE,
        l_o   TYPE string,
        l_v   TYPE string,
        BEGIN OF i_bot_par OCCURS 0,
          b TYPE sy-ucomm,
          t TYPE smp_dyntxt-text,
          i TYPE smp_dyntxt-icon_id,
          q TYPE smp_dyntxt-quickinfo,
        END OF i_bot_par,
        BEGIN OF i_opciones OCCURS 0,
          opcion TYPE string,
          valor  TYPE string,
        END OF i_opciones,
        l_ancho_optimizado        TYPE abap_bool,
        l_botones_estandard       TYPE abap_bool,
        l_check_editable          TYPE abap_bool,
        l_campos_checkbox         TYPE string,
        l_campos_input_no_cero    TYPE abap_bool,
        l_ocultar_columnas_vacias TYPE abap_bool,
        l_campo_color             TYPE lvc_fname,
        l_handle                  TYPE slis_handl,
        l_campos_no_cero          TYPE string.

  SET PF-STATUS 'ST_POPUP_DYN' OF PROGRAM 'ZAP_STATUS'.

  SET TITLEBAR '001' WITH v_titulo.

  PERFORM campo_usuario USING 1 CHANGING svald1.
  zcl_ap_dynpro=>screen_visible( campo = 'SVALD1-KEYTEXT' variable = v_campo_usuario1 input = '0' ).
  zcl_ap_dynpro=>screen_visible( campo = 'SVALD1-VALUE' variable = v_campo_usuario1 input = '1' ).

  PERFORM campo_usuario USING 2 CHANGING svald2.
  zcl_ap_dynpro=>screen_visible( campo = 'SVALD2-KEYTEXT' variable = v_campo_usuario2 input = '0' ).
  zcl_ap_dynpro=>screen_visible( campo = 'SVALD2-VALUE' variable = v_campo_usuario2 input = '1' ).

  PERFORM campo_usuario USING 3 CHANGING svald3.
  zcl_ap_dynpro=>screen_visible( campo = 'SVALD3-KEYTEXT' variable = v_campo_usuario3 input = '0' ).
  zcl_ap_dynpro=>screen_visible( campo = 'SVALD3-VALUE' variable = v_campo_usuario3 input = '1' ).

  IF v_inicio IS INITIAL.
    l_ancho_optimizado = 'X'.
    if v_opciones cs '&&'.
      SPLIT v_opciones AT '&&' INTO TABLE i_opc.
    else.
      SPLIT v_opciones AT ',' INTO TABLE i_opc.
    endif.
    LOOP AT i_opc.
      SPLIT i_opc AT '=' INTO i_opciones-opcion i_opciones-valor.
      APPEND i_opciones.

      CASE i_opciones-opcion.
        WHEN 'ANCHO_OPTIMIZADO'.
          l_ancho_optimizado = i_opciones-valor.
        WHEN 'BOTONES_ESTANDARD'.
          l_botones_estandard = i_opciones-valor.
        WHEN 'CHECK_EDITABLE'.
          l_check_editable = i_opciones-valor.
        WHEN 'CHECKBOX'.
          __add_lista l_campos_checkbox i_opciones-valor.
        WHEN 'CAMPOS_INPUT_NO_CERO'.
          l_campos_input_no_cero = 'X'.
        WHEN 'OCULTAR_COLUMNAS_VACIAS'.
          l_ocultar_columnas_vacias = 'X'.
        WHEN 'CAMPO_COLOR'.
          l_campo_color = i_opciones-valor.
        WHEN 'HANDLE'.
          l_handle = i_opciones-valor.
        WHEN 'CAMPOS_NO_CERO'.
          l_campos_no_cero = i_opciones-valor.
        WHEN 'FICHERO_EXCEL'.
          v_fichero_excel = i_opciones-valor.
      ENDCASE.
    ENDLOOP.
    REFRESH i_opc.

    IF l_check_editable = 'X'.
      v_controlar_cambios = 'X'.
    ENDIF.

    v_inicio = 'X'.
    LOOP AT SCREEN.
      IF screen-name = 'V_TEXTO' OR screen-name = 'V_TEXTO2'.
        screen-length = v_max_ancho_txt.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    zcl_ap_dynpro=>screen_visible( campo = 'V_TEXTO' variable = v_texto input = '0' ).
    zcl_ap_dynpro=>screen_visible( campo = 'V_TEXTO2' variable = v_texto2 input = '0' ).


    DATA l_aux3(3).
    l_aux3 = v_texto2.
    IF l_aux3 = '!#!'.
      v_texto2 = v_texto2+3.
      LOOP AT SCREEN.
        IF screen-name = 'V_TEXTO2'.
          screen-intensified = 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF v_botones(1) = '$'.
      REFRESH i_bot_par.
* Ejemplo '$B=F01&&T=Aceptar&&I=@0V@||B=F02&&T=Borrar&&I=@11@'
      SPLIT v_botones+1 AT '||' INTO TABLE i_bot.
      LOOP AT i_bot.
        SPLIT i_bot AT '&&' INTO TABLE i_opc.
        CLEAR: i_bot_par.
        LOOP AT i_opc.
          SPLIT i_opc AT '=' INTO l_o l_v.
          IF NOT l_o IS INITIAL.
            TRANSLATE l_o TO UPPER CASE.
            CASE l_o(1).
              WHEN 'B'. i_bot_par-b = l_v.
              WHEN 'T'. i_bot_par-t = l_v.
              WHEN 'I'. i_bot_par-i = l_v.
              WHEN 'Q'. i_bot_par-q = l_v.
              WHEN 'S'. IF l_v = 'X'. __add_lista v_validar_seleccion i_bot_par-b. ENDIF.
            ENDCASE.
          ENDIF.
        ENDLOOP.
        APPEND i_bot_par.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF v_grid IS INITIAL.
    IF o_alv_popup IS INITIAL.
      CREATE OBJECT o_alv_popup
        EXPORTING
          tabla = ''.

      IF v_botones(1) = '$'.
        LOOP AT i_bot_par.
          o_alv_popup->add_button( button = i_bot_par-b text = i_bot_par-t icon = i_bot_par-i qinfo = i_bot_par-q ).
        ENDLOOP.
      ELSE.
        IF v_botones CS 'OK'.
          o_alv_popup->add_button( button = 'F01' text = 'Aceptar' icon = '@0V@' qinfo = 'Aceptar' ).
        ENDIF.
        IF v_botones CS 'PRINT'.
          o_alv_popup->add_button( button = 'F02' text = 'Imprimir' icon = '@0X@' qinfo = 'Imprimir' ).
        ENDIF.
        IF v_botones CS 'CANCEL'.
          o_alv_popup->add_button( button = 'F03' text = 'Cancelar' icon = '@0W@' qinfo = 'Cancelar' ).
        ENDIF.
      ENDIF.


      DATA(l_no_layout) = SWITCH xfeld( l_handle WHEN '' THEN 'X' ELSE '' ).
      IF v_check = 'X'.
        IF l_handle IS INITIAL.
          l_handle = sy-cprog.
        ENDIF.

        o_alv_popup->constructor_tabla( EXPORTING campo_check        = 'CHECK'
                                                  sel                = 'M'
                                                  no_layout          = l_no_layout
                                                  cprog              = 'SAPZAP_POPUP'
*            status           = 'ST_DYN'
*            status_prog      = 'ZAP_STATUS'
                                                  container_name     = 'CALV'
                                                  color              = l_campo_color
                                                  botones_standard   = l_botones_estandard
                                                  restriccion_layout = if_salv_c_layout=>restrict_user_dependant
                                                  handle             = l_handle
                                        CHANGING  t_tabla            = <tabla> ).

        o_alv_popup->set_field_noout( 'CHECK' ).
        o_alv_popup->set_seleccion( CHANGING t_tabla = <tabla> ).
      ELSE.
        o_alv_popup->constructor_tabla( EXPORTING no_layout          = l_no_layout
                                                  cprog              = 'SAPZAP_POPUP'
*            status           = 'ST_DYN'
*            status_prog      = 'ZAP_STATUS'
                                                  container_name     = 'CALV'
                                                  color              = l_campo_color
                                                  botones_standard   = l_botones_estandard
                                                  restriccion_layout = if_salv_c_layout=>restrict_user_dependant
                                                  handle             = l_handle
                                        CHANGING  t_tabla            = <tabla> ).
      ENDIF.
      IF NOT v_campos_noout IS INITIAL.
        o_alv_popup->set_field_noout( v_campos_noout ).
      ENDIF.

      IF NOT v_campos_sum IS INITIAL.
        o_alv_popup->set_agregacion( v_campos_sum ).
      ENDIF.

      SPLIT v_campos_texto AT '&&' INTO TABLE i_opc.
      LOOP AT i_opc.
        SPLIT i_opc AT '=' INTO l_o l_v.
        IF NOT l_v IS INITIAL.
          o_alv_popup->set_field_text( campo = l_o valor = l_v ).
        ENDIF.
      ENDLOOP.

      IF NOT v_campos_hotspot IS INITIAL.
        IF v_campos_hotspot CS '('.
          DATA(i_hotspots) = zcl_ap_lista=>get_valores_lista( v_campos_hotspot ).
          LOOP AT i_hotspots ASSIGNING FIELD-SYMBOL(<hotspot>).
            o_alv_popup->set_field_hotspot( campo = <hotspot>-valor valor = <hotspot>-subvalor ).
          ENDLOOP.
        ELSE.
          o_alv_popup->set_field_hotspot( campo = v_campos_hotspot auto = 'X' ).
        ENDIF.
      ENDIF.

      IF NOT v_alv_helper IS INITIAL.
        o_alv_popup->o_alv_helper = v_alv_helper.
      ENDIF.

      IF NOT v_campos_orden IS INITIAL.
        o_alv_popup->set_orden( v_campos_orden ).
      ENDIF.

      IF NOT l_campos_no_cero IS INITIAL.
        o_alv_popup->set_field( campo = l_campos_no_cero op = 'NO_CERO' ).
      ENDIF.

      IF l_ocultar_columnas_vacias = 'X'.
        o_alv_popup->ocultar_columnas_vacias( t_tabla = <tabla> ).
      ENDIF.

      o_alv_popup->show( ).

    ELSE.
      o_alv_popup->refresh( ).
    ENDIF.
  ELSE.
    IF o_grid_popup IS INITIAL.
      CREATE OBJECT o_event_popup.

      CREATE OBJECT o_grid_popup
        EXPORTING
          estructura     = ''
          o_event        = o_event_popup
          obj_contenedor = 'CALV'
          nombre_layout  = 'Z_POPUP_ALV_AP'.

      IF v_botones(1) = '$'.
        LOOP AT i_bot_par.
          o_grid_popup->add_button( button = i_bot_par-b text = i_bot_par-t icon = i_bot_par-i qinfo = i_bot_par-q ).
        ENDLOOP.
      ELSE.
        IF v_botones CS 'OK'.
          o_grid_popup->add_button( button = 'F01' text = 'Aceptar' icon = '@0V@' qinfo = 'Aceptar' ).
        ENDIF.
        IF v_botones CS 'PRINT'.
          o_grid_popup->add_button( button = 'F02' text = 'Imprimir' icon = '@0X@' qinfo = 'Imprimir' ).
        ENDIF.
        IF v_botones CS 'CANCEL'.
          o_grid_popup->add_button( button = 'F03' text = 'Cancelar' icon = '@0W@' qinfo = 'Cancelar' ).
        ENDIF.
      ENDIF.

      o_grid_popup->set_campos_tabint( <tabla> ).

      DATA(l_input) = ''.
      IF l_check_editable = 'X'.
        CLEAR v_check.
        l_input = 'X'.
        o_grid_popup->set_field_input( 'CHECK' ).
        o_grid_popup->set_field( campo = 'CHECK' op = 'CHECKBOX' ).
      ENDIF.

      IF NOT l_campos_checkbox IS INITIAL.
        o_grid_popup->set_field( campo = l_campos_checkbox op = 'CHECKBOX' ).
      ENDIF.

      IF NOT v_campos_noout IS INITIAL.
        o_grid_popup->set_field_noout( v_campos_noout ).
      ENDIF.

      IF NOT v_campos_sum IS INITIAL.
        o_grid_popup->set_field( campo = v_campos_sum op = 'SUM' ).
      ENDIF.

      IF NOT v_campos_hotspot IS INITIAL.
        o_grid_popup->set_field_hotspot( campo = v_campos_hotspot auto = 'X' ).
      ENDIF.

      SPLIT v_campos_texto AT '&&' INTO TABLE i_opc.
      LOOP AT i_opc.
        SPLIT i_opc AT '=' INTO l_o l_v.
        IF NOT l_v IS INITIAL.
          o_grid_popup->set_field_text( campo = l_o valor = l_v ).
        ENDIF.
      ENDLOOP.

      IF NOT v_campos_input IS INITIAL.
        l_input = 'X'.
        o_grid_popup->set_field_input( v_campos_input ).
        IF l_campos_input_no_cero = 'X'.
          o_grid_popup->set_field( campo = v_campos_input op = 'NO_CERO' ).
        ENDIF.
      ENDIF.

      IF NOT v_alv_helper IS INITIAL.
        o_grid_popup->o_alv_helper = v_alv_helper.
      ENDIF.

      IF NOT v_campos_orden IS INITIAL.
        o_grid_popup->set_orden( v_campos_orden ).
      ENDIF.

      IF v_check IS INITIAL.
        DATA l_sel_mode .
        l_sel_mode = 'N'.
      ELSE.
        l_sel_mode = 'D'.
      ENDIF.
      IF l_botones_estandard IS INITIAL.
        o_grid_popup->set_layout( no_toolbar = 'X' sel_mode = l_sel_mode ancho_optimizado = l_ancho_optimizado input = l_input ).
      ELSE.
        o_grid_popup->set_layout( no_toolbar = '' sel_mode = l_sel_mode ancho_optimizado = l_ancho_optimizado input = l_input ).
        o_grid_popup->quitar_botones_insercion( ).
      ENDIF.

      o_grid_popup->show( CHANGING tabla = <tabla> ).

      o_grid_popup->actualiza_campos_grid( campos_no_opt = v_campos_input ).

    ELSE.
      o_grid_popup->refrescar_grid( ).
    ENDIF.
  ENDIF.


ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'ST_POPUP_DYN' OF PROGRAM 'ZAP_STATUS'.

  SET TITLEBAR '001' WITH v_titulo.

  zcl_ap_dynpro=>screen_input( campo = 'TVARVC-LOW' variable = v_edit ).
  PERFORM remove_button IN PROGRAM zap_status USING 'F02'.
  PERFORM add_button IN PROGRAM zap_status USING 'F03' 'Cancelar' icon_cancel 'Cancelar'.
  IF v_edit = 'X'.
    PERFORM add_button IN PROGRAM zap_status USING 'F01' 'Aceptar' icon_okay 'Aceptar cambios'.
  ELSE.
    PERFORM remove_button IN PROGRAM zap_status USING 'F01'.
    IF v_editable = 'X'.
      PERFORM add_button IN PROGRAM zap_status USING 'F02' 'Modificar' icon_change 'Modificar valor'.
    ENDIF.
  ENDIF.

ENDMODULE.
