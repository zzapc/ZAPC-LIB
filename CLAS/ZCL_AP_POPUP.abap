*/---------------------------------------------------------------------\
*|   Development's Control                                             |
*|                                                                     |
*|   ZDEV_CONTROL is free software; you can redistribute it and/or     |
*|   modify it under the terms of the GNU General Public License as    |
*|   publishedby the Free Software Foundation; either version 2 of the |
*|   License, or (at your option) any later version.                   |
*|                                                                     |
*|   SAPlink is distributed in the hope that it will be useful,        |
*|   but WITHOUT ANY WARRANTY; without even the implied warranty of    |
*|   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |
*|   GNU General Public License for more details.                      |
*|                                                                     |
*\---------------------------------------------------------------------/
*| Lead Developers : Andrés Picazo       apicazo@gmail.com             |
*|---------------------------------------------------------------------|
*| For more information visit:                                         |
*|                                                                     |
*| project homepage: https://cw.sdn.sap.com/cw/groups/zdev_control     |
*\---------------------------------------------------------------------/
CLASS zcl_ap_popup DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_log_simple,
        lights  TYPE zico_estado_mensaje,
        clave   TYPE string,
        message TYPE bapi_msg,
      END OF t_log_simple.
    TYPES tt_log_simple TYPE STANDARD TABLE OF t_log_simple WITH NON-UNIQUE KEY clave.

    DATA i_errores    TYPE ztab_ap_errores.
    DATA i_popup      TYPE zt_parametros.
    DATA o_log        TYPE REF TO zcl_ap_log.
    DATA i_log_simple TYPE tt_log_simple.

    METHODS constructor
      IMPORTING o_log TYPE REF TO zcl_ap_log OPTIONAL.

    CLASS-METHODS confirmar
      IMPORTING titulo           TYPE any       OPTIONAL
                texto            TYPE any       OPTIONAL
                texto2           TYPE any       OPTIONAL
                opcion           TYPE c         DEFAULT 'J'
                start_col        TYPE sy-cucol  DEFAULT 25
                start_row        TYPE sy-curow  DEFAULT 6
                cancel_display   TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER texto
      RETURNING VALUE(respuesta) TYPE abap_bool.

    CLASS-METHODS informar
      IMPORTING titulo TYPE any DEFAULT 'Información'
                texto1 TYPE any OPTIONAL
                texto2 TYPE any DEFAULT ''
                texto3 TYPE any DEFAULT ''
                texto4 TYPE any DEFAULT ''
      PREFERRED PARAMETER texto1.

    CLASS-METHODS popup_usuario
      IMPORTING campo1       TYPE any
                campo2       TYPE any OPTIONAL
                campo3       TYPE any OPTIONAL
                campo4       TYPE any OPTIONAL
                titulo       TYPE any OPTIONAL
                start_column TYPE any DEFAULT '5'
                start_row    TYPE any DEFAULT '5'
      EXPORTING !return      TYPE any
      CHANGING  valor1       TYPE any OPTIONAL
                valor2       TYPE any OPTIONAL
                valor3       TYPE any OPTIONAL
                valor4       TYPE any OPTIONAL.

    CLASS-METHODS confirmar_2
      IMPORTING titulo           TYPE any       OPTIONAL
                texto            TYPE any       OPTIONAL
                texto2           TYPE any       OPTIONAL
                opcion           TYPE c         DEFAULT 'Y'
                texto3           TYPE any       OPTIONAL
                linea1           TYPE any       OPTIONAL
                linea2           TYPE any       OPTIONAL
                cancel_display   TYPE abap_bool DEFAULT 'X'
      PREFERRED PARAMETER texto
      RETURNING VALUE(respuesta) TYPE abap_bool.

    CLASS-METHODS list_dynpro
      IMPORTING campo        TYPE any
                valores      TYPE tpda_vrm_values OPTIONAL
                dominio      TYPE any             OPTIONAL
                tabla        TYPE any             OPTIONAL
                !key         TYPE any             OPTIONAL
                !text        TYPE any             OPTIONAL
                campo_idioma TYPE any             DEFAULT ''
                !where       TYPE any             DEFAULT ''.

    METHODS add_message
      IMPORTING msgid       TYPE any       DEFAULT '00'
                msgty       TYPE any       DEFAULT 'E'
                msgno       TYPE any       DEFAULT '398'
                msgv1       TYPE any       OPTIONAL
                msgv2       TYPE any       OPTIONAL
                msgv3       TYPE any       OPTIONAL
                msgv4       TYPE any       OPTIONAL
                !message    TYPE any       OPTIONAL
                !color      TYPE any       DEFAULT ''
                !string     TYPE any       DEFAULT ''
                trim_spaces TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER message.

    METHODS show_errores
      IMPORTING show_msgv1 TYPE abap_bool DEFAULT ''
                show_msgv2 TYPE abap_bool DEFAULT ''
                titulo     TYPE any       DEFAULT ''.

    METHODS add_message_syst
      IMPORTING msgid    TYPE any DEFAULT sy-msgid
                msgty    TYPE any DEFAULT sy-msgty
                msgno    TYPE any DEFAULT sy-msgno
                msgv1    TYPE any DEFAULT sy-msgv1
                msgv2    TYPE any DEFAULT sy-msgv2
                msgv3    TYPE any DEFAULT sy-msgv3
                msgv4    TYPE any DEFAULT sy-msgv4
                !message TYPE any DEFAULT ''.

    METHODS add_message_bapireturn1
      IMPORTING message1 TYPE bapireturn1 OPTIONAL.

    METHODS add_message_bapiret2
      IMPORTING message2 TYPE bapiret2 OPTIONAL.

    METHODS clear_messages.

    METHODS add_message_bapiret2_t
      IMPORTING tabla TYPE bapiret2_t OPTIONAL.

    CLASS-METHODS popup_campos_estructura
      IMPORTING estructura     TYPE any
                ini_campos_no  TYPE string DEFAULT ''
                ini_campos_no2 TYPE string DEFAULT ''
      CHANGING  r_campos       TYPE roij_fieldname_rtab.

    METHODS free.

    METHODS set_objeto
      IMPORTING objeto TYPE any.

    METHODS add_datos_popup
      IMPORTING campo       TYPE any
                valor       TYPE any OPTIONAL
                descripcion TYPE any OPTIONAL.

    METHODS show_popup
      IMPORTING texto             TYPE any       DEFAULT ''
                titulo            TYPE any       DEFAULT ''
                ocultar_col_valor TYPE abap_bool DEFAULT ''
      RETURNING VALUE(ok)         TYPE abap_bool.

    METHODS add_message_i
      IMPORTING msgid    TYPE any DEFAULT '00'
                msgty    TYPE any DEFAULT 'I'
                msgno    TYPE any DEFAULT '398'
                msgv1    TYPE any OPTIONAL
                msgv2    TYPE any OPTIONAL
                msgv3    TYPE any OPTIONAL
                msgv4    TYPE any OPTIONAL
                !message TYPE any OPTIONAL
                !color   TYPE any DEFAULT ''
      PREFERRED PARAMETER message.

    METHODS guardar_log
      IMPORTING proceso TYPE any
                msgv1   TYPE any DEFAULT ''
                clave   TYPE any DEFAULT ''.

    METHODS enviar_log_por_mail
      IMPORTING info                    TYPE abap_bool DEFAULT ''
                errores                 TYPE abap_bool DEFAULT 'X'
                warnings                TYPE abap_bool DEFAULT 'X'
                titulo                  TYPE any
                destinatario            TYPE any
                aviso_usuario_online    TYPE abap_bool DEFAULT ''
                log_completo_en_adjunto TYPE abap_bool DEFAULT ''
                !object                 TYPE any       DEFAULT 'LOG'
                clave                   TYPE any       DEFAULT ''.

    CLASS-METHODS lista_opciones
      IMPORTING titulo           TYPE any            OPTIONAL
                texto            TYPE any            OPTIONAL
                texto2           TYPE any            OPTIONAL
                start_col        TYPE sy-cucol       DEFAULT 25
                start_row        TYPE sy-curow       DEFAULT 6
                i_lista          TYPE ccseq_t_values OPTIONAL
                lista            TYPE any            DEFAULT ''
      PREFERRED PARAMETER texto
      RETURNING VALUE(respuesta) TYPE spopli-varoption.

    CLASS-METHODS confirmar_grabacion
      IMPORTING titulo           TYPE any       DEFAULT 'Datos sin guardar'
                texto            TYPE any       DEFAULT 'Existen datos no grabados'
                texto2           TYPE any       DEFAULT '¿Está seguro de salir sin guardar?'
                opcion           TYPE c         DEFAULT 'N'
                start_col        TYPE sy-cucol  DEFAULT 25
                start_row        TYPE sy-curow  DEFAULT 6
                cancel_display   TYPE abap_bool DEFAULT ''
      RETURNING VALUE(respuesta) TYPE abap_bool.

    METHODS get_last_message
      IMPORTING !type          TYPE any DEFAULT 'E'
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS edit_text
      IMPORTING titulo       TYPE any DEFAULT 'Edición de texto'
                texto        TYPE any DEFAULT ''
                max_cols     TYPE i   DEFAULT 132
                display_mode TYPE any DEFAULT ''
      EXPORTING modificado   TYPE abap_bool
      CHANGING  !string      TYPE any.

    CLASS-METHODS popup_fecha
      IMPORTING titulo           TYPE any  OPTIONAL
                texto            TYPE any  OPTIONAL
                texto2           TYPE any  OPTIONAL
                fecha            TYPE dats DEFAULT sy-datum
      RETURNING VALUE(fecha_sel) TYPE dats.

    CLASS-METHODS popup_bapiret2
      IMPORTING i_return TYPE bapirettab
                msgv1    TYPE any      OPTIONAL
                msgv2    TYPE any      OPTIONAL
                msgv3    TYPE any      OPTIONAL
                msgv4    TYPE any      OPTIONAL
                msgty    TYPE sy-msgty DEFAULT 'E'
                msgid    TYPE sy-msgid DEFAULT '00'
                msgno    TYPE sy-msgno DEFAULT '398'.

    CLASS-METHODS gui_disponible
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS popup_permitido
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS popup_2_opciones
      IMPORTING titulo           TYPE any       DEFAULT ''
                texto            TYPE any       OPTIONAL
                texto2           TYPE any       DEFAULT ''
                opcion1          TYPE any       OPTIONAL
                start_col        TYPE sy-cucol  DEFAULT 25
                start_row        TYPE sy-curow  DEFAULT 6
                cancel_display   TYPE abap_bool DEFAULT ''
                texto3           TYPE any       DEFAULT ''
                defaultoption    TYPE char1     DEFAULT '1'
                opcion2          TYPE any       OPTIONAL
                linea1           TYPE any       DEFAULT ''
                linea2           TYPE any       DEFAULT ''
                linea3           TYPE any       DEFAULT ''
      RETURNING VALUE(respuesta) TYPE char1.

    METHODS add_log
      IMPORTING clave               TYPE string     OPTIONAL
                return2             TYPE bapiret2   OPTIONAL
                i_return2           TYPE bapiret2_t OPTIONAL
                !type               TYPE sy-msgty   OPTIONAL
                !message            TYPE bapi_msg   OPTIONAL
                quitar_msg_tecnicos TYPE abap_bool  DEFAULT ''
      RETURNING VALUE(log)          TYPE t_log_simple.

    METHODS add_log_simple
      IMPORTING clave      TYPE string
                !type      TYPE sy-msgty OPTIONAL
                icono      TYPE icon_d   OPTIONAL
                !message   TYPE bapi_msg
      RETURNING VALUE(log) TYPE t_log_simple.

    METHODS show_log
      IMPORTING titulo              TYPE string    DEFAULT ''
                campos_texto        TYPE string    DEFAULT ''
                quitar_msg_tecnicos TYPE abap_bool DEFAULT ''.

    CLASS-METHODS popup_fechas
      IMPORTING begda            TYPE dats DEFAULT sy-datum
                endda            TYPE dats DEFAULT sy-datum
      EXPORTING VALUE(begda_sel) TYPE dats
                VALUE(endda_sel) TYPE dats
                cancelado        TYPE abap_bool.

    CLASS-METHODS popup_alv_ok_cancel
      IMPORTING titulo       TYPE any OPTIONAL
                texto        TYPE any OPTIONAL
                texto2       TYPE any OPTIONAL
                tabla        TYPE table
                campos_noout TYPE any OPTIONAL
                campos_texto TYPE any OPTIONAL
                campos_sum   TYPE any OPTIONAL
                campos_orden TYPE any OPTIONAL
                ancho        TYPE i DEFAULT 120
                botones      TYPE any DEFAULT 'OK_CANCEL'
      RETURNING VALUE(ok)    TYPE abap_bool.

    CLASS-METHODS popup_alv_seleccion
      IMPORTING titulo       TYPE any OPTIONAL
                texto        TYPE any OPTIONAL
                texto2       TYPE any OPTIONAL
                campos_noout TYPE any OPTIONAL
                campos_texto TYPE any OPTIONAL
                campos_sum   TYPE any OPTIONAL
                campos_orden TYPE any OPTIONAL
                ancho        TYPE i DEFAULT 120
                botones      TYPE any DEFAULT 'OK_CANCEL'
                solo_una     TYPE abap_bool
                obligatoria  TYPE abap_bool
      EXPORTING registro     TYPE any
                cancelado    TYPE abap_bool
      CHANGING  tabla        TYPE table.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA objeto TYPE zest_ap_errores-objeto.
endclass. "ZCL_AP_POPUP definition
class ZCL_AP_POPUP implementation.
  METHOD add_datos_popup.
    DATA l_popup TYPE zv_parametros.

    l_popup-atributo1 = campo.
    WRITE valor TO l_popup-atributo2. "#EC *
    l_popup-comentario = descripcion.
    APPEND l_popup TO i_popup.
  ENDMETHOD.
  METHOD add_log.
    IF NOT return2 IS INITIAL.
      add_log_simple( type = return2-type
                      clave = clave
                      message = return2-message ).
    ENDIF.

    DATA(li_return2) = i_return2.
    IF quitar_msg_tecnicos = 'X'.
      IF line_exists( i_return2[ id = 'V4' number = '233' ] ).
        LOOP AT i_return2 TRANSPORTING NO FIELDS WHERE NOT ( id = 'V4' AND number = '233' ). "#EC PREF_LINE_EX
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          DELETE li_return2 WHERE id = 'V4' AND number = '233'.
        ENDIF.
      ENDIF.
    ENDIF.
    LOOP AT li_return2 ASSIGNING FIELD-SYMBOL(<return2>).
      add_log_simple( type = <return2>-type
                      clave = clave
                      message = <return2>-message ).
    ENDLOOP.

    IF NOT message IS INITIAL.
      add_log_simple( type = type
                      clave = clave
                      message = message  ).
    ENDIF.
  ENDMETHOD.
  METHOD add_log_simple.
    CLEAR log.
    log-clave   = clave.
    log-message = message.
    IF NOT icono IS INITIAL.
      DATA(l_icono) = icono.
    ELSEIF NOT type IS INITIAL.
      CASE type.
        WHEN 'I' OR 'S'.
          l_icono = icon_green_light.
        WHEN 'W'.
          l_icono = icon_yellow_light.
        WHEN 'E'.
          l_icono = icon_red_light.
        WHEN 'A'.
          l_icono = icon_message_critical.
        WHEN OTHERS.
          l_icono = icon_question.
      ENDCASE.
    ENDIF.

    IF NOT l_icono IS INITIAL.
      log-lights = zcl_ap_alv=>set_icono( icono = l_icono mensaje = message ).
    ENDIF.

    APPEND log TO i_log_simple.

    IF NOT o_log IS INITIAL.
      o_log->log( clave = clave msgty = type message = message ).
    ENDIF.
  ENDMETHOD.
  METHOD add_message.
    DATA: l_error  TYPE zest_ap_errores,
          l_cadena TYPE c LENGTH 160.

    CLEAR l_error.
    l_error-msgty = msgty.
    l_error-msgid = msgid.
    l_error-msgno = msgno.
    l_error-msgv1 = msgv1.
    l_error-msgv2 = msgv2.
    l_error-msgv3 = msgv3.
    l_error-msgv4 = msgv4.

    IF NOT string IS INITIAL.
      l_cadena = string.
      l_error-msgv1 = l_cadena(40).
      l_error-msgv2 = l_cadena+40(40).
      l_error-msgv3 = l_cadena+80(40).
      l_error-msgv4 = l_cadena+120(40).
    ENDIF.
    l_error-message = message.
    l_error-color   = color.
    l_error-objeto  = objeto.

    CASE msgty.
      WHEN 'I' OR 'S'.
        l_error-lights = zcl_ap_alv=>c_sem_verde.
      WHEN 'W'.
        l_error-lights = zcl_ap_alv=>c_sem_ambar.
      WHEN '?'.
        CLEAR l_error-lights.
      WHEN OTHERS.
        l_error-lights = zcl_ap_alv=>c_sem_rojo.
    ENDCASE.

    IF         l_error-message IS INITIAL
       AND NOT l_error-msgno   IS INITIAL
       AND NOT l_error-msgid   IS INITIAL.
      IF trim_spaces IS NOT INITIAL.
        DATA(l_msgv1) = l_error-msgv1.
        DATA(l_msgv2) = l_error-msgv2.
        DATA(l_msgv3) = l_error-msgv3.
        DATA(l_msgv4) = l_error-msgv4.
        CONDENSE: l_msgv1, l_msgv2, l_msgv3, l_msgv4.
        MESSAGE ID l_error-msgid TYPE 'S' NUMBER l_error-msgno
                WITH l_msgv1 l_msgv2 l_msgv3 l_msgv4
                INTO l_error-message.
      ELSE.
        MESSAGE ID l_error-msgid TYPE 'S' NUMBER l_error-msgno
                WITH l_error-msgv1 l_error-msgv2 l_error-msgv3 l_error-msgv4
                INTO l_error-message.
      ENDIF.
    ENDIF.

    APPEND l_error TO i_errores.
  ENDMETHOD.
  METHOD add_message_bapiret2.
    add_message( msgid = message2-id
                 msgty = message2-type
                 msgno = message2-number
                 msgv1 = message2-message_v1
                 msgv2 = message2-message_v2
                 msgv3 = message2-message_v3
                 msgv4 = message2-message_v4
                 message = message2-message ).
  ENDMETHOD.
  METHOD add_message_bapiret2_t.
    DATA l_bapiret2 TYPE bapiret2.

    LOOP AT tabla INTO l_bapiret2.
      add_message_bapiret2( l_bapiret2 ).
    ENDLOOP.
  ENDMETHOD.
  METHOD add_message_bapireturn1.
    add_message( msgid = message1-id
                 msgty = message1-type
                 msgno = message1-number
                 msgv1 = message1-message_v1
                 msgv2 = message1-message_v2
                 msgv3 = message1-message_v3
                 msgv4 = message1-message_v4
                 message = message1-message ).
  ENDMETHOD.
  METHOD add_message_i.
    DATA l_error TYPE zest_ap_errores.

    CLEAR l_error.
    l_error-msgty   = msgty.
    l_error-msgid   = msgid.
    l_error-msgno   = msgno.
    l_error-msgv1   = msgv1.
    l_error-msgv2   = msgv2.
    l_error-msgv3   = msgv3.
    l_error-msgv4   = msgv4.
    l_error-message = message.
    l_error-color   = color.
    l_error-objeto  = objeto.

    CASE msgty.
      WHEN 'I' OR 'S'.
        l_error-lights = zcl_ap_alv=>c_sem_verde.
      WHEN 'W'.
        l_error-lights = zcl_ap_alv=>c_sem_ambar.
      WHEN OTHERS.
        l_error-lights = zcl_ap_alv=>c_sem_rojo.
    ENDCASE.

    IF         l_error-message IS INITIAL
       AND NOT l_error-msgno   IS INITIAL
       AND NOT l_error-msgid   IS INITIAL.
      MESSAGE ID l_error-msgid TYPE 'S' NUMBER l_error-msgno
              WITH l_error-msgv1 l_error-msgv2 l_error-msgv3 l_error-msgv4
              INTO l_error-message.
    ENDIF.

    APPEND l_error TO i_errores.
  ENDMETHOD.
  METHOD add_message_syst.
    IF NOT msgid IS INITIAL OR NOT msgno IS INITIAL.
      add_message( msgty = msgty
                   msgid = msgid
                   msgno = msgno
                   msgv1 = msgv1
                   msgv2 = msgv2
                   msgv3 = msgv3
                   msgv4 = msgv4
                   message = message ).
    ENDIF.
  ENDMETHOD.
  METHOD clear_messages.
    CLEAR i_errores.
  ENDMETHOD.
  METHOD confirmar.
    DATA: l_titulo TYPE string,
          l_resp   TYPE c LENGTH 1.

    CLEAR respuesta.

    IF titulo IS INITIAL.
      l_titulo = texto.
    ELSE.
      l_titulo = titulo.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP' "#EC FB_OLDED
      EXPORTING
        defaultoption  = opcion
        textline1      = texto
        textline2      = texto2
        titel          = l_titulo
        start_column   = start_col
        start_row      = start_row
        cancel_display = cancel_display
      IMPORTING
        answer         = l_resp.

    IF l_resp = 'J'.
      respuesta = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD confirmar_2.
    DATA: l_titulo TYPE string,
          l_resp   TYPE c LENGTH 1.

    CLEAR respuesta.

    IF titulo IS INITIAL.
      l_titulo = texto.
    ELSE.
      l_titulo = titulo.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE' "#EC FB_OLDED
      EXPORTING
        defaultoption  = opcion
        diagnosetext1  = texto
        diagnosetext2  = texto2
        diagnosetext3  = texto3
        textline1      = linea1
        textline2      = linea2
        titel          = l_titulo
*       START_COLUMN   = 25
*       START_ROW      = 6
        cancel_display = cancel_display
      IMPORTING
        answer         = l_resp.

    IF l_resp = 'J'.
      respuesta = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD confirmar_grabacion.
    respuesta = confirmar( opcion = opcion
                           titulo = titulo
                           texto  = texto
                           texto2 = texto2
                           start_col = start_col
                           start_row = start_row
                           cancel_display = cancel_display ).
  ENDMETHOD.
  METHOD constructor.
    me->o_log = o_log.
  ENDMETHOD.
  METHOD edit_text.
    CALL FUNCTION 'Z_POPUP_EDIT_TEXT'
      EXPORTING
        titulo       = titulo
        texto        = texto
        max_cols     = max_cols
        display_mode = display_mode
      IMPORTING
        modificado   = modificado
      CHANGING
        string       = string.
  ENDMETHOD.
  METHOD enviar_log_por_mail.
    DATA: o_log     TYPE REF TO zcl_ap_log,
          l_errores TYPE zest_ap_errores,
          l_log     TYPE zlog_ap.

    o_log = NEW #(
        object = object ).

    LOOP AT i_errores INTO l_errores.
      CLEAR l_log.
      MOVE-CORRESPONDING l_errores TO l_log.
      l_log-msgv1 = clave.
      CLEAR: l_log-msgv2, l_log-msgv3, l_log-msgv4.
      APPEND l_log TO o_log->i_log.
    ENDLOOP.

    o_log->enviar_log_por_mail( info                     = info
                                errores                  = errores
                                warnings                 = warnings
                                titulo                   = titulo
                                destinatario             = destinatario
                                aviso_usuario_online     = aviso_usuario_online
                                log_completo_en_adjunto  = log_completo_en_adjunto ).
  ENDMETHOD.
  METHOD free.
    CLEAR: i_errores, i_popup.
  ENDMETHOD.
  METHOD get_last_message.
    FIELD-SYMBOLS <mes> TYPE zest_ap_errores.

    CLEAR message.
    LOOP AT i_errores ASSIGNING <mes> WHERE msgty = type.
      message = <mes>-message.
    ENDLOOP.
  ENDMETHOD.
  METHOD guardar_log.
    DATA: error   TYPE zest_ap_errores,
          l_msgv1 TYPE sy-msgv1.

    IF o_log IS INITIAL.
      o_log = NEW #(
          object = proceso ).
    ENDIF.

    LOOP AT i_errores INTO error WHERE guardada = ''.
      IF msgv1 IS INITIAL AND clave IS INITIAL.
        l_msgv1 = error-msgv1.
      ELSE.
        l_msgv1 = msgv1.
      ENDIF.
      error-guardada = 'X'.
      MODIFY i_errores FROM error.

      o_log->set_tabla_log( clave    = clave
                            msgid    = error-msgid
                            msgty    = error-msgty
                            msgno    = error-msgno
                            msgv1    = l_msgv1
                            msgv2    = error-msgv2
                            msgv3    = error-msgv3
                            msgv4    = error-msgv4
                            message  = error-message ).
    ENDLOOP.
  ENDMETHOD.
  METHOD gui_disponible.
    DATA return TYPE c LENGTH 1.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = return.
    si = return.
  ENDMETHOD.
  METHOD informar.
    DATA l_string TYPE string.

    IF popup_permitido( ) = 'X'.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = titulo
          txt1  = texto1
          txt2  = texto2
          txt3  = texto3
          txt4  = texto4.
    ELSE.
      CONCATENATE texto1 texto2 texto3 texto4 INTO l_string SEPARATED BY space.
      MESSAGE l_string TYPE 'I'.
    ENDIF.
  ENDMETHOD.
  METHOD list_dynpro.
    DATA datos TYPE REF TO data.

    FIELD-SYMBOLS: <tabla> TYPE STANDARD TABLE,
                   <linea> TYPE any,
                   <fs>    TYPE any.

    DATA: i_values TYPE vrm_values,
          l_value  LIKE LINE OF i_values,
          l_where  TYPE string.
**  CALL FUNCTION 'VRM_REFRESH_VALUES'.

    IF NOT valores IS INITIAL.
      i_values = valores.
    ELSEIF NOT dominio IS INITIAL.
      SELECT domvalue_l ddtext FROM  dd07t
        INTO (l_value-key, l_value-text)
       WHERE domname    = dominio
         AND ddlanguage = sy-langu.
        APPEND l_value TO i_values.
      ENDSELECT.
    ELSEIF NOT tabla IS INITIAL.
      zcl_ap_fs=>create_it_from_struc( EXPORTING i_struc = tabla
                                       IMPORTING e_table = datos ).
      ASSIGN datos->* TO <tabla>.
      IF NOT campo_idioma IS INITIAL.
        CONCATENATE '''' sy-langu '''' INTO l_where.
        CONCATENATE campo_idioma '= ' l_where INTO l_where SEPARATED BY space.
      ENDIF.

      IF NOT where IS INITIAL.
        IF l_where IS INITIAL.
          l_where = where.
        ELSE.
          CONCATENATE l_where 'AND' where INTO l_where SEPARATED BY space.
        ENDIF.
      ENDIF.
      SELECT * FROM (tabla)
        INTO CORRESPONDING FIELDS OF TABLE <tabla>
       WHERE (l_where).
      LOOP AT <tabla> ASSIGNING <linea>.
        CLEAR l_value.
        ASSIGN COMPONENT key OF STRUCTURE <linea> TO <fs>.
        IF sy-subrc = 0.
          l_value-key = <fs>.
        ENDIF.
        ASSIGN COMPONENT text OF STRUCTURE <linea> TO <fs>.
        IF sy-subrc = 0.
          l_value-text = <fs>.
        ENDIF.
        APPEND l_value TO i_values.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = campo
        values = i_values.
  ENDMETHOD.
  METHOD lista_opciones.
    DATA: l_titulo       TYPE string,
          i_lista_string TYPE TABLE OF string,
          l_string       TYPE string,
          l_lista        TYPE spopli,
          l_first        TYPE c LENGTH 1,
          i_lista_local  TYPE ccseq_t_values,
          l_answer       TYPE c LENGTH 1.

    CLEAR respuesta.

    IF titulo IS INITIAL.
      l_titulo = texto.
    ELSE.
      l_titulo = titulo.
    ENDIF.

    IF i_lista IS INITIAL AND NOT lista IS INITIAL.
      SPLIT lista AT ',' INTO TABLE i_lista_string.

      LOOP AT i_lista_string INTO l_string.
        CLEAR l_lista.
        l_lista-varoption = l_string.
        IF l_first IS INITIAL.
          l_first = 'X'.
          l_lista-selflag = l_first.
        ENDIF.
        APPEND l_lista TO i_lista_local.
      ENDLOOP.
    ELSE.
      i_lista_local = i_lista.
    ENDIF.
    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        textline1          = texto
        textline2          = texto2
        titel              = l_titulo
        start_col          = start_col
        start_row          = start_row
      IMPORTING
        answer             = l_answer
      TABLES
        t_spopli           = i_lista_local
      EXCEPTIONS
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3.
    IF sy-subrc = 0.
      IF l_answer = 'A'. " Cancelado!
        CLEAR respuesta.
      ELSE.
        READ TABLE i_lista_local INTO l_lista WITH KEY selflag = 'X'.
        IF sy-subrc = 0.
          respuesta = l_lista-varoption.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR respuesta.
    ENDIF.
  ENDMETHOD.
  METHOD popup_2_opciones.
    " TODO: parameter START_COL is never used (ABAP cleaner)
    " TODO: parameter START_ROW is never used (ABAP cleaner)
    " TODO: parameter CANCEL_DISPLAY is never used (ABAP cleaner)

    DATA l_titulo TYPE text80.

    IF titulo IS INITIAL.
      l_titulo = texto.
    ELSE.
      l_titulo = titulo.
    ENDIF.

    CALL FUNCTION 'POPUP_WITH_2_BUTTONS_TO_CHOOSE'
      EXPORTING
        defaultoption = '1'
        diagnosetext1 = texto
        diagnosetext2 = texto2
        diagnosetext3 = texto3
        textline1     = linea1
        textline2     = linea2
        textline3     = linea3
        text_option1  = opcion1
        text_option2  = opcion2
        titel         = l_titulo
      IMPORTING
        answer        = respuesta.
  ENDMETHOD.
  METHOD popup_alv_ok_cancel.
    DATA: lr_table_descr TYPE REF TO cl_abap_tabledescr,
          lr_data        TYPE REF TO data,
          l_ucomm        TYPE sy-ucomm.

    FIELD-SYMBOLS <tabla> TYPE STANDARD TABLE.

* Copiamos la tabla para poder mandarla como modificable aunque no nos interesa la salida.
    lr_table_descr ?= cl_abap_tabledescr=>describe_by_data( tabla ).
    CREATE DATA lr_data TYPE HANDLE lr_table_descr.
    ASSIGN lr_data->* TO <tabla>.
    <tabla> = tabla.

    CLEAR ok.
    CALL FUNCTION 'Z_POPUP_ALV_AP'
      EXPORTING
        titulo       = titulo
        texto        = texto
        texto2       = texto2
        campos_noout = campos_noout
        campos_texto = campos_texto
        campos_sum   = campos_sum
        campos_orden = campos_orden
        botones      = botones
        ancho        = ancho
      IMPORTING
        ucomm        = l_ucomm
      TABLES
        t_datos      = <tabla>.
    IF l_ucomm = 'F01'.
      ok = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD popup_alv_seleccion.
    DATA: l_ucomm TYPE sy-ucomm,
          l_fila  TYPE i.

    DO.
      CALL FUNCTION 'Z_POPUP_ALV_AP'
        EXPORTING
          titulo       = titulo
          texto        = texto
          texto2       = texto2
          campos_noout = campos_noout
          campos_texto = campos_texto
          campos_sum   = campos_sum
          campos_orden = campos_orden
          botones      = botones
          ancho        = ancho
          check        = 'X'
        IMPORTING
          ucomm        = l_ucomm
          fila         = l_fila
        TABLES
          t_datos      = tabla.

      IF l_ucomm IS INITIAL AND NOT l_fila IS INITIAL.
        l_ucomm = 'F01'.

* En caso de doble click, sólo queremos el selecionado
        LOOP AT tabla ASSIGNING FIELD-SYMBOL(<registro>).
          ASSIGN COMPONENT 'CHECK' OF STRUCTURE <registro> TO FIELD-SYMBOL(<check>).
          IF sy-subrc = 0.
            CLEAR <check>.
          ENDIF.
        ENDLOOP.

        ASSIGN tabla[ l_fila ] TO <registro>.
        ASSIGN COMPONENT 'CHECK' OF STRUCTURE <registro> TO <check>.
        IF sy-subrc = 0.
          <check> = 'X'.
        ENDIF.
      ENDIF.

      IF l_ucomm = 'F01'.
        DATA(cont) = 0.
        LOOP AT tabla ASSIGNING <registro>.
          ASSIGN COMPONENT 'CHECK' OF STRUCTURE <registro> TO <check>.
          IF sy-subrc = 0.
            IF <check> = 'X'.
              cont = cont + 1.
              registro = <registro>.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF cont = 0 AND obligatoria = 'X'.
          MESSAGE 'Seleccione al menos un registro' TYPE 'I'.
        ELSEIF cont > 1 AND solo_una = 'X'.
          MESSAGE 'Seleccione solo un registro' TYPE 'I'.
        ELSE.
          RETURN.
        ENDIF.
      ELSE.
        cancelado = 'X'.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD popup_bapiret2.
    DATA: i_ret    TYPE bapirettab,
          l_return TYPE bapiret2.

    i_ret = i_return.
    IF NOT msgv1 IS INITIAL.
      l_return-type       = msgty.
      l_return-id         = msgid.
      l_return-number     = msgno.
      l_return-message_v1 = msgv1.
      l_return-message_v2 = msgv2.
      l_return-message_v3 = msgv3.
      l_return-message_v4 = msgv4.
      __concat4 l_return-message msgv1 msgv2 msgv3 msgv4.
      INSERT l_return INTO i_ret INDEX 1.
    ENDIF.

    CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
*             EXPORTING
*               SEND_IF_ONE         = ' '
*               OBJECT              = ' '
*               SHOW_LINNO          = 'X'
*               AMODAL_WINDOW       = ' '
      TABLES
        ss_bapiret2 = i_ret.
  ENDMETHOD.
  METHOD popup_campos_estructura.
    TYPES: BEGIN OF t_alv,
             check     TYPE c LENGTH 1,
             fieldname TYPE fieldname,
             fieldtext TYPE as4text,
           END OF t_alv.

    DATA: i_campos  TYPE ddfields,
          l_long    TYPE i,
          l_campo   TYPE dfies,
          l_alv     TYPE t_alv,
          i_alv     TYPE TABLE OF t_alv,
          o_alv     TYPE REF TO zcl_ap_alv,
          lr_campos TYPE roij_fieldname_rstr.

    i_campos = zcl_ap_dev=>get_fieldcatalog_tabla( estructura ).

    IF NOT ini_campos_no IS INITIAL.
      l_long = strlen( ini_campos_no ).
      LOOP AT i_campos INTO l_campo WHERE fieldname CS ini_campos_no.
        IF l_campo-fieldname(l_long) = ini_campos_no.
          DELETE i_campos.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT ini_campos_no2 IS INITIAL.
      l_long = strlen( ini_campos_no2 ).
      LOOP AT i_campos INTO l_campo WHERE fieldname CS ini_campos_no2.
        IF l_campo-fieldname(l_long) = ini_campos_no2.
          DELETE i_campos.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT i_campos INTO l_campo.
      CLEAR l_alv.
      MOVE-CORRESPONDING l_campo TO l_alv.
      IF l_alv-fieldname IN r_campos.
        l_alv-check = 'X'.
      ENDIF.
      APPEND l_alv TO i_alv.
    ENDLOOP.

    o_alv = NEW #(
        tabla = '' ).

    o_alv->constructor_tabla( EXPORTING campo_check = 'CHECK' sel = 'M' no_layout = 'X' CHANGING t_tabla = i_alv ).

    o_alv->set_seleccion( CHANGING t_tabla = i_alv ).

    o_alv->show_popup( ).

    o_alv->get_seleccion( CHANGING t_tabla = i_alv ).

    CLEAR r_campos.
    LOOP AT i_alv INTO l_alv WHERE check = 'X'.
      CLEAR lr_campos.
      lr_campos-option = 'EQ'.
      lr_campos-sign   = 'I'.
      lr_campos-low    = l_alv-fieldname.
      APPEND lr_campos TO r_campos.
    ENDLOOP.
  ENDMETHOD.
  METHOD popup_fecha.
    DATA: l_title        TYPE text128,
          l_description1 TYPE text128,
          l_description2 TYPE text128.

    l_title = titulo.
    IF titulo IS INITIAL.
      l_title = texto.
    ENDIF.
    l_description1 = texto.
    l_description2 = texto2.

    CALL FUNCTION 'TR_POPUP_INPUT_DATE'
      EXPORTING
        iv_title               = l_title
        iv_description1        = l_description1
        iv_description2        = l_description2
        iv_date                = fecha
      IMPORTING
        ev_date                = fecha_sel
      EXCEPTIONS
        action_aborted_by_user = 1
        value_not_changed      = 2
        OTHERS                 = 3.

    IF sy-subrc = 2.
      fecha_sel = fecha.
    ELSEIF sy-subrc <> 0.
      CLEAR fecha_sel.
    ENDIF.
  ENDMETHOD.
  METHOD popup_fechas.
    begda_sel = begda.
    endda_sel = endda.
    DO.
      CALL FUNCTION 'RHPK_POPUP_BEGDA_ENDA'
        CHANGING
          begda    = begda_sel
          endda    = endda_sel
        EXCEPTIONS
          canceled = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
        cancelado = 'X'.
        RETURN.
      ELSEIF begda_sel IS INITIAL OR endda_sel IS INITIAL.
        MESSAGE 'Informe fechaS' TYPE 'I'.
      ELSEIF endda_sel < begda_sel.
        MESSAGE 'Fecha fin no puede ser menor a fecha inicial' TYPE 'I'.
      ELSE.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD popup_permitido.
    DATA lv_update_process TYPE sy-subrc.

    IF sy-batch IS INITIAL AND sy-binpt IS INITIAL. " No ejecutamos en fondo ni en batch input
      CALL FUNCTION 'TH_IN_UPDATE_TASK'             " Ni en ejecución en fondo
        IMPORTING
          in_update_task = lv_update_process.

      IF lv_update_process = 0.
        IF gui_disponible( ) = 'X'.                 " Ni si no se está ejecutando el GUI
          si = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD popup_usuario.
    DATA: l_campo  TYPE sval,
          i_campos TYPE TABLE OF sval.
    DATA: v_pgv_texto1 TYPE c LENGTH 80 ##NEEDED,
          v_pgv_texto2 TYPE c LENGTH 80 ##NEEDED.

    SPLIT campo1 AT '-' INTO l_campo-tabname l_campo-fieldname.
    l_campo-value = valor1.
    APPEND l_campo TO i_campos.

    IF NOT campo2 IS INITIAL.
      CLEAR l_campo.
      SPLIT campo2 AT '-' INTO l_campo-tabname l_campo-fieldname.
      l_campo-value = valor2.
      APPEND l_campo TO i_campos.
    ENDIF.

    IF NOT campo3 IS INITIAL.
      CLEAR l_campo.
      SPLIT campo3 AT '-' INTO l_campo-tabname l_campo-fieldname.
      l_campo-value = valor3.
      APPEND l_campo TO i_campos.
    ENDIF.

    IF NOT campo4 IS INITIAL.
      CLEAR l_campo.
      SPLIT campo4 AT '-' INTO l_campo-tabname l_campo-fieldname.
      l_campo-value = valor4.
      APPEND l_campo TO i_campos.
    ENDIF.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title  = titulo
        start_column = start_column
        start_row    = start_row
      IMPORTING
        returncode   = return
      TABLES
        fields       = i_campos.

    IF return <> 'A'. " Cancelado
      LOOP AT i_campos INTO l_campo.
        CASE sy-tabix.
          WHEN 1.
            valor1 = l_campo-value.
          WHEN 2.
            valor2 = l_campo-value.
          WHEN 3.
            valor3 = l_campo-value.
          WHEN 4.
            valor4 = l_campo-value.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD set_objeto.
    me->objeto = objeto.
  ENDMETHOD.
  METHOD show_errores.
    CALL FUNCTION 'Z_POPUP_ERRORES'
      EXPORTING
        show_msgv1 = show_msgv1
        show_msgv2 = show_msgv2
        titulo     = titulo
      TABLES
        t_errores  = i_errores.
  ENDMETHOD.
  METHOD show_log.
    " TODO: parameter QUITAR_MSG_TECNICOS is never used (ABAP cleaner)

    IF NOT titulo IS INITIAL.
      DATA(l_titulo) = titulo.
    ELSE.
      l_titulo = 'Resultado ejecución'.
    ENDIF.

    DATA(l_campos_texto) = campos_texto.
    IF NOT l_campos_texto CS '='.
      l_campos_texto = |CLAVE={ campos_texto }|.
    ENDIF.

    popup_alv_ok_cancel( titulo       = l_titulo
                         tabla        = i_log_simple
                         campos_texto = l_campos_texto
                         botones      = 'OK' ).
  ENDMETHOD.
  METHOD show_popup.
    CALL FUNCTION 'Z_POPUP_CONFIRM_ALV'
      EXPORTING
        texto             = texto
        titulo            = titulo
        ocultar_col_valor = ocultar_col_valor
      IMPORTING
        ok                = ok
      TABLES
        t_popup           = i_popup.
  ENDMETHOD.
