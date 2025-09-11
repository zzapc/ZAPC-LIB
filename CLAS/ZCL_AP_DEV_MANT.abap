
CLASS zcl_ap_dev_mant DEFINITION
  PUBLIC
  INHERITING FROM zcl_ap_dev
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_campos_texto,
             campo      TYPE string,
             fieldname  TYPE dd05q-fieldname,
             checktable TYPE dd05q-checktable,
             checkfield TYPE dd05q-checkfield,
             tabname    TYPE dd08l-tabname,
             texto      TYPE dd05q-fieldname,
             scrtext_s  TYPE lvc_s_fcat-scrtext_s,
             scrtext_m  TYPE lvc_s_fcat-scrtext_m,
             scrtext_l  TYPE lvc_s_fcat-scrtext_l,
           END OF t_campos_texto.

    TYPES tt_campos_texto TYPE STANDARD TABLE OF t_campos_texto WITH KEY campo.

    TYPES:
      BEGIN OF t_ini_listado,
        clave_interna TYPE string,
        check         TYPE xfeld,
        lights        TYPE zico_estado_mensaje,
      END OF t_ini_listado.
    TYPES:
      BEGIN OF t_fin_listado,
        message TYPE bapi_msg,
        style   TYPE lvc_t_styl,
        color   TYPE lvc_t_scol,
        updkz   TYPE char1,
        tabix   TYPE int4,
      END OF t_fin_listado.

    DATA tabla               TYPE tabname.
    DATA campos_clave        TYPE string.
    DATA campos_input        TYPE string.
    DATA campos_obligatorios TYPE string.
    DATA longitud_clave      TYPE int4.
    DATA i_campos_alv        TYPE lvc_t_fcat.
    DATA i_campos_tabla      TYPE enh_t_dd03l.
    DATA where               TYPE string.
    DATA tabix               TYPE int4.
    DATA i_campos_texto      TYPE tt_campos_texto.

    METHODS configuracion
      IMPORTING tabla TYPE tabname
                datos TYPE table.

    METHODS where_clave
      IMPORTING listado      TYPE any
      RETURNING VALUE(where) TYPE string.

    METHODS validaciones
      IMPORTING !mod      TYPE abap_bool DEFAULT ''
                datos_ini TYPE table     OPTIONAL
      CHANGING  listado   TYPE any.

    METHODS validar_registro
      IMPORTING !mod    TYPE abap_bool DEFAULT ''
                ini     TYPE any       OPTIONAL
      CHANGING  listado TYPE any.

    METHODS validar_duplicados
      IMPORTING datos    TYPE table
      EXPORTING !message TYPE bapi_msg.

    METHODS status_dynpro_0100
      IMPORTING o_alv      TYPE REF TO zcl_ap_alv_grid
                visualizar TYPE abap_bool DEFAULT ''
                crear_ot   TYPE abap_bool DEFAULT 'X'
                comparar   TYPE abap_bool DEFAULT 'X'
      CHANGING  i_listado  TYPE table.

    METHODS command_dynpro_0100
      IMPORTING o_alv         TYPE REF TO zcl_ap_alv_grid
      CHANGING  i_listado     TYPE table
                i_listado_ini TYPE table OPTIONAL.

    METHODS grabar_datos
      CHANGING i_listado     TYPE table
               i_listado_ini TYPE table OPTIONAL.

    METHODS grabar_registro
      IMPORTING registro  TYPE any
      RETURNING VALUE(ok) TYPE abap_bool.

    METHODS nuevo_registro
      IMPORTING o_alv     TYPE REF TO zcl_ap_alv_grid OPTIONAL
      CHANGING  registro  TYPE any
                i_listado TYPE table.

    METHODS borrar_registro
      IMPORTING o_alv     TYPE REF TO zcl_ap_alv_grid
      CHANGING  registro  TYPE any
                i_listado TYPE table.

    METHODS copiar_registro
      IMPORTING o_alv     TYPE REF TO zcl_ap_alv_grid
      CHANGING  registro  TYPE any
                i_listado TYPE table.

    METHODS personalizar_alv
      IMPORTING o_alv TYPE REF TO zcl_ap_alv_grid.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_DEV_MANT definition
class ZCL_AP_DEV_MANT implementation.
  METHOD borraR_registro.
    " TODO: parameter REGISTRO is never used or assigned (ABAP cleaner)

    DATA l_hay_sel TYPE xfeld.

    o_alv->comprobar_cambios( ).
    o_alv->set_marca_filas_sel( EXPORTING validar_seleccion = 'X' CHANGING t_tabla = i_listado hay_sel = l_hay_sel ).
    IF l_hay_sel = 'X'.
      LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
        ASSIGN COMPONENT 'CHECK' OF STRUCTURE <listado> TO FIELD-SYMBOL(<check>).
        IF <check> = 'X'.
          ASSIGN COMPONENT 'UPDKZ' OF STRUCTURE <listado> TO FIELD-SYMBOL(<updkz>).
          IF <updkz> = 'I'.
            DELETE i_listado.
          ELSEIF <updkz> = 'D'.
            ASSIGN COMPONENT 'LIGHTS' OF STRUCTURE <listado> TO FIELD-SYMBOL(<lights>).
            CLEAR: <updkz>, <check>, <lights>.
            validaciones( EXPORTING mod = 'U' CHANGING listado = <listado> ).
          ELSE.
            validaciones( EXPORTING mod = 'D' CHANGING listado = <listado> ).
          ENDIF.
        ENDIF.
      ENDLOOP.
      o_alv->refrescar_grid( ).
    ENDIF.
  ENDMETHOD.
  METHOD command_dynpro_0100.
    DATA: l_comm_sel TYPE string VALUE 'OT',
          l_hay_sel  TYPE c LENGTH 1.
    DATA: reg     TYPE REF TO data,
          l_key   TYPE text255,
          l_index TYPE int4.
    DATA i_keys TYPE TABLE OF string.

    FIELD-SYMBOLS <reg> TYPE any.

*          l_key  TYPE c LENGTH 120.

    command_dynpro( EXPORTING o_alv = o_alv seleccion = l_comm_sel
                    CHANGING i_listado = i_listado i_listado_ini = i_listado_ini hay_sel = l_hay_sel ).

    CASE ucomm.
      WHEN 'GRABAR'.
        validar_duplicados( EXPORTING datos = i_listado IMPORTING message = DATA(l_message) ).
        IF NOT l_message IS INITIAL.
          MESSAGE 'No puede grabar si hay duplicados' TYPE 'I'.
          o_alv->refrescar_grid( ).
          RETURN.
        ENDIF.

        LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
          ASSIGN COMPONENT 'UPDKZ' OF STRUCTURE <listado> TO FIELD-SYMBOL(<updkz>).
          IF <updkz> <> ''.
            ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <listado> TO FIELD-SYMBOL(<message>).
            IF <message> <> ''.
              MESSAGE 'No puede grabar si hay errores' TYPE 'I'.
              RETURN.
            ENDIF.
          ENDIF.
        ENDLOOP.

        grabar_datos( CHANGING i_listado = i_listado
                               i_listado_ini = i_listado_ini ).
        o_alv->set_seleccion( CHANGING t_tabla = i_listado ).
        o_alv->refrescar_grid( ).

      WHEN 'OT'.
        LOOP AT i_listado ASSIGNING <listado>.
          ASSIGN COMPONENT 'CHECK' OF STRUCTURE <listado> TO FIELD-SYMBOL(<check>).
          IF <check> = 'X'.
            zcl_ap_fs=>create_wa_from_struc( EXPORTING i_struc = tabla
                                             IMPORTING e_workarea = reg ).
            ASSIGN reg->* TO <reg>.
            MOVE-CORRESPONDING <listado> TO <reg>.

            ASSIGN COMPONENT 'MANDT' OF STRUCTURE <listado> TO FIELD-SYMBOL(<mandt>).
            IF sy-subrc = 0.
              <mandt> = sy-mandt.
            ENDIF.

            CLEAR l_key.
            l_index = 0.
            LOOP AT i_campos_tabla ASSIGNING FIELD-SYMBOL(<campo_clave>) WHERE keyflag = 'X'.
              ASSIGN COMPONENT <campo_clave>-fieldname OF STRUCTURE <reg> TO FIELD-SYMBOL(<valor>).
              l_key+l_index(<campo_clave>-leng) = <valor>.
              l_index = l_index + <campo_clave>-leng.
            ENDLOOP.
            APPEND l_key TO i_keys.
          ENDIF.
        ENDLOOP.

        zcl_ap_utils=>grabar_tabla_en_ot( tabla = tabla i_keys = i_keys ).

      WHEN 'COMP'.
        SUBMIT zap_comparar_tablas
               AND RETURN
               WITH s_table = tabla.
*             WITH p_clave = p_cprog. "CAMBIAR

    ENDCASE.
  ENDMETHOD.
  METHOD configuracion.
    me->tabla = tabla.
    CLEAR: campos_clave, i_campos_tabla, i_campos_alv, i_campos_texto.

    i_campos_alv = get_fieldcatalog_tabla_alv( tabla = datos ).

    SELECT fieldname keyflag leng position FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE i_campos_tabla
     WHERE tabname = tabla
     ORDER BY position.
    IF sy-subrc <> 0.
      MESSAGE |No existe tabla { tabla }| TYPE 'E'.
    ENDIF.

    LOOP AT i_campos_tabla ASSIGNING FIELD-SYMBOL(<campo>) WHERE fieldname <> 'MANDT'.
      IF line_exists(  i_campos_alv[ fieldname = <campo>-fieldname ] ).
        IF <campo>-keyflag = 'X'.
          IF campos_clave IS INITIAL.
            campos_clave = <campo>-fieldname.
          ELSE.
            CONCATENATE campos_clave <campo>-fieldname INTO campos_clave SEPARATED BY ','.
          ENDIF.
          longitud_clave = longitud_clave + <campo>-leng.
        ELSE.
          __add_lista campos_input <campo>-fieldname.
        ENDIF.
      ENDIF.
    ENDLOOP.

    campos_obligatorios = campos_clave.

* Intento autimaticamente definir la tabal de referencia
    SELECT dd05q~fieldname, dd05q~checktable, dd05q~checkfield,
           dd08l~tabname
      FROM dd05q LEFT OUTER JOIN dd08l ON  dd08l~checktable = dd05q~checktable
                                       AND dd08l~fieldname  = dd05q~checkfield
                                       AND dd08l~frkart     = 'TEXT'
      INTO CORRESPONDING FIELDS OF TABLE @i_campos_texto
     WHERE dd05q~tabname     = @tabla
       AND dd05q~checktable <> ''
       AND dd05q~fieldname  <> 'MANDT'
       AND dd05q~checkfield <> 'MANDT'.

    LOOP AT i_campos_texto ASSIGNING FIELD-SYMBOL(<ctext>).
      <ctext>-campo = <ctext>-fieldname && '_T'.

      ASSIGN i_campos_alv[ fieldname = <ctext>-campo ] TO FIELD-SYMBOL(<alv>).
      IF sy-subrc = 0.
        <ctext>-scrtext_s = <alv>-scrtext_s.
        <ctext>-scrtext_m = <alv>-scrtext_m.
        <ctext>-scrtext_l = <alv>-scrtext_l.
        SELECT position
          FROM dd03l
          WHERE tabname = @<ctext>-tabname
            AND fieldname = @<ctext>-checkfield
          ORDER BY PRIMARY KEY
          INTO @DATA(l_pos)
          UP TO 1 ROWS.
        ENDSELECT.
        l_pos = l_pos + 1.
        SELECT fieldname
          FROM dd03l
          WHERE tabname = @<ctext>-tabname
            AND position = @l_pos
          ORDER BY PRIMARY KEY
          INTO @<ctext>-texto
          UP TO 1 ROWS.
        ENDSELECT.
      ELSE.
        DELETE i_campos_texto.
      ENDIF.
    ENDLOOP.

* Si no, el texto será el mismo que el del origen
    LOOP AT i_campos_alv ASSIGNING <alv> WHERE fieldname CS '_T' AND scrtext_s IS INITIAL.
      SPLIT <alv>-fieldname AT '_T' INTO DATA(l_campo) DATA(l_sufijo).
      IF l_sufijo IS INITIAL.
        ASSIGN i_campos_alv[ fieldname = l_campo ] TO FIELD-SYMBOL(<alv_origen>).
        IF sy-subrc = 0.
          <alv>-scrtext_s = <alv_origen>-scrtext_s.
          <alv>-scrtext_m = <alv_origen>-scrtext_m.
          <alv>-scrtext_l = <alv_origen>-scrtext_l.
          <alv>-col_pos   = <alv_origen>-col_pos.
          APPEND INITIAL LINE TO i_campos_texto ASSIGNING <ctext>.
          MOVE-CORRESPONDING <alv> TO <ctext>.
          <ctext>-campo     = <ctext>-fieldname.
          <ctext>-fieldname = l_campo.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD copiar_registro.
    DATA l_hay_sel TYPE xfeld.

    o_alv->comprobar_cambios( ).
    o_alv->set_marca_filas_sel( EXPORTING validar_seleccion = 'X' CHANGING t_tabla = i_listado hay_sel = l_hay_sel ).
    IF l_hay_sel = 'X'.
      DATA(l_max_tabix) = tabix.
      LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
        ASSIGN COMPONENT 'TABIX' OF STRUCTURE <listado> TO FIELD-SYMBOL(<tabix>).
        IF <tabix> > l_max_tabix.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT 'CHECK' OF STRUCTURE <listado> TO FIELD-SYMBOL(<check>).
        IF <check> = 'X'.
          registro = <listado>.
          tabix = tabix + 1.
          ASSIGN COMPONENT 'TABIX' OF STRUCTURE registro TO <tabix>.
          <tabix> = tabix.
          ASSIGN COMPONENT 'CHECK' OF STRUCTURE registro TO <check>.
          ASSIGN COMPONENT 'CHECK' OF STRUCTURE <listado> TO <check>.
          CLEAR <check>.
          validaciones( EXPORTING mod = 'I' CHANGING listado = registro ).
          APPEND registro TO i_listado.
        ENDIF.
      ENDLOOP.
      o_alv->refrescar_grid( soft_refresh = '' ).
    ENDIF.
  ENDMETHOD.
  METHOD grabar_datos.
    DATA(l_cont) = 0.
    DATA(l_cont_error) = 0.
    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
      ASSIGN COMPONENT 'UPDKZ' OF STRUCTURE <listado> TO FIELD-SYMBOL(<updkz>).
      ASSIGN COMPONENT 'TABIX' OF STRUCTURE <listado> TO FIELD-SYMBOL(<tabix>).
      IF <updkz> <> ''.
        IF <updkz> = 'D'.
          DATA(l_where_delete) = where_clave( <listado> ).
          DELETE FROM (tabla)
           WHERE (l_where_delete).
          IF sy-subrc = 0.
            l_cont = l_cont + 1.
            LOOP AT i_listado_ini ASSIGNING FIELD-SYMBOL(<ini>).
              ASSIGN COMPONENT 'TABIX' OF STRUCTURE <ini> TO FIELD-SYMBOL(<tabix_ini>).
              IF <tabix_ini> = <tabix>.
                DELETE i_listado_ini.
                EXIT.
              ENDIF.
            ENDLOOP.
            DELETE i_listado.
          ELSE.
            l_cont_error = l_cont_error + 1.
          ENDIF.
        ELSE.
          IF grabar_registro( registro = <listado> ).
            l_cont = l_cont + 1.
            ASSIGN COMPONENT 'LIGHTS' OF STRUCTURE <listado> TO FIELD-SYMBOL(<lights>).
            ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <listado> TO FIELD-SYMBOL(<message>).
            ASSIGN COMPONENT 'STYLE' OF STRUCTURE <listado> TO FIELD-SYMBOL(<style>).
            CLEAR: <updkz>, <lights>, <message>, <style>.

            DATA(l_ok) = ''.
            LOOP AT i_listado_ini ASSIGNING <ini>.
              ASSIGN COMPONENT 'TABIX' OF STRUCTURE <ini> TO <tabix_ini>.
              IF <tabix_ini> = <tabix>.
                <ini> = <listado>.
                l_ok = 'X'.
                EXIT.
              ENDIF.
            ENDLOOP.
            IF l_ok IS INITIAL.
              APPEND <listado> TO i_listado_ini.
            ENDIF.
          ELSE.
            l_cont_error = l_cont_error + 1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF l_cont = 0.
      IF l_cont_error IS INITIAL.
        MESSAGE 'No había cambios' TYPE 'S'.
      ELSE.
        MESSAGE |Error guardando { l_cont_error } registros | TYPE 'I'.
      ENDIF.
    ELSE.
      IF l_cont_error IS INITIAL.
        MESSAGE |Se han grabado { l_cont } registros| TYPE 'S'.
      ELSE.
        MESSAGE |Se han grabado { l_cont } registros pero se han quedado sin grabar { l_cont_error }| TYPE 'I'.
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD grabar_registro.
    DATA reg TYPE REF TO data.

    FIELD-SYMBOLS <reg> TYPE any.

    CLEAR ok.
    zcl_ap_fs=>create_wa_from_struc( EXPORTING i_struc = tabla
                                     IMPORTING e_workarea = reg ).
    ASSIGN reg->* TO <reg>.
    MOVE-CORRESPONDING registro TO <reg>.
    MODIFY (tabla) FROM <reg>.
    IF sy-subrc = 0.
      ok = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD nuevo_registro.
    ASSIGN COMPONENT 'TABIX' OF STRUCTURE registro TO FIELD-SYMBOL(<tabix>).
    tabix = tabix + 1.
    <tabix> = tabix.
    validaciones( EXPORTING mod = 'I' CHANGING listado = registro ).
    APPEND registro TO i_listado.

    IF NOT o_alv IS INITIAL.
      o_alv->refrescar_grid( ).
    ENDIF.
  ENDMETHOD.
  METHOD personalizar_alv.
    LOOP AT i_campos_texto ASSIGNING FIELD-SYMBOL(<ctext>).
      o_alv->set_field( campo = <ctext>-campo op = 'COL_AFTER' valor = <ctext>-fieldname ).
      o_alv->set_field_text( campo = <ctext>-campo valor = <ctext>-scrtext_s valor2 = <ctext>-SCRTEXT_l  ).
    ENDLOOP.
  ENDMETHOD.
  METHOD status_dynpro_0100.
    IF inicio IS NOT INITIAL.
      RETURN.
    ENDIF.

    status_dynpro( EXPORTING cprog = 'ZAP_STATUS' status = 'ST_DYN' titulo = string CHANGING i_listado = i_listado ).

    inicio = 'X'.
    o_alv->registrar_mod( ).
    o_alv->set_layout( ancho_optimizado = 'X' no_rowmove = 'X' no_rowins = 'X' style = 'STYLE' colort = 'COLOR' ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_refresh ).
    o_alv->set_campos_tabint( i_listado ).
    o_alv->set_field_quitar( 'CHECK,MANDT,MESSAGE,TABIX,UPDKZ,CLAVE_INTERNA' ).

    IF visualizar IS INITIAL.
      o_alv->add_button( button = 'F01' text = 'Grabar' icon = icon_system_save ucomm = 'GRABAR' ).

      IF sy-sysid = zcl_c=>entorno_desarrollo.
        IF crear_ot = 'X'.
          o_alv->add_button( button = 'F02' text = 'Crear OT'  icon =  icon_import_transport_request ucomm = 'OT' ).
        ENDIF.
        IF comparar = 'X'.
          o_alv->add_button( button = 'F03' text = 'Comparar con producción'  icon =  icon_compare ucomm = 'COMP' ).
        ENDIF.
      ELSE.
        IF comparar = 'X'.
          o_alv->add_button( button = 'F03' text = 'Comparar con desarrollo'  icon =  icon_compare ucomm = 'COMP' ).
        ENDIF.
      ENDIF.
      o_alv->set_field_input( campos_input ).
    ENDIF.

    personalizar_alv( o_alv = o_alv ).

    o_alv->set_orden( campos_clave ).

    sgpi_texto( 'Generando informe' ).
    o_alv->show( CHANGING tabla = i_listado ).

    i_campos_alv = o_alv->get_fcat_inicial( ).
  ENDMETHOD.
  METHOD validaciones.
    DATA: campos_edit  TYPE string,
          campos_error TYPE string.

    ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE listado TO FIELD-SYMBOL(<message>).
    ASSIGN COMPONENT 'STYLE' OF STRUCTURE listado TO FIELD-SYMBOL(<style>).
    ASSIGN COMPONENT 'COLOR' OF STRUCTURE listado TO FIELD-SYMBOL(<color>).
    ASSIGN COMPONENT 'LIGHTS' OF STRUCTURE listado TO FIELD-SYMBOL(<lights>).
    ASSIGN COMPONENT 'UPDKZ' OF STRUCTURE listado TO FIELD-SYMBOL(<updkz>).
    ASSIGN COMPONENT 'TABIX' OF STRUCTURE listado TO FIELD-SYMBOL(<tabix>).
    ASSIGN COMPONENT 'CLAVE_INTERNA' OF STRUCTURE listado TO FIELD-SYMBOL(<clave_interna>).

    CLEAR: <message>, <style>, <color>.

    LOOP AT i_campos_texto ASSIGNING FIELD-SYMBOL(<ctext>) WHERE tabname <> ''.
      ASSIGN COMPONENT <ctext>-campo OF STRUCTURE listado TO FIELD-SYMBOL(<campo_t>).
      ASSIGN COMPONENT <ctext>-fieldname OF STRUCTURE listado TO FIELD-SYMBOL(<campo_o>).
      <campo_t> = get( tabla = <ctext>-tabname clave = <campo_o> ).
    ENDLOOP.

    CASE mod.
      WHEN 'I'.
        <updkz> = 'I'.
      WHEN 'D'.
        <updkz> = 'D'.
      WHEN 'U'.
        IF <updkz> IS INITIAL.
          LOOP AT datos_ini ASSIGNING FIELD-SYMBOL(<ini>).
            ASSIGN COMPONENT 'TABIX' OF STRUCTURE <ini> TO FIELD-SYMBOL(<tabix_ini>).
            IF <tabix_ini> = <tabix>.
              IF <ini> <> listado.
                DATA(l_enc) = 'X'.
                <updkz> = 'U'.
              ENDIF.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF l_enc = 'X'.
            <updkz> = 'U'.
          ENDIF.
        ENDIF.
    ENDCASE.

    CASE <updkz>.
      WHEN 'I'.
        DATA(l_icono) = icon_create.
        campos_edit = campos_clave.
      WHEN 'D'.
        l_icono = icon_delete.
      WHEN 'U'.
        l_icono = icon_change.
    ENDCASE.

    IF <updkz> = 'I' OR <updkz> = 'U'.
      SPLIT campos_obligatorios AT ',' INTO TABLE DATA(i_campos_obl).
      LOOP AT i_campos_obl ASSIGNING FIELD-SYMBOL(<obl>).
        ASSIGN COMPONENT <obl> OF STRUCTURE listado TO FIELD-SYMBOL(<fs>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <fs> IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        __add_lista campos_error <obl>.
        IF <message> IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        ASSIGN i_campos_alv[ fieldname = <obl> ] TO FIELD-SYMBOL(<alv>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        DATA(l_desc_campo) = <alv>-scrtext_m.
        IF l_desc_campo IS INITIAL.
          SELECT SINGLE ddtext FROM dd03t
           INTO l_desc_campo
          WHERE tabname    = tabla
            AND ddlanguage = sy-langu
            AND fieldname  = <obl>.
          IF sy-subrc <> 0.
            ASSIGN i_campos_tabla[ fieldname = <obl> ] TO FIELD-SYMBOL(<campo>).
            SELECT ddtext FROM dd04t
              INTO l_desc_campo
              UP TO 1 ROWS
             WHERE ddlanguage = sy-langu
               AND rollname   = <campo>-rollname
              ORDER BY PRIMARY KEY.
            ENDSELECT.
          ENDIF.
          IF l_desc_campo IS INITIAL.
            l_desc_campo = <obl>.
          ENDIF.
        ENDIF.

        <message> = |Informe campo { l_desc_campo }|.
        l_icono = icon_red_light.
      ENDLOOP.
    ENDIF.

    CLEAR <clave_interna>.
    LOOP AT i_campos_tabla ASSIGNING <campo> WHERE keyflag = 'X'.
      ASSIGN COMPONENT <campo>-fieldname OF STRUCTURE listado TO <fs>.
      IF sy-subrc = 0.
        CONCATENATE <clave_interna> <fs> INTO <clave_interna> SEPARATED BY '-'.
      ENDIF.
    ENDLOOP.

    IF <ini> IS ASSIGNED.
      validar_registro( EXPORTING mod = mod
                                  ini = <ini>
                         CHANGING listado = listado ).
    ELSE.
      validar_registro( EXPORTING mod = mod
                    CHANGING listado = listado ).
    ENDIF.

    set_status_list( EXPORTING message = <message>
                               icono = l_icono
                               campos_editables = campos_edit
                               resaltar_campos = campos_error
                               color_resalte = 'R'
                     CHANGING list = listado ).
  ENDMETHOD.
  METHOD validar_duplicados.
    TYPES: BEGIN OF t_list_dup,
             clave_interna TYPE string,
             cont          TYPE int4,
             tabix         TYPE string,
           END OF t_list_dup.

    DATA: i_list_dup TYPE TABLE OF t_list_dup,
          l_list_dup TYPE t_list_dup.

    LOOP AT datos ASSIGNING FIELD-SYMBOL(<listado>).
      ASSIGN COMPONENT 'CLAVE_INTERNA' OF STRUCTURE <listado> TO FIELD-SYMBOL(<clave_interna>).
      IF line_exists( i_list_dup[ clave_interna = <clave_interna>  ] ).
        i_list_dup[ clave_interna = <clave_interna>  ]-cont = 2.
      ELSE.
        l_list_dup-clave_interna = <clave_interna>.
        l_list_dup-tabix         = 1.
        APPEND l_list_dup TO i_list_dup.
      ENDIF.
    ENDLOOP.
    LOOP AT i_list_dup ASSIGNING FIELD-SYMBOL(<list>) WHERE tabix > 1.
      LOOP AT datos ASSIGNING <listado>.
        ASSIGN COMPONENT 'CLAVE_INTERNA' OF STRUCTURE <listado> TO <clave_interna>.
        IF <clave_interna> = <list>-clave_interna.
          ASSIGN COMPONENT 'COLOR' OF STRUCTURE <listado> TO FIELD-SYMBOL(<color>).
          CLEAR <color>.
          set_status_list( EXPORTING color = 'R' CHANGING list = <listado> ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF sy-subrc = 0.
      message = 'No puede grabar si hay duplicados'.
    ENDIF.
  ENDMETHOD.
  METHOD validar_registro.
  "#EC EMPTY_PROCEDURE
  ENDMETHOD.
  METHOD where_clave.
    CLEAR where.
    LOOP AT i_campos_tabla ASSIGNING FIELD-SYMBOL(<campo>) WHERE keyflag = 'X'.
      ASSIGN COMPONENT <campo>-fieldname OF STRUCTURE listado TO FIELD-SYMBOL(<fs>).
      DATA(l_w) = |{ <campo>-fieldname } = '{ <fs> }'|.
      IF where IS INITIAL.
        where = l_w.
      ELSE.
        where = |{ where } AND { l_w }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
