CLASS zcl_ap_alv DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_top_of_page,
        tipo     TYPE char1, "(P)arametro/(R)ango
        param    TYPE string,
        texto    TYPE string,
        tabla    TYPE tabname,
        campo    TYPE fieldname,
        no_ceros TYPE abap_bool,
        valor    TYPE string,
      END OF t_top_of_page.
    TYPES tt_top_of_page TYPE STANDARD TABLE OF t_top_of_page.
    TYPES:
      BEGIN OF t_orden,
        name  TYPE lvc_fname,
        order TYPE i,
      END OF t_orden.
    TYPES tt_orden TYPE TABLE OF t_orden.
    TYPES: BEGIN OF t_boton,
             boton TYPE c LENGTH 10.
             INCLUDE TYPE smp_dyntxt.
    TYPES:   ucomm TYPE sy-ucomm,
           END OF t_boton.
    TYPES:
      BEGIN OF t_hotspot,
        campo       TYPE fieldname,
        campo_ref   TYPE fieldname,
        transaccion TYPE string,
        filtro      TYPE string,
      END OF t_hotspot.
    TYPES tt_hotspot TYPE TABLE OF t_hotspot.
    TYPES t_botones  TYPE TABLE OF t_boton.
    TYPES:
      BEGIN OF t_buttons,
        f01 TYPE rsfunc_txt,
        f02 TYPE rsfunc_txt,
        f03 TYPE rsfunc_txt,
        f04 TYPE rsfunc_txt,
        f05 TYPE rsfunc_txt,
        f06 TYPE rsfunc_txt,
        f07 TYPE rsfunc_txt,
        f08 TYPE rsfunc_txt,
        f09 TYPE rsfunc_txt,
        f10 TYPE rsfunc_txt,
        f11 TYPE rsfunc_txt,
        f12 TYPE rsfunc_txt,
        f13 TYPE rsfunc_txt,
        f14 TYPE rsfunc_txt,
        f15 TYPE rsfunc_txt,
        f16 TYPE rsfunc_txt,
        f17 TYPE rsfunc_txt,
        f18 TYPE rsfunc_txt,
        f19 TYPE rsfunc_txt,
        f20 TYPE rsfunc_txt,
        f21 TYPE rsfunc_txt,
        f22 TYPE rsfunc_txt,
        f23 TYPE rsfunc_txt,
        f24 TYPE rsfunc_txt,
        f25 TYPE rsfunc_txt,
        f26 TYPE rsfunc_txt,
        f27 TYPE rsfunc_txt,
        f28 TYPE rsfunc_txt,
        f29 TYPE rsfunc_txt,
        f30 TYPE rsfunc_txt,
        f31 TYPE rsfunc_txt,
        f32 TYPE rsfunc_txt,
        f33 TYPE rsfunc_txt,
        f34 TYPE rsfunc_txt,
        f35 TYPE rsfunc_txt,
        m01 TYPE rsfunc_txt,
        m02 TYPE rsfunc_txt,
        m03 TYPE rsfunc_txt,
        m04 TYPE rsfunc_txt,
        m05 TYPE rsfunc_txt,
        m06 TYPE rsfunc_txt,
        m07 TYPE rsfunc_txt,
        m08 TYPE rsfunc_txt,
        m09 TYPE rsfunc_txt,
        m10 TYPE rsfunc_txt,
        m11 TYPE rsfunc_txt,
        m12 TYPE rsfunc_txt,
        m13 TYPE rsfunc_txt,
        m14 TYPE rsfunc_txt,
      END OF t_buttons.

    DATA o_alv            TYPE REF TO cl_salv_table.
    DATA o_functions      TYPE REF TO cl_salv_functions.
    DATA o_columns        TYPE REF TO cl_salv_columns_table.
    DATA o_column         TYPE REF TO cl_salv_column.
    DATA o_dspset         TYPE REF TO cl_salv_display_settings.
    DATA o_layout         TYPE REF TO cl_salv_layout.
    DATA o_selections     TYPE REF TO cl_salv_selections.
    DATA o_events         TYPE REF TO cl_salv_events_table.
    DATA i_columns        TYPE salv_t_column_ref.
    DATA o_content        TYPE REF TO cl_salv_form_element.
    DATA i_rows           TYPE salv_t_row.
    DATA campo_check      TYPE fieldname.
    DATA o_functions_list TYPE REF TO cl_salv_functions_list.
    DATA status           TYPE string READ-ONLY.

    CONSTANTS c_color_rojo TYPE i VALUE 6 ##NO_TEXT.

    DATA o_aggregation TYPE REF TO cl_salv_aggregations.

    CONSTANTS c_color_amarillo     TYPE i    VALUE 3 ##NO_TEXT.
    CONSTANTS c_color_azul         TYPE i    VALUE 1 ##NO_TEXT.
    CONSTANTS c_color_naranja      TYPE i    VALUE 7 ##NO_TEXT.
    CONSTANTS c_color_verde        TYPE i    VALUE 5 ##NO_TEXT.
    CONSTANTS c_color_gris         TYPE i    VALUE 2 ##NO_TEXT.
    CONSTANTS c_color_amarillo_osc TYPE i    VALUE 4 ##NO_TEXT.
    CONSTANTS c_sem_verde          TYPE num1 VALUE 3 ##NO_TEXT.
    CONSTANTS c_sem_rojo           TYPE num1 VALUE 1 ##NO_TEXT.
    CONSTANTS c_sem_ambar          TYPE num1 VALUE 2 ##NO_TEXT.

    DATA ya_mostrado TYPE abap_bool.
    DATA o_top_page  TYPE REF TO zcl_ap_alv_form.
    DATA i_columnas  TYPE zap_alv_columnas_t.
    DATA sin_buffer  TYPE abap_bool VALUE '' ##NO_TEXT.
    DATA i_columnas3 TYPE zap_alv_columnas3_t.

    CONSTANTS c_color_blanco TYPE i VALUE 0 ##NO_TEXT.

    DATA top_of_page_auto   TYPE abap_bool  VALUE '' ##NO_TEXT.
    DATA top_of_page_titulo TYPE string     VALUE '' ##NO_TEXT.
    DATA logo               TYPE bds_typeid VALUE '' ##NO_TEXT.

    CONSTANTS c_ico_verde        TYPE icon_d VALUE icon_green_light ##NO_TEXT.
    CONSTANTS c_ico_ambar        TYPE icon_d VALUE icon_yellow_light ##NO_TEXT.
    CONSTANTS c_ico_rojo         TYPE icon_d VALUE icon_red_light ##NO_TEXT.
    CONSTANTS c_ico_warning      TYPE icon_d VALUE icon_message_warning ##NO_TEXT.
    CONSTANTS c_ico_bloqueo      TYPE icon_d VALUE icon_locked ##NO_TEXT.
    CONSTANTS c_ico_verde_simple TYPE icon_d VALUE icon_led_green ##NO_TEXT.
    CONSTANTS c_ico_ambar_simple TYPE icon_d VALUE icon_led_yellow ##NO_TEXT.
    CONSTANTS c_ico_rojo_simple  TYPE icon_d VALUE icon_led_red ##NO_TEXT.
    CONSTANTS c_ico_check_ok     TYPE icon_d VALUE icon_checked ##NO_TEXT.
    CONSTANTS c_ico_check_ko     TYPE icon_d VALUE icon_incomplete ##NO_TEXT.
    CONSTANTS c_ico_check_fallo  TYPE icon_d VALUE icon_failure ##NO_TEXT.
    CONSTANTS c_ico_dummy        TYPE icon_d VALUE icon_dummy ##NO_TEXT.
    CONSTANTS c_ico_cerrado      TYPE icon_d VALUE icon_locked ##NO_TEXT.
    CONSTANTS c_ico_abierto      TYPE icon_d VALUE icon_unlocked ##NO_TEXT.
    CONSTANTS c_ico_papelera     TYPE icon_d VALUE icon_delete ##NO_TEXT.
    CONSTANTS c_ico_stop         TYPE icon_d VALUE icon_message_critical ##NO_TEXT.

    DATA buttons      TYPE t_buttons.
    DATA i_botones    TYPE t_botones.
    DATA optimize     TYPE abap_bool.
    DATA i_hotspot    TYPE tt_hotspot.
    DATA key_layout   TYPE salv_s_layout_key.
    DATA o_alv_helper TYPE REF TO zcl_ap_alv.
    DATA o_dev        TYPE REF TO zcl_ap_dev.
    DATA buttons_v0   TYPE t_buttons.
    DATA i_botones_v0 TYPE t_botones.

    METHODS constructor
      IMPORTING tabla              TYPE string                  DEFAULT 'I_LISTADO'
                list_display       TYPE abap_bool               DEFAULT ''
                cprog              TYPE sy-cprog                DEFAULT sy-cprog
                optimize           TYPE abap_bool               DEFAULT 'X'
                botones_standard   TYPE abap_bool               DEFAULT 'X'
                !color             TYPE lvc_fname               DEFAULT ' '
                !status            TYPE any                     DEFAULT ' '
                sel                TYPE c                       DEFAULT ''
                campo_check        TYPE any                     DEFAULT ''
                lights             TYPE string                  DEFAULT ''
                container_name     TYPE c                       DEFAULT ''
                no_layout          TYPE abap_bool               DEFAULT ''
                r_container        TYPE REF TO cl_gui_container OPTIONAL
                restriccion_layout TYPE int4                    DEFAULT if_salv_c_layout=>restrict_none
                inicio_columna     TYPE any                     DEFAULT ''
                sin_buffer         TYPE abap_bool               DEFAULT ''
                top_of_page_auto   TYPE abap_bool               DEFAULT ''
                top_of_page_titulo TYPE any                     DEFAULT ''
                logo               TYPE any                     DEFAULT ''
                status_prog        TYPE any                     DEFAULT ''
                generar_docking    TYPE int4                    DEFAULT 0
                nombre_docking     TYPE any                     DEFAULT 'DOCK_CONT'
                !handle            TYPE slis_handl              DEFAULT ''
                o_dev              TYPE REF TO zcl_ap_dev       OPTIONAL
                no_agrupar_celdas  TYPE any                     DEFAULT ''.

    METHODS show.

    METHODS handle_double_click
      FOR EVENT if_salv_events_actions_table~double_click OF cl_salv_events_table
      IMPORTING !row
                !column.

    METHODS handle_user_command
      FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

    METHODS handle_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING !row
                !column.

    METHODS set_field
      IMPORTING op     TYPE text10
                campo  TYPE any
                valor  TYPE any OPTIONAL
                valor2 TYPE any OPTIONAL.

    METHODS handle_top_of_page
      FOR EVENT top_of_page OF cl_salv_events_table
      IMPORTING r_top_of_page
                !page
                table_index.

    METHODS handle_end_of_page
      FOR EVENT end_of_page OF cl_salv_events_table
      IMPORTING r_end_of_page
                !page.

    METHODS top_of_page
      IMPORTING nreg    TYPE int4           DEFAULT 0
                i_param TYPE tt_top_of_page OPTIONAL
                excluir TYPE any            DEFAULT ''.

    METHODS end_of_page.

    METHODS show_popup
      IMPORTING start_column TYPE i         DEFAULT 1
                end_column   TYPE i         DEFAULT 100
                start_line   TYPE i         DEFAULT 1
                end_line     TYPE i         DEFAULT 20
                sel          TYPE abap_bool DEFAULT 'X'
                titulo       TYPE any       DEFAULT ''.

    METHODS set_color
      IMPORTING campo  TYPE string OPTIONAL
                !color TYPE i.

    METHODS set_status
      IMPORTING !status TYPE sypfkey
                repid   TYPE sy-repid DEFAULT sy-repid.

    METHODS get_seleccion
      IMPORTING deshacer_ordenacion TYPE abap_bool DEFAULT 'X'
      EXPORTING hay_sel             TYPE abap_bool
      CHANGING  t_tabla             TYPE table     OPTIONAL.

    METHODS set_orden
      IMPORTING campo    TYPE string             OPTIONAL
                !up      TYPE char1              DEFAULT 'X'
                down     TYPE char1              DEFAULT ''
                !group   TYPE salv_de_sort_group DEFAULT if_salv_c_sort=>group_none
                subtot   TYPE char1              DEFAULT ''
                compress TYPE abap_bool          DEFAULT ''
      PREFERRED PARAMETER campo.

    METHODS set_agregacion
      IMPORTING campo TYPE any
                tipo  TYPE salv_de_aggregation DEFAULT if_salv_c_aggregation=>total.

    METHODS refresh
      IMPORTING s_stable     TYPE lvc_s_stbl       OPTIONAL
                refresh_mode TYPE salv_de_constant DEFAULT if_salv_c_refresh=>soft.

    METHODS set_top_of_page
      IMPORTING nreg    TYPE int4           DEFAULT 0
                i_param TYPE tt_top_of_page OPTIONAL
                excluir TYPE any            DEFAULT ''.

    METHODS set_end_of_page.

    METHODS get_f4_layout
      RETURNING VALUE(layout) TYPE disvariant-variant.

    METHODS set_layout
      IMPORTING !layout TYPE disvariant-variant.

    METHODS get_fila_activa
      RETURNING VALUE(fila) TYPE int4.

    METHODS constructor_tabla
      IMPORTING list_display       TYPE abap_bool  DEFAULT ''
                optimize           TYPE abap_bool  DEFAULT 'X'
                botones_standard   TYPE abap_bool  DEFAULT 'X'
                !color             TYPE lvc_fname  DEFAULT ' '
                !status            TYPE sypfkey    DEFAULT ' '
                sel                TYPE c          DEFAULT ''
                campo_check        TYPE fieldname  DEFAULT ''
                lights             TYPE string     DEFAULT ''
                container_name     TYPE c          DEFAULT ''
                tabla              TYPE string     DEFAULT ''
                no_layout          TYPE abap_bool  DEFAULT ''
                restriccion_layout TYPE int4       DEFAULT if_salv_c_layout=>restrict_none
                cprog              TYPE sy-cprog   DEFAULT sy-cprog
                top_of_page_auto   TYPE abap_bool  DEFAULT ''
                top_of_page_titulo TYPE any        DEFAULT ''
                status_prog        TYPE any        DEFAULT ''
                !handle            TYPE slis_handl DEFAULT ''
      CHANGING  t_tabla            TYPE table.

    METHODS set_seleccion
      CHANGING t_tabla TYPE table OPTIONAL.

    METHODS get_columna_activa
      RETURNING VALUE(columna) TYPE salv_t_column.

    METHODS get_nombre_columna_activa
      RETURNING VALUE(col) TYPE string.

    METHODS get_celda_activa
      RETURNING VALUE(celda) TYPE salv_t_cell.

    METHODS free.

    CLASS-METHODS append_color
      IMPORTING campo       TYPE any  DEFAULT ''
                !color      TYPE int4 OPTIONAL
                colorc      TYPE any  OPTIONAL
                int         TYPE int1 DEFAULT 0
                inv         TYPE int4 DEFAULT 0
      CHANGING  tabla_color TYPE lvc_t_scol.

    CLASS-METHODS traduce_color
      IMPORTING codigo       TYPE any
      RETURNING VALUE(color) TYPE int4.

    METHODS get_datos_celda_activa
      RETURNING VALUE(celda) TYPE salv_s_cell.

    METHODS set_field_input
      IMPORTING campo TYPE any OPTIONAL
      PREFERRED PARAMETER campo.

    METHODS set_field_noout
      IMPORTING campo TYPE any OPTIONAL
      PREFERRED PARAMETER campo.

    METHODS set_field_text
      IMPORTING campo  TYPE any OPTIONAL
                valor  TYPE any OPTIONAL
                valor2 TYPE any OPTIONAL
      PREFERRED PARAMETER campo.

    METHODS exportar_excel
      IMPORTING fichero_salida  TYPE string    OPTIONAL
                cerrar          TYPE abap_bool DEFAULT ''
                columna_inicial TYPE i         DEFAULT 1
                col_texto       TYPE string    DEFAULT ''
                col_importes    TYPE string    DEFAULT ''
                !replacement    TYPE any       DEFAULT 46
                rightfooter     TYPE string    OPTIONAL
                centerheader    TYPE string    OPTIONAL
                preview         TYPE abap_bool OPTIONAL
                dialogo_fichero TYPE abap_bool DEFAULT ''
      CHANGING  t_tabla         TYPE table     OPTIONAL.

    METHODS set_field_decimales
      IMPORTING campo TYPE any OPTIONAL
                valor TYPE any DEFAULT 0
      PREFERRED PARAMETER campo.

    METHODS get_default_layout
      RETURNING VALUE(layout) TYPE disvariant-variant.

    METHODS exportar_csv
      IMPORTING fichero_salida             TYPE string    OPTIONAL
                servidor                   TYPE abap_bool DEFAULT ''
                dialogo_fichero            TYPE abap_bool DEFAULT ''
                separador                  TYPE c         DEFAULT ';'
                mostrar_mensajes           TYPE abap_bool DEFAULT 'X'
                quitar_caracteres_extranos TYPE abap_bool DEFAULT ''
      EXPORTING i_csv                      TYPE table_of_strings
      CHANGING  t_tabla                    TYPE table     OPTIONAL.

    METHODS get_csv_as_string
      IMPORTING quitar_caracteres_extranos TYPE abap_bool DEFAULT ''
      EXPORTING string_csv                 TYPE string
      CHANGING  t_tabla                    TYPE table     OPTIONAL.

    METHODS set_columnas
      IMPORTING i_columnas TYPE zap_alv_columnas_t OPTIONAL
                campo      TYPE any                OPTIONAL
      PREFERRED PARAMETER i_columnas.

    METHODS set_columnas3
      IMPORTING i_columnas TYPE zap_alv_columnas3_t OPTIONAL
                campo      TYPE any                 OPTIONAL
      PREFERRED PARAMETER i_columnas.

    METHODS correguir_variantes
      IMPORTING i_columnas3 TYPE zap_alv_columnas3_t OPTIONAL
                i_columnas  TYPE zap_alv_columnas_t  OPTIONAL.

    METHODS fill_columnas
      IMPORTING campo             TYPE any
      RETURNING VALUE(i_columnas) TYPE zap_alv_columnas_t.

    METHODS set_filter
      IMPORTING campo       TYPE any
                valor       TYPE any
                valor_hasta TYPE any OPTIONAL
                !sign       TYPE any DEFAULT 'I'
                !option     TYPE any DEFAULT 'EQ'.

    CLASS-METHODS show_popup_st
      IMPORTING start_column        TYPE i                 DEFAULT 1
                end_column          TYPE i                 DEFAULT 100
                start_line          TYPE i                 DEFAULT 1
                !color              TYPE lvc_fname         DEFAULT ''
                field_dosum         TYPE any               DEFAULT ''
                field_noout         TYPE any               DEFAULT ''
                field_orden         TYPE any               DEFAULT ''
                field_hotspot       TYPE any               DEFAULT ''
                filtro_op1          TYPE text10            DEFAULT 'EQ'
                filtro_campo1       TYPE any               OPTIONAL
                filtro_valor1       TYPE any               OPTIONAL
                filtro_valor_hasta  TYPE any               OPTIONAL
                filtro_op2          TYPE text10            DEFAULT 'EQ'
                filtro_campo2       TYPE any               OPTIONAL
                filtro_valor2       TYPE any               OPTIONAL
                filtro_valor_hasta2 TYPE any               OPTIONAL
                filtro_op3          TYPE text10            DEFAULT 'EQ'
                filtro_campo3       TYPE any               OPTIONAL
                filtro_valor3       TYPE any               OPTIONAL
                filtro_valor_hasta3 TYPE any               OPTIONAL
                filtro_op4          TYPE text10            DEFAULT 'EQ'
                filtro_campo4       TYPE any               OPTIONAL
                filtro_valor4       TYPE any               OPTIONAL
                filtro_valor_hasta4 TYPE any               OPTIONAL
                no_layout           TYPE abap_bool         DEFAULT ''
                restriccion_layout  TYPE int4              DEFAULT if_salv_c_layout=>restrict_none
                field_pos           TYPE any               DEFAULT ''
                o_alv_helper        TYPE REF TO zcl_ap_alv OPTIONAL
      CHANGING  t_tabla             TYPE table.

    CLASS-METHODS exportar_excel_st
      IMPORTING fichero_salida  TYPE string    OPTIONAL
                cerrar          TYPE abap_bool DEFAULT ''
                columna_inicial TYPE i         DEFAULT 1
                col_texto       TYPE string    DEFAULT ''
                col_importes    TYPE string    DEFAULT ''
                !replacement    TYPE any       DEFAULT 46
                rightfooter     TYPE string    OPTIONAL
                centerheader    TYPE string    OPTIONAL
                preview         TYPE abap_bool OPTIONAL
                dialogo_fichero TYPE abap_bool DEFAULT ''
      EXPORTING mensaje         TYPE bapi_msg
      CHANGING  t_tabla         TYPE table     OPTIONAL.

    METHODS exportar_xml
      IMPORTING tipo_xml               TYPE salv_bs_constant DEFAULT if_salv_bs_xml=>c_type_mhtml
                fichero                TYPE any              OPTIONAL
                dialogo_grabar_fichero TYPE abap_bool        DEFAULT ''
      EXPORTING xstring                TYPE xstring
                solix_tab              TYPE solix_tab
                xml_size               TYPE i.

    METHODS get_columnas
      RETURNING VALUE(i_col) TYPE zap_alv_columnas3_t.

    METHODS ocultar_columnas_vacias
      IMPORTING t_tabla TYPE table OPTIONAL.

    CLASS-METHODS set_icono
      IMPORTING icono           TYPE any
                mensaje         TYPE any
                permitir_filtro TYPE any DEFAULT '*'
      RETURNING VALUE(valor)    TYPE string.

    METHODS excluir_boton
      IMPORTING boton TYPE any.

    METHODS get_valor_campo
      IMPORTING registro     TYPE any
                colum        TYPE numc2
                campo        TYPE any DEFAULT ''
      RETURNING VALUE(valor) TYPE string.

    METHODS get_col
      IMPORTING colum      TYPE numc2
                campo      TYPE any DEFAULT ''
                aux1       TYPE any DEFAULT ''
      RETURNING VALUE(col) TYPE zap_alv_columnas.

    METHODS set_valor_campo
      IMPORTING colum        TYPE numc2     OPTIONAL
                campo        TYPE any       DEFAULT ''
                valor        TYPE any
                quitar_style TYPE abap_bool DEFAULT ''
                quitar_color TYPE abap_bool DEFAULT ''
                campo_style  TYPE fieldname DEFAULT 'STYLE'
                campo_color  TYPE fieldname DEFAULT 'COLOR'
                aux1         TYPE any       DEFAULT ''
      CHANGING  registro     TYPE any.

    METHODS get_lista_columnas
      IMPORTING excluir_campos TYPE any DEFAULT ''
                solo_campos    TYPE any DEFAULT ''
      RETURNING VALUE(lista)   TYPE string.

    METHODS set_titulo_col_sin_nombre.

    METHODS add_button
      IMPORTING button   TYPE sy-ucomm
                !text    TYPE smp_dyntxt-text      OPTIONAL
                !icon    TYPE smp_dyntxt-icon_id   OPTIONAL
                qinfo    TYPE smp_dyntxt-quickinfo OPTIONAL
                !allowed TYPE abap_bool            DEFAULT abap_true
                forzar   TYPE abap_bool            DEFAULT ''
                ucomm    TYPE sy-ucomm             DEFAULT ''
                status2  TYPE abap_bool            DEFAULT ''
                borrar   TYPE abap_bool            DEFAULT ''.

    METHODS show_button
      IMPORTING button TYPE sy-ucomm.

    METHODS get_boton_quickinfo
      IMPORTING boton            TYPE any
      RETURNING VALUE(quickinfo) TYPE smp_dyntxt-quickinfo.

    METHODS get_boton_text
      IMPORTING boton       TYPE any
      RETURNING VALUE(text) TYPE smp_dyntxt-text.

    METHODS exportar_xlsx
      IMPORTING tipo_xml               TYPE salv_bs_constant DEFAULT if_salv_bs_xml=>c_type_xlsx
                fichero                TYPE any              OPTIONAL
                dialogo_grabar_fichero TYPE abap_bool        DEFAULT ''
                abrir_excel            TYPE abap_bool        DEFAULT ''
                dir_temp               TYPE abap_bool        DEFAULT ''
      EXPORTING xstring                TYPE xstring.

    METHODS set_field_quitar
      IMPORTING campo TYPE any OPTIONAL
      PREFERRED PARAMETER campo.

    METHODS get_orden
      EXPORTING orden TYPE tt_orden.

    METHODS get_datos_layout
      IMPORTING reordenar_tabla TYPE abap_bool DEFAULT 'X'
                tabla_ref       TYPE tabname   DEFAULT ''
      EXPORTING !sort           TYPE kkblo_t_sortinfo
                !filter         TYPE kkblo_t_filter
      CHANGING  t_tabla         TYPE table.

    METHODS remove_button
      IMPORTING button TYPE sy-ucomm.

    METHODS get_registros_filtrados
      IMPORTING i_tabla               TYPE table
      EXPORTING i_registros_filtrados TYPE table.

    METHODS set_field_hotspot
      IMPORTING campo  TYPE any
                !auto  TYPE abap_bool DEFAULT ''
                valor  TYPE any       OPTIONAL
                valor2 TYPE any       OPTIONAL.

    CLASS-METHODS visualizar_obj
      IMPORTING !hotspot       TYPE t_hotspot OPTIONAL
                !list          TYPE any
                !column        TYPE any       OPTIONAL
      EXPORTING salida         TYPE string
                !mod           TYPE abap_bool
      RETURNING VALUE(message) TYPE bapi_msg.

    METHODS visualizar_objeto
      IMPORTING !hotspot       TYPE t_hotspot OPTIONAL
                !list          TYPE any
                !column        TYPE any
                !row           TYPE any       OPTIONAL
      RETURNING VALUE(message) TYPE bapi_msg.

    METHODS save_buttons
      IMPORTING status2 TYPE abap_bool DEFAULT ''.

    METHODS restore_buttons
      IMPORTING status2 TYPE abap_bool DEFAULT ''.

    CLASS-METHODS display_data_deep_enabled
      IMPORTING it_data TYPE table.

  PROTECTED SECTION.
    DATA nombre_tabla     TYPE string.
    DATA ucomm            TYPE sy-ucomm.
    DATA ucomm_text       TYPE string.
    DATA accion_ejecutada TYPE abap_bool.
    DATA check_ucomm_sel  TYPE string.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_allowed_but,
        function TYPE sy-ucomm,
      END OF t_allowed_but.
    TYPES tt_excluded_but TYPE STANDARD TABLE OF sy-ucomm.
    TYPES tt_allowed_but  TYPE STANDARD TABLE OF t_allowed_but.

    DATA fully_dynamic    TYPE abap_bool.
    DATA excluded_buttons TYPE tt_excluded_but.
    DATA o_sorts          TYPE REF TO cl_salv_sorts.
    DATA o_container      TYPE REF TO cl_gui_custom_container.
    DATA campo_color      TYPE string.

    CLASS-DATA o_event_h TYPE REF TO object.

    DATA allowed_buttons TYPE tt_allowed_but.
    DATA table_ref       TYPE REF TO data.

    METHODS inicializa_objetos
      IMPORTING list_display       TYPE abap_bool         DEFAULT ''
                optimize           TYPE abap_bool         DEFAULT 'X'
                botones_standard   TYPE abap_bool         DEFAULT 'X'
                !color             TYPE lvc_fname         DEFAULT ' '
                !status            TYPE any               DEFAULT ' '
                sel                TYPE c                 DEFAULT ''
                campo_check        TYPE any               DEFAULT ''
                lights             TYPE string            DEFAULT ''
                tabla              TYPE string            DEFAULT 'I_LISTADO'
                no_layout          TYPE abap_bool         DEFAULT ''
                restriccion_layout TYPE int4              DEFAULT if_salv_c_layout=>restrict_none
                inicio_columna     TYPE any               DEFAULT ''
                sin_buffer         TYPE abap_bool         DEFAULT ''
                cprog              TYPE sy-cprog          DEFAULT sy-cprog
                top_of_page_auto   TYPE abap_bool         DEFAULT ''
                top_of_page_titulo TYPE any               DEFAULT ''
                logo               TYPE any               DEFAULT ''
                status_prog        TYPE any               DEFAULT ''
                !handle            TYPE slis_handl        DEFAULT ''
                o_dev              TYPE REF TO zcl_ap_dev OPTIONAL
                no_agrupar_celdas  TYPE any               DEFAULT ''.
endclass. "ZCL_AP_ALV definition
class ZCL_AP_ALV implementation.
  METHOD add_button.
    DATA: but     TYPE smp_dyntxt,
          l_boton TYPE t_boton.

    FIELD-SYMBOLS <bt> TYPE any.

    IF button IS INITIAL.
      RETURN.
    ENDIF.

    IF text IS INITIAL AND icon IS INITIAL AND borrar IS INITIAL.
      RETURN.
    ENDIF.

    IF borrar IS INITIAL.
      but-icon_id   = icon.
      but-icon_text = text.
      but-text      = text.
      but-quickinfo = qinfo.
    ELSE.
      CLEAR but.
    ENDIF.

    MOVE-CORRESPONDING but TO l_boton.
    DELETE i_botones WHERE boton = button.

    l_boton-boton = button.
    l_boton-ucomm = ucomm.
    APPEND l_boton TO i_botones.

    ASSIGN COMPONENT button OF STRUCTURE buttons TO <bt>.
    IF <bt> IS ASSIGNED.
      IF <bt> IS INITIAL OR forzar = 'X' OR borrar = 'X'.
        <bt> = but.
        IF status2 = 'X'.
          PERFORM set_boton IN PROGRAM zap_status2 USING buttons.
        ELSE.
          PERFORM set_boton IN PROGRAM zap_status USING buttons.
        ENDIF.
        IF allowed = abap_true.
          show_button( button = button ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD append_color.
    DATA: i_column TYPE TABLE OF lvc_fname,
          l_column TYPE lvc_fname,
          ls_color TYPE lvc_s_scol.

    SPLIT campo AT ',' INTO TABLE i_column.
    IF i_column IS INITIAL.
      APPEND '' TO i_column.
    ENDIF.

    LOOP AT i_column INTO l_column.
      TRANSLATE l_column TO UPPER CASE.
      CLEAR ls_color.
      ls_color-fname = l_column.
      ls_color-color-int = int.
      ls_color-color-inv = inv.
      IF NOT color IS INITIAL.
        ls_color-color-col = color.
      ELSE.
        ls_color-color-col = traduce_color( colorc ).
      ENDIF.
      APPEND ls_color TO tabla_color.
    ENDLOOP.
  ENDMETHOD.
  METHOD constructor.
    DATA: l_tabla          TYPE string,
          l_tabla2         TYPE string,
          l_error          TYPE string,
          l_container_name TYPE c LENGTH 80,
          l_string         TYPE string,
          o_excepcion      TYPE REF TO cx_salv_msg.
    DATA: lv_repid TYPE sy-repid,
          lv_dynnr TYPE sy-dynnr.
    DATA: lo_dock TYPE REF TO cl_gui_docking_container,
          lo_cont TYPE REF TO cl_gui_container.

    FIELD-SYMBOLS <tabla> TYPE table.

*
    IF tabla = 'HELPER'.
      RETURN.
    ENDIF.

* Si no se informa la tabla, debemos utilizar el falso constructor diferido
    IF tabla IS INITIAL.
      RETURN.
    ENDIF.

    IF NOT cprog IS INITIAL.
      CONCATENATE '(' cprog ')' tabla INTO l_tabla.
    ELSE.
      l_tabla = tabla.
    ENDIF.

    IF NOT l_tabla CS '[]'.
      CONCATENATE l_tabla '[]' INTO l_tabla.
    ENDIF.

    nombre_tabla = l_tabla.
    ASSIGN (l_tabla) TO <tabla>.
    IF sy-subrc <> 0.
      IF NOT cprog IS INITIAL.
        CONCATENATE '(' cprog ')o_prog->' tabla INTO l_tabla2.
        ASSIGN (l_tabla2) TO <tabla>.
        IF sy-subrc = 0.
          nombre_tabla = l_tabla2.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT <tabla> IS ASSIGNED.
      CONCATENATE 'La tabla'(tab) l_tabla 'no existe'(noe)
                  INTO l_error SEPARATED BY space.
      MESSAGE l_error TYPE 'I'
              DISPLAY LIKE 'E'.
    ELSE.
      table_ref = REF #( <tabla> ).
    ENDIF.

    IF NOT container_name IS INITIAL.
      l_container_name = container_name.
    ELSEIF generar_docking > 0.
      lv_repid = 'SAPMSSY0'.
      lv_dynnr = '0120'.

*
*   Create a docking control at bottom
      IF lo_dock IS NOT INITIAL.
        RETURN.
      ENDIF.
      CREATE OBJECT lo_dock
        EXPORTING
          repid                       = lv_repid
          dynnr                       = lv_dynnr
          ratio                       = generar_docking
          side                        = cl_gui_docking_container=>dock_at_bottom
          name                        = nombre_docking
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE 'Error in the Docking control'(edc) TYPE 'S'.
      ELSE.
        l_container_name = nombre_docking.
      ENDIF.
    ENDIF.

    TRY.
        IF NOT l_container_name IS INITIAL.
          l_string = l_container_name.

          IF l_container_name = nombre_docking.
            lo_cont ?= lo_dock.
            cl_salv_table=>factory(
              EXPORTING
                list_display   = list_display
                r_container    = lo_cont
                container_name = l_string
              IMPORTING
                r_salv_table   = o_alv
              CHANGING
                t_table        = <tabla> ).
          ELSE.
            o_container = NEW #(
                container_name = l_container_name ).

            cl_salv_table=>factory(
              EXPORTING
                list_display   = list_display
                r_container    = o_container
                container_name = l_string
              IMPORTING
                r_salv_table   = o_alv
              CHANGING
                t_table        = <tabla> ).
          ENDIF.
        ELSEIF r_container IS INITIAL.
          cl_salv_table=>factory(
            EXPORTING
              list_display = list_display
            IMPORTING
              r_salv_table = o_alv
            CHANGING
              t_table      = <tabla> ).
        ELSE.
          cl_salv_table=>factory(
            EXPORTING
              list_display = list_display
              r_container  = r_container
            IMPORTING
              r_salv_table = o_alv
            CHANGING
              t_table      = <tabla> ).
        ENDIF.

        inicializa_objetos( list_display       = list_display
                            optimize           = optimize
                            botones_standard   = botones_standard
                            color              = color
                            status             = status
                            sel                = sel
                            campo_check        = campo_check
                            lights             = lights
                            tabla              = tabla
                            no_layout          = no_layout
                            restriccion_layout = restriccion_layout
                            inicio_columna     = inicio_columna
                            sin_buffer         = sin_buffer
                            top_of_page_auto   = top_of_page_auto
                            top_of_page_titulo = top_of_page_titulo
                            logo               = logo
                            status_prog        = status_prog
                            handle             = handle
                            o_dev              = o_dev
                            no_agrupar_celdas  = no_agrupar_celdas ).

      CATCH cx_salv_msg INTO o_excepcion.
        l_string = o_excepcion->get_text( ).
        MESSAGE l_string TYPE 'E'
                DISPLAY LIKE 'I'.
    ENDTRY.
  ENDMETHOD.
  METHOD constructor_tabla.
    DATA l_string TYPE string.

    TRY.
        IF NOT container_name IS INITIAL.
          o_container = NEW #(
              container_name = container_name ).

          l_string = container_name.

          cl_salv_table=>factory(
            EXPORTING
              list_display   = list_display
              r_container    = o_container
              container_name = l_string
            IMPORTING
              r_salv_table   = o_alv
            CHANGING
              t_table        = t_tabla ).
        ELSE.
          cl_salv_table=>factory(
            EXPORTING
              list_display = list_display
            IMPORTING
              r_salv_table = o_alv
            CHANGING
              t_table      = t_tabla ).
        ENDIF.

        table_ref = REF #( t_tabla ).

        inicializa_objetos( list_display       = list_display
                            optimize           = optimize
                            botones_standard   = botones_standard
                            color              = color
                            status             = status
                            sel                = sel
                            campo_check        = campo_check
                            lights             = lights
                            tabla              = tabla
                            cprog              = cprog
                            no_layout          = no_layout
                            handle             = handle
                            status_prog        = status_prog
                            restriccion_layout = restriccion_layout
                            top_of_page_auto   = top_of_page_auto
                            top_of_page_titulo = top_of_page_titulo ).

      CATCH cx_salv_msg.
        MESSAGE 'ALV display not possible'(adn) TYPE 'I'
                DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
  METHOD correguir_variantes.
    DATA: i_ltdx        TYPE TABLE OF ltdx,
          rs_varkey     TYPE ltdxkey,
          rt_dbfieldcat TYPE TABLE OF ltdxdata.

    FIELD-SYMBOLS: <ltdx>  TYPE ltdx,
                   <col>   TYPE zap_alv_columnas,
                   <campo> TYPE ltdxdata,
                   <col3>  TYPE zap_alv_columnas3.

    SELECT relid report handle log_group username variant type FROM ltdx
      INTO CORRESPONDING FIELDS OF TABLE i_ltdx
     WHERE relid  = 'LT'
       AND report = key_layout-report
       AND type   = 'F'.

    SORT i_ltdx.
    DELETE ADJACENT DUPLICATES FROM i_ltdx.

    LOOP AT i_ltdx ASSIGNING <ltdx>.
      MOVE-CORRESPONDING <ltdx> TO rs_varkey.

      IMPORT lt_dbfieldcat TO rt_dbfieldcat
             FROM DATABASE ltdx(lt)
             CLIENT sy-mandt
             ID rs_varkey.

      LOOP AT i_columnas ASSIGNING <col> WHERE no_variante = 'X'.
        LOOP AT rt_dbfieldcat ASSIGNING <campo> WHERE     key1  = <col>-campo
                                                      AND param = 'NO_OUT'.
          <campo>-value = <col>-noout.
        ENDLOOP.
      ENDLOOP.

      LOOP AT i_columnas3 ASSIGNING <col3> WHERE no_variante = 'X'.
        LOOP AT rt_dbfieldcat ASSIGNING <campo> WHERE     key1  = <col3>-campo
                                                      AND param = 'NO_OUT'.
          <campo>-value = <col3>-noout.
        ENDLOOP.
      ENDLOOP.

      EXPORT lt_dbfieldcat FROM rt_dbfieldcat
             TO DATABASE ltdx(lt)
             ID rs_varkey.
    ENDLOOP.
  ENDMETHOD.
  METHOD display_data_deep_enabled. "#EC *
*    DATA lo_table_descr  TYPE REF TO cl_abap_tabledescr.
*    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.
*
*    lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_data ).
*    lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
*    "note: this might fail if it's a single-coloum-Table.
*
*    DATA(lt_components_with_ref) = lo_struct_descr->get_components( ).
*    DATA(lt_components_simple_tab) = lo_struct_descr->components.
*
*    "find and remove the components that would hurt us (that is: dump)
*    LOOP AT lt_components_simple_tab ASSIGNING FIELD-SYMBOL(<component>)
*                                    WHERE type_kind = cl_abap_tabledescr=>typekind_table.
*      DELETE lt_components_with_ref WHERE name = <component>-name.
*    ENDLOOP.
*
*    " New Structure + Table.
*    lo_struct_descr = cl_abap_structdescr=>create( p_components = lt_components_with_ref ).
*    lo_table_descr = cl_abap_tabledescr=>create( p_line_type = lo_struct_descr ).
*    DATA lt_new TYPE REF TO data.
*    " Create the Data-Ref of the new Type.
*    CREATE DATA lt_new TYPE HANDLE lo_table_descr.
*    " Fill it with data:
*    lt_new->* = CORRESPONDING #( it_data ).
*
*    TRY.
*        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lref_alv)
*                                CHANGING  t_table      = lt_new->*  ).
*      CATCH cx_salv_msg.
*    ENDTRY.
*
*    lref_alv->get_layout( )->set_key( VALUE #( report = sy-repid ) ).
*    lref_alv->get_layout( )->set_default( abap_true ).
*    lref_alv->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).
*    lref_alv->get_functions( )->set_all( abap_true ).
*    lref_alv->display( ).
  ENDMETHOD.
  METHOD end_of_page.
    CLEAR o_content.
*  DATA: lr_header TYPE REF TO cl_salv_form_header_info,
*        l_text    TYPE string.
*  l_text = 'end of page for the report'.
*  CREATE OBJECT lr_header
*    EXPORTING
*      text    = l_text
*      tooltip = l_text.
*  o_content = lr_header.
  ENDMETHOD.
  METHOD excluir_boton.
    DATA: lt_func_list TYPE salv_t_ui_func,
          i_botones    TYPE TABLE OF string,
          la_func_list LIKE LINE OF lt_func_list,
          l_boton      TYPE string.

* Get all functions
    lt_func_list = o_functions->get_functions( ).
    SPLIT boton AT ',' INTO TABLE i_botones.

* Now hide the MYFUNCTION
    LOOP AT lt_func_list INTO la_func_list.
      LOOP AT i_botones INTO l_boton.
        IF la_func_list-r_function->get_name( ) = l_boton.
          la_func_list-r_function->set_visible( ' ' ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD exportar_csv.
    TYPES: BEGIN OF t_cols,
             orden  TYPE n LENGTH 3,
             name   TYPE fieldname,
             titulo TYPE string,
             tipo   TYPE abap_typekind,
           END OF t_cols.

    DATA: i_columns      TYPE salv_t_column_ref,
          l_campos       TYPE abap_componentdescr,
          i_campos       TYPE abap_component_tab,
          lr_data        TYPE REF TO data,
          l_orden        TYPE i,
          l_string       TYPE string,
          l_column       TYPE salv_s_column_ref,
          l_col          TYPE t_cols,
          i_cols         TYPE TABLE OF t_cols,
          l_campo        TYPE c LENGTH 40,
          l_valor        TYPE c LENGTH 255,
          l_valor_string TYPE string,
          l_neg          TYPE c LENGTH 1.
    DATA: i_ddfiels TYPE ddfields,
          l_dfies   TYPE dfies.

    FIELD-SYMBOLS: <tabla> TYPE table,
                   <linea> TYPE any,
                   <campo> TYPE any.

    DEFINE add_campo.
      IF l_col-tipo = 'C'.
        REPLACE ALL OCCURRENCES OF separador IN &1 WITH ':'.
      ENDIF.
      WRITE &1 TO l_valor LEFT-JUSTIFIED.
      IF l_col-tipo = 'P'.
        IF l_valor CS '-'.
          REPLACE ALL OCCURRENCES OF '-' IN l_valor WITH ''.
          CONCATENATE '-' l_valor INTO l_valor.
        ENDIF.
      ENDIF.

      IF quitar_caracteres_extranos = 'X'.
        l_valor_string = l_valor.
        zcl_ap_string=>quitar_caracteres_extranos( CHANGING string = l_valor_string ).
        l_valor = l_valor_string.
      ENDIF.

      IF l_col-orden = 1.
        l_string = l_valor.
      ELSE.
        CONCATENATE l_string separador l_valor INTO l_string.
      ENDIF.
    END-OF-DEFINITION.

    i_columns = o_columns->get( ).
    IF me->nombre_tabla IS INITIAL.
      i_ddfiels = zcl_ap_dev=>get_fieldcatalog_ddic( t_tabla ).
      LOOP AT i_ddfiels INTO l_dfies.
        l_campos-name   = l_dfies-fieldname.
        l_campos-suffix = l_dfies-inttype.
        APPEND l_campos TO i_campos.
      ENDLOOP.
      IF sy-subrc <> 0.
        i_campos = zcl_ap_dev=>get_fieldcatalog( t_tabla ).
      ENDIF.
      CREATE DATA lr_data LIKE LINE OF t_tabla.
    ELSE.
      ASSIGN (me->nombre_tabla) TO <tabla>.
      i_campos = zcl_ap_dev=>get_fieldcatalog( <tabla> ).
      CREATE DATA lr_data LIKE LINE OF <tabla>.
    ENDIF.

    CLEAR: l_orden, l_string, i_csv.
    LOOP AT i_columns INTO l_column.
      TRY.
          o_column = o_columns->get_column( l_column-columnname ).
        CATCH cx_salv_not_found.
          MESSAGE |Error recuperando columna { l_column-columnname }| TYPE 'S'.
      ENDTRY.
      IF o_column->is_visible( ) <> 'X'.
        CONTINUE.
      ENDIF.

      l_orden = l_orden + 1.
      CLEAR l_col.
      l_col-orden  = l_orden.
      l_col-name   = l_column-columnname.
      l_col-titulo = o_column->get_long_text( ).
      READ TABLE i_campos INTO l_campos WITH KEY name = l_column-columnname.
      IF sy-subrc = 0.
        IF NOT l_campos-type IS INITIAL.
          l_col-tipo = l_campos-type->type_kind.
        ELSE.
          l_col-tipo = l_campos-suffix.
        ENDIF.

        APPEND l_col TO i_cols.

        l_col-tipo = 'C'.
        add_campo l_col-titulo.
      ENDIF.
    ENDLOOP.
    APPEND l_string TO i_csv.

    IF NOT me->nombre_tabla IS INITIAL.
      ASSIGN (me->nombre_tabla) TO <tabla>.
      LOOP AT <tabla> ASSIGNING <linea>.
        LOOP AT i_cols INTO l_col.
          CONCATENATE '<LINEA>-' l_col-name INTO l_campo.
          ASSIGN (l_campo) TO <campo>.
          IF sy-subrc = 0.
            add_campo <campo>.
          ENDIF.
        ENDLOOP.
        APPEND l_string TO i_csv.
      ENDLOOP.
    ELSE.
      LOOP AT t_tabla ASSIGNING <linea>.
        LOOP AT i_cols INTO l_col.
          CONCATENATE '<LINEA>-' l_col-name INTO l_campo.
          ASSIGN (l_campo) TO <campo>.
          IF sy-subrc = 0.
            add_campo <campo>.
          ENDIF.
        ENDLOOP.
        APPEND l_string TO i_csv.
      ENDLOOP.
    ENDIF.

    IF NOT fichero_salida IS INITIAL OR dialogo_fichero = 'X'.
      IF servidor IS INITIAL OR dialogo_fichero = 'X'.
        zcl_ap_ficheros=>grabar( EXPORTING fichero       = fichero_salida
                                           dialogo       = dialogo_fichero
                                           mostrar_error = mostrar_mensajes
                                 CHANGING  tabla         = i_csv ).
      ELSE.
        zcl_ap_ficheros=>graba_fich_servidor( EXPORTING fichero          = fichero_salida
                                                        mostrar_mensajes = mostrar_mensajes
                                              CHANGING  tabla            = i_csv ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD exportar_excel.
    TYPES: BEGIN OF t_tit,
             orden  TYPE n LENGTH 3,
             titulo TYPE scrtext_l,
             campo  TYPE abap_componentdescr,
           END OF t_tit.

    DATA: o_excel      TYPE REF TO zcl_ap_excel_ole,
          i_columns    TYPE salv_t_column_ref,
          i_campos     TYPE abap_component_tab,
          i_col        TYPE zap_alv_columnas3_t,     " Tipo tabla ZAP_ALV_COLUMN
          l_campos     TYPE abap_componentdescr,
          l_col        TYPE zap_alv_columnas3,       " Columnas
          l_tit        TYPE t_tit,
          l_titulo     TYPE scrtext_l,
          i_tit        TYPE TABLE OF t_tit,
          i_titulos    TYPE TABLE OF scrtext_l,
          i_campos_sel TYPE abap_component_tab,
          l_column     TYPE salv_s_column_ref.

    FIELD-SYMBOLS <tabla> TYPE table.

    o_excel = NEW #(
        visible = 0 ).

    o_excel->crear_documento( ).

    i_columns = o_columns->get( ).
    IF me->nombre_tabla IS INITIAL.
      IF NOT t_tabla IS INITIAL.
        i_campos = zcl_ap_dev=>get_fieldcatalog( t_tabla ).
      ENDIF.
    ELSE.
      ASSIGN (me->nombre_tabla) TO <tabla>.
      IF sy-subrc = 0.
        IF NOT <tabla> IS INITIAL.
          i_campos = zcl_ap_dev=>get_fieldcatalog( <tabla> ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF i_campos IS INITIAL.
      RETURN.
    ENDIF.

    i_col = get_columnas( ).

    DELETE i_campos WHERE name IS INITIAL.
    LOOP AT i_campos INTO l_campos.
*    READ TABLE i_columns INTO l_column WITH KEY columnname = l_campos-name.
      READ TABLE i_col INTO l_col WITH KEY campo = l_campos-name.
      IF sy-subrc = 0.
        l_tit-orden = l_col-colum.
*      o_column = o_columns->get_column( l_column-columnname ).
*      IF o_column->is_visible( ) = 'X'.
*        l_titulo = o_column->get_long_text( ).
        IF l_col-noout IS INITIAL.
          l_titulo = l_col-descr.
          IF l_titulo IS INITIAL.
*          l_titulo = l_column-columnname.
            l_titulo = l_col-campo.
          ENDIF.
        ELSE.
          l_titulo = '#INVISIBLE#'.
        ENDIF.
      ELSE.
        l_titulo = l_campos-name.
        l_tit-orden = 900.
      ENDIF.
*    APPEND l_titulo TO i_titulos.
      l_tit-titulo = l_titulo.
      l_tit-campo  = l_campos.
      APPEND l_tit TO i_tit.
    ENDLOOP.

    SORT i_tit.
    LOOP AT i_tit INTO l_tit.
      APPEND l_tit-titulo TO i_titulos.
      APPEND l_tit-campo TO i_campos_sel.
    ENDLOOP.

    IF NOT me->nombre_tabla IS INITIAL.
      ASSIGN (me->nombre_tabla) TO <tabla>.
      o_excel->pegar_tabla( cabecera        = 'X'
                            i_tabla         = <tabla>
                            autofiltro      = 'X'
                            i_titulos       = i_titulos
                            columna_inicial = columna_inicial
                            col_texto       = col_texto
                            col_importes    = col_importes
                            replacement     = replacement
                            i_campos_sel    = i_campos_sel ).
    ELSE.
      o_excel->pegar_tabla( cabecera        = 'X'
                            i_tabla         = t_tabla
                            autofiltro      = 'X'
                            i_titulos       = i_titulos
                            columna_inicial = columna_inicial
                            col_texto       = col_texto
                            col_importes    = col_importes
                            replacement     = replacement
                            i_campos_sel    = i_campos_sel ).
    ENDIF.

    IF NOT fichero_salida IS INITIAL.
      o_excel->grabar_documento( fichero         = fichero_salida
                                 cerrar          = cerrar
                                 mostrar_dialogo = dialogo_fichero ).
    ENDIF.
    IF cerrar IS INITIAL.
      o_excel->fijar_visible( ).
    ENDIF.

    IF rightfooter <> '' OR centerheader <> '' OR preview <> ''.
      o_excel->configurar_pagina( rightfooter = rightfooter centerheader = centerheader preview = preview ).
    ENDIF.

    o_excel->free( ).
  ENDMETHOD.
  METHOD exportar_excel_st.
    " TODO: parameter MENSAJE is never cleared or assigned (ABAP cleaner)

    DATA o_alv_xls TYPE REF TO zcl_ap_alv.

    o_alv_xls = NEW #(
        tabla = '' ).

    o_alv_xls->constructor_tabla( CHANGING t_tabla = t_tabla ).

    o_alv_xls->exportar_excel( EXPORTING dialogo_fichero = dialogo_fichero
                                         fichero_salida  = fichero_salida
                                         columna_inicial = columna_inicial
                                         col_texto       = col_texto
                                         col_importes    = col_importes
                                         replacement     = replacement
                                         rightfooter     = rightfooter
                                         centerheader    = centerheader
                                         preview         = preview
                                         cerrar          = cerrar
                               CHANGING  t_tabla         = t_tabla ).
  ENDMETHOD.
  METHOD exportar_xlsx.
    DATA: l_dir     TYPE string,
          l_fichero TYPE string,
          l_mensaje TYPE bapi_msg.

    xstring = o_alv->to_xml( xml_type = tipo_xml ).

    IF xstring IS INITIAL.
      RETURN.
    ENDIF.

    IF fichero IS INITIAL AND dialogo_grabar_fichero IS INITIAL.
      RETURN.
    ENDIF.

    IF dir_temp = 'X'.
      l_dir = zcl_ap_documentos=>get_directorio_temporal( ).
      l_fichero = zcl_ap_ficheros=>concat_ruta( fichero = fichero directorio = l_dir ).
    ELSE.
      l_fichero = fichero.
    ENDIF.
    IF NOT l_fichero CS '.'.
      CONCATENATE l_fichero '.XLSX' INTO l_fichero.
    ENDIF.
    zcl_ap_ficheros=>grabar_xstring( EXPORTING fichero       = l_fichero
                                               xstring       = xstring
                                               dialogo       = dialogo_grabar_fichero
                                     IMPORTING fichero_final = l_fichero
                                               mensaje       = l_mensaje ).
    IF NOT l_mensaje IS INITIAL.
      __concat2 l_mensaje 'Error guardando XLSX'(egx) l_mensaje.
      MESSAGE l_mensaje TYPE 'I'.
    ELSE.
      IF abrir_excel = 'X' AND NOT l_fichero IS INITIAL.
        zcl_ap_gos=>visualizar_fichero_st( l_fichero ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD exportar_xml.
    xstring = o_alv->to_xml( xml_type = tipo_xml ).

    xml_size = xstrlen( xstring ).

    solix_tab = cl_document_bcs=>xstring_to_solix( ip_xstring = xstring ).

    IF NOT fichero IS INITIAL OR NOT dialogo_grabar_fichero IS INITIAL.
      zcl_ap_ficheros=>grabar( EXPORTING fichero      = fichero
                                         dialogo      = dialogo_grabar_fichero
                                         tipo         = 'BIN'
                                         bin_filesize = xml_size
                               CHANGING  tabla        = solix_tab ).
    ENDIF.
  ENDMETHOD.
  METHOD fill_columnas.
    DATA: l_long    TYPE i,
          i_columns TYPE salv_t_column_ref,
          l_column  TYPE salv_s_column_ref,
          l_aux     TYPE c LENGTH 40,
          l_col     TYPE zap_alv_columnas.

    l_long = strlen( campo ).

    i_columns = o_columns->get( ).

    LOOP AT i_columns INTO l_column.
      l_aux = l_column-columnname(l_long).
      IF l_aux = campo.
        IF l_column-columnname+l_long(2) CO '0123456789'.
          CLEAR l_col.
          l_col-campo = l_column-columnname.
          l_col-noout = 'X'.
          l_col-colum = l_column-columnname+l_long(2).
          l_col-descr = l_column-columnname.
          APPEND l_col TO i_columnas.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD free.
    cl_gui_cfw=>flush( ).
    IF NOT o_container IS INITIAL.
      o_container->free( ).
      CLEAR o_container.
      cl_gui_cfw=>flush( ).
    ENDIF.
  ENDMETHOD.
  METHOD get_boton_quickinfo.
    DATA but TYPE t_boton.

    READ TABLE i_botones INTO but WITH KEY boton = boton.
    IF sy-subrc = 0.
      quickinfo = but-quickinfo.
    ENDIF.
  ENDMETHOD.
  METHOD get_boton_text.
    DATA but TYPE t_boton.

    READ TABLE i_botones INTO but WITH KEY boton = boton.
    IF sy-subrc = 0.
      IF NOT but-text IS INITIAL.
        text = but-text.
      ELSEIF NOT but-quickinfo IS INITIAL.
        text = but-quickinfo.
      ELSE.
        text = boton.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_celda_activa.
    CLEAR celda.
    celda = o_selections->get_selected_cells( ).
  ENDMETHOD.
  METHOD get_col.
    CLEAR col.
    IF NOT colum IS INITIAL.
      READ TABLE i_columnas INTO col WITH KEY colum = colum.
    ELSEIF NOT campo IS INITIAL.
      READ TABLE i_columnas INTO col WITH KEY campo = campo.
    ELSEIF NOT aux1 IS INITIAL.
      READ TABLE i_columnas INTO col WITH KEY aux1 = aux1.
    ENDIF.
  ENDMETHOD.
  METHOD get_columna_activa.
    CLEAR columna.
    columna = o_selections->get_selected_columns( ).
  ENDMETHOD.
  METHOD get_columnas.
    DATA: o_colt TYPE REF TO cl_salv_columns_table,
          i_cols TYPE salv_t_column_ref,
          l_col  TYPE zap_alv_columnas3.

    FIELD-SYMBOLS <col> TYPE salv_s_column_ref.

    o_colt = o_alv->get_columns( ).
    i_cols = o_colt->get( ).

    LOOP AT i_cols ASSIGNING <col>.
      CLEAR l_col.
      l_col-colum       = sy-tabix.
      l_col-campo       = <col>-columnname.
      l_col-descr       = <col>-r_column->get_medium_text( ).
      l_col-descr_larga = <col>-r_column->get_long_text( ).
      IF <col>-r_column->is_visible( ) = ''.
        l_col-noout = 'X'.
      ENDIF.
      APPEND l_col TO i_col.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_csv_as_string.
    DATA: i_csv    TYPE table_of_strings,
          l_string TYPE string.

    exportar_csv( EXPORTING separador                  = cl_abap_char_utilities=>horizontal_tab
                            quitar_caracteres_extranos = quitar_caracteres_extranos
                  IMPORTING i_csv                      = i_csv
                  CHANGING  t_tabla                    = t_tabla ).

    CLEAR string_csv.
    LOOP AT i_csv INTO l_string.
      IF string_csv IS INITIAL.
        string_csv = l_string.
      ELSE.
        CONCATENATE string_csv cl_abap_char_utilities=>cr_lf l_string INTO string_csv.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_datos_celda_activa.
    DATA i_celdas_sel TYPE salv_t_cell.

    CLEAR celda.
    i_celdas_sel = get_celda_activa( ).
    READ TABLE i_celdas_sel INTO celda INDEX 1.
  ENDMETHOD.
  METHOD get_datos_layout.
    " TODO: parameter SORT is never cleared or assigned (ABAP cleaner)
    " TODO: parameter FILTER is never cleared or assigned (ABAP cleaner)

    DATA:
      ls_vari TYPE disvariant,
      lt_fcat TYPE lvc_t_fcat,
      ls_layo TYPE lvc_s_layo,
      l_sort  TYPE kkblo_sortinfo.
*      lt_sort TYPE lvc_t_sort,
*      lt_filt TYPE lvc_t_filt.
    DATA lt_kkblo_fieldcat TYPE kkblo_t_fieldcat.
    DATA lt_kkblo_sort     TYPE kkblo_t_sortinfo.
    " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
    DATA lt_kkblo_filter   TYPE kkblo_t_filter.
    DATA ls_kkblo_layout   TYPE kkblo_layout.
    DATA: l_sortc TYPE abap_sortorder,
          i_sortc TYPE abap_sortorder_tab.

    cl_salv_controller_metadata=>get_variant(
      EXPORTING
*       r_layout  = me->r_layout
        r_layout  = o_alv->get_layout( )
      CHANGING
        s_variant = ls_vari ).

* get columns object (basic field catalog data)
    DATA(lo_columns) = o_alv->get_columns( ).
* get aggregations object (Sorts&Sums)
    DATA(lo_aggregations) = o_alv->get_aggregations( ).
* use method to create field catalog from this information
    lt_fcat =
      cl_salv_controller_metadata=>get_lvc_fieldcatalog(
      r_columns      = lo_columns
      r_aggregations = lo_aggregations ).

    IF NOT tabla_ref IS INITIAL.
      IF tabla_ref = 'X'.
        LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fcat>) WHERE domname <> '' AND ref_table IS INITIAL.
          SELECT entitytab
            FROM dd01l
            WHERE domname = @<fcat>-domname
            ORDER BY PRIMARY KEY
            INTO @<fcat>-ref_table
            UP TO 1 ROWS.
          ENDSELECT.
          IF NOT <fcat>-ref_table IS INITIAL.
            SELECT fieldname
              FROM dd03l
              WHERE tabname  = @<fcat>-ref_table
                AND rollname = @<fcat>-rollname
                AND domname  = @<fcat>-domname
              ORDER BY PRIMARY KEY
              INTO @<fcat>-ref_field
              UP TO 1 ROWS.
            ENDSELECT.
            IF sy-subrc = 0.
              set_field( campo = <fcat>-fieldname op = 'DDIC_REF' valor = <fcat>-ref_table valor2 = <fcat>-ref_field ).
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        DATA(i_campos_tab) = zcl_ap_dev=>get_fieldcatalog_tabla( tabla_ref ).
        LOOP AT lt_fcat ASSIGNING <fcat> WHERE ref_table IS INITIAL.
          ASSIGN i_campos_tab[ fieldname = <fcat>-fieldname ] TO FIELD-SYMBOL(<campo>).
          IF sy-subrc = 0.
            <fcat>-ref_field = <campo>-fieldname.
            <fcat>-ref_table = tabla_ref.
            set_field( campo = <fcat>-fieldname op = 'DDIC_REF' valor = <fcat>-ref_table valor2 = <fcat>-ref_field ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*    lt_fcat = zcl_ap_dev=>get_fieldcatalog_tabla_alv( t_tabla  ).
* the fieldcatalog is not complete yet!
    CALL FUNCTION 'LVC_FIELDCAT_COMPLETE'
      EXPORTING
        i_complete       = 'X'
        i_refresh_buffer = space
        i_buffer_active  = space
        is_layout        = ls_layo
        i_test           = '1'
        i_fcat_complete  = 'X'
*     IMPORTING
*       E_EDIT           =
*       ES_LAYOUT        =
      CHANGING
        ct_fieldcat      = lt_fcat.

    IF ls_vari-variant IS NOT INITIAL.

      CALL FUNCTION 'LVC_TRANSFER_TO_KKBLO'
        EXPORTING
          it_fieldcat_lvc         = lt_fcat
        IMPORTING
          et_fieldcat_kkblo       = lt_kkblo_fieldcat
*     TABLES
*         IT_DATA                 =
        EXCEPTIONS
          it_data_missing         = 1
          it_fieldcat_lvc_missing = 2
          OTHERS                  = 3.
      IF sy-subrc <> 0. "#EC EMPTY_IF_BRANCH
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL FUNCTION 'LT_VARIANT_LOAD'
        EXPORTING
*         I_TOOL              = 'LT'
          i_tabname           = '1'
*         I_TABNAME_SLAVE     =
          i_dialog            = ' '
*         I_USER_SPECIFIC     = ' '
*         I_DEFAULT           = 'X'
*         I_NO_REPTEXT_OPTIMIZE       =
*         I_VIA_GRID          =
          i_fcat_complete     = 'X'
        IMPORTING
*         E_EXIT              =
          et_fieldcat         = lt_kkblo_fieldcat
          et_sort             = lt_kkblo_sort
          et_filter           = lt_kkblo_filter
        CHANGING
          cs_layout           = ls_kkblo_layout
          ct_default_fieldcat = lt_kkblo_fieldcat
          cs_variant          = ls_vari
        EXCEPTIONS
          wrong_input         = 1
          fc_not_complete     = 2
          not_found           = 3
          OTHERS              = 4.
      IF sy-subrc <> 0. "#EC EMPTY_IF_BRANCH
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ELSE.
        IF reordenar_tabla = 'X'.
          LOOP AT lt_kkblo_sort INTO l_sort.
            CLEAR l_sortc.
            l_sortc-name = l_sort-fieldname.
            IF l_sort-down = 'X'.
              l_sortc-descending = 'X'.
            ENDIF.
            APPEND l_sortc TO i_sortc.
          ENDLOOP.
          SORT t_tabla BY (i_sortc).
        ENDIF.
      ENDIF.

*      CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
*        EXPORTING
**         I_TECH_COMPLETE   =
**         I_STRUCTURE_NAME  =
*          it_fieldcat_kkblo = lt_kkblo_fieldcat
*          it_sort_kkblo     = lt_kkblo_sort
*          it_filter_kkblo   = lt_kkblo_filter
**         IT_SPECIAL_GROUPS_KKBLO         =
**         IT_FILTERED_ENTRIES_KKBLO       =
**         IT_GROUPLEVELS_KKBLO            =
**         IS_SUBTOT_OPTIONS_KKBLO         =
*          is_layout_kkblo   = ls_kkblo_layout
**         IS_REPREP_ID_KKBLO              =
**         I_CALLBACK_PROGRAM_KKBLO        =
**         IT_ADD_FIELDCAT   =
**         IT_EXCLUDING_KKBLO              =
**         IT_EXCEPT_QINFO_KKBLO           =
*        IMPORTING
*          et_fieldcat_lvc   = lt_fcat
*          et_sort_lvc       = lt_sort
*          et_filter_lvc     = lt_filt
**         ET_SPECIAL_GROUPS_LVC           =
**         ET_FILTER_INDEX_LVC             =
**         ET_GROUPLEVELS_LVC              =
**         ES_TOTAL_OPTIONS_LVC            =
*          es_layout_lvc     = ls_layo
**         ES_VARIANT_LVC    =
**         E_VARIANT_SAVE_LVC              =
**         ES_PRINT_INFO_LVC =
**         ES_REPREP_LVC     =
**         E_REPREP_ACTIVE_LVC             =
**         ET_EXCLUDING_LVC  =
**         ET_EXCEPT_QINFO_LVC             =
*        TABLES
*          it_data           = t_tabla
*        EXCEPTIONS
*          it_data_missing   = 1
*          OTHERS            = 2.
*      IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.

    ELSE.
      IF reordenar_tabla = 'X'.
        get_orden( IMPORTING orden = DATA(i_orden) ).
        LOOP AT i_orden ASSIGNING FIELD-SYMBOL(<orden>).
          APPEND VALUE #( name = <orden>-name ) TO i_sortc.
        ENDLOOP.
        SORT t_tabla BY (i_sortc).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_default_layout.
    DATA ls_layout TYPE salv_s_layout_info.

    ls_layout = cl_salv_layout_service=>get_default_layout(
      s_key    = key_layout
      restrict = cl_salv_layout=>restrict_none ).

    layout = ls_layout-layout.
  ENDMETHOD.
  METHOD get_f4_layout.
    DATA ls_layout TYPE salv_s_layout_info.

    ls_layout = cl_salv_layout_service=>f4_layouts(
      s_key    = key_layout
      restrict = cl_salv_layout=>restrict_none ).

    layout = ls_layout-layout.
  ENDMETHOD.
  METHOD get_fila_activa.
    DATA l_celda TYPE salv_s_cell.

    CLEAR: fila, i_rows.
    o_alv->get_metadata( ).
    i_rows = o_selections->get_selected_rows( ).
    READ TABLE i_rows INTO fila INDEX 1.

    IF sy-subrc <> 0.
      l_celda = get_datos_celda_activa( ).
      fila = l_celda-row.
    ENDIF.
  ENDMETHOD.
  METHOD get_lista_columnas.
    DATA: i_col   TYPE zap_alv_columnas3_t,
          r_rango TYPE rstt_t_range_string.

    FIELD-SYMBOLS <col> TYPE zap_alv_columnas3.

    i_col = get_columnas( ).

    IF NOT excluir_campos IS INITIAL.
      r_rango = zcl_ap_lista=>lista2rango( lista = excluir_campos ).
      DELETE i_col WHERE campo IN r_rango.
    ENDIF.

    IF NOT solo_campos IS INITIAL.
      r_rango = zcl_ap_lista=>lista2rango( lista = solo_campos ).
      DELETE i_col WHERE NOT campo IN r_rango.
    ENDIF.

    LOOP AT i_col ASSIGNING <col>.
      __add_lista lista <col>-campo.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_nombre_columna_activa.
    DATA i_columnas TYPE salv_t_column.

    i_columnas = get_columna_activa( ).
    READ TABLE i_columnas INDEX 1 INTO col.
  ENDMETHOD.
  METHOD get_orden.
    DATA: o_sorts         TYPE REF TO cl_salv_sorts,
          o_sort_defined  TYPE sap_bool,
          o_sort_table    TYPE salv_t_sort_ref,
          o_sort_column   TYPE salv_s_sort_ref,
          wa_sort_columns TYPE t_orden.

    o_sorts = o_alv->get_sorts( ).
    o_sort_defined = o_sorts->is_sort_defined( ).

    IF o_sort_defined = abap_true.

      o_sort_table = o_sorts->get( ).

      LOOP AT o_sort_table INTO o_sort_column.
        CLEAR wa_sort_columns.
        wa_sort_columns-name  = o_sort_column-columnname.
        wa_sort_columns-order = o_sort_column-r_sort->get_sequence( ).

        APPEND wa_sort_columns TO orden.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_registros_filtrados.
    TYPES: BEGIN OF seln_ty,
             column TYPE string,
             r_seln TYPE RANGE OF string,
           END OF seln_ty.

    DATA lp_data TYPE REF TO data.

    FIELD-SYMBOLS: <et_data>  TYPE INDEX TABLE,
                   <ls_data>  TYPE any,
                   <l_column> TYPE any.

    DATA: ls_sel_range  TYPE seln_ty,
          lr_filters    TYPE REF TO cl_salv_filters,
          lt_filters    TYPE salv_t_filter_ref,
          ls_filter     TYPE salv_s_filter_ref,
          ltr_selns     TYPE salv_t_selopt_ref,
          lr_seln       TYPE REF TO cl_salv_selopt,
          ls_sel        LIKE LINE OF ls_sel_range-r_seln,
          lth_sel_range TYPE HASHED TABLE OF seln_ty WITH UNIQUE KEY column.

* Build data set to export
    CREATE DATA lp_data LIKE i_tabla.
    ASSIGN lp_data->* TO <et_data>.

* Build work area
    CREATE DATA lp_data LIKE LINE OF i_tabla.
    ASSIGN lp_data->* TO <ls_data>.

    <et_data> = i_tabla.
    lr_filters = o_alv->get_filters( ).
    lt_filters = lr_filters->get( ).

* Go through the filters on each columns, build select ranges
    LOOP AT lt_filters INTO ls_filter.
      ls_sel_range-column = ls_filter-columnname.
      CLEAR ls_sel_range-r_seln.
      ltr_selns = ls_filter-r_filter->get( ).
      LOOP AT ltr_selns INTO lr_seln.
        ls_sel-sign   = lr_seln->get_sign( ).
        ls_sel-option = lr_seln->get_option( ).
        ls_sel-low    = lr_seln->get_low( ).
        ls_sel-high   = lr_seln->get_high( ).
        INSERT ls_sel INTO TABLE ls_sel_range-r_seln.
      ENDLOOP.

      INSERT ls_sel_range INTO TABLE lth_sel_range.
    ENDLOOP.

* Go through the data eliminating records that don't match the filters
    LOOP AT <et_data> INTO <ls_data>.
      LOOP AT lth_sel_range INTO ls_sel_range.
        ASSIGN COMPONENT ls_sel_range-column OF STRUCTURE <ls_data> TO <l_column>.
        IF <l_column> IN ls_sel_range-r_seln.
          CONTINUE.
        ENDIF.
        DELETE <et_data>.
        EXIT.
      ENDLOOP.
    ENDLOOP.

    i_registros_filtrados = <et_data>.
  ENDMETHOD.
  METHOD get_seleccion.
    DATA: l_campo TYPE string,
          l_row   TYPE int4,
          l_sorts TYPE REF TO cl_salv_sorts.

    FIELD-SYMBOLS: <tabla> TYPE table,
                   <check> TYPE any,
                   <campo> TYPE c.

*  IF deshacer_ordenacion = 'X'.
*    l_sorts = o_alv->get_sorts( ).
*
*    l_sorts->clear( ).
*  ENDIF.

    CLEAR: i_rows, hay_sel.
    IF o_selections IS INITIAL.
      RETURN.
    ENDIF.

    o_alv->get_metadata( ).
    i_rows = o_selections->get_selected_rows( ).

    IF campo_check IS INITIAL.
      RETURN.
    ENDIF.

    IF t_tabla IS INITIAL.
      ASSIGN (nombre_tabla) TO <tabla>.
      IF sy-subrc = 0.
        LOOP AT <tabla> ASSIGNING <check>.
          CONCATENATE '<CHECK>-' campo_check INTO l_campo.
          ASSIGN (l_campo) TO <campo>.
          READ TABLE i_rows FROM sy-tabix TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            <campo> = 'X'.
            hay_sel = 'X'.
          ELSE.
            CLEAR <campo>.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      LOOP AT t_tabla ASSIGNING <check>.
        CONCATENATE '<CHECK>-' campo_check INTO l_campo.
        ASSIGN (l_campo) TO <campo>.
        READ TABLE i_rows FROM sy-tabix TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          <campo> = 'X'.
          hay_sel = 'X'.
        ELSE.
          CLEAR <campo>.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_valor_campo.
    DATA: l_col   TYPE zap_alv_columnas,
          l_campo TYPE string.

    FIELD-SYMBOLS <fs> TYPE any.

    CLEAR valor.
    l_col = get_col( colum = colum campo = campo ).

    IF NOT l_col IS INITIAL.
      CONCATENATE 'REGISTRO-' l_col-campo INTO l_campo.
      ASSIGN (l_campo) TO <fs>.
      IF sy-subrc = 0.
        valor = <fs>.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD handle_double_click.
    handle_link_click( row = row column = column ).
  ENDMETHOD.
  METHOD handle_end_of_page.
  ENDMETHOD.
  METHOD handle_link_click.
    DATA: l_campo   TYPE string,
          l_hotspot TYPE t_hotspot,
          l_message TYPE bapi_msg.

    FIELD-SYMBOLS: <tabla>  TYPE table,
                   <list>   TYPE any,
                   <valor>  TYPE any,
                   <valor2> TYPE any,
                   <valor3> TYPE any.

    SET PARAMETER ID 'Z_NO_VAR_DEF' FIELD 'X'.
    IF table_ref IS INITIAL.
      CONCATENATE '' nombre_tabla INTO l_campo.
      ASSIGN (l_campo) TO <tabla>.
    ELSE.
      ASSIGN me->table_ref->* TO <tabla>.
    ENDIF.

    IF <tabla> IS ASSIGNED.
      ASSIGN <tabla>[ row ] TO <list>.
      IF sy-subrc = 0.

        READ TABLE i_hotspot INTO l_hotspot WITH KEY campo = column.

        l_message = visualizar_objeto( list = <list> column = column hotspot = l_hotspot row = row ).
        IF l_message IS INITIAL.
          SET PARAMETER ID 'Z_NO_VAR_DEF' FIELD ''.
          RETURN.
        ENDIF.

        IF NOT o_alv_helper IS INITIAL.
          l_message = o_alv_helper->visualizar_objeto( list = <list> column = column hotspot = l_hotspot row = row ).
          IF l_message IS INITIAL.
            SET PARAMETER ID 'Z_NO_VAR_DEF' FIELD ''.
            RETURN.
          ENDIF.
        ENDIF.

        IF NOT l_hotspot IS INITIAL.
          IF l_hotspot-transaccion <> ''.
            visualizar_obj( hotspot = l_hotspot list = <list> column = column ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    SET PARAMETER ID 'Z_NO_VAR_DEF' FIELD ''.
  ENDMETHOD.
  METHOD handle_top_of_page.
  ENDMETHOD.
  METHOD handle_user_command.
*  BREAK-POINT.

*  get_seleccion( ).

    FIELD-SYMBOLS <tabla> TYPE table.

    CLEAR: ucomm, ucomm_text, accion_ejecutada.
    ucomm = e_salv_function.
    IF ( ucomm(1) = 'F' OR ucomm(1) = 'M' ) AND strlen( ucomm ) = 3.
      ASSIGN i_botones[ boton = ucomm ] TO FIELD-SYMBOL(<boton>).
      IF sy-subrc = 0.
        ucomm_text = <boton>-text.
        IF NOT <boton>-ucomm IS INITIAL.
          ucomm = <boton>-ucomm.
        ENDIF.
      ENDIF.
    ENDIF.

    IF zcl_ap_lista=>es_elemento( lista = check_ucomm_sel elemento = ucomm ).
      IF NOT o_dev IS INITIAL AND NOT table_ref IS INITIAL.
        ASSIGN table_ref->* TO <tabla>.
        IF sy-subrc = 0.
          get_seleccion( CHANGING t_tabla = <tabla> ).
          LOOP AT <tabla> ASSIGNING FIELD-SYMBOL(<linea>).
            IF o_dev->get_vals( campo = 'CHECK' linea = <linea> ) = 'X'.
              DATA(l_ok) = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF l_ok IS INITIAL.
            check_ucomm_sel = 'ERROR'.
            MESSAGE i104(dlcn). " Seleccione por lo menos un registro
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CASE ucomm.
      WHEN 'VISIBLE'.
        IF NOT o_dev IS INITIAL.
          accion_ejecutada = 'X'.
          zcl_ap_batch_input=>cambiar_modo( CHANGING modo = o_dev->modo_ct ).
        ENDIF.
      WHEN 'EXCEL'.
        IF NOT o_dev IS INITIAL AND NOT table_ref IS INITIAL.
          ASSIGN table_ref->* TO <tabla>.
          IF sy-subrc = 0.
            accion_ejecutada = 'X'.

            SELECT SINGLE clsname FROM seoclass
              " TODO: variable is assigned but never used (ABAP cleaner)
              INTO @DATA(l_clase)
             WHERE clsname = 'ZCL_AP_ABAP2XLS'.
            IF sy-subrc = 0.
              zcl_ap_abap2xls=>alv_2_xls( alv = o_alv tabla = <tabla> abrir = 'X' nombre_fichero = sy-title ).
            ELSE.
              exportar_excel( ).
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD inicializa_objetos.
    DATA l_cprog  TYPE sy-cprog.
    DATA l_status TYPE sypfkey.

    me->logo = logo.

    o_dspset = o_alv->get_display_settings( ).
    o_dspset->set_striped_pattern( cl_salv_display_settings=>true ).
    IF no_agrupar_celdas = 'X'.
      o_dspset->set_no_merging( cl_salv_display_settings=>true ).
    ENDIF.

    o_layout = o_alv->get_layout( ).

    me->sin_buffer = sin_buffer.

    key_layout-report = cprog.
    IF NOT handle IS INITIAL.
      key_layout-handle = handle.
    ELSEIF tabla IS INITIAL.
      key_layout-handle = 'NONE'.
    ELSE.
      key_layout-handle = tabla.
    ENDIF.

    IF no_layout = ''.
      o_layout->set_key( key_layout ).
      o_layout->set_default( 'X' ).
      o_layout->set_save_restriction( restriccion_layout ).
    ENDIF.

    me->status = status.

    IF NOT status_prog IS INITIAL.
      l_cprog = status_prog.
    ELSE.
      l_cprog = cprog.
    ENDIF.
    SELECT obj_code
      FROM rsmptexts                                    "#EC CI_GENBUFF
      WHERE progname = @l_cprog
        AND obj_type = 'C'
        AND obj_code = @me->status
      ORDER BY PRIMARY KEY
      INTO @me->status
      UP TO 1 ROWS.
    ENDSELECT.
    IF sy-subrc <> 0.
      CLEAR me->status.
    ENDIF.

* activate ALV generic Functions
    IF botones_standard = 'X'.
      o_functions = o_alv->get_functions( ).
      o_functions->set_all( ).

      o_functions_list = o_alv->get_functions( ).

*    o_alv->SET_SCREEN_STATUS( PFSTATUS = 'DISPLAY'
*                                 REPORT = SY-REPID
*                                 SET_FUNCTIONS = o_alv->C_FUNCTIONS_ALL ).
    ENDIF.

    IF NOT me->status IS INITIAL.
*        set_status( status ).
      l_status = me->status.
      o_alv->set_screen_status(
          report        = l_cprog
          pfstatus      = l_status
          set_functions = o_alv->c_functions_all ).
    ENDIF.

* set the columns technical
    o_columns = o_alv->get_columns( ).
    me->optimize = optimize.
    o_columns->set_optimize( optimize ).
    i_columns = o_columns->get( ).

    IF NOT lights IS INITIAL.
      TRY.
          o_columns->set_exception_column( value = 'LIGHTS' ).
        CATCH cx_salv_data_error.
          MESSAGE 'Error fijando columna estado' TYPE 'S'.
      ENDTRY.
    ENDIF.

* Selecciones
    o_selections = o_alv->get_selections( ).
    IF NOT sel IS INITIAL.
      IF NOT campo_check IS INITIAL.
        me->campo_check = campo_check.
        set_field( op = 'NOOUT' campo = campo_check ).
      ENDIF.
      IF sel = 'X'.
        o_selections->set_selection_mode( if_salv_c_selection_mode=>single ).
      ELSE.
        o_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
      ENDIF.
    ENDIF.

    o_events = o_alv->get_event( ).
    SET HANDLER handle_double_click FOR o_events.
    SET HANDLER handle_user_command FOR o_events.
    SET HANDLER handle_link_click FOR o_events.

    IF NOT color IS INITIAL.
* El campo de color debe de ser: t_color TYPE lvc_t_scol
      TRY.
          o_columns->set_color_column( color ).
        CATCH cx_salv_data_error.
          MESSAGE 'Error fijando campo color' TYPE 'S'.
      ENDTRY.
      campo_color = color.
    ENDIF.

    o_sorts = o_alv->get_sorts( ).
    o_aggregation = o_alv->get_aggregations( ).

    set_seleccion( ).

    o_top_page = NEW #( ).

    IF NOT inicio_columna IS INITIAL.
      i_columnas = fill_columnas( inicio_columna ).
    ENDIF.

    me->top_of_page_auto   = top_of_page_auto.
    me->top_of_page_titulo = top_of_page_titulo.

    me->o_dev              = o_dev.
  ENDMETHOD.
  METHOD ocultar_columnas_vacias.
    DATA: i_campos TYPE abap_component_tab,
          l_campos TYPE abap_componentdescr,
          l_valor  TYPE c LENGTH 1.

    FIELD-SYMBOLS: <tabla> TYPE table,
                   <linea> TYPE any,
                   <valor> TYPE any.

    IF me->nombre_tabla IS INITIAL.
      IF NOT t_tabla IS INITIAL.
        i_campos = zcl_ap_dev=>get_fieldcatalog( t_tabla ).
        ASSIGN t_tabla TO <tabla>.
      ENDIF.
    ELSE.
      ASSIGN (me->nombre_tabla) TO <tabla>.
      IF sy-subrc = 0.
        IF NOT <tabla> IS INITIAL.
          i_campos = zcl_ap_dev=>get_fieldcatalog( <tabla> ).
        ENDIF.
      ENDIF.
    ENDIF.

    DELETE i_campos WHERE name IS INITIAL.
    DELETE i_campos WHERE name = 'CHECK' OR name = 'LIGHTS'.
    IF i_campos IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT i_campos INTO l_campos.
      CLEAR l_valor.
      LOOP AT <tabla> ASSIGNING <linea>.
        ASSIGN COMPONENT l_campos-name OF STRUCTURE <linea> TO <valor>.
        IF sy-subrc = 0.
          IF NOT <valor> IS INITIAL.
            l_valor = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF l_valor IS INITIAL.
        set_field_noout( l_campos-name ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD refresh.
    DATA l_stable TYPE lvc_s_stbl.

    IF NOT s_stable IS SUPPLIED.
      l_stable-row = 'X'.
      l_stable-col = 'X'.
    ELSE.
      l_stable = s_stable.
    ENDIF.

    IF me->optimize = 'X'.
      o_columns->set_optimize( 'X' ).
    ENDIF.

    TRY.
        o_alv->refresh( s_stable     = l_stable
                        refresh_mode = refresh_mode ).
      CATCH cx_root. "#EC *
    ENDTRY.
  ENDMETHOD.
  METHOD remove_button.
    IF button IS NOT INITIAL.
      IF NOT line_exists( allowed_buttons[ function = button ] ).
        DELETE excluded_buttons WHERE table_line = button.
        APPEND button TO excluded_buttons.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD restore_buttons.
    IF status2 IS INITIAL.
      PERFORM restaurar_botones_v1 IN PROGRAM zap_status.
    ELSE.
      PERFORM restaurar_botones_v1 IN PROGRAM zap_status2.
    ENDIF.
    i_botones = i_botones_v0.
    buttons = buttons_v0.
  ENDMETHOD.
  METHOD save_buttons.
    IF status2 IS INITIAL.
      PERFORM guardar_botones_v1 IN PROGRAM zap_status.
      PERFORM clear_botones IN PROGRAM zap_status.
    ELSE.
      PERFORM guardar_botones_v1 IN PROGRAM zap_status2.
      PERFORM clear_botones IN PROGRAM zap_status2.
    ENDIF.
    i_botones_v0 = i_botones.
    buttons_v0 = buttons.
    CLEAR: i_botones, buttons.
  ENDMETHOD.
  METHOD set_agregacion.
    DATA: i_columnas  TYPE TABLE OF lvc_fname,
          l_columname TYPE lvc_fname,
          l_error     TYPE c LENGTH 80.

    SPLIT campo AT ',' INTO TABLE i_columnas.
    LOOP AT i_columnas INTO l_columname.
      TRY.
          o_column = o_columns->get_column(
                         columnname = l_columname ).
          o_aggregation->add_aggregation( columnname = l_columname aggregation = tipo ).
        CATCH cx_salv_not_found.
          CONCATENATE 'Campo'(cam) l_columname 'no existe'(noe) INTO l_error
                      SEPARATED BY space.
          MESSAGE l_error TYPE 'I'
                  DISPLAY LIKE 'E'.
        CATCH cx_salv_data_error.
          MESSAGE 'Error fijando campo como subtotal' TYPE 'S'.
        CATCH cx_salv_existing.
          MESSAGE 'Error fijando campo como subtotal' TYPE 'S'.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_color.
    DATA: l_column  TYPE string,
          lr_column TYPE REF TO cl_salv_column_table,
          ls_color  TYPE lvc_s_colo.

    IF campo IS INITIAL.
      l_column = campo_color.
    ELSE.
      l_column = campo.
    ENDIF.
    IF l_column IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT l_column AT ',' INTO TABLE DATA(i_column).
    LOOP AT i_column INTO l_column.
      l_column = to_upper( l_column ).
      TRY.
          lr_column ?= o_columns->get_column( CONV #( l_column ) ).
        CATCH cx_salv_not_found.
          MESSAGE 'Campo no existe para fijar color' TYPE 'E'.
      ENDTRY.

      ls_color-col = color.

      ls_color-int = 0.

      ls_color-inv = 0.

      lr_column->set_color( ls_color ).

    ENDLOOP.
* El campo de color debe de ser: t_color TYPE lvc_t_scol
  ENDMETHOD.
  METHOD set_columnas.
    DATA: i_cols  TYPE zap_alv_columnas_t,
          i_colsc TYPE zap_alv_columnas_t,
          l_col   TYPE zap_alv_columnas.

    IF i_columnas IS INITIAL.
      i_cols = me->i_columnas.
    ELSE.
      i_cols = i_columnas.
    ENDIF.

    IF NOT campo IS INITIAL.
      i_colsc = fill_columnas( campo ).
      LOOP AT i_colsc INTO l_col.
        IF NOT line_exists( i_cols[ campo = l_col-campo ] ).
          APPEND l_col TO i_cols.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT i_cols INTO l_col.

      set_field_text( campo = l_col-campo valor = l_col-descr valor2 = l_col-descr_larga ).
      IF l_col-noout = 'X'.
        set_field_noout( l_col-campo ).
      ENDIF.
      IF l_col-input = 'X'.
        set_field_input( l_col-campo ).
      ENDIF.
      IF l_col-dosum = 'X'.
        set_field( campo = l_col-campo op = 'SUM' ).
      ENDIF.
      IF l_col-no_cero = 'X'.
        set_field( campo = l_col-campo op = 'NO_CERO' ).
      ENDIF.
      IF l_col-cambia_decimales = 'X'.
        set_field_decimales( campo = l_col-campo valor = l_col-decimales ).
      ENDIF.
      IF l_col-ancho <> 0.
        set_field( campo = l_col-campo op = 'ANCHO' valor = l_col-ancho ).
      ENDIF.
      IF l_col-borrar = 'X'.
        set_field_quitar( l_col-campo ).
      ENDIF.
    ENDLOOP.

    IF me->i_columnas IS INITIAL.
      me->i_columnas = i_cols.
    ENDIF.
  ENDMETHOD.
  METHOD set_columnas3.
    DATA: i_cols  TYPE zap_alv_columnas3_t,
          i_colsc TYPE zap_alv_columnas3_t,
          l_col   TYPE zap_alv_columnas3.

    IF i_columnas IS INITIAL.
      i_cols = i_columnas3.
    ELSE.
      i_cols = i_columnas.
    ENDIF.

    IF NOT campo IS INITIAL.
*    i_colsc = fill_columnas( campo ).
      LOOP AT i_colsc INTO l_col.
        IF NOT line_exists( i_cols[ campo = l_col-campo ] ).
          APPEND l_col TO i_cols.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT i_cols INTO l_col.
      set_field_text( campo = l_col-campo valor = l_col-descr ).
      IF l_col-noout = 'X'.
        set_field_noout( l_col-campo ).
      ENDIF.
      IF l_col-input = 'X'.
        set_field_input( l_col-campo ).
      ENDIF.
      IF l_col-dosum = 'X'.
        set_field( campo = l_col-campo op = 'SUM' ).
      ENDIF.
      IF l_col-no_cero = 'X'.
        set_field( campo = l_col-campo op = 'NO_CERO' ).
      ENDIF.
      IF l_col-cambia_decimales = 'X'.
        set_field_decimales( campo = l_col-campo valor = l_col-decimales ).
      ENDIF.
      IF l_col-ancho <> 0.
        set_field( campo = l_col-campo op = 'ANCHO' valor = l_col-ancho ).
      ENDIF.
      IF l_col-borrar = 'X'.
        set_field_quitar( l_col-campo ).
      ENDIF.
    ENDLOOP.

    IF i_columnas IS INITIAL.
      i_columnas3 = i_cols.
    ENDIF.
  ENDMETHOD.
  METHOD set_end_of_page.
    end_of_page( ).
    o_alv->set_end_of_list( o_content ).
    SET HANDLER handle_end_of_page FOR o_events.
  ENDMETHOD.
  METHOD set_field.
    DATA: i_column     TYPE TABLE OF lvc_fname,
          l_column     TYPE lvc_fname,
          l_text       TYPE c LENGTH 100,
          l_decimals   TYPE lvc_decmls,
          l_dec_column TYPE lvc_dfname,
          lr_column    TYPE REF TO cl_salv_column_table,
          l_outputlen  TYPE lvc_outlen,
          l_texto      TYPE scrtext_s,
          l_texto2     TYPE scrtext_l,
          l_texto3     TYPE scrtext_m,
          l_opt        TYPE abap_bool,
          l_int        TYPE i,
          l_error      TYPE string.
    DATA l_hotspot TYPE t_hotspot.
    DATA ls_ddic   TYPE salv_s_ddic_reference.

    SPLIT campo AT ',' INTO TABLE i_column.
    LOOP AT i_column INTO l_column.
      TRANSLATE l_column TO UPPER CASE.
      TRY.
          IF op = 'POSICION' AND l_column CS '/'.
            SPLIT l_column AT '/' INTO l_column l_text.
          ENDIF.
          o_column = o_columns->get_column(
                         columnname = l_column ).
          CASE op.
            WHEN 'EDIT' OR 'INPUT'. " o_column->set_edit( 'X' ).
            WHEN 'NOOUT' OR 'NO_OUT'. o_column->set_visible( ' ' ).
            WHEN 'QUITAR'. o_column->set_technical( if_salv_c_bool_sap=>true ).
            WHEN 'PONER'. o_column->set_technical( if_salv_c_bool_sap=>false ).
            WHEN 'OUT'. o_column->set_visible( 'X' ).
            WHEN 'DECIMALS'.
              l_decimals = valor.
              o_column->set_decimals( l_decimals ).
              IF NOT valor2 IS INITIAL.
                l_dec_column = valor2.
                TRY.
                    o_column->set_decimals_column( l_dec_column ).
                  CATCH cx_salv_data_error.
                    MESSAGE 'Error fijando cambiando decimales' TYPE 'S'.
                ENDTRY.
              ENDIF.
            WHEN 'SUM' OR 'DOSUM'.
              set_agregacion( l_column ).
            WHEN 'MERGE'. "<fcat>-no_merging = ''.
            WHEN 'KEY'.
              lr_column ?= o_columns->get_column( l_column ).
              lr_column->set_key( abap_true ).
              o_columns->set_key_fixation( value = abap_true ).
            WHEN 'LONG'. "<fcat>-outputlen = valor.
            WHEN 'ANCHO'.
              l_outputlen = valor.
              o_column->set_output_length( l_outputlen ).
            WHEN 'CHECKBOX'.
*           o_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
              lr_column ?= o_columns->get_column( l_column ).
              lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).

            WHEN 'NO_ZERO' OR 'NO_CERO'.
              o_column->set_zero( '' ).

            WHEN 'DROPDOWN'.
              "<fcat>-drdn_hndl = valor.
            WHEN 'TEXTO'.
              l_texto = valor.
              IF valor2 IS INITIAL.
                l_texto2 = valor.
              ELSE.
                l_texto2 = valor2.
              ENDIF.

              IF valor IS INITIAL AND valor2 IS INITIAL.
                l_text = l_column.
                l_text+1 = to_lower( l_text+1 ).
                l_text+1 = translate( val  = l_text+1
                                      from = `_`
                                      to   = ` ` ).
                l_texto  = l_text.
                l_texto2 = l_text.
                l_texto3 = l_text.

                IF l_texto CS ' de '.
                  REPLACE ALL OCCURRENCES OF ' de ' IN l_texto WITH ' '.
                ENDIF.
              ENDIF.

              o_column->set_short_text( l_texto ).
              l_texto3 = l_texto2.
              o_column->set_medium_text( l_texto3 ).
              o_column->set_long_text( l_texto2 ).

            WHEN 'HOTSPOT'.
              lr_column ?= o_columns->get_column( l_column ).
              lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
              IF NOT valor IS INITIAL.
                l_hotspot-campo       = l_column.
                l_hotspot-transaccion = valor.
                l_texto = valor.
                IF l_texto(4) = 'IDA_'.
                  l_hotspot-filtro = valor2.
                ELSE.
                  l_hotspot-campo_ref = valor2.
                ENDIF.
                APPEND l_hotspot TO i_hotspot.
              ENDIF.
            WHEN 'OPT'.
              lr_column ?= o_columns->get_column( l_column ).
              l_opt = valor.
              lr_column->set_optimized( l_opt ).

            WHEN 'POSICION'.
              IF l_text IS INITIAL.
                l_int = valor.
              ELSE.
                l_int = l_text.
              ENDIF.
              o_columns->set_column_position( columnname = l_column position = l_int ).

            WHEN 'DDIC_REF'.
              lr_column ?= o_columns->get_column( l_column ).
              IF lr_column IS INITIAL.
                IF sy-uname = zcl_c=>usuario_ap.
                  MESSAGE s398(00) WITH 'Error en SET_FIELD OP='(esf) op l_column ''.
                ENDIF.
              ELSE.
                ls_ddic-table = valor.
                ls_ddic-field = valor2.

                TRY.
                    lr_column->set_ddic_reference( ls_ddic ).
                    lr_column->set_f4( abap_true ).

                  CATCH cx_root INTO DATA(o_root). "#EC *
                    IF sy-sysid <> zcl_c=>entorno_produccion OR sy-uname = zcl_c=>usuario_ap.
                      DATA(l_msg) = o_root->get_text( ).
                      CONCATENATE 'Campo'(cam) l_column 'Error en DDIC_REF' l_msg INTO l_error
                                  SEPARATED BY space.
                      MESSAGE l_error TYPE 'I' DISPLAY LIKE 'E'.
                    ENDIF.
                ENDTRY.
              ENDIF.

            WHEN 'COL_AFTER'.
              l_int = o_columns->get_column_position( columnname = CONV #( valor ) ).
              o_columns->set_column_position( columnname = l_column position = l_int ).
              o_columns->set_column_position(  columnname = CONV #( valor ) position   = l_int ).

            WHEN 'COL_BEFORE'.
              l_int = o_columns->get_column_position( columnname = CONV #( valor ) ).
              IF l_int > 1.
                l_int = l_int - 1.
                o_columns->set_column_position( columnname = l_column position = l_int ).
              ENDIF.

            WHEN OTHERS.
              MESSAGE e398(00) WITH 'Error en SET_FIELD OP='(esf) op
                                    'no definida'(nod) ''.
          ENDCASE.

        CATCH cx_salv_not_found.
          IF sy-sysid <> zcl_c=>entorno_produccion OR sy-uname = zcl_c=>usuario_ap.
            CONCATENATE 'Campo'(cam) l_column 'no existe'(noe) INTO l_error
                        SEPARATED BY space.
            MESSAGE l_error TYPE 'I'
                    DISPLAY LIKE 'E'.
          ENDIF.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_field_decimales.
    set_field( op = 'DECIMALS' campo = campo valor = valor ).
  ENDMETHOD.
  METHOD set_field_hotspot.
    DATA: i_campos TYPE TABLE OF string,
          l_campo  TYPE string,
          l_aux    TYPE c LENGTH 20.

    IF auto IS INITIAL.
      set_field( op = 'HOTSPOT' campo = campo valor = valor valor2 = valor2 ).
    ELSE.
      SPLIT campo AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        l_campo = to_upper( l_campo ).
        CASE l_campo.
          WHEN 'KUNNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'XD03' ).
          WHEN 'LIFNR' OR 'TDLNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'XK03' ).
          WHEN 'MATNR' OR 'PLNBEZ'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'MM03' ).
          WHEN 'VBELN' OR 'VBELN_VA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VA03' ).
          WHEN 'VBELN_VL' OR 'ENTREGA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VL03N' ).
          WHEN 'VBELN_VF'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VF03' ).
          WHEN 'TKNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VT03N' ).
          WHEN 'BANFN'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'ME53N' ).
          WHEN 'BNFPO'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'ME53N' valor2 = 'BANFN' ).
          WHEN 'EBELN' OR 'KONNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'ME23N' ).
          WHEN 'EBELP'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'ME23N' valor2 = 'EBELN' ).
          WHEN 'KTPNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'ME23N' valor2 = 'KONNR' ).
          WHEN 'BELNR' OR 'AUGBL'.
            set_field( op = 'HOTSPOT' campo = l_campo valor = 'FB03' ).
          WHEN 'BELNR_RBKP'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'MIR7' ).
          WHEN 'GJAHR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'FB03'  valor2 = 'BELNR' ).
          WHEN 'MBLNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'MB03' ).
          WHEN 'MJAHR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'MB03'  valor2 = 'MJAHR' ).
          WHEN 'EXIDV'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'HUMO' ).
          WHEN 'AUFNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'CO03' ).
          WHEN 'WARPL'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'IP03' ).
          WHEN 'TPLNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'IL03' ).
          WHEN 'EQUNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'IE03' ).
          WHEN 'MDOCM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'FO8U' ).
          WHEN 'POINT'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'FO8X' ).
          WHEN 'QMNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'IW23' ).
          WHEN 'TANUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LT21' ).
          WHEN 'FKNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VI03' ).
          WHEN 'TBNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LB03' ).
          WHEN 'LQNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LS23' ).
          WHEN 'PRUEFLOS'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'QA03' ).
          WHEN 'PLANS'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'PP01_S' ).
          WHEN 'ORGEH'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'PP01_O' ).
          WHEN 'CURSO'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'CURSO' ).
          WHEN 'CURSO_EDT'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'CURSO_EDT' ).
          WHEN 'DOCNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'WEDI' ).
          WHEN 'ANLN1' OR 'ANLN2'.
            set_field( op = 'HOTSPOT' campo = l_campo valor = 'AS03' ).
          WHEN 'KNUMA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VBO3' ).
          WHEN 'PS_PSP_PNR' OR 'PSPNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'CJ13' ).
          WHEN 'URL'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'URL' ).
          WHEN 'WI_ID' OR 'WORKITEMID'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'WI_ID' ).
          WHEN 'DOKNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'CV03N' ).
          WHEN 'LGTYP' OR 'LGPLA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LS03N' ).
          WHEN 'VLTYP' OR 'VLPLA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LS03N_V' ).
          WHEN 'NLTYP' OR 'NLPLA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LS03N_N' ).
          WHEN 'REINR' OR 'TRIPNO'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'PR05' ).
          WHEN 'PERNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'PA20' ).
          WHEN 'CHARG'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'MSC3N' ).
          WHEN 'BUSINESSPARTNER' OR 'PARTNER'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'BP' ).
          WHEN 'OBSERVACIONES' OR 'TEXTO'.
            set_field( op = 'HOTSPOT' campo = l_campo valor = 'TEXT' ).
          WHEN OTHERS.
            l_aux = l_campo.
            IF l_aux(3) = 'URL'.
              set_field( op = 'HOTSPOT' campo = l_campo valor = 'URL' ).
            ELSE.
              set_field( op = 'HOTSPOT' campo = l_campo valor = valor valor2 = valor2 ).
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD set_field_input.
    set_field( op = 'INPUT' campo = campo ).
  ENDMETHOD.
  METHOD set_field_noout.
    set_field( op = 'NO_OUT' campo = campo ).
  ENDMETHOD.
  METHOD set_field_quitar.
    set_field( op = 'QUITAR' campo = campo ).
  ENDMETHOD.
  METHOD set_field_text.
    set_field( op = 'TEXTO' campo = campo valor = valor valor2 = valor2 ).
  ENDMETHOD.
  METHOD set_filter.
    DATA: lo_filters TYPE REF TO cl_salv_filters,
          l_low      TYPE salv_de_selopt_low,
          l_high     TYPE salv_de_selopt_high,
          l_op       TYPE char2.

    lo_filters = o_alv->get_filters( ).

    l_low = valor.
    l_high = valor_hasta.
    l_op = option.
    TRY.
        lo_filters->add_filter(
            columnname = campo
            sign       = sign
            option     = l_op
            low        = l_low
            high       = l_high ).

      CATCH cx_salv_not_found ##NO_HANDLER.
      CATCH cx_salv_data_error ##NO_HANDLER.
      CATCH cx_salv_existing ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.
  METHOD set_icono.
    DATA: l_icono   TYPE c LENGTH 4,
          l_long    TYPE c LENGTH 3,
          l_mensaje TYPE text255,
          l_lon     TYPE i.

    l_icono = icono.
    CASE l_icono.
      WHEN 'R' OR '1' OR 'E' OR 'A'. l_icono = c_ico_rojo.
      WHEN '2' OR 'W'. l_icono = c_ico_ambar.
      WHEN 'V' OR '3' OR 'I' OR 'S'. l_icono = c_ico_verde.
*      WHEN 'W'. l_icono = c_ico_warning.
      WHEN ''. l_icono = icon_space.
      WHEN OTHERS.
        l_icono = icono.
        IF l_icono(1) = '@'.
          l_long = strlen( l_icono ).
          IF l_long = 3.
            CONCATENATE l_icono '@' INTO l_icono.
          ENDIF.
        ENDIF.
    ENDCASE.

    IF mensaje IS INITIAL.
      valor = l_icono.
    ELSE.
      l_mensaje = mensaje.
      DO 3 TIMES.
        IF zcl_ap_string=>ultimo_caracter( l_mensaje ) = '@'.
          l_lon = strlen( l_mensaje ) - 1.
          CLEAR l_mensaje+l_lon(1).
        ENDIF.
      ENDDO.
      IF NOT permitir_filtro IS INITIAL.
        CONCATENATE permitir_filtro l_mensaje INTO l_mensaje.
      ENDIF.

      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          name                  = l_icono
          info                  = l_mensaje
          add_stdinf            = ''
        IMPORTING
          result                = valor
        EXCEPTIONS
          icon_not_found        = 1
          outputfield_too_short = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        valor = icono.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD set_layout.
    o_layout->set_initial_layout( layout ).
  ENDMETHOD.
  METHOD set_orden.
    DATA: l_sequence  TYPE salv_de_sort_sequence,
          l_columname TYPE lvc_fname,
          i_columnas  TYPE TABLE OF lvc_fname,
          l_subtot    TYPE abap_bool,
          l_len       TYPE i,
          l_error     TYPE c LENGTH 80.

    IF down = 'X'.
      l_sequence = 2.
    ELSE.
      l_sequence = 1.
    ENDIF.

    l_columname = campo.
    SPLIT campo AT ',' INTO TABLE i_columnas.
    LOOP AT i_columnas INTO l_columname.
      TRY.
          l_subtot = subtot.
          IF l_columname CS '*'.
            l_len = strlen( l_columname ).
            IF l_len > 2.
              l_len = l_len - 1.
              IF l_columname+l_len(1) = '*'.
                l_columname = l_columname(l_len).
                l_subtot = 'X'.
              ENDIF.
            ENDIF.
          ENDIF.
          o_column = o_columns->get_column(
                         columnname = l_columname ).
          o_sorts->add_sort( columnname = l_columname
                             subtotal   = l_subtot
                             sequence   = l_sequence
                             group      = group ).

          IF compress = 'X'.
            o_sorts->set_compressed_subtotal( l_columname ).
          ENDIF.
        CATCH cx_salv_not_found.
          CONCATENATE 'Campo'(cam) l_columname 'no existe'(noe) INTO l_error SEPARATED BY space.
          MESSAGE l_error TYPE 'I' DISPLAY LIKE 'E'.
        CATCH cx_salv_data_error.
          CONCATENATE 'Error ALV en orden campo'(eoc) l_columname INTO l_error SEPARATED BY space.
          MESSAGE l_error TYPE 'I' DISPLAY LIKE 'E'.
        CATCH cx_salv_existing.
          CONCATENATE 'Error no existe ALV en orden campo'(eno) l_columname INTO l_error SEPARATED BY space.
          MESSAGE l_error TYPE 'I' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_seleccion.
    DATA: t_row   TYPE salv_t_row,
          l_row   TYPE int4,
          l_campo TYPE string.

    FIELD-SYMBOLS: <tabla> TYPE table,
                   <check> TYPE any,
                   <campo> TYPE c.

    CLEAR i_rows.
    IF o_selections IS INITIAL.
      RETURN.
    ENDIF.

    IF campo_check IS INITIAL.
      RETURN.
    ENDIF.

    IF t_tabla IS INITIAL.
      ASSIGN (nombre_tabla) TO <tabla>.
      IF sy-subrc = 0.
        CLEAR t_row.
        LOOP AT <tabla> ASSIGNING <check>.
          l_row = sy-tabix.
          CONCATENATE '<CHECK>-' campo_check INTO l_campo.
          ASSIGN (l_campo) TO <campo>.
          IF <campo> = 'X'.
            APPEND l_row TO t_row.
          ENDIF.
        ENDLOOP.
        o_selections->set_selected_rows( t_row ).
      ENDIF.
    ELSE.
      LOOP AT t_tabla ASSIGNING <check>.
        l_row = sy-tabix.
        CONCATENATE '<CHECK>-' campo_check INTO l_campo.
        ASSIGN (l_campo) TO <campo>.
        IF <campo> = 'X'.
          APPEND l_row TO t_row.
        ENDIF.
      ENDLOOP.
      o_selections->set_selected_rows( t_row ).
    ENDIF.
  ENDMETHOD.
  METHOD set_status.
*  DATA: repid TYPE sy-repid.

*  repid = sy-repid.
    o_alv->set_screen_status(
        report        = repid
        pfstatus      = status
        set_functions = o_alv->c_functions_all ).
  ENDMETHOD.
  METHOD set_titulo_col_sin_nombre.
    DATA i_col TYPE TABLE OF zap_alv_columnas3.

    FIELD-SYMBOLS <col> TYPE zap_alv_columnas3.

    i_col = get_columnas( ).
    LOOP AT i_col ASSIGNING <col> WHERE descr IS INITIAL AND descr_larga IS INITIAL.
      set_field_text( <col>-campo ).
    ENDLOOP.
  ENDMETHOD.
  METHOD set_top_of_page.
    DATA: o_content_logo TYPE REF TO cl_salv_form_layout_logo,
          lr_grid        TYPE REF TO cl_salv_form_layout_grid.

    SET HANDLER handle_top_of_page FOR o_events.
    top_of_page( nreg    = nreg
                 i_param = i_param
                 excluir = excluir ).

    IF me->logo IS INITIAL.
      o_alv->set_top_of_list( o_content ).
    ELSE.
      o_content_logo = NEW #( ).
      lr_grid = o_top_page->get_grid( ).
      o_content_logo->set_left_content( lr_grid ).
      o_content_logo->set_right_logo( logo ).
      o_alv->set_top_of_list( o_content_logo ).
    ENDIF.
  ENDMETHOD.
  METHOD set_valor_campo.
    DATA: l_col   TYPE zap_alv_columnas,
          l_campo TYPE string.

    FIELD-SYMBOLS: <fs>    TYPE any,
                   <color> TYPE lvc_t_scol,
                   <style> TYPE lvc_t_styl.

    l_col = get_col( colum = colum campo = campo aux1 = aux1 ).

    IF l_col IS INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE 'REGISTRO-' l_col-campo INTO l_campo.
    ASSIGN (l_campo) TO <fs>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    <fs> = valor.

    IF quitar_color = 'X'.
      CONCATENATE 'REGISTRO-' campo_color INTO l_campo.
      ASSIGN (l_campo) TO <color>.
      IF sy-subrc = 0.
        DELETE <color> WHERE fname = l_col-campo.
      ENDIF.
    ENDIF.

    IF quitar_style = 'X'.
      CONCATENATE 'REGISTRO-' campo_style INTO l_campo.
      ASSIGN (l_campo) TO <style>.
      IF sy-subrc = 0.
        DELETE <style> WHERE fieldname = l_col-campo.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD show.
    DATA: l_fecha_buffer     TYPE d,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_bypassing_buffer TYPE c LENGTH 1,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_buffer           TYPE c LENGTH 1.

*With parameter I_BUFFER_ACTIVE = 'X', buffer A is activated.If I_BUFFER_ACTIVE remains initial, the contents are nevertheless buffered in buffer B.
* For dynamic reasons, you have to forgo buffering completely (for example, a constant dynamic creation of  structures) so that you have the following options as a developer:
*1. Set SET/GET parameters ALVBUFFER to the current date.All report, variant or field catalog buffers are reset until the current date exceeds the date of the SET/GET parameter.
*2. Set I_BYPASSING_BUFFER = 'X' on the call interface for the function modules and class methods
*- Note 339258 - ALV buffer: concept, recommendations and known errors (BC-SRV-ALV, consulting)
*- Note 122975 - Resetting buffering of ALV field catalog (BC-SRV-ALV, Consulting)
*- Note 356792 - ALV buffer: SET/GET parameter ALVBUFFER created
    IF sy-sysid = zcl_c=>entorno_desarrollo OR sin_buffer = 'X'.
      cl_gui_alv_grid=>m_buffer_inactive = 'X'.
      l_fecha_buffer = sy-datum.                              " + 1.
      SET PARAMETER ID 'ALVBUFFER' FIELD l_fecha_buffer.
      l_bypassing_buffer = 'X'.
* SUBMIT BALVBUFDEL
    ELSE.
      l_buffer = 'X'.
    ENDIF.

    o_alv->display( ).
    ya_mostrado = 'X'.
  ENDMETHOD.
  METHOD show_button.
    DATA allowed TYPE t_allowed_but.

    IF button IS INITIAL.
      RETURN.
    ENDIF.
    IF NOT line_exists( allowed_buttons[ function = button ] ).
      allowed = button.
      APPEND allowed TO allowed_buttons.
      DELETE excluded_buttons WHERE table_line = button.
    ENDIF.
  ENDMETHOD.
  METHOD show_popup.
    o_alv->set_screen_popup(
      start_column = start_column
      end_column   = end_column
      start_line   = start_line
      end_line     = end_line ).

    IF sel <> ''.
      o_selections = o_alv->get_selections( ).

      IF sel = 'X'.
        o_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
      ELSE.
        o_selections->set_selection_mode( if_salv_c_selection_mode=>single ).
      ENDIF.
    ENDIF.

    IF NOT titulo IS INITIAL.
      o_alv->get_display_settings( )->set_list_header( CONV #( titulo ) ).
    ENDIF.

    o_alv->display( ).
  ENDMETHOD.
  METHOD show_popup_st.
    DATA: o_alv      TYPE REF TO zcl_ap_alv,
          l_end_line TYPE i.

    o_alv = NEW #(
        tabla = '' ).

    o_alv->constructor_tabla( EXPORTING color = color restriccion_layout = restriccion_layout no_layout = no_layout CHANGING t_tabla = t_tabla ).

    l_end_line = lines( t_tabla ).
    l_end_line = l_end_line + start_line + 1.
    IF l_end_line > 20.
      l_end_line = 20.
    ENDIF.

    IF NOT field_dosum IS INITIAL.
      o_alv->set_agregacion( field_dosum ).
    ENDIF.

    IF NOT field_noout IS INITIAL.
      o_alv->set_field_noout( field_noout ).
    ENDIF.

    IF NOT field_pos IS INITIAL.
      o_alv->set_field( campo = field_pos op = 'POSICION' ).
    ENDIF.

    IF NOT field_orden IS INITIAL.
      o_alv->set_orden( field_orden ).
    ENDIF.

    IF NOT field_hotspot IS INITIAL.
      o_alv->set_field_hotspot( campo = field_hotspot auto = 'X' ).
    ENDIF.

    IF NOT filtro_campo1 IS INITIAL.
      o_alv->set_filter( option = filtro_op1 campo = filtro_campo1 valor = filtro_valor1 valor_hasta = filtro_valor_hasta ).
    ENDIF.
    IF NOT filtro_campo2 IS INITIAL.
      o_alv->set_filter( option = filtro_op2 campo = filtro_campo2 valor = filtro_valor2 valor_hasta = filtro_valor_hasta2 ).
    ENDIF.
    IF NOT filtro_campo3 IS INITIAL.
      o_alv->set_filter( option = filtro_op3 campo = filtro_campo3 valor = filtro_valor3 valor_hasta = filtro_valor_hasta3 ).
    ENDIF.
    IF NOT filtro_campo4 IS INITIAL.
      o_alv->set_filter( option = filtro_op4 campo = filtro_campo4 valor = filtro_valor4 valor_hasta = filtro_valor_hasta4 ).
    ENDIF.

    o_alv->o_alv_helper = o_alv_helper.

    o_alv->show_popup( end_line     = l_end_line
                       start_column = start_column
                       end_column   = end_column
                       start_line   = start_line ).

*CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv FINAL.
*  PUBLIC SECTION.
*    METHODS: visualizar_objeto REDEFINITION.
*ENDCLASS.
*
*CLASS lcl_alv IMPLEMENTATION.
*
*  METHOD visualizar_objeto.
*    MESSAGE 'Hola' TYPE 'I'.
*  ENDMETHOD. "handle_double_click
*
*ENDCLASS. "lcl_alv IMPLEMENTATION
*
*
*START-OF-SELECTION.
*  SELECT kunnr, name1 FROM kna1
*    UP TO 10 ROWS
*    INTO TABLE @DATA(i_soc).
*
*  DATA(o_alv) = NEW lcl_alv( tabla = 'HELPER' ).
*  zcl_ap_alv=>show_popup_st( EXPORTING field_hotspot = 'KUNNR' o_alv_helper = o_alv CHANGING t_tabla = i_soc ).
  ENDMETHOD.
  METHOD top_of_page.
    DATA l_text TYPE string.

    CLEAR: o_content,
           o_top_page->i_filtros.

    IF top_of_page_titulo = 'X'.
      o_top_page->set_titulo( sy-title ).
    ELSEIF top_of_page_titulo = 'Z'.
      IF sy-batch = 'X' OR sy-tcode IS INITIAL OR sy-tcode = 'YAP' OR sy-tcode = 'SE38'.
        CONCATENATE 'Report:' sy-cprog sy-title INTO l_text SEPARATED BY space.
      ELSE.
        CONCATENATE 'Transacción:'(tra) sy-tcode sy-title INTO l_text SEPARATED BY space.
      ENDIF.
      o_top_page->set_titulo( text = l_text ).
    ELSEIF top_of_page_titulo = 'R'.
      CONCATENATE 'Report:' sy-cprog sy-title INTO l_text SEPARATED BY space.
      o_top_page->set_titulo( text = l_text ).
    ELSEIF NOT top_of_page_titulo IS INITIAL.
      o_top_page->set_titulo( top_of_page_titulo ).
    ENDIF.

    IF top_of_page_auto = 'X'.
      o_top_page->add_rango_auto( i_param = i_param excluir = excluir ).

      IF NOT nreg IS INITIAL.
        o_top_page->add_rango( texto = 'N.Registros'(nre) valor = nreg ).
      ENDIF.

      o_top_page->crea_info_seleccion( ).
      o_content = o_top_page->get_grid( ).
    ENDIF.

*  DATA: lr_grid   TYPE REF TO cl_salv_form_layout_grid,
*        lr_grid_1 TYPE REF TO cl_salv_form_layout_grid,
*        lr_label  TYPE REF TO cl_salv_form_label,
*        lr_text   TYPE REF TO cl_salv_form_text,
*        l_text    TYPE string.
*  CREATE OBJECT lr_grid.
*  l_text = 'TOP OF PAGE for the report' .
*  lr_grid->create_header_information(
*    row    = 1
*    column = 1
*    text    = l_text
*    tooltip = l_text ).
*  lr_grid->add_row( ).
*  lr_grid_1 = lr_grid->create_grid(
*                row    = 3
*                column = 1 ).
*  lr_label = lr_grid_1->create_label(
*    row     = 1
*    column  = 1
*    text    = 'Number of Data Records'
*    tooltip = 'Number of Data Records' ).
*  lr_text = lr_grid_1->create_text(
*    row     = 1
*    column  = 2
*    text    = '10'
*    tooltip = '10' ).
*  lr_label->set_label_for( lr_text ).
*  lr_label = lr_grid_1->create_label(
*    row    = 2
*    column = 1
*    text    = 'date'
*    tooltip = 'date' ).
*  l_text = 'today'.
*  lr_text = lr_grid_1->create_text(
*    row    = 2
*    column = 2
*    text    = 'today'
*    tooltip = 'today' ).
*  lr_label->set_label_for( lr_text ).
*  o_content = lr_grid.
  ENDMETHOD.
  METHOD traduce_color.
    DATA: l_codigo  TYPE c LENGTH 12,
          l_inicial TYPE c LENGTH 1.

* http://www.abap.es/Centro_ALV_colores.htm
    l_codigo = codigo.
    l_codigo = to_upper( l_codigo ).

    l_inicial = codigo.

    CASE l_inicial.
      WHEN 'R'.    " Rojo
        color = c_color_rojo.
      WHEN 'B'.   " Azul
        IF l_codigo <> 'BLANCO'.
          color = c_color_azul.
        ENDIF.
      WHEN 'N'.    " Naranja
        color = c_color_naranja.
      WHEN 'V'.    " Verde
        color = c_color_verde.
      WHEN 'G'.    " Gris
        color = c_color_gris.
      WHEN 'T'.
        color = c_color_amarillo_osc.
      WHEN 'L'.
        color = c_color_blanco.
      WHEN OTHERS.
        CASE l_codigo.
          WHEN 'BLANCO'.
            color = c_color_blanco.
          WHEN 'AZUL'.
            color = c_color_azul.
          WHEN 'AMARILLO'.
            color = c_color_amarillo.
          WHEN 'AMARILLO OSC'.
            color = c_color_amarillo_osc.
        ENDCASE.
    ENDCASE.
  ENDMETHOD.
  METHOD visualizar_obj.
    DATA: o_root   TYPE REF TO cx_root,
          l_aux1   TYPE text255,
          l_sufijo TYPE string,
          l_text   TYPE text132,
          l_campo  TYPE string,
          l_metodo TYPE string,
          l_clase  TYPE string,
          l_aux2   TYPE text255,
          l_cont   TYPE i,
          tab      TYPE abap_parmbind,
          ptab     TYPE abap_parmbind_tab.
    DATA l_vbeln TYPE vbeln_va.
    DATA l_ebeln TYPE ebeln.
    DATA l_aufnr TYPE aufnr.
    DATA l_otype TYPE hrp1000-otype.

    FIELD-SYMBOLS: <valor>  TYPE any,
                   <valor2> TYPE any,
                   <valor3> TYPE any,
                   <valor4> TYPE any,
                   <tabla>  TYPE table.

    CLEAR: mod, salida.

    DATA(l_alv_cliente) = zcl_c=>get_constante( 'ZCL_ALV' ).
    IF NOT l_alv_cliente IS INITIAL.
      TRY.
          CALL METHOD (l_alv_cliente)=>('VISUALIZAR_OBJ_CLIENTE')
            EXPORTING
              hotspot = hotspot
              list    = list
              column  = column
            IMPORTING
              salida  = salida
              mod     = mod
            RECEIVING
              message = message.
          IF NOT mod IS INITIAL.
            RETURN.
          ELSE.
            CLEAR message.
          ENDIF.
        CATCH cx_root INTO o_root. "#EC *
* No hay implementación especifica de cliente, continuamos.
      ENDTRY.
    ENDIF.

    IF hotspot-campo_ref IS INITIAL.
      ASSIGN COMPONENT column OF STRUCTURE  list TO <valor>.
    ELSE.
      ASSIGN COMPONENT hotspot-campo_ref OF STRUCTURE list TO <valor>.
    ENDIF.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SPLIT column AT '_' INTO l_aux1 l_sufijo.

    IF NOT <valor> IS INITIAL.
      l_text = hotspot-transaccion.
      IF l_text(4) = 'IDA_'.
        l_campo = hotspot-filtro.
        SPLIT hotspot-transaccion AT 'IDA_' INTO l_metodo l_clase.
        IF l_campo CS '$'.
          SPLIT l_campo AT '$' INTO l_aux1 l_aux2.
          ASSIGN COMPONENT l_aux2 OF STRUCTURE list TO <valor>.
          IF sy-subrc = 0.
            CONCATENATE '$' l_aux2 INTO l_aux2.
            l_cont = strlen( l_aux2 ).
            REPLACE l_aux2(l_cont) WITH <valor> INTO l_campo.
          ENDIF.
        ELSE.
          REPLACE '?' WITH <valor> INTO l_campo.
        ENDIF.
        NEW zcl_ap_alv_ida( tabla = l_clase filtros = l_campo )->fullscreen_display( ).
      ELSE.
        CASE hotspot-transaccion.
          WHEN 'VA03'.
            SPLIT <valor> AT ',' INTO DATA(l_ped) DATA(l_aux).
            l_vbeln = l_ped.
            __poner_ceros l_vbeln.
            SELECT SINGLE vbeln FROM vbak
              INTO l_aux1
             WHERE vbeln = l_vbeln.
            IF sy-subrc = 0.
              l_clase = 'ZCL_AP_PEDIDO_SD'.
            ELSE.
              SELECT SINGLE vbeln FROM likp
                INTO l_aux1
               WHERE vbeln = l_vbeln.
              IF sy-subrc = 0.
                l_clase = 'ZCL_AP_ENTREGAS'.
              ELSE.
                SELECT SINGLE vbeln FROM vbrk
                  INTO l_aux1
                 WHERE vbeln = l_vbeln.
                IF sy-subrc = 0.
                  l_clase = 'ZCL_AP_FACTURA_SD'.
                ELSE.
                  SELECT SINGLE ebeln FROM ekko
                    INTO l_aux1
                   WHERE ebeln = l_vbeln.
                  IF sy-subrc = 0.
                    l_clase = 'ZCL_AP_PEDIDO_MM'.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
            IF l_clase IS INITIAL.
              RETURN.
            ENDIF.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'VBELN'.
            tab-value = REF #( <valor> ).
            IF l_clase = 'ZCL_AP_PEDIDO_MM'.
              tab-name = 'EBELN'.
              l_ebeln = <valor>.
              tab-value = REF #( l_ebeln ).
            ENDIF.

            INSERT tab INTO TABLE ptab.

          WHEN 'VL03N'.
            l_clase = 'ZCL_AP_ENTREGAS'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'VBELN'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'VF03'.
            l_clase = 'ZCL_AP_FACTURA_SD'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'VBELN'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'VT03N'.
            l_clase = 'ZCL_AP_TRANSPORTE'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'TKNUM'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'ME53N'.
            l_clase = 'ZCL_AP_SOLICITUD'.
            l_metodo = 'VISUALIZAR_SOLICITUD_ST'.
            tab-name  = 'BANFN'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'BNFPO' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'BNFPO'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'ME23N'.
            l_clase = 'ZCL_AP_PEDIDO_MM'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'EBELN'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'EBELP' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'EBELP'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'IW23' OR 'QM03'.
            l_metodo = 'VISUALIZAR_ST'.
            tab-name  = 'QMNUM'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            SELECT qmtyp
              FROM tq80
              JOIN qmel ON tq80~qmart = qmel~qmart     "#EC CI_BUFFJOIN
              WHERE qmnum = @<valor>
              ORDER BY qmnum
              INTO @DATA(l_qmtyp)
              UP TO 1 ROWS.
            ENDSELECT.
            IF sy-subrc <> 0.
              RETURN.
            ELSE.
              CASE l_qmtyp.
                WHEN '01'. " Aviso mantenim.
                  l_clase = 'ZCL_AP_AVISO_PM'.
                WHEN '02'. "  Aviso de calidad
                  l_clase = 'ZCL_AP_AVISO_QM'.
                WHEN '03'. "  Aviso de servicio
                  SET PARAMETER ID 'IQM' FIELD <valor>.
                  CALL TRANSACTION 'IW53' AND SKIP FIRST SCREEN.
                  RETURN.
                WHEN '04'. "  Reclamación
                WHEN '05'. "  Aviso general
              ENDCASE.
            ENDIF.

          WHEN 'MM03'.
            l_clase = 'ZCL_AP_MATERIAL'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'MATNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'FB03' OR 'FB09'.
            l_clase = 'ZCL_AP_DOC_FI'.
            l_metodo = 'FB03'.
            tab-name  = 'BELNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            CLEAR l_aux2.
            IF NOT l_sufijo IS INITIAL.
              CONCATENATE 'BUKRS_' l_sufijo INTO l_aux1.
              ASSIGN COMPONENT l_aux1 OF STRUCTURE list TO <valor2>.
              IF sy-subrc = 0.
                IF NOT <valor2> IS INITIAL.
                  l_aux2 = 'FOUND'.
                  tab-value = REF #( <valor2> ).
                ENDIF.
              ENDIF.
            ENDIF.

            IF l_aux2 IS INITIAL.
              ASSIGN COMPONENT 'BUKRS' OF STRUCTURE list TO <valor2>.
              IF sy-subrc = 0.
                tab-value = REF #( <valor2> ).
              ELSE.
                tab-value = REF #( zcl_c=>bukrs ).
              ENDIF.
            ENDIF.
            tab-name = 'BUKRS'.
            INSERT tab INTO TABLE ptab.

            IF hotspot-transaccion = 'FB09'.
              CLEAR l_aux2.
              ASSIGN COMPONENT 'BUZEI' OF STRUCTURE list TO <valor2>.
              IF sy-subrc = 0.
                IF NOT <valor2> IS INITIAL.
                  tab-value = REF #( <valor2> ).
                  tab-name  = 'BUZEI'.
                  INSERT tab INTO TABLE ptab.
                ENDIF.
              ENDIF.
            ENDIF.

            IF column = 'AUGBL'.
              ASSIGN COMPONENT 'AUGDT' OF STRUCTURE list TO <valor2>.
              IF sy-subrc = 0.
                IF NOT <valor2> IS INITIAL.
                  l_aux2 = 'FOUND'.
                  tab-value = REF #( <valor2>(4) ).
                  tab-name  = 'GJAHR'.
                  INSERT tab INTO TABLE ptab.
                ENDIF.
              ENDIF.
            ENDIF.
            IF l_aux2 IS INITIAL.
              IF NOT l_sufijo IS INITIAL.
                CONCATENATE 'GJAHR_' l_sufijo INTO l_aux1.
                ASSIGN COMPONENT l_aux1 OF STRUCTURE list TO <valor2>.
                IF sy-subrc = 0.
                  IF NOT <valor2> IS INITIAL.
                    l_aux2 = 'FOUND'.
                    tab-value = REF #( <valor2> ).
                    tab-name  = 'GJAHR'.
                    INSERT tab INTO TABLE ptab.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
            IF l_aux2 IS INITIAL.
              ASSIGN COMPONENT 'GJAHR' OF STRUCTURE list TO <valor3>.
              IF sy-subrc = 0.
                tab-value = REF #( <valor3> ).
                tab-name  = 'GJAHR'.
                INSERT tab INTO TABLE ptab.
              ENDIF.
            ENDIF.

          WHEN 'MB03'.
            l_clase = 'ZCL_AP_DOCMAT'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'MBLNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'MJAHR' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'MJAHR'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'ZEILE' OF STRUCTURE list TO <valor3>.
            IF sy-subrc = 0.
              tab-name  = 'ZEILE'.
              tab-value = REF #( <valor3> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'LS03N'.
            l_clase = 'ZCL_AP_OT'.
            l_metodo = 'LS03N'.

            ASSIGN COMPONENT 'LGNUM' OF STRUCTURE list TO <valor>.
            IF sy-subrc = 0.
              tab-name  = 'LGNUM'.
              tab-value = REF #( <valor> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'LGTYP' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'LGTYP'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'LGPLA' OF STRUCTURE list TO <valor3>.
            IF sy-subrc = 0.
              tab-name  = 'LGPLA'.
              tab-value = REF #( <valor3> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'LS03N_V'.
            l_clase = 'ZCL_AP_OT'.
            l_metodo = 'LS03N'.

            ASSIGN COMPONENT 'LGNUM' OF STRUCTURE list TO <valor>.
            IF sy-subrc = 0.
              tab-name  = 'LGNUM'.
              tab-value = REF #( <valor> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'VLTYP' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'LGTYP'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'VLPLA' OF STRUCTURE list TO <valor3>.
            IF sy-subrc = 0.
              tab-name  = 'LGPLA'.
              tab-value = REF #( <valor3> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'LS03N_N'.
            l_clase = 'ZCL_AP_OT'.
            l_metodo = 'LS03N'.

            ASSIGN COMPONENT 'LGNUM' OF STRUCTURE list TO <valor>.
            IF sy-subrc = 0.
              tab-name  = 'LGNUM'.
              tab-value = REF #( <valor> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'NLTYP' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'LGTYP'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'NLPLA' OF STRUCTURE list TO <valor3>.
            IF sy-subrc = 0.
              tab-name  = 'LGPLA'.
              tab-value = REF #( <valor3> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'CV03N'.
            l_clase = 'ZCL_AP_GD'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'DOKNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'DOKAR' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'DOKAR'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'DOKTL' OF STRUCTURE list TO <valor3>.
            IF sy-subrc = 0.
              tab-name  = 'DOKTL'.
              tab-value = REF #( <valor3> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'DOKVR' OF STRUCTURE list TO <valor4>.
            IF sy-subrc = 0.
              tab-name  = 'DOKVR'.
              tab-value = REF #( <valor4> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'LT21'.
            l_clase = 'ZCL_AP_OT'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'TANUM'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'LGNUM' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'LGNUM'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'TAPOS' OF STRUCTURE list TO <valor3>.
            IF sy-subrc = 0.
              IF NOT <valor3> IS INITIAL.
                tab-name  = 'TAPOS'.
                tab-value = REF #( <valor3> ).
                INSERT tab INTO TABLE ptab.
              ENDIF.
            ENDIF.

          WHEN 'QA03'.
            l_clase = 'ZCL_AP_LOTES_INSP'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'PRUEFLOS'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'LB03'.
            l_clase = 'ZCL_AP_NT'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'TBNUM'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'LGNUM' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'LGNUM'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'PR05'.
            l_clase = 'ZCL_AP_GV'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'TRIPNO'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'PERNR' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'PERNR'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'PA20'.
            l_clase = 'ZCL_AP_EMPLEADO'.
            l_metodo = 'VISUALIZAR_ST'.
            tab-name  = 'PERNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'BEGDA' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'BEGDA'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'ENDDA' OF STRUCTURE list TO <valor3>.
            IF sy-subrc = 0.
              tab-name  = 'ENDDA'.
              tab-value = REF #( <valor3> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

            ASSIGN COMPONENT 'INFTY' OF STRUCTURE list TO <valor4>.
            IF sy-subrc = 0.
              tab-name  = 'INFTY'.
              tab-value = REF #( <valor4> ).
              INSERT tab INTO TABLE ptab.
            ENDIF.

          WHEN 'BP'.
            l_clase = 'ZCL_AP_BP'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'BP'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'XD03'.
            l_clase = 'ZCL_AP_CLIENTE'.
            l_metodo = 'VISUALIZAR_CLIENTE_ST'.
            tab-name  = 'KUNNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'XK03'.
            l_clase = 'ZCL_AP_PROVEEDOR'.
            l_metodo = 'VISUALIZAR_PROVEEDOR_ST'.
            tab-name  = 'LIFNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'CO03'.
            l_aufnr = <valor>.
            __poner_ceros l_aufnr.
            SELECT SINGLE aufnr FROM aufk
              INTO l_aufnr
             WHERE aufnr = l_Aufnr.
            IF sy-subrc = 0 OR <valor> CS ','.
              l_clase = 'ZCL_AP_ORDEN_PP'.
              l_metodo = 'VISUALIZAR'.
              tab-name  = 'AUFNR'.
              tab-value = REF #( <valor> ).
              INSERT tab INTO TABLE ptab.
            ELSE.
              SELECT SINGLE plnum FROM plaf
                INTO @DATA(l_plnum)
               WHERE plnum = @<valor>.
              IF sy-subrc = 0.
                l_clase = 'ZCL_AP_ORDPREV'.
                l_metodo = 'VISUALIZAR'.
                tab-name  = 'PLNUM'.
                tab-value = REF #( l_plnum ).
                INSERT tab INTO TABLE ptab.
              ENDIF.
            ENDIF.

          WHEN 'IP03'.
            l_clase = 'ZCL_AP_ORDEN_PP'.
            l_metodo = 'VISUALIZAR_PLAN'.
            tab-name  = 'WARPL'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'IE03'.
            l_clase = 'ZCL_AP_EQUIPO'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'EQUNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'IL03'.
            l_clase = 'ZCL_AP_UBICACION'.
            l_metodo = 'VISUALIZAR'.
            tab-name  = 'TPLNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'FO8U'.
            l_clase = 'ZCL_AP_UBICACION'.
            l_metodo = 'VER_DOC_MEDICION'.
            tab-name  = 'MDOCM'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'FO8X'.
            l_clase = 'ZCL_AP_UBICACION'.
            l_metodo = 'VER_PTO_MEDIDA'.
            tab-name  = 'POINT'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'HUMO'.
            l_clase = 'ZCL_AP_HU'.
            l_metodo = 'VISUALIZAR'.

            tab-name  = 'EXIDV'.
            tab-value = REF #( <valor> ).

            ASSIGN COMPONENT 'VENUM' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              IF NOT <valor2> IS INITIAL.
                tab-name  = 'VENUM'.
                tab-value = REF #( <valor2> ).
              ENDIF.
            ENDIF.

            INSERT tab INTO TABLE ptab.

          WHEN 'CJ13'.
            l_clase = 'ZCL_AP_PEP'.
            l_metodo = 'VISUALIZAR'.

            tab-name  = 'PEP'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'WEDI'.
            l_clase = 'ZCL_AP_IDOC'.
            l_metodo = 'VISUALIZAR'.

            tab-name  = 'DOCNUM'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'WI_ID'.
            l_clase = 'ZCL_AP_WF'.
            l_metodo = 'VISUALIZAR'.

            tab-name  = 'ID'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'URL'.
            l_clase = 'ZCL_AP_GOS'.
            l_metodo = 'VISUALIZAR_FICHERO_ST'.

            tab-name  = 'FICHERO'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'CURSO' OR 'CURSO_EDT'.
            l_clase = 'ZCL_CURSO'.

            IF hotspot-transaccion = 'CURSO_EDT'.
              l_metodo = 'EDITAR'.
            ELSE.
              l_metodo = 'VISUALIZAR'.
            ENDIF.

            tab-name  = 'CURSO'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

          WHEN 'TEXT'.
            l_campo = <valor>.
            zcl_ap_string=>popup_texto( CHANGING texto = l_campo ).
            RETURN.

          WHEN 'TEXT_EDT'.
            l_campo = <valor>.
            zcl_ap_string=>popup_texto( EXPORTING editar = 'X' IMPORTING modificado = mod CHANGING texto = l_campo ).
            IF mod = 'X'.
              salida = l_campo.
            ENDIF.
            RETURN.

          WHEN 'LS23'.
            ASSIGN COMPONENT 'LGNUM' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              SET PARAMETER ID 'LGN' FIELD <valor2>.
            ENDIF.
            SET PARAMETER ID 'LQN' FIELD <valor>.
            CALL TRANSACTION 'LS23 ' AND SKIP FIRST SCREEN.
            RETURN.

          WHEN 'KB13N'.
            SET PARAMETER ID 'BLN' FIELD <valor>.
            SET PARAMETER ID 'CO_BLN' FIELD <valor>.

            ASSIGN COMPONENT 'KOKRS' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              SET PARAMETER ID 'CAC' FIELD <valor2>.
            ENDIF.

            CALL TRANSACTION 'KB13N ' AND SKIP FIRST SCREEN.
            RETURN.

          WHEN 'VI03'.
            SET PARAMETER ID 'FKK' FIELD <valor>.
            CALL TRANSACTION 'VI03 ' AND SKIP FIRST SCREEN.
            RETURN.

          WHEN 'VBO3'.
            SET PARAMETER ID 'VBO' FIELD <valor>.
            CALL TRANSACTION 'VBO3' AND SKIP FIRST SCREEN.
            RETURN.
          WHEN 'AS03'.
            ASSIGN COMPONENT 'ANLN1' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              IF NOT <valor2> IS INITIAL.
                SET PARAMETER ID 'AN1' FIELD <valor2>.
              ENDIF.
            ENDIF.
            ASSIGN COMPONENT 'ANLN2' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              IF NOT <valor2> IS INITIAL.
                SET PARAMETER ID 'AN2' FIELD <valor2>.
              ENDIF.
            ENDIF.
            ASSIGN COMPONENT 'BUKRS' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              IF NOT <valor2> IS INITIAL.
                SET PARAMETER ID 'BUK' FIELD <valor2>.
              ENDIF.
            ENDIF.
            CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
            RETURN.

          WHEN 'MIRO'.
            l_clase = 'ZCL_AP_FACTURA_MM'.
            l_metodo = 'VISUALIZAR'.

            tab-name  = 'BELNR'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'GJAHR' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'GJAHR'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ELSE.
              ASSIGN COMPONENT 'GJAHR_F' OF STRUCTURE list TO <valor2>.
              IF sy-subrc = 0.
                tab-name  = 'GJAHR'.
                tab-value = REF #( <valor2> ).
                INSERT tab INTO TABLE ptab.
              ENDIF.
            ENDIF.

          WHEN 'MSC3N'.
            l_clase = 'ZCL_AP_LOTE'.
            l_metodo = 'VER'.

            tab-name  = 'CHARG'.
            tab-value = REF #( <valor> ).
            INSERT tab INTO TABLE ptab.

            ASSIGN COMPONENT 'MATNR' OF STRUCTURE list TO <valor2>.
            IF sy-subrc = 0.
              tab-name  = 'MATNR'.
              tab-value = REF #( <valor2> ).
              INSERT tab INTO TABLE ptab.
            ELSE.
              ASSIGN COMPONENT 'WERKS' OF STRUCTURE list TO <valor2>.
              IF sy-subrc = 0.
                tab-name  = 'WERKS'.
                tab-value = REF #( <valor2> ).
                INSERT tab INTO TABLE ptab.
              ENDIF.
            ENDIF.

          WHEN OTHERS.
            l_text = hotspot-transaccion.
            IF l_text(4) = 'PP01'.
              l_clase = 'ZCL_AP_OBJETO_PD'.
              l_metodo = 'PP01'.

              tab-name  = 'OBJID'.
              tab-value = REF #( <valor> ).
              INSERT tab INTO TABLE ptab.

              IF l_text+5(2) <> ''.
                tab-name = 'OTYPE'.
                l_otype = l_text+5(2).
                tab-value = REF #( l_otype ).
                INSERT tab INTO TABLE ptab.
              ENDIF.

              ASSIGN COMPONENT 'BEGDA' OF STRUCTURE list TO <valor2>.
              IF sy-subrc = 0.
                IF NOT <valor2> IS INITIAL.
                  tab-name  = 'BEGDA'.
                  tab-value = REF #( <valor2> ).
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT 'ENDDA' OF STRUCTURE list TO <valor2>.
              IF sy-subrc = 0.
                IF NOT <valor2> IS INITIAL.
                  tab-name  = 'ENDDA'.
                  tab-value = REF #( <valor2> ).
                ENDIF.
              ENDIF.
            ELSE.
              DATA l_valor TYPE string.
              l_text = <valor>.
              TRANSLATE l_text TO UPPER CASE.
              IF l_text(5) = 'HTTP:' OR l_text(6) = 'HTTPS:'.
                l_clase = 'ZCL_AP_GOS'.
                l_metodo = 'VISUALIZAR_FICHERO_ST'.

                tab-name  = 'FICHERO'.
                tab-value = REF #( <valor> ).
                INSERT tab INTO TABLE ptab.
              ENDIF.
            ENDIF.
        ENDCASE.

        IF l_clase <> '' AND l_metodo <> ''.
          TRY.
              CALL METHOD (l_clase)=>(l_metodo) PARAMETER-TABLE ptab.
            CATCH cx_root INTO o_root. "#EC *
              message = o_root->get_longtext( ).
              IF message IS INITIAL.
                message = o_root->get_text( ).
              ENDIF.
          ENDTRY.
        ENDIF.
      ENDIF.
    ELSE.
      CASE hotspot-transaccion.
        WHEN 'TEXT_EDT'.
          l_campo = <valor>.
          zcl_ap_string=>popup_texto( EXPORTING editar = 'X' IMPORTING modificado = mod CHANGING texto = l_campo ).
          IF mod = 'X'.
            salida = l_campo.
          ENDIF.
          RETURN.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar_objeto.
    CASE column.
      WHEN 'X'.

      WHEN OTHERS.
        message = 'No implementado'.
    ENDCASE.
  ENDMETHOD.