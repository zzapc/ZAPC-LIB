CLASS lcl_alv_private DEFINITION
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES if_alv_rm_grid_friend .
    METHODS constructor
      IMPORTING
        !io_grid TYPE REF TO cl_gui_alv_grid .
    METHODS create_ex_result
      RETURNING
        VALUE(rv_results) TYPE REF TO cl_salv_ex_result_data_table .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA grid TYPE REF TO cl_gui_alv_grid .
ENDCLASS.
CLASS lcl_alv_private IMPLEMENTATION.
  METHOD constructor.
    grid = io_grid.
  ENDMETHOD.
  METHOD create_ex_result.
    DATA: tab      TYPE abap_parmbind,
          ptab     TYPE abap_parmbind_tab,
          o_result TYPE REF TO cl_salv_ex_result_data_table.
    tab-name  = 'ER_RESULT_TABLE'.
    tab-kind  = cl_abap_objectdescr=>returning.
    tab-value = REF #( o_result ).
    INSERT tab INTO TABLE ptab.
    TRY.
        CALL METHOD me->grid->('CREATE_EX_RESULT') PARAMETER-TABLE ptab.
        READ TABLE ptab INTO tab WITH KEY name = 'ER_RESULT_TABLE'.
        IF sy-subrc = 0.
          rv_results = o_result.
        ENDIF.
        IF rv_results IS INITIAL.
          MESSAGE 'Error convirtiendo los datos a XLSX' TYPE 'E'.
        ENDIF.
      CATCH cx_root.
        MESSAGE 'No es posible convertir los datos a XLSX' TYPE 'E'.
    ENDTRY.
*    rv_results =  me->grid=->create_ex_result( ).
  ENDMETHOD.
ENDCLASS.
CLASS zcl_ap_alv_grid DEFINITION
  PUBLIC
  CREATE PUBLIC
  GLOBAL FRIENDS cl_gui_alv_grid.

  PUBLIC SECTION.
    TYPES: BEGIN OF t_boton,
             boton TYPE c LENGTH 10.
             INCLUDE TYPE smp_dyntxt.
    TYPES:   ucomm TYPE sy-ucomm,
           END OF t_boton.
    TYPES t_botones TYPE TABLE OF t_boton.
    TYPES:BEGIN OF t_buttons,
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
      END OF t_buttons.

    DATA o_grid             TYPE REF TO cl_gui_alv_grid.
    DATA t_fcat             TYPE lvc_t_fcat.
    DATA layout             TYPE lvc_s_layo.
    DATA variant            TYPE disvariant.
    DATA o_custom_container TYPE REF TO cl_gui_custom_container.
    DATA i_filas_sel        TYPE lvc_t_row.
    DATA fila               TYPE lvc_s_row.

    CONSTANTS c_sem_verde      TYPE num1 VALUE 3 ##NO_TEXT.
    CONSTANTS c_sem_rojo       TYPE num1 VALUE 1 ##NO_TEXT.
    CONSTANTS c_sem_ambar      TYPE num1 VALUE 2 ##NO_TEXT.
    CONSTANTS c_color_rojo     TYPE i    VALUE 6 ##NO_TEXT.
    CONSTANTS c_color_amarillo TYPE i    VALUE 3 ##NO_TEXT.
    CONSTANTS c_color_azul     TYPE i    VALUE 1 ##NO_TEXT.
    CONSTANTS c_color_naranja  TYPE i    VALUE 7 ##NO_TEXT.
    CONSTANTS c_color_verde    TYPE i    VALUE 5 ##NO_TEXT.
    CONSTANTS c_color_gris     TYPE i    VALUE 2 ##NO_TEXT.
    CONSTANTS c_color_azul_osc TYPE i    VALUE 4 ##NO_TEXT.

    DATA key_layout   TYPE salv_s_layout_key.
    DATA i_columnas3  TYPE zap_alv_columnas3_t.
    DATA o_dragdrop   TYPE REF TO cl_dragdrop.
    DATA nombre_tabla TYPE string.
    DATA cprog        TYPE sy-cprog.
    DATA i_filter     TYPE lvc_t_filt.
    DATA sin_buffer   TYPE abap_bool.
    DATA inicio       TYPE abap_bool VALUE '' ##NO_TEXT.

    CONSTANTS c_color_amarillo_osc TYPE i VALUE 4 ##NO_TEXT.
    CONSTANTS c_color_blanco       TYPE i VALUE 0 ##NO_TEXT.

    DATA i_fcat_ini   TYPE lvc_t_fcat.
    DATA i_columnas   TYPE zap_alv_columnas_t.
    DATA max_cols     TYPE i VALUE 10 ##NO_TEXT.
    DATA campo        TYPE fieldname.
    DATA buttons      TYPE t_buttons.
    DATA i_botones    TYPE t_botones.
    DATA i_hotspot    TYPE zcl_ap_alv=>tt_hotspot.
    DATA o_alv_helper TYPE REF TO zcl_ap_alv.
    DATA table_ref    TYPE REF TO data.
    DATA buttons_v0   TYPE t_buttons.
    DATA i_botones_v0 TYPE t_botones.
    DATA campo_check  TYPE string.

    METHODS constructor
      IMPORTING obj_contenedor     TYPE scrfname                       DEFAULT 'GRID'
                estructura         TYPE dd02l-tabname                  OPTIONAL
                o_event            TYPE REF TO zcl_ap_alv_grid_eventos OPTIONAL
                restriccion_layout TYPE c                              DEFAULT 'A'
                o_container        TYPE REF TO cl_gui_container        OPTIONAL
                default_layout     TYPE c                              DEFAULT 'X'
                nombre_tabla       TYPE any                            DEFAULT 'I_LISTADO'
                cprog              TYPE sy-cprog                       DEFAULT sy-cprog
                sin_buffer         TYPE abap_bool                      DEFAULT ''
                nombre_layout      TYPE any                            DEFAULT ''
                !handle            TYPE slis_handl                     DEFAULT ''
                dragdrop           TYPE abap_bool                      DEFAULT ''
      PREFERRED PARAMETER obj_contenedor.

    METHODS show
      IMPORTING !handle          TYPE slis_handl   OPTIONAL
                !include         TYPE sy-repid     DEFAULT sy-cprog
                internal_tabname TYPE slis_tabname DEFAULT ''
                programa         TYPE sy-repid     DEFAULT sy-cprog
      CHANGING  it_fieldcatalog  TYPE lvc_t_fcat   OPTIONAL
                tabla            TYPE STANDARD TABLE.

    METHODS set_layout
      IMPORTING sel_mode         TYPE char1  OPTIONAL
                zebra            TYPE char1  OPTIONAL
                !input           TYPE char1  OPTIONAL
                !edit            TYPE char1  OPTIONAL
                ancho_optimizado TYPE char1  OPTIONAL
                lights           TYPE string OPTIONAL
                !color           TYPE string OPTIONAL
                colort           TYPE string OPTIONAL
                !style           TYPE string OPTIONAL
                no_merging       TYPE any    OPTIONAL
                no_toolbar       TYPE any    OPTIONAL
                no_rowmove       TYPE any    OPTIONAL
                no_rowins        TYPE any    OPTIONAL
      PREFERRED PARAMETER sel_mode.

    METHODS set_field
      IMPORTING op     TYPE text10
                campo  TYPE any
                valor  TYPE any OPTIONAL
                valor2 TYPE any OPTIONAL.

    METHODS set_orden
      IMPORTING campo  TYPE string OPTIONAL
                !up    TYPE char1  DEFAULT 'X'
                down   TYPE char1  DEFAULT ''
                !group TYPE string DEFAULT ''
                subtot TYPE char1  DEFAULT ''
      PREFERRED PARAMETER campo.

    METHODS comprobar_cambios
      IMPORTING forzar_validaciones TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER forzar_validaciones.

    METHODS refrescar_grid
      IMPORTING soft_refresh TYPE abap_bool DEFAULT 'X'
                new_code     TYPE syucomm   DEFAULT ''
                estable      TYPE abap_bool DEFAULT 'X'
                set_focus    TYPE abap_bool DEFAULT ''
                mantener_sel TYPE abap_bool DEFAULT 'X'
      PREFERRED PARAMETER soft_refresh.

    METHODS quitar_opciones
      IMPORTING opcion TYPE ui_func.

    METHODS set_dropdown
      IMPORTING !handle TYPE int4
                valor   TYPE any OPTIONAL.

    METHODS barra_botones_minima_edit.

    METHODS quitar_botones_insercion
      IMPORTING todos TYPE abap_bool DEFAULT ''.

    METHODS free.
    METHODS get_filas_sel.
    METHODS registrar_enter.

    METHODS set_field_input
      IMPORTING campo TYPE any.

    METHODS set_field_noout
      IMPORTING campo TYPE any.

    METHODS get_fila_activa
      IMPORTING indice_sin_filtros TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(fila)        TYPE int4.

    CLASS-METHODS append_color
      IMPORTING campo                   TYPE any       DEFAULT ''
                !color                  TYPE int4      OPTIONAL
                colorc                  TYPE any       OPTIONAL
                int                     TYPE int1      DEFAULT 0
                inv                     TYPE int4      DEFAULT 0
                solo_si_no_color_previo TYPE abap_bool DEFAULT ''
      CHANGING  tabla_color             TYPE lvc_t_scol.

    CLASS-METHODS traduce_color
      IMPORTING codigo       TYPE any
      RETURNING VALUE(color) TYPE int4.

    METHODS get_catalogo_campos_tabint
      IMPORTING !include         TYPE sy-repid DEFAULT sy-cprog
                internal_tabname TYPE slis_tabname
                programa         TYPE sy-repid DEFAULT sy-cprog
      CHANGING  tabla            TYPE table
                it_fieldcatalog  TYPE lvc_t_fcat.

    METHODS exportar_excel
      IMPORTING fichero_salida TYPE string    OPTIONAL
                cerrar         TYPE abap_bool DEFAULT ''
                autofiltro     TYPE abap_bool DEFAULT 'X'
      CHANGING  t_tabla        TYPE table     OPTIONAL.

    METHODS set_field_text
      IMPORTING campo  TYPE any
                valor  TYPE any OPTIONAL
                valor2 TYPE any OPTIONAL.

    METHODS get_default_layout
      RETURNING VALUE(layout) TYPE disvariant-variant.

    METHODS get_f4_layout
      RETURNING VALUE(layout) TYPE disvariant-variant.

    METHODS quitar_todos_botones_insercion.

    METHODS set_columnas
      IMPORTING i_columnas TYPE zap_alv_columnas_t.

    CLASS-METHODS append_style
      IMPORTING campo       TYPE any DEFAULT ''
                !style      TYPE lvc_style
      CHANGING  tabla_style TYPE lvc_t_styl.

    CLASS-METHODS append_style_no_edit
      IMPORTING campo       TYPE any DEFAULT ''
      CHANGING  tabla_style TYPE lvc_t_styl.

    METHODS set_columnas3
      IMPORTING i_columnas TYPE zap_alv_columnas3_t.

    METHODS set_titulo
      IMPORTING titulo TYPE any.

    METHODS correguir_variantes
      IMPORTING i_columnas3 TYPE zap_alv_columnas3_t OPTIONAL
                i_columnas  TYPE zap_alv_columnas_t  OPTIONAL.

    METHODS importar_excel
      IMPORTING fichero_salida TYPE string    OPTIONAL
                cerrar         TYPE abap_bool DEFAULT ''
                autofiltro     TYPE abap_bool DEFAULT 'X'
      CHANGING  t_tabla        TYPE table.

    METHODS get_tabla_filas_sel
      IMPORTING t_tabla     TYPE table
      CHANGING  t_tabla_sel TYPE table.

    METHODS set_marca_filas_sel
      IMPORTING campo             TYPE any DEFAULT 'CHECK'
                validar_seleccion TYPE any DEFAULT ''
                fila_activa       TYPE any DEFAULT ''
      CHANGING  t_tabla           TYPE table
                hay_sel           TYPE any OPTIONAL.

    CLASS-METHODS append_style_edit
      IMPORTING campo       TYPE any DEFAULT ''
      CHANGING  tabla_style TYPE lvc_t_styl.

    METHODS set_seleccion
      IMPORTING campo   TYPE any DEFAULT 'CHECK'
      CHANGING  t_tabla TYPE table.

    METHODS add_filtro
      IMPORTING campo   TYPE any
                valor   TYPE any
                !sign   TYPE any DEFAULT 'I'
                !option TYPE any DEFAULT 'EQ'.

    METHODS quitar_botones
      IMPORTING todos       TYPE abap_bool DEFAULT ''
                insercion   TYPE abap_bool DEFAULT ''
                ordenacion  TYPE abap_bool DEFAULT ''
                !layout     TYPE abap_bool DEFAULT ''
                no_toolbar  TYPE abap_bool DEFAULT ''
                operaciones TYPE abap_bool DEFAULT ''
                resto       TYPE abap_bool DEFAULT ''
                filtro      TYPE abap_bool DEFAULT ''
                exportacion TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER todos.

    METHODS set_campos_tabint
      IMPORTING tabla TYPE table.

    METHODS forzar_no_edicion
      IMPORTING campos TYPE any DEFAULT ''.

    METHODS get_fcat_inicial
      RETURNING VALUE(fcat) TYPE lvc_t_fcat.

    METHODS actualiza_campos_grid
      IMPORTING VALUE(campos_borrar) TYPE any                DEFAULT ''
                refrescar            TYPE abap_bool          DEFAULT 'X'
                get_fcat_inicial     TYPE abap_bool          DEFAULT ''
                i_columnas           TYPE zap_alv_columnas_t OPTIONAL
                campos_input         TYPE any                DEFAULT ''
                campos_no_out        TYPE any                DEFAULT ''
                fix_col_noout_92     TYPE abap_bool          DEFAULT ''
                campos_no_input      TYPE any                DEFAULT ''
                soft_refresh         TYPE abap_bool          DEFAULT ''
                campos_opt           TYPE any                DEFAULT ''
                campos_out           TYPE any                DEFAULT ''
                campos_no_opt        TYPE any                DEFAULT ''.

    METHODS cambiar_variant
      IMPORTING !handle                        TYPE slis_handl DEFAULT '?'
                variant                        TYPE slis_vari  DEFAULT ''
                get_default_variant            TYPE abap_bool  DEFAULT ''
                get_fcat_inicial_si_no_variant TYPE abap_bool  DEFAULT ''.

    METHODS registrar_mod.

    METHODS completar_columnas
      IMPORTING max_cols      TYPE i         DEFAULT 10
                mostrar_aviso TYPE abap_bool DEFAULT 'X'
                campo         TYPE any
                borrar        TYPE abap_bool DEFAULT 'X'
      CHANGING  i_columnas    TYPE zap_alv_columnas_t.

    METHODS get_valor_campo
      IMPORTING registro     TYPE any
                colum        TYPE numc2 OPTIONAL
                campo        TYPE any   DEFAULT ''
      RETURNING VALUE(valor) TYPE string.

    METHODS get_col
      IMPORTING colum      TYPE numc2 OPTIONAL
                campo      TYPE any   DEFAULT ''
      RETURNING VALUE(col) TYPE zap_alv_columnas.

    METHODS set_valor_campo
      IMPORTING colum        TYPE numc2     OPTIONAL
                campo        TYPE any       DEFAULT ''
                VALUE(valor) TYPE any
                quitar_style TYPE abap_bool DEFAULT ''
                quitar_color TYPE abap_bool DEFAULT ''
                campo_style  TYPE fieldname DEFAULT 'STYLE'
                campo_color  TYPE fieldname DEFAULT 'COLOR'
      CHANGING  registro     TYPE any.

    CLASS-METHODS get_lista_campos
      IMPORTING tabla         TYPE any
                quitar_campos TYPE any
      RETURNING VALUE(lista)  TYPE string.

    METHODS set_field_dropdown
      IMPORTING campo       TYPE any
                valor       TYPE any OPTIONAL
                valor2      TYPE any OPTIONAL
                dominio     TYPE any DEFAULT ''
                tabla       TYPE any DEFAULT ''
                campo_desc  TYPE any DEFAULT ''
                campo_clave TYPE any DEFAULT ''
                valores     TYPE any DEFAULT ''
                !where      TYPE any DEFAULT ''
                add_clave   TYPE any DEFAULT ''
                caract      TYPE any DEFAULT ''.

    METHODS get_valor_from_desc
      IMPORTING campo        TYPE any
                descripcion  TYPE any
      RETURNING VALUE(valor) TYPE string.

    METHODS add_button
      IMPORTING button   TYPE sy-ucomm
                !text    TYPE smp_dyntxt-text      OPTIONAL
                !icon    TYPE smp_dyntxt-icon_id   OPTIONAL
                qinfo    TYPE smp_dyntxt-quickinfo OPTIONAL
                !allowed TYPE abap_bool            DEFAULT abap_true
                forzar   TYPE abap_bool            DEFAULT ''
                ucomm    TYPE sy-ucomm             DEFAULT ''
                borrar   TYPE abap_bool            DEFAULT ''
                status2  TYPE abap_bool            DEFAULT ''.

    METHODS remove_button
      IMPORTING button TYPE sy-ucomm.

    METHODS show_button
      IMPORTING button TYPE sy-ucomm.

    METHODS cambia_layout
      IMPORTING campo TYPE any DEFAULT ''
                valor TYPE any DEFAULT ''.

    METHODS cambia_atributo_columna
      IMPORTING refrescar        TYPE abap_bool          DEFAULT 'X'
                get_fcat_inicial TYPE abap_bool          DEFAULT ''
                i_columnas       TYPE zap_alv_columnas_t OPTIONAL
                campos           TYPE any                OPTIONAL
                valor            TYPE any                DEFAULT ''
                atributo         TYPE any                OPTIONAL
                soft_refresh     TYPE any                DEFAULT ''.

    METHODS export_to_spread_sheet.

    METHODS ocultar_columnas_vacias
      IMPORTING t_tabla TYPE table.

    METHODS get_boton_text
      IMPORTING boton       TYPE any
      RETURNING VALUE(text) TYPE smp_dyntxt-text.

    METHODS exportar_xlsx
      IMPORTING fichero                TYPE any       DEFAULT ''
                dialogo_grabar_fichero TYPE abap_bool DEFAULT ''
                abrir_excel            TYPE abap_bool DEFAULT ''
                dir_temp               TYPE abap_bool DEFAULT ''
      EXPORTING xstring                TYPE xstring.

    METHODS set_field_hotspot
      IMPORTING campo  TYPE any
                !auto  TYPE abap_bool DEFAULT ''
                valor  TYPE any       OPTIONAL
                valor2 TYPE any       OPTIONAL.

    METHODS set_field_quitar
      IMPORTING campo TYPE any.

    METHODS get_registros_filtrados
      EXPORTING i_registros_filtrados TYPE table.

    METHODS set_color
      IMPORTING campo  TYPE any
                !color TYPE i.

    METHODS save_buttons
      IMPORTING status2 TYPE abap_bool DEFAULT ''.

    METHODS restore_buttons
      IMPORTING status2 TYPE abap_bool DEFAULT ''.

    METHODS get_desc_from_clave
      IMPORTING campo              TYPE any
                VALUE(valor)       TYPE any
      RETURNING VALUE(descripcion) TYPE string.

  PROTECTED SECTION.
    METHODS fijar_layout_inicial.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_campos_desc,
        campo       TYPE string,
        dominio     TYPE string,
        caract      TYPE string,
        tabla       TYPE string,
        where       TYPE string,
        campo_desc  TYPE string,
        campo_clave TYPE string,
        valores     TYPE string,
        add_clave   TYPE string,
      END OF t_campos_desc.
    TYPES tt_campos_desc TYPE TABLE OF t_campos_desc.
    TYPES:
      BEGIN OF t_allowed_but,
        function TYPE sy-ucomm,
      END OF t_allowed_but.
    TYPES tt_excluded_but TYPE STANDARD TABLE OF sy-ucomm.
    TYPES tt_allowed_but  TYPE STANDARD TABLE OF t_allowed_but.

    DATA prnt               TYPE lvc_s_prnt.
    DATA hype               TYPE lvc_t_hype.
    DATA t_tool             TYPE ui_functions.
    DATA t_sort             TYPE lvc_t_sort.
    DATA consistency_check  TYPE char1.
    DATA contenedor         TYPE scrfname.
    DATA estructura         TYPE dd02l-tabname.
    DATA input              TYPE char1.
    DATA o_event            TYPE REF TO zcl_ap_alv_grid_eventos.
    DATA t_dropdown         TYPE lvc_t_drop.
    DATA i_filas_sel2       TYPE lvc_t_roid.
    DATA gs_f4              TYPE lvc_s_f4.
    DATA gt_f4              TYPE lvc_t_f4.
    DATA restriccion_layout TYPE c LENGTH 1 VALUE 'A' ##NO_TEXT.
    DATA opt_cols           TYPE abap_bool VALUE 'X' ##NO_TEXT.
    DATA v_usar_dragdrop    TYPE abap_bool.
    DATA default_layout     TYPE abap_bool VALUE 'X' ##NO_TEXT.
    DATA i_campos_desc      TYPE tt_campos_desc.
    DATA excluded_buttons   TYPE tt_excluded_but.
    DATA allowed_buttons    TYPE tt_allowed_but.
    DATA campo_color        TYPE string.

    METHODS get_catalogo_campos.

    METHODS create_ex_result
      RETURNING VALUE(er_result_table) TYPE REF TO cl_salv_ex_result_data_table.
endclass. "ZCL_AP_ALV_GRID definition
class ZCL_AP_ALV_GRID implementation.
  METHOD actualiza_campos_grid.
    DATA: i_fcat   TYPE lvc_t_fcat,
          i_campos TYPE TABLE OF string,
          l_campo  TYPE string.

    FIELD-SYMBOLS: <col>  TYPE zap_alv_columnas,
                   <fcat> TYPE lvc_s_fcat.

    IF get_fcat_inicial = 'X'.
      i_fcat = get_fcat_inicial( ).
    ELSE.
      o_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = i_fcat ).
    ENDIF.

    IF NOT campos_borrar IS INITIAL.
      SPLIT campos_borrar AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        CONDENSE l_campo NO-GAPS.
        l_campo = to_upper( l_campo ).
        DELETE i_fcat WHERE fieldname = l_campo.
      ENDLOOP.
    ENDIF.
    IF NOT i_columnas IS INITIAL.
      LOOP AT i_columnas ASSIGNING <col> WHERE borrar = 'X'.
        DELETE i_fcat WHERE fieldname = <col>-campo.
      ENDLOOP.
    ENDIF.

    IF NOT campos_input IS INITIAL.
      SPLIT campos_input AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        CONDENSE l_campo NO-GAPS.
        l_campo = to_upper( l_campo ).
        ASSIGN i_fcat[ fieldname = l_campo ] TO <fcat>.
        IF sy-subrc = 0.
          <fcat>-edit = 'X'.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT campos_no_input IS INITIAL.
      SPLIT campos_no_input AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        CONDENSE l_campo NO-GAPS.
        l_campo = to_upper( l_campo ).
        ASSIGN i_fcat[ fieldname = l_campo ] TO <fcat>.
        IF sy-subrc = 0.
          <fcat>-edit = ''.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT campos_no_out IS INITIAL.
      SPLIT campos_no_out AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        CONDENSE l_campo NO-GAPS.
        l_campo = to_upper( l_campo ).
        ASSIGN i_fcat[ fieldname = l_campo ] TO <fcat>.
        IF sy-subrc = 0.
          <fcat>-no_out = 'X'.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT campos_out IS INITIAL.
      SPLIT campos_out AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        CONDENSE l_campo NO-GAPS.
        l_campo = to_upper( l_campo ).
        ASSIGN i_fcat[ fieldname = l_campo ] TO <fcat>.
        IF sy-subrc = 0.
          CLEAR <fcat>-no_out.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF fix_col_noout_92 = 'X'.
      LOOP AT i_fcat ASSIGNING <fcat> WHERE     col_pos > '92'
                                            AND no_out  = 'X'.
        IF i_columnas IS INITIAL.
          CLEAR <fcat>-no_out.
        ELSE.
          ASSIGN i_columnas[ campo = <fcat>-fieldname ] TO <col>.
          IF sy-subrc = 0.
            IF <col>-noout = ''.
              CLEAR <fcat>-no_out.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT campos_opt IS INITIAL.
      SPLIT campos_opt AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        CONDENSE l_campo NO-GAPS.
        l_campo = to_upper( l_campo ).
        ASSIGN i_fcat[ fieldname = l_campo ] TO <fcat>.
        IF sy-subrc = 0.
          <fcat>-col_opt = 'X'.
        ENDIF.
      ENDLOOP.
    ELSEIF NOT campos_no_opt IS INITIAL.
      LOOP AT i_fcat ASSIGNING <fcat>.
        <fcat>-col_opt = 'X'.
      ENDLOOP.

      SPLIT campos_no_opt AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        CONDENSE l_campo NO-GAPS.
        l_campo = to_upper( l_campo ).
        ASSIGN i_fcat[ fieldname = l_campo ] TO <fcat>.
        IF sy-subrc = 0.
          <fcat>-col_opt = ''.
        ENDIF.
      ENDLOOP.
    ENDIF.

    o_grid->set_frontend_fieldcatalog( it_fieldcatalog = i_fcat ).

    IF refrescar = 'X'.
      refrescar_grid( soft_refresh = soft_refresh ).
    ENDIF.
  ENDMETHOD.
  METHOD add_button.
    DATA: but     TYPE smp_dyntxt,
          l_boton TYPE t_boton.

    FIELD-SYMBOLS <bt> TYPE any.

    CHECK button IS NOT INITIAL.

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
*      ELSE.
*        RAISE button_already_filled.
      ENDIF.
    ELSE.
*      RAISE button_does_not_exists.
    ENDIF.
  ENDMETHOD.
  METHOD add_filtro.
    DATA l_filter TYPE lvc_s_filt.

    l_filter-fieldname = campo.
    l_filter-low       = valor.
    l_filter-sign      = sign.
    l_filter-option    = option.
    APPEND l_filter TO i_filter.
  ENDMETHOD.
  METHOD append_color.
    DATA: ls_color TYPE lvc_s_scol,
          i_column TYPE TABLE OF lvc_fname,
          l_column TYPE lvc_fname.

    IF campo IS INITIAL.
      IF solo_si_no_color_previo = 'X'.
        IF line_exists( tabla_color[ fname = campo ] ).
          RETURN.
        ENDIF.
      ENDIF.
      DELETE tabla_color WHERE fname = campo.
      CLEAR ls_color.
      ls_color-fname = campo.
      ls_color-color-int = int.
      ls_color-color-inv = inv.
      IF NOT color IS INITIAL.
        ls_color-color-col = color.
      ELSEIF colorc(1) = 'C' AND strlen( colorc ) = 4.
        ls_color-color-col = colorc+1(1).
        ls_color-color-int = colorc+2(1).
        ls_color-color-inv = colorc+3(1).
      ELSE.
        ls_color-color-col = traduce_color( colorc ).
      ENDIF.
      APPEND ls_color TO tabla_color.
    ELSE.
      SPLIT campo AT ',' INTO TABLE i_column.
      LOOP AT i_column INTO l_column.
        TRANSLATE l_column TO UPPER CASE.
        IF solo_si_no_color_previo = 'X'.
          IF line_exists( tabla_color[ fname = l_column ] ).
            CONTINUE.
          ENDIF.
        ENDIF.
        DELETE tabla_color WHERE fname = l_column.
        CLEAR ls_color.
        ls_color-fname = l_column.
        ls_color-color-int = int.
        ls_color-color-inv = inv.
        IF NOT color IS INITIAL.
          ls_color-color-col = color.
        ELSEIF colorc(1) = 'C' AND strlen( colorc ) = 4.
          ls_color-color-col = colorc+1(1).
          ls_color-color-int = colorc+2(1).
          ls_color-color-inv = colorc+3(1).
        ELSE.
          ls_color-color-col = traduce_color( colorc ).
        ENDIF.
        APPEND ls_color TO tabla_color.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD append_style.
    DATA: ls_style TYPE lvc_s_styl,
          i_column TYPE TABLE OF lvc_fname,
          l_column TYPE lvc_fname.

    IF campo IS INITIAL.
      CLEAR ls_style.
      ls_style-fieldname = campo.
      ls_style-style     = style.
      INSERT ls_style INTO TABLE tabla_style.
    ELSE.
      SPLIT campo AT ',' INTO TABLE i_column.
      SORT i_column.
      LOOP AT i_column INTO l_column.
        TRANSLATE l_column TO UPPER CASE.
        DELETE tabla_style WHERE fieldname = l_column.

        CLEAR ls_style.
        ls_style-fieldname = l_column.
        ls_style-style     = style.
        INSERT ls_style INTO TABLE tabla_style.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD append_style_edit.
    append_style( EXPORTING campo       = campo
                            style       = cl_gui_alv_grid=>mc_style_enabled
                  CHANGING  tabla_style = tabla_style ).
  ENDMETHOD.
  METHOD append_style_no_edit.
    append_style( EXPORTING campo       = campo
                            style       = cl_gui_alv_grid=>mc_style_disabled
                  CHANGING  tabla_style = tabla_style ).
  ENDMETHOD.
  METHOD barra_botones_minima_edit.
    quitar_opciones( cl_gui_alv_grid=>mc_fc_data_save ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_variant_admin ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_copy ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_cut ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_undo ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_info ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_average ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_refresh ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_graph ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_check ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_view_crystal ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_view_excel ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_view_lotus ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_expcrdata ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_expcrdesig ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_expcrtempl ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_call_report ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_maintain_variant ).
*FPG04/05/2011 - Se muestra el botón de filtro.
* quitar_opciones( cl_gui_alv_grid=>mc_fc_filter  ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_subtot  ).
    quitar_opciones( cl_gui_alv_grid=>mc_mb_export  ).
  ENDMETHOD.
  METHOD cambia_atributo_columna.
    DATA: i_fcat   TYPE lvc_t_fcat,
          i_campos TYPE TABLE OF string,
          l_campo  TYPE string,
          l_atrib  TYPE string.

    " TODO: variable is assigned but never used (ABAP cleaner)
    FIELD-SYMBOLS: <fcat> TYPE lvc_s_fcat,
                   <fs>   TYPE any.

    IF get_fcat_inicial = 'X'.
      i_fcat = get_fcat_inicial( ).
    ELSE.
      o_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = i_fcat ).
    ENDIF.

    IF NOT campos IS INITIAL.
      SPLIT campos AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        CONDENSE l_campo NO-GAPS.
        l_campo = to_upper( l_campo ).
        ASSIGN i_fcat[ fieldname = l_campo ] TO <fcat>.
        IF sy-subrc = 0.
          CONCATENATE '<FCAT>-' atributo INTO l_atrib.
          ASSIGN (l_atrib) TO <fs>.
          IF sy-subrc = 0.
            <fs> = valor.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    o_grid->set_frontend_fieldcatalog( it_fieldcatalog = i_fcat ).

    IF refrescar = 'X'.
      refrescar_grid( soft_refresh = soft_refresh ).
    ENDIF.
  ENDMETHOD.
  METHOD cambia_layout.
    DATA : e_layo  TYPE lvc_s_layo,
           l_campo TYPE string.

    FIELD-SYMBOLS <fs> TYPE any.

    o_grid->get_frontend_layout( IMPORTING es_layout = e_layo ).

    CONCATENATE 'E_LAYO-' campo INTO l_campo.
    ASSIGN (l_campo) TO <fs>.
    IF sy-subrc = 0.
      IF <fs> <> valor.
        <fs> = valor.
        o_grid->set_frontend_layout( is_layout = e_layo ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD cambiar_variant.
    DATA: lv_user_specific TYPE c LENGTH 1,
          lt_fieldcat      TYPE lvc_t_fcat,
          lt_sort          TYPE lvc_t_sort,
          lt_filter        TYPE lvc_t_filt,
          ls_stable        TYPE lvc_s_stbl.

    IF handle <> '?'.
      me->variant-handle = handle.
    ENDIF.

    IF NOT variant IS INITIAL.
      me->variant-variant = variant.
    ENDIF.

    IF get_default_variant = 'X'.
      key_layout-report = me->variant-report.
      key_layout-handle = me->variant-handle.
      me->variant-variant = get_default_layout( ).

      IF me->variant-variant <> ''.
        IF me->variant-variant(1) <> '/'.
          me->variant-username = sy-uname.
          lv_user_specific = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF me->variant-variant IS INITIAL.
      IF NOT i_fcat_ini IS INITIAL AND get_fcat_inicial_si_no_variant = 'X'.
        lt_fieldcat = i_fcat_ini.
      ENDIF.
    ENDIF.

    IF lt_fieldcat IS INITIAL.
      o_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fieldcat ).
    ENDIF.

    IF NOT me->variant-variant IS INITIAL.
      CALL FUNCTION 'LVC_VARIANT_SELECT'
        EXPORTING  i_dialog            = space
                   i_user_specific     = lv_user_specific
                   it_default_fieldcat = lt_fieldcat
        IMPORTING  et_fieldcat         = lt_fieldcat
                   et_sort             = lt_sort
                   et_filter           = lt_filter
        CHANGING   cs_variant          = me->variant
        EXCEPTIONS wrong_input         = 1
                   fc_not_complete     = 2
                   not_found           = 3
                   program_error       = 4
                   data_missing        = 5
                   OTHERS              = 6.
      IF sy-subrc <> 0.
        MESSAGE 'Error recuperando layout'(erl) TYPE 'S'.
      ENDIF.

    ENDIF.

    o_grid->set_variant( me->variant ).
    o_grid->set_frontend_fieldcatalog( lt_fieldcat ).
    o_grid->set_filter_criteria( lt_filter ).
    o_grid->set_sort_criteria( lt_sort ).
    ls_stable = 'XX'.
    o_grid->refresh_table_display( is_stable      = ls_stable
                                   i_soft_refresh = space ).
  ENDMETHOD.
  METHOD completar_columnas.
    DATA: l_lineas TYPE i,
          l_cont   TYPE i,
          l_col    TYPE zap_alv_columnas.

    l_lineas = lines( i_columnas ).
    IF l_lineas > max_cols.
      IF mostrar_aviso = 'X'.
        MESSAGE 'Solo es posible mostrar 10 columnas'(c10) TYPE 'I'.
      ENDIF.

      DELETE i_columnas FROM max_cols.
    ELSE.
      l_cont = l_lineas + 1.
      WHILE l_cont <= max_cols.
        CLEAR l_col.
        l_col-colum = l_cont.
        CONCATENATE campo l_col-colum INTO l_col-campo.
        l_col-noout  = 'X'.
        l_col-borrar = borrar.
        APPEND l_col TO i_columnas.
        l_cont = l_cont + 1.
      ENDWHILE.
    ENDIF.

    me->max_cols = max_cols.
    me->campo    = campo.
  ENDMETHOD.
  METHOD comprobar_cambios.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_valid TYPE c LENGTH 1.

    CHECK o_grid IS BOUND.

* http://scn.sap.com/community/abap/blog/2013/08/13/trigger-alv-datachange-event-manual?campaigncode=CRM-XM13-SOC-TW_SCNB
    IF forzar_validaciones = 'X'.
      o_grid->activate_display_protocol( i_dialog = 'X' ).

      o_grid->if_cached_prop~set_prop( propname = 'GridModified' propvalue = '1' ).
    ENDIF.

    o_grid->check_changed_data( IMPORTING e_valid = l_valid ).

    IF forzar_validaciones = 'X'.
      o_grid->activate_display_protocol( i_dialog = '' ).
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
*  IF cl_gui_alv_grid=>offline( ) = 1.
*    MESSAGE 'No es posible crear el ALV GRID' TYPE 'W'.
*    EXIT.
*  ENDIF.

    DATA l_event TYPE cntl_simple_event.

* Si no se había creado el objeto...
    IF o_custom_container IS NOT INITIAL.
      RETURN.
    ENDIF.

* Creamos el objeto y lo asignamos al dynpro
    IF o_container IS INITIAL.
      o_custom_container = NEW #( container_name = obj_contenedor ).

      o_grid = NEW #( i_parent = o_custom_container ).
    ELSE.
      o_grid = NEW #( i_parent = o_container ).
    ENDIF.

    IF o_event IS SUPPLIED AND NOT o_event IS INITIAL.
      o_event->o_alv = me.
      me->o_event = o_event.
      IF o_event->v_double_click IS INITIAL.
        SET HANDLER o_event->double_click FOR o_grid.
      ENDIF.
      IF o_event->v_data_changed IS INITIAL.
        SET HANDLER o_event->data_changed FOR o_grid.
      ENDIF.
      IF o_event->v_data_changed_finished IS INITIAL.
        SET HANDLER o_event->data_changed_finished FOR o_grid.
      ENDIF.
      IF o_event->v_user_command IS INITIAL.
        SET HANDLER o_event->user_command FOR o_grid.
      ENDIF.
      IF o_event->v_after_refresh IS INITIAL.
        SET HANDLER o_event->after_refresh FOR o_grid.
      ENDIF.
      IF o_event->v_print_top_of_page IS INITIAL.
        SET HANDLER o_event->print_top_of_page FOR o_grid.
      ENDIF.
      IF o_event->v_top_of_page IS INITIAL.
        SET HANDLER o_event->top_of_page FOR o_grid.
      ENDIF.
      IF o_event->v_toolbar IS INITIAL.
        SET HANDLER o_event->toolbar FOR o_grid.
      ENDIF.
      IF o_event->v_onf4 IS INITIAL.
        SET HANDLER o_event->onf4 FOR o_grid.
      ENDIF.
      IF o_event->v_button_click IS INITIAL.
        SET HANDLER o_event->button_click FOR o_grid.
      ENDIF.
      IF o_event->v_hotspot_click IS INITIAL.
        SET HANDLER o_event->hotspot_click FOR o_grid.
      ENDIF.
      IF o_event->v_ondrag IS INITIAL.
        v_usar_dragdrop = dragdrop.
        SET HANDLER o_event->ondrag FOR o_grid.
      ENDIF.
      IF o_event->v_ondrop IS INITIAL.
        v_usar_dragdrop = dragdrop.
        SET HANDLER o_event->ondrop FOR o_grid.
      ENDIF.
      IF o_event->v_after_user_command IS INITIAL.
        SET HANDLER o_event->after_user_command FOR o_grid.
      ENDIF.
      IF o_event->v_delayed_changed_sel IS INITIAL.
        l_event-eventid = cl_gui_alv_grid=>mc_evt_delayed_change_select.
        o_grid->register_delayed_event( EXPORTING  i_event_id = l_event-eventid
                                        EXCEPTIONS OTHERS     = 1 ).

        SET HANDLER o_event->delayed_changed_sel_callback FOR o_grid.
      ENDIF.
    ENDIF.

    fijar_layout_inicial( ).

    me->nombre_tabla       = nombre_tabla.
    me->cprog              = cprog.
    me->estructura         = estructura.
    me->restriccion_layout = restriccion_layout.
    me->default_layout     = default_layout.
    me->sin_buffer         = sin_buffer.
    IF NOT estructura IS INITIAL.
      get_catalogo_campos( ).
    ENDIF.

    IF nombre_layout = '¡No!'.
      CONCATENATE sy-cprog(30) '-' estructura INTO variant-report.
    ELSEIF nombre_layout IS INITIAL.
      IF estructura IS INITIAL.
        CONCATENATE sy-cprog(30) '-' nombre_tabla INTO variant-report.
      ELSE.
        CONCATENATE sy-cprog(30) '-' estructura INTO variant-report.
      ENDIF.
    ELSE.
      variant-report = nombre_layout.
    ENDIF.

    key_layout-report = variant-report.
    variant-handle = handle.
    key_layout-handle = handle.
  ENDMETHOD.
  METHOD correguir_variantes.
    DATA: l_report      TYPE salv_s_layout_key-report,
          i_ltdx        TYPE TABLE OF ltdx,
          rs_varkey     TYPE ltdxkey,
          rt_dbfieldcat TYPE TABLE OF ltdxdata.

    FIELD-SYMBOLS: <ltdx>  TYPE ltdx,
                   <col>   TYPE zap_alv_columnas,
                   <campo> TYPE ltdxdata,
                   <col3>  TYPE zap_alv_columnas3.

    l_report = key_layout-report.
    SELECT relid report handle log_group username variant type FROM ltdx
      INTO CORRESPONDING FIELDS OF TABLE i_ltdx
     WHERE relid  = 'LT'
       AND report = l_report
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
          IF <campo>-value <> <col>-noout.
            sin_buffer = 'X'.
          ENDIF.
          <campo>-value = <col>-noout.
        ENDLOOP.

        LOOP AT rt_dbfieldcat ASSIGNING <campo> WHERE     key1  = <col>-campo
                                                      AND param = 'TECH'.
          IF <campo>-value <> <col>-borrar.
            sin_buffer = 'X'.
          ENDIF.
          <campo>-value = <col>-borrar.
        ENDLOOP.
      ENDLOOP.

      LOOP AT i_columnas3 ASSIGNING <col3> WHERE no_variante = 'X'.
        LOOP AT rt_dbfieldcat ASSIGNING <campo> WHERE     key1  = <col3>-campo
                                                      AND param = 'NO_OUT'.
          IF <campo>-value <> <col3>-noout.
            sin_buffer = 'X'.
          ENDIF.

          <campo>-value = <col3>-noout.
        ENDLOOP.

        LOOP AT rt_dbfieldcat ASSIGNING <campo> WHERE     key1  = <col3>-campo
                                                      AND param = 'TECH'.
          IF <campo>-value <> <col3>-borrar.
            sin_buffer = 'X'.
          ENDIF.

          <campo>-value = <col3>-borrar.
        ENDLOOP.
      ENDLOOP.

      EXPORT lt_dbfieldcat FROM rt_dbfieldcat
             TO DATABASE ltdx(lt)
             ID rs_varkey.
    ENDLOOP.
  ENDMETHOD.
  METHOD create_ex_result.
    DATA o_grid_p TYPE REF TO lcl_alv_private.

    o_grid_p = NEW #( io_grid = o_grid ).

    er_result_table = o_grid_p->create_ex_result( ).
  ENDMETHOD.
  METHOD export_to_spread_sheet.
*    data: lr_result_data type ref to cl_salv_ex_result_data_table,
*          ls_error type cl_salv_bs_lex_support=>ys_export_error,
*          lt_errors type cl_salv_bs_lex_support=>yt_export_error,
*          ls_mask type string,
*          ls_def_filename type string,
*          ls_application type string.
*
*    lr_result_data  = o_grid->CREATE_EX_RESULT( ).
*

**DATA I_REPORT          TYPE SY-REPID.
**DATA IT_FIELDCAT       TYPE LVC_T_FCAT.
**DATA IT_FIELDCAT_KKBLO TYPE KKBLO_T_FIELDCAT.
**DATA E_EXIT            TYPE CHAR1.
**DATA IT_DATA           TYPE TABLE.
*
*CALL FUNCTION 'ALV_DATA_EXPORT'
*  EXPORTING
*    i_report                = i_report
**   IT_FIELDCAT             = IT_FIELDCAT
**   IT_FIELDCAT_KKBLO       = IT_FIELDCAT_KKBLO
** IMPORTING
**   E_EXIT                  = E_EXIT
*  TABLES
*    it_data                 = it_data
*          .
*
**...choose Transformation  Y7AK044553
*  data: ls_xml_choice type if_salv_bs_xml=>s_type_xml_choice.
*  data: lt_xml_choice type if_salv_bs_xml=>t_type_xml_choice.
*  data: xml           type xstring.
*
*  data: l_gui_type type i.
*  O_GRID->export_to_xml('&XXL').
*
*  call method O_GRID->get_gui_type
*    receiving
*      gui_type = l_gui_type.
*
**  if l_gui_type eq m_gui_type_java
**  or l_gui_type eq m_gui_type_html.
**    me->export_to_xml('&XXL').
**    exit.
**  endif.
*
*  data:
*    l_xml type xstring,
*    l_filename type string,
*    l_file_extension type string,
*    l_mimetype type string,
*    l_enable_lean_export type abap_bool,
*    l_lean_export_format type if_salv_bs_lex_format=>ys_format.
*
*
*  l_enable_lean_export = cl_alv_z_params=>get_parameter(
*                             cl_alv_z_params=>c_flag-use_lean_export ).
*  " ALV Lean Export
*  if l_enable_lean_export eq abap_true.
*    l_lean_export_format = cl_alv_z_params=>get_parameter(
*                             cl_alv_z_params=>c_lean_export_format_switch-name ).
*
*    data: lr_result_data type ref to cl_salv_ex_result_data_table,
*          ls_error type cl_salv_bs_lex_support=>ys_export_error,
*          lt_errors type cl_salv_bs_lex_support=>yt_export_error,
*          ls_mask type string,
*          ls_def_filename type string,
*          ls_application type string.
*
*    lr_result_data  = me->CREATE_EX_RESULT( ).
*
*    cl_salv_bs_lex=>export_from_result_data_table(
*      exporting
*        is_format            = l_lean_export_format
*        ir_result_data_table = lr_result_data
*      importing
*        er_result_file       = l_xml
*        et_export_errors     = lt_errors
*        es_filename          = l_filename
*        es_file_extension   = l_file_extension
**        es_mimetype          = l_mimetype
*        ).
*
*    ls_application = ''.
*
*    " if we have MHTML format, we need to set application
*    " otherwise the MHTML file would open in Browser
*    if l_lean_export_format eq if_salv_bs_lex_format=>mc_format_mhtml.
*      "... determine PC appl.: only possible in Windows GUI due to REGEX calls
*      if l_gui_type eq cl_salv_export_xml_dialog=>c_gui_type_windows.
*
*        ls_application = cl_salv_bs_xml_utils=>get_pc_application( cl_alv_bds=>mc_mhtml_frontend ).
*      else.
*        " If we cannot specify the application, we just rename the file extension.
*        " Then the file also opens up in Excel - but there might be a warning message
*        " from Excel that the content does not match the extension.
*        l_file_extension = 'xls'.
*      endif.
*    endif.
*
*    ls_def_filename = 'export.' && l_file_extension.
*    ls_mask = l_file_extension && ' file (*.' && l_file_extension && ')|*.' && l_file_extension.
*
**... call Filedownload Dialog and download file
*    call function 'XML_EXPORT_DIALOG'
*      exporting
*        i_xml                      = l_xml
*        i_default_extension        = l_file_extension
*        i_initial_directory        = ''
*        i_default_file_name        = ls_def_filename
*        i_mask                     = ls_mask
*        i_application              = ls_application
*      exceptions
*        application_not_executable = 1
*        others                     = 2.
*
*    return.
*  endif.
*
*  data: l_display_mode type i.
*  if m_eventid eq evt_context_menu_selected.
*    l_display_mode = CL_SALV_EXPORT_XML_DIALOG=>C_DISPLAY_MODE_INACTIVE_CBOX.
*  endif.
*
*  if 1 = 1. "Y7AK048443
*    lt_xml_choice = cl_salv_export_xml_dialog=>execute( gui_type     = l_gui_type
*                                                        display_mode = l_display_mode ).
*
*    read table lt_xml_choice into ls_xml_choice index 1.
*    if sy-subrc ne 0.
*      exit.
*    endif.
*  else.
*    ls_xml_choice-xml_type = if_salv_bs_xml=>c_type_xxl.
*  endif.
*
*  if ls_xml_choice-xml_type eq if_salv_bs_xml=>c_type_xxl.
*
*    field-symbols: <tab1> type standard table.
*
*    data: ls_layout      type kkblo_layout,
*          l_appl         type i,
*          lt_fieldcat_wa type kkblo_fieldcat,
*          lt_fieldcat    type kkblo_t_fieldcat.
*
*    data: l_tabname      type slis_tabname.
*
*    message i064(0k).                                       "B5AK000316
*    system-call pop list.                            "#EC CI_SYSTEMCALL
*
**... Check Outtab
*    assign mt_outtab->* to <tab1>.
*    if not sy-subrc is initial.
*      message a534(0k).
*    endif.
*
*    call function 'LVC_TRANSFER_TO_KKBLO'
*      exporting
*        it_fieldcat_lvc   = m_cl_variant->mt_fieldcatalog
*        is_layout_lvc     = m_cl_variant->ms_layout
*        is_tech_complete  = ' '
*      importing
*        es_layout_kkblo   = ls_layout
*        et_fieldcat_kkblo = lt_fieldcat.
*
*    loop at lt_fieldcat into lt_fieldcat_wa.
*      clear lt_fieldcat_wa-tech_complete.
*      if lt_fieldcat_wa-tabname is initial.
*        lt_fieldcat_wa-tabname = '1'.
*        modify lt_fieldcat from lt_fieldcat_wa.
*      endif.
*      l_tabname = lt_fieldcat_wa-tabname.
*    endloop.
*
*    call function 'ALV_XXL_CALL'
*      exporting
*        i_tabname           = l_tabname
*        is_layout           = ls_layout
*        it_fieldcat         = lt_fieldcat
*        i_title             = sy-title
*      tables
*        it_outtab           = <tab1>
*      exceptions
*        fatal_error         = 1
*        no_display_possible = 2
*        others              = 3.
*    if sy-subrc <> 0.
*      message id sy-msgid type 'S' number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    endif.
*
*  else.
**...Target
*    data:
*      lr_export  type ref to cl_salv_bs_tt_file.
*
*    field-symbols: <lt_data>       type standard table.
*
*    if cl_salv_bs_a_xml_base=>get_version( ) eq if_salv_bs_xml=>version_25 or
*       cl_salv_bs_a_xml_base=>get_version( ) eq if_salv_bs_xml=>version_26.
*
*      data: r_result_data type ref to cl_salv_ex_result_data_table.
*
*      r_result_data  = me->CREATE_EX_RESULT( ).
*
*      data: l_flavour type string.
*      data: l_version type string.
*
*      case cl_salv_bs_a_xml_base=>get_version( ).
*        when if_salv_bs_xml=>version_25.
*          l_version = if_salv_bs_xml=>version_25.
*        when if_salv_bs_xml=>version_26.
*          l_version = if_salv_bs_xml=>version_26.
*      endcase.
*
**      if ls_xml_choice-xml_type eq IF_SALV_BS_C_TT=>C_TT_ALVXML.
**        l_flavour = IF_SALV_BS_C_TT=>C_TT_XML_FLAVOUR_FULL.
**      else.
*        l_flavour = IF_SALV_BS_C_TT=>C_TT_XML_FLAVOUR_EXPORT.
**      endif.
*
*      call method cl_salv_bs_tt_util=>IF_SALV_BS_TT_UTIL~TRANSFORM
*          exporting xml_type      = ls_xml_choice-xml_type
*                    xml_version   = l_version
*                    r_result_data = r_result_data
*                    xml_flavour   = l_flavour
*                    gui_type      = if_salv_bs_xml=>c_gui_type_gui  "Y6DK066330
*          importing xml           = xml
*                    t_msg         = data(lt_msg).        "YI3K229857
*
*      if lt_msg is not initial.
*        data: ls_msg type symsg,
*              l_text type string.
** msgid = 203  IGS not available, 204 IGS without ZIP interpreter,
** single msg added in cl_salv_bs_tt_ods->add_metadata
*        read TABLE lt_msg index 1 into ls_msg.   "msgid = SALV_BS_MSG'
*        if sy-subrc eq 0.
*           message id ls_msg-msgid type ls_msg-msgty number ls_msg-msgno
*                   with ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
*                   into l_text.
*
*            message l_text type 'S'.
*        endif.
*        return.
*      endif.       "YI3K229857
*
*    else.
*      data: lr_controller type ref to cl_salv_export_c8r.
*      data: lr_table      type ref to cl_salv_table.
*      data: lr_columns    type ref to cl_salv_columns_table.
*      data: l_column      type lvc_fname.
*      data: ls_hype       type if_salv_export=>s_type_hlink.
*
*      if me->r_salv_adapter is bound.
*        lr_table ?= me->r_salv_adapter->r_controller->r_model.
*        lr_columns = lr_table->get_columns( ).
*        l_column = lr_columns->get_hyperlink_entry_column( ).
*      endif.
*
*      ls_hype-t_hype = mt_hyperlinks[].
*      ls_hype-hlink_colname = l_column.
*
*      create object lr_controller
*        exporting
*          t_choice = lt_xml_choice.
*
*      lr_controller->execute(
*                      exporting
*                        t_fcat     = m_cl_variant->mt_fieldcatalog
*                        t_sort     = m_cl_variant->mt_sort
*                        t_filt     = m_cl_variant->mt_filter
*                        s_layo     = m_cl_variant->ms_layout
*                        s_hype     = ls_hype
**                        r_form_tol = s_param_lvc-r_form_tol
**                        r_form_eol = s_param_lvc-r_form_eol
*                      importing
*                        e_xml      = xml
*                      changing
*                        r_data     = mt_outtab
*                             ).
*
*    endif.
**...Filedownload
*    cl_salv_export_xml_dialog=>download(
*                      exporting
*                        s_xml_choice = ls_xml_choice
*                        xml          = xml
*                      ).
*  endif.
  ENDMETHOD.
  METHOD exportar_excel.
    DATA: o_excel      TYPE REF TO zcl_ap_excel_ole,
          l_titulo     TYPE scrtext_l,
          i_titulos    TYPE TABLE OF scrtext_l,
          l_tabla      TYPE string,
          l_error      TYPE string,
          i_campos     TYPE abap_component_tab,
          i_campos_sel TYPE abap_component_tab.

    FIELD-SYMBOLS: <fcat>   TYPE lvc_s_fcat,
                   <campos> TYPE abap_componentdescr.

    FIELD-SYMBOLS <tabla> TYPE table.

    o_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = t_fcat ).

    o_excel = NEW #( visible = 0 ).

    o_excel->crear_documento( ).

    LOOP AT t_fcat ASSIGNING <fcat>.
      IF <fcat>-no_out = 'X'.
        l_titulo = '#INVISIBLE#'.
      ELSE.
        l_titulo = <fcat>-scrtext_l.
        IF l_titulo IS INITIAL.
          l_titulo = <fcat>-fieldname.
        ENDIF.
      ENDIF.
      APPEND l_titulo TO i_titulos.
    ENDLOOP.

    IF t_tabla IS INITIAL.
      CONCATENATE '(' cprog ')' nombre_tabla '[]' INTO l_tabla.
      ASSIGN (l_tabla) TO <tabla>.
      IF sy-subrc <> 0.
        CONCATENATE '(' cprog ')O_PROG->' nombre_tabla '[]' INTO l_tabla.
        ASSIGN (l_tabla) TO <tabla>.
      ENDIF.
      IF <tabla> IS NOT ASSIGNED.
        CONCATENATE 'La tabla'(tab) l_tabla 'no existe'(nex)
                    INTO l_error SEPARATED BY space.
        MESSAGE l_error TYPE 'I'
                DISPLAY LIKE 'E'.
      ELSE.
        IF NOT <tabla> IS INITIAL.

          IF NOT t_fcat IS INITIAL.
            i_campos = zcl_ap_dev=>get_fieldcatalog( <tabla> ).

            LOOP AT t_fcat ASSIGNING <fcat>.
              ASSIGN i_campos[ name = <fcat>-fieldname ] TO <campos>.
              IF sy-subrc = 0.
                APPEND <campos> TO i_campos_sel.
              ENDIF.
            ENDLOOP.
          ENDIF.

          o_excel->pegar_tabla( cabecera     = 'X'
                                i_tabla      = <tabla>
                                autofiltro   = autofiltro
                                i_titulos    = i_titulos
                                i_columnas3  = i_columnas3
                                i_campos_sel = i_campos_sel
                                replacement  = 32  ).
        ENDIF.
      ENDIF.
    ELSE.
      o_excel->pegar_tabla( cabecera = 'X' i_tabla = t_tabla autofiltro = autofiltro i_titulos = i_titulos i_columnas3 = i_columnas3 replacement = 32 ).
    ENDIF.

    IF NOT fichero_salida IS INITIAL.
      o_excel->grabar_documento( fichero = fichero_salida
                                 cerrar  = cerrar ).
    ENDIF.

    IF cerrar IS INITIAL.
      o_excel->fijar_visible( ).
    ENDIF.

    o_excel->free( ).
  ENDMETHOD.
  METHOD exportar_xlsx.
    DATA result_data     TYPE REF TO cl_salv_ex_result_data_table.
    DATA version         TYPE string.
    DATA salv_intf_descr TYPE REF TO cl_abap_objectdescr.
    DATA file_type       TYPE salv_bs_constant.
    DATA: l_dir     TYPE string,
          l_fichero TYPE string,
          l_mensaje TYPE bapi_msg.

    IF    cl_salv_bs_a_xml_base=>get_version( ) = if_salv_bs_xml=>version_25
       OR cl_salv_bs_a_xml_base=>get_version( ) = if_salv_bs_xml=>version_26.

      result_data = create_ex_result( ).

      CASE cl_salv_bs_a_xml_base=>get_version( ).
        WHEN if_salv_bs_xml=>version_25.
          version = if_salv_bs_xml=>version_25.
        WHEN if_salv_bs_xml=>version_26.
          version = if_salv_bs_xml=>version_26.
      ENDCASE.

      " if XLSX is possible then we create it,  if not then MHTML excel file
      salv_intf_descr ?= cl_abap_intfdescr=>describe_by_name( p_name = 'IF_SALV_BS_XML' ).
      IF salv_intf_descr IS NOT INITIAL AND line_exists( salv_intf_descr->attributes[ name = 'C_TYPE_XLSX'  ] ).
        file_type = 10.
      ELSE.
        file_type = 02.
      ENDIF.

      " transformation of data to excel
      cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
        EXPORTING xml_type      = file_type
                  xml_version   = version
                  r_result_data = result_data
                  xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
                  gui_type      = if_salv_bs_xml=>c_gui_type_gui
        IMPORTING xml           = xstring ).
    ENDIF.

    IF xstring IS INITIAL.
      RETURN.
    ENDIF.

    IF NOT fichero IS INITIAL OR NOT dialogo_grabar_fichero IS INITIAL.
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
    ENDIF.
  ENDMETHOD.
  METHOD fijar_layout_inicial.
**   Atributos del grid

    DATA: effect     TYPE i,
          handle_alv TYPE i.

    layout-cwidth_opt = 'X'.
    layout-zebra      = 'X'.
    layout-sel_mode   = 'D'.                " Permite selección multiple
    layout-no_rowmark = ''.               " Permitir selección de filas
*    ls_layo-info_fname = 'COLOR'.          "Campo que indica el color
**    ls_layo-excp_fname = 'LIGHTS'.         "Campo que indica semáfor
*    ls_layo-stylefname = 'ESTILO_CELDA'.   "Campo que indica el estilo
    layout-no_merging = ''.               " Permite mezclar filas
    CONCATENATE sy-repid sy-dynnr INTO variant-report.
    key_layout-report = variant-report.

    IF v_usar_dragdrop <> 'X'.
      RETURN.
    ENDIF.

    o_dragdrop = NEW #( ).

    effect = cl_dragdrop=>move + cl_dragdrop=>copy.
    o_dragdrop->add( flavor     = 'Line'                                 ##NO_TEXT
                     dragsrc    = 'X'
                     droptarget = 'X'
                     effect     = effect ).

    o_dragdrop->get_handle( IMPORTING handle = handle_alv ).

    layout-s_dragdrop-row_ddid  = handle_alv.
    layout-s_dragdrop-fieldname = 'DRAGDROP'.
    layout-s_dragdrop-grid_ddid = handle_alv.
    layout-s_dragdrop-cntr_ddid = handle_alv.

*CNTR_DDID
*GRID_DDID
*COL_DDID
*ROW_DDID
*FIELDNAME
  ENDMETHOD.
  METHOD forzar_no_edicion.
    DATA : ts_fcat TYPE lvc_t_fcat,
           e_layo  TYPE lvc_s_layo.

    FIELD-SYMBOLS <fs_fcat> TYPE lvc_s_fcat.

    o_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = ts_fcat ).

    LOOP AT ts_fcat ASSIGNING <fs_fcat> WHERE edit = 'X'.
      IF campos IS INITIAL OR campos CS <fs_fcat>-fieldname.
        CLEAR <fs_fcat>-edit.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0.
      o_grid->set_frontend_fieldcatalog( it_fieldcatalog = ts_fcat ).
    ENDIF.

    o_grid->get_frontend_layout( IMPORTING es_layout = e_layo ).
    IF NOT e_layo-edit IS INITIAL.
      CLEAR e_layo-edit.

      o_grid->set_frontend_layout( is_layout = e_layo ).
    ENDIF.
  ENDMETHOD.
  METHOD free.
    IF NOT o_custom_container IS INITIAL.
      o_custom_container->free( ).
      CLEAR o_custom_container.
    ENDIF.
    IF NOT o_grid IS INITIAL.
      o_grid->free( EXCEPTIONS cntl_error        = 1
                               cntl_system_error = 2
                               OTHERS            = 3 ).

      CLEAR o_grid.
    ENDIF.

    CLEAR: t_fcat,
           layout,
           variant,
           prnt,
           hype,
           t_tool,
           t_sort,
           consistency_check,
           contenedor,
           o_custom_container,
           estructura,
           input,
           o_event,
           t_dropdown,
           i_filas_sel,
           fila,
           i_filas_sel2.
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
  METHOD get_catalogo_campos.
    FIELD-SYMBOLS <fcat> TYPE lvc_s_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING  i_buffer_active        = ' '
                 i_structure_name       = estructura
                 i_client_never_display = 'X'
                 i_bypassing_buffer     = ' '
      CHANGING   ct_fieldcat            = t_fcat[]
      EXCEPTIONS inconsistent_interface = 1
                 program_error          = 2
                 OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE 'No se han recuperado campos'(nrc) TYPE 'S'.
    ENDIF.

* Alguna vez, y no sé por qué, me han aparecido campos marcados como NOOUT por defecto
    LOOP AT t_fcat ASSIGNING <fcat> WHERE no_out = 'X'.
      CLEAR <fcat>-no_out.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_catalogo_campos_tabint.
    " TODO: parameter INCLUDE is only used in commented-out code (ABAP cleaner)
    " TODO: parameter INTERNAL_TABNAME is only used in commented-out code (ABAP cleaner)
    " TODO: parameter PROGRAMA is never used (ABAP cleaner)

*  DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
*        l_abap_source TYPE TABLE OF rssource.
*
*  CLASS cx_sy_read_src_line_too_long DEFINITION LOAD.
*  DATA: ex_too_long TYPE REF TO  cx_sy_read_src_line_too_long.
*  TRY.
*      READ REPORT include INTO l_abap_source.
*    CATCH cx_sy_read_src_line_too_long INTO ex_too_long.
*      MESSAGE 'Longitud de línea demasiado larga' type 'E'.
*  ENDTRY.
*  CHECK sy-subrc EQ 0.
*
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name         = PROGRAMA
*      i_internal_tabname     = internal_tabname
*      i_inclname             = include
*      i_bypassing_buffer     = 'X'
*    CHANGING
*      ct_fieldcat            = it_fieldcat
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.
*
*  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
*    EXPORTING
*      it_fieldcat_alv       = it_fieldcat
**   IT_SORT_ALV           =
**   IT_FILTER_ALV         =
**   IS_LAYOUT_ALV         =
*   IMPORTING
*     et_fieldcat_lvc       = it_fieldcatalog
**   ET_SORT_LVC           =
**   ET_FILTER_LVC         =
**   ES_LAYOUT_LVC         =
*    TABLES
*      it_data               = tabla
*   EXCEPTIONS
*     it_data_missing       = 1
*     OTHERS                = 2.

    DATA:
      lr_table        TYPE REF TO data,
      lo_salv_table   TYPE REF TO cl_salv_table,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lo_aggregations TYPE REF TO cl_salv_aggregations.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

* create unprotected table from import table
* This is required because IMPORTed IT_TABLE can not be passed to CHANGING parameter t_table
    CREATE DATA lr_table LIKE tabla.
    ASSIGN lr_table->* TO <table>.
* New SALV Instance
    TRY.
        cl_salv_table=>factory( EXPORTING list_display = abap_false
                                IMPORTING r_salv_table = lo_salv_table
                                CHANGING  t_table      = <table> ).
      CATCH cx_salv_msg ##NO_HANDLER.
    ENDTRY.
* get columns object (basic field catalog data)
    lo_columns = lo_salv_table->get_columns( ).
* get aggregations object (Sorts&Sums)
    lo_aggregations = lo_salv_table->get_aggregations( ).
* use method to create field catalog from this information
    it_fieldcatalog =
      cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lo_columns
                                                         r_aggregations = lo_aggregations ).
  ENDMETHOD.
  METHOD get_col.
    CLEAR col.
    IF NOT colum IS INITIAL.
      READ TABLE i_columnas INTO col WITH KEY colum = colum.
    ELSEIF NOT campo IS INITIAL.
      READ TABLE i_columnas INTO col WITH KEY campo = campo.
    ENDIF.
  ENDMETHOD.                    " GET_VALOR_CAMPO
  METHOD get_default_layout.
    DATA ls_layout TYPE salv_s_layout_info.

    ls_layout = cl_salv_layout_service=>get_default_layout( s_key    = key_layout
                                                            restrict = cl_salv_layout=>restrict_none ).

    layout = ls_layout-layout.
  ENDMETHOD.
  METHOD get_desc_from_clave.
    DATA: l_campos      TYPE t_campos_desc,
          l_campo_clave TYPE string,
          l_where       TYPE string.

    READ TABLE i_campos_desc INTO l_campos WITH KEY campo = campo.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF NOT l_campos-dominio IS INITIAL.
      descripcion = zcl_ap_utils=>get_texto_dominio( dominio = l_campos-dominio valor = valor ).
    ELSEIF NOT l_campos-tabla IS INITIAL.
      IF l_campos-campo_clave IS INITIAL.
        l_campo_clave = campo.
      ELSE.
        l_campo_clave = l_campos-campo_clave.
      ENDIF.

      CONCATENATE l_campo_clave ' = ''' valor '''' INTO l_where.
      IF NOT l_campos-where IS INITIAL.
        CONCATENATE l_where 'and' l_campos-where INTO l_where SEPARATED BY space.
      ENDIF.
      SELECT SINGLE (l_campos-campo_desc) FROM (l_campos-tabla)
        INTO descripcion
       WHERE (l_where).

      IF NOT l_campos-add_clave IS INITIAL.
        CONCATENATE valor descripcion INTO descripcion SEPARATED BY l_campos-add_clave.
      ENDIF.

    ELSEIF NOT l_campos-valores IS INITIAL.
      DATA(i_lista) = zcl_ap_lista=>get_valores_lista( lista = l_campos-valores ).
      ASSIGN i_lista[ valor = valor ] TO FIELD-SYMBOL(<lista>).
      IF sy-subrc = 0.
        descripcion = <lista>-subvalor.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_f4_layout.
    DATA ls_layout TYPE salv_s_layout_info.

    ls_layout = cl_salv_layout_service=>f4_layouts( s_key    = key_layout
                                                    restrict = cl_salv_layout=>restrict_none ).

    layout = ls_layout-layout.
  ENDMETHOD.
  METHOD get_fcat_inicial.
    IF i_fcat_ini IS INITIAL.
      o_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = i_fcat_ini ).
    ENDIF.

    fcat = i_fcat_ini.
  ENDMETHOD.
  METHOD get_fila_activa.
    DATA l_row_no TYPE lvc_s_roid.

    IF indice_sin_filtros IS INITIAL.
      o_grid->get_current_cell( IMPORTING e_row = fila  ). " Fila filtrada
    ELSE.
      o_grid->get_current_cell( IMPORTING es_row_no = l_row_no ). " Fila sin tener en cuenta filtros
      fila = l_row_no-row_id.
    ENDIF.
  ENDMETHOD.
  METHOD get_filas_sel.
    CLEAR: i_filas_sel,
           i_filas_sel2.
    o_grid->get_selected_rows( IMPORTING et_index_rows = i_filas_sel
                                         et_row_no     = i_filas_sel2 ).
  ENDMETHOD.
  METHOD get_lista_campos.
    FIELD-SYMBOLS <campo> TYPE dfies.

    CLEAR lista.
    DATA(i_campos) = zcl_ap_dev=>get_fieldcatalog_tabla( tabla ).

    DATA(r_campos_quitar) = zcl_ap_itab=>get_rango_campos( quitar_campos ).

    LOOP AT i_campos ASSIGNING <campo> WHERE NOT fieldname IN r_campos_quitar.
      zcl_ap_lista=>add( EXPORTING valor = <campo>-fieldname CHANGING lista = lista ).
    ENDLOOP.
  ENDMETHOD.
  METHOD get_registros_filtrados.
    FIELD-SYMBOLS <tabla> TYPE table.

    ASSIGN table_ref->* TO <tabla>.

    i_registros_filtrados = <tabla>.

    " Get excluded rows
    o_grid->get_filtered_entries( IMPORTING et_filtered_entries = DATA(lit_index) ).

    " Reverse order to keep correct indizes; thnx futu
    SORT lit_index DESCENDING.

    " Remove excluded rows from buffer
    LOOP AT lit_index ASSIGNING FIELD-SYMBOL(<index>).
      DELETE i_registros_filtrados INDEX <index>.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_tabla_filas_sel.
*  DATA l_tabla TYPE REF TO data.

    FIELD-SYMBOLS <tabla> TYPE any.

    CLEAR t_tabla_sel.
    get_filas_sel( ).
*  CREATE DATA l_tabla LIKE LINE OF t_tabla.

    LOOP AT i_filas_sel INTO fila WHERE rowtype IS INITIAL.
      ASSIGN t_tabla[ fila-index ] TO <tabla>.
      IF sy-subrc = 0.
        APPEND <tabla> TO t_tabla_sel.
      ENDIF.
    ENDLOOP.
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
  METHOD get_valor_from_desc.
    DATA: l_campos TYPE t_campos_desc,
          l_where  TYPE string.
    DATA l_descripcion TYPE string.

    READ TABLE i_campos_desc INTO l_campos WITH KEY campo = campo.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF NOT l_campos-dominio IS INITIAL.
      valor = zcl_ap_utils=>get_valor_dominio( dominio = l_campos-dominio texto = descripcion ).
    ELSEIF NOT l_campos-tabla IS INITIAL.
      l_descripcion = descripcion.
      IF NOT l_campos-add_clave IS INITIAL.
        SPLIT descripcion AT l_campos-add_clave INTO valor l_descripcion.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
      ENDIF.

      CONCATENATE l_campos-campo_desc ' = ''' l_descripcion '''' INTO l_where.
      IF NOT l_campos-where IS INITIAL.
        CONCATENATE l_where 'and' l_campos-where INTO l_where SEPARATED BY space.
      ENDIF.
      SELECT SINGLE (l_campos-campo_clave) FROM (l_campos-tabla)
        INTO valor
       WHERE (l_where).
    ELSEIF NOT l_campos-valores IS INITIAL.
      DATA(i_lista) = zcl_ap_lista=>get_valores_lista( lista = l_campos-valores ).
      ASSIGN i_lista[ subvalor = descripcion ] TO FIELD-SYMBOL(<lista>).
      IF sy-subrc = 0.
        valor = <lista>-valor.
*    ELSE.
*      valor = descripcion.
      ENDIF.
    ELSEIF NOT l_campos-caract IS INITIAL.
      valor = zcl_ap_clasificacion=>get_valor_caract_text2key( caract = l_Campos-caract valor = descripcion ).
    ENDIF.
  ENDMETHOD.
  METHOD importar_excel.
    " TODO: parameter FICHERO_SALIDA is never used (ABAP cleaner)
    " TODO: parameter CERRAR is never used (ABAP cleaner)
    " TODO: parameter AUTOFILTRO is never used (ABAP cleaner)

    DATA: l_fcat     TYPE lvc_s_fcat,
          t_fieldcat TYPE lvc_t_fcat,
          tabla      TYPE REF TO data,
          ep_line    TYPE REF TO data,
          ep_tabla   TYPE REF TO data,
          l_fichero  TYPE string.
    DATA l_min   TYPE i.
    DATA l_numc4 TYPE numc4.

    FIELD-SYMBOLS: <col3>          TYPE zap_alv_columnas3,
                   <l_table>       TYPE STANDARD TABLE,
                   <l_line>        TYPE any,
                   <tabla>         TYPE any,
                   <fcat>          TYPE lvc_s_fcat,
                   <valor_destino> TYPE any,
                   <valor_origen>  TYPE any.

    LOOP AT t_fcat INTO l_fcat WHERE no_out = ''.
      IF l_fcat-datatype = 'DATS'.
        l_fcat-datatype = 'CHAR'.
        l_fcat-inttype  = 'C'.
        l_fcat-intlen   = '000010'.
        l_fcat-domname  = 'CHAR10'.
        CLEAR l_fcat-ref_table.
      ELSEIF l_fcat-datatype = 'TIMS'.
        l_fcat-datatype = 'CHAR'.
        l_fcat-inttype  = 'C'.
        l_fcat-intlen   = '000008'.
        l_fcat-domname  = 'CHAR08'.
        CLEAR l_fcat-ref_table.
      ELSE.
        ASSIGN i_columnas3[ campo = l_fcat-fieldname ] TO <col3>.
        IF sy-subrc = 0.
          IF <col3>-aux3 = 'NUMC4'.
            l_fcat-datatype = 'CHAR'.
            l_fcat-inttype  = 'C'.
            l_fcat-intlen   = '000004'.
            l_fcat-domname  = 'NUMC4'.
            l_fcat-convexit = 'ALPHA'.
            CLEAR l_fcat-ref_table.
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND l_fcat TO t_fieldcat.
    ENDLOOP.

    cl_alv_table_create=>create_dynamic_table( EXPORTING  it_fieldcatalog           = t_fieldcat
                                               IMPORTING  ep_table                  = tabla
                                               EXCEPTIONS generate_subpool_dir_full = 1
                                                          OTHERS                    = 2 ).

    ASSIGN tabla->* TO <l_table>.
    CREATE DATA ep_line LIKE LINE OF <l_table>.
    ASSIGN ep_line->* TO <l_line>.

    CREATE DATA ep_tabla LIKE LINE OF t_tabla.
    ASSIGN ep_tabla->* TO <tabla>.

    l_fichero = zcl_ap_ficheros=>popup_select_fichero( file_filter = zcl_ap_ficheros=>c_filtro_xls ).
    IF NOT l_fichero IS INITIAL.
      zcl_ap_import_excel=>importar( EXPORTING fichero = l_fichero
                                     CHANGING  tabla   = <l_table> ).

      IF NOT <l_table> IS INITIAL.
        CLEAR t_tabla.
        LOOP AT <l_table> ASSIGNING <l_line>.
          LOOP AT t_fieldcat ASSIGNING <fcat>.
            ASSIGN COMPONENT <fcat>-fieldname OF STRUCTURE <tabla> TO <valor_destino>.
            ASSIGN COMPONENT <fcat>-fieldname OF STRUCTURE <l_line> TO <valor_origen>.
            <valor_destino> = <valor_origen>.

            READ TABLE t_fcat INTO l_fcat WITH KEY fieldname = <fcat>-fieldname.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            IF l_fcat-datatype = 'DATS'.
              <valor_destino> = zcl_ap_fechas=>string2fecha( <valor_origen> ).
            ELSEIF l_fcat-datatype = 'TIMS'.
              REPLACE ',' WITH '.' INTO <valor_origen>.
              l_min = 86400 * <valor_origen> ##NUMBER_OK.
              <valor_destino> = l_min.
            ELSEIF l_fcat-convexit = 'ALPHA' OR l_fcat-convexit = 'NUMCV'.
              zcl_ap_string=>poner_ceros( EXPORTING cadena = <valor_origen>
                                          CHANGING  salida = <valor_destino> ).
            ELSE.
              READ TABLE t_fieldcat INTO l_fcat WITH KEY fieldname = <fcat>-fieldname.
              IF sy-subrc = 0.
                IF l_fcat-domname = 'NUMC4'.
                  zcl_ap_string=>poner_ceros( EXPORTING cadena = <valor_origen>
                                              CHANGING  salida = l_numc4 ).
                  <valor_destino> = l_numc4.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
          INSERT <tabla> INTO TABLE t_tabla.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD ocultar_columnas_vacias.
    DATA: i_campos TYPE abap_component_tab,
          l_campos TYPE abap_componentdescr,
          l_valor  TYPE c LENGTH 1.

    FIELD-SYMBOLS: <linea> TYPE any,
                   <valor> TYPE any.

    i_campos = zcl_ap_dev=>get_fieldcatalog( t_tabla ).

    DELETE i_campos WHERE name IS INITIAL.
    DELETE i_campos WHERE name = 'CHECK' OR name = 'LIGHTS'.
    IF i_campos[] IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT i_campos INTO l_campos.
      CLEAR l_valor.
      LOOP AT t_tabla ASSIGNING <linea>.
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
  METHOD quitar_botones.
    me->layout-no_toolbar = no_toolbar.

    IF insercion = 'X' OR todos = 'X'.
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_insert_row ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_append_row ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_copy_row ).

      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_delete_row ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_cut ).

      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_copy ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_undo ).
    ENDIF.

    IF ordenacion = 'X' OR todos = 'X'.
      quitar_opciones( cl_gui_alv_grid=>mc_fc_sort ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_sort_asc ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_sort_dsc ).
    ENDIF.

    IF filtro = 'X' OR todos = 'X'.
      quitar_opciones( cl_gui_alv_grid=>mc_fc_filter ).
    ENDIF.

    IF operaciones = 'X' OR todos = 'X'.
      quitar_opciones( cl_gui_alv_grid=>mc_fc_subtot ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_sum ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_maximum ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_minimum ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_average ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_graph ).
    ENDIF.

    IF resto = 'X' OR todos = 'X'.
      quitar_opciones( cl_gui_alv_grid=>mc_fc_detail ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_check ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_find ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_refresh ).
    ENDIF.

    IF exportacion = 'X' OR todos = 'X'.
      quitar_opciones( cl_gui_alv_grid=>mc_fc_print ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_print_prev ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_print_back ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_info ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_views ).
      quitar_opciones( cl_gui_alv_grid=>mc_mb_export ).
    ENDIF.

    IF layout = 'X' OR todos = 'X'.
      quitar_opciones( cl_gui_alv_grid=>mc_fc_load_variant ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_save_variant ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_variant_admin ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_current_variant ).
    ENDIF.
  ENDMETHOD.
  METHOD quitar_botones_insercion.
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_insert_row ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_append_row ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_copy_row ).

    IF todos = 'X'.
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_delete_row ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
      quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_cut ).
    ENDIF.
  ENDMETHOD.
  METHOD quitar_opciones.
    DATA ls_exclude TYPE ui_func.

    ls_exclude = opcion. " cl_gui_alv_grid=>mc_fc_find.
    APPEND ls_exclude TO t_tool.
  ENDMETHOD.
  METHOD quitar_todos_botones_insercion.
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_insert_row ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_append_row ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_copy_row ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_delete_row ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_copy ).
    quitar_opciones( cl_gui_alv_grid=>mc_fc_loc_cut ).
  ENDMETHOD.
  METHOD refrescar_grid.
    DATA l_lvc_s_stbl TYPE lvc_s_stbl.

* Refrescamos el ALV y limpiamos la variable

    IF mantener_sel = 'X'.
      get_filas_sel( ).
    ENDIF.

    IF opt_cols = 'X'.
      o_grid->get_frontend_layout( IMPORTING es_layout = layout ).

      layout-cwidth_opt = 'X'.

* SE LANZA EL LAYOUT PARA QUE SE AJUSTE EL ANCHO
      o_grid->set_frontend_layout( is_layout = layout ).
    ENDIF.

    IF estable = 'X'.
      l_lvc_s_stbl = 'XX'.
    ENDIF.

    o_grid->refresh_table_display( is_stable = l_lvc_s_stbl i_soft_refresh = soft_refresh ).

    IF mantener_sel = 'X' AND NOT i_filas_sel IS INITIAL.
      o_grid->set_selected_rows( it_index_rows            = i_filas_sel
                                 is_keep_other_selections = '' ).
    ENDIF.

    IF new_code IS INITIAL.
      IF set_focus = 'X'.
        cl_gui_control=>set_focus( control = o_grid ).
      ENDIF.

      cl_gui_cfw=>flush( ).
    ELSE.
      cl_gui_cfw=>set_new_ok_code( new_code ).
    ENDIF.
  ENDMETHOD.
  METHOD registrar_enter.
    o_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
  ENDMETHOD.
  METHOD registrar_mod.
    o_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  ENDMETHOD.
  METHOD remove_button.
    CHECK button IS NOT INITIAL.
    IF NOT line_exists( allowed_buttons[ function = button ] ).
      DELETE excluded_buttons WHERE table_line = button.
      APPEND button TO excluded_buttons.
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
    CLEAR: i_botones,
           buttons.
  ENDMETHOD.
  METHOD set_campos_tabint.
* https://wiki.scn.sap.com/wiki/display/Snippets/ALV+fieldcatalog+-+create+for+ANY+table

    DATA:
      lr_table        TYPE REF TO data,
      lo_salv_table   TYPE REF TO cl_salv_table,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lo_aggregations TYPE REF TO cl_salv_aggregations,
      lr_data         TYPE REF TO data,
      lv_help_id      TYPE string.

    FIELD-SYMBOLS:
      <table>    TYPE STANDARD TABLE,
      <fieldcat> TYPE lvc_s_fcat,
      <rec>      TYPE any,
      <any>      TYPE any.

* create unprotected table from import table
* This is required because IMPORTed IT_TABLE can not be passed to CHANGING parameter t_table
    CREATE DATA lr_table LIKE tabla.
    ASSIGN lr_table->* TO <table>.
* New SALV Instance
    TRY.
        cl_salv_table=>factory( EXPORTING list_display = abap_false
                                IMPORTING r_salv_table = lo_salv_table
                                CHANGING  t_table      = <table> ).
      CATCH cx_salv_msg ##NO_HANDLER.
    ENDTRY.
* get columns object (basic field catalog data)
    lo_columns = lo_salv_table->get_columns( ).
* get aggregations object (Sorts&Sums)
    lo_aggregations = lo_salv_table->get_aggregations( ).
* use method to create field catalog from this information
    t_fcat =
      cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lo_columns
                                                         r_aggregations = lo_aggregations ).

    LOOP AT t_fcat ASSIGNING <fieldcat>.
      data(l_tabix) = sy-tabix.
      if <fieldcat>-col_pos is initial.
        <fieldcat>-col_pos = l_tabix.
      endif.

* gt_alv_out is the table to be displayed using i.e.FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      IF <rec> IS NOT ASSIGNED.
* just create a line dynamically that it works even for an empty table
        CREATE DATA lr_data LIKE LINE OF tabla.
        ASSIGN lr_data->* TO <rec>.
      ENDIF.
* get information about every field
      ASSIGN COMPONENT <fieldcat>-fieldname OF STRUCTURE <rec> TO <any>.
      DESCRIBE FIELD <any> HELP-ID lv_help_id.
* set reference table and fieldname - perfect F1 and F4!
* (wonder why SAP can't do it themselves)
      SPLIT lv_help_id AT '-'
            INTO <fieldcat>-ref_table <fieldcat>-ref_field.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_color.
    DATA l_column TYPE lvc_fname.

    IF campo IS INITIAL.
      l_column = campo_color.
    ELSE.
      l_column = campo.
    ENDIF.
    IF l_column IS INITIAL.
      RETURN.
    ENDIF.

    TRANSLATE l_column TO UPPER CASE.
    ASSIGN t_fcat[ fieldname = l_column ] TO FIELD-SYMBOL(<fcat>).
    IF sy-subrc = 0.
      <fcat>-emphasize(1)   = 'C'.
      <fcat>-emphasize+1(1) = color.
      <fcat>-emphasize+2(1) = '0'. " INT
      <fcat>-emphasize+3(1) = '0'. " INV
    ENDIF.
  ENDMETHOD.
  METHOD set_columnas.
    DATA l_col TYPE zap_alv_columnas.

    LOOP AT i_columnas INTO l_col.
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
      IF l_col-f4 = 'X'.
        set_field( campo = l_col-campo op = 'F4' ).
      ENDIF.
      IF l_col-cambia_decimales = 'X'.
        set_field( campo = l_col-campo op = 'DECIMALS' valor = l_col-decimales ).
      ENDIF.
*    IF l_col-ancho NE 0.
*      set_field( campo = l_col-campo op = 'ANCHO' valor = l_col-ancho ).
*    ENDIF.
    ENDLOOP.

    me->i_columnas = i_columnas.
  ENDMETHOD.
  METHOD set_columnas3.
    DATA l_col TYPE zap_alv_columnas3.

    i_columnas3 = i_columnas.

    LOOP AT i_columnas INTO l_col.
      set_field_text( campo = l_col-campo valor = l_col-descr ).
      IF l_col-noout = 'X'.
        set_field_noout( l_col-campo ).
      ELSE.
        set_field( campo = l_col-campo op = 'OUT' ).
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
      IF l_col-f4 = 'X'.
        set_field( campo = l_col-campo op = 'F4' ).
      ENDIF.
      IF l_col-borrar = 'X'.
*        set_field_noout( l_col-campo ).
        set_field( campo = l_col-campo op = 'QUITAR' ).
      ENDIF.
      IF l_col-ancho <> 0.
        set_field( campo = l_col-campo op = 'ANCHO' valor = l_col-ancho ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_dropdown.
    DATA l_drop TYPE lvc_s_drop.

    CLEAR l_drop.
    l_drop-handle = handle.
    l_drop-value  = valor.
    APPEND l_drop TO t_dropdown.
  ENDMETHOD.
  METHOD set_field.
    DATA: i_column TYPE TABLE OF lvc_fname,
          l_column TYPE lvc_fname,
          l_texto  TYPE scrtext_l, " s,
          l_texto2 TYPE scrtext_l,
          l_text   TYPE c LENGTH 100,
          l_texto3 TYPE scrtext_m,
          l_error  TYPE string.
    DATA l_hotspot TYPE zcl_ap_alv=>t_hotspot.

    FIELD-SYMBOLS: <fcat> TYPE lvc_s_fcat,
                   <fs>   TYPE any.

    SPLIT campo AT ',' INTO TABLE i_column.
    LOOP AT i_column INTO l_column.
      TRANSLATE l_column TO UPPER CASE.
      ASSIGN t_fcat[ fieldname = l_column ] TO <fcat>.
      IF sy-subrc = 0.
        CASE op.
          WHEN 'BORRAR'.
            DELETE t_fcat INDEX sy-tabix.
          WHEN 'EDIT' OR 'INPUT'. <fcat>-edit = 'X'.
          WHEN 'NOEDIT' OR 'NOINPUT'. <fcat>-edit = ''.
          WHEN 'OUT'. <fcat>-no_out = ''.
          WHEN 'NOOUT' OR 'NO_OUT'. <fcat>-no_out = 'X'.
          WHEN 'QUITAR'. <fcat>-tech = 'X'.
          WHEN 'SUM' OR 'DOSUM'. <fcat>-do_sum = 'X'.
          WHEN 'MERGE'. <fcat>-no_merging = ''.
          WHEN 'KEY'. <fcat>-key = 'X'.
          WHEN 'FIX'. <fcat>-fix_column = 'X'.
          WHEN 'LONG'. <fcat>-outputlen = valor.
          WHEN 'NO_CERO'. <fcat>-no_zero = 'X'.
          WHEN 'CHECKBOX'. <fcat>-checkbox = 'X'.
          WHEN 'ICON'. <fcat>-icon = 'X'.
          WHEN 'COLOR'. <fcat>-emphasize = valor.
          WHEN 'ANCHO'. <fcat>-dd_outlen = valor.
                        <fcat>-outputlen = valor.
                        <fcat>-col_opt   = ''.
          WHEN 'DECIMALS'. <fcat>-decimals_o = valor.
                           <fcat>-decimals   = valor.
          WHEN 'DRDN_FIELD'.
            <fcat>-drdn_field = campo.
          WHEN 'DROPDOWN'.
            <fcat>-drdn_hndl = valor.
          WHEN 'F4'.
            <fcat>-f4availabl = 'X'.
            CLEAR gs_f4.
            gs_f4-fieldname = l_column.
            gs_f4-register  = 'X'.
*      gs_f4-getbefore  = 'X'.
*      gs_f4-chngeafter = 'X'.
            INSERT gs_f4 INTO TABLE gt_f4.
          WHEN 'HOTSPOT'.
            <fcat>-hotspot = 'X'.
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
          WHEN 'BUTTON'.
            <fcat>-style = cl_gui_alv_grid=>mc_style_button.
          WHEN 'TEXTO'.
            l_texto = valor.
            IF valor2 IS INITIAL.
              l_texto2 = valor.
            ELSE.
              l_texto2 = valor2.
            ENDIF.

            IF valor IS INITIAL AND valor2 IS INITIAL.
              l_text = l_column.
              TRANSLATE l_text+1 TO LOWER CASE.
              l_text+1 = translate( val  = l_text+1
                                    from = `_`
                                    to   = ` ` ).
              l_texto  = l_text.
              l_texto2 = l_text.
              l_texto3 = l_text.
            ENDIF.
            IF l_texto3 IS INITIAL.
              l_texto3 = l_texto2.
            ENDIF.

            <fcat>-scrtext_m = l_texto3.
            <fcat>-scrtext_s = l_texto.
            <fcat>-coltext   = l_texto.
            <fcat>-scrtext_l = l_texto2.

          WHEN 'DDIC_REF'.
            <fcat>-ref_table = valor.
            <fcat>-ref_field = valor2.

          WHEN 'COL_AFTER'.
            READ TABLE t_fcat INTO DATA(l_fcat) WITH KEY fieldname = valor.
            IF sy-subrc = 0.
              <fcat>-col_pos = l_fcat-col_pos + 1.
              LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<fcat2>) WHERE col_pos > l_fcat-col_pos AND fieldname <> l_column.
                <fcat2>-col_pos = <fcat2>-col_pos + 1.
              ENDLOOP.
              SORT t_fcat BY row_pos
                             col_pos.
            ENDIF.
          WHEN OTHERS.
            ASSIGN COMPONENT op OF STRUCTURE <fcat> TO <fs>.
            IF sy-subrc = 0.
* DO_SUM A MAXIMO/B MINIMO/C MEDIA
              <fs> = valor.
            ELSE.
              MESSAGE e398(00) WITH 'Error en SET_FIELD OP='(sfo) op
                                    'no definida'(nod) ''.
            ENDIF.
        ENDCASE.
      ELSE.
        IF sy-sysid <> zcl_c=>entorno_produccion OR sy-uname = zcl_c=>usuario_ap.
          CONCATENATE 'Campo'(cam) l_column 'no existe'(noe) INTO l_error
                      SEPARATED BY space.
          MESSAGE l_error TYPE 'I'
                  DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_field_dropdown.
    TYPES: BEGIN OF t_reg,
             clave TYPE string,
             descr TYPE string,
           END OF t_reg.

    DATA: l_string     TYPE string,
          i_campos_sel TYPE TABLE OF string,
          l_reg        TYPE t_reg,
          i_lista      TYPE zap_lista_t,
          l_lista      TYPE zap_lista,
          l_campos     TYPE t_campos_desc.
    DATA l_cawnt TYPE cawnt.

    set_field( op = 'DROPDOWN' campo = campo valor = valor valor2 = valor2 ).

    IF NOT dominio IS INITIAL.
      SELECT ddtext FROM dd07t
        INTO l_string
       WHERE domname    = dominio
         AND ddlanguage = sy-langu.
        set_dropdown( handle = valor valor = l_string ).
      ENDSELECT.
    ENDIF.

    IF NOT tabla IS INITIAL AND NOT campo_desc IS INITIAL.
      IF NOT add_clave IS INITIAL.
        IF campo_clave IS INITIAL.
          APPEND |{ campo } as clave| TO i_campos_sel.
        ELSE.
          APPEND |{ campo_clave } as clave| TO i_campos_sel.
        ENDIF.
        APPEND |{ campo_desc } as descr| TO i_campos_sel.
        SELECT (i_campos_sel) FROM (tabla)
          INTO l_reg
         WHERE (where).
          CONCATENATE l_reg-clave l_reg-descr INTO l_string SEPARATED BY add_clave.
          set_dropdown( handle = valor valor = l_string ).
        ENDSELECT.
      ELSE.
        SELECT (campo_desc) FROM (tabla)
          INTO l_string
         WHERE (where).
          set_dropdown( handle = valor valor = l_string ).
        ENDSELECT.
      ENDIF.
    ENDIF.

    IF NOT valores IS INITIAL.
      i_lista = zcl_ap_lista=>get_valores_lista( lista = valores ).
      LOOP AT i_lista INTO l_lista.
        IF l_lista-subvalor IS INITIAL.
          set_dropdown( handle = valor valor = l_lista-valor ).
        ELSE.
          set_dropdown( handle = valor valor = l_lista-subvalor ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT caract IS INITIAL.
      DATA(i_cawn) = zcl_ap_clasificacion=>get_valores_caract( caract ).
      LOOP AT i_cawn INTO DATA(l_cawn).
        CLEAR l_cawnt.
        SELECT SINGLE atwtb FROM  cawnt
          INTO l_cawnt-atwtb
         WHERE atinn = l_cawn-atinn
           AND atzhl = l_cawn-atzhl
           AND spras = sy-langu
           AND adzhl = l_cawn-adzhl.
        IF sy-subrc <> 0.
          SELECT atwtb FROM  cawnt
            INTO l_cawnt-atwtb
            UP TO 1 ROWS
           WHERE atinn = l_cawn-atinn
             AND atzhl = l_cawn-atzhl
             AND adzhl = l_cawn-adzhl
           ORDER BY PRIMARY KEY.
          ENDSELECT.
        ENDIF.
        set_dropdown( handle = valor valor = l_cawnt-atwtb ).
      ENDLOOP.

    ENDIF.

    CLEAR l_campos.
    l_campos-campo       = campo.
    l_campos-dominio     = dominio.
    l_campos-tabla       = tabla.
    l_campos-caract      = caract.
    l_campos-where       = where.
    l_campos-campo_desc  = campo_desc.
    l_campos-campo_clave = campo_clave.
    l_campos-valores     = valores.
    l_campos-add_clave   = add_clave.
    APPEND l_campos TO i_campos_desc.
  ENDMETHOD.
  METHOD set_field_hotspot.
    DATA: i_campos TYPE TABLE OF string,
          l_campo  TYPE string.

    IF auto IS INITIAL.
      set_field( op = 'HOTSPOT' campo = campo valor = valor valor2 = valor2 ).
    ELSE.
      SPLIT campo AT ',' INTO TABLE i_campos.
      LOOP AT i_campos INTO l_campo.
        l_campo = to_upper( l_campo ).
        CASE l_campo.
          WHEN 'KUNNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'XD03' ).
          WHEN 'LIFNR' OR 'TDLNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'XK03' ).
          WHEN 'MATNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'MM03' ).
          WHEN 'VBELN'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VA03' ).
          WHEN 'VBELN_VA' OR 'KDAUF'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VA03' ).
          WHEN 'VBELN_VL'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VL03N' ).
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
          WHEN 'TBNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LB03' ).
          WHEN 'LQNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LS23' ).
          WHEN 'FKNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VI03' ).
          WHEN 'PLANS'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'PP01_S' ).
          WHEN 'ORGEH'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'PP01_O' ).
          WHEN 'CURSO'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'CURSO' ).
          WHEN 'CURSO_EDT'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'CURSO_EDT' ).
          WHEN 'DOCNUM'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'WEDI' ).
          WHEN 'ANLN1' OR 'ANLN1'.
            set_field( op = 'HOTSPOT' campo = l_campo valor = 'AS03' ).
          WHEN 'KNUMA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'VBO3' ).
          WHEN 'PS_PSP_PNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'CJ13' ).
          WHEN 'URL'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'URL' ).
          WHEN 'WI_ID' OR 'WORKITEMID'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'WI_ID' ).
          WHEN 'DOKNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'CV03N' ).
          WHEN 'LGTYP' OR 'LGPLA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LS03N' ).
          WHEN 'VLTYP' OR 'VLPLA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LS03N_V' ).
          WHEN 'NLTYP' OR 'NLPLA'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'LS03N_N' ).
          WHEN 'REINR' OR 'TRIPNO'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'PR05' ).
          WHEN 'PERNR'. set_field( op = 'HOTSPOT' campo = l_campo valor = 'PA20' ).
          WHEN 'OBSERVACIONES' OR 'TEXTO' OR 'COMENTARIOS'.
            set_field( op = 'HOTSPOT' campo = l_campo valor = 'TEXT' ).
          WHEN OTHERS.
            IF l_campo(3) = 'URL'.
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
  METHOD set_layout.
* Indicador de selección:
* http://help.sap.com/saphelp_erp2004/helpdata/en/ef/a2e9eff88311d2b48d006094192fe3/content.htm
    " A -> Fila y columna
    " B (y blanco) Simple
    " D -> Múltiple
    IF sel_mode IS SUPPLIED.
      IF sel_mode = 'N'.          " No queremos selección
        layout-no_rowmark = 'X'.
      ELSE.
        layout-sel_mode = sel_mode.
      ENDIF.
    ENDIF.

    IF zebra IS SUPPLIED.
      layout-zebra = zebra.
    ENDIF.

    IF input IS SUPPLIED.
      me->input = input.
    ENDIF.

    IF edit IS SUPPLIED.
      layout-edit = edit.
    ENDIF.

    IF input = 'X' OR edit = 'X'.
      registrar_enter( ).
      registrar_mod( ).
    ENDIF.

    IF lights IS SUPPLIED.
      layout-excp_fname = lights.
    ENDIF.

    IF color IS SUPPLIED.
      layout-info_fname = color.
    ENDIF.
    IF colort IS SUPPLIED.
      layout-ctab_fname = colort.
    ENDIF.

    IF ancho_optimizado IS SUPPLIED.
      opt_cols = ancho_optimizado.
      layout-cwidth_opt = ancho_optimizado.
    ENDIF.

    IF style IS SUPPLIED.
      layout-stylefname = style.
    ENDIF.

    IF no_merging IS SUPPLIED.
      layout-no_merging = no_merging.
    ENDIF.

    IF no_toolbar IS SUPPLIED.
      layout-no_toolbar = no_toolbar.
    ENDIF.

    IF no_rowmove IS SUPPLIED.
      layout-no_rowmove = no_rowmove.
    ENDIF.

    IF no_rowins IS SUPPLIED.
      layout-no_rowins = no_rowins.
    ENDIF.
  ENDMETHOD.
  METHOD set_marca_filas_sel.
    DATA: l_campo_check TYPE string,
          l_campo       TYPE c LENGTH 80.

    " TODO: variable is assigned but never used (ABAP cleaner)
    FIELD-SYMBOLS: <tabla> TYPE any,
                   <campo> TYPE any.

    IF NOT me->campo_check IS INITIAL.
      l_campo_check = campo_check.
    ELSE.
      l_campo_check = campo.
    ENDIF.

    IF validar_seleccion = 'X'.
      comprobar_cambios( ).
    ENDIF.

    CLEAR i_filas_sel.
    get_filas_sel( ).
    IF i_filas_sel IS INITIAL AND fila_activa = 'X'.
      fila-index = get_fila_activa( ).
      IF NOT fila-index IS INITIAL.
        APPEND fila TO i_filas_sel.
      ENDIF.
    ENDIF.

    LOOP AT t_tabla ASSIGNING <tabla>.
      CONCATENATE '<TABLA>-' l_campo_check INTO l_campo.
      ASSIGN (l_campo) TO <campo>.
      IF sy-subrc = 0.
        CLEAR <campo>.
      ENDIF.
    ENDLOOP.

    LOOP AT i_filas_sel INTO fila WHERE rowtype = ''.
      ASSIGN t_tabla[ fila-index ] TO <tabla>.
      IF sy-subrc = 0.
        ASSIGN (l_campo) TO <campo>.
        IF sy-subrc = 0.
          <campo> = 'X'.
          hay_sel = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF validar_seleccion = 'X' AND hay_sel IS INITIAL.
      MESSAGE TEXT-saf TYPE 'I'. "'Seleccione alguna fila'
    ENDIF.
  ENDMETHOD.
  METHOD set_orden.
    DATA: l_columname TYPE lvc_fname,
          i_columnas  TYPE TABLE OF lvc_fname,
          l_subtot    TYPE abap_bool,
          l_len       TYPE i,
          l_sort      TYPE lvc_s_sort.

    l_columname = campo.
    SPLIT campo AT ',' INTO TABLE i_columnas.
    LOOP AT i_columnas INTO l_columname.
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

      CLEAR l_sort.
      l_sort-spos      = lines( t_sort ).
      l_sort-spos      = l_sort-spos + 1.
      l_sort-fieldname = l_columname.
      l_sort-up        = up.
      l_sort-down      = down.
      l_sort-subtot    = l_subtot.
      l_sort-group     = group.
      APPEND l_sort TO t_sort.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_seleccion.
    DATA: l_campo_check TYPE string,
          l_row         TYPE lvc_s_row,
          l_campo       TYPE string,
          t_row         TYPE lvc_t_row.

    " TODO: variable is assigned but never used (ABAP cleaner)
    FIELD-SYMBOLS: <check> TYPE any,
                   <campo> TYPE c.

    IF NOT me->campo_check IS INITIAL.
      l_campo_check = campo_check.
    ELSE.
      l_campo_check = campo.
    ENDIF.

    IF NOT l_campo_check IS INITIAL.
      LOOP AT t_tabla ASSIGNING <check>.
        l_row-index = sy-tabix.
        CONCATENATE '<CHECK>-' l_campo_check INTO l_campo.
        ASSIGN (l_campo) TO <campo>.
        IF <campo> = 'X'.
          APPEND l_row TO t_row.
        ENDIF.
      ENDLOOP.
      o_grid->set_selected_rows( it_index_rows            = t_row
                                 is_keep_other_selections = '' ).
    ENDIF.
  ENDMETHOD.
  METHOD set_titulo.
    DATA l_title TYPE lvc_title.

    l_title = titulo.
    o_grid->set_gridtitle( i_gridtitle = l_title ).
  ENDMETHOD.
  METHOD set_valor_campo.
    DATA: l_col   TYPE zap_alv_columnas,
          l_campo TYPE string.

    FIELD-SYMBOLS: <fs>    TYPE any,
                   <color> TYPE lvc_t_scol,
                   <style> TYPE lvc_t_styl.

    l_col = get_col( colum = colum campo = campo ).

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
  ENDMETHOD.                    " GET_VALOR_CAMPO
  METHOD show.
    DATA: l_fecha_buffer     TYPE d,
          l_bypassing_buffer TYPE c LENGTH 1,
          l_buffer           TYPE c LENGTH 1.

    IF input = 'X'.
      o_grid->set_ready_for_input( i_ready_for_input = 1 ).
    ENDIF.

    IF NOT t_dropdown IS INITIAL.
      o_grid->set_drop_down_table( it_drop_down = t_dropdown ).
    ENDIF.

    IF NOT gt_f4 IS INITIAL.
      o_grid->register_f4_for_fields( it_f4 = gt_f4 ).
    ENDIF.

*  variant-report = sy-repid.
    variant-handle = handle.

    IF NOT it_fieldcatalog IS INITIAL.
      t_fcat = it_fieldcatalog.
    ELSEIF me->estructura IS INITIAL AND NOT internal_tabname IS INITIAL.
      get_catalogo_campos_tabint( EXPORTING include          = include
                                            programa         = programa
                                            internal_tabname = internal_tabname
                                  CHANGING  it_fieldcatalog  = t_fcat
                                            tabla            = tabla ).
      IF t_fcat IS INITIAL.
        MESSAGE 'No se ha podido identificar campos de tabla interna'(nti) TYPE 'E'.
      ENDIF.
    ELSEIF me->estructura IS INITIAL AND t_fcat IS INITIAL.
      set_campos_tabint( tabla ).
    ENDIF.

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

    IF NOT tabla IS INITIAL.
      table_ref = REF #( tabla ).
    ENDIF.

*The I_SAVE "Options for saving layouts" parameter can have the following values:
*
*U Only user specific layouts can be saved
*X Only global layouts can be saved
*A Both user specific and global layouts can be saved
*Space Layouts can not be saved
*S Si es superusuario puede grabar todo, sino, solo layouts de usuario.
    DATA(l_restriccion_layout) = restriccion_layout.
    IF restriccion_layout = 'S'.
      IF zcl_ap_autorizacion=>es_sapall( ).
        l_restriccion_layout = 'A'.
      ELSE.
        l_restriccion_layout = 'U'.
      ENDIF.
    ENDIF.

    o_grid->set_table_for_first_display( EXPORTING i_buffer_active      = l_buffer
                                                   i_bypassing_buffer   = l_bypassing_buffer
                                                   i_structure_name     = estructura
                                                   is_layout            = layout
                                                   i_save               = l_restriccion_layout
                                                   i_default            = default_layout
                                                   is_variant           = variant
                                                   it_toolbar_excluding = t_tool
                                         CHANGING  it_outtab            = tabla
                                                   it_sort              = t_sort
                                                   it_fieldcatalog      = t_fcat
                                                   it_filter            = i_filter ).

    cl_gui_control=>set_focus( control = o_grid ).
    cl_gui_cfw=>flush( ).

    inicio = 'X'.
  ENDMETHOD.
  METHOD show_button.
    DATA allowed TYPE t_allowed_but.

    CHECK button IS NOT INITIAL.
    IF NOT line_exists( allowed_buttons[ function = button ] ).
      allowed = button.
      APPEND allowed TO allowed_buttons.
      DELETE excluded_buttons WHERE table_line = button.
    ENDIF.
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
      WHEN 'A'.
        color = c_color_amarillo.
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
