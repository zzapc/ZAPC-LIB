class ZCL_AP_ALV_JERAR definition
  public
  create public .

public section.

  data O_ALV type ref to CL_SALV_HIERSEQ_TABLE .
  data O_FUNCTIONS type ref to CL_SALV_FUNCTIONS .
  data O_COLUMNS type ref to CL_SALV_COLUMNS_HIERSEQ .
  data O_COLUMN type ref to CL_SALV_COLUMN .
  data O_DSPSET type ref to CL_SALV_DISPLAY_SETTINGS .
  data O_LAYOUT type ref to CL_SALV_LAYOUT .
  data O_SELECTIONS type ref to CL_SALV_SELECTIONS .
  data O_EVENTS type ref to CL_SALV_EVENTS_HIERSEQ .
  data I_COLUMNS type SALV_T_COLUMN_REF .
  data O_CONTENT type ref to CL_SALV_FORM_ELEMENT .
  data I_ROWS type SALV_T_ROW .
  data CAMPO_CHECK type FIELDNAME .
  data O_FUNCTIONS_LIST type ref to CL_SALV_FUNCTIONS_LIST .
  data STATUS type STRING read-only .
  constants C_COLOR_ROJO type I value 6 ##NO_TEXT.
  constants C_COLOR_AMARILLO type I value 3 ##NO_TEXT.
  constants C_COLOR_AZUL type I value 1 ##NO_TEXT.
  constants C_COLOR_NARANJA type I value 7 ##NO_TEXT.
  constants C_COLOR_VERDE type I value 5 ##NO_TEXT.
  constants C_COLOR_GRIS type I value 8 ##NO_TEXT.
  constants C_COLOR_AMARILLO_OSC type I value 4 ##NO_TEXT.
  constants C_SEM_VERDE type NUM1 value 3 ##NO_TEXT.
  constants C_SEM_ROJO type NUM1 value 1 ##NO_TEXT.
  constants C_SEM_AMBAR type NUM1 value 2 ##NO_TEXT.
  data YA_MOSTRADO type ABAP_BOOL .
  data O_TOP_PAGE type ref to ZCL_AP_ALV_FORM .
  data I_COLUMNAS type ZAP_ALV_COLUMNAS_T .
  data O_COLUMNS_DET type ref to CL_SALV_COLUMNS_HIERSEQ .
  data I_COLUMNS_DET type SALV_T_COLUMN_REF .
  data I_COLUMNAS3 type ZAP_ALV_COLUMNAS3_T .

  methods CONSTRUCTOR
    importing
      !TABLA_CAB type STRING default 'I_CABECERA'
      !LIST_DISPLAY type ABAP_BOOL default ''
      !CPROG type SY-CPROG default SY-CPROG
      !OPTIMIZE type ABAP_BOOL default 'X'
      !BOTONES_STANDARD type ABAP_BOOL default 'X'
      !COLOR type LVC_FNAME default ' '
      !STATUS type SYPFKEY default ' '
      !SEL type C default ''
      !CAMPO_CHECK type FIELDNAME default ''
      !LIGHTS type STRING default ''
      !CONTAINER_NAME type C default ''
      !NO_LAYOUT type ABAP_BOOL default ''
      !R_CONTAINER type ref to CL_GUI_CONTAINER optional
      !RESTRICCION_LAYOUT type INT4 default IF_SALV_C_LAYOUT=>RESTRICT_NONE
      !INICIO_COLUMNA type ANY default ''
      !TABLA_DET type STRING default 'I_DETALLE'
      !CLAVE type STRING optional
      !CHECK_DETALLE type ABAP_BOOL default '' .
  methods SHOW .
  methods HANDLE_DOUBLE_CLICK
    for event IF_SALV_EVENTS_ACTIONS_HIERSEQ~DOUBLE_CLICK of CL_SALV_EVENTS_HIERSEQ
    importing
      !LEVEL
      !ROW
      !COLUMN .
  methods HANDLE_USER_COMMAND
    for event IF_SALV_EVENTS_FUNCTIONS~ADDED_FUNCTION of CL_SALV_EVENTS_HIERSEQ
    importing
      !E_SALV_FUNCTION .
  methods HANDLE_LINK_CLICK
    for event LINK_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
  methods SET_FIELD
    importing
      !OP type TEXT10
      !CAMPO type ANY
      !VALOR type ANY optional
      !VALOR2 type ANY optional
      !DET type ABAP_BOOL default '' .
  methods HANDLE_TOP_OF_PAGE
    for event TOP_OF_PAGE of CL_SALV_EVENTS_TABLE
    importing
      !R_TOP_OF_PAGE
      !PAGE
      !TABLE_INDEX .
  methods HANDLE_END_OF_PAGE
    for event END_OF_PAGE of CL_SALV_EVENTS_TABLE
    importing
      !R_END_OF_PAGE
      !PAGE .
  methods TOP_OF_PAGE .
  methods END_OF_PAGE .
  methods SHOW_POPUP
    importing
      !START_COLUMN type I default 1
      !END_COLUMN type I default 100
      !START_LINE type I default 1
      !END_LINE type I default 20
      !SEL type ABAP_BOOL default 'X' .
  methods SET_COLOR
    importing
      !CAMPO type STRING optional
      !COLOR type I .
  methods SET_STATUS
    importing
      !STATUS type SYPFKEY
      !REPID type SY-REPID default SY-REPID .
  methods GET_SELECCION
    changing
      !T_TABLA type TABLE optional .
  methods SET_ORDEN
    importing
      !CAMPO type STRING optional
      !UP type CHAR1 default 'X'
      !DOWN type CHAR1 default ''
      !GROUP type SALV_DE_SORT_GROUP default IF_SALV_C_SORT=>GROUP_NONE
      !SUBTOT type CHAR1 default ''
    preferred parameter CAMPO .
  methods SET_AGREGACION
    importing
      !CAMPO type ANY .
  methods REFRESH
    importing
      !S_STABLE type LVC_S_STBL optional
      !REFRESH_MODE type SALV_DE_CONSTANT default IF_SALV_C_REFRESH=>SOFT .
  methods SET_TOP_OF_PAGE .
  methods SET_END_OF_PAGE .
  methods GET_F4_LAYOUT
    returning
      value(LAYOUT) type DISVARIANT-VARIANT .
  methods SET_LAYOUT
    importing
      !LAYOUT type DISVARIANT-VARIANT .
  methods GET_FILA_ACTIVA
    returning
      value(FILA) type INT4 .
  methods CONSTRUCTOR_TABLA
    importing
      !LIST_DISPLAY type ABAP_BOOL default ''
      !OPTIMIZE type ABAP_BOOL default 'X'
      !BOTONES_STANDARD type ABAP_BOOL default 'X'
      !COLOR type LVC_FNAME default ' '
      !STATUS type SYPFKEY default ' '
      !SEL type C default ''
      !CAMPO_CHECK type FIELDNAME default ''
      !LIGHTS type STRING default ''
      !CONTAINER_NAME type C default ''
      !TABLA_CAB type STRING default ''
      !NO_LAYOUT type ABAP_BOOL default ''
      !RESTRICCION_LAYOUT type INT4 default IF_SALV_C_LAYOUT=>RESTRICT_NONE
      !TABLA_DET type STRING default ''
    changing
      !T_TABLA_DET type TABLE
      !T_TABLA_CAB type TABLE .
  methods SET_SELECCION
    changing
      !T_TABLA type TABLE optional .
  methods GET_COLUMNA_ACTIVA
    returning
      value(COLUMNA) type SALV_T_COLUMN .
  methods GET_NOMBRE_COLUMNA_ACTIVA
    returning
      value(COL) type STRING .
  methods GET_CELDA_ACTIVA
    returning
      value(CELDA) type SALV_T_CELL .
  methods FREE .
  class-methods APPEND_COLOR
    importing
      !CAMPO type ANY default ''
      !COLOR type INT4 optional
      !COLORC type ANY optional
      !INT type INT1 default 0
      !INV type INT4 default 0
    changing
      !TABLA_COLOR type LVC_T_SCOL .
  class-methods TRADUCE_COLOR
    importing
      !CODIGO type ANY
    returning
      value(COLOR) type INT4 .
  methods GET_DATOS_CELDA_ACTIVA
    returning
      value(CELDA) type SALV_S_CELL .
  methods SET_FIELD_INPUT
    importing
      !CAMPO type ANY optional
    preferred parameter CAMPO .
  methods SET_FIELD_NOOUT
    importing
      !CAMPO type ANY optional
      !DET type ABAP_BOOL default ''
    preferred parameter CAMPO .
  methods SET_FIELD_TEXT
    importing
      !CAMPO type ANY optional
      !VALOR type ANY optional
      !VALOR2 type ANY optional
    preferred parameter CAMPO .
  methods EXPORTAR_EXCEL
    importing
      !FICHERO_SALIDA type STRING optional
      !CERRAR type ABAP_BOOL default ''
      !COLUMNA_INICIAL type I default 1
      !COL_TEXTO type STRING default ''
      !COL_IMPORTES type STRING default ''
      !REPLACEMENT type ANY default 46
      !RIGHTFOOTER type STRING optional
      !CENTERHEADER type STRING optional
      !PREVIEW type ABAP_BOOL optional
    changing
      !T_TABLA type TABLE optional .
  methods SET_FIELD_DECIMALES
    importing
      !CAMPO type ANY optional
      !VALOR type ANY default 0
    preferred parameter CAMPO .
  methods GET_DEFAULT_LAYOUT
    returning
      value(LAYOUT) type DISVARIANT-VARIANT .
  methods EXPORTAR_CSV
    importing
      !FICHERO_SALIDA type STRING optional
      !SERVIDOR type ABAP_BOOL default ''
      !DIALOGO_FICHERO type ABAP_BOOL default ''
      !SEPARADOR type C default ';'
      !MOSTRAR_MENSAJES type ABAP_BOOL default 'X'
    exporting
      !I_CSV type TABLE_OF_STRINGS
    changing
      !T_TABLA type TABLE optional .
  methods GET_CSV_AS_STRING
    exporting
      !STRING_CSV type STRING
    changing
      !T_TABLA type TABLE optional .
  methods SET_COLUMNAS
    importing
      !I_COLUMNAS type ZAP_ALV_COLUMNAS_T optional
      !CAMPO type ANY optional
    preferred parameter I_COLUMNAS .
  methods SET_COLUMNAS3
    importing
      !I_COLUMNAS type ZAP_ALV_COLUMNAS3_T optional
      !CAMPO type ANY optional
    preferred parameter I_COLUMNAS .
  methods CORREGUIR_VARIANTES
    importing
      !I_COLUMNAS3 type ZAP_ALV_COLUMNAS3_T optional
      !I_COLUMNAS type ZAP_ALV_COLUMNAS_T optional .
  methods FILL_COLUMNAS
    importing
      !CAMPO type ANY
    returning
      value(I_COLUMNAS) type ZAP_ALV_COLUMNAS_T .
  methods SET_FILTER
    importing
      !CAMPO type ANY
      !VALOR type ANY
      !VALOR_HASTA type ANY optional
      !SIGN type ANY default 'I'
      !OPTION type ANY default 'EQ' .
  class-methods SHOW_POPUP_ST
    changing
      !T_TABLA type TABLE .
  methods FILL_COLUMNAS3
    importing
      !CAMPO type ANY
    returning
      value(I_COLUMNAS) type ZAP_ALV_COLUMNAS3_T .
protected section.

  data NOMBRE_TABLA type STRING .
private section.

  data O_SORTS type ref to CL_SALV_SORTS .
  data O_AGGREGATION type ref to CL_SALV_AGGREGATIONS .
  data O_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data KEY_LAYOUT type SALV_S_LAYOUT_KEY .
  data CAMPO_COLOR type STRING .

  methods INICIALIZA_OBJETOS
    importing
      !LIST_DISPLAY type ABAP_BOOL default ''
      !OPTIMIZE type ABAP_BOOL default 'X'
      !BOTONES_STANDARD type ABAP_BOOL default 'X'
      !COLOR type LVC_FNAME default ' '
      !STATUS type SYPFKEY default ' '
      !SEL type C default ''
      !CAMPO_CHECK type FIELDNAME default ''
      !LIGHTS type STRING default ''
      !TABLA_CAB type STRING default 'I_LISTADO'
      !NO_LAYOUT type ABAP_BOOL default ''
      !RESTRICCION_LAYOUT type INT4 default IF_SALV_C_LAYOUT=>RESTRICT_NONE
      !INICIO_COLUMNA type ANY default ''
      !TABLA_DET type STRING default 'I_LISTADO' .
endclass. "ZCL_AP_ALV_JERAR definition
class ZCL_AP_ALV_JERAR implementation.
method APPEND_COLOR.
  DATA: ls_color TYPE lvc_s_scol,
        l_column TYPE lvc_fname,
        i_column TYPE TABLE OF lvc_fname.

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

endmethod.
METHOD constructor.
  DATA: l_tabla_cab TYPE string,
        l_tabla_det TYPE string,
        l_error     TYPE string,
        l_string    TYPE string,
        i_claves    TYPE TABLE OF string,
        l_clave     TYPE string.

  DATA: lt_bind TYPE salv_t_hierseq_binding,
        la_bind LIKE LINE OF lt_bind.
  FIELD-SYMBOLS: <tabla_cab> TYPE table,
                 <tabla_det> TYPE table.

* Si no se informa la tabla, debemos utilizar el falso constructor diferido
  CHECK NOT tabla_cab IS INITIAL.

  IF NOT cprog IS INITIAL.
    CONCATENATE '(' cprog ')' tabla_cab INTO l_tabla_cab.
  ELSE.
    l_tabla_cab = tabla_cab.
  ENDIF.

  IF NOT cprog IS INITIAL.
    CONCATENATE '(' cprog ')' tabla_det INTO l_tabla_det.
  ELSE.
    l_tabla_det = tabla_det.
  ENDIF.


  CONCATENATE l_tabla_cab '[]' INTO l_tabla_cab.
  me->nombre_tabla = l_tabla_cab.
  ASSIGN (l_tabla_cab) TO <tabla_cab>.

  IF sy-subrc NE 0.
    CONCATENATE 'La tabla'(tab) l_tabla_cab 'no existe'(noe)
           INTO l_error SEPARATED BY space.
    MESSAGE l_error TYPE 'I'
            DISPLAY LIKE 'E'.
  ENDIF.

  CONCATENATE l_tabla_det '[]' INTO l_tabla_det.
  IF check_detalle = 'X'.
    me->nombre_tabla = l_tabla_det.
  ENDIF.
  ASSIGN (l_tabla_det) TO <tabla_det>.

  IF sy-subrc NE 0.
    CONCATENATE 'La tabla'(tab) l_tabla_det 'no existe'(noe)
           INTO l_error SEPARATED BY space.
    MESSAGE l_error TYPE 'I'
            DISPLAY LIKE 'E'.
  ENDIF.

  SPLIT clave AT ',' INTO TABLE i_claves.
  LOOP AT i_claves INTO l_clave.
    la_bind-master = l_clave.
    la_bind-slave  = l_clave.
    APPEND la_bind TO lt_bind.
  ENDLOOP.

  TRY.
      cl_salv_hierseq_table=>factory(
        EXPORTING t_binding_level1_level2 = lt_bind
        IMPORTING r_hierseq = o_alv
        CHANGING t_table_level1  = <tabla_cab>
                 t_table_level2  = <tabla_det>
           ).


    CATCH cx_salv_data_error .
      MESSAGE 'ALV display not possible'(anp) TYPE 'E' DISPLAY LIKE 'I'.
    CATCH cx_salv_not_found .
      MESSAGE 'ALV display not possible'(anp) TYPE 'E' DISPLAY LIKE 'I'.
  ENDTRY.

  TRY.

      inicializa_objetos( list_display     = list_display
                          optimize         = optimize
                          botones_standard = botones_standard
                          color            = color
                          status           = status
                          sel              = sel
                          campo_check      = campo_check
                          lights           = lights
                          tabla_cab        = tabla_cab
                          tabla_det         = tabla_det
                          no_layout        = no_layout
                          restriccion_layout = restriccion_layout
                          inicio_columna   = inicio_columna ).

    CATCH cx_salv_msg.
      MESSAGE 'ALV display not possible'(anp) TYPE 'E' DISPLAY LIKE 'I'.
  ENDTRY.
ENDMETHOD.
METHOD constructor_tabla.
  DATA: l_error  TYPE string,
        l_string TYPE string.
  DATA: lt_bind TYPE salv_t_hierseq_binding,
        la_bind LIKE LINE OF lt_bind.

  TRY.
      TRY.
          cl_salv_hierseq_table=>factory(
            EXPORTING t_binding_level1_level2 = lt_bind
            IMPORTING r_hierseq = o_alv
            CHANGING t_table_level1  = t_tabla_cab
                     t_table_level2  = t_tabla_det
               ).
        CATCH cx_salv_data_error .
          MESSAGE 'ALV display not possible'(anp) TYPE 'I' DISPLAY LIKE 'E'.
        CATCH cx_salv_not_found .
          MESSAGE 'ALV display not possible'(anp) TYPE 'I' DISPLAY LIKE 'E'.
      ENDTRY.

      inicializa_objetos( list_display     = list_display
                          optimize         = optimize
                          botones_standard = botones_standard
                          color            = color
                          status           = status
                          sel              = sel
                          campo_check      = campo_check
                          lights           = lights
                          tabla_cab        = tabla_cab
                          tabla_det        = tabla_det
                          no_layout        = no_layout
                          restriccion_layout = restriccion_layout ).

    CATCH cx_salv_msg.
      MESSAGE 'ALV display not possible'(anp) TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMETHOD.
METHOD correguir_variantes.
  DATA: i_ltdx        TYPE TABLE OF ltdx,
        rs_varkey     TYPE ltdxkey,
        rt_dbfieldcat TYPE TABLE OF ltdxdata.

  FIELD-SYMBOLS: <ltdx>  TYPE ltdx,
                 <campo> TYPE ltdxdata,
                 <col>   TYPE zap_alv_columnas,
                 <col3>  TYPE zap_alv_columnas3.

  SELECT relid report handle log_group username variant type FROM ltdx
    INTO CORRESPONDING FIELDS OF TABLE i_ltdx
   WHERE relid = 'LT'
     AND report = key_layout-report
     AND type   = 'F'.

  SORT i_ltdx.
  DELETE ADJACENT DUPLICATES FROM i_ltdx.

  LOOP AT i_ltdx ASSIGNING <ltdx>.
    MOVE-CORRESPONDING <ltdx> TO rs_varkey.

    IMPORT lt_dbfieldcat TO rt_dbfieldcat
                         FROM  DATABASE ltdx(lt)
                         CLIENT sy-mandt
                         ID rs_varkey.

    LOOP AT i_columnas ASSIGNING <col> WHERE no_variante = 'X'.
      LOOP AT rt_dbfieldcat ASSIGNING <campo> WHERE key1 = <col>-campo
                                                AND param = 'NO_OUT'.
        <campo>-value = <col>-noout.
      ENDLOOP.
    ENDLOOP.

    LOOP AT i_columnas3 ASSIGNING <col3> WHERE no_variante = 'X'.
      LOOP AT rt_dbfieldcat ASSIGNING <campo> WHERE key1 = <col3>-campo
                                                AND param = 'NO_OUT'.
        <campo>-value = <col3>-noout.
      ENDLOOP.
    ENDLOOP.

    EXPORT lt_dbfieldcat FROM rt_dbfieldcat
                         TO DATABASE ltdx(lt)
                         ID rs_varkey.
  ENDLOOP.

ENDMETHOD.
method END_OF_PAGE.

  clear o_content.
*  DATA: lr_header TYPE REF TO cl_salv_form_header_info,
*        l_text    TYPE string.
*  l_text = 'end of page for the report'.
*  CREATE OBJECT lr_header
*    EXPORTING
*      text    = l_text
*      tooltip = l_text.
*  o_content = lr_header.
endmethod.
METHOD exportar_csv.
  TYPES: BEGIN OF t_cols,
           orden(3) TYPE n,
           name     TYPE fieldname,
           titulo   TYPE string,
           tipo     TYPE abap_typekind,
         END OF t_cols.

  DATA: i_columns    TYPE salv_t_column_ref,
        l_column     TYPE salv_s_column_ref,
        i_campos     TYPE abap_component_tab,
        l_campos     TYPE abap_componentdescr,
        i_cols       TYPE TABLE OF t_cols,
        l_col        TYPE t_cols,
        l_orden      TYPE i,
        l_string     TYPE string,
        l_campo(40),
        l_valor(255),
        l_neg,
        lr_data      TYPE REF TO data.

  FIELD-SYMBOLS: <tabla> TYPE table,
                 <campo> TYPE any,
                 <linea> TYPE any.

  DEFINE add_campo.
    if l_col-tipo = 'C'.
      replace all occurrences of separador in &1 with ':'.
    endif.
    write &1 to l_valor left-justified.
    if l_col-tipo = 'P'.
      if l_valor cs '-'.
        replace all occurrences of '-' in l_valor with ''.
        concatenate '-' l_valor into l_valor.
      endif.
    endif.
    if l_col-orden = 1.
      l_string = l_valor.
    else.
      concatenate l_string separador l_valor into l_string.
    endif.
  END-OF-DEFINITION.

  i_columns = o_columns->get( ).
  IF me->nombre_tabla IS INITIAL.
    DATA: i_ddfiels TYPE ddfields,
          l_dfies   TYPE dfies.

    i_ddfiels = zcl_ap_dev=>get_fieldcatalog_ddic( t_tabla ).
    IF NOT i_ddfiels IS INITIAL.
      LOOP AT i_ddfiels INTO l_dfies.
        l_campos-name = l_dfies-fieldname.
        l_campos-suffix = l_dfies-inttype.
        APPEND l_campos TO i_campos.
      ENDLOOP.
    ELSE.
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
      CATCH cx_root.
    ENDTRY.
    IF o_column->is_visible( ) = 'X'.
      ADD 1 TO l_orden.
      CLEAR l_col.
      l_col-orden = l_orden.
      l_col-name = l_column-columnname.
      l_col-titulo = o_column->get_long_text( ).
      READ TABLE i_campos INTO l_campos WITH KEY name = l_column-columnname.
      IF sy-subrc = 0.
        IF NOT l_campos-type IS INITIAL.
          l_col-tipo = l_campos-type->type_kind.
        ELSE.
          l_col-tipo = l_campos-suffix.
        ENDIF.
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
      zcl_ap_ficheros=>grabar( EXPORTING fichero = fichero_salida
                                         dialogo = dialogo_fichero
                                         mostrar_error = mostrar_mensajes
                            CHANGING  tabla   = i_csv ).
    ELSE.
      zcl_ap_ficheros=>graba_fich_servidor( EXPORTING fichero = fichero_salida
                                                      mostrar_mensajes = mostrar_mensajes
                                            CHANGING  tabla   = i_csv ).
    ENDIF.
  ENDIF.


ENDMETHOD.
METHOD exportar_excel.
  DATA: o_excel   TYPE REF TO zcl_ap_excel_ole,
        i_titulos TYPE TABLE OF scrtext_l,
        l_titulo  TYPE scrtext_l,
        i_columns TYPE salv_t_column_ref,
        l_column  TYPE salv_s_column_ref,
        i_campos  TYPE abap_component_tab,
        l_campos  TYPE abap_componentdescr.

  FIELD-SYMBOLS: <tabla> TYPE table.

  CREATE OBJECT o_excel
    EXPORTING
      visible = 0.

  o_excel->crear_documento( ).

  i_columns = o_columns->get( ).
  IF me->nombre_tabla IS INITIAL.
    i_campos = zcl_ap_dev=>get_fieldcatalog( t_tabla ).
  ELSE.
    ASSIGN (me->nombre_tabla) TO <tabla>.
    i_campos = zcl_ap_dev=>get_fieldcatalog( <tabla> ).
  ENDIF.

  DELETE i_campos WHERE name IS INITIAL.
  LOOP AT i_campos INTO l_campos.
    READ TABLE i_columns INTO l_column WITH KEY columnname = l_campos-name.
    IF sy-subrc = 0.
      TRY.
          o_column = o_columns->get_column( l_column-columnname ).
        CATCH cx_root.
      ENDTRY.
      IF o_column->is_visible( ) = 'X'.
        l_titulo = o_column->get_long_text( ).
        IF l_titulo IS INITIAL.
          l_titulo = l_column-columnname.
        ENDIF.
      ELSE.
        l_titulo = '#INVISIBLE#'.
      ENDIF.
    ELSE.
      l_titulo = l_campos-name.
    ENDIF.
    APPEND l_titulo TO i_titulos.
  ENDLOOP.

  IF NOT me->nombre_tabla IS INITIAL.
    ASSIGN (me->nombre_tabla) TO <tabla>.
    o_excel->pegar_tabla( cabecera = 'X' i_tabla = <tabla> autofiltro = 'X' i_titulos = i_titulos
                          columna_inicial = columna_inicial
                          col_texto = col_texto col_importes = col_importes
                          replacement = replacement ).
  ELSE.
    o_excel->pegar_tabla( cabecera = 'X' i_tabla = t_tabla autofiltro = 'X' i_titulos = i_titulos
                          columna_inicial = columna_inicial
                          col_texto = col_texto col_importes = col_importes
                          replacement = replacement ).
  ENDIF.

  IF NOT fichero_salida IS INITIAL.
    o_excel->grabar_documento( fichero = fichero_salida
                               cerrar  = cerrar ).
  ENDIF.
  IF cerrar IS INITIAL.
    o_excel->fijar_visible( ).
  ENDIF.

  IF rightfooter NE '' OR centerheader NE '' OR preview NE ''.
    o_excel->configurar_pagina( rightfooter = rightfooter centerheader = centerheader preview = preview ).
  ENDIF.

  o_excel->free( ).

ENDMETHOD.
method FILL_COLUMNAS.
  DATA: i_columns TYPE salv_t_column_ref,
        l_column TYPE salv_s_column_ref,
        l_long TYPE i,
        l_col TYPE zap_alv_columnas,
        l_aux(40).

  l_long = STRLEN( campo ).

  i_columns = o_columns->get( ).

  LOOP AT i_columns INTO l_column.
    l_aux = l_column-columnname(l_long).
    IF l_aux = campo.
      IF l_column-columnname+l_long(2) CO '0123456789'.
        CLEAR l_col.
        l_col-campo  = l_column-columnname.
        l_col-noout  = 'X'.
        l_col-colum  = l_column-columnname+l_long(2).
        L_COL-DESCR  = l_column-columnname.
        APPEND l_col TO i_columnas.
      ENDIF.
    ENDIF.
  ENDLOOP.

endmethod.
method FILL_COLUMNAS3.
  DATA: i_columns TYPE salv_t_column_ref,
        l_column TYPE salv_s_column_ref,
        l_long TYPE i,
        l_col TYPE zap_alv_columnas3,
        l_aux(40).

  l_long = STRLEN( campo ).

  i_columns = o_columns->get( ).

  LOOP AT i_columns INTO l_column.
    l_aux = l_column-columnname(l_long).
    IF l_aux = campo.
      IF l_column-columnname+l_long(2) CO '0123456789'.
        CLEAR l_col.
        l_col-campo  = l_column-columnname.
        l_col-noout  = 'X'.
        l_col-colum  = l_column-columnname+l_long(2).
        L_COL-DESCR  = l_column-columnname.
        APPEND l_col TO i_columnas.
      ENDIF.
    ENDIF.
  ENDLOOP.

endmethod.
method FREE.

  cl_gui_cfw=>flush( ).
  IF NOT o_container IS INITIAL.
    o_container->free( ).
    CLEAR o_container.
    cl_gui_cfw=>flush( ).
  ENDIF.


endmethod.
method GET_CELDA_ACTIVA.

  clear celda.
  celda = o_selections->get_selected_cells( ).

endmethod.
method GET_COLUMNA_ACTIVA.

  clear columna.
  columna = o_selections->GET_SELECTED_COLUMNS( ).

endmethod.
method GET_CSV_AS_STRING.
  DATA: i_csv TYPE table_of_strings,
        l_string TYPE string.

  exportar_csv( EXPORTING separador = cl_abap_char_utilities=>horizontal_tab
                IMPORTING i_csv = i_csv
                CHANGING t_tabla = t_tabla ).

  CLEAR string_csv.
  LOOP AT i_csv INTO l_string.
    IF string_csv IS INITIAL.
      string_csv = l_string.
    ELSE.
      CONCATENATE string_csv cl_abap_char_utilities=>cr_lf l_string INTO string_csv.
    ENDIF.
  ENDLOOP.

endmethod.
method GET_DATOS_CELDA_ACTIVA.
  DATA i_celdas_sel TYPE salv_t_cell.

  CLEAR celda.
  i_celdas_sel = get_celda_activa( ).
  READ TABLE i_celdas_sel INTO celda INDEX 1.

endmethod.
method GET_DEFAULT_LAYOUT.
  DATA: ls_layout TYPE salv_s_layout_info.

  ls_layout = cl_salv_layout_service=>get_default_layout(
                           s_key    = key_layout
                           restrict = cl_salv_layout=>restrict_none ).

  layout = ls_layout-layout.

endmethod.
method GET_F4_LAYOUT.
  DATA: ls_layout TYPE salv_s_layout_info.

  ls_layout = cl_salv_layout_service=>f4_layouts(
                           s_key    = key_layout
                           restrict = cl_salv_layout=>restrict_none ).

  layout = ls_layout-layout.

endmethod.
method GET_FILA_ACTIVA.

  clear: fila, i_rows.
  i_rows = o_selections->get_selected_rows( ).
  READ TABLE i_rows INTO fila INDEX 1.

endmethod.
method GET_NOMBRE_COLUMNA_ACTIVA.
  DATA i_columnas TYPE salv_t_column.

  i_columnas = get_columna_activa( ).
  READ TABLE i_columnas INDEX 1 into col.

endmethod.
METHOD get_seleccion.
  DATA: l_campo TYPE string,
        l_row TYPE int4.

  FIELD-SYMBOLS: <tabla> TYPE table,
                 <check> TYPE ANY,
                 <campo> TYPE c.

  CLEAR i_rows.
  IF NOT o_selections IS INITIAL.
*    o_alv->get_metadata( ).
    i_rows = o_selections->get_selected_rows( ).

    IF NOT campo_check IS INITIAL.
      IF t_tabla IS INITIAL.
        ASSIGN (nombre_tabla) TO <tabla>.
        IF sy-subrc = 0.
          LOOP AT <tabla> ASSIGNING <check>.
            CONCATENATE '<CHECK>-' campo_check INTO l_campo.
            ASSIGN (l_campo) TO <campo>.
            READ TABLE i_rows FROM sy-tabix TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              <campo> = 'X'.
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
          ELSE.
            CLEAR <campo>.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDIF.


ENDMETHOD.
method HANDLE_DOUBLE_CLICK.

*  BREAK-POINT.
*    FIELD-SYMBOLS <scarr> TYPE scarr.
*    READ TABLE scarr_tab INDEX row ASSIGNING <scarr>.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*    IF column = 'CARRNAME'.
*      demo=>detail( <scarr>-carrid ).
*    ELSEIF column = 'URL'.
*      demo=>browser( <scarr>-url ).
*    ENDIF.

endmethod.
method HANDLE_END_OF_PAGE.
endmethod.
METHOD handle_link_click.

**  BREAK-POINT.
*    FIELD-SYMBOLS <scarr> TYPE scarr.
*    READ TABLE scarr_tab INDEX row ASSIGNING <scarr>.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*    IF column = 'CARRNAME'.
*      demo=>detail( <scarr>-carrid ).
*    ELSEIF column = 'URL'.
*      demo=>browser( <scarr>-url ).
*    ENDIF.

ENDMETHOD.
method HANDLE_TOP_OF_PAGE.

endmethod.
method HANDLE_USER_COMMAND.

*  BREAK-POINT.

    GET_SELECCION( ).


*    FIELD-SYMBOLS <scarr> TYPE scarr.
*    READ TABLE scarr_tab INDEX row ASSIGNING <scarr>.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*    IF column = 'CARRNAME'.
*      demo=>detail( <scarr>-carrid ).
*    ELSEIF column = 'URL'.
*      demo=>browser( <scarr>-url ).
*    ENDIF.

endmethod.
METHOD inicializa_objetos.
  DATA: l_error  TYPE string,
        l_string TYPE string.

  o_dspset = o_alv->get_display_settings( ).
  o_dspset->set_striped_pattern( cl_salv_display_settings=>true ).

  o_layout = o_alv->get_layout( ).


  key_layout-report = sy-cprog.
  IF tabla_cab IS INITIAL.
    key_layout-handle = 'NONE'.
  ELSE.
    key_layout-handle = tabla_cab.
  ENDIF.

  IF no_layout = ''.
    o_layout->set_key( key_layout ).
    o_layout->set_default( 'X' ).
    o_layout->set_save_restriction( restriccion_layout ).
  ENDIF.

  me->status = status.

* activate ALV generic Functions
  IF botones_standard = 'X'.
    o_functions = o_alv->get_functions( ).
    o_functions->set_all( ).

    o_functions_list = o_alv->get_functions( ).

*    o_alv->SET_SCREEN_STATUS( PFSTATUS = 'DISPLAY'
*                                 REPORT = SY-REPID
*                                 SET_FUNCTIONS = o_alv->C_FUNCTIONS_ALL ).
  ENDIF.

  IF NOT status IS INITIAL.
*        set_status( status ).
    CALL METHOD o_alv->set_screen_status(
      EXPORTING
        report        = sy-cprog
        pfstatus      = status
        set_functions = o_alv->c_functions_all ).
  ENDIF.

* set the columns technical
  TRY.
      o_columns = o_alv->get_columns( level = 1 ).
    CATCH cx_root.
  ENDTRY.
  o_columns->set_optimize( optimize ).
  i_columns = o_columns->get( ).
  TRY.
      o_columns_det = o_alv->get_columns( level = 2 ).
    CATCH cx_root.
  ENDTRY.
  o_columns_det->set_optimize( optimize ).
  i_columns_det = o_columns_det->get( ).

  IF NOT lights IS INITIAL.
    TRY.
        o_columns->set_exception_column( value = 'LIGHTS' ).
      CATCH cx_root.
    ENDTRY.
  ENDIF.


* Selecciones
  TRY.
      o_selections = o_alv->get_selections( level = 1 ).
    CATCH cx_root.
  ENDTRY.
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
*  SET HANDLER handle_link_click FOR o_events.


  IF NOT color IS INITIAL.
* El campo de color debe de ser: t_color TYPE lvc_t_scol
    TRY.
        o_columns->set_color_column( color ).
      CATCH cx_root.
    ENDTRY.
    me->campo_color = color.
  ENDIF.

  TRY.
      o_sorts = o_alv->get_sorts( level = 1 ).
    CATCH cx_root.
  ENDTRY.
  TRY.
      o_aggregation = o_alv->get_aggregations( level = 1 ).
    CATCH cx_root.
  ENDTRY.

  set_seleccion( ).

  CREATE OBJECT o_top_page.

  IF NOT inicio_columna IS INITIAL.
    i_columnas = fill_columnas( inicio_columna ).
  ENDIF.

ENDMETHOD.
method REFRESH.
data l_stable type LVC_S_STBL.

  if not s_stable is SUPPLIED.
    l_stable-row = 'X'.
    l_stable-col = 'X'.
  else.
    l_stable = s_stable.
  endif.

  o_alv->refresh( s_stable = l_stable
                  refresh_mode = refresh_mode ).

endmethod.
METHOD set_agregacion.
  DATA: l_columname TYPE lvc_fname,
        i_columnas  TYPE TABLE OF lvc_fname,
        l_error(80).


  SPLIT campo AT ',' INTO TABLE i_columnas.
  LOOP AT i_columnas INTO l_columname.
    TRY.
        CALL METHOD o_columns->get_column
          EXPORTING
            columnname = l_columname
          RECEIVING
            value      = o_column.
        TRY.
            o_aggregation->add_aggregation( l_columname ).
          CATCH cx_root.
        ENDTRY.
      CATCH cx_salv_not_found .
        CONCATENATE 'Campo'(cam) l_columname 'no existe'(noe) INTO l_error
          SEPARATED BY space.
        MESSAGE l_error TYPE 'I'
                DISPLAY LIKE 'E'.
    ENDTRY.
  ENDLOOP.
ENDMETHOD.
METHOD set_color.
  DATA: ls_color  TYPE lvc_s_colo,
        lr_column TYPE REF TO cl_salv_column_table,
        l_column  TYPE lvc_fname.

  IF campo IS INITIAL.
    l_column = campo_color.
  ELSE.
    l_column = campo.
  ENDIF.
  CHECK NOT l_column IS INITIAL.
  TRY.
      lr_column ?= o_columns->get_column( l_column ).
    CATCH cx_root.
  ENDTRY.

  ls_color-col = color.

  ls_color-int = 0.

  ls_color-inv = 0.

  lr_column->set_color( ls_color ).

* El campo de color debe de ser: t_color TYPE lvc_t_scol
ENDMETHOD.
METHOD SET_COLUMNAS.
  DATA: l_col TYPE zap_alv_columnas,
        i_cols TYPE zap_alv_columnas_t,
        i_colsc TYPE zap_alv_columnas_t.

  IF i_columnas IS INITIAL.
    i_cols = me->i_columnas.
  ELSE.
    i_cols = i_columnas.
  ENDIF.

  IF NOT campo IS INITIAL.
    i_colsc = fill_columnas( campo ).
    LOOP AT i_colsc INTO l_col.
      READ TABLE i_cols TRANSPORTING NO FIELDS WITH KEY campo = l_col-campo.
      IF sy-subrc NE 0.
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
  ENDLOOP.

  IF i_columnas IS INITIAL.
    me->i_columnas = i_cols.
  ENDIF.

ENDMETHOD.
METHOD SET_COLUMNAS3.
  DATA: l_col TYPE zap_alv_columnas3,
        i_cols TYPE zap_alv_columnas3_t,
        i_colsc TYPE zap_alv_columnas3_t.

  IF i_columnas IS INITIAL.
    i_cols = me->i_columnas3.
  ELSE.
    i_cols = i_columnas.
  ENDIF.

  IF NOT campo IS INITIAL.
    i_colsc = fill_columnas3( campo ).
    LOOP AT i_colsc INTO l_col.
      READ TABLE i_cols TRANSPORTING NO FIELDS WITH KEY campo = l_col-campo.
      IF sy-subrc NE 0.
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
  ENDLOOP.

  IF i_columnas IS INITIAL.
    me->i_columnas3 = i_cols.
  ENDIF.

ENDMETHOD.
method SET_END_OF_PAGE.

  end_of_page( ).
  o_alv->set_end_of_list( o_content ).
*  SET HANDLER handle_end_of_page FOR o_events.

endmethod.
METHOD set_field.
  DATA: l_column     TYPE lvc_fname,
        i_column     TYPE TABLE OF lvc_fname,
        l_error      TYPE string,
        l_text(100),
        l_texto      TYPE scrtext_s,
        l_texto2     TYPE scrtext_l,
        l_texto3     TYPE scrtext_m,
        l_decimals   TYPE lvc_decmls,
        l_dec_column TYPE lvc_dfname,
        lr_column    TYPE REF TO cl_salv_column_table.

  SPLIT campo AT ',' INTO TABLE i_column.
  LOOP AT i_column INTO l_column.
    TRANSLATE l_column TO UPPER CASE.
    TRY.
        IF det IS INITIAL.
          CALL METHOD o_columns->get_column
            EXPORTING
              columnname = l_column
            RECEIVING
              value      = o_column.
        ELSE.
          CALL METHOD o_columns_det->get_column
            EXPORTING
              columnname = l_column
            RECEIVING
              value      = o_column.
        ENDIF.
        CASE op.
          WHEN 'EDIT' OR 'INPUT'. "o_column->set_edit( 'X' ).
          WHEN 'NOOUT' OR 'NO_OUT'. o_column->set_visible( ' ' ).
          WHEN 'OUT'. o_column->set_visible( 'X' ).
          WHEN 'DECIMALS'.
            l_decimals = valor.
            o_column->set_decimals( l_decimals ).
            IF NOT valor2 IS INITIAL.
              l_dec_column = valor2.
              TRY.
                  o_column->set_decimals_column( l_dec_column ).
                CATCH cx_root.
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
              TRANSLATE l_text+1 TO LOWER CASE.
              TRANSLATE l_text+1 USING '_ '.
              l_texto  = l_text.
              l_texto2 = l_text.
              l_texto3 = l_text.
            ENDIF.

            o_column->set_short_text( l_texto ).
            l_texto3 = l_texto2.
            o_column->set_medium_text( l_texto3 ).
            o_column->set_long_text( l_texto2 ).

          WHEN 'HOTSPOT'.
            lr_column ?= o_columns->get_column( l_column ).
            lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).


          WHEN OTHERS.
            MESSAGE e398(00) WITH 'Error en SET_FIELD OP='(esf) op
                                  'no definida'(nod) ''.
        ENDCASE.
        .
      CATCH cx_salv_not_found .
        CONCATENATE 'Campo'(cam) l_column 'no existe'(noe) INTO l_error
          SEPARATED BY space.
        MESSAGE l_error TYPE 'I'
                DISPLAY LIKE 'E'.
    ENDTRY.
  ENDLOOP.


ENDMETHOD.
method SET_FIELD_DECIMALES.

  set_field( op = 'DECIMALS' campo = campo valor = valor  ).

endmethod.
method SET_FIELD_INPUT.

  set_field( op = 'INPUT' campo = campo ).

endmethod.
METHOD set_field_noout.

  set_field( op = 'NO_OUT' campo = campo det = det ).

ENDMETHOD.
method SET_FIELD_TEXT.

  set_field( op = 'TEXTO' campo = campo valor = valor valor2 = valor2 ).

endmethod.
METHOD set_filter.
  DATA: lo_filters TYPE REF TO cl_salv_filters,
        l_low      TYPE salv_de_selopt_low,
        l_high     TYPE salv_de_selopt_high.

  TRY.
      lo_filters = o_alv->get_filters( level = 1 ).
    CATCH cx_root.
  ENDTRY.

  l_low = valor.
  l_high = valor_hasta.
  TRY.
      CALL METHOD lo_filters->add_filter
        EXPORTING
          columnname = campo
          sign       = sign
          option     = option
          low        = l_low
          high       = l_high.

    CATCH cx_salv_not_found .                           "#EC NO_HANDLER
    CATCH cx_salv_data_error .                          "#EC NO_HANDLER
    CATCH cx_salv_existing .                            "#EC NO_HANDLER
  ENDTRY.

ENDMETHOD.
method SET_LAYOUT.

  o_layout->set_initial_layout( layout ).

endmethod.
method SET_ORDEN.
  DATA: l_columname TYPE lvc_fname,
        i_columnas TYPE TABLE OF lvc_fname,
        l_sequence TYPE salv_de_sort_sequence,
          l_error(80).

  IF down = 'X'.
    l_sequence = 2.
  ELSE.
    l_sequence = 1.
  ENDIF.

  l_columname = campo.
  SPLIT campo AT ',' INTO TABLE i_columnas.
  LOOP AT i_columnas INTO l_columname.
    TRY.
        CALL METHOD o_columns->get_column
          EXPORTING
            columnname = l_columname
          RECEIVING
            value      = o_column.
        try.
        o_sorts->add_sort( columnname = l_columname
                           subtotal = subtot
                           sequence = l_sequence
                           group    = group ).
        CATCH cx_root.
        endtry.
      CATCH cx_salv_not_found .
        CONCATENATE 'Campo'(cam) l_columname 'no existe'(noe) INTO l_error
          SEPARATED BY space.
        MESSAGE l_error TYPE 'I'
                DISPLAY LIKE 'E'.
    ENDTRY.
  ENDLOOP.

endmethod.
method SET_SELECCION.
  data: l_campo type string,
        l_row type int4,
        t_row type salv_t_row.

  field-symbols: <tabla> type table,
                 <check> type any,
                 <campo> type c.

  clear i_rows.
  if not o_selections is initial.
    if not campo_check is initial.
      if t_tabla is initial.
        assign (nombre_tabla) to <tabla>.
        if sy-subrc = 0.
          clear t_row.
          loop at <tabla> assigning <check>.
            l_row = sy-tabix.
            concatenate '<CHECK>-' campo_check into l_campo.
            assign (l_campo) to <campo>.
            if <campo> = 'X'.
              append l_row to t_row.
            endif.
          endloop.
          o_selections->set_selected_rows( t_row ).
        endif.
      else.
        loop at t_tabla assigning <check>.
          l_row = sy-tabix.
          concatenate '<CHECK>-' campo_check into l_campo.
          assign (l_campo) to <campo>.
          if <campo> = 'X'.
            append l_row to t_row.
          endif.
        endloop.
        o_selections->set_selected_rows( t_row ).
      endif.
    endif.
  endif.

endmethod.
method SET_STATUS.
*  DATA: repid TYPE sy-repid.

*  repid = sy-repid.
  CALL METHOD o_alv->set_screen_status(
    EXPORTING
     report = repid
     pfstatus = status
     set_functions = o_alv->c_functions_all ).

endmethod.
method SET_TOP_OF_PAGE.

*  SET HANDLER handle_top_of_page FOR o_events.
*  top_of_page( ).
*  o_alv->set_top_of_list( o_content ).

endmethod.
method SHOW.
  o_alv->display( ).
  YA_MOSTRADO = 'X'.
endmethod.
METHOD show_popup.

  o_alv->set_screen_popup(
    start_column = start_column
    end_column   = end_column
    start_line   = start_line
    end_line     = end_line ).

  IF sel = 'X'.
    TRY.
        o_selections = o_alv->get_selections( level = 1 ).
      CATCH cx_root.
    ENDTRY.
    o_selections->set_selection_mode(
         if_salv_c_selection_mode=>row_column ).
  ENDIF.

  o_alv->display( ).
ENDMETHOD.
METHOD SHOW_POPUP_ST.
  DATA o_alv TYPE REF TO zcl_ap_alv.

  CREATE OBJECT o_alv
    EXPORTING
      tabla = ''.

  o_alv->constructor_tabla( CHANGING t_tabla = t_tabla ).

  o_alv->show_popup( ).

ENDMETHOD.
method TOP_OF_PAGE.

  CLEAR o_content.
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

endmethod.
method TRADUCE_COLOR.
  DATA: l_inicial(1) TYPE c,
        l_codigo(12) TYPE c.

  l_codigo = codigo.
  TRANSLATE l_codigo TO UPPER CASE.

  l_inicial = codigo.

  CASE l_inicial.
    WHEN 'R'.    "Rojo
      color = c_color_rojo.
    WHEN 'B'.   "Azul
      color = c_color_azul.
    WHEN 'N'.    "Naranja
      color = c_color_naranja.
    WHEN 'V'.    "Verde
      color = c_color_verde.
    WHEN 'G'.    "Gris
      color = c_color_gris.
    WHEN 'T'.
      color = c_color_amarillo_osc.
    WHEN OTHERS.
      CASE l_codigo.
        when 'AMARILLO'.
          color = c_color_amarillo.
        WHEN 'AZUL'.
          color = c_color_azul.
        WHEN 'AMARILLO OSC'.
          color = c_color_amarillo_osc.
      ENDCASE.
  ENDCASE.

endmethod.
