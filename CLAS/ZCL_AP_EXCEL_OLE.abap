type-pools abap.
class ZCL_AP_EXCEL_OLE definition
  public
  inheriting from ZCL_AP_OLE
  create public .

public section.

  constants C_BORDE_TOP type I value 8 ##NO_TEXT.
  constants C_BORDE_LEFT type I value 7 ##NO_TEXT.
  constants C_BORDE_BOTTON type I value 9 ##NO_TEXT.
  constants C_BORDE_RIGHT type I value 10 ##NO_TEXT.
  constants C_ESTILO_SOLIDO type I value 1 ##NO_TEXT.
  constants C_COLORINDEX_NEGRO type I value -4105 ##NO_TEXT.
  data ACTIVE_SHEET type OLE2_OBJECT .
  constants C_ALIGN_CENTER type I value -4108 ##NO_TEXT.
  constants C_COLOR_AZUL type I value 41 ##NO_TEXT.
  data FUENTE_FONDO type I value -999 ##NO_TEXT.
  constants C_XLTOLEFT type I value -4159 ##NO_TEXT.
  constants C_COLOR_FONDO_AMARILLO type I value 65535 ##NO_TEXT.
  constants C_COLOR_FONDO_AMARILLO_CLARO type I value 13434879 ##NO_TEXT.
  constants C_COLOR_FONDO_VERDE type I value 5296274 ##NO_TEXT.
  constants C_COLOR_FONDO_SALMON type I value 10079487 ##NO_TEXT.
  data FONDO_CELDA type I value -999 ##NO_TEXT.
  constants C_COLOR_FONDO_AZUL type I value 16764057 ##NO_TEXT.
  constants C_BORDE_VERTICAL type I value 11 ##NO_TEXT.
  constants C_BORDE_HORIZONTAL type I value 12 ##NO_TEXT.
  constants C_XLNONE type I value -4142 ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !O_LOG type ref to ZCL_AP_LOG optional
      !MOSTRAR_ERRORES type CHAR1 default 'I'
      !VISIBLE type INT1 default 0 .
  methods CREAR_DOCUMENTO .
  methods CARGAR_PLANTILLA
    importing
      !FICHERO type STRING .
  methods GRABAR_DOCUMENTO
    importing
      !FICHERO type STRING
      !CERRAR type ABAP_BOOL default ''
      !MOSTRAR_DIALOGO type ABAP_BOOL default '' .
  methods IR_A_MARCADOR
    importing
      !MARCADOR type STRING
      !BORRAR type ABAP_BOOL default ''
      !MOSTRAR_ERRORES type ABAP_BOOL default 'X'
    returning
      value(ERROR) type ABAP_BOOL .
  methods BORRAR_MARCADOR
    importing
      !MARCADOR type STRING .
  methods REEMPLAZAR_TEXTO_DOC
    importing
      !CLAVE type STRING
      !TEXTO type ANY .
  methods VALOR_CELDA
    importing
      !FILA type I optional
      !COLUMNA type I optional
      !VALOR type ANY
      !APLICAR_FORMATO_FUENTE type ABAP_BOOL default ''
      !CELDA type ANY optional .
  methods SET_FORMATO_FUENTE
    importing
      !NOMBRE type STRING optional
      !NEGRITA type I optional
      !CURSIVA type I optional
      !SUBRAYADO type I optional
      !TAMANYO type I optional
      !COLOR type I optional
      !FONDO type I optional
      !FONDO_CELDA type I optional .
  class-methods CONVERT_COLUMNA
    importing
      !COLUMNA type I
    returning
      value(LETRAS) type CHAR2 .
  methods CONVERT_RANGO
    importing
      !FILA_INI type I
      !COLUMNA_INI type I
      !FILA_FIN type I optional
      !COLUMNA_FIN type I optional
    returning
      value(RANGO) type STRING .
  methods RANGO_SET_FORMATO_FUENTE .
  methods SET_RANGO
    importing
      !RANGO type STRING optional
      !FILA_INI type I optional
      !COLUMNA_INI type I optional
      !FILA_FIN type I optional
      !COLUMNA_FIN type I optional
      !SET_SELECCION type ABAP_BOOL default 'X'
      !COPIAR type ABAP_BOOL default ''
      !INSERTAR type ABAP_BOOL default ''
      !BORRAR type ABAP_BOOL default ''
    preferred parameter RANGO .
  methods RANGO_SET_FORMATO_BORDE
    importing
      !LADO type I optional
      !LINESTYLE type I default C_ESTILO_SOLIDO
      !WEIGHT type I default 2
      !COLORINDEX type I default C_COLORINDEX_NEGRO
    preferred parameter LADO .
  methods SET_CELDA_POR_NOMBRE
    importing
      !NOMBRE type ANY
      !VALOR type ANY .
  methods GET_CELDA_POR_NOMBRE
    importing
      !NOMBRE type ANY
      !PROPIEDAD type ANY default 'FormulaR1C1'
    returning
      value(VALOR) type STRING .
  methods PROTEGER_HOJA
    importing
      !PASSWORD type STRING .
  methods PASTE .
  methods GET_ACTIVE_SHEET .
  methods PEGAR_COLUMNA
    changing
      !I_COLUMNA type TABLE .
  methods GENERAR_GRAFICO
    importing
      !TITULO type STRING optional
      !P_CHARTTYPE type STRING optional
      !P_COORDX type STRING optional
      !P_COORDY type STRING optional
      !P_HASLEGEND type INTEGER optional
    preferred parameter TITULO .
  methods COPIAR_TABLA
    changing
      !I_TABLA type TABLE .
  methods COLOR_RANGO
    importing
      !COLOR type I optional
      !TRAMA type I default 1
    preferred parameter COLOR .
  methods FIJAR_VISIBLE .
  methods PEGAR_TABLA
    importing
      !CABECERA type ABAP_BOOL default ''
      !I_TITULOS type TABLE optional
      !I_TABLA type TABLE
      !CELDA_ORIGEN type STRING default 'A1'
      !COLUMNA_INICIAL type I default 1
      !COLUMNA_FINAL type I default 999
      !BORDE type C default ''
      !AUTOFILTRO type ABAP_BOOL default ''
      !FILA_INI type I optional
      !COLUMNA_INI type I optional
      !AUTOFIT type ABAP_BOOL default 'X'
      !RESALTAR_TITULOS type ABAP_BOOL default 'X'
      !COL_TEXTO type STRING default ''
      !COL_IMPORTES type STRING default ''
      !REPLACEMENT type ANY default ''
      !I_COLUMNAS3 type ZAP_ALV_COLUMNAS3_T optional
      !I_CAMPOS_SEL type ABAP_COMPONENT_TAB optional
      !CAMPOS_REP_PUNTO_COMA type STRING default '' .
  methods PEGAR_TABLA_DDIC
    changing
      !I_TABLA type TABLE .
  class-methods INDICE_COLUMNA
    importing
      value(LETRAS) type CHAR2
    returning
      value(COLUMNA) type I .
  methods CELDA2POS
    importing
      !CELDA type ANY
    returning
      value(POSICION) type STRING .
  methods IR_A_CELDA
    importing
      !CELDA type STRING optional
      !FILA type I optional
      !COLUMNA type I optional
    preferred parameter CELDA .
  methods CELDA2FILACOL
    importing
      !CELDA type ANY
    exporting
      value(FILA) type I
      !COLUMNA type I .
  methods RANGO_UNIR_CELDAS .
  methods RANGO_COLOR_INTERIOR .
  methods RANGO_CENTRAR
    importing
      !ALINEACION type I default C_ALIGN_CENTER .
  methods RANGO_ANCHO_COLUMNA
    importing
      !ANCHO type I optional .
  methods INMOVILIZAR_PANELES .
  methods PEGAR_IMAGEN
    importing
      !FICHERO type ANY .
  methods CONFIGURAR_PAGINA
    importing
      !RIGHTFOOTER type STRING optional
      !CENTERHEADER type STRING optional
      !PREVIEW type ABAP_BOOL optional .
  methods REEMPLAZAR_TEXTO
    importing
      !CLAVE type STRING
      !TEXTO type ANY .
  methods BORRAR_SELECCION
    importing
      !SHIFT type I default C_XLTOLEFT .
  methods RANGO_FORMATEAR_SELECCION
    importing
      !WRAPTEXT type ANY optional .
  methods AUTOFIT .
  methods INSERTAR_HOJA .
  methods COPIAR_HOJA
    importing
      !ORIGEN type STRING default 'Hoja1'
      !DESTINO type STRING default '' .
  methods SELECCIONAR_HOJA
    importing
      !HOJA type STRING default 'Hoja1' .
  methods SET_DISPLAYALERTS
    importing
      !VALOR type I default FALSE .
  methods IMPRIMIR
    importing
      !CERRAR type ABAP_BOOL default ''
      !MOSTRAR_DIALOGO type ABAP_BOOL default '' .
  class-methods CELDA_2_FILA_COL
    importing
      !CELDA type ANY
    exporting
      !FILA type ANY
      !COLUMNA type ANY .

  methods FREE
    redefinition .
  PROTECTED SECTION.
    METHODS control_error REDEFINITION.

  PRIVATE SECTION.
    DATA rango            TYPE ole2_object.
    DATA fuente_nombre    TYPE string.
    DATA fuente_negrita   TYPE i           VALUE 0 ##NO_TEXT.
    DATA fuente_cursiva   TYPE i           VALUE 0 ##NO_TEXT.
    DATA fuente_subrayado TYPE i           VALUE -4142 ##NO_TEXT.
    DATA fuente_tamanyo   TYPE i           VALUE 10 ##NO_TEXT.
    DATA fuente_color     TYPE i           VALUE 0 ##NO_TEXT.
endclass. "ZCL_AP_EXCEL_OLE definition
class ZCL_AP_EXCEL_OLE implementation.
  METHOD autofit.
    DATA l_columns TYPE ole2_object.

    CALL METHOD OF selection 'Columns' = l_columns.
    CALL METHOD OF l_columns 'Autofit'.
    FREE OBJECT l_columns.
  ENDMETHOD.
  METHOD borrar_marcador.
    DATA l_marcador TYPE ole2_object.

    l_marcador = get_obj( origen = documento
                          metodo = 'Bookmarks'
                          p1 = marcador ).
    exec_metodo( obj = l_marcador metodo = 'Delete' ).
  ENDMETHOD.
  METHOD borrar_seleccion.

    CHECK NOT rango IS INITIAL.
    CALL METHOD OF rango 'Delete'
      EXPORTING
        #1 = shift.    " Shift
  ENDMETHOD.
  METHOD cargar_plantilla.
    open_file( obj = documento fichero = fichero ).
  ENDMETHOD.
  METHOD celda2filacol.
    DATA: l_lon   TYPE i,
          l_index TYPE i,
          l_letra TYPE c LENGTH 1,
          l_col   TYPE c LENGTH 2.

    l_lon = strlen( celda ).
    DO l_lon TIMES.
      l_index = sy-index - 1.
      l_letra = celda+l_index(1).
      IF l_letra CO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
        CONCATENATE l_col l_letra INTO l_col.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    fila = celda+l_index.
    columna = indice_columna( l_col ).
  ENDMETHOD.
  METHOD celda2pos.
    DATA: l_row    TYPE i,
          l_col    TYPE i,
          l_coltxt TYPE c LENGTH 3,
          l_rowtxt TYPE c LENGTH 5.

    celda2filacol( EXPORTING celda   = celda
                   IMPORTING fila    = l_row
                             columna = l_col ).
    l_coltxt = l_col.
    l_rowtxt = l_row.
    CONCATENATE 'R' l_rowtxt 'C' l_coltxt INTO posicion.
    CONDENSE posicion NO-GAPS.
  ENDMETHOD.
  METHOD celda_2_fila_col.
    DATA: l_celda TYPE string,
          l_fila  TYPE string.

    l_celda = celda.

    fila = zcl_ap_string=>busca_numeros( string = l_celda cualquiera = 'X' ).
    l_fila = fila.
    CONDENSE l_fila NO-GAPS.
    SPLIT l_celda AT l_fila INTO columna l_celda.
  ENDMETHOD.
  METHOD color_rango.
    DATA l_celda TYPE ole2_object.

    CHECK NOT selection IS INITIAL.
    l_celda = get_obj( origen = selection metodo = 'Interior' ).

    SET PROPERTY OF l_celda 'ColorIndex' = color.
    SET PROPERTY OF l_celda 'Pattern' = trama.
  ENDMETHOD.
  METHOD configurar_pagina.
    " TODO: parameter PREVIEW is never used (ABAP cleaner)

    DATA: l_activesheet TYPE ole2_object,
          l_pagesetup   TYPE ole2_object.

    l_activesheet = get_obj( origen = app metodo = 'ActiveSheet' ).
    l_pagesetup = get_obj( origen = l_activesheet metodo = 'PageSetup' ).
    set_prop( obj = l_pagesetup prop = 'CenterHeader' valor = centerheader ).
    set_prop( obj = l_pagesetup prop = 'RightFooter' valor = rightfooter ).

    exec_metodo( obj = l_activesheet
               metodo = 'PrintPreview' ).
  ENDMETHOD.
  METHOD constructor.
    super->constructor(
        objeto          = 'excel.application'
        mostrar_errores = mostrar_errores
        o_log           = o_log ).

    set_prop_app( prop = 'Visible' valor = visible ).
    set_prop_app( prop = 'ScreenUpdating' valor = visible ).
    documento = get_obj_app( 'WorkBooks' ).
  ENDMETHOD.
  METHOD control_error.
    DATA l_subrc TYPE sy-subrc.

    l_subrc = sy-subrc.

    IF l_subrc <> 0.
      error = 'X'.

      IF mostrar_errores = 'X'.
        IF NOT o_log IS INITIAL.
          o_log->l_msg2 = texto2.
          o_log->l_msg3 = texto3.
          o_log->msg_error( msgv1 = texto
                            msgv2 = o_log->l_msg2
                            msgv3 = o_log->l_msg3 ).
          IF me->mostrar_errores = 'X'.
            o_log->grabar( ).
          ENDIF.
        ENDIF.
      ENDIF.

      hay_error = 'X'.
      fijar_visible( ).

      IF mostrar_errores = 'X'.
        IF me->mostrar_errores = 'X'.
          MESSAGE e398(00) WITH texto texto2 texto3.
        ELSEIF me->mostrar_errores = 'I'.
          MESSAGE s398(00) WITH texto texto2 texto3.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD convert_columna.
    DATA: lv_column TYPE int4,
          lv_module TYPE int4,
          lv_uccpi  TYPE i,
          lv_text   TYPE sychar02.

    lv_column = columna.
    WHILE lv_column > 0.
      lv_module = ( lv_column - 1 ) MOD 26.
      lv_uccpi  = 65 + lv_module.

      lv_column = ( lv_column - lv_module ) / 26.

      lv_text   = cl_abap_conv_in_ce=>uccpi( lv_uccpi ).
      CONCATENATE lv_text letras INTO letras.
    ENDWHILE.
  ENDMETHOD.
  METHOD convert_rango.
    DATA: l_col_ini  TYPE c LENGTH 2,
          l_fila_ini TYPE c LENGTH 5,
          l_col_fin  TYPE c LENGTH 2,
          l_fila_fin TYPE c LENGTH 5.

    l_col_ini = convert_columna( columna_ini ).
    l_fila_ini = fila_ini.

    IF columna_fin IS INITIAL.
      l_col_fin = l_col_ini.
      l_fila_fin = l_fila_fin.
    ELSE.
      l_col_fin = convert_columna( columna_fin ).
      l_fila_fin = fila_fin.
    ENDIF.

    CONCATENATE l_col_ini l_fila_ini ':' l_col_fin l_fila_fin
                INTO rango.
    CONDENSE rango NO-GAPS.
  ENDMETHOD.
  METHOD copiar_hoja.
    DATA: work_copy TYPE ole2_object,
          workarea  TYPE ole2_object,
          l_destino TYPE string.

    work_copy = get_obj_app( metodo = 'Sheets' p1 = origen ).
    workarea = get_obj_app( metodo = 'Sheets' p1 = origen ).
    exec_metodo( obj = workarea metodo = 'Activate' ).
    exec_metodo( obj = workarea metodo = 'Copy' p1 = work_copy no_write = 'X' ).

    get_active_sheet( ).
    exec_metodo( obj = active_sheet metodo = 'Select' ).

    IF NOT destino IS INITIAL.
      l_destino = destino.
    ENDIF.

    set_prop( obj = active_sheet prop = 'Name' valor = l_destino ).
    exec_metodo( obj = workarea metodo = 'Move'  p2 = active_sheet no_write = 'X' ).
  ENDMETHOD.
  METHOD copiar_tabla.
    DATA l_rc TYPE i.

    cl_gui_frontend_services=>clipboard_export(
      IMPORTING
        data                 = i_TABLA
      CHANGING
        rc                   = l_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error exportando al portapapeles' TYPE 'I'.
    ENDIF.
  ENDMETHOD.
  METHOD crear_documento.
    exec_metodo( obj = documento metodo = 'Add' ).
    set_prop_app( prop = 'Calculation' valor = -4135 ). " Manual
  ENDMETHOD.
  METHOD fijar_visible.
    set_prop_app( prop = 'Visible' valor = true ).
    set_prop_app( prop = 'ScreenUpdating' valor = true ).
    SET PROPERTY OF me->app 'Calculation' = -4105. " Automático
  ENDMETHOD.
  METHOD free.
*CALL METHOD SUPER->FREE
*    .
  ENDMETHOD.
  METHOD generar_grafico.
    DATA: l_charts       TYPE ole2_object,
          l_chart        TYPE ole2_object,
          l_titulo       TYPE ole2_object,
          l_caracteres   TYPE ole2_object,
          l_axisx        TYPE ole2_object,
          l_axisy        TYPE ole2_object,
          l_legend       TYPE ole2_object,
          l_chartarea    TYPE ole2_object,
          l_plotarea     TYPE ole2_object,
          l_border       TYPE ole2_object,
          l_interior     TYPE ole2_object,
          l_activewindow TYPE ole2_object.

    l_charts = get_prop_obj( obj = app prop = 'Charts' ).
    l_chart  = get_obj( origen = l_charts metodo = 'Add' ).

* Tipo de gráfico:
    IF p_charttype IS SUPPLIED.
      SET PROPERTY OF l_chart 'ChartType' = p_charttype.
    ENDIF.

* Título del gráfico:
    IF titulo IS SUPPLIED.
      SET PROPERTY OF l_chart 'HasTitle' = 1.
      l_titulo = get_prop_obj( obj = l_chart prop = 'ChartTitle' ).
      l_caracteres = get_prop_obj( obj = l_titulo prop = 'Characters' ).
      SET PROPERTY OF l_caracteres 'Text' = titulo.
    ENDIF.

* Título de la coordenada X:
    IF p_coordx IS SUPPLIED.
      l_axisx = get_obj( origen = l_chart metodo = 'Axes'
                         p1 = 2 p2 = 1 ).
      SET PROPERTY OF l_axisx 'HasTitle' = 1.
      l_titulo = get_prop_obj( obj = l_axisx prop = 'AxisTitle' ).
      l_caracteres = get_prop_obj( obj = l_titulo prop = 'Characters' ).
      SET PROPERTY OF l_caracteres 'Text' = p_coordx.
    ENDIF.

* Título de la coordenada Y:
    IF p_coordy IS SUPPLIED.
      l_axisy = get_obj( origen = l_chart metodo = 'Axes'
                         p1 = 2 p2 = 1 ).
      SET PROPERTY OF l_axisy 'HasTitle' = 1.
      l_titulo = get_prop_obj( obj = l_axisy prop = 'AxisTitle' ).
      l_caracteres = get_prop_obj( obj = l_titulo prop = 'Characters' ).
      SET PROPERTY OF l_caracteres 'Text' = p_coordy.
    ENDIF.

* Posición de la leyenda:
    IF p_haslegend IS SUPPLIED.
      IF p_haslegend = 0.
        SET PROPERTY OF l_chart 'HasLegend' = 0.
      ELSE.
        l_legend = get_prop_obj( obj = l_chart prop = 'Legend' ).
        exec_metodo( obj = l_legend metodo = 'Select' ).
        SET PROPERTY OF l_legend 'Position' = -4107.    " Posición abajo
      ENDIF.
    ELSE.
      l_legend = get_prop_obj( obj = l_chart prop = 'Legend' ).
      exec_metodo( obj = l_legend metodo = 'Select' ).
      SET PROPERTY OF l_legend 'Position' = -4107.       " Posición abajo
    ENDIF.

* En caso de tratarse de un gráfico de quesitos, se quiere que salgan
* los datos de los valores y los porcentajes. Se quita un área cuadrada
* que sale por defecto en el gráfico:
    IF p_charttype = '5'.
      CALL METHOD OF l_chart 'ApplyDataLabels'
        EXPORTING
          #1 = 1
          #2 = 1
          #3 = 0
          #4 = 0
          #5 = 0
          #6 = 1
          #7 = 1
          #8 = 1.

      l_chartarea = get_prop_obj( obj = l_chart prop = 'ChartArea' ).
      exec_metodo( obj = l_chartarea metodo = 'Select' ).
      l_plotarea = get_prop_obj( obj = l_chart prop = 'PlotArea' ).
      exec_metodo( obj = l_chartarea metodo = 'Select' ).

      l_border = get_prop_obj( obj = l_plotarea prop = 'Border' ).
      SET PROPERTY OF l_border 'Weight' = 2.
      SET PROPERTY OF l_border 'LineStyle' = -4142.

      l_interior = get_prop_obj( obj = l_plotarea prop = 'Interior' ).
      SET PROPERTY OF l_interior 'ColorIndex' = -4142.
    ENDIF.

* Ponemos el gráfico como un objeto dentro de una hoja de datos para
* poder poner el logo de Vossloh:
    IF zcl_c=>cliente_tasks = 'VOS'.
      exec_metodo( obj = l_chart metodo = 'Location' p1 = 2
                                                     p2 = 'Hoja2' ).
    ELSE.
      exec_metodo( obj = l_chart metodo = 'Location' p1 = 2
                                                     p2 = 'Hoja1' ).
    ENDIF.

* Quitamos la selección al gráfico para poder seleccionar celdas y
* poner el logo y la fecha del sistema:
    l_activewindow = get_prop_obj( obj = app prop = 'ActiveWindow' ).
    SET PROPERTY OF l_activewindow 'Visible' = false.

    IF zcl_c=>cliente_tasks = 'VOS'.
* Ponemos la fecha del sistema:
      valor_celda( fila = 2 columna = 1 valor = sy-datum
                            aplicar_formato_fuente = 'X' ).
* Ponemos el logo de la empresa:
      fuente_nombre = 'Vossloh Logo'.
      fuente_tamanyo = 20.
      valor_celda( fila = 1 columna = 1 valor = 'abcd'
                            aplicar_formato_fuente = 'X' ).
    ENDIF.
* Activamos el gráfico:
    exec_metodo( obj = l_chart metodo = 'Activate' ).
  ENDMETHOD.
  METHOD get_active_sheet.
    active_sheet = get_obj_app( metodo = 'ActiveSheet' ).
  ENDMETHOD.
  METHOD get_celda_por_nombre.
    DATA l_celda TYPE ole2_object.

    IF ir_a_marcador( marcador = nombre mostrar_errores = '' ) = ''.
      l_celda = get_obj_app( metodo = 'ActiveCell' ).

      valor = get_prop( obj = l_celda prop = propiedad ).
    ENDIF.
  ENDMETHOD.
  METHOD grabar_documento.
    DATA: l_active_workbook TYPE ole2_object,
          l_fichero         TYPE string.

    l_active_workbook = get_obj_app( metodo = 'ActiveWorkbook' ).

    l_fichero = fichero.

    IF mostrar_dialogo = 'X'.
      zcl_ap_ficheros=>dialogo_grabar_fichero(
                              EXPORTING fichero_inicial = l_fichero
                                        mostrar_error   = 'X'
                              IMPORTING ruta            = l_fichero ).
      IF l_fichero IS INITIAL.
        EXIT.
      ENDIF.
    ENDIF.

*  save_file( obj = documento fichero = fichero ).
    save_file( obj = l_active_workbook fichero = l_fichero ).

    IF cerrar = 'X'.
*    set_prop_app( prop = 'Saved' valor = true ).      "lrr28/05/18
      set_prop_app( prop = 'DisplayAlerts' valor = false ).

*    documento = get_obj_app( 'WorkBooks' ).
*    exec_metodo( obj = documento metodo = 'Save' ).
*    exec_metodo( obj = app metodo = 'Save' ).

**   exec_metodo( obj = l_active_workbook metodo = 'Save' ).
*    exec_metodo( obj = documento metodo = 'Close' p1 = false ).
      exec_metodo( obj = l_active_workbook metodo = 'Close' p1 = false ).
      exec_metodo( obj = app metodo = 'Quit' ).
      MESSAGE i398(00) WITH 'Se ha grabado fichero' l_fichero.
    ENDIF.
  ENDMETHOD.
  METHOD imprimir.
    " TODO: parameter MOSTRAR_DIALOGO is never used (ABAP cleaner)

    DATA l_active_workbook TYPE ole2_object.

    l_active_workbook = get_obj_app( metodo = 'ActiveWorkbook' ).

    exec_metodo( obj = l_active_workbook metodo = 'PrintOut' ).

    IF cerrar = 'X'.
      set_prop_app( prop = 'DisplayAlerts' valor = false ).
      exec_metodo( obj = l_active_workbook metodo = 'Close' p1 = false ).
      exec_metodo( obj = app metodo = 'Quit' ).
    ENDIF.
  ENDMETHOD.
  METHOD indice_columna.
    DATA: lv_char  TYPE c LENGTH 1,
          lv_uccpi TYPE i.

* Calculate most significant letter
    lv_char = letras+1(1).
    IF lv_char IS NOT INITIAL. " To avoid the first 26 column that have only a char in first position
      columna = cl_abap_conv_out_ce=>uccpi( lv_char ).

      columna = columna MOD 64.

      lv_char  = letras(1).
      lv_uccpi = cl_abap_conv_out_ce=>uccpi( lv_char ).

      lv_uccpi = ( lv_uccpi MOD 64 ) * 26.

      columna = columna + lv_uccpi.
    ELSE.
      lv_char = letras(1).
      columna = cl_abap_conv_out_ce=>uccpi( lv_char ).
      columna = columna - 64.
    ENDIF.
  ENDMETHOD.
  METHOD inmovilizar_paneles.
    DATA l_activewindow TYPE ole2_object.

    l_activewindow = get_obj_app( metodo = 'ActiveWindow' ).

    set_prop( obj = l_activewindow prop = 'FreezePanes' valor = 1 ).
  ENDMETHOD.
  METHOD insertar_hoja.
    DATA l_sheet TYPE ole2_object.

    l_sheet = get_obj_app( metodo = 'Sheets' ).
    exec_metodo( obj = l_sheet
                 metodo = 'Add' ).
  ENDMETHOD.
  METHOD ir_a_celda.
    DATA: l_celda    TYPE string,
          l_row      TYPE c LENGTH 10,
          l_posicion TYPE string.

    IF celda IS INITIAL.
      l_celda = convert_columna( columna ).
      l_row = fila.
      CONCATENATE l_celda l_row INTO l_celda.
      CONDENSE l_celda NO-GAPS.
    ELSE.
      l_celda = celda.
    ENDIF.

    l_posicion = celda2pos( l_celda ).
    exec_metodo( obj = app
                 metodo = 'GoTo'
                 p1 = l_posicion ).
  ENDMETHOD.
  METHOD ir_a_marcador.
    error = exec_metodo( obj = app
                 metodo = 'GoTo'
                 p1 = marcador
                 mostrar_errores = mostrar_errores ).

    IF error IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF borrar = 'X'.
      borrar_marcador( marcador ).
    ENDIF.
  ENDMETHOD.
  METHOD paste.
    get_active_sheet( ).
    exec_metodo( obj = active_sheet metodo = 'Paste' ).
  ENDMETHOD.
  METHOD pegar_columna.
    copiar_tabla( CHANGING i_tabla = i_columna ).
    paste( ).
  ENDMETHOD.
  METHOD pegar_imagen.
    DATA: l_activesheet TYPE ole2_object,
          l_pictures    TYPE ole2_object.

    l_activesheet = get_obj( origen = app metodo = 'ActiveSheet' ).
    l_pictures = get_obj( origen = l_activesheet metodo = 'Pictures' ).

    exec_metodo( obj = l_pictures
               metodo = 'Insert'
               p1 = fichero ).
  ENDMETHOD.
  METHOD pegar_tabla.
    TYPES: BEGIN OF t_columnas,
             col   TYPE i,
             campo TYPE c LENGTH 20,
           END OF t_columnas.
    TYPES t_texto_largo TYPE c LENGTH 65535.

    DATA: i_col_texto    TYPE TABLE OF text1024,
          i_col_importes TYPE TABLE OF text1024,
          l_fila_ini     TYPE i,
          l_col_ini      TYPE i,
          l_fila_fin     TYPE i,
          l_col_fin      TYPE i,
          i_campos       TYPE abap_component_tab,
          i_campos_del   TYPE abap_component_tab,
          l_tab          TYPE t_texto_largo,
          i_tab          TYPE TABLE OF t_texto_largo,
          o_type         TYPE REF TO cl_abap_datadescr,
          l_col          TYPE i,
          l_columna      TYPE t_columnas,
          i_columnas     TYPE TABLE OF t_columnas,
          l_primera_fila TYPE c LENGTH 1,
          l_valortxt     TYPE c LENGTH 512,
          l_aux          TYPE c LENGTH 40,
          l_string       TYPE string,
          l_col_texto    TYPE text1024.
    DATA l_font    TYPE ole2_object.
    DATA l_columns TYPE ole2_object.

    FIELD-SYMBOLS: <campo> TYPE abap_componentdescr,
                   <valor> TYPE any,
                   <tabla> TYPE any,
                   <col3>  TYPE zap_alv_columnas3.

    SPLIT col_texto AT ',' INTO TABLE i_col_texto.
    SPLIT col_importes AT ',' INTO TABLE i_col_importes.

    IF NOT fila_ini IS INITIAL.
      l_fila_ini = fila_ini.
      l_col_ini  = columna_ini.
    ELSEIF NOT celda_origen IS INITIAL.
      celda2filacol( EXPORTING celda   = celda_origen
                     IMPORTING fila    = l_fila_ini
                               columna = l_col_ini ).
    ENDIF.
    l_fila_fin = l_fila_ini - 1.
    l_col_fin = l_col_ini - 1.

    IF i_campos_sel IS INITIAL.
      i_campos = zcl_ap_dev=>get_fieldcatalog( i_tabla ).
    ELSE.
      i_campos = i_campos_sel.
    ENDIF.
    DELETE i_campos WHERE name IS INITIAL.

    IF NOT i_titulos IS INITIAL.
      LOOP AT i_campos ASSIGNING <campo>.
        READ TABLE i_titulos ASSIGNING <valor> INDEX sy-tabix.
        IF sy-subrc = 0.
          IF <valor> = '#INVISIBLE#'.
            APPEND <campo> TO i_campos_del.
          ENDIF.
        ENDIF.
      ENDLOOP.
      LOOP AT i_campos_del ASSIGNING <campo>.
        DELETE i_campos WHERE name = <campo>-name.
      ENDLOOP.
    ENDIF.

    IF cabecera = 'X'.
      CLEAR l_tab.
      IF i_titulos IS INITIAL.
        LOOP AT i_campos ASSIGNING <campo> FROM columna_inicial.
          IF l_tab IS INITIAL.
            l_tab = <campo>-name.
          ELSE.
            CONCATENATE l_tab cl_abap_char_utilities=>horizontal_tab <campo>-name INTO l_tab.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT i_titulos ASSIGNING <valor> FROM columna_inicial.
          IF <valor> <> '#INVISIBLE#'.
            IF l_tab IS INITIAL.
              l_tab = <valor>.
            ELSE.
              CONCATENATE l_tab cl_abap_char_utilities=>horizontal_tab <valor> INTO l_tab.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      APPEND l_tab TO i_tab.
    ENDIF.

    LOOP AT i_campos ASSIGNING <campo> FROM columna_inicial.
      o_type = <campo>-type.
      IF o_type->type_kind = 'h'. " No procesamos tablas internas
        CONTINUE.
      ENDIF.
      IF sy-tabix > columna_final.
        EXIT.
      ENDIF.
      l_col = l_col + 1.
      CLEAR l_columna.
      l_columna-col   = l_col.
      l_columna-campo = <campo>-name.
      APPEND l_columna TO i_columnas.
    ENDLOOP.

    LOOP AT i_tabla ASSIGNING <tabla>.
      l_fila_fin = l_fila_fin + 1.
      CLEAR l_tab.
      LOOP AT i_campos ASSIGNING <campo> FROM columna_inicial.
        o_type = <campo>-type.
        IF o_type->type_kind = 'h'. " No procesamos tablas internas
          CONTINUE.
        ENDIF.
        IF sy-tabix > columna_final.
          EXIT.
        ENDIF.
        IF l_primera_fila IS INITIAL.
          l_col_fin = l_col_fin + 1.
        ENDIF.
        ASSIGN COMPONENT <campo>-name OF STRUCTURE <tabla> TO <valor>.
        IF o_type->type_kind = 'D' OR o_type->type_kind = 'T'.
          WRITE <valor> TO l_valortxt.
          IF o_type->type_kind = 'D'.
            IF l_valortxt = '00.00.0000'.
              CLEAR l_valortxt.
            ELSE.
              REPLACE ALL OCCURRENCES OF '.' IN l_valortxt WITH '/'.
              l_aux = l_valortxt(2).
              l_valortxt(2) = l_valortxt+3(2).
              l_valortxt+3(2) = l_aux(2).
            ENDIF.
          ENDIF.
        ELSE.
          l_valortxt = <valor>.

          IF o_type->type_kind = 'P'.
            IF l_valortxt CS '-'.
              REPLACE '-' WITH '' INTO l_valortxt.
              CONCATENATE '-' l_valortxt INTO l_valortxt.
            ENDIF.

            if l_Valortxt cs '.' and not CAMPOS_REP_PUNTO_COMA is initial.
              if zcl_ap_lista=>es_elemento( lista  = CAMPOS_REP_PUNTO_COMA elemento = <campo>-name ).
                replace '.' in l_valortxt with ','.
              endif.
            endif.
          ELSE.
            IF o_type->type_kind = 'C'.
              READ TABLE i_columnas3 ASSIGNING <col3> WITH KEY campo = <campo>-name.
              IF sy-subrc = 0.
                IF <col3>-aux3 = 'F'.
                  REPLACE ',' WITH '.' INTO l_valortxt.
                  IF l_valortxt CS '-'.
                    REPLACE '-' WITH '' INTO l_valortxt.
                    CONCATENATE '-' l_valortxt INTO l_valortxt.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.

            IF l_valortxt CS cl_abap_char_utilities=>cr_lf.
              REPLACE ALL OCCURRENCES OF '"' IN l_valortxt WITH ''.
              CONCATENATE '"' l_valortxt '"' INTO l_valortxt.
              sy-tfill = strlen( l_valortxt ).
              IF sy-tfill = 512.
                l_valortxt+511 = '"'.
              ELSEIF sy-tfill = 511 AND l_valortxt+511(1) = ''.
                l_valortxt+511 = '"'.
              ENDIF.
            ELSE.
              IF replacement <> ''.
                l_string = l_valortxt.
                zcl_ap_string=>quitar_caracteres_extranos( EXPORTING replacement = replacement
                                                           CHANGING  string = l_string ).
*APC20140924 Eliminamos también las comillas porque causan problemas
                REPLACE ALL OCCURRENCES OF '"' IN l_string WITH ''.
                l_valortxt = l_string.
              ENDIF.
            ENDIF.

            IF o_type->type_kind = 'C'.
              IF <campo>-name = 'QMNUM' OR <campo>-name = 'MATNR' OR <campo>-name = 'QMINC'.
                __quitar_ceros l_valortxt.
              ENDIF.

              IF l_valortxt CO '0123456789 '.
                sy-tfill = strlen( l_valortxt ).
                IF sy-tfill > 8.
                  CONCATENATE '''' l_valortxt INTO l_valortxt.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        IF     o_type->type_kind <> 'C'
           AND o_type->type_kind <> 'g'. " String
          CONDENSE l_valortxt NO-GAPS.
        ENDIF.

        IF l_tab IS INITIAL.
          IF l_valortxt IS INITIAL.
            l_tab = '"'.
          ELSE.
            l_tab = l_valortxt.
          ENDIF.
        ELSE.
          IF l_tab = '"'.
            CLEAR l_tab.
          ENDIF.
          CONCATENATE l_tab cl_abap_char_utilities=>horizontal_tab l_valortxt INTO l_tab.
        ENDIF.
      ENDLOOP.
      APPEND l_tab TO i_tab.
      l_primera_fila = 'X'.
    ENDLOOP.

    l_fila_fin = l_fila_fin + 1.

    LOOP AT i_col_texto INTO l_col_texto.
      READ TABLE i_columnas INTO l_columna WITH KEY campo = l_col_texto.
      IF sy-subrc = 0.
        set_rango( columna_ini = l_columna-col fila_ini = l_fila_ini
                   columna_fin = l_columna-col fila_fin = l_fila_fin
                   set_seleccion = 'X' ).
        set_prop( obj = selection prop = 'NumberFormat' valor = '@' ).
      ENDIF.
    ENDLOOP.

    LOOP AT i_col_importes INTO l_col_texto.
      READ TABLE i_columnas INTO l_columna WITH KEY campo = l_col_texto.
      IF sy-subrc = 0.
        set_rango( columna_ini = l_columna-col fila_ini = l_fila_ini
                   columna_fin = l_columna-col fila_fin = l_fila_fin
                   set_seleccion = 'X' ).
        set_prop( obj = selection prop = 'NumberFormat' valor = '0.00' ).
      ENDIF.
    ENDLOOP.

    set_rango( columna_ini = 1 fila_ini = 1
               columna_fin = 1 fila_fin = 1
               set_seleccion = 'X' ).

    l_fila_fin = l_fila_fin - 1.

    IF NOT fila_ini IS INITIAL.
      ir_a_celda( fila = fila_ini columna = columna_ini ).
    ELSEIF NOT celda_origen IS INITIAL.
      ir_a_celda( celda_origen ).
    ENDIF.

    copiar_tabla( CHANGING i_tabla = i_tab ).
    paste( ).

    IF resaltar_titulos = 'X'.
      set_rango( columna_ini = l_col_ini fila_ini = l_fila_ini
                 columna_fin = l_col_fin fila_fin = l_fila_ini
                 set_seleccion = 'X' ).

      l_font = get_obj( origen = selection metodo = 'Font' ).

      IF NOT l_font IS INITIAL.
        SET PROPERTY OF l_font 'Bold' = 1.
        FREE OBJECT l_font.
      ENDIF.

      l_font = get_obj( origen = selection metodo = 'Interior' ).
      IF NOT l_font IS INITIAL.
        SET PROPERTY OF l_font 'ColorIndex' = 33.
        SET PROPERTY OF l_font 'Pattern' = 1.
        FREE OBJECT l_font.
      ENDIF.
    ENDIF.

    IF l_fila_fin < l_fila_ini.
      l_fila_fin = l_fila_ini.
    ENDIF.

    set_rango( columna_ini = l_col_ini fila_ini = l_fila_ini
               columna_fin = l_col_fin fila_fin = l_fila_fin
               set_seleccion = 'X' ).

    IF NOT borde IS INITIAL.
      rango_set_formato_borde( ).
    ENDIF.

    IF NOT autofiltro IS INITIAL.
      exec_metodo( obj = selection metodo = 'AutoFilter' ).
    ENDIF.

    IF NOT autofit IS INITIAL.
      CALL METHOD OF selection 'Columns' = l_columns.
      CALL METHOD OF l_columns 'Autofit'.
      FREE OBJECT l_columns.
    ENDIF.

    set_rango( columna_ini = 1 fila_ini = 1
               columna_fin = 1 fila_fin = 1
               set_seleccion = 'X' ).
  ENDMETHOD.
  METHOD pegar_tabla_ddic.
    DATA: i_campos   TYPE ddfields,
          l_tab      TYPE text255,
          l_valortxt TYPE c LENGTH 40,
          i_tab      TYPE TABLE OF text255.

    FIELD-SYMBOLS: <tabla> TYPE any,
                   <campo> TYPE dfies,
                   <valor> TYPE any.

    i_campos = zcl_ap_dev=>get_fieldcatalog_ddic( i_tabla ).

    LOOP AT i_tabla ASSIGNING <tabla>.
      CLEAR l_tab.
      LOOP AT i_campos ASSIGNING <campo> WHERE dynpfld = abap_true.
        ASSIGN COMPONENT <campo>-fieldname OF STRUCTURE <tabla> TO <valor>.
        IF sy-subrc = 0.
          WRITE <valor> TO l_valortxt.
          IF <campo>-inttype <> 'C'.
            CONDENSE l_valortxt NO-GAPS.
          ENDIF.
          IF l_tab IS INITIAL.
            l_tab = l_valortxt.
          ELSE.
            CONCATENATE l_tab cl_abap_char_utilities=>horizontal_tab l_valortxt INTO l_tab.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND l_tab TO i_tab.
    ENDLOOP.

    copiar_tabla( CHANGING i_tabla = i_tab ).
    paste( ).
  ENDMETHOD.
  METHOD proteger_hoja.
    DATA l_activesheet TYPE ole2_object.

    l_activesheet = get_obj( origen = app
                          metodo = 'ActiveSheet' ).

    exec_metodo( obj = l_activesheet p1 = password metodo = 'Protect' ).

    set_prop( obj = l_activesheet prop = 'EnableSelection'
                                  valor = -4142 ).
  ENDMETHOD.
  METHOD rango_ancho_columna.
    set_prop_sel( prop = 'ColumnWidth' valor = ancho ).
  ENDMETHOD.
  METHOD rango_centrar.
    set_prop_sel( prop = 'HorizontalAlignment' valor = alineacion ).
  ENDMETHOD.
  METHOD rango_color_interior.
    DATA l_interior TYPE ole2_object.

    l_interior = get_obj( origen = selection metodo = 'Interior' ).
    IF NOT l_interior IS INITIAL.
      SET PROPERTY OF l_interior 'ColorIndex' = 33.
      SET PROPERTY OF l_interior 'Pattern' = 1.
      FREE OBJECT l_interior.
    ENDIF.
  ENDMETHOD.
  METHOD rango_formatear_seleccion.
    DATA l_selection TYPE ole2_object.

    CHECK NOT rango IS INITIAL.
    IF selection IS INITIAL.
      l_selection = get_obj( origen = rango metodo = 'Select' ).
    ELSE.
      l_selection = selection.
    ENDIF.

    IF NOT l_selection IS INITIAL.
      IF wraptext IS SUPPLIED.
        SET PROPERTY OF l_selection 'WrapText' = wraptext.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD rango_set_formato_borde.
    DATA l_borders TYPE ole2_object.

    CHECK NOT selection IS INITIAL.

    IF NOT lado IS SUPPLIED.
      l_borders = get_obj( origen = selection metodo = 'Borders' ).
    ELSE.
      l_borders = get_obj( origen = selection metodo = 'Borders'
                           p1 = lado ).
    ENDIF.

    IF NOT l_borders IS INITIAL.
      set_prop( obj = l_borders prop = 'LineStyle' valor = linestyle ).
      IF weight <> 0.
        set_prop( obj = l_borders prop = 'Weight' valor = weight ).
      ENDIF.
      set_prop( obj = l_borders prop = 'ColorIndex' valor = colorindex ).
    ENDIF.
  ENDMETHOD.
  METHOD rango_set_formato_fuente.
    DATA l_font TYPE ole2_object.

    CHECK NOT rango IS INITIAL.
    l_font = get_obj( origen = rango metodo = 'Font' ).

    IF NOT l_font IS INITIAL.
      IF NOT fuente_nombre IS INITIAL.
        SET PROPERTY OF l_font 'Name' = fuente_nombre.
      ENDIF.
      SET PROPERTY OF l_font 'Size' = fuente_tamanyo.
      SET PROPERTY OF l_font 'Bold' = fuente_negrita.
      SET PROPERTY OF l_font 'Italic' = fuente_cursiva.
      SET PROPERTY OF l_font 'Underline' = fuente_subrayado.
      SET PROPERTY OF l_font 'ColorIndex' = fuente_color.

      FREE OBJECT l_font.

      IF fuente_fondo <> -999.
        l_font = get_obj( origen = selection metodo = 'Interior' ).
        IF NOT l_font IS INITIAL.
          SET PROPERTY OF l_font 'ColorIndex' = fuente_fondo.
          SET PROPERTY OF l_font 'Pattern' = 1.
          FREE OBJECT l_font.
        ENDIF.
      ENDIF.

      IF fondo_celda <> -999.
        l_font = get_obj( origen = selection metodo = 'Interior' ).
        IF NOT l_font IS INITIAL.
          SET PROPERTY OF l_font 'Color' = fondo_celda.
          SET PROPERTY OF l_font 'Pattern' = 1.
          SET PROPERTY OF l_font 'PatternColorIndex' = -4105. " xlAutomatic
          SET PROPERTY OF l_font 'TintAndShade' = 0.
          SET PROPERTY OF l_font 'PatternTintAndShade' = 0.
          FREE OBJECT l_font.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD rango_unir_celdas.
    CHECK NOT rango IS INITIAL.
    set_prop_app( prop = 'DisplayAlerts' valor = false ).
    exec_metodo( obj = rango metodo = 'Merge' ).
  ENDMETHOD.
  METHOD reemplazar_texto.
    DATA l_find TYPE ole2_object.

    l_find = get_obj_app( metodo = 'Cells' ).

    CALL METHOD OF l_find 'Replace'
      EXPORTING
        #1  = clave    " What
        #2  = texto.    " Text
*      #3  = 1      "LookAt (xlWhole= 1 (coincidir todo el contenido), xlPart=2 parcialmente
*      #4  = '1'      "SearchOrder (xlByRows = 1)
*      #5  = false  "MatchCase
*      #6  = false  "SearchFormat
*      #7  = false. "ReplaceFormat.

    control_error( texto = 'Busqueda:' texto2 = clave texto3 = texto ).
  ENDMETHOD.
  METHOD reemplazar_texto_doc.
    DATA l_find TYPE ole2_object.

*  l_find = get_obj( origen = documento
*                        metodo = 'Find' ).
    l_find = get_obj_app( metodo = 'Find' ).
    CALL METHOD OF l_find 'Execute'
      EXPORTING
        #1  = clave
        #2  = '0'
        #3  = '0'
        #4  = '0'
        #5  = '0'
        #6  = '0'
        #7  = '1'
        #8  = '1'
        #9  = '0'
        #10 = texto
        #11 = '2'
        #12 = '1'
        #13 = '1'
        #14 = '1'
        #15 = '1'.
    control_error( texto = 'Busqueda:' texto2 = clave texto3 = texto ).
  ENDMETHOD.
  METHOD seleccionar_hoja.
    DATA l_sheet TYPE ole2_object.

    l_sheet = get_obj_app( metodo = 'Sheets' p1 = hoja ).
    exec_metodo( obj = l_sheet metodo = 'Activate' ).
    exec_metodo( obj = l_sheet metodo = 'Select' ).
  ENDMETHOD.
  METHOD set_celda_por_nombre.
    DATA l_celda TYPE ole2_object.

    CLEAR hay_error.
    ir_a_marcador( nombre ).
    IF hay_error IS INITIAL.
      l_celda = get_obj_app( metodo = 'ActiveCell' ).

      set_prop( obj = l_celda prop = 'FormulaR1C1' valor = valor ).
    ENDIF.
  ENDMETHOD.
  METHOD set_displayalerts.
    set_prop_app( prop = 'DisplayAlerts' valor = valor ).
  ENDMETHOD.
  METHOD set_formato_fuente.
    IF nombre IS SUPPLIED.
      fuente_nombre = nombre.
    ENDIF.
    IF negrita IS SUPPLIED.
      fuente_negrita = negrita.
    ENDIF.
    IF cursiva IS SUPPLIED.
      fuente_cursiva = cursiva.
    ENDIF.
    IF subrayado IS SUPPLIED.
      fuente_subrayado = subrayado.
    ENDIF.
    IF tamanyo IS SUPPLIED.
      fuente_tamanyo = tamanyo.
    ENDIF.
    IF color IS SUPPLIED.
      fuente_color = color.
    ENDIF.
    IF fondo IS SUPPLIED.
      fuente_fondo = fondo.
    ENDIF.
    IF fondo_celda IS SUPPLIED.
      me->fondo_celda = fondo_celda.
    ENDIF.
  ENDMETHOD.
  METHOD set_rango.
    DATA l_rango TYPE string.

    IF NOT rango IS INITIAL.
      l_rango = rango.
    ELSE.
      l_rango = convert_rango( columna_ini = columna_ini
                               fila_ini = fila_ini
                               columna_fin = columna_fin
                               fila_fin = fila_fin ).
    ENDIF.

    me->rango = get_obj_app( metodo = 'Range'
                             p1 = l_rango ).

    IF set_seleccion = 'X' OR copiar = 'X' OR insertar = 'X' OR borrar = 'X'.
      IF NOT me->rango IS INITIAL.
        exec_metodo( obj = me->rango metodo = 'Select' ).
***????
        selection = get_obj( origen = me->rango metodo = 'Select' ).
*      IF set_seleccion = 'X' OR copiar = 'X' OR insertar = 'X' OR borrar = 'X'.
*        CALL METHOD OF seleccion 'Activate'.
**        exec_metodo( obj = seleccion metodo = 'Activate' ).
*      ENDIF.
***????
*      get_seleccion( ).
        selection = get_obj_app( metodo = 'Selection' ).
        exec_metodo( obj = selection metodo = 'Activate' ).

        IF copiar = 'X'.
          exec_metodo( obj = selection metodo = 'Copy' ).
*        exec_metodo_seleccion( 'Copy' ).
        ENDIF.

        IF insertar = 'X'.
          exec_metodo( obj = selection metodo = 'Insert' ).
*        exec_metodo_seleccion( 'Insert' ).
        ENDIF.

        IF borrar = 'X'.
          exec_metodo( obj = selection metodo = 'Delete' ).
*        exec_metodo_seleccion( 'Delete' ).
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD valor_celda.
    DATA: l_row    TYPE i,
          l_col    TYPE i,
          l_celda  TYPE ole2_object,
          l_font   TYPE ole2_object,
          l_texto  TYPE c LENGTH 15,
          l_texto2 TYPE string.

    IF celda IS INITIAL.
      l_row = fila.
      l_col = columna.
    ELSE.
      celda2filacol( EXPORTING celda   = celda
                     IMPORTING fila    = l_row
                               columna = l_col ).
    ENDIF.

    l_celda = get_obj_app( metodo = 'Cells'
                             p1 = l_row p2 = l_col ).

*  call method of app 'Cells' = l_celda
*    exporting
*      #1 = fila
*      #2 = columna.
    IF NOT l_celda IS INITIAL.
      SET PROPERTY OF l_celda 'Value' = valor.

      IF aplicar_formato_fuente = 'X'.
        GET PROPERTY OF l_celda 'Font' = l_font.

        IF NOT fuente_nombre IS INITIAL.
          SET PROPERTY OF l_font 'Name' = fuente_nombre.
        ENDIF.
        SET PROPERTY OF l_font 'Size' = fuente_tamanyo.
        SET PROPERTY OF l_font 'Bold' = fuente_negrita.
        SET PROPERTY OF l_font 'Italic' = fuente_cursiva.
        SET PROPERTY OF l_font 'Underline' = fuente_subrayado.
        SET PROPERTY OF l_font 'ColorIndex' = fuente_color.

        FREE OBJECT l_font.
      ENDIF.
      FREE OBJECT l_celda.
    ENDIF.

    IF sy-subrc <> 0.
      WRITE: fila TO l_texto(5),
             '-' TO l_texto+6(1),
             columna TO l_texto+7(3).
      l_texto = l_texto2.
      control_error( texto = 'Celda:' texto2 = l_texto2 ).
    ENDIF.
  ENDMETHOD.
