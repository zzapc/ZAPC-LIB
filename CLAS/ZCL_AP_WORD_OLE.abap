class ZCL_AP_WORD_OLE definition
  public
  inheriting from ZCL_AP_OLE
  create public .

public section.

  data DOCUMENTO_ACTIVO type OLE2_OBJECT .
  data CONTENIDO_DOCUMENTO type OLE2_OBJECT .
  data VENTANA_ACTIVA type OLE2_OBJECT .
  constants C_UN_CELDA type I value 12 ##NO_TEXT.
  constants C_UN_LINEA type I value 5 ##NO_TEXT.
  constants C_UN_CARACTER type I value 1 ##NO_TEXT.
  constants C_TEXTURA_PLANA type I value 0 ##NO_TEXT.
  constants C_COLOR_NEGRO type I value -16777216 ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !MOSTRAR_ERRORES type CHAR1 default 'I'
      !VISIBLE type ABAP_BOOL default '1'
      !O_LOG type ref to ZCL_AP_LOG optional .
  methods CREAR_DOCUMENTO .
  methods CARGAR_PLANTILLA
    importing
      !FICHERO type STRING .
  methods GRABAR_DOCUMENTO
    importing
      !FICHERO type STRING
      !CERRAR type ABAP_BOOL default ''
      !VISIBLE type ABAP_BOOL default '1' .
  methods IR_AL_FINAL_DOCUMENTO
    importing
      !OBJ type STRING default 'D' .
  methods INSERTAR_NUEVA_PAGINA
    importing
      !OBJ type STRING default 'D'
      !TIPO type I default 7
    preferred parameter OBJ .
  methods INSERTAR_FICHERO
    importing
      !FICHERO type STRING
      !OBJ type STRING default 'S' .
  methods IR_A_MARCADOR
    importing
      !MARCADOR type STRING
      !BORRAR type ABAP_BOOL default ''
      !HEADER type ABAP_BOOL default '' .
  methods BORRAR_MARCADOR
    importing
      !MARCADOR type STRING .
  methods MOVER_A_CELDA_DERECHA
    importing
      !TIPO type I default C_UN_CELDA
      !CANTIDAD type I default 1
      !MARCAR type I default 0
    preferred parameter CANTIDAD .
  methods TECLEA_TEXTO
    importing
      !TEXTO type ANY
      !FORMATEAR type ABAP_BOOL default 'X'
      !DECIMALES type INT2 default 99 .
  methods REEMPLAZAR_TEXTO
    importing
      !CLAVE type STRING
      !TEXTO type ANY
      !OBJ type OLE2_OBJECT optional .
  methods HABILITAR_HEADER
    importing
      !SEEKVIEW type C default '9' .
  methods IR_A_INICIO_DOCUMENTO .
  methods NUEVO_PARRAFO .
  methods COPIA_N_LINEAS
    importing
      !LINEAS type I
      !DIRECCION type ANY default 'MoveDown'
      !EXTEND type I default 1
      !UNIDAD type I default 5 .
  methods BORRAR_SELECCION .
  methods PEGAR_SELECCION
    importing
      !FORMATO type INT2 default 1 .
  methods SUBIR_N_LINEAS
    importing
      !LINEAS type I
      !EXTEND type I default 0
    preferred parameter LINEAS .
  methods MOVER_A_CELDA_IZQUIERDA
    importing
      !TIPO type I default C_UN_CELDA
      !CANTIDAD type I default 1
      !MARCAR type I default 0 .
  methods SET_REEMPLAZAR
    importing
      !VALOR type I default TRUE .
  methods SALTO_LINEA .
  methods EXPORTAR_DOCUMENTO
    importing
      !FICHERO type STRING
      !EXTENSION type ANY
    returning
      value(NUEVO_FICHERO) type STRING .
  methods CERRAR .
  methods EXISTE_MARCADOR
    importing
      !MARCADOR type STRING
    returning
      value(EXISTE) type ABAP_BOOL .
  methods SET_NEGRITA
    importing
      !NEGRITA type ABAP_BOOL default 'X' .
  methods INSERTAR_FICHERO_FIN_DOC
    importing
      !FICHERO type STRING
      !OBJ type STRING default 'S' .
  methods INSERTAR_IMAGEN
    importing
      !IMAGEN type ANY
      !ANCHO type I default 0
    preferred parameter IMAGEN .
  methods FORMATO_FUENTE
    importing
      !NEGRITA type ABAP_BOOL optional
      !FUENTE type ANY optional
      !TAMANYO type ANY optional
      !CURSIVA type ABAP_BOOL optional
      !SUBRAYADO type ABAP_BOOL optional .
  methods ESCRIBE_TABLA_STRING
    importing
      !T_STRINGS type TABLE_OF_STRINGS .
  methods CREAR_TABLA
    importing
      !COLUMNAS type ANY default 2
      !FILAS type ANY default 1 .
  methods TECLEA_TEXTO_FORMATO
    importing
      !TEXTO type ANY
      !FORMATEAR type ABAP_BOOL default 'X'
      !DECIMALES type INT2 default 99 .
  methods INSERTAR_URL
    importing
      !URL type ANY
      !TEXTO type ANY optional .
  methods MOVER_A_CELDA_ABAJO
    importing
      !TIPO type I default C_UN_CELDA
      !CANTIDAD type I default 1
      !MARCAR type I default 0 .
  methods BORRAR_CARACTER .
  methods COLOR_CELDA
    importing
      !TEXTURA type INT4 default C_TEXTURA_PLANA
      !FOREGROUNDPATTERNCOLOR type INT4 default C_COLOR_NEGRO
      !BACKGROUNDPATTERNCOLOR type INT4 default 0 .
  methods BORRAR_FILA .
  methods GRABAR_COMO_PDF
    importing
      !FICHERO type STRING
      !CERRAR type ABAP_BOOL default ''
      !VISIBLE type ABAP_BOOL default '1' .
protected section.
private section.
endclass. "ZCL_AP_WORD_OLE definition
class ZCL_AP_WORD_OLE implementation.
method BORRAR_CARACTER.

    exec_metodo_seleccion( metodo = 'TypeBackspace'  ).

endmethod.
METHOD borrar_fila.
  DATA: l_sel  TYPE ole2_object,
        l_rows TYPE ole2_object.

  l_sel = get_obj_app( 'Selection'  ).
  l_rows = get_obj( origen = l_sel metodo = 'Rows' ).
  exec_metodo( obj = l_rows metodo = 'Delete' ).

*
ENDMETHOD.
method BORRAR_MARCADOR.
  data l_marcador type ole2_object.

  l_marcador = get_obj( origen = documento_activo
                        metodo = 'Bookmarks'
                        p1 = marcador ).
  exec_metodo( obj = l_marcador metodo = 'Delete' ).

endmethod.
method BORRAR_SELECCION.

  exec_metodo_seleccion( 'Delete' ).

endmethod.
method CARGAR_PLANTILLA.

  open_file( obj = documento fichero = fichero ).

  documento_activo = get_obj_app( 'ActiveDocument' ).

*  contenido_documento = get_obj_app( 'Content' ).

  ventana_activa = get_obj( origen = documento_activo metodo = 'ActiveWindow' ).

  selection = get_obj_app( 'Selection' ).

endmethod.
method CERRAR.

  set_prop_app( prop = 'DisplayAlerts' valor = false ).
  exec_metodo( obj = documento metodo = 'Close' p1 = false ).
  exec_metodo( obj = app metodo = 'Quit' ).

endmethod.
METHOD color_celda.
  DATA: shading TYPE ole2_object.

  IF selection IS INITIAL.
    selection = get_obj_app( 'Selection' ).
  ENDIF.
  shading = get_obj( origen = selection metodo = 'Shading' ).

  set_prop( obj = shading prop = 'Texture' valor = textura ).
  set_prop( obj = shading prop = 'ForegroundPatternColor' valor = foregroundpatterncolor ).
  set_prop( obj = shading prop = 'BackgroundPatternColor' valor = backgroundpatterncolor ).

ENDMETHOD.
method CONSTRUCTOR.

  CALL METHOD super->constructor
    EXPORTING
      objeto          = 'word.application'
      mostrar_errores = mostrar_errores.

  set_prop_app( prop = 'Visible' valor = VISIBLE ).

  documento = get_obj_app( 'Documents' ).

  selection = get_obj_app( 'Selection' ).

endmethod.
method COPIA_N_LINEAS.

  exec_metodo_seleccion( metodo = direccion p1 = unidad p2 = lineas p3 = EXTEND ).

*Unit Opcional WdUnits Unidad por la que va a moverse la selección. El valor predeterminado eswdLine.  Lineas = 5
*Count Opcional Variant Número de unidades que va a moverse la selección. El valor predeterminado es 1.
*Extend Opcional Variant Puede ser wdMove o wdExtend. Si se utiliza wdMove, se contrae la selección en el extremo final y desciende. Si se utiliza wdExtend, la selección desciende. El valor predeterminado es wdMove.

  exec_metodo_seleccion( 'Copy' ).

endmethod.
method CREAR_DOCUMENTO.

  exec_metodo( obj = documento metodo = 'Add' ).

  documento_activo = get_obj_app( 'ActiveDocument' ).

  contenido_documento = get_obj_app( 'Content' ).

  ventana_activa = get_obj( origen = documento_activo metodo = 'ActiveWindow' ).

  selection = get_obj_app( 'Selection' ).

endmethod.
method CREAR_TABLA.
  DATA: l_sel   TYPE ole2_object,
        l_rango TYPE ole2_object,
        l_tabla TYPE ole2_object.

*    ActiveDocument.Tables.Add Range:=Selection.Range, NumRows:=1, NumColumns:= _
*        2, DefaultTableBehavior:=wdWord9TableBehavior, AutoFitBehavior:= _
*        wdAutoFitFixed
  l_sel = get_obj_app( 'Selection'  ).
  l_rango = get_obj( origen = l_sel metodo = 'Range' ).
  l_tabla = get_obj( origen = documento_activo metodo = 'Tables' ).
  exec_metodo( obj = l_tabla metodo = 'Add' p1 = l_rango p2 = filas p3 = columnas no_write = 'X' ).

*
endmethod.
method ESCRIBE_TABLA_STRING.
  DATA: l_string TYPE string,
        l_string_normal TYPE string,
        l_string_formateado TYPE string,
        l_string_normal2 TYPE string.

  LOOP AT t_strings INTO l_string.
    teclea_texto_formato( texto = l_string ).
    nuevo_parrafo( ).
  ENDLOOP.

endmethod.
method EXISTE_MARCADOR.

  get_seleccion( ).
  CALL METHOD OF selection 'GoTo'
    EXPORTING
    #1 = -1
    #2 = 0
    #3 = 0
    #4 = marcador.

 if sy-subrc = 0.
   existe = 'X'.
 else.
   clear existe.
 endif.

endmethod.
method EXPORTAR_DOCUMENTO.
  DATA: l_formato TYPE i,
        l_long TYPE i,
        l_fichero(255).

  l_fichero = fichero.
  l_long = STRLEN( l_fichero ).
  l_long = l_long - 3.
  CASE extension.
    WHEN 'PDF'.
      l_formato = 17.
      l_fichero+l_long = 'PDF'.
  ENDCASE.

  nuevo_fichero = l_fichero.
  exec_metodo( obj = documento_activo metodo = 'ExportAsFixedFormat'
               p1 = nuevo_fichero
               p2 = l_formato ).

endmethod.
method FORMATO_FUENTE.
  DATA l_obj TYPE ole2_object.

  l_obj = get_obj_sel( 'Font' ).

  IF negrita IS SUPPLIED.
    IF negrita = 'X'.
      set_prop( obj = l_obj prop = 'Bold' valor = true ).
    ELSE.
      set_prop( obj = l_obj prop = 'Bold' valor = false ).
    ENDIF.
  ENDIF.

  IF cursiva IS SUPPLIED.
    IF cursiva = 'X'.
      set_prop( obj = l_obj prop = 'Italic' valor = true ).
    ELSE.
      set_prop( obj = l_obj prop = 'Italic' valor = false ).
    ENDIF.
  ENDIF.

  IF subrayado IS SUPPLIED.
    IF subrayado = 'X'.
      set_prop( obj = l_obj prop = 'Underline' valor = true ).
    ELSE.
      set_prop( obj = l_obj prop = 'Underline' valor = false ).
    ENDIF.
  ENDIF.

  IF fuente IS SUPPLIED.
    set_prop( obj = l_obj prop = 'Name' valor = fuente ).
  ENDIF.

  IF tamanyo IS SUPPLIED.
    set_prop( obj = l_obj prop = 'Size' valor = tamanyo ).
  ENDIF.

endmethod.
method GRABAR_COMO_PDF.
  DATA l_documento_activo TYPE ole2_object.

  l_documento_activo = get_obj_app( 'ActiveDocument' ).

  exec_metodo( obj = l_documento_activo metodo = 'ExportAsFixedFormat'
               p1 = fichero
               p2 = 17       "ExportFormat:=wdExportFormatPDF
               p3 = 1        "OpenAfterExport:=True
               ).


endmethod.
method GRABAR_DOCUMENTO.
  DATA l_documento_activo TYPE ole2_object.

  l_documento_activo = get_obj_app( 'ActiveDocument' ).

  save_file( obj = documento_activo  fichero = fichero ).

  IF visible = '1'.
    set_prop_app( prop = 'Visible' valor = visible ).
    set_prop_app( prop = 'WindowState' valor = '1' ).

  ENDIF.

  IF cerrar = 'X'.
    set_prop_app( prop = 'DisplayAlerts' valor = false ).
    exec_metodo( obj = documento metodo = 'Save' ).
    exec_metodo( obj = app metodo = 'Save' ).
    exec_metodo( obj = documento metodo = 'Close' p1 = false ).
    exec_metodo( obj = app metodo = 'Quit' ).
  ENDIF.

endmethod.
METHOD habilitar_header.
  DATA: l_active_pane TYPE ole2_object,
        l_view TYPE ole2_object.

*wdSeekCurrentPageFooter  10  The current page footer.
*wdSeekCurrentPageHeader  9 The current page header.
*wdSeekEndnotes	8	Endnotes.
*wdSeekEvenPagesFooter  6 The even pages footer.
*wdSeekEvenPagesHeader  3 The even pages header.
*wdSeekFirstPageFooter  5 The first page footer.
*wdSeekFirstPageHeader  2 The first page header.
*wdSeekFootnotes  7 Footnotes.
*wdSeekMainDocument	0	The main document.
*wdSeekPrimaryFooter  4 The primary footer.
*wdSeekPrimaryHeader  1 The primary header.

  documento_activo = get_obj_app( 'ActiveDocument' ).

  ventana_activa = get_obj( origen = documento_activo metodo = 'ActiveWindow' ).

  l_active_pane = get_obj( origen = ventana_activa metodo = 'ActivePane' ).
  l_view = get_obj( origen = l_active_pane metodo = 'View' ).

  set_prop( obj = l_view prop = 'SeekView' valor = seekview ). "wdSeekCurrentPageHeader=9
  "wdSeekMainDocument=0



ENDMETHOD.
method INSERTAR_FICHERO.

  IF obj = 'S'.
    exec_metodo( obj = selection metodo = 'InsertFile' p1 = fichero ).
  ELSE.
*    exec_metodo( obj = documento metodo = 'InsertFile' p1 = fichero ).
    IF documento_activo IS INITIAL.
      exec_metodo( obj = documento metodo = 'InsertFile' p1 = fichero ).
    ELSE.
      exec_metodo( obj = documento_activo metodo = 'InsertFile' p1 = fichero ).
    ENDIF.
  ENDIF.
endmethod.
method INSERTAR_FICHERO_FIN_DOC.
  DATA l_doc TYPE ole2_object.

  l_doc = get_obj_app( 'Selection'  ).
  exec_metodo( obj = l_doc metodo = 'EndKey' p1 = 6 ).
  exec_metodo( obj = l_doc metodo = 'InsertBreak' p1 = 7 ).
  exec_metodo( obj = l_doc metodo = 'InsertFile' p1 = fichero ).

endmethod.
method INSERTAR_IMAGEN.

  DATA: l_sel   TYPE ole2_object,
        l_forma TYPE ole2_object,
        l_ancho TYPE i,
        l_alto  TYPE i.

  l_sel = get_obj_app( 'Selection'  ).
  l_forma = get_obj( origen = l_sel metodo = 'InlineShapes' ).
  exec_metodo( obj = l_forma metodo = 'AddPicture' p1 = imagen ).

  IF ancho > 0.

    exec_metodo_seleccion( metodo = 'MoveLeft' p1 = 1 p2 = 1 p3 = 1 ).
    l_forma = get_obj( origen = l_sel metodo = 'InlineShapes' p1 = 1 ).

    l_ancho = get_prop( obj = l_forma prop = 'Width' ).
    set_prop( obj = l_forma prop = 'Width' valor = ancho ).

    l_alto  = get_prop( obj = l_forma prop = 'Height' ).
    IF l_ancho NE 0.
      l_alto = l_alto * ancho / l_ancho.
      set_prop( obj = l_forma prop = 'Height' valor = l_alto ).
    ENDIF.
    set_prop( obj = l_forma prop = 'LockAspectRatio' valor = -1 ).
    exec_metodo_seleccion( metodo = 'MoveRight' p1 = 1 p2 = 1 p3 = 0 ).
  ENDIF.

endmethod.
method INSERTAR_NUEVA_PAGINA.

* P1 = 7 Nueva página normal
* p1 = 2 Nueva seccion
  IF obj = 'S'.
    exec_metodo( obj = selection metodo = 'InsertBreak' p1 = tipo ).
  ELSE.
    exec_metodo( obj = documento metodo = 'InsertBreak' p1 = tipo ).
  ENDIF.

endmethod.
method INSERTAR_URL.

  DATA: l_hyperlink   TYPE ole2_object,
        l_doc TYPE ole2_object,
        l_sel TYPE ole2_object,
        l_rango TYPE ole2_object.

  l_doc = get_obj_app( 'ActiveDocument'  ).
  l_hyperlink =   get_obj( origen = l_doc metodo = 'Hyperlinks'  ).
  l_sel = get_obj_app( 'Selection'  ).
  l_rango = get_obj( origen = l_sel metodo = 'Range' ).

  CALL METHOD OF l_hyperlink 'Add'
    EXPORTING
    #1 = l_rango
    #2 = url
    #3 = ''
    #4 = texto
    #5 = texto.

endmethod.
method IR_A_INICIO_DOCUMENTO.

  exec_metodo_seleccion( metodo = 'HomeKey' p1 = 6 ).

endmethod.
METHOD ir_a_marcador.
  data l_marcador type ole2_object.

  IF header IS INITIAL.
    get_seleccion( ).
    exec_metodo_seleccion( metodo = 'GoTo'
                           p1 = -1 p2 = 0 p3 = 0 p4 = marcador ).

  ELSE.
    l_marcador = get_obj( origen = documento_activo
                          metodo = 'Bookmarks'
                          p1 = marcador ).
    exec_metodo( obj = l_marcador metodo = 'Select' ).
  ENDIF.

  IF borrar = 'X'.
    borrar_marcador( marcador ).
  ENDIF.


ENDMETHOD.
method IR_AL_FINAL_DOCUMENTO.

  IF obj = 'S'.
    exec_metodo( obj = selection  metodo = 'EndKey' p1 = 6 ).
  ELSE.
    IF documento_activo IS INITIAL.
      exec_metodo( obj = documento metodo = 'EndKey' p1 = 6 ).
    ELSE.
      exec_metodo( obj = documento_activo metodo = 'EndKey' p1 = 6 ).
    ENDIF.
  ENDIF.

endmethod.
method MOVER_A_CELDA_ABAJO.

  exec_metodo_seleccion( metodo = 'MoveDown' p1 = tipo p2 = cantidad p3 = marcar ).

endmethod.
method MOVER_A_CELDA_DERECHA.

  exec_metodo_seleccion( metodo = 'MoveRight' p1 = tipo p2 = cantidad p3 = marcar ).

*Unit Opcional WdUnits Unidad por la que va a moverse la selección. El valor predeterminado eswdCharacter.
*Count Opcional Variant Número de unidades que va a moverse la selección. El valor predeterminado es 1.
*Extend Opcional Variant Puede ser wdMove o wdExtend. Si se utiliza wdMove, se contrae la selección en el extremo final y se mueve hacia la izquierda. Si se utiliza wdExtend, la selección se extiende hacia la derecha. El valor predeterminado es wdMove.

endmethod.
method MOVER_A_CELDA_IZQUIERDA.

  exec_metodo_seleccion( metodo = 'MoveLeft' p1 = tipo p2 = cantidad p3 = marcar ).

endmethod.
method NUEVO_PARRAFO.
  exec_metodo_seleccion( metodo = 'TypeParagraph' ).
endmethod.
method PEGAR_SELECCION.

*  exec_metodo_seleccion( metodo = 'PasteAndFormat' p1 = formato ).
  exec_metodo_seleccion( 'Paste' ).

endmethod.
METHOD reemplazar_texto.
  DATA l_find TYPE ole2_object.

  IF obj IS INITIAL.
    IF contenido_documento IS INITIAL.
      selection = get_obj_app( 'Selection' ).
      l_find = get_obj( origen = selection metodo = 'Find' ).
    ELSE.
      l_find = get_obj( origen = contenido_documento metodo = 'Find' ).
    ENDIF.
  ELSE.
    l_find = get_obj( origen = obj metodo = 'Find' ).
  ENDIF.

  IF strlen( texto ) < 255 OR texto CS clave.
    CALL METHOD OF l_find 'Execute'
      EXPORTING
        #1  = clave  "FindText
        #2  = '0'
        #3  = '0'
        #4  = '0'
        #5  = '0'
        #6  = '0'
        #7  = '1'
        #8  = '1'    "Wrap -> 1 = wdFindContinue
        #9  = '0'
        #10 = texto  "ReplaceWith
        #11 = '2'
        #12 = '1'
        #13 = '1'
        #14 = '1'
        #15 = '1'.
    control_error( texto = 'Busqueda:' texto2 = clave texto3 = texto ).
  ELSE.
    DATA: l_texto    TYPE string,
          l_texto200 TYPE string.
    l_texto200 = texto(200) && clave.
    IF NOT texto+200 IS INITIAL.
      l_texto = texto+200.
      reemplazar_texto( obj = obj clave = clave texto = l_texto200 ).
      reemplazar_texto( obj = obj clave = clave texto = l_texto ).
    ENDIF.
  ENDIF.

*Ejecuta la operación de búsqueda especificada. Devuelve True si la operación de búsqueda se realiza con éxito. Boolean
*Sintaxis
*
*expresión.Execute(FindText, MatchCase, MatchWholeWord, MatchWildcards, MatchSoundsLike, MatchAllWordForms, Forward, Wrap, Format,
* ReplaceWith, Replace, MatchKashida, MatchDiacritics, MatchAlefHamza, MatchControl, MatchPrefix, MatchSuffix, MatchPhrase, IgnoreSpace, IgnorePunct)
*
*expresión   Requerida. Variable que representa un objeto Find.
*
*Parámetros
*
*Nombre Obligatorio/Opcional Tipo de datos Descripción
*1. FindText Opcional Variant El texto que se va a buscar. Para buscar sólo formato se debe usar una cadena vacía ("").
* Para buscar caracteres especiales se deben especificar los códigos de caracteres apropiados. Por ejemplo, "^p" corresponde a una marca de párrafo y "^t" corresponde a un carácter de tabulación.
*MatchCase Opcional Variant True para especificar que el texto de búsqueda distinga mayúsculas de minúsculas. Corresponde a la casilla Mayúsculas/minúsculas en el cuadro de diálogo Buscar y reemplazar (menú Edición).
*MatchWholeWord Opcional Variant True para que la operación de búsqueda encuentre solamente palabras completas y no texto que es parte de una palabra más extensa.
*Corresponde a la casilla Sólo palabras completas en el cuadro de diálogo Buscar y reemplazar.
*MatchWildcards Opcional Variant True para hacer que el texto de búsqueda sea un operador de búsqueda especial. Corresponde a la casilla Usar caracteres comodín en el cuadro de diálogo Buscar y reemplazar.
*MatchSoundsLike Opcional Variant True para hacer que la operación de búsqueda encuentre palabras que suenan de un modo similar al texto de búsqueda. Corresponde a la casilla Suena como en el cuadro de diálogo Buscar y reemplazar.
*MatchAllWordForms Opcional Variant True para hacer que la operación de búsqueda encuentre todas las formas del texto de búsqueda (por ejemplo, "sentarse" encuentra "sentado" y "sentó").
*Corresponde a la casilla Todas las formas de la palabra en el  cuadro de diálogo Buscar y reemplazar.
*Forward Opcional Variant True para buscar hacia adelante (hacia el final del documento).
*8.Wrap Opcional Variant Controla lo que sucede si la búsqueda se inicia en otro punto que no sea el comienzo del documento y se alcanza el final del documento (o viceversa si Forward está establecido en False).
* Este argumento controla además lo que  sucede si hay una selección o un intervalo y no se puede encontrar el texto de búsqueda en la selección o el intervalo. Puede ser alguna de las constantes WdFindWrap.
*9.Format Opcional Variant True para hacer que la operación de búsqueda encuentre formato además de, o en lugar de, el texto de búsqueda.
*10.ReplaceWith Opcional Variant Texto de reemplazo. Para eliminar el texto especificado por el argumento Find, se debe usar una cadena vacía ("").
*Los caracteres especiales y los criterios de búsqueda avanzada se especifican del mismo modo en que se  hace con el argumento Find.
*Para especificar un objeto gráfico u otro elemento que no sea de texto como el reemplazo, se debe mover el elemento al Portapapeles y especificar "^c" en ReplaceWith.
*11.Replace Opcional Variant Especifica cuántos reemplazos se efectuarán: uno, todos o ninguno. Puede ser cualquier constante WdReplace constant.
*12.MatchKashida Opcional Variant True si las operaciones de búsqueda encuentran texto con kashida en un documento escrito en un idioma árabe.
*Es probable que este argumento no esté disponible para usted según cuál sea la compatibilidad de idiomas  [Inglés (EE. UU.) por ejemplo] que haya seleccionado o instalado.
*13.MatchDiacritics Opcional Variant True si las operaciones de búsqueda encuentran texto con diacríticos en un documento en un idioma en el cual se escribe de derecha a izquierda.
* Es probable que este argumento no esté disponible para usted según cuál  sea la compatibilidad de idiomas [Inglés (EE. UU.) por ejemplo] que haya seleccionado o instalado.
*14.MatchAlefHamza Opcional Variant True si las operaciones de búsqueda encuentran texto con alif hamza en un documento escrito en un idioma árabe.
* Es probable que este argumento no esté disponible para usted según cuál sea la compatibilidad de idiomas  [Inglés (EE. UU.) por ejemplo] que haya seleccionado o instalado.
*15.MatchControl Opcional Variant True si las operaciones de búsqueda encuentran texto con caracteres de control bidireccionales en un documento en un idioma en el que se escribe de derecha a izquierda.
*Es probable que este argumento no esté disponible para usted según cuál sea la compatibilidad de idiomas [Inglés (EE. UU.) por ejemplo] que haya seleccionado o instalado.
*MatchPrefix Opcional Variant True para encontrar palabras que comiencen con la cadena de búsqueda. Corresponde a la casilla Prefijo en el cuadro de diálogoBuscar y reemplazar.
*MatchSuffix Opcional Variant True para encontrar palabras que terminen con la cadena de búsqueda. Corresponde a la casilla Sufijo en el cuadro de diálogo Buscar y reemplazar.
*MatchPhrase Opcional Variant True omite todos los espacios en blanco y caracteres de control entre palabras.
*IgnoreSpace Opcional Variant True ignora todos los espacios en blanco entre las palabras. Corresponde a la casilla Omitir espacios en blanco en el cuadro de diálogo Buscar y reemplazar.
*IgnorePunct Opcional Variant True ignora todos los caracteres de puntuación entre las palabras

ENDMETHOD.
method SALTO_LINEA.

  exec_metodo_seleccion( metodo = 'TypeParagraph' ).

endmethod.
method SET_NEGRITA.
  DATA l_obj TYPE ole2_object.

  l_obj = get_obj_sel( 'Font' ).
  IF negrita = 'X'.
    set_prop( obj = l_obj prop = 'Bold' valor = true ).
  ELSE.
    set_prop( obj = l_obj prop = 'Bold' valor = false ).
  ENDIF.

endmethod.
method SET_REEMPLAZAR.

  set_prop( obj = selection prop = 'ReplaceSelection ' valor = valor ).

endmethod.
method SUBIR_N_LINEAS.

  exec_metodo_seleccion( metodo = 'MoveUp' p1 = 5 p2 = lineas p3 = extend ).

*Unit Opcional WdUnits Unidad por la que va a moverse la selección. El valor predeterminado eswdLine.  Lineas = 5
*Count Opcional Variant Número de unidades que va a moverse la selección. El valor predeterminado es 1.
*Extend Opcional Variant Puede ser wdMove o wdExtend. Si se utiliza wdMove, se contrae la selección en el extremo final y desciende. Si se utiliza wdExtend, la selección desciende. El valor predeterminado es wdMove.


endmethod.
method TECLEA_TEXTO.
  data: l_tipo type c,
        l_texto(10),
        l_string(255).

  describe field texto type l_tipo.
  if l_tipo = 'I' or l_tipo = 'P' or l_tipo = 'D'.

    if formatear = 'X'.
      if decimales = 99.
        write texto to l_texto.
      else.
        write texto to l_texto decimals decimales.
      endif.
    endif.

    exec_metodo_seleccion( metodo = 'TypeText' p1 = l_texto ).
  else.

    if formatear = 'X'.
      write texto to l_string.
      exec_metodo_seleccion( metodo = 'TypeText' p1 = l_string ).
    else.
      exec_metodo_seleccion( metodo = 'TypeText' p1 = texto ).
    endif.
  endif.
endmethod.
method TECLEA_TEXTO_FORMATO.
  DATA: l_string_normal TYPE string,
        l_string_formateado TYPE string,
        l_string_normal2 TYPE string,
        l_negrita,
        l_cursiva,
        l_subrayado.

  IF texto CS '<H>'.
    SPLIT texto AT '<H>' INTO l_string_normal l_string_formateado.
    l_negrita = 'X'.
  ELSEIF texto CS '<U>'.
    SPLIT texto AT '<U>' INTO l_string_normal l_string_formateado.
    l_subrayado = 'X'.
  ELSEIF texto CS '<C>'.
    SPLIT texto AT '<C>' INTO l_string_normal l_string_formateado.
    l_cursiva = 'X'.
  ELSEif texto cs '</>'.
    l_string_formateado = texto.
  ELSE.
    teclea_texto( texto = texto formatear = formatear decimales = decimales ).
    EXIT.
  ENDIF.

  SPLIT l_string_formateado AT '</>' INTO l_string_formateado l_string_normal2.

  IF NOT l_string_normal IS INITIAL.
    teclea_texto( texto = l_string_normal formatear = formatear decimales = decimales ).
  ENDIF.

  IF NOT l_string_formateado IS INITIAL.
    IF l_negrita = 'X'.
      formato_fuente( negrita = 'X' ).
    ELSEIF l_cursiva = 'X'.
      formato_fuente( cursiva = 'X' ).
    ELSEIF l_subrayado = 'X'.
      formato_fuente( subrayado = 'X' ).
    ENDIF.
    teclea_texto( texto = l_string_formateado ).
    IF l_negrita = 'X'.
      formato_fuente( negrita = '' ).
    ELSEIF l_cursiva = 'X'.
      formato_fuente( cursiva = '' ).
    ELSEIF l_subrayado = 'X'.
      formato_fuente( subrayado = '' ).
    ENDIF.
  ENDIF.

  IF NOT l_string_normal2 IS INITIAL.
    teclea_texto( texto = l_string_normal2 ).
  ENDIF.


endmethod.
