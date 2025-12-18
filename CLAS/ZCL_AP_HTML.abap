class ZCL_AP_HTML definition
  public
  create public .

public section.

  data O_HTML type ref to CL_GUI_HTML_VIEWER .
  data O_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data I_HTML type W3HTMLTABTYPE .
  data HTML type W3HTML .
  constants C_INI_HTML_BTF type STRING value '<!DOCTYPE HTML PUBLIC' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER optional
      !NOMBRE_CONTAINER type ANY default '' .
  methods SET_PAGE
    importing
      !URL type ANY default 'index.htm' .
  methods SHOW
    importing
      !URL type ANY default 'index.htm' .
  methods PRINT .
  methods FREE .
  methods SET_CONTAINER
    importing
      !CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER optional
      !NOMBRE_CONTAINER type ANY default '' .
  methods HEADER
    importing
      !CHARSET type STRING default 'Windows-1252' .
  methods SET_TEXT
    importing
      !TEXTO type ANY optional
      !TEXTO2 type ANY optional
      !TEXTO3 type ANY optional
      !TEXTO4 type ANY optional
      !TABLA type W3HTMLTABTYPE optional
    preferred parameter TEXTO .
  methods ADD_URL
    importing
      !TEXTO type ANY default ''
      !URL type ANY
      !PARRAFO type ABAP_BOOL default ''
      !CODIGO_PARRAFO type STRING default 'P' .
  methods ADD_PARRAFO
    importing
      !TEXTO type ANY
      !PARTIR_SIEMPRE type ABAP_BOOL default ''
      !OPTIMIZAR type ABAP_BOOL default 'X'
      !PARRAFO_UNICO type ABAP_BOOL default '' .
  methods HANDLE_LEFT_CLICK
    for event LEFT_CLICK_RUN of CL_GUI_HTML_VIEWER .
  methods HANDLE_SAPEVENT
    for event SAPEVENT of CL_GUI_HTML_VIEWER
    importing
      !ACTION
      !FRAME
      !GETDATA
      !POSTDATA
      !QUERY_TABLE .
  methods ADD_URL_ACCION
    importing
      !TEXTO type ANY default ''
      !URL type ANY
      !PARRAFO type ABAP_BOOL default ''
      !CODIGO_PARRAFO type STRING default 'P' .
  class-methods REMOVE_TAGS
    importing
      !STRING_HTML type ANY
    returning
      value(STRING) type STRING .
  class-methods GRABAR_FICHERO_CACHE_SERVER
    importing
      !CONTENIDO type XSTRING
      !TIMEOUT type I default 120
      !HOST type ANY default ''
      !NOMBRE type ANY default ''
      !EXTENSION type ANY default ''
      !RUTA type STRING default '/sap/public/'
    returning
      value(URL) type STRING .
protected section.
private section.
endclass. "ZCL_AP_HTML definition
class ZCL_AP_HTML implementation.
METHOD add_parrafo.
  DATA: l_long      TYPE i, l_long2 TYPE i, l_long_ant TYPE i,
        i_lineas    TYPE TABLE OF text255,
        i_lineas2   TYPE TABLE OF text255,
        l_linea     TYPE text255,
        l_linea_ant TYPE text255,
        l_string    TYPE string.
  DATA: l_c(65535),
        l_c2(65535),
        l_cont      TYPE i,
        l_cont2     TYPE i,
        l_cont3     TYPE i,
        l_cont4     TYPE i,
        l_dif       TYPE i,
        c1,
        c2,
        c3,
        c4,
        l_borrar.

  l_long = STRLEN( texto ).

  IF l_long < 248 AND partir_siempre = ''.
    set_text( texto = '<P>' texto2 = texto texto3 = '</P>' ).
  ELSE.
    l_string = texto.

    IF optimizar = 'X' AND l_long < 65000 AND NOT texto CS '<!DOCTYPE HTML PUBLIC'.
      CLEAR l_long2.
      l_c    = l_string.
      l_long = STRLEN( l_c ).
      DO l_long TIMES.
        l_cont  = sy-index - 1 + l_dif.
        IF l_cont > l_long.
          EXIT.
        ENDIF.
        c1      = l_c+l_cont(1).
        l_cont2 = l_cont + 1.
        c2      = l_c+l_cont2(1).
        l_cont3 = l_cont + 2.
        c3      = l_c+l_cont3(1).
        l_cont4 = l_cont + 3.
        c4      = l_c+l_cont4(1).
        IF c3 = cl_abap_char_utilities=>cr_lf(1) AND c1 NE '.'.
          l_c2+l_long2(1) = c1.
          ADD 1 TO l_long2.
          ADD 2 TO l_dif.
        ELSE.
          l_c2+l_long2(1) = c1.
          ADD 1 TO l_long2.
        ENDIF.
      ENDDO.
      l_string = l_c2.
    ENDIF.


    zcl_ap_string=>string2tabla( EXPORTING string = l_string
                                           longitud = 248
                                 CHANGING  tabla = i_lineas ).

*    IF optimizar = 'X'.
*      CLEAR: l_long, l_long2, l_long_ant.
*      i_lineas2 = i_lineas.
*      clear i_lineas.
*      LOOP AT i_lineas2 INTO l_linea.
*        l_long2 = l_long2 + STRLEN( l_linea ).
*        IF l_long2 >= 200 AND l_long2 <= 248 AND NOT l_linea_ant IS INITIAL.
*          CONCATENATE l_linea_ant l_linea INTO l_linea SEPARATED BY space.
*          clear l_linea_ant.
*        ENDIF.
*
*        IF zcl_ap_string=>ultimo_caracter( l_linea ) = 'X'.
*          l_long2 = 999.
*        ENDIF.
*
*        IF l_long2 < 200.
*          IF l_linea_ant IS INITIAL.
*            l_linea_ant = l_linea.
*          ELSE.
*            CONCATENATE l_linea_ant l_linea INTO l_linea_ant SEPARATED BY space.
*          ENDIF.
*        ELSE.
*          IF NOT l_linea_ant IS INITIAL.
*            APPEND l_linea_ant TO i_lineas.
*            CLEAR: l_linea_ant.
*          ENDIF.
*          APPEND l_linea TO i_lineas.
*          CLEAR l_long2.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.

    IF texto CS '<!DOCTYPE HTML PUBLIC'.
      CLEAR l_borrar.
      LOOP AT i_lineas INTO l_linea.
        IF l_linea CS '<!DOCTYPE HTML '.
          l_borrar = 'X'.
        ELSEIF l_borrar = 'X'.
          IF l_linea CS '<BODY>'.
            CLEAR l_borrar.
            REPLACE '<BODY>' WITH '' INTO l_linea.
            set_text( texto = l_linea ).
          ENDIF.
        ELSE.
          REPLACE '</BODY></HTML>' WITH '' INTO l_linea.
          set_text( texto = l_linea ).
        ENDIF.
      ENDLOOP.
    ELSE.
      IF parrafo_unico IS INITIAL.
        LOOP AT i_lineas INTO l_linea.
          set_text( texto = '<P>' texto2 = l_linea texto3 = '</P>' ).
        ENDLOOP.
      ELSE.
        set_text( '<p>' ).
        LOOP AT i_lineas INTO l_linea.
          set_text( texto = l_linea  texto2 = '<br/>' ).
        ENDLOOP.
        set_text( '</p>' ).
      ENDIF.
    ENDIF.

  ENDIF.

ENDMETHOD.
method ADD_URL.
  data: l_texto          type string,
        l_linea          type string,
        l_inicio_parrafo type string,
        l_fin_parrafo    type string.

  if texto is initial.
    l_texto = url.
  else.
    l_texto = texto.
  endif.

  concatenate '<a href="' url '">' l_texto '</a>' into l_linea.
  if parrafo = 'X'.
    concatenate '<' codigo_parrafo '>' into l_inicio_parrafo.
    concatenate '</' codigo_parrafo '>' into l_fin_parrafo.
    concatenate l_inicio_parrafo l_linea l_fin_parrafo into l_linea.
  endif.

  set_text( l_linea ).

endmethod.
method ADD_URL_ACCION.
  data: l_texto          type string,
        l_linea          type string,
        l_inicio_parrafo type string,
        l_fin_parrafo    type string.

  if texto is initial.
    l_texto = url.
  else.
    l_texto = texto.
  endif.

  concatenate '<FORM method=post action=SAPEVENT:PRESS_ME?MYPARAMERS><a href="' 'SAPEVENT:CLICK_ON_ME?MYPARAMERS' '">' l_texto '</a></FORM>' into l_linea.
  if parrafo = 'X'.
    concatenate '<' codigo_parrafo '>' into l_inicio_parrafo.
    concatenate '</' codigo_parrafo '>' into l_fin_parrafo.
    concatenate l_inicio_parrafo l_linea l_fin_parrafo into l_linea.
  endif.

  set_text( l_linea ).

endmethod.
method CONSTRUCTOR.

  CLEAR: i_html, html.

  IF NOT container IS INITIAL OR NOT nombre_container IS INITIAL.
    set_container( container        = container
                   nombre_container = nombre_container ).
  ENDIF.

endmethod.
method FREE.

  CLEAR: i_html.
  IF NOT o_container IS INITIAL.
    o_container->free( ).
  ENDIF.

endmethod.
METHOD grabar_fichero_cache_server.
  DATA: lo_cached_response TYPE REF TO if_http_response,
        lv_app_type        TYPE string,
        lv_guid            TYPE guid_32,
        l_uuid_16          TYPE sysuuid_x16.

  CREATE OBJECT lo_cached_response
    TYPE
    cl_http_response
    EXPORTING
      add_c_msg = 1.

****set the data and the headers
  lo_cached_response->set_data( contenido ).
  CONCATENATE '.' extension INTO lv_app_type.
  TRANSLATE lv_app_type TO UPPER CASE.

  lo_cached_response->set_header_field( name  = if_http_header_fields=>content_type
                                        value = lv_app_type ).
****Set the Response Status
  lo_cached_response->set_status( code = 200 reason = 'OK' ).

****Set the Cache Timeout - 60 seconds - we only need this in the cache
****long enough to build the page
  lo_cached_response->server_cache_expire_rel( expires_rel = timeout ).

****Create a unique URL for the object and export URL
  IF nombre IS INITIAL.
*    CALL FUNCTION 'GUID_CREATE'
*      IMPORTING
*        ev_guid_32 = lv_guid.
    TRY.
        l_uuid_16 = cl_system_uuid=>create_uuid_x16_static( ).
        cl_system_uuid=>convert_uuid_x16_static( EXPORTING uuid = l_uuid_16
                                                 IMPORTING uuid_c32 = lv_guid ).
      CATCH cx_root.
    ENDTRY.
  ELSE.
    lv_guid = nombre.
  ENDIF.

  CONCATENATE  host ruta lv_guid '.' extension INTO url.

****Cache the URL
  lv_app_type = url.
  cl_http_server=>server_cache_upload( url      = lv_app_type
                                       response = lo_cached_response
                                       scope    =  ihttp_inv_global ).

ENDMETHOD.
method HANDLE_LEFT_CLICK.
*  BREAK-POINT.
endmethod.
method HANDLE_SAPEVENT.
*ACTION
*FRAME
*GETDATA
*POSTDATA
*QUERY_TABLE

*  BREAK-POINT.

endmethod.
method HEADER.

  data l_meta type string.
  set_text( '<head>' ).
  set_text( '<title>Documento sin t&iacute;tulo</title>' ).
  concatenate '<meta http-equiv="Content-Type" content="text/html;'
              'charset=' charset '">' into l_meta separated by space.
  set_text( l_meta ).
  set_text( '<style type="text/css">' ).
  set_text( '<!--' ).
  set_text( 'Estilo1 {font-family: Arial, Helvetica, sans-serif}' ).  "#EC *
  set_text( '-->' ).
  set_text( '</style>' ).
  set_text( '</head>' ).

  set_text( '<body class="Estilo1">' ).

endmethod.
method PRINT.

  CALL METHOD o_html->execwb
    EXPORTING
      cmd_id     = o_html->wb_cmdid_print
    EXCEPTIONS
      cntl_error = 1.
  IF sy-subrc <> 0.
    MESSAGE e003(cnht).
  ENDIF.

endmethod.
method REMOVE_TAGS.

  string = string_html.
  CALL FUNCTION 'SOTR_TAGS_REMOVE_FROM_STRING'
    CHANGING
      text = string.

endmethod.
method SET_CONTAINER.
  TYPES: BEGIN OF cntl_simple_event,
           eventid       TYPE i,
           appl_event(1) TYPE c,
         END OF cntl_simple_event.

  TYPES: cntl_simple_events TYPE TABLE OF cntl_simple_event.

  DATA: myevent_tab TYPE cntl_simple_events,
        myevent     TYPE cntl_simple_event.

  IF NOT container IS INITIAL.
    o_container = container.
  ELSEIF NOT nombre_container IS INITIAL.
    CREATE OBJECT o_container
      EXPORTING
        container_name = nombre_container.
  ENDIF.

  IF NOT o_container IS INITIAL.
    CREATE OBJECT o_html
      EXPORTING
        parent = o_container.
  ENDIF.

  o_html->set_ui_flag( o_html->uiflag_no3dborder ).

*    myevent-eventid = o_html->left_click_run.
*    myevent-appl_event = 'X'.
  myevent-eventid    = o_html->m_id_sapevent.
  myevent-appl_event = 'X'.
  APPEND myevent TO myevent_tab.
  CALL METHOD o_html->set_registered_events
    EXPORTING
      events = myevent_tab.


  SET HANDLER handle_left_click FOR o_html.
  SET HANDLER handle_sapevent FOR o_html.

endmethod.
method SET_PAGE.
  DATA l_url(2048).

  l_url = url.
  CALL METHOD o_html->load_data
    EXPORTING
      url        = l_url
    CHANGING
      data_table = i_html.

endmethod.
METHOD set_text.
  DATA: l_texto  TYPE w3html,
        l_texto2 TYPE w3html,
        l_tipo   TYPE c.

  IF tabla IS INITIAL.
    IF strlen( texto ) > 255 AND texto2 IS INITIAL AND texto3 IS INITIAL.
      DATA i_tabla TYPE TABLE OF w3html.
      zcl_ap_string=>string2tabla( EXPORTING string = texto
                                             longitud = 255
                                   CHANGING  tabla = i_tabla ).
      LOOP AT i_tabla INTO l_texto.
        APPEND l_texto TO i_html.
      ENDLOOP.
    ELSE.
      WRITE texto TO l_texto.

      IF texto2 IS SUPPLIED.
        DESCRIBE FIELD texto2 TYPE l_tipo.
        IF l_tipo = 'P'.
          WRITE texto2 TO l_texto2(15).
        ELSE.
          WRITE texto2 TO l_texto2.
        ENDIF.
        CONCATENATE l_texto l_texto2 INTO l_texto SEPARATED BY space.
      ENDIF.
      IF texto3 IS SUPPLIED.
        WRITE texto3 TO l_texto2.
        CONCATENATE l_texto l_texto2 INTO l_texto SEPARATED BY space.
      ENDIF.
      IF texto4 IS SUPPLIED.
        WRITE texto4 TO l_texto2.
        CONCATENATE l_texto l_texto2 INTO l_texto SEPARATED BY space.
      ENDIF.

      APPEND l_texto TO i_html.
    ENDIF.
  ELSE.
    i_html = tabla.
  ENDIF.


ENDMETHOD.
method SHOW.
  DATA l_url(2048).

  l_url = url.
  CALL METHOD o_html->show_url
    EXPORTING
      url = l_url.

endmethod.
