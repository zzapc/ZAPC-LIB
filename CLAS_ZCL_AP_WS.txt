CLASS zcl_ap_ws DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_name_pair,
        name  TYPE string,
        value TYPE string,
      END OF ty_name_pair.
    TYPES tt_name_pair TYPE STANDARD TABLE OF ty_name_pair.

    DATA o_http   TYPE REF TO if_http_client.
    DATA error    TYPE bapireturn1-message.
    DATA o_xml    TYPE REF TO cl_xml_document.
    DATA string   TYPE string.
    DATA host     TYPE string.
    DATA usuario  TYPE string.
    DATA password TYPE string.

    METHODS constructor
      IMPORTING host     TYPE string OPTIONAL
                service  TYPE string DEFAULT '80'
                scheme   TYPE i      DEFAULT cl_http_client=>schemetype_http
                username TYPE string OPTIONAL
                password TYPE string OPTIONAL.

    METHODS set_header_field
      IMPORTING !name  TYPE string
                !value TYPE string DEFAULT ''.

    METHODS request
      IMPORTING !method          TYPE string    DEFAULT 'POST'
                uri              TYPE string    OPTIONAL
                peticion         TYPE string    OPTIONAL
                !format          TYPE abap_bool DEFAULT 'X'
                !action          TYPE string    DEFAULT ''
                convert_to_utf8  TYPE abap_bool DEFAULT ''
                !encoding        TYPE any       DEFAULT 'utf-8'
                peticionx        TYPE xstring   OPTIONAL
                content_type     TYPE string    DEFAULT ''
                  PREFERRED PARAMETER method
      RETURNING VALUE(respuesta) TYPE string.

    CLASS-METHODS ver_xml
      IMPORTING !xml  TYPE string  DEFAULT ''
                x_xml TYPE xstring OPTIONAL
                  PREFERRED PARAMETER xml.

    CLASS-METHODS grabar_xml
      IMPORTING fichero              TYPE any
                !string              TYPE string    OPTIONAL
                mostrar_error        TYPE abap_bool DEFAULT 'X'
                dialogo              TYPE abap_bool DEFAULT ''
                xstring              TYPE xstring   OPTIONAL
                codepage             TYPE any       DEFAULT '4102'
                formatear_xml_salida TYPE abap_bool DEFAULT ''
      EXPORTING num_excepcion        TYPE i
                fichero_dialogo      TYPE any.

    METHODS close.

    METHODS set_xml
      IMPORTING !string      TYPE string
      RETURNING VALUE(subrc) TYPE sy-subrc.

    METHODS get_xml_value
      IMPORTING nodo         TYPE any
      RETURNING VALUE(valor) TYPE string.

    METHODS get_message
      IMPORTING !string        TYPE string
      RETURNING VALUE(mensaje) TYPE string.

    METHODS get_xml_value_direct
      IMPORTING nodo         TYPE any
      RETURNING VALUE(valor) TYPE string.

    CLASS-METHODS send_request
      IMPORTING iv_req_type TYPE string
                iv_url      TYPE string
                it_header   TYPE tt_name_pair OPTIONAL
                it_form     TYPE tt_name_pair OPTIONAL
      EXPORTING ev_response TYPE xstring.

    CLASS-METHODS xml_str2table
      IMPORTING !string        TYPE any
                limpiar        TYPE abap_bool DEFAULT ''
                max_long_linea TYPE int4      DEFAULT 0
      RETURNING VALUE(i_tabla) TYPE table_of_strings.

    CLASS-METHODS edit_xml_string
      IMPORTING formatear TYPE abap_bool DEFAULT ''
                editar    TYPE abap_bool DEFAULT ''
      CHANGING  !string   TYPE string.

    CLASS-METHODS get_fichero
      IMPORTING url          TYPE any
                get_string   TYPE abap_bool DEFAULT ''
                content_type TYPE string    DEFAULT ''
      EXPORTING !error       TYPE i
                !string      TYPE string
                xstring      TYPE xstring
                !message     TYPE bapi_msg.

    CLASS-METHODS xstring2solix
      IMPORTING xstring  TYPE xstring
      EXPORTING tabla    TYPE solix_tab
                longitud TYPE int4.

    CLASS-METHODS transformar_string
      IMPORTING !string        TYPE string
                transformacion TYPE any
      EXPORTING !xml           TYPE any
                mensaje        TYPE bapi_msg.

    CLASS-METHODS xstring2base64
      IMPORTING xstring       TYPE xstring OPTIONAL
                fichero       TYPE any     OPTIONAL
                  PREFERRED PARAMETER xstring
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS get_fichero_local
      IMPORTING url      TYPE any
      EXPORTING xstring  TYPE xstring
                !message TYPE bapi_msg.

    CLASS-METHODS get_fichero_fondo
      IMPORTING url          TYPE any
                rfcdest      TYPE rfcdes-rfcdest DEFAULT 'SAPHTTP'
                blankstocrlf TYPE char1          DEFAULT ''
      EXPORTING xstring      TYPE xstring
                !message     TYPE bapi_msg.

    CLASS-METHODS get_file
      IMPORTING url        TYPE any
                get_string TYPE abap_bool      DEFAULT ''
                rfcdest    TYPE rfcdes-rfcdest DEFAULT 'SAPHTTP'
      EXPORTING xstring    TYPE xstring
                !message   TYPE bapi_msg
                !string    TYPE string.

    CLASS-METHODS get_fichero_dest
      IMPORTING url          TYPE any       DEFAULT ''
                get_string   TYPE abap_bool DEFAULT ''
                content_type TYPE string    DEFAULT ''
                rfcdest      TYPE rfcdes-rfcdest
      EXPORTING !error       TYPE i
                !string      TYPE string
                xstring      TYPE xstring
                !message     TYPE bapi_msg.

  PROTECTED SECTION.
  PRIVATE SECTION.
endclass. "ZCL_AP_WS definition
class ZCL_AP_WS implementation.
  METHOD close.
    o_http->close(
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2 ).
    IF sy-subrc NE 0.
      MESSAGE 'Error cerrando conexión' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    IF host IS INITIAL.
      RETURN.
    ENDIF.

    cl_http_client=>create(
      EXPORTING
        host    = host
        service = service
        scheme  = scheme
      IMPORTING
        client  = o_http ).

    o_http->propertytype_logon_popup = o_http->co_disabled.

    IF NOT username IS INITIAL.
      o_http->authenticate(
          username = username
          password = password ).
    ENDIF.
  ENDMETHOD.
  METHOD edit_xml_string.
    DATA: i_string TYPE TABLE OF string,
          l_string TYPE string,
          l_mod    TYPE abap_bool.

    IF formatear = 'X'.
      i_string = xml_str2table( string ).
      l_string = zcl_ap_string=>tabla2string( tabla = i_string ).
    ELSE.
      l_string = string.
    ENDIF.

*  l_string = zcl_ap_string=>editor_popup_string( string = l_string titulo = 'Edición XML' ). "#EC *
*  IF l_string NE '#!#'.
*    string = l_string.
*  ENDIF.

    zcl_ap_string=>popup_texto( EXPORTING titulo = 'Edición XML'
                                          editar = editar
                                IMPORTING modificado = l_mod
                                CHANGING  texto = l_string ).
    IF l_mod = 'X'.
      string = l_string.
    ENDIF.
  ENDMETHOD.
  METHOD get_fichero.
    DATA l_url          TYPE string.
    DATA lo_http_client TYPE REF TO if_http_client.

    l_url = url.
    cl_http_client=>create_by_url(
      EXPORTING
        url                = l_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3 ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.

    lo_http_client->request->set_header_field( name  = '~request_method'
                                               value = 'GET' ).

    lo_http_client->request->set_header_field( name  = '~server_protocol'
                                               value = 'HTTP/1.1' ).

    IF content_type <> ''.
      lo_http_client->request->set_header_field(
          name  = 'Content-Type'                            "#EC *
          value = content_type ).
    ENDIF.

    lo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2 ).

    IF sy-subrc <> 0.
      lo_http_client->get_last_error(
        IMPORTING
          code    = DATA(lv_return_code)
          message = DATA(lv_message) ).
      message = lv_message.
      error = lv_return_code.
    ELSE.
      lo_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3 ).

      IF sy-subrc <> 0.
        lo_http_client->get_last_error(
          IMPORTING
            code    = lv_return_code
            message = lv_message ).
        message = lv_message.
        error = lv_return_code.
      ENDIF.

      lo_http_client->response->get_status( IMPORTING code = error ).

      IF error <> 200.
        RETURN.
      ENDIF.

      xstring = lo_http_client->response->get_data( ).

      IF NOT xstring IS INITIAL AND get_string = 'X'.
        string = zcl_ap_string=>xstring2string( xstring ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_fichero_dest.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_url          TYPE string.
    DATA lo_http_client TYPE REF TO if_http_client.

    IF rfcdest IS INITIAL.
      zcl_ap_ws=>get_file(
        EXPORTING
          url        = url
          get_string = get_string
        IMPORTING
          xstring    = xstring
          message    = message
          string     = string ).
      RETURN.
    ENDIF.

    l_url = url.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = rfcdest
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5 ).

    IF sy-subrc <> 0.
      message = |Error { sy-subrc } creando conexión|.
    ENDIF.

    IF lo_http_client IS INITIAL.
      message = |Error creando conexión. No se ha instanciado el objeto|.
    ENDIF.

    IF NOT url IS INITIAL.
      cl_http_utility=>set_request_uri(
          request = lo_http_client->request
          uri     = url ).
    ENDIF.

    lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.

    lo_http_client->request->set_header_field( name  = '~request_method'
                                               value = 'GET' ).

    lo_http_client->request->set_header_field( name  = '~server_protocol'
                                               value = 'HTTP/1.1' ).

    lo_http_client->request->set_header_field( name  = 'cache-control'
                                               value = 'no-cache' ).

    IF content_type <> ''.
      lo_http_client->request->set_header_field(
          name  = 'Content-Type'                            "#EC *
          value = content_type ).
    ENDIF.

    lo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2 ).

    IF sy-subrc <> 0.
      lo_http_client->get_last_error(
        IMPORTING
          code    = DATA(lv_return_code)
          message = DATA(lv_message) ).
      message = lv_message.
      error = lv_return_code.
    ELSE.
      lo_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3 ).

      IF sy-subrc <> 0.
        lo_http_client->get_last_error(
          IMPORTING
            code    = lv_return_code
            message = lv_message ).
        message = lv_message.
        error = lv_return_code.
      ENDIF.

      lo_http_client->response->get_status( IMPORTING code = error ).

      IF error <> 200.
        RETURN.
      ENDIF.

      xstring = lo_http_client->response->get_data( ).

      IF NOT xstring IS INITIAL AND get_string = 'X'.
        string = zcl_ap_string=>xstring2string( xstring ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_fichero_fondo.
    TYPES: BEGIN OF text,
             line TYPE c LENGTH 256,
           END OF text.

    DATA l_url TYPE text4096.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: status     TYPE c LENGTH 3,
          " TODO: variable is assigned but never used (ABAP cleaner)
          statustext TYPE c LENGTH 128,
          rlength    TYPE i.
    DATA: response         TYPE TABLE OF text,
          response_headers TYPE TABLE OF text.

    l_url = url.

    CALL FUNCTION 'HTTP_GET'
      EXPORTING
        absolute_uri                = l_url
        rfc_destination             = rfcdest
        blankstocrlf                = blankstocrlf
*       USER                        = 'xxx' - Specify your user here
*       Password                    = 'yyy' - Specify your password here
      IMPORTING
        status_code                 = status
        status_text                 = statustext
        response_entity_body_length = rlength
      TABLES
        response_entity_body        = response
        response_headers            = response_headers
      EXCEPTIONS
        connect_failed              = 1
        timeout                     = 2
        internal_error              = 3
        tcpip_error                 = 4
        data_error                  = 5
        system_failure              = 6
        communication_failure       = 7
        error_message               = 999 " Cualquier otra excepción!
        OTHERS                      = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH
              sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ENDIF.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination = rfcdest
      EXCEPTIONS
        OTHERS      = 0.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = rlength " 999999999
*       FIRST_LINE   = 0
*       LAST_LINE    = 0
      IMPORTING
        buffer       = xstring
      TABLES
        binary_tab   = response
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH
              sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ENDIF.
  ENDMETHOD.
  METHOD get_fichero_local.
    TYPES ole2_parameter TYPE swcbcont-value.
    TYPES: BEGIN OF cndp_user_info,
             user          TYPE ole2_parameter, " Username am Server
             password      TYPE ole2_parameter, " Password am Server
             proxy         TYPE ole2_parameter, " Proxy (incl. Port)
             proxyuser     TYPE ole2_parameter, " User am Proxy
             proxypassword TYPE ole2_parameter, " Password am Proxy
             scrambled     TYPE c LENGTH 1,     " Flag ob verschlüsselt
           END OF cndp_user_info.

    DATA: l_url           TYPE saeuri,
          user_info       TYPE cndp_user_info,
          l_size          TYPE i,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_errorstate    TYPE i,
          content_bin_255 TYPE solix_tab.

    CLEAR: xstring, message.

    l_url = url.

    CALL FUNCTION 'DP_GET_STREAM_FROM_URL'
      EXPORTING
        url            = l_url
        userinfo       = user_info
      IMPORTING
        size           = l_size
        errorstate     = l_errorstate
      TABLES
        data           = content_bin_255
      EXCEPTIONS
        dp_fail        = 1
        dp_failed_init = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      CALL FUNCTION 'AC_FLUSH_CALL'
        EXPORTING
          system_flush = 'X'.

      message = 'URL no existe'(une).
    ENDIF.

    IF content_bin_255 IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = l_size
*       FIRST_LINE   = 0
*       LAST_LINE    = 0
      IMPORTING
        buffer       = xstring
      TABLES
        binary_tab   = content_bin_255
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      message = 'Error recuperando contenido'(erc).
    ENDIF.

* Pruebas
*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
*      bin_filesize            = l_size
*      filename                = 'c:\temp\p2.xlsx'
*      filetype                = 'BIN'
*    TABLES
*      data_tab                = content_bin_255
*    EXCEPTIONS
*      file_write_error        = 1
*      no_batch                = 2
*      gui_refuse_filetransfer = 3
*      invalid_type            = 4
*      no_authority            = 5
*      unknown_error           = 6
*      header_not_allowed      = 7
*      separator_not_allowed   = 8
*      filesize_not_allowed    = 9
*      header_too_long         = 10
*      dp_error_create         = 11
*      dp_error_send           = 12
*      dp_error_write          = 13
*      unknown_dp_error        = 14
*      access_denied           = 15
*      dp_out_of_memory        = 16
*      disk_full               = 17
*      dp_timeout              = 18
*      file_not_found          = 19
*      dataprovider_exception  = 20
*      control_flush_error     = 21
*      OTHERS                  = 22.
  ENDMETHOD.
  METHOD get_file.
    IF sy-batch IS INITIAL.
      get_fichero_local( EXPORTING url = url IMPORTING xstring = xstring message = message ).
    ELSE.
      get_fichero_fondo( EXPORTING url = url rfcdest = rfcdest IMPORTING xstring = xstring message = message ).
    ENDIF.

    IF get_string = 'X'.
      string = zcl_ap_string=>xstring2string( xstring ).
    ENDIF.
  ENDMETHOD.
  METHOD get_message.
    CLEAR mensaje.
    IF set_xml( string ) = 0.
      mensaje = get_xml_value( 'faultstring' ).             "#EC *
      IF mensaje IS INITIAL.
        mensaje = get_xml_value( 'title' ).                 "#EC *
        IF mensaje IS INITIAL.
          mensaje = get_xml_value( 'Detail' ).              "#EC *
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_xml_value.
    DATA node TYPE REF TO if_ixml_node.

    CHECK NOT o_xml IS INITIAL.

    node = o_xml->find_node( name = nodo ).

    IF node IS INITIAL.
      RETURN.
    ENDIF.

    valor = node->get_value( ).
  ENDMETHOD.
  METHOD get_xml_value_direct.
    DATA: l_aux  TYPE string,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_eti1 TYPE string.

    CLEAR valor.
    IF string NS nodo.
      RETURN.
    ENDIF.

    CONCATENATE nodo '>' INTO l_aux.
    IF string CS l_aux.
      SPLIT string AT l_aux INTO l_eti1 valor.
      IF sy-subrc = 0 OR NOT string CS l_aux.
        CONCATENATE '</' nodo INTO l_aux.
        IF valor CS l_aux.
          SPLIT valor AT l_aux INTO valor l_eti1.
        ENDIF.
      ELSE.
        CONCATENATE '</' nodo INTO l_aux.
        IF string CS l_aux.
          SPLIT string AT l_aux INTO valor l_eti1.
        ENDIF.
      ENDIF.
    ELSE.
      SPLIT string AT nodo INTO l_eti1 valor.
      IF valor CS '"/'.
        SPLIT valor AT '"/' INTO valor l_eti1.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD grabar_xml.
    " TODO: parameter MOSTRAR_ERROR is never used (ABAP cleaner)
    " TODO: parameter NUM_EXCEPCION is never cleared or assigned (ABAP cleaner)

    DATA i_string TYPE TABLE OF string.

    IF formatear_xml_salida IS INITIAL.
      zcl_ap_ficheros=>grabar_xml( EXPORTING fichero  = fichero
                                             string   = string
                                             xstring  = xstring
                                             dialogo  = dialogo
                                             codepage = codepage
                                   IMPORTING fichero_dialogo = fichero_dialogo ).
    ELSE.
      i_string = xml_str2table( string ).

      zcl_ap_ficheros=>grabar( EXPORTING fichero  = fichero
                                         dialogo  = dialogo
                                         codepage = codepage
                               CHANGING  tabla    = i_string
                                         fichero_dialogo = fichero_dialogo ).
    ENDIF.
  ENDMETHOD.
  METHOD request.
    DATA: l_aux     TYPE string,
          l_lon     TYPE i,
          l_string  TYPE string,
          l_lon_txt TYPE string,
          l_xstring TYPE xstring,
          subrc     TYPE i,
          errortext TYPE string.
    DATA cvto_utf8 TYPE REF TO cl_abap_conv_out_ce.

    o_http->request->set_header_field(
        name  = '~request_method'                           "#EC *
        value = method ).

    o_http->request->set_header_field(
        name  = '~server_protocol'                          "#EC *
        value = 'HTTP/1.1' ).                               "#EC *

    o_http->request->set_header_field(
        name  = '~request_uri'                              "#EC *
        value = uri ).

    IF NOT action IS INITIAL.
      o_http->request->set_header_field(
          name  = 'SOAPAction'                              "#EC *
          value = action ).
    ENDIF.

    IF content_type IS INITIAL.
      CONCATENATE 'text/xml; charset=' encoding INTO l_aux. "#EC *
    ELSE.
      l_aux = content_type.
    ENDIF.
    o_http->request->set_header_field(
        name  = 'Content-Type'                              "#EC *
        value = l_aux ).

    IF convert_to_utf8 IS INITIAL AND peticionx IS INITIAL.
      IF NOT peticion IS INITIAL.
        l_lon = strlen( peticion ).

        l_string = peticion.
        l_lon_txt = l_lon.

        o_http->request->set_header_field(
            name  = 'Content-Length'                        "#EC *
            value = l_lon_txt ).

        o_http->request->set_cdata(
            data   = l_string
            offset = 0
            length = l_lon ).
      ENDIF.
    ELSE.
      IF convert_to_utf8 IS INITIAL.
        l_xstring = peticionx.
      ELSE.
        cvto_utf8 = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
        cvto_utf8->write( data = peticion ).
        l_xstring = cvto_utf8->get_buffer( ).
      ENDIF.

      l_lon = xstrlen( l_xstring ).
      l_lon_txt = l_lon.

      o_http->request->set_header_field(
          name  = 'Content-Length'                          "#EC *
          value = l_lon_txt ).

      o_http->request->set_data(
          data   = l_xstring
          offset = 0
          length = l_lon ).
    ENDIF.

    o_http->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2 ).
    IF sy-subrc <> 0.
      o_http->get_last_error(
        IMPORTING
          code    = subrc
          message = errortext ).
    ENDIF.

    o_http->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3 ).
    IF sy-subrc <> 0.
      o_http->get_last_error(
        IMPORTING
          code    = subrc
          message = errortext ).
    ENDIF.

    CLEAR respuesta.
    respuesta = o_http->response->get_cdata( ).
    string = respuesta.

    IF respuesta IS INITIAL.
      respuesta = subrc.
      CONCATENATE respuesta errortext INTO respuesta SEPARATED BY space.
    ENDIF.

    IF format = 'X'.
      REPLACE ALL OCCURRENCES OF '&lt;' IN respuesta WITH '<'.
      REPLACE ALL OCCURRENCES OF '&gt;' IN respuesta WITH '>'.
    ENDIF.
  ENDMETHOD.
  METHOD send_request.
    DATA lo_http_client TYPE REF TO if_http_client.

    FIELD-SYMBOLS: <header> TYPE ty_name_pair,
                   <form>   TYPE ty_name_pair.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    ASSERT sy-subrc = 0.

    lo_http_client->request->set_method( iv_req_type ).

    LOOP AT it_header ASSIGNING <header>.
      lo_http_client->request->set_header_field( name = <header>-name value = <header>-value ).
    ENDLOOP.

    LOOP AT it_form ASSIGNING <form>.
      lo_http_client->request->set_form_field( name = <form>-name value = <form>-value ).
    ENDLOOP.

    lo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3 ).

    ASSERT sy-subrc = 0.

    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3 ).

    ASSERT sy-subrc = 0.

    ev_response = lo_http_client->response->get_data( ).

* DOC in http://scn.sap.com/community/abap/blog/2014/02/24/generate-qrcode-via-abap-with-help-of-online-service
*DATA:
*     ls_form   type zcl_http_tool=>ty_name_pair,
*     lt_form   type zcl_http_tool=>tt_name_pair,
*     lv_code   type xstring.
*ls_form = value #( name = 'type' value = 'TEXT' ).
*APPEND ls_form TO lt_form.
*ls_form = value #( name = 'foreground_color' value = '000000' ).
*APPEND ls_form TO lt_form.
*ls_form = value #( name = 'ecc_level' value = 'L' ).
*APPEND ls_form TO lt_form.
*ls_form = value #( name = 'width_pixels' value = '200' ).
*APPEND ls_form TO lt_form.
*ls_form = value #( name = 'dpi' value = '72' ).
*APPEND ls_form TO lt_form.
*ls_form = value #( name = 'file_type' value = 'png' ).
*APPEND ls_form TO lt_form.
*ls_form = value #( name = 'text' value = 'testforQRCode' ).
*APPEND ls_form TO lt_form.
*zcl_http_tool=>send_request( EXPORTING iv_url      = 'http://www.qrstuff.com/generate.generate'
*                                       iv_req_type = if_http_request=>co_request_method_get
*                                       it_form     = lt_form
*IMPORTING ev_response = lv_code ).
  ENDMETHOD.
  METHOD set_header_field.
    o_http->request->set_header_field(
        name  = name
        value = value ).
  ENDMETHOD.
  METHOD set_xml.
    o_xml = NEW #( ).
    subrc = o_xml->parse_string( string ).
    me->string = string.
  ENDMETHOD.
  METHOD transformar_string.
    DATA: l_err_conv    TYPE REF TO cx_sy_conversion_data_loss,
          l_err_trans   TYPE REF TO cx_invalid_transformation,
          l_err_match   TYPE REF TO cx_st_match_element,
          l_err_call_st TYPE REF TO cx_call_st_error.

    IF transformacion IS INITIAL.
      mensaje = 'No es posible generar línea'(ngl).
    ELSEIF string IS INITIAL.
      mensaje = 'Mensaje vacío'(mva).
    ELSE.
      TRY.
          CALL TRANSFORMATION (transformacion)
               SOURCE XML string
               RESULT cab = xml.

        CATCH cx_sy_conversion_data_loss INTO l_err_conv.
          mensaje = l_err_conv->get_text( ).
          CONCATENATE 'T:' transformacion mensaje INTO mensaje SEPARATED BY space.
          EXIT.
        CATCH cx_invalid_transformation INTO l_err_trans.
          mensaje = l_err_trans->get_text( ).
          CONCATENATE 'T:' transformacion mensaje INTO mensaje SEPARATED BY space.
          EXIT.
        CATCH cx_st_match_element INTO l_err_match.
          mensaje = l_err_match->get_text( ).
          CONCATENATE 'T:' transformacion mensaje INTO mensaje SEPARATED BY space.
          EXIT.
        CATCH cx_call_st_error INTO l_err_call_st.
          mensaje = l_err_call_st->get_text( ).
          CONCATENATE 'T:' transformacion mensaje INTO mensaje SEPARATED BY space.
          EXIT.
      ENDTRY.

      IF xml IS INITIAL.
        CONCATENATE 'Error en la transformación'(etr) transformacion INTO mensaje SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD ver_xml.
*  IF NOT xml IS INITIAL.
*    l_xml = xml.
*  ELSE.
*    l_xml = zcl_ap_string=>xstring2string( x_xml ).
*  ENDIF.
*    call function 'SCOL_TRACE_SHOW_XML'
*      exporting
*        doc = xml.
    DATA gcl_xml TYPE REF TO cl_xml_document.

    gcl_xml = NEW #( ).

*Parses XML String to DOM
    IF NOT xml IS INITIAL.
      gcl_xml->parse_string(
          stream = xml ).
    ELSE.
      gcl_xml->parse_xstring(
          stream = x_xml ).
    ENDIF.

*Display XML
    gcl_xml->display( ).
*  ELSE.
*    CALL FUNCTION 'SCOL_TRACE_SHOW_XML'
*      EXPORTING
*        xdoc = x_xml.
*  ENDIF.
  ENDMETHOD.
  METHOD xml_str2table.
    DATA: i_xmlstring  TYPE string,
          l_cnt_length TYPE i,
          l_cnt_index  TYPE i,
          l_char       TYPE c LENGTH 1,
          l_nextchar   TYPE c LENGTH 1,
          l_indent     TYPE i,
          l_oldchar    TYPE c LENGTH 1,
          l_xmlline    TYPE string.

    i_xmlstring = string.
    l_cnt_length = strlen( i_xmlstring ).

    DO l_cnt_length TIMES.
      l_cnt_index = sy-index - 1.
      l_char = i_xmlstring+l_cnt_index(1).
      IF sy-index < l_cnt_length.
        l_nextchar = i_xmlstring+sy-index(1).
      ENDIF.
      IF l_char <> cl_abap_char_utilities=>newline.

        IF    ( l_char = '<' AND l_nextchar = '/' )
           OR ( l_char = '/' AND l_nextchar = '>' ).
          l_indent = l_indent - 1.
          IF l_indent < 0.
            l_indent = 0.
          ENDIF.
        ENDIF.

        IF l_char = '<' AND l_oldchar = '>'.
          APPEND l_xmlline TO i_tabla.
*      write: / l_xmlline.
          CLEAR l_xmlline.
          DO l_indent TIMES.
            CONCATENATE space space l_xmlline INTO l_xmlline SEPARATED BY space.
          ENDDO.
        ENDIF.
        IF l_char <> space.
          CONCATENATE l_xmlline l_char INTO l_xmlline.
        ELSE.
          CONCATENATE l_xmlline l_char INTO l_xmlline SEPARATED BY space.
        ENDIF.

        IF l_char = '<' AND l_nextchar <> '/'.
          l_indent = l_indent + 1.
        ENDIF.

      ELSE.
        l_indent = 0.
        APPEND l_xmlline TO i_tabla.
        CLEAR l_xmlline.
      ENDIF.
      l_oldchar = l_char.
    ENDDO.
    APPEND l_xmlline TO i_tabla.

    IF limpiar = 'X'.
      LOOP AT i_tabla ASSIGNING FIELD-SYMBOL(<tabla>).
        DATA(l_long) = strlen( <tabla> ).
        IF l_long > 1.
          l_long = l_long - 1.
          l_char = <tabla>+l_long(1).
          IF l_char = cl_abap_char_utilities=>cr_lf(1).
            <tabla> = <tabla>(l_long).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT max_long_linea IS INITIAL.
      DATA(i_t2) = i_tabla.
      CLEAR i_tabla.
      LOOP AT i_t2 ASSIGNING <tabla>.
        DO.
          l_long = strlen( <tabla> ).
          IF l_long > max_long_linea.
            l_xmlline = <tabla>(max_long_linea).
            APPEND l_xmlline TO i_tabla.
            <tabla> = <tabla>+max_long_linea.
            IF <tabla> IS INITIAL.
              EXIT.
            ENDIF.
          ELSE.
            l_xmlline = <tabla>.
            APPEND l_xmlline TO i_tabla.
            EXIT.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD xstring2base64.
    DATA l_xstring TYPE xstring.

    IF NOT xstring IS INITIAL.
      l_xstring = xstring.
    ELSEIF NOT fichero IS INITIAL.
      zcl_ap_ficheros=>leer_xstring( EXPORTING fichero = fichero  IMPORTING xstring = l_xstring ).
    ENDIF.

    IF l_xstring IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        bindata                  = l_xstring
*       BINLENG                  =
      IMPORTING
        b64data                  = string
      EXCEPTIONS
        ssf_krn_error            = 1
        ssf_krn_noop             = 2
        ssf_krn_nomemory         = 3
        ssf_krn_opinv            = 4
        ssf_krn_input_data_error = 5
        ssf_krn_invalid_par      = 6
        ssf_krn_invalid_parlen   = 7
        OTHERS                   = 8.
    IF sy-subrc NE 0.
      MESSAGE 'Error convirtiendo a base 64' TYPE 'E'.
    ENDIF.

  ENDMETHOD.
  METHOD xstring2solix.
    DATA: i_tabla TYPE enh_version_management_hex_tb,
          l_solix TYPE solix.

    FIELD-SYMBOLS <tab> TYPE enh_version_management_hex.

    CALL FUNCTION 'ENH_XSTRING_TO_TAB'
      EXPORTING
        im_xstring = xstring
      IMPORTING
        ex_data    = i_tabla
        ex_leng    = longitud.

    LOOP AT i_tabla ASSIGNING <tab>.
      l_solix-line = <tab>.
      APPEND l_solix TO tabla.
    ENDLOOP.
  ENDMETHOD.
