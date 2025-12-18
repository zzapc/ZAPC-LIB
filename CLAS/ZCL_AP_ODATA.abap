CLASS zcl_ap_odata DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA host               TYPE string.
    DATA usuario            TYPE string.
    DATA password           TYPE string.
    DATA peticion_soap      TYPE string.
    DATA nivel_formato_soap TYPE i.
    DATA destination        TYPE char40.
    DATA ssl_id             TYPE ssfapplssl.
    DATA proceso_log        TYPE zproceso_log.
    DATA auth               TYPE string.
    DATA i_header           TYPE tihttpnvp.
    DATA prefijo_tag        TYPE string.
    DATA respuesta          TYPE string.

    METHODS constructor
      IMPORTING !host        TYPE any DEFAULT ''
                usuario      TYPE any DEFAULT ''
                password     TYPE any DEFAULT ''
                !destination TYPE any DEFAULT ''
                ssl_id       TYPE any DEFAULT ''
                proceso_log  TYPE any DEFAULT ''
                auth         TYPE any DEFAULT ''
                prefijo_tag  TYPE any DEFAULT ''.

    METHODS get_odata
      IMPORTING servicio        TYPE any
                entidad         TYPE any                 OPTIONAL
                odata_sap       TYPE abap_bool           DEFAULT 'X'
                apikey          TYPE string              DEFAULT ''
                parametros      TYPE any                 DEFAULT ''
                popup_respuesta TYPE abap_bool           DEFAULT ''
                !top            TYPE i                   DEFAULT 0
                !filter         TYPE string              DEFAULT ''
                campos          TYPE string              DEFAULT ''
                !key            TYPE string              DEFAULT ''
                get_tabla       TYPE abap_bool           DEFAULT ''
                get_datos       TYPE abap_bool           DEFAULT ''
                get_valor       TYPE any                 DEFAULT ''
                !method         TYPE any                 DEFAULT 'GET'
                peticion        TYPE string              DEFAULT ''
                proxy_host      TYPE string              DEFAULT ''
                proxy_service   TYPE string              DEFAULT ''
                content_type    TYPE any                 DEFAULT 'application/json'
                !accept         TYPE any                 DEFAULT ''
                i_form          TYPE apb_lpd_t_key_value OPTIONAL
                peticionx       TYPE xstring             OPTIONAL
      EXPORTING !message        TYPE string
                respuesta       TYPE string
                !subrc          TYPE i
                tabla           TYPE table
                datos           TYPE any
                http_code       TYPE i
                http_status     TYPE string
                xstring         TYPE xstring.

    METHODS call_soap
      IMPORTING servicio        TYPE any
                api_sap         TYPE abap_bool DEFAULT ''
                apikey          TYPE string    DEFAULT ''
                parametros      TYPE any       DEFAULT ''
                popup_respuesta TYPE abap_bool DEFAULT ''
                get_tabla       TYPE abap_bool DEFAULT ''
                sap_client      TYPE string    DEFAULT ''
                !method         TYPE string    DEFAULT 'POST'
                !action         TYPE string    DEFAULT ''
                peticion        TYPE string    DEFAULT ''
                convert_to_utf8 TYPE abap_bool DEFAULT ''
                !encoding       TYPE any       DEFAULT 'utf-8'
                peticionx       TYPE xstring   OPTIONAL
                content_type    TYPE string    DEFAULT ''
                proxy_host      TYPE string    DEFAULT ''
                proxy_service   TYPE string    DEFAULT ''
      EXPORTING reason          TYPE string
                !message        TYPE string
                respuesta       TYPE string
                !subrc          TYPE i
                !status         TYPE string
                status_i        TYPE i.

    METHODS ini_pet_soap
      IMPORTING envelope TYPE any       DEFAULT 'X'
                !header  TYPE abap_bool DEFAULT 'X'
                body     TYPE abap_bool DEFAULT 'X'.

    METHODS add_tag_soap
      IMPORTING tag             TYPE any
                valor           TYPE any       DEFAULT ''
                abrir           TYPE abap_bool DEFAULT ''
                cerrar          TYPE abap_bool DEFAULT ''
                subvalor        TYPE any       DEFAULT ''
                formato_importe TYPE any       DEFAULT ''
                !escape         TYPE abap_bool DEFAULT ''
                add_hora        TYPE abap_bool DEFAULT ''.

    METHODS fin_pet_soap
      IMPORTING envelope TYPE abap_bool DEFAULT 'X'
                body     TYPE abap_bool DEFAULT 'X'.

    CLASS-METHODS get_elementos_xml
      IMPORTING elementos      TYPE string OPTIONAL
                !xml           TYPE string
      CHANGING  datos          TYPE any    OPTIONAL
      RETURNING VALUE(i_datos) TYPE apb_lpd_t_key_value.

    METHODS get_atributo
      IMPORTING atributo     TYPE string
      RETURNING VALUE(valor) TYPE string.


endclass. "ZCL_AP_ODATA definition
class ZCL_AP_ODATA implementation.
  METHOD add_tag_soap.
    DATA l_nivel_inicial TYPE i.
    DATA l_aux           TYPE string.
    DATA l_string        TYPE string.
    DATA l_valor         TYPE text4096.
    DATA l_nesp          TYPE i.

    l_nivel_inicial = nivel_formato_soap.

    IF subvalor IS INITIAL.
      l_aux = tag.
    ELSE.
      CONCATENATE tag subvalor INTO l_aux SEPARATED BY space.
    ENDIF.

    IF prefijo_tag IS NOT INITIAL AND NOT tag CS 'soapenv:'.
      CONCATENATE prefijo_tag l_aux INTO l_aux.
    ENDIF.

    IF abrir = 'X' AND cerrar = 'X'.
      CONCATENATE '<' l_aux '/>' INTO l_string.
    ELSEIF abrir = 'X'.
      CONCATENATE '<' l_aux '>' INTO l_string.
      nivel_formato_soap = nivel_formato_soap + 1.
    ELSEIF cerrar = 'X'.
      CONCATENATE '</' l_aux '>' INTO l_string.
      IF nivel_formato_soap > 0.
        nivel_formato_soap = nivel_formato_soap - 1.
        l_nivel_inicial = nivel_formato_soap.
      ENDIF.
    ELSE.
      DESCRIBE FIELD valor TYPE DATA(l_tipo).
      CASE l_tipo.
        WHEN 'D'.
          CONCATENATE valor(4) valor+4(2) valor+6(2) INTO l_valor SEPARATED BY '-'.
          IF add_hora = 'X'.
            CONCATENATE l_valor 'T00:00:00Z' INTO l_valor.
          ENDIF.
        WHEN 'P'.
          IF formato_importe IS INITIAL.
            WRITE valor TO l_valor.                         "#EC *
            CONDENSE l_valor.
          ELSE.
            CASE formato_importe.
              WHEN '-99999.999'.
                l_valor = abs( valor ).
                CONDENSE l_valor NO-GAPS.
                IF valor < 0.
                  CONCATENATE '-' l_valor INTO l_valor.
                ENDIF.
            ENDCASE.
          ENDIF.
        WHEN OTHERS.
          WRITE valor TO l_valor.                           "#EC *
          CONDENSE l_valor.

          IF escape IS NOT INITIAL.
            l_string = l_valor.
            IF escape = 'L'.
              zcl_ap_string=>quitar_caracteres_extranos( CHANGING string = l_string ).
            ELSEIF escape = 'X'.
              l_string = cl_http_utility=>if_http_utility~escape_url( unescaped = l_string ).
            ENDIF.
            IF l_string <> l_valor.
              l_valor = l_string.
            ENDIF.
          ENDIF.
      ENDCASE.

**      CONCATENATE '<' l_aux '>' l_valor '</' l_aux '>' INTO l_string. "APC20241125
      CONCATENATE '<' l_aux '>' l_valor '</' tag '>' INTO l_string.     " APC20241125
    ENDIF.

    IF l_nivel_inicial > 0.
      l_nesp = 3 * l_nivel_inicial.

      DO l_nesp TIMES.
        l_string = | { l_string }|.
      ENDDO.
    ENDIF.

    IF l_string IS NOT INITIAL.
      IF peticion_soap IS INITIAL.
        peticion_soap = l_string.
      ELSE.
        CONCATENATE peticion_soap l_string INTO peticion_soap SEPARATED BY cl_abap_char_utilities=>cr_lf.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD call_soap.
    " TODO: parameter GET_TABLA is never used (ABAP cleaner)

    DATA l_url          TYPE string.
    DATA l_string       TYPE string.
    DATA l_parametros   TYPE string.
    DATA lo_http_client TYPE REF TO if_http_client.
    DATA l_xstring      TYPE xstring.
    DATA l_lon_txt      TYPE string.
    DATA cvto_utf8      TYPE REF TO cl_abap_conv_out_ce.

    CLEAR: status,
           status_i,
           reason,
           message,
           subrc,
           respuesta.

    " create HTTP client by url
    " API endpoint for API sandbox

    l_url = host.
    IF api_sap = 'X'.
      CONCATENATE l_url '/sap/bc/srt/scs_ext/sap/' INTO l_url.
    ENDIF.

    CONCATENATE l_url servicio INTO l_url SEPARATED BY '/'.

    SPLIT l_url AT '//' INTO l_url l_string.
    REPLACE ALL OCCURRENCES OF '//' IN l_string WITH '/'.
    CONCATENATE l_url '//' l_string INTO l_url.

    IF parametros IS NOT INITIAL.
      l_parametros = parametros.
    ENDIF.

    IF sap_client IS NOT INITIAL.
      l_string = |&$sap_client={ sap_client }|.
      CONCATENATE l_parametros l_string INTO l_parametros.
    ENDIF.

    IF l_parametros IS NOT INITIAL.
      CONCATENATE l_url '?' l_parametros INTO l_url.
    ENDIF.

    cl_http_client=>create_by_url( EXPORTING  url                = l_url
                                              ssl_id             = ssl_id
                                              proxy_host         = proxy_host
                                              proxy_service      = proxy_service
                                   IMPORTING  client             = lo_http_client
                                   EXCEPTIONS argument_not_found = 1
                                              plugin_not_active  = 2
                                              internal_error     = 3
                                              OTHERS             = 4 ).

    IF sy-subrc <> 0.
      message = 'Error al abrir conexion'.
      CASE sy-subrc.
        WHEN 1. message = |{ message }Argumento no encontrado|.
        WHEN 2. message = |{ message }Plugin no activo|.
        WHEN 3. message = |{ message }Error interno|.
        WHEN OTHERS.
          IF sy-msgty IS NOT INITIAL.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_string.
            message = |{ message } { l_string }|.
          ENDIF.
      ENDCASE.
      RETURN.
    ENDIF.

    " setting request method
    lo_http_client->request->set_method( method ).

    lo_http_client->request->set_header_field( name  = '~server_protocol'                          "#EC *
                                               value = 'HTTP/1.1' ).                               "#EC *

    IF action IS NOT INITIAL.
      lo_http_client->request->set_header_field( name  = 'SOAPAction'                              "#EC *
                                                 value = action ).
    ENDIF.

    IF content_type IS INITIAL.
      CONCATENATE 'text/xml; charset=' encoding INTO l_string.
    ELSE.
      l_string = content_type.
    ENDIF.

    lo_http_client->request->set_header_field( name  = 'Content-Type'
                                               value = l_string ).

    IF convert_to_utf8 IS INITIAL AND peticionx IS INITIAL.
      IF peticion IS NOT INITIAL.
        DATA(l_lon) = strlen( peticion ).

        l_string = peticion.
        l_lon_txt = l_lon.
        l_lon_txt = condense( val  = l_lon_txt
                              from = ` `
                              to   = `` ).

        lo_http_client->request->set_header_field( name  = 'Content-Length'                        "#EC *
                                                   value = l_lon_txt ).

        lo_http_client->request->set_cdata( data   = l_string
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
      l_lon_txt = condense( val  = l_lon_txt
                            from = ` `
                            to   = `` ).

      lo_http_client->request->set_header_field( name  = 'Content-Length'                          "#EC *
                                                 value = l_lon_txt ).

      lo_http_client->request->set_data( data   = l_xstring
                                         offset = 0
                                         length = l_lon ).
    ENDIF.

    IF apikey IS NOT INITIAL.
      lo_http_client->request->set_header_field( name  = 'APIKey'
                                                 value = apikey ).
    ENDIF.

    IF auth IS NOT INITIAL.
      lo_http_client->request->set_header_field( name  = 'Authorization'
                                                 value = auth ).
    ELSEIF password IS NOT INITIAL.
      l_string = |{ usuario }:{ password }|.
      l_xstring = zcl_ap_string=>string2xstring( l_string ).
      l_string = zcl_ap_string=>xstring2base64( l_xstring ).
      l_string = |Basic { l_string }|.

      lo_http_client->request->set_header_field( name  = 'Authorization'
                                                 value = l_string ).
    ENDIF.

    lo_http_client->send( EXCEPTIONS http_communication_failure = 1
                                     http_invalid_state         = 2
                                     http_processing_failed     = 3
                                     http_invalid_timeout       = 4
                                     OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      lo_http_client->get_last_error( IMPORTING code    = subrc
                                                message = message ).
      RETURN.
    ENDIF.

    IF sy-subrc = 0.
      lo_http_client->receive( EXCEPTIONS http_communication_failure = 1
                                          http_invalid_state         = 2
                                          http_processing_failed     = 3
                                          OTHERS                     = 5 ).
      IF sy-subrc <> 0.
        lo_http_client->get_last_error( IMPORTING code    = subrc
                                                  message = message ).
        RETURN.
      ENDIF.
    ENDIF.

    respuesta = lo_http_client->response->get_cdata( ).
    CLEAR i_header.
    lo_http_client->response->get_header_fields( CHANGING fields = i_header ).
    ASSIGN i_header[ name = '~status_code' ] TO FIELD-SYMBOL(<header>).
    IF sy-subrc = 0.
      status = <header>-value.

      IF message IS INITIAL.
        CASE status.
          WHEN '401'.
            ASSIGN i_header[ name = 'x-message-code' ] TO <header>.
            IF sy-subrc = 0.
              message = <header>-value.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.

    IF respuesta CS 'class="errorTextHeader">'.
      SPLIT respuesta AT 'class="errorTextHeader">' INTO l_string message.
      SPLIT message AT '<' INTO message l_string.
    ELSEIF respuesta CS '<faultstring xml:lang="en">'.
      SPLIT respuesta AT '<faultstring xml:lang="en">' INTO l_string message.
      SPLIT message AT '"<' INTO message l_string.
    ELSEIF respuesta CS '<faultstring>'.
      SPLIT respuesta AT '<faultstring>' INTO l_string message.
      SPLIT message AT '"<' INTO message l_string.
    ENDIF.

    lo_http_client->response->get_status( IMPORTING code = status_i ).
    lo_http_client->response->get_status( IMPORTING reason = reason ).

    IF popup_respuesta = 'X'.
      l_string = |URL={ l_url }|.

      IF message IS NOT INITIAL.
        l_parametros = |MENSAJE={ message }|.
        CONCATENATE l_string l_parametros INTO l_string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      ENDIF.
      IF subrc IS NOT INITIAL.
        l_parametros = |SUBRC={ subrc }|.
        CONCATENATE l_string l_parametros INTO l_string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      ENDIF.
      IF status IS NOT INITIAL.
        l_parametros = |STATUS={ status }|.
        CONCATENATE l_string l_parametros INTO l_string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      ENDIF.
      CONCATENATE l_string respuesta INTO l_string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      zcl_ap_ws=>edit_xml_string( EXPORTING formatear = 'X'
                                  CHANGING  string    = l_string ).
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    me->host        = host.
    me->usuario     = usuario.
    me->password    = password.
    me->destination = destination.
    me->ssl_id      = ssl_id.
    me->proceso_log = proceso_log.
    me->auth        = auth.
    me->prefijo_tag = prefijo_tag.
  ENDMETHOD.
  METHOD fin_pet_soap.
    IF body = 'X'.
      add_tag_soap( tag    = 'soapenv:Body'
                    cerrar = 'X' ).
    ENDIF.

    IF envelope = 'X'.
      add_tag_soap( tag    = 'soapenv:Envelope'
                    cerrar = 'X' ).
    ENDIF.
  ENDMETHOD.
  METHOD get_atributo.
    valor = zcl_ap_regexp=>recuperar_valor( string   = respuesta
                                            atributo = atributo ).
  ENDMETHOD.
  METHOD get_elementos_xml.
    DATA l_datos TYPE wdy_key_value.

    DATA(o_xml) = NEW cl_xml_document( ).
    DATA(l_subrc) = o_xml->parse_string( xml ).

    IF l_subrc <> 0.
      RETURN.
    ENDIF.

    IF o_xml->m_document IS INITIAL.
      RETURN.
    ENDIF.

    DATA(l_iterator) = o_xml->m_document->create_iterator( ).

    DATA(l_node) = l_iterator->get_next( ).

    WHILE l_node IS NOT INITIAL.
      CASE l_node->get_type( ).
        WHEN if_ixml_node=>co_node_element.
          l_datos-key = l_node->get_name( ).

          DATA(l_nodemap) = l_node->get_attributes( ).

          IF l_nodemap IS NOT INITIAL.
            DATA(l_count) = l_nodemap->get_length( ).

            DO l_count TIMES.
              DATA(l_index) = sy-index - 1.
              DATA(l_attr) = l_nodemap->get_item( l_index ).

              l_datos-key   = l_attr->get_name( ).
*              DATA(l_prefix) = l_attr->get_namespace_prefix( ).
              l_datos-value = l_attr->get_value( ).

              APPEND l_datos TO i_datos.
            ENDDO.
          ENDIF.

        WHEN if_ixml_node=>co_node_text OR if_ixml_node=>co_node_cdata_section.
          l_datos-value = l_node->get_value( ).
          APPEND l_datos TO i_datos.
      ENDCASE.
      l_node = l_iterator->get_next( ).
    ENDWHILE.

    IF elementos IS NOT INITIAL.
      DATA(r_rango) = zcl_ap_lista=>lista2rango( elementos ).
      IF r_rango IS NOT INITIAL.
        DELETE i_datos WHERE NOT key IN r_rango.
      ENDIF.
    ENDIF.

    IF datos IS SUPPLIED.
      LOOP AT i_Datos ASSIGNING FIELD-SYMBOL(<dato>).
        DATA(val) = <dato>-value.
        CONDENSE val NO-GAPS.
        IF val IS NOT INITIAL.
          ASSIGN COMPONENT <dato>-key OF STRUCTURE datos TO FIELD-SYMBOL(<campo>).
          IF sy-subrc = 0.
            <campo> = <dato>-value.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_odata.
    DATA l_url          TYPE string.
    DATA l_string       TYPE string.
    DATA l_parametros   TYPE string.
    DATA lo_http_client TYPE REF TO if_http_client.
    DATA l_xstring      TYPE xstring.
    DATA l_c1           TYPE c LENGTH 1.
    DATA l_lon_txt      TYPE string.

    CLEAR: message,
           respuesta,
           subrc,
           tabla,
           datos,
           http_code,
           http_status,
           xstring.

    l_url = host.
    IF odata_sap = 'X' AND entidad IS NOT INITIAL.
      CONCATENATE l_url '/sap/opu/odata/sap/' INTO l_url.
    ENDIF.

    IF servicio IS NOT INITIAL.
      CONCATENATE l_url servicio INTO l_url SEPARATED BY '/'.
    ENDIF.
    IF entidad IS NOT INITIAL.
      CONCATENATE l_url entidad INTO l_url SEPARATED BY '/'.
    ENDIF.

    IF destination IS INITIAL.
      SPLIT l_url AT '://' INTO l_url l_string.
      REPLACE ALL OCCURRENCES OF '//' IN l_string WITH '/'.
      IF l_url IS NOT INITIAL.
        CONCATENATE l_url '://' l_string INTO l_url.
      ENDIF.
    ELSE.
      REPLACE ALL OCCURRENCES OF '//' IN l_url WITH '/'.
    ENDIF.

    IF key IS NOT INITIAL.
      IF key(1) = ''''.
        l_string = key.
      ELSE.
        CONCATENATE '''' key '''' INTO l_string.
      ENDIF.
      CONCATENATE l_url '(' l_string ')' INTO l_url.
    ENDIF.

    IF parametros IS NOT INITIAL.
      l_parametros = parametros.
    ENDIF.

    IF filter IS NOT INITIAL.
      l_string = |&$filter={ filter }|.
      CONCATENATE l_parametros l_string INTO l_parametros.
    ENDIF.

    IF campos IS NOT INITIAL.
      l_string = |&$select={ campos }|.
      CONCATENATE l_parametros l_string INTO l_parametros.
    ENDIF.

    IF top IS NOT INITIAL.
      l_string = |&$top={ top }|.
      CONCATENATE l_parametros l_string INTO l_parametros.
    ENDIF.

    IF l_parametros IS NOT INITIAL.
      CONCATENATE l_url '?' l_parametros INTO l_url.
    ENDIF.

    IF get_valor IS NOT INITIAL.
      CONCATENATE l_url get_valor '$value' INTO l_url SEPARATED BY '/'.
    ENDIF.

    IF destination IS INITIAL.
      cl_http_client=>create_by_url( EXPORTING  url                = l_url
                                                ssl_id             = ssl_id
                                                proxy_host         = proxy_host
                                                proxy_service      = proxy_service
                                     IMPORTING  client             = lo_http_client
                                     EXCEPTIONS argument_not_found = 1
                                                plugin_not_active  = 2
                                                internal_error     = 3
                                                OTHERS             = 4 ).
      IF sy-subrc <> 0.
        message = |Error accediendo a { l_url }|.
        RETURN.
      ENDIF.
      IF proceso_log IS NOT INITIAL.
        zcl_ap_log=>set_log( proceso = proceso_log
                             p1      = l_url
                             msgty   = 'I' ).
      ENDIF.
    ELSE.
      cl_http_client=>create_by_destination( EXPORTING  destination              = destination
                                             IMPORTING  client                   = lo_http_client
                                             EXCEPTIONS argument_not_found       = 1
                                                        destination_not_found    = 2
                                                        destination_no_authority = 3
                                                        plugin_not_active        = 4
                                                        internal_error           = 5 ).

      IF sy-subrc <> 0.
        message = |Error accediendo a { destination }|.
        RETURN.
      ENDIF.

      IF l_url IS NOT INITIAL.
        cl_http_utility=>set_request_uri( request = lo_http_client->request
                                          uri     = l_url ).
      ENDIF.

      IF proceso_log IS NOT INITIAL.
        zcl_ap_log=>set_log( proceso = proceso_log
                             p1      = destination
                             p2      = l_url
                             msgty   = 'I' ).
      ENDIF.

    ENDIF.

    IF sy-subrc <> 0.
      message = 'Error al abrir conexion'.
      CASE sy-subrc.
        WHEN 1. message = |{ message }Argumento no encontrado|.
        WHEN 2. message = |{ message }Plugin no activo|.
        WHEN 3. message = |{ message }Error interno|.
        WHEN OTHERS.
          IF sy-msgty IS NOT INITIAL.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_string.
            message = |{ message } { l_string }|.
          ENDIF.
      ENDCASE.

      IF lo_http_client IS NOT INITIAL.
        lo_http_client->get_last_error( IMPORTING code    = subrc
                                                  message = DATA(l_msg) ).
        IF l_msg IS NOT INITIAL.
          message = |{ message } { l_msg }|.
        ENDIF.
      ENDIF.
    ENDIF.

    IF message IS INITIAL.
      lo_http_client->request->set_method( method ).

      IF content_type IS NOT INITIAL.
        lo_http_client->request->set_header_field( name  = 'Content-Type'
                                                   value = content_type ).
      ELSE.
        lo_http_client->request->set_header_field( name  = 'Content-Type'
                                                   value = 'application/json' ).
      ENDIF.
      lo_http_client->request->set_header_field( name  = 'Cache-Control'
                                                 value = 'no-cache' ).
      IF accept IS INITIAL.
        IF get_valor IS INITIAL.
          lo_http_client->request->set_header_field( name  = 'Accept'
                                                     value = 'application/json' ).
        ELSE.
          lo_http_client->request->set_header_field( name  = 'Accept'
                                                     value = '*/*' ).
        ENDIF.
      ELSE.
        lo_http_client->request->set_header_field( name  = 'Accept'
                                                   value = accept ).
      ENDIF.

      IF apikey IS NOT INITIAL.
        lo_http_client->request->set_header_field( name  = 'APIKey'
                                                   value = apikey ).
      ENDIF.

      LOOP AT i_form ASSIGNING FIELD-SYMBOL(<form>).
        lo_http_client->request->set_form_field( name  = <form>-key
                                                 value = <form>-value ).
      ENDLOOP.

      IF peticion IS NOT INITIAL.
        DATA(l_lon) = strlen( peticion ).

        l_string = peticion.
        l_lon_txt = l_lon.

        lo_http_client->request->set_header_field( name  = 'Content-Length'                        "#EC *
                                                   value = l_lon_txt ).

        lo_http_client->request->set_cdata( data   = l_string
                                            offset = 0
                                            length = l_lon ).
      ENDIF.

      IF peticionx IS NOT INITIAL.
        l_lon = xstrlen( peticionx ).

        l_lon_txt = l_lon.

        lo_http_client->request->set_header_field( name  = 'Content-Length'                        "#EC *
                                                   value = l_lon_txt ).

        lo_http_client->request->set_data( data   = peticionx
                                           offset = 0
                                           length = l_lon ).
      ENDIF.

      IF password IS NOT INITIAL.
        l_string = |{ usuario }:{ password }|.
        l_xstring = zcl_ap_string=>string2xstring( l_string ).
        l_string = zcl_ap_string=>xstring2base64( l_xstring ).
        l_string = |Basic { l_string }|.

        lo_http_client->request->set_header_field( name  = 'Authorization'
                                                   value = l_string ).
      ELSEIF me->auth IS NOT INITIAL.
        lo_http_client->request->set_header_field( name  = 'Authorization'
                                                   value = auth ).
      ENDIF.

      lo_http_client->send( EXCEPTIONS http_communication_failure = 1
                                       http_invalid_state         = 2
                                       http_processing_failed     = 3
                                       http_invalid_timeout       = 4
                                       OTHERS                     = 5 ).
      IF sy-subrc <> 0.
        lo_http_client->get_last_error( IMPORTING code    = subrc
                                                  message = message ).
      ENDIF.
    ENDIF.

    IF message IS INITIAL.
      lo_http_client->receive( EXCEPTIONS http_communication_failure = 1
                                          http_invalid_state         = 2
                                          http_processing_failed     = 3
                                          OTHERS                     = 5 ).
      IF sy-subrc <> 0.
        lo_http_client->get_last_error( IMPORTING code    = subrc
                                                  message = message ).
      ENDIF.

      lo_http_client->response->get_status( IMPORTING code   = http_code
                                                      reason = http_status ).

      IF message IS INITIAL.

        respuesta = lo_http_client->response->get_cdata( ).

        lo_http_client->response->get_status( IMPORTING code   = http_code
                                                        reason = http_status ).

        xstring = lo_http_client->response->get_data( ).

*      DATA(L_XSTRING3) = lo_http_client->response->GET_DATA( ).
*      DATA(L_XSTRING2) = lo_http_client->response->GET_RAW_MESSAGE( ).
*
*      DATA(L_STRING3) = ZCL_AP_STRING=>XSTRING2STRING( L_XSTRING3 ).
*      DATA(L_STRING2) = ZCL_AP_STRING=>XSTRING2STRING( L_XSTRING2 ).

        lo_http_client->get_last_error( IMPORTING code    = subrc
                                                  message = message ).

        IF respuesta CS 'class="errorTextHeader">'.
          SPLIT respuesta AT 'class="errorTextHeader">' INTO l_string message.
          SPLIT message AT '<' INTO message l_string.
        ELSEIF respuesta CS '{"error":{"code":"'.
          SPLIT respuesta AT '","value":"' INTO l_string message.
          SPLIT message AT '"}' INTO message l_string.
        ELSEIF respuesta CS 'error' AND respuesta CS 'code' AND respuesta CS 'message' AND respuesta CS '"value": "'.
          SPLIT respuesta AT '"value": "' INTO l_string message.
          SPLIT message AT '"' INTO message l_string.
        ELSEIF respuesta CS '<head><title>405 Not Allowed'.
          message = '405 Not Allowed'.
        ELSEIF respuesta CS 'Error 403 (Forbidden)'.
          message = '403 (Forbidden)'.
        ELSE.
          IF get_valor IS NOT INITIAL.
            l_c1 = respuesta.
            IF l_c1 = '{'.
              message = 'Error recuperando valor'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    me->respuesta = respuesta.
    IF respuesta IS NOT INITIAL AND ( get_tabla = 'X' OR get_datos = 'X' ).
      l_string = respuesta.
      IF strlen( respuesta ) > 16.
        IF respuesta(16) = '{"d":{"results":'.
          l_string = respuesta.
          l_string = l_string+16.
          DATA(l_long) = strlen( l_string ).
          l_long = l_long - 2.
          IF l_long > 0.
            l_string = l_string(l_long).
          ENDIF.
        ENDIF.
      ENDIF.
      IF get_tabla = 'X'.
        zcl_ap_segw=>set_json( EXPORTING json    = l_string
                               IMPORTING datos   = tabla
                                         message = DATA(l_msg2) ).
        IF message IS INITIAL.
          message = l_msg2.
        ENDIF.
      ELSEIF get_datos = 'X'.
        l_c1 = l_string.
        IF l_c1 = '['.
          l_string = l_string+1.
          l_long = strlen( l_string ).
          l_long = l_long - 1.
          IF l_long > 0.
            l_string = l_string(l_long).
          ENDIF.
        ENDIF.

        zcl_ap_segw=>set_json( EXPORTING json    = l_string
                               IMPORTING datos   = datos
                                         message = l_msg2 ).
        IF message IS INITIAL.
          message = l_msg.
        ENDIF.
      ENDIF.
    ENDIF.

    IF proceso_log IS NOT INITIAL.
      IF message IS NOT INITIAL.
        zcl_ap_log=>set_log( proceso = proceso_log
                             p1      = message
                             msgty   = 'E' ).
      ENDIF.
      IF respuesta IS NOT INITIAL.
        zcl_ap_log=>set_log( proceso = proceso_log
                             p1      = respuesta
                             msgty   = 'S' ).
      ENDIF.
    ENDIF.

    IF popup_respuesta = 'X' OR ( popup_respuesta = 'E' AND message IS NOT INITIAL ).
      l_string = |URL={ l_url }|.

      IF message IS NOT INITIAL.
        l_parametros = |MENSAJE={ message }|.
        CONCATENATE l_string l_parametros INTO l_string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      ENDIF.
      CONCATENATE l_string respuesta INTO l_string SEPARATED BY cl_abap_char_utilities=>cr_lf.
      zcl_ap_ws=>edit_xml_string( CHANGING string = l_string ).
    ENDIF.
  ENDMETHOD.
  METHOD ini_pet_soap.
    CLEAR: peticion_soap,
           nivel_formato_soap.

    IF envelope IS NOT INITIAL.
      IF envelope = 'X'.
        add_tag_soap( tag   = 'soapenv:Envelope'
                      abrir = 'X' ).
      ELSE.
        add_tag_soap( tag      = 'soapenv:Envelope'
                      subvalor = envelope
                      abrir    = 'X' ).
      ENDIF.
    ENDIF.

    IF header = 'X' AND body = 'X'.
      add_tag_soap( tag    = 'soapenv:Header'
                    abrir  = 'X'
                    cerrar = 'X' ).
    ENDIF.

    IF body = 'X'.
      add_tag_soap( tag   = 'soapenv:Body'
                    abrir = 'X' ).
    ENDIF.
  ENDMETHOD.
