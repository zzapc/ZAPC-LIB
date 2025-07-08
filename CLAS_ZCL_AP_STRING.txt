CLASS zcl_ap_string DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_error_float TYPE qmittelwrt VALUE '-99999.99999' ##NO_TEXT.

    CLASS-METHODS tabla2string
      IMPORTING tabla               TYPE table
                separar_con_espacio TYPE abap_bool DEFAULT ''
      RETURNING VALUE(string)       TYPE string.

    CLASS-METHODS string2tabla
      IMPORTING !string               TYPE string
                longitud              TYPE i         DEFAULT 132
                forzar_enter          TYPE abap_bool DEFAULT ''
                partir_solo_en_blanco TYPE abap_bool DEFAULT ''
      CHANGING  tabla                 TYPE table.

    CLASS-METHODS editor_popup_string
      IMPORTING !string       TYPE string OPTIONAL
                titulo        TYPE string DEFAULT ''
      PREFERRED PARAMETER string
      RETURNING VALUE(salida) TYPE string.

    CLASS-METHODS comprimir_string
      IMPORTING !string        TYPE string
      RETURNING VALUE(xstring) TYPE xstring.

    CLASS-METHODS descomprimir_string
      IMPORTING xstring       TYPE xstring
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS string2xstring
      IMPORTING !string        TYPE string
                comprimir      TYPE abap_bool DEFAULT ''
                !encoding      TYPE any       DEFAULT 'DEFAULT'
      RETURNING VALUE(xstring) TYPE xstring.

    CLASS-METHODS xstring2string
      IMPORTING xstring       TYPE xstring
                !encoding     TYPE any       DEFAULT 'DEFAULT'
                descomprimir  TYPE abap_bool DEFAULT ''
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS quitar_caracteres_extranos
      IMPORTING !replacement TYPE any DEFAULT 46
      CHANGING  !string      TYPE string.

    CLASS-METHODS limpiar_nombre_fichero
      CHANGING !string TYPE string.

    CLASS-METHODS busca_numeros
      IMPORTING !string       TYPE any
                cualquiera    TYPE abap_bool DEFAULT ''
      RETURNING VALUE(numero) TYPE string.

    CLASS-METHODS poner_ceros
      IMPORTING cadena        TYPE any
      CHANGING  VALUE(salida) TYPE any.

    CLASS-METHODS string_estandar
      IMPORTING entrada       TYPE string
      RETURNING VALUE(salida) TYPE string.

    CLASS-METHODS tline2string
      IMPORTING tabla              TYPE tlinetab
                blancos_son_saltos TYPE abap_bool DEFAULT ''
      RETURNING VALUE(string)      TYPE string.

    CLASS-METHODS string2tline
      IMPORTING !string TYPE string
      CHANGING  tabla   TYPE tlinetab.

    CLASS-METHODS formatear_tline
      CHANGING tabla TYPE tlinetab.

    CLASS-METHODS ultimo_caracter
      IMPORTING texto       TYPE any
                num_car     TYPE i DEFAULT 1
      RETURNING VALUE(char) TYPE string.

    CLASS-METHODS justificar_string2tabla
      IMPORTING !string  TYPE string
                longitud TYPE i DEFAULT 132
      CHANGING  tabla    TYPE table.

    CLASS-METHODS condensar_tabla
      IMPORTING longitud TYPE i DEFAULT 132
      CHANGING  tabla    TYPE table.

    CLASS-METHODS quitar_ceros
      IMPORTING cadena        TYPE any
      CHANGING  VALUE(salida) TYPE any.

    CLASS-METHODS quitar_ceros_c
      CHANGING cadena TYPE any.

    CLASS-METHODS string2tablastring
      IMPORTING !string  TYPE string
                longitud TYPE i DEFAULT 132
      CHANGING  tabla    TYPE table.

    CLASS-METHODS right
      IMPORTING entrada       TYPE any
                !long         TYPE i
      RETURNING VALUE(salida) TYPE string.

    CLASS-METHODS poner_ceros_c
      CHANGING cadena TYPE any.

    CLASS-METHODS string2importe
      IMPORTING !string        TYPE any
      RETURNING VALUE(importe) TYPE awkgr.

    CLASS-METHODS limpia_numeros
      IMPORTING !string       TYPE any
      RETURNING VALUE(numero) TYPE string.

    CLASS-METHODS get_text_from_url
      IMPORTING url            TYPE saeuri
                mostrar_error  TYPE abap_bool DEFAULT ''
      RETURNING VALUE(i_texto) TYPE soli_tab.

    CLASS-METHODS string_to_binary_tab
      IMPORTING !string     TYPE string
      RETURNING VALUE(itab) TYPE solix_tab.

    CLASS-METHODS string_pot_2importe
      IMPORTING !string        TYPE any
      RETURNING VALUE(importe) TYPE dec_16_05_s.

    CLASS-METHODS string2cantidad
      IMPORTING !string        TYPE any
      RETURNING VALUE(importe) TYPE bbbtr.

    CLASS-METHODS lista2rango
      IMPORTING lista          TYPE any
                separador      TYPE any       DEFAULT ','
                !option        TYPE any       DEFAULT 'EQ'
                conv_asterisco TYPE abap_bool DEFAULT ''
      RETURNING VALUE(rango)   TYPE rstt_t_range_string.

    CLASS-METHODS siguiente_letra
      IMPORTING letra            TYPE c
      RETURNING VALUE(siguiente) TYPE char1.

    CLASS-METHODS es_numero
      IMPORTING cadena    TYPE any
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS string2float
      IMPORTING !string      TYPE any
      RETURNING VALUE(float) TYPE qmittelwrt.

    CLASS-METHODS get_string_from_url
      IMPORTING url             TYPE any
                mostrar_error   TYPE abap_bool DEFAULT ''
                !convert        TYPE abap_bool DEFAULT ''
      EXPORTING !size           TYPE i
                content_bin_255 TYPE bapidoccontentab
      RETURNING VALUE(string)   TYPE string.

    CLASS-METHODS convert_html_chars
      CHANGING !string TYPE any.

    CLASS-METHODS es_numerop
      IMPORTING cadena    TYPE any
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS transform_object_to_rawstring
      IMPORTING  objeto         TYPE REF TO if_serializable_object OPTIONAL
                 comprimir      TYPE abap_bool                     DEFAULT 'X'
                 tabla          TYPE table                         OPTIONAL
                 variable       TYPE any                           OPTIONAL
                 json           TYPE abap_bool                     DEFAULT ''
      RETURNING  VALUE(xstring) TYPE xstring
      EXCEPTIONS transformation_failed.

    CLASS-METHODS transform_rawstring_to_object
      IMPORTING  descomprimir         TYPE abap_bool                     DEFAULT 'X'
                 xstring              TYPE xstring                       OPTIONAL
                 get_objeto           TYPE abap_bool                     DEFAULT ''
                 get_tabla            TYPE abap_bool                     DEFAULT ''
                 get_variable         TYPE abap_bool                     DEFAULT ''
                 !string              TYPE any                           DEFAULT ''
                 codepage             TYPE any                           DEFAULT '1100'
                 json                 TYPE abap_bool                     DEFAULT ''
                 visualizar_xml       TYPE abap_bool                     DEFAULT ''
      EXPORTING  xstring_decompressed TYPE xstring
      CHANGING   objeto               TYPE REF TO if_serializable_object OPTIONAL
                 tabla                TYPE table                         OPTIONAL
                 variable             TYPE any                           OPTIONAL
      EXCEPTIONS transformation_failed.

    CLASS-METHODS es_numero_coma
      IMPORTING cadena    TYPE any
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS string2ctd
      IMPORTING ctd_texto TYPE any
      EXPORTING cantidad  TYPE any
                mensaje   TYPE any.

    CLASS-METHODS split_left
      IMPORTING !string   TYPE any
                separador TYPE any
      EXPORTING izquierda TYPE any
                derecha   TYPE any.

    CLASS-METHODS convierte_codificacion
      IMPORTING cadena          TYPE any       OPTIONAL
                incode          TYPE any       DEFAULT zcl_c=>codepage_html
                outcode         TYPE any       DEFAULT zcl_c=>codepage
                mostrar_error   TYPE abap_bool DEFAULT ''
                conv_caract_in  TYPE any       OPTIONAL
                conv_caract_out TYPE any       OPTIONAL
      PREFERRED PARAMETER cadena
      RETURNING VALUE(salida)   TYPE string.

    CLASS-METHODS get_string_from_url_fondo
      IMPORTING url           TYPE any
                mostrar_error TYPE abap_bool DEFAULT ''
                !convert      TYPE abap_bool DEFAULT ''
      EXPORTING !message      TYPE bapi_msg
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS eliminar_caract_no_validos
      IMPORTING cadena        TYPE any
      RETURNING VALUE(salida) TYPE string.

    CLASS-METHODS to_clipboard
      IMPORTING !string  TYPE string    OPTIONAL
                show_msg TYPE abap_bool DEFAULT 'X'
                tabla    TYPE table     OPTIONAL
      PREFERRED PARAMETER string.

    CLASS-METHODS from_clipboard
      IMPORTING show_msg   TYPE abap_bool DEFAULT 'X'
                get_tabla  TYPE abap_bool DEFAULT ''
                get_string TYPE abap_bool DEFAULT 'X'
      EXPORTING tabla      TYPE table
                !string    TYPE string.

    CLASS-METHODS xstring2base64
      IMPORTING xstring       TYPE xstring
      RETURNING VALUE(base64) TYPE string.

    CLASS-METHODS numero2string
      IMPORTING numero               TYPE any
                quitar_ceros_derecha TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(string)        TYPE string.

    CLASS-METHODS popup_texto
      IMPORTING titulo             TYPE any    DEFAULT ''
                editar             TYPE any    DEFAULT ''
                intro              TYPE any    DEFAULT ''
                max_cols           TYPE i      DEFAULT 132
                titulo_texto_corto TYPE string DEFAULT ''
      EXPORTING modificado         TYPE any
                cancelado          TYPE any
      CHANGING  texto              TYPE any
                texto_corto        TYPE any    OPTIONAL.

    CLASS-METHODS get_hash
      IMPORTING datos       TYPE any       OPTIONAL
                algoritmo   TYPE string    DEFAULT 'MD5'
                b64         TYPE abap_bool DEFAULT 'X'
                tabla       TYPE table     OPTIONAL
      PREFERRED PARAMETER datos
      RETURNING VALUE(hash) TYPE string.

    CLASS-METHODS ctd2string
      IMPORTING ctd              TYPE any
                quitar_decimales TYPE abap_bool DEFAULT 'X'
                max_decimales    TYPE i         DEFAULT 9999
                condensar        TYPE abap_bool DEFAULT ''
      RETURNING VALUE(string)    TYPE string.

    CLASS-METHODS encrypt
      IMPORTING !string        TYPE any
      RETURNING VALUE(encrypt) TYPE string.

    CLASS-METHODS decrypt
      IMPORTING !string        TYPE any
      RETURNING VALUE(decrypt) TYPE string.

    CLASS-METHODS escape_url
      IMPORTING !string    TYPE any
      RETURNING VALUE(url) TYPE string.

    CLASS-METHODS get_guid
      RETURNING VALUE(guid) TYPE edoc_guid.

    CLASS-METHODS base642xstring
      IMPORTING base64         TYPE string
      RETURNING VALUE(xstring) TYPE xstring.


endclass. "ZCL_AP_STRING definition
class ZCL_AP_STRING implementation.
  METHOD base642xstring.
    IF base64 IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SSFC_BASE64_DECODE' "#EC *
      EXPORTING  b64data                  = base64
*                 B64LENG                  =
*                 B_CHECK                  =
      IMPORTING  bindata                  = xstring
      EXCEPTIONS ssf_krn_error            = 1
                 ssf_krn_noop             = 2
                 ssf_krn_nomemory         = 3
                 ssf_krn_opinv            = 4
                 ssf_krn_input_data_error = 5
                 ssf_krn_invalid_par      = 6
                 ssf_krn_invalid_parlen   = 7
                 OTHERS                   = 8.
    IF sy-subrc <> 0.
      MESSAGE 'Error decodificando de base 64' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD busca_numeros.
    DATA: pe_texto TYPE c LENGTH 1000,
          l_indice TYPE i,
          l_long   TYPE i,
          l_letra  TYPE c LENGTH 1.

    pe_texto = string.
    CLEAR numero.
    l_indice = 0.
    l_long   = strlen( pe_texto ).
    DO.
      IF l_indice > l_long.
        EXIT.
      ENDIF.

      l_letra = pe_texto+l_indice(1).
      IF l_letra CO '0123456789'.
        CONCATENATE numero l_letra INTO numero.
      ELSEIF l_letra CO ' .-' AND numero IS INITIAL. "#EC *
      ELSE.
        IF cualquiera IS INITIAL.
          EXIT.
        ENDIF.
      ENDIF.
      l_indice = l_indice + 1.
    ENDDO.
  ENDMETHOD.
  METHOD comprimir_string.
    DATA izip      TYPE REF TO cl_abap_zip.
    DATA converter TYPE REF TO cl_abap_conv_out_ce.

    izip = NEW #( ).

*-- Convert
    converter = cl_abap_conv_out_ce=>create( encoding = 'DEFAULT' ).
    converter->reset( ).
    converter->write( data = string ).

    xstring = converter->get_buffer( ).

    izip->add( name    = 'xstring'
               content = xstring ).

    xstring = izip->save( ).
  ENDMETHOD.
  METHOD condensar_tabla.
    DATA: l_linea  TYPE text1000,
          l_long   TYPE i,
          l_ant    TYPE text1000,
          t_aux    TYPE TABLE OF text1000,
          l_aux    TYPE text1000,
          t_tabla2 TYPE TABLE OF text1000.

    LOOP AT tabla INTO l_linea.
      l_long = strlen( l_linea ) - 1.
      CONCATENATE l_ant l_linea INTO l_ant SEPARATED BY space.
      IF l_long >= 0.
        IF l_linea+l_long = '.'.
          IF l_ant(1) = ' '. l_ant = l_ant+1. ENDIF.
          APPEND l_ant TO t_aux.
          CLEAR l_ant.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF NOT l_ant IS INITIAL.
      IF l_ant(1) = ' '. l_ant = l_ant+1. ENDIF.
      APPEND l_ant TO t_aux.
    ENDIF.

    CLEAR tabla.
    LOOP AT t_aux INTO l_aux.
      REFRESH t_tabla2.
      CALL FUNCTION 'RKD_WORD_WRAP'
        EXPORTING  textline            = l_aux
*                   DELIMITER           = ' '
                   outputlen           = longitud
* IMPORTING
*                   OUT_LINE1           =
*                   OUT_LINE2           =
*                   OUT_LINE3           =
        TABLES     out_lines           = t_tabla2
        EXCEPTIONS outputlen_too_large = 1
                   OTHERS              = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Texto a justificar demasiado largo'(tdl) TYPE 'S'.
      ENDIF.

      LOOP AT t_tabla2 INTO l_aux.
        l_aux = l_aux(longitud).
        APPEND l_aux TO tabla.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD convert_html_chars.
    REPLACE ALL OCCURRENCES OF '\u00e1' IN string WITH 'á'.
    REPLACE ALL OCCURRENCES OF '\u00e9' IN string WITH 'é'.
    REPLACE ALL OCCURRENCES OF '\u00ed' IN string WITH 'í'.
    REPLACE ALL OCCURRENCES OF '\u00f3' IN string WITH 'ó'.
    REPLACE ALL OCCURRENCES OF '\u00fa' IN string WITH 'ú'.
    REPLACE ALL OCCURRENCES OF '\u00f1' IN string WITH 'ñ'.

    REPLACE ALL OCCURRENCES OF '\u00e0' IN string WITH 'á'.
    REPLACE ALL OCCURRENCES OF '\u00e9' IN string WITH 'é'.
    REPLACE ALL OCCURRENCES OF '\u00ec' IN string WITH 'í'.
    REPLACE ALL OCCURRENCES OF '\u00f2' IN string WITH 'ó'.
    REPLACE ALL OCCURRENCES OF '\u00f9' IN string WITH 'ú'.

    REPLACE ALL OCCURRENCES OF '\u00fc' IN string WITH 'ü'.

    REPLACE ALL OCCURRENCES OF '\u00c1' IN string WITH 'Á'.
    REPLACE ALL OCCURRENCES OF '\u00c9' IN string WITH 'É'.
    REPLACE ALL OCCURRENCES OF '\u00cd' IN string WITH 'Í'.
    REPLACE ALL OCCURRENCES OF '\u00d3' IN string WITH 'Ó'.
    REPLACE ALL OCCURRENCES OF '\u00da' IN string WITH 'Ú'.
    REPLACE ALL OCCURRENCES OF '\u00d1' IN string WITH 'Ñ'.

    REPLACE ALL OCCURRENCES OF '\u00c0' IN string WITH 'À'.
    REPLACE ALL OCCURRENCES OF '\u00c8' IN string WITH 'È'.
    REPLACE ALL OCCURRENCES OF '\u00cc' IN string WITH 'Ì'.
    REPLACE ALL OCCURRENCES OF '\u00d2' IN string WITH 'Ò'.
    REPLACE ALL OCCURRENCES OF '\u00d9' IN string WITH 'Ù'.

    REPLACE ALL OCCURRENCES OF '\u00bf' IN string WITH '¿'.
    REPLACE ALL OCCURRENCES OF '\n' IN string WITH ' '.
    REPLACE ALL OCCURRENCES OF '\/' IN string WITH '/'.

    REPLACE ALL OCCURRENCES OF '\u2018' IN string WITH '#'.
    REPLACE ALL OCCURRENCES OF '\u2019' IN string WITH '#'.
  ENDMETHOD.
  METHOD convierte_codificacion.
    DATA: l_incode  TYPE tcp00-cpcodepage,
          l_outcode TYPE tcp00-cpcodepage.

    IF incode = outcode.
      salida = cadena.
      RETURN.
    ENDIF.

    l_incode = incode.
    l_outcode = outcode.

    CALL FUNCTION 'SCP_TRANSLATE_CHARS'
      EXPORTING  inbuff           = cadena
*                 INBUFFLG         = 0
                 incode           = l_incode
*                 OUTBUFFLG        = 0
                 outcode          = l_outcode
*                 CSUBST           = 'X'
*                 SUBSTC_HASH      = ' '
*                 SUBSTC_DOT       = ' '
*                 SUBSTC_SPACE     = ' '
*                 SUBSTC           = '00035'
      IMPORTING
*                 INUSED           =
                 outbuff          = salida
*                 OUTOVERFLOW      =
*                 OUTUSED          =
*                 SUBSTED          =
*                 INPUT_ENDS_IN_CHAR =
*                 ERRMSG           =
      EXCEPTIONS invalid_codepage = 1
                 internal_error   = 2
                 cannot_convert   = 3
                 fields_bad_type  = 4
                 OTHERS           = 5.

    IF sy-subrc <> 0.
      IF mostrar_error = 'X'.
        MESSAGE e398(00) WITH 'Error'(err) sy-subrc 'en conversión'(enc) ''.
      ELSE.
        salida = cadena.
      ENDIF.
    ELSE.
      IF NOT conv_caract_in IS INITIAL.
        REPLACE ALL OCCURRENCES OF conv_caract_in IN salida WITH conv_caract_out.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD ctd2string.
    CLEAR string.
    string = ctd.
    CONDENSE string NO-GAPS.
    SPLIT string AT '.' INTO DATA(entero) DATA(decimales).
    IF decimales CO '0 ' OR quitar_decimales = 'X'.
      string = entero.
    ELSE.
      IF max_decimales > 0.
        IF strlen( decimales ) > max_decimales.
          CONCATENATE entero '.' decimales(max_decimales) INTO string.
        ENDIF.
      ENDIF.
    ENDIF.

    IF condensar = 'X'.
      CONDENSE string NO-GAPS.
      IF string CS '.'.
        DO 5 TIMES.
          DATA(l_long) = strlen( string ) - 1.
          IF string+l_long(1) = '0'.
            string = string(l_long).
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD decrypt.
    CLEAR decrypt.
    TRY.
        decrypt = NEW cl_hard_wired_encryptor( )->decrypt_string2string( CONV #( string ) ).
      CATCH cx_root INTO DATA(o_cx_root). "#EC * " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE 'Error desencriptando' TYPE 'S'.
    ENDTRY.
  ENDMETHOD.
  METHOD descomprimir_string.
    DATA izip      TYPE REF TO cl_abap_zip.
    DATA l_xstring TYPE xstring.
    DATA converter TYPE REF TO cl_abap_conv_in_ce.

    izip = NEW #( ).

    izip->load( xstring ).

    izip->get( EXPORTING name    = 'xstring'
               IMPORTING content = l_xstring ).

*-- Convert
    converter = cl_abap_conv_in_ce=>create( encoding = 'DEFAULT'
                                            input    = l_xstring ).
*  converter->reset( ).
*  call method converter->write
*    exporting
*      data = l_xstring.

    string = converter->get_buffer( ).
  ENDMETHOD.
  METHOD editor_popup_string.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: l_titulo TYPE text80,
          texto    TYPE swhttext80.

    IF titulo IS INITIAL.
      l_titulo = 'Nota'(not).
    ELSE.
      l_titulo = titulo.
    ENDIF.

    zcl_ap_string=>string2tabla( EXPORTING string   = string
                                           longitud = 70 ##NUMBER_OK
                                 CHANGING  tabla    = texto ).

    CALL FUNCTION 'TERM_CONTROL_EDIT'
      EXPORTING  titel          = titulo
                 langu          = sy-langu
      TABLES     textlines      = texto
      EXCEPTIONS user_cancelled = 1
                 OTHERS         = 2.
    IF sy-subrc = 0.
      salida = zcl_ap_string=>tabla2string( tabla = texto ).
    ELSE.
      salida = '#!#'. " Error
    ENDIF.
  ENDMETHOD.
  METHOD eliminar_caract_no_validos.
    DATA: l_lon   TYPE i,
          l_pos   TYPE i,
          l_char  TYPE c LENGTH 1,
          l_ascii TYPE url_code.

    CLEAR salida.
    l_lon = strlen( cadena ).
    DO l_lon TIMES.
      l_pos = sy-index - 1.
      l_char = cadena+l_pos(1).

      CALL FUNCTION 'URL_ASCII_CODE_GET'
        EXPORTING trans_char = l_char
        IMPORTING char_code  = l_ascii.

      IF l_ascii = '01' OR l_ascii = '20' OR l_ascii = '0D'.
        CONCATENATE salida ` ` INTO salida.
      ELSE.
        CONCATENATE salida l_char INTO salida.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD encrypt.
    CLEAR encrypt.
    TRY.
        encrypt = NEW cl_hard_wired_encryptor( )->encrypt_string2string( CONV #( string ) ).
      CATCH cx_root INTO DATA(o_cx_root). "#EC * " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE 'Error encriptando' TYPE 'S'. "#EC *
    ENDTRY.
  ENDMETHOD.
  METHOD es_numero.
    DATA htype TYPE dd01v-datatype.

    CLEAR si.
    IF cadena CN '0123456789 '.
      RETURN.
    ENDIF.

    CALL FUNCTION 'NUMERIC_CHECK'
      EXPORTING string_in = cadena
      IMPORTING
*                STRING_OUT =
                htype     = htype.

    IF htype = 'NUMC'.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD es_numero_coma.
    DATA l_cadena TYPE c LENGTH 100.

    l_cadena = cadena.
    CONDENSE l_cadena.
    CLEAR si.
    IF l_cadena CO '0123456789, '.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD es_numerop.
    DATA l_cadena TYPE c LENGTH 100.

    l_cadena = cadena.
    CONDENSE l_cadena.
    CLEAR si.
    IF l_cadena CO '0123456789. '.
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD escape_url.
    CLEAR url.
    url = cl_http_utility=>escape_url( CONV #( string ) ).
  ENDMETHOD.
  METHOD formatear_tline.
    DATA: t_aux2  TYPE tlinetab,
          t_aux   TYPE tlinetab,
          l_aux   TYPE tline,
          l_index TYPE sy-tabix,
          l_aux2  TYPE tline,
          l_long  TYPE i,
          l_long2 TYPE i,
          l_tot   TYPE i.

    t_aux2 = tabla.
    t_aux = t_aux2.

    CLEAR tabla.

    LOOP AT t_aux INTO l_aux.
      l_index = sy-tabix.
      CLEAR l_aux2.
      l_index = l_index + 1.
      READ TABLE t_aux2 INTO l_aux2 INDEX l_index.
      IF sy-subrc <> 0.
        APPEND l_aux TO tabla.
      ELSE.
        l_long = strlen( l_aux-tdline ).
        IF    ultimo_caracter( l_aux-tdline )  = '.'
           OR l_long >= 130 OR (                            ##NUMBER_OK
                                                          l_aux-tdline <> '*' AND l_aux-tdline <> ' ' ).
          APPEND l_aux TO tabla.
        ELSE.
          l_long2 = strlen( l_aux2-tdline ).
          l_tot = l_long + l_long2.
          IF l_aux2-tdline IS INITIAL AND l_aux2-tdformat = '*'.
            DELETE t_aux INDEX l_index.
            DELETE t_aux2 INDEX l_index.
            APPEND l_aux TO tabla.
          ELSEIF l_aux2-tdformat = '*' AND l_tot < 130 ##NUMBER_OK.
            CONCATENATE l_aux-tdline l_aux2-tdline INTO l_aux-tdline
                        SEPARATED BY space.
            APPEND l_aux TO tabla.
            DELETE t_aux INDEX l_index.
            DELETE t_aux2 INDEX l_index.
          ELSE.
            APPEND l_aux TO tabla.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD from_clipboard.
    " TODO: parameter GET_STRING is never used (ABAP cleaner)

    DATA: i_data TYPE TABLE OF text4096,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_len  TYPE i,
          i_col  TYPE TABLE OF text4096,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_data TYPE REF TO data,
          l_col  TYPE text4096.

    FIELD-SYMBOLS: <data>  TYPE text4096,
                   <linea> TYPE any,
                   <col>   TYPE any.

    CLEAR: tabla,
           string.

    cl_gui_frontend_services=>clipboard_import( IMPORTING  data                 = i_data
                                                           length               = l_len
                                                EXCEPTIONS cntl_error           = 1
                                                           error_no_gui         = 2
                                                           not_supported_by_gui = 3
                                                           OTHERS               = 4 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF get_tabla = 'X'.
        LOOP AT i_data ASSIGNING <data>.
          SPLIT <data> AT cl_bcs_convert=>gc_tab INTO TABLE i_col.

          CREATE DATA l_data LIKE LINE OF tabla.
          APPEND INITIAL LINE TO tabla ASSIGNING <linea>.
          LOOP AT i_col INTO l_col.
            ASSIGN COMPONENT sy-tabix
                   OF STRUCTURE <linea> TO <col>.
            IF sy-subrc = 0.
              <col> = l_col.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ELSE.
        string = tabla2string( i_data ).
      ENDIF.

      IF show_msg = 'X'.
        MESSAGE 'Se han copiado datos desde portapapeles'(cdp) TYPE 'S'.
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD get_guid.
    DATA time_char TYPE timestampl.

    CLEAR guid.
    TRY.
        guid = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ). " Create UUID
      CATCH cx_uuid_error INTO DATA(lref_cx_uuid_error). " TODO: variable is assigned but never used (ABAP cleaner)
        GET TIME STAMP FIELD time_char.
        guid = time_char.
    ENDTRY.
  ENDMETHOD.
  METHOD get_hash.
    DATA l_string TYPE string.

    CLEAR hash.
    IF NOT datos IS INITIAL.
      l_string = datos.
    ELSEIF NOT tabla IS INITIAL.
      l_string = /ui2/cl_json=>serialize( data     = tabla
                                          compress = 'X' ).
    ENDIF.

    IF b64 = 'X'.
      TRY.
          cl_abap_message_digest=>calculate_hash_for_char( EXPORTING if_algorithm     = algoritmo
                                                                     if_data          = l_string
*                                                                     if_length        = 0
                                                           IMPORTING
*                                                                     ef_hashstring    =
*                                                                     ef_hashxstring   =
                                                                     ef_hashb64string = hash ).
        CATCH cx_abap_message_digest. "#EC *

      ENDTRY.

    ELSE.
      TRY.
          cl_abap_message_digest=>calculate_hash_for_char( EXPORTING if_algorithm  = algoritmo
                                                                     if_data       = l_string
*                                                                     if_length     = 0
                                                           IMPORTING ef_hashstring = hash ).
        CATCH cx_abap_message_digest. "#EC *

      ENDTRY.

    ENDIF.
  ENDMETHOD.
  METHOD get_string_from_url.
    TYPES ole2_parameter TYPE swcbcont-value.
    TYPES: BEGIN OF cndp_user_info,
             user          TYPE ole2_parameter, " Username am Server
             password      TYPE ole2_parameter, " Password am Server
             proxy         TYPE ole2_parameter, " Proxy (incl. Port)
             proxyuser     TYPE ole2_parameter, " User am Proxy
             proxypassword TYPE ole2_parameter, " Password am Proxy
             scrambled     TYPE c LENGTH 1,     " Flag ob verschlüsselt
           END OF cndp_user_info.

    DATA: l_url     TYPE saeuri,
          user_info TYPE cndp_user_info.

    CLEAR content_bin_255.

    l_url = url.
    CALL FUNCTION 'DP_GET_STREAM_FROM_URL'
      EXPORTING  url            = l_url
                 userinfo       = user_info
      IMPORTING  size           = size
      TABLES     data           = content_bin_255
      EXCEPTIONS dp_fail        = 1
                 dp_failed_init = 2
                 OTHERS         = 3.

    IF sy-subrc <> 0.
      CALL FUNCTION 'AC_FLUSH_CALL'
        EXPORTING system_flush = 'X'.

      IF mostrar_error = 'X'.
        MESSAGE 'URL no existe'(eur) TYPE 'E'.
      ENDIF.
    ENDIF.

    IF content_bin_255 IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING input_length = 999999999 ##NUMBER_OK
      IMPORTING text_buffer  = string
*                output_length = lv_len_out
      TABLES    binary_tab   = content_bin_255.

    IF convert = 'X'.
      convert_html_chars( CHANGING string = string ).
    ENDIF.
  ENDMETHOD.
  METHOD get_string_from_url_fondo.
    " TODO: parameter CONVERT is never used (ABAP cleaner)

    TYPES: BEGIN OF text,
             line TYPE c LENGTH 256,
           END OF text.

    DATA: l_url            TYPE c LENGTH 1024,
          response         TYPE TABLE OF text,
          response_headers TYPE TABLE OF text,
          l_line           TYPE text.
    DATA dest TYPE rfcdes-rfcdest VALUE 'SAPHTTP'.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: status     TYPE c LENGTH 3,
          " TODO: variable is assigned but never used (ABAP cleaner)
          statustext TYPE c LENGTH 128,
          " TODO: variable is assigned but never used (ABAP cleaner)
          rlength    TYPE i.

    CLEAR message.

    l_url = url.
    CALL FUNCTION 'HTTP_GET'
      EXPORTING  absolute_uri                = l_url
                 rfc_destination             = dest
                 blankstocrlf                = 'Y'
*                 USER                        = 'xxx' - Specify your user here
*                 Password                    = 'yyy' - Specify your password here
      IMPORTING  status_code                 = status
                 status_text                 = statustext
                 response_entity_body_length = rlength
      TABLES     response_entity_body        = response
                 response_headers            = response_headers
      EXCEPTIONS connect_failed              = 1
                 timeout                     = 2
                 internal_error              = 3
                 tcpip_error                 = 4
                 data_error                  = 5
                 system_failure              = 6
                 communication_failure       = 7
                 OTHERS                      = 8.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1. message = 'Error de conexión'(ecn).
        WHEN 2. message = 'TimeOut'(tmo).
        WHEN 3. message = 'Error interno'(ein).
        WHEN 4. message = 'Error de TCPIP'(tcp).
        WHEN 5. message = 'Error de datos'(eda).
        WHEN 6. message = 'Fallo de sistema'(fsi).
        WHEN 7. message = 'Fallo de comunicación'(fco).
        WHEN OTHERS.
          message = 'Error llamando a HTTP_GET'(ehg).
      ENDCASE.

      IF NOT message IS INITIAL.
        IF mostrar_error = 'X'.
          MESSAGE message TYPE 'E'.
        ELSE.
          SET PARAMETER ID 'ZHTTP_GET_ERROR' FIELD message. "#EC *
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT message IS INITIAL.
      RETURN.
    ENDIF.

*  LOOP AT response_headers.
*    WRITE response_headers-line.
*  ENDLOOP.
*  SKIP 2.
    LOOP AT response INTO l_line.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab
              IN l_line-line WITH ' '.
      CONCATENATE string l_line-line INTO string.
    ENDLOOP.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING  destination = dest
      EXCEPTIONS OTHERS      = 0.

    SET PARAMETER ID 'ZHTTP_GET_ERROR' FIELD ''.            "#EC *
  ENDMETHOD.
  METHOD get_text_from_url.
    DATA lv_string TYPE string.

    lv_string = get_string_from_url( url           = url
                                     mostrar_error = mostrar_error ).

    CLEAR i_texto.
    SPLIT lv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE i_texto.
  ENDMETHOD.
  METHOD justificar_string2tabla.
    DATA: t_tabla  TYPE TABLE OF text1000,
          l_text   TYPE text1000,
          t_tabla2 TYPE TABLE OF text1000.

    string2tabla( EXPORTING string   = string
                            longitud = 1000
                  CHANGING  tabla    = t_tabla ).

    LOOP AT t_tabla INTO l_text.
      REFRESH t_tabla2.
      CALL FUNCTION 'RKD_WORD_WRAP'
        EXPORTING  textline            = l_text
*                   DELIMITER           = ' '
                   outputlen           = longitud
* IMPORTING
*                   OUT_LINE1           =
*                   OUT_LINE2           =
*                   OUT_LINE3           =
        TABLES     out_lines           = t_tabla2
        EXCEPTIONS outputlen_too_large = 1
                   OTHERS              = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Texto a justificar demasiado largo'(tdl) TYPE 'S'.
      ENDIF.

      LOOP AT t_tabla2 INTO l_text.
        l_text = l_text(longitud).
        APPEND l_text TO tabla.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD limpia_numeros.
    DATA: pe_texto  TYPE c LENGTH 1000,
          l_indice  TYPE i,
          l_long    TYPE i,
          l_letra   TYPE c LENGTH 1,
          l_npuntos TYPE i,
          l_neg     TYPE c LENGTH 1.

    pe_texto = string.
    CLEAR numero.
    l_indice = 0.
    l_long = strlen( pe_texto ).
    DO.
      IF l_indice > l_long.
        EXIT.
      ENDIF.

      l_letra = pe_texto+l_indice(1).
      IF l_letra CO '0123456789'.
        CONCATENATE numero l_letra INTO numero.
      ELSEIF l_letra CO ' '. "#EC *
      ELSEIF l_letra CO '.,'.
        CONCATENATE numero '.' INTO numero.
        l_npuntos = l_npuntos + 1.
      ELSEIF l_letra = '-'.
        l_neg = ''.
      ENDIF.
      l_indice = l_indice + 1.
    ENDDO.

    IF l_neg = 'X'.
      CONCATENATE '-' numero INTO numero.
    ENDIF.

    IF l_npuntos > 1.
      l_npuntos = l_npuntos - 1.
      DO l_npuntos TIMES.
        REPLACE '.' WITH '' INTO numero.
        CONDENSE numero NO-GAPS.
      ENDDO.
    ENDIF.
  ENDMETHOD.
  METHOD limpiar_nombre_fichero.
    zcl_ap_string=>quitar_caracteres_extranos( CHANGING string = string ).

    REPLACE ALL OCCURRENCES OF ':' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '.' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '/' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '\' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '>' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '<' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '?' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '¿' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '%' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '$' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '&' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '"' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '*' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '''' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '{' IN string WITH ''.
    REPLACE ALL OCCURRENCES OF '}' IN string WITH ''.
  ENDMETHOD.
  METHOD lista2rango.
    DATA: i_lista  TYPE TABLE OF string,
          l_string TYPE string,
          l_rango  TYPE rstt_s_range_string.

    SPLIT lista AT separador INTO TABLE i_lista.

    LOOP AT i_lista INTO l_string.
      CLEAR l_rango.
      l_rango-option = option.
      l_rango-sign   = 'I'.
      IF option = 'CP'.
        CONCATENATE '*' l_string '*' INTO l_rango-low.
      ELSEIF option = 'LK'.
        l_rango-option = 'CP'.
        CONCATENATE l_string '*' INTO l_rango-low.
      ELSE.
        l_rango-low = l_string.
      ENDIF.

      IF l_rango-low CS '*'.
        IF conv_asterisco = 'X' AND option = 'EQ'.
          l_rango-option = 'CP'.
        ENDIF.
      ENDIF.

      APPEND l_rango TO rango.
    ENDLOOP.
    IF sy-subrc <> 0.
      CLEAR l_rango.
      l_rango-option = 'EQ'.
      l_rango-sign   = 'I'.
      l_rango-low    = '¿¿¿¿????'.
      APPEND l_rango TO rango.
    ENDIF.
  ENDMETHOD.
  METHOD numero2string.
    DATA: l_aux1 TYPE string,
          l_aux2 TYPE string.

    string = numero.
    IF string CS '.'.
      SPLIT string AT '.' INTO l_aux1 l_aux2.
      IF l_aux2 CO '0 '.
        string = l_aux1.
      ELSEIF quitar_ceros_derecha = 'X'.
        CONDENSE l_aux2 NO-GAPS.
        DATA(l_long) = strlen( l_aux2 ).
        WHILE l_long > 1.
          l_long = l_long - 1.
          IF l_aux2+l_long(1) = '0' OR l_aux2+l_long(1) = ''.
            l_aux2 = l_aux2(l_long).
            CONDENSE l_aux2 NO-GAPS.
          ELSE.
            EXIT.
          ENDIF.
          l_long = strlen( l_aux2 ).
        ENDWHILE.
        CONCATENATE l_aux1 l_aux2 INTO string SEPARATED BY '.'.
      ENDIF.
    ENDIF.
    CONDENSE string NO-GAPS.
  ENDMETHOD.
  METHOD poner_ceros.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = cadena
      IMPORTING output = salida.
  ENDMETHOD.
  METHOD poner_ceros_c.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = cadena
      IMPORTING output = cadena.
  ENDMETHOD.
  METHOD popup_texto.
    DATA: l_titulo  TYPE string,
          l_display TYPE xfeld,
          l_string  TYPE string,
          l_mod     TYPE xfeld,
          l_canc    TYPE xfeld.

    IF zcl_ap_popup=>popup_permitido( ) = ''.
      RETURN.
    ENDIF.

    CLEAR: modificado,
           cancelado.
    l_titulo = titulo.
    IF editar IS INITIAL.
      l_display = 'X'.
      IF l_titulo IS INITIAL.
        l_titulo = 'Visualizar texto'.
      ENDIF.
    ELSE.
      IF l_titulo IS INITIAL.
        l_titulo = 'Editar texto'.
      ENDIF.
    ENDIF.

    l_string = texto.
    CALL FUNCTION 'Z_POPUP_EDIT_TEXT'
      EXPORTING titulo       = l_titulo
                texto        = intro
*                INI_X        = 5
*                INI_Y        = 3
*                FIN_X        = 140
*                FIN_Y        = 24
                max_cols     = max_cols
                display_mode = l_display
                titulo_texto_corto = titulo_texto_corto
      IMPORTING modificado   = l_mod
                cancelado    = l_canc
      CHANGING  string       = l_string
                texto_corto  = texto_corto.

    modificado = l_mod.
    cancelado = l_canc.

    IF modificado = 'X'.
      texto = l_string.
    ENDIF.
  ENDMETHOD.
  METHOD quitar_caracteres_extranos.
    IF string IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING  intext            = string
*                 INTEXT_LG         = 0
*                 INTER_CP          = '0000'
*                 INTER_BASE_CP     = '0000'
*                 IN_CP             = '0000'
                 replacement       = replacement
      IMPORTING  outtext           = string
*                 OUTUSED           =
*                 OUTOVERFLOW       =
      EXCEPTIONS invalid_codepage  = 1
                 codepage_mismatch = 2
                 internal_error    = 3
                 cannot_convert    = 4
                 fields_not_type_c = 5
                 OTHERS            = 6.

    IF sy-subrc <> 0.
      MESSAGE 'Error reemplazando carácteres'(erc) TYPE 'S'.
    ELSE.
      IF string CS '€'.
        REPLACE ALL OCCURRENCES OF '€' IN string WITH 'EUR'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD quitar_ceros.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING input  = cadena
      IMPORTING output = salida.
  ENDMETHOD.
  METHOD quitar_ceros_c.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING input  = cadena
      IMPORTING output = cadena.
  ENDMETHOD.
  METHOD right.
    DATA: l_long TYPE i,
          l_char TYPE c LENGTH 1000.

    l_long = strlen( entrada ).
    IF long > l_long.
      salida = entrada.
    ELSE.
      l_char = entrada.
      l_long = l_long - long.
      salida = l_char+l_long(long).
    ENDIF.
  ENDMETHOD.
  METHOD siguiente_letra.
    DATA l_ascii TYPE i.

    l_ascii = cl_abap_conv_out_ce=>uccpi( letra ).
    l_ascii = l_ascii + 1.
    siguiente = cl_abap_conv_in_ce=>uccpi( l_ascii ).
  ENDMETHOD.
  METHOD split_left.
    DATA: l_long    TYPE i,
          i_strings TYPE TABLE OF string.

    CLEAR izquierda.
    IF string CS separador.
      l_long = strlen( separador ).
      SPLIT string AT separador(l_long) INTO TABLE i_strings.
      DESCRIBE TABLE i_strings LINES sy-tfill.
      IF sy-tfill = 2.
        READ TABLE i_strings INDEX 1 INTO izquierda.
        READ TABLE i_strings INDEX 2 INTO derecha.
      ELSE.
        READ TABLE i_strings INDEX sy-tfill INTO derecha.
        l_long = strlen( string ) - ( strlen( derecha ) + strlen( separador ) ).
        izquierda = string(l_long).
      ENDIF.
    ELSE.
      derecha = string.
    ENDIF.
  ENDMETHOD.
  METHOD string2cantidad.
    DATA: l_string   TYPE string,
          l_potencia TYPE n LENGTH 2.

    TRY.
        l_string = string.
        REPLACE '.' WITH '' INTO l_string.
        REPLACE '.' WITH '' INTO l_string.
        REPLACE ',' WITH '.' INTO l_string.
        CONDENSE l_string NO-GAPS.

        l_string = to_upper( l_string ).
        IF l_string CS 'E+'.
          SPLIT l_string AT 'E+' INTO l_string l_potencia.
          IF l_potencia = '00'.
            importe = l_string.
          ELSE.
            importe = l_string * ( 10 ** l_potencia ).
          ENDIF.
        ELSEIF l_string CS 'E-'.
          SPLIT l_string AT 'E-' INTO l_string l_potencia.
          IF l_potencia = '00'.
            importe = l_string.
          ELSE.
            importe = l_string / ( 10 ** l_potencia ).
          ENDIF.
        ELSE.
          importe = l_string.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
        MESSAGE e398(00) WITH 'Imposible convertir'(imc) string 'a importe'(aim) ''.
      CATCH cx_sy_conversion_overflow.
        MESSAGE e398(00) WITH 'Imposible convertir'(imc) string 'a importe'(aim) ''.

    ENDTRY.
  ENDMETHOD.
  METHOD string2ctd.
    DATA: l_string    TYPE string,
          l_ctd_texto TYPE c LENGTH 20,
          l_error     TYPE c LENGTH 1,
          l_cantidad  TYPE c LENGTH 20,
          l_message   TYPE message,
          l_decimales TYPE c LENGTH 20.

    DATA: lv_value      TYPE char100,
          lv_tabfield   TYPE tabfield,
          lv_cell_value TYPE pgpl-absat.

    CLEAR: mensaje,
           cantidad.

* Busco si

    l_string = ctd_texto.
    quitar_caracteres_extranos( EXPORTING replacement = 32
                                CHANGING  string      = l_string ).
    lv_value = l_string.

    lv_value = to_upper( lv_value ).
    IF lv_value CS 'E'.

      lv_tabfield-tabname   = 'PGPL'.
      lv_tabfield-fieldname = 'ABSAT'.
      REPLACE '.' WITH ',' INTO lv_value.
      CALL FUNCTION 'RS_CONV_EX_2_IN'
        EXPORTING  input_external               = lv_value
                   table_field                  = lv_tabfield
        IMPORTING  output_internal              = lv_cell_value
        EXCEPTIONS input_not_numerical          = 1
                   too_many_decimals            = 2
                   more_than_one_sign           = 3
                   ill_thousand_separator_dist  = 4
                   too_many_digits              = 5
                   sign_for_unsigned            = 6
                   too_large                    = 7
                   too_small                    = 8
                   invalid_date_format          = 9
                   invalid_date                 = 10  ##NUMBER_OK
                   invalid_time_format          = 11  ##NUMBER_OK
                   invalid_time                 = 12  ##NUMBER_OK
                   invalid_hex_digit            = 13  ##NUMBER_OK
                   unexpected_error             = 14  ##NUMBER_OK
                   invalid_fieldname            = 15  ##NUMBER_OK
                   field_and_descr_incompatible = 16  ##NUMBER_OK
                   input_too_long               = 17  ##NUMBER_OK
                   no_decimals                  = 18  ##NUMBER_OK
                   invalid_float                = 19  ##NUMBER_OK
                   conversion_exit_error        = 20  ##NUMBER_OK
                   OTHERS                       = 21 ##NUMBER_OK.

      IF sy-subrc = 0.
        TRY.
            cantidad = lv_cell_value.
          CATCH cx_sy_conversion_no_number.
            lv_value = lv_cell_value.
            CONCATENATE 'Error de conversión'(eco) lv_value INTO mensaje SEPARATED BY space.
          CATCH cx_sy_conversion_overflow.
            lv_value = lv_cell_value.
            CONCATENATE 'Cantidad'(ctd) lv_value 'es demasiado grande para variable de salida'(dts) INTO mensaje SEPARATED BY space.
          CATCH cx_root. "#EC *
            lv_value = lv_cell_value.
            CONCATENATE 'Cantidad indeterminado en conversión'(cic) lv_value INTO mensaje SEPARATED BY space.
        ENDTRY.
        RETURN.
      ENDIF.
    ENDIF.

    l_ctd_texto = l_string.
    IF l_ctd_texto CS ','.
      IF l_ctd_texto CS '.'.
        mensaje = 'Utilize el . como separador de decimales'(usd).
      ELSE.
        REPLACE ',' WITH '.' INTO l_ctd_texto.
      ENDIF.
    ENDIF.

    IF NOT mensaje IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CHECK_AND_CONVERT_NUMERICS'
      EXPORTING
*                DFELD        = ' '
                dmzei        = '.'
                dtype        = 'QUAN'
*                DYPNO        = ' '
                efeld        = l_ctd_texto
*                FNAME        = ' '
*                PROGR        = ' '
                imp_decimals = '3'
      IMPORTING error        = l_error
                ifeld        = l_cantidad
                messg        = l_message.
*                MSGLN        = MSGLN
    IF l_message-msgty = 'E' AND l_message-msgid = 'CH' AND l_message-msgno = '142'. " Sólo se permiten '3 ' decimales
      CLEAR: l_error,
             l_message.
      SPLIT l_ctd_texto AT '.' INTO l_cantidad l_decimales.
      CONCATENATE l_cantidad '.' l_decimales(3) INTO l_ctd_texto.
      CALL FUNCTION 'CHECK_AND_CONVERT_NUMERICS'
        EXPORTING
*                  DFELD        = ' '
                  dmzei        = '.'
                  dtype        = 'QUAN'
*                  DYPNO        = ' '
                  efeld        = l_ctd_texto
*                  FNAME        = ' '
*                  PROGR        = ' '
                  imp_decimals = '3'
        IMPORTING error        = l_error
                  ifeld        = l_cantidad
                  messg        = l_message.
    ENDIF.
    IF l_error = 'X'.
      IF l_message-msgtx IS INITIAL.
        mensaje = 'Se ha produccido un error al convertir cantidad'(spe).
      ELSE.
        mensaje = l_message-msgtx.
      ENDIF.
      CLEAR cantidad.
    ELSE.
      CLEAR mensaje.
      TRY.
          cantidad = l_cantidad.
        CATCH cx_sy_conversion_no_number.
          lv_value = lv_cell_value.
          CONCATENATE 'Error de conversión'(eco) lv_value INTO mensaje SEPARATED BY space.
        CATCH cx_sy_conversion_overflow.
          lv_value = lv_cell_value.
          CONCATENATE 'Cantidad'(ctd) lv_value 'es demasiado grande para variable de salida'(dts) INTO mensaje SEPARATED BY space.
        CATCH cx_root. "#EC *
          lv_value = lv_cell_value.
          CONCATENATE 'Cantidad indeterminado en conversión'(cic) lv_value INTO mensaje SEPARATED BY space.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD string2float.
    CALL FUNCTION 'CHAR_FLTP_CONVERSION'
      EXPORTING
*                 DYFLD              = ' '
*                 MASKN              = ' '
*                 MAXDEC             = '16'
*                 MAXEXP             = '59+'
*                 MINEXP             = '60-'
                 string             = string
*                 MSGTYP_DECIM       = 'W'
      IMPORTING
*                 DECIM              =
*                 EXPON              =
                 flstr              = float
*                 IVALU              =
      EXCEPTIONS exponent_too_big   = 1
                 exponent_too_small = 2
                 string_not_fltp    = 3
                 too_many_decim     = 4
                 OTHERS             = 5.

    IF sy-subrc <> 0.
      float = c_error_float.
    ENDIF.
  ENDMETHOD.
  METHOD string2importe.
    DATA: l_punto    TYPE c LENGTH 1,
          l_coma     TYPE c LENGTH 1,
          l_string   TYPE string,
          l_potencia TYPE n LENGTH 2.

    IF string CS '.'.
      l_punto = 'X'.
    ENDIF.
    IF string CS ','.
      l_coma = 'X'.
    ENDIF.

    TRY.
        l_string = string.
        IF NOT ( l_punto = 'X' AND l_coma = '' ).
          REPLACE '.' WITH '' INTO l_string.
          REPLACE '.' WITH '' INTO l_string.
          REPLACE ',' WITH '.' INTO l_string.
        ENDIF.
        CONDENSE l_string NO-GAPS.

        IF l_string CS 'E+'.
          SPLIT l_string AT 'E+' INTO l_string l_potencia.
          IF l_potencia = ''.
            importe = l_string * 10.
          ELSEIF l_potencia = '00'.
            importe = l_string.
          ELSE.
            importe = l_string * ( 10 ** l_potencia ).
          ENDIF.
        ELSEIF l_string CS 'E-'.
          SPLIT l_string AT 'E-' INTO l_string l_potencia.
          IF l_potencia = '00'.
            importe = l_string.
          ELSE.
            importe = l_string / ( 10 ** l_potencia ).
          ENDIF.
        ELSE.
          importe = l_string.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
        MESSAGE e398(00) WITH 'Imposible convertir'(imc) string 'a importe'(aim) ''.
      CATCH cx_sy_conversion_overflow.
        MESSAGE e398(00) WITH 'Imposible convertir'(imc) string 'a importe'(aim) ''.
    ENDTRY.
  ENDMETHOD.
  METHOD string2tabla.
* Divide una lÃ­nea que llega con sÃ¡ltos de pÃ¡gina a un formato que puede
* introducirse directamente con la funciÃ³n SAVE_TEXT, convirtiendo el
* carÃ¡cter retorno de carro 0D0A en una nueva lÃ­nea, asÃ­ mismo si el
* texto tuviera mÃ¡s de 132 carÃ¡cteres lo parte.
************************************************************************

    DATA: l_long   TYPE i,
          l_veces  TYPE i,
          l_inicio TYPE i,
          l_cont   TYPE i,
          l_aux    TYPE i.
    DATA l_lin TYPE c LENGTH 65535.
    DATA c_aux TYPE c LENGTH 1.
    DATA: l_enter1    TYPE c LENGTH 1,
          l_fin_linea TYPE c LENGTH 1.
    DATA l_linea TYPE c LENGTH 65535.

* Los XML no tienen saltos y nos los parte bien.
    IF NOT string CS cl_abap_char_utilities=>cr_lf AND string CS '/><'.
      tabla = zcl_ap_ws=>xml_str2table( string ).
      RETURN.
    ENDIF.

    l_long = strlen( string ).
    l_veces = 1 + floor( l_long / 65535 ) ##NUMBER_OK.
    IF l_veces > 1.
      SPLIT string AT cl_abap_char_utilities=>cr_lf INTO TABLE tabla.
    ELSE.
      DO l_veces TIMES.
        l_inicio = ( sy-index - 1 ) * 65535 ##NUMBER_OK.
        IF l_inicio >= l_long.
          EXIT.
        ENDIF.
        l_lin = string+l_inicio.
        CLEAR l_cont.
        DO.
          l_cont = l_cont + 1.
          c_aux = l_lin(1).

          IF c_aux = cl_abap_char_utilities=>cr_lf+1(1).
            IF l_enter1 = 'X' OR forzar_enter = 'X'.
              IF l_fin_linea = 'X' AND l_linea IS INITIAL.
                CLEAR l_fin_linea.
              ELSE.
                APPEND l_linea TO tabla.
              ENDIF.
              CLEAR l_linea.
              CLEAR l_cont.
            ENDIF.
          ENDIF.

          IF c_aux = cl_abap_char_utilities=>cr_lf(1).
            l_enter1 = 'X'.
          ELSE.
            CLEAR l_enter1.
          ENDIF.

          IF     c_aux <> cl_abap_char_utilities=>cr_lf(1)
             AND c_aux <> cl_abap_char_utilities=>cr_lf+1(1).
            l_aux = l_cont - 1.
            l_linea+l_aux(1) = c_aux.
          ENDIF.

          SHIFT l_lin LEFT.
          IF l_lin IS INITIAL.
            IF NOT l_linea IS INITIAL.
              APPEND l_linea TO tabla.
              CLEAR l_cont.
              CLEAR l_fin_linea.
            ENDIF.
            EXIT.
          ENDIF.

          IF l_cont >= longitud.
            IF partir_solo_en_blanco = 'X' AND c_aux <> ''.
              CONTINUE.
            ENDIF.
            APPEND l_linea TO tabla.
            CLEAR l_linea.
            CLEAR l_cont.
            l_fin_linea = 'X'.
          ENDIF.

        ENDDO.
      ENDDO.
    ENDIF.

*  CALL FUNCTION 'SWA_STRING_TO_TABLE'
*    EXPORTING
*      character_string                 = string
*    IMPORTING
*      CHARACTER_TABLE                  = tabla
*   EXCEPTIONS
*      NO_FLAT_CHARLIKE_STRUCTURE       = 1
*      OTHERS                           = 2.
  ENDMETHOD.
  METHOD string2tablastring.
*Divide una lÃ­nea que llega con sÃ¡ltos de pÃ¡gina a un formato que
*puede
* introducirse directamente con la funciÃ³n SAVE_TEXT, convirtiendo el
* carÃ¡cter retorno de carro 0D0A en una nueva lÃ­nea, asÃ­ mismo si el
* texto tuviera mÃ¡s de 132 carÃ¡cteres lo parte.
************************************************************************

    DATA l_lin TYPE c LENGTH 10000.
    DATA: l_cont   TYPE i,
          l_string TYPE zlinea_string,
          l_aux    TYPE i.
    DATA c_aux TYPE c LENGTH 1.
    DATA: l_enter1    TYPE c LENGTH 1,
          l_fin_linea TYPE c LENGTH 1.
    DATA l_linea TYPE c LENGTH 10000.

    REFRESH tabla.
    l_lin = string.
    CLEAR l_cont.
    DO.
      l_cont = l_cont + 1.
      c_aux = l_lin(1).

      IF c_aux = cl_abap_char_utilities=>cr_lf+1(1).
        IF l_enter1 = 'X'.
          IF l_fin_linea = 'X' AND l_linea IS INITIAL.
            CLEAR l_fin_linea.
          ELSE.
            IF longitud < 9999 ##NUMBER_OK.
              APPEND l_linea TO tabla.
            ELSE.
              l_string-linea = l_linea.
              APPEND l_string TO tabla.
            ENDIF.
          ENDIF.
          CLEAR l_linea.
          CLEAR l_cont.
        ENDIF.
      ENDIF.

      IF c_aux = cl_abap_char_utilities=>cr_lf(1).
        l_enter1 = 'X'.
      ELSE.
        CLEAR l_enter1.
      ENDIF.

      IF     c_aux <> cl_abap_char_utilities=>cr_lf(1)
         AND c_aux <> cl_abap_char_utilities=>cr_lf+1(1).
        l_aux = l_cont - 1.
        l_linea+l_aux(1) = c_aux.
      ENDIF.

      SHIFT l_lin LEFT.
      IF l_lin IS INITIAL.
        IF NOT l_linea IS INITIAL.
          IF longitud < 9999 ##NUMBER_OK.
            APPEND l_linea TO tabla.
          ELSE.
            l_string-linea = l_linea.
            APPEND l_string TO tabla.
          ENDIF.
          CLEAR l_cont.
          CLEAR l_fin_linea.
        ENDIF.
        EXIT.
      ENDIF.

      IF l_cont = longitud.
        IF longitud < 9999 ##NUMBER_OK.
          APPEND l_linea TO tabla.
        ELSE.
          l_string-linea = l_linea.
          APPEND l_string TO tabla.
        ENDIF.
        CLEAR l_linea.
        CLEAR l_cont.
        l_fin_linea = 'X'.
      ENDIF.

    ENDDO.

*  CALL FUNCTION 'SWA_STRING_TO_TABLE'
*    EXPORTING
*      character_string                 = string
*    IMPORTING
*      CHARACTER_TABLE                  = tabla
*   EXCEPTIONS
*      NO_FLAT_CHARLIKE_STRUCTURE       = 1
*      OTHERS                           = 2.
  ENDMETHOD.
  METHOD string2tline.
* Divide una lÃ­nea que llega con sÃ¡ltos de pÃ¡gina a un formato que puede
* introducirse directamente con la funciÃ³n SAVE_TEXT, convirtiendo el
* carÃ¡cter retorno de carro 0D0A en una nueva lÃ­nea, asÃ­ mismo si el
* texto tuviera mÃ¡s de 132 carÃ¡cteres lo parte.
************************************************************************

    DATA l_lin TYPE c LENGTH 10000.
    DATA: l_cont  TYPE i,
          l_tline TYPE tline,
          l_aux   TYPE i.
    DATA c_aux TYPE c LENGTH 1.
    DATA: l_enter1    TYPE c LENGTH 1,
          l_fin_linea TYPE c LENGTH 1.

    CLEAR tabla.

    l_lin = string.
    CLEAR l_cont.
    DO.
      l_cont = l_cont + 1.
      c_aux = l_lin(1).

      IF c_aux = cl_abap_char_utilities=>cr_lf+1(1).
        IF l_enter1 = 'X'.
          IF l_fin_linea = 'X' AND l_tline-tdline IS INITIAL.
            CLEAR l_fin_linea.
          ELSE.
            APPEND l_tline TO tabla.
          ENDIF.
          CLEAR l_tline.
          l_tline-tdformat = '*'.
          CLEAR l_cont.
        ENDIF.
      ENDIF.

      IF c_aux = cl_abap_char_utilities=>cr_lf(1).
        l_enter1 = 'X'.
      ELSE.
        CLEAR l_enter1.
      ENDIF.

      IF     c_aux <> cl_abap_char_utilities=>cr_lf(1)
         AND c_aux <> cl_abap_char_utilities=>cr_lf+1(1).
        l_aux = l_cont - 1.
        l_tline-tdline+l_aux(1) = c_aux.
      ENDIF.

      SHIFT l_lin LEFT.
      IF l_lin IS INITIAL.
        IF NOT l_tline-tdline IS INITIAL.
          APPEND l_tline TO tabla.
          CLEAR l_cont.
          CLEAR l_fin_linea.
        ENDIF.
        EXIT.
      ENDIF.

      IF l_cont = 132 ##NUMBER_OK.
        APPEND l_tline TO tabla.
        CLEAR l_tline.
        l_tline-tdformat = '='.
        CLEAR l_cont.
        l_fin_linea = 'X'.
      ENDIF.

    ENDDO.
  ENDMETHOD.
  METHOD string2xstring.
    DATA: l_encoding TYPE abap_encoding,
          converter  TYPE REF TO cl_abap_conv_out_ce.

    l_encoding = encoding.
    converter = cl_abap_conv_out_ce=>create( encoding = l_encoding ).
    converter->reset( ).
    TRY.
        converter->write( data = string ).

        xstring = converter->get_buffer( ).
      CATCH cx_root. "#EC *
        RETURN.
    ENDTRY.

    IF NOT xstring IS INITIAL AND comprimir = 'X'.
      TRY.
          cl_abap_gzip=>compress_binary( EXPORTING raw_in   = xstring
                                         IMPORTING gzip_out = xstring ).
        CATCH cx_parameter_invalid_range. "#EC *
        CATCH cx_sy_buffer_overflow. "#EC *
        CATCH cx_sy_compression_error. "#EC *
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD string_estandar.
    salida = entrada.
    limpiar_nombre_fichero( CHANGING string = salida ).
    salida = to_upper( salida ).
  ENDMETHOD.
  METHOD string_pot_2importe.
    DATA: l_string   TYPE string,
          l_potencia TYPE n LENGTH 2.

    TRY.
        l_string = string.
        IF l_string CS 'E+'.
          SPLIT l_string AT 'E+' INTO l_string l_potencia.
          IF l_potencia = '00'.
            importe = l_string.
          ELSE.
            importe = l_string * ( 10 ** l_potencia ).
          ENDIF.
        ELSEIF l_string CS 'E-'.
          SPLIT l_string AT 'E-' INTO l_string l_potencia.
          IF l_potencia = '00'.
            importe = l_string.
          ELSE.
            importe = l_string / ( 10 ** l_potencia ).
          ENDIF.
        ELSE.
          importe = l_string.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
        MESSAGE e398(00) WITH 'Imposible convertir'(imc) string 'a importe'(aim) ''.
      CATCH cx_sy_conversion_overflow.
        MESSAGE e398(00) WITH 'Imposible convertir'(imc) string 'a importe'(aim) ''.

    ENDTRY.
  ENDMETHOD.
  METHOD string_to_binary_tab.
    DATA lv_xstring TYPE xstring.

* Convert the string into xstring
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING  text   = string
      IMPORTING  buffer = lv_xstring
      EXCEPTIONS failed = 1
                 OTHERS = 2.

    IF sy-subrc = 0.
* Convert the string into binary table
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING buffer     = lv_xstring
        TABLES    binary_tab = itab.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD tabla2string.
    DATA: l_linea TYPE string,
          l_first TYPE c LENGTH 1.

    CLEAR string.
    LOOP AT tabla INTO l_linea.
      CLEAR l_first.
      AT FIRST.
        l_first = 'X'.
      ENDAT.

      IF l_first = 'X'.
        string = l_linea.
      ELSE.
        IF separar_con_espacio IS INITIAL.
          CONCATENATE string cl_abap_char_utilities=>cr_lf l_linea
                      INTO string.
        ELSE.
          CONCATENATE string l_linea INTO string SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.

*  CALL FUNCTION 'SWA_STRING_FROM_TABLE'
*    EXPORTING
*      character_table                  = tabla
**     NUMBER_OF_CHARACTERS             =
**     LINE_SIZE                        =
**     KEEP_TRAILING_SPACES             = ' '
**     CHECK_TABLE_TYPE                 = ' '
*    IMPORTING
*      CHARACTER_STRING                 = string
*    EXCEPTIONS
*      NO_FLAT_CHARLIKE_STRUCTURE       = 1
*      OTHERS                           = 2.
  ENDMETHOD.
  METHOD tline2string.
    DATA tline TYPE tline.

    CLEAR string.
    LOOP AT tabla INTO tline.
      IF string IS INITIAL.
        string = tline-tdline.
      ELSE.
        IF tline-tdformat = '=' OR tline-tdformat = ''.

          IF tline-tdformat = '' AND blancos_son_saltos = 'X'.
            CONCATENATE string cl_abap_char_utilities=>cr_lf tline-tdline INTO string.
          ELSE.
            CONCATENATE string tline-tdline INTO string.
          ENDIF.
        ELSE.
          CONCATENATE string cl_abap_char_utilities=>cr_lf tline-tdline INTO string.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD to_clipboard.
    DATA: i_data TYPE TABLE OF text4096,
          l_data TYPE text4096,
          l_rc   TYPE i.

    FIELD-SYMBOLS: <linea> TYPE any,
                   <campo> TYPE any.

    IF NOT string IS INITIAL.
      string2tabla( EXPORTING string   = string
                              longitud = 4096
                    CHANGING  tabla    = i_data ).
    ELSEIF NOT tabla IS INITIAL.
      LOOP AT tabla ASSIGNING <linea>.
        CLEAR: l_data,
               sy-subrc.
        WHILE sy-subrc = 0.
          ASSIGN COMPONENT sy-index OF STRUCTURE <linea> TO <campo>.
          IF sy-subrc = 0.
            IF l_data IS INITIAL.
              l_data = <campo>.
            ELSE.
              CONCATENATE l_data cl_bcs_convert=>gc_tab <campo> INTO l_data.
            ENDIF.
          ENDIF.
        ENDWHILE.
        APPEND l_data TO i_data.
      ENDLOOP.
    ENDIF.

    cl_gui_frontend_services=>clipboard_export( EXPORTING  no_auth_check        = 'X'
                                                IMPORTING  data                 = i_data
                                                CHANGING   rc                   = l_rc
                                                EXCEPTIONS cntl_error           = 1
                                                           error_no_gui         = 2
                                                           not_supported_by_gui = 3
                                                           no_authority         = 4
                                                           OTHERS               = 5 ).

    IF sy-subrc <> 0.
      IF sy-msgty IS INITIAL.
        MESSAGE 'Error copiando datos al portapapeles' TYPE 'E'.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      IF show_msg = 'X'.
        MESSAGE 'Se han copiado datos al portapapeles'(cdp) TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD transform_object_to_rawstring.
    TYPES line_t  TYPE x LENGTH 4096.
    TYPES table_t TYPE STANDARD TABLE OF line_t.

    DATA lt_restab TYPE table_t.

    FIELD-SYMBOLS <lfs_restab> LIKE LINE OF lt_restab.

    IF NOT objeto IS INITIAL.
      CALL TRANSFORMATION id_indent
           SOURCE obj = objeto
           RESULT XML lt_restab.
    ELSEIF NOT tabla IS INITIAL.
      IF json IS INITIAL.
        CALL TRANSFORMATION id_indent
             SOURCE obj = tabla
             RESULT XML lt_restab.
      ELSE.
        DATA(json_data) = /ui2/cl_json=>serialize( data     = tabla
                                                   compress = 'X' ).
      ENDIF.
    ELSEIF NOT variable IS INITIAL.
      IF json IS INITIAL.
        CALL TRANSFORMATION id_indent
             SOURCE obj = variable
             RESULT XML lt_restab.
      ELSE.
        json_data = /ui2/cl_json=>serialize( data     = variable
                                             compress = 'X' ).
      ENDIF.
    ELSE.
* No hay datos de entrada, salimos
      RETURN.
    ENDIF.

    IF lt_restab IS NOT INITIAL OR NOT json IS INITIAL.
      IF NOT json_data IS INITIAL.
        xstring = string2xstring( json_data ).
      ELSE.
        LOOP AT lt_restab ASSIGNING <lfs_restab>.
          CONCATENATE xstring <lfs_restab>
                      INTO xstring IN BYTE MODE.
        ENDLOOP.
      ENDIF.

**compresss here
      IF comprimir = 'X'.
        TRY.
            cl_abap_gzip=>compress_binary( EXPORTING raw_in   = xstring
                                           IMPORTING gzip_out = xstring ).
          CATCH cx_parameter_invalid_range. "#EC *
          CATCH cx_sy_buffer_overflow. "#EC *
          CATCH cx_sy_compression_error. "#EC *
        ENDTRY.
      ENDIF.
    ELSE.
      RAISE transformation_failed.
    ENDIF.
  ENDMETHOD.
  METHOD transform_rawstring_to_object.
    DATA: l_string   TYPE string,
          l_xstring  TYPE xstring,
          l_encoding TYPE abap_encoding,
          l_err_root TYPE REF TO cx_root.

    CLEAR xstring_decompressed.
    IF xstring IS INITIAL AND string IS INITIAL.
      RETURN.
    ENDIF.

    IF xstring IS INITIAL AND NOT string IS INITIAL.
      l_string = string.
      l_xstring = zcl_ap_string=>string2xstring( l_string ).
    ELSE.
      l_xstring = xstring.
    ENDIF.

***Restore the object here
****first decompress the object
*now decompress
    IF descomprimir IS INITIAL.
      xstring_decompressed = l_xstring.
    ELSE.
      TRY.
          cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = l_xstring
*                                                     gzip_in_len = -1
                                           IMPORTING raw_out = xstring_decompressed
*                                                     raw_out_len =
            ).
        CATCH cx_parameter_invalid_range. "#EC *
        CATCH cx_sy_buffer_overflow. "#EC *
        CATCH cx_sy_compression_error. "#EC *
          IF NOT string IS INITIAL AND NOT codepage IS INITIAL.
            l_encoding = codepage.
            l_xstring = string2xstring( string   = string
                                        encoding = l_encoding ).
            TRY.
                cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = l_xstring
*                                                           gzip_in_len = -1
                                                 IMPORTING raw_out = xstring_decompressed
*                                                           raw_out_len =
                  ).
              CATCH cx_parameter_invalid_range. "#EC *
              CATCH cx_sy_buffer_overflow. "#EC *
              CATCH cx_sy_compression_error. "#EC *
                l_xstring = string2xstring( string   = string
                                            encoding = '4103' ).
                TRY.
                    cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = l_xstring
                                                     IMPORTING raw_out = xstring_decompressed  ).
                  CATCH cx_parameter_invalid_range. "#EC *
                  CATCH cx_sy_buffer_overflow. "#EC *
                  CATCH cx_sy_compression_error. "#EC *
                ENDTRY.
            ENDTRY.
          ENDIF.
        CATCH cx_root. "#EC *
      ENDTRY.
    ENDIF.

    IF visualizar_xml = 'X'.
      zcl_ap_ws=>ver_xml( x_xml = xstring_decompressed ).
      RETURN.
    ENDIF.

    " revert TRANSFORMATION

    TRY.
        IF json = 'X'.
          DATA(json_data) = xstring2string( xstring_decompressed ).
        ENDIF.

        IF get_objeto = 'X'.
          CALL TRANSFORMATION id_indent
               SOURCE XML xstring_decompressed
               RESULT obj = objeto.

          IF NOT objeto IS BOUND.
            RAISE transformation_failed.
          ENDIF.
        ELSEIF get_tabla = 'X'.
          IF json IS INITIAL.
            CALL TRANSFORMATION id_indent
                 SOURCE XML xstring_decompressed
                 RESULT obj = tabla.
          ELSE.
            /ui2/cl_json=>deserialize( EXPORTING json = json_data
                                       CHANGING  data = tabla ).

          ENDIF.
        ELSEIF get_variable = 'X'.
          IF json IS INITIAL.
            CALL TRANSFORMATION id_indent
                 SOURCE XML xstring_decompressed
                 RESULT obj = variable.
          ELSE.
            /ui2/cl_json=>deserialize( EXPORTING json = json_data
                                       CHANGING  data = tabla ).

          ENDIF.
        ENDIF.
      CATCH cx_root INTO l_err_root. "#EC *
        l_string = l_err_root->get_text( ).
        CONCATENATE 'Error XML:'(exm) l_string INTO l_string SEPARATED BY space.
        MESSAGE l_string TYPE 'S'.
        RAISE transformation_failed.
    ENDTRY.

*METHOD TEST_TRANSFORM.
*   DATA: LO_OBJECT TYPE REF TO ZCL_TEST,
*         LO_OBJECT_RECOVERED TYPE REF TO ZCL_TEST,
*         LO_OBJECT_SERILIAZABLE TYPE REF TO IF_SERIALIZABLE_OBJECT,
*         LV_XSTRING_COMPRESSED TYPE XSTRING
*        .
*
*
*
*   CREATE OBJECT LO_OBJECT.
*
*   LV_XSTRING_COMPRESSED =
*   ZCL_AKSA_GENERAL=>TRANSFORM_OBJECT_TO_RAWSTRING(
*   IO_OBJECT = LO_OBJECT
*     ).
*
*
*   ZCL_AKSA_GENERAL=>TRANSFORM_RAWSTRING_TO_OBJECT(
*      EXPORTING
*        IV_XSTRING            = LV_XSTRING_COMPRESSED
*      IMPORTING
*        EO_OBJECT  =   LO_OBJECT_SERILIAZABLE
*     EXCEPTIONS
*       TRANSFORMATION_FAILED = 1
*          ).
*   IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*   ENDIF.
*
*
****type cast
*   LO_OBJECT_RECOVERED ?= LO_OBJECT_SERILIAZABLE.
*
*
**  CL_AUNIT_ASSERT=>ASSERT_EQUALS(
**      EXP                  = LO_OBJECT
**      ACT                  = LO_OBJECT_RECOVERED
***    MSG                  = MSG
***      LEVEL                = CRITICAL
***    TOL                  = TOL
***    QUIT                 = METHOD
***    IGNORE_HASH_SEQUENCE = ABAP_FALSE
**         ).
*
*
*
*ENDMETHOD.
  ENDMETHOD.
  METHOD ultimo_caracter.
    DATA l_long TYPE i.

    l_long = strlen( texto ) - num_car.
    IF l_long >= 0.
      char = texto+l_long(num_car).
    ENDIF.
  ENDMETHOD.
  METHOD xstring2base64.
    DATA l_long TYPE i.

    IF xstring IS INITIAL.
      RETURN.
    ENDIF.

    l_long = xstrlen( xstring ).
    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING  bindata                  = xstring
                 binleng                  = l_long
      IMPORTING  b64data                  = base64
      EXCEPTIONS ssf_krn_error            = 1
                 ssf_krn_noop             = 2
                 ssf_krn_nomemory         = 3
                 ssf_krn_opinv            = 4
                 ssf_krn_input_data_error = 5
                 ssf_krn_invalid_par      = 6
                 ssf_krn_invalid_parlen   = 7
                 OTHERS                   = 8.
    IF sy-subrc <> 0.
      MESSAGE 'Error codificando a base 64' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD xstring2string.
    DATA: l_xstring TYPE xstring,
          loc_conv  TYPE REF TO cl_abap_conv_in_ce.

    IF NOT xstring IS INITIAL AND descomprimir = 'X'.
      TRY.
          cl_abap_gzip=>decompress_binary( EXPORTING gzip_in = xstring
                                           IMPORTING raw_out = l_xstring ).
        CATCH cx_parameter_invalid_range. "#EC *
        CATCH cx_sy_buffer_overflow. "#EC *
      ENDTRY.
    ELSE.
      l_xstring = xstring.
    ENDIF.

    loc_conv = cl_abap_conv_in_ce=>create( input       = l_xstring
                                           encoding    = encoding
                                           replacement = '?'
                                           ignore_cerr = abap_true ).

    TRY.
        loc_conv->read( IMPORTING data = string ).
      CATCH cx_sy_conversion_codepage. "#EC *
*-- Should ignore errors in code conversions
      CATCH cx_sy_codepage_converter_init. "#EC *
*-- Should ignore errors in code conversions
      CATCH cx_parameter_invalid_type. "#EC *
      CATCH cx_parameter_invalid_range. "#EC *
    ENDTRY.
  ENDMETHOD.
