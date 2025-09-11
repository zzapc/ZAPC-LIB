CLASS zcl_ap_envio_mail DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA i_texto_mail    TYPE srm_t_solisti1.
    DATA i_destinatarios TYPE swftlisti1.
    DATA o_send_request  TYPE REF TO cl_bcs.
    DATA o_document      TYPE REF TO cl_document_bcs.
    DATA i_adjuntos      TYPE rmps_t_post_content.
    DATA message         TYPE bapi_msg.
    DATA plantilla       TYPE zap_textos_mail.
    DATA msg_pdf         TYPE string.

    METHODS constructor
      IMPORTING usar_clases TYPE abap_bool DEFAULT 'X'.

    METHODS envio_mail
      IMPORTING subject              TYPE any
                direccion            TYPE any       OPTIONAL
                urgente              TYPE abap_bool DEFAULT 'X'
                html                 TYPE abap_bool DEFAULT ''
                !commit              TYPE abap_bool DEFAULT 'X'
                forzar_mail_externo  TYPE abap_bool DEFAULT ''
                confirmacion_lectura TYPE bcs_rqst  DEFAULT 'E'
                emisor               TYPE any       DEFAULT ''
                lista_distribucion   TYPE abap_bool DEFAULT ''
                dest_copia           TYPE any       DEFAULT ''
                dest_copia_oculta    TYPE any       DEFAULT ''
                mostrar_info_error   TYPE any       DEFAULT 'X'
                controlar_salida     TYPE abap_bool DEFAULT ''
                parar_en_error       TYPE abap_bool DEFAULT 'X'
                clave                TYPE any       DEFAULT ''
                cprog                TYPE any       DEFAULT ''
                popup                TYPE abap_bool DEFAULT ''
                mostrar_cco          TYPE abap_bool DEFAULT ''
                no_si_envio_previo   TYPE abap_bool DEFAULT ''
                outlook              TYPE abap_bool DEFAULT ''
      RETURNING VALUE(error)         TYPE abap_bool.

    METHODS set_text
      IMPORTING texto  TYPE any            OPTIONAL
                texto2 TYPE any            OPTIONAL
                texto3 TYPE any            OPTIONAL
                texto4 TYPE any            OPTIONAL
                tabla  TYPE srm_t_solisti1 OPTIONAL
      PREFERRED PARAMETER texto.

    METHODS ini_textos.

    METHODS cabecera_html
      IMPORTING charset TYPE string DEFAULT 'Windows-1252'.

    METHODS inicio_tabla_html
      IMPORTING longitud TYPE any DEFAULT '800'
                c1       TYPE any
                c2       TYPE any OPTIONAL
                c3       TYPE any OPTIONAL
                c4       TYPE any OPTIONAL
                c5       TYPE any OPTIONAL
                c6       TYPE any OPTIONAL
                c7       TYPE any OPTIONAL
                c8       TYPE any OPTIONAL
                c9       TYPE any OPTIONAL
                c10      TYPE any OPTIONAL
                c11      TYPE any OPTIONAL
                c12      TYPE any OPTIONAL
                c13      TYPE any OPTIONAL
                c14      TYPE any OPTIONAL
                border   TYPE i   DEFAULT 0
                c15      TYPE any OPTIONAL
                c16      TYPE any OPTIONAL
                c17      TYPE any OPTIONAL
                c18      TYPE any OPTIONAL
                !color   TYPE any DEFAULT '#6699FF'.

    METHODS add_fila_html
      IMPORTING !color    TYPE any OPTIONAL
                c1        TYPE any
                c2        TYPE any OPTIONAL
                c3        TYPE any OPTIONAL
                c4        TYPE any OPTIONAL
                c5        TYPE any OPTIONAL
                c6        TYPE any OPTIONAL
                c7        TYPE any OPTIONAL
                c8        TYPE any OPTIONAL
                c9        TYPE any OPTIONAL
                c10       TYPE any OPTIONAL
                c11       TYPE any OPTIONAL
                c12       TYPE any OPTIONAL
                c13       TYPE any OPTIONAL
                c14       TYPE any OPTIONAL
                align_c1  TYPE any OPTIONAL
                align_c2  TYPE any OPTIONAL
                align_c3  TYPE any OPTIONAL
                align_c4  TYPE any OPTIONAL
                align_c5  TYPE any OPTIONAL
                align_c6  TYPE any OPTIONAL
                align_c7  TYPE any OPTIONAL
                align_c8  TYPE any OPTIONAL
                align_c9  TYPE any OPTIONAL
                align_c10 TYPE any OPTIONAL
                align_c11 TYPE any OPTIONAL
                align_c12 TYPE any OPTIONAL
                align_c13 TYPE any OPTIONAL
                align_c14 TYPE any OPTIONAL
                c15       TYPE any OPTIONAL
                align_c15 TYPE any OPTIONAL
                c16       TYPE any OPTIONAL
                align_c16 TYPE any OPTIONAL
                c17       TYPE any OPTIONAL
                align_c17 TYPE any OPTIONAL
                c18       TYPE any OPTIONAL
                align_c18 TYPE any OPTIONAL.

    METHODS fin_tabla_html.

    METHODS add_parrafo_html
      IMPORTING texto          TYPE any
                partir_siempre TYPE abap_bool DEFAULT ''.

    METHODS add_destinatario
      IMPORTING destinatario        TYPE any
                urgente             TYPE abap_bool DEFAULT 'X'
                forzar_mail_externo TYPE abap_bool DEFAULT ''
                lista_distribucion  TYPE abap_bool DEFAULT ''
                copia               TYPE abap_bool DEFAULT ''
                copia_oculta        TYPE abap_bool DEFAULT ''
                parar_en_error      TYPE abap_bool DEFAULT ''
      RETURNING VALUE(message)      TYPE bapi_msg.

    METHODS set_text_as_string
      IMPORTING texto TYPE any OPTIONAL
      PREFERRED PARAMETER texto.

    METHODS free.

    CLASS-METHODS consulta_mails_enviados
      IMPORTING fecha            TYPE datum OPTIONAL
                fecha_hasta      TYPE datum OPTIONAL
                r_fecha          TYPE sxdatrngt
                hora             TYPE uzeit OPTIONAL
                hora_hasta       TYPE uzeit OPTIONAL
      RETURNING VALUE(i_sndrecs) TYPE soxsp2tab.

    CLASS-METHODS get_dir_envio_from_adrnr
      IMPORTING adrnr        TYPE adrnr
      RETURNING VALUE(email) TYPE string.

    CLASS-METHODS consulta_sost
      IMPORTING fecha         TYPE datum OPTIONAL
                fecha_hasta   TYPE datum OPTIONAL
                r_fecha       TYPE sxdatrngt
                hora          TYPE uzeit OPTIONAL
                hora_hasta    TYPE uzeit OPTIONAL
      RETURNING VALUE(i_sost) TYPE zsost_t.

    CLASS-METHODS usr2email
      IMPORTING !uname       TYPE any
      RETURNING VALUE(email) TYPE string.

    CLASS-METHODS mail
      IMPORTING subject              TYPE any                    OPTIONAL
                direccion            TYPE any                    OPTIONAL
                urgente              TYPE abap_bool              DEFAULT 'X'
                html                 TYPE abap_bool              DEFAULT ''
                !commit              TYPE abap_bool              DEFAULT 'X'
                forzar_mail_externo  TYPE abap_bool              DEFAULT ''
                confirmacion_lectura TYPE bcs_rqst               DEFAULT 'N'
                emisor               TYPE any                    DEFAULT ''
                texto                TYPE any                    OPTIONAL
                pdf                  TYPE fpcontent              OPTIONAL
                nombre_fichero       TYPE any                    DEFAULT ''
                i_textos             TYPE rspc_t_text            OPTIONAL
                lista_distribucion   TYPE abap_bool              DEFAULT ''
                i_adjuntos           TYPE rmps_t_post_content    OPTIONAL
                zip                  TYPE abap_bool              DEFAULT ''
                nombre_fichero_tabla TYPE any                    DEFAULT ''
                tabla_como_alv       TYPE abap_bool              DEFAULT 'X'
                dest_copia           TYPE any                    DEFAULT ''
                dest_copia_oculta    TYPE any                    DEFAULT ''
                o_alv_origen         TYPE REF TO zcl_ap_alv      OPTIONAL
                conversion_sap       TYPE abap_bool              DEFAULT ''
                pdf2                 TYPE fpcontent              OPTIONAL
                nombre_fichero2      TYPE any                    DEFAULT ''
                tabla_como_txt       TYPE any                    DEFAULT ''
                controlar_salida     TYPE abap_bool              DEFAULT ''
                parar_en_error       TYPE abap_bool              DEFAULT 'X'
                xstring              TYPE xstring                OPTIONAL
                grupo                TYPE any                    DEFAULT ''
                codigo               TYPE any                    DEFAULT ''
                !version             TYPE any                    DEFAULT '88'
                spras                TYPE sy-langu               DEFAULT sy-langu
                clave                TYPE any                    DEFAULT ''
                variables            TYPE apb_lpd_t_key_value    OPTIONAL
                cprog                TYPE any                    DEFAULT ''
                popup                TYPE abap_bool              DEFAULT ''
                plantilla            TYPE zap_textos_mail        OPTIONAL
                mostrar_cco          TYPE abap_bool              DEFAULT ''
                no_si_envio_previo   TYPE abap_bool              DEFAULT ''
                o_grid_origen        TYPE REF TO zcl_ap_alv_grid OPTIONAL
                i_adj_bin            TYPE zcl_ap_gos=>tt_adj     OPTIONAL
                outlook              TYPE abap_bool              DEFAULT ''
      EXPORTING !message             TYPE bapi_msg
      CHANGING  i_tabla              TYPE table                  OPTIONAL.

    CLASS-METHODS nombre_usuario
      IMPORTING !uname        TYPE any
      RETURNING VALUE(nombre) TYPE string.

    METHODS set_texto_estandar_html
      IMPORTING !id     TYPE any
                !name   TYPE any
                spras   TYPE stxh-tdspras OPTIONAL
                !object TYPE any.

    METHODS add_adjunto
      IMPORTING tipo       TYPE soodk-objtp DEFAULT 'BIN'
                titulo     TYPE any         DEFAULT ''
                longitud   TYPE any         OPTIONAL
                binario    TYPE solix_tab   OPTIONAL
                !header    TYPE soli_tab    OPTIONAL
                xstring    TYPE xstring     OPTIONAL
                !string    TYPE string      OPTIONAL
                formato    TYPE any         DEFAULT ''
      RETURNING VALUE(adj) TYPE rmps_post_content.

    METHODS add_otf
      IMPORTING i_otfdata TYPE tt_itcoo
                titulo    TYPE any.

    METHODS add_fichero_serv
      IMPORTING fichero    TYPE any
                !legacy    TYPE abap_bool     DEFAULT ''
                modo_texto TYPE abap_bool     DEFAULT 'X'
                codepage   TYPE abap_encoding DEFAULT ''
      EXPORTING mensaje    TYPE bapi_msg.

    METHODS add_itab_as_xls
      IMPORTING itab        TYPE data
                descripcion TYPE so_obj_des
                tipo        TYPE any       DEFAULT 'XLS'
                zip         TYPE abap_bool DEFAULT ''.

    METHODS add_itab_alv_as_xls
      IMPORTING descripcion                TYPE any
                quitar_caracteres_extranos TYPE abap_bool              DEFAULT ''
                o_alv_origen               TYPE REF TO zcl_ap_alv      OPTIONAL
                zip                        TYPE abap_bool              DEFAULT ''
                tipo                       TYPE any                    DEFAULT 'XLS'
                conversion_sap             TYPE abap_bool              DEFAULT ''
                o_grid_origen              TYPE REF TO zcl_ap_alv_grid OPTIONAL
      CHANGING  itab                       TYPE table.

    METHODS add_fichero_local
      IMPORTING fichero    TYPE any OPTIONAL
                tipo       TYPE any DEFAULT 'EXT'
                como_texto TYPE any DEFAULT ''
      PREFERRED PARAMETER fichero.

    METHODS add_pdf
      IMPORTING pdf    TYPE fpcontent
                titulo TYPE any.

    METHODS add_adjunto_zip
      IMPORTING titulo               TYPE any       DEFAULT ''
                xstring              TYPE xstring   OPTIONAL
                !string              TYPE string    OPTIONAL
                tabla                TYPE table     OPTIONAL
                reemplazar_extension TYPE abap_bool DEFAULT 'X'.

    METHODS add_url_html
      IMPORTING texto          TYPE any       DEFAULT ''
                url            TYPE any       OPTIONAL
                parrafo        TYPE abap_bool DEFAULT ''
                codigo_parrafo TYPE string    DEFAULT 'P'
      PREFERRED PARAMETER texto.

    CLASS-METHODS info_envio_mail
      IMPORTING subject     TYPE any
                add_icono   TYPE abap_bool DEFAULT 'X'
                !mail       TYPE any DEFAULT ''
                add_destino TYPE abap_bool DEFAULT ''
      EXPORTING !status     TYPE soes-status
                crdat       TYPE sood-crdat
                crtim       TYPE sood-crtim
                sndnam      TYPE soos-sndnam
                !message    TYPE any
                !type       TYPE any
                msgv1       TYPE soes-msgv1.

    METHODS get_plantilla
      IMPORTING grupo            TYPE any
                codigo           TYPE any
                !version         TYPE any                 DEFAULT '88'
                spras            TYPE sy-langu            DEFAULT sy-langu
                variables        TYPE apb_lpd_t_key_value OPTIONAL
      RETURNING VALUE(plantilla) TYPE zap_textos_mail.

    CLASS-METHODS validar_email
      IMPORTING email             TYPE any
      EXPORTING email_normalizado TYPE string
      RETURNING VALUE(message)    TYPE bapi_msg.

    METHODS outlook
      IMPORTING direccion         TYPE any
                subject           TYPE any
                dest_copia        TYPE any         DEFAULT ''
                dest_copia_oculta TYPE any         DEFAULT ''
                i_textos          TYPE rspc_t_text OPTIONAL
                texto             TYPE any         DEFAULT ''.

    CLASS-METHODS mod_subject_cliente_entorno
      CHANGING subject TYPE any.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA l_texto     TYPE solisti1.
    DATA usar_clases TYPE abap_bool.
    DATA o_sender    TYPE REF TO if_sender_bcs.
    DATA o_recipient TYPE REF TO if_recipient_bcs.
    DATA destino     TYPE string.
endclass. "ZCL_AP_ENVIO_MAIL definition
class ZCL_AP_ENVIO_MAIL implementation.
  METHOD add_adjunto.
    DATA: l_adjunto TYPE rmps_post_content,
          l_size    TYPE so_obj_len.

    IF tipo IS INITIAL.
      l_adjunto-doc_type = zcl_ap_ficheros=>get_extension( titulo ).
      TRANSLATE l_adjunto-doc_type TO UPPER CASE.
    ELSE.
      l_adjunto-doc_type = tipo.
    ENDIF.
    IF l_adjunto-doc_type IS INITIAL.
      l_adjunto-doc_type = 'BIN'.
    ENDIF.

    l_adjunto-docsize = longitud.

    IF     ( l_adjunto-doc_type = 'XLS' OR l_adjunto-doc_type = 'SAP' )
       AND NOT string IS INITIAL.
      l_adjunto-binary = 'X'.
      TRY.
          cl_bcs_convert=>string_to_solix(
            EXPORTING
              iv_string   = string
              iv_codepage = '4103'  " suitable for MS Excel, leave empty
              iv_add_bom  = 'X'     " for other doc types
            IMPORTING
              et_solix  = l_adjunto-cont_hex
              ev_size   = l_size ).
        CATCH cx_bcs.
          MESSAGE 'Error adjuntando Excel'(eax) TYPE 'E'.
      ENDTRY.
      l_adjunto-docsize = l_size.
    ELSEIF    l_adjunto-doc_type = 'BIN' OR l_adjunto-doc_type = 'XLS' OR l_adjunto-doc_type = 'PDF' OR l_adjunto-doc_type = 'PNG'
           OR l_adjunto-doc_type = 'JPG' OR l_adjunto-doc_type = 'GIF' OR l_adjunto-doc_type = 'EXE' OR l_adjunto-doc_type = 'DOC'
           OR l_adjunto-doc_type = 'CSV'.
      l_adjunto-binary = 'X'.
      IF NOT binario IS INITIAL.
        l_adjunto-cont_hex = binario.
      ELSEIF NOT xstring IS INITIAL.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer          = xstring
            append_to_table = ' '
          IMPORTING
            output_length   = l_adjunto-docsize
          TABLES
            binary_tab      = l_adjunto-cont_hex.
      ENDIF.
    ELSEIF l_adjunto-doc_type = 'TXT' AND NOT xstring IS INITIAL.
      l_adjunto-binary   = 'X'.
      l_adjunto-cont_hex = cl_bcs_convert=>xstring_to_solix(
                               iv_xstring   = xstring ).
    ELSEIF ( l_adjunto-doc_type = 'TXT' OR formato = 'TXT' ) AND NOT string IS INITIAL.
      l_adjunto-binary = 'X'.
      TRY.
          cl_bcs_convert=>string_to_solix(
            EXPORTING
              iv_string   = string
              iv_codepage = '4103'  " suitable for MS Excel, leave empty
              iv_add_bom  = 'X'     " for other doc types
            IMPORTING
              et_solix  = l_adjunto-cont_hex
              ev_size   = l_size ).
        CATCH cx_bcs.
          MESSAGE 'Error adjuntando fichero de texto'(eat) TYPE 'E'.
      ENDTRY.
    ELSE.
      IF l_adjunto-doc_type = 'BTX'.
        l_adjunto-doc_type = 'BIN'.
      ENDIF.
      l_adjunto-cont_text = header.
    ENDIF.
    l_adjunto-subject = titulo.

*  IF l_adjunto-doc_type = 'XLS'.
*    l_adjunto-doc_type = 'BIN'.
*  ENDIF.

    APPEND l_adjunto TO i_adjuntos.

    adj = l_adjunto.
  ENDMETHOD.
  METHOD add_adjunto_zip.
    DATA: l_xstring        TYPE xstring,
          l_string         TYPE string,
          o_zip            TYPE REF TO cl_abap_zip,
          l_nombre_fichero TYPE string.

    IF NOT xstring IS INITIAL.
      l_xstring = xstring.
    ELSEIF NOT string IS INITIAL.
      l_xstring = zcl_ap_string=>string2xstring( string ).
    ELSEIF NOT tabla IS INITIAL.
      l_string = zcl_ap_string=>tabla2string( tabla ).
      l_xstring = zcl_ap_string=>string2xstring( l_string ).
    ENDIF.

    o_zip = NEW #( ).
    l_string = titulo.
    o_zip->add( name    = l_string
                content = l_xstring ).

    l_xstring = o_zip->save( ).

    l_nombre_fichero = titulo.
    IF reemplazar_extension IS INITIAL.
      CONCATENATE l_nombre_fichero '.ZIP' INTO l_nombre_fichero.
    ELSE.
      l_string = zcl_ap_ficheros=>get_extension( titulo ).
      IF l_string IS INITIAL.
        CONCATENATE titulo '.ZIP' INTO l_nombre_fichero.
      ELSE.
        REPLACE l_string WITH 'ZIP' INTO l_nombre_fichero.
      ENDIF.
    ENDIF.

    add_adjunto( titulo = l_nombre_fichero xstring = l_xstring ).
  ENDMETHOD.
  METHOD add_destinatario.
    DATA: l_destinatario TYPE string,
          i_dest         TYPE TABLE OF string,
          l_usr21        TYPE usr21,
          l_dest         TYPE solisti1,
          l_long         TYPE i,
          l_lm           TYPE c LENGTH 1,
          l_desti        TYPE so_obj_nam.
    DATA l_email  TYPE adr6-smtp_addr.
    DATA l_number TYPE ad_pagnmbr.
    DATA l_uname  TYPE uname.

    IF copia_oculta IS INITIAL.
      __add_lista destino destinatario.
    ENDIF.

    l_destinatario = destinatario.
    REPLACE ALL OCCURRENCES OF ';' IN l_destinatario WITH ','. " Permitimos ',' y ';' como separadores
    SPLIT l_destinatario AT ',' INTO TABLE i_dest.

    LOOP AT i_dest INTO l_destinatario.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN l_destinatario WITH ''.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN l_destinatario WITH ''.
      CONDENSE l_destinatario NO-GAPS.
      IF     forzar_mail_externo  = 'X'
         AND lista_distribucion  IS INITIAL
         AND l_destinatario(1)   <> '$'
         AND (    zcl_c=>salida_mail_desarrollo = 'X'
               OR zcl_c=>entorno_produccion     = sy-sysid ).
        l_destinatario = to_upper( l_destinatario ).
        SELECT SINGLE addrnumber persnumber FROM usr21
          INTO CORRESPONDING FIELDS OF l_usr21
         WHERE bname = l_destinatario.
        IF sy-subrc = 0.
          SELECT smtp_addr FROM adr6
            INTO l_destinatario
            UP TO 1 ROWS
           WHERE addrnumber = l_usr21-addrnumber
             AND persnumber = l_usr21-persnumber
            ORDER BY PRIMARY KEY.
          ENDSELECT.
        ENDIF.
      ENDIF.

      IF usar_clases IS INITIAL.
        l_dest = l_destinatario.
        APPEND l_dest TO i_destinatarios.
      ELSE.
        l_long = strlen( l_destinatario ).
        IF l_destinatario CS '@'.
          l_email = l_destinatario.
          TRY.
              o_recipient = cl_cam_address_bcs=>create_internet_address( l_email ).
            CATCH cx_address_bcs.
              IF parar_en_error = 'X'.
                MESSAGE e398(00) WITH 'Error en direccion de email'(ecd) l_email '' ''.
              ELSE.
                CONCATENATE 'Error en direccion de email'(ecd) l_email INTO message SEPARATED BY space.
              ENDIF.
          ENDTRY.
        ELSE.
          l_lm = l_destinatario.
          IF lista_distribucion = 'X' OR l_lm = '$'.
            IF l_lm = '$'.
              l_destinatario = l_destinatario+1.
            ENDIF.
            l_desti = l_destinatario.
            TRANSLATE l_desti TO UPPER CASE.
            TRY.
                o_recipient = cl_distributionlist_bcs=>getu_persistent(
                                  i_private = ' '
                                  i_dliname = l_desti ).
              CATCH cx_address_bcs.
                IF parar_en_error = 'X'.
                  MESSAGE e398(00) WITH 'No existe lista dist.'(nel) l_desti '' ''.
                ELSE.
                  CONCATENATE 'No existe lista dist.'(nel) l_desti INTO message SEPARATED BY space.
                ENDIF.
            ENDTRY.
          ELSEIF l_destinatario CO '0123456789 +' AND l_long >= 9.
*   Configuracion del receptor del SMS
            l_number = l_destinatario.
            TRY.
                o_recipient = cl_cam_address_bcs=>create_sms_address( i_service = 'SMS'
                                                                  i_number  = l_number ).
              CATCH cx_address_bcs.
                IF parar_en_error = 'X'.
                  MESSAGE e398(00) WITH 'Error en numero destino SMS'(esm) l_number '' ''.
                ELSE.
                  CONCATENATE 'Error en numero destino SMS'(esm) l_number INTO message SEPARATED BY space.
                ENDIF.
            ENDTRY.
          ELSE.
            l_uname = l_destinatario.
            TRANSLATE l_uname TO UPPER CASE.
            TRY.
                o_recipient = cl_sapuser_bcs=>create( l_uname ).
              CATCH cx_address_bcs.
                IF parar_en_error = 'X'.
                  MESSAGE e398(00) WITH 'No existe el usuario'(neu) l_uname '' ''.
                ENDIF.
            ENDTRY.
          ENDIF.
        ENDIF.

        TRY.
            o_send_request->add_recipient( i_recipient  = o_recipient
                                           i_copy       = copia
                                           i_blind_copy = copia_oculta
                                           i_express    = urgente ).
          CATCH cx_send_req_bcs.
            IF parar_en_error = 'X'.
              MESSAGE e398(00) WITH 'Error al añadir destinatario'(ead) destinatario '' ''.
            ELSE.
              CONCATENATE 'Error al añadir destinatario'(ead) destinatario INTO message SEPARATED BY space.
            ENDIF.
        ENDTRY.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD add_fichero_local.
    DATA: l_fichero TYPE string,
          l_long    TYPE i,
*        i_tabla TYPE STANDARD TABLE OF x255 WITH NON-UNIQUE DEFAULT KEY,
          i_tabla   TYPE TABLE OF solix,
          l_xstring TYPE xstring,
          l_string  TYPE string,
          l_adjunto TYPE rmps_post_content,
          i_ttext   TYPE soli_tab.

    l_fichero = fichero.

    IF NOT zcl_ap_ficheros=>existe( l_fichero ).
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filetype                = 'BIN'
        filename                = l_fichero
      IMPORTING
        filelength              = l_long
      CHANGING
        data_tab                = i_tabla
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      IF sy-msgty = 'E'.
        MESSAGE ID sy-msgty TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE |Error { sy-subrc } al intentar leer fichero { l_fichero }| TYPE 'E'.
      ENDIF.
    ENDIF.

    IF como_texto = 'X'.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = l_long
        IMPORTING
          buffer       = l_xstring
        TABLES
          binary_tab   = i_tabla.

      l_string = zcl_ap_string=>xstring2string( l_xstring ).

      CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
        EXPORTING
          text      = l_string
        IMPORTING
          length    = l_adjunto-docsize
        TABLES
          ftext_tab = i_ttext.
      l_adjunto-cont_text = i_ttext.
    ELSE.
      l_adjunto-docsize  = l_long.
      l_adjunto-cont_hex = i_tabla.
      l_adjunto-binary   = 'X'.
    ENDIF.

    l_adjunto-doc_type = tipo.
    l_adjunto-subject  = zcl_ap_ficheros=>get_nombre_fichero( fichero = l_fichero con_extension = 'X' ).
    APPEND l_adjunto TO i_adjuntos.
  ENDMETHOD.
  METHOD add_fichero_serv.
    TYPES t_linea TYPE c LENGTH 65535.

    DATA: l_adjunto   TYPE rmps_post_content,
          i_att_cont2 TYPE TABLE OF t_linea,
          l_linea     TYPE string,
          l_string    TYPE string,
          i_ttext     TYPE soli_tab.

    zcl_ap_ficheros=>lee_fich_servidor( EXPORTING fichero = fichero
                                                  modo_texto = modo_texto
                                                  legacy  = legacy
                                                  codepage = codepage
                                        IMPORTING longitud = l_adjunto-docsize
                                                  mensaje  = mensaje
                                        CHANGING  tabla   = i_att_cont2 ).

    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT i_att_cont2 INTO l_linea.
      IF l_string IS INITIAL.
        l_string = l_linea.
      ELSE.
        CONCATENATE l_string cl_abap_char_utilities=>cr_lf l_linea INTO l_string.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = l_string
      IMPORTING
        length    = l_adjunto-docsize
      TABLES
        ftext_tab = i_ttext.

    IF codepage = '4103' AND modo_texto = 'X'.
      l_adjunto-docsize = 2 * l_adjunto-docsize.
    ENDIF.

    l_adjunto-doc_type  = 'EXT'.
    l_adjunto-binary    = ''.
    l_adjunto-subject   = fichero.
    l_adjunto-cont_text = i_ttext.
*  IF legacy = 'X'.
*    l_adjunto-docsize = l_adjunto-docsize * 4.
*  ENDIF.
    APPEND l_adjunto TO i_adjuntos.
  ENDMETHOD.
  METHOD add_fila_html.
    DATA: l_texto TYPE solisti1,
          l_td    TYPE string,
          l_tipo  TYPE c LENGTH 1.

    DEFINE set_td.
      IF align_&1 IS INITIAL.
        DESCRIBE FIELD &1 TYPE l_tipo.
        IF l_tipo = 'P' OR l_tipo = 'i'.
          l_td = '  <td align="right">'.
        ELSE.
          l_td = '  <td>'.
        ENDIF.
      ELSE.
        CONCATENATE '  <td align="' align_&1 '">' INTO l_td.
      ENDIF.
    END-OF-DEFINITION.

    IF NOT color IS INITIAL.
      CONCATENATE '<tr bgcolor="' color '">' INTO l_texto.
      APPEND l_texto TO i_texto_mail.
    ELSE.
      set_text(  '  <tr>' ).
    ENDIF.

    set_td c1.
    set_text( texto  = l_td texto2 = c1
              texto3 = '</td>' ).
    IF c2 IS SUPPLIED.
      set_td c2.
      set_text( texto  = l_td texto2 = c2
                texto3 = '</td>' ).
    ENDIF.
    IF c3 IS SUPPLIED.
      set_td c3.
      set_text( texto  = l_td texto2 = c3
                texto3 = '</td>' ).
    ENDIF.
    IF c4 IS SUPPLIED.
      set_td c4.
      set_text( texto  = l_td texto2 = c4
                texto3 = '</td>' ).
    ENDIF.
    IF c5 IS SUPPLIED.
      set_td c5.
      set_text( texto  = l_td texto2 = c5
                texto3 = '</td>' ).
    ENDIF.
    IF c6 IS SUPPLIED.
      set_td c6.
      set_text( texto  = l_td texto2 = c6
                texto3 = '</td>' ).
    ENDIF.
    IF c7 IS SUPPLIED.
      set_td c7.
      set_text( texto  = l_td texto2 = c7
                texto3 = '</td>' ).
    ENDIF.
    IF c8 IS SUPPLIED.
      set_td c8.
      set_text( texto  = l_td texto2 = c8
                texto3 = '</td>' ).
    ENDIF.
    IF c9 IS SUPPLIED.
      set_td c9.
      set_text( texto  = l_td texto2 = c9
                texto3 = '</td>' ).
    ENDIF.
    IF c10 IS SUPPLIED.
      set_td c10.
      set_text( texto  = l_td texto2 = c10
                texto3 = '</td>' ).
    ENDIF.
    IF c11 IS SUPPLIED.
      set_td c11.
      set_text( texto  = l_td texto2 = c11
                texto3 = '</td>' ).
    ENDIF.
    IF c12 IS SUPPLIED.
      set_td c12.
      set_text( texto  = l_td texto2 = c12
                texto3 = '</td>' ).
    ENDIF.
    IF c13 IS SUPPLIED.
      set_td c13.
      set_text( texto  = l_td texto2 = c13
                texto3 = '</td>' ).
    ENDIF.
    IF c14 IS SUPPLIED.
      set_td c14.
      set_text( texto  = l_td texto2 = c14
                texto3 = '</td>' ).
    ENDIF.
    IF c15 IS SUPPLIED.
      set_td c15.
      set_text( texto  = l_td texto2 = c15
                texto3 = '</td>' ).
    ENDIF.
    IF c16 IS SUPPLIED.
      set_td c16.
      set_text( texto  = l_td texto2 = c16
                texto3 = '</td>' ).
    ENDIF.
    IF c17 IS SUPPLIED.
      set_td c17.
      set_text( texto  = l_td texto2 = c17
                texto3 = '</td>' ).
    ENDIF.
    IF c18 IS SUPPLIED.
      set_td c18.
      set_text( texto  = l_td texto2 = c18
                texto3 = '</td>' ).
    ENDIF.

    set_text(  '  </tr>' ).
  ENDMETHOD.
  METHOD add_itab_alv_as_xls.
    DATA: l_fichero   TYPE string,
          l_extension TYPE string,
          o_alv       TYPE REF TO zcl_ap_alv,
          l_string    TYPE string,
          l_long      TYPE i,
          x_excelx    TYPE solix_tab.
    DATA: lv_xml_type TYPE salv_bs_constant,
          lv_xml      TYPE xstring.

    l_fichero = descripcion.
    l_extension = zcl_ap_ficheros=>get_extension( l_fichero ).
    IF l_extension IS INITIAL.
      IF conversion_sap = 'X' OR conversion_sap = 'Y'.
        CONCATENATE l_fichero '.xlsx' INTO l_fichero.
      ELSE.
        CONCATENATE l_fichero '.' tipo INTO l_fichero.
      ENDIF.
    ENDIF.

    IF o_grid_origen IS INITIAL.
      IF o_alv_origen IS INITIAL.
        o_alv = NEW #(
            tabla = '' ).

        o_alv->constructor_tabla( CHANGING t_tabla = itab ).
      ELSE.
        o_alv = o_alv_origen.
      ENDIF.
    ENDIF.

    IF conversion_sap IS INITIAL.
      o_alv->get_csv_as_string( EXPORTING quitar_caracteres_extranos = quitar_caracteres_extranos
                                IMPORTING string_csv = l_string
                                CHANGING t_tabla = itab ).

      l_long = strlen( l_string ).
      x_excelx = zcl_ap_string=>string_to_binary_tab( l_string ).
    ELSEIF conversion_sap = 'X'.
      cl_salv_bs_runtime_info=>set(
          display        = abap_false
          metadata       = abap_false
          data           = abap_true ).

      o_alv->o_alv->display( ).
      cl_salv_bs_runtime_info=>clear_all( ).

      lv_xml_type = if_salv_bs_xml=>c_type_mhtml.
      lv_xml      = o_alv->o_alv->to_xml( xml_type = lv_xml_type ).
      l_long = xstrlen( lv_xml ).
      x_excelx = cl_document_bcs=>xstring_to_solix( ip_xstring = lv_xml ).
    ELSEIF conversion_sap = 'Y'.
      IF NOT o_grid_origen IS INITIAL.
        DATA(l_xstring) = zcl_ap_abap2xls=>alv_2_xls( tabla = itab alv = o_grid_origen->o_grid ).
      ELSEIF NOT o_alv IS INITIAL.
        l_xstring = zcl_ap_abap2xls=>alv_2_xls( tabla = itab alv = o_alv->o_alv ).
      ENDIF.
      IF l_xstring IS INITIAL.
        RETURN.
      ENDIF.
      l_long = xstrlen( l_xstring ).

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = l_xstring
        TABLES
          binary_tab = x_excelx.
    ENDIF.

    IF zip IS INITIAL.

      add_adjunto( titulo = l_fichero tipo = 'XLS' binario = x_excelx longitud = l_long ).

*    add_adjunto( titulo = l_fichero tipo = 'XLS' string = l_string ).
    ELSE.
      add_adjunto_zip( titulo = l_fichero string = l_string ).
    ENDIF.
  ENDMETHOD.
  METHOD add_itab_as_xls.
    DATA: l_fichero       TYPE string,
          l_extension     TYPE string,
          lr_data         TYPE REF TO data,
          lr_structdescr  TYPE REF TO cl_abap_structdescr,
          lv_is_dd_object TYPE boolean,
*          lv_dd_header    TYPE x030l,
          lv_dd_object    TYPE dd_x031l_table,
          lx_comp         TYPE abap_component_tab,
          lv_string       TYPE string,
          wa_field        TYPE string,
          l_long          TYPE i,
          x_excelx        TYPE solix_tab.

    FIELD-SYMBOLS:
      <fs_table>  TYPE ANY TABLE,
      <fs_dd_tab> TYPE x031l,
      <fs_comp>   TYPE abap_componentdescr,
      <dyn_wa>    TYPE any,
      <fs_field>  TYPE any.

    IF tipo = 'XLSX'.
      DATA(o_xls) = NEW zcl_ap_abap2xls( ).
      o_xls->set_tabla( tabla = itab ).
      o_xls->add_adj_mail( o_mail = me fichero = descripcion add_ext = 'X' ).
      CLEAR o_xls.
    ELSE.
      TYPE-POOLS truxs.

      l_fichero = descripcion.
      l_extension = zcl_ap_ficheros=>get_extension( l_fichero ).
      IF l_extension IS INITIAL.
        CONCATENATE l_fichero '.' tipo INTO l_fichero.
      ENDIF.

* Deduce data type of the imported table using RTTS
      ASSIGN itab TO <fs_table>.
      CREATE DATA lr_data LIKE LINE OF <fs_table>.

* Get the table structure
      lr_structdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).

* Check if this is a DD object or not
      lv_is_dd_object = lr_structdescr->is_ddic_type( ).

* If table is typed on a DD object, we can examine the fields
      IF lv_is_dd_object = abap_true.

* (Not really needed - just to show how to get header info)
*        lv_dd_header = lr_structdescr->get_ddic_header( ).
* Retrieves DD info of table/structure
        lv_dd_object = lr_structdescr->get_ddic_object( ).

* Move results to table LX_COMP for ease of use below.
        LOOP AT lv_dd_object ASSIGNING <fs_dd_tab>.
          APPEND INITIAL LINE TO lx_comp ASSIGNING <fs_comp>.
          <fs_comp>-name = <fs_dd_tab>-fieldname.
        ENDLOOP.

* If not a DD object, use GET_COMPONENTS.
* Does not fully work for DD objects, because does not
* handle embedded include's.
      ELSE.
        lx_comp = lr_structdescr->get_components( ).
      ENDIF.

* Now we're ready to move the contents from the imported
* table into a string for converting to xls attachment
      IF NOT <fs_table> IS INITIAL.

* Retrieve values for each "cell" in the source table, move to excel table:
* Get table row

* First, get the header line into the final string
        LOOP AT lx_comp ASSIGNING <fs_comp>.
          CONCATENATE lv_string <fs_comp>-name wa_field cl_abap_char_utilities=>horizontal_tab
                      INTO lv_string.
        ENDLOOP.

        CONCATENATE lv_string cl_abap_char_utilities=>cr_lf INTO lv_string.

* Then, add contents of table into final string
        LOOP AT <fs_table> ASSIGNING <dyn_wa>.        " New record
          LOOP AT lx_comp ASSIGNING <fs_comp>. " For each field in the record
            IF <fs_comp> IS INITIAL.
              CONTINUE.
            ENDIF.

            ASSIGN COMPONENT <fs_comp>-name OF STRUCTURE <dyn_wa> TO <fs_field>.
            IF <fs_field> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.

            DESCRIBE FIELD <fs_field> TYPE DATA(l_tipo).
            IF l_tipo <> 'h'. " Tabla interna.
              TRY.
                  wa_field = <fs_field>.
                  CONCATENATE lv_string wa_field cl_abap_char_utilities=>horizontal_tab INTO lv_string.
                CATCH cx_root INTO DATA(o_root). "#EC *
                  MESSAGE o_root->get_text( ) TYPE 'S'.
              ENDTRY.
            ENDIF.
          ENDLOOP.
          CONCATENATE lv_string cl_abap_char_utilities=>cr_lf INTO lv_string.
        ENDLOOP.
*
        IF zip IS INITIAL.
          l_long = strlen( lv_string ).
          x_excelx = zcl_ap_string=>string_to_binary_tab( lv_string ).

          add_adjunto( titulo = l_fichero tipo = 'BIN' binario = x_excelx longitud = l_long ).
        ELSE.
          add_adjunto_zip( titulo = l_fichero string = lv_string ).
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD add_otf.
    DATA: i_pdf      TYPE solix_tab,
          l_long_pdf TYPE i.

    zcl_ap_smartforms=>get_pdf_from_otfdata( EXPORTING i_otfdata = i_otfdata
                                          IMPORTING i_pdf = i_pdf
                                                    longitud_pdf = l_long_pdf ).

    add_adjunto( titulo = titulo
                 longitud = l_long_pdf
                 binario = i_pdf ).
  ENDMETHOD.
  METHOD add_parrafo_html.
    DATA: l_long   TYPE i,
          l_string TYPE string,
          i_lineas TYPE TABLE OF text255,
          l_linea  TYPE text255.

    l_long = strlen( texto ).

    IF l_long < 248 AND partir_siempre = ''.
      set_text( texto = '<P>' texto2 = texto texto3 = '</P>' ).
    ELSE.
      l_string = texto.
      zcl_ap_string=>string2tabla( EXPORTING string = l_string
                                             longitud = 248
                                   CHANGING  tabla = i_lineas ).

      LOOP AT i_lineas INTO l_linea.
        set_text( texto = '<P>' texto2 = l_linea texto3 = '</P>' ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD add_pdf.
    DATA: l_long_pdf  TYPE i,
          i_pdf       TYPE solix_tab,
          l_extension TYPE soodk-objtp.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = pdf
      IMPORTING
        output_length = l_long_pdf
      TABLES
        binary_tab    = i_pdf.

    l_extension = zcl_ap_ficheros=>get_extension( titulo ).
    TRANSLATE l_extension TO UPPER CASE.
    IF l_extension IS INITIAL.
      l_extension = 'PDF'.
    ENDIF.
    add_adjunto( titulo = titulo
                 longitud = l_long_pdf
                 binario = i_pdf
                 tipo    = l_extension ).
  ENDMETHOD.
  METHOD add_url_html.
    DATA: l_texto          TYPE string,
          l_linea          TYPE string,
          l_inicio_parrafo TYPE string,
          l_fin_parrafo    TYPE string.

    IF texto IS INITIAL.
      l_texto = url.
    ELSE.
      l_texto = texto.
    ENDIF.

    CONCATENATE '<a href="' url '">' l_texto '</a>' INTO l_linea.
    IF parrafo = 'X'.
      CONCATENATE '<' codigo_parrafo '>' INTO l_inicio_parrafo.
      CONCATENATE '</' codigo_parrafo '>' INTO l_fin_parrafo.
      CONCATENATE l_inicio_parrafo l_linea l_fin_parrafo INTO l_linea.
    ENDIF.

    set_text( l_linea ).
  ENDMETHOD.
  METHOD cabecera_html.
    DATA l_meta TYPE string.

    set_text( '<head>' ).
    set_text( '<title>Documento sin t&iacute;tulo</title>' ).
    CONCATENATE '<meta http-equiv="Content-Type" content="text/html;'
                'charset=' charset '">' INTO l_meta SEPARATED BY space.
    set_text( l_meta ).
    set_text( '<style type="text/css">' ).
    set_text( '<!--' ).
    set_text( 'Estilo1 {font-family: Arial, Helvetica, sans-serif}' ).
    set_text( '-->' ).
    set_text( '</style>' ).
    set_text( '</head>' ).

    set_text( '<body class="Estilo1">' ).
  ENDMETHOD.
  METHOD constructor.
    me->usar_clases = usar_clases.

    IF usar_clases = 'X'.
      TRY.
          o_send_request = cl_bcs=>create_persistent( ).
        CATCH cx_bcs INTO DATA(o_bcs).
          MESSAGE o_bcs->get_text( ) TYPE 'S'.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD consulta_mails_enviados.
    DATA: i_snd_date TYPE sxdatrngt,
          l_snd_date TYPE sxdatrngl,
          l_snd_time TYPE sxtimrngl,
          i_snd_time TYPE sxtimrngt,
          l_status   TYPE soststatus.

    IF NOT r_fecha IS INITIAL.
      i_snd_date = r_fecha.
    ELSE.
      IF NOT fecha IS INITIAL.
        CLEAR l_snd_date.
        l_snd_date-sign = 'I'.
        l_snd_date-low  = fecha.

        IF fecha_hasta IS INITIAL.
          l_snd_date-option = 'EQ'.
        ELSE.
          l_snd_date-option = 'BT'.
          l_snd_date-high   = fecha_hasta.
        ENDIF.

        APPEND l_snd_date TO i_snd_date.
      ENDIF.
    ENDIF.

    IF NOT hora IS INITIAL.
      CLEAR l_snd_time.
      l_snd_time-sign = 'I'.
      l_snd_time-low  = hora.

      IF hora_hasta IS INITIAL.
        l_snd_time-option = 'EQ'.
      ELSE.
        l_snd_time-option = 'BT'.
        l_snd_time-high   = hora_hasta.
      ENDIF.

      APPEND l_snd_time TO i_snd_time.
    ENDIF.

    l_status = 'XXXXXXXX '.
    CALL FUNCTION 'SX_SNDREC_SELECT'
      EXPORTING
*       SND_ART       =
        snd_date      = i_snd_date
        snd_time      = i_snd_time
*       DEL_DATE      =
*       DEL_TIME      =
        status        = l_status
        notifications = 'X'
*       SENDER        =
*       MAXSEL        =
        all_waiting   = ' '
      IMPORTING
        sndrecs       = i_sndrecs.
  ENDMETHOD.
  METHOD consulta_sost.
    DATA: i_sndrecs TYPE soxsp2tab,
          l_sndrec  TYPE soxsp2,
          l_sost    TYPE zsost.

    i_sndrecs = consulta_mails_enviados( fecha = fecha
                                         fecha_hasta = fecha_hasta
                                         r_fecha = r_fecha
                                         hora  = hora
                                         hora_hasta  = hora_hasta ).

    LOOP AT i_sndrecs INTO l_sndrec.
      CLEAR l_sost.

      IF l_sndrec-msgty = 'E'.
        l_sost-status = icon_led_red.
      ELSE.
        l_sost-status = icon_led_green.
      ENDIF.

      CASE l_sndrec-sndart.
        WHEN 'INT'.
          l_sost-forma_envio = 'Email'.
        WHEN 'PAG'.
          l_sost-forma_envio = 'SMS'.
        WHEN OTHERS.
          l_sost-forma_envio = l_sndrec-sndart.
      ENDCASE.

      l_sost-titulo       = l_sndrec-titel.
      l_sost-emisor       = l_sndrec-usernam.
      l_sost-destinatario = get_dir_envio_from_adrnr( l_sndrec-adrnr ).
      l_sost-fecha_envio  = l_sndrec-stat_date.
      l_sost-hora_envio   = l_sndrec-stat_time.

      APPEND l_sost TO i_sost.

    ENDLOOP.
  ENDMETHOD.
  METHOD envio_mail.
    DATA: l_fecha        TYPE dats,
          l_hora         TYPE uzeit,
          l_funcname     TYPE tfdir-funcname,
          l_smtp_addr    TYPE adr6-smtp_addr,
          l_uname        TYPE uname,
          l_destinatario TYPE solisti1,
          l_adjunto      TYPE rmps_post_content,
          lt_att_head    TYPE soli_tab,
          lv_text_line   TYPE soli,
          i_recipients   TYPE bcsy_re,
          l_result       TYPE os_boolean.
    DATA: l_tipo     TYPE c LENGTH 3,
          l_title    TYPE sood-objdes,
          l_mailtext TYPE soli_tab,
          l_subject  TYPE string,
          l_mailhex  TYPE solix_tab.
    DATA: o_document_bcs TYPE REF TO cx_document_bcs,
          o_send_req_bcs TYPE REF TO cx_send_req_bcs,
          l_titulo       TYPE sood-objdes,
          l_long         TYPE sood-objlen,
          l_document     TYPE REF TO cl_document_bcs,
          l_error        TYPE string.
    DATA: l_limite_megas TYPE mengv13,
          l_megas        TYPE mengv13,
          l_total_megas  TYPE mengv13.
    DATA: zap_mail_log TYPE zap_mail_log,
          l_dest       TYPE string,
          l_dest_copia TYPE string,
          l_dest_cco   TYPE string.
    DATA l_s    TYPE string.
    DATA l_e    TYPE string.
    DATA l_soes TYPE soes.

    GET TIME.
    l_fecha = sy-datum.
    l_hora  = sy-uzeit.

    CLEAR error.

    SET PARAMETER ID 'ZMAIL' FIELD 'X'.                     "#EC *
    IF me->usar_clases IS INITIAL.
      SELECT SINGLE funcname FROM tfdir
        INTO l_funcname
       WHERE funcname = 'Z_ENVIO_MAIL'.
      IF sy-subrc = 0.
        CALL FUNCTION l_funcname
          EXPORTING
            subject             = subject
            direccion           = direccion
            urgente             = urgente
            html                = html
            forzar_mail_externo = forzar_mail_externo
*           DOC_ID              =
            sender              = emisor
            commit              = commit
*           SMS                 = ''
*   IMPORTING
*           RETURNCODE          =
          TABLES
            texto               = i_texto_mail
*           T_FICHEROS          =
            i_destinatarios     = i_destinatarios.
      ELSE.
        MESSAGE 'No existe funcion Z_ENVIO_MAIL'(nef) TYPE 'E'.
      ENDIF.
    ELSE.
      IF html IS INITIAL.
        l_tipo = 'RAW'.
      ELSE.
        l_tipo = 'HTM'.
      ENDIF.

      l_title = subject.

      mod_subject_cliente_entorno( CHANGING subject = l_title ).
      l_mailtext = i_texto_mail.

      TRY.
          o_document = cl_document_bcs=>create_document(
                           i_type    = l_tipo
                           i_subject = l_title
                           i_text    = l_mailtext ).
        CATCH cx_document_bcs INTO o_document_bcs.
          message = |Error creando mail { o_document_bcs->get_text( ) }|.
          IF mostrar_info_error = 'X'.
            MESSAGE message TYPE 'I'.
          ENDIF.
      ENDTRY.

*    o_document = cl_document_bcs=>create_document(
*        i_type = l_tipo
*        i_text = l_mailtext
*        i_subject = l_title ).

      IF o_send_request IS INITIAL.
        error = 'X'.
      ELSE.
        l_subject = subject.
        mod_subject_cliente_entorno( CHANGING subject = l_subject ).
        TRY.
            o_send_request->set_message_subject(
                ip_subject = l_subject ).
          CATCH cx_send_req_bcs INTO o_send_req_bcs.
            message = |{ message } Error { o_send_req_bcs->get_text( ) } definiendo asunto: { l_subject }|.
            IF mostrar_info_error = 'X'.
              MESSAGE message TYPE 'I'.
            ENDIF.
        ENDTRY.

        IF emisor IS INITIAL.
          TRY.
              o_sender = cl_sapuser_bcs=>create(
                             i_user = sy-uname ).
            CATCH cx_address_bcs.
              __concat2 message 'Error al definir emisor para usuario:'(eau) sy-uname.
              IF mostrar_info_error = 'X'.
                MESSAGE message TYPE 'I'.
              ENDIF.
          ENDTRY.

*        o_sender = cl_sapuser_bcs=>create( sy-uname ).
        ELSE.
          IF emisor CS '@'.
            l_smtp_addr = emisor.
            TRY.
                o_sender = cl_cam_address_bcs=>create_internet_address( l_smtp_addr ).
              CATCH cx_bcs.
                message = 'Error al definir email'(ede).
                IF mostrar_info_error = 'X'.
                  MESSAGE message TYPE 'I'.
                ENDIF.
            ENDTRY.

          ELSE.
            l_uname = emisor.
            TRY.
                o_sender = cl_sapuser_bcs=>create( l_uname ).
              CATCH cx_bcs.
                message = 'Error al añadir emisor'(eae).
                IF mostrar_info_error = 'X'.
                  MESSAGE message TYPE 'I'.
                ENDIF.
            ENDTRY.
          ENDIF.
        ENDIF.

        IF NOT direccion IS INITIAL.
          DATA(l_msg) = add_destinatario( destinatario = direccion
                            urgente      = urgente
                            parar_en_error = parar_en_error
                            forzar_mail_externo = forzar_mail_externo
                            lista_distribucion = lista_distribucion ).
          __add_lista message l_msg.
        ELSEIF NOT i_destinatarios IS INITIAL.
          LOOP AT i_destinatarios INTO l_destinatario.
            message = add_destinatario( destinatario = l_destinatario
                              urgente      = urgente
                              parar_en_error = parar_en_error
                              forzar_mail_externo = forzar_mail_externo ).
            __add_lista message l_msg.
          ENDLOOP.
        ENDIF.

        IF NOT dest_copia IS INITIAL.
          message = add_destinatario( destinatario = dest_copia
                            copia        = 'X'
                            urgente      = urgente
                            parar_en_error = parar_en_error
                            forzar_mail_externo = forzar_mail_externo
                            lista_distribucion = lista_distribucion ).
          __add_lista message l_msg.
        ENDIF.

        IF NOT dest_copia_oculta IS INITIAL.
          message = add_destinatario( destinatario = dest_copia_oculta
                            copia_oculta = 'X'
                            urgente      = urgente
                            parar_en_error = parar_en_error
                            forzar_mail_externo = forzar_mail_externo
                            lista_distribucion = lista_distribucion ).
          __add_lista message l_msg.
        ENDIF.
        IF NOT message IS INITIAL.
          IF mostrar_info_error = 'X'.
            MESSAGE message TYPE 'I'.
          ENDIF.
        ENDIF.

        TRY.
            o_send_request->set_sender( o_sender ).
          CATCH cx_bcs.
            message = 'Error al añadir emisor'(eae).
            IF mostrar_info_error = 'X'.
              MESSAGE message TYPE 'I'.
            ENDIF.
        ENDTRY.
        IF urgente = 'X'.
          TRY.
              o_send_request->set_send_immediately( abap_true ).
            CATCH cx_bcs.
              message = 'Error en envío inmediato'(eei).
              IF mostrar_info_error = 'X'.
                MESSAGE message TYPE 'I'.
              ENDIF.
          ENDTRY.

          TRY.
              o_send_request->set_priority( '1' ).
            CATCH cx_bcs.
              message = 'Error definiendo prioridad'(edp).
              IF mostrar_info_error = 'X'.
                MESSAGE message TYPE 'I'.
              ENDIF.
          ENDTRY.
        ENDIF.

        TRY.
            o_send_request->set_status_attributes( i_requested_status = confirmacion_lectura ).
          CATCH cx_bcs.
            message = 'Error definiendo confirmacion de lectura'(ecl).
            IF mostrar_info_error = 'X'.
              MESSAGE message TYPE 'I'.
            ENDIF.
        ENDTRY.

        LOOP AT i_adjuntos INTO l_adjunto.
          TRY.
              l_titulo = l_adjunto-subject.
              l_long = l_adjunto-docsize.

              ASSIGN ('ZCL_C=>MAIL_LIMITE_TAMANYO_ADJUNTOS') TO FIELD-SYMBOL(<limite>).
              IF sy-subrc = 0.
                IF <limite> > 0.
                  l_limite_megas = <limite>.
                  l_megas = l_adjunto-docsize / 1048576.
                  l_total_megas = l_total_megas + l_megas.
                  IF l_megas > <limite>.
                    DATA(l_aviso_megas) = 'X'.
                    message = |El tamaño en megas del fichero { l_titulo } es { l_megas }, superior al máximo permitido { <limite> }|.
                    IF mostrar_info_error = 'X'.
                      MESSAGE message TYPE 'I'.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

              CLEAR lt_att_head.
              lv_text_line = zcl_ap_ficheros=>get_extension( l_titulo ).
              IF NOT lv_text_line IS INITIAL.
                sy-tfill = strlen( lv_text_line ).
                IF sy-tfill > 3.
                  CONCATENATE '&SO_FILENAME=' l_titulo INTO lv_text_line.
                  APPEND lv_text_line TO lt_att_head.
                  l_titulo = zcl_ap_ficheros=>get_nombre_fichero( l_titulo ).
                ENDIF.
              ENDIF.

              o_document->add_attachment( i_attachment_type = l_adjunto-doc_type
                                                  i_attachment_subject = l_titulo
                                                  i_attachment_size = l_long
                                                  i_att_content_text = l_adjunto-cont_text
                                                  i_att_content_hex = l_adjunto-cont_hex
                                                  i_attachment_header = lt_att_head ).
            CATCH cx_document_bcs INTO o_document_bcs.
              message = o_document_bcs->get_longtext( ).
              IF message IS INITIAL.
                message = o_document_bcs->get_text( ).
              ENDIF.
              IF message CS 'Se ha producido una excepc'.
                message = |Error adjuntando { l_titulo }|.
              ENDIF.
              IF mostrar_info_error = 'X'.
                MESSAGE message TYPE 'I'.
              ENDIF.
          ENDTRY.
        ENDLOOP.

        IF l_limite_megas > 0 AND l_total_megas > l_limite_megas AND l_aviso_megas IS INITIAL.
          message = |El tamaño en megas del total de adjuntos { l_total_megas } es superior al máximo permitido { <limite> }|.
          IF mostrar_info_error = 'X'.
            MESSAGE message TYPE 'I'.
          ENDIF.
        ENDIF.

        TRY.
            o_send_request->set_document( o_document ).
          CATCH cx_bcs.
            message = 'Error al adjuntar documento'(edo).
            IF mostrar_info_error = 'X'.
              MESSAGE message TYPE 'I'.
            ENDIF.
        ENDTRY.

        TRY.
            i_recipients = o_send_request->recipients( ).
          CATCH cx_bcs.
            message = 'Error al insertar receptores'(ear).
            IF mostrar_info_error = 'X'.
              MESSAGE message TYPE 'I'.
            ENDIF.
          CATCH cx_root INTO DATA(o_root).
            message = |Error al insertar receptores { o_root->get_text( ) }|.
            IF mostrar_info_error = 'X'.
              MESSAGE message TYPE 'I'.
            ENDIF.
        ENDTRY.

        IF NOT i_recipients IS INITIAL OR popup = 'X'.
          SET UPDATE TASK LOCAL.

          MOVE-CORRESPONDING plantilla TO zap_mail_log.
          zap_mail_log-clave  = clave.
          zap_mail_log-fecha  = sy-datum.
          zap_mail_log-hora   = sy-uzeit.
          zap_mail_log-asunto = subject.
          IF direccion IS INITIAL.
            l_dest = destino.
            zap_mail_log-destino = l_dest.
          ELSE.
            l_dest = direccion.
            zap_mail_log-destino = l_dest.
          ENDIF.
          IF zap_mail_log-destino CS '@'.
            TRANSLATE zap_mail_log-destino TO LOWER CASE.
          ENDIF.
          zap_mail_log-emisor = emisor.
          IF zap_mail_log-emisor IS INITIAL.
            zap_mail_log-emisor = sy-uname.
          ENDIF.
          l_dest_copia = dest_copia.
          zap_mail_log-copia = l_dest_copia.
          zap_mail_log-html  = html.
          zap_mail_log-tcode = sy-tcode.
          IF cprog IS INITIAL.
            zap_mail_log-cprog = sy-cprog.
          ELSE.
            zap_mail_log-cprog = cprog.
          ENDIF.

          IF no_si_envio_previo = 'X'.
            DATA(l_string) = zcl_ap_string=>tabla2string( i_texto_mail ).
            zap_mail_log-hash = zcl_ap_string=>get_hash( l_string ).
            IF NOT zap_mail_log-hash IS INITIAL.
              SELECT clave
                FROM zap_mail_log                       "#EC CI_NOFIRST
                WHERE clave   = @zap_mail_log-clave
                  AND destino = @zap_mail_log-destino
                  AND grupo   = @zap_mail_log-grupo
                  AND codigo  = @zap_mail_log-codigo
                  AND asunto  = @zap_mail_log-asunto
                  AND emisor  = @zap_mail_log-emisor
                  AND copia   = @zap_mail_log-copia
                  AND status  = ''
                  AND hash    = @zap_mail_log-hash
                ORDER BY PRIMARY KEY
                INTO @zap_mail_log-clave
                UP TO 1 ROWS.
              ENDSELECT.
              IF sy-subrc = 0.
                error = 'X'.
                message = 'Mail ya fue enviado previamente'.
                zap_mail_log-message = message.
                zap_mail_log-status  = 'DUP'.
                MODIFY zap_mail_log FROM zap_mail_log.
              ENDIF.
            ENDIF.
          ENDIF.

          IF popup = 'X'.
            IF NOT dest_copia IS INITIAL.
              DATA(l_mostrar_copia) = 'X'.
            ENDIF.

            l_dest_cco = dest_copia_oculta.

            IF outlook IS INITIAL.
              CALL FUNCTION 'Z_POPUP_MAIL'
                EXPORTING
                  mostrar_emisor       = 'X'
                  mostrar_destinatario = 'X'
                  mostrar_copia        = l_mostrar_copia
                  mostrar_cco          = mostrar_cco
                  enviar_mail          = 'X'
                  i_adjuntos           = i_adjuntos
                  html                 = html
                  boton_adjuntos       = 'X'
                  plantilla            = plantilla
                  clave                = clave
                TABLES
                  i_texto              = i_texto_mail
                CHANGING
                  subject              = zap_mail_log-asunto
                  direccion            = l_dest
                  emisor               = zap_mail_log-emisor
                  copia                = l_dest_copia
                  cco                  = l_dest_cco
                EXCEPTIONS
                  envio_cancelado      = 1
                  OTHERS               = 2.
              IF sy-subrc <> 0.
                error = 'X'.
                message = 'Se ha cancelado el envío del mail'.
              ENDIF.
            ELSE.
              outlook( subject           = zap_mail_log-asunto
                       direccion         = l_dest
                       dest_copia        = l_dest_copia
                       dest_copia_oculta = l_dest_cco
                       i_textos          = i_texto_mail ).
            ENDIF.
            RETURN.
          ENDIF.

          IF commit = 'X'.
* Si es un proceso en fondo, nos aseguramos que no es en UPDATE, si es así, lanzamos la funcion Z_MAIL_REMOTO que lo lanza en otro hilo de ejecucion
            DATA(l_upd_task) = zcl_ap_utils=>es_in_update_task( ).
            IF l_upd_task IS INITIAL.
              l_upd_task = zcl_ap_utils=>existe_en_pila( evento = 'BADI_IN_UPDATE' ).
            ENDIF.
            IF l_upd_task = 'X'.
              IF zcl_ap_utils=>existe_en_pila( evento = 'Z_MAIL_REMOTO' ).
                CLEAR l_upd_task.
              ENDIF.
            ENDIF.
          ENDIF.
          IF l_upd_task = 'X'.
            zap_mail_log-clave = clave.
            l_s = zap_mail_log-asunto.
            l_e = zap_mail_log-emisor.
            CALL FUNCTION 'Z_MAIL_REMOTO'
              DESTINATION 'NONE'
              EXPORTING
                i_adjuntos         = i_adjuntos
                html               = html
                clave              = zap_mail_log-clave
                lista_distribucion = lista_distribucion
                subject            = l_s
                direccion          = l_dest
                emisor             = l_e
                copia              = l_dest_copia
                cco                = l_dest_cco
              TABLES
                i_texto            = i_texto_mail
              EXCEPTIONS
                OTHERS             = 1.

            IF sy-subrc <> 0.
              message = 'Error llamando función Z_MAIL_REMOTO'.
            ENDIF.

            RETURN.
          ELSE.
            TRY.
                l_result = o_send_request->send(
                               i_with_error_screen = space ).
              CATCH cx_send_req_bcs INTO o_send_req_bcs.
                message = o_send_req_bcs->get_text( ).
                IF mostrar_info_error = 'X'.
                  MESSAGE message TYPE 'I'.
                ENDIF.
            ENDTRY.
          ENDIF.

          IF commit = 'X'.
            COMMIT WORK AND WAIT.
            IF controlar_salida = 'X' AND message IS INITIAL.
              DO 3 TIMES.
                SELECT  *
                   FROM soos AS f JOIN soes AS g
                         ON  f~rectp = g~rectp
                         AND f~recyr = g~recyr
                         AND f~recno = g~recno
                                  JOIN sood AS s
                           ON  f~objtp = s~objtp
                           AND f~objyr = s~objyr
                           AND f~objno = s~objno
                                  JOIN bcst_sr AS e
                           ON f~sndreq = e~os_guid
                  INTO CORRESPONDING FIELDS OF l_soes
                  UP TO 1 ROWS
                  WHERE s~objdes  = subject
                    AND crdat    >= l_fecha
                    AND crtim    >= l_hora
                    AND sndnam    = sy-uname
                  ORDER BY crdat DESCENDING crtim DESCENDING.
                ENDSELECT.

                IF sy-subrc = 0.
                  CASE l_soes-status.
                    WHEN '850'.
                      message = 'Mensaje no enviado porque se ha sobrepasado el tamaño máximo de adjuntos'(mne).
                  ENDCASE.
                  IF mostrar_info_error = 'X' AND NOT message IS INITIAL.
                    MESSAGE message TYPE 'I'.
                  ENDIF.
                  EXIT.
                ELSE.
                  WAIT UP TO 1 SECONDS.
                ENDIF.
              ENDDO.
            ENDIF.
          ENDIF.

          IF NOT clave IS INITIAL.
            zap_mail_log-message = message.
            IF NOT msg_pdf IS INITIAL.
              CONCATENATE zap_mail_log-message msg_pdf INTO zap_mail_log-message SEPARATED BY space.
            ENDIF.
            MODIFY zap_mail_log FROM zap_mail_log.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    SET PARAMETER ID 'ZMAIL' FIELD ''.                      "#EC *
  ENDMETHOD.
  METHOD fin_tabla_html.
    set_text( '</table>' ).
  ENDMETHOD.
  METHOD free.
    CLEAR: i_texto_mail, i_destinatarios,
           o_send_request, o_document, o_sender, o_recipient,
           i_adjuntos, destino, plantilla.
  ENDMETHOD.
  METHOD get_dir_envio_from_adrnr.
    SELECT g~name_text
           FROM adcp AS f JOIN adrp AS g
           ON  f~persnumber = g~persnumber
           AND f~date_from  = g~date_from
           AND f~nation     = g~nation
     INTO (email)
      UP TO 1 ROWS
    WHERE f~so_key = adrnr
      ORDER BY f~addrnumber g~persnumber f~date_from f~nation.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_plantilla.
    DATA l_texto TYPE string.

    IF version <> '88'.
      SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
       WHERE grupo   = grupo
         AND codigo  = codigo
         AND version = version
         AND spras   = spras.
      IF sy-subrc <> 0.
        SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
         WHERE grupo   = grupo
           AND codigo  = codigo
           AND version = version
           AND spras   = ''.
        IF sy-subrc <> 0.
          SELECT *
            FROM zap_textos_mail
            WHERE grupo   = @grupo
              AND codigo  = @codigo
              AND version = @version
            ORDER BY PRIMARY KEY
            INTO @plantilla
            UP TO 1 ROWS.
          ENDSELECT.
        ENDIF.
      ENDIF.
    ELSE.
      SELECT *
        FROM zap_textos_mail
        WHERE grupo   = @grupo
          AND codigo  = @codigo
          AND defecto = 'X'
          AND spras   = @spras
        ORDER BY PRIMARY KEY
        INTO @plantilla
        UP TO 1 ROWS.
      ENDSELECT.
      IF sy-subrc <> 0.
        SELECT *
          FROM zap_textos_mail
          WHERE grupo   = @grupo
            AND codigo  = @codigo
            AND defecto = 'X'
            AND spras   = ''
          ORDER BY PRIMARY KEY
          INTO @plantilla
          UP TO 1 ROWS.
        ENDSELECT.
        IF sy-subrc <> 0.
          SELECT *
            FROM zap_textos_mail
            WHERE grupo   = @grupo
              AND codigo  = @codigo
              AND defecto = 'X'
            ORDER BY PRIMARY KEY
            INTO @plantilla
            UP TO 1 ROWS.
          ENDSELECT.
        ENDIF.
      ENDIF.
      IF plantilla IS INITIAL.
        SELECT *
          FROM zap_textos_mail
          WHERE grupo  = @grupo
            AND codigo = @codigo
            AND spras  = @spras
          ORDER BY PRIMARY KEY
          INTO @plantilla
          UP TO 1 ROWS.
        ENDSELECT.
        IF sy-subrc <> 0.
          SELECT *
            FROM zap_textos_mail
            WHERE grupo  = @grupo
              AND codigo = @codigo
              AND spras  = ''
            ORDER BY PRIMARY KEY
            INTO @plantilla
            UP TO 1 ROWS.
          ENDSELECT.
          IF sy-subrc <> 0.
            SELECT *
              FROM zap_textos_mail
              WHERE grupo  = @grupo
                AND codigo = @codigo
              ORDER BY PRIMARY KEY
              INTO @plantilla
              UP TO 1 ROWS.
            ENDSELECT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT plantilla-email_pruebas IS INITIAL.
      IF plantilla-email_pruebas = 'SY-UNAME'.
        IF sy-uname = 'WF-BATCH' OR sy-uname = 'SCP' OR sy-uname = 'SAP_WFRT'. "#EC EMPTY_IF_BRANCH
* Usuarios no "reales", no pueden recibir email de pruebas.
        ELSE.
          plantilla-email_pruebas = sy-uname.
        ENDIF.
      ENDIF.
      IF NOT plantilla-email_pruebas IS INITIAL.
        l_texto = 'Destino al que debería haber ido el mail {mail_destino}'.
        IF plantilla-html = 'X'.
          CONCATENATE '<P><B>' l_texto '</B></P>' INTO l_texto.
        ENDIF.
        CONCATENATE plantilla-texto cl_abap_char_utilities=>cr_lf
                    cl_abap_char_utilities=>cr_lf l_texto INTO plantilla-texto.
      ENDIF.
    ENDIF.

    IF sy-sysid <> zcl_c=>entorno_produccion.
      IF plantilla-aviso_pruebas = 'X'.
        CONCATENATE plantilla-asunto 'ENTORNO DE PRUEBAS:' sy-sysid INTO plantilla-asunto SEPARATED BY space.
        CONCATENATE
       'MAIL ENVIADO EN ENTORNO DE PRUEBAS:' sy-sysid cl_abap_char_utilities=>cr_lf
       INTO l_texto.
        IF plantilla-html = 'X'.
          CONCATENATE '<P><B>' l_texto '</B></P>' INTO l_texto.
        ENDIF.
        CONCATENATE plantilla-texto cl_abap_char_utilities=>cr_lf
                    cl_abap_char_utilities=>cr_lf l_texto INTO plantilla-texto.
      ENDIF.
    ENDIF.

    LOOP AT variables ASSIGNING FIELD-SYMBOL(<var>).
      REPLACE ALL OCCURRENCES OF <var>-key IN plantilla-texto WITH <var>-value IGNORING CASE.

      REPLACE ALL OCCURRENCES OF <var>-key IN plantilla-asunto WITH <var>-value IGNORING CASE.
    ENDLOOP.
    CONDENSE plantilla-asunto.

    me->plantilla = plantilla.

    CLEAR msg_pdf.
    IF NOT plantilla-adobe IS INITIAL.
      DATA(o_sfp) = NEW zcl_ap_sfp( form = plantilla-adobe  getpdf = 'X' ).

      IF o_sfp->cancelado IS INITIAL AND o_sfp->error IS INITIAL.
        TRY.
            CALL FUNCTION o_sfp->funcion
              EXPORTING
                /1bcdwb/docparams  = o_sfp->docparams
                variables          = variables
                cuerpo             = plantilla-texto
              IMPORTING
                /1bcdwb/formoutput = o_sfp->output
              EXCEPTIONS
                usage_error        = 1
                system_error       = 2
                internal_error     = 3
                OTHERS             = 4.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                      INTO msg_pdf.
            ENDIF.
          CATCH cx_root INTO DATA(o_root). "#EC *
            msg_pdf = o_root->get_text( ).
        ENDTRY.

        IF NOT o_sfp->output IS INITIAL.
          IF plantilla-nombre_adjunto IS INITIAL OR NOT plantilla-binario IS INITIAL.
            l_texto = 'Contenido.pdf'.
          ELSE.
            l_texto = plantilla-nombre_adjunto.
            DATA(l_ext) = zcl_ap_ficheros=>get_extension( plantilla-nombre_adjunto ).
            TRANSLATE l_ext TO LOWER CASE.
            IF l_ext <> 'pdf'.
              CONCATENATE l_texto '.pdf' INTO l_texto.
            ENDIF.
          ENDIF.

          add_pdf( pdf = o_sfp->output-pdf titulo = l_texto ).
        ENDIF.
      ELSE.
        msg_pdf = 'Error abriendo form'.
      ENDIF.

      o_sfp->cerrar_job( ).
      IF NOT msg_pdf IS INITIAL.
        CONCATENATE 'Error en Adobe' plantilla-adobe msg_pdf INTO msg_pdf SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF NOT plantilla-binario IS INITIAL AND NOT plantilla-nombre_adjunto IS INITIAL.
      add_pdf( pdf = plantilla-binario titulo = plantilla-nombre_adjunto ).
    ENDIF.

    IF NOT plantilla-binario2 IS INITIAL AND NOT plantilla-nombre_adjunto2 IS INITIAL.
      add_pdf( pdf = plantilla-binario2 titulo = plantilla-nombre_adjunto2 ).
    ENDIF.
  ENDMETHOD.
  METHOD info_envio_mail.
    DATA: r_msgv1  TYPE RANGE OF symsgv,
          lr_msgv1 LIKE LINE OF r_msgv1,
          l_icono  TYPE icon_d.

    IF NOT mail IS INITIAL.
      CLEAR lr_msgv1.
      lr_msgv1-option = 'EQ'.
      lr_msgv1-sign   = 'I'.
      lr_msgv1-low    = mail.
      APPEND lr_msgv1 TO r_msgv1.
    ENDIF.

    SELECT status crdat crtim sndnam msgv1
       FROM soos AS f JOIN soes AS g
             ON  f~rectp = g~rectp
             AND f~recyr = g~recyr
             AND f~recno = g~recno
                      JOIN sood AS s
               ON  f~objtp = s~objtp
               AND f~objyr = s~objyr
               AND f~objno = s~objno
                      JOIN bcst_sr AS e
               ON f~sndreq = e~os_guid
      INTO (status, crdat, crtim, sndnam, msgv1)
      UP TO 1 ROWS
      WHERE s~objdes  = subject
        AND msgv1    IN r_msgv1
      ORDER BY crdat DESCENDING crtim DESCENDING.
    ENDSELECT.
    IF sy-subrc <> 0.
      type = 'N'.
    ELSE.
      CASE status.
        WHEN '73'.
          type = 'S'.
          l_icono = icon_mail. " ICON_MAIL
          message = zcl_ap_utils=>concat( p1 = 'Mail enviado el'(mee) p2 = crdat p3 = 'a las'(ala) p4 = crtim p5 = 'por'(por) p6 = sndnam ).
        WHEN '672'.
          type = 'W'.
          l_icono = icon_time. " ICON_TIME
          message = zcl_ap_utils=>concat( p1 = 'Mail en espera de envío. Creado el'(mev) p2 = crdat p3 = 'a las'(ala) p4 = crtim p5 = 'por'(por) p6 = sndnam ).
        WHEN '751'.
          type = 'W'.
          l_icono = icon_failure. " FAILURE
          message = zcl_ap_utils=>concat( p1 = 'Mail en espera por ERROR CONEXION. Creado el'(mec) p2 = crdat p3 = 'a las'(ala) p4 = crtim p5 = 'por'(por) p6 = sndnam  ).
        WHEN '850'.
          type = 'E'.
          l_icono = icon_led_red. " ICON_LED_RED
          message = 'Mensaje no enviado porque se ha sobrepasado el tamaño máximo de adjuntos'(mta).
        WHEN OTHERS.
          type = 'E'.
          l_icono = icon_led_red. " ICON_LED_RED
          message = zcl_ap_utils=>concat( p1 = 'Mail con status'(mcs) p2 = status p3 = 'no envíado. Creado el'(nec) p4 = crdat p5 = 'a las'(ala) p6 = crtim p7 = 'por'(por) p8 = sndnam  ).
      ENDCASE.

      IF add_destino = 'X' AND NOT msgv1 IS INITIAL.
        message = zcl_ap_utils=>concat( p1 = message  p2 = 'a' p3 = msgv1 ).
      ENDIF.
    ENDIF.

    IF NOT add_icono IS INITIAL AND NOT message IS INITIAL AND NOT l_icono IS INITIAL.
      CONCATENATE l_icono message INTO message SEPARATED BY space.
    ENDIF.
  ENDMETHOD.
  METHOD ini_textos.
    CLEAR i_texto_mail.
  ENDMETHOD.
  METHOD inicio_tabla_html.
    DATA: l_long   TYPE c LENGTH 10,
          l_border TYPE c LENGTH 3,
          l_aux    TYPE c LENGTH 40.

    l_long = longitud.
    WRITE border TO l_border.
    CONDENSE l_border NO-GAPS.

    CONCATENATE '" border="' l_border '">' INTO l_aux.
    set_text( texto  = '<table width="' texto2 = l_long
              texto3 = l_aux ).
    CONCATENATE '  <tr bgcolor="' color '">' INTO l_aux.
    set_text(  l_aux ).
    set_text( texto  = '  <th scope="col">' texto2 = c1
              texto3 = '</th>' ).
    IF c2 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c2
                texto3 = '</th>' ).
    ENDIF.
    IF c3 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c3
                texto3 = '</th>' ).
    ENDIF.
    IF c4 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c4
                texto3 = '</th>' ).
    ENDIF.
    IF c5 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c5
                texto3 = '</th>' ).
    ENDIF.
    IF c6 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c6
                texto3 = '</th>' ).
    ENDIF.
    IF c7 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c7
                texto3 = '</th>' ).
    ENDIF.
    IF c8 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c8
                texto3 = '</th>' ).
    ENDIF.
    IF c9 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c9
                texto3 = '</th>' ).
    ENDIF.
    IF c10 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c10
                texto3 = '</th>' ).
    ENDIF.
    IF c11 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c11
                texto3 = '</th>' ).
    ENDIF.
    IF c12 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c12
                texto3 = '</th>' ).
    ENDIF.
    IF c13 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c13
                texto3 = '</th>' ).
    ENDIF.
    IF c14 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c14
                texto3 = '</th>' ).
    ENDIF.
    IF c15 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c15
                texto3 = '</th>' ).
    ENDIF.
    IF c16 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c16
                texto3 = '</th>' ).
    ENDIF.
    IF c17 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c17
                texto3 = '</th>' ).
    ENDIF.
    IF c18 IS SUPPLIED.
      set_text( texto  = '  <th scope="col">' texto2 = c18
                texto3 = '</th>' ).
    ENDIF.

    set_text(  '  </tr>' ).
  ENDMETHOD.
  METHOD mail.
    DATA: o_mail           TYPE REF TO zcl_ap_envio_mail,
          l_direccion      TYPE string,
          l_texto          TYPE solisti1,
          l_titulo         TYPE string,
          l_nombre_fichero TYPE so_obj_des,
          l_tipo           TYPE soodk-objtp,
          l_xstring        TYPE xstring,
          l_aux            TYPE c LENGTH 10,
          l_asunto         TYPE string,
          l_html           TYPE abap_bool,
          l_emisor         TYPE string.

    o_mail = NEW #(
        usar_clases = 'X' ).

    IF NOT plantilla IS INITIAL.
      o_mail->plantilla = plantilla.
    ELSEIF NOT grupo IS INITIAL.
      o_mail->get_plantilla( grupo = grupo codigo = codigo version = version spras = spras variables = variables ).
    ENDIF.

    IF direccion IS INITIAL.
      l_direccion = o_mail->plantilla-destino.
    ELSE.
      l_direccion = direccion.
    ENDIF.

    IF NOT o_mail->plantilla-email_pruebas IS INITIAL.
      REPLACE '{mail_destino}' WITH l_direccion INTO o_mail->plantilla-texto.
      l_direccion = o_mail->plantilla-email_pruebas.
    ENDIF.

    IF NOT texto IS INITIAL.
      o_mail->set_text( texto ).
    ELSEIF NOT o_mail->plantilla-texto IS INITIAL.
      o_mail->set_text( o_mail->plantilla-texto ).
    ENDIF.

    LOOP AT i_textos INTO l_texto.
      o_mail->set_text( l_texto ).
    ENDLOOP.

    APPEND LINES OF i_adjuntos TO o_mail->i_adjuntos.

    IF NOT pdf IS INITIAL.
      IF NOT nombre_fichero IS INITIAL.
        l_titulo = nombre_fichero.
      ELSE.
        l_titulo = 'ADJUNTO.PDF'.
      ENDIF.
      o_mail->add_pdf( pdf = pdf titulo = l_titulo ).
    ELSEIF NOT nombre_fichero IS INITIAL AND i_tabla IS INITIAL.
      o_mail->add_fichero_local( fichero = nombre_fichero ).
    ENDIF.

    IF NOT pdf2 IS INITIAL.
      IF NOT nombre_fichero2 IS INITIAL.
        l_titulo = nombre_fichero2.
      ELSE.
        l_titulo = 'ADJUNTO2.PDF'.
      ENDIF.
      o_mail->add_pdf( pdf = pdf2 titulo = l_titulo ).
    ENDIF.

    IF nombre_fichero_tabla IS INITIAL.
      IF nombre_fichero IS INITIAL.
        l_nombre_fichero = 'TABLA.XLSX'.
      ELSE.
        l_nombre_fichero = nombre_fichero.
      ENDIF.
    ELSE.
      l_nombre_fichero = nombre_fichero_tabla.
    ENDIF.

    IF NOT i_tabla IS INITIAL.
      l_tipo = zcl_ap_ficheros=>get_extension( l_nombre_fichero ).
      TRANSLATE l_tipo TO UPPER CASE.
      IF tabla_como_txt <> ''.
        CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
          IMPORTING
            buffer   = l_xstring
          TABLES
            text_tab = i_tabla
          EXCEPTIONS
            failed   = 1
            OTHERS   = 2.
        IF sy-subrc = 0.
          o_mail->add_adjunto(
            tipo    = l_tipo
            titulo  = l_nombre_fichero
            xstring = l_xstring ).
        ENDIF.
      ELSEIF NOT xstring IS INITIAL.
        o_mail->add_adjunto(
            tipo    = l_tipo
            titulo  = l_nombre_fichero
            xstring = l_xstring ).
      ELSE.
        DESCRIBE TABLE i_tabla LINES sy-tfill.
        IF sy-tfill > 20000. " Si tiene más de 20000 líneas lo más securo es que casque por rendimiento, así que en lugar de enviar la tabla enviamos un mensaje diciendo que no se envía
          l_aux = sy-tfill.
          CONCATENATE 'Tabla demasiado grande ('(tdg) l_aux '). No es posible adjuntar contenido'(npa) INTO l_texto.
          o_mail->set_text( l_texto ).
        ELSE.
          IF tabla_como_alv IS INITIAL.
            o_mail->add_itab_as_xls( descripcion = l_nombre_fichero zip = zip itab = i_tabla ).
          ELSE.
            o_mail->add_itab_alv_as_xls( EXPORTING descripcion = l_nombre_fichero zip = zip quitar_caracteres_extranos = 'X' o_alv_origen = o_alv_origen o_grid_origen = o_grid_origen conversion_sap = conversion_sap
                                         CHANGING itab = i_tabla ).
          ENDIF.
        ENDIF.
      ENDIF.
    ELSEIF NOT o_grid_origen IS INITIAL.
      o_mail->add_itab_alv_as_xls( EXPORTING descripcion = l_nombre_fichero  zip = zip quitar_caracteres_extranos = 'X' o_grid_origen = o_grid_origen conversion_sap = conversion_sap
                                   CHANGING itab = i_tabla ).
    ELSEIF NOT o_alv_origen IS INITIAL.
      o_mail->add_itab_alv_as_xls( EXPORTING descripcion = l_nombre_fichero  zip = zip quitar_caracteres_extranos = 'X' o_alv_origen = o_alv_origen conversion_sap = conversion_sap
                                   CHANGING itab = i_tabla ).
    ELSEIF NOT xstring IS INITIAL.
      l_tipo = zcl_ap_ficheros=>get_extension( l_nombre_fichero ).
      o_mail->add_adjunto(
          tipo    = l_tipo
          titulo  = l_nombre_fichero
          xstring = xstring ).
    ENDIF.

    IF subject IS INITIAL.
      l_asunto = o_mail->plantilla-asunto.
    ELSE.
      l_asunto = subject.
    ENDIF.

    IF html IS INITIAL.
      l_html = o_mail->plantilla-html.
    ELSE.
      l_html = html.
    ENDIF.

    l_emisor = emisor.
    IF l_emisor IS INITIAL AND NOT o_mail->plantilla-emisor IS INITIAL.
      l_emisor = o_mail->plantilla-emisor.
    ENDIF.

    LOOP AT i_adj_bin ASSIGNING FIELD-SYMBOL(<adjbin>).
      o_mail->add_adjunto( titulo = <adjbin>-fichero xstring = <adjbin>-xstring tipo = '' ).
    ENDLOOP.

    IF o_mail->envio_mail( subject    = l_asunto
      direccion  = l_direccion
      dest_copia = dest_copia
      dest_copia_oculta = dest_copia_oculta
      mostrar_cco = mostrar_cco
      urgente = urgente
      html    = l_html
      commit  = commit
      parar_en_error      = parar_en_error
      forzar_mail_externo = forzar_mail_externo
      confirmacion_lectura = confirmacion_lectura
      emisor  = l_emisor
      controlar_salida = controlar_salida
      lista_distribucion = lista_distribucion
      clave = clave
      cprog = cprog
      no_si_envio_previo = no_si_envio_previo
      popup = popup
      outlook = outlook ) = 'X'.
      message = o_mail->message.
    ENDIF.

    o_mail->free( ).
  ENDMETHOD.
  METHOD mod_subject_cliente_entorno.
    DATA: l_prod    TYPE abap_bool,
          l_sistema TYPE string.

    ASSIGN ('ZCL_C=>MAIL_INCLUIR_SISTEMA_EN_ASUNTO') TO FIELD-SYMBOL(<sistema>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF <sistema> IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
* Buscamos si existe el método especico en la clase
        CALL METHOD ('ZCL_C')=>('ES_PRODUCCION')
          EXPORTING
            sistema  = sy-sysid
          RECEIVING
            response = l_prod.
      CATCH cx_sy_dyn_call_illegal_method.
* Si no lo hacemos por la constante
        IF sy-sysid = zcl_c=>entorno_produccion.
          l_prod = 'X'.
        ENDIF.
    ENDTRY.

    IF l_prod IS INITIAL.
      l_sistema = <sistema>.
      REPLACE '{SY-SYSID}' IN l_sistema WITH sy-sysid.
      IF NOT subject CS l_sistema.
        CONCATENATE l_sistema subject INTO subject SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD nombre_usuario.
    SELECT adrp~name_text
     FROM usr21 JOIN adrp ON usr21~persnumber = adrp~persnumber
      UP TO 1 ROWS
      INTO (nombre)
     WHERE usr21~bname = uname
      ORDER BY adrp~date_from DESCENDING.
    ENDSELECT.
  ENDMETHOD.
  METHOD outlook.
    DATA: t_vbs          TYPE STANDARD TABLE OF soli,
          l_dir          TYPE string,
          l_vbs          TYPE soli,
          g_last         TYPE c LENGTH 1,
          i_text         TYPE TABLE OF string,
          g_vbs_filename TYPE rlgrap-filename,
          commandline    TYPE c LENGTH 1000.
    DATA l_string TYPE string.

    APPEND: 'Dim myolapp ' TO t_vbs,
        'Dim olNamespace ' TO t_vbs,
        'Dim myItem ' TO t_vbs,
        'Dim myRecipient ' TO t_vbs,
        'Dim myRecipientCC ' TO t_vbs,
        'Dim myRecipientCCO ' TO t_vbs,
        'Dim myAttachments ' TO t_vbs,
        ' ' TO t_vbs,
        'Set myolapp = CreateObject("Outlook.Application") ' TO t_vbs,
        'Set olNamespace = myolapp.GetNamespace("MAPI") ' TO t_vbs,
        'Set myItem = myolapp.CreateItem(olMailItem) ' TO t_vbs,
        ' ' TO t_vbs.

    IF NOT direccion IS INITIAL.
      l_dir = direccion.
      REPLACE ALL OCCURRENCES OF ';' IN l_dir WITH ','.
      SPLIT l_dir AT ',' INTO TABLE DATA(i_dest1).

      LOOP AT i_dest1 ASSIGNING FIELD-SYMBOL(<des>).
        CONCATENATE 'Set myRecipient = myItem.Recipients.Add("' <des> '")' INTO l_vbs.
        APPEND l_vbs TO t_vbs.
      ENDLOOP.
    ENDIF.

    IF NOT dest_copia IS INITIAL.
      l_dir = dest_copia.
      REPLACE ALL OCCURRENCES OF ';' IN l_dir WITH ','.
      SPLIT l_dir AT ',' INTO TABLE DATA(i_dest2).

      LOOP AT i_dest2 ASSIGNING FIELD-SYMBOL(<dest2>).
        CONCATENATE 'Set myRecipientCC = myItem.Recipients.Add("' <dest2> '")' INTO l_vbs.
        APPEND l_vbs TO t_vbs.
        APPEND 'myRecipientCC.Type = olCC' TO t_vbs.
        APPEND 'myRecipientCC.Resolve' TO t_vbs.
        CONCATENATE 'myItem.CC = "' <dest2> '"' INTO l_vbs.
        APPEND l_vbs TO t_vbs.
      ENDLOOP.
    ENDIF.

    IF NOT dest_copia_oculta IS INITIAL.
      l_dir = dest_copia_oculta.
      REPLACE ALL OCCURRENCES OF ';' IN l_dir WITH ','.
      SPLIT l_dir AT ',' INTO TABLE DATA(i_dest3).

      LOOP AT i_dest3 ASSIGNING FIELD-SYMBOL(<dest3>).
        CONCATENATE 'Set myRecipientCCO = myItem.Recipients.Add("' <dest3> '")' INTO l_vbs.
        APPEND l_vbs TO t_vbs.
        APPEND 'myRecipientCCO.Type = olBCC' TO t_vbs.
        APPEND 'myRecipientCCO.Resolve' TO t_vbs.
        CONCATENATE 'myItem.BCC = "' <dest3> '"' INTO l_vbs.
        APPEND l_vbs TO t_vbs.
      ENDLOOP.
    ENDIF.

    CONCATENATE 'myItem.Subject = "' subject '"' INTO l_vbs.
    APPEND l_vbs TO t_vbs.

*- Ficheros adjuntos
    APPEND 'Set myAttachments = myItem.Attachments' TO t_vbs.

*- Chequeamos la existencia de los ficheros adjuntos

***    g_file = l_titulo.
***
***    CALL FUNCTION 'WS_QUERY'
***      EXPORTING
***        filename       = g_file
***        query          = 'FE'
***      EXCEPTIONS
***        inv_query      = 1
***        no_batch       = 2
***        frontend_error = 3
***        OTHERS         = 4.
***    IF sy-subrc EQ 0.
***      CONCATENATE 'myAttachments.Add("' l_titulo '")'
***      INTO l_vbs.
***      APPEND l_vbs TO t_vbs.
***    ELSE.
***      MESSAGE i000(38) WITH
***        'No se ha podido adjuntar el fichero' g_file.
***    ENDIF.

*- Cuerpo del email
    CLEAR: g_last, l_vbs.
    APPEND l_vbs TO t_vbs.

    IF NOT texto IS INITIAL.
      zcl_ap_string=>string2tabla( EXPORTING string = texto CHANGING tabla = i_text ).
    ELSEIF NOT i_textos IS INITIAL.
      LOOP AT i_textos ASSIGNING FIELD-SYMBOL(<textos>).
        APPEND <textos> TO i_text.
      ENDLOOP.
    ENDIF.

    LOOP AT i_text ASSIGNING FIELD-SYMBOL(<text>).
      AT FIRST.
        APPEND 'myitem.body = _' TO t_vbs.
      ENDAT.
      AT LAST.
        g_last = 'X'.
      ENDAT.

*- Double-quotes(") will cause an error in VBScript
*- Replace with a hex-tab and then replace with
*- 2 double-quotes ("")
      WHILE sy-subrc = 0.
        REPLACE cl_abap_char_utilities=>cr_lf+1(1) WITH '""' INTO <text>.
      ENDWHILE.

      IF g_last = 'X'.
        CONCATENATE '"' <text> '" &vbCrLf '
                    INTO l_vbs.
      ELSE.
        CONCATENATE '"' <text> '" &vbCrLf &_'
                    INTO l_vbs.
      ENDIF.
      APPEND l_vbs TO t_vbs.
    ENDLOOP.

    APPEND 'myItem.Display' TO t_vbs.
*  APPEND 'myItem.Send' TO t_vbs.

*- Preparamos el nombre de fichero vbscript para descargarlo
*- y ejecutarlo, llamando a la variable de entorno de Windows
*- TEMP
    CLEAR g_vbs_filename.
    CALL FUNCTION 'WS_QUERY' ##FM_OLDED
      EXPORTING
        environment    = 'TEMP'
        query          = 'EN'
      IMPORTING
        return         = g_vbs_filename
      EXCEPTIONS
        inv_query      = 1
        no_batch       = 2
        frontend_error = 3
        OTHERS         = 4.
    IF sy-subrc = 0.
      MESSAGE 'Error buscando directorio temporal' TYPE 'E'.
    ENDIF.

    CONCATENATE g_vbs_filename 'mail.vbs' INTO g_vbs_filename.
    commandline = g_vbs_filename.

    l_string = g_vbs_filename.

*- Descargamos el fichero vbscript
    zcl_ap_ficheros=>grabar( EXPORTING fichero = l_string
                             CHANGING tabla = t_vbs ).

    CALL FUNCTION 'WS_EXECUTE' ##FM_OLDED
      EXPORTING
        commandline    = commandline
        program        = 'WSCRIPT.EXE'
      EXCEPTIONS
        frontend_error = 1
        no_batch       = 2
        prog_not_found = 3
        illegal_option = 4
        OTHERS         = 5.
    IF sy-subrc <> 0.
      MESSAGE |Error ejecutando { commandline }| TYPE 'E'.
    ENDIF.

    MESSAGE s000(38) WITH 'Abriendo Outlook... Espere por favor'.
    WAIT UP TO 5 SECONDS.
  ENDMETHOD.
  METHOD set_text.
    DATA: l_tipo   TYPE c LENGTH 1,
          l_long   TYPE i,
          l_long2  TYPE i,
          l_texto  TYPE solisti1,
          l_texto2 TYPE solisti1,
          l_string TYPE string,
          i_tt     TYPE TABLE OF solisti1.

    IF tabla IS INITIAL.

      DESCRIBE FIELD texto TYPE l_tipo.
      IF l_tipo = 'C' OR l_tipo = 'g'.
        l_long = strlen( texto ).
      ENDIF.
      IF texto2 IS SUPPLIED.
        DESCRIBE FIELD texto2 TYPE l_tipo.
        IF l_tipo = 'C' OR l_tipo = 'g'.
          l_long2 = strlen( texto2 ).
        ENDIF.
      ENDIF.
      IF l_long <= 255 AND l_long2 <= 255.
        WRITE texto TO l_texto.
        IF texto2 IS SUPPLIED.
          DESCRIBE FIELD texto2 TYPE l_tipo.
          WRITE texto2 TO l_texto2.
          CASE l_tipo.
            WHEN 'P'.
              WRITE texto2 TO l_texto2(15).
            WHEN 'D'.
              IF texto2 = '00000000'.
                CLEAR l_texto2.
              ELSE.
                WRITE texto2 TO l_texto2.
              ENDIF.
            WHEN 'T'.
              IF texto2 = '000000'.
                CLEAR l_texto2.
              ELSE.
                WRITE texto2 TO l_texto2.
              ENDIF.
            WHEN 's' OR 'I'.
              CONDENSE l_texto2 NO-GAPS.
          ENDCASE.
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

        APPEND l_texto TO i_texto_mail.
      ELSE.
        CONCATENATE texto texto2 texto3 texto4 INTO l_string SEPARATED BY space.
        IF l_string CS '<br>'.
          SPLIT l_string AT '<br>' INTO DATA(l_lin1) DATA(l_lin2).
          IF strlen( l_lin1 ) <= 255 AND strlen( l_lin2 ) <= 255.
            APPEND l_lin1 TO i_texto_mail.
            APPEND '<br>' TO i_texto_mail.
            APPEND l_lin2 TO i_texto_mail.
            DATA(l_ok) = 'X'.
          ENDIF.
        ENDIF.

        IF l_ok IS INITIAL.
          zcl_ap_string=>string2tabla( EXPORTING string = l_string longitud = 250 CHANGING tabla = i_tt ).
          LOOP AT i_tt INTO l_texto.
            APPEND l_texto TO i_texto_mail.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE.
      i_texto_mail = tabla.
    ENDIF.
  ENDMETHOD.
  METHOD set_text_as_string.
    zcl_ap_string=>string2tablastring( EXPORTING string = texto
                                                 longitud = 255
                                       CHANGING  tabla = i_texto_mail ).
  ENDMETHOD.
  METHOD set_texto_estandar_html.
    DATA: i_lineas     TYPE tlinetab,
          l_texto      TYPE solisti1,
          l_tline      TYPE tline,
          l_texto_ant  TYPE solisti1,
          l_first      TYPE c LENGTH 1,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_fin        TYPE c LENGTH 1,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_format_ant TYPE tdformat.

    i_lineas = zcl_ap_textos=>get_texto( id     = id
                                      name   = name
                                      spras  = spras
                                      object = object ).

    IF i_lineas IS INITIAL.
      RETURN.
    ENDIF.

    l_texto = '<P>'.
    LOOP AT i_lineas INTO l_tline.
      l_texto_ant = l_texto.
      CLEAR: l_first, l_fin.
      AT FIRST.
        l_first = 'X'.
      ENDAT.
      AT LAST.
        l_fin = 'X'.
      ENDAT.
      IF l_first = ''.
        IF NOT l_tline-tdformat IS INITIAL.
          CONCATENATE l_texto '</P>' INTO l_texto.
          set_text( l_texto ).
          l_texto = '<P>'.
        ENDIF.
      ENDIF.
      CONCATENATE l_texto l_tline-tdline INTO l_texto.
      l_format_ant = l_tline-tdformat.

      IF strlen( l_texto ) > 250.
        CONCATENATE l_texto_ant '</P>' INTO l_texto_ant.
        set_text( l_texto_ant ).
        CONCATENATE '<P>' l_tline-tdline INTO l_texto.
      ENDIF.
    ENDLOOP.
    IF l_texto <> '<P>'.
      CONCATENATE l_texto '</P>' INTO l_texto.
      IF strlen( l_texto ) > 250.
        CONCATENATE l_texto_ant '</P>' INTO l_texto_ant.
        set_text( l_texto_ant ).
        CONCATENATE '<P>' l_tline-tdline INTO l_texto.
      ENDIF.
      set_text( l_texto ).
    ENDIF.
  ENDMETHOD.
  METHOD usr2email.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_date_from TYPE adr6-date_from.

    SELECT adr6~smtp_addr date_from
      INTO (email, l_date_from)
      UP TO 1 ROWS
     FROM usr21 JOIN adr6 ON usr21~persnumber = adr6~persnumber
     WHERE usr21~bname = uname
      ORDER BY adr6~date_from DESCENDING.
    ENDSELECT.
  ENDMETHOD.
  METHOD validar_email.
    " TODO: parameter EMAIL_NORMALIZADO is never cleared or assigned (ABAP cleaner)

    TYPES: sx_addr_type TYPE sx_addrtyp, " R/3 Addresstype
           sx_addr      TYPE so_rec_ext. " Address in plain string

    TYPES: BEGIN OF sx_address,           " SAPconnect general addr
             type    TYPE sx_addr_type,
             address TYPE sx_addr,
           END OF sx_address.

    CONSTANTS cx_t_int TYPE sx_addr_type VALUE 'INT'. " SMTP address

    DATA ls_addr_unst      TYPE sx_address.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_address_normal TYPE sx_address.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_local          TYPE sx_addr.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_domain         TYPE sx_addr.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_comment        TYPE sx_addr.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA ls_addr           TYPE sx_address.

    ls_addr_unst-type    = cx_t_int.
    ls_addr_unst-address = email.

    CALL FUNCTION 'SX_INTERNET_ADDRESS_TO_NORMAL'
      EXPORTING
        address_unstruct     = ls_addr_unst
      IMPORTING
        address_normal       = lv_address_normal
        local                = lv_local
        domain               = lv_domain
        comment              = lv_comment
        addr_normal_no_upper = ls_addr
      EXCEPTIONS
        error_address_type   = 1
        error_address        = 2
        error_group_address  = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO message.
    ENDIF.
  ENDMETHOD.
