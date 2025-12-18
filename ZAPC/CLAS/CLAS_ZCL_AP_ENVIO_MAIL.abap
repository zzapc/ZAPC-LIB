class ZCL_AP_ENVIO_MAIL definition
  public
  create public .

public section.

  data I_TEXTO_MAIL type SRM_T_SOLISTI1 .
  data I_DESTINATARIOS type SWFTLISTI1 .
  data O_SEND_REQUEST type ref to CL_BCS .
  data O_DOCUMENT type ref to CL_DOCUMENT_BCS .
  data I_ADJUNTOS type RMPS_T_POST_CONTENT .
  data MESSAGE type BAPI_MSG .
  data PLANTILLA type ZAP_TEXTOS_MAIL .
  data MSG_PDF type STRING .
  data MENSAJE_YA_MOSTRADO type ABAP_BOOL .
  data ERROR_CRITICO type ABAP_BOOL .

  class-methods ALINK_2_ADJUNTO
    importing
      value(ARCHIV_ID) type SAEARCHIVI
      value(ARC_DOC_ID) type SAEARDOID
    exporting
      value(ADJUNTO) type RMPS_POST_CONTENT
    returning
      value(RET) type BAPIRET2 .
  methods CONSTRUCTOR
    importing
      !USAR_CLASES type ABAP_BOOL default 'X' .
  methods ENVIO_MAIL
    importing
      !SUBJECT type ANY
      !DIRECCION type ANY optional
      !URGENTE type ABAP_BOOL default 'X'
      !HTML type ABAP_BOOL default ''
      !COMMIT type ABAP_BOOL default 'X'
      !FORZAR_MAIL_EXTERNO type ABAP_BOOL default ''
      !CONFIRMACION_LECTURA type BCS_RQST default 'E'
      !EMISOR type ANY default ''
      !LISTA_DISTRIBUCION type ABAP_BOOL default ''
      !DEST_COPIA type ANY default ''
      !DEST_COPIA_OCULTA type ANY default ''
      !MOSTRAR_INFO_ERROR type ANY default 'X'
      !CONTROLAR_SALIDA type ABAP_BOOL default ''
      !PARAR_EN_ERROR type ABAP_BOOL default 'X'
      !CLAVE type ANY default ''
      !CPROG type ANY default ''
      !POPUP type ABAP_BOOL default ''
      !MOSTRAR_CCO type ABAP_BOOL default ''
      !NO_SI_ENVIO_PREVIO type ABAP_BOOL default ''
      !OUTLOOK type ABAP_BOOL default ''
    returning
      value(ERROR) type ABAP_BOOL .
  methods SET_TEXT
    importing
      !TEXTO type ANY optional
      !TEXTO2 type ANY optional
      !TEXTO3 type ANY optional
      !TEXTO4 type ANY optional
      !TABLA type SRM_T_SOLISTI1 optional
    preferred parameter TEXTO .
  methods INI_TEXTOS .
  methods CABECERA_HTML
    importing
      !CHARSET type STRING default 'Windows-1252' .
  methods INICIO_TABLA_HTML
    importing
      !LONGITUD type ANY default '800'
      !C1 type ANY
      !C2 type ANY optional
      !C3 type ANY optional
      !C4 type ANY optional
      !C5 type ANY optional
      !C6 type ANY optional
      !C7 type ANY optional
      !C8 type ANY optional
      !C9 type ANY optional
      !C10 type ANY optional
      !C11 type ANY optional
      !C12 type ANY optional
      !C13 type ANY optional
      !C14 type ANY optional
      !BORDER type I default 0
      !C15 type ANY optional
      !C16 type ANY optional
      !C17 type ANY optional
      !C18 type ANY optional
      !COLOR type ANY default '#6699FF' .
  methods ADD_FILA_HTML
    importing
      !COLOR type ANY optional
      !C1 type ANY
      !C2 type ANY optional
      !C3 type ANY optional
      !C4 type ANY optional
      !C5 type ANY optional
      !C6 type ANY optional
      !C7 type ANY optional
      !C8 type ANY optional
      !C9 type ANY optional
      !C10 type ANY optional
      !C11 type ANY optional
      !C12 type ANY optional
      !C13 type ANY optional
      !C14 type ANY optional
      !ALIGN_C1 type ANY optional
      !ALIGN_C2 type ANY optional
      !ALIGN_C3 type ANY optional
      !ALIGN_C4 type ANY optional
      !ALIGN_C5 type ANY optional
      !ALIGN_C6 type ANY optional
      !ALIGN_C7 type ANY optional
      !ALIGN_C8 type ANY optional
      !ALIGN_C9 type ANY optional
      !ALIGN_C10 type ANY optional
      !ALIGN_C11 type ANY optional
      !ALIGN_C12 type ANY optional
      !ALIGN_C13 type ANY optional
      !ALIGN_C14 type ANY optional
      !C15 type ANY optional
      !ALIGN_C15 type ANY optional
      !C16 type ANY optional
      !ALIGN_C16 type ANY optional
      !C17 type ANY optional
      !ALIGN_C17 type ANY optional
      !C18 type ANY optional
      !ALIGN_C18 type ANY optional .
  methods FIN_TABLA_HTML .
  methods ADD_PARRAFO_HTML
    importing
      !TEXTO type ANY
      !PARTIR_SIEMPRE type ABAP_BOOL default '' .
  methods ADD_DESTINATARIO
    importing
      !DESTINATARIO type ANY
      !URGENTE type ABAP_BOOL default 'X'
      !FORZAR_MAIL_EXTERNO type ABAP_BOOL default ''
      !LISTA_DISTRIBUCION type ABAP_BOOL default ''
      !COPIA type ABAP_BOOL default ''
      !COPIA_OCULTA type ABAP_BOOL default ''
      !PARAR_EN_ERROR type ABAP_BOOL default ''
    returning
      value(MESSAGE) type BAPI_MSG .
  methods SET_TEXT_AS_STRING
    importing
      !TEXTO type ANY optional
    preferred parameter TEXTO .
  methods FREE .
  class-methods CONSULTA_MAILS_ENVIADOS
    importing
      !FECHA type DATUM optional
      !FECHA_HASTA type DATUM optional
      !R_FECHA type SXDATRNGT
      !HORA type UZEIT optional
      !HORA_HASTA type UZEIT optional
    returning
      value(I_SNDRECS) type SOXSP2TAB .
  class-methods GET_DIR_ENVIO_FROM_ADRNR
    importing
      !ADRNR type ADRNR
    returning
      value(EMAIL) type STRING .
  class-methods CONSULTA_SOST
    importing
      !FECHA type DATUM optional
      !FECHA_HASTA type DATUM optional
      !R_FECHA type SXDATRNGT
      !HORA type UZEIT optional
      !HORA_HASTA type UZEIT optional
    returning
      value(I_SOST) type ZSOST_T .
  class-methods USR2EMAIL
    importing
      !UNAME type ANY
    returning
      value(EMAIL) type STRING .
  class-methods MAIL
    importing
      !SUBJECT type ANY optional
      !DIRECCION type ANY optional
      !URGENTE type ABAP_BOOL default 'X'
      !HTML type ABAP_BOOL default ''
      !COMMIT type ABAP_BOOL default 'X'
      !FORZAR_MAIL_EXTERNO type ABAP_BOOL default ''
      !CONFIRMACION_LECTURA type BCS_RQST default 'N'
      !EMISOR type ANY default ''
      !TEXTO type ANY optional
      !PDF type FPCONTENT optional
      !NOMBRE_FICHERO type ANY default ''
      !I_TEXTOS type RSPC_T_TEXT optional
      !LISTA_DISTRIBUCION type ABAP_BOOL default ''
      !I_ADJUNTOS type RMPS_T_POST_CONTENT optional
      !ZIP type ABAP_BOOL default ''
      !NOMBRE_FICHERO_TABLA type ANY default ''
      !TABLA_COMO_ALV type ABAP_BOOL default 'X'
      !DEST_COPIA type ANY default ''
      !DEST_COPIA_OCULTA type ANY default ''
      !O_ALV_ORIGEN type ref to ZCL_AP_ALV optional
      !CONVERSION_SAP type ABAP_BOOL default ''
      !PDF2 type FPCONTENT optional
      !NOMBRE_FICHERO2 type ANY default ''
      !TABLA_COMO_TXT type ANY default ''
      !CONTROLAR_SALIDA type ABAP_BOOL default ''
      !PARAR_EN_ERROR type ABAP_BOOL default 'X'
      !XSTRING type XSTRING optional
      !GRUPO type ANY default ''
      !CODIGO type ANY default ''
      !VERSION type ANY default '88'
      !SPRAS type SY-LANGU default SY-LANGU
      !CLAVE type ANY default ''
      !VARIABLES type APB_LPD_T_KEY_VALUE optional
      !CPROG type ANY default ''
      !POPUP type ABAP_BOOL default ''
      !PLANTILLA type ZAP_TEXTOS_MAIL optional
      !MOSTRAR_CCO type ABAP_BOOL default ''
      !NO_SI_ENVIO_PREVIO type ABAP_BOOL default ''
      !O_GRID_ORIGEN type ref to ZCL_AP_ALV_GRID optional
      !I_ADJ_BIN type ZCL_AP_GOS=>TT_ADJ optional
      !OUTLOOK type ABAP_BOOL default ''
    exporting
      !MESSAGE type BAPI_MSG
      !MENSAJE_YA_MOSTRADO type ABAP_BOOL
    changing
      !I_TABLA type TABLE optional .
  class-methods NOMBRE_USUARIO
    importing
      !UNAME type ANY
    returning
      value(NOMBRE) type STRING .
  methods SET_TEXTO_ESTANDAR_HTML
    importing
      !ID type ANY
      !NAME type ANY
      !SPRAS type STXH-TDSPRAS optional
      !OBJECT type ANY .
  methods ADD_ADJUNTO
    importing
      !TIPO type SOODK-OBJTP default 'BIN'
      !TITULO type ANY default ''
      !LONGITUD type ANY optional
      !BINARIO type SOLIX_TAB optional
      !HEADER type SOLI_TAB optional
      !XSTRING type XSTRING optional
      !STRING type STRING optional
      !FORMATO type ANY default ''
    returning
      value(ADJ) type RMPS_POST_CONTENT .
  methods ADD_OTF
    importing
      !I_OTFDATA type TT_ITCOO
      !TITULO type ANY .
  methods ADD_FICHERO_SERV
    importing
      !FICHERO type ANY
      !LEGACY type ABAP_BOOL default ''
      !MODO_TEXTO type ABAP_BOOL default 'X'
      !CODEPAGE type ABAP_ENCODING default ''
    exporting
      !MENSAJE type BAPI_MSG .
  methods ADD_ITAB_AS_XLS
    importing
      !ITAB type DATA
      !DESCRIPCION type SO_OBJ_DES
      !TIPO type ANY default 'XLS'
      !ZIP type ABAP_BOOL default '' .
  methods ADD_ITAB_ALV_AS_XLS
    importing
      !DESCRIPCION type ANY
      !QUITAR_CARACTERES_EXTRANOS type ABAP_BOOL default ''
      !O_ALV_ORIGEN type ref to ZCL_AP_ALV optional
      !ZIP type ABAP_BOOL default ''
      !TIPO type ANY default 'XLS'
      !CONVERSION_SAP type ABAP_BOOL default ''
      !O_GRID_ORIGEN type ref to ZCL_AP_ALV_GRID optional
    changing
      !ITAB type TABLE .
  methods ADD_FICHERO_LOCAL
    importing
      !FICHERO type ANY optional
      !TIPO type ANY default 'EXT'
      !COMO_TEXTO type ANY default ''
    preferred parameter FICHERO .
  methods ADD_PDF
    importing
      !PDF type FPCONTENT
      !TITULO type ANY .
  methods ADD_ADJUNTO_ZIP
    importing
      !TITULO type ANY default ''
      !XSTRING type XSTRING optional
      !STRING type STRING optional
      !TABLA type TABLE optional
      !REEMPLAZAR_EXTENSION type ABAP_BOOL default 'X' .
  methods ADD_URL_HTML
    importing
      !TEXTO type ANY default ''
      !URL type ANY optional
      !PARRAFO type ABAP_BOOL default ''
      !CODIGO_PARRAFO type STRING default 'P'
    preferred parameter TEXTO .
  class-methods INFO_ENVIO_MAIL
    importing
      !SUBJECT type ANY
      !ADD_ICONO type ABAP_BOOL default 'X'
      !MAIL type ANY default ''
      !ADD_DESTINO type ABAP_BOOL default ''
    exporting
      !STATUS type SOES-STATUS
      !CRDAT type SOOD-CRDAT
      !CRTIM type SOOD-CRTIM
      !SNDNAM type SOOS-SNDNAM
      !MESSAGE type ANY
      !TYPE type ANY
      !MSGV1 type SOES-MSGV1 .
  methods GET_PLANTILLA
    importing
      !GRUPO type ANY
      !CODIGO type ANY
      !VERSION type ANY default '88'
      !SPRAS type SY-LANGU default SY-LANGU
      !VARIABLES type APB_LPD_T_KEY_VALUE optional
    returning
      value(PLANTILLA) type ZAP_TEXTOS_MAIL .
  class-methods VALIDAR_EMAIL
    importing
      !EMAIL type ANY
    exporting
      !EMAIL_NORMALIZADO type STRING
    returning
      value(MESSAGE) type BAPI_MSG .
  methods OUTLOOK
    importing
      !DIRECCION type ANY
      !SUBJECT type ANY
      !DEST_COPIA type ANY default ''
      !DEST_COPIA_OCULTA type ANY default ''
      !I_TEXTOS type RSPC_T_TEXT optional
      !I_ADJUNTOS type RMPS_T_POST_CONTENT optional
      !TEXTO type ANY default '' .
  class-methods MOD_SUBJECT_CLIENTE_ENTORNO
    changing
      !SUBJECT type ANY .
protected section.
private section.

  data L_TEXTO type SOLISTI1 .
  data USAR_CLASES type ABAP_BOOL .
  data O_SENDER type ref to IF_SENDER_BCS .
  data O_RECIPIENT type ref to IF_RECIPIENT_BCS .
  data DESTINO type STRING .
endclass. "ZCL_AP_ENVIO_MAIL definition
class ZCL_AP_ENVIO_MAIL implementation.
METHOD add_adjunto.
  DATA: l_adjunto TYPE rmps_post_content,
        l_size    TYPE so_obj_len.

  CLEAR l_adjunto.
  IF tipo IS INITIAL.
    l_adjunto-doc_type = zcl_ap_ficheros=>get_extension( titulo ).
    TRANSLATE l_adjunto-doc_type TO UPPER CASE.
  ELSE.
    l_adjunto-doc_type = tipo.
  ENDIF.
  IF l_adjunto-doc_type IS INITIAL.
    l_adjunto-doc_type = 'BIN'.
  ENDIF.

  l_adjunto-docsize  = longitud.

  IF ( l_adjunto-doc_type = 'XLS' OR l_adjunto-doc_type = 'SAP' )
     AND NOT string IS INITIAL.
    l_adjunto-binary   = 'X'.
    TRY.
        cl_bcs_convert=>string_to_solix(
          EXPORTING
            iv_string   = string
            iv_codepage = '4103'  "suitable for MS Excel, leave empty
            iv_add_bom  = 'X'     "for other doc types
          IMPORTING
            et_solix  = l_adjunto-cont_hex
            ev_size   = l_size ).
      CATCH cx_root.
        MESSAGE 'Error adjuntando Excel'(eax) TYPE 'E'.
    ENDTRY.
    l_adjunto-docsize = l_size.
  ELSEIF l_adjunto-doc_type = 'BIN' OR l_adjunto-doc_type = 'XLS' OR l_adjunto-doc_type = 'PDF' OR l_adjunto-doc_type = 'PNG'
      OR l_adjunto-doc_type = 'JPG' OR l_adjunto-doc_type = 'GIF' OR l_adjunto-doc_type = 'EXE' OR l_adjunto-doc_type = 'DOC'
      OR l_adjunto-doc_type = 'CSV'.
    l_adjunto-binary   = 'X'.
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
    cl_bcs_convert=>xstring_to_solix(
      EXPORTING
        iv_xstring   = xstring
      RECEIVING
        et_solix  = l_adjunto-cont_hex ).
  ELSEIF ( l_adjunto-doc_type = 'TXT' OR formato = 'TXT' ) AND NOT string IS INITIAL.
    l_adjunto-binary   = 'X'.
    TRY.
        cl_bcs_convert=>string_to_solix(
          EXPORTING
            iv_string   = string
            iv_codepage = '4103'  "suitable for MS Excel, leave empty
            iv_add_bom  = 'X'     "for other doc types
          IMPORTING
            et_solix  = l_adjunto-cont_hex
            ev_size   = l_size ).
      CATCH cx_root.
        MESSAGE 'Error adjuntando fichero de texto'(eat) TYPE 'E'.
    ENDTRY.
  ELSE.
    IF l_adjunto-doc_type = 'BTX'.
      l_adjunto-doc_type = 'BIN'.
    ENDIF.
    l_adjunto-cont_text = header.
  ENDIF.
  l_adjunto-subject  = titulo.

*  IF l_adjunto-doc_type = 'XLS'.
*    l_adjunto-doc_type = 'BIN'.
*  ENDIF.

  APPEND l_adjunto TO i_adjuntos.

  adj = l_adjunto.

ENDMETHOD.
METHOD add_adjunto_zip.
    DATA: l_string         TYPE string,
          l_xstring        TYPE xstring,
          l_nombre_fichero TYPE string,
          o_zip            TYPE REF TO cl_abap_zip.

    IF NOT xstring IS INITIAL.
      l_xstring = xstring.
    ELSEIF NOT string IS INITIAL.
      l_xstring = zcl_ap_string=>string2xstring( string ).
    ELSEIF NOT tabla IS INITIAL.
      l_string = zcl_ap_string=>tabla2string( tabla ).
      l_xstring = zcl_ap_string=>string2xstring( l_string ).
    ENDIF.

    CREATE OBJECT o_zip.
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
  DATA: l_dest         TYPE solisti1,
        l_destinatario TYPE string,
        l_usr21        TYPE usr21,
        i_dest         TYPE TABLE OF string,
        l_desti        TYPE so_obj_nam,
        l_long         TYPE i,
        l_lm.

  IF copia_oculta IS INITIAL.
    __add_lista destino destinatario.
  ENDIF.

  l_destinatario = destinatario.
  REPLACE ALL OCCURRENCES OF ';' IN l_destinatario WITH ','. "Permitimos ',' y ';' como separadores
  SPLIT l_destinatario AT ',' INTO TABLE i_dest.

  LOOP AT i_dest INTO l_destinatario.
    CONDENSE l_destinatario NO-GAPS.
    IF forzar_mail_externo = 'X' AND
       lista_distribucion IS INITIAL AND
       l_destinatario(1) NE '$' AND
      ( zcl_c=>salida_mail_desarrollo = 'X' OR
        zcl_c=>entorno_produccion = sy-sysid ).
      TRANSLATE l_destinatario TO UPPER CASE.
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
        DATA l_email TYPE adr6-smtp_addr.
        l_email = l_destinatario.
        TRY.
            o_recipient = cl_cam_address_bcs=>create_internet_address( l_email ).
          CATCH cx_address_bcs .
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
              CALL METHOD cl_distributionlist_bcs=>getu_persistent
                EXPORTING
                  i_private = ' '
                  i_dliname = l_desti
                RECEIVING
                  result    = o_recipient.
            CATCH cx_address_bcs.
              IF parar_en_error = 'X'.
                MESSAGE e398(00) WITH 'No existe lista dist.'(nel) l_desti '' ''.
              ELSE.
                CONCATENATE 'No existe lista dist.'(nel) l_desti INTO message SEPARATED BY space.
              ENDIF.
          ENDTRY.
        ELSEIF l_destinatario CO '0123456789 +' AND l_long >= 9.
          DATA l_number TYPE ad_pagnmbr.
*   Configuracion del receptor del SMS
          l_number = l_destinatario.
          TRY.
              o_recipient = cl_cam_address_bcs=>create_sms_address( i_service = 'SMS'
                                                                i_number  = l_number ).
            CATCH cx_address_bcs .
              IF parar_en_error = 'X'.
                MESSAGE e398(00) WITH 'Error en numero destino SMS'(esm) l_number '' ''.
              ELSE.
                CONCATENATE 'Error en numero destino SMS'(esm) l_number INTO message SEPARATED BY space.
              ENDIF.
          ENDTRY.
        ELSE.
          DATA l_uname TYPE uname.
          l_uname = l_destinatario.
          TRANSLATE l_uname TO UPPER CASE.
          TRY.
              o_recipient = cl_sapuser_bcs=>create( l_uname ).
            CATCH cx_address_bcs .
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
        CATCH cx_send_req_bcs .
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
        l_string  TYPE string,
        l_xstring TYPE xstring,
*        i_tabla TYPE STANDARD TABLE OF x255 WITH NON-UNIQUE DEFAULT KEY,
        i_tabla   TYPE TABLE OF solix,
        l_long    TYPE i,
        l_linea   TYPE string,
        l_adjunto TYPE rmps_post_content,
        i_ttext   TYPE soli_tab.

  l_fichero = fichero.

  IF zcl_ap_ficheros=>existe( l_fichero ).


    CALL METHOD cl_gui_frontend_services=>gui_upload
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
        OTHERS                  = 19.

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
  ENDIF.

ENDMETHOD.
METHOD add_fichero_serv.
    TYPES: t_linea(65535).
    DATA: i_att_cont2 TYPE TABLE OF t_linea,
          l_string    TYPE string,
          l_linea     TYPE string,
          l_adjunto   TYPE rmps_post_content,
          i_ttext     TYPE soli_tab.

    CLEAR l_adjunto.
    zcl_ap_ficheros=>lee_fich_servidor( EXPORTING fichero = fichero
                                                  modo_texto = modo_texto
                                                  legacy  = legacy
                                                  codepage = codepage
                                        IMPORTING longitud = l_adjunto-docsize
                                                  mensaje  = mensaje
                                        CHANGING  tabla   = i_att_cont2 ).

    IF mensaje IS INITIAL.
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

      l_adjunto-doc_type = 'EXT'.
      l_adjunto-binary   = ''.
      l_adjunto-subject  = fichero.
      l_adjunto-cont_text = i_ttext.
*  IF legacy = 'X'.
*    l_adjunto-docsize = l_adjunto-docsize * 4.
*  ENDIF.
      APPEND l_adjunto TO i_adjuntos.
    ENDIF.

  ENDMETHOD.
METHOD add_fila_html.
  DATA: l_texto TYPE solisti1,
        l_td    TYPE string,
        l_tipo.

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
  DATA: l_adjunto   TYPE rmps_post_content,
        o_alv       TYPE REF TO zcl_ap_alv,
        x_excelx    TYPE solix_tab,
        l_string    TYPE string,
        l_long      TYPE i,
        l_extension TYPE string,
        l_fichero   TYPE string.

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
      CREATE OBJECT o_alv
        EXPORTING
          tabla = ''.

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

    DATA: lv_xml_type TYPE salv_bs_constant,
          lv_xml      TYPE xstring.

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
  DATA: lv_string       TYPE string,
        wa_field        TYPE string,
        lv_is_dd_object TYPE boolean,
        lv_dd_header    TYPE x030l,
        lv_dd_object    TYPE dd_x031l_table,
        lt_filetable    TYPE solix_tab,
        lr_data         TYPE REF TO data,
        lr_structdescr  TYPE REF TO cl_abap_structdescr,
        lr_tabledescr   TYPE REF TO cl_abap_datadescr,
        lx_comp         TYPE abap_component_tab,
        x_excelx        TYPE solix_tab,
        l_adjunto       TYPE rmps_post_content,
        o_typedes       TYPE REF TO cl_abap_typedescr,
        l_long          TYPE i,
        l_extension     TYPE string,
        l_fichero       TYPE string.

  IF tipo = 'XLSX'.
    DATA(o_xls) = NEW zcl_ap_abap2xls( ).
    o_xls->set_tabla( tabla = itab ).
    o_xls->add_adj_mail( o_mail = me fichero = descripcion add_ext = 'X' ).
    CLEAR o_xls.
  ELSE.
    TYPE-POOLS: truxs.

    l_fichero = descripcion.
    l_extension = zcl_ap_ficheros=>get_extension( l_fichero ).
    IF l_extension IS INITIAL.
      CONCATENATE l_fichero '.' tipo INTO l_fichero.
    ENDIF.


    FIELD-SYMBOLS:
      <dyn_wa>    TYPE any,
      <fs_field>  TYPE any,
      <fs_table>  TYPE ANY TABLE,
      <fs_dd_tab> TYPE x031l,
      <fs_comp>   TYPE abap_componentdescr.



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
      lv_dd_header = lr_structdescr->get_ddic_header( ).
* Retrieves DD info of table/structure
      lv_dd_object = lr_structdescr->get_ddic_object( ).

* Move results to table LX_COMP for ease of use below.
      LOOP AT lv_dd_object ASSIGNING <fs_dd_tab>.
        APPEND INITIAL LINE TO lx_comp ASSIGNING <fs_comp>.
        MOVE <fs_dd_tab>-fieldname TO <fs_comp>-name.
      ENDLOOP.

* If not a DD object, use GET_COMPONENTS.
* Does not fully work for DD objects, because does not
* handle embedded include's.
    ELSE.
      lx_comp = lr_structdescr->get_components( ).
    ENDIF.

* Now we're ready to move the contents from the imported
* table into a string for converting to xls attachment
    IF NOT <fs_table>[] IS INITIAL.

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
          IF NOT <fs_comp> IS INITIAL.
            ASSIGN COMPONENT <fs_comp>-name OF STRUCTURE <dyn_wa> TO <fs_field>.
            IF <fs_field> IS ASSIGNED.
              DESCRIBE FIELD <fs_field> TYPE DATA(l_tipo).
              IF l_tipo = 'h'. "Tabla interna.

              ELSE.
                TRY.
                    MOVE <fs_field> TO wa_field.
                    CONCATENATE lv_string wa_field cl_abap_char_utilities=>horizontal_tab INTO lv_string.
                  CATCH cx_root INTO DATA(o_root).
                ENDTRY.
              ENDIF.
            ENDIF.
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
    DATA: l_long_pdf TYPE i,
          i_pdf      TYPE solix_tab.

    zcl_ap_smartforms=>get_pdf_from_otfdata( EXPORTING i_otfdata = i_otfdata
                                          IMPORTING i_pdf = i_pdf
                                                    longitud_pdf = l_long_pdf ).

    add_adjunto( titulo = titulo
                 longitud = l_long_pdf
                 binario = i_pdf ).

  ENDMETHOD.
METHOD add_parrafo_html.
    DATA: l_long   TYPE i,
          i_lineas TYPE TABLE OF text255,
          l_linea  TYPE text255,
          l_string TYPE string.

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
  METHOD alink_2_adjunto.

*PARAMETERS
*
*ARCHIV_ID  Importing Type  SAEARCHIVI
*ARC_DOC_ID Importing Type  SAEARDOID
*ADJUNTO  Exporting Type  RMPS_POST_CONTENT
*RET  Returning Type  BAPIRET2

    DATA: l_adjunto     TYPE rmps_post_content,
          l_filename    TYPE toaat-filename,
          l_filecontent TYPE xstring,
          l_message     TYPE bapi_msg.

    l_filecontent = zcl_ap_gd=>get_xstring_alink(
      EXPORTING archive  = archiv_id
                doc_id   = arc_doc_id
      IMPORTING filename = l_filename
                message  = l_message ).

    CLEAR: l_adjunto.
    l_adjunto-doc_type = zcl_ap_ficheros=>get_extension( l_filename ).
    TRANSLATE l_adjunto-doc_type TO UPPER CASE.

    l_adjunto-binary   = 'X'.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer          = l_filecontent
        append_to_table = ' '
      IMPORTING
        output_length   = l_adjunto-docsize
      TABLES
        binary_tab      = l_adjunto-cont_hex.

    l_adjunto-subject  = l_filename.

    adjunto = l_adjunto.
    ret-type = 'S'.

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
        CATCH cx_root.
      ENDTRY.
    ENDIF.

  ENDMETHOD.
METHOD consulta_mails_enviados.
    DATA: i_snd_date TYPE  sxdatrngt,
          l_snd_date TYPE sxdatrngl,
          i_snd_time TYPE  sxtimrngt,
          l_snd_time TYPE sxtimrngl,
          l_status   TYPE  soststatus,
          l_sndrec   TYPE soxsp2.

    IF NOT r_fecha IS INITIAL.
      i_snd_date = r_fecha.
    ELSE.
      IF NOT fecha IS INITIAL.
        CLEAR l_snd_date.
        l_snd_date-sign   = 'I'.
        l_snd_date-low    = fecha.

        IF fecha_hasta IS INITIAL.
          l_snd_date-option = 'EQ'.
        ELSE.
          l_snd_date-option = 'BT'.
          l_snd_date-high    = fecha_hasta.
        ENDIF.

        APPEND l_snd_date TO i_snd_date.
      ENDIF.
    ENDIF.

    IF NOT hora IS INITIAL.
      CLEAR l_snd_time.
      l_snd_time-sign   = 'I'.
      l_snd_time-low    = hora.

      IF hora_hasta IS INITIAL.
        l_snd_time-option = 'EQ'.
      ELSE.
        l_snd_time-option = 'BT'.
        l_snd_time-high    = hora_hasta.
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
        l_sost-status = '@5C@'.
      ELSE.
        l_sost-status = '@5B@'.
      ENDIF.

      CASE l_sndrec-sndart.
        WHEN 'INT'.
          l_sost-forma_envio = 'Email'.
        WHEN 'PAG'.
          l_sost-forma_envio = 'SMS'.
        WHEN OTHERS.
          l_sost-forma_envio =  l_sndrec-sndart.
      ENDCASE.

      l_sost-titulo = l_sndrec-titel.
      l_sost-emisor = l_sndrec-usernam.
      l_sost-destinatario = get_dir_envio_from_adrnr( l_sndrec-adrnr ).
      l_sost-fecha_envio = l_sndrec-stat_date.
      l_sost-hora_envio = l_sndrec-stat_time.

      APPEND l_sost TO i_sost.

    ENDLOOP.

  ENDMETHOD.
METHOD envio_mail.
  DATA: l_adjunto      TYPE rmps_post_content,
        i_recipients   TYPE bcsy_re,
        l_destinatario TYPE solisti1,
        l_smtp_addr    TYPE adr6-smtp_addr,
        l_uname        TYPE uname,
        l_result       TYPE os_boolean,
        l_fecha        TYPE dats,
        l_hora         TYPE uzeit,
        l_funcname     TYPE tfdir-funcname,
        lt_att_head    TYPE soli_tab,
        lv_text_line   TYPE soli.

  GET TIME.
  l_fecha = sy-datum.
  l_hora  = sy-uzeit.

  CLEAR: error, error_critico.

  SET PARAMETER ID 'ZMAIL' FIELD 'X'.                       "#EC *
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
*         DOC_ID              =
          sender              = emisor
          commit              = commit
*         SMS                 = ''
*   IMPORTING
*         RETURNCODE          =
        TABLES
          texto               = i_texto_mail
*         T_FICHEROS          =
          i_destinatarios     = i_destinatarios.
    ELSE.
      MESSAGE 'No existe funcion Z_ENVIO_MAIL'(nef) TYPE 'E'.
    ENDIF.
  ELSE.
    DATA: l_mailtext TYPE soli_tab,
          l_mailhex  TYPE solix_tab,
          l_title    TYPE sood-objdes,
          l_tipo(3),
          l_subject  TYPE string.

    IF html IS INITIAL.
      l_tipo = 'RAW'.
    ELSE.
      l_tipo = 'HTM'.
    ENDIF.

    l_title = subject.

    mod_subject_cliente_entorno( CHANGING subject = l_title ).
    l_mailtext = i_texto_mail.

    TRY.
        CALL METHOD cl_document_bcs=>create_document
          EXPORTING
            i_type    = l_tipo
            i_subject = l_title
*           i_length  =
*           i_language    = SPACE
*           i_importance  =
*           i_sensitivity =
            i_text    = l_mailtext
*           i_hex     =
*           i_header  =
*           i_sender  =
          RECEIVING
            result    = o_document.
      CATCH cx_document_bcs .
        message = 'Error creando mail'(ecm).
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
          CALL METHOD o_send_request->set_message_subject
            EXPORTING
              ip_subject = l_subject.
        CATCH cx_send_req_bcs .
          __concat2 message 'Error definiendo asunto:'(eda) l_subject.
          IF mostrar_info_error = 'X'.
            MESSAGE message TYPE 'I'.
          ENDIF.
      ENDTRY.


      IF emisor IS INITIAL.
        TRY.
            CALL METHOD cl_sapuser_bcs=>create
              EXPORTING
                i_user = sy-uname
              RECEIVING
                result = o_sender.
          CATCH cx_address_bcs .
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
              o_sender =  cl_cam_address_bcs=>create_internet_address( l_smtp_addr ).
            CATCH cx_root.
              message = 'Error al definir email'(ede).
              IF mostrar_info_error = 'X'.
                MESSAGE message TYPE 'I'.
              ENDIF.
          ENDTRY.

        ELSE.
          l_uname = emisor.
          TRY.
              o_sender = cl_sapuser_bcs=>create( l_uname ).
            CATCH cx_root.
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
        CATCH cx_root.
          message = 'Error al añadir emisor'(eae).
          IF mostrar_info_error = 'X'.
            MESSAGE message TYPE 'I'.
          ENDIF.
      ENDTRY.
      IF urgente = 'X'.
        TRY.
            o_send_request->set_send_immediately( abap_true ).
          CATCH cx_root.
            message = 'Error en envío inmediato'(eei).
            IF mostrar_info_error = 'X'.
              MESSAGE message TYPE 'I'.
            ENDIF.
        ENDTRY.

        TRY.
            o_send_request->set_priority( '1' ).
          CATCH cx_root.
            message = 'Error definiendo prioridad'(edp).
            IF mostrar_info_error = 'X'.
              MESSAGE message TYPE 'I'.
            ENDIF.
        ENDTRY.
      ENDIF.

      TRY.
          o_send_request->set_status_attributes( i_requested_status = confirmacion_lectura ).
        CATCH cx_root.
          message = 'Error definiendo confirmacion de lectura'(ecl).
          IF mostrar_info_error = 'X'.
            MESSAGE message TYPE 'I'.
          ENDIF.
      ENDTRY.

      DATA: o_document_bcs TYPE REF TO cx_document_bcs,
            o_send_req_bcs TYPE REF TO cx_send_req_bcs,
            l_long         TYPE sood-objlen,
            l_titulo       TYPE sood-objdes,
            l_document     TYPE REF TO cl_document_bcs,
            l_error        TYPE string.

      LOOP AT i_adjuntos INTO l_adjunto.
        TRY.
            l_titulo = l_adjunto-subject.
            l_long = l_adjunto-docsize.

            ASSIGN ('ZCL_C=>MAIL_LIMITE_TAMANYO_ADJUNTOS') TO FIELD-SYMBOL(<limite>).
            IF sy-subrc = 0.
              IF <limite> > 0.
                DATA: l_megas        TYPE mengv13,
                      l_total_megas  TYPE mengv13,
                      l_limite_megas TYPE  mengv13.

                l_limite_megas = <limite>.
                l_megas = l_adjunto-docsize / 1048576.
                ADD l_megas TO l_total_megas.
                IF l_megas > <limite>.
                  DATA(l_aviso_megas) = 'X'.
                  message = |El tamaño en megas del fichero { l_titulo } es { l_megas }, superior al máximo permitido { <limite> }|.
                  error_critico = 'X'.
                  IF mostrar_info_error = 'X'.
                    mensaje_ya_mostrado = 'X'.
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

            o_document->add_attachment( EXPORTING i_attachment_type = l_adjunto-doc_type
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
              message = `Error adjuntando ` && l_titulo.
            ENDIF.
            IF mostrar_info_error = 'X'.
              MESSAGE message TYPE 'I'.
            ENDIF.
        ENDTRY.
      ENDLOOP.

      IF l_limite_megas > 0 AND l_total_megas > l_limite_megas AND l_aviso_megas IS INITIAL.
        message = |El tamaño en megas del total de adjuntos { l_total_megas } es superior al máximo permitido { <limite> }|.
        error_critico = 'X'.
        IF mostrar_info_error = 'X'.
          mensaje_ya_mostrado = 'X'.
          MESSAGE message TYPE 'I'.
        ENDIF.
      ENDIF.

      TRY.
          o_send_request->set_document( o_document ).
        CATCH cx_root.
          message = 'Error al adjuntar documento'(edo).
          error_critico = 'X'.
          IF mostrar_info_error = 'X'.
            mensaje_ya_mostrado = 'X'.
            MESSAGE message TYPE 'I'.
          ENDIF.
      ENDTRY.

      TRY.
          i_recipients = o_send_request->recipients( ).
        CATCH cx_root.
          message = 'Error al insertar receptores'(ear).
          error_critico = 'X'.
          IF mostrar_info_error = 'X'.
            mensaje_ya_mostrado = 'X'.
            MESSAGE message TYPE 'I'.
          ENDIF.
      ENDTRY.


      IF error_critico = 'X'.
        error = 'X'.
        RETURN.
      ENDIF.

      IF NOT i_recipients IS INITIAL OR popup = 'X'.
        SET UPDATE TASK LOCAL.

        DATA: zap_mail_log TYPE zap_mail_log,
              l_dest       TYPE string,
              l_dest_copia TYPE string,
              l_dest_cco   TYPE string.
        MOVE-CORRESPONDING plantilla TO zap_mail_log.
        zap_mail_log-clave = clave.
        zap_mail_log-fecha = sy-datum.
        zap_mail_log-hora = sy-uzeit.
        zap_mail_log-asunto = subject.
        IF direccion IS INITIAL.
          zap_mail_log-destino = l_dest = me->destino.
        ELSE.
          zap_mail_log-destino = l_dest = direccion.
        ENDIF.
        IF zap_mail_log-destino CS '@'.
          TRANSLATE zap_mail_log-destino TO LOWER CASE.
        ENDIF.
        zap_mail_log-emisor = emisor.
        IF zap_mail_log-emisor IS INITIAL.
          zap_mail_log-emisor = sy-uname.
        ENDIF.
        zap_mail_log-copia = l_dest_copia = dest_copia.
        zap_mail_log-html = html.
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
            SELECT SINGLE clave FROM zap_mail_log
              INTO zap_mail_log-clave
             WHERE clave   = zap_mail_log-clave
               AND destino = zap_mail_log-destino
               AND grupo   = zap_mail_log-grupo
               AND codigo  = zap_mail_log-codigo
               AND asunto  = zap_mail_log-asunto
               AND emisor  = zap_mail_log-emisor
               AND copia   = zap_mail_log-copia
               AND status  = ''
               AND hash    = zap_mail_log-hash.
            IF sy-subrc = 0.
              error = 'X'.
              message = 'Mail ya fue enviado previamente'.
              zap_mail_log-message = message.
              zap_mail_log-status = 'DUP'.
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
            IF sy-subrc NE 0.
              error = 'X'.
              message = 'Se ha cancelado el envío del mail'.
            ENDIF.
          ELSE.
            outlook( subject           = zap_mail_log-asunto
                     direccion         = l_dest
                     dest_copia        = l_dest_copia
                     dest_copia_oculta = l_dest_cco
                     i_textos          = i_texto_mail
                   ).
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
          DATA l_s TYPE string.
          l_s = zap_mail_log-asunto.
          DATA l_e TYPE string.
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
              envio_cancelado    = 1
              OTHERS             = 2.
          RETURN.
        ELSE.
          TRY.
              CALL METHOD o_send_request->send
                EXPORTING
                  i_with_error_screen = space
                RECEIVING
                  result              = l_result.
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
            DATA: l_soes       TYPE soes.
            DO 3 TIMES.
              SELECT  *
                 FROM soos AS f JOIN soes AS g
                       ON f~rectp = g~rectp
                      AND f~recyr = g~recyr
                      AND f~recno = g~recno
                                JOIN sood AS s
                         ON f~objtp = s~objtp
                       AND f~objyr = s~objyr
                       AND f~objno = s~objno
                                JOIN bcst_sr AS e
                         ON f~sndreq = e~os_guid
                INTO CORRESPONDING FIELDS OF l_soes
                UP TO 1 ROWS
                WHERE s~objdes = subject
                AND crdat >= l_fecha
                AND crtim >= l_hora
                AND sndnam = sy-uname
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
  SET PARAMETER ID 'ZMAIL' FIELD ''.                        "#EC *
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
  DATA: l_addrnumber TYPE adcp-addrnumber,
        l_date_from  TYPE adcp-date_from,
        l_nation     TYPE adcp-nation,
        l_persnumber TYPE adrp-persnumber.

  SELECT g~name_text f~addrnumber g~persnumber f~date_from f~nation
         FROM adcp AS f JOIN adrp AS g
         ON  f~persnumber = g~persnumber
         AND f~date_from  = g~date_from
         AND f~nation     = g~nation
   INTO (email, l_addrnumber, l_persnumber, l_date_from, l_nation)
    UP TO 1 ROWS
  WHERE f~so_key = adrnr
    ORDER BY f~addrnumber g~persnumber f~date_from f~nation.
  ENDSELECT.

ENDMETHOD.
METHOD get_plantilla.
  DATA l_texto TYPE string.

  IF version NE '88'.
    SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
     WHERE grupo    = grupo
       AND codigo   = codigo
       AND version  = version
       AND spras    = spras.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
       WHERE grupo    = grupo
         AND codigo   = codigo
         AND version  = version
         AND spras    = ''.
      IF sy-subrc NE 0.
        SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
         WHERE grupo    = grupo
           AND codigo   = codigo
           AND version  = version.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
     WHERE grupo    = grupo
       AND codigo   = codigo
       AND defecto  = 'X'
       AND spras    = spras.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
       WHERE grupo    = grupo
         AND codigo   = codigo
         AND defecto  = 'X'
         AND spras    = ''.
      IF sy-subrc NE 0.
        SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
         WHERE grupo    = grupo
           AND codigo   = codigo
           AND defecto  = 'X'.
      ENDIF.
    ENDIF.
    IF plantilla IS INITIAL.
      SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
       WHERE grupo    = grupo
         AND codigo   = codigo
         AND spras    = spras.
      IF sy-subrc NE 0.
        SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
         WHERE grupo    = grupo
           AND codigo   = codigo
           AND spras    = ''.
        IF sy-subrc NE 0.
          SELECT SINGLE * FROM  zap_textos_mail INTO plantilla
           WHERE grupo    = grupo
             AND codigo   = codigo.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


  IF NOT plantilla-email_pruebas IS INITIAL.
    IF plantilla-email_pruebas = 'SY-UNAME'.
      IF sy-uname = 'WF-BATCH' OR sy-uname = 'SCP'.
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

  IF sy-sysid NE zcl_c=>entorno_produccion.
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
        CATCH cx_root INTO DATA(o_root).
          msg_pdf = o_root->get_text( ).
      ENDTRY.

      IF NOT o_sfp->output IS INITIAL.
        IF plantilla-nombre_adjunto IS INITIAL OR NOT plantilla-binario IS INITIAL.
          l_texto = 'Contenido.pdf'.
        ELSE.
          l_texto = plantilla-nombre_adjunto.
          DATA(l_ext) = zcl_ap_ficheros=>get_extension( plantilla-nombre_adjunto ).
          TRANSLATE l_ext TO LOWER CASE.
          IF l_ext NE 'pdf'.
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
             ON f~rectp = g~rectp
            AND f~recyr = g~recyr
            AND f~recno = g~recno
                      JOIN sood AS s
               ON f~objtp = s~objtp
             AND f~objyr = s~objyr
             AND f~objno = s~objno
                      JOIN bcst_sr AS e
               ON f~sndreq = e~os_guid
      INTO (status, crdat, crtim, sndnam, msgv1)
      UP TO 1 ROWS
      WHERE s~objdes = subject
        AND msgv1 IN r_msgv1
      ORDER BY crdat DESCENDING crtim DESCENDING.
    ENDSELECT.
    IF sy-subrc NE 0.
      type = 'N'.
    ELSE.
      CASE status.
        WHEN '73'.
          type = 'S'.
          l_icono = '@1S@'. "ICON_MAIL
          message = zcl_ap_utils=>concat( p1 = 'Mail enviado el'(mee) p2 = crdat p3 = 'a las'(ala) p4 = crtim p5 = 'por'(por) p6 = sndnam ).
        WHEN '672'.
          type = 'W'.
          l_icono = '@1T@'. "ICON_TIME
          message = zcl_ap_utils=>concat( p1 = 'Mail en espera de envío. Creado el'(mev) p2 = crdat p3 = 'a las'(ala) p4 = crtim p5 = 'por'(por) p6 = sndnam ).
        WHEN '751'.
          type = 'W'.
          l_icono = '@03@'. "FAILURE
          message = zcl_ap_utils=>concat( p1 = 'Mail en espera por ERROR CONEXION. Creado el'(mec) p2 = crdat p3 = 'a las'(ala) p4 = crtim p5 = 'por'(por) p6 = sndnam  ).
        WHEN '850'.
          type = 'E'.
          l_icono = '@5C@'. "ICON_LED_RED
          message = 'Mensaje no enviado porque se ha sobrepasado el tamaño máximo de adjuntos'(mta).
        WHEN OTHERS.
          type = 'E'.
          l_icono = '@5C@'. "ICON_LED_RED
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
    DATA: l_long(10),
          l_border(3),
          l_aux(40).

    l_long = longitud.
    WRITE border TO l_border.
    CONDENSE l_border NO-GAPS.

    CONCATENATE '" border="' l_border '">' INTO l_aux.
    set_text( texto  = '<table width="' texto2 = l_long
              texto3 = l_aux ).
    CONCATENATE '  <tr bgcolor="' color '">' into l_aux.
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
        l_texto          TYPE solisti1,
        l_titulo         TYPE string,
        l_aux(10),
        l_nombre_fichero TYPE so_obj_des,
        l_xstring        TYPE xstring,
        l_tipo           TYPE soodk-objtp,
        l_asunto         TYPE string,
        l_emisor         TYPE string,
        l_direccion      TYPE string,
        l_html           TYPE abap_bool.

  CLEAR: message, mensaje_ya_mostrado.

  CREATE OBJECT o_mail
    EXPORTING
      usar_clases = 'X'.

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
    IF tabla_como_txt NE ''.
      CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
        IMPORTING
          buffer   = l_xstring
        TABLES
          text_tab = i_tabla
        EXCEPTIONS
          failed   = 1
          OTHERS   = 2.
      CALL METHOD o_mail->add_adjunto
        EXPORTING
          tipo    = l_tipo
          titulo  = l_nombre_fichero
          xstring = l_xstring.
    ELSEIF NOT xstring IS INITIAL.
      CALL METHOD o_mail->add_adjunto
        EXPORTING
          tipo    = l_tipo
          titulo  = l_nombre_fichero
          xstring = l_xstring.
    ELSE.
      DESCRIBE TABLE i_tabla LINES sy-tfill.
      IF sy-tfill > 20000. "Si tiene más de 20000 líneas lo más securo es que casque por rendimiento, así que en lugar de enviar la tabla enviamos un mensaje diciendo que no se envía
        l_aux = sy-tfill.
        CONCATENATE 'Tabla demasiado grande ('(tdg) l_aux '). No es posible adjuntar contenido'(npa) INTO l_texto.
        o_mail->set_text( l_texto ).
      ELSE.
        IF tabla_como_alv IS INITIAL.
          o_mail->add_itab_as_xls( descripcion = l_nombre_fichero zip = zip itab = i_tabla[] ).
        ELSE.
          o_mail->add_itab_alv_as_xls( EXPORTING descripcion = l_nombre_fichero zip = zip quitar_caracteres_extranos = 'X' o_alv_origen = o_alv_origen o_grid_origen = o_grid_origen conversion_sap = conversion_sap
                                       CHANGING itab = i_tabla[] ).
        ENDIF.
      ENDIF.
    ENDIF.
  ELSEIF NOT o_grid_origen IS INITIAL.
    o_mail->add_itab_alv_as_xls( EXPORTING descripcion = l_nombre_fichero  zip = zip quitar_caracteres_extranos = 'X' o_grid_origen = o_grid_origen conversion_sap = conversion_sap
                                 CHANGING itab = i_tabla[] ).
  ELSEIF NOT o_alv_origen IS INITIAL.
    o_mail->add_itab_alv_as_xls( EXPORTING descripcion = l_nombre_fichero  zip = zip quitar_caracteres_extranos = 'X' o_alv_origen = o_alv_origen conversion_sap = conversion_sap
                                 CHANGING itab = i_tabla[] ).
  ELSEIF NOT xstring IS INITIAL.
    l_tipo = zcl_ap_ficheros=>get_extension( l_nombre_fichero ).
    CALL METHOD o_mail->add_adjunto
      EXPORTING
        tipo    = l_tipo
        titulo  = l_nombre_fichero
        xstring = xstring.
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
    mensaje_ya_mostrado = o_mail->mensaje_ya_mostrado.
  ENDIF.

  o_mail->free( ).

ENDMETHOD.
  METHOD mod_subject_cliente_entorno.
    DATA: l_prod TYPE abap_bool,
          l_sistema type string.

    ASSIGN ('ZCL_C=>MAIL_INCLUIR_SISTEMA_EN_ASUNTO') TO FIELD-SYMBOL(<sistema>).
    IF sy-subrc NE 0.
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
      CATCH cx_root INTO DATA(o_root).
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
  DATA l_date_from TYPE adrp-date_from.

  SELECT adrp~name_text adrp~date_from
   FROM usr21 JOIN adrp ON usr21~persnumber EQ adrp~persnumber
    UP TO 1 ROWS
    INTO (nombre, l_date_from)
   WHERE usr21~bname = uname
    ORDER BY adrp~date_from DESCENDING.
  ENDSELECT.

ENDMETHOD.
METHOD outlook.
    DATA: g_file(120),
          g_translate(2),
          t_vbs             TYPE STANDARD TABLE OF soli,
          g_last,
          g_vbs_filename    TYPE rlgrap-filename,
          commandline(1000),
          l_vbs             TYPE soli,
          email_body        TYPE TABLE OF string,
          l_email_body      TYPE string,
          l_dir             TYPE string,
          i_text            TYPE TABLE OF string.

*- Prepare a code to translate a quote into a hex-tab
*- so it can then be translated back to 2 double quotes.
    CONCATENATE '"' cl_abap_char_utilities=>cr_lf+1(1) INTO g_translate.

    DATA email_address TYPE string VALUE  'andres@sap4.com'.

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
    APPEND: 'Set myAttachments = myItem.Attachments' TO t_vbs.

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
      TRANSLATE l_email_body USING g_translate.
      WHILE sy-subrc EQ 0.
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
    CALL FUNCTION 'WS_QUERY'
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

    CONCATENATE g_vbs_filename 'mail.vbs' INTO g_vbs_filename.
    commandline = g_vbs_filename.


    DATA: l_string TYPE string.
    l_string = g_vbs_filename.

*- Descargamos el fichero vbscript
    zcl_ap_ficheros=>grabar( EXPORTING fichero = l_string
                          CHANGING tabla = t_vbs[] ).


    CALL FUNCTION 'WS_EXECUTE'
      EXPORTING
        commandline    = commandline
        program        = 'WSCRIPT.EXE'
      EXCEPTIONS
        frontend_error = 1
        no_batch       = 2
        prog_not_found = 3
        illegal_option = 4
        OTHERS         = 5.


    MESSAGE s000(38) WITH 'Abriendo Outlook... Espere por favor'.
    WAIT UP TO 5 SECONDS.

  ENDMETHOD.
METHOD set_text.
  DATA: l_string TYPE string,
        l_texto  TYPE solisti1,
        l_texto2 TYPE solisti1,
        l_tipo   TYPE c,
        l_long   TYPE i,
        l_long2  TYPE i,
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

      IF l_ok is initial.
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
          l_tline      TYPE tline,
          i_tdline     TYPE TABLE OF tdline,
          l_format_ant TYPE tdformat,
          l_first, l_fin,
          l_texto      TYPE solisti1,
          l_texto_ant  TYPE solisti1.

    i_lineas = zcl_ap_textos=>get_texto( id     = id
                                      name   = name
                                      spras  = spras
                                      object = object ).

    IF NOT i_lineas IS INITIAL.
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
      IF l_texto NE '<P>'.
        CONCATENATE l_texto '</P>' INTO l_texto.
        IF strlen( l_texto ) > 250.
          CONCATENATE l_texto_ant '</P>' INTO l_texto_ant.
          set_text( l_texto_ant ).
          CONCATENATE '<P>' l_tline-tdline INTO l_texto.
        ENDIF.
        set_text( l_texto ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD usr2email.
  DATA l_date_from TYPE adr6-date_from.

  SELECT adr6~smtp_addr date_from
    INTO (email, l_date_from)
    UP TO 1 ROWS
   FROM usr21 JOIN adr6 ON usr21~persnumber EQ adr6~persnumber
   WHERE usr21~bname EQ uname
    ORDER BY adr6~date_from DESCENDING.
  ENDSELECT.

ENDMETHOD.
METHOD validar_email.
    TYPES: sx_addr_type TYPE sx_addrtyp, "R/3 Addresstype
           sx_addr      TYPE so_rec_ext . "Address in plain string

    TYPES: BEGIN OF sx_address,           "SAPconnect general addr
             type    TYPE sx_addr_type,
             address TYPE sx_addr,
           END OF sx_address.

    CONSTANTS: cx_t_int    TYPE sx_addr_type VALUE 'INT'.  "SMTP address

    DATA: ls_addr_unst      TYPE sx_address.
    DATA: ls_addr           TYPE sx_address.
    DATA: lv_address_normal TYPE sx_address.
    DATA: lv_local          TYPE sx_addr.
    DATA: lv_domain         TYPE sx_addr.
    DATA: lv_comment        TYPE sx_addr.

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
    IF sy-subrc NE 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          INTO message.
    ENDIF.

  ENDMETHOD.