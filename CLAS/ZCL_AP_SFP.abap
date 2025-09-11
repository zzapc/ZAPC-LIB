CLASS zcl_ap_sfp DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA outputparams TYPE sfpoutputparams.
    DATA docparams    TYPE sfpdocparams.
    DATA formname     TYPE tdsfname.
    DATA funcion      TYPE funcname.
    DATA output       TYPE fpformoutput.
    DATA cancelado    TYPE abap_bool       VALUE '' ##NO_TEXT.
    DATA error        TYPE abap_bool       VALUE '' ##NO_TEXT.

    CONSTANTS c_dest TYPE rfcdest VALUE 'ADS' ##NO_TEXT.

    METHODS constructor
      IMPORTING !form         TYPE any
                nodialog      TYPE fpnodialog      DEFAULT ''
                preview       TYPE fppreview       DEFAULT 'X'
                dest          TYPE rspopname       DEFAULT ''
                copias        TYPE any             DEFAULT 1
                getpdf        TYPE fpgetpdf        DEFAULT ''
                outparams_ini TYPE sfpoutputparams OPTIONAL
                abrir_job     TYPE abap_bool       DEFAULT 'X'
                reqimm        TYPE syprimm         DEFAULT 'X'
                covtitle      TYPE any             DEFAULT ''
                reqnew        TYPE syprnew         DEFAULT '?'
                reqdel        TYPE syprrel         DEFAULT '?'
                langu         TYPE sy-langu        DEFAULT sy-langu.

    METHODS abrir_job.

    CLASS-METHODS get_funcion
      IMPORTING !form          TYPE tdsfname
      RETURNING VALUE(funcion) TYPE funcname.

    METHODS cerrar_job.

    CLASS-METHODS grabar_pdf
      IMPORTING pdf             TYPE fpcontent
                fichero         TYPE any
                mostrar_error   TYPE abap_bool DEFAULT 'X'
                dialogo         TYPE any       DEFAULT ''
      EXPORTING fichero_dialogo TYPE any.

    CLASS-METHODS archivar_pdf
      IMPORTING pdf            TYPE fpcontent
                sap_object     TYPE any
                ar_object      TYPE any
                descripcion    TYPE any
                objectid       TYPE any
      RETURNING VALUE(mensaje) TYPE string.

    CLASS-METHODS enviar_pdf_a_spool
      IMPORTING pdf             TYPE xstring
                dest            TYPE tsp01-rqdest
                !name           TYPE tsp01-rq0name DEFAULT 'PBFORM'
                immediate_print TYPE abap_bool     DEFAULT ''
                titleline       TYPE any           DEFAULT ''
                !extension      TYPE any           DEFAULT 'pdf'
                prio            TYPE tsp01-rqprio  DEFAULT 5
                suffix2         TYPE tsp01-rq2name DEFAULT sy-uname
      RETURNING VALUE(message)  TYPE bapi_msg.

    CLASS-METHODS get_pdf_pages
      IMPORTING pdf            TYPE xstring
      RETURNING VALUE(paginas) TYPE i.

    CLASS-METHODS convertir_pdf_a_ps
      IMPORTING pdf      TYPE xstring
      EXPORTING ps       TYPE xstring
                !message TYPE bapi_msg.

    CLASS-METHODS get_mensaje
      RETURNING VALUE(message) TYPE string.

    CLASS-METHODS recupera_pdf_from_mensaje
      IMPORTING nast          TYPE nast       OPTIONAL
                kappl         TYPE any        OPTIONAL
                objky         TYPE any        OPTIONAL
                kschl         TYPE any        OPTIONAL
                sort1         TYPE nast-sort1 DEFAULT 'ZPDF'
                previsualizar TYPE abap_bool  DEFAULT ''
                memory_id     TYPE any        DEFAULT 'ZMENSAJE_OTFDATA'
                ldest         TYPE rspopname  DEFAULT ''
      RETURNING VALUE(pdf)    TYPE fpcontent.

    CLASS-METHODS recupera_form_from_mensaje
      IMPORTING nast                TYPE nast       OPTIONAL
                kappl               TYPE any        OPTIONAL
                objky               TYPE any        OPTIONAL
                kschl               TYPE any        OPTIONAL
                sort1               TYPE nast-sort1 DEFAULT 'ZPDF'
                previsualizar       TYPE abap_bool  DEFAULT ''
                memory_id           TYPE any        DEFAULT 'ZFORM_OUTPUT'
                ldest               TYPE rspopname  DEFAULT ''
                copias              TYPE na_anzal   DEFAULT 1
                impresion_inmediata TYPE nast-dimme DEFAULT ''
                memory_id_otf       TYPE any        DEFAULT 'ZMENSAJE_OTFDATA'
      RETURNING VALUE(form)         TYPE fpformoutput.

    CLASS-METHODS preview_pdf
      IMPORTING pdf            TYPE fpcontent
                nombre_fichero TYPE any DEFAULT ''
                directorio     TYPE any DEFAULT ''.

    CLASS-METHODS format_html_text
      IMPORTING replace_tags TYPE string DEFAULT '<H>|<b>,<C>|<i>|'
      CHANGING  !string      TYPE string.

    CLASS-METHODS get_logo_xstring
      IMPORTING grafico             TYPE any       DEFAULT ''
                as_string           TYPE abap_bool DEFAULT ''
      EXPORTING logo_string         TYPE string
                VALUE(logo_xstring) TYPE ptrv_web_xstring
                !message            TYPE bapi_msg.

    CLASS-METHODS unir_pdfs
      IMPORTING i_adj         TYPE zcl_ap_gos=>tt_adj
                limite        TYPE int4   DEFAULT 999999999
                fichero       TYPE string DEFAULT 'PDF'
      EXPORTING VALUE(i_pdfs) TYPE zcl_ap_gos=>tt_adj
                !message      TYPE bapi_msg.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA user_defaults TYPE bapidefaul.
endclass. "ZCL_AP_SFP definition
class ZCL_AP_SFP implementation.
  METHOD abrir_job.
    CHECK sy-tcode <> 'SE37'. " Si probamos desde SE37, la apertura del job es automática!

* Abrimos el JOB inicial
    CLEAR: cancelado,
           error.
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING   ie_outputparams = outputparams
      EXCEPTIONS cancel          = 1
                 usage_error     = 2
                 system_error    = 3
                 internal_error  = 4
                 OTHERS          = 5.

    IF sy-subrc = 1.
      cancelado = 'X'.
    ELSEIF sy-subrc <> 0.
      error = 'X'.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD archivar_pdf.
    DATA: l_toaom     TYPE toaom,
          l_long      TYPE i,
          i_pdf       TYPE TABLE OF tbl1024,
          l_ar_doc_id TYPE saeardoid,
          l_objectid  TYPE sapb-sapobjid,
          l_descr     TYPE c LENGTH 60.

    SELECT * FROM toaom
      INTO l_toaom
      UP TO 1 ROWS
     WHERE sap_object = sap_object
       AND ar_object  =  ar_object
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe entrada'(nee) sap_object ar_object 'en TOAOM'(toa) INTO mensaje SEPARATED BY space.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING buffer        = pdf
      IMPORTING output_length = l_long
      TABLES    binary_tab    = i_pdf.

    CALL FUNCTION 'SCMS_AO_TABLE_CREATE'
      EXPORTING  arc_id       = l_toaom-archiv_id
                 doc_type     = 'PDF'
                 length       = l_long
      IMPORTING  doc_id       = l_ar_doc_id
      TABLES     data         = i_pdf
      EXCEPTIONS error_http   = 1
                 error_archiv = 2
                 error_kernel = 3
                 error_config = 4.

    l_objectid = objectid.
    l_descr = descripcion.
    CALL FUNCTION 'ARCHIV_CONNECTION_INSERT'
      EXPORTING  archiv_id             = l_toaom-archiv_id
                 arc_doc_id            = l_ar_doc_id
                 ar_date               = sy-datum
                 ar_object             = l_toaom-ar_object
                 object_id             = l_objectid
                 sap_object            = l_toaom-sap_object
                 doc_type              = 'PDF'
                 descr                 = l_descr
      EXCEPTIONS error_connectiontable = 1
                 OTHERS                = 2.
  ENDMETHOD.
  METHOD cerrar_job.
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS usage_error    = 1
                 system_error   = 2
                 internal_error = 3
                 OTHERS         = 4.

*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
  ENDMETHOD.
  METHOD constructor.
    formname = form.
    funcion = get_funcion( me->formname ).

    user_defaults = zcl_ap_usuario=>get_defaults( ).

    IF NOT langu IS INITIAL.
      docparams-langu = langu.
    ENDIF.

    IF outparams_ini IS INITIAL.
      CLEAR outputparams.
      outputparams-copies = copias.

      IF dest IS INITIAL.
        outputparams-dest = user_defaults-spld.
      ELSE.
        outputparams-dest = dest.
      ENDIF.

      IF getpdf IS INITIAL.
        outputparams-nodialog = nodialog. " Suprimir diálogo usuario
        outputparams-preview  = preview.
        outputparams-covtitle = covtitle.
        IF outputparams-dest IS NOT INITIAL.
          outputparams-reqnew = 'X'. " Orden SPOOL nueva (parámetros de impresión)
          outputparams-reqimm = reqimm.   " Salidas inmediatas (parámetros de impresión)
*    ls_outputparams-REQDEL = 'X'.   "Borrar tras salida (parámetros de impresión)
          IF user_defaults-spda = 'D'.
            outputparams-reqdel = 'X'.
          ENDIF.
        ELSE.
          CLEAR outputparams-nodialog.
        ENDIF.

        IF reqnew <> '?'.
          outputparams-reqnew = reqnew.
        ENDIF.
        IF reqdel <> '?'.
          outputparams-reqdel = reqdel.
        ENDIF.
      ELSE.
        outputparams-getpdf   = 'X'.
        outputparams-nodialog = 'X'.
        outputparams-preview  = ''.
      ENDIF.
    ELSE.
      outputparams = outparams_ini.
      outputparams-nodialog = 'X'.
    ENDIF.

    IF abrir_job = 'X'.
      abrir_job( ).
    ENDIF.
  ENDMETHOD.
  METHOD convertir_pdf_a_ps.
    DATA: l_fp     TYPE REF TO if_fp,
          l_pdfobj TYPE REF TO if_fp_pdf_object,
          l_fpex   TYPE REF TO cx_fp_runtime.

    CLASS cl_fp DEFINITION LOAD.

* get FP reference
    l_fp = cl_fp=>get_reference( ).

    TRY.
*   create PDF Object
        l_pdfobj = l_fp->create_pdf_object( connection = c_dest ).

*   set document
        l_pdfobj->set_document( pdfdata = pdf ).

        l_pdfobj->set_task_convertpdf2pdl( pdltype = 'ps' ).

*   execute, call ADS
        l_pdfobj->execute( ).

*   get result
        l_pdfobj->get_pdl( IMPORTING pdldata = ps ).

      CATCH cx_fp_runtime_internal INTO l_fpex.
        message = l_fpex->get_text( ).
      CATCH cx_fp_runtime_system INTO l_fpex.
        message = l_fpex->get_text( ).
      CATCH cx_fp_runtime_usage INTO l_fpex.
        message = l_fpex->get_text( ).
    ENDTRY.
  ENDMETHOD.
  METHOD enviar_pdf_a_spool.
    DATA: l_tsp01        TYPE tsp01,
          l_handle       TYPE sy-tabix,
          l_spoolid      TYPE tsp01-rqident ##NEEDED,
          l_partname     TYPE adspart ##NEEDED,
          l_subrc        TYPE c LENGTH 2,
          l_filename     TYPE c LENGTH 128,
          l_paginas      TYPE i,
          l_new_partname TYPE adspart ##NEEDED.

    l_tsp01-rqtitle = titleline.
    CALL FUNCTION 'ADS_SR_OPEN'
      EXPORTING  dest             = dest
                 immediate_print  = immediate_print
                 name             = name
                 doctype          = 'ADSP'
                 titleline        = l_tsp01-rqtitle
                 prio             = prio
                 suffix2          = suffix2
      IMPORTING  handle           = l_handle
                 spoolid          = l_spoolid
                 partname         = l_partname
      EXCEPTIONS device_missing   = 1
                 no_such_device   = 2
                 operation_failed = 3
                 wrong_doctype    = 4
                 wrong_devicetype = 5
                 OTHERS           = 6.
    IF sy-subrc <> 0.
      l_subrc = sy-subrc.
      CONCATENATE 'Error'(err) l_subrc 'abriendo spool'(abs) INTO message SEPARATED BY space.
      EXIT.
    ENDIF.

    CONCATENATE l_partname '.' extension INTO l_filename.

    CALL FUNCTION 'ADS_WRITE_TO_FILE'
      EXPORTING  filename                     = l_filename
                 buffer                       = pdf
*                 APPEND                       = ' '
                 useglobaldir                 = 'X'
      EXCEPTIONS cannot_open_file             = 1
                 open_dataset_no_authority    = 2
                 open_dataset_internal_error  = 3
                 open_dataset_too_many_files  = 4
                 dataset_cant_close           = 5
                 close_dataset_internal_error = 6
                 cannot_close_file            = 7
                 cannot_transfer_data         = 8
                 transfer_internal_error      = 9
                 dataset_write_error          = 10
                 OTHERS                       = 11.
    IF sy-subrc <> 0.
      l_subrc = sy-subrc.
      CONCATENATE 'Error'(err) l_subrc 'escribiendo fichero'(esf) l_filename INTO message SEPARATED BY space.
      EXIT.
    ENDIF.

    l_paginas = get_pdf_pages( pdf ).
    CALL FUNCTION 'ADS_SR_CONFIRM'
      EXPORTING  handle           = l_handle
                 partname         = l_partname
                 size             = 0
                 pages            = l_paginas
*                 NO_PDF           = ' '
      IMPORTING  new_partname     = l_new_partname
      EXCEPTIONS handle_not_valid = 1
                 operation_failed = 2
                 OTHERS           = 3.
    IF sy-subrc <> 0.
      l_subrc = sy-subrc.
      CONCATENATE 'Error'(err) l_subrc 'confirmando spool'(cop) INTO message SEPARATED BY space.
      EXIT.
    ENDIF.

    CALL FUNCTION 'ADS_SR_CLOSE'
      EXPORTING  handle           = l_handle
                 final            = abap_true
      EXCEPTIONS handle_not_valid = 1
                 operation_failed = 2
                 OTHERS           = 3.

    IF sy-subrc <> 0.
      l_subrc = sy-subrc.
      CONCATENATE 'Error'(err) l_subrc 'cerrando spool'(cer) INTO message SEPARATED BY space.
      EXIT.
    ENDIF.
  ENDMETHOD.
  METHOD format_html_text.
*https://blogs.sap.com/2017/02/13/displaying-rich-formatted-text-in-sap-adobe-form/
*1. Definir campo texto como Rich Text
*2. En evento initialize poner esté código JavaScript
*var htmldata = this.rawValue;
*var envel = '<?xml version="1.0" encoding="UTF-8"?><exData contentType="text/html" xmlns="http://www.xfa.org/schema/xfa-template/2.8/">
*<body xmlns="http://www.w3.org/1999/xhtml" xmlns:xfa="http://www.xfa.org/schema/xfa-data/1.0/" xfa:APIVersion="Acroform:2.7.0.0" xfa:spec="2.1"><p>PLACEHOLDER</p></body></exData>';
*var newStr = envel.replace("PLACEHOLDER",htmldata);
*this.value.exData.loadXML(newStr, 1, 1);

*Sólo soporta estos tags Sección 27 de Adobe XML forms specification https://reference.pdfa.org/iso/32000/wp-content/uploads/2017/04/XFA-3_3.pdf#page=1187&zoom=100,0,86
*a “Hyperlink Support” on page 1189
*b “Bold” on page 1199
*br “Line Break” on page 1191
*body “Body Element” on page 1189
*html “HTML Element” on page 1189
*i “Italic” on page 1203
*p “Paragraph” on page 1194
*span “Span” on page 1204
*sub “Subscript” on page 1204
*sup “Superscript” on page 1205
*color “Color” on page 1200
*font “Font” on page 1200
*font-family
*font-size
*font-stretch
*font-style
*font-weight
*margin “Set Margins” on page 1195
*margin-bottom “Space After Paragraph” on page 1196
*margin-left “Left Margin” on page 1191
*margin-right “Right Margin” on page 1194
*margin-top “Space Before Paragraph” on page 1196
*letter-spacing “Letter Spacing” on page 1204
*line-height “Line Spacing” on page 1191
*orphans “Orphan Control” on page 1192
*page-break-after “Page Break Control” on page 1193
*page-break-before
*page-break-inside
*tab-interval “Tab Stops” on page 1205
*tab-stop
*text-decoration “Underline and Strikethrough” on page 1208
*text-indent “First Line Indent” on page 1190
*vertical-align “Vertical Alignment” on page 1197
*widows “Orphan Control” on page 1192
*kerning-mode “Kerning” on page 1203
*xfa-font-horizontal-scale “Font Scale” on page 1202
*xfa-font-vertical-scale
*xfa-tab-stops “

*Ejemplo TABS
*lv_summary = '<p style="tab-interval:0.5in">A' &&
*             '<span style="xfa-tab-count:2" />B' &&
*             '<span style="xfa-tab-count:1" />C</p>'.

    DATA: i_tags TYPE TABLE OF string,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_orig TYPE string,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_dest TYPE string.

    IF NOT replace_tags IS INITIAL.
      SPLIT replace_tags AT ',' INTO TABLE i_tags.
      LOOP AT i_tags ASSIGNING FIELD-SYMBOL(<tag>).
        SPLIT <tag> AT '|' INTO l_orig l_dest.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD get_funcion.
    DATA: gx_exc_api    TYPE REF TO cx_fp_api,
          gv_err_string TYPE string.

    CLEAR funcion.
    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING i_name     = form
          IMPORTING e_funcname = funcion.
      CATCH cx_fp_api INTO gx_exc_api.
        gv_err_string = gx_exc_api->get_text( ).
        MESSAGE gv_err_string TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
  METHOD get_logo_xstring.
    DATA l_long TYPE i.

    CLEAR: logo_xstring,
           logo_string,
           message.

    SELECT SINGLE * FROM stxbitmaps
      INTO @DATA(stxbitmaps)
     WHERE tdobject = 'GRAPHICS'
       AND tdname = @grafico.

    IF sy-subrc <> 0.
      message = 'No existe el gráfico'.
      RETURN.
    ENDIF.

    cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp( EXPORTING  p_object       = stxbitmaps-tdobject
                                                             p_name         = stxbitmaps-tdname
                                                             p_id           = stxbitmaps-tdid
                                                             p_btype        = stxbitmaps-tdbtype
                                                  RECEIVING  p_bmp          = logo_xstring
                                                  EXCEPTIONS not_found      = 1
                                                             internal_error = 2
                                                             OTHERS         = 3 ).

    CASE sy-subrc.
      WHEN 0.
        IF as_string = 'X'.
          IF NOT logo_xstring IS INITIAL.
            l_long = xstrlen( logo_xstring ).
            CALL FUNCTION 'SSFC_BASE64_ENCODE'
              EXPORTING  bindata                  = logo_xstring
                         binleng                  = l_long
              IMPORTING  b64data                  = logo_string
              EXCEPTIONS ssf_krn_error            = 1
                         ssf_krn_noop             = 2
                         ssf_krn_nomemory         = 3
                         ssf_krn_opinv            = 4
                         ssf_krn_input_data_error = 5
                         ssf_krn_invalid_par      = 6
                         ssf_krn_invalid_parlen   = 7
                         OTHERS                   = 8.
            IF sy-subrc <> 0.
              message = 'Error convirtiendo logo a string'.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 1.
        message = 'No existe el gráfico'.
      WHEN 2.
        message = 'Error interno'.
      WHEN OTHERS.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ENDCASE.
  ENDMETHOD.
  METHOD get_mensaje.
    CALL FUNCTION 'FP_GET_LAST_ADS_ERRSTR'
      IMPORTING e_adserrstr = message.

    IF sy-tcode = 'SE37'.
      IF message CS 'Job'.
        message = 'No se pueden ejecutar Adobe forms desde SE37'(s37). " https://launchpad.support.sap.com/#/notes/858325
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_pdf_pages.
    DATA lt_result TYPE match_result_tab.
    DATA: lv_cont  TYPE string,
          lv_lines TYPE i,
          lv_temp  TYPE string,
          lv_pages TYPE numc5.

    FIELD-SYMBOLS: <fs_result> LIKE LINE OF lt_result,
                   <fs_subm>   LIKE LINE OF <fs_result>-submatches.

    paginas = 0.

*  lv_cont = zcl_ap_string=>xstring2string( xstring = pdf ENCODING = zcl_c=>CODEPAGE_UNICODE ).
    CALL 'ICT_DISPATCH' ID 'did' FIELD 'append_xstring_to_string' "#EC CI_CCALL
         ID 'source' FIELD pdf
         ID 'dest'   FIELD lv_cont.

    FIND REGEX `/N (.{1,5})/` IN lv_cont IGNORING CASE RESULTS lt_result.
    IF sy-subrc <> 0.
      FIND ALL OCCURRENCES OF REGEX `/count (.{1,4})/` IN lv_cont IGNORING CASE RESULTS lt_result.
    ENDIF.
    lv_lines = lines( lt_result ).
    IF lv_lines IS NOT INITIAL.
      READ TABLE lt_result ASSIGNING <fs_result> INDEX lv_lines.
      IF sy-subrc = 0.
        READ TABLE <fs_result>-submatches ASSIGNING <fs_subm> INDEX 1.
        IF sy-subrc = 0.
          lv_temp = lv_cont+<fs_subm>-offset(<fs_subm>-length).
          CONDENSE lv_temp NO-GAPS.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING input  = lv_temp
            IMPORTING output = lv_pages.
          paginas = lv_pages.
        ENDIF.
      ENDIF.
    ENDIF.

    IF paginas = 0 AND NOT lv_cont IS INITIAL.
      paginas = 1.
    ENDIF.
  ENDMETHOD.
  METHOD grabar_pdf.
    DATA: l_long TYPE i,
          i_pdf  TYPE TABLE OF hrb2a_raw255.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING buffer        = pdf
      IMPORTING output_length = l_long
      TABLES    binary_tab    = i_pdf.

    zcl_ap_ficheros=>grabar( EXPORTING fichero         = fichero
                                       tipo            = 'BIN'
                                       bin_filesize    = l_long
                                       mostrar_error   = mostrar_error
                                       dialogo         = dialogo
                             CHANGING  tabla           = i_pdf
                                       fichero_dialogo = fichero_dialogo ).
  ENDMETHOD.
  METHOD preview_pdf.
    DATA l_fichero TYPE string.

    IF pdf IS INITIAL.
      RETURN.
    ENDIF.

    IF NOT nombre_fichero IS INITIAL.
      l_fichero = nombre_fichero.
    ELSE.
      l_fichero = 'fichero'.
    ENDIF.

    IF NOT ( l_fichero CS '.pdf' OR l_fichero CS '.PDF' ).
      CONCATENATE l_fichero '.pdf' INTO l_fichero.
    ENDIF.

    IF directorio IS INITIAL.
      l_fichero = zcl_ap_ficheros=>get_directorio_temporal( fichero = l_fichero ).
    ELSE.
      l_fichero = zcl_ap_ficheros=>concat_ruta( directorio = directorio fichero = l_fichero ).
    ENDIF.

    zcl_ap_ficheros=>visualizar( fichero = l_fichero xstring = pdf ).
  ENDMETHOD.
  METHOD recupera_form_from_mensaje.
    DATA: l_tabla   TYPE tabname VALUE 'NAST',
          l_nast    TYPE nast,
          l_pos     TYPE i,
          l_objky   TYPE nast-objky,
          l_erdat   TYPE dats,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_code    TYPE sy-subrc,
          i_otfdata TYPE tsfotf.

    IF nast IS INITIAL.
      SELECT * FROM (l_tabla)
        INTO l_nast
        UP TO 1 ROWS
        WHERE kappl = kappl
          AND objky = objky
          AND kschl = kschl.
      ENDSELECT.
      IF sy-subrc <> 0.
        l_pos = strlen( objky ).
        IF l_pos > 1.
          l_objky = objky.
          l_pos = l_pos - 2.
          l_objky+l_pos(2) = '% '.
          SELECT * FROM (l_tabla)
            INTO l_nast
            UP TO 1 ROWS
           WHERE kappl = kappl
             AND objky LIKE l_objky
             AND kschl = kschl.
          ENDSELECT.
        ENDIF.

        IF l_nast IS INITIAL.
          l_erdat = sy-datum - 15.
          SELECT * FROM (l_tabla)
            INTO l_nast
            UP TO 1 ROWS
           WHERE kappl = kappl
             AND kschl = kschl
             AND erdat >= l_erdat.
          ENDSELECT.
          IF sy-subrc <> 0.
            SELECT * FROM (l_tabla)
              INTO l_nast
              UP TO 1 ROWS
              WHERE kappl = kappl
                AND kschl = kschl
            ORDER BY erdat DESCENDING.
            ENDSELECT.
          ENDIF.
        ENDIF.
        IF NOT l_nast IS INITIAL.
          l_nast-objky = objky.
        ENDIF.
      ENDIF.
    ELSE.
      l_nast = nast.
    ENDIF.

    IF l_nast IS INITIAL.
      RETURN.
    ENDIF.

    IF previsualizar = 'X'.
      CLEAR l_nast-sort1.
    ELSE.
      l_nast-sort1 = sort1.
    ENDIF.
    l_nast-nacha = '1'. " Envio por impresora

    IF NOT ldest IS INITIAL.
      l_nast-ldest = ldest.
    ENDIF.
    l_nast-anzal = copias.
    l_nast-dimme = impresion_inmediata.

* Verificamos si existe el dispositivo, y si no cojo una que si lo haga
    SELECT SINGLE padest FROM tsp03
      INTO l_nast-ldest
     WHERE padest = l_nast-ldest.
    IF sy-subrc <> 0.
      SELECT padest FROM tsp03                            "#EC CI_GENBUFF
        INTO l_nast-ldest
       UP TO 1 ROWS.
      ENDSELECT.
    ENDIF.
    SET PARAMETER ID sort1 FIELD 'X'.

    FREE MEMORY ID memory_id.

    IF NOT memory_id_otf IS INITIAL.
      FREE MEMORY ID memory_id_otf.
    ENDIF.

    IF previsualizar = 'X'.
      CALL FUNCTION 'WFMC_MESSAGE_SINGLE_SCREEN'
        EXPORTING pi_nast  = l_nast                                "#EC *
        IMPORTING pe_rcode = l_code.
    ELSEIF previsualizar = 'I'.
      CLEAR l_nast-dimme. " Sin impresión inmediata
      CALL FUNCTION 'WFMC_MESSAGE_SINGLE_NO_UPDATE'
        EXPORTING pi_nast  = l_nast                                "#EC *
        IMPORTING pe_rcode = l_code.
    ELSE.
      CLEAR l_nast-tdarmod.
      CALL FUNCTION 'WFMC_MESSAGE_SINGLE_SCREEN'
        EXPORTING pi_nast = l_nast.                                "#EC *
    ENDIF.

    IMPORT form_output TO form FROM MEMORY ID memory_id.
    IF sy-subrc = 0 AND form IS INITIAL AND memory_id = 'PDF_FILE'. " Pedidos de compras exportación WebDynpro
      TRY.
          IMPORT lv_pdf_file TO form-pdf FROM MEMORY ID memory_id.
        CATCH cx_root.
      ENDTRY.
    ENDIF.

    IF form-pdf IS INITIAL AND NOT memory_id_otf IS INITIAL.
      IMPORT form_output TO form FROM MEMORY ID memory_id_otf.
    ENDIF.

    FREE MEMORY ID memory_id.

    SET PARAMETER ID 'ZGET_PDF' FIELD ''.                     "#EC *

    IF form-pdf IS INITIAL.
      IMPORT l_output_info-otfdata TO i_otfdata FROM MEMORY ID memory_id.
      IF i_otfdata[] IS INITIAL.
        IMPORT i_otfdata TO i_otfdata FROM MEMORY ID memory_id_otf.
      ENDIF.
      IF NOT i_otfdata IS INITIAL.
        zcl_ap_smartforms=>get_pdf_xstring_from_otfdata( EXPORTING i_otfdata = i_otfdata IMPORTING pdf = form-pdf ).
      ENDIF.
    ENDIF.

    IF form-pdf IS INITIAL.
      IF NOT memory_id_otf IS INITIAL.
        IMPORT l_output_info-otfdata TO i_otfdata FROM MEMORY ID memory_id_otf.
        IF i_otfdata[] IS INITIAL.
          IMPORT i_otfdata TO i_otfdata FROM MEMORY ID memory_id_otf.
        ENDIF.
        IF NOT i_otfdata IS INITIAL.
          zcl_ap_smartforms=>get_pdf_xstring_from_otfdata( EXPORTING i_otfdata = i_otfdata IMPORTING pdf = form-pdf ).
        ENDIF.
        FREE MEMORY ID memory_id_otf.
      ENDIF.
    ENDIF.

    FREE MEMORY ID memory_id.
  ENDMETHOD.
  METHOD recupera_pdf_from_mensaje.
    DATA l_form TYPE fpformoutput.

    l_form = recupera_form_from_mensaje( nast          = nast
                                         kappl         = kappl
                                         objky         = objky
                                         kschl         = kschl
                                         sort1         = sort1
                                         ldest         = ldest
                                         memory_id     = memory_id
                                         previsualizar = previsualizar ).

    pdf = l_form-pdf.
  ENDMETHOD.
  METHOD unir_pdfs.
    DATA: l_resto          TYPE int4,
          l_cont           TYPE int4,
          l_long           TYPE int4,
          l_last           TYPE xfeld,
          l_ficheros_error TYPE string,
          l_xleng          TYPE int4,
          l_nfile          TYPE i,
          l_adj            TYPE zcl_ap_gos=>t_adj,
          l_rc             TYPE i.

    IF lines( i_adj ) = 0.
      RETURN.
    ELSEIF lines( i_adj ) = 1.
      i_pdfs = i_adj.
      i_pdfs[ 1 ]-fichero = |{ fichero }.pdf|.
    ELSE.

      TRY.
          DATA(o_pdf_merger) = NEW cl_rspo_pdf_merge( ).
        CATCH cx_root INTO DATA(o_root).
          message = o_root->get_text( ).
          RETURN.
      ENDTRY.

      l_resto = limite.
      CLEAR: l_cont,
             l_long.
      LOOP AT i_adj ASSIGNING FIELD-SYMBOL(<adj>).
        AT LAST.
          l_last = 'X'.
        ENDAT.
        __add_lista l_ficheros_error <adj>-fichero.
        l_xleng = xstrlen( <adj>-xstring ).
        IF l_xleng > 0.
          IF xstrlen( <adj>-xstring ) < l_resto OR l_cont = 0.
            o_pdf_merger->add_document( <adj>-xstring ).
            l_long = l_long + l_xleng.
            l_resto = l_resto - l_xleng.
            l_cont = l_cont + 1.
          ELSE.
            IF l_nfile = 0.
              l_adj-fichero = |{ fichero }.pdf|.
            ELSE.
              l_adj-fichero = |{ fichero } ({ l_nfile }).pdf|.
            ENDIF.
            o_pdf_merger->merge_documents( IMPORTING merged_document = l_adj-xstring rc = l_rc ).
            IF l_rc <> 0 AND l_adj-xstring = 0.
              message = 'Error generando fichero. Revise ficheros:' && l_ficheros_error.
            ENDIF.
            APPEND l_adj TO i_pdfs.
            CLEAR l_ficheros_error.

            l_nfile = l_nfile + 1.
            CLEAR: o_pdf_merger,
                   l_cont,
                   l_long.
            l_resto = limite.
            o_pdf_merger = NEW cl_rspo_pdf_merge( ).
            o_pdf_merger->add_document( <adj>-xstring ).
            l_long = l_long + l_xleng.
            l_resto = l_resto - l_xleng.
          ENDIF.
        ENDIF.
        IF l_last = 'X'.
          IF l_nfile = 0.
            l_adj-fichero = |{ fichero }.pdf|.
          ELSE.
            l_adj-fichero = |{ fichero } ({ l_nfile }).pdf|.
          ENDIF.
          o_pdf_merger->merge_documents( IMPORTING merged_document = l_adj-xstring rc = l_rc ).
          IF l_rc <> 0 AND l_adj-xstring = 0.
            message = 'Error generando fichero. Revise ficheros:' && l_ficheros_error.
          ENDIF.
          APPEND l_adj TO i_pdfs.
          CLEAR l_ficheros_error.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
