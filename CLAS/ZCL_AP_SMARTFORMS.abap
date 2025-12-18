CLASS zcl_ap_smartforms DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_nombre_funcion
      IMPORTING nombre            TYPE any
      RETURNING VALUE(formulario) TYPE rs38l_fnam.

    CLASS-METHODS get_pdf_from_otfdata
      IMPORTING i_otfdata    TYPE tt_itcoo
                get_xstring  TYPE abap_bool DEFAULT ''
      EXPORTING longitud_pdf TYPE i
                i_pdf        TYPE solix_tab
                xstring      TYPE xstring.

    CLASS-METHODS grabar_pdf
      IMPORTING ruta           TYPE string
                i_pdf          TYPE solix_tab
                longitud_pdf   TYPE i
                preguntar_ruta TYPE abap_bool DEFAULT 'X'.

    CLASS-METHODS grabar_pdf_from_otfdata
      IMPORTING ruta           TYPE string
                preguntar_ruta TYPE abap_bool DEFAULT 'X'
                i_otfdata      TYPE tt_itcoo.

    CLASS-METHODS set_textos
      IMPORTING spras      TYPE sy-langu DEFAULT sy-langu
                !form      TYPE any
                subclave   TYPE any      OPTIONAL
      CHANGING  estructura TYPE any.

    CLASS-METHODS recupera_otfdata_from_mensaje
      IMPORTING nast             TYPE nast       OPTIONAL
                kappl            TYPE any        OPTIONAL
                objky            TYPE any        OPTIONAL
                kschl            TYPE any        OPTIONAL
                sort1            TYPE nast-sort1 DEFAULT 'ZPDF'
                previsualizar    TYPE abap_bool  DEFAULT ''
                memory_id        TYPE any        DEFAULT 'ZMENSAJE_OTFDATA'
      EXPORTING !message         TYPE bapi_msg
      RETURNING VALUE(i_otfdata) TYPE tsfotf.

    CLASS-METHODS previsualizar_mensaje
      IMPORTING nast      TYPE nast       OPTIONAL
                kappl     TYPE any        OPTIONAL
                objky     TYPE any        OPTIONAL
                kschl     TYPE any        OPTIONAL
                prev      TYPE abap_bool  DEFAULT 'X'
                inmediato TYPE abap_bool  DEFAULT ''
                ldest     TYPE nast-ldest DEFAULT ''
                anzal     TYPE any        OPTIONAL
                as_pdf    TYPE abap_bool  DEFAULT ''.

    CLASS-METHODS get_pdf_xstring_from_otfdata
      IMPORTING i_otfdata    TYPE tt_itcoo
      EXPORTING longitud_pdf TYPE i
                pdf          TYPE xstring
                !message     TYPE bapi_msg.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA i_pdf TYPE solix_tab.

    METHODS pdf_preview
      IMPORTING i_otfdata TYPE tt_itcoo.
endclass. "ZCL_AP_SMARTFORMS definition
class ZCL_AP_SMARTFORMS implementation.
  METHOD get_nombre_funcion.
    DATA l_formname TYPE tdsfname.

    l_formname = nombre.
* Inicializar smartform
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = l_formname
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
      IMPORTING
        fm_name            = formulario
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE e398(00) WITH 'ERROR'(ERR) sy-subrc 'AL ABRIR SMARTFORM'(AAS) ''.
    ENDIF.
  ENDMETHOD.
  METHOD get_pdf_from_otfdata.
    DATA: content_txt    TYPE soli_tab,
          l_otfdata      TYPE itcoo,
          l_otfdata2     TYPE solisti1,
          " TODO: variable is assigned but never used (ABAP cleaner)
          t_otfdata2     TYPE TABLE OF solisti1,
          ac             LIKE LINE OF content_txt,
          l_transfer_bin TYPE sx_boolean,
          l_longitud_pdf TYPE so_obj_len,
          objhead        TYPE soli_tab.

    LOOP AT i_otfdata INTO l_otfdata.
      CONCATENATE l_otfdata-tdprintcom l_otfdata-tdprintpar INTO
      l_otfdata2.
      APPEND l_otfdata2 TO t_otfdata2.
      ac = l_otfdata2.
      APPEND ac TO content_txt.
    ENDLOOP.

    l_transfer_bin = ''.
    CALL FUNCTION 'SX_OBJECT_CONVERT_OTF_PDF'
      EXPORTING
        format_src      = 'OTF'
        format_dst      = 'PDF'
        devtype         = 'ASCIIPRI'
      CHANGING
        transfer_bin    = l_transfer_bin
        len             = l_longitud_pdf
        content_txt     = content_txt
        content_bin     = i_pdf
        objhead         = objhead
      EXCEPTIONS
        err_conv_failed = 1
        OTHERS          = 2.

    longitud_pdf = l_longitud_pdf.

    IF get_xstring = 'X'.
      xstring = cl_bcs_convert=>solix_to_xstring( it_solix = i_pdf ).
    ENDIF.
  ENDMETHOD.
  METHOD get_pdf_xstring_from_otfdata.
    DATA lines TYPE TABLE OF tline.

    CLEAR: message, pdf, longitud_pdf.

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
*     MAX_LINEWIDTH         = 132
*     ARCHIVE_INDEX         = ' '
*     COPYNUMBER            = 0
*     ASCII_BIDI_VIS2LOG    = ' '
*     PDF_DELETE_OTFTAB     = ' '
*     PDF_USERNAME          = ' '
*     PDF_PREVIEW           = ' '
*     USE_CASCADING         = ' '
*     MODIFIED_PARAM_TABLE  =
      IMPORTING
        bin_filesize          = longitud_pdf
        bin_file              = pdf
      TABLES
        otf                   = i_otfdata
        lines                 = lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    IF sy-subrc <> 0.
      IF sy-msgty = 'E'.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ELSE.
        __concat3 message 'Se ha producido error'(SPE) sy-subrc 'al convertir OTFDATA en PDF'(COP).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD grabar_pdf.
    DATA : l_ruta        TYPE string,
           l_user_action TYPE i.

    l_ruta = ruta.
    IF NOT preguntar_ruta IS INITIAL.
      CALL FUNCTION 'GUI_FILE_SAVE_DIALOG'
        EXPORTING
          window_title      = 'Guardar PDF'(GPD)
          default_extension = 'PDF'
          default_file_name = l_ruta
        IMPORTING
          fullpath          = l_ruta
          user_action       = l_user_action.

      IF l_user_action = 9.
        return.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = longitud_pdf
        filename                = l_ruta
        filetype                = 'BIN'
      TABLES
        data_tab                = i_pdf
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
  ENDMETHOD.
  METHOD grabar_pdf_from_otfdata.
    DATA i_pdf          TYPE solix_tab.
    DATA l_longitud_pdf TYPE i.

    get_pdf_from_otfdata( EXPORTING i_otfdata = i_otfdata
                                          IMPORTING i_pdf = i_pdf
                                                    longitud_pdf = l_longitud_pdf ).

    grabar_pdf( ruta = ruta
                                i_pdf = i_pdf
                                longitud_pdf = l_longitud_pdf
                                preguntar_ruta = 'X' ).
  ENDMETHOD.
  METHOD pdf_preview.
    CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
      EXPORTING
        i_otf                    = i_otfdata
      EXCEPTIONS
        convert_otf_to_pdf_error = 1
        cntl_error               = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD previsualizar_mensaje.
    DATA: l_tabla  TYPE tabname VALUE 'NAST',
          l_nast   TYPE nast,
          l_return TYPE char1,
          l_ldest  TYPE nast-ldest.
    DATA rseumod TYPE rseumod.

    IF nast IS INITIAL.
      SELECT * FROM (l_tabla)
        INTO l_nast
        UP TO 1 ROWS
        WHERE kappl = kappl
          AND objky = objky
          AND kschl = kschl.
      ENDSELECT.
      IF sy-subrc <> 0.
        SELECT * FROM (l_tabla)
          INTO l_nast
          UP TO 1 ROWS
          WHERE kappl = kappl
            AND kschl = kschl
            AND usnam = sy-uname
         ORDER BY erdat DESCENDING.
        ENDSELECT.
        IF sy-subrc = 0.
          l_nast-objky = objky.
        ELSE.
          SELECT * FROM (l_tabla)
            INTO l_nast
            UP TO 1 ROWS
            WHERE kappl = kappl
              AND kschl = kschl
           ORDER BY erdat DESCENDING.
          ENDSELECT.
          IF sy-subrc = 0.
            l_nast-objky = objky.
            l_nast-usnam = sy-uname.
            l_nast-ldest = zcl_ap_usuario=>get_impresora( ).
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      l_nast = nast.
    ENDIF.

    IF NOT ldest IS INITIAL.
      IF ldest = '?'.
        zcl_ap_popup=>popup_usuario( EXPORTING campo1 = 'NAST-LDEST'
                                               titulo = 'Seleccion dispositivo destino'
                                     IMPORTING return = l_return
                                     CHANGING  valor1 = l_ldest ).
        IF l_return <> 'A' AND l_ldest <> ''.
          l_nast-ldest = l_ldest.
        ENDIF.
      ELSE.
        l_nast-ldest = ldest.
      ENDIF.
    ENDIF.

    IF NOT anzal IS INITIAL.
      l_nast-anzal = anzal.
    ENDIF.

    IF prev = 'X'.
      IF as_pdf = 'X'.
        IMPORT rseumod TO rseumod FROM MEMORY ID 'RSEUMOD'.
        IF rseumod IS INITIAL.
          SELECT SINGLE * FROM rseumod "#EC *
            INTO rseumod
           WHERE uname = sy-uname.
        ENDIF.
        IF rseumod-pdf_prev IS INITIAL.
          rseumod-pdf_prev = 'X'.
          EXPORT rseumod FROM rseumod TO MEMORY ID 'RSEUMOD'.
        ENDIF.
      ENDIF.
      CLEAR l_nast-sort1.
      CALL FUNCTION 'WFMC_MESSAGE_SINGLE_SCREEN'
        EXPORTING
          pi_nast = l_nast.
    ELSE.
      IF inmediato = 'X'.
        l_nast-dimme = 'X'.
      ELSE.
        l_nast-dimme = ''.
      ENDIF.
      CALL FUNCTION 'WFMC_MESSAGE_SINGLE'
        EXPORTING
          pi_nast = l_nast.
    ENDIF.
  ENDMETHOD.
  METHOD recupera_otfdata_from_mensaje.
    DATA: l_nast  TYPE nast,
          l_pos   TYPE i,
          l_objky TYPE nast-objky,
          l_erdat TYPE dats,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_code  TYPE sy-subrc.

    CLEAR message.
    IF nast IS INITIAL.
      SELECT * FROM nast
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
          SELECT * FROM nast
            INTO l_nast
            UP TO 1 ROWS
           WHERE kappl    = kappl
             AND objky LIKE l_objky
             AND kschl    = kschl.
          ENDSELECT.
        ENDIF.

        IF l_nast IS INITIAL.
          l_erdat = sy-datum - 15.
          SELECT * FROM nast
            INTO l_nast
            UP TO 1 ROWS
           WHERE kappl  = kappl
             AND kschl  = kschl
             AND erdat >= l_erdat.
          ENDSELECT.
          IF sy-subrc <> 0.
            SELECT * FROM nast
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

    l_nast-sort1 = sort1.
    l_nast-nacha = '1'. " Envio por impresora
* Verificamos si existe el dispositivo, y si no cojo una que si lo haga
    SELECT SINGLE padest FROM tsp03
      INTO l_nast-ldest
     WHERE padest = l_nast-ldest.
    IF sy-subrc <> 0.
      SELECT padest FROM tsp03                            "#EC CI_GENBUFF
        INTO l_nast-ldest
       UP TO 1 ROWS
       ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.

    TRY.
        IF previsualizar = 'X'.
          CALL FUNCTION 'WFMC_MESSAGE_SINGLE_SCREEN'
            EXPORTING
              pi_nast       = l_nast
            IMPORTING
              pe_rcode      = l_code
            EXCEPTIONS
              error_message = 999.
        ELSE.
          CALL FUNCTION 'WFMC_MESSAGE_SINGLE_SCREEN'
            EXPORTING
              pi_nast       = l_nast
            EXCEPTIONS
              error_message = 999.
        ENDIF.
        IF sy-subrc = 999.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
          RETURN.
        ENDIF.
      CATCH cx_root INTO DATA(o_root).
        message = o_root->get_text( ).
        RETURN.
    ENDTRY.

    IMPORT l_output_info-otfdata TO i_otfdata FROM MEMORY ID memory_id.
    IF i_otfdata IS INITIAL.
      IMPORT i_otfdata TO i_otfdata FROM MEMORY ID memory_id.
    ENDIF.
    IF i_otfdata IS INITIAL.
      IMPORT otf TO i_otfdata FROM MEMORY ID memory_id.
    ENDIF.

    FREE MEMORY ID memory_id.

*Ejemplo de recuperar XSTRING si se usa el formulario estándar de pedido de compra
*DATA(i_otfdata) = zcl_ap_smartforms=>recupera_otfdata_from_mensaje( kappl = 'EF' objky = '4501558391' kschl = 'NEU' SORT1 = 'SWP' MEMORY_ID = '4501558391' ).
*IF NOT i_otfdata IS INITIAL.
*  zcl_ap_smartforms=>get_pdf_xstring_from_otfdata( EXPORTING i_otfdata = i_otfdata IMPORTING pdf = DATA(l_xstring_fact) longitud_pdf = DATA(l_long_pdf) ).
*ENDIF.
  ENDMETHOD.
  METHOD set_textos.
    DATA: lr_data     TYPE REF TO data,
          lo_tabdescr TYPE REF TO cl_abap_structdescr,
          i_campos    TYPE abap_component_tab,
          l_dfies     TYPE abap_componentdescr,
          l_texto     TYPE ztextos_sfo-texto,
          l_campo     TYPE c LENGTH 40.

    FIELD-SYMBOLS <fs> TYPE any.

    CREATE DATA lr_data LIKE estructura.

    lo_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).

    i_campos = lo_tabdescr->get_components( ).

    DEFINE get_texto.
      IF l_texto IS INITIAL.
        SELECT SINGLE texto FROM ztextos_sfo
          INTO l_texto
         WHERE form     = &1
           AND subclave = subclave
           AND spras    = &2
           AND clave    = &3.
        IF sy-subrc <> 0 AND subclave <> ''.
          SELECT SINGLE texto FROM ztextos_sfo
            INTO l_texto
           WHERE form     = &1
             AND subclave = ''
             AND spras    = &2
             AND clave    = &3.
        ENDIF.
      ENDIF.
    END-OF-DEFINITION.

    LOOP AT i_campos INTO l_dfies.
      CLEAR l_texto.

* Primero buscamos el literal exacto
      get_texto form spras l_dfies-name.
* Sino, el genérico para ese idioma
      get_texto '' spras l_dfies-name.
      IF spras <> 'E'.
* Primero buscamos el literal exacto para el idioma inglés
        get_texto form 'E' l_dfies-name.
* Sino, el genérico para el inglés
        get_texto '' 'E' l_dfies-name.
      ENDIF.
      IF spras <> 'S'.
* Primero buscamos el literal exacto para el idioma español
        get_texto form 'S' l_dfies-name.
* Sino, el genérico para el español
        get_texto '' 'S' l_dfies-name.
      ENDIF.
* Si no, por la definición existente independientemente dle idioma
      IF l_texto IS INITIAL.
        SELECT texto FROM ztextos_sfo
          INTO l_texto
          UP TO 1 ROWS
         WHERE form  = form
           AND clave = l_dfies-name.
        ENDSELECT.
      ENDIF.

* En caso contrario, hacemos que se llame como el campo
      IF l_texto IS INITIAL.
        l_texto = l_dfies-name.
      ENDIF.

      CONCATENATE 'ESTRUCTURA-' l_dfies-name INTO l_campo.
      ASSIGN (l_campo) TO <fs>.
      IF sy-subrc = 0.
        <fs> = l_texto.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
