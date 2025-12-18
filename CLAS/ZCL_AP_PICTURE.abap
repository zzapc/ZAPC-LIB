CLASS zcl_ap_picture DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA o_imagen TYPE REF TO cl_gui_picture.
    DATA url      TYPE string.

    METHODS constructor
      IMPORTING obj_contenedor TYPE scrfname DEFAULT 'IMAGEN'.

    METHODS cargar_imagen_local
      IMPORTING fichero TYPE any.

    METHODS fijar_formato
      IMPORTING formato TYPE i DEFAULT cl_gui_picture=>display_mode_fit.

    METHODS free.

    METHODS set_visible
      IMPORTING !visible TYPE char1.

    METHODS cargar_imagen_bds
      IMPORTING tdobject TYPE stxbitmaps-tdobject DEFAULT 'GRAPHICS'
                tdname   TYPE any
                tdid     TYPE stxbitmaps-tdid     DEFAULT 'BMAP'
                tdbtype  TYPE stxbitmaps-tdbtype  DEFAULT 'BCOL'.

    METHODS cargar_imagen
      IMPORTING url TYPE text255.

    METHODS get_url_imagen_bds
      IMPORTING tdobject   TYPE stxbitmaps-tdobject DEFAULT 'GRAPHICS'
                tdname     TYPE any
                tdid       TYPE stxbitmaps-tdid     DEFAULT 'BMAP'
                tdbtype    TYPE stxbitmaps-tdbtype  DEFAULT 'BCOL'
      RETURNING VALUE(url) TYPE text255.

    METHODS handle_double_click
      FOR EVENT picture_dblclick OF cl_gui_picture
      IMPORTING mouse_pos_x
                mouse_pos_y.

    CLASS-METHODS get_imagen_from_html
      IMPORTING url      TYPE any
      EXPORTING xstring  TYPE xstring
                !message TYPE bapi_msg.

    CLASS-METHODS xstring_2_mime
      IMPORTING xstring     TYPE xstring
      RETURNING VALUE(mime) TYPE w3mimetabtype.

    CLASS-METHODS convierte_formato_imagen
      IMPORTING mime            TYPE w3mimetabtype
                formato_origen  TYPE any
                formato_destino TYPE any
                !width          TYPE i OPTIONAL
                height          TYPE i OPTIONAL
      EXPORTING !blob           TYPE w3mimetabtype
                blob_size       TYPE w3param-cont_len
                blob_type       TYPE w3param-cont_type
                !message        TYPE bapi_msg.

    CLASS-METHODS subir_imagen_a_se78
      IMPORTING p_object     TYPE stxbitmaps-tdobject   DEFAULT 'GRAPHICS'
                p_blob       TYPE w3mimetabtype
                p_name       TYPE stxbitmaps-tdname
                p_id         TYPE stxbitmaps-tdid       DEFAULT 'BMAP'
                p_btype      TYPE stxbitmaps-tdbtype    DEFAULT 'BCOL'
                p_format     TYPE any                   DEFAULT 'BMP'
                p_title      TYPE bapisignat-prop_value DEFAULT ''
                p_resident   TYPE stxbitmaps-resident   DEFAULT ''
                p_autoheight TYPE any                   DEFAULT 'X'
                p_bmcomp     TYPE stxbitmaps-bmcomp     DEFAULT 'X'
      EXPORTING !message     TYPE bapi_msg
      CHANGING  p_resolution TYPE stxbitmaps-resolution OPTIONAL
                p_docid      TYPE stxbitmaps-docid      OPTIONAL.

    CLASS-METHODS generar_qrcode
      IMPORTING url             TYPE any DEFAULT 'http://qr.topscan.com/api.php?&bg=ffffff&fg=cc0000&pt=00ff00&inpt=000000&text='
                texto           TYPE any
                objeto          TYPE any
                formato_origen  TYPE any DEFAULT 'image/png'
                formato_destino TYPE any DEFAULT 'image/bmp'
                !width          TYPE i   OPTIONAL
                height          TYPE i   OPTIONAL
      EXPORTING !message        TYPE bapi_msg.

    CLASS-METHODS mime_2_xstring
      IMPORTING mime           TYPE w3mimetabtype
                !size          TYPE int4
      RETURNING VALUE(xstring) TYPE xstring.

    CLASS-METHODS redimensionar
      IMPORTING imagen              TYPE xstring
                mimetype            TYPE string OPTIONAL
                alto                TYPE int4   OPTIONAL
                ancho               TYPE int4
      RETURNING VALUE(imagen_redim) TYPE xstring.

    CLASS-METHODS get_info
      IMPORTING imagen   TYPE xstring
      EXPORTING mimetype TYPE string
                xres     TYPE i
                yres     TYPE i
                xdpi     TYPE i
                ydpi     TYPE i
                bitdepth TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA o_container TYPE REF TO cl_gui_custom_container.
endclass. "ZCL_AP_PICTURE definition
class ZCL_AP_PICTURE implementation.
  METHOD cargar_imagen.
    me->url = url. " Nos guardamos la ruta al fichero
    o_imagen->load_picture_from_url(
      EXPORTING
        url    = url
*    IMPORTING
*      RESULT = l_result
      EXCEPTIONS
        error  = 1
        OTHERS = 2 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD cargar_imagen_bds.
    DATA l_url TYPE c LENGTH 255.

    l_url = get_url_imagen_bds( tdobject = tdobject
                                tdname   = tdname
                                tdid     = tdid
                                tdbtype  = tdbtype ).

    cargar_imagen( l_url ).
  ENDMETHOD.
  METHOD cargar_imagen_local.
    DATA l_url TYPE text255.

    url = fichero. " Nos guardamos la ruta al fichero
    IF zcl_ap_documentos=>existe_fichero( fichero ) = 'X'.
      CONCATENATE 'file://' fichero INTO l_url.
      o_imagen->load_picture_from_url( l_url ).
    ELSE.
      set_visible( ' ' ).
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
* Registramos evento para doble click

    DATA: event_tab_line TYPE cntl_simple_event,
          event_tab      TYPE TABLE OF cntl_simple_event.

    free( ).
    o_container = NEW #(
        container_name = obj_contenedor ).
    o_imagen = NEW #(
        parent = o_container ).

    fijar_formato( ).

    event_tab_line-eventid = cl_gui_picture=>eventid_picture_dblclick.
    APPEND event_tab_line TO event_tab.

    o_imagen->set_registered_events(
        events = event_tab ).

    SET HANDLER handle_double_click FOR o_imagen.
  ENDMETHOD.
  METHOD convierte_formato_imagen.
    DATA i_igs_image_converter TYPE REF TO cl_igs_image_converter.
    DATA l_content_length      TYPE i.

    i_igs_image_converter = NEW #( ).

    i_igs_image_converter->input  = formato_origen.
    i_igs_image_converter->output = formato_destino.
    i_igs_image_converter->width  = width.
    i_igs_image_converter->height = height.

    i_igs_image_converter->set_image(
        blob      = mime
        blob_size = l_content_length ).

    i_igs_image_converter->execute(
      EXCEPTIONS
        communication_error = 1
        internal_error      = 2
        external_error      = 3
        OTHERS              = 4 ).

    IF sy-subrc = 0.

      i_igs_image_converter->get_image(
        IMPORTING
          blob      = blob
          blob_size = blob_size
          blob_type = blob_type ).

    ELSE.
      message = 'Error convitiendo imagen'.
    ENDIF.
  ENDMETHOD.
  METHOD fijar_formato.
    o_imagen->set_display_mode( formato ).
  ENDMETHOD.
  METHOD free.
    IF NOT o_imagen IS INITIAL.
      o_imagen->free( ).
      CLEAR o_imagen.
    ENDIF.

    IF NOT o_container IS INITIAL.
      o_container->free( ).
      CLEAR o_container.
    ENDIF.
  ENDMETHOD.
  METHOD generar_qrcode.
    DATA: l_url     TYPE string,
          l_xstring TYPE xstring,
          l_mime    TYPE w3mimetabtype,
          l_blob    TYPE w3mimetabtype.

    CONCATENATE url texto INTO l_url.

    get_imagen_from_html(
      EXPORTING
        url     = l_url
      IMPORTING
        xstring = l_xstring
        message = message ).

    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF l_xstring IS INITIAL.
      message = 'No se ha recuperado QR'.
      EXIT.
    ENDIF.

    l_mime = xstring_2_mime( l_xstring ).

    IF l_mime IS INITIAL.
      message = 'Fallo en conversión a MIME'.
      EXIT.
    ENDIF.

    convierte_formato_imagen(
      EXPORTING
        mime            = l_mime
        formato_origen  = formato_origen
        formato_destino = formato_destino
        width           = width
        height          = height
      IMPORTING
        blob            = l_blob
*       blob_size       =
*       blob_type       =
        message         = message ).

    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.

    subir_imagen_a_se78(
      EXPORTING
*       p_object     = 'GRAPHICS'
        p_blob  = l_blob
        p_name  = objeto
*       p_id    = 'BMAP'
*       p_btype = 'BCOL'
*       p_format     = 'BMP'
*       p_title = ''
*       p_resident   = ''
*       p_autoheight = 'X'
*       p_bmcomp     = 'X'
      IMPORTING
        message = message ).
*   CHANGING
*     p_resolution =
*     p_docid      =
*     .
  ENDMETHOD.
  METHOD get_imagen_from_html.
    DATA : http_client TYPE REF TO if_http_client,
           l_subrc     TYPE c LENGTH 5.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = url
      IMPORTING
        client             = http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).

    IF sy-subrc = 0.
      http_client->send(
*  EXPORTING
*    timeout                    = CO_TIMEOUT_DEFAULT
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5 ).

      IF sy-subrc <> 0.
        l_subrc = sy-subrc.
        CONCATENATE 'Error' l_subrc 'accediendo a URL' INTO message SEPARATED BY space.
      ELSE.
        http_client->receive(
          EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            OTHERS                     = 4 ).
        IF sy-subrc <> 0.
          l_subrc = sy-subrc.
          CONCATENATE 'Error' l_subrc 'accediendo a URL' INTO message SEPARATED BY space.
        ELSE.

          xstring = http_client->response->get_data( ).
        ENDIF.
      ENDIF.
      http_client->close( ).
    ELSE.
      l_subrc = sy-subrc.
      CONCATENATE 'Error' l_subrc 'accediendo a URL' INTO message SEPARATED BY space.
    ENDIF.
  ENDMETHOD.
  METHOD get_info.
    TRY.
        DATA(image_processor) = NEW cl_fxs_image_processor( ).
        DATA(handle) = image_processor->add_image( iv_data = imagen ).

        image_processor->get_info( EXPORTING iv_handle   = handle
                                   IMPORTING ev_mimetype = mimetype
                                             ev_xres     = xres
                                             ev_yres     = yres
                                             ev_xdpi     = xdpi
                                             ev_ydpi     = ydpi
                                             ev_bitdepth = bitdepth ).

      CATCH cx_root INTO DATA(o_root). " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE 'Error recuperando imagen' TYPE 'I'.
    ENDTRY.
  ENDMETHOD.
  METHOD get_url_imagen_bds.
    TYPES: BEGIN OF t_graphic_table,
             line TYPE x LENGTH 255,
           END OF t_graphic_table.

    DATA: g_stxbitmaps TYPE stxbitmaps,
          l_bytecount  TYPE i,
          l_content    TYPE STANDARD TABLE OF bapiconten INITIAL SIZE 0.
    DATA: graphic_size    TYPE i,
          i_graphic_table TYPE TABLE OF t_graphic_table.

    g_stxbitmaps-tdobject = tdobject.
    g_stxbitmaps-tdname   = tdname.
    g_stxbitmaps-tdid     = tdid.
    g_stxbitmaps-tdbtype  = tdbtype.  "(BMON = black&white, BCOL = color)

    CALL FUNCTION 'SAPSCRIPT_GET_GRAPHIC_BDS'
      EXPORTING
        i_object       = g_stxbitmaps-tdobject
        i_name         = g_stxbitmaps-tdname
        i_id           = g_stxbitmaps-tdid
        i_btype        = g_stxbitmaps-tdbtype
      IMPORTING
        e_bytecount    = l_bytecount
      TABLES
        content        = l_content
      EXCEPTIONS
        not_found      = 1
        bds_get_failed = 2
        bds_no_content = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Error en función SAPSCRIPT_GET_GRAPHIC_BDS' TYPE 'E'.
    ENDIF.

    CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP'
      EXPORTING
        old_format               = 'BDS'
        new_format               = 'BMP'
        bitmap_file_bytecount_in = l_bytecount
      IMPORTING
        bitmap_file_bytecount    = graphic_size
      TABLES
        bds_bitmap_file          = l_content
        bitmap_file              = i_graphic_table
      EXCEPTIONS
        OTHERS                   = 1.
    IF sy-subrc <> 0.
      MESSAGE 'Error en función SAPSCRIPT_CONVERT_BITMAP' TYPE 'E'.
    ENDIF.

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type     = 'image'                                    ##NO_TEXT
        subtype  = cndp_sap_tab_unknown
        size     = graphic_size
        lifetime = cndp_lifetime_transaction
      TABLES
        data     = i_graphic_table
      CHANGING
        url      = url
      EXCEPTIONS
        OTHERS   = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Error en función DP_CREATE_URL' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD handle_double_click.
    IF NOT me->url IS INITIAL.
      zcl_ap_gos=>visualizar_fichero_st( fichero = url ).
    ENDIF.
  ENDMETHOD.
  METHOD mime_2_xstring.
    DATA lv_remaining TYPE i.

    lv_remaining = size.

    CLEAR xstring.
    LOOP AT mime INTO DATA(ls_mime).
      IF lv_remaining > 255.
        CONCATENATE xstring ls_mime-line INTO xstring IN BYTE MODE.
        lv_remaining = lv_remaining - 255.
      ELSE.
        CONCATENATE xstring ls_mime-line(lv_remaining) INTO xstring IN BYTE MODE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD redimensionar.
    TYPES: BEGIN OF t_w3mime,
             line TYPE x LENGTH 255,
           END OF t_w3mime.
    TYPES tt_w3mime TYPE STANDARD TABLE OF t_w3mime.

    DATA: lv_size        TYPE i,
          lt_mimetab     TYPE tt_w3mime,
          lo_igs_imgconv TYPE REF TO cl_igs_image_converter.

    CLEAR imagen_redim.

    lv_size = xstrlen( imagen ).

    lt_mimetab = xstring_2_mime( imagen ).

    zcl_ap_picture=>get_info( EXPORTING imagen = imagen
                              IMPORTING mimetype = DATA(l_mimetype)
                                        xres = DATA(l_xres)
                                        yres = DATA(l_yres) ).

    IF l_mimetype IS INITIAL AND l_xres = 0 AND l_yres = 0.
      RETURN.
    ENDIF.

    lo_igs_imgconv = NEW #( ).

    lo_igs_imgconv->set_image( blob = lt_mimetab blob_size = lv_size ).
    lo_igs_imgconv->input = l_mimetype.
    IF NOT mimetype IS INITIAL.
      lo_igs_imgconv->output = mimetype.
    ELSE.
      lo_igs_imgconv->output = l_mimetype.
    ENDIF.

    lo_igs_imgconv->width = ancho.

    IF NOT alto IS INITIAL.
      lo_igs_imgconv->height = alto.
    ELSEIF NOT l_xres IS INITIAL.
      lo_igs_imgconv->height = l_yres * ancho / l_xres.
    ENDIF.

    TRY.
        lo_igs_imgconv->execute(
          EXCEPTIONS
            communication_error = 1
            internal_error      = 2
            external_error      = 3
            OTHERS              = 4 ).
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        lo_igs_imgconv->get_image(
          IMPORTING
            blob      = lt_mimetab
            blob_size = lv_size ).

        CLEAR: lo_igs_imgconv->input,
               lo_igs_imgconv->output,
               lo_igs_imgconv->width,
               lo_igs_imgconv->height.

        imagen_redim = mime_2_xstring( mime = lt_mimetab size = lv_size ).

      CATCH cx_root INTO DATA(o_root). " TODO: variable is assigned but never used (ABAP cleaner)
        MESSAGE 'Error recuperando imagen' TYPE 'I'.
    ENDTRY.
  ENDMETHOD.
  METHOD set_visible.
    o_imagen->set_visible( visible ).
  ENDMETHOD.
  METHOD subir_imagen_a_se78.
    TYPES: BEGIN OF t_bitmap,
             l TYPE x LENGTH 64,
           END OF t_bitmap.

    CONSTANTS:
      c_bds_classname TYPE sbdst_classname  VALUE 'DEVC_STXD_BITMAP',
      c_bds_classtype TYPE sbdst_classtype  VALUE 'OT', " others
      c_bds_mimetype  TYPE bds_mimetp       VALUE 'application/octet-stream',
      c_bds_original  TYPE sbdst_doc_var_tg VALUE 'OR'.

    DATA: l_bytecount     TYPE i,
          l_bds_bytecount TYPE i,
          l_filename      TYPE string.
    DATA: l_width_tw   TYPE stxbitmaps-widthtw,
          l_height_tw  TYPE stxbitmaps-heighttw,
          l_width_pix  TYPE stxbitmaps-widthpix,
          l_height_pix TYPE stxbitmaps-heightpix,
          l_color      TYPE c LENGTH 1.
    DATA: l_bds_content     TYPE sbdst_content,
          l_bds_object      TYPE REF TO cl_bds_document_set,
          wa_bds_components TYPE LINE OF sbdst_components,
          l_bds_components  TYPE sbdst_components,
          wa_bds_signature  TYPE LINE OF sbdst_signature,
          l_bds_signature   TYPE sbdst_signature,
          wa_bds_properties TYPE LINE OF sbdst_properties,
          l_bds_properties  TYPE sbdst_properties.
    DATA l_object_key  TYPE sbdst_object_key.
    DATA wa_stxbitmaps TYPE stxbitmaps.
    DATA l_tab         TYPE ddobjname.

    DATA l_bitmap      TYPE TABLE OF t_bitmap.

    DEFINE enqueue_graphic.
      CALL FUNCTION 'ENQUEUE_ESSGRABDS'
        EXPORTING
*     MODE_STXBITMAPS = 'E'
          tdobject     = &1
          tdname       = &2
          tdid         = &3
          tdbtype      = &4
*     X_TDOBJECT   = ' '
*     X_TDNAME     = ' '
*     X_TDID       = ' '
*     X_TDBTYPE    = ' '
*     _SCOPE       = '2'
*     _WAIT        = ' '
*     _COLLECT     = ' '
        EXCEPTIONS
          foreign_lock = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO message.
      ENDIF.
    END-OF-DEFINITION.

    DEFINE dequeue_graphic.

      CALL FUNCTION 'DEQUEUE_ESSGRABDS'
        EXPORTING
*     MODE_STXBITMAPS = 'E'
*     X_TDOBJECT      = ' '
*     X_TDNAME = ' '
*     X_TDID   = ' '
*     X_TDBTYPE       = ' '
*     _SCOPE   = '3'
*     _SYNCHRON       = ' '
*     _COLLECT = ' '
          tdobject = &1
          tdname   = &2
          tdid     = &3
          tdbtype  = &4.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*                INTO message.
*      ENDIF.
    END-OF-DEFINITION.

* Enqueue
    enqueue_graphic p_object p_name p_id p_btype.

* Bitmap conversion
    CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP_BDS'
      EXPORTING
        color                    = 'X'
        format                   = p_format
        resident                 = p_resident
        bitmap_bytecount         = l_bytecount
        compress_bitmap          = p_bmcomp
      IMPORTING
        width_tw                 = l_width_tw
        height_tw                = l_height_tw
        width_pix                = l_width_pix
        height_pix               = l_height_pix
        dpi                      = p_resolution
        bds_bytecount            = l_bds_bytecount
      TABLES
        bitmap_file              = p_blob
        bitmap_file_bds          = l_bds_content
      EXCEPTIONS
        format_not_supported     = 1
        no_bmp_file              = 2
        bmperr_invalid_format    = 3
        bmperr_no_colortable     = 4
        bmperr_unsup_compression = 5
        bmperr_corrupt_rle_data  = 6
        OTHERS                   = 7.

    IF sy-subrc <> 0.

      dequeue_graphic p_object p_name p_id p_btype.

    ENDIF.

* Save bitmap in BDS
    l_bds_object = NEW #( ).

    wa_bds_components-doc_count  = '1'.
    wa_bds_components-comp_count = '1'.
    wa_bds_components-mimetype   = c_bds_mimetype.
    wa_bds_components-comp_size  = l_bds_bytecount.
    APPEND wa_bds_components TO l_bds_components.

    IF p_docid IS INITIAL.          " graphic is new

      wa_bds_signature-doc_count = '1'.
      APPEND wa_bds_signature TO l_bds_signature.

      l_bds_object->create_with_table(
        EXPORTING
          classname  = c_bds_classname
          classtype  = c_bds_classtype
          components = l_bds_components
          content    = l_bds_content
        CHANGING
          signature  = l_bds_signature
          object_key = l_object_key
        EXCEPTIONS
          OTHERS     = 1 ).

      IF sy-subrc <> 0.

        dequeue_graphic p_object p_name p_id p_btype.
      ENDIF.

      READ TABLE l_bds_signature INDEX 1 INTO wa_bds_signature
           TRANSPORTING doc_id.

      IF sy-subrc = 0.

        p_docid = wa_bds_signature-doc_id.

      ELSE.

        dequeue_graphic p_object p_name p_id p_btype.

      ENDIF.

    ELSE.                " graphic already exists

********* read object_key for faster access *****
      CLEAR l_object_key.
      SELECT SINGLE * FROM stxbitmaps INTO wa_stxbitmaps
          WHERE tdobject = p_object
            AND tdid     = p_id
            AND tdname   = p_name
            AND tdbtype  = p_btype.

      SELECT SINGLE tabname FROM bds_locl INTO l_tab
         WHERE classname = c_bds_classname
           AND classtype = c_bds_classtype.

      IF sy-subrc = 0.

        SELECT SINGLE object_key FROM (l_tab) INTO l_object_key
          WHERE loio_id   = wa_stxbitmaps-docid+10(32)
            AND classname = c_bds_classname
            AND classtype = c_bds_classtype.

      ENDIF.

******** read object_key end ********************

      l_bds_object->update_with_table(
        EXPORTING
          classname     = c_bds_classname
          classtype     = c_bds_classtype
          object_key    = l_object_key
          doc_id        = p_docid
          doc_ver_no    = '1'
          doc_var_id    = '1'
        CHANGING
          components    = l_bds_components
          content       = l_bds_content
        EXCEPTIONS
          nothing_found = 1
          OTHERS        = 2 ).

      IF sy-subrc = 1.   " inconsistency STXBITMAPS - BDS; repeat check in

        wa_bds_signature-doc_count = '1'.
        APPEND wa_bds_signature TO l_bds_signature.

        l_bds_object->create_with_table(
          EXPORTING
            classname  = c_bds_classname
            classtype  = c_bds_classtype
            components = l_bds_components
            content    = l_bds_content
          CHANGING
            signature  = l_bds_signature
            object_key = l_object_key
          EXCEPTIONS
            OTHERS     = 1 ).

        IF sy-subrc <> 0.
          dequeue_graphic p_object p_name p_id p_btype.

        ENDIF.

        READ TABLE l_bds_signature INDEX 1 INTO wa_bds_signature
             TRANSPORTING doc_id.
        IF sy-subrc = 0.
          p_docid = wa_bds_signature-doc_id.
        ELSE.

          dequeue_graphic p_object p_name p_id p_btype.

        ENDIF.

      ELSEIF sy-subrc = 2.

        dequeue_graphic p_object p_name p_id p_btype.

      ENDIF.

    ENDIF.

* Save bitmap header in STXBITPMAPS
    wa_stxbitmaps-tdname     = p_name.
    wa_stxbitmaps-tdobject   = p_object.
    wa_stxbitmaps-tdid       = p_id.
    wa_stxbitmaps-tdbtype    = p_btype.
    wa_stxbitmaps-docid      = p_docid.
    wa_stxbitmaps-widthpix   = l_width_pix.
    wa_stxbitmaps-heightpix  = l_height_pix.
    wa_stxbitmaps-widthtw    = l_width_tw.
    wa_stxbitmaps-heighttw   = l_height_tw.
    wa_stxbitmaps-resolution = p_resolution.
    wa_stxbitmaps-resident   = p_resident.
    wa_stxbitmaps-autoheight = p_autoheight.
    wa_stxbitmaps-bmcomp     = p_bmcomp.
    INSERT INTO stxbitmaps VALUES wa_stxbitmaps.

    IF sy-subrc <> 0.

      UPDATE stxbitmaps FROM wa_stxbitmaps.

      IF sy-subrc <> 0.

*       message e285 with p_name 'STXBITMAPS'.

      ENDIF.

    ENDIF.

* Set description in BDS attributes

    wa_bds_properties-prop_name  = 'DESCRIPTION'.
    wa_bds_properties-prop_value = p_title.
    APPEND wa_bds_properties TO l_bds_properties.

    l_bds_object->change_properties(
      EXPORTING
        classname  = c_bds_classname
        classtype  = c_bds_classtype
        object_key = l_object_key
        doc_id     = p_docid
        doc_ver_no = '1'
        doc_var_id = '1'
      CHANGING
        properties = l_bds_properties
      EXCEPTIONS
        OTHERS     = 1 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error subiendo imagen' TYPE 'I'.
    ENDIF.

    dequeue_graphic p_object p_name p_id p_btype.
  ENDMETHOD.
  METHOD xstring_2_mime.
    DATA l_str_length TYPE i.

    l_str_length = xstrlen( xstring ).

    CALL FUNCTION 'RSFO_XSTRING_TO_MIME'
      EXPORTING
        c_xstring = xstring
        i_length  = l_str_length
      TABLES
        c_t_mime  = mime.
  ENDMETHOD.