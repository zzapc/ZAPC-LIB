CLASS zcl_ap_documentos DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_documentos TYPE STANDARD TABLE OF zdocumentos WITH KEY tcode nombre.

    CLASS-METHODS cargar_documento
      IMPORTING fichero   TYPE string
                servidor  TYPE abap_bool DEFAULT ''
      CHANGING  documento TYPE zdocumentos.

    CLASS-METHODS grabar_documento
      IMPORTING documento TYPE zdocumentos OPTIONAL
                fichero   TYPE string      OPTIONAL
      PREFERRED PARAMETER documento.

    CLASS-METHODS get_directorio_temporal
      RETURNING VALUE(directorio) TYPE string.

    CLASS-METHODS get_nombre_fichero_temporal
      IMPORTING documento            TYPE zdocumentos OPTIONAL
                nuevo_nombre         TYPE any         DEFAULT ''
                no_borrar            TYPE abap_bool   DEFAULT ''
      PREFERRED PARAMETER documento
      RETURNING VALUE(fichero_local) TYPE string.

    CLASS-METHODS existe_fichero
      IMPORTING fichero       TYPE any
      RETURNING VALUE(existe) TYPE abap_bool.

    CLASS-METHODS grabar_documento_temporal
      IMPORTING tcode          TYPE any         OPTIONAL
                nombre         TYPE any         OPTIONAL
                documento      TYPE zdocumentos OPTIONAL
                nuevo_nombre   TYPE any         DEFAULT ''
                no_borrar      TYPE abap_bool   DEFAULT ''
      PREFERRED PARAMETER documento
      RETURNING VALUE(fichero) TYPE string.

    CLASS-METHODS visualizar_documento
      IMPORTING tcode         TYPE any DEFAULT sy-cprog
                nombre        TYPE any OPTIONAL
      PREFERRED PARAMETER nombre
      RETURNING VALUE(existe) TYPE abap_bool.

    CLASS-METHODS popup_documento
      IMPORTING operacion TYPE char1
      CHANGING  documento TYPE zest_documento.

    CLASS-METHODS crear_documento
      IMPORTING tcode    TYPE zdocumentos-tcode
                nombre   TYPE zdocumentos-nombre
                servidor TYPE abap_bool DEFAULT ''
                fichero  TYPE string.

    CLASS-METHODS popup_list
      IMPORTING tcode         TYPE any                       DEFAULT sy-cprog
                clasificacion TYPE zdocumentos-clasificacion OPTIONAL.

    CLASS-METHODS matchcode_documentos
      IMPORTING tcode         TYPE any
      RETURNING VALUE(nombre) TYPE zdocumentos-nombre.

    CLASS-METHODS existe_doc
      IMPORTING tcode         TYPE any                       DEFAULT sy-cprog
                nombre        TYPE any                       OPTIONAL
                clasificacion TYPE zdocumentos-clasificacion OPTIONAL
      PREFERRED PARAMETER nombre
      RETURNING VALUE(existe) TYPE abap_bool.

    CLASS-METHODS ejecutar
      IMPORTING tcode          TYPE any         OPTIONAL
                nombre         TYPE any         OPTIONAL
                documento      TYPE zdocumentos OPTIONAL
                parametros     TYPE any         DEFAULT ''
                sincrono       TYPE string      DEFAULT 'X'
      RETURNING VALUE(mensaje) TYPE string.

    CLASS-METHODS borrar_documento
      IMPORTING tcode  TYPE zdocumentos-tcode
                nombre TYPE zdocumentos-nombre.

    CLASS-METHODS get_documentos_por_clas
      IMPORTING tcode         TYPE zdocumentos-tcode
                clasificacion TYPE zdocumentos-clasificacion
      EXPORTING i_documentos  TYPE tt_documentos
                eliminados    TYPE int4.


endclass. "ZCL_AP_DOCUMENTOS definition
class ZCL_AP_DOCUMENTOS implementation.
  METHOD borrar_documento.
    DELETE FROM zdocumentos
     WHERE tcode  = tcode
       AND nombre = nombre.
  ENDMETHOD.
  METHOD cargar_documento.
    TYPES: BEGIN OF t_linea,
             c TYPE c LENGTH 65535,
           END OF t_linea.

    DATA l_fichero TYPE string.

    l_fichero = zcl_ap_ficheros=>limpiar_fichero( fichero ).

    documento-extension = zcl_ap_ficheros=>get_extension( l_fichero ).

    zcl_ap_ficheros=>leer_xstring( EXPORTING fichero = l_fichero
                                             servidor = servidor
                                   IMPORTING xstring = documento-xstring  ).
    documento-longitud = xstrlen( documento-xstring ).

  ENDMETHOD.
  METHOD crear_documento.
    DATA l_documento TYPE zdocumentos.

    CLEAR l_documento.
    l_documento-tcode   = tcode.
    l_documento-nombre  = nombre.
    l_documento-fichero = fichero.
    l_documento-erdat   = sy-datum.
    l_documento-erzet   = sy-uzeit.
    l_documento-ernam   = sy-uname.
    cargar_documento( EXPORTING fichero   = fichero
                                                servidor  = servidor
                                       CHANGING documento = l_documento ).
    MODIFY zdocumentos FROM l_documento.
  ENDMETHOD.
  METHOD ejecutar.
    DATA l_fichero TYPE string.

    l_fichero = grabar_documento_temporal( tcode     = tcode
                                           nombre    = nombre
                                           documento = documento ).
    IF NOT l_fichero IS INITIAL.
      cl_gui_frontend_services=>execute(
        EXPORTING
*       document               =
          application            = l_fichero
          parameter              = parametros
*       default_directory      =
*       maximized              =
*       minimized              =
          synchronous            = sincrono
*       operation              = 'OPEN'
        EXCEPTIONS
          cntl_error             = 1
          error_no_gui           = 2
          bad_parameter          = 3
          file_not_found         = 4
          path_not_found         = 5
          file_extension_unknown = 6
          error_execute_failed   = 7
          synchronous_failed     = 8
          not_supported_by_gui   = 9
          OTHERS                 = 10 ).
      IF sy-subrc <> 0.
        l_fichero = sy-subrc.
        CONCATENATE 'Error'(err) l_fichero 'llamando al programa'(llp) nombre INTO mensaje SEPARATED BY space.
      ENDIF.
    ELSE.
      CONCATENATE 'Programa'(pro) nombre 'no existe'(noe) INTO mensaje SEPARATED BY space.
    ENDIF.
  ENDMETHOD.
  METHOD existe_doc.
    DATA r_clas  TYPE RANGE OF zdocumentos-clasificacion.
    DATA l_erdat TYPE zdocumentos-erdat.

    CLEAR existe.

    IF clasificacion IS SUPPLIED.
      r_clas = VALUE #( ( option = 'EQ' sign = 'I' low = clasificacion ) ).
    ENDIF.

    SELECT SINGLE erdat FROM zdocumentos
      INTO @l_erdat
     WHERE tcode          = @tcode
       AND nombre         = @nombre
       AND clasificacion IN @r_clas.
    IF sy-subrc = 0.
      existe = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD existe_fichero.
    DATA l_fichero TYPE string.

    l_fichero = fichero.
    existe = cl_gui_frontend_services=>file_exist( l_fichero ).
  ENDMETHOD.
  METHOD get_directorio_temporal.
    cl_gui_frontend_services=>environment_get_variable(
      EXPORTING
        variable             = 'TEMP'
      CHANGING
        value                = directorio
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando directorio temporal'(ert) TYPE 'E'.
    ENDIF.

    cl_gui_cfw=>flush(
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2
        OTHERS            = 3 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error de flush' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_documentos_por_clas.
    CLEAR: i_documentos, eliminados.

    SELECT * FROM zdocumentos
      INTO TABLE i_documentos
     WHERE tcode         = tcode
       AND clasificacion = clasificacion.

* Devolvemos los documentos en función de los permisos
    LOOP AT i_documentos ASSIGNING FIELD-SYMBOL(<documento>) WHERE permisos <> ''.
      IF sy-uname = zcl_c=>usuario_ap.
        IF zcl_ap_lista=>es_elemento( lista = <documento>-permisos elemento = 'APC' ).
          CONTINUE.
        ENDIF.
      ENDIF.
      IF zcl_ap_lista=>es_elemento( lista = <documento>-permisos elemento = 'SISTEMAS' ).
        IF zcl_ap_autorizacion=>es_usuario_sistemas( ).
          CONTINUE.
        ENDIF.
      ENDIF.
      IF zcl_ap_lista=>es_elemento( lista = <documento>-permisos elemento = 'SAPALL' ).
        IF zcl_ap_autorizacion=>es_sapall( ).
          CONTINUE.
        ENDIF.
      ENDIF.

      eliminados = eliminados + 1.
      DELETE i_documentos.
    ENDLOOP.

* Si hay dos posibles documentos y uno es editable, devolvemos ese sobre el otro
    LOOP AT i_documentos ASSIGNING <documento> WHERE nombre CS '_EDT'.
      SPLIT <documento>-nombre AT '_EDT' INTO DATA(l_nombre) DATA(l_aux).
      IF l_aux IS INITIAL.
        DELETE i_documentos WHERE nombre = l_nombre.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_nombre_fichero_temporal.
    DATA: l_directorio TYPE string,
          l_salir      TYPE c LENGTH 1,
          l_cont       TYPE c LENGTH 2,
          l_extension  TYPE string,
          l_rc         TYPE i.

    l_directorio = get_directorio_temporal( ).

    WHILE l_salir = ''.
      IF nuevo_nombre IS INITIAL.
        CONCATENATE l_directorio '\' documento-nombre l_cont
                    INTO fichero_local.
      ELSE.
        CONCATENATE l_directorio '\' nuevo_nombre l_cont
                    INTO fichero_local.
      ENDIF.

      l_extension = zcl_ap_ficheros=>get_extension( fichero_local ).
      IF l_extension IS INITIAL.
        l_extension = zcl_ap_ficheros=>get_extension( documento-fichero ).
        CONCATENATE fichero_local '.' l_extension INTO fichero_local.
      ENDIF.

      IF existe_fichero( fichero_local ) = 'X'.
        IF no_borrar IS INITIAL.
          cl_gui_frontend_services=>file_delete(
                                      EXPORTING filename = fichero_local
                                                 CHANGING rc = l_rc
            EXCEPTIONS
              file_delete_failed   = 1
              cntl_error           = 2
              error_no_gui         = 3
              file_not_found       = 4
              access_denied        = 5
              unknown_error        = 6
              not_supported_by_gui = 7
              wrong_parameter      = 8
              OTHERS               = 9 ).
          IF sy-subrc <> 0.
            MESSAGE 'Error borrando fichero' TYPE 'S'.
          ENDIF.

          IF existe_fichero( fichero_local ) = 'X'.
            l_cont = l_cont + 1.
          ELSE.
            l_salir = 'X'.
          ENDIF.
        ELSE.
          l_cont = l_cont + 1.
        ENDIF.
      ELSE.
        l_salir = 'X'.
      ENDIF.

      IF l_cont = '99'.
        l_salir = 'X'.
      ENDIF.

    ENDWHILE.
  ENDMETHOD.
  METHOD grabar_documento.
    TYPES: BEGIN OF t_linea,
             c TYPE c LENGTH 65535,
           END OF t_linea.

    DATA: xstring    TYPE xstring,
          l_encoding TYPE abap_encoding,
          converter  TYPE REF TO cl_abap_conv_out_ce.
    DATA: l_error TYPE c LENGTH 1,
          i_tabla TYPE TABLE OF t_linea.

    IF NOT documento-xstring IS INITIAL.
      zcl_ap_ficheros=>grabar_xstring( fichero       = fichero
                                       mostrar_error = 'X'
                                       xstring       = documento-xstring ).
    ELSE.
      IF zcl_c=>get_constante( 'ZDOCUMENTOS_GRABAR_XSTRING' ) = 'X'.
        l_encoding = zcl_c=>get_constante( 'ZDOCUMENTOS_CODEPAGE_OLD' ).
        converter = cl_abap_conv_out_ce=>create( encoding = l_encoding ).
        converter->reset( ).
        TRY.
            converter->write(
                data = documento-string ).
          CATCH cx_root. "#EC *
            l_error = 'X'.
        ENDTRY.

        IF l_error IS INITIAL.
          xstring = converter->get_buffer( ).

          zcl_ap_ficheros=>grabar_xstring( fichero       = fichero
                                  mostrar_error = 'X'
                                  xstring       = xstring ).
          return.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'SWA_STRING_TO_TABLE'
        EXPORTING
          character_string           = documento-string
        IMPORTING
          character_table            = i_tabla
        EXCEPTIONS
          no_flat_charlike_structure = 1
          OTHERS                     = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Error convirtiendo string a tabla' TYPE 'E'.
      ENDIF.

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize            = documento-longitud
          filename                = fichero
          filetype                = 'BIN'
        CHANGING
          data_tab                = i_tabla
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
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD grabar_documento_temporal.
    DATA l_documento TYPE zdocumentos.

    IF documento IS INITIAL.
      SELECT SINGLE fichero longitud nombre xstring string FROM zdocumentos
        INTO CORRESPONDING FIELDS OF l_documento
       WHERE tcode  = tcode
         AND nombre = nombre.
      IF sy-subrc <> 0.
        MESSAGE e398(00) WITH 'No existe doc.'(ned) tcode nombre ''.
      ENDIF.
    ELSE.
      l_documento = documento.
    ENDIF.

    IF NOT l_documento IS INITIAL.
      fichero = get_nombre_fichero_temporal( documento    = l_documento
                                             nuevo_nombre = nuevo_nombre
                                             no_borrar    = no_borrar ).
      IF NOT fichero IS INITIAL.
        grabar_documento( documento    = l_documento
                          fichero      = fichero ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD matchcode_documentos.
    DATA: i_docs  TYPE TABLE OF zdocumentos,
          o_popup TYPE REF TO zcl_ap_matchcode_z,
          l_doc   TYPE zdocumentos.

    SELECT nombre FROM zdocumentos
      INTO CORRESPONDING FIELDS OF TABLE i_docs
     WHERE tcode = tcode.

    o_popup = NEW #( ).

    o_popup->add_field( tabname = 'ZDOCUMENTOS' field = 'NOMBRE'
                        selectflag = 'X' ).

    LOOP AT i_docs INTO l_doc.
      o_popup->add_valor( l_doc-nombre ).
    ENDLOOP.

    o_popup->matchcode( EXPORTING tabname = 'ZDOCUMENTOS'
                                  field   = 'NOMBRE'
                        CHANGING  valor   = nombre ).
  ENDMETHOD.
  METHOD popup_documento.
    CALL FUNCTION 'Z_POPUP_DOCUMENTO'
      EXPORTING
        operacion = operacion
      CHANGING
        documento = documento.
  ENDMETHOD.
  METHOD popup_list.
    TYPES: BEGIN OF t_docs,
             tcode       TYPE zdocumentos-tcode,
             nombre      TYPE zdocumentos-nombre,
             descripcion TYPE zdocumentos-descripcion,
             fichero     TYPE zdocumentos-fichero,
             extension   TYPE zdocumentos-extension,
           END OF t_docs.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA r_clas TYPE RANGE OF zdocumentos-clasificacion.
    DATA: i_docs  TYPE TABLE OF t_docs,
          l_ucomm TYPE sy-ucomm,
          l_fila  TYPE int4.

    IF clasificacion IS SUPPLIED.
      r_clas = VALUE #( ( option = 'EQ' sign = 'I' low = clasificacion ) ).
      get_documentos_por_clas( EXPORTING tcode = CONV #( tcode )
                                         clasificacion = clasificacion
                               IMPORTING i_documentos  = DATA(i_documentos) ).
    ELSE.
      SELECT * FROM zdocumentos
        INTO CORRESPONDING FIELDS OF TABLE i_documentos
        UP TO 20 ROWS " En algunos casos guardamos documentación en esta tabla y puede fallar por memoria si cogemos todo
       WHERE tcode = tcode
        ORDER BY PRIMARY KEY.
    ENDIF.

    IF i_documentos IS INITIAL.
      MESSAGE 'No hay documentos' TYPE 'S'.
    ELSEIF lines( i_documentos ) = 1.
      visualizar_documento( tcode   = i_documentos[ 1 ]-tcode
                            nombre  = i_documentos[ 1 ]-nombre ).
    ELSE.
      MOVE-CORRESPONDING i_documentos TO i_docs.
      DO.
        CALL FUNCTION 'Z_POPUP_ALV_AP'
          EXPORTING
            titulo       = 'Seleccione documento a visualizar'
            campos_noout = 'TCODE,NOMBRE'
            botones      = 'OK_CANCEL'
          IMPORTING
            ucomm        = l_ucomm
            fila         = l_fila
          TABLES
            t_datos      = i_docs.
        IF l_ucomm = 'F03'.
          RETURN.
        ELSEIF l_fila IS INITIAL.
          MESSAGE 'Haga doble click sobre un documento para visualizarlo' TYPE 'S'.
        ELSE.
          visualizar_documento( tcode   = i_docs[ l_fila ]-tcode
                                nombre  = i_docs[ l_fila ]-nombre ).
          RETURN.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar_documento.
    DATA: l_documento TYPE zdocumentos,
          l_fichero   TYPE string.

    SELECT SINGLE fichero longitud nombre xstring string FROM zdocumentos
      INTO CORRESPONDING FIELDS OF l_documento
     WHERE tcode  = tcode
       AND nombre = nombre.
    IF sy-subrc = 0.
      existe = 'X'.
      IF l_documento-longitud = 0.
        l_fichero = l_documento-fichero.
      ELSE.
        l_fichero = grabar_documento_temporal( l_documento ).
      ENDIF.
      IF NOT l_fichero IS INITIAL.
        zcl_ap_gos=>visualizar_fichero_st( l_fichero ).
      ENDIF.
    ELSE.
      CLEAR existe.
    ENDIF.
  ENDMETHOD.
