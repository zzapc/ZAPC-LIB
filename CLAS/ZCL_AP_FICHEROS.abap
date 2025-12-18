CLASS zcl_ap_ficheros DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_filtro_xls  TYPE string VALUE 'Excel |*.XLS*' ##NO_TEXT.
    CONSTANTS c_filtro_txt  TYPE string VALUE 'Texto |*.TXT' ##NO_TEXT.
    CONSTANTS c_filtro_xlsx TYPE string VALUE 'Excel |*.XLSX' ##NO_TEXT.

    CLASS-METHODS lista_ficheros
      IMPORTING !directory        TYPE any
                !filter           TYPE any       DEFAULT '*.*'
                recursivo         TYPE abap_bool DEFAULT ''
                max_ficheros      TYPE i         DEFAULT 9999
      RETURNING VALUE(file_table) TYPE rstt_t_files.

    CLASS-METHODS get_extension
      IMPORTING fichero          TYPE any
      RETURNING VALUE(extension) TYPE string.

    CLASS-METHODS get_nombre_fichero
      IMPORTING fichero       TYPE any OPTIONAL
                con_extension TYPE any DEFAULT ''
          PREFERRED PARAMETER fichero
      RETURNING VALUE(nombre) TYPE string.

    CLASS-METHODS get_directorio_fichero
      IMPORTING fichero           TYPE any OPTIONAL
          PREFERRED PARAMETER fichero
      RETURNING VALUE(directorio) TYPE string.

    CLASS-METHODS lee_fich_servidor
      IMPORTING fichero          TYPE any
                modo_texto       TYPE abap_bool     DEFAULT 'X'
                mostrar_mensajes TYPE abap_bool     DEFAULT ''
                !legacy          TYPE abap_bool     DEFAULT ''
                codepage         TYPE abap_encoding DEFAULT ''
      EXPORTING longitud         TYPE i
                mensaje          TYPE bapi_msg
      CHANGING  tabla            TYPE table.

    CLASS-METHODS limpiar_fichero
      IMPORTING fichero       TYPE string
      RETURNING VALUE(salida) TYPE string.

    CLASS-METHODS ultima_letra_ruta
      IMPORTING directorio   TYPE string
      RETURNING VALUE(letra) TYPE char1.

    CLASS-METHODS concat_ruta
      IMPORTING directorio          TYPE any       DEFAULT ''
                fichero             TYPE any
                !extension          TYPE any       OPTIONAL
                directorio_temporal TYPE abap_bool DEFAULT ''
      RETURNING VALUE(ruta)         TYPE string.

    CLASS-METHODS lee_ficheros_servidor
      IMPORTING  dir_name     TYPE any
                 file_mask    TYPE any               OPTIONAL
                 recursion    TYPE abap_bool         DEFAULT ''
                 solo_dir     TYPE zt_lista_ficheros OPTIONAL
                 max_ficheros TYPE i                 DEFAULT 9999
                 mayusculas   TYPE abap_bool         DEFAULT 'X'
      CHANGING   dir_list     TYPE zt_lista_ficheros
      EXCEPTIONS read_directory_failed
                 empty_directory_list.

    CLASS-METHODS grabar
      IMPORTING fichero                  TYPE any
                tipo                     TYPE char10    DEFAULT 'ASC'
                mostrar_error            TYPE abap_bool DEFAULT 'X'
                dialogo                  TYPE abap_bool DEFAULT ''
                trunc                    TYPE abap_bool DEFAULT 'X'
                bin_filesize             TYPE i         DEFAULT 0
                codepage                 TYPE any       DEFAULT ''
                write_lf_after_last_line TYPE abap_bool DEFAULT 'X'
                write_bom                TYPE abap_bool DEFAULT 'X'
      EXPORTING num_excepcion            TYPE i
                mensaje                  TYPE any
      CHANGING  tabla                    TYPE table
                fichero_dialogo          TYPE any       DEFAULT ''.

    CLASS-METHODS grabar_xml
      IMPORTING fichero         TYPE any
                !string         TYPE string    OPTIONAL
                mostrar_error   TYPE abap_bool DEFAULT 'X'
                codepage        TYPE any       DEFAULT zcl_c=>codepage
                dialogo         TYPE abap_bool DEFAULT ''
                xstring         TYPE xstring   OPTIONAL
                partir_xml      TYPE abap_bool DEFAULT ''
                !local          TYPE abap_bool DEFAULT ''
      EXPORTING num_excepcion   TYPE i
                mensaje         TYPE bapi_msg
                fichero_dialogo TYPE any.

    CLASS-METHODS leer
      IMPORTING fichero       TYPE any
                tipo          TYPE char10    DEFAULT 'ASC'
                mostrar_error TYPE abap_bool DEFAULT 'X'
                codepage      TYPE any       DEFAULT ''
      EXPORTING num_excepcion TYPE i
                mensaje       TYPE bapi_msg
                filelength    TYPE i
      CHANGING  tabla         TYPE table.

    CLASS-METHODS leer_xml
      IMPORTING fichero       TYPE any
                mostrar_error TYPE abap_bool DEFAULT 'X'
      EXPORTING !string       TYPE string
                num_excepcion TYPE i
                mensaje       TYPE bapi_msg.

    CLASS-METHODS popup_select_fichero
      IMPORTING default_extension TYPE string    OPTIONAL
                initial_directory TYPE string    OPTIONAL
                file_filter       TYPE string    OPTIONAL
                servidor          TYPE abap_bool DEFAULT ''
      RETURNING VALUE(fichero)    TYPE string.

    CLASS-METHODS graba_fich_servidor
      IMPORTING  fichero          TYPE any
                 modo_texto       TYPE abap_bool     DEFAULT 'X'
                 mostrar_mensajes TYPE abap_bool     DEFAULT ''
                 !unicode         TYPE abap_bool     DEFAULT ''
                 bin_filesize     TYPE i             DEFAULT 0
                 !string          TYPE string        OPTIONAL
                 codepage         TYPE abap_encoding DEFAULT 'UTF-8'
                 tabla_bin        TYPE abap_bool     DEFAULT ''
                 xstring          TYPE xstring       OPTIONAL
      EXPORTING  mensaje          TYPE bapi_msg
      CHANGING   tabla            TYPE table         OPTIONAL
      EXCEPTIONS error_abrir_fichero
                 error_transfer
                 error_cerrar_fichero.

    CLASS-METHODS existe_directorio
      IMPORTING fichero       TYPE any
      RETURNING VALUE(existe) TYPE abap_bool.

    CLASS-METHODS ver_fichero_texto
      IMPORTING fichero    TYPE any
                !extension TYPE any DEFAULT ''.

    CLASS-METHODS grabar_fichero
      IMPORTING fichero          TYPE any
                modo_texto       TYPE abap_bool     DEFAULT 'X'
                mostrar_mensajes TYPE abap_bool     DEFAULT ''
                !unicode         TYPE abap_bool     DEFAULT 'X'
                trunc            TYPE abap_bool     DEFAULT ''
                binario          TYPE abap_bool     DEFAULT ''
                bin_filesize     TYPE i             DEFAULT 0
                !string          TYPE string        OPTIONAL
                tabla_bin        TYPE abap_bool     DEFAULT ''
                xstring          TYPE xstring       OPTIONAL
                desc_front       TYPE abap_bool     DEFAULT ''
                codepage         TYPE abap_encoding DEFAULT ''
      EXPORTING mensaje          TYPE bapi_msg
      CHANGING  tabla            TYPE table         OPTIONAL.

    CLASS-METHODS get_directorio_temporal
      IMPORTING fichero           TYPE any       DEFAULT ''
                nuevo_si_existe   TYPE abap_bool DEFAULT ''
      RETURNING VALUE(directorio) TYPE string.

    CLASS-METHODS lista_ficheros_comun
      IMPORTING !directory        TYPE any
                !filter           TYPE any       DEFAULT '*.*'
                recursivo         TYPE abap_bool DEFAULT ''
                servidor          TYPE abap_bool DEFAULT ''
                max_ficheros      TYPE i         DEFAULT 9999
                mayusculas        TYPE abap_bool DEFAULT 'X'
                !mask             TYPE any       DEFAULT ''
      RETURNING VALUE(file_table) TYPE rstt_t_files.

    CLASS-METHODS leer_fichero
      IMPORTING fichero       TYPE any
                tipo          TYPE char10    DEFAULT 'ASC'
                mostrar_error TYPE abap_bool DEFAULT 'X'
                codepage      TYPE any       DEFAULT ''
      EXPORTING num_excepcion TYPE i
                mensaje       TYPE bapi_msg
                filelength    TYPE i
      CHANGING  tabla         TYPE table.

    CLASS-METHODS dialogo_grabar_fichero
      IMPORTING fichero_inicial TYPE any
                mostrar_error   TYPE abap_bool DEFAULT ''
      EXPORTING ruta            TYPE string
                mensaje         TYPE any
                num_excepcion   TYPE i.

    CLASS-METHODS grabar_xstring
      IMPORTING mostrar_error TYPE abap_bool     DEFAULT 'X'
                codepage      TYPE abap_encoding DEFAULT '4102'
                xstring       TYPE xstring       OPTIONAL
                fichero       TYPE any           OPTIONAL
                dialogo       TYPE abap_bool     DEFAULT ''
                !string       TYPE string        OPTIONAL
                bin_filesize  TYPE i             DEFAULT 0
                servidor      TYPE abap_bool     DEFAULT ''
                !local        TYPE abap_bool     DEFAULT ''
      EXPORTING num_excepcion TYPE i
                mensaje       TYPE bapi_msg
                fichero_final TYPE any.

    CLASS-METHODS existe_dir
      IMPORTING directorio    TYPE any
                servidor      TYPE abap_bool DEFAULT ''
      RETURNING VALUE(existe) TYPE abap_bool.

    CLASS-METHODS leer_xstring
      IMPORTING fichero              TYPE any       OPTIONAL
                mostrar_error        TYPE abap_bool DEFAULT 'X'
                get_string           TYPE abap_bool DEFAULT ''
                get_tabla            TYPE abap_bool DEFAULT ''
                servidor             TYPE abap_bool DEFAULT ''
                !local               TYPE abap_bool DEFAULT ''
                popup_select_fichero TYPE abap_bool DEFAULT ''
      EXPORTING !string              TYPE string
                num_excepcion        TYPE i
                xstring              TYPE xstring
                !message             TYPE bapi_msg
                longitud             TYPE i
                i_tabla_txt          TYPE table
                fichero_salida       TYPE any.

    CLASS-METHODS borrar_fichero
      IMPORTING fichero        TYPE any
                servidor       TYPE abap_bool DEFAULT ''
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    CLASS-METHODS copia_serv_2_local
      IMPORTING fichero_origen  TYPE any
                fichero_destino TYPE any
                mostrar_error   TYPE abap_bool DEFAULT 'X'
      EXPORTING !message        TYPE bapi_msg
      RETURNING VALUE(error)    TYPE abap_bool.

    CLASS-METHODS copia_local_2_serv
      IMPORTING fichero_origen  TYPE any
                fichero_destino TYPE any
                mostrar_error   TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(error)    TYPE abap_bool.

    CLASS-METHODS buscar_ruta_fichero
      IMPORTING rutas       TYPE any
                fichero     TYPE any DEFAULT ''
      RETURNING VALUE(ruta) TYPE string.

    CLASS-METHODS existe
      IMPORTING fichero       TYPE any
                servidor      TYPE abap_bool DEFAULT ''
      RETURNING VALUE(existe) TYPE abap_bool.

    CLASS-METHODS mover
      IMPORTING fichero             TYPE any
                mostrar_error       TYPE abap_bool DEFAULT 'X'
                directorio_destino  TYPE any
                servidor_orig       TYPE abap_bool DEFAULT ''
                servidor_dest       TYPE abap_bool DEFAULT ''
                crear_dir_dest_serv TYPE abap_bool DEFAULT 'X'
      EXPORTING num_excepcion       TYPE i
                mensaje             TYPE bapi_msg.

    CLASS-METHODS popup_select_directorio
      IMPORTING initial_directory TYPE string    DEFAULT 'C:\'
                titulo            TYPE string    DEFAULT ''
                servidor          TYPE abap_bool DEFAULT ''
      CHANGING  VALUE(directorio) TYPE any.

    CLASS-METHODS visualizar
      IMPORTING fichero        TYPE any
                directorio     TYPE any       DEFAULT ''
                servidor       TYPE abap_bool DEFAULT ''
                xstring        TYPE xstring   OPTIONAL
                bin_filesize   TYPE i         DEFAULT 0
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS borrar_directorio
      IMPORTING directorio     TYPE any
                servidor       TYPE abap_bool DEFAULT ''
      RETURNING VALUE(mensaje) TYPE bapi_msg.

    CLASS-METHODS crear_directorio_serv
      IMPORTING dir            TYPE any
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS get_tipo_ruta
      IMPORTING fichero     TYPE string
                dialogo     TYPE abap_bool DEFAULT ''
                !local      TYPE abap_bool DEFAULT ''
                servidor    TYPE abap_bool DEFAULT ''
      RETURNING VALUE(tipo) TYPE char1.

    CLASS-METHODS get_fichero_var
      IMPORTING fichero       TYPE string
      RETURNING VALUE(salida) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_FICHEROS definition
class ZCL_AP_FICHEROS implementation.
  METHOD borrar_directorio.
    DATA: l_directorio TYPE string,
          l_servidor   TYPE abap_bool,
          l_rc         TYPE i.
    DATA: l_cmd   TYPE text255,
          l_lines TYPE TABLE OF char255.

    l_directorio = directorio.

    IF servidor = 'X'.
      l_servidor = 'X'.
    ENDIF.

    IF l_servidor IS INITIAL.
      cl_gui_frontend_services=>directory_delete(
        EXPORTING
          directory               = l_directorio
        CHANGING
          rc                      = l_rc
        EXCEPTIONS
          directory_delete_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          path_not_found          = 4
          directory_access_denied = 5
          unknown_error           = 6
          not_supported_by_gui    = 7
          wrong_parameter         = 8 ).
      IF sy-subrc <> 0.                                     "#EC *
        mensaje = 'Error borrando directorio'(ebd).         "#EC *
      ENDIF.                                                "#EC *
    ELSE.
      CONCATENATE 'rmdir' l_directorio INTO l_cmd SEPARATED BY space.
      CALL 'SYSTEM' ID 'COMMAND' FIELD l_cmd              "#EC CI_CCALL
           ID 'TAB' FIELD l_lines.

      IF sy-subrc <> 0.                                     "#EC *
        mensaje = 'Error borrando directorio'(ebd).         "#EC *
      ENDIF.                                                "#EC *
    ENDIF.
  ENDMETHOD.
  METHOD borrar_fichero.
    DATA: l_fichero  TYPE string,
          l_servidor TYPE abap_bool,
          l_rc       TYPE i.

    l_fichero = fichero.

    IF servidor = 'X'.
      l_servidor = 'X'.
    ELSEIF NOT l_fichero CS ':'.
      l_servidor = 'X'.
    ENDIF.

    IF l_servidor IS INITIAL.
      cl_gui_frontend_services=>file_delete(
        EXPORTING
          filename             = l_fichero
        CHANGING
          rc                   = l_rc
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
      IF sy-subrc <> 0 OR l_rc <> 0.
        mensaje = 'Error borrando fichero'(ebr).
      ENDIF.
    ELSE.
      DELETE DATASET l_fichero.
      IF sy-subrc <> 0.
        mensaje = 'Error borrando fichero'(ebr).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD buscar_ruta_fichero.
    DATA i_rutas TYPE TABLE OF string.

    FIELD-SYMBOLS <ruta> TYPE string.

    i_rutas = zcl_ap_lista=>lista2tabla( rutas ).

    LOOP AT i_rutas ASSIGNING <ruta>.
      IF fichero IS INITIAL.
        ruta = <ruta>.
        IF existe_directorio( ruta ) = 'X'.
          EXIT.
        ELSE.
          CLEAR ruta.
        ENDIF.
      ELSE.
        ruta = concat_ruta( directorio = <ruta> fichero = fichero ).
        IF existe( ruta ) = 'X'.
          EXIT.
        ELSE.
          CLEAR ruta.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD concat_ruta.
    DATA: l_dir_temp   TYPE string,
          l_directorio TYPE string,
          l_separador  TYPE c LENGTH 1,
          l_ext        TYPE c LENGTH 30.

    IF directorio_temporal = 'X' AND directorio IS INITIAL.
      l_dir_temp = zcl_ap_documentos=>get_directorio_temporal( ).
    ENDIF.

    IF directorio IS INITIAL.
      IF l_dir_temp IS INITIAL.
        ruta = fichero.
      ELSE.
        CONCATENATE l_dir_temp '\' fichero INTO ruta.
      ENDIF.
    ELSE.
      IF directorio IS INITIAL.
        l_directorio = l_dir_temp.
      ELSE.
        l_directorio = directorio.
      ENDIF.

      IF l_directorio CS '/'.
        l_separador = '/'.
      ELSE.
        l_separador = '\'.
      ENDIF.

      IF ultima_letra_ruta( l_directorio ) = l_separador.
        CONCATENATE l_directorio fichero INTO ruta.
      ELSE.
        CONCATENATE l_directorio l_separador fichero INTO ruta.
      ENDIF.
    ENDIF.

    IF NOT extension IS INITIAL.
      CONCATENATE '.' extension INTO l_ext.
      IF NOT fichero CS l_ext.
        CONCATENATE ruta l_ext INTO ruta.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD copia_local_2_serv.
    DATA: path       TYPE sapb-sappfad,
          targetpath TYPE sapb-sappfad.

    path = fichero_origen.
    targetpath = fichero_destino.

    CALL FUNCTION 'ARCHIVFILE_CLIENT_TO_SERVER'
      EXPORTING
        path       = path
        targetpath = targetpath
      EXCEPTIONS
        error_file = 1.
    IF sy-subrc <> 0.
      error = 'X'.
      IF mostrar_error = 'X'.
        MESSAGE e398(00) WITH 'Error al copiar'(eac) path 'a'(aaa) targetpath.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD copia_serv_2_local.
    DATA: path       TYPE sapb-sappfad,
          targetpath TYPE sapb-sappfad.

    path = fichero_origen.
    targetpath = fichero_destino.

    CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
      EXPORTING
        path       = path
        targetpath = targetpath
      EXCEPTIONS
        error_file = 1.
    IF sy-subrc <> 0.
      IF sy-msgty = 'E'.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ELSE.
        message = |Error al copiar { path } a { targetpath }|.
      ENDIF.
      error = 'X'.
      IF mostrar_error = 'X'.
        MESSAGE e398(00) WITH message.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD crear_directorio_serv.
    DATA: l_cmd   TYPE text255,
          l_lines TYPE TABLE OF char255.

    CONCATENATE 'mkdir' dir INTO l_cmd SEPARATED BY space ##NO_TEXT.
    TRY.
        CALL 'SYSTEM' ID 'COMMAND' FIELD l_cmd            "#EC CI_CCALL
             ID 'TAB' FIELD l_lines.

        IF sy-subrc <> 0.
          message = 'Error creando directorio'(ecd).
        ENDIF.
      CATCH cx_root.                                        "#EC *
        message = 'Error creando directorio'(ecd).
    ENDTRY.
  ENDMETHOD.
  METHOD dialogo_grabar_fichero.
    DATA: l_fichero    TYPE string,
          l_extension  TYPE string,
          l_directorio TYPE string,
          l_path       TYPE string,
          l_fullpath   TYPE string.
    DATA l_txt TYPE text1024.

    CLEAR: ruta, mensaje, num_excepcion.

    l_fichero = fichero_inicial.
    l_extension = get_extension( l_fichero ).
    l_directorio = get_directorio_fichero( l_fichero ).
    l_fichero = get_nombre_fichero( l_fichero ).
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
*       window_title         =
        default_extension    = l_extension
        default_file_name    = l_fichero
*       with_encoding        =
*       file_filter          =
        initial_directory    = l_directorio
*       prompt_on_overwrite  = 'X'
      CHANGING
        filename             = l_fichero
        path                 = l_path
        fullpath             = l_fullpath
*       user_action          =
*       file_encoding        =
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      num_excepcion = sy-subrc.
      IF mostrar_error = 'X'.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                INTO mensaje.
        RETURN.
      ENDIF.
    ELSE.
* Los XLSX los acorta a XLS!
      IF strlen( l_extension ) > 3.
        DATA(l_extension_final) = get_extension( l_fullpath ).
        IF strlen( l_extension_final ) = 3.
          DATA(l_long) = strlen( l_fullpath ) - 3.
          l_txt = l_fullpath.
          l_txt+l_long = l_extension.
          l_fullpath = l_txt.
        ENDIF.
      ENDIF.

      l_fichero = l_fullpath.
      ruta = l_fullpath.
      IF l_fullpath IS INITIAL.
        num_excepcion = 99.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD existe.
    DATA l_file TYPE string.

    l_file = fichero.
    CLEAR existe.

    IF get_tipo_ruta( fichero = l_file servidor = servidor dialogo = '' ) = 'L'.
      cl_gui_frontend_services=>file_exist(
        EXPORTING
          file                 = l_file
        RECEIVING
          result               = existe
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5 ).
      IF sy-subrc <> 0.
        CLEAR existe.
      ENDIF.
    ELSE.
      TRY.
          OPEN DATASET l_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
          IF sy-subrc = 0.
            existe = 'X'.
            CLOSE DATASET l_file.
          ENDIF.
        CATCH cx_root.                                      "#EC *
          CLEAR existe.
      ENDTRY.

      IF existe IS INITIAL AND servidor IS INITIAL AND sy-batch IS INITIAL.
        cl_gui_frontend_services=>file_exist(
          EXPORTING
            file                 = l_file
          RECEIVING
            result               = existe
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            wrong_parameter      = 3
            not_supported_by_gui = 4
            OTHERS               = 5 ).
        IF sy-subrc <> 0.
          CLEAR existe.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD existe_dir.
    DATA: l_dir      TYPE string,
          l_name     TYPE salfile-longname,
          i_file_tbl TYPE TABLE OF salfldir.

    CLEAR existe.

    IF get_tipo_ruta( fichero = CONV #( directorio ) servidor = servidor dialogo = '' ) = 'L'.
      l_dir = directorio.
      cl_gui_frontend_services=>directory_exist(
        EXPORTING
          directory            = l_dir
        RECEIVING
          result               = existe
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5 ).
      IF sy-subrc <> 0.
        CLEAR existe.
      ENDIF.
    ELSE.
      l_name = directorio.
      CALL FUNCTION 'RZL_READ_DIR_LOCAL'
        EXPORTING
          name           = l_name
        TABLES
          file_tbl       = i_file_tbl
        EXCEPTIONS
          argument_error = 1
          not_found      = 2
          OTHERS         = 3.
      IF sy-subrc = 0.
        existe = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD existe_directorio.
    DATA l_file TYPE string.

    l_file = fichero.
    CLEAR existe.

    IF get_tipo_ruta( fichero = l_file  ) = 'L'.
      cl_gui_frontend_services=>directory_exist(
        EXPORTING
          directory            = l_file
        RECEIVING
          result               = existe
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5 ).
      IF sy-subrc <> 0.
        CLEAR existe.
      ENDIF.
    ELSE.
      OPEN DATASET l_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc = 0.
        existe = 'X'.
        CLOSE DATASET l_file.
      ELSE.
        existe = existe_dir( directorio = fichero ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_directorio_fichero.
    DATA i_path TYPE text1024.
    DATA e_path TYPE string.

    i_path = fichero.
    CALL FUNCTION 'CACS_SPLIT_PATH'
      EXPORTING
        i_path = i_path
      IMPORTING
        e_path = directorio.
  ENDMETHOD.
  METHOD get_directorio_temporal.
    DATA: l_fichero   TYPE string,
          l_extension TYPE string,
          l_index     TYPE int4.

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
      MESSAGE 'Refresco' TYPE 'S'.
    ENDIF.

    IF NOT fichero IS INITIAL.
      DATA(l_dir) = directorio.
      directorio = concat_ruta( directorio = directorio
                                fichero    = fichero ).

      IF nuevo_si_existe = 'X'.
        CALL FUNCTION 'CRM_EMAIL_SPLIT_FILENAME'
          EXPORTING
            iv_path      = fichero
          IMPORTING
            ev_filename  = l_fichero
            ev_extension = l_extension.

        DO.
          IF existe( directorio ).
            l_index = l_index + 1.
            DATA(l_fichero2) = |{ l_fichero } ({ l_index }).{ l_extension }|.
            directorio = concat_ruta( directorio = l_dir
                                      fichero    = l_fichero2 ).
          ELSE.
            RETURN.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_extension.
    DATA: l_filename  TYPE c LENGTH 1000,
          l_extension TYPE c LENGTH 30.

    IF fichero NS '.'.
      RETURN.
    ENDIF.

    l_filename = fichero.
    l_filename = to_upper( l_filename ).
    CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
      EXPORTING
        filename  = l_filename
        uppercase = 'X'
      IMPORTING
        extension = l_extension.
    extension = l_extension.

    IF extension = 'BIN'.
      IF zcl_ap_string=>right( entrada = l_filename long = 3 ) <> 'BIN'.
        CLEAR l_extension.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_fichero_var.
    DATA l_fecha TYPE c LENGTH 10.

    salida = fichero.
    DATA(var) = zcl_ap_regexp=>buscar_llaves( fichero ).

    LOOP AT var ASSIGNING FIELD-SYMBOL(<var>).
      TRANSLATE <var> TO UPPER CASE.
      CASE <var>.
        WHEN '{FECHA}'.
          WRITE sy-datum TO l_fecha.
          REPLACE ALL OCCURRENCES OF '.' IN l_fecha WITH '-'.
          REPLACE ALL OCCURRENCES OF <var> IN salida WITH l_fecha IGNORING CASE.
        WHEN '{SY-DATUM}' OR '{SY_DATUM}'.
          REPLACE ALL OCCURRENCES OF <var> IN salida WITH sy-datum IGNORING CASE.
        WHEN '{HORA}' OR '{SY-UZEIT}' OR '{SY_UZEIT}'.
          REPLACE ALL OCCURRENCES OF <var> IN salida WITH sy-uzeit IGNORING CASE.
        WHEN '{USUARIO}' OR '{SY-UNAME}' OR '{SY_UNAME}'.
          REPLACE ALL OCCURRENCES OF <var> IN salida WITH sy-uname IGNORING CASE.
        WHEN '{SISTEMA}' OR '{SY-SYSID}' OR '{SY_SYSID}'.
          REPLACE ALL OCCURRENCES OF <var> IN salida WITH sy-sysid IGNORING CASE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_nombre_fichero.
    DATA: iv_path      TYPE string,
          ev_filename  TYPE string,
          ev_extension TYPE string.

    iv_path = fichero.
    CALL FUNCTION 'CRM_EMAIL_SPLIT_FILENAME'
      EXPORTING
        iv_path      = iv_path
      IMPORTING
        ev_filename  = ev_filename
        ev_extension = ev_extension.

    IF con_extension IS INITIAL.
      nombre = ev_filename.
    ELSE.
      CONCATENATE ev_filename ev_extension INTO nombre SEPARATED BY '.'.
    ENDIF.
  ENDMETHOD.
  METHOD get_tipo_ruta.
    DATA l_local           TYPE char1.
    DATA lv_update_process TYPE sy-subrc.

    CLEAR tipo.
    DATA(l_fichero) = to_lower( fichero ).
    IF servidor = 'X'.
      tipo = 'S'.
    ELSEIF servidor = 'N'.
      tipo = 'L'.
    ELSEIF dialogo = 'X'.
      tipo = 'X'.
    ELSEIF sy-batch = 'X'.
      tipo = 'S'.
    ELSEIF local = 'X'.
      tipo = 'L'.
    ELSEIF fichero CS ':'.
      tipo = 'L'.
    ELSE.
      TRY.
          CALL METHOD ('ZCL_FICHEROS')=>es_ruta_local
            EXPORTING
              fichero = l_fichero
            RECEIVING
              local   = l_local.
          IF l_local = 'X'.
            tipo = 'L'.
          ELSE.
            tipo = 'S'.
          ENDIF.
        CATCH cx_root INTO DATA(o_root). "#EC * " TODO: variable is assigned but never used (ABAP cleaner)
          IF fichero CS '/'.
            tipo = 'S'.
          ELSE.
            CALL FUNCTION 'TH_IN_UPDATE_TASK'             " En fondo asumimos que siempre es en el servidor
              IMPORTING
                in_update_task = lv_update_process.
            IF lv_update_process = 0.
              tipo = 'L'.
            ELSE.
              tipo = 'S'.
            ENDIF.
          ENDIF.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD graba_fich_servidor.
    DATA: mess          TYPE string,
          l_dir         TYPE string,
          l_xstring     TYPE xstring,
          l_string      TYPE string,
          l_linea       TYPE c LENGTH 65535,
          cvto_utf8     TYPE REF TO cl_abap_conv_out_ce,
          l_long        TYPE i,
          l_err_file_io TYPE REF TO cx_sy_file_io.

    FIELD-SYMBOLS <linea> TYPE any.

    IF modo_texto = 'X' AND tabla_bin IS INITIAL.
      IF unicode IS INITIAL.
        OPEN DATASET fichero FOR OUTPUT IN TEXT MODE ENCODING NON-UNICODE MESSAGE mess.
      ELSE.
        OPEN DATASET fichero FOR OUTPUT IN TEXT MODE ENCODING UTF-8 MESSAGE mess.
      ENDIF.
    ELSE.
      OPEN DATASET fichero FOR OUTPUT IN BINARY MODE.
      IF sy-subrc = 8.
        l_dir = get_directorio_fichero( fichero ).
        IF existe_directorio( l_dir ) = ''.
          mess = crear_directorio_serv( l_dir ).
          IF mess IS INITIAL.
            OPEN DATASET fichero FOR OUTPUT IN BINARY MODE.
          ELSE.
            IF mostrar_mensajes = 'X'.
              MESSAGE e398(00) WITH 'No existe el directorio'(ned) l_dir mess ''.
            ELSE.
              __concat3 mensaje 'No existe el directorio'(ned) l_dir mess.
              RAISE error_abrir_fichero.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF sy-subrc <> 0.
      IF mostrar_mensajes = 'X'.
        MESSAGE e093(ch) WITH fichero.
      ELSE.
        MESSAGE e093(ch) WITH fichero INTO mensaje.
      ENDIF.
      RAISE error_abrir_fichero.
    ENDIF.

    IF modo_texto = '' AND unicode = 'X'.
      IF tabla_bin IS INITIAL.
        IF NOT xstring IS INITIAL.
          l_xstring = xstring.
        ELSE.
          IF NOT string IS INITIAL.
            l_string = string.
          ELSE.
            l_string = zcl_ap_string=>tabla2string( tabla ).
          ENDIF.
          IF     NOT bin_filesize IS INITIAL
             AND NOT string       IS INITIAL. " Este ajuste no funciona si viene de tabla
            l_linea = l_string.
            l_linea+bin_filesize = cl_abap_char_utilities=>newline.
            l_string = l_linea.
          ENDIF.
*    TRANSFER l_string TO fichero. "LENGTH l_long.
          cvto_utf8 = cl_abap_conv_out_ce=>create( encoding = codepage ).
          TRY.
              cvto_utf8->write( data = l_string ).
            CATCH cx_sy_conversion_codepage.
              mensaje = 'Error de conversion'(erc).
          ENDTRY.

          l_xstring = cvto_utf8->get_buffer( ).
        ENDIF.

        l_long = xstrlen( l_xstring ).
*    l_xstring = zcl_ap_string=>string2xstring( l_string ).
      ELSE.
        l_long = bin_filesize.
        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = bin_filesize
*           FIRST_LINE   = 0
*           LAST_LINE    = 0
          IMPORTING
            buffer       = l_xstring
          TABLES
            binary_tab   = tabla
          EXCEPTIONS
            failed       = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.
          IF mostrar_mensajes = 'X'.
            MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ELSE.
            MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO mensaje.
          ENDIF.
        ENDIF.

      ENDIF.
      TRANSFER l_xstring TO fichero LENGTH l_long.
    ELSE.
      LOOP AT tabla ASSIGNING <linea>.
        l_linea = <linea>.
        TRY.
            IF modo_texto = 'X'.
              TRANSFER <linea> TO fichero.
            ELSE.
              IF unicode = 'X'.
                cvto_utf8 = cl_abap_conv_out_ce=>create( encoding = codepage ).
                l_long = strlen( l_linea ).
                cvto_utf8->write( data = l_linea(l_long) ).
                l_xstring = cvto_utf8->get_buffer( ).
                l_long = xstrlen( l_xstring ).
                TRANSFER l_xstring TO fichero LENGTH l_long.
              ELSE.
                l_long = strlen( l_linea ).
                TRANSFER l_linea TO fichero LENGTH l_long.
              ENDIF.
            ENDIF.
          CATCH cx_sy_conversion_codepage.
            l_string = l_linea.
            zcl_ap_string=>quitar_caracteres_extranos( CHANGING string = l_string ).
            l_linea = l_string.
            TRANSFER l_linea TO fichero.
          CATCH cx_sy_file_io INTO l_err_file_io.
            mensaje = l_err_file_io->get_text( ).
            EXIT.
        ENDTRY.
        IF sy-subrc <> 0.
          IF mostrar_mensajes = 'X'.
            MESSAGE e094(ch) WITH fichero.
          ELSE.
            MESSAGE e094(ch) WITH fichero INTO mensaje.
          ENDIF.
          RAISE error_transfer.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLOSE DATASET fichero.
    IF sy-subrc <> 0.
      IF mostrar_mensajes = 'X'.
        MESSAGE e411(tl) WITH fichero.
      ELSE.
        MESSAGE e411(tl) WITH fichero INTO mensaje.
      ENDIF.
      RAISE error_cerrar_fichero.
    ENDIF.
  ENDMETHOD.
  METHOD grabar.
    DATA: l_fichero      TYPE string,
          l_bin_filesize TYPE i,
          l_dat_mode     TYPE c LENGTH 1,
          l_codepage     TYPE abap_encoding,
          lx_root        TYPE REF TO cx_root.

    CLEAR: mensaje, num_excepcion.

    l_fichero = fichero.
    IF dialogo = 'X'.
      dialogo_grabar_fichero( EXPORTING fichero_inicial = l_fichero
                                        mostrar_error   = mostrar_error
                              IMPORTING ruta            = l_fichero
                                        mensaje         = mensaje
                                        num_excepcion   = num_excepcion ).

      fichero_dialogo = l_fichero.
      IF NOT num_excepcion IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF tipo = 'BIN'.
      l_bin_filesize = bin_filesize.
    ENDIF.
    IF tipo = 'DAT'.
      l_dat_mode = 'X'.
    ENDIF.

    l_codepage = codepage.
    TRY.
        cl_gui_frontend_services=>gui_download(
          EXPORTING
            bin_filesize              = l_bin_filesize
            filename                  = l_fichero
            filetype                  = tipo
*           append                    = SPACE
*           write_field_separator     = SPACE
*           header                    = '00'
*           trunc_trailing_blanks     = space
*           write_lf                  = 'X'
*           col_select                = SPACE
*           col_select_mask           = SPACE
            dat_mode                  = l_dat_mode
*           confirm_overwrite         = SPACE
*           no_auth_check             = SPACE
            codepage                  = l_codepage
*           ignore_cerr               = ABAP_TRUE
*           replacement               = '#'
            write_bom                 = write_bom
            trunc_trailing_blanks_eol = trunc
            write_lf_after_last_line  = write_lf_after_last_line
*           wk1_n_format              = SPACE
*           wk1_n_size                = SPACE
*           wk1_t_format              = SPACE
*           wk1_t_size                = SPACE
*    IMPORTING
*           filelength                =
          CHANGING
            data_tab                  = tabla
          EXCEPTIONS
            file_write_error          = 1
            no_batch                  = 2
            gui_refuse_filetransfer   = 3
            invalid_type              = 4
            no_authority              = 5
            unknown_error             = 6
            header_not_allowed        = 7
            separator_not_allowed     = 8
            filesize_not_allowed      = 9
            header_too_long           = 10
            dp_error_create           = 11
            dp_error_send             = 12
            dp_error_write            = 13
            unknown_dp_error          = 14
            access_denied             = 15
            dp_out_of_memory          = 16
            disk_full                 = 17
            dp_timeout                = 18
            file_not_found            = 19
            dataprovider_exception    = 20
            control_flush_error       = 21
            not_supported_by_gui      = 22
            error_no_gui              = 23
            OTHERS                    = 24 ).
        IF sy-subrc <> 0.
          num_excepcion = sy-subrc.
        ENDIF.
      CATCH cx_root INTO lx_root.                           "#EC *
        sy-subrc = 1001.
        mensaje = lx_root->if_message~get_text( ).
    ENDTRY.

    IF mostrar_error = 'X'.
      IF num_excepcion <> 0.
        CASE num_excepcion.
          WHEN 1.                                           "#EC *
            MESSAGE e153(14) WITH fichero.                  "#EC *
          WHEN OTHERS.                                      "#EC *
            MESSAGE e153(14) WITH fichero.                  "#EC *
        ENDCASE.
      ENDIF.
    ELSE.
      IF num_excepcion <> 0 AND mensaje IS INITIAL.
        CASE num_excepcion.
          WHEN 1.                                           "#EC *
            MESSAGE e153(14) WITH fichero INTO mensaje.     "#EC *
          WHEN OTHERS.                                      "#EC *
            MESSAGE e153(14) WITH fichero INTO mensaje.     "#EC *
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD grabar_fichero.
    DATA: l_tipo       TYPE c LENGTH 10,
          l_modo_texto TYPE c LENGTH 1.

    IF binario = 'X'.
      l_tipo = 'BIN'.
    ELSE.
      l_modo_texto = 'X'.
      l_tipo = 'ASC'.
    ENDIF.

    IF get_tipo_ruta( fichero = CONV #( fichero ) local = desc_front   ) = 'L'.
      IF NOT xstring IS INITIAL.
        grabar_xstring( EXPORTING fichero       = fichero
                                  mostrar_error = mostrar_mensajes
                                  bin_filesize  = bin_filesize
                                  xstring       = xstring
                                  codepage      = codepage
                        IMPORTING mensaje       = mensaje ).
      ELSE.
        grabar( EXPORTING fichero       = fichero
                          mostrar_error = mostrar_mensajes
                          trunc         = trunc
                          bin_filesize  = bin_filesize
                          tipo          = l_tipo
                          codepage      = codepage
                IMPORTING mensaje       = mensaje
                CHANGING  tabla         = tabla ).
      ENDIF.
    ELSE.
      graba_fich_servidor( EXPORTING  fichero              = fichero
                                      mostrar_mensajes     = mostrar_mensajes
                                      unicode              = unicode
                                      modo_texto           = l_modo_texto
                                      bin_filesize         = bin_filesize
                                      string               = string
                                      xstring              = xstring
                                      tabla_bin            = tabla_bin
                           IMPORTING  mensaje              = mensaje
                           CHANGING   tabla                = tabla
                           EXCEPTIONS error_abrir_fichero  = 1
                                      error_transfer       = 2
                                      error_cerrar_fichero = 3
                                      OTHERS               = 4 ).
      IF sy-subrc <> 0.
        IF mensaje IS INITIAL.
          mensaje = |Error grabando fichero { fichero }|.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD grabar_xml.
    DATA: tempstring     TYPE string,
          l_long         TYPE i,
          temptable_char TYPE table_of_strings,
          l_unicode      TYPE c LENGTH 1.

    FIELD-SYMBOLS <string> TYPE string.

    CLEAR: num_excepcion, mensaje, fichero_dialogo.

    IF NOT string IS INITIAL.
      tempstring = string.
      l_long = strlen( string ).
    ELSE.
      tempstring = zcl_ap_string=>xstring2string( xstring ).
      l_long = xstrlen( xstring ).
    ENDIF.

    IF partir_xml IS INITIAL.
      SPLIT tempstring AT cl_abap_char_utilities=>newline INTO TABLE temptable_char.
    ELSE.
      temptable_char = zcl_ap_ws=>xml_str2table( string ).
    ENDIF.

* Eliminamos posible carÃ¡cter extraÃ±o al inicio del fichero.
    ASSIGN temptable_char[ 1 ] TO <string>.
    IF sy-subrc = 0.
      l_long = strlen( <string> ).
      IF l_long > 1.
        IF <string>(1) <> '<'.
          <string> = <string>+1.
        ENDIF.
      ENDIF.
    ENDIF.

    IF fichero CS ':' OR dialogo = 'X' OR local = 'X'.
      grabar( EXPORTING fichero         = fichero
                        tipo            = 'DAT'
                        mostrar_error   = 'X'
                        codepage        = codepage
                        dialogo         = dialogo
              IMPORTING num_excepcion   = num_excepcion
              CHANGING  tabla           = temptable_char
                        fichero_dialogo = fichero_dialogo ).
    ELSE.
      IF codepage = '4110'.
        l_unicode = 'X'.
      ENDIF.
      zcl_ap_ficheros=>grabar_fichero( EXPORTING fichero      = fichero
                                                 binario      = 'X'
                                                 bin_filesize = l_long
                                                 unicode      = l_unicode
                                       IMPORTING mensaje      = mensaje
                                       CHANGING  tabla        = temptable_char ).
    ENDIF.
  ENDMETHOD.
  METHOD grabar_xstring.
    DATA: l_xstring    TYPE xstring,
          l_filelength TYPE i,
          i_tabla      TYPE STANDARD TABLE OF x255,
          l_fichero    TYPE string,
          l_subrc      TYPE sy-subrc,
          l_dir        TYPE string,
          l_lineas     TYPE i,
          l_resto      TYPE i,
          l_tabla      TYPE x255,
          l_last       TYPE c LENGTH 1,
          l_long       TYPE i,
          l_servidor   TYPE abap_bool.

    CLEAR: mensaje, num_excepcion, fichero_final.

    IF NOT xstring IS INITIAL.
      l_xstring = xstring.
    ELSE.
      l_xstring = zcl_ap_string=>string2xstring( string ).
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = l_xstring
      IMPORTING
        output_length = l_filelength
      TABLES
        binary_tab    = i_tabla.

    IF NOT bin_filesize IS INITIAL.
      l_filelength = bin_filesize.
    ENDIF.

    l_fichero = get_fichero_var( fichero = CONV #( fichero ) ).

    DATA(l_tipo_ruta) = get_tipo_ruta( fichero = CONV #( fichero ) servidor = servidor local = local dialogo = dialogo ).
    IF    l_tipo_ruta = 'L'  " Local
       OR l_tipo_ruta = 'X'.    " Diálogo
      grabar( EXPORTING fichero         = l_fichero
                        tipo            = 'BIN'
                        mostrar_error   = mostrar_error
                        codepage        = codepage
                        dialogo         = dialogo
                        bin_filesize    = l_filelength
              IMPORTING num_excepcion   = num_excepcion
                        mensaje         = mensaje
              CHANGING  tabla           = i_tabla
                        fichero_dialogo = fichero_final ).
    ELSE.
*Download file to the application server
      TRY.
          OPEN DATASET l_fichero FOR OUTPUT IN BINARY MODE.
          l_subrc = sy-subrc.
          IF sy-subrc = 8.
            l_dir = get_directorio_fichero( l_fichero ).
            IF existe_directorio( l_dir ) = ''.
              mensaje = crear_directorio_serv( l_dir ).
              IF mensaje IS INITIAL.
                OPEN DATASET l_fichero FOR OUTPUT IN BINARY MODE.
                l_subrc = sy-subrc.
              ELSE.
                __concat3 mensaje 'No existe el directorio'(ned) l_dir mensaje.
                RETURN.
              ENDIF.
            ENDIF.
          ENDIF.

          IF l_subrc <> 0.
            mensaje = 'Error al abrir fichero'(eaf).
          ELSE.
            l_lineas = lines( i_tabla ).
            l_resto = l_filelength.
            LOOP AT i_tabla INTO l_tabla.
              AT LAST.
                l_last = 'X'.
              ENDAT.

              IF l_lineas = 1.
                TRANSFER l_tabla TO l_fichero LENGTH l_filelength.
              ELSEIF l_last = 'X'.
                TRANSFER l_tabla TO l_fichero LENGTH l_resto.
              ELSE.
                TRANSFER l_tabla TO l_fichero.
                l_long = xstrlen( l_tabla ).
                l_resto = l_resto - l_long.
              ENDIF.
              IF sy-subrc <> 0.
                mensaje = 'Error al escribir fichero'(eef).
                EXIT.
              ENDIF.
            ENDLOOP.
            IF mensaje IS INITIAL.
              TRY.
                  CLOSE DATASET l_fichero.
                  IF sy-subrc <> 0.
                    mensaje = 'Error al cerrar fichero'(ecf).
                  ENDIF.
                CATCH cx_root INTO DATA(o_root).            "#EC *
                  mensaje = o_root->get_text( ).
              ENDTRY.
            ENDIF.
          ENDIF.

        CATCH cx_sy_file_authority.
          CONCATENATE 'No autorizado a abrir el fichero'(naf) l_fichero INTO mensaje SEPARATED BY space.
        CATCH cx_root INTO o_root.                          "#EC *
          mensaje = o_root->get_text( ).
      ENDTRY.
    ENDIF.

    IF fichero_final IS INITIAL.
      fichero_final = l_fichero.
    ENDIF.

    IF mostrar_error = 'X' AND NOT mensaje IS INITIAL.
      MESSAGE e153(14) WITH l_fichero.
    ENDIF.
  ENDMETHOD.
  METHOD lee_fich_servidor.
    DATA: l_long  TYPE i,
          l_linea TYPE c LENGTH 65535.

    CLEAR : mensaje, longitud.
    IF modo_texto = 'X'.
      IF legacy IS INITIAL.
        IF codepage = '4110'.
          OPEN DATASET fichero FOR INPUT IN TEXT MODE ENCODING UTF-8.
        ELSEIF codepage <> ''.
          OPEN DATASET fichero FOR INPUT IN TEXT MODE ENCODING NON-UNICODE.
        ELSE.
          OPEN DATASET fichero FOR INPUT IN TEXT MODE ENCODING DEFAULT.
        ENDIF.
      ELSE.
        IF codepage IS INITIAL.
          OPEN DATASET fichero FOR INPUT IN LEGACY TEXT MODE.
        ELSE.
          OPEN DATASET fichero FOR INPUT IN LEGACY TEXT MODE CODE PAGE codepage.
        ENDIF.
      ENDIF.
    ELSE.
      OPEN DATASET fichero FOR INPUT IN BINARY MODE.
    ENDIF.
    IF sy-subrc <> 0.
      IF mostrar_mensajes = 'X'.
        MESSAGE e025(ba) WITH fichero.
      ELSE.
        MESSAGE e025(ba) WITH fichero INTO mensaje.
        RETURN.
      ENDIF.
    ENDIF.

    FREE tabla.
    DO.
      CLEAR l_long.
      TRY.
          CLEAR l_linea.
          READ DATASET fichero INTO l_linea LENGTH l_long.
          IF sy-subrc <> 0 AND l_long = 0.
            EXIT.
          ELSE.
            APPEND l_linea TO tabla.
            longitud = longitud + l_long.
          ENDIF.
        CATCH cx_sy_file_open_mode.
          IF mostrar_mensajes = 'X'.
            MESSAGE e151(0d) WITH fichero.
          ELSE.
            MESSAGE e151(0d) WITH fichero INTO mensaje.
            EXIT.
          ENDIF.
        CATCH cx_sy_conversion_codepage.
          IF l_linea IS INITIAL.
            IF mostrar_mensajes = 'X'.
              MESSAGE e202(0k) WITH fichero.
            ELSE.
              MESSAGE e202(0k) WITH fichero INTO mensaje.
              EXIT.
            ENDIF.
          ELSE.
            APPEND l_linea TO tabla.
            longitud = longitud + l_long.
            mensaje = 'Problemas conversion codepage'(pcc).
          ENDIF.
      ENDTRY.
    ENDDO.

    CLOSE DATASET fichero.
    IF sy-subrc <> 0.
      IF mostrar_mensajes = 'X'.
        MESSAGE e804(fu) WITH fichero.
      ELSE.
        MESSAGE e804(fu) WITH fichero INTO mensaje.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD lee_ficheros_servidor.
    DATA file_counter TYPE i.
    DATA: i_dir      TYPE zt_lista_ficheros,
          l_dir_list TYPE zt_lista_ficheros.
    DATA l_dir_name TYPE c LENGTH 75.
    DATA: BEGIN OF file,
            dirname TYPE c LENGTH 75,            " name of directory. (possibly truncated.)
            name    TYPE c LENGTH 75,            " name of entry. (possibly truncated.)
            type    TYPE c LENGTH 10,            " type of entry.
            len     TYPE p LENGTH 8  DECIMALS 0, " length in bytes.
            owner   TYPE c LENGTH 8,             " owner of the entry.
            mtime   TYPE p LENGTH 6  DECIMALS 0, " last modification date, seconds since 1970
            mode    TYPE c LENGTH 9,             " like "rwx-r-x--x": protection mode.
            errno   TYPE c LENGTH 3,
            errmsg  TYPE c LENGTH 40,
          END OF file,
          l_dir  TYPE zlista_ficheros,
          l_time TYPE c LENGTH 8,
          l_cont TYPE i.

* get directory listing
    CALL 'C_DIR_READ_FINISH'                              "#EC CI_CCALL
         ID 'ERRNO'  FIELD file-errno
         ID 'ERRMSG' FIELD file-errmsg.

    CALL 'C_DIR_READ_START'                               "#EC CI_CCALL
         ID 'DIR'    FIELD dir_name
         ID 'FILE'   FIELD file_mask
         ID 'ERRNO'  FIELD file-errno
         ID 'ERRMSG' FIELD file-errmsg.
    IF sy-subrc <> 0.
      RAISE read_directory_failed.
    ENDIF.

    CLEAR dir_list.
    DO.
      IF l_cont > max_ficheros.
        EXIT.
      ENDIF.

      CLEAR file.
      CLEAR l_dir.
      CALL 'C_DIR_READ_NEXT'                              "#EC CI_CCALL
           ID 'TYPE'   FIELD file-type
           ID 'NAME'   FIELD file-name
           ID 'LEN'    FIELD file-len
           ID 'OWNER'  FIELD file-owner
           ID 'MTIME'  FIELD file-mtime
           ID 'MODE'   FIELD file-mode
           ID 'ERRNO'  FIELD file-errno
           ID 'ERRMSG' FIELD file-errmsg.

      IF    sy-subrc = 0
         OR sy-subrc = 4. " Filename too long!

*      IF file-type(1) = 'f' OR              " regular file
*         file-type(1) = 'F'.
        IF file-name(1) <> '.'.
          IF file-type = 'directory'.
            IF mayusculas = 'X'.
              TRANSLATE file-name TO UPPER CASE.
            ENDIF.
            IF NOT line_exists( solo_dir[ dirname = file-name ] ) AND NOT solo_dir IS INITIAL. "#EC CI_STDSEQ
              CONTINUE.
            ENDIF.
          ENDIF.

          MOVE-CORRESPONDING file TO l_dir.
          PERFORM p6_to_date_time_tz IN PROGRAM rstr0400 USING file-mtime
                                                     l_time
                                                     l_dir-mod_date.
          IF l_time CS ':'.
            REPLACE ':' WITH '' INTO l_time.
            REPLACE ':' WITH '' INTO l_time.
            CONDENSE l_time NO-GAPS.
          ENDIF.
          l_dir-mod_time = l_time.

          file_counter = file_counter + 1.
          l_dir-dirname   = dir_name.
          l_dir-procesado = 'X'.
* APC170108 Para homogeneizar, devolvemos los ficheros siempre
* en mayÃºsculas
          IF mayusculas = 'X'.
            TRANSLATE l_dir-name TO UPPER CASE.
            TRANSLATE l_dir-dirname TO UPPER CASE.
          ENDIF.
          APPEND l_dir TO dir_list.
          l_cont = l_cont + 1.
        ENDIF.
      ELSEIF sy-subrc = 1.
        EXIT.
      ELSE.
* APC170108 Para homogeneizar, devolvemos los ficheros siempre
* en mayÃºsculas
        IF mayusculas = 'X'.
          TRANSLATE l_dir-name TO UPPER CASE.
          TRANSLATE l_dir-dirname TO UPPER CASE.
        ENDIF.
        APPEND l_dir TO dir_list.
        l_cont = l_cont + 1.
      ENDIF.
    ENDDO.

    CALL 'C_DIR_READ_FINISH'                              "#EC CI_CCALL
         ID 'ERRNO'  FIELD file-errno
         ID 'ERRMSG' FIELD file-errmsg.

    IF file_counter > 0.
      SORT dir_list BY name ASCENDING.
    ELSE.
      RAISE empty_directory_list.
    ENDIF.

    IF recursion = 'X'.
      LOOP AT dir_list INTO l_dir WHERE     type      = 'directory' "#EC CI_STDSEQ
                                        AND procesado = 'X'.
        TRANSLATE l_dir-name TO UPPER CASE.
        TRANSLATE l_dir-dirname TO UPPER CASE.
        APPEND l_dir TO i_dir.
        CLEAR l_dir-procesado.
        MODIFY dir_list FROM l_dir.
      ENDLOOP.
      LOOP AT i_dir INTO l_dir.
        CONCATENATE dir_name '\' l_dir-name INTO l_dir_name.
        REFRESH l_dir_list.
        lee_ficheros_servidor(
          EXPORTING
            dir_name              = l_dir_name
            file_mask             = file_mask
            recursion             = recursion
            solo_dir              = solo_dir
            max_ficheros          = max_ficheros
            mayusculas            = mayusculas
          CHANGING
            dir_list              = l_dir_list
          EXCEPTIONS
            read_directory_failed = 1
            empty_directory_list  = 2 ).
        IF sy-subrc = 0.
          LOOP AT l_dir_list INTO l_dir.
            TRANSLATE l_dir-name TO UPPER CASE.
            TRANSLATE l_dir-dirname TO UPPER CASE.
            APPEND l_dir TO dir_list.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD leer.
    DATA: l_tipo    TYPE char10,
          l_sep     TYPE c LENGTH 1,
          l_fichero TYPE string.

    l_tipo = tipo.
    IF tipo = 'DAT'.
      l_sep = 'X'.
      l_tipo = 'ASC'.
    ENDIF.

    l_fichero = fichero.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = l_fichero
        filetype                = l_tipo
        has_field_separator     = l_sep
*       header_length           = 0
*       read_by_line            = 'X'
        dat_mode                = l_sep
        codepage                = codepage
*       ignore_cerr             = ABAP_TRUE
*       replacement             = '#'
*       virus_scan_profile      =
      IMPORTING
        filelength              = filelength
*       header                  =
      CHANGING
        data_tab                = tabla
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
    num_excepcion = sy-subrc.

    IF mostrar_error = 'X'.
      IF num_excepcion <> 0.
        CASE num_excepcion.
          WHEN 1.
            MESSAGE e503(0u) WITH fichero.
          WHEN 8.
            MESSAGE 'Error en formato de datos'(efd) TYPE 'E'.
          WHEN OTHERS.
            MESSAGE e503(0u) WITH fichero.
        ENDCASE.
      ENDIF.
    ELSE.
      IF num_excepcion <> 0.
        CASE num_excepcion.
          WHEN 1.
            MESSAGE i503(0u) WITH fichero INTO mensaje.
          WHEN 8.
            mensaje = 'Error en formato de datos'(efd).
          WHEN OTHERS.
            MESSAGE i503(0u) WITH fichero INTO mensaje.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD leer_fichero.
    DATA l_modo_texto TYPE c LENGTH 1.

    IF get_tipo_ruta( fichero = CONV #( fichero ) ) = 'L'.
      leer( EXPORTING fichero       = fichero
                      tipo          = tipo
                      codepage      = codepage
                      mostrar_error = mostrar_error
            IMPORTING num_excepcion = num_excepcion
                      mensaje       = mensaje
                      filelength    = filelength
            CHANGING  tabla         = tabla ).
    ELSE.
      IF tipo <> 'BIN'.
        l_modo_texto = 'X'.
      ENDIF.
      lee_fich_servidor( EXPORTING fichero          = fichero
                                   modo_texto       = l_modo_texto
                                   mostrar_mensajes = mostrar_error
                                   codepage         = codepage
                         IMPORTING mensaje          = mensaje
                                   longitud         = filelength
                         CHANGING  tabla            = tabla ).
      IF mensaje CS 'codepage'.
        CLEAR mensaje.
        lee_fich_servidor( EXPORTING fichero          = fichero
                                     modo_texto       = l_modo_texto
                                     mostrar_mensajes = mostrar_error
                                     codepage         = '4110'
                           IMPORTING mensaje          = mensaje
                                     longitud         = filelength
                           CHANGING  tabla            = tabla ).
        IF mensaje CS 'codepage' AND l_modo_texto = 'X'.
          CLEAR l_modo_texto.
          CLEAR mensaje.
          lee_fich_servidor( EXPORTING fichero          = fichero
                                       modo_texto       = l_modo_texto
                                       mostrar_mensajes = mostrar_error
                             IMPORTING mensaje          = mensaje
                                       longitud         = filelength
                             CHANGING  tabla            = tabla ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD leer_xml.
    DATA: temptable  TYPE table_of_strings,
          tempstring TYPE string,
          l_last     TYPE c LENGTH 1.

    CLEAR string.

    IF fichero CS ':'.
      leer( EXPORTING fichero       = fichero
                      tipo          = 'DAT'
                      mostrar_error = mostrar_error
            IMPORTING num_excepcion = num_excepcion
            CHANGING  tabla         = temptable ).
    ELSE.
      leer_fichero( EXPORTING fichero       = fichero
                              mostrar_error = mostrar_error
                    IMPORTING mensaje       = mensaje
                    CHANGING  tabla         = temptable ).
    ENDIF.

    LOOP AT temptable INTO tempstring.
      AT LAST.
        l_last = 'X'.
      ENDAT.
      IF l_last IS INITIAL.
        CONCATENATE string tempstring cl_abap_char_utilities=>newline
                    INTO string.
      ELSE.
        CONCATENATE string tempstring INTO string.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD leer_xstring.
    DATA: l_fichero  TYPE string,
          l_serv     TYPE abap_bool,
          i_tabla    TYPE STANDARD TABLE OF x255,
          l_long     TYPE i,
          l_err_root TYPE REF TO cx_root,
          i_solix    TYPE solix_tab.

    CLEAR: string, num_excepcion, xstring, message, longitud, i_tabla_txt, fichero_salida.

    IF popup_select_fichero = 'X'.
      l_fichero = popup_select_fichero( ).
    ELSE.
      l_fichero = fichero.
    ENDIF.

    IF NOT l_fichero IS INITIAL AND sy-batch IS INITIAL.
      cl_gui_frontend_services=>file_exist(
        EXPORTING
          file                 = l_fichero
        RECEIVING
          result               = DATA(l_existe)
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5 ).
      IF sy-subrc = 0.
        IF l_existe IS INITIAL.
          l_serv = 'X'.
        ENDIF.
      ENDIF.
    ELSEIF get_tipo_ruta( fichero  = l_fichero
                          local    = local
                          servidor = servidor  ) = 'S'.
      l_serv = 'X'.
    ENDIF.

    fichero_salida = l_fichero.

    IF l_serv IS INITIAL.
      leer( EXPORTING fichero       = l_fichero
                      tipo          = 'BIN'
                      mostrar_error = mostrar_error
            IMPORTING num_excepcion = num_excepcion
                      filelength    = longitud
            CHANGING  tabla         = i_tabla ).

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length = longitud
        IMPORTING
          buffer       = xstring
        TABLES
          binary_tab   = i_tabla
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.

      IF sy-subrc = 0.
        IF get_tabla = 'X'.
          CALL FUNCTION 'SCMS_BINARY_TO_TEXT'
            EXPORTING
              input_length  = longitud
            IMPORTING
              output_length = longitud
            TABLES
              binary_tab    = i_tabla
              text_tab      = i_tabla_txt
            EXCEPTIONS
              failed        = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
            message = 'Error convirtiendo binario a texto'.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      TRY.
          OPEN DATASET l_fichero FOR INPUT IN BINARY MODE.
          IF sy-subrc <> 0.
            CONCATENATE 'No se puede abrir el fichero'(npa) fichero INTO message SEPARATED BY space.
          ELSE.
            READ DATASET l_fichero INTO xstring LENGTH l_long.
            IF sy-subrc = 0.
              longitud = longitud + l_long.
            ENDIF.

            CLOSE DATASET l_fichero.
          ENDIF.
        CATCH cx_root INTO l_err_root.                      "#EC *
          message = l_err_root->get_text( ).
      ENDTRY.

      IF message IS INITIAL.
        IF get_tabla = 'X'.
          i_solix = cl_bcs_convert=>xstring_to_solix( xstring ).

          CALL FUNCTION 'SCMS_BINARY_TO_TEXT'
            EXPORTING
              input_length  = longitud
            IMPORTING
              output_length = longitud
            TABLES
              binary_tab    = i_solix
              text_tab      = i_tabla_txt
            EXCEPTIONS
              failed        = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
            message = 'Error convirtiendo binario a texto'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT message IS INITIAL.
      IF mostrar_error = 'X'.
        MESSAGE message TYPE 'E'.
      ENDIF.
    ELSE.

      IF get_string = 'X'.
        string = zcl_ap_string=>xstring2string( xstring ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD limpiar_fichero.
    DATA l_linea TYPE c LENGTH 10000.

    l_linea = fichero.
    REPLACE '\\' WITH '\' INTO l_linea+1.
    salida = l_linea.
  ENDMETHOD.
  METHOD lista_ficheros.
    DATA: l_dir         TYPE string,
          l_filter      TYPE string,
          i_directorios TYPE TABLE OF file_info,
          l_count       TYPE i,
          l_fichero     TYPE file_info,
          l_cont        TYPE i.

    REFRESH file_table.

    l_dir = directory.
    l_filter = filter.
    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory                   = l_dir
        filter                      = l_filter
      CHANGING
        file_table                  = i_directorios
        count                       = l_count
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6 ).
    IF sy-subrc = 0.
      LOOP AT i_directorios INTO l_fichero WHERE isdir = 0.
        CONCATENATE directory '\' l_fichero-filename
                    INTO l_fichero-filename.
        REPLACE '\\' WITH '\' INTO l_fichero-filename+2.
        APPEND l_fichero TO file_table.
        l_count = l_count + 1.
        IF max_ficheros > 0 AND l_cont > max_ficheros.
          RETURN.
        ENDIF.
      ENDLOOP.

      IF recursivo = 'X'.
        LOOP AT i_directorios INTO l_fichero WHERE isdir = 1.
          DATA(l_max) = max_ficheros - l_count.
          IF l_max > 0 OR max_ficheros = 0.
            CONCATENATE directory '\' l_fichero-filename
                        INTO l_fichero-filename.
            DATA(i_fich) = lista_ficheros( directory    = l_fichero-filename
                                           recursivo    = recursivo
                                           filter       = filter
                                           max_ficheros = l_max ).
            APPEND LINES OF i_fich TO file_table.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD lista_ficheros_comun.
    DATA: l_dir      TYPE text255,
          i_dir_list TYPE zt_lista_ficheros,
          l_file     TYPE file_info.
    DATA: l_aux1      TYPE string,
          l_ext_mask  TYPE string,
          l_file_mask TYPE string,
          l_p1        TYPE string,
          l_p2        TYPE string,
          l_p3        TYPE string,
          l_ext_file  TYPE string,
          l_file_file TYPE string.

    FIELD-SYMBOLS <dir> TYPE zlista_ficheros.

    SET PARAMETER ID 'ZLF_MSG' FIELD '' ##EXISTS.

    IF servidor IS INITIAL.
      file_table = lista_ficheros( directory    = directory
                                   filter       = filter
                                   max_ficheros = max_ficheros
                                   recursivo    = recursivo ).
    ELSE.
      l_dir = directory.
      IF mayusculas = 'X'.
        TRANSLATE l_dir TO UPPER CASE.
      ENDIF.

      lee_ficheros_servidor(
        EXPORTING
          dir_name              = l_dir
          file_mask             = filter
          recursion             = recursivo
          max_ficheros          = max_ficheros
          mayusculas            = mayusculas
*         solo_dir              =
        CHANGING
          dir_list              = i_dir_list
        EXCEPTIONS
          read_directory_failed = 1
          empty_directory_list  = 2
          OTHERS                = 3 ).

      IF sy-subrc = 1.
        SET PARAMETER ID 'ZLF_MSG' FIELD 'Error leyendo directorio'(eld) ##EXISTS.
      ENDIF.

      LOOP AT i_dir_list ASSIGNING <dir>.
        CLEAR l_file.
        IF <dir>-type = 'directory'.
          l_file-isdir = 1.
        ENDIF.
        l_file-filename   = zcl_ap_ficheros=>concat_ruta( directorio = <dir>-dirname
                                                          fichero    = <dir>-name ).
        l_file-filelength = <dir>-len.
        l_file-createdate = <dir>-mod_date.
        l_file-createtime = <dir>-mod_time.
        APPEND l_file TO file_table.
      ENDLOOP.
    ENDIF.

    IF mask <> '' AND mask <> '*.*'.
      IF NOT mask IS INITIAL.
        l_aux1 = mask.
        l_aux1 = to_upper( l_aux1 ).
        l_ext_mask = zcl_ap_ficheros=>get_extension( l_aux1 ).
        l_file_mask = zcl_ap_ficheros=>get_nombre_fichero( fichero = l_aux1 con_extension = '' ).
      ENDIF.
      SPLIT l_file_mask AT '*' INTO l_p1 l_p2 l_p3.

      LOOP AT file_table INTO l_file.
        l_aux1 = l_file-filename.
        l_aux1 = to_upper( l_aux1 ).
        l_ext_file = zcl_ap_ficheros=>get_extension( l_aux1 ).
        l_file_file = zcl_ap_ficheros=>get_nombre_fichero( fichero = l_aux1 con_extension = '' ).

        IF l_ext_mask <> l_ext_file.
          DELETE file_table.
        ELSEIF NOT l_p1 IS INITIAL AND NOT l_file_file CS l_p1.
          DELETE file_table.
        ELSEIF NOT l_p2 IS INITIAL AND NOT l_file_file CS l_p2.
          DELETE file_table.
        ELSEIF NOT l_p3 IS INITIAL AND NOT l_file_file CS l_p3.
          DELETE file_table.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD mover.
    DATA: l_serv_orig TYPE c LENGTH 1,
          l_serv_dest TYPE c LENGTH 1,
          l_file      TYPE string,
          l_xstring   TYPE xstring,
          l_long      TYPE i.
    DATA: l_cmd   TYPE text255,
          l_lines TYPE TABLE OF char255.

    IF servidor_orig = 'X'.
      l_serv_orig = 'X'.
    ELSEIF servidor_orig IS INITIAL AND NOT fichero CS ':'.
      l_serv_orig = 'X'.
    ENDIF.

    IF servidor_dest = 'X'.
      l_serv_dest = 'X'.
    ELSEIF servidor_dest IS INITIAL AND NOT fichero CS ':'.
      l_serv_dest = 'X'.
    ENDIF.

    l_file = get_nombre_fichero( fichero = fichero con_extension = 'X' ).
    l_file = concat_ruta( fichero = l_file directorio = directorio_destino ).

    leer_xstring(
      EXPORTING
        fichero       = fichero
        mostrar_error = mostrar_error
        servidor      = l_serv_orig
      IMPORTING
        num_excepcion = num_excepcion
        xstring       = l_xstring
        message       = mensaje
        longitud      = l_long ).

    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF l_serv_dest = 'X' AND crear_dir_dest_serv = 'X'.
      IF existe_dir( directorio = directorio_destino servidor = l_serv_dest ) = ''.
        CONCATENATE 'mkdir' directorio_destino INTO l_cmd SEPARATED BY space.
        CALL 'SYSTEM' ID 'COMMAND' FIELD l_cmd            "#EC CI_CCALL
             ID 'TAB' FIELD l_lines.

        IF sy-subrc <> 0.
          mensaje = 'Error creando directorio'(ecd).
        ENDIF.
      ENDIF.
    ENDIF.

    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    grabar_xstring(
      EXPORTING
        mostrar_error = mostrar_error
        xstring       = l_xstring
        fichero       = l_file
        bin_filesize  = l_long
        servidor      = l_serv_dest
      IMPORTING
        num_excepcion = num_excepcion
        mensaje       = mensaje ).

    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    mensaje = borrar_fichero(
      fichero  = fichero
      servidor = l_serv_orig ).
  ENDMETHOD.
  METHOD popup_select_directorio.
    DATA l_dir TYPE string.

    IF servidor IS INITIAL.
      cl_gui_frontend_services=>directory_browse(
        EXPORTING
          window_title         = titulo
          initial_folder       = initial_directory
        CHANGING
          selected_folder      = l_dir
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4 ).
      IF sy-subrc = 0 AND NOT l_dir IS INITIAL.
        directorio = l_dir.
      ENDIF.
    ELSE.
      IF initial_directory CS ':'.
        CLEAR l_dir.
      ELSE.
        l_dir = initial_directory.
      ENDIF.
      CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
        EXPORTING
          directory        = l_dir
          filemask         = 'directory'
        IMPORTING
          serverfile       = directorio
        EXCEPTIONS
          canceled_by_user = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        CLEAR directorio.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD popup_select_fichero.
    DATA: l_rc       TYPE i,
          i_ficheros TYPE filetable,
          l_fichero  TYPE file_table,
          i_path     TYPE dxfields-longpath,
          filemask   TYPE dxfields-filemask,
          o_path     TYPE dxfields-longpath,
          abend_flag TYPE dxfields-abendflag.

    IF servidor IS INITIAL.
      cl_gui_frontend_services=>file_open_dialog(
        EXPORTING
          default_extension = default_extension
          initial_directory = initial_directory
          file_filter       = file_filter
        CHANGING
          rc                = l_rc
          file_table        = i_ficheros ).

      READ TABLE i_ficheros INTO l_fichero INDEX 1.
      IF sy-subrc = 0.
        fichero = l_fichero.
      ENDIF.
    ELSE.
      i_path = initial_directory.
      filemask = file_filter.
      CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
        EXPORTING
          i_location_flag = 'A'
          i_server        = ''
          i_path          = i_path
          filemask        = filemask
        IMPORTING
          o_path          = o_path
          abend_flag      = abend_flag
        EXCEPTIONS
          rfc_error       = 1
          error_with_gui  = 2
          OTHERS          = 3.
      IF sy-subrc = 0 AND abend_flag IS INITIAL.
        fichero = o_path.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD ultima_letra_ruta.
    DATA: l_char TYPE c LENGTH 1000,
          l_lon  TYPE i.

    l_char = directorio.
    l_lon = strlen( l_char ).
    IF l_lon > 1.
      l_lon = l_lon - 1.
    ENDIF.
    letra = l_char+l_lon(1).
  ENDMETHOD.
  METHOD ver_fichero_texto.
    DATA: l_fichero   TYPE string,
          i_tabla     TYPE TABLE OF string,
          l_documento TYPE zdocumentos.

    IF NOT existe( fichero ).
      RETURN.
    ENDIF.

    l_fichero = fichero.
    IF NOT fichero CS ':'.
      lee_fich_servidor( EXPORTING fichero = l_fichero
                         CHANGING  tabla   = i_tabla ).

      l_documento-nombre  = 'temporal'.
      l_documento-fichero = fichero.
      l_fichero = zcl_ap_documentos=>get_nombre_fichero_temporal( l_documento ).
      grabar( EXPORTING fichero = l_fichero
              CHANGING  tabla   = i_tabla ).
    ENDIF.
    zcl_ap_gos=>visualizar_fichero_st( fichero = l_fichero extension = extension ).
  ENDMETHOD.
  METHOD visualizar.
    DATA: l_fichero TYPE string,
          l_xstring TYPE xstring,
          l_ruta    TYPE string.

    CLEAR message.
    IF xstring IS INITIAL.
      l_fichero = concat_ruta( fichero = fichero directorio = directorio ).
      leer_xstring( EXPORTING fichero  = l_fichero
                              servidor = servidor
                    IMPORTING xstring  = l_xstring ).
    ELSE.
      l_xstring = xstring.
    ENDIF.
    IF l_xstring IS INITIAL.
      RETURN.
    ENDIF.
    l_ruta = get_directorio_temporal( ).
    IF l_ruta IS INITIAL.
      message = 'Error recuperando ruta temporal'.
      RETURN.
    ENDIF.
    l_fichero = get_nombre_fichero( fichero = fichero con_extension = 'X' ).
    l_fichero = concat_ruta( fichero = l_fichero directorio = l_ruta ).
    grabar_xstring( EXPORTING xstring      = l_xstring
                              fichero      = l_fichero
                              bin_filesize = bin_filesize
                    IMPORTING mensaje      = message ).
    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.
    zcl_ap_gos=>visualizar_fichero_st( l_fichero ).
  ENDMETHOD.