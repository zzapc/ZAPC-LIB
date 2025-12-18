CLASS zcl_ap_ftp DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA i_lineas       TYPE tpstdouts.
    DATA error_conexion TYPE abap_bool.
    DATA error_comando  TYPE abap_bool.
    DATA mensaje_error  TYPE tpstdout.

    METHODS constructor
      IMPORTING !user              TYPE any               OPTIONAL
                password           TYPE any               OPTIONAL
                host               TYPE any               OPTIONAL
                encriptar_password TYPE abap_bool         DEFAULT 'X'
                destino_rfc        TYPE any               DEFAULT 'SAPFTPA'
                parametros         TYPE zparametros-clave DEFAULT ''.

    METHODS connect
      IMPORTING !user              TYPE any
                password           TYPE any
                host               TYPE any
                encriptar_password TYPE abap_bool         DEFAULT 'X'
                destino_rfc        TYPE any               DEFAULT 'SAPFTPA'
                parametros         TYPE zparametros-clave DEFAULT ''
      RETURNING VALUE(message)     TYPE bapi_msg.

    CLASS-METHODS encriptar_password_viejo
      CHANGING password TYPE any.

    METHODS command
      IMPORTING comando        TYPE string OPTIONAL
                !index         TYPE i      DEFAULT 0
      PREFERRED PARAMETER comando
      RETURNING VALUE(control) TYPE char3.

    METHODS cd
      IMPORTING directorio TYPE any
      RETURNING VALUE(ok)  TYPE abap_bool.

    METHODS dir_actual
      RETURNING VALUE(directorio_actual) TYPE string.

    METHODS get_lista_ficheros
      IMPORTING mascara           TYPE any DEFAULT ''
      RETURNING VALUE(i_ficheros) TYPE rstt_t_files.

    METHODS put
      IMPORTING fichero   TYPE any
      RETURNING VALUE(ok) TYPE abap_bool.

    METHODS get
      IMPORTING fichero       TYPE any
                directorio_pc TYPE any       OPTIONAL
                dir_temp      TYPE any       DEFAULT ''
                directorio    TYPE any       DEFAULT ''
                passive_mode  TYPE abap_bool DEFAULT 'X'
      EXPORTING fichero_pc    TYPE any
                VALUE(ok)     TYPE abap_bool.

    METHODS delete
      IMPORTING fichero   TYPE any
      RETURNING VALUE(ok) TYPE abap_bool.

    METHODS disconnect.

    METHODS upload_text_file
      IMPORTING fichero        TYPE csequence
                i_tabla        TYPE table
                passive_mode   TYPE abap_bool DEFAULT 'X'
                directorio     TYPE any       DEFAULT ''
      EXPORTING fichero_final  TYPE string
      RETURNING VALUE(message) TYPE bapi_msg.

    METHODS upload_binary_file
      IMPORTING fichero        TYPE any
                i_tabla        TYPE table     OPTIONAL
                passive_mode   TYPE abap_bool DEFAULT 'X'
                xstring        TYPE xstring   OPTIONAL
                !string        TYPE string    OPTIONAL
                directorio     TYPE any       DEFAULT ''
      EXPORTING fichero_final  TYPE string
      RETURNING VALUE(message) TYPE bapi_msg.

    METHODS download_text_file
      IMPORTING fichero        TYPE any
                passive_mode   TYPE abap_bool DEFAULT 'X'
                directorio     TYPE any       DEFAULT ''
      EXPORTING i_tabla        TYPE table
      RETURNING VALUE(message) TYPE bapi_msg.

    METHODS download_binary_file
      IMPORTING fichero        TYPE any
                passive_mode   TYPE abap_bool DEFAULT 'X'
                directorio     TYPE any       DEFAULT ''
      EXPORTING file_length    TYPE i
                i_tabla        TYPE table
      RETURNING VALUE(message) TYPE bapi_msg.

    METHODS lcd
      IMPORTING directorio TYPE any
      RETURNING VALUE(ok)  TYPE abap_bool.

    METHODS mv
      IMPORTING fichero_origen  TYPE any
                fichero_destino TYPE any
      RETURNING VALUE(ok)       TYPE abap_bool.

    METHODS grabar_fichero
      IMPORTING !user              TYPE any               DEFAULT ''
                password           TYPE any               DEFAULT ''
                host               TYPE any               DEFAULT ''
                encriptar_password TYPE abap_bool         DEFAULT 'X'
                destino_rfc        TYPE any               DEFAULT 'SAPFTPA'
                fichero            TYPE any
                xstring            TYPE xstring           OPTIONAL
                i_tabla            TYPE table             OPTIONAL
                directorio         TYPE any               DEFAULT ''
                binario            TYPE abap_bool         DEFAULT 'X'
                parametros         TYPE zparametros-clave DEFAULT ''
                no_desconectar     TYPE abap_bool         DEFAULT ''
                passive_mode       TYPE abap_bool         DEFAULT 'X'
      EXPORTING !message           TYPE bapi_msg
                fichero_final      TYPE string.

    METHODS lista_ficheros
      IMPORTING !user              TYPE any               DEFAULT ''
                password           TYPE any               DEFAULT ''
                host               TYPE any               DEFAULT ''
                encriptar_password TYPE abap_bool         DEFAULT 'X'
                destino_rfc        TYPE any               DEFAULT 'SAPFTPA'
                directorio         TYPE any               DEFAULT ''
                parametros         TYPE zparametros-clave DEFAULT ''
                mascara            TYPE any               DEFAULT ''
                no_desconectar     TYPE abap_bool         DEFAULT ''
      EXPORTING !message           TYPE bapi_msg
                i_ficheros         TYPE rstt_t_files.

    METHODS leer_fichero
      IMPORTING !user              TYPE any               DEFAULT ''
                password           TYPE any               DEFAULT ''
                host               TYPE any               DEFAULT ''
                encriptar_password TYPE abap_bool         DEFAULT 'X'
                destino_rfc        TYPE any               DEFAULT 'SAPFTPA'
                fichero            TYPE any
                directorio         TYPE any               DEFAULT ''
                binario            TYPE abap_bool         DEFAULT 'X'
                parametros         TYPE zparametros-clave DEFAULT ''
                no_desconectar     TYPE abap_bool         DEFAULT ''
                passive_mode       TYPE abap_bool         DEFAULT 'X'
      EXPORTING xstring            TYPE xstring
                i_tabla            TYPE table
                !message           TYPE bapi_msg.

    CLASS-METHODS matar_sesiones_rfc.


private section.

  data HANDLE type I .
  data LINEA type TPSTDOUT .
  data DIR_TEMP type STRING .
  data DESTINO_RFC type RFCDEST .
  data SCRAMBLE_KEY type I value 26101957 ##NO_TEXT.
  data SALIDA_SIN_LOG type ABAP_BOOL .
  data USER type STRING .
  data PASSWORD type STRING .
  data HOST type STRING .
  data DIRECTORIO type STRING .

  methods CONTROL_COMANDO
    importing
      !INDEX type I default 0
    returning
      value(CODIGO) type CHAR3 .
  methods ENCRIPTAR_PASSWORD
    changing
      !PASSWORD type CSEQUENCE .
  methods RECONECTAR
    returning
      value(MESSAGE) type BAPI_MSG .
endclass. "ZCL_AP_FTP definition
class ZCL_AP_FTP implementation.
  METHOD cd.
    DATA l_string TYPE string.

    CLEAR: mensaje_error, error_comando.

    IF handle IS INITIAL.
      IF NOT me->user IS INITIAL.
        CLEAR me->directorio.
        mensaje_error-line = reconectar( ).
        IF NOT mensaje_error-line IS INITIAL.
          error_comando = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    me->directorio = directorio.

    CONCATENATE 'cd' directorio INTO l_string SEPARATED BY space.

    DATA(salida) = command( l_string ).

    CASE salida.
      WHEN '250'.
        ok = 'X'.
      WHEN '550'.
        error_comando = 'X'.
        mensaje_error-line = |No se encuentra el directorio { directorio }|.
      WHEN OTHERS.
        mensaje_error-line = linea+4.
        error_comando = 'X'.
    ENDCASE.
  ENDMETHOD.
  METHOD command.
    DATA l_comando TYPE c LENGTH 255.

    IF handle IS INITIAL OR error_conexion = 'X'.
      error_comando = 'X'.
      mensaje_error = 'FTP no conectado'.
      RETURN.
    ENDIF.

    l_comando = comando.

    CLEAR: i_lineas, salida_sin_log.
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        handle        = handle
        command       = l_comando
      TABLES
        data          = i_lineas
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      DATA(l_subrc) = sy-subrc.
      error_comando = 'X'.
      CASE l_subrc.
        WHEN 1. mensaje_error = 'TCPIP ERROR'.
        WHEN 2. mensaje_error = 'COMMAND ERROR'.
        WHEN 3. mensaje_error = 'DATA ERROR'.
      ENDCASE.
      mensaje_error-line = |Error { l_subrc } { mensaje_error-line } en comando { l_comando }|.
    ENDIF.

* En S4 no se devuelven lineas con la salida del comando, en ese caso simulamos los códigos de error
    READ TABLE i_lineas INTO DATA(l_primera_linea) INDEX 1.
    IF l_comando <> l_primera_linea.
      salida_sin_log = 'X'.
      IF error_comando IS INITIAL.
        IF l_comando = 'pwd'.
          control = '257'.
        ELSEIF l_comando(3) = `get` OR l_comando(3) = `put ` OR l_comando(2) = `ls`.
          control = '226'.
        ELSEIF l_comando(2) = `mv`.
          control = '350'.
        ELSE.
          control = '250'.
        ENDIF.
      ELSEIF l_subrc = 2.
        IF l_comando(2) = `cd` OR l_comando(3) = `lcd`.
          control = '550 Error cambiando de directorio'.
        ELSE.
          control = mensaje_error-line.
        ENDIF.
      ELSE.
        control = mensaje_error-line.
      ENDIF.
    ELSE.
      control = control_comando( index ).
    ENDIF.
  ENDMETHOD.
  METHOD connect.
    DATA: l_user     TYPE c LENGTH 100,
          l_password TYPE c LENGTH 100,
          l_host     TYPE c LENGTH 255.

    CLEAR message.

* El sitio FTP tiene que estar permitido en la tabla SAPFTP_SERVERS
* Ejemplo de uso de funciones en RSFTP007
* Webs de test https://dlptest.com/ftp-test/

    l_user = user.
    l_password = password.
    l_host = host.

    IF NOT parametros IS INITIAL.
      DATA: i_p      TYPE TABLE OF string,
            l_campo  TYPE zparametros-campo,
            l_valor  TYPE zparametros-valor,
            l_valor2 TYPE zparametros-valor2,
            l_c      TYPE string,
            l_v      TYPE string.
      FIELD-SYMBOLS <p> TYPE string.

      DEFINE get_valor_par.
        CLEAR: l_campo, l_valor, l_valor2.
        SPLIT l_&1 AT '|' INTO TABLE i_p.
        LOOP AT i_p ASSIGNING <p>.
          IF <p> CS '='.
            SPLIT <p> AT '=' INTO l_c l_v.
            CASE l_c.
              WHEN 'C1'. l_campo = l_v.
              WHEN 'C2'. l_valor = l_v.
              WHEN 'C3'. l_valor2 = l_v.
            ENDCASE.
          ENDIF.
        ENDLOOP.
        IF NOT l_campo IS INITIAL.
          SELECT SINGLE atributo1 FROM zparametros
            INTO l_&1
           WHERE clave  = parametros
             AND campo  = l_campo
             AND valor  = l_valor
             AND valor2 = l_valor2.
          IF sy-subrc <> 0.
            message = |No definido parámetro { parametros } { l_campo } { l_valor } { l_valor2 }|.
            error_conexion = 'X'.
            RETURN.
          ENDIF.
        ENDIF.
      END-OF-DEFINITION.
      get_valor_par: user, password, host.
    ENDIF.

    IF encriptar_password = 'X'.
      encriptar_password( CHANGING password = l_password ).
    ENDIF.

    IF destino_rfc IS INITIAL.
      IF sy-batch IS INITIAL.
        me->destino_rfc = 'SAPFTPA'.
      ELSE.
        me->destino_rfc = 'SAPFTP'.
      ENDIF.
    ELSE.
      me->destino_rfc = destino_rfc.
    ENDIF.

    me->user     = l_user.
    me->password = l_password.
    me->host     = l_host.

    CALL FUNCTION 'FTP_CONNECT'
      EXPORTING
*-- Your SAP-UNIX FTP user name (case sensitive)
        user            = l_user
        password        = l_password
*-- Your SAP-UNIX server host name (case sensitive)
        host            = l_host
        rfc_destination = me->destino_rfc
      IMPORTING
        handle          = handle
      EXCEPTIONS
        not_connected   = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      IF me->destino_rfc = 'SAPFTP'.
        me->destino_rfc = 'SAPFTPA'.
      ELSE.
        me->destino_rfc = 'SAPFTP'.
      ENDIF.
      CALL FUNCTION 'FTP_CONNECT'
        EXPORTING
*-- Your SAP-UNIX FTP user name (case sensitive)
          user            = l_user
          password        = l_password
*-- Your SAP-UNIX server host name (case sensitive)
          host            = l_host
          rfc_destination = me->destino_rfc
        IMPORTING
          handle          = handle
        EXCEPTIONS
          not_connected   = 1
          OTHERS          = 2.

      IF sy-subrc <> 0.
        error_conexion = 'X'.
        mensaje_error = 'Error conectando al FTP'.
        message = mensaje_error.
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    IF NOT host IS INITIAL.
      connect( user               = user
                password           = password
                host               = host
                encriptar_password = encriptar_password
                destino_rfc        = destino_rfc
                parametros         = parametros ).
    ENDIF.
  ENDMETHOD.
  METHOD control_comando.
    DATA l_lin TYPE i.

    l_lin = lines( i_lineas ).

    l_lin = l_lin + index.

    READ TABLE i_lineas INTO linea INDEX l_lin.

    codigo = linea(3).
  ENDMETHOD.
  METHOD delete.
    DATA l_string TYPE string.

    IF handle IS INITIAL AND NOT me->user IS INITIAL.
      mensaje_error = reconectar( ).
      IF NOT mensaje_error IS INITIAL.
        error_comando = 'X'.
        RETURN.
      ENDIF.
    ENDIF.

    CONCATENATE 'delete ' fichero INTO l_string SEPARATED BY space.

    IF command( l_string ) = '250'.
      ok = 'X'.
      CLEAR error_comando.
    ELSE.
      error_comando = 'X'.
      IF NOT ( mensaje_error CS 'Error' AND mensaje_error CS 'comando' ).
        mensaje_error = linea+4.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD dir_actual.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: l_s1 TYPE tpstdout,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_s2 TYPE tpstdout.

    IF command( 'pwd' ) = '257'.
      SPLIT linea AT '"' INTO l_s1 directorio_actual l_s2.
      CLEAR error_comando.
    ELSE.
      error_comando = 'X'.
      mensaje_error = linea+4.
    ENDIF.
  ENDMETHOD.
  METHOD disconnect.
    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        handle = handle
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0. "#EC EMPTY_IF_BRANCH
*     message 'Error desconectando' type 'S'.
    ENDIF.

* Close the associated RFC connection:
    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination = destino_rfc
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0. "#EC EMPTY_IF_BRANCH
*     message 'Error cerrando conexión RFC' type 'S'.
    ENDIF.
* Reset the internally managed session handle:
    CLEAR handle.
  ENDMETHOD.
  METHOD download_binary_file.
    DATA: l_fichero      TYPE c LENGTH 1024,
          l_subrc        TYPE sy-subrc,
          lv_ftp_command TYPE string.

    IF handle IS INITIAL OR error_conexion = 'X'.
      mensaje_error = 'FTP no conectado'.
      message = mensaje_error.
      RETURN.
    ENDIF.

    l_fichero = zcl_ap_ficheros=>get_fichero_var( fichero = CONV #( fichero ) ).

    IF NOT directorio IS INITIAL.
      CONCATENATE '/' directorio '/' l_fichero INTO l_fichero.
      REPLACE ALL OCCURRENCES OF '//' IN l_fichero WITH '/'.
    ENDIF.

*    SET the passive MODE - AS necessary:
    IF passive_mode = abap_true.
      command( 'set passive on' ).
    ENDIF.

* Download the selected text file from the FTP host:
    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle        = handle
        fname         = l_fichero
      IMPORTING
        blob_length   = file_length
      TABLES
        blob          = i_tabla
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.

    IF sy-subrc <> 0.
      l_subrc = sy-subrc.
      error_comando = 'X'.
      CASE l_subrc.
        WHEN 1. mensaje_error = 'TCPIP ERROR'.
        WHEN 2. mensaje_error = 'COMMAND ERROR'.
        WHEN 3. mensaje_error = 'DATA ERROR'.
      ENDCASE.
      __concat4 mensaje_error 'Error' l_subrc mensaje_error 'descargando fichero de FTP'.
      message = mensaje_error.
    ENDIF.
  ENDMETHOD.
  METHOD download_text_file.
    DATA: l_fichero      TYPE c LENGTH 1024,
          l_subrc        TYPE i,
          lv_ftp_command TYPE string.

    CLEAR: i_tabla, message.

    IF handle IS INITIAL OR error_conexion = 'X'.
      mensaje_error = 'FTP no conectado'(fnc).
      message = mensaje_error.
      RETURN.
    ENDIF.

* Ejemplo en RSFTP007

    l_fichero = zcl_ap_ficheros=>get_fichero_var( fichero = CONV #( fichero ) ).

    IF NOT directorio IS INITIAL.
      CONCATENATE '/' directorio '/' l_fichero INTO l_fichero.
      REPLACE ALL OCCURRENCES OF '//' IN l_fichero WITH '/'.
    ENDIF.

* Set the passive mode - as necessary:
    IF passive_mode = abap_true.
      command( 'set passive on' ).
    ENDIF.

    command( 'ascii' ).

* Download the selected text file from the FTP host:
    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle         = handle
        fname          = l_fichero
        character_mode = 'X'
      TABLES
        text           = i_tabla
      EXCEPTIONS
        tcpip_error    = 1
        command_error  = 2
        data_error     = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      l_subrc = sy-subrc.
      error_comando = 'X'.
      CASE l_subrc.
        WHEN 1. mensaje_error = 'TCPIP ERROR'.
        WHEN 2. mensaje_error = 'COMMAND ERROR'.
        WHEN 3. mensaje_error = 'DATA ERROR'.
      ENDCASE.
      __concat4 mensaje_error 'Error'(err) l_subrc mensaje_error 'descargando fichero de FTP'(dft).
      message = mensaje_error.
    ENDIF.
  ENDMETHOD.
  METHOD encriptar_password.
* Method-Local Data Declarations:

    DATA lv_password_length TYPE i.

* Calculate the length of the provided password:
    lv_password_length = strlen( password ).

* Scramble the password using the standard HTTP_SCRAMBLE module:
    CALL FUNCTION 'HTTP_SCRAMBLE'
      EXPORTING
        source      = password
        sourcelen   = lv_password_length
        key         = scramble_key
      IMPORTING
        destination = password.
  ENDMETHOD.
  METHOD encriptar_password_viejo.
    DATA l_pass TYPE c LENGTH 100.

    l_pass = password.
    CALL FUNCTION 'SCRAMBLE_STRING'
      EXPORTING
        source = password
      IMPORTING
        target = l_pass.
    password = l_pass.
  ENDMETHOD.
  METHOD get.
    DATA: l_fichero_ftp TYPE string,
          l_fichero_pc  TYPE string,
          l_string      TYPE string.

    IF directorio IS INITIAL.
      CONCATENATE '"' fichero '"' INTO l_fichero_ftp.
    ELSE.
      CONCATENATE '"/' directorio '/' fichero '"' INTO l_fichero_ftp.
      REPLACE ALL OCCURRENCES OF '//' IN l_fichero_ftp WITH '/'.
    ENDIF.

    IF dir_temp IS INITIAL.
      CONCATENATE '"' directorio_pc '\' fichero '"' INTO l_fichero_pc.
    ELSE.
      IF me->dir_temp = 'X'.
        me->dir_temp = zcl_ap_documentos=>get_directorio_temporal( ).
      ELSEIF dir_temp <> ''.
        me->dir_temp = dir_temp.
      ENDIF.
      fichero_pc = zcl_ap_ficheros=>concat_ruta( directorio = me->dir_temp fichero = fichero ).
      CONCATENATE '"' fichero_pc '"' INTO l_fichero_pc.
    ENDIF.

    IF passive_mode = abap_true.
      command( 'set passive on' ).
    ENDIF.

    CONCATENATE 'get ' l_fichero_ftp l_fichero_pc INTO l_string SEPARATED BY space.

    IF command( comando = l_string index = -1 ) = '226'.
      ok = 'X'.
      CLEAR error_comando.
    ELSE.
      error_comando = 'X'.
      mensaje_error = linea+4.
    ENDIF.
  ENDMETHOD.
  METHOD get_lista_ficheros.
    DATA: l_comando            TYPE string,
          l_line_fin           TYPE i,
          l_line_ini           TYPE i,
          l_file               TYPE file_info,
          l_ini                TYPE i,
          l_inim               TYPE i,
          l_aux1               TYPE c LENGTH 255,
          l_aux2               TYPE c LENGTH 255,
          l_desp               TYPE i,
          l_fecha_al_principio TYPE c LENGTH 1,
          l_desp2              TYPE i.

    IF mascara IS INITIAL.
      l_comando = 'ls'.
    ELSE.
      CONCATENATE 'ls' mascara INTO l_comando SEPARATED BY space.
    ENDIF.

    IF command( l_comando ) = '226'.
      CLEAR error_comando.

      l_line_fin = lines( i_lineas ).
      l_line_fin = l_line_fin - 1.
      l_line_ini = l_line_fin.
      LOOP AT i_lineas INTO linea.
        IF salida_sin_log IS INITIAL.
          IF    linea+4(7)  = 'Opening' OR linea CS 'Connecting to port' OR linea CS 'Data connection already open' OR linea(5) = 'total'
             OR linea      CS '200 PORT command successful.' OR linea CS '150 Here comes the directory listing.'.
            l_line_ini = sy-tabix + 1.
          ENDIF.

          IF sy-tabix < l_line_ini OR sy-tabix > l_line_fin.
            CONTINUE.
          ENDIF.
        ENDIF.

        CLEAR l_file.
        IF linea(1) = 'd' OR linea CS '<dir>' OR linea CS '<DIR>'.
          l_file-isdir = 1.
        ENDIF.

        CLEAR: l_ini,
               l_inim.
        IF mascara CS '*'.
          SPLIT mascara AT '*' INTO l_aux1 l_aux2.
          IF linea+21 CS l_aux1 AND linea CS l_aux2.
            SEARCH linea+21 FOR l_aux1 IN CHARACTER MODE.
            IF sy-fdpos > 0.
              l_inim = sy-fdpos.
              l_desp = l_inim + 21.
              l_file-filename = linea+l_desp.
            ENDIF.
          ENDIF.
        ENDIF.
        IF linea+58(1) = ':'.
          l_ini = 41.
        ELSEIF linea+13(3) = 'ftp'.
          l_ini = 28.
        ELSEIF linea+13(10) = 'user group'.
          l_ini = 31.
        ELSEIF linea+2(1) = '-' AND linea+16(1) = 'M'.
          l_ini = 30.
          l_fecha_al_principio = 'X'.
        ELSEIF    linea+30(5) = ' Jan ' OR linea+30(5) = ' Feb ' OR linea+30(5) = ' Mar '
               OR linea+30(5) = ' Apr ' OR linea+30(5) = ' Mai ' OR linea+30(5) = ' May ' OR linea+30(5) = ' Jun '
               OR linea+30(5) = ' Jul ' OR linea+30(5) = ' Aug ' OR linea+30(5) = ' Sep '
               OR linea+30(5) = ' Oct ' OR linea+30(5) = ' Nov ' OR linea+30(5) = ' Dec '.
          l_ini = 23.
        ELSEIF    linea+31(5) = ' Jan ' OR linea+31(5) = ' Feb ' OR linea+31(5) = ' Mar '
               OR linea+31(5) = ' Apr ' OR linea+31(5) = ' Mai ' OR linea+31(5) = ' May ' OR linea+31(5) = ' Jun '
               OR linea+31(5) = ' Jul ' OR linea+31(5) = ' Aug ' OR linea+31(5) = ' Sep '
               OR linea+31(5) = ' Oct ' OR linea+31(5) = ' Nov ' OR linea+31(5) = ' Dec '.
          l_ini = 24.
        ELSEIF    linea+32(5) = ' Jan ' OR linea+32(5) = ' Feb ' OR linea+32(5) = ' Mar '
               OR linea+32(5) = ' Apr ' OR linea+32(5) = ' Mai ' OR linea+32(5) = ' May ' OR linea+32(5) = ' Jun '
               OR linea+32(5) = ' Jul ' OR linea+32(5) = ' Aug ' OR linea+32(5) = ' Sep '
               OR linea+32(5) = ' Oct ' OR linea+32(5) = ' Nov ' OR linea+32(5) = ' Dec '.
          l_ini = 25.
        ELSEIF NOT l_inim IS INITIAL.
          l_ini = l_inim.
        ELSE.
          l_ini = 34.
        ENDIF.

        l_file-filelength = zcl_ap_string=>busca_numeros( linea+l_ini(8) ).
        IF l_fecha_al_principio = 'X'.
          IF linea+2(1) = '-' AND linea+5(1) = '-'.
            DATA(l_dia) = linea+3(2).
            linea+3(2) = linea(2).
            linea(2) = l_dia.
          ENDIF.
          l_file-createdate = zcl_ap_fechas=>string2fecha( linea(8) ).

          IF linea+16(1) = 'M'.
            l_file-createtime = zcl_ap_fechas=>string2hora( linea+10(5) ).
            IF linea+15(2) = 'PM'.
              TRY.
                  l_file-createtime(2) = l_file-createtime(2) + 12.
                CATCH cx_root INTO DATA(o_root).            "#EC *
                  MESSAGE |Error recuperando hora { o_root->get_text( ) }| TYPE 'S'.
              ENDTRY.
            ENDIF.
          ELSE.
            l_file-createtime = zcl_ap_fechas=>string2hora( linea+10(7) ).
          ENDIF.

          l_desp = 39.
        ELSE.
          l_file-createdate(4) = sy-datum(4).
          l_desp = l_ini + 8.
          CASE linea+l_desp(3).
            WHEN 'Jan'. l_file-createdate+4(2) = '01'.
            WHEN 'Feb'. l_file-createdate+4(2) = '02'.
            WHEN 'Mar'. l_file-createdate+4(2) = '03'.
            WHEN 'Apr'. l_file-createdate+4(2) = '04'.
            WHEN 'Mai' OR 'May'. l_file-createdate+4(2) = '05'.
            WHEN 'Jun'. l_file-createdate+4(2) = '06'.
            WHEN 'Jul'. l_file-createdate+4(2) = '07'.
            WHEN 'Aug'. l_file-createdate+4(2) = '08'.
            WHEN 'Sep'. l_file-createdate+4(2) = '09'.
            WHEN 'Oct'. l_file-createdate+4(2) = '10'.
            WHEN 'Nov'. l_file-createdate+4(2) = '11'.
            WHEN 'Dec'. l_file-createdate+4(2) = '12'.
          ENDCASE.
          l_desp = l_ini + 12.
          l_file-createdate+6(2) = linea+l_desp(2).
          l_desp = l_ini + 15.
          l_desp2 = l_ini + 18.
          CONCATENATE linea+l_desp(2) linea+l_desp2(2) '00' INTO l_file-createtime.
          l_desp = l_ini + 21.
        ENDIF.

        IF l_file-filename IS INITIAL.
          l_file-filename = linea+l_desp.

          DO 5 TIMES.
            IF l_file-filename(1) = ''.
              l_file-filename = l_file-filename+1.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.
        ENDIF.

        APPEND l_file TO i_ficheros.
      ENDLOOP.
    ELSE.
      error_comando = 'X'.
      mensaje_error = linea+4.
    ENDIF.
  ENDMETHOD.
  METHOD grabar_fichero.
    CLEAR message.
    IF handle IS INITIAL.
      IF user IS INITIAL AND NOT me->user IS INITIAL.
        message = reconectar( ).
      ELSE.
        message = connect( user               = user
                 password           = password
                 host               = host
                 encriptar_password = encriptar_password
                 destino_rfc        = destino_rfc
                 parametros         = parametros ).
      ENDIF.
    ENDIF.

    IF NOT message IS INITIAL.
      disconnect( ).
    ELSE.
      IF xstring IS INITIAL AND binario IS INITIAL.
        message = upload_text_file( EXPORTING fichero    = fichero
                                              directorio = directorio
                                              i_tabla    = i_tabla
                                              passive_mode  = passive_mode
                                    IMPORTING fichero_final = fichero_final ).
      ELSE.
        IF NOT i_tabla IS INITIAL AND xstring IS INITIAL.
          DATA(l_string) = zcl_ap_string=>tabla2string( i_tabla ).
        ENDIF.
        message = upload_binary_file( EXPORTING fichero    = fichero
                                                directorio = directorio
                                                xstring    = xstring
                                                string     = l_string
                                                i_tabla    = i_tabla
                                                passive_mode  = passive_mode
                                      IMPORTING fichero_final = fichero_final ).
      ENDIF.

      IF no_desconectar IS INITIAL OR NOT message IS INITIAL.
        disconnect( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD lcd.
    DATA l_string TYPE string.

    CONCATENATE 'lcd' directorio INTO l_string SEPARATED BY space.

    IF command( l_string ) = '250'.
      ok = 'X'.
      CLEAR error_comando.
    ELSE.
      error_comando = 'X'.
      mensaje_error = linea+4.
    ENDIF.
  ENDMETHOD.
  METHOD leer_fichero.
    DATA: i_blob   TYPE solix_tab,
          l_length TYPE i.

    CLEAR message.
    IF handle IS INITIAL.
      IF user IS INITIAL AND NOT me->user IS INITIAL.
        message = reconectar( ).
      ELSE.
        message = connect( user               = user
                 password           = password
                 host               = host
                 encriptar_password = encriptar_password
                 destino_rfc        = destino_rfc
                 parametros         = parametros ).
      ENDIF.
    ENDIF.
    IF NOT message IS INITIAL.
      disconnect( ).
    ELSE.
      IF binario = 'X'.
        message = download_binary_file( EXPORTING fichero    = fichero
                                                  directorio = directorio
                                                 passive_mode = passive_mode
                                       IMPORTING i_tabla    = i_blob
                                                 file_length = l_length ).
        IF NOT i_blob IS INITIAL.
*          CALL METHOD cl_bcs_convert=>raw_to_xstring
*            EXPORTING
*              it_soli    = i_blob
*            RECEIVING
*              ev_xstring = xstring.

          xstring = cl_bcs_convert=>solix_to_xstring( it_solix = i_blob iv_size = l_length ).

          " NB: we assume that file has UTF-8 encoding
*  e_string = cl_abap_codepage=>convert_from( xstring ). " UTF-8 by default
        ENDIF.
      ELSE.
        message = download_text_file( EXPORTING fichero    = fichero
                                                 directorio = directorio
                                                 passive_mode = passive_mode
                                       IMPORTING i_tabla    = i_tabla ).
      ENDIF.

      IF no_desconectar IS INITIAL OR NOT message IS INITIAL.
        disconnect( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD lista_ficheros.
    IF handle IS INITIAL.
      message = connect( user               = user
                         password           = password
                         host               = host
                         encriptar_password = encriptar_password
                         destino_rfc        = destino_rfc
                         parametros         = parametros ).
    ENDIF.
    IF NOT message IS INITIAL.
      disconnect( ).
    ELSE.
      IF NOT directorio IS INITIAL.
        cd( directorio ).
        IF error_comando = 'X'.
          message = |Error cambiando a directorio { directorio } { mensaje_error-line }|.
          RETURN.
        ENDIF.
      ENDIF.

      i_ficheros = get_lista_ficheros( mascara = mascara ).
      IF error_comando = 'X'.
        message = |Error recuperando lista de ficheros  { mensaje_error-line }|.
        RETURN.
      ENDIF.

      IF no_desconectar IS INITIAL.
        disconnect( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD matar_sesiones_rfc.
    DATA:
      lt_servers TYPE TABLE OF msxxlist,
      lt_uinfo   TYPE TABLE OF uinfo,
      lt_usrlist TYPE TABLE OF usrinfo.

    FIELD-SYMBOLS: <ls_server> TYPE msxxlist,
                   <ls_user>   TYPE usrinfo.

    CALL FUNCTION 'TH_SERVER_LIST'
      TABLES
        list           = lt_servers
      EXCEPTIONS
        no_server_list = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TH_USER_LIST'
      TABLES
        list          = lt_uinfo
        usrlist       = lt_usrlist
      EXCEPTIONS
        auth_misssing = 1
        OTHERS        = 2.
    IF sy-subrc = 0.
      LOOP AT lt_servers ASSIGNING <ls_server>.
        LOOP AT lt_usrlist
             ASSIGNING <ls_user> WHERE     bname     = sy-uname
                                       AND rfc_type  = 'E'
                                       AND hostaddr  = '0.0.0.0'
                                       AND type      = '32'
                                       AND protocol  = -1
                                       AND tcode     = ''
                                       AND term     CS '.'.
          CALL 'ThSndDelUser' "#EC CI_CCALL
               ID 'MANDT'  FIELD sy-mandt
               ID 'BNAME'  FIELD <ls_user>-bname
               ID 'SERVER' FIELD <ls_server>-name
               ID 'TID'    FIELD <ls_user>-tid.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD mv.
    DATA: l_fichero_pc  TYPE string,
          l_fichero_ftp TYPE string,
          l_string      TYPE string.

    CONCATENATE '"' fichero_origen '"' INTO l_fichero_pc.
    CONCATENATE '"' fichero_destino '"' INTO l_fichero_ftp.
    CONCATENATE 'rename ' l_fichero_pc l_fichero_ftp INTO l_string SEPARATED BY space.

    IF command( comando = l_string index = -1 ) = '350'.
      ok = 'X'.
      CLEAR error_comando.
    ELSE.
      error_comando = 'X'.
      mensaje_error = linea+4.
    ENDIF.
  ENDMETHOD.
  METHOD put.
    DATA: l_nombrefichero TYPE string,
          l_fichero_pc    TYPE string,
          l_fichero_ftp   TYPE string,
          l_string        TYPE string.

    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = fichero
      IMPORTING
        stripped_name = l_nombrefichero
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.
    IF sy-subrc <> 0. "#EC EMPTY_IF_BRANCH
*    message 'Error obteniendo nombre de fichero' type 'S'.
    ENDIF.

    CONCATENATE '"' fichero '"' INTO l_fichero_pc.
    CONCATENATE '"' l_nombrefichero '"' INTO l_fichero_ftp.
    CONCATENATE 'put ' l_fichero_pc l_fichero_ftp INTO l_string SEPARATED BY space.

    IF command( comando = l_string index = -1 ) = '226'.
      ok = 'X'.
      CLEAR error_comando.
    ELSE.
      error_comando = 'X'.
      mensaje_error = linea+4.
    ENDIF.
  ENDMETHOD.
  METHOD reconectar.
    message = connect( user               = user
                       password           = password
                       host               = host
                       encriptar_password = ''
                       destino_rfc        = destino_rfc ).

    IF message IS INITIAL AND NOT me->directorio IS INITIAL.
      cd( me->directorio ).
      IF error_comando = 'X'.
        message = |Error cambiando a directorio { me->directorio } { mensaje_error-line }|.
        RETURN.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD upload_binary_file.
    TYPES t_linea TYPE c LENGTH 65000.

    DATA: l_xstring      TYPE xstring,
          lv_file_size   TYPE i,
          i_tabla_lineas TYPE TABLE OF t_linea,
          l_linea        TYPE t_linea,
          l_long         TYPE i,
          l_fichero      TYPE text1024,
          l_subrc        TYPE c LENGTH 4,
          lv_ftp_command TYPE string.

    FIELD-SYMBOLS <fs> TYPE any.

    IF handle IS INITIAL OR error_conexion = 'X'.
      mensaje_error = 'FTP no conectado'.
      message = mensaje_error.
      RETURN.
    ENDIF.

    IF NOT xstring IS INITIAL.
      l_xstring = xstring.
    ELSEIF NOT string IS INITIAL.
      l_xstring = zcl_ap_string=>string2xstring( string ).
    ENDIF.

    IF NOT l_xstring IS INITIAL.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = l_xstring
        IMPORTING
          output_length = lv_file_size
        TABLES
          binary_tab    = i_tabla_lineas.
    ELSE.
      LOOP AT i_tabla ASSIGNING <fs>.
        l_linea = <fs>.
        l_long = strlen( l_linea ).
        lv_file_size = lv_file_size + l_long.
        APPEND l_linea TO i_tabla_lineas.
      ENDLOOP.
    ENDIF.

* Set the passive mode - as necessary:
    IF passive_mode = abap_true.
      command( 'set passive on' ).
    ENDIF.

* Upload the file to the FTP host:
    l_fichero = zcl_ap_ficheros=>get_fichero_var( fichero = CONV #( fichero ) ).
    fichero_final = l_fichero.

    IF NOT directorio IS INITIAL.
      IF zcl_ap_ficheros=>ultima_letra_ruta( CONV #( directorio ) ) = '/'.
        CONCATENATE directorio fichero INTO l_fichero.
      ELSE.
        CONCATENATE directorio '/' fichero INTO l_fichero.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        handle        = handle
        fname         = l_fichero
        blob_length   = lv_file_size
      TABLES
        blob          = i_tabla_lineas
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.

    IF sy-subrc <> 0.
      l_subrc = sy-subrc.
      CASE l_subrc.
        WHEN 1. message = 'TCPIP ERROR'.
        WHEN 2.
          message = 'COMMAND ERROR'.
          IF NOT directorio IS INITIAL.
* Intentamos acotar si el error es por el directorio
            IF cd( directorio ) = ''.
              message = |Error accediendo a directorio { directorio }|.
              RETURN.
            ENDIF.
          ENDIF.
        WHEN 3. message = 'DATA ERROR'.
      ENDCASE.
      __concat4 message 'Error' l_subrc message 'subiendo fichero a FTP'.
    ENDIF.
  ENDMETHOD.
  METHOD upload_text_file.
    DATA: l_fichero      TYPE text1024,
          l_subrc        TYPE c LENGTH 4,
          lv_ftp_command TYPE string.

    IF handle IS INITIAL OR error_conexion = 'X'.
      mensaje_error = 'FTP no conectado'.
      message = mensaje_error.
      RETURN.
    ENDIF.

* Set the passive mode - as necessary:
    IF passive_mode = abap_true.
      command( 'set passive on' ).
    ENDIF.

* Turn on the ASCII transfer mode:
    command( 'ascii' ).

* Try to upload the file:
    l_fichero = zcl_ap_ficheros=>get_fichero_var( fichero = CONV #( fichero ) ).
    fichero_final = l_fichero.

    IF NOT directorio IS INITIAL.
      CONCATENATE '/' directorio '/' l_fichero INTO l_fichero.
      REPLACE ALL OCCURRENCES OF '//' IN l_fichero WITH '/'.
    ENDIF.

    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        handle         = handle
        fname          = l_fichero
        character_mode = 'X'
      TABLES
        text           = i_tabla
      EXCEPTIONS
        tcpip_error    = 1
        command_error  = 2
        data_error     = 3
        OTHERS         = 4.

    IF sy-subrc <> 0.
      l_subrc = sy-subrc.
      CASE l_subrc.
        WHEN 1. message = 'TCPIP ERROR'.
        WHEN 2. message = 'COMMAND ERROR'.
        WHEN 3. message = 'DATA ERROR'.
      ENDCASE.
      __concat4 message 'Error' l_subrc message 'subiendo fichero a FTP'.
    ENDIF.
  ENDMETHOD.
