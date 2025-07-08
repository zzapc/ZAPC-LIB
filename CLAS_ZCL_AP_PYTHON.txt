class ZCL_AP_PYTHON definition
  public
  final
  create public .

public section.

  data DIRECTORIO type STRING .
  data PYTHON_PATH type STRING .
  data MESSAGE type STRING .
  data INFO type ABAP_BOOL .
  data FICHERO_PY type STRING .
  data FICHERO_LOG type STRING .
  data FICHERO_BAT type STRING .
  data BORRAR type ABAP_BOOL .

  methods CONSTRUCTOR
    importing
      !DIRECTORIO type STRING default 'c:\python\'
      !INFO type ABAP_BOOL default ''
      !BORRAR type ABAP_BOOL default 'X' .
  methods GRABAR_SCRIPT
    importing
      !GRUPO type ZAP_TEXTOS_MAIL-GRUPO default 'PYTHON'
      !CODIGO type ZAP_TEXTOS_MAIL-CODIGO default ''
    exporting
      !MESSAGE type STRING
      !FICHERO type STRING
    changing
      !SCRIPT type STRING optional
      !NOMBRE_SCRIPT type STRING optional .
  methods EJECUTAR_SCRIPT
    importing
      !FICHERO type STRING
      !PARAMETROS type STRING default ''
    exporting
      !MESSAGE type STRING
      !SALIDA type STRING .
  methods EJECUTAR_FRONTEND
    importing
      !APP type STRING
      !PAR type STRING optional
    exporting
      !MESSAGE type STRING .
  class-methods GET_FILE_TEXT
    importing
      !FICHERO type STRING
    exporting
      !STRING type STRING
      !MESSAGE type STRING .
protected section.
private section.
endclass. "ZCL_AP_PYTHON definition
class ZCL_AP_PYTHON implementation.
METHOD constructor.

    me->directorio = directorio.
    me->info = info.
    me->borrar = 'borrar'.

    CALL METHOD cl_gui_frontend_services=>registry_get_value
      EXPORTING
        root                = cl_gui_frontend_services=>hkey_local_machine
        key                 = 'SOFTWARE\WOW6432Node\Python\PyLauncher'
        value               = ''
      IMPORTING
        reg_value           = me->python_path
      EXCEPTIONS
        get_regvalue_failed = 1
        cntl_error          = 2
        error_no_gui        = 3
        OTHERS              = 4.

    IF me->python_path IS INITIAL.
      get_file_text( EXPORTING fichero = |{ me->directorio }python_path.txt|
                     IMPORTING string  = me->python_path
                               message = message ).
      IF me->python_path IS INITIAL.
        DATA l_file TYPE string.
        l_file = |where python > { me->directorio }python_path.txt|.
        IF me->info = 'X'.
          CONCATENATE l_file 'pause' INTO l_file SEPARATED BY cl_abap_char_utilities=>cr_lf.
        ENDIF.
        zcl_ap_ficheros=>grabar_xstring( EXPORTING string        = |where python > { me->directorio }python_path.txt|
                                                   fichero       = |{ directorio }where_python.bat|
                                                   mostrar_error = ''
                                         IMPORTING mensaje       = DATA(l_msg) ).
        message = l_msg.
        IF message IS INITIAL.
          ejecutar_frontend( EXPORTING app     = |{ directorio }where_python.bat|
                             IMPORTING message = message ).
          IF message IS INITIAL.
            get_file_text( EXPORTING fichero = |{ me->directorio }python_path.txt|
                           IMPORTING string  = me->python_path
                                     message = message ).
            IF me->python_path IS INITIAL AND message IS INITIAL.
              message = 'No se ha podido encontrar ruta a python. Instalelo desde la tienda de Windows o desde https://www.python.org/downloads/windows/'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD ejecutar_frontend.

    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
*       document               =
        application            = app
        parameter              = par
        default_directory      = me->directorio
        maximized              = 'X'
*       minimized              =
        synchronous            = 'X'
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
        OTHERS                 = 10.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN '4'.
          message = |Aplicación { app } no existe|.
        WHEN OTHERS.
          message = |Error { sy-subrc }|.
      ENDCASE.
    ENDIF.

  ENDMETHOD.
METHOD ejecutar_script.
    DATA: l_par           TYPE string,
          l_contenido_bat TYPE string.

    CLEAR: message, salida.
    fichero_py = fichero.
    fichero_log = fichero_bat = to_lower( fichero ).
    REPLACE '.py' IN fichero_log WITH '_log.txt'.
    REPLACE '.py' IN fichero_bat WITH '.bat'.

    zcl_ap_ficheros=>borrar_fichero( fichero_log ).
    zcl_ap_ficheros=>borrar_fichero( fichero_bat ).

    l_contenido_bat = |{ me->python_path } { fichero_py } { parametros } > { fichero_log }|.
    IF me->info = 'X'.
      CONCATENATE l_contenido_bat 'pause' INTO l_contenido_bat SEPARATED BY cl_abap_char_utilities=>cr_lf.
    ENDIF.
    zcl_ap_ficheros=>grabar_xstring( EXPORTING string        = l_contenido_bat
                                               fichero       = fichero_bat
                                               mostrar_error = ''
                                     IMPORTING mensaje       = DATA(l_msg) ).
    message = l_msg.
    IF message IS INITIAL.
      ejecutar_frontend( EXPORTING app     = fichero_bat
                         IMPORTING message = message ).

      IF message IS INITIAL.
        get_file_text( EXPORTING fichero = fichero_log
                       IMPORTING string  = salida
                                 message = message ).

        IF salida CS 'No module named'.
          SPLIT salida AT 'No module named ''' INTO DATA(l_aux) DATA(l_modulo).
          SPLIT l_modulo AT '''' INTO l_modulo l_aux.
          IF zcl_ap_popup=>confirmar( |¿Quiere instalar modulo python { l_modulo }?| ).
            l_contenido_bat = |pip install { l_modulo }|.
            CONCATENATE l_contenido_bat 'pause' INTO l_contenido_bat SEPARATED BY cl_abap_char_utilities=>cr_lf.
            zcl_ap_ficheros=>grabar_xstring( EXPORTING string        = l_contenido_bat
                                                       fichero       = fichero_bat
                                                       mostrar_error = ''
                                             IMPORTING mensaje       = l_msg ).
            ejecutar_frontend( EXPORTING app     = fichero_bat
                               IMPORTING message = message ).
            IF message IS INITIAL.
              message = |Se ha instalado el módulo { l_modulo }. Reintente lanzar el script|.
            ENDIF.
          ENDIF.
        elseif me->borrar = 'X' and message is initial.
          zcl_ap_ficheros=>borrar_fichero( fichero ).
          zcl_ap_ficheros=>borrar_fichero( fichero_log ).
          zcl_ap_ficheros=>borrar_fichero( fichero_bat ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD get_file_text.

    CLEAR: message, string.
    zcl_ap_ficheros=>leer_xstring( EXPORTING fichero       = fichero
                                             mostrar_error = ''
                                             get_string    = 'X'
                                   IMPORTING string        = string
                                             message       = DATA(l_msg) ).
    message = l_msg.
    IF NOT string IS INITIAL.
      DATA(l_long) = strlen( string ).
      IF l_long > 2.
        SUBTRACT 2 FROM l_long.
        IF string+l_long = cl_abap_char_utilities=>cr_lf.
          string = string(l_long).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
METHOD grabar_script.

    IF script IS INITIAL AND NOT codigo IS INITIAL.
      SELECT SINGLE texto FROM zap_textos_mail
        INTO script
       WHERE grupo = grupo
         AND codigo = codigo.
      IF sy-subrc = 0.
        IF nombre_script IS INITIAL.
          nombre_script = codigo && '.py'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF script IS INITIAL.
      message = 'No se ha definido script'.
      RETURN.
    ENDIF.

    fichero = zcl_ap_ficheros=>concat_ruta( directorio = me->directorio fichero = nombre_script ).


    zcl_ap_ficheros=>grabar_xstring( EXPORTING string        = script
                                               fichero       = fichero
                                               mostrar_error = ''
                                     IMPORTING mensaje       = DATA(l_msg) ).
    message = l_msg.

  ENDMETHOD.
