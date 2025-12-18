CLASS zcl_ap_repo DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA i_repo        TYPE zap_repo_t.
    DATA ruta_pc_local TYPE string VALUE 'D:\BC\Reports slnk\LIB' ##NO_TEXT.
    DATA i_codigo      TYPE soli_tab.

    METHODS constructor
      IMPORTING origen        TYPE char1  DEFAULT 'F'
                ruta_pc_local TYPE string DEFAULT ''
      PREFERRED PARAMETER origen.

    METHODS conexion
      IMPORTING password TYPE any.

    METHODS desconexion.

    METHODS importar_slnk
      IMPORTING fichero TYPE string.

    METHODS importa_slnk_from_ftp
      IMPORTING repo TYPE zap_repo.

    METHODS activar_objeto
      IMPORTING !object TYPE trobjtype
                nombre  TYPE any.

    METHODS actualiza_indice_repo.

    METHODS actualizar_fichero
      IMPORTING fichero TYPE any.

    CLASS-METHODS existe_objeto
      IMPORTING !object       TYPE trobjtype
                nombre        TYPE any
      RETURNING VALUE(existe) TYPE abap_bool.

    CLASS-METHODS borrar_report
      IMPORTING nombre TYPE any.

    METHODS instalar_report_temp
      IMPORTING nombre TYPE any.

    CLASS-METHODS get_clase_desarrollo
      IMPORTING !object         TYPE trobjtype
                nombre          TYPE any
      RETURNING VALUE(devclass) TYPE devclass.

    CLASS-METHODS cambiar_clase_desarrollo
      IMPORTING !object  TYPE trobjtype
                nombre   TYPE any
                devclass TYPE devclass.

    CLASS-METHODS check_update
      IMPORTING !report       TYPE sy-cprog  DEFAULT sy-cprog
                !version      TYPE p         OPTIONAL
                mostrar_error TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER report.


  PRIVATE SECTION.
    DATA o_ftp  TYPE REF TO zcl_ap_ftp.
    DATA origen TYPE char1.

    METHODS get_nombre_slnk
      IMPORTING nombre      TYPE any
                !object     TYPE trobjtype
      RETURNING VALUE(slnk) TYPE string.

    METHODS get_codigo
      IMPORTING url           TYPE saeuri
                mostrar_error TYPE abap_bool DEFAULT ''.
endclass. "ZCL_AP_REPO definition
class ZCL_AP_REPO implementation.
  METHOD activar_objeto.
    DATA: l_object   TYPE e071-object,
          l_obj_name TYPE e071-obj_name.

    l_object = object.
    l_obj_name = nombre.

    CALL FUNCTION 'RS_WORKING_OBJECT_ACTIVATE'
      EXPORTING
        object                           = l_object
        obj_name                         = l_obj_name
*   FORCE_ACTIVATION                 = ' '
*   ACTIVATE_ONLY_THIS_OBJECT        =
*   OBJECT_SAVED                     =
*   DICTIONARY_ONLY                  = ' '
*   P_WB_MANAGER                     =
*   P_CALLER_PROGRAM                 =
*   CWB_MODE                         =
*   DISPLAY_SYSID                    = ' '
* IMPORTING
*   BIND_ERROR_WINDOW                =
* TABLES
*   OBJECTS                          =
     EXCEPTIONS
       object_not_in_working_area       = 1
       execution_error                  = 2
       cancelled                        = 3
       insert_into_corr_error           = 4
       OTHERS                           = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD actualiza_indice_repo.
    DATA: l_xml      TYPE string,
          l_dir_temp TYPE string,
          l_fichero  TYPE string.

    CALL TRANSFORMATION id
         SOURCE tabla = i_repo
         RESULT XML l_xml.

    IF origen = 'F'.
      l_dir_temp = zcl_ap_documentos=>get_directorio_temporal( ).
    ELSE.
      l_dir_temp = ruta_pc_local.
    ENDIF.

    CONCATENATE l_dir_temp '\repo.xml' INTO l_fichero.

    zcl_ap_ficheros=>grabar_xml( fichero = l_fichero string = l_xml ).

    IF origen = 'F'.
      o_ftp->put( l_fichero ).
    ENDIF.
  ENDMETHOD.
  METHOD actualizar_fichero.
    IF origen = 'F'.
      o_ftp->put( fichero ).
    ENDIF.
  ENDMETHOD.
  METHOD borrar_report.
    DATA program TYPE sy-repid.

    program = nombre.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
*   CORRNUMBER                       =
        program                          = program
*   SUPPRESS_CHECKS                  = ' '
*   SUPPRESS_COMMIT                  = ' '
        suppress_popup                   = 'X'
*   MASS_DELETE_CALL                 = ' '
*   WITH_CUA                         = 'X'
*   WITH_DOCUMENTATION               = 'X'
*   WITH_DYNPRO                      = 'X'
*   WITH_INCLUDES                    = ' '
*   WITH_TEXTPOOL                    = 'X'
*   WITH_VARIANTS                    = 'X'
*   TADIR_DEVCLASS                   =
*   SKIP_PROGRESS_IND                = ' '
*   FORCE_DELETE_USED_INCLUDES       = ' '
* IMPORTING
*   CORRNUMBER                       =
*   PROGRAM                          =
   EXCEPTIONS
     enqueue_lock                     = 1
     object_not_found                 = 2
     permission_failure               = 3
     reject_deletion                  = 4
     OTHERS                           = 5.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.
  METHOD cambiar_clase_desarrollo.
    DATA l_obj_name TYPE e071-obj_name.

    l_obj_name = nombre.

    CALL FUNCTION 'TR_TADIR_POPUP_ENTRY_E071'
      EXPORTING
        wi_e071_pgmid             = 'R3TR'
        wi_e071_object            = object
        wi_e071_obj_name          = l_obj_name
        wi_tadir_devclass         = devclass
      EXCEPTIONS
        display_mode              = 1
        exit                      = 2
        global_tadir_insert_error = 3
        no_repair_selected        = 4
        no_systemname             = 5
        no_systemtype             = 6
        no_tadir_type             = 7
        reserved_name             = 8
        tadir_enqueue_failed      = 9
        devclass_not_found        = 10
        tadir_not_exist           = 11
        object_exists             = 12
        internal_error            = 13
        object_append_error       = 14
        tadir_modify_error        = 15
        object_locked             = 16
        no_object_authority       = 17
        OTHERS                    = 18.
  ENDMETHOD.
  METHOD check_update.
    DATA: o_repo    TYPE REF TO zcl_ap_repo,
          l_url     TYPE saeuri,
          l_linea   TYPE soli,
          l_aux1    TYPE soli,
          l_aux2    TYPE soli,
          l_version TYPE p LENGTH 8 DECIMALS 0.

    DATA: lv_msg  TYPE string,
          " TODO: variable is assigned but never used (ABAP cleaner)
          lv_line TYPE n LENGTH 1,
          " TODO: variable is assigned but never used (ABAP cleaner)
          lv_word TYPE char50.

    o_repo = NEW #( ).

    CONCATENATE 'http://sap4.com/repo/' report '.txt' INTO l_url.
    TRANSLATE l_url TO LOWER CASE.
    o_repo->get_codigo( url = l_url mostrar_error = mostrar_error ).

    LOOP AT o_repo->i_codigo INTO l_linea.
      TRANSLATE l_linea TO UPPER CASE.
      IF l_linea CS 'C_VERSION'.
        SPLIT l_linea AT 'VALUE' INTO l_aux1 l_aux2.
        l_version = zcl_ap_string=>limpia_numeros( l_aux2 ).
        EXIT.
      ENDIF.
    ENDLOOP.

    IF l_version IS INITIAL.
      RETURN.
    ENDIF.

    IF l_version > version.
      WRITE l_version TO l_aux2(7).
      CONCATENATE 'Nueva versión'(NVE) l_aux2 INTO l_aux1 SEPARATED BY space.
      IF zcl_ap_popup=>confirmar( titulo = 'Actualización del programa!'(APR)
                                  texto  = l_aux1
                                  texto2 = '¿Desea actualizar la versión?'(AVR) ) = 'X'.
        SYNTAX-CHECK FOR o_repo->i_codigo
                     MESSAGE lv_msg
                     LINE lv_line
                     WORD lv_word
                     PROGRAM sy-repid.
        IF sy-subrc <> 0.
          MESSAGE i398(00) WITH lv_msg '' '' ''.
        ELSE.
          INSERT REPORT report FROM o_repo->i_codigo.         "#EC *
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD conexion.
    DATA: l_fichero_pc TYPE string,
          l_ok         TYPE c LENGTH 1,
          l_xml        TYPE string.

    FIELD-SYMBOLS <repo> TYPE zap_repo.

    IF origen = 'F'.
      o_ftp = NEW #( ).

      o_ftp->connect( user = 'sap4abap' password = password host = 'sap4.com' ).

      IF NOT o_ftp->error_conexion IS INITIAL.
        MESSAGE 'Error de conexión'(ECO) TYPE 'E'.
      ELSE.
        IF o_ftp->cd( 'repo' ) <> 'X'.
          MESSAGE 'Error al cambiar al directorio REPO'(ECD) TYPE 'E'.
        ENDIF.

        o_ftp->get( EXPORTING fichero = 'repo.xml'
                              dir_temp = 'X'
                    IMPORTING fichero_pc = l_fichero_pc
                              ok = l_ok ).

        IF l_ok IS INITIAL.
          MESSAGE 'Error al recuperar el fichero indice de repositorio'(EFI) TYPE 'I'.
        ENDIF.
      ENDIF.
    ELSE.
      CONCATENATE ruta_pc_local '\repo.xml' INTO l_fichero_pc.
    ENDIF.

    IF NOT l_fichero_pc IS INITIAL.
      zcl_ap_ficheros=>leer_xml( EXPORTING fichero = l_fichero_pc
                                        mostrar_error = ''
                              IMPORTING string = l_xml ).

      IF l_xml IS INITIAL.
        MESSAGE 'No existen elementos en el repositorio'(NER) TYPE 'I'.
      ELSE.
        CALL TRANSFORMATION id
             SOURCE XML l_xml
             RESULT tabla = i_repo.

        LOOP AT i_repo ASSIGNING <repo>.
          <repo>-devclass = get_clase_desarrollo( object = <repo>-object nombre = <repo>-nombre ).
          IF <repo>-devclass IS INITIAL.
            <repo>-lights = zcl_ap_alv_grid=>c_sem_rojo.
          ELSE.
            <repo>-lights = zcl_ap_alv_grid=>c_sem_verde.

            SELECT libreria
              FROM zbc003
              WHERE name = @<repo>-nombre
              ORDER BY PRIMARY KEY
              INTO @<repo>-libreria
              UP TO 1 ROWS.
            ENDSELECT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    me->origen = origen.
    IF NOT ruta_pc_local IS INITIAL.
      me->ruta_pc_local = ruta_pc_local.
    ENDIF.
  ENDMETHOD.
  METHOD desconexion.
    IF NOT o_ftp IS INITIAL.
      o_ftp->disconnect( ).
    ENDIF.
  ENDMETHOD.
  METHOD existe_objeto.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_tadir TYPE tadir.

    CLEAR existe.
    SELECT SINGLE * FROM tadir
      INTO l_tadir
     WHERE pgmid    = 'R3TR'
       AND object   = object
       AND obj_name = nombre.
    IF sy-subrc = 0.
      existe = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD get_clase_desarrollo.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_trdir TYPE trdir.

    CLEAR devclass.
    IF object = 'PROG'.
      SELECT SINGLE name FROM trdir
       INTO l_trdir-name
      WHERE name = nombre.
      IF sy-subrc <> 0.
        return.
      ENDIF.
    ENDIF.

    SELECT SINGLE devclass FROM tadir
      INTO devclass
     WHERE pgmid    = 'R3TR'
       AND object   = object
       AND obj_name = nombre.
  ENDMETHOD.
  METHOD get_codigo.
    i_codigo = zcl_ap_string=>get_text_from_url( url = url mostrar_error = mostrar_error ).
  ENDMETHOD.
  METHOD get_nombre_slnk.
    slnk = nombre.
    slnk = to_upper( slnk ).
    CONCATENATE object '_' slnk '.slnk' INTO slnk.
  ENDMETHOD.
  METHOD importa_slnk_from_ftp.
    DATA: l_ok           TYPE c LENGTH 1,
          l_devclass     TYPE devclass,
          l_fichero_slnk TYPE string,
          l_fichero_pc   TYPE string,
          l_long         TYPE i.
    DATA: i_ficheros TYPE TABLE OF file_info,
          l_fichero  TYPE file_info.

    IF existe_objeto( object = repo-object
                      nombre = repo-nombre ) = 'X'.
      l_ok = zcl_ap_popup=>confirmar( texto  = 'Objecto ya existe'(OYE)
                                      texto2 = '¿Desea actualizarlo desde el fichero?'(DAF) ).
      l_devclass = get_clase_desarrollo( object = repo-object
                                         nombre = repo-nombre ).
    ELSE.
      l_ok = 'X'.
    ENDIF.

    IF l_ok <> 'X'.
      RETURN.
    ENDIF.

    l_fichero_slnk = get_nombre_slnk( nombre = repo-nombre
                                      object = repo-object ).

    IF origen = 'F'.
      o_ftp->get( EXPORTING fichero = l_fichero_slnk
                          dir_temp = 'X'
                IMPORTING fichero_pc = l_fichero_pc
                          ok = l_ok ).
    ELSE.
      l_fichero_pc = zcl_ap_ficheros=>concat_ruta( directorio = ruta_pc_local
                                                   fichero = l_fichero_slnk ).

      IF zcl_ap_ficheros=>existe( l_fichero_pc ) = ''.
        CLEAR l_fichero_pc.
        i_ficheros = zcl_ap_ficheros=>lista_ficheros( ruta_pc_local ).
        l_long = strlen( l_fichero_slnk ) - 5.
        IF l_long > 4.
          LOOP AT i_ficheros INTO l_fichero WHERE filename CS l_fichero_slnk(l_long).
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            l_fichero_pc = l_fichero-filename.
          ENDIF.
        ENDIF.
      ENDIF.

      IF NOT l_fichero_pc IS INITIAL.
        importar_slnk( l_fichero_pc ).

        cambiar_clase_desarrollo( nombre = repo-nombre
                                  object = repo-object
                                  devclass = l_devclass ).

        activar_objeto( object = repo-object nombre = repo-nombre ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD importar_slnk.
    DATA: isslinkee TYPE c LENGTH 1,
          l_file    TYPE c LENGTH 300.

    isslinkee = 'X'.
    EXPORT isslinkee = isslinkee TO MEMORY ID 'ISSLNK'.

    l_file = fichero.
    SUBMIT zsaplink                                        "#EC CI_SUBMIT
     AND RETURN
           WITH filename = l_file
           WITH overwr   = 'X'.
  ENDMETHOD.
  METHOD instalar_report_temp.
    DATA: l_existe TYPE c LENGTH 1,
          l_repo   TYPE zap_repo.

    l_existe = existe_objeto( object = 'PROG' nombre = nombre ).

    l_repo-object = 'PROG'.
    l_repo-nombre = nombre.
    importa_slnk_from_ftp( repo = l_repo ).

    IF existe_objeto( object = 'PROG' nombre = nombre ) = 'X'.
      SUBMIT (nombre) "#EC CI_SUBMIT
             VIA SELECTION-SCREEN.
    ENDIF.

    IF l_existe IS INITIAL.
      borrar_report( nombre ).
    ENDIF.
  ENDMETHOD.
