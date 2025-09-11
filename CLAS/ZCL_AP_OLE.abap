TYPE-POOLS OLE2 .
CLASS zcl_ap_ole DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPE-POOLS ole2.
    DATA app       TYPE ole2_object.
    DATA documento TYPE ole2_object.
    DATA hay_error TYPE char1.
    DATA selection TYPE ole2_object.

    CONSTANTS true  TYPE i VALUE 1. "#EC NOTEXT
    CONSTANTS false TYPE i VALUE 0. "#EC NOTEXT

    METHODS constructor
      IMPORTING objeto          TYPE string
                o_log           TYPE REF TO zcl_ap_log OPTIONAL
                mostrar_errores TYPE char1             DEFAULT ''.

    METHODS set_prop
      IMPORTING obj   TYPE ole2_object
                prop  TYPE string
                valor TYPE any.

    METHODS set_prop_app
      IMPORTING obj   TYPE ole2_object OPTIONAL
                prop  TYPE string
                valor TYPE any.

    METHODS get_obj
      IMPORTING origen     TYPE ole2_object
                metodo     TYPE string
                p1         TYPE any OPTIONAL
                p2         TYPE any OPTIONAL
      RETURNING VALUE(obj) TYPE ole2_object.

    TYPE-POOLS abap.

    METHODS exec_metodo
      IMPORTING obj             TYPE ole2_object
                p1              TYPE any       OPTIONAL
                p2              TYPE any       OPTIONAL
                p3              TYPE any       OPTIONAL
                p4              TYPE any       OPTIONAL
                p5              TYPE any       OPTIONAL
                metodo          TYPE string
                no_write        TYPE any       DEFAULT ''
                mostrar_errores TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(error)    TYPE abap_bool.

    METHODS get_obj_app
      IMPORTING metodo     TYPE string
                p1         TYPE any OPTIONAL
                p2         TYPE any OPTIONAL
      RETURNING VALUE(obj) TYPE ole2_object.

    METHODS get_seleccion.

    METHODS exec_metodo_seleccion
      IMPORTING p1     TYPE any OPTIONAL
                p2     TYPE any OPTIONAL
                p3     TYPE any OPTIONAL
                p4     TYPE any OPTIONAL
                p5     TYPE any OPTIONAL
                metodo TYPE string.

    METHODS free.

    METHODS get_prop
      IMPORTING obj          TYPE ole2_object
                prop         TYPE string
      RETURNING VALUE(valor) TYPE string.

    METHODS get_prop_obj
      IMPORTING obj          TYPE ole2_object
                prop         TYPE string
      RETURNING VALUE(valor) TYPE ole2_object.

    METHODS get_obj_sel
      IMPORTING metodo     TYPE string
                p1         TYPE any OPTIONAL
                p2         TYPE any OPTIONAL
      RETURNING VALUE(obj) TYPE ole2_object.

    METHODS set_prop_sel
      IMPORTING obj   TYPE ole2_object OPTIONAL
                prop  TYPE string
                valor TYPE any.

  PROTECTED SECTION.
    DATA mostrar_errores TYPE char1.
    DATA o_log           TYPE REF TO zcl_ap_log.

    TYPE-POOLS abap.

    METHODS control_error
      IMPORTING texto           TYPE any
                texto2          TYPE any       OPTIONAL
                texto3          TYPE any       OPTIONAL
                mostrar_errores TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(error)    TYPE abap_bool.

    METHODS open_file
      IMPORTING obj     TYPE any
                fichero TYPE string.

    METHODS save_file
      IMPORTING obj     TYPE any
                fichero TYPE string.

  PRIVATE SECTION.
endclass. "ZCL_AP_OLE definition
class ZCL_AP_OLE implementation.
  METHOD constructor.
    DATA l_obj TYPE c LENGTH 100.

    me->mostrar_errores = mostrar_errores.
    me->o_log           = o_log.

    l_obj = objeto.
    CREATE OBJECT app l_obj.
    control_error( texto  = 'Error al crear objeto'
                   texto2 = objeto ).
  ENDMETHOD.
  METHOD control_error.
    DATA l_subrc TYPE sy-subrc.

    l_subrc = sy-subrc.

    IF l_subrc <> 0.
      error = 'X'.
      IF mostrar_errores = 'X'.
        IF NOT o_log IS INITIAL.
          o_log->l_msg2 = texto2.
          o_log->l_msg3 = texto3.
          o_log->msg_error( msgv1 = texto
                            msgv2 = o_log->l_msg2
                            msgv3 = o_log->l_msg3 ).
          IF me->mostrar_errores = 'X'.
            o_log->grabar( ).
          ENDIF.
        ENDIF.
      ENDIF.

      hay_error = 'X'.
      SET PROPERTY OF app 'Visible' = 1.

      IF mostrar_errores = 'X'.
        IF me->mostrar_errores = 'X'.
          MESSAGE e398(00) WITH texto texto2 texto3.
        ELSEIF me->mostrar_errores = 'I'.
          MESSAGE s398(00) WITH texto texto2 texto3.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD exec_metodo.
    DATA: l_metodo  TYPE ole_verb,
          l_comando TYPE c LENGTH 80,
          l_p1      TYPE c LENGTH 20,
          l_p2      TYPE c LENGTH 20,
          l_p3      TYPE c LENGTH 20,
          l_p4      TYPE c LENGTH 20,
          l_p5      TYPE c LENGTH 20.

    IF NOT obj IS INITIAL.
      l_metodo = metodo.
      IF NOT p1 IS SUPPLIED.
        IF NOT p2 IS SUPPLIED.
          CALL METHOD OF obj l_metodo.
        ELSE.
          IF no_write IS INITIAL.
            WRITE p2 TO l_comando.
          ENDIF.

          CALL METHOD OF obj l_metodo
            EXPORTING
            #2 = p2.
        ENDIF.
      ELSEIF NOT p2 IS SUPPLIED.
        IF no_write IS INITIAL.
          WRITE p1 TO l_comando.
        ENDIF.

        CALL METHOD OF obj l_metodo
          EXPORTING
          #1 = p1.
      ELSEIF NOT p3 IS SUPPLIED.
        IF no_write IS INITIAL.
          WRITE: p1 TO l_p1, p2 TO l_p2.
          CONDENSE: l_p1, l_p2.
          CONCATENATE l_p1 l_p2 INTO l_comando SEPARATED BY space.
        ENDIF.
        CALL METHOD OF obj l_metodo
          EXPORTING
          #1 = p1
          #2 = p2.
      ELSEIF NOT p4 IS SUPPLIED.
        IF no_write IS INITIAL.
          WRITE: p1 TO l_p1, p2 TO l_p2, p3 TO l_p3.
          CONDENSE: l_p1, l_p2, l_p3.
          CONCATENATE l_p1 l_p2 l_p3 INTO l_comando SEPARATED BY space.
        ENDIF.

        CALL METHOD OF obj l_metodo
          EXPORTING
          #1 = p1
          #2 = p2
          #3 = p3.
      ELSEIF NOT p5 IS SUPPLIED.
        IF no_write IS INITIAL.
          WRITE: p1 TO l_p1, p2 TO l_p2, p3 TO l_p3, p4 TO l_p4.
          CONDENSE: l_p1, l_p2, l_p3, l_p4.
          CONCATENATE l_p1 l_p2 l_p3 l_p4 INTO l_comando SEPARATED BY space.
        ENDIF.

        CALL METHOD OF obj l_metodo
          EXPORTING
          #1 = p1
          #2 = p2
          #3 = p3
          #4 = p4.
      ELSE.
        IF no_write IS INITIAL.
          WRITE: p1 TO l_p1, p2 TO l_p2, p3 TO l_p3, p4 TO l_p4, p5 TO l_p5.
          CONDENSE: l_p1, l_p2, l_p3, l_p4, l_p5.
          CONCATENATE l_p1 l_p2 l_p3 l_p4 l_p5 INTO l_comando SEPARATED BY space.
        ENDIF.

        CALL METHOD OF obj l_metodo
          EXPORTING
          #1 = p1
          #2 = p2
          #3 = p3
          #4 = p4
          #5 = p5.
      ENDIF.

      error = control_error( texto = 'Exec Obj:' texto2 = metodo texto3 = l_comando mostrar_errores = mostrar_errores ).
    ELSE.
      error = control_error( texto = 'Exec Obj:' texto2 = metodo
                     texto3 = 'Obj. no existe' mostrar_errores = mostrar_errores ).
    ENDIF.
  ENDMETHOD.
  METHOD exec_metodo_seleccion.
    IF p1 IS SUPPLIED AND p2 IS SUPPLIED AND p3 IS SUPPLIED AND p4 IS SUPPLIED AND p5 IS SUPPLIED.
      exec_metodo( obj = selection
                   metodo = metodo
                   p1 = p1 p2 = p2 p3 = p3 p4 = p4 p5 = p5 ).
    ELSEIF p1 IS SUPPLIED AND p2 IS SUPPLIED AND p3 IS SUPPLIED AND p4 IS SUPPLIED.
      exec_metodo( obj = selection
                   metodo = metodo
                   p1 = p1 p2 = p2 p3 = p3 p4 = p4 ).
    ELSEIF p1 IS SUPPLIED AND p2 IS SUPPLIED AND p3 IS SUPPLIED.
      exec_metodo( obj = selection
                   metodo = metodo
                   p1 = p1 p2 = p2 p3 = p3 ).
    ELSEIF p1 IS SUPPLIED AND p2 IS SUPPLIED.
      exec_metodo( obj = selection
                   metodo = metodo
                   p1 = p1 p2 = p2 ).
    ELSEIF p1 IS SUPPLIED.
      exec_metodo( obj = selection
                   metodo = metodo
                   p1 = p1 ).
    ELSE.
      exec_metodo( obj = selection
                   metodo = metodo ).
    ENDIF.
  ENDMETHOD.
  METHOD free.
    FREE OBJECT: app, documento, selection.
  ENDMETHOD.
  METHOD get_obj.
    DATA l_metodo TYPE ole_verb.

    IF NOT origen IS INITIAL.
      l_metodo = metodo.
      IF NOT p1 IS SUPPLIED.
        CALL METHOD OF origen l_metodo = obj.
      ELSEIF NOT p2 IS SUPPLIED.
        CALL METHOD OF origen l_metodo = obj
          EXPORTING
            #1 = p1.
      ELSE.
        CALL METHOD OF origen l_metodo = obj
          EXPORTING
            #1 = p1
            #1 = p2.
      ENDIF.
      control_error( texto = 'Get Obj:' texto2 = metodo ).
    ELSE.
      control_error( texto = 'Get Obj:' texto2 = metodo
                     texto3 = 'Obj.Origen no existe' ).
    ENDIF.
  ENDMETHOD.
  METHOD get_obj_app.
    IF NOT p1 IS SUPPLIED.
      obj = get_obj( origen = app metodo = metodo ).
    ELSEIF NOT p2 IS SUPPLIED.
      obj = get_obj( origen = app metodo = metodo p1 = p1 ).
    ELSE.
      obj = get_obj( origen = app metodo = metodo p1 = p1 p2 = p2 ).
    ENDIF.
  ENDMETHOD.
  METHOD get_obj_sel.
    IF NOT p1 IS SUPPLIED.
      obj = get_obj( origen = selection metodo = metodo ).
    ELSEIF NOT p2 IS SUPPLIED.
      obj = get_obj( origen = selection metodo = metodo p1 = p1 ).
    ELSE.
      obj = get_obj( origen = selection metodo = metodo p1 = p1 p2 = p2 ).
    ENDIF.
  ENDMETHOD.
  METHOD get_prop.
    DATA: l_prop  TYPE ole_verb,
          l_valor TYPE string.

    IF NOT obj IS INITIAL.
      l_prop = prop.
      GET PROPERTY OF obj l_prop = valor.
      l_valor = valor.
      control_error( texto = 'Propiedad:' texto2 = prop texto3 = l_valor ).
    ELSE.
      control_error( texto = 'Objecto no asignado. Propiedad:'
                     texto2 = prop texto3 = l_valor ).
    ENDIF.
  ENDMETHOD.
  METHOD get_prop_obj.
    DATA: l_prop  TYPE ole_verb,
          l_valor TYPE ole2_object.

    IF NOT obj IS INITIAL.
      l_prop = prop.
      GET PROPERTY OF obj l_prop = valor.
      l_valor = valor.
      control_error( texto = 'Propiedad:' texto2 = prop texto3 = l_valor ).
    ELSE.
      control_error( texto = 'Objecto no asignado. Propiedad:'
                     texto2 = prop texto3 = l_valor ).
    ENDIF.
  ENDMETHOD.
  METHOD get_seleccion.
    IF NOT selection IS INITIAL.
      FREE OBJECT selection.
    ENDIF.

    selection = get_obj_app( metodo = 'Selection' ).
  ENDMETHOD.
  METHOD open_file.
    DATA l_existe TYPE char1.

    l_existe = cl_gui_frontend_services=>file_exist( fichero ).
    IF l_existe IS INITIAL.
      control_error( texto = 'No existe' texto2 = fichero ).
      IF mostrar_errores IS INITIAL.
        IF NOT o_log IS INITIAL.
          o_log->grabar( ).
        ENDIF.
        MESSAGE e398(00) WITH 'Error al leer fichero:' fichero.
      ENDIF.
    ELSE.
      control_error( texto = 'Se lee fichero' texto2 = fichero ).
    ENDIF.
    exec_metodo( obj = obj metodo = 'Open' p1 = fichero ).
  ENDMETHOD.
  METHOD save_file.
    exec_metodo( obj = obj metodo = 'SaveAs'
                 p1 = fichero ).
  ENDMETHOD.
  METHOD set_prop.
    DATA: l_prop  TYPE ole_verb,
          l_valor TYPE string.

    IF NOT obj IS INITIAL.
      l_prop = prop.
      SET PROPERTY OF obj l_prop = valor.
      l_valor = valor.
      control_error( texto = 'Propiedad:' texto2 = prop texto3 = l_valor ).
    ELSE.
      control_error( texto = 'Objecto no asignado. Propiedad:'
                     texto2 = prop texto3 = l_valor ).
    ENDIF.
  ENDMETHOD.
  METHOD set_prop_app.
    set_prop( obj = app prop = prop valor = valor ).
  ENDMETHOD.
  METHOD set_prop_sel.
    set_prop( obj = selection prop = prop valor = valor ).
  ENDMETHOD.
