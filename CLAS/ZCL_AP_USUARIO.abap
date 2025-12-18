CLASS zcl_ap_usuario DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS get_email
      IMPORTING !uname       TYPE any DEFAULT sy-uname
      RETURNING VALUE(email) TYPE ad_smtpadr.

    CLASS-METHODS get_nombre
      IMPORTING !uname        TYPE sy-uname DEFAULT sy-uname
                fecha         TYPE sy-datum DEFAULT sy-datum
                campo         TYPE string   DEFAULT 'name_text'
      PREFERRED PARAMETER uname
      RETURNING VALUE(nombre) TYPE adrp-name_text.

    CLASS-METHODS get_defaults
      IMPORTING !uname          TYPE sy-uname DEFAULT sy-uname
      PREFERRED PARAMETER uname
      RETURNING VALUE(defaults) TYPE bapidefaul.

    CLASS-METHODS get_telefono
      IMPORTING !uname          TYPE sy-uname DEFAULT sy-uname
      RETURNING VALUE(telefono) TYPE adr2-tel_number.

    CLASS-METHODS get_apellido
      IMPORTING !uname          TYPE sy-uname DEFAULT sy-uname
                fecha           TYPE sy-datum DEFAULT sy-datum
      PREFERRED PARAMETER uname
      RETURNING VALUE(apellido) TYPE adrp-name_last.

    CLASS-METHODS get_impresora
      IMPORTING !uname           TYPE sy-uname DEFAULT sy-uname
      PREFERRED PARAMETER uname
      RETURNING VALUE(impresora) TYPE rspopname.

    CLASS-METHODS get_print_parameters
      IMPORTING !uname            TYPE any DEFAULT sy-uname
      RETURNING VALUE(pri_params) TYPE pri_params.

    CLASS-METHODS get_total_modos_activos
      IMPORTING !uname       TYPE any DEFAULT ''
      RETURNING VALUE(modos) TYPE int4.

  PROTECTED SECTION.
  PRIVATE SECTION.
endclass. "ZCL_AP_USUARIO definition
class ZCL_AP_USUARIO implementation.
  METHOD get_apellido.
    DATA: l_usr21 TYPE usr21,
          l_adrnr TYPE cdpos-value_old.

    CLEAR apellido.
    SELECT SINGLE persnumber FROM usr21
      INTO l_usr21-persnumber
     WHERE bname = uname.
    IF sy-subrc = 0.
      SELECT SINGLE name_last FROM adrp
        INTO apellido
       WHERE persnumber = l_usr21-persnumber.
    ENDIF.

* Si han especificado una fecha anterior a la del sistemas es que quieren
* obtener el valor que tenia a una fecha dada
    IF fecha >= sy-datum OR fecha IS INITIAL.
      RETURN.
    ENDIF.

    l_adrnr = zcl_ap_control_cambios=>get_valor_a_fecha( objectclas = 'IDENTITY'
                                                         objectid   = uname
                                                         tabname    = 'USR21'
                                                         fname      = 'PERSNUMBER'
                                                         fecha      = fecha ).
    IF l_adrnr <> ''.
      IF l_adrnr <> l_usr21-persnumber.
        SELECT name_last FROM adrp
          INTO apellido
          UP TO 1 ROWS
         WHERE persnumber = l_adrnr
         ORDER BY date_from DESCENDING.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_defaults.
    DATA i_return TYPE TABLE OF bapiret2.

    CLEAR defaults.
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = uname
      IMPORTING
        defaults = defaults
      TABLES
        return   = i_return.
  ENDMETHOD.
  METHOD get_email.
    CLEAR email.
    SELECT adr6~smtp_addr INTO email
      UP TO 1 ROWS
     FROM usr21 JOIN adr6 ON  usr21~persnumber = adr6~persnumber
                          AND usr21~addrnumber = adr6~addrnumber
     WHERE usr21~bname = uname
     ORDER BY date_from DESCENDING.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_impresora.
    DATA l_def TYPE bapidefaul.

    l_def = zcl_ap_usuario=>get_defaults( uname ).
    impresora = l_def-spld.
  ENDMETHOD.
  METHOD get_nombre.
    DATA: l_usr21 TYPE usr21,
          l_adrnr TYPE cdpos-value_old.

    CLEAR nombre.
    SELECT SINGLE persnumber FROM usr21
      INTO l_usr21-persnumber
     WHERE bname = uname.
    IF sy-subrc = 0.
      SELECT SINGLE (campo) FROM adrp
        INTO nombre
       WHERE persnumber = l_usr21-persnumber.
    ENDIF.

* Si han especificado una fecha anterior a la del sistemas es que quieren
* obtener el valor que tenia a una fecha dada
    IF fecha >= sy-datum OR fecha IS INITIAL.
      RETURN.
    ENDIF.

    l_adrnr = zcl_ap_control_cambios=>get_valor_a_fecha( objectclas = 'IDENTITY'
                                                         objectid   = uname
                                                         tabname    = 'USR21'
                                                         fname      = 'PERSNUMBER'
                                                         fecha      = fecha ).
    IF l_adrnr <> ''.
      IF l_adrnr <> l_usr21-persnumber.
        SELECT (campo) FROM adrp
          INTO nombre
          UP TO 1 ROWS
         WHERE persnumber = l_adrnr
        ORDER BY PRIMARY KEY.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_print_parameters.
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        mode           = 'BATCH'
        report         = sy-cprog
        no_dialog      = 'X'
        immediately    = ' '
        new_list_id    = 'X'
        release        = 'X'
*       destination    = impresora
        user           = uname
      IMPORTING
*       out_archive_parameters = arc_param
        out_parameters = pri_params.
  ENDMETHOD.
  METHOD get_telefono.
    CLEAR telefono.
    SELECT adr2~tel_number INTO telefono
      UP TO 1 ROWS
     FROM usr21 JOIN adr2 ON  usr21~persnumber = adr2~persnumber
                          AND usr21~addrnumber = adr2~addrnumber
     WHERE usr21~bname = uname
     ORDER BY usr21~bname.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_total_modos_activos.
    DATA info TYPE TABLE OF uinfo2.

    CLEAR modos.
    CALL FUNCTION 'TH_LONG_USR_INFO'
      EXPORTING
        user      = uname
      TABLES
        user_info = info.

    modos = lines( info ).
  ENDMETHOD.
