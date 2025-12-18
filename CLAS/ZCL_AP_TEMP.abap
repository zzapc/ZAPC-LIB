CLASS zcl_ap_temp DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA subclave TYPE ztemp-subclave.

    METHODS constructor
      IMPORTING clave     TYPE ztemp-clave OPTIONAL
                subclave  TYPE any         OPTIONAL
                sub_auto  TYPE abap_bool   DEFAULT ''
                no_grabar TYPE abap_bool   DEFAULT ''.

    METHODS set
      IMPORTING valor1     TYPE any OPTIONAL
                valor2     TYPE any OPTIONAL
                valor3     TYPE any OPTIONAL
                texto      TYPE any OPTIONAL
                permanente TYPE any DEFAULT ''
      PREFERRED PARAMETER valor1.

    CLASS-METHODS set_st
      IMPORTING clave         TYPE any OPTIONAL
                subclave      TYPE any OPTIONAL
                indice        TYPE any OPTIONAL
                valor1        TYPE any OPTIONAL
                valor2        TYPE any OPTIONAL
                valor3        TYPE any OPTIONAL
                texto         TYPE any OPTIONAL
                permanente    TYPE any DEFAULT ''
                subclave_auto TYPE any DEFAULT ''
                texto2        TYPE any DEFAULT ''
                indice_auto   TYPE any DEFAULT ''
      PREFERRED PARAMETER valor1.

    CLASS-METHODS borrar_st
      IMPORTING clave    TYPE any
                subclave TYPE any         OPTIONAL
                ernam    TYPE ztemp-ernam OPTIONAL
                erdat    TYPE ztemp-erdat OPTIONAL.

    METHODS set_string
      IMPORTING !string    TYPE any OPTIONAL
                valor      TYPE any OPTIONAL
                permanente TYPE any DEFAULT ''.

    CLASS-METHODS set_string_st
      IMPORTING clave       TYPE any
                subclave    TYPE any           OPTIONAL
                indice      TYPE ztemps-indice OPTIONAL
                !string     TYPE any           OPTIONAL
                permanente  TYPE any           DEFAULT ''
                texto       TYPE any           DEFAULT ''
                indice_auto TYPE any           DEFAULT ''
                valor       TYPE any           DEFAULT ''.

    METHODS set_string_tabla
      IMPORTING tabla TYPE table        OPTIONAL
                valor TYPE ztemps-valor OPTIONAL.

    METHODS get_string
      IMPORTING valor         TYPE ztemps-valor OPTIONAL
      RETURNING VALUE(string) TYPE string.

    METHODS get_string_tabla
      IMPORTING valor TYPE ztemps-valor OPTIONAL
      CHANGING  tabla TYPE table.

    METHODS get_tabla
      RETURNING VALUE(tabla) TYPE ztab_temp.

    CLASS-METHODS get_st_valor1
      IMPORTING clave         TYPE any
                subclave      TYPE any OPTIONAL
                indice        TYPE any OPTIONAL
      RETURNING VALUE(valor1) TYPE ztemp-valor1.

    METHODS get_st_val1
      IMPORTING subclave      TYPE any  OPTIONAL
                indice        TYPE any  OPTIONAL
                erdat         TYPE dats OPTIONAL
      RETURNING VALUE(valor1) TYPE ztemp-valor1.

    METHODS borrar
      IMPORTING subclave TYPE ztemp-subclave OPTIONAL
                ernam    TYPE ztemp-ernam    OPTIONAL
                erdat    TYPE ztemp-erdat    OPTIONAL.

    CLASS-METHODS delete_st
      IMPORTING clave    TYPE any
                subclave TYPE any OPTIONAL
                indice   TYPE any OPTIONAL.

    CLASS-METHODS existe_st
      IMPORTING clave         TYPE any
                subclave      TYPE any OPTIONAL
                indice        TYPE any DEFAULT '000'
      RETURNING VALUE(existe) TYPE abap_bool.

    CLASS-METHODS get_string_st
      IMPORTING clave         TYPE any
                subclave      TYPE any DEFAULT ''
                indice        TYPE any DEFAULT '000'
                valor         TYPE any DEFAULT ''
      RETURNING VALUE(string) TYPE string.

    CLASS-METHODS borrar_antiguos
      IMPORTING clave TYPE ztemp-clave
                dias  TYPE i DEFAULT 30.

    CLASS-METHODS delete_string_st
      IMPORTING clave    TYPE any
                subclave TYPE any OPTIONAL
                indice   TYPE any OPTIONAL.

    METHODS delete_string
      IMPORTING indice TYPE any OPTIONAL.

    METHODS existe
      IMPORTING indice        TYPE ztemp-indice OPTIONAL
      RETURNING VALUE(existe) TYPE abap_bool.

    METHODS delete
      IMPORTING indice TYPE any OPTIONAL.

    CLASS-METHODS get_st_texto
      IMPORTING clave        TYPE any
                subclave     TYPE any OPTIONAL
                indice       TYPE any OPTIONAL
      RETURNING VALUE(texto) TYPE ztemp-texto.

    CLASS-METHODS get_st
      IMPORTING clave        TYPE any  OPTIONAL
                subclave     TYPE any  OPTIONAL
                indice       TYPE any  OPTIONAL
                max_dias     TYPE int4 DEFAULT 0
      RETURNING VALUE(ztemp) TYPE ztemp.

    CLASS-METHODS get_segundos_st
      IMPORTING clave           TYPE any OPTIONAL
                subclave        TYPE any OPTIONAL
                indice          TYPE any OPTIONAL
      EXPORTING ztemp           TYPE ztemp
      RETURNING VALUE(segundos) TYPE int4.


  PRIVATE SECTION.
    DATA clave     TYPE ztemp-clave.
    DATA indice    TYPE ztemp-indice.
    DATA no_grabar TYPE abap_bool.
endclass. "ZCL_AP_TEMP definition
class ZCL_AP_TEMP implementation.
  METHOD borrar.
    DATA l_subclave TYPE ztemp-subclave.

    IF subclave IS INITIAL.
      l_subclave = me->subclave.
    ELSE.
      l_subclave = subclave.
    ENDIF.

    borrar_st( clave = clave
               subclave = l_subclave
               ernam = ernam
               erdat = erdat ).
  ENDMETHOD.
  METHOD borrar_antiguos.
    DATA l_fecha TYPE d.

    l_fecha = sy-datum - dias.

    DELETE FROM ztemp
     WHERE clave  = clave
       AND erdat <= l_fecha.
  ENDMETHOD.
  METHOD borrar_st.
    " TODO: parameter ERNAM is never used (ABAP cleaner)

    DATA: r_subclave  TYPE RANGE OF ztemp-subclave,
          r_erdat     TYPE RANGE OF ztemp-erdat,
          lr_subclave LIKE LINE OF r_subclave,
          lr_erdat    LIKE LINE OF r_erdat.

    IF NOT subclave IS INITIAL.
      lr_subclave-option = 'EQ'.
      lr_subclave-sign   = 'I'.
      lr_subclave-low    = subclave.
      APPEND lr_subclave TO r_subclave.
    ENDIF.

    IF NOT erdat IS SUPPLIED.
      lr_erdat-option = 'EQ'.
      lr_erdat-sign   = 'I'.
      lr_erdat-low    = erdat.
      APPEND lr_erdat TO r_erdat.
    ENDIF.

    DELETE FROM ztemp
     WHERE clave     = clave
       AND subclave IN r_subclave
       AND erdat    IN r_erdat.
  ENDMETHOD.
  METHOD constructor.
    me->clave     = clave.
    me->subclave  = subclave.
    me->no_grabar = no_grabar.
    IF sub_auto = 'X'.
      CONCATENATE sy-uname '_' sy-datum '_' sy-uzeit INTO me->subclave.
    ENDIF.
    CLEAR indice.
  ENDMETHOD.
  METHOD delete.
    DATA(l_reintentos) = 0.
    TRY.
        IF indice IS INITIAL.
          DELETE FROM ztemp
           WHERE clave    = clave
             AND subclave = subclave.
        ELSE.
          DELETE FROM ztemp
           WHERE clave    = clave
             AND subclave = subclave
             AND indice   = indice.
        ENDIF.
      CATCH cx_root INTO DATA(o_root). " TODO: variable is assigned but never used (ABAP cleaner)
        l_reintentos = l_reintentos + 1.
        IF l_reintentos < 3.
          WAIT UP TO 1 SECONDS.
          RETRY.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD delete_st.
    DATA(l_reintentos) = 0.
    TRY.
        IF indice IS SUPPLIED.
          DELETE FROM ztemp
           WHERE clave    = clave
             AND subclave = subclave
             AND indice   = indice.
        ELSE.
          DELETE FROM ztemp
           WHERE clave    = clave
             AND subclave = subclave.
        ENDIF.
      CATCH cx_root INTO DATA(o_root). " TODO: variable is assigned but never used (ABAP cleaner)
        l_reintentos = l_reintentos + 1.
        IF l_reintentos < 3.
          WAIT UP TO 1 SECONDS.
          RETRY.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD delete_string.
    DATA(l_reintentos) = 0.
    TRY.
        DELETE FROM ztemps
         WHERE clave    = clave
           AND subclave = subclave
           AND indice   = indice.
      CATCH cx_root INTO DATA(o_root). " TODO: variable is assigned but never used (ABAP cleaner)
        l_reintentos = l_reintentos + 1.
        IF l_reintentos < 3.
          WAIT UP TO 1 SECONDS.
          RETRY.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD delete_string_st.
    DATA(l_reintentos) = 0.
    TRY.
        DELETE FROM ztemps
         WHERE clave    = clave
           AND subclave = subclave
           AND indice   = indice.
      CATCH cx_root INTO DATA(o_root). " TODO: variable is assigned but never used (ABAP cleaner)
        l_reintentos = l_reintentos + 1.
        IF l_reintentos < 3.
          WAIT UP TO 1 SECONDS.
          RETRY.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD existe.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_valor1 TYPE ztemp-valor1.

    CLEAR existe.
    IF indice IS INITIAL.
      SELECT SINGLE valor1 FROM ztemp
        INTO l_valor1
        WHERE clave    = clave
          AND subclave = subclave.
    ELSE.
      SELECT SINGLE valor1 FROM ztemp
        INTO l_valor1
        WHERE clave    = clave
          AND subclave = subclave
          AND indice   = indice.
    ENDIF.
    IF sy-subrc = 0.
      existe = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD existe_st.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_valor1 TYPE ztemp-valor1.

    CLEAR existe.
    SELECT SINGLE valor1 FROM ztemp
      INTO l_valor1
      WHERE clave    = clave
        AND subclave = subclave
        AND indice   = indice.
    IF sy-subrc = 0.
      existe = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD get_segundos_st.
    CLEAR: ztemp,
           segundos.
    SELECT SINGLE * FROM ztemp
      INTO ztemp
      WHERE clave    = clave
        AND subclave = subclave
        AND indice   = indice.
    IF sy-subrc <> 0.
      segundos = 99999.
    ELSE.
      segundos = zcl_ap_fechas=>get_duracion_intervalo_sec( fini = ztemp-erdat  hini = ztemp-erzet
                                                            ffin = sy-datum hfin = sy-uzeit ).
    ENDIF.
  ENDMETHOD.
  METHOD get_st.
    DATA l_dias TYPE i.

    CLEAR ztemp.
    SELECT SINGLE * FROM ztemp
      INTO ztemp
      WHERE clave    = clave
        AND subclave = subclave
        AND indice   = indice.
    IF sy-subrc = 0.
      IF NOT max_dias IS INITIAL.
        l_dias = sy-datum - ztemp-erdat.
        IF l_dias > max_dias.
          CLEAR ztemp.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_st_texto.
    CLEAR texto.
    SELECT SINGLE texto FROM ztemp
      INTO texto
      WHERE clave    = clave
        AND subclave = subclave
        AND indice   = indice.
  ENDMETHOD.
  METHOD get_st_val1.
    DATA: r_indice   TYPE RANGE OF ztemp-indice,
          r_erdat    TYPE RANGE OF ztemp-erdat,
          l_subclave TYPE ztemp-subclave,
          lr_indice  LIKE LINE OF r_indice,
          lr_erdat   LIKE LINE OF r_erdat.

    IF subclave IS INITIAL.
      l_subclave = me->subclave.
    ELSE.
      l_subclave = subclave.
    ENDIF.

    IF indice IS SUPPLIED.
      lr_indice-option = 'EQ'.
      lr_indice-sign   = 'I'.
      lr_indice-low    = indice.
      APPEND lr_indice TO r_indice.
    ENDIF.

    IF erdat IS SUPPLIED.
      lr_erdat-option = 'EQ'.
      lr_erdat-sign   = 'I'.
      lr_erdat-low    = erdat.
      APPEND lr_erdat TO r_erdat.
    ENDIF.

    SELECT valor1 FROM ztemp
      INTO valor1
      UP TO 1 ROWS
      WHERE clave     = clave
        AND subclave  = l_subclave
        AND indice   IN r_indice
        AND erdat    IN r_erdat
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_st_valor1.
    CLEAR valor1.
    SELECT SINGLE valor1 FROM ztemp
      INTO valor1
      WHERE clave    = clave
        AND subclave = subclave
        AND indice   = indice.
  ENDMETHOD.
  METHOD get_string.
    CLEAR string.
    SELECT string FROM ztemps
      INTO string
      UP TO 1 ROWS
      WHERE clave    = clave
        AND subclave = subclave
        AND valor    = valor
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_string_st.
    CLEAR string.
    SELECT SINGLE string FROM ztemps
      INTO string
      WHERE clave    = clave
        AND subclave = subclave
        AND indice   = indice
        AND valor    = valor.
  ENDMETHOD.
  METHOD get_string_tabla.
    DATA: l_string  TYPE string,
          l_stringx TYPE xstring.

    CLEAR tabla.
    SELECT string FROM ztemps
      INTO l_string
      UP TO 1 ROWS
      WHERE clave    = clave
        AND subclave = subclave
        AND valor    = valor
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      l_stringx = zcl_ap_string=>string2xstring( l_string ).

      CALL TRANSFORMATION id
           SOURCE XML l_stringx
           RESULT tabla = tabla.
    ENDIF.
  ENDMETHOD.
  METHOD get_tabla.
    SELECT * FROM ztemp
      INTO TABLE tabla
      WHERE clave    = clave
        AND subclave = subclave.
  ENDMETHOD.
  METHOD set.
    DATA l_ztemp TYPE ztemp.

    indice = indice + 1.

    IF me->no_grabar IS NOT INITIAL.
      RETURN.
    ENDIF.

    CLEAR l_ztemp.
    l_ztemp-clave      = clave.
    l_ztemp-subclave   = subclave.
    l_ztemp-indice     = indice.
    l_ztemp-valor1     = valor1.
    l_ztemp-valor2     = valor2.
    l_ztemp-valor3     = valor3.
    l_ztemp-texto      = texto.
    l_ztemp-ernam      = sy-uname.
    l_ztemp-erdat      = sy-datum.
    l_ztemp-erzet      = sy-uzeit.
    l_ztemp-permanente = permanente.
    MODIFY ztemp FROM l_Ztemp.
  ENDMETHOD.
  METHOD set_st.
    DATA l_ztemp TYPE ztemp.

    CLEAR l_ztemp.
    l_ztemp-clave    = clave.
    l_ztemp-subclave = subclave.
    l_ztemp-indice   = indice.
    IF subclave IS INITIAL AND subclave_auto = 'X'.
      CONCATENATE sy-datum '_' sy-uzeit '_' sy-uname INTO l_ztemp-subclave.
      IF l_ztemp-indice IS INITIAL.
        SELECT MAX( indice ) FROM ztemp
          INTO l_ztemp-indice
          WHERE clave    = clave
            AND subclave = l_ztemp-subclave.
        l_ztemp-indice = l_ztemp-indice + 1.
      ENDIF.
    ENDIF.

    IF indice_auto = 'X' AND l_ztemp-indice IS INITIAL.
      SELECT MAX( indice ) FROM ztemp
        INTO l_ztemp-indice
        WHERE clave    = clave
          AND subclave = l_ztemp-subclave.
      l_ztemp-indice = l_ztemp-indice + 1.
    ENDIF.

    l_ztemp-valor1     = valor1.
    l_ztemp-valor2     = valor2.
    l_ztemp-valor3     = valor3.
    l_ztemp-texto      = texto.
    l_ztemp-texto2     = texto2.
    l_ztemp-ernam      = sy-uname.
    l_ztemp-erdat      = sy-datum.
    l_ztemp-erzet      = sy-uzeit.
    l_ztemp-permanente = permanente.

    DATA(l_reintentos) = 0.
    TRY.
        MODIFY ztemp FROM l_ztemp.
      CATCH cx_root.
        l_reintentos = l_reintentos + 1.
        IF l_reintentos < 3.
          WAIT UP TO 1 SECONDS.
          RETRY.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
  METHOD set_string.
    DATA l_ztemps TYPE ztemps.

    indice = indice + 1.
    IF me->no_grabar IS NOT INITIAL.
      RETURN.
    ENDIF.

    CLEAR l_ztemps.
    l_ztemps-clave      = clave.
    l_ztemps-subclave   = subclave.
    l_ztemps-indice     = indice.
    l_ztemps-valor      = valor.
    l_ztemps-string     = string.
    l_ztemps-ernam      = sy-uname.
    l_ztemps-erdat      = sy-datum.
    l_ztemps-erzet      = sy-uzeit.
    l_ztemps-permanente = permanente.
    MODIFY ztemps FROM l_ztemps.
  ENDMETHOD.
  METHOD set_string_st.
    DATA l_ztemps TYPE ztemps.

    CLEAR l_ztemps.
    l_ztemps-clave    = clave.
    l_ztemps-subclave = subclave.
    l_ztemps-indice   = indice.

    IF indice_auto = 'X' AND l_ztemps-indice IS INITIAL.
      SELECT MAX( indice ) FROM ztemps
        INTO l_ztemps-indice
        WHERE clave    = clave
          AND subclave = l_ztemps-subclave.
      l_ztemps-indice = l_ztemps-indice + 1.
    ENDIF.

    l_ztemps-string     = string.
    l_ztemps-ernam      = sy-uname.
    l_ztemps-erdat      = sy-datum.
    l_ztemps-erzet      = sy-uzeit.
    l_ztemps-valor      = valor.
    l_ztemps-texto      = texto.
    l_ztemps-permanente = permanente.
    MODIFY ztemps FROM l_ztemps.
  ENDMETHOD.
  METHOD set_string_tabla.
    DATA: l_ztemps  TYPE ztemps,
          l_stringx TYPE xstring.

    CHECK me->no_grabar IS INITIAL.

    IF tabla IS INITIAL.
      RETURN.
    ENDIF.

    indice = indice + 1.

    CLEAR l_ztemps.
    l_ztemps-clave    = clave.
    l_ztemps-subclave = subclave.
    l_ztemps-indice   = indice.
    l_ztemps-valor    = valor.

    CALL TRANSFORMATION id
         SOURCE tabla = tabla
         RESULT XML l_stringx.
    l_ztemps-string = zcl_ap_string=>xstring2string( l_stringx ).

    l_ztemps-ernam  = sy-uname.
    l_ztemps-erdat  = sy-datum.
    l_ztemps-erzet  = sy-uzeit.
    MODIFY ztemps FROM l_ztemps.
  ENDMETHOD.
