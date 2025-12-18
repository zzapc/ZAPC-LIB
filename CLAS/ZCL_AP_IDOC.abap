class ZCL_AP_IDOC definition
  public
  create public .

public section.

  types:
    BEGIN OF t_cambios,
             segmento TYPE string,
             campo    TYPE string,
             valor    TYPE string,
           END OF t_cambios .
  types:
    tt_cambios TYPE STANDARD TABLE OF t_cambios WITH KEY segmento campo .

  class-methods BORRAR
    importing
      !DOCNUM type EDIDC-DOCNUM
      !MOSTRAR_ERRORES type ABAP_BOOL default 'X'
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods VISUALIZAR
    importing
      !DOCNUM type EDIDC-DOCNUM
      !MOSTRAR_ERRORES type ABAP_BOOL default 'X'
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods GET_SEGMENTO
    importing
      !DOCNUM type ANY
      !SEGMENTO type ANY
      !INI_DATOS type ANY default ''
    returning
      value(DATOS) type EDID4-SDATA .
  class-methods GET_VALOR
    importing
      !DOCNUM type ANY
      !SEGMENTO type ANY
      !CAMPO type ANY
      !INI_DATOS type ANY default ''
    returning
      value(VALOR) type STRING .
  class-methods GET_SEGMENTO_MEM
    importing
      !SEGMENTO type ANY
      !INI_DATOS type ANY default ''
      !IDOC_DATA type IDOC_DATA
      !PSGNUM type EDID4-PSGNUM optional
    returning
      value(DATOS) type EDID4-SDATA .
  class-methods GET_PRIMER_VALOR_INFORMADO
    importing
      !DOCNUM type ANY
      !SEGMENTO type ANY
      !CAMPO type ANY
      !INI_DATOS type ANY default ''
    exporting
      value(VALOR) type ANY .
  class-methods COPIAR
    importing
      !DOCNUM type EDIDC-DOCNUM
      !I_CAMBIOS type TT_CAMBIOS optional
    exporting
      !NUEVO_IDOC type EDIDC-DOCNUM
      !MESSAGE type BAPI_MSG .
  class-methods EJECUTAR
    importing
      !DOCNUM type EDIDC-DOCNUM
    exporting
      !MESSAGE type BAPI_MSG .
protected section.
private section.
endclass. "ZCL_AP_IDOC definition
class ZCL_AP_IDOC implementation.
METHOD borrar.
    DATA l_docnum TYPE edidc-docnum.

    l_docnum = docnum.
    CALL FUNCTION 'EDI_DOCUMENT_DELETE'
      EXPORTING
        document_number        = l_docnum
      EXCEPTIONS
        idoc_does_not_exist    = 1
        document_foreign_lock  = 2
        idoc_cannot_be_deleted = 3
        not_all_tables_deleted = 4
        OTHERS                 = 5.

    IF sy-subrc <> 0.
      IF mostrar_errores = 'X'.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD copiar.
    DATA: edidc   TYPE edidc,
          i_edidc TYPE TABLE OF edidc,
          i_edidd TYPE TABLE OF edidd.

    CLEAR: nuevo_idoc, message.
    SELECT SINGLE * FROM edidc
    INTO edidc
    WHERE docnum = docnum.
    IF sy-subrc NE 0.
      message = 'No existe el IDOC origen'.
      RETURN.
    ENDIF.

    SELECT * FROM edid4
    INTO CORRESPONDING FIELDS OF TABLE i_edidd
    WHERE docnum = docnum.
    IF sy-subrc NE 0.
      message = 'IDOC origen sin datos'.
    ENDIF.

    DATA lo_struct_ref TYPE REF TO cl_abap_structdescr.
    DATA lt_components TYPE cl_abap_structdescr=>component_table.
    DATA lo_wa TYPE REF TO cl_abap_structdescr.
    DATA e_workarea TYPE REF TO data.

    FIELD-SYMBOLS: <datos> TYPE any,
                   <dato>  TYPE any.


    LOOP AT i_cambios ASSIGNING FIELD-SYMBOL(<cambios>).
      lo_struct_ref ?= cl_abap_typedescr=>describe_by_name( <cambios>-segmento ).
      lt_components = lo_struct_ref->get_components( ).
      TRY.
          lo_wa = cl_abap_structdescr=>create( lt_components ).
          CREATE DATA e_workarea TYPE HANDLE lo_wa.

          ASSIGN e_workarea->* TO <datos>.

          LOOP AT i_edidd ASSIGNING FIELD-SYMBOL(<eddic>) WHERE segnam = <cambios>-segmento.
            <datos> = <eddic>-sdata.
            ASSIGN COMPONENT <cambios>-campo OF STRUCTURE <datos> TO <dato>.
            IF sy-subrc = 0.
              <dato> = <cambios>-valor.
              <eddic>-sdata = <datos>.
            ENDIF.
          ENDLOOP.

        CATCH cx_sy_struct_creation INTO DATA(lo_error).
          message = lo_error->get_text( ).
      ENDTRY.
    ENDLOOP.

    clear: edidc-status, edidc-UPDDAT, edidc-UPDTIM.
    edidc-CREDAT = sy-datum.
    edidc-CRETIM = sy-uzeit.

    CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
      EXPORTING
        master_idoc_control            = edidc
      TABLES
        communication_idoc_control     = i_edidc
        master_idoc_data               = i_edidd
      EXCEPTIONS
        error_in_idoc_control          = 1
        error_writing_idoc_status      = 2
        error_in_idoc_data             = 3
        sending_logical_system_unknown = 4
        OTHERS                         = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ELSE.
      READ TABLE i_edidc ASSIGNING FIELD-SYMBOL(<edidc>) INDEX 1.
      IF sy-subrc = 0.
        nuevo_idoc = <edidc>-docnum.
      ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD ejecutar.

    CLEAR message.

    CALL FUNCTION 'IDOC_MANUAL_INPUT'
      EXPORTING
        idoc_number                  = docnum
        input_exception              = '0'
        no_dialog                    = 'X'
      EXCEPTIONS
        idoc_not_in_database         = 1
        no_input_function_found      = 2
        no_function_parameters_found = 3
        no_status_record_found       = 4
        no_authorization             = 5
        OTHERS                       = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      IF message = 'S::000'. CLEAR message. ENDIF.
    ENDIF.

  ENDMETHOD.
METHOD get_primer_valor_informado.
    DATA: i_datos TYPE TABLE OF edid4,
          l_dato  TYPE edid4.

    CLEAR valor.
    SELECT * FROM edid4
      INTO TABLE i_datos
     WHERE docnum = docnum
       AND segnam = segmento.

    LOOP AT i_datos INTO l_dato.
      valor = zcl_ap_fs=>get_valor_from_struc( estructura = segmento campo = campo datos = l_dato-sdata ).
      IF NOT valor IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
METHOD get_segmento.
    DATA: l_long TYPE i.
    __data_set_vart edid4.

    CLEAR datos.

    IF ini_datos IS INITIAL.
      SELECT SINGLE * FROM edid4
        INTO l_edid4
       WHERE docnum = docnum
         AND segnam = segmento.
      IF sy-subrc = 0.
        datos = l_edid4-sdata.
      ENDIF.
    ELSE.
      l_long = strlen( ini_datos ).
      SELECT * FROM edid4
        INTO TABLE i_edid4
       WHERE docnum = docnum
         AND segnam = segmento.

      LOOP AT i_edid4 ASSIGNING <edid4>.
        IF <edid4>-sdata(l_long) = ini_datos.
          datos = <edid4>-sdata.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
METHOD get_segmento_mem.
    DATA: l_sdata  TYPE edid4-sdata,
          r_rango  TYPE RANGE OF string,
          lr_rango LIKE LINE OF r_rango.

    FIELD-SYMBOLS <data> TYPE edidd.

    CLEAR datos.

    IF NOT ini_datos IS INITIAL.
      CLEAR lr_rango.
      lr_rango-option = 'CP'.
      lr_rango-sign = 'I'.
      CONCATENATE ini_datos '*' INTO lr_rango-low.
      APPEND lr_rango TO r_rango.
    ENDIF.

    LOOP AT idoc_data ASSIGNING <data> WHERE segnam = segmento
                                         AND psgnum   = psgnum
                                         AND sdata    IN r_rango.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT idoc_data ASSIGNING <data> WHERE segnam = segmento
                                           AND segnum   = psgnum
                                           AND sdata    IN r_rango.
        EXIT.
      ENDLOOP.
    ENDIF.
    IF <data> IS ASSIGNED.
      datos = <data>-sdata.
    ENDIF.

  ENDMETHOD.
METHOD get_valor.
    DATA l_datos TYPE edid4-sdata.

    l_datos = get_segmento( docnum = docnum segmento = segmento ini_datos = ini_datos ).

    IF NOT l_datos IS INITIAL.
      valor = zcl_ap_fs=>get_valor_from_struc( estructura = segmento campo = campo datos = l_datos ).
    ENDIF.

  ENDMETHOD.
METHOD visualizar.
    DATA l_docnum TYPE edidc-docnum.

    l_docnum = docnum.

    CALL FUNCTION 'EDI_DOCUMENT_DATA_DISPLAY'
      EXPORTING
        docnum               = l_docnum
      EXCEPTIONS
        no_data_record_found = 1
        OTHERS               = 2.

    IF sy-subrc <> 0.
      IF mostrar_errores = 'X'.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      ENDIF.
    ENDIF.

  ENDMETHOD.
