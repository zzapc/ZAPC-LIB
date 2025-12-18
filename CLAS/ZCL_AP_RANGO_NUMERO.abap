class ZCL_AP_RANGO_NUMERO definition
  public
  final
  create public .

public section.

  class-methods SIGUIENTE_NUMERO
    importing
      !RANGO type INRI-OBJECT
      !RANGO_NO type INRI-NRRANGENR default '01'
      !SUBOBJECT type ANY default ' '
      !ANYO type NRYEAR default '0000'
      !ROLLBACK type ABAP_BOOL default ''
      !SIMULAR type ABAP_BOOL default ''
      !CREAR_SI_NO_EXISTE type ABAP_BOOL default ''
      !PARAR_EN_ERROR type ABAP_BOOL default 'X'
    exporting
      value(NUMERO) type ANY
      !MESSAGE type BAPI_MSG .
  class-methods SIGUIENTE_NUMERO_N10
    importing
      !RANGO type INRI-OBJECT
      !RANGO_NO type INRI-NRRANGENR default '01'
      !SUBOBJECT type ANY default ' '
      !ANYO type NRYEAR default '0000'
      !SIMULAR type ABAP_BOOL default ''
      !CREAR_SI_NO_EXISTE type ABAP_BOOL default ''
      !PARAR_EN_ERROR type ABAP_BOOL default 'X'
    preferred parameter RANGO
    returning
      value(NUMERO) type NUMC10 .
  class-methods INFO_NUMERO
    importing
      !RANGO type INRI-OBJECT
      !RANGO_NO type INRI-NRRANGENR default '01'
      !SUBOBJECT type ANY default ' '
      !ANYO type NRYEAR default '0000'
    exporting
      value(NUMERO) type ANY .
  class-methods MANTENER_RANGO
    importing
      !RANGO type INRI-OBJECT .
protected section.
private section.

  methods DEFINIR_RANGO .
endclass. "ZCL_AP_RANGO_NUMERO definition
class ZCL_AP_RANGO_NUMERO implementation.
method DEFINIR_RANGO.

* El rango se define llamando a la transacción SNRO

endmethod.
METHOD info_numero.
  DATA: l_nriv TYPE nriv,
        l_long TYPE i.

  CALL FUNCTION 'NUMBER_GET_INFO'
    EXPORTING
      nr_range_nr        = rango_no
      object             = rango
      subobject          = subobject
      toyear             = anyo
    IMPORTING
      interval           = l_nriv
    EXCEPTIONS
      interval_not_found = 1
      object_not_found   = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    add 1 to l_nriv-nrlevel.
    DESCRIBE FIELD numero LENGTH l_long IN CHARACTER MODE.
    if l_long > 20 or l_long = 0.
      numero = l_nriv-nrlevel.
    else.
      numero = l_nriv-nrlevel+l_long.
    endif.
  ENDIF.

ENDMETHOD.
METHOD mantener_rango.
  DATA o_bi TYPE REF TO zcl_ap_batch_input.
  DATA l_mensaje TYPE bapireturn1-message.

  CREATE OBJECT o_bi.

  o_bi->inicio( ).

* Menú acceso a actual.objetos rango de números
  o_bi->dynpro( program = 'SAPMSNRO' dynpro = '0150').
  o_bi->campos( campo = 'BDC_OKCODE' valor = '=IUPD').
  o_bi->campos( campo = 'NRIV-OBJECT' valor = rango ). " Nombre del objeto rango de números

* Imagen inicial rangos de números
  o_bi->dynpro( program = 'SAPMSNUM' dynpro = '0100').
  o_bi->campos( campo = 'BDC_OKCODE' valor = '=LUPD').

* Actualizar intervalos rango de números, sin año, con número
  o_bi->dynpro( program = 'SAPLSNR0' dynpro = '0503').

  l_mensaje = o_bi->llamar_transaccion( tcode = 'SNRO' modo = 'E').

ENDMETHOD.
METHOD siguiente_numero.
  DATA: ret_code TYPE inri-returncode.

  CLEAR message.
  IF simular IS INITIAL.
* Generate a new number for the form.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = rango_no
        object                  = rango
        quantity                = '1'
        subobject               = subobject
        toyear                  = anyo
      IMPORTING
        number                  = numero
*       QUANTITY                =
        returncode              = ret_code
      EXCEPTIONS
        interval_not_found      = 01
        number_range_not_intern = 02
        object_not_found        = 03
        quantity_is_0           = 04.

    IF sy-subrc = 0.
      IF rollback = 'X'.
        ROLLBACK WORK.
      ENDIF.
      CASE ret_code.
        WHEN ' '.
        WHEN '1'.
          MESSAGE w398(00) WITH
            'El rango de números' rango 'está a punto de agotarse'.
        WHEN '2'.
          message = |El rango de números { rango }  está agotado|.
          IF parar_en_error = 'X'.
            MESSAGE message TYPE 'E'.
          ENDIF.
        WHEN OTHERS.
          message = |Error en rango de nº { rango }|.

          IF parar_en_error = 'X'.
            MESSAGE message TYPE 'E'.
          ENDIF.
      ENDCASE.
    ELSE.
      IF crear_si_no_existe IS INITIAL.
        MESSAGE e398(00) WITH 'Error en rango de nº' rango rango_no.
      ELSE.
* If range does not exist, create a new one.
        IF sy-subrc = 1.
          CALL FUNCTION 'SWU_NUMBER_RANGE_CREATE'
            EXPORTING
              object               = rango
              nr_range_nr1         = rango_no
              quantity             = 1
            IMPORTING
              number               = numero
            EXCEPTIONS
              range_already_exists = 01
              object_not_found     = 02
              system_error         = 03
              OTHERS               = 04.
          CASE ret_code.
            WHEN ' '.
            WHEN '1'.
              MESSAGE w398(00) WITH
                'Intento crear rango' rango rango_no 'pero ya existe'.
            WHEN '2'.
              message = |Rango  { rango } no existe|.
              IF parar_en_error = 'X'.
                MESSAGE message TYPE 'E'.
              ENDIF.
            WHEN OTHERS.
              message = |Error intentando crear rango { rango } { rango_no }|.
              IF parar_en_error = 'X'.
                MESSAGE message TYPE 'E'.
              ENDIF.
          ENDCASE.
        ELSE.
          message =  |Error en rango de nº { rango }|.
          IF parar_en_error = 'X'.
            MESSAGE message TYPE 'E'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSE.
    info_numero( EXPORTING rango     = rango
                           subobject = subobject
                           rango_no  = rango_no
                           anyo      = anyo
                 IMPORTING numero    = numero ).
  ENDIF.

ENDMETHOD.
METHOD siguiente_numero_n10.

  CALL METHOD siguiente_numero
    EXPORTING
      rango              = rango
      rango_no           = rango_no
      subobject          = subobject
      anyo               = anyo
      simular            = simular
      crear_si_no_existe = crear_si_no_existe
      parar_en_error     = parar_en_error
    IMPORTING
      numero             = numero.

ENDMETHOD.