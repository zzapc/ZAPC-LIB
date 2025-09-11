class ZCL_AP_SOCIEDAD definition
  public
  create public .

public section.

  class-methods GET_PARAMETRO
    importing
      !BUKRS type BUKRS
      !PARAMETRO type ANY
    returning
      value(VALOR) type PAVAL .
  class-methods GET_CIF
    importing
      !BUKRS type BUKRS
    returning
      value(VALOR) type PAVAL .
  class-methods GET_DATOS_CUENTA
    importing
      !BUKRS type BUKRS
      !HBKID type HBKID
      !HKTID type HKTID optional
    exporting
      !TIBAN type TIBAN
      !T012 type T012
      !T012K type T012K
      !MENSAJE type BAPI_MSG
      !BNKA type BNKA .
protected section.
private section.
endclass. "ZCL_AP_SOCIEDAD definition
class ZCL_AP_SOCIEDAD implementation.
METHOD get_cif.

  valor = get_parametro( bukrs = bukrs parametro = 'SAP001' ).

ENDMETHOD.
METHOD get_datos_cuenta.

  SELECT SINGLE * FROM t012
    INTO t012
   WHERE bukrs = bukrs
     AND hbkid = hbkid.
  IF sy-subrc NE 0.
    CONCATENATE 'No existe el banco propio' hbkid INTO mensaje SEPARATED BY space.
  ELSE.
    IF hktid IS INITIAL.
      SELECT COUNT( * ) FROM t012k
       WHERE bukrs = bukrs
         AND hbkid = hbkid.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM t012k
          INTO t012k
         WHERE bukrs = bukrs
           AND hbkid = hbkid.
      ELSE.
        CONCATENATE 'Debe especificar el Id. de cuenta para' hbkid INTO mensaje SEPARATED BY space.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM t012k
        INTO t012k
       WHERE bukrs = bukrs
         AND hbkid = hbkid
         AND hktid = hktid.
      IF sy-subrc NE 0.
        CONCATENATE 'No existe el banco propio' hbkid 'cuenta' hktid INTO mensaje SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT t012k IS INITIAL.
    SELECT SINGLE * FROM  tiban
      INTO tiban
     WHERE banks  = t012-banks
       AND bankl  = t012-bankl
       AND bankn  = t012k-bankn
       AND bkont  = t012k-bkont.

    SELECT SINGLE * FROM  bnka
      INTO bnka
     WHERE banks  = t012-banks
       AND bankl  = t012-bankl.
  ENDIF.

ENDMETHOD.
METHOD get_parametro.

  CLEAR valor.
  SELECT SINGLE paval FROM  t001z
    INTO valor
   WHERE bukrs  = bukrs
     AND party  = parametro.

ENDMETHOD.
