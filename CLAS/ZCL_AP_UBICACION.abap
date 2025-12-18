class ZCL_AP_UBICACION definition
  public
  create public .

public section.

  class-methods VISUALIZAR
    importing
      !TPLNR type ANY .
  class-methods GET_VALOR_CLAS
    importing
      !CARACT type ANY
      !TPLNR type ANY
    returning
      value(VALOR) type ATWRT .
  class-methods GET_INTERLOCUTOR
    importing
      !TPLNR type ANY
      !PARVW type ANY
    returning
      value(PARNR) type IHPA-PARNR .
  class-methods SET_VALOR_CLAS
    importing
      !CLASSNUM type ANY
      !TPLNR type ANY
      !COMMIT type ABAP_BOOL default 'X'
      !CARACT type ANY
      !VALOR type ANY
    returning
      value(MENSAJE) type BAPI_MSG .
  class-methods ASIGNAR_CLASE
    importing
      !CLASSNUM type ANY
      !TPLNR type ANY
    returning
      value(MENSAJE) type BAPI_MSG .
  class-methods GET_PADRE
    importing
      !TPLNR type ANY
    returning
      value(PADRE) type TPLNR .
  class-methods TIENE_CLASIFICACION
    importing
      !TPLNR type ANY
      !CLASIFICACION type ANY
      !KLART type KLAH-KLART default '003'
    returning
      value(SI) type ABAP_BOOL .
  class-methods GET_PTO_MEDIDA
    importing
      !TPLNR type ANY
      !MPTYP type IMPTT-MPTYP
      !CARACT type ANY
    returning
      value(POINT) type IMRC_POINT .
  class-methods CREAR_PTO_MEDIDA
    importing
      !TPLNR type ANY
      !MPTYP type IMPTT-MPTYP
      !CARACT type ANY default ''
      !DESCRIPCION type IMPT-PTTXT default ''
      !MODO_CT type BDC_MODE default 'M'
      !INDCT type IMPT-INDCT default 'X'
    exporting
      !POINT type IMRC_POINT
      !MESSAGE type BAPI_MSG .
  class-methods BORRAR_PTO_MEDIDA
    importing
      !TPLNR type ANY
      !MODO_CT type BDC_MODE default 'N'
      !POINT type IMRC_POINT
    returning
      value(MESSAGE) type BAPI_MSG .
  class-methods VER_DOC_MEDICION
    importing
      !MDOCM type ANY .
  class-methods VER_PTO_MEDIDA
    importing
      !POINT type ANY .
protected section.
private section.
endclass. "ZCL_AP_UBICACION definition
class ZCL_AP_UBICACION implementation.
METHOD asignar_clase.
  DATA: l_objectkey TYPE bapi1003_key-object,
        i_return TYPE TABLE OF bapiret2,
        l_return TYPE bapiret2.

  l_objectkey = tplnr.
  CALL FUNCTION 'BAPI_OBJCL_CREATE'
    EXPORTING
      objectkeynew   = l_objectkey
      objecttablenew = 'IFLOT'
      classnumnew    = classnum
      classtypenew   = '003'
      keydate        = sy-datum
    TABLES
      return         = i_return.

  LOOP AT i_return INTO l_return WHERE type = 'E'.
    IF mensaje IS INITIAL.
      mensaje = l_return-message.
    ELSE.
      CONCATENATE mensaje l_return-message INTO mensaje SEPARATED BY space.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    zcl_ap_dev=>commit( ).
  ENDIF.

*  DATA: l_inob TYPE inob,
*        l_cont(2) TYPE n.
*
*  SELECT SINGLE * FROM inob
*    INTO l_inob
*   WHERE klart = '003'
*     AND obtab = 'IFLOT'
*     AND objek = tplnr.
*
*  SELECT COUNT( * ) FROM  kssk
*    INTO l_cont
*   WHERE objek  = l_inob-CUOBJ
*     AND mafid  = 'O'
*     AND klart  = '003'.
*  ADD 1 TO l_cont.
*
*  DATA o_bi TYPE REF TO zcl_ap_batch_input.
*  CREATE OBJECT o_bi.
*
*  o_bi->inicio( ).
*
** Ubicación técnica: acceso
*  o_bi->dynpro( program = 'SAPMILO0' dynpro = '1110').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '/00').
*  o_bi->campos( campo = 'IFLO-TPLNR' valor = tplnr ). " Ubicación técnica
*
** Functional Location: Data Screen With Tabstrip Control
*  o_bi->dynpro( program = 'SAPMILO0' dynpro = '2100').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=CL').
*
** Fijar categ. clase para batch-input
*  o_bi->dynpro( program = 'SAPLCLCA' dynpro = '0602').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENTE').
*  o_bi->campos( campo = 'RMCLF-KLART' valor = '003'). " Categoría de la clase
*
*  IF l_cont > 1.
*    o_bi->dynpro( program = 'SAPLCLFM' dynpro = '0500').
*    o_bi->campos( campo = 'BDC_OKCODE' valor = '=NEUZ').
*  ENDIF.
*
** Objeto p. clases
*  o_bi->dynpro( program = 'SAPLCLFM' dynpro = '0500').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '/00').
*  o_bi->campos( campo = 'RMCLF-CLASS' ind = l_cont valor = classnum ). " N° de clase
*
** Valoración caract. como ventana diál.modal -sólo batchinput-
*  o_bi->dynpro( program = 'SAPLCTMS' dynpro = '0109').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=BACK').
*
** Objeto p. clases
*  o_bi->dynpro( program = 'SAPLCLFM' dynpro = '0500').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=ENDE').
*
** Functional Location: Data Screen With Tabstrip Control
*  o_bi->dynpro( program = 'SAPMILO0' dynpro = '2100').
*  o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU').
*
*  mensaje = o_bi->llamar_transaccion( tcode = 'IL02' modo = 'E').
*  IF o_bi->msgty = 'S'.
*    CLEAR mensaje.
*  ENDIF.

ENDMETHOD.
METHOD borrar_pto_medida.
  DATA: o_bi      TYPE REF TO zcl_ap_batch_input,
        l_objnr   TYPE iflot-objnr,
        l_ind     TYPE i,
        l_ind_new TYPE i.

  CLEAR message.
  SELECT objnr FROM iflo
    INTO l_objnr
    UP TO 1 ROWS
   WHERE tplnr = tplnr
   ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc NE 0.
    __concat2 message 'No existe ubicacion'(neu) tplnr.
  ELSE.

    SELECT COUNT( * ) FROM imptt
      INTO l_ind
     WHERE mpobj = l_objnr.
    IF l_ind = 0.
      message = 'No existen puntos de medida'(npm).
    ELSEIF l_ind > 1.
      message = 'Sólo puede borrar si sólo hay un punto de medida'(sbp).
    ELSE.

      DATA es_return                TYPE bapiret2.
      DATA: it_cancel_requests TYPE STANDARD TABLE OF imrg_mdocm,
            imrg_mdocm         TYPE imrg_mdocm,
            imrg               TYPE imrg.


      SELECT mdocm FROM imrg
        INTO TABLE it_cancel_requests
       WHERE point = point
         AND cancl = ''.
      IF sy-subrc = 0.
        CALL FUNCTION 'MEASUREM_DOCUM_RFC_CANCEL'
          IMPORTING
            es_return          = es_return
          TABLES
            it_cancel_requests = it_cancel_requests.

        IF es_return-type = 'E'.
          message = es_return-message.
          EXIT.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.
      ENDIF.


      SELECT COUNT( * ) FROM imrg
        INTO l_ind_new
       WHERE point = point
         AND cancl = ''.
      IF l_ind_new > 0.
        __concat3 message 'Existen'(exi) l_ind_new 'documentos de medición'(dom).
        EXIT.
      ELSE.
* La validación estándar de la IL02 no tiene en cuenta el flag de cancelación, los borramos
        DELETE FROM imrg
         WHERE point = point
           AND cancl = 'X'.

        CREATE OBJECT o_bi.

        o_bi->inicio( ).

* Ubicación técnica: acceso
        o_bi->dynpro( program = 'SAPMILO0' dynpro = '1110').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '/00').
        o_bi->campos( campo = 'IFLO-TPLNR' valor = tplnr ). " Ubicación técnica

        o_bi->dynpro( program = 'SAPMILO0' dynpro = '2100').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=MP').

* Puntos de medida: resumen
        o_bi->dynpro( program = 'SAPLIMR0' dynpro = '4110').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=ALLM').

* Puntos de medida: resumen
        o_bi->dynpro( program = 'SAPLIMR0' dynpro = '4110').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '/EDELP').

* popup_to_confirm_step
        o_bi->dynpro( program = 'SAPLSPO1' dynpro = '0100').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=YES').

* Puntos de medida: resumen
        o_bi->dynpro( program = 'SAPLIMR0' dynpro = '4110').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '/00').

* Puntos de medida: resumen
        o_bi->dynpro( program = 'SAPLIMR0' dynpro = '4110').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '/ERW').

        o_bi->dynpro( program = 'SAPMILO0' dynpro = '2100').
        o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU').

        message = o_bi->llamar_transaccion( tcode = 'IL02' modo = modo_ct ).
        IF sy-msgty = 'S'.
          SUBTRACT 1 FROM l_ind.
          SELECT COUNT( * ) FROM imptt
            INTO l_ind_new
           WHERE mpobj = l_objnr.
          IF l_ind_new = l_ind.
            CLEAR message.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.
METHOD crear_pto_medida.
  DATA: o_bi         TYPE REF TO zcl_ap_batch_input,
        l_objnr      TYPE iflot-objnr,
        l_ind(2)     TYPE n,
        l_ind_new(2) TYPE n.

  CLEAR point.
  SELECT objnr FROM iflo
    INTO l_objnr
    UP TO 1 ROWS
   WHERE tplnr = tplnr
   ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc NE 0.
    __concat2 message 'No existe ubicacion'(neu) tplnr.
  ELSE.
    SELECT COUNT( * ) FROM imptt
      INTO l_ind
     WHERE mpobj = l_objnr.

    ADD 1 TO l_ind.
    CREATE OBJECT o_bi.

    o_bi->inicio( ).

* Ubicación técnica: acceso
    o_bi->dynpro( program = 'SAPMILO0' dynpro = '1110').
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/00').
    o_bi->campos( campo = 'IFLO-TPLNR' valor = tplnr ). " Ubicación técnica

* Functional Location: Data Screen With Tabstrip Control
    o_bi->dynpro( program = 'SAPMILO0' dynpro = '2100').
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=MP').

    IF l_ind > '01'.
      o_bi->dynpro( program = 'SAPLIMR0' dynpro = '4110').
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ADDP').
    ENDIF.

* Puntos de medida: resumen
    o_bi->dynpro( program = 'SAPLIMR0' dynpro = '4110').
    o_bi->campos( campo = 'BDC_OKCODE' valor = '/ERW').
    o_bi->campos( campo = 'IMPT-PSORT' ind = l_ind valor = ''). " Nº de posición del pto.-medida en objeto (campo clasif.)
    o_bi->campos( campo = 'IMPT-MPTYP' ind = l_ind valor = mptyp ). " Tipo de punto de medida
    o_bi->campos( campo = 'IMPT-ATNAM' ind = l_ind valor = caract ). " Nombre de característica
    o_bi->campos( campo = 'IMPT-PTTXT' ind = l_ind valor = descripcion ). " Denominación del punto de medida
    o_bi->campos( campo = 'IMPT-INDCT' ind = l_ind valor = indct ). " Indicador: punto de medida es un contador

* Functional Location: Data Screen With Tabstrip Control
    o_bi->dynpro( program = 'SAPMILO0' dynpro = '2100').
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU').

    message = o_bi->llamar_transaccion( tcode = 'IL02' modo = modo_ct ).
    IF sy-msgty = 'S'.
      SUBTRACT 1 FROM l_ind.
      SELECT COUNT( * ) FROM imptt
        INTO l_ind_new
       WHERE mpobj = l_objnr.
      IF l_ind_new > l_ind.
        SELECT MAX( point ) FROM imptt
          INTO point
         WHERE mpobj = l_objnr.
        CLEAR message.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.
METHOD get_interlocutor.

  SELECT parnr FROM iflot JOIN ihpa ON iflot~objnr = ihpa~objnr
    INTO parnr
    UP TO 1 ROWS
   WHERE tplnr = tplnr
     AND parvw = parvw
     AND kzloesch = ''
   ORDER BY tplnr parvw.
  ENDSELECT.

ENDMETHOD.
method GET_PADRE.

  select single tplma from iflot
    into padre
   where tplnr = tplnr.

endmethod.
METHOD get_pto_medida.
  DATA: l_objnr TYPE iflot-objnr,
        l_atinn TYPE atinn.

  CLEAR point.
  SELECT objnr FROM iflo
    INTO l_objnr
    UP TO 1 ROWS
   WHERE tplnr = tplnr
   ORDER BY PRIMARY KEY.
  ENDSELECT.
  CHECK sy-subrc = 0.

  l_atinn = zcl_ap_clasificacion=>get_caract_interno( caract ).

  SELECT point FROM imptt
    INTO point
    UP TO 1 ROWS
   WHERE mpobj = l_objnr
     AND mptyp = mptyp
     AND atinn = l_atinn
   ORDER BY PRIMARY KEY.
  ENDSELECT.

ENDMETHOD.
METHOD get_valor_clas.
  DATA l_atinn TYPE atinn.

  l_atinn = zcl_ap_clasificacion=>get_caract_interno( caract ).

  SELECT atwrt FROM inob JOIN ausp ON inob~cuobj = ausp~objek
    INTO valor
    UP TO 1 ROWS
   WHERE inob~klart = '003'
     AND obtab = 'IFLOT'
     AND inob~objek = tplnr
     AND atinn = l_atinn
   ORDER BY atinn.
  ENDSELECT.

ENDMETHOD.
METHOD set_valor_clas.
  DATA: o_cld TYPE REF TO zcl_ap_clasificacion,
        l_key TYPE bapi1003_key-object.

  CREATE OBJECT o_cld
    EXPORTING
      objecttable = 'IFLOT'
      classnum    = classnum
      classtype   = '003'.

  l_key = tplnr.
  o_cld->get_datos( objectkey = l_key unvaluated_chars = 'X' ).

  READ TABLE o_cld->i_return INTO o_cld->return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    mensaje = o_cld->return-message.
  ELSE.
    READ TABLE o_cld->i_return INTO o_cld->return WITH KEY id = 'CL' number = '732'.
    IF sy-subrc = 0.
      mensaje = o_cld->return-message.
    ENDIF.
  ENDIF.

  CHECK mensaje IS INITIAL.

  DELETE o_cld->i_car_char WHERE charact NE caract AND value_char IS INITIAL.
  DELETE o_cld->i_car_num WHERE charact NE caract AND value_from IS INITIAL AND value_to IS INITIAL.
  DELETE o_cld->i_car_curr WHERE charact NE caract AND value_from IS INITIAL AND value_to IS INITIAL.

  READ TABLE o_cld->i_car_char INTO o_cld->car_char WITH KEY charact = caract.
  IF sy-subrc = 0.
    o_cld->set_char( charact = caract value_char = valor value_neutral = valor ).
  ELSE.
    READ TABLE o_cld->i_car_num INTO o_cld->car_num WITH KEY charact = caract.
    IF sy-subrc = 0.
      o_cld->set_num( charact = caract value_from = valor ).
    ELSE.
      READ TABLE o_cld->i_car_curr INTO o_cld->car_curr WITH KEY charact = caract.
      IF sy-subrc = 0.
        o_cld->set_curr( charact = caract value_from = valor ).
      ENDIF.
    ENDIF.
  ENDIF.

  o_cld->set_datos( objectkey = l_key commit = commit dequeue_all = 'X' ).

  READ TABLE o_cld->i_return INTO o_cld->return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    mensaje = o_cld->return-message.
  ENDIF.

  o_cld->free( ).

ENDMETHOD.
METHOD tiene_clasificacion.
  DATA: l_inob TYPE inob,
        l_kssk TYPE kssk,
        l_klah TYPE klah.

  SELECT cuobj FROM inob
    INTO l_inob-cuobj
    UP TO 1 ROWS
   WHERE klart = klart
     AND obtab = 'IFLOT'
     AND objek = tplnr
   ORDER BY PRIMARY KEY.
  ENDSELECT.

  CHECK sy-subrc = 0.

  SELECT clint FROM klah
    INTO l_klah-clint
    UP TO 1 ROWS
   WHERE klart = klart
     AND class = clasificacion
   ORDER BY PRIMARY KEY.
  ENDSELECT.

  CHECK sy-subrc = 0.

  SELECT SINGLE objek FROM  kssk
    INTO l_kssk-objek
   WHERE objek  = l_inob-cuobj
     AND mafid  = 'O'
     AND klart  = klart
     AND clint  = l_klah-clint.
  IF sy-subrc = 0.
    si = 'X'.
  ENDIF.

ENDMETHOD.
METHOD ver_doc_medicion.

    SET PARAMETER ID 'IMD' FIELD mdocm.
    CALL TRANSACTION 'FO8U' AND SKIP FIRST SCREEN.       "#EC CI_CALLTA

  ENDMETHOD.
METHOD ver_pto_medida.

    SUBMIT riimr020
      AND RETURN
     WITH point = point.

  ENDMETHOD.
METHOD visualizar.

  SET PARAMETER ID 'IFL' FIELD tplnr.
  CALL TRANSACTION 'IL03' AND SKIP FIRST SCREEN.         "#EC CI_CALLTA

ENDMETHOD.
