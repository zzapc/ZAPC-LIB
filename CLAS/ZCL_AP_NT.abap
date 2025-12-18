
class ZCL_AP_NT definition
  public
  create public .

public section.

  types:
    tt_ltba TYPE TABLE OF ltba .

  constants C_LGNUM type LGNUM value '002' ##NO_TEXT.

  class-methods VISUALIZAR
    importing
      !LGNUM type LTBK-LGNUM default C_LGNUM
      !TBNUM type LTBK-TBNUM
      !TBPOS type LTBP-TBPOS optional
      !MODIFICAR type ABAP_BOOL default '' .
  class-methods BORRAR
    importing
      !LGNUM type LTBK-LGNUM default C_LGNUM
      !TBNUM type LTBK-TBNUM
      !TBPOS type LTBP-TBPOS optional
      !MODO type ALLGAZMD default 'E'
    returning
      value(BAPIRET2) type BAPIRET2 .
  class-methods MARCAR_ENTREGA_FINAL
    importing
      !LGNUM type LTBK-LGNUM default C_LGNUM
      !TBNUM type LTBK-TBNUM
      !TBPOS type LTBP-TBPOS optional
      !MODO type ALLGAZMD default 'E'
    returning
      value(BAPIRET2) type BAPIRET2 .
  class-methods MODIFICAR_POS_NT
    importing
      !LGNUM type LGNUM
      !TBNUM type TBNUM
      !TBPOS type TBPOS
      !ELIKZ type LTBP_ELIKZ optional
      !MENGA type LTBP_MENGA optional
      !COMMIT_WORK type ABAP_BOOL default 'X'
    returning
      value(MENSAJE) type STRING .
  class-methods CREAR
    importing
      !COMMIT type ABAP_BOOL default 'X'
    exporting
      !TBNUM type TBNUM
      !MESSAGE type BAPI_MSG
    changing
      !I_LTBA type TT_LTBA .
  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_NT definition
class ZCL_AP_NT implementation.
  METHOD borrar.
    DATA o_bi TYPE REF TO zcl_ap_batch_input.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Visualizar/modificar NT: pantalla inicial
    o_bi->dynpro( program = 'SAPML02B' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE'
                  valor = '/00' ).
    o_bi->campos( campo = 'LTBK-LGNUM'
                  valor = lgnum ). " Núm.almacén/Complejo alm.
    o_bi->campos( campo = 'LTBK-TBNUM'
                  valor = tbnum ). " Número de necesidad de transpo
    o_bi->campos( campo = 'LTBP-TBPOS'
                  valor = tbpos ). " Posición de necesidad transporte

* TR Processing: Item Overview
    o_bi->dynpro( program = 'SAPML02B' dynpro = '1103' ).
    o_bi->campos( campo = 'BDC_OKCODE'
                  valor = '=DLK' ).

* Popup_to_confirm_with_value
    o_bi->dynpro( program = 'SAPLSPO1' dynpro = '0400' ).
    o_bi->campos( campo = 'BDC_OKCODE'
                  valor = '=YES' ).

    CLEAR bapiret2.
    bapiret2-message    = o_bi->llamar_transaccion( tcode = 'LB02' modo = modo ).
    bapiret2-type       = o_bi->msgty.
    bapiret2-id         = o_bi->msgid.
    bapiret2-number     = o_bi->msgno.
    bapiret2-message_v1 = o_bi->msgv1.
    bapiret2-message_v2 = o_bi->msgv2.
    bapiret2-message_v3 = o_bi->msgv3.
    bapiret2-message_v4 = o_bi->msgv4.
  ENDMETHOD.
  METHOD crear.
    DATA: l_subrc TYPE sy-subrc,
          l_text  TYPE t100-text.

    FIELD-SYMBOLS <ltba> TYPE ltba.

    IF i_ltba IS INITIAL.
      message = 'No ha informado posiciones'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'L_TR_CREATE'
      EXPORTING
        i_single_item         = ' '
        i_save_only_all       = ' '
        i_update_task         = ' '
        i_commit_work         = commit
      TABLES
        t_ltba                = i_ltba
      EXCEPTIONS
        item_error            = 1
        no_entry_in_int_table = 2
        item_without_number   = 3
        no_update_item_error  = 4.
    l_subrc = sy-subrc.

    LOOP AT i_ltba ASSIGNING <ltba>.
      IF NOT <ltba>-tbnum IS INITIAL.
        tbnum = <ltba>-tbnum.
      ELSEIF NOT <ltba>-msgid IS INITIAL.
        SELECT SINGLE text FROM t100
          INTO l_text
         WHERE sprsl = sy-langu
           AND arbgb = <ltba>-msgid
           AND msgnr = <ltba>-msgno.
        IF sy-subrc = 0.
          __add_lista message l_text.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF tbnum IS INITIAL AND message IS INITIAL.
      CASE l_subrc.
        WHEN 1. message = 'Error en posición'.
        WHEN 2. message = 'No entradas en tabla'.
        WHEN 3. message = 'Posición sin nº'.
        WHEN 4. message = 'Error actualización'.
      ENDCASE.
    ENDIF.

*  t_ltba-matnr = 'LUCIACADENA'.
*  t_ltba-lgort = '0002'.
*  t_ltba-werks = '0002'.
*  t_ltba-menga = 1.
*  t_ltba-lgnum = '002'.
*  t_ltba-altme = 'UN'.
*  t_ltba-betyp = 'P'.
*  t_ltba-benum = 'PRUEBA'.
*  t_ltba-bwlvs = '900'.
*  t_ltba-vltyp = '911'.
*  t_ltba-vlpla = 'PINTURA'.
*  t_ltba-nltyp = '911'.
**      t_ltba-nlpla = gt_lqua-lgpla.
*  t_ltba-nkdyn = 'X'.
**      t_ltba-rsnum = gt_resb_2-rsnum.
**      t_ltba-rspos = gt_resb-rspos.
*  t_ltba-lznum = 'PRUEBA LZNUM'.
*  t_ltba-sobkz = 'E'.
*  t_ltba-sonum = '0000002097000010'.
*  APPEND t_ltba.
  ENDMETHOD.
  METHOD marcar_entrega_final.
    DATA o_bi TYPE REF TO zcl_ap_batch_input.

    o_bi = NEW #( ).

    o_bi->inicio( ).

* Visualizar/modificar NT: pantalla inicial
    o_bi->dynpro( program = 'SAPML02B' dynpro = '0100' ).
    o_bi->campos( campo = 'BDC_OKCODE'
                  valor = '/00' ).
    o_bi->campos( campo = 'LTBK-LGNUM'
                  valor = lgnum ). " Núm.almacén/Complejo alm.
    o_bi->campos( campo = 'LTBK-TBNUM'
                  valor = tbnum ). " Número de necesidad de transpo
    o_bi->campos( campo = 'LTBP-TBPOS'
                  valor = tbpos ). " Posición de necesidad transporte

    o_bi->dynpro( program = 'SAPML02B' dynpro = '0102' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=BU' ).
    o_bi->campos( campo = 'LTBP-ELIKZ' valor = 'X' ).

    CLEAR bapiret2.
    bapiret2-message    = o_bi->llamar_transaccion( tcode = 'LB02' modo = modo ).
    bapiret2-type       = o_bi->msgty.
    bapiret2-id         = o_bi->msgid.
    bapiret2-number     = o_bi->msgno.
    bapiret2-message_v1 = o_bi->msgv1.
    bapiret2-message_v2 = o_bi->msgv2.
    bapiret2-message_v3 = o_bi->msgv3.
    bapiret2-message_v4 = o_bi->msgv4.
  ENDMETHOD.
  METHOD modificar_pos_nt.
    DATA i_ltbc TYPE TABLE OF ltbc.

    FIELD-SYMBOLS <ltbc> TYPE ltbc.

    SELECT ELIKZ MENGA FROM ltbp
      INTO CORRESPONDING FIELDS OF TABLE i_ltbc
     WHERE lgnum = lgnum
       AND tbnum = tbnum
       AND tbpos = tbpos.
    IF sy-subrc <> 0.
      CONCATENATE 'No existe la OT' lgnum tbnum tbpos INTO mensaje SEPARATED BY space.
    ELSE.
      ASSIGN i_ltbc[ 1 ] TO <ltbc>.
      IF NOT (  elikz = <ltbc>-elikz
         AND menga = <ltbc>-menga ).
* Si lo que queremos es quitar el indicador de entrega final, la función no funciona,
* bien, por lo que primero actualizamos este indicador a pelo.
        IF <ltbc>-elikz = 'X' AND elikz = ''.
          UPDATE ltbp
            SET elikz = ''
           WHERE lgnum = lgnum
             AND tbnum = tbnum
             AND tbpos = tbpos.
        ENDIF.

        <ltbc>-elikz = elikz.
        <ltbc>-menga = menga.
        CALL FUNCTION 'L_TR_CHANGE'
          EXPORTING
*     I_SAVE_ONLY_ALL                = 'X'
*     I_UPDATE_TASK                  = I_UPDATE_TASK
            i_commit_work                  = commit_work
          TABLES
            t_ltbc                         = i_ltbc
         EXCEPTIONS
           item_error                     = 1
           no_update_item_error           = 2
           no_update_no_entry             = 3
           no_update_without_commit       = 4
           tr_locked                      = 5.

        IF sy-subrc <> 0.
          ASSIGN i_ltbc[ 1 ] TO <ltbc>.
          IF <ltbc>-msgid IS INITIAL.
            CASE sy-subrc.
              WHEN 1. mensaje = 'Error de posición'.
              WHEN 2. mensaje = 'Error al actualizar posición'.
              WHEN 3. mensaje = 'Error: no existen datos'.
              WHEN 4. mensaje = 'Error: actualización sin commit'.
              WHEN 5. mensaje = 'Error de bloqueo'.
            ENDCASE.
          ELSE.
            MESSAGE ID <ltbc>-msgid TYPE 'S' NUMBER <ltbc>-msgno WITH
                    <ltbc>-msgv1 <ltbc>-msgv2 <ltbc>-msgv3 <ltbc>-msgv4 INTO mensaje.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD visualizar.
    SET PARAMETER ID 'LGN' FIELD lgnum.
    SET PARAMETER ID 'TBN' FIELD tbnum.
    SET PARAMETER ID 'TBP' FIELD tbpos.

    IF modificar IS INITIAL.
      CALL TRANSACTION 'LB03' AND SKIP FIRST SCREEN.
    ELSE.
      CALL TRANSACTION 'LB02' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDMETHOD.
