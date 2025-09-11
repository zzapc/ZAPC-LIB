CLASS zcl_ap_hu DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA vekp     TYPE vekp.
    DATA vepo     TYPE vepo.
    DATA i_vepo   TYPE tab_vepo.
    DATA return   TYPE bapiret2.
    DATA i_return TYPE bapiret2_t.
    DATA huheader TYPE bapihuheader.
    DATA emkpf    TYPE emkpf.

    CONSTANTS c_traspaso TYPE huwbevent VALUE '0012' ##NO_TEXT.

    DATA lights  TYPE lights.
    DATA mensaje TYPE bapi_msg.

    METHODS constructor
      IMPORTING exidv TYPE exidv OPTIONAL
                venum TYPE venum OPTIONAL
      PREFERRED PARAMETER exidv.

    METHODS get_vekp
      IMPORTING venum          TYPE venum     OPTIONAL
                exidv          TYPE exidv     OPTIONAL
                solo_get_venum TYPE abap_bool DEFAULT ''.

    METHODS get_vepo
      IMPORTING exidv TYPE exidv OPTIONAL
                venum TYPE venum OPTIONAL
      PREFERRED PARAMETER exidv.

    METHODS crear
      IMPORTING matnr              TYPE matnr
                werks              TYPE werks_d   OPTIONAL
                lgort              TYPE lgort_d   OPTIONAL
                exidv              TYPE exidv     OPTIONAL
                validar_existencia TYPE abap_bool DEFAULT ''
                exidv2             TYPE exidv2    OPTIONAL
                vegr1              TYPE vegr1     DEFAULT ''
      RETURNING VALUE(hukey)       TYPE bapihukey-hu_exid.

    METHODS pack
      IMPORTING hukey          TYPE exidv
                matnr          TYPE matnr
                charg          TYPE charg_d
                werks          TYPE werks_d
                lgort          TYPE lgort_d
                item_type      TYPE velin DEFAULT '1'
                pack_qty       TYPE any
                meins          TYPE meins
                bestq          TYPE bestq DEFAULT ''
      RETURNING VALUE(mensaje) TYPE string.

    CLASS-METHODS contiene_status
      IMPORTING venum              TYPE venum
                !status            TYPE any
                spras              TYPE sy-langu DEFAULT sy-langu
      RETURNING VALUE(si_contiene) TYPE abap_bool.

    CLASS-METHODS get_siguiente_n_emb_prop
      IMPORTING matnr      TYPE matnr
      RETURNING VALUE(num) TYPE nriv-nrlevel.

    CLASS-METHODS visualizar
      IMPORTING venum TYPE venum OPTIONAL
                exidv TYPE any   OPTIONAL
      PREFERRED PARAMETER venum.

    CLASS-METHODS esta_encajado
      IMPORTING matnr        TYPE matnr
                charg        TYPE charg_d
                werks        TYPE werks_d
                lgort        TYPE lgort_d
      RETURNING VALUE(exidv) TYPE vekp-exidv.

    METHODS borrar
      IMPORTING exidv          TYPE exidv     OPTIONAL
                venum          TYPE venum     OPTIONAL
                !commit        TYPE abap_bool DEFAULT ''
      PREFERRED PARAMETER exidv
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS get_materials
      IMPORTING exidv               TYPE exidv
      RETURNING VALUE(i_materiales) TYPE hum_cum_matererial_t.

    METHODS unpack
      IMPORTING hukey          TYPE exidv OPTIONAL
                venum          TYPE venum OPTIONAL
      RETURNING VALUE(message) TYPE bapi_msg.

    METHODS mover
      IMPORTING huwbevent        TYPE huwbevent          DEFAULT '0010'
                matnr            TYPE matnr              OPTIONAL
                werks            TYPE werks_d            OPTIONAL
                lgort            TYPE lgort_d            OPTIONAL
                bwart            TYPE bwart              OPTIONAL
                grund            TYPE mb_grbew           OPTIONAL
                kostl            TYPE kostl              OPTIONAL
                espera_a_grabado TYPE abap_bool          DEFAULT 'X'
                dequeue_all      TYPE abap_bool          DEFAULT 'X'
                i_cajas_ext      TYPE hum_exidv_t        OPTIONAL
                i_datos_mov      TYPE hum_data_move_to_t OPTIONAL
                i_cajas_int      TYPE hum_venum_t        OPTIONAL
                tcode            TYPE sy-tcode           DEFAULT 'VLMOVE'
                xblnr            TYPE xblnr              OPTIONAL
                budat            TYPE budat              DEFAULT sy-datum
                bktxt            TYPE bktxt              DEFAULT ''
                charg            TYPE charg_d            OPTIONAL.

    CLASS-METHODS get_status
      IMPORTING venum         TYPE venum
      RETURNING VALUE(status) TYPE bsvx-sttxt.

    CLASS-METHODS get_exidv
      IMPORTING venum        TYPE venum
      RETURNING VALUE(exidv) TYPE exidv.

    CLASS-METHODS cambiar_estado_hu
      IMPORTING venum          TYPE venum
                estado_origen  TYPE any
                estado_destino TYPE any
                tcode          TYPE sy-tcode DEFAULT 'VLMOVE'
                xblnr          TYPE xblnr    OPTIONAL
                budat          TYPE budat    DEFAULT sy-datum
                bktxt          TYPE bktxt    DEFAULT ''
                grund          TYPE mb_grbew OPTIONAL
      EXPORTING mensaje        TYPE any
                emkpf          TYPE emkpf
                tipo_mov       TYPE string.

    CLASS-METHODS get_venum
      IMPORTING VALUE(exidv) TYPE any
      RETURNING VALUE(venum) TYPE venum.

    CLASS-METHODS get_vekp_st
      IMPORTING venum          TYPE venum     OPTIONAL
                exidv          TYPE exidv     OPTIONAL
                solo_get_venum TYPE abap_bool DEFAULT ''
      RETURNING VALUE(vekp)    TYPE vekp.

    CLASS-METHODS get_mat_embalaje
      IMPORTING matnr        TYPE any
                werks        TYPE any
      RETURNING VALUE(vhilm) TYPE vhilm.

    CLASS-METHODS generar_num_exidv
      IMPORTING exidv                 TYPE any
      RETURNING VALUE(exidv_completa) TYPE exidv.

    CLASS-METHODS lanzar_humo
      IMPORTING exidv     TYPE any      OPTIONAL
                lista_hus TYPE any      OPTIONAL
                modo      TYPE bdc_mode DEFAULT 'E'.

    CLASS-METHODS cambiar_status
      IMPORTING venum TYPE venum   OPTIONAL
                exidv TYPE exidv   OPTIONAL
                stat  TYPE j_status
                inact TYPE j_inact DEFAULT ''.

    CLASS-METHODS get_nueva_sscc
      IMPORTING !object      TYPE inri-object    DEFAULT 'LVS_LENUM'
                !range       TYPE inri-nrrangenr DEFAULT '01'
      RETURNING VALUE(exidv) TYPE exidv.

    CLASS-METHODS insertar_log_hu
      IMPORTING exidv          TYPE exidv       OPTIONAL
                venum          TYPE venum       OPTIONAL
                !handle        TYPE vevw-handle OPTIONAL
                !object        TYPE vevw-object
                objkey         TYPE any
      RETURNING VALUE(message) TYPE bapi_msg.

    CLASS-METHODS esta_bloqueada
      IMPORTING exidv            TYPE exidv OPTIONAL
                venum            TYPE venum OPTIONAL
      RETURNING VALUE(bloqueada) TYPE abap_bool.

    CLASS-METHODS espera_si_bloqueada
      IMPORTING exidv           TYPE exidv OPTIONAL
                venum           TYPE venum OPTIONAL
                segundos_espera TYPE int2  DEFAULT 10
      RETURNING VALUE(message)  TYPE bapi_msg.

    CLASS-METHODS calc_digito_control
      IMPORTING hu            TYPE char18
      RETURNING VALUE(digito) TYPE numc1.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_HU definition
class ZCL_AP_HU implementation.
  METHOD borrar.
    DATA l_exidv TYPE exidv.

    IF NOT exidv IS INITIAL.
      l_exidv = exidv.
    ELSE.
      SELECT SINGLE exidv FROM vekp
        INTO l_exidv
       WHERE venum = venum.
    ENDIF.

    CLEAR: i_return, message.

    CALL FUNCTION 'BAPI_HU_DELETE'
      EXPORTING
        hukey  = l_exidv
*     WITHLOWERHUS       = ' '
*     IFPACKED           = ' '
      TABLES
        return = i_return.

    CLEAR return.
    READ TABLE i_return INTO return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      LOOP AT i_return INTO return WHERE type = 'E'.
        __add_lista message return-message.
      ENDLOOP.
    ELSE.
      IF commit = 'X'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD calc_digito_control.
    DATA: work_string   TYPE c LENGTH 50,
          length        TYPE i,
          lv_first      TYPE int4        VALUE 1,
          lv_temp       TYPE n LENGTH 1,
          lv_sum_first  TYPE int4,
          lv_second     TYPE int4        VALUE 2,
          lv_sum_second TYPE int4,
          lv_final_res  TYPE int4.

    CLEAR digito.
    work_string = hu.
    length = strlen( work_string ).
    SHIFT work_string LEFT DELETING LEADING space.

    DO length TIMES.
      WRITE work_string+lv_first(1) TO lv_temp.
      lv_sum_first = lv_sum_first + lv_temp.
      WRITE work_string+lv_second(1) TO lv_temp.
      lv_sum_second = lv_sum_second + lv_temp.
      lv_first = lv_first + 2.
      lv_second = lv_second + 2.
    ENDDO.

    lv_final_res = ( lv_sum_first * 3 ) + lv_sum_second.

    digito = 10 - ( lv_final_res MOD 10 ).
  ENDMETHOD.
  METHOD cambiar_estado_hu.
    DATA: o_hu   TYPE REF TO zcl_ap_hu,
          l_mov  TYPE huwbevent,
          l_aux1 TYPE c LENGTH 1,
          l_aux2 TYPE c LENGTH 1.

    CLEAR: emkpf, mensaje, tipo_mov.

    o_hu = NEW #( ).
    o_hu->get_vekp( venum = venum ).
    IF o_hu->vekp IS INITIAL.
      mensaje = 'HU no existe'(hne).
    ENDIF.
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    o_hu->get_vepo( venum = venum ).

    IF o_hu->i_vepo IS INITIAL.
      mensaje = 'HU sin posiciones'(hsp).
    ENDIF.
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

    l_mov = '9999'.
    CASE estado_origen.
      WHEN ''.
        CASE estado_destino.
          WHEN 'S'.
            l_mov = '0023'.
            tipo_mov = 'de libre a bloqueado'(l2b).
          WHEN 'Q'.
            l_mov = '0004'.
            tipo_mov = 'de libre a control de calidad'(l2q).
          WHEN 'D'. " Desguace!!!!
            l_mov = '0014'.
            tipo_mov = 'de libre a desguace'(l2d).
        ENDCASE.
      WHEN 'S'.
        CASE estado_destino.
          WHEN ''.
*          l_mov = '0022'.
            l_mov = '0003'.
            tipo_mov = 'de bloqueado a libre'(b2l).
          WHEN 'Q'.
            l_mov = '0021'.
            tipo_mov = 'de bloqueado a calidad'(b2q).
          WHEN 'D'. " Desguace!!!!
            l_mov = '0014'.
            tipo_mov = 'de bloqueado a desguace'(b2d).
        ENDCASE.
      WHEN 'Q'.
        CASE estado_destino.
          WHEN ''.
            l_mov = '0003'.
            tipo_mov = 'de calidad a libre'(q2l).
          WHEN 'D'. " Desguace!!!!
            l_mov = '0014'.
            tipo_mov = 'de calidad a desguace'(q2d).
          WHEN 'S'.
            l_mov = '0005'.
            tipo_mov = 'de calidad a bloqueado'(q2b).
        ENDCASE.
    ENDCASE.

    IF l_mov = '9999'.
      l_aux1 = estado_origen.
      l_aux2 = estado_destino.
      CONCATENATE 'No es posible determinar tipo mov. de'(npt) l_aux1 'a' l_aux2 INTO mensaje SEPARATED BY space.
    ENDIF.
    IF mensaje IS NOT INITIAL.
      RETURN.
    ENDIF.

* El paso de bloqueado a calidad, en un paso, da problemas, por eso lo hacemos en dos pasos
    IF l_mov = '0021'.
      o_hu->mover( huwbevent = '0003' tcode = tcode xblnr = xblnr bktxt = bktxt budat = budat grund = grund ).    " De bloqueado a libre
      IF NOT o_hu->emkpf-mblnr IS INITIAL.
        o_hu->mover( huwbevent = '0004' tcode = tcode xblnr = xblnr bktxt = bktxt budat = budat grund = grund ).    " De libre a calidad
      ENDIF.
    ELSE. " Caso normal
      IF l_mov = '0014' AND estado_origen = 'S'. " Para desguazar, hay que liberar primero
        o_hu->mover( huwbevent = '0003' tcode = tcode xblnr = xblnr bktxt = bktxt budat = budat grund = grund ).    " De bloqueado a libre
      ENDIF.
      o_hu->mover( huwbevent = l_mov tcode = tcode xblnr = xblnr bktxt = bktxt budat = budat grund = grund ).
    ENDIF.

    mensaje = o_hu->mensaje.
    emkpf = o_hu->emkpf.
  ENDMETHOD.
  METHOD cambiar_status.
    DATA: l_venum TYPE venum,
          l_objnr TYPE j_objnr,
          husstat TYPE husstat.

    IF venum IS INITIAL.
      SELECT venum FROM vekp
        INTO l_venum
        UP TO 1 ROWS
       WHERE exidv = exidv
         ORDER BY venum DESCENDING.
      ENDSELECT.
    ELSE.
      l_venum = venum.
    ENDIF.

    IF l_venum IS INITIAL.
      RETURN.
    ENDIF.
    CONCATENATE 'HU' l_venum INTO l_objnr.

    SELECT SINGLE * FROM  husstat
      INTO husstat
     WHERE objnr = l_objnr
       AND stat  = stat.
    IF sy-subrc = 0.
      IF husstat-inact <> inact.
        UPDATE husstat
          SET inact = inact
         WHERE objnr = l_objnr
           AND stat  = stat.
      ENDIF.
    ELSE.
      CLEAR husstat.
      husstat-objnr = l_objnr.
      husstat-stat  = stat.
      husstat-inact = inact.
      MODIFY husstat FROM husstat.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    IF NOT exidv IS INITIAL OR NOT venum IS INITIAL.
      get_vekp( venum = venum
                exidv = exidv ).
      get_vepo( ).
    ENDIF.
  ENDMETHOD.
  METHOD contiene_status.
    DATA: r_status  TYPE RANGE OF j_status,
          i_status  TYPE TABLE OF j_txt04,
          l_txt04   TYPE j_txt04,
          l_istat   TYPE j_status,
          lr_status LIKE LINE OF r_status,
          l_objnr   TYPE jest-objnr.

    SPLIT status AT ',' INTO TABLE i_status.
    IF i_status IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT i_status INTO l_txt04.
      SELECT istat FROM tj02t                            "#EC CI_GENBUFF.
        INTO l_istat
        UP TO 1 ROWS
       WHERE spras = spras
         AND txt04 = l_txt04
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        CLEAR lr_status.
        lr_status-option = 'EQ'.
        lr_status-sign   = 'I'.
        lr_status-low    = l_istat.
        APPEND lr_status TO r_status.
      ENDIF.
    ENDLOOP.

    IF r_status IS INITIAL.
      EXIT.
    ENDIF.

    CONCATENATE 'HU' venum INTO l_objnr.
    SELECT SINGLE stat FROM  husstat
      INTO l_istat
     WHERE objnr  = l_objnr
       AND stat  IN r_status
       AND inact  = ''.
    IF sy-subrc = 0.
      si_contiene = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD crear.
    DATA l_exidv TYPE exidv.
    DATA: l_header TYPE bapihuhdrproposal,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_venum  TYPE venum.

    IF validar_existencia = 'X'.
      zcl_ap_string=>poner_ceros( EXPORTING cadena = exidv
                                  CHANGING  salida = l_exidv ).
      SELECT SINGLE exidv FROM vekp
        INTO l_exidv
       WHERE exidv = l_exidv.
      IF sy-subrc = 0.
        CLEAR return.
        return-type       = 'E'.
        return-id         = 'L3'.
        return-number     = '415'.
        return-message_v1 = exidv.
        return-message    = zcl_ap_log=>get_text_message( return ).
        APPEND return TO i_return.
        EXIT.
      ENDIF.
    ENDIF.

    CLEAR l_header.
    l_header-pack_mat    = matnr.
    l_header-hu_exid     = exidv.
    l_header-plant       = werks.
    l_header-stge_loc    = lgort.
    l_header-ext_id_hu_2 = exidv2.
    l_header-hu_grp1     = vegr1.

    CLEAR: huheader, hukey, return, i_return.
    CALL FUNCTION 'BAPI_HU_CREATE'
      EXPORTING
        headerproposal       = l_header
     IMPORTING
       huheader             = huheader
       hukey                = hukey
      TABLES
*       itemsproposal        = items
*       ITEMSSERIALNO        =
        return               = i_return.

    IF NOT hukey IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      DO 10 TIMES.
        SELECT SINGLE venum FROM vekp
          INTO l_venum
         WHERE venum = huheader-hu_id.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.
  METHOD espera_si_bloqueada.
    DATA bloqueada TYPE c LENGTH 1.

    DO segundos_espera TIMES.
      bloqueada = esta_bloqueada( venum = venum exidv = exidv ).
      IF bloqueada = 'X'.
        message = |HU bloqueada por { sy-msgv1 }|.
        WAIT UP TO 1 SECONDS.
      ELSE.
        CLEAR message.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
  METHOD esta_bloqueada.
    DATA(vekp) = get_vekp_st( venum = venum exidv = exidv solo_get_venum = 'X' ).

    bloqueada = zcl_ap_utils=>comprobar_bloqueo( tabla = 'VEKP'
                                                 clave = sy-mandt
                                                 clave2 = vekp-venum ).
  ENDMETHOD.
  METHOD esta_encajado.
    DATA l_venum TYPE venum.

    CLEAR exidv.

    SELECT venum FROM vepo "#EC CI_NOFIELD.
      INTO l_venum
      UP TO 1 ROWS
     WHERE matnr = matnr
       AND charg = charg
       AND werks = werks
       AND lgort = lgort
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      SELECT SINGLE exidv FROM vekp
        INTO exidv
       WHERE venum = l_venum.
    ENDIF.
  ENDMETHOD.
  METHOD generar_num_exidv.
    DATA l_exidv TYPE exidv.

    zcl_ap_string=>poner_ceros( EXPORTING cadena = exidv
                                CHANGING salida = l_exidv ).

    CALL FUNCTION 'LE_CHECK_DIGIT_CALCULATION'
      EXPORTING
        if_number_wo_check_digit       = l_exidv+1(19)
*   IF_CALC_METHOD                 = 'A'
*   IF_USER_CALC_METHOD            =
*   IF_ONLY_CHECKING               = ' '
      IMPORTING
        ef_number_w_check_digit        = exidv_completa
*   EF_CHECK_DIGIT_OK              =
*   EF_CHECK_DIGIT                 =
      EXCEPTIONS
        invalid_parameter              = 1
        OTHERS                         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_exidv ).
    ENDIF.
  ENDMETHOD.
  METHOD get_exidv.
    CLEAR exidv.
    SELECT SINGLE exidv FROM vekp
      INTO exidv
     WHERE venum = venum.
  ENDMETHOD.
  METHOD get_mat_embalaje.
    DATA: l_komgp            TYPE komgp,
          Datos TYPE REF TO data.
FIELD-SYMBOLS: <tabla> TYPE STANDARD TABLE.

    l_komgp-werks = werks.
    l_komgp-matnr = matnr.

  IF zcl_c=>hana IS INITIAL.
    data(l_estructura) = 'ISUATO_PINSTRUCTION'.
  ELSE.
    L_ESTRUCTURA = 'VHU_PINSTRUCTION'.
  ENDIF.

      zcl_ap_fs=>create_it_from_struc( EXPORTING i_struc = L_ESTRUCTURA
                                       IMPORTING e_table = datos ).
                                       ASSIGN datos->* TO <tabla>.

    CALL FUNCTION 'VHUPIBAPI_BUILD_PACK_INST_INFO'
      EXPORTING
        i_komgp       = l_komgp
      TABLES
        tpinstruction = <tabla>
      EXCEPTIONS
        locked        = 1
        not_found     = 2
        fatal_error   = 3
        OTHERS        = 4.
    IF sy-subrc = 0.
      READ TABLE <tabla> ASSIGNING FIELD-SYMBOL(<instruction>) INDEX 1.
      IF sy-subrc = 0.
      ASSIGN COMPONENT 'LOADCARR' OF STRUCTURE <INSTRUCTION> TO FIELD-SYMBOL(<LOADCARR>).
        vhilm = <LOADCARR>.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD get_materials.
    CALL FUNCTION 'HU_GET_MATERIALS'
     EXPORTING
        if_identification         = exidv
*   IF_OBJECT                 =
*   IT_INTERNAL_NUMBERS       =
     IMPORTING
*   ES_HEADER                 =
*   ET_HEADERS                =
        et_materials              = i_materiales
     EXCEPTIONS
       error                     = 1
       OTHERS                    = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Error recuperando materiales' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
  METHOD get_nueva_sscc.
    CLEAR exidv.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = range
        object                  = object
        quantity                = 1
      IMPORTING
        number                  = exidv
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   Calculamos el dígito de control
    __quitar_ceros exidv.
    CALL FUNCTION 'LE_CHECK_DIGIT_CALCULATION'
      EXPORTING
        if_number_wo_check_digit = exidv
      IMPORTING
        ef_number_w_check_digit  = exidv
      EXCEPTIONS
        invalid_parameter        = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    __poner_ceros exidv.
  ENDMETHOD.
  METHOD get_siguiente_n_emb_prop.
    DATA: l_magrv     TYPE mara-magrv,
          l_vhart     TYPE mara-vhart,
          l_traty     TYPE tervh-traty,
          l_nrverg    TYPE tvty-nrverg,
          l_int_nkr   TYPE tvty-int_nkr,
          l_exidv_max TYPE exidv,
          l_exidv_min TYPE exidv,
          l_exidv     TYPE exidv.

    CLEAR num.
* Busco grupo de material de embalaje
    SELECT SINGLE magrv vhart FROM mara
      INTO (l_magrv, l_vhart)
     WHERE matnr = matnr.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* Obtengo el tipo de material de embalaje.
    IF l_vhart IS INITIAL.
      SELECT traty FROM tervh
        INTO l_traty
        UP TO 1 ROWS
       WHERE magrv = l_magrv
        ORDER BY PRIMARY KEY.
      ENDSELECT.
    ELSE.
      SELECT traty FROM tervh
        INTO l_traty
        UP TO 1 ROWS
       WHERE traty = l_vhart
       ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.
    IF NOT l_traty IS INITIAL.
* Recuperamos el nº de
      SELECT SINGLE nrverg int_nkr FROM tvty
        INTO (l_nrverg, l_int_nkr )
       WHERE traty = l_traty.
      IF sy-subrc = 0.
        IF l_nrverg = 'B'.
          zcl_ap_rango_numero=>info_numero( EXPORTING rango = 'HU_VEKP'
                                                        rango_no = l_int_nkr
                                              IMPORTING numero = num ).
        ELSEIF l_nrverg = ''.
          zcl_ap_rango_numero=>info_numero( EXPORTING rango = 'RV_VEKP'
                                                        rango_no = '01'
                                              IMPORTING numero = num ).
        ENDIF.
        l_exidv_max = num.
        l_exidv_min = num - 50.
        zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_exidv_min ).
        SELECT MAX( exidv ) FROM vekp
          INTO l_exidv
         WHERE exidv >= l_exidv_min
           AND exidv <= l_exidv_max.
        IF sy-subrc = 0 AND NOT l_exidv IS INITIAL.
          l_exidv = l_exidv + 1.
          zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_exidv ).
          num = l_exidv.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_status.
    DATA: l_objnr TYPE jest-objnr,
          l_istat TYPE husstat-stat,
          l_txt04 TYPE tj02t-txt04.

    CONCATENATE 'HU' venum INTO l_objnr.
*  CALL FUNCTION 'STATUS_TEXT_EDIT'
*    EXPORTING
*      objnr            = l_objnr
*      FLG_USER_STAT     = 'X'
*      spras            = sy-langu
*      BYPASS_BUFFER    = 'X'
*    IMPORTING
*      E_STSMA          = l_stsma
*      line             = status
*      USER_LINE        = l_USER_LINE
*    EXCEPTIONS
*      object_not_found = 1
*      OTHERS           = 2.

    SELECT stat FROM  husstat
      INTO l_istat
     WHERE objnr = l_objnr
       AND inact = ''.
      SELECT SINGLE txt04 FROM tj02t
        INTO l_txt04
       WHERE spras = sy-langu
         AND istat = l_istat.
      IF sy-subrc = 0.
        IF status IS INITIAL.
          status = l_txt04.
        ELSE.
          CONCATENATE status l_txt04 INTO status SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_vekp.
    CLEAR vekp.

    IF NOT venum IS INITIAL.
      IF solo_get_venum IS INITIAL.
        SELECT SINGLE * FROM vekp
          INTO vekp
         WHERE venum = venum.
      ELSE.
        SELECT SINGLE venum FROM vekp
          INTO vekp-venum
         WHERE venum = venum.
      ENDIF.
    ELSE.
      IF solo_get_venum IS INITIAL.
        SELECT * FROM vekp
          INTO vekp
          UP TO 1 ROWS
         WHERE exidv = exidv
         ORDER BY venum DESCENDING.
        ENDSELECT.
      ELSE.
        SELECT venum FROM vekp
          INTO vekp-venum
          UP TO 1 ROWS
         WHERE exidv = exidv
         ORDER BY venum DESCENDING.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_vekp_st.
    CLEAR vekp.

    IF NOT venum IS INITIAL.
      IF solo_get_venum IS INITIAL.
        SELECT SINGLE * FROM vekp
          INTO vekp
         WHERE venum = venum.
      ELSE.
        SELECT SINGLE venum FROM vekp
          INTO vekp-venum
         WHERE venum = venum.
      ENDIF.
    ELSE.
      IF solo_get_venum IS INITIAL.
        SELECT * FROM vekp
          INTO vekp
          UP TO 1 ROWS
         WHERE exidv = exidv
         ORDER BY venum DESCENDING.
        ENDSELECT.
      ELSE.
        SELECT venum FROM vekp
          INTO vekp-venum
          UP TO 1 ROWS
         WHERE exidv = exidv
         ORDER BY venum DESCENDING.
        ENDSELECT.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_venum.
    CLEAR venum.
    SELECT venum FROM vekp
      INTO venum
      UP TO 1 ROWS
     WHERE exidv = exidv
     ORDER BY venum DESCENDING.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_vepo.
    CLEAR: i_vepo, vepo.

    IF NOT venum IS INITIAL OR NOT exidv IS INITIAL.
      get_vekp( venum = venum
                exidv = exidv ).
    ENDIF.
    IF NOT vekp-venum IS INITIAL.
      SELECT * FROM vepo
        INTO TABLE i_vepo
       WHERE venum = vekp-venum
        ORDER BY PRIMARY KEY.

      READ TABLE i_vepo INTO vepo INDEX 1.
    ENDIF.
  ENDMETHOD.
  METHOD insertar_log_hu.
    DATA l_handle TYPE vevw-handle.
    DATA vevw     TYPE vevw.

    CLEAR message.

    IF NOT handle IS INITIAL.
      l_handle = handle.
    ELSEIF NOT venum IS INITIAL.
      SELECT SINGLE handle FROM vekp
        INTO l_handle
       WHERE venum = venum.
    ELSE.
      SELECT handle FROM vekp
        INTO l_handle
        UP TO 1 ROWS
       WHERE exidv   = exidv
         AND status <> '0060'
       ORDER BY venum DESCENDING.
      ENDSELECT.
    ENDIF.

    IF l_handle IS INITIAL.
      message = 'No se ha encontrado la SSCC'.
    ELSE.
      SELECT SINGLE handle FROM vevw
        INTO vevw-handle
       WHERE handle = l_handle
         AND object = object
         AND objkey = objkey.
      IF sy-subrc <> 0.
        CLEAR vevw.
        vevw-handle = l_handle.
        vevw-object = object.
        vevw-objkey = objkey.
        CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP vevw-tstamp TIME ZONE sy-zonlo.
        INSERT vevw FROM vevw.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD lanzar_humo.
    DATA: l_exidv   TYPE exidv,
          i_exidv   TYPE TABLE OF exidv,
          o_bi      TYPE REF TO zcl_ap_batch_input,
          l_rc      TYPE i,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_mensaje TYPE bapireturn1-message.

    IF NOT exidv IS INITIAL.
      l_exidv = exidv.
    ELSEIF NOT lista_hus IS INITIAL.
      SPLIT lista_hus AT ',' INTO TABLE i_exidv.
      DESCRIBE TABLE i_exidv LINES sy-tfill.
      IF sy-tfill = 1.
        READ TABLE i_exidv INTO l_exidv INDEX 1.
      ENDIF.
    ENDIF.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    IF NOT l_exidv IS INITIAL.
      o_bi->dynpro( program = 'RHU_HELP' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ONLI' ).
      o_bi->campos( campo = 'SELEXIDV-LOW' valor = l_exidv ).
      o_bi->campos( campo = 'LSTAND' valor = 'X' ).
    ELSEIF NOT i_exidv IS INITIAL.
      cl_gui_frontend_services=>clipboard_export(
  IMPORTING
    data                 = i_exidv
  CHANGING
    rc                   = l_rc
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
*    no_authority         = 4
    OTHERS               = 5 ).
      IF sy-subrc <> 0.
        MESSAGE 'Error dejando exidv en portapapeles' TYPE 'I'.
        RETURN.
      ENDIF.

      o_bi->dynpro( program = 'RHU_HELP' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=%00120100000198451' ).
      o_bi->campos( campo = 'SELEXIDV-LOW' valor = '' ).
      o_bi->campos( campo = 'LSTAND' valor = 'X' ).

      o_bi->dynpro( program = 'SAPLALDB' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=CLIP' ).

      o_bi->dynpro( program = 'SAPLALDB' dynpro = '3000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ACPT' ).

      o_bi->dynpro( program = 'RHU_HELP' dynpro = '1000' ).
      o_bi->campos( campo = 'BDC_OKCODE' valor = '=ONLI' ).
    ENDIF.

    l_mensaje = o_bi->llamar_transaccion( tcode = 'HUMO' modo = modo ).
  ENDMETHOD.
  METHOD mover.
    DATA gt_venum TYPE hum_venum_t.
    DATA: gt_exidv TYPE hum_exidv_t,
          imkpf    TYPE imkpf.
    DATA lt_move_to TYPE hum_data_move_to_t.
    DATA ls_venum   TYPE hum_venum.
    DATA ls_move_to TYPE hum_data_move_to.
    DATA ef_posted  TYPE sysubrc.
    DATA: message    TYPE huitem_messages,
          i_messages TYPE huitem_messages_t.

*  DATA: ls_hu_items   TYPE hum_humseg.

    CLEAR: emkpf, i_return, return.

    REFRESH: gt_venum, gt_exidv, lt_move_to.

    IF i_datos_mov IS INITIAL.
      IF vekp IS INITIAL.
        MESSAGE 'No hay datos de caja'(ndc) TYPE 'E'.
      ENDIF.

      ls_venum-venum = vekp-venum.
      APPEND ls_venum TO gt_venum.

      ls_move_to-huwbevent = huwbevent.
      ls_move_to-matnr     = matnr.
      IF NOT charg IS INITIAL.
        ls_move_to-charg = charg.
      ENDIF.
      ls_move_to-werks = werks.
      ls_move_to-lgort = lgort.
      ls_move_to-bwart = bwart.
      ls_move_to-grund = grund.
      ls_move_to-kostl = kostl.

*  LOOP AT me->i_vepo INTO vepo.
*    ls_hu_items-venum = vekp-venum.
*    ls_hu_items-vepos = vepo-vepos.
*    APPEND ls_hu_items TO ls_move_to-hu_items.
*  ENDLOOP.

      APPEND ls_move_to TO lt_move_to.
    ELSE.
      lt_move_to = i_datos_mov.
      gt_venum   = i_cajas_int.
      gt_exidv   = i_cajas_ext.
    ENDIF.

    IF NOT xblnr IS INITIAL OR NOT bktxt IS INITIAL.
      imkpf-xblnr = xblnr.
      imkpf-budat = budat.
      imkpf-bktxt = bktxt.
    ENDIF.

    CALL FUNCTION 'HU_PACKING_REFRESH'.

    CALL FUNCTION 'HU_CREATE_GOODS_MOVEMENT'
      EXPORTING
        if_simulate    = ' '
        if_commit      = ' '
        if_tcode       = tcode
        it_move_to     = lt_move_to
        it_internal_id = gt_venum
        it_external_id = gt_exidv
        is_imkpf       = imkpf
      IMPORTING
        ef_posted      = ef_posted
        es_message     = message
        et_messages    = i_messages
        es_emkpf       = emkpf.

    IF NOT emkpf IS INITIAL.
      COMMIT WORK AND WAIT.
      lights = zcl_ap_alv=>c_sem_verde.

      IF espera_a_grabado = 'X'.
        DO 5 TIMES.
          SELECT SINGLE mblnr FROM mkpf
            INTO emkpf-mblnr
           WHERE mblnr = emkpf-mblnr
             AND mjahr = emkpf-mjahr.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.
      ENDIF.

      IF NOT message-msgid IS INITIAL.
        MESSAGE ID message-msgid TYPE 'S' NUMBER message-msgno
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4
                INTO mensaje.
      ENDIF.
    ELSE.
      lights = zcl_ap_alv=>c_sem_rojo.

      IF NOT message-msgid IS INITIAL.
        CLEAR return.
        return-id = message-msgid.
        IF emkpf IS INITIAL.
          return-type = 'E'.
        ELSE.
          return-type = message-msgty.
        ENDIF.
        return-number     = message-msgno.
        return-message_v1 = message-msgv1.
        return-message_v2 = message-msgv2.
        return-message_v3 = message-msgv3.
        return-message_v4 = message-msgv4.

        MESSAGE ID message-msgid TYPE 'S' NUMBER message-msgno
                WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4
                INTO return-message.
        mensaje = return-message.
        APPEND return TO i_return.
      ENDIF.

      LOOP AT i_messages ASSIGNING FIELD-SYMBOL(<MESSAGE>) WHERE msgid <> ''.
        CLEAR return.
        return-id         = <MESSAGE>-msgid.
        return-type       = <MESSAGE>-msgty.
        return-number     = <MESSAGE>-msgno.
        return-message_v1 = <MESSAGE>-msgv1.
        return-message_v2 = <MESSAGE>-msgv2.
        return-message_v3 = <MESSAGE>-msgv3.
        return-message_v4 = <MESSAGE>-msgv4.

        MESSAGE ID <MESSAGE>-msgid TYPE <MESSAGE>-msgty NUMBER <MESSAGE>-msgno
                WITH <MESSAGE>-msgv1 <MESSAGE>-msgv2 <MESSAGE>-msgv3 <MESSAGE>-msgv4
                INTO return-message.
        APPEND return TO i_return.
        IF mensaje IS INITIAL.
          mensaje = return-message.
        ENDIF.
      ENDLOOP.

      IF mensaje IS INITIAL.
        mensaje = 'No ha sido posible hacer el movimiento'(npm).
      ENDIF.

    ENDIF.

    IF dequeue_all = 'X'.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDIF.

    IF message = 'S::000'.
      CLEAR message.
    ENDIF.
    IF mensaje = 'S::000'.
      CLEAR mensaje.
    ENDIF.
  ENDMETHOD.
  METHOD pack.
    DATA item TYPE bapihuitmproposal.

    CLEAR item.
    item-base_unit_qty = meins.
    item-hu_item_type  = item_type.
    item-material      = matnr.
    item-batch         = charg.
    item-plant         = werks.
    item-stge_loc      = lgort.
    item-pack_qty      = pack_qty.
    item-stock_cat     = bestq.

    CLEAR: return, i_return.
    CALL FUNCTION 'BAPI_HU_PACK'
      EXPORTING
        hukey               = hukey
        itemproposal        = item
*        IMPORTING
*         huitem              = huitem
*   HUHEADER            =
      TABLES
*   SERIALNUMBERS       =
        return              = i_return.

    IF NOT hukey IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

    READ TABLE i_return INTO return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      mensaje = return-message.
    ENDIF.
  ENDMETHOD.
  METHOD unpack.
    DATA: l_venum TYPE vekp-venum,
          l_exidv TYPE vekp-exidv,
          i_vepo  TYPE TABLE OF vepo,
          l_item  TYPE bapihuitmunpack.

    IF hukey IS INITIAL.
      l_venum = venum.
      SELECT SINGLE exidv FROM vekp
        INTO l_exidv
      WHERE venum = venum.
    ELSE.
      l_exidv = hukey.
      IF venum IS INITIAL.
        SELECT venum FROM vekp
          INTO l_venum
          UP TO 1 ROWS
         WHERE exidv = hukey
          ORDER BY PRIMARY KEY.
        ENDSELECT.
      ELSE.
        l_venum = venum.
      ENDIF.
    ENDIF.

    SELECT velin vepos matnr charg vemng vemeh werks lgort FROM vepo
      INTO CORRESPONDING FIELDS OF TABLE i_vepo
     WHERE venum = l_venum
      ORDER BY PRIMARY KEY.

    LOOP AT i_vepo ASSIGNING FIELD-SYMBOL(<VEPO>).
      CLEAR l_item.
      l_item-hu_item_type   = <VEPO>-velin.
      l_item-hu_item_number = <VEPO>-vepos.
      l_item-material       = <VEPO>-matnr.
      l_item-batch          = <VEPO>-charg.
      l_item-pack_qty       = <VEPO>-vemng.
      l_item-base_unit_qty  = <VEPO>-vemeh.
      l_item-plant          = <VEPO>-werks.
      l_item-stge_loc       = <VEPO>-lgort.

      CLEAR: return, i_return.
      CALL FUNCTION 'BAPI_HU_UNPACK'
        EXPORTING
          hukey      = l_exidv
          itemunpack = l_item
        TABLES
          return     = i_return.

      READ TABLE i_return INTO return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        message = return-message.
        RETURN.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDLOOP.
    IF sy-subrc <> 0.
      message = 'No hay posiciones a desembalar'(npd).
    ENDIF.
  ENDMETHOD.
  METHOD visualizar.
    DATA: l_venum TYPE venum,
          i_venum TYPE hum_venum_t.

    IF venum IS INITIAL AND NOT exidv IS INITIAL.
      SELECT venum FROM vekp
        INTO l_venum
        UP TO 1 ROWS
       WHERE exidv   = exidv
         AND status <> '0060'
        ORDER BY venum DESCENDING.
      ENDSELECT.
      IF sy-subrc <> 0.
        SELECT venum FROM vekp
          INTO l_venum
          UP TO 1 ROWS
         WHERE exidv = exidv
          ORDER BY venum DESCENDING.
        ENDSELECT.
      ENDIF.

    ELSE.
      l_venum = venum.
    ENDIF.

    APPEND l_venum TO i_venum.
    CALL FUNCTION 'HU_DISPLAY'
      EXPORTING
*     IF_DISPLAY   = 'X'
*     IF_COMMIT    = ' '
        it_venum     = i_venum
*     IF_AR_HUS    = ' '
*     IF_EXIDV_FOR_DELETED       = ' '
* IMPORTING
*     EF_FCODE     =
*     EF_DATA_CHANGED            =
*     ET_MESSAGES  =
*     ET_HEADER    =
*     ET_ITEMS     =
      EXCEPTIONS
        no_hus_found = 1
        not_allowed  = 2
        fatal_error  = 3
        OTHERS       = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
