CLASS zcl_ap_clas_lote DEFINITION
  PUBLIC
  INHERITING FROM zcl_ap_clasificacion
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS clase_lotes TYPE bapi1003_key-classnum VALUE '' ##NO_TEXT.

    DATA i_cabn  TYPE tt_cabn.
    DATA i_cawnt TYPE tt_cawnt.

    CONSTANTS clase_material TYPE bapi1003_key-classnum VALUE '' ##NO_TEXT.

    METHODS constructor
      IMPORTING objecttable TYPE bapi1003_key-objecttable DEFAULT 'MCHA'
                classnum    TYPE bapi1003_key-classnum    DEFAULT clase_lotes
                classtype   TYPE bapi1003_key-classtype   DEFAULT '023'.

    METHODS get_datos_pedido
      IMPORTING vbeln TYPE any
                posnr TYPE any
                opt   TYPE abap_bool DEFAULT ''
                unif  TYPE abap_bool DEFAULT ''.

    METHODS get_datos_material_lote
      IMPORTING matnr            TYPE any
                charg            TYPE any
                opt              TYPE abap_bool DEFAULT ''
                caract           TYPE any       DEFAULT ''
                unif             TYPE abap_bool DEFAULT ''
                unvaluated_chars TYPE abap_bool DEFAULT ''
                klart            TYPE any       DEFAULT '023'
                obtab            TYPE any       DEFAULT 'MCH1'
                werks            TYPE werks_d   DEFAULT ''
                atinn            TYPE atinn     OPTIONAL
      EXPORTING !message         TYPE bapi_msg.

    METHODS get_datos_material
      IMPORTING matnr            TYPE any
                opt              TYPE abap_bool  DEFAULT ''
                unif             TYPE abap_bool  DEFAULT ''
                klart            TYPE klassenart DEFAULT '001'
                classnum         TYPE klasse_d   DEFAULT 'MODULOS_ZFER'
                unvaluated_chars TYPE abap_bool  DEFAULT ''.

    METHODS modifica_lote
      IMPORTING matnr   TYPE matnr
                charg   TYPE charg_d
                werks   TYPE werks_d   DEFAULT ''
                !commit TYPE abap_bool DEFAULT 'X'
                obtab   TYPE any       DEFAULT 'MCH1'
                klart   TYPE any       DEFAULT '023'.

    CLASS-METHODS existe_clas_material_lote
      IMPORTING matnr     TYPE any
                charg     TYPE any
      RETURNING VALUE(si) TYPE abap_bool.

    CLASS-METHODS get_caract_mat_lote
      IMPORTING matnr        TYPE any
                charg        TYPE any
                caract       TYPE any       OPTIONAL
                opt          TYPE abap_bool DEFAULT ''
                fecha        TYPE abap_bool DEFAULT ''
                klart        TYPE any       DEFAULT '023'
                obtab        TYPE any       DEFAULT 'MCH1'
                werks        TYPE werks_d   DEFAULT ''
                atinn        TYPE atinn     OPTIONAL
      RETURNING VALUE(valor) TYPE string.

    CLASS-METHODS get_caract_mat
      IMPORTING matnr        TYPE any
                caract       TYPE any
                num          TYPE abap_bool  DEFAULT ''
                klart        TYPE ausp-klart DEFAULT '001'
      RETURNING VALUE(valor) TYPE string.

    METHODS modifica_mat
      IMPORTING matnr    TYPE matnr
                classnum TYPE any OPTIONAL.

    CLASS-METHODS matchcode_caract2
      IMPORTING caract       TYPE any
      RETURNING VALUE(valor) TYPE atwrt.

    METHODS get_datos_reserva
      IMPORTING rsnum TYPE rsnum      OPTIONAL
                rspos TYPE rspos      OPTIONAL
                cuobj TYPE resb-cuobj OPTIONAL.

    CLASS-METHODS get_caract_orden
      IMPORTING aufnr           TYPE aufnr
      RETURNING VALUE(i_caract) TYPE tt_bapi1003_alloc_values_char.

    CLASS-METHODS ver_mat_lote
      IMPORTING matnr TYPE any
                charg TYPE any.

    CLASS-METHODS get_caract_mat_num
      IMPORTING matnr        TYPE any
                caract       TYPE any
                klart        TYPE ausp-klart DEFAULT '001'
      RETURNING VALUE(valor) TYPE atflv.

    CLASS-METHODS set_caract_mat_lote
      IMPORTING matnr            TYPE any
                charg            TYPE any
                caract           TYPE any       OPTIONAL
                VALUE(valor)     TYPE any
                unvaluated_chars TYPE abap_bool DEFAULT ''
                !commit          TYPE abap_bool DEFAULT 'X'
                werks            TYPE werks_d   DEFAULT ''
                obtab            TYPE any       DEFAULT 'MCH1'
                klart            TYPE any       DEFAULT '023'
                atinn            TYPE atinn     OPTIONAL
      RETURNING VALUE(mensaje)   TYPE string.

    CLASS-METHODS get_valnum_mcha
      IMPORTING matnr        TYPE any
                charg        TYPE any
                werks        TYPE any
                caract       TYPE any
                klart        TYPE ausp-klart DEFAULT '022'
      RETURNING VALUE(valor) TYPE atflv.

    CLASS-METHODS asignar_clase_lote
      IMPORTING classnum       TYPE any
                classtype      TYPE any       DEFAULT '023'
                tabla          TYPE any       DEFAULT 'MCH1'
                matnr          TYPE any
                !commit        TYPE abap_bool DEFAULT 'X'
                charg          TYPE charg_d
                werks          TYPE werks_d   OPTIONAL
      RETURNING VALUE(mensaje) TYPE bapi_msg.

  PROTECTED SECTION.

  PRIVATE SECTION.
endclass. "ZCL_AP_CLAS_LOTE definition
class ZCL_AP_CLAS_LOTE implementation.
  METHOD asignar_clase_lote.
    DATA l_objek TYPE c LENGTH 40.

    l_objek(18) = matnr.
    IF werks IS INITIAL.
      l_objek+18 = charg.
    ELSE.
      l_objek+18 = werks.
      l_objek+22 = charg.
    ENDIF.

    mensaje = zcl_ap_clas_lote=>asignar_clase(
                  classnum  = classnum
                  classtype = classtype
                  tabla     = tabla
                  objectkey = l_objek
                  commit    = commit ).
  ENDMETHOD.
  METHOD constructor.
    DATA l_classnum TYPE bapi1003_key-classnum.

*  IF sy-sysid = 'DES' AND classnum = clase_lotes.
*    l_classnum = 'ZPRUEBA'.
*  ELSE.
    l_classnum = classnum.
*  ENDIF.

    super->constructor(
        objecttable = objecttable
        classnum    = l_classnum
        classtype   = classtype ).
  ENDMETHOD.
  METHOD existe_clas_material_lote.
    DATA o_cl TYPE REF TO zcl_ap_clas_lote.

    o_cl = NEW #( ).

    o_cl->get_datos_material_lote( matnr = matnr charg = charg ).

    IF NOT o_cl->i_car_curr[] IS INITIAL. " Busco que tenga ZH_COSTE!
      si = 'X'.
    ENDIF.
  ENDMETHOD.
  METHOD get_caract_mat.
    DATA: l_atinn      TYPE atinn,
          l_objek      TYPE ausp-objek,
          l_value_char TYPE ausp-atwrt,
          l_value_num  TYPE ausp-atflv,
          l_value_p    TYPE p LENGTH 8 DECIMALS 3.

    l_atinn = get_caract_interno( caract ).

    CLEAR valor.

    IF klart = '001'.
      l_objek = matnr.
    ELSE.
      SELECT cuobj FROM inob
        INTO l_objek
        UP TO 1 ROWS
       WHERE klart = '023'
         AND objek = matnr
         AND obtab = 'MARA'
        ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.

    SELECT atwrt atflv FROM ausp
      INTO (l_value_char, l_value_num)
      UP TO 1 ROWS
     WHERE objek = l_objek
       AND mafid = 'O'
       AND klart = klart
       AND atinn = l_atinn
      ORDER BY PRIMARY KEY.
    ENDSELECT.

    IF sy-subrc = 0.
      IF NOT num IS INITIAL.
        l_value_p = l_value_num.
        l_value_char = l_value_p.
      ENDIF.
      valor = l_value_char.
    ENDIF.
  ENDMETHOD.
  METHOD get_caract_mat_lote.
    DATA: o_cl     TYPE REF TO zcl_ap_clas_lote,
          l_caract TYPE atwrt.
    DATA l_str TYPE c LENGTH 30.

    o_cl = NEW #( ).
    o_cl->get_datos_material_lote( matnr = matnr charg = charg opt = opt caract = caract atinn = atinn klart = klart obtab = obtab werks = werks ).

    IF NOT caract IS INITIAL.
      l_caract = caract.
    ELSEIF NOT atinn IS INITIAL.
      l_caract = get_caract_externo( atinn ).
    ENDIF.

    valor = o_cl->get_char_clave( l_caract ).
    IF valor = ''.
      IF fecha = 'X'.
        l_str = o_cl->get_curr_clave( l_caract ).
        REPLACE '.' WITH '' INTO l_str.
        CONDENSE l_str NO-GAPS.
        valor = l_str(10).
      ELSE.
        valor = o_cl->get_curr_clave( l_caract ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_caract_mat_num.
    DATA: l_atinn TYPE atinn,
          l_objek TYPE ausp-objek.

    l_atinn = get_caract_interno( caract ).

    CLEAR valor.

    IF klart = '001'.
      l_objek = matnr.
    ELSE.
      SELECT cuobj FROM inob
        INTO l_objek
        UP TO 1 ROWS
       WHERE klart = '023'
         AND objek = matnr
         AND obtab = 'MARA'
        ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.

    SELECT atflv FROM ausp
      INTO valor
      UP TO 1 ROWS
     WHERE objek = l_objek
       AND mafid = 'O'
       AND klart = klart
       AND atinn = l_atinn
      ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.
  METHOD get_caract_orden.
    DATA: i_resb  TYPE TABLE OF resb,
          l_resb  TYPE resb,
          o_lotes TYPE REF TO zcl_ap_clas_lote.

    CLEAR i_caract.

    SELECT rsnum rspos cuobj FROM resb                  "#EC CI_NOFIELD
      INTO CORRESPONDING FIELDS OF TABLE i_resb
     WHERE aufnr = aufnr
      ORDER BY PRIMARY KEY.

    LOOP AT i_resb INTO l_resb.
      AT FIRST.
        o_lotes = NEW #( ).
      ENDAT.

      o_lotes->get_datos_reserva( rsnum = l_resb-rsnum rspos = l_resb-rspos cuobj = l_resb-cuobj ).

      LOOP AT o_lotes->i_car_char INTO o_lotes->car_char.
        APPEND o_lotes->car_char TO i_caract.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_datos_material.
    DATA: l_objek TYPE inob-objek,
          i_ausp  TYPE TABLE OF ausp,
          l_cabn  TYPE cabn.

    FIELD-SYMBOLS: <ausp> TYPE ausp,
                   <cabn> TYPE cabn.

    CLEAR: i_car_num, i_car_char, i_car_curr, i_car_unif.

    objecttable = 'MARA'.
    me->classnum = classnum.
    classtype = klart.
    l_objek = matnr.

    IF opt IS INITIAL.
      get_datos( objectkey = l_objek unif = unif unvaluated_chars = unvaluated_chars ).
    ELSE.
      CLEAR: i_car_num, i_car_char, i_car_curr, i_return.

      SELECT atflb atflv atinn atwrt FROM ausp
        INTO CORRESPONDING FIELDS OF TABLE i_ausp
       WHERE objek = l_objek
         AND mafid = 'O'
         AND klart = klart.

      LOOP AT i_ausp ASSIGNING <ausp>.
        CLEAR car_char.
        ASSIGN i_cabn[ atinn = <ausp>-atinn ] TO <cabn>.
        IF sy-subrc = 0.
          car_char-charact = <cabn>-atnam.
        ELSE.
          SELECT * FROM cabn
            INTO l_cabn
            UP TO 1 ROWS
           WHERE atinn = <ausp>-atinn
            ORDER BY PRIMARY KEY.
          ENDSELECT.
          APPEND l_cabn TO i_cabn.
          car_char-charact = l_cabn-atnam.
        ENDIF.

        IF <ausp>-atwrt = '' AND <ausp>-atflv <> 0.
          CLEAR car_curr.
          car_curr-value_from = <ausp>-atflv.
          car_curr-value_to   = <ausp>-atflb.
          car_curr-charact    = car_char-charact.
          APPEND car_curr TO i_car_curr.
        ELSE.
          car_char-value_char = <ausp>-atwrt.
          APPEND car_char TO i_car_char.
        ENDIF.
      ENDLOOP.

      IF unif = 'X'.
        i_car_unif = get_caract_simple( i_car_char = i_car_char
                                        i_car_num  = i_car_num
                                        i_car_curr = i_car_curr ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_datos_material_lote.
    DATA: l_leng   TYPE dd01l-leng,
          l_long   TYPE i,
          l_objek  TYPE inob-objek,
          l_cuobj  TYPE inob-cuobj,
          l_klart  TYPE tcla-klart,
          l_number TYPE inob-cuobj,
          l_atinn  TYPE atinn,
          i_ausp   TYPE TABLE OF ausp,
          l_cabn   TYPE cabn,
          l_cawnt  TYPE wrf_apc_v_cawnt,
          i_cawnt  TYPE TABLE OF wrf_apc_v_cawnt.

    FIELD-SYMBOLS <ausp> TYPE ausp.

    CLEAR message.
    SELECT leng FROM dd01l
      INTO l_leng
      UP TO 1 ROWS
     WHERE domname = 'MATNR'
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      l_long = l_leng.

    ELSE.
      l_long = 18.
    ENDIF.

    CLEAR: i_car_num, i_car_char, i_car_curr, i_return.
    IF matnr IS INITIAL.
      RETURN.
    ENDIF.
    IF charg IS INITIAL.
      RETURN.
    ENDIF.
    objecttable = obtab.
    l_objek(l_long) = matnr.
    IF werks IS INITIAL.
      l_objek+l_long = charg.

* Verificamos si existe ese valor con 40 o con 18
      IF l_long = 40.
        SELECT cuobj FROM inob
          INTO l_cuobj
          UP TO 1 ROWS
         WHERE klart = klart
           AND obtab = obtab
           AND objek = l_objek
          ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          CLEAR l_objek.
          l_long = 18.
          l_objek(l_long) = matnr.
          l_objek+l_long  = charg.
        ENDIF.
      ENDIF.
    ELSE.
      l_objek+l_long = werks.
      l_long = l_long + 4.
      l_objek+l_long = charg.
    ENDIF.

    IF opt IS INITIAL OR unvaluated_chars = 'X'.
      l_klart = klart.
      CALL FUNCTION 'CUOB_GET_NUMBER'
        EXPORTING
          class_type    = l_klart
          object_id     = l_objek
          table         = objecttable
        IMPORTING
          object_number = l_number
        EXCEPTIONS
          OTHERS        = 0.

      CALL FUNCTION 'CLFM_GET_CLASS_FOR_CUOBJ'
        EXPORTING
          internal_obj_number = l_number
*         DATE_OF_CHANGE      =
*         I_AENNR             = ' '
        IMPORTING
          class               = classnum
*         CLASSTYPE           =
*         FROM_BUFFER         =
*         FROM_DATABASE       =
        EXCEPTIONS
          no_class_found      = 1
          OTHERS              = 2.
*
      IF sy-subrc <> 0.
        message = 'Error recuperando clase'.
        RETURN.
      ENDIF.
      get_datos( objectkey        = l_objek
                 unif             = unif
                 unvaluated_chars = unvaluated_chars ).
    ELSE.
      SELECT cuobj FROM inob
        INTO l_cuobj
        UP TO 1 ROWS
       WHERE klart = klart
         AND obtab = obtab
         AND objek = l_objek
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      IF NOT atinn IS INITIAL.
        l_atinn = atinn.
      ELSEIF NOT caract IS INITIAL.
        l_atinn = get_caract_interno( caract ).
      ENDIF.

      IF l_atinn IS INITIAL.
        SELECT atflv atinn atwrt FROM ausp
          INTO CORRESPONDING FIELDS OF TABLE i_ausp
         WHERE objek = l_cuobj
           AND mafid = 'O'
           AND klart = klart.
      ELSE.
        SELECT atflv atinn atwrt FROM ausp
          INTO CORRESPONDING FIELDS OF TABLE i_ausp
         WHERE objek = l_cuobj
           AND mafid = 'O'
           AND klart = klart
           AND atinn = l_atinn.
      ENDIF.

      LOOP AT i_ausp ASSIGNING <ausp>.
        CLEAR car_char.
        READ TABLE i_cabn INTO l_cabn WITH KEY atinn = <ausp>-atinn.
        IF sy-subrc = 0.
          car_char-charact = l_cabn-atnam.
        ELSE.
          SELECT * FROM cabn
            INTO l_cabn
            UP TO 1 ROWS
           WHERE atinn = <ausp>-atinn
            ORDER BY PRIMARY KEY.
          ENDSELECT.
          IF sy-subrc = 0.
            APPEND l_cabn TO i_cabn.
            car_char-charact = l_cabn-atnam.
          ENDIF.
        ENDIF.

        IF <ausp>-atwrt = '' AND <ausp>-atflv <> 0.
          CLEAR car_curr.
          car_curr-value_from = <ausp>-atflv.
          car_curr-charact    = car_char-charact.
          APPEND car_curr TO i_car_curr.
        ELSE.

          CLEAR l_cawnt.
          READ TABLE i_cawnt INTO l_cawnt WITH KEY atinn = <ausp>-atinn
                                                   atwrt = <ausp>-atwrt.
          IF sy-subrc <> 0.
            SELECT  *
              INTO CORRESPONDING FIELDS OF l_cawnt
              UP TO 1 ROWS
              FROM wrf_apc_v_cawnt
             WHERE atinn = <ausp>-atinn
               AND atwrt = <ausp>-atwrt
               AND spras = sy-langu
             ORDER BY PRIMARY KEY.
            ENDSELECT.
            IF sy-subrc <> 0.
              l_cawnt-atinn = <ausp>-atinn.
              l_cawnt-atwrt = <ausp>-atwrt.
              l_cawnt-atwtb = '@@NO@@'.
            ENDIF.
            APPEND l_cawnt TO i_cawnt.
          ENDIF.

          car_char-value_neutral = <ausp>-atwrt.
          IF l_cawnt IS INITIAL OR l_cawnt-atwtb = '@@NO@@'.
            car_char-value_char = <ausp>-atwrt.
          ELSE.
            car_char-value_char = l_cawnt-atwtb.
          ENDIF.
          APPEND car_char TO i_car_char.
        ENDIF.
      ENDLOOP.

      IF unif = 'X'.
        i_car_unif = get_caract_simple( i_car_char = i_car_char
                                        i_car_num  = i_car_num
                                        i_car_curr = i_car_curr ).
      ENDIF.

    ENDIF.
  ENDMETHOD.
  METHOD get_datos_pedido.
    DATA: l_objek TYPE inob-objek,
          l_cuobj TYPE inob-cuobj,
          i_ausp  TYPE TABLE OF ausp,
          l_cabn  TYPE cabn.

    FIELD-SYMBOLS: <ausp> TYPE ausp,
                   <cabn> TYPE cabn.

    objecttable = 'VBAP'.
    CONCATENATE vbeln posnr INTO l_objek.

    IF opt = ''.
      get_datos( l_objek ).
    ELSE.
      CLEAR: i_car_num, i_car_char, i_car_curr, i_return.
      SELECT cuobj FROM inob
        INTO l_cuobj
        UP TO 1 ROWS
       WHERE klart = '023'
         AND obtab = 'VBAP'
         AND objek = l_objek
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      SELECT atflv atinn atwrt FROM ausp
        INTO CORRESPONDING FIELDS OF TABLE i_ausp
       WHERE objek = l_cuobj
         AND mafid = 'O'
         AND klart = '023'.

      LOOP AT i_ausp ASSIGNING <ausp>.
        CLEAR car_char.
        ASSIGN i_cabn[ atinn = <ausp>-atinn ] TO <cabn>.
        IF sy-subrc = 0.
          car_char-charact = <cabn>-atnam.
        ELSE.
          SELECT * FROM cabn
            INTO l_cabn
            UP TO 1 ROWS
           WHERE atinn = <ausp>-atinn
            ORDER BY PRIMARY KEY.
          ENDSELECT.
          APPEND l_cabn TO i_cabn.
          car_char-charact = l_cabn-atnam.
        ENDIF.

        IF <ausp>-atwrt = '' AND <ausp>-atflv <> 0.
          CLEAR car_curr.
          car_curr-value_from = <ausp>-atflv.
          car_curr-charact    = car_char-charact.
          APPEND car_curr TO i_car_curr.
        ELSE.
          car_char-value_neutral = <ausp>-atwrt.
          car_char-value_char    = car_char-value_neutral.
          APPEND car_char TO i_car_char.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF unif = 'X'.
      i_car_unif = get_caract_simple( i_car_char = i_car_char
                                      i_car_num  = i_car_num
                                      i_car_curr = i_car_curr ).
    ENDIF.
  ENDMETHOD.
  METHOD get_datos_reserva.
    DATA: l_objek TYPE inob-objek,
          l_cuobj TYPE inob-cuobj,
          i_ausp  TYPE TABLE OF ausp,
          l_cabn  TYPE cabn.

    FIELD-SYMBOLS: <ausp> TYPE ausp,
                   <cabn> TYPE cabn.

    objecttable = 'RESB'.

    IF cuobj IS INITIAL.
      CONCATENATE rsnum rspos INTO l_objek.

      CLEAR: i_car_num, i_car_char, i_car_curr, i_return.
      SELECT cuobj FROM inob
        INTO l_cuobj
        UP TO 1 ROWS
       WHERE klart = '023'
         AND obtab = 'RESB'
         AND objek = l_objek
        ORDER BY PRIMARY KEY.
      ENDSELECT.
    ELSE.
      l_cuobj = cuobj.
    ENDIF.

    IF l_cuobj IS INITIAL.
      RETURN.
    ENDIF.

    SELECT atflv atinn atwrt FROM ausp
      INTO CORRESPONDING FIELDS OF TABLE i_ausp
     WHERE objek = l_cuobj
       AND mafid = 'O'
       AND klart = '023'.

    LOOP AT i_ausp ASSIGNING <ausp>.
      CLEAR car_char.
      ASSIGN i_cabn[ atinn = <ausp>-atinn ] TO <cabn>.
      IF sy-subrc = 0.
        car_char-charact = <cabn>-atnam.
      ELSE.
        SELECT * FROM cabn
          INTO l_cabn
          UP TO 1 ROWS
         WHERE atinn = <ausp>-atinn
          ORDER BY PRIMARY KEY.
        ENDSELECT.
        APPEND l_cabn TO i_cabn.
        car_char-charact = l_cabn-atnam.
      ENDIF.

      IF <ausp>-atwrt = '' AND <ausp>-atflv <> 0.
        CLEAR car_curr.
        car_curr-value_from = <ausp>-atflv.
        car_curr-charact    = car_char-charact.
        APPEND car_curr TO i_car_curr.
      ELSE.
        car_char-value_char = <ausp>-atwrt.
        APPEND car_char TO i_car_char.
      ENDIF.
    ENDLOOP.

*  DATA: l_objek TYPE inob-objek,
*          l_number TYPE inob-cuobj,
*          l_object TYPE bapi1003_key-object,
*           i_ausp TYPE TABLE OF ausp.
*  FIELD-SYMBOLS <ausp> TYPE AUSP.
*
*  me->objecttable = 'RESB'.
*
*
*  CONCATENATE rsnum rspos INTO l_objek.
*
*  CALL FUNCTION 'CUOB_GET_NUMBER'
*    EXPORTING
*      class_type    = '023'
*      object_id     = l_objek
*      table         = 'RESB'
*    IMPORTING
*      object_number = l_number
*    EXCEPTIONS
*      OTHERS        = 0.
*

*
*  CALL FUNCTION 'CLFM_GET_CLASS_FOR_CUOBJ'
*    EXPORTING
*      internal_obj_number       = l_number
**   DATE_OF_CHANGE            =
**   I_AENNR                   = ' '
*    IMPORTING
*      class                     = me->classnum
**   CLASSTYPE                 =
**   FROM_BUFFER               =
**   FROM_DATABASE             =
*   EXCEPTIONS
*     no_class_found            = 1
*     OTHERS                    = 2.
*
*  l_object = l_number.
**  get_datos( l_object ).
*  DATA i_ausp TYPE TABLE OF ausp.
*  CALL FUNCTION 'CLFM_SELECT_AUSP'
*         EXPORTING
*           mafid              = 'O'
*           classtype          = '023'
*           object             = l_number
**          key_date           = rmclf-datuv1
**          with_change_number = change_subsc_act
**          i_aennr            = rmclf-aennr1
*           i_atzhl_same_ini   = 'X'
*         TABLES
*           exp_ausp           = i_ausp
*         EXCEPTIONS
*           no_values          = 01.
  ENDMETHOD.
  METHOD get_valnum_mcha.
    DATA l_objectkey TYPE inob-objek.

    l_objectkey    = matnr.
    l_objectkey+18 = werks.
    l_objectkey+22 = charg.

    CLEAR valor.
    SELECT ausp~atflv
      INTO valor
      UP TO 1 ROWS
      FROM inob INNER JOIN ausp
        ON  ausp~objek = inob~cuobj
        AND ausp~klart = inob~klart INNER JOIN cabn
        ON  cabn~atinn = ausp~atinn
        AND cabn~atnam = caract
     WHERE inob~obtab = 'MCHA'
       AND inob~objek = l_objectkey
       AND inob~klart = klart
      ORDER BY inob~cuobj ausp~objek ausp~atinn ausp~atzhl ausp~mafid ausp~klart ausp~adzhl.
    ENDSELECT.
  ENDMETHOD.
  METHOD matchcode_caract2.
    DATA: i_cawn  TYPE TABLE OF cawn,
          o_mc    TYPE REF TO zcl_ap_matchcode_z,
          l_cawn  TYPE cawn,
          l_cawnt TYPE cawnt.

    i_cawn = get_valores_caract( caract ).
    IF i_cawn IS INITIAL.
      RETURN.
    ENDIF.

    o_mc = NEW #(
        tabname = 'CAWN' ).

    o_mc->add_field( field = 'ATWRT' selectflag = 'X' ).
    o_mc->add_field( tabname = 'CAWNT' field = 'ATWTB' ).

    LOOP AT i_cawn INTO l_cawn.
      o_mc->add_valor( l_cawn-atwrt ).

      CLEAR l_cawnt.
      SELECT SINGLE atwtb FROM  cawnt
        INTO l_cawnt-atwtb
     WHERE atinn = l_cawn-atinn
       AND atzhl = l_cawn-atzhl
       AND spras = sy-langu
       AND adzhl = l_cawn-adzhl.
      IF sy-subrc <> 0.
        SELECT atwtb FROM  cawnt
          INTO l_cawnt-atwtb
          UP TO 1 ROWS
         WHERE atinn = l_cawn-atinn
           AND atzhl = l_cawn-atzhl
           AND adzhl = l_cawn-adzhl
         ORDER BY PRIMARY KEY.
        ENDSELECT.
      ENDIF.
      o_mc->add_valor( l_cawnt-atwtb ).
    ENDLOOP.

    o_mc->matchcode( CHANGING valor = valor ).
  ENDMETHOD.
  METHOD modifica_lote.
    DATA l_objek TYPE inob-objek.

    objecttable = obtab.
    classtype   = klart.

    l_objek(18) = matnr.
    IF werks IS INITIAL.
      l_objek+18 = charg.
    ELSE.
      l_objek+18 = werks.
      l_objek+22 = charg.
    ENDIF.

    set_datos( objectkey = l_objek commit = commit dequeue_all = 'X' ).
  ENDMETHOD.
  METHOD modifica_mat.
    DATA l_objek TYPE inob-objek.

    me->objecttable = 'MARA'.
*    classtype   = '001'.
    me->classnum = classnum.

    SELECT klart FROM klah
      INTO me->classtype
      UP TO 1 ROWS
     WHERE class = classnum
      ORDER BY klart.
    ENDSELECT.

    l_objek = matnr.

    set_datos( l_objek ).
  ENDMETHOD.
  METHOD set_caract_mat_lote.
    DATA: o_cld    TYPE REF TO zcl_ap_clas_lote,
          l_caract TYPE atnam.

    o_cld = NEW #( ).

    IF NOT caract IS INITIAL.
      l_caract = caract.
    ELSE.
      l_caract = get_caract_externo( atinn ).
    ENDIF.

    o_cld->classtype = klart.
    o_cld->get_datos_material_lote( matnr = matnr
                                    charg = charg
                                    werks = werks
                                    obtab = obtab
                                    klart = klart
                                    unvaluated_chars = 'X' ).

    IF unvaluated_chars IS INITIAL.
      DELETE o_cld->i_car_char WHERE charact <> l_caract AND value_char IS INITIAL.
      DELETE o_cld->i_car_num WHERE charact <> l_caract AND value_from IS INITIAL AND value_to IS INITIAL.
      DELETE o_cld->i_car_curr WHERE charact <> l_caract AND value_from IS INITIAL AND value_to IS INITIAL.
    ENDIF.

    READ TABLE o_cld->i_car_char INTO o_cld->car_char WITH KEY charact = l_caract.
    IF sy-subrc = 0.
      o_cld->set_char( charact = l_caract value_char = valor value_neutral = valor ).
    ELSE.
      READ TABLE o_cld->i_car_num INTO o_cld->car_num WITH KEY charact = l_caract.
      IF sy-subrc = 0.
        o_cld->set_num( charact = l_caract value_from = valor ).
      ELSE.
        READ TABLE o_cld->i_car_curr INTO o_cld->car_curr WITH KEY charact = l_caract.
        IF sy-subrc = 0.
          o_cld->set_curr( charact = l_caract value_from = valor ).
        ENDIF.
      ENDIF.
    ENDIF.

    o_cld->modifica_lote( matnr = matnr charg = charg werks = werks commit = commit obtab = obtab klart = klart ).
    LOOP AT o_cld->i_return INTO o_cld->return WHERE type = 'E'.
    ENDLOOP.
    IF sy-subrc = 0.
      mensaje = o_cld->return-message.
    ENDIF.

    o_cld->free( ).
  ENDMETHOD.
  METHOD ver_mat_lote.
    DATA o_bi      TYPE REF TO zcl_ap_batch_input.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA l_mensaje TYPE bapireturn1-message.

    o_bi = NEW #( ).

    o_bi->inicio( ).

    o_bi->dynpro( program = 'SAPLCHRG' dynpro = '1000' ).
    o_bi->campos( campo = 'BDC_OKCODE' valor = '=CLAS' ).
    o_bi->campos( campo = 'DFBATCH-MATNR' valor = matnr ). " Número de material
    o_bi->campos( campo = 'DFBATCH-CHARG' valor = charg ). " Número de lote
    o_bi->campos( campo = 'DFBATCH-WERKS' valor = '' ). " Centro
    o_bi->campos( campo = 'DFBATCH-LGORT' valor = '' ). " Almacén

    l_mensaje = o_bi->llamar_transaccion( tcode = 'MSC3N' modo = 'E' ).
  ENDMETHOD.
