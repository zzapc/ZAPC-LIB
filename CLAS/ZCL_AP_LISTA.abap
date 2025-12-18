
class ZCL_AP_LISTA definition
  public
  final
  create public .

public section.

  data LISTA type STRING .
  data I_LISTA type ZAP_LISTA_T .
  data LIST type ZAP_LISTA .
  class-data C_NO_VALOR type CHAR3 .

  methods CONSTRUCTOR
    importing
      !LISTA type ANY default ''
      !SEPARADOR type ANY default ',' .
  class-methods LISTA2RANGO
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
      !OPTION type ANY default 'EQ'
      !PERMITIR_VACIAS type ABAP_BOOL default ''
      !SIGN type SIGN default 'I'
      !INI_EXC type CHAR1 default ''
      !OPCIONES type STRING default ''
    returning
      value(RANGO) type RSTT_T_RANGE_STRING .
  class-methods ADD_LISTA
    importing
      !SEPARADOR type ANY default ','
      !VALOR type ANY
      !SUBVALOR type ANY default ''
      !CONDENSE type ABAP_BOOL default 'X'
      !QUITAR_CEROS type ABAP_BOOL default ''
    changing
      !LISTA type ANY .
  class-methods GET_VALORES_LISTA
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
      !RANGOS type ABAP_BOOL default ''
      !AGRUPAR type ABAP_BOOL default ''
      !DESCOMPONER_SUBVALORES type ABAP_BOOL default ''
    returning
      value(I_VALORES) type ZAP_LISTA_T .
  class-methods AGRUPAR_VALORES_LISTA
    importing
      !I_VALORES type ZAP_LISTA_T
    returning
      value(I_AGRUP) type ZAP_LISTA_T .
  class-methods ADD
    importing
      !SEPARADOR type ANY default ','
      !VALOR type ANY
      !QUITAR_CEROS type ABAP_BOOL default ''
    changing
      !LISTA type ANY .
  class-methods TABLA2LISTA
    importing
      !SEPARADOR type ANY default ','
      !TABLA type TABLE optional
      !QUITAR_CEROS type ABAP_BOOL default ''
    preferred parameter TABLA
    returning
      value(LISTA) type STRING .
  class-methods GET_N_VALORES
    importing
      !SEPARADOR type ANY default ','
      !LISTA type ANY
    returning
      value(N) type INT4 .
  class-methods ES_ELEMENTO
    importing
      !LISTA type ANY
      !ELEMENTO type ANY
      !SEPARADOR type ANY default ','
    returning
      value(SI) type ABAP_BOOL .
  class-methods LISTA2INTERVALO
    importing
      !LISTA type ANY
      !SEP_LISTA type ANY default ','
      !ENTEROS type ABAP_BOOL default ''
      !SEP_INT type ANY default '-'
    returning
      value(INTERVALO) type STRING .
  class-methods LISTA2TABLA
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
      !MAYUSCULAS type ABAP_BOOL default ''
      !ELIMINAR_ESPACIOS type ABAP_BOOL default ''
    returning
      value(TABLA) type TABLE_OF_STRINGS .
  class-methods LISTA2TABLA_N10
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
    returning
      value(TABLA) type TT_VBELN .
  class-methods LISTA2RANGO_N10
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
    returning
      value(RANGO) type RANGE_C10_T .
  class-methods LISTA2TABLA_N12
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
    returning
      value(TABLA) type AUFNR_T .
  class-methods COMPARAR_LISTAS
    importing
      !LISTA1 type ANY
      !LISTA2 type ANY
      !LISTAS_RANGO type ABAP_BOOL default ''
    exporting
      !IGUALES type ABAP_BOOL
      !ELEMENTOS_COMUNES type ANY
      !ELEMENTOS_DIFERENTES type ANY
      !NUM_ELEMENTOS_COMUNES type ANY
      !ELEMENTOS_DIFERENTES_LISTA1 type ANY
      !ELEMENTOS_DIFERENTES_LISTA2 type ANY
      !NUM_ELEMENTOS_DIFERENTES_L1 type ANY
      !NUM_ELEMENTOS_DIFERENTES_L2 type ANY .
  class-methods LISTA_COL_TABLA
    importing
      !TABLA type TABLE
      !CAMPO type ANY
      !CAMPO_CHECK type ANY default ''
      !NO_INITIAL type ABAP_BOOL default ''
    returning
      value(LISTA) type STRING .
  class-methods REMOVE
    importing
      !SEPARADOR type ANY default ','
      !VALOR type ANY
      !QUITAR_CEROS type ABAP_BOOL default ''
    changing
      !LISTA type ANY .
  class-methods GET_NUM_ELEMENTOS
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
    returning
      value(NUM_ELEMENTOS) type INT4 .
  class-methods LISTA2TABLA_N20
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
    returning
      value(TABLA) type HUM_EXIDV_T .
  class-methods LISTA2RANGO_N12
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
    returning
      value(RANGO) type LXHME_RANGE_N12_T .
  class-methods GET_POS_LISTA
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
      !VALOR type ANY
    returning
      value(POS) type I .
  class-methods VALOR_ATR
    importing
      !LISTA type ANY
      !ATRIBUTO type ANY
    returning
      value(VALOR) type STRING .
  class-methods LISTA2TABLA_N18
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
    returning
      value(TABLA) type EHS_MATNR_T .
  class-methods LISTA2RANGO_C3
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
    returning
      value(RANGO) type LXHME_RANGE_C3_T .
  class-methods RANGO2LISTA
    importing
      !SEPARADOR type ANY default ','
      !RANGO type TABLE
      !SIGN type CHAR1 default ''
      !QUITAR_CEROS type ABAP_BOOL default ''
    returning
      value(LISTA) type STRING .
  class-methods LISTA2RANGO_N2
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
    returning
      value(RANGO) type LMBP_RANGE_N2_T .
  class-methods LISTA2RANGO_C2
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
      !PERMITIR_VACIAS type ABAP_BOOL default 'X'
      !OPCIONES type STRING default ''
    returning
      value(RANGO) type LXHME_RANGE_C2_T .
  class-methods LISTA2RANGO_C4
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
      !PONER_CEROS type ABAP_BOOL default ''
      !PERMITIR_VACIAS type ABAP_BOOL default 'X'
      !OPCIONES type STRING default ''
    returning
      value(RANGO) type LXHME_RANGE_C4_T .
  class-methods LISTA2RANGO_C1
    importing
      !LISTA type ANY
      !SEPARADOR type ANY default ','
      !PERMITIR_VACIAS type ABAP_BOOL default 'X'
      !OPCIONES type STRING default ''
    returning
      value(RANGO) type LXHME_RANGE_C1_T .
protected section.
private section.
endclass. "ZCL_AP_LISTA definition
class ZCL_AP_LISTA implementation.
METHOD add.
    DATA: i_valores TYPE TABLE OF string,
          l_valor   TYPE string.

    FIELD-SYMBOLS <valores> TYPE string.

    IF valor CS separador AND NOT separador IS INITIAL.
      SPLIT valor AT separador INTO TABLE i_valores.
      LOOP AT i_valores ASSIGNING <valores>.
        add( EXPORTING valor        = <valores>
                       separador    = separador
                       quitar_ceros = quitar_ceros
             CHANGING  lista        = lista ).
      ENDLOOP.
    ELSE.
      SPLIT lista AT separador INTO TABLE i_valores.
      l_valor = valor.
      IF quitar_ceros = 'X'.
        zcl_ap_string=>quitar_ceros_c( CHANGING cadena = l_valor ).
        CONDENSE l_valor NO-GAPS.
      ENDIF.
      COLLECT l_valor INTO i_valores.
      SORT i_valores.
      DELETE ADJACENT DUPLICATES FROM i_valores.

      CLEAR lista.
      LOOP AT i_valores ASSIGNING <valores>.
        IF lista IS INITIAL.
          lista = <valores>.
        ELSE.
          IF separador = ' '.
            CONCATENATE lista <valores> INTO lista SEPARATED BY space.
          ELSE.
            CONCATENATE lista separador <valores> INTO lista.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
METHOD add_lista.
    DATA: i_valores  TYPE TABLE OF zap_lista,
          l_valor    TYPE zap_lista-valor,
          l_subvalor TYPE zap_lista-subvalor,
          l_lista    TYPE string,
          l_valores  TYPE zap_lista,
          l_string   TYPE string.

    FIELD-SYMBOLS <valores> TYPE zap_lista.

    IF valor CS separador AND subvalor IS INITIAL.
      i_valores = get_valores_lista( lista     = valor
                                     separador = separador ).
      LOOP AT i_valores ASSIGNING <valores>.
        add_lista( EXPORTING valor        = <valores>
                             separador    = separador
                             condense     = condense
                             quitar_ceros = quitar_ceros
                   CHANGING  lista        = lista ).
      ENDLOOP.
    ELSE.
      l_valor = valor.
      l_subvalor = subvalor.
      REPLACE ALL OCCURRENCES OF separador IN l_valor WITH ''.
      REPLACE ALL OCCURRENCES OF separador IN l_subvalor WITH ''.
      REPLACE ALL OCCURRENCES OF '(' IN l_subvalor WITH '['.
      REPLACE ALL OCCURRENCES OF ')' IN l_subvalor WITH ']'.
      l_lista = lista.
      IF quitar_ceros = 'X'.
        zcl_ap_string=>quitar_ceros_c( CHANGING cadena = l_valor ).
        zcl_ap_string=>quitar_ceros_c( CHANGING cadena = l_subvalor ).
        zcl_ap_string=>quitar_ceros_c( CHANGING cadena = l_lista ).
      ENDIF.

      i_valores = get_valores_lista( lista     = l_lista
                                     separador = separador ).

      ASSIGN i_valores[ valor = l_valor ] TO <valores>.
      IF sy-subrc = 0.
        IF NOT subvalor IS INITIAL.
          add_lista( EXPORTING valor     = l_subvalor
                               separador = separador
                     CHANGING  lista     = <valores>-subvalor ).
        ENDIF.
      ELSE.
        CLEAR l_valores.
        l_valores-valor    = l_valor.
        l_valores-subvalor = l_subvalor.
        APPEND l_valores TO i_valores.
      ENDIF.

      SORT i_valores.

      CLEAR lista.
      LOOP AT i_valores ASSIGNING <valores>.
        IF <valores> IS INITIAL.
          CONTINUE.
        ENDIF.

        IF NOT <valores>-subvalor IS INITIAL.
          CONCATENATE '(' <valores>-subvalor ')' INTO l_string.
          IF condense = 'X'.
            CONDENSE l_string NO-GAPS.
          ENDIF.
          CONCATENATE <valores>-valor l_string INTO l_string.
        ELSE.
          l_string = <valores>-valor.
        ENDIF.
        IF lista IS INITIAL.
          lista = l_string.
        ELSE.
          CONCATENATE lista separador l_string INTO lista.
          IF condense = 'X'.
            CONDENSE lista NO-GAPS.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
METHOD agrupar_valores_lista.
    DATA: l_valor   TYPE zap_lista,
          lr_string TYPE rstt_s_range_string.

    FIELD-SYMBOLS <agrup> TYPE zap_lista.

    CLEAR i_agrup.
    LOOP AT i_valores INTO l_valor.
      ASSIGN i_agrup[ valor = l_valor-valor ] TO <agrup>.
      IF sy-subrc <> 0.
        APPEND l_valor TO i_agrup.
      ELSE.
        LOOP AT l_valor-rango_subvalor INTO lr_string.
          COLLECT lr_string INTO <agrup>-rango_subvalor.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
METHOD comparar_listas.
    " TODO: parameter ELEMENTOS_DIFERENTES is never cleared or assigned (ABAP cleaner)

    DATA: i_elem1 TYPE table_of_strings,
          i_elem2 TYPE table_of_strings,
          l_elem1 TYPE string,
          l_elem2 TYPE string,
          l_ini4  TYPE c LENGTH 4,
          l_low   TYPE string,
          l_high  TYPE string.

    CLEAR: elementos_comunes,
           elementos_diferentes_lista1,
           elementos_diferentes_lista2,
           iguales,
           num_elementos_comunes,
           num_elementos_diferentes_l1,
           num_elementos_diferentes_l2.

    i_elem1 = lista2tabla( lista1 ).
    i_elem2 = lista2tabla( lista2 ).

    SORT: i_elem1, i_elem2.
    IF i_elem1 = i_elem2.
      iguales = 'X'.
    ENDIF.

    LOOP AT i_elem1 INTO l_elem1.
      READ TABLE i_elem2 INTO l_elem2 WITH KEY table_line = l_elem1.
      IF sy-subrc = 0.
        DELETE i_elem2 INDEX sy-tabix.
        num_elementos_comunes = num_elementos_comunes + 1.
        add_lista( EXPORTING valor = l_elem1
                   CHANGING  lista = elementos_comunes ).
        DELETE i_elem1.
      ENDIF.
    ENDLOOP.

    IF listas_rango = 'X'.
      LOOP AT i_elem1 INTO l_elem1.
        l_ini4 = l_elem1.
        IF l_ini4 = '(BT)'.
          SPLIT l_elem1+4 AT '-' INTO l_low l_high.
          LOOP AT i_elem2 INTO l_elem2.
            IF l_elem2 >= l_low AND l_elem2 <= l_high.
              DELETE i_elem2.
              num_elementos_comunes = num_elementos_comunes + 1.
              add_lista( EXPORTING valor = l_elem2
                         CHANGING  lista = elementos_comunes ).
              DELETE i_elem1.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
      LOOP AT i_elem2 INTO l_elem2.
        l_ini4 = l_elem2.
        IF l_ini4 = '(BT)'.
          SPLIT l_elem2+4 AT '-' INTO l_low l_high.
          LOOP AT i_elem1 INTO l_elem1.
            IF l_elem1 >= l_low AND l_elem1 <= l_high.
              DELETE i_elem1.
              num_elementos_comunes = num_elementos_comunes + 1.
              add_lista( EXPORTING valor = l_elem1
                         CHANGING  lista = elementos_comunes ).
              DELETE i_elem2.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT i_elem1 INTO l_elem1.
      num_elementos_diferentes_l1 = num_elementos_diferentes_l1 + 1.
      add_lista( EXPORTING valor = l_elem1
                 CHANGING  lista = elementos_diferentes_lista1 ).
    ENDLOOP.

    LOOP AT i_elem2 INTO l_elem2.
      num_elementos_diferentes_l2 = num_elementos_diferentes_l2 + 1.
      add_lista( EXPORTING valor = l_elem2
                 CHANGING  lista = elementos_diferentes_lista2 ).
    ENDLOOP.
  ENDMETHOD.
METHOD constructor.
    " TODO: parameter SEPARADOR is never used (ABAP cleaner)

    IF NOT lista IS INITIAL.
      me->lista = lista.
      i_lista = get_valores_lista( lista                  = lista
                                   descomponer_subvalores = 'X' ).
    ENDIF.
  ENDMETHOD.
METHOD es_elemento.
    DATA: i_lista  TYPE TABLE OF string,
          l_string TYPE string.

    CLEAR si.
    SPLIT lista AT separador INTO TABLE i_lista.
    LOOP AT i_lista INTO l_string.
      IF l_string = elemento.
        si = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF si IS NOT INITIAL.
      RETURN.
    ENDIF.
    IF elemento CS separador.
      SPLIT elemento AT separador INTO TABLE DATA(i_elem).
      LOOP AT i_elem ASSIGNING FIELD-SYMBOL(<elem>).
        LOOP AT i_lista INTO l_string.
          IF l_string = <elem>.
            si = 'X'.
            RETURN.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
METHOD get_n_valores.
    DATA i_lista TYPE TABLE OF string.

    SPLIT lista AT separador INTO TABLE i_lista.
    n = LINES( i_lista ).
  ENDMETHOD.
METHOD get_num_elementos.
    DATA i_lista TYPE TABLE OF string.

    SPLIT lista AT separador INTO TABLE i_lista.
    num_elementos = LINES( i_lista ).
  ENDMETHOD.
METHOD get_pos_lista.
    DATA i_valores TYPE TABLE OF string.

    IF lista CS valor.
      SPLIT lista AT separador INTO TABLE i_valores.
      READ TABLE i_valores WITH KEY table_line = valor TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        pos = sy-tabix.
      ENDIF.
    ENDIF.
  ENDMETHOD.
METHOD get_valores_lista.
    DATA: l_char         TYPE c LENGTH 65535,
          l_ini_subvalor TYPE c LENGTH 1,
          l_valores      TYPE zap_lista,
          l_valor        TYPE string,
          l_subvalor     TYPE string,
          i_lista        TYPE TABLE OF string,
          l_string       TYPE string,
          l_val2         TYPE zap_lista,
          i_val2         TYPE zap_lista_t.

    FIELD-SYMBOLS <valores> TYPE zap_lista.

    IF lista CS '('.
      l_char = lista.
      WHILE l_char <> ''.
        CASE l_char(1).
          WHEN '('.
            l_ini_subvalor = 'X'.
          WHEN ')'.
            CLEAR l_ini_subvalor.
          WHEN separador.
            IF l_ini_subvalor IS INITIAL.
              CLEAR l_valores.
              l_valores-valor    = l_valor.
              l_valores-subvalor = l_subvalor.
              APPEND l_valores TO i_valores.
              CLEAR: l_valor,
                     l_subvalor,
                     l_ini_subvalor.
            ELSE.
              CONCATENATE l_subvalor l_char(1) INTO l_subvalor.
            ENDIF.

          WHEN OTHERS.
            IF l_ini_subvalor = 'X'.
              CONCATENATE l_subvalor l_char(1) INTO l_subvalor RESPECTING BLANKS.
            ELSE.
              CONCATENATE l_valor l_char(1) INTO l_valor RESPECTING BLANKS.
            ENDIF.
        ENDCASE.
        l_char = l_char+1.
      ENDWHILE.
      IF l_valor <> '' OR l_subvalor <> ''.
        CLEAR l_valores.
        l_valores-valor    = l_valor.
        l_valores-subvalor = l_subvalor.
        APPEND l_valores TO i_valores.
      ENDIF.
    ELSE.
      SPLIT lista AT separador INTO TABLE i_lista.
      LOOP AT i_lista INTO l_string.
        CLEAR l_valores.
        l_valores-valor = l_string.
        APPEND l_valores TO i_valores.
      ENDLOOP.
    ENDIF.

    IF descomponer_subvalores = 'X'.
      LOOP AT i_valores INTO l_valores WHERE subvalor CS separador.
        SPLIT l_valores-subvalor AT separador INTO TABLE i_lista.
        LOOP AT i_lista INTO l_string.
          l_val2 = l_valores.
          l_val2-subvalor = l_string.
          APPEND l_val2 TO i_val2.
        ENDLOOP.
        DELETE i_valores.
      ENDLOOP.

      LOOP AT i_val2 INTO l_valores.
        APPEND l_valores TO i_valores.
      ENDLOOP.
      SORT i_valores.
    ENDIF.

    IF rangos = 'X'.
      LOOP AT i_valores ASSIGNING <valores>.
        <valores>-rango_valor    = lista2rango( <valores>-valor ).
        <valores>-rango_subvalor = lista2rango( lista     = <valores>-subvalor
                                                separador = separador ).
      ENDLOOP.
    ENDIF.

    IF agrupar = 'X'.
      i_valores = agrupar_valores_lista( i_valores ).
    ENDIF.
  ENDMETHOD.
METHOD lista2intervalo.
    DATA: i_valores         TYPE TABLE OF string,
          l_fin             TYPE c LENGTH 1,
          l_primero         TYPE string,
          l_ultimo          TYPE string,
          l_entero          TYPE i,
          l_entero_anterior TYPE i,
          l_anterior        TYPE string,
          l_valor           TYPE string.

    FIELD-SYMBOLS <valores> TYPE string.

    SPLIT lista AT sep_lista INTO TABLE i_valores.
    DESCRIBE TABLE i_valores LINES sy-tfill.
    IF sy-tfill = 1.
      intervalo = lista.
    ELSE.
      SORT i_valores.
      LOOP AT i_valores ASSIGNING <valores>.

        AT LAST.
          l_fin = 'X'.
        ENDAT.

        IF l_primero IS INITIAL.
          l_ultimo = <valores>.
          l_primero = <valores>.
        ELSE.
          IF enteros = 'X'.
            IF zcl_ap_string=>es_numero( <valores> ) = 'X'.
              l_entero = <valores>.
              l_entero_anterior = l_entero - 1.
              IF l_entero_anterior = l_anterior.
                l_ultimo = <valores>.
                IF l_fin = 'X'.
                  CONCATENATE l_primero sep_int l_ultimo INTO l_valor.
                  add( EXPORTING valor = l_valor
                       CHANGING  lista = intervalo ).
                ENDIF.
              ELSE.
                IF l_ultimo IS INITIAL.
                  l_valor = l_anterior.
                ELSE.
                  CONCATENATE l_primero sep_int l_ultimo INTO l_valor.
                ENDIF.
                add( EXPORTING valor = l_valor
                     CHANGING  lista = intervalo ).
                l_anterior = <valores>.
                l_primero = <valores>.
                CLEAR l_ultimo.

                IF l_fin = 'X'.
                  add( EXPORTING valor = <valores>
                       CHANGING  lista = intervalo ).
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        l_anterior = <valores>.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
METHOD lista2rango.
    DATA: i_lista  TYPE TABLE OF string,
          l_string TYPE string,
          l_c      TYPE char1,
          l_rango  TYPE rstt_s_range_string,
          l_ini    TYPE char1.

    DATA(l_long) = strlen( separador ).
    SPLIT lista AT separador(l_long) INTO TABLE i_lista.

    LOOP AT i_lista INTO l_string.
      l_c = l_string.
      CLEAR l_rango.
      IF option = 'E*'.
        IF l_string CS '*'.
          l_rango-option = 'CP'.
        ELSE.
          l_rango-option = 'EQ'.
        ENDIF.
      ELSE.
        l_rango-option = option.
      ENDIF.
      l_rango-sign = sign.

      IF ini_exc <> ''.
        l_ini = l_string.
        IF l_ini = ini_exc.
          l_rango-sign = 'E'.
          l_string = l_string+1.
        ENDIF.
      ENDIF.

      IF option = 'CP'.
        CONCATENATE '*' l_string '*' INTO l_rango-low.
      ELSEIF option = 'LK'.
        l_rango-option = 'CP'.
        CONCATENATE l_string '*' INTO l_rango-low.
      ELSE.
        l_rango-low = l_string.

        IF opciones CS '*' AND l_string CS '*'.
          l_rango-option = 'CP'.
        ENDIF.
        IF opciones CS '-' AND l_c = '-'.
          l_rango-sign = 'E'.
          l_rango-low  = l_rango-low+1.
        ENDIF.

      ENDIF.
      APPEND l_rango TO rango.
    ENDLOOP.
    IF sy-subrc <> 0.
      IF permitir_vacias IS INITIAL.
        CLEAR l_rango.
        l_rango-option = 'EQ'.
        l_rango-sign   = 'I'.
        l_rango-low    = '¿¿¿¿????'.
        APPEND l_rango TO rango.
      ENDIF.
    ENDIF.
  ENDMETHOD.
METHOD lista2rango_c1.

  DATA(r_rango_string) = zcl_ap_lista=>lista2rango( lista           = lista
                                                    separador       = separador
                                                    opciones        = opciones
                                                    permitir_vacias = permitir_vacias ).
  MOVE-CORRESPONDING r_rango_string TO rango.

ENDMETHOD.
METHOD lista2rango_c2.

  DATA(r_rango_string) = zcl_ap_lista=>lista2rango( lista           = lista
                                                    separador       = separador
                                                    opciones        = opciones
                                                    permitir_vacias = permitir_vacias ).
  MOVE-CORRESPONDING r_rango_string TO rango.

ENDMETHOD.
METHOD lista2rango_c3.
    DATA: i_c3  TYPE TABLE OF string,
          lr_c3 TYPE lxhme_range_c3,
          l_c3  TYPE string.

    i_c3 = lista2tabla( lista     = lista
                        separador = separador ).

    CLEAR lr_c3.
    lr_c3-option = 'EQ'.
    lr_c3-sign   = 'I'.
    LOOP AT i_c3 INTO l_c3.
      IF l_c3 CS '*'.
        lr_c3-option = 'CP'.
      ELSE.
        lr_c3-option = 'EQ'.
      ENDIF.
      lr_c3-low = l_c3.
      COLLECT lr_c3 INTO rango.
    ENDLOOP.
  ENDMETHOD.
METHOD lista2rango_c4.
  DATA(r_rango_string) = zcl_ap_lista=>lista2rango( lista           = lista
                                                    separador       = separador
                                                    opciones        = opciones
                                                    permitir_vacias = permitir_vacias ).
  MOVE-CORRESPONDING r_rango_string TO rango.
ENDMETHOD.
METHOD lista2rango_n10.
    DATA: i_n10  TYPE TABLE OF vbeln_va,
          lr_n10 TYPE range_c10,
          l_n10  TYPE vbeln_va.

    i_n10 = lista2tabla_n10( lista     = lista
                             separador = separador ).

    CLEAR lr_n10.
    lr_n10-option = 'EQ'.
    lr_n10-sign   = 'I'.
    LOOP AT i_n10 INTO l_n10.
      lr_n10-low = l_n10.
      COLLECT lr_n10 INTO rango.
    ENDLOOP.
  ENDMETHOD.
METHOD lista2rango_n12.
    DATA: i_n12  TYPE TABLE OF aufnr,
          lr_n12 TYPE lxhme_range_n12,
          l_n12  TYPE aufnr.

    i_n12 = lista2tabla_n12( lista     = lista
                             separador = separador ).

    CLEAR lr_n12.
    lr_n12-option = 'EQ'.
    lr_n12-sign   = 'I'.
    LOOP AT i_n12 INTO l_n12.
      lr_n12-low = l_n12.
      COLLECT lr_n12 INTO rango.
    ENDLOOP.
  ENDMETHOD.
METHOD lista2rango_n2.
    DATA: i_n2  TYPE TABLE OF string,
          lr_n2 TYPE lxhme_range_n2,
          l_n2  TYPE string.

    i_n2 = lista2tabla( lista     = lista
                        separador = separador ).

    CLEAR lr_n2.
    lr_n2-option = 'EQ'.
    lr_n2-sign   = 'I'.
    LOOP AT i_n2 INTO l_n2.
      IF l_n2 CS '*'.
        lr_n2-option = 'CP'.
      ELSE.
        lr_n2-option = 'EQ'.
      ENDIF.
      lr_n2-low = l_n2.
      __poner_ceros: lr_n2-low, lr_n2-high.
      COLLECT lr_n2 INTO rango.
    ENDLOOP.
  ENDMETHOD.
METHOD lista2tabla.
    FIELD-SYMBOLS <string> TYPE string.

    SPLIT lista AT separador INTO TABLE tabla.

    IF NOT ( mayusculas = 'X' OR eliminar_espacios = 'X' ).
      RETURN.
    ENDIF.

    LOOP AT tabla ASSIGNING <string>.
      IF eliminar_espacios = 'X'.
        CONDENSE <string> NO-GAPS.
      ENDIF.
      IF mayusculas = 'X'.
        <string> = to_upper( <string> ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
METHOD lista2tabla_n10.
    DATA: i_strings TYPE table_of_strings,
          l_vbeln   TYPE vbeln_va.

    FIELD-SYMBOLS <string> TYPE string.

    CLEAR tabla.

    i_strings = lista2tabla( lista             = lista
                             separador         = separador
                             mayusculas        = 'X'
                             eliminar_espacios = 'X' ).

    LOOP AT i_strings ASSIGNING <string>.
      l_vbeln = <string>.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_vbeln ).
      APPEND l_vbeln TO tabla.
    ENDLOOP.
  ENDMETHOD.
METHOD lista2tabla_n12.
    DATA: i_strings TYPE table_of_strings,
          l_aufnr   TYPE aufnr.

    FIELD-SYMBOLS <string> TYPE string.

    CLEAR tabla.

    i_strings = lista2tabla( lista             = lista
                             separador         = separador
                             mayusculas        = 'X'
                             eliminar_espacios = 'X' ).

    LOOP AT i_strings ASSIGNING <string>.
      l_aufnr = <string>.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_aufnr ).
      APPEND l_aufnr TO tabla.
    ENDLOOP.
  ENDMETHOD.
METHOD lista2tabla_n18.
    DATA: i_strings TYPE table_of_strings,
          l_matnr   TYPE matnr.

    FIELD-SYMBOLS <string> TYPE string.

    CLEAR tabla.

    i_strings = lista2tabla( lista             = lista
                             separador         = separador
                             mayusculas        = 'X'
                             eliminar_espacios = 'X' ).

    LOOP AT i_strings ASSIGNING <string>.
      l_matnr = <string>.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_matnr ).
      APPEND l_matnr TO tabla.
    ENDLOOP.
  ENDMETHOD.
METHOD lista2tabla_n20.
    DATA: i_strings TYPE table_of_strings,
          l_exidv   TYPE exidv.

    FIELD-SYMBOLS <string> TYPE string.

    CLEAR tabla.

    i_strings = lista2tabla( lista             = lista
                             separador         = separador
                             mayusculas        = 'X'
                             eliminar_espacios = 'X' ).

    LOOP AT i_strings ASSIGNING <string>.
      l_exidv = <string>.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_exidv ).
      APPEND l_exidv TO tabla.
    ENDLOOP.
  ENDMETHOD.
METHOD lista_col_tabla.
    FIELD-SYMBOLS: <linea> TYPE any,
                   <valor> TYPE any.

    LOOP AT tabla ASSIGNING <linea>.
      IF NOT campo_check IS INITIAL.
        ASSIGN COMPONENT campo_check OF STRUCTURE <linea> TO <valor>.
        IF <valor> IS INITIAL.
          CONTINUE.
        ENDIF.
      ENDIF.
      ASSIGN COMPONENT campo OF STRUCTURE <linea> TO <valor>.
      IF sy-subrc = 0.
        IF no_initial = 'X' AND <valor> IS INITIAL.
          CONTINUE.
        ELSE.
          add_lista( EXPORTING valor = <valor>
                     CHANGING  lista = lista ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
METHOD rango2lista.
    DATA: l_low    TYPE string,
          l_high   TYPE string,
          elemento TYPE string.

    FIELD-SYMBOLS: <option> TYPE any,
                   <sign>   TYPE any,
                   <low>    TYPE any,
                   <high>   TYPE any.

    CLEAR lista.

    LOOP AT rango ASSIGNING FIELD-SYMBOL(<rango>).
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <rango> TO <option>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'SIGN' OF STRUCTURE <rango> TO <sign>.
        IF sy-subrc = 0.
          IF sign <> '' AND <sign> <> sign.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT 'LOW' OF STRUCTURE <rango> TO <low>.
          ASSIGN COMPONENT 'HIGH' OF STRUCTURE <rango> TO <high>.

          l_low = <low>.
          l_high = <high>.
          IF quitar_ceros = 'X'.
            __quitar_ceros: l_low, l_high.
            CONDENSE: l_low, l_high.
            IF <high> IS INITIAL.
              CLEAR l_high.
            ENDIF.
          ENDIF.
          IF l_low = l_high OR l_high IS INITIAL OR <option> = 'EQ'.
            IF l_low = ''.
              elemento = ''''''.
            ELSE.
              elemento = l_low.
            ENDIF.
          ELSE.
            CONCATENATE l_low '-' l_high INTO elemento.
          ENDIF.

          IF <sign> = 'E'.
            CONCATENATE '[E]' elemento INTO elemento.
          ENDIF.
          IF <option> <> 'EQ'.
            CONCATENATE '(' <option> ')' elemento INTO elemento.
          ENDIF.
        ENDIF.
      ENDIF.
      add( EXPORTING valor = elemento
           CHANGING  lista = lista ).
    ENDLOOP.
  ENDMETHOD.
METHOD remove.
    DATA: i_valores TYPE TABLE OF string,
          l_valor   TYPE string.

    FIELD-SYMBOLS <valores> TYPE string.

    DATA(l_long) = strlen( separador ).
    IF valor CS separador.
      SPLIT valor AT separador(l_long) INTO TABLE DATA(i_quitar).
      LOOP AT i_quitar ASSIGNING FIELD-SYMBOL(<quitar>).
        remove( EXPORTING separador    = separador
                          valor        = <quitar>
                          quitar_ceros = quitar_ceros
                CHANGING  lista        = lista ).
      ENDLOOP.
    ELSE.
      SPLIT lista AT separador(l_long) INTO TABLE i_valores.
      l_valor = valor.
      IF quitar_ceros = 'X'.
        zcl_ap_string=>quitar_ceros_c( CHANGING cadena = l_valor ).
        CONDENSE l_valor NO-GAPS.
      ENDIF.

      CLEAR lista.
      LOOP AT i_valores ASSIGNING <valores>.
        IF <valores> <> l_valor.
          IF lista IS INITIAL.
            lista = <valores>.
          ELSE.
            IF separador = ' '.
              CONCATENATE lista <valores> INTO lista SEPARATED BY space.
            ELSE.
              CONCATENATE lista separador <valores> INTO lista.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
METHOD tabla2lista.
    DATA: l_valor   TYPE string,
          i_valores TYPE TABLE OF string.

    FIELD-SYMBOLS <valores> TYPE string.

    LOOP AT tabla INTO l_valor.
      IF quitar_ceros = 'X'.
        zcl_ap_string=>quitar_ceros_c( CHANGING cadena = l_valor ).
        CONDENSE l_valor NO-GAPS.
      ENDIF.
      COLLECT l_valor INTO i_valores.
    ENDLOOP.
    SORT i_valores.
    DELETE ADJACENT DUPLICATES FROM i_valores.

    CLEAR lista.
    LOOP AT i_valores ASSIGNING <valores>.
      IF lista IS INITIAL.
        lista = <valores>.
      ELSE.
        CONCATENATE lista separador <valores> INTO lista.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
METHOD valor_atr.
    DATA: l_atr  TYPE string,
          l_aux1 TYPE string,
          l_aux2 TYPE string.

    CONCATENATE atributo '=' INTO l_atr.
    IF lista CS l_atr.
      SPLIT lista AT l_atr INTO l_aux1 l_aux2.
    ELSEIF lista CS atributo.
      SPLIT lista AT atributo INTO l_aux1 l_aux2.
    ELSE.
      valor = c_no_valor.
      EXIT.
    ENDIF.

    IF NOT l_aux2 IS INITIAL.
      SPLIT l_aux2 AT ',' INTO l_aux1 l_aux2.
      valor = l_aux1.
    ENDIF.
  ENDMETHOD.