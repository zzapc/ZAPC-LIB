class ZCL_AP_REGEXP definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF t_busquedas,
        texto  TYPE string,
        offset TYPE int4,
        length TYPE int4,
      END OF t_busquedas .
  types:
    tt_busquedas TYPE STANDARD TABLE OF t_busquedas .

  class-methods BUSCAR_PATRON
    importing
      !STRING type STRING
      !PATRON type ANY
      !IGNORING_CASE type ABAP_BOOL default 'X'
      !TIPO type STRING default ''
    exporting
      !BUSQUEDAS type TT_BUSQUEDAS
    returning
      value(OCURRENCIAS) type TABLE_OF_STRINGS .
  class-methods BUSCAR_FECHAS
    importing
      !STRING type STRING
      !PATRON type STRING default '(\d{1,2}[.|/]\d{1,2}[.|/]\d{4})|(\d{4}[.|/]\d{1,2}[.|/]\d{1,2})'
    returning
      value(OCURRENCIAS) type TABLE_OF_STRINGS .
  class-methods GET_NUM_PATRON
    importing
      !STRING type STRING
      !PATRON type STRING
      !IGNORING_CASE type ABAP_BOOL default ''
    returning
      value(OCURRENCIAS) type INT4 .
  class-methods BUSCAR_PARENTESIS
    importing
      !STRING type STRING
      !PATRON type STRING default '\(([^()]*)\)'
    returning
      value(OCURRENCIAS) type TABLE_OF_STRINGS .
  class-methods BUSCAR_CORCHETES
    importing
      !STRING type STRING
      !PATRON type STRING default '\[([^]]+)\]'
    returning
      value(OCURRENCIAS) type TABLE_OF_STRINGS .
  class-methods GET_STRING_LETRAS_NUMEROS
    importing
      !STRING type STRING
      !PATRON type STRING default '[A-ZA-Z0-9]*'
      !CONDENSE_NO_GAPS type ABAP_BOOL default 'X'
      !TO_UPPER type ABAP_BOOL default 'X'
      !CONDENSE type ABAP_BOOL default ''
    returning
      value(CADENA) type STRING .
  class-methods BUSCAR_HORAS
    importing
      !STRING type STRING
      !PATRON type STRING default '(0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]'
    returning
      value(OCURRENCIAS) type TABLE_OF_STRINGS .
  class-methods BUSCAR_LLAVES
    importing
      !STRING type STRING
      !PATRON type STRING default '\{([^{}]*)\}'
    returning
      value(OCURRENCIAS) type TABLE_OF_STRINGS .
  class-methods buscar_numeros
    importing
      !STRING type STRING
      !PATRON type STRING default '\d+'
    returning
      value(OCURRENCIAS) type TABLE_OF_STRINGS .
  class-methods HTML2TEXT
    importing
      !HTML type STRING
    returning
      value(TEXTO) type STRING .
  class-methods RECUPERAR_VALOR
    importing
      !STRING type STRING
      !ATRIBUTO type STRING
    returning
      value(VALOR) type STRING .
  class-methods ELIMINAR_PATRON
    importing
      !STRING type STRING
      !PATRON type ANY
      !IGNORING_CASE type ABAP_BOOL default 'X'
      !TIPO type STRING default ''
    exporting
      !BUSQUEDAS type TT_BUSQUEDAS
    returning
      value(SALIDA) type STRING .

    CLASS-METHODS convert_posix_to_pcre
      IMPORTING iv_pattern TYPE string
      RETURNING VALUE(rv_pattern) TYPE string.


endclass. "ZCL_AP_REGEXP definition
class ZCL_AP_REGEXP implementation.
  METHOD buscar_corchetes.
    ocurrencias = buscar_patron( string = string patron = patron ).
  ENDMETHOD.
  METHOD buscar_fechas.
    ocurrencias = buscar_patron( string = string patron = patron ).
  ENDMETHOD.
  METHOD buscar_horas.
    ocurrencias = buscar_patron( string = string patron = patron ).
  ENDMETHOD.
  METHOD buscar_llaves.
    ocurrencias = buscar_patron( string = string patron = patron ).
  ENDMETHOD.
  METHOD buscar_numeros.
    ocurrencias = buscar_patron( string = string patron = patron ).
  ENDMETHOD.
  METHOD buscar_parentesis.
    ocurrencias = buscar_patron( string = string patron = patron ).
  ENDMETHOD.
  METHOD buscar_patron.
    DATA: l_patron               TYPE string,
          lt_result_tab          TYPE match_result_tab,
          pattern_ref            TYPE REF TO cl_abap_regex,
          ls_submatch_result_tab TYPE match_result,
          lv_txt                 TYPE string.

    CLEAR: ocurrencias, busquedas.
    l_patron = patron.

    CASE tipo.
      WHEN ''.
        IF ignoring_case IS INITIAL.
          FIND ALL OCCURRENCES OF REGEX l_patron IN string RESULTS lt_result_tab.
        ELSE.
          FIND ALL OCCURRENCES OF REGEX l_patron IN string IGNORING CASE RESULTS lt_result_tab.
        ENDIF.
      WHEN 'PCRE'.
        TRY.
            CALL METHOD ('CL_ABAP_REGEX')=>('CREATE_PCRE')
              EXPORTING
                pattern     = l_patron
                ignore_case = ignoring_case
                extended    = 'X'
              RECEIVING
                regex       = pattern_ref.
          CATCH cx_root INTO DATA(o_root). " TODO: variable is assigned but never used (ABAP cleaner)
        ENDTRY.

        FIND ALL OCCURRENCES OF REGEX pattern_ref IN string RESULTS lt_result_tab.
    ENDCASE.

    LOOP AT lt_result_tab INTO ls_submatch_result_tab.
      lv_txt = string+ls_submatch_result_tab-offset(ls_submatch_result_tab-length).
      APPEND lv_txt TO ocurrencias.
      APPEND VALUE #( texto = lv_txt offset = ls_submatch_result_tab-offset length = ls_submatch_result_tab-length ) TO busquedas.
    ENDLOOP.

* Demo Program DEMO_REGEX_TOY
*    ^ : Matches the starting position within the string
* () : The string matched within the parentheses can be recalled later.
* [] : Matches a character that is contained within the brackets.
* – : Specifies a range.
* {} : Number of characters.
* \d : Digits.
* \. : Mandatory dot.
* ?  : Indicates there is zero or one of the preceding element.
* $ : Matches the ending position of the string.
* [0-9]       : Digits only any number between 0 to 9.
* {0,12}     : Up to 13 characters allowed only before decimal.
* {0,2}        : up to 2 characters allowed only after decimal.

* Ejemplo buscar pedidos 4599999999 ->'45[0-9]{8}'.
  ENDMETHOD.
  METHOD CONVERT_POSIX_TO_PCRE.
    rv_pattern = iv_pattern.

    " Mapas básicos POSIX -> PCRE/Unicode
    REPLACE ALL OCCURRENCES OF '[[:digit:]]' IN rv_pattern WITH '\d'.
    REPLACE ALL OCCURRENCES OF '[[:space:]]' IN rv_pattern WITH '\s'.
    REPLACE ALL OCCURRENCES OF '[[:alpha:]]' IN rv_pattern WITH '[A-Za-z]'.
    REPLACE ALL OCCURRENCES OF '[[:alnum:]]' IN rv_pattern WITH '[A-Za-z0-9]'.
    REPLACE ALL OCCURRENCES OF '[[:lower:]]' IN rv_pattern WITH '[a-z]'.
    REPLACE ALL OCCURRENCES OF '[[:upper:]]' IN rv_pattern WITH '[A-Z]'.
  ENDMETHOD.
  METHOD ELIMINAR_PATRON.

salida = string.
    DATA(i_var) = zcl_ap_regexp=>buscar_patron( string = string patron = patron tipo = TIPO ).

    SORT i_var.
    DELETE ADJACENT DUPLICATES FROM i_var.

* Hay que ordenar por tamaño, porque sino el reeplazar falla.
    TYPES: BEGIN OF t_var_t,
             var  TYPE string,
             long TYPE int4,
           END OF t_var_t.

    DATA i_var_t TYPE TABLE OF t_var_t.
    LOOP AT i_Var ASSIGNING FIELD-SYMBOL(<var>).
      APPEND INITIAL LINE TO i_var_t ASSIGNING FIELD-SYMBOL(<var_t>).
      <var_t>-var = <var>.
      <var_t>-long = strlen( <var> ).
    ENDLOOP.
    SORT i_var_t BY long DESCENDING var.
    LOOP AT i_Var_t ASSIGNING <var_t>.
        REPLACE ALL OCCURRENCES OF <var_t>-var IN salida WITH ''.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_num_patron.
    TRY.
        IF ignoring_case = 'X'.
          FIND ALL OCCURRENCES OF
               REGEX patron IN string
               MATCH COUNT  ocurrencias
               IGNORING CASE.
        ELSE.
          FIND ALL OCCURRENCES OF
               REGEX patron IN string
               MATCH COUNT  ocurrencias.
        ENDIF.
      CATCH cx_root INTO DATA(o_root). " TODO: variable is assigned but never used (ABAP cleaner)
    ENDTRY.
  ENDMETHOD.
  METHOD get_string_letras_numeros.
    CLEAR cadena.
    DATA(l_string) = string.
    IF condense_no_gaps = 'X'.
      CONDENSE l_string NO-GAPS.
    ENDIF.
    IF to_upper = 'X'.
      TRANSLATE l_string TO UPPER CASE.
    ENDIF.

    DATA(ocurrencias) = buscar_patron( string = l_string patron = patron ).

    LOOP AT ocurrencias ASSIGNING FIELD-SYMBOL(<ocur>).
      IF cadena IS INITIAL.
        cadena = <ocur>.
      ELSE.
        CONCATENATE cadena <ocur> INTO cadena SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF condense_no_gaps = 'X'.
      CONDENSE cadena NO-GAPS.
    ELSEIF condense = 'X'.
      CONDENSE cadena.
    ENDIF.
  ENDMETHOD.
  METHOD html2text.
    texto = html.
    DATA(i_tags) = buscar_patron( string = html patron = '<[^>]*>' ).
    LOOP AT i_tags ASSIGNING FIELD-SYMBOL(<tag>).
      REPLACE ALL OCCURRENCES OF <tag> IN texto WITH ''.
    ENDLOOP.
  ENDMETHOD.
  METHOD recuperar_valor.
    CLEAR valor.
    DATA(pattern) = |"{ atributo }":"([^"]*)"|.
    DATA(regex) = NEW cl_abap_regex( pattern = pattern ).

    DATA(matcher) = regex->create_matcher( text = string ).

    IF matcher->find_next( ).
      valor = matcher->get_submatch( 1 ).
    ENDIF.
  ENDMETHOD.
