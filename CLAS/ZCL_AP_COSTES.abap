
DEFINE add_rango_eq.
  data: lr_&1 type range of &1,
        l_&1 like line of lr_&1.

  if not &1 is initial.

    if &1 cs ','.
      lr_string = zcl_ap_string=>lista2rango( &1 ).
      loop at lr_string into l_string.
        move-corresponding l_string to l_&1.
        append l_&1 to lr_&1.
      endloop.
    else.
      clear l_&1.
      if &1 cs '*'.
        l_&1-option = 'CP'.
      else.
        l_&1-option = 'EQ'.
      endif.
      l_&1-sign   = 'I'.
      l_&1-low    = &1.
      append l_&1 to lr_&1.
    endif.
  endif.
END-OF-DEFINITION.
class ZCL_AP_COSTES definition
  public
  create public .

public section.

  types:
    begin of T_VALORES,
           mes(2)    type n,
           valor     type WTGXXX,
           valor_soc type WKGXXX,
           cantidad  type MEGXXX,
         end of t_valores .
  types:
    tt_valores type table of t_valores .

  data I_COSP type TY_COSP .
  constants C_REAL type WRTTP value '04' ##NO_TEXT.
  constants C_PLAN type WRTTP value '01' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TIPO type C optional
      !KSTAR type KSTAR optional
      !PAROB type PAROB optional
      !BEKNZ type BEKNZ optional
      !HRKFT type HRKFT optional
      !WRTTP type WRTTP optional
      !GJAHR type GJAHR optional
      !R_KSTAR type TAB_RANGE_C10 optional .
  class-methods GET_COSTES
    importing
      !TIPO type C default ''
      !KSTAR type KSTAR optional
      !PAROB type PAROB optional
      !BEKNZ type BEKNZ optional
      !HRKFT type HRKFT optional
      !WRTTP type WRTTP optional
      !GJAHR type GJAHR optional
      !R_KSTAR type TAB_RANGE_C10 optional
    returning
      value(I_COSP) type TY_COSP .
  class-methods GET_VALOR_COSTE
    importing
      !TIPO type C default ''
      !KSTAR type KSTAR optional
      !PAROB type PAROB optional
      !BEKNZ type BEKNZ optional
      !HRKFT type HRKFT optional
      !WRTTP type WRTTP optional
      !CANTIDAD type ABAP_BOOL default ''
      !GJAHR type GJAHR optional
      !R_MESES type LMBP_RANGE_N2_T optional
      !R_KSTAR type TAB_RANGE_C10 optional
    returning
      value(COSTE) type COSP-WTG001 .
  class-methods GET_VALOR_COSP
    importing
      !COSP type COSP
      !CANTIDAD type ABAP_BOOL default ''
      !R_MESES type LMBP_RANGE_N2_T optional
    returning
      value(COSTE) type COSP-WTG001 .
  class-methods F4_GRUPO
    changing
      !AUFGR type AUFGR .
  class-methods GET_ORDENES
    importing
      !AUFGR type AUFGR
    returning
      value(R_AUFNR) type RANGE_T_AUFNR .
  class-methods GET_JERARQUIA
    importing
      !AUFGR type ANY optional
      !KSTAR type ANY optional
    exporting
      !I_SETHIER type ZT_SETHIER
      !I_SETVALUES type ZT_SETVALUES .
  class-methods F4_GRUPO_KSTAR
    changing
      !KAGRU type KAGRU .
  class-methods GET_CECOS
    importing
      !KSGRU type KSGRU
      !KOKRS type KOKRS
    returning
      value(R_KOSTL) type CCHRY_KOSTL_RANGE .
  class-methods F4_GRUPO_KOSTL
    importing
      !KOKRS type KOKRS
    changing
      !KSGRU type KSGRU .
  class-methods GET_VALOR_COSS
    importing
      !COSS type COSS
      !CANTIDAD type ABAP_BOOL default ''
      !R_MESES type LMBP_RANGE_N2_T optional
      !MES type MONTH optional
    returning
      value(COSTE) type COSS-WTG001 .
  class-methods GET_VALOR_COSR
    importing
      !COSR type COSR
      !R_MESES type LMBP_RANGE_N2_T optional
      !MES type MONTH optional
    returning
      value(COSTE) type COSS-WTG001 .
  class-methods GET_VALORES_COSS
    importing
      !COSS type COSS
      !R_MESES type LMBP_RANGE_N2_T optional
      !MES type MONTH optional
    exporting
      !VALORES type TT_VALORES .
  class-methods GET_RANGO_KSTAR
    importing
      !KAGRU type KAGRU
    returning
      value(R_KSTAR) type TAB_RANGE_C10 .
protected section.
private section.
endclass. "ZCL_AP_COSTES definition
class ZCL_AP_COSTES implementation.
method CONSTRUCTOR.

  i_cosp = get_costes( tipo  = tipo
                       gjahr = gjahr
                       kstar = kstar
                       parob = parob
                       beknz = beknz
                       hrkft = hrkft
                       wrttp = wrttp
                       r_kstar = r_kstar ).

endmethod.
method F4_GRUPO.
  DATA: l_setid TYPE rgsbs-setnr.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      class         = '0103'
      field_name    = 'AUFNR                         '
      table         = 'CCSS                          '
    IMPORTING
      setid         = l_setid
      set_name      = aufgr
    EXCEPTIONS
      no_set_picked = 02.

endmethod.
method F4_GRUPO_KOSTL.
  DATA: l_setid TYPE rgsbs-setnr.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      class         = '0101'
      field_name    = 'KOSTL                         '
      table         = 'CCSS                          '
      kokrs         = kokrs
    IMPORTING
      setid         = l_setid
      set_name      = ksgru
    EXCEPTIONS
      no_set_picked = 02.

endmethod.
method F4_GRUPO_KSTAR.
  DATA: l_setid TYPE rgsbs-setnr.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      field_name    = 'KSTAR'
      SEARCHFLD     = 'PCAT'
    IMPORTING
      setid         = l_setid
      set_name      = kagru
    EXCEPTIONS
      no_set_picked = 02.

endmethod.
method GET_CECOS.
  DATA: l_csks TYPE csks,
        i_set_hierarchy TYPE TABLE OF sethier,
        i_set_values TYPE TABLE OF setvalues,
        l_setid TYPE rgsbs-setnr,
        i_csks TYPE TABLE OF csks,
        lr_kostl TYPE CCHRS_KOSTL_RANGE.

  FIELD-SYMBOLS: <setvalues> TYPE setvalues,
                 <csks> TYPE csks.

  CONCATENATE '0101' KOKRS KSGRU INTO l_setid.
  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      fieldname                       = 'KOSTL'
      setid                           = l_setid
      tabname                         = 'CCSS'
*     NO_VARIABLE_REPLACEMENT         = ' '
*     ROOT_HEADER_ONLY                = ' '
*     NO_TABLE_BUFFERING              = ' '
*     MAX_HIER_LEVEL                  = 99
*     DATE_FROM                       =
*     DATE_TO                         =
*   IMPORTING
*     SET_NOT_TRANSPARENT             =
    TABLES
      set_hierarchy                   = i_set_hierarchy
      set_values                      = i_set_values
    EXCEPTIONS
      set_not_found                   = 1
      illegal_field_replacement       = 2
      illegal_table_replacement       = 3
      OTHERS                          = 4.

  LOOP AT i_set_values ASSIGNING <setvalues>.
    IF <setvalues>-to IS INITIAL.
      SELECT * FROM csks
       APPENDING CORRESPONDING FIELDS OF TABLE i_csks
       WHERE KOKRS = kokrs
         and kostl = <setvalues>-from
         and datbi >= sy-datum
         and datab <= sy-datum.
    ELSE.
      SELECT * FROM csks
       APPENDING CORRESPONDING FIELDS OF TABLE i_csks
       WHERE KOKRS = kokrs
         and kostl >= <setvalues>-from
         AND kostl <= <setvalues>-to
         and datbi >= sy-datum
         and datab <= sy-datum.
    ENDIF.
  ENDLOOP.

  LOOP AT i_csks ASSIGNING <csks>.
    CLEAR lr_kostl.
    lr_kostl-option = 'EQ'.
    lr_kostl-sign   = 'I'.
    lr_kostl-low    = <csks>-kostl.
    COLLECT lr_kostl INTO r_kostl.
  ENDLOOP.

endmethod.
method GET_COSTES.
  DATA: l_cosp TYPE cosp,
        lr_string TYPE RANGE OF string,
        l_string LIKE LINE OF lr_string.

  add_rango_eq: kstar,
                hrkft,
                parob,
                beknz,
                gjahr,
                wrttp.

  IF NOT r_kstar IS INITIAL.
    lr_kstar = r_kstar.
  ENDIF.

  IF tipo = 'P' OR tipo = ''.
    SELECT * FROM  cosp
      INTO TABLE i_cosp
     WHERE lednr  = '00'
*       AND objnr  = l_objnr
       AND gjahr IN lr_gjahr
       AND wrttp IN lr_wrttp
       AND versn  = '000'
       AND kstar IN lr_kstar
       AND hrkft IN lr_hrkft
       AND beknz IN lr_beknz.
  ELSE.
    SELECT * FROM  coss
      INTO CORRESPONDING FIELDS OF TABLE i_cosp
     WHERE lednr  = '00'
*       AND objnr  = l_objnr
       AND gjahr IN lr_gjahr
       AND wrttp IN lr_wrttp
       AND versn  = '000'
       AND kstar IN lr_kstar
       AND parob IN lr_parob
       AND beknz IN lr_beknz.
  ENDIF.


endmethod.
method GET_JERARQUIA.
  DATA: l_aufk TYPE aufk,
        l_setid TYPE rgsbs-setnr.

  IF NOT aufgr IS INITIAL.
    CONCATENATE '0103' aufgr INTO l_setid.
  ELSEIF NOT kstar IS INITIAL.
    CONCATENATE '0102PCAT' kstar INTO l_setid.
  ENDIF.
  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      fieldname                       = 'AUFNR'
      setid                           = l_setid
      tabname                         = 'CCSS'
*     NO_VARIABLE_REPLACEMENT         = ' '
*     ROOT_HEADER_ONLY                = ' '
*     NO_TABLE_BUFFERING              = ' '
*     MAX_HIER_LEVEL                  = 99
*     DATE_FROM                       =
*     DATE_TO                         =
*   IMPORTING
*     SET_NOT_TRANSPARENT             =
    TABLES
      set_hierarchy                   = i_sethier
      set_values                      = i_setvalues
    EXCEPTIONS
      set_not_found                   = 1
      illegal_field_replacement       = 2
      illegal_table_replacement       = 3
      OTHERS                          = 4.

endmethod.
METHOD get_ordenes.
  DATA: l_aufk          TYPE aufk,
        i_set_hierarchy TYPE TABLE OF sethier,
        i_set_values    TYPE TABLE OF setvalues,
        l_setid         TYPE rgsbs-setnr,
        i_aufk          TYPE TABLE OF aufk,
        lr_aufnr        TYPE range_s_aufnr.

  FIELD-SYMBOLS: <setvalues> TYPE setvalues,
                 <aufk>      TYPE aufk.

  CONCATENATE '0103' aufgr INTO l_setid.
  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      fieldname                 = 'AUFNR'
      setid                     = l_setid
      tabname                   = 'CCSS'
*     NO_VARIABLE_REPLACEMENT   = ' '
*     ROOT_HEADER_ONLY          = ' '
*     NO_TABLE_BUFFERING        = ' '
*     MAX_HIER_LEVEL            = 99
*     DATE_FROM                 =
*     DATE_TO                   =
*   IMPORTING
*     SET_NOT_TRANSPARENT       =
    TABLES
      set_hierarchy             = i_set_hierarchy
      set_values                = i_set_values
    EXCEPTIONS
      set_not_found             = 1
      illegal_field_replacement = 2
      illegal_table_replacement = 3
      OTHERS                    = 4.

  LOOP AT i_set_values ASSIGNING <setvalues>.
    IF <setvalues>-to IS INITIAL.
      SELECT aufnr FROM aufk
       APPENDING CORRESPONDING FIELDS OF TABLE i_aufk
       WHERE aufnr = <setvalues>-from.
    ELSE.
      SELECT aufnr FROM aufk
       APPENDING CORRESPONDING FIELDS OF TABLE i_aufk
       WHERE aufnr >= <setvalues>-from
         AND aufnr <= <setvalues>-to.
    ENDIF.
  ENDLOOP.

  LOOP AT i_aufk ASSIGNING <aufk>.
    CLEAR lr_aufnr.
    lr_aufnr-option = 'EQ'.
    lr_aufnr-sign   = 'I'.
    lr_aufnr-low    = <aufk>-aufnr.
    COLLECT lr_aufnr INTO r_aufnr.
  ENDLOOP.

ENDMETHOD.
METHOD get_rango_kstar.
    DATA lr_kstar LIKE LINE OF r_kstar.

    CLEAR r_kstar.

    CLEAR lr_kstar.
    lr_kstar-sign   = 'I'.
    lr_kstar-option = 'EQ'.

    get_jerarquia( EXPORTING kstar = kagru
                   IMPORTING i_sethier = DATA(i_sethier)
                             i_setvalues = DATA(i_setvalues) ).

    LOOP AT i_setvalues ASSIGNING FIELD-SYMBOL(<values>).
      IF <values>-from IS INITIAL.
        lr_kstar-option = 'EQ'.
        lr_kstar-low = <values>-from.
      ELSE.
        lr_kstar-option = 'BT'.
        lr_kstar-low = <values>-from.
        lr_kstar-high = <values>-to.
      ENDIF.
      COLLECT lr_kstar INTO r_kstar.
    ENDLOOP.
    IF sy-subrc NE 0.
      lr_kstar-low = '?'.
      COLLECT lr_kstar INTO r_kstar.
    ENDIF.

  ENDMETHOD.
method GET_VALOR_COSP.
  DATA: l_wtg TYPE cosp-wtg001,
        l_meg TYPE cosp-meg001,
        l_mes(2) TYPE n,
        l_cosp TYPE cosp.

  l_cosp = cosp.

  IF cantidad IS INITIAL.
    DO VARYING l_wtg FROM l_cosp-wkg001 NEXT l_cosp-wkg002.
      l_mes = sy-index.

      IF l_mes IN r_meses.
        ADD l_wtg TO coste.
      ENDIF.

      IF l_mes = 12.
        EXIT.
      ENDIF.
    ENDDO.
  ELSE.
    DO VARYING l_meg FROM l_cosp-meg001 NEXT l_cosp-meg002.
      l_mes = sy-index.
      IF l_mes IN r_meses.
        ADD l_meg TO coste.
      ENDIF.

      IF l_mes = 12.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

endmethod.
method GET_VALOR_COSR.
  DATA: l_wtg TYPE cosr-sme001,
        l_mes(2) TYPE n,
        l_cosr TYPE cosr.

  l_cosr = cosr.

  DO VARYING l_wtg FROM l_cosr-sme001 NEXT l_cosr-sme002.
    l_mes = sy-index.

    IF ( l_mes IN r_meses AND mes IS INITIAL ) OR l_mes = mes.
      ADD l_wtg TO coste.
    ENDIF.

    IF l_mes = 12.
      EXIT.
    ENDIF.
  ENDDO.


endmethod.
method GET_VALOR_COSS.
  DATA: l_wtg TYPE coss-wtg001,
        l_meg TYPE coss-meg001,
        l_mes(2) TYPE n,
        l_coss TYPE coss.

  l_coss = coss.

  IF cantidad IS INITIAL.
    DO VARYING l_wtg FROM l_coss-wkg001 NEXT l_coss-wkg002.
      l_mes = sy-index.

      IF ( l_mes IN r_meses and mes is initial ) or l_mes = mes.
        ADD l_wtg TO coste.
      ENDIF.

      IF l_mes = 12.
        EXIT.
      ENDIF.
    ENDDO.
  ELSE.
    DO VARYING l_meg FROM l_coss-meg001 NEXT l_coss-meg002.
      l_mes = sy-index.

      IF ( l_mes IN r_meses and mes is initial ) or l_mes = mes.
        ADD l_meg TO coste.
      ENDIF.

      IF l_mes = 12.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

endmethod.
method GET_VALOR_COSTE.
  DATA: i_cosp TYPE TABLE OF cosp,
          l_wtg TYPE cosp-wtg001,
          l_meg TYPE cosp-meg001,
          l_mes(2) TYPE n.

  FIELD-SYMBOLS <cosp> TYPE cosp.

  i_cosp = get_costes( tipo  = tipo
                       gjahr = gjahr
                       kstar = kstar
                       parob = parob
                       beknz = beknz
                       hrkft = hrkft
                       wrttp = wrttp
                       R_KSTAR = R_KSTAR  ).

  LOOP AT i_cosp ASSIGNING <cosp>.
    coste = get_valor_cosp( <cosp> ).
  ENDLOOP.

endmethod.
METHOD get_valores_coss.
  DATA: l_wtg TYPE coss-wtg001,
        l_wkg TYPE coss-wkg001,
        l_meg TYPE coss-meg001,
        l_mes(2) TYPE n,
        l_coss TYPE coss,
        l_valores TYPE t_valores.

  l_coss = coss.

  DO VARYING l_wtg FROM l_coss-wtg001 NEXT l_coss-wtg002
     VARYING l_wkg FROM l_coss-wkg001 NEXT l_coss-wkg002
     VARYING l_meg FROM l_coss-meg001 NEXT l_coss-meg002.
    l_mes = sy-index.

    IF ( l_mes IN r_meses AND mes IS INITIAL ) OR l_mes = mes.
      IF l_wtg NE 0 OR l_wkg NE 0 OR l_meg NE 0.
        CLEAR l_valores.
        l_valores-mes       = l_mes.
        l_valores-valor     = l_wtg.
        l_valores-valor_soc = l_wkg.
        l_valores-cantidad  = l_meg.
        APPEND l_valores TO valores.
      ENDIF.
    ENDIF.

    IF l_mes = 16.
      EXIT.
    ENDIF.
  ENDDO.

ENDMETHOD.
