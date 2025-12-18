types: begin of t_objid,
         sign(1),
         option(2),
         low   type hrp1001-objid,
         high  type hrp1001-objid,
      end of T_objid.
types: r_objid type table of t_objid.
types: begin of t_sobid,
         sign(1),
         option(2),
         low   type hrp1001-sobid,
         high  type hrp1001-sobid,
      end of T_sobid.
types: r_sobid type table of t_sobid.
class ZCL_AP_REL_PD definition
  public
  create public .

public section.

  data T_REL type ZTAB_REL_PD .
  data REL type ZEST_REL_PD .
  constants C_PLVAR type HRP1000-PLVAR value '01' ##NO_TEXT.
  constants C_UNIDAD_ORG type OTYPE value 'O' ##NO_TEXT.
  data R_RANGO type LSO_RANGE_OBJID_TAB .

  class-methods GET_REL_PD
    importing
      !TIPO_ORIGEN type HRP1001-OTYPE
      !R_OBJID type TABLE optional
      !OBJID type HRP1001-OBJID optional
      !TIPO_DESTINO type HRP1001-OTYPE
      !R_SOBID type TABLE optional
      !RSIGN type HRP1001-RSIGN default 'A'
      !RELAT type HRP1001-RELAT optional
      !ISTAT type HRP1001-ISTAT default '1'
      !BEGDA type HRP1001-BEGDA default '00000000'
      !ENDDA type HRP1001-ENDDA default '99991231'
    returning
      value(T_REL) type ZTAB_REL_PD .
  methods SELECT_REL_PD
    importing
      !TIPO_ORIGEN type HRP1001-OTYPE
      !OBJID type HRP1001-OBJID optional
      !R_OBJID type TABLE optional
      !TIPO_DESTINO type HRP1001-OTYPE
      !R_SOBID type TABLE optional
      !RSIGN type HRP1001-RSIGN default 'A'
      !RELAT type HRP1001-RELAT
      !ISTAT type HRP1001-ISTAT default '1'
      !BEGDA type HRP1001-BEGDA default '00000000'
      !ENDDA type HRP1001-ENDDA default '99991231' .
  class-methods GET_OBJ_REL
    importing
      !TIPO_ORIGEN type HRP1001-OTYPE
      !R_OBJID type TABLE optional
      !OBJID type HRP1001-OBJID optional
      !TIPO_DESTINO type HRP1001-OTYPE
      !R_SOBID type TABLE optional
      !RSIGN type HRP1001-RSIGN default 'A'
      !RELAT type HRP1001-RELAT optional
      !ISTAT type HRP1001-ISTAT default '1'
      !BEGDA type HRP1001-BEGDA default '00000000'
      !ENDDA type HRP1001-ENDDA default '99991231'
    returning
      value(SOBID) type HRP1001-SOBID .
  class-methods GET_RANGO_ORGEH
    importing
      !S_ORGEH type CCHRY_ORGEH_RANGE
      !PLVAR type PLVAR default C_PLVAR
    returning
      value(R_ORGEH) type CCHRY_ORGEH_RANGE .
  class-methods BUSCAR_ORG
    importing
      !ORGEH type ORGEH
    returning
      value(I_ORG) type PIQ_HRP1001_TAB .
  class-methods GET_RANGO_OBJID
    importing
      !S_OBJID type LSO_RANGE_OBJID_TAB
      !OTYPE type HRP1001-OTYPE
      !RELAT type HRP1001-RELAT
      !PLVAR type PLVAR default C_PLVAR
    returning
      value(R_OBJID) type LSO_RANGE_OBJID_TAB .
  class-methods BUSCAR_OBJID
    importing
      !OBJID type HRP1001-OBJID
      !OTYPE type HRP1001-OTYPE
      !RELAT type RELAT
    returning
      value(I_ORG) type PIQ_HRP1001_TAB .
  methods MATCHCODE
    returning
      value(OBJID) type HRP1000-OBJID .
  methods GET_RANGO
    returning
      value(R_OBJID) type LSO_RANGE_OBJID_TAB .
  class-methods GET_ARBOL_ORGEH
    importing
      !ORGEH type ORGEH
      !FECHA type DATS default SY-DATUM
    exporting
      !CODIGOS type TSWHACTOR
      !OBJETOS type PIQ_OBJEC_T
      !RELACIONES type STRUC_T .
  class-methods GET_EMPLEADOS_ORGEH
    importing
      !ORGEH type ORGEH
      !FECHA type DATS default SY-DATUM
      !RECURSIVO type ABAP_BOOL default 'X'
    returning
      value(EMPLEADOS) type PA0001_ITAB .
  class-methods GET_EMPLEADOS_POSICION
    importing
      !PLANS type PLANS
      !FECHA type DATS default SY-DATUM
    returning
      value(EMPLEADOS) type PA0001_ITAB .
protected section.
private section.
endclass. "ZCL_AP_REL_PD definition
class ZCL_AP_REL_PD implementation.
method BUSCAR_OBJID.
  DATA: i_rel TYPE ztab_rel_pd,
        l_rel TYPE zest_rel_pd,
        l_org TYPE hrp1001,
        i_org2 TYPE TABLE OF hrp1001,
        l_org2 TYPE hrp1001,
        l_objid TYPE hrp1001-objid.

  CLEAR i_org.
  CALL METHOD get_rel_pd
    EXPORTING
      tipo_origen  = otype
      objid        = objid
      tipo_destino = otype
      rsign        = 'B'
      relat        = relat
      istat        = 'N'
    RECEIVING
      t_rel        = i_rel.

  LOOP AT i_rel INTO l_rel.
    CLEAR l_org.
    MOVE-CORRESPONDING l_rel TO l_org.
    APPEND l_org TO i_org.
    l_objid = l_rel-sobid.
    i_org2 = buscar_objid( objid = l_objid
                           otype = otype
                           relat = relat ).
    LOOP AT i_org2 INTO l_org2.
      l_org-objid = objid.
      l_org-sobid = l_org2-sobid.
      APPEND l_org TO i_org.
    ENDLOOP.
  ENDLOOP.

  SORT i_org.
  DELETE ADJACENT DUPLICATES FROM i_org.

endmethod.
method BUSCAR_ORG.
  DATA: i_rel TYPE ztab_rel_pd,
        l_rel TYPE zest_rel_pd,
        l_org TYPE hrp1001,
        i_org2 TYPE TABLE OF hrp1001,
        l_org2 TYPE hrp1001,
        l_orgeh TYPE orgeh.

  CLEAR i_org.
  CALL METHOD zcl_ap_rel_pd=>get_rel_pd
    EXPORTING
      tipo_origen  = c_unidad_org
      objid        = orgeh
      tipo_destino = c_unidad_org
      rsign        = 'B'
      relat        = '002'
      istat        = 'N'
    RECEIVING
      t_rel        = i_rel.

  LOOP AT i_rel INTO l_rel.
    CLEAR l_org.
    MOVE-CORRESPONDING l_rel TO l_org.
    APPEND l_org TO i_org.
    l_orgeh = l_rel-sobid.
    i_org2 = buscar_org( l_orgeh ).
    LOOP AT i_org2 INTO l_org2.
      l_org-objid = orgeh.
      l_org-sobid = l_org2-sobid.
      APPEND l_org TO i_org.
    ENDLOOP.
  ENDLOOP.

  SORT i_org.
  DELETE ADJACENT DUPLICATES FROM i_org.

endmethod.
METHOD get_arbol_orgeh.

  CLEAR: codigos, objetos, relaciones.

  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype      = 'O'
      act_objid      = orgeh
      act_wegid      = 'ORGEH'
*     ACT_INT_FLAG   = ACT_INT_FLAG
*     ACT_PLVAR      = ' '
      act_begda      = fecha
      act_endda      = fecha
*     ACT_TDEPTH     = 0
*     ACT_TFLAG      = 'X'
*     ACT_VFLAG      = 'X'
*     AUTHORITY_CHECK        = 'X'
*     TEXT_BUFFER_FILL       = TEXT_BUFFER_FILL
*     BUFFER_MODE    = BUFFER_MODE
* IMPORTING
*     ACT_PLVAR      = ACT_PLVAR
    TABLES
      result_tab     = codigos
      result_objec   = objetos
      result_struc   = relaciones
    EXCEPTIONS
      no_plvar_found = 1
      no_entry_found = 2.
  IF sy-subrc NE 0.
    MESSAGE 'Error en RH_STRUC_GET'(esg) TYPE 'E'.
  ENDIF.

ENDMETHOD.
METHOD get_empleados_orgeh.
  DATA: codigos TYPE tswhactor,
        codigo  TYPE swhactor.

  CLEAR empleados.


  IF NOT recursivo IS INITIAL.
    get_arbol_orgeh( EXPORTING orgeh   = orgeh
                               fecha   = fecha
                     IMPORTING codigos = codigos ).
  ENDIF.

  codigo-otype = 'O'.
  codigo-objid = orgeh.
  APPEND codigo TO codigos.

  LOOP AT codigos INTO codigo.
    SELECT * FROM pa0001
      APPENDING CORRESPONDING FIELDS OF TABLE empleados
     WHERE begda <= fecha
       AND endda >= fecha
       AND orgeh = codigo-objid.
  ENDLOOP.


ENDMETHOD.
METHOD get_empleados_posicion.

  CLEAR empleados.

  SELECT * FROM pa0001                                  "#EC CI_NOFIRST
    APPENDING CORRESPONDING FIELDS OF TABLE empleados
   WHERE begda <= fecha
     AND endda >= fecha
     AND plans = plans.

ENDMETHOD.
METHOD get_obj_rel.
  DATA: i_rel TYPE ztab_rel_pd,
        l_rel TYPE zest_rel_pd.

  CLEAR sobid.
  CALL METHOD get_rel_pd
    EXPORTING
      tipo_origen  = tipo_origen
      r_objid      = r_objid
      objid        = objid
      tipo_destino = tipo_destino
      r_sobid      = r_sobid
      rsign        = rsign
      relat        = relat
      istat        = istat
      begda        = begda
      endda        = endda
    RECEIVING
      t_rel        = i_rel.

  READ TABLE i_rel INTO l_rel INDEX 1.                  "#EC CI_NOORDER
  IF sy-subrc = 0.
    sobid = l_rel-sobid.
  ENDIF.

ENDMETHOD.
method GET_RANGO.
  DATA l_rango TYPE lso_range_objid.

  CLEAR r_objid.

  LOOP AT t_rel INTO rel.
    CLEAR l_rango.
    l_rango-option = 'EQ'.
    l_rango-sign   = 'I'.
    l_rango-low    = rel-sobid.
    COLLECT l_rango INTO r_objid.
  ENDLOOP.

  r_rango = r_objid.

endmethod.
METHOD get_rango_objid.
  DATA: i_hrp1000 TYPE TABLE OF hrp1000,
        hrp1000   TYPE hrp1000,
        i_org     TYPE TABLE OF hrp1001,
        l_org     TYPE hrp1001,
        l_objid   TYPE lso_range_objid.

  r_objid = s_objid.

  IF NOT s_objid IS INITIAL.
    SELECT objid FROM hrp1000
     INTO CORRESPONDING FIELDS OF TABLE i_hrp1000
    WHERE plvar = plvar
      AND otype = otype
      AND objid IN s_objid.
    LOOP AT i_hrp1000 INTO hrp1000.
      i_org = buscar_objid( objid = hrp1000-objid
                            otype = otype
                            relat = relat ).
      LOOP AT i_org INTO l_org.
        CLEAR l_objid.
        l_objid-option = 'EQ'.
        l_objid-sign   = 'I'.
        l_objid-low    = l_org-sobid.
        COLLECT l_objid INTO r_objid.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDMETHOD.
METHOD get_rango_orgeh.
  DATA: i_hrp1000 TYPE TABLE OF hrp1000,
        hrp1000   TYPE hrp1000,
        i_org     TYPE TABLE OF hrp1001,
        l_org     TYPE hrp1001,
        l_orgeh   TYPE cchrs_orgeh_range.

  r_orgeh = s_orgeh.

  IF NOT s_orgeh IS INITIAL.
    SELECT objid FROM hrp1000
     INTO CORRESPONDING FIELDS OF TABLE i_hrp1000
    WHERE plvar = plvar
      AND otype = 'O'
      AND objid IN s_orgeh.
    LOOP AT i_hrp1000 INTO hrp1000.
      i_org = buscar_org( hrp1000-objid ).
      LOOP AT i_org INTO l_org.
        CLEAR l_orgeh.
        l_orgeh-option = 'EQ'.
        l_orgeh-sign   = 'I'.
        l_orgeh-low    = l_org-sobid.
        COLLECT l_orgeh INTO r_orgeh.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDMETHOD.
method GET_REL_PD.
  data: r_rsign type range of hrp1001-rsign,
        l_rsign like line of r_rsign,
        r_relat type range of hrp1001-relat,
        l_relat like line of r_relat,
        rr_objid type range of hrp1001-objid,
        l_objid like line of rr_objid,
        r_istat type range of hrp1001-istat,
        l_istat like line of r_istat.

  if not rsign is initial.
    l_rsign-option = 'EQ'.
    l_rsign-sign = 'I'.
    l_rsign-low = rsign.
    append l_rsign to r_rsign.
  endif.

  if not relat is initial.
    l_relat-option = 'EQ'.
    l_relat-sign = 'I'.
    l_relat-low = relat.
    append l_relat to r_relat.
  endif.

  rr_objid = r_objid.
  if not objid is initial.
    l_objid-option = 'EQ'.
    l_objid-sign = 'I'.
    l_objid-low = objid.
    append l_objid to rr_objid.
  endif.

  if istat ne 'N' and istat ne ''.
    l_istat-option = 'EQ'.
    l_istat-sign = 'I'.
    l_istat-low = istat.
    append l_istat to r_istat.
  endif.


  select * from hrp1001
    into CORRESPONDING FIELDS OF TABLE t_rel
   where otype = tipo_origen
     and objid in rr_objid
     and plvar = c_plvar
     and rsign in r_rsign
     and relat in r_relat
     and istat in r_istat
     and begda <= endda
     and endda >= begda
     and sclas = tipo_destino
     and sobid in r_sobid.

endmethod.
method MATCHCODE.
  DATA: o_popup TYPE REF TO zcl_ap_matchcode_z,
        l_stext TYPE hrp1000-stext.


  CREATE OBJECT o_popup
    EXPORTING
      tabname = 'HRP1000'.

  o_popup->add_field( field = 'OBJID' selectflag = 'X' ).
  o_popup->add_field( field = 'STEXT' ).

  LOOP AT t_rel INTO rel.
    o_popup->add_valor( rel-sobid ).
    l_stext = zcl_ap_objeto_pd=>get_descripcion( otype = rel-sclas objid = rel-sobid ).
    o_popup->add_valor( l_stext ).
  ENDLOOP.

  o_popup->matchcode( EXPORTING field   = 'OBJID'
                      CHANGING  valor   = objid ).

endmethod.
METHOD select_rel_pd.

  t_rel = get_rel_pd( tipo_origen  = tipo_origen
                                  r_objid      = r_objid
                                  objid        = objid
                                  tipo_destino = tipo_destino
                                  r_sobid      = r_sobid
                                  rsign        = rsign
                                  relat        = relat
                                  istat        = istat
                                  begda        = begda
                                  endda        = endda
                                ).

ENDMETHOD.
