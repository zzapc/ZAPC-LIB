class ZCL_AP_MATCHCODE_Z definition
  public
  final
  create public .

public section.

  data I_VALUES type SOLI_TAB .

  methods CONSTRUCTOR
    importing
      !TABNAME type ANY optional .
  methods ADD_FIELD
    importing
      !TABNAME type ANY optional
      !FIELD type ANY
      !SELECTFLAG type ANY default '' .
  methods ADD_VALOR
    importing
      !VALOR type ANY .
  methods MATCHCODE
    importing
      !TABNAME type ANY optional
      !FIELD type ANY optional
    changing
      !VALOR type ANY .
  methods MATCHCODE_INDEX
    importing
      !TABNAME type ANY optional
      !FIELD type ANY optional
    returning
      value(INDICE) type I .
  class-methods GET_VALOR_CAMPO
    importing
      !DYNAME type SY-CPROG default SY-CPROG
      !DYNUMB type SY-DYNNR default SY-DYNNR
      !CAMPO type ANY optional
      !STEPL type SYSTEPL default 0
    preferred parameter CAMPO
    returning
      value(VALOR) type DYNFIELDVALUE .
  class-methods SET_VALOR_CAMPO
    importing
      !DYNAME type SY-CPROG default SY-CPROG
      !DYNUMB type SY-DYNNR default SY-DYNNR
      !CAMPO type ANY
      !VALOR type ANY .
  class-methods MATCHCODE_DDIC
    importing
      !TABNAME type ANY
      !FIELD type ANY
    exporting
      !VALOR type ANY
    changing
      !I_VALORES type TABLE optional .
  class-methods MATCHCODE_STD
    importing
      !MATCHCODE type ANY
    returning
      value(VALOR) type STRING .
  class-methods MATCHCODE_STD_VALMULT
    importing
      !MATCHCODE type ANY optional
      !F4FIELD type ANY default ''
    preferred parameter MATCHCODE
    returning
      value(I_VALUES) type TINV_DDSHRETVAL .
  methods FREE .
  class-methods MATCHCODE_STRUCTURED
    importing
      !CAMPO type ANY
      !CPROG type SY-CPROG default SY-CPROG
      !DYNNR type SY-DYNNR default SY-DYNNR
      !I_VALORES type TABLE optional
      !MULTIPLE type DDBOOL_D default '' .
protected section.
private section.

  data I_FIELDS type HRHELP_VALUE_TAB_OLD .
  data TABNAME type STRING .
  data CLAVE type STRING .
endclass. "ZCL_AP_MATCHCODE_Z definition
class ZCL_AP_MATCHCODE_Z implementation.
method ADD_FIELD.
  DATA: l_field TYPE help_value,
        l_column TYPE lvc_fname,
        i_column TYPE TABLE OF lvc_fname.

  SPLIT field AT ',' INTO TABLE i_column.
  LOOP AT i_column INTO l_column.
    TRANSLATE l_column TO UPPER CASE.

    CLEAR l_field.
    IF NOT tabname IS INITIAL.
      l_field-tabname    = tabname.
    ELSE.
      l_field-tabname    = me->tabname.
    ENDIF.
    l_field-fieldname  = l_column.
    l_field-selectflag = selectflag.
    APPEND l_field TO i_fields.

    IF selectflag = 'X'.
      IF me->clave IS INITIAL.
        me->clave = field.
      ENDIF.
    ENDIF.
  ENDLOOP.

endmethod.
method ADD_VALOR.
  DATA l_valor TYPE soli.

  l_valor = valor.
  APPEND l_valor TO i_values.

endmethod.
method CONSTRUCTOR.
  CLEAR: i_fields, i_values.

  me->tabname = tabname.

endmethod.
method FREE.

  cleaR: I_FIELDS, I_VALUES, TABNAME, CLAVE.

endmethod.
METHOD get_valor_campo.
  DATA: i_dvr_dynpfields TYPE TABLE OF dynpread,
        l_dynpfields     TYPE dynpread.

  CLEAR valor.

  l_dynpfields-fieldname = campo.
  l_dynpfields-stepl = stepl.
  APPEND l_dynpfields TO i_dvr_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = dyname
      dynumb               = dynumb
    TABLES
      dynpfields           = i_dvr_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      OTHERS               = 9.

  READ TABLE i_dvr_dynpfields INTO l_dynpfields INDEX 1.
  IF sy-subrc = 0.
    valor = l_dynpfields-fieldvalue.
  ENDIF.

ENDMETHOD.
method MATCHCODE.
data: l_FIELDNAME type HELP_INFO-FIELDNAME,
      l_tabname   type HELP_INFO-TABNAME.

  if not tabname is initial.
    l_tabname = tabname.
  else.
    l_tabname = me->tabname.
  endif.

  if not field is initial.
    l_fieldname = field.
  else.
    l_fieldname = me->clave.
  endif.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'                "#EC *
      EXPORTING
*         CUCOL                         = 0
**         CUROW                         = 0
          DISPLAY                       = ''
            fieldname                     = l_fieldname
            tabname                       = l_tabname
*         NO_MARKING_OF_CHECKVALUE      = ' '
*         TITLE_IN_VALUES_LIST          = ' '
*         TITEL                         = ' '
        show_all_values_at_first_time = 'X'
*         NO_CONVERSION                 = ' '
       IMPORTING
            select_value                  = valor
       TABLES
            fields                        = i_fields
            valuetab                      = i_values
       EXCEPTIONS
            field_not_in_ddic             = 1
            more_then_one_selectfield     = 2
            no_selectfield                = 3
            OTHERS                        = 4.
  IF sy-subrc = 1.
    MESSAGE 'El campo no existe'(CNE) TYPE 'E'.
  ENDIF.

endmethod.
method MATCHCODE_DDIC.
  DATA: field_tab TYPE TABLE OF dfies,
        l_field TYPE dfies,
        return_tab TYPE TABLE OF ddshretval,
        l_return TYPE ddshretval.

  CLEAR: field_tab, return_tab.


  l_field-fieldname = field.
  l_field-tabname = tabname.
  APPEND l_field TO field_tab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = field
    TABLES
      value_tab       = i_valores
      field_tab       = field_tab
      return_tab      = return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  READ TABLE return_tab INTO l_return INDEX 1.
  IF sy-subrc = 0.
    valor = l_return-fieldval.
  ENDIF.



endmethod.
method MATCHCODE_INDEX.
  DATA: l_fieldname TYPE help_info-fieldname,
        l_tabname   TYPE help_info-tabname.

  IF NOT tabname IS INITIAL.
    l_tabname = tabname.
  ELSE.
    l_tabname = me->tabname.
  ENDIF.

  IF NOT field IS INITIAL.
    l_fieldname = field.
  ELSE.
    l_fieldname = me->clave.
  ENDIF.

  CALL FUNCTION 'F4TOOL_F4FUNCTION_BRIDGE'
       EXPORTING
            tabname        = l_tabname
            fieldname      = l_fieldname
            display_only   = ' '
       IMPORTING
            selected_index = indice
       TABLES
*           SHVALUE_TAB    =
*           SHSTRUC_TAB    =
            value_tab      = i_values
            fields_tab     = i_fields
*           VALUES_OUT     =
*           COLTITLE_TAB   =
          .

endmethod.
METHOD matchcode_std.
  DATA l_mc TYPE help_info-mcobj.

  CLEAR valor.
  l_mc = matchcode.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_MATCHCODE'  "#EC *
    EXPORTING
*     DISPLAY                   = ' '
*     FIELDNAME                 = ' '
*     INPUT_VALUE               = ' '
      matchcode_object          = l_mc
*     TABNAME                   = ' '
    IMPORTING
      select_value              = valor
    EXCEPTIONS
      invalid_dictionary_field  = 1
      invalid_matchdcode_object = 2
      no_selection              = 3
      OTHERS                    = 4.

ENDMETHOD.
METHOD matchcode_std_valmult.
  DATA: shlp TYPE  shlp_descr,
        l_shlpname type SHLPNAME.

  FIELD-SYMBOLS <interface> TYPE ddshiface.

  l_shlpname = matchcode.
  CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
    EXPORTING
      shlpname = l_shlpname
    IMPORTING
      shlp     = shlp.

  LOOP AT shlp-interface ASSIGNING <interface>.
    <interface>-valfield = 'X'.
    if <interface>-SHLPFIELD = f4field.
      <interface>-f4field = 'X'.
    endif.
  ENDLOOP.
  CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
    EXPORTING
      shlp          = shlp
    TABLES
      return_values = i_values.

ENDMETHOD.
METHOD matchcode_structured.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = campo                                            " Name of return field in FIELD_TAB -
      dynprofield = campo                                           " Name of screen field for value return -
      dynpprog    = cprog                                         " Current program
      dynpnr      = dynnr                                         " Screen number
      value_org   = 'S'                                              " Value return S: structured
      MULTIPLE_CHOICE = MULTIPLE
    TABLES
      value_tab   = i_valores.                                        " F4 help values


ENDMETHOD.
method SET_VALOR_CAMPO.
  DATA: i_dvr_dynpfields TYPE TABLE OF dynpread,
        l_dynpfields TYPE dynpread.

  l_dynpfields-fieldname = campo.
  l_dynpfields-fieldvalue = valor.
  APPEND l_dynpfields TO i_dvr_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = dyname
      dynumb               = dynumb
    TABLES
      dynpfields           = i_dvr_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.


endmethod.
