class ZCL_AP_FS definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_type_definition,
        level  TYPE i,
        field  TYPE string,
        kind   TYPE char1,
        type   TYPE string,
        parent TYPE i,
      END OF ty_type_definition .
  types:
    tt_type_definition TYPE STANDARD TABLE OF ty_type_definition .
  types:
    BEGIN OF ty_type_ref,
        level  TYPE i,
        parent TYPE i,
        field  TYPE string,
        ref    TYPE REF TO cl_abap_typedescr,
      END OF ty_type_ref .
  types:
    tt_type_ref TYPE STANDARD TABLE OF ty_type_ref .

  data CAMPO type TEXT40 .
  data CONT type NUMC2 .
  data INI_COLUMNA type TEXT40 .
  data CPROG type SY-CPROG .
  constants C_ELEMENT type CHAR1 value 'E' ##NO_TEXT.
  constants C_TABLE type CHAR1 value 'T' ##NO_TEXT.
  constants C_STRUCTURE type CHAR1 value 'S' ##NO_TEXT.
  data FIN_COLUMNA type TEXT40 .

  methods CONSTRUCTOR
    importing
      !COLUMNA type ANY optional
      !CPROG type SY-CPROG default SY-CPROG
      !FIN_COLUMNA type ANY default '' .
  methods GET_FS_STRING
    importing
      !COLUMNA type ANY
    returning
      value(STRING) type STRING .
  methods SET_FS_STRING
    importing
      !COLUMNA type ANY
      value(STRING) type ANY .
  methods GET_FS_INT
    importing
      !COLUMNA type ANY
      !DATOS type ANY optional
    returning
      value(INT) type I .
  class-methods OP_COLUMNAS
    importing
      !OP1 type ANY
      !OP2 type ANY
      !OPERACION type CHAR1 default '+'
    changing
      !RESUL type ANY .
  methods SET_FS
    importing
      !COLUMNA type ANY
      value(VALOR) type ANY
      !INCREMENTAR type ABAP_BOOL default '' .
  class-methods CREATE_IT_FIELDS_BASE
    importing
      !I_BASE_FIELDS type ANY
      !I_NEW_FIELDS type LVC_T_FCAT
    exporting
      !E_TABLE type ref to DATA .
  class-methods CREATE_IT_FIELDS_BASE_REF
    importing
      !I_BASE_FIELDS type ref to DATA
      !I_NEW_FIELDS type LVC_T_FCAT
    exporting
      !E_TABLE type ref to DATA .
  class-methods GET_COMPONENT_FROM_FCAT
    importing
      !I_FCAT type LVC_T_FCAT
    returning
      value(R_COMPONENTS) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
  class-methods PIVOT_TABLE
    importing
      !I_PIVOT_PATTERN type STRING
      !I_MOVE_FIELD type STRING
      !I_ITAB_SOURCE type STANDARD TABLE
      !I_PIVOT_FIELD type STRING
    changing
      !C_PIVOT_TABLE type STANDARD TABLE .
  class-methods CREATE_IT_FROM_STRUC
    importing
      !I_STRUC type ANY
      !SOLO_CAMPOS type ANY default ''
      !I_STRUC2 type ANY optional
      !CAMPOS_ADICIONALES type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE optional
      !ERROR_SI_CAMPO_NO_EXISTE type ABAP_BOOL default ''
    exporting
      !E_TABLE type ref to DATA
      !E_WORKAREA type ref to DATA
      !MESSAGE type BAPI_MSG .
  class-methods CREATE_WA_FROM_STRUC
    importing
      !I_STRUC type ANY
    exporting
      !E_WORKAREA type ref to DATA .
  class-methods COPIAR_ESTRUCTURA
    importing
      !ORIGEN type ANY
    exporting
      !DESTINO type ANY .
  class-methods CREATE_VARIABLE
    importing
      !IT_DES_TAB type TT_TYPE_DEFINITION
    exporting
      !ER_REF type ref to DATA
    exceptions
      INVALID_STRUCTURE .
  class-methods CREA_SUBTABLA
    importing
      !TABLA_PRINCIPAL type TABLE
      !CAMPOS type ANY
      !ESTRUCTURA type ANY default ''
    exporting
      !SUBTABLA type ref to DATA .
  class-methods GET_VALOR_FROM_STRUC
    importing
      !ESTRUCTURA type ANY
      !CAMPO type ANY
      !DATOS type ANY
    exporting
      !MESSAGE type BAPI_MSG
    returning
      value(VALOR) type STRING .
  class-methods ES_INICIAL
    importing
      !CPROG type SY-CPROG default SY-CPROG
      !CAMPO type ANY optional
    preferred parameter CAMPO
    returning
      value(INICIAL) type ABAP_BOOL .
  class-methods SET
    importing
      !CAMPO type ANY
      !VALOR type ANY
      !SUFIJO type ANY default ''
      !CPROG type SY-CPROG default SY-CPROG .
  class-methods GET_TABLA
    importing
      !TABLA type ANY
      !CAMPOS type ANY
      !WHERE type ANY
      !GET_JSON type ABAP_BOOL default ''
      !GET_XML type ABAP_BOOL default ''
      !SOLO_CAMPOS_INDICADOS type ABAP_BOOL default ''
      !GET_TABLA type ABAP_BOOL default ''
    exporting
      !DATOS type ref to DATA
      !JSON type STRING
      !XML type ZXSTRING
      !MESSAGE type STRING
      !TABLA_DATOS type TABLE .
  class-methods GET_COL_TABLA
    importing
      !TABLA type TABLE
      !CAMPO type ANY
      !NO_INITIAL type ABAP_BOOL default ''
      !CAMPO_CHECK type ANY default ''
      !COLLECT type ABAP_BOOL default 'X'
    returning
      value(LISTA) type TABLE_OF_STRINGS .
  class-methods GET_VAL
    importing
      !CAMPO type ANY
      !LINEA type ANY
    exporting
      !VALOR type ANY
      !MESSAGE type BAPI_MSG .
  class-methods GET_VALS
    importing
      !CAMPO type ANY optional
      !LINEA type ANY default ''
    preferred parameter CAMPO
    returning
      value(VALOR) type STRING .
  class-methods GET_LISTA_CAMPOS_DIF
    importing
      !VAR1 type ANY
      !SOLO_CAMPOS type ANY default ''
      !VAR2 type ANY optional
    returning
      value(CAMPOS_DIF) type STRING .
  class-methods RECURSIVE_GET_COMPONENTS
    importing
      !IO_STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR
    returning
      value(RT_COMPONENTS) type ABAP_COMPONENT_TAB .
protected section.

  class-data D_TEXT_TYPE type STRING value 'TYPE=' ##NO_TEXT.

  class-methods GET_NAME_OF_TYPE
    importing
      !I_ABSOLUTE_NAME type ABAP_ABSTYPENAME
    returning
      value(R_NAME_TYPE) type STRING .
private section.

  class-data MT_TYPE_DES_TAB type TT_TYPE_DEFINITION .
  class-data MT_TYPE_REF type TT_TYPE_REF .
  class-data MV_MAX_LEVEL type I .

  class-methods FILL_ELEMENT_REF
    importing
      !IV_ELE_TYPE type TY_TYPE_DEFINITION .
  class-methods FILL_STRUCT_REF
    importing
      !IV_STRUCT_REF type TY_TYPE_DEFINITION .
  class-methods FILL_TABLE_REF
    importing
      !IV_TABLE_REF type TY_TYPE_DEFINITION
    exceptions
      INVALID_STRUCTURE .
  class-methods GENERATE_VARIABLE
    exporting
      !ER_REF type ref to DATA .
  class-methods CHECK
    exceptions
      INVALID_STRUCTURE .
endclass. "ZCL_AP_FS definition
class ZCL_AP_FS implementation.
METHOD check.
    DATA: ls_line LIKE LINE OF mt_type_des_tab,
          " TODO: variable is assigned but never used (ABAP cleaner)
          lr_type TYPE REF TO cl_abap_typedescr,
          lt_temp LIKE mt_type_des_tab.

    LOOP AT mt_type_des_tab INTO ls_line.
      IF ls_line-kind <> c_structure AND ls_line-kind <> c_table AND ls_line-kind <> c_element.
        RAISE invalid_structure.
      ENDIF.

      IF ls_line-kind <> c_element.
        CONTINUE.
      ENDIF.

      cl_abap_typedescr=>describe_by_name(
        EXPORTING
          p_name         = ls_line-type
        RECEIVING
          p_descr_ref    = lr_type
        EXCEPTIONS
          type_not_found = 1 ).
      IF sy-subrc <> 0.
        RAISE invalid_structure.
      ENDIF.
    ENDLOOP.

    lt_temp = mt_type_des_tab.
    SORT lt_temp BY level ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING level.

    DO mv_max_level TIMES.
      READ TABLE lt_temp INTO ls_line INDEX sy-index.
      IF ls_line-level <> sy-index.
        RAISE invalid_structure.
      ENDIF.
    ENDDO.
  ENDMETHOD.
METHOD constructor.
    ini_columna = columna.
    me->fin_columna = fin_columna.
    me->cprog       = cprog.
  ENDMETHOD.
METHOD copiar_estructura.
    CALL FUNCTION 'HR_99S_COPY_STRUC1_STRUC2'
      EXPORTING
        p_struct1 = origen
      IMPORTING
        p_struct2 = destino.
  ENDMETHOD.
METHOD crea_subtabla.
    DATA lo_struct_ref TYPE REF TO cl_abap_structdescr.
    DATA lt_components TYPE cl_abap_structdescr=>component_table.
    DATA:
*        i_wa     TYPE REF TO data,
      l_campos TYPE string,
      l_wa     TYPE REF TO data.

    FIELD-SYMBOLS: <fs>                  TYPE any,
                   <subtabla>            TYPE STANDARD TABLE,
                   <reg_tabla_principal> TYPE any,
                   <reg_subtabla>        TYPE any.

    IF campos = '!SOLO_INFORMADOS'.
      lo_struct_ref ?= cl_abap_typedescr=>describe_by_name( estructura ).
      lt_components = lo_struct_ref->get_components( ).

      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<comp>) WHERE name <> ''.
        DATA(l_valor) = ''.
        LOOP AT tabla_principal ASSIGNING FIELD-SYMBOL(<tabla>).
          ASSIGN COMPONENT <comp>-name OF STRUCTURE <tabla> TO <fs>.
          IF sy-subrc = 0.
            IF NOT <fs> IS INITIAL.
              l_valor = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF l_valor = 'X'.
          IF l_campos IS INITIAL.
            l_campos = <comp>-name.
          ELSE.
            CONCATENATE l_campos <comp>-name INTO l_campos SEPARATED BY ','.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      l_campos = campos.
    ENDIF.

    create_it_from_struc( EXPORTING i_struc = estructura
                                     solo_campos = l_campos
*                                   IMPORTING e_table = i_wa
                                     IMPORTING e_table = subtabla
                                               e_workarea = l_wa ).

    ASSIGN subtabla->* TO <subtabla>.
    LOOP AT tabla_principal ASSIGNING <reg_tabla_principal>.
      ASSIGN l_wa->* TO <reg_subtabla>.
      MOVE-CORRESPONDING <reg_tabla_principal> TO <reg_subtabla>.
      APPEND <reg_subtabla> TO <subtabla>.
    ENDLOOP.
  ENDMETHOD.
METHOD create_it_fields_base.
    DATA lo_data TYPE REF TO data.

    GET REFERENCE OF i_base_fields INTO lo_data.

    create_it_fields_base_ref(
      EXPORTING
        i_base_fields = lo_data
        i_new_fields  = i_new_fields
      IMPORTING
        e_table       = e_table ).
  ENDMETHOD.
METHOD create_it_fields_base_ref.
    DATA lo_struct_ref     TYPE REF TO cl_abap_structdescr.
    DATA lt_components     TYPE cl_abap_structdescr=>component_table.
    DATA lt_components_new TYPE cl_abap_structdescr=>component_table.
    DATA lo_new_tab        TYPE REF TO cl_abap_tabledescr.

* Obtengo los componentes del tipo de datos
    lo_struct_ref ?= cl_abap_typedescr=>describe_by_data_ref( i_base_fields ).

* Obtengo los componentes
    lt_components = lo_struct_ref->get_components( ).

* Convierto el fieldcat del campo en componentes
    lt_components_new = get_component_from_fcat(
                            i_fcat       = i_new_fields ).

* Añado los nuevos componentes a los existentes
    APPEND LINES OF lt_components_new TO lt_components.

* Creo un nuevo tipo de datos con los componentes pasados.
    lo_struct_ref = cl_abap_structdescr=>create( lt_components ).

* Creo que la nueva tabla interna
    lo_new_tab = cl_abap_tabledescr=>create(
                    p_line_type  = lo_struct_ref
                    p_table_kind = cl_abap_tabledescr=>tablekind_std
                    p_unique     = abap_false ).

* Creamos el manejador de la nueva tabla
    CREATE DATA e_table TYPE HANDLE lo_new_tab.
  ENDMETHOD.
METHOD create_it_from_struc.
    DATA lo_struct_ref TYPE REF TO cl_abap_structdescr.
    DATA lt_components TYPE cl_abap_structdescr=>component_table.
    DATA: lt_components2 TYPE cl_abap_structdescr=>component_table,
          l_compo        TYPE abap_componentdescr,
          i_campos       TYPE TABLE OF ash_tabfields,
          l_campo        TYPE ash_tabfields.
    DATA lo_new_wa    TYPE REF TO cl_abap_structdescr.
    DATA lo_new_tab   TYPE REF TO cl_abap_tabledescr.
    DATA lo_error     TYPE REF TO cx_sy_table_creation.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA ld_txt_error TYPE string.

    CLEAR message.
* Obtengo los componentes del tipo de datos
    lo_struct_ref ?= cl_abap_typedescr=>describe_by_name( i_struc ).

* Obtengo los componentes
*    lt_components = lo_struct_ref->get_components( ).
    lt_components = recursive_get_components( lo_struct_ref ).

    IF i_struc2 <> ''.
      lo_struct_ref ?= cl_abap_typedescr=>describe_by_name( i_struc2 ).
      lt_components2 = lo_struct_ref->get_components( ).
      LOOP AT lt_components2 INTO l_compo.
        APPEND l_compo TO lt_components.
      ENDLOOP.
    ENDIF.

    DELETE lt_components WHERE name = ''.

    IF NOT solo_campos IS INITIAL.
      SPLIT solo_campos AT ',' INTO TABLE i_campos.
      LOOP AT lt_components INTO l_compo.
        IF NOT line_exists( i_campos[ reftab = l_compo-name ] ).
          DELETE lt_components.
        ENDIF.
      ENDLOOP.

      lt_components2 = lt_components.
      CLEAR lt_components.
      LOOP AT i_campos INTO l_campo.
        READ TABLE lt_components2 INTO l_compo WITH KEY name = l_campo-reftab.
        IF sy-subrc = 0.
          APPEND l_compo TO lt_components.
        ELSE.
          message = |No existe el campo { l_campo-reftab } en { i_struc }|.
          IF error_si_campo_no_existe = 'X'.
            MESSAGE message TYPE 'E'.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT campos_adicionales IS INITIAL.
      APPEND LINES OF campos_adicionales TO lt_components.
    ENDIF.

* Creo un nuevo tipo de datos con los componentes pasados.
    lo_new_wa = cl_abap_structdescr=>create( lt_components ).

* Creo que la nueva tabla interna
    TRY.
        lo_new_tab = cl_abap_tabledescr=>create(
          p_line_type  = lo_new_wa
          p_table_kind = cl_abap_tabledescr=>tablekind_std
          p_unique     = abap_false ).

* Creamos el manejador de la nueva tabla
        CREATE DATA e_table TYPE HANDLE lo_new_tab.

* Y su cabecera
        CREATE DATA e_workarea TYPE HANDLE lo_new_wa.

      CATCH cx_sy_table_creation INTO lo_error.
        ld_txt_error = lo_error->get_text( ).
    ENDTRY.
  ENDMETHOD.
METHOD create_variable.
* http://scn.sap.com/community/abap/blog/2014/02/28/a-handy-rttc-tool

    DATA ls_line LIKE LINE OF mt_type_des_tab.

    CLEAR: mt_type_des_tab, mt_type_ref, mv_max_level.
    mt_type_des_tab = it_des_tab.

    SORT mt_type_des_tab BY level DESCENDING.
    IF mt_type_des_tab IS INITIAL.
      RETURN.
    ENDIF.
    READ TABLE mt_type_des_tab INTO ls_line INDEX 1.
    mv_max_level = ls_line-level.

    check(
      EXCEPTIONS
        invalid_structure = 1 ).
    IF sy-subrc <> 0.
      RAISE invalid_structure.
    ENDIF.

    LOOP AT mt_type_des_tab INTO ls_line.
      IF ls_line-kind = c_element.
        fill_element_ref( ls_line ).
      ELSEIF ls_line-kind = c_structure.
        fill_struct_ref( ls_line ).
      ELSEIF ls_line-kind = c_table.
        fill_table_ref( ls_line ).
      ENDIF.
    ENDLOOP.

    generate_variable( IMPORTING er_ref = er_ref ).
  ENDMETHOD.
METHOD create_wa_from_struc.
    DATA lo_struct_ref TYPE REF TO cl_abap_structdescr.
    DATA lt_components TYPE cl_abap_structdescr=>component_table.
    DATA lo_wa         TYPE REF TO cl_abap_structdescr.
    DATA lo_error      TYPE REF TO cx_sy_struct_creation.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA d_txt_error   TYPE string.

* Obtengo los componentes del tipo de datos
    lo_struct_ref ?= cl_abap_typedescr=>describe_by_name( i_struc ).

* Obtengo los componentes
    lt_components = lo_struct_ref->get_components( ).

* Creo un nuevo tipo de datos con los componentes pasados.
    TRY.
        lo_wa = cl_abap_structdescr=>create( lt_components ).

* Creamos el manejador de la nueva tabla
        CREATE DATA e_workarea TYPE HANDLE lo_wa.

      CATCH cx_sy_struct_creation INTO lo_error.
        d_txt_error = lo_error->get_text( ).

    ENDTRY.
  ENDMETHOD.
METHOD es_inicial.
    DATA l_campo TYPE c LENGTH 80.

    FIELD-SYMBOLS <fs> TYPE any.

    CONCATENATE '(' cprog ')' campo INTO l_campo.
    ASSIGN (l_campo) TO <fs>.
    IF sy-subrc = 0.
      IF <fs> IS INITIAL.
        inicial = 'X'.
      ENDIF.
    ELSE.
      inicial = '?'.
    ENDIF.
  ENDMETHOD.
METHOD fill_element_ref.
    DATA: lr_type TYPE REF TO cl_abap_typedescr,
          ls_ref  LIKE LINE OF mt_type_ref.

    lr_type = cl_abap_typedescr=>describe_by_name( iv_ele_type-type ).

    ls_ref-level   = iv_ele_type-level.
    ls_ref-parent  = iv_ele_type-parent.
    ls_ref-field   = iv_ele_type-field.
    ls_ref-ref    ?= lr_type.
    APPEND ls_ref TO mt_type_ref.
  ENDMETHOD.
METHOD fill_struct_ref.
    DATA: ls_type_line  LIKE LINE OF mt_type_des_tab,
          ls_comp       TYPE abap_componentdescr,
          ls_ref        LIKE LINE OF mt_type_ref,
          lt_components TYPE abap_component_tab,
          lr_type       TYPE REF TO cl_abap_typedescr.

    LOOP AT mt_type_des_tab INTO ls_type_line WHERE parent = iv_struct_ref-level.
      CLEAR ls_comp.
      ls_comp-name = ls_type_line-field.
      READ TABLE mt_type_ref INTO ls_ref WITH KEY level = ls_type_line-level parent = ls_type_line-parent
          field = ls_type_line-field.
      ASSERT sy-subrc = 0.
      ls_comp-type ?= ls_ref-ref.
      APPEND ls_comp TO lt_components.
    ENDLOOP.

    lr_type ?= cl_abap_structdescr=>create( lt_components ).

    CLEAR ls_ref.
    ls_ref-level   = iv_struct_ref-level.
    ls_ref-parent  = iv_struct_ref-parent.
    ls_ref-ref    ?= lr_type.
    APPEND ls_ref TO mt_type_ref.
  ENDMETHOD.
METHOD fill_table_ref.
    DATA: ls_type_line  LIKE LINE OF mt_type_des_tab,
          ls_ref        LIKE LINE OF mt_type_ref,
          lr_struct_ref TYPE REF TO cl_abap_structdescr,
          lr_type       TYPE REF TO cl_abap_typedescr.

    LOOP AT mt_type_des_tab INTO ls_type_line WHERE parent = iv_table_ref-level.
      READ TABLE mt_type_ref INTO ls_ref WITH KEY level = ls_type_line-level parent = ls_type_line-parent.
      ASSERT sy-subrc = 0.
      IF ls_type_line-kind = c_structure.
        EXIT.
      ELSE.
        RAISE invalid_structure.
      ENDIF.
    ENDLOOP.

    lr_struct_ref ?= ls_ref-ref.
    IF lr_struct_ref IS NOT INITIAL.
      lr_type ?= cl_abap_tabledescr=>create( lr_struct_ref ).
    ELSE.
      READ TABLE mt_type_des_tab INTO ls_type_line WITH KEY level = iv_table_ref-level field = iv_table_ref-field.
      ASSERT sy-subrc = 0.
      lr_type ?= cl_abap_typedescr=>describe_by_name( ls_type_line-type ).
    ENDIF.

    CLEAR ls_ref.
    ls_ref-level   = iv_table_ref-level.
    ls_ref-parent  = iv_table_ref-parent.
    ls_ref-field   = iv_table_ref-field.
    ls_ref-ref    ?= lr_type.
    APPEND ls_ref TO mt_type_ref.
  ENDMETHOD.
METHOD generate_variable.
    DATA: ls_type       LIKE LINE OF mt_type_des_tab,
          ls_comp       TYPE abap_componentdescr,
          ls_ref        LIKE LINE OF mt_type_ref,
          lt_components TYPE abap_component_tab,
          lr_root       TYPE REF TO cl_abap_structdescr.

    LOOP AT mt_type_des_tab INTO ls_type WHERE parent = 0.
      CLEAR ls_comp.
      ls_comp-name = ls_type-field.
      READ TABLE mt_type_ref INTO ls_ref WITH KEY level = ls_type-level parent = ls_type-parent.
      ls_comp-type ?= ls_ref-ref.
      APPEND ls_comp TO lt_components.
    ENDLOOP.

    lr_root = cl_abap_structdescr=>create( lt_components ).
    CREATE DATA er_ref TYPE HANDLE lr_root.
  ENDMETHOD.
METHOD get_col_tabla.
    FIELD-SYMBOLS: <linea> TYPE any,
                   <valor> TYPE any.

    CLEAR lista.
    LOOP AT tabla ASSIGNING <linea>.
      IF NOT campo_check IS INITIAL.
        IF get_vals( campo = campo_check linea = <linea> ) = ''.
          CONTINUE.
        ENDIF.
      ENDIF.
      ASSIGN COMPONENT campo OF STRUCTURE <linea> TO <valor>.
      IF sy-subrc = 0.
        IF no_initial = 'X' AND <valor> IS INITIAL.
        ELSE.
          APPEND <valor> TO lista.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF collect = 'X'.
      SORT lista.
      DELETE ADJACENT DUPLICATES FROM lista.
    ENDIF.
  ENDMETHOD.
METHOD get_component_from_fcat.
    DATA le_component TYPE LINE OF cl_abap_structdescr=>component_table.
    DATA lo_typedescr TYPE REF TO cl_abap_typedescr.
    DATA ld_type_elem TYPE string.

    FIELD-SYMBOLS <ls_fcat> TYPE LINE OF lvc_t_fcat.

    CLEAR r_components.

    LOOP AT i_fcat ASSIGNING <ls_fcat>.

      le_component-name = <ls_fcat>-fieldname.

* Obtengo la información del elementos de datos
      lo_typedescr = cl_abap_elemdescr=>describe_by_name( <ls_fcat>-rollname ).

      ld_type_elem = lo_typedescr->type_kind.

      CASE ld_type_elem.

        WHEN cl_abap_elemdescr=>typekind_char.
          le_component-type = cl_abap_elemdescr=>get_c( p_length   = lo_typedescr->length ).

        WHEN cl_abap_elemdescr=>typekind_packed.
* Obtengo el tipo de datos segun el componente
          le_component-type = cl_abap_elemdescr=>get_p( p_length   = lo_typedescr->length
                                                        p_decimals = lo_typedescr->decimals ).

        WHEN cl_abap_elemdescr=>typekind_num.
          le_component-type = cl_abap_elemdescr=>get_n( p_length   = lo_typedescr->length ).

        WHEN cl_abap_elemdescr=>typekind_date.
          le_component-type = cl_abap_elemdescr=>get_d( ).

        WHEN cl_abap_elemdescr=>typekind_string.
          le_component-type = cl_abap_elemdescr=>get_string( ).

        WHEN cl_abap_elemdescr=>typekind_int.
          le_component-type = cl_abap_elemdescr=>get_i( ).

        WHEN cl_abap_elemdescr=>typekind_struct1. " Estructura diccionario

          le_component-type ?= cl_abap_typedescr=>describe_by_name( <ls_fcat>-rollname ).

        WHEN cl_abap_elemdescr=>typekind_table. " Tabla interna

          le_component-type ?= cl_abap_typedescr=>describe_by_name( <ls_fcat>-rollname ).

*    when cl_abap_elemdescr=>TYPEKIND_struct2.
*    when cl_abap_elemdescr=>TYPEKIND_INT1
*    when cl_abap_elemdescr=>TYPEKIND_CLASS.

      ENDCASE.
      IF le_component IS NOT INITIAL.
        APPEND le_component TO r_components.
        CLEAR le_component.
        FREE lo_typedescr.
      ENDIF.

    ENDLOOP.

*    DATA lo_typedescr  TYPE REF TO cl_abap_elemdescr.
*  DATA le_component TYPE LINE OF cl_abap_structdescr=>component_table.
*
*  FIELD-SYMBOLS <ls_fcat> TYPE LINE OF lvc_t_fcat.
*
*  CLEAR r_components.
*
*  LOOP AT i_fcat ASSIGNING <ls_fcat>.
*
*    le_component-name = <ls_fcat>-fieldname.
*
** Obtengo la información del elementos de datos
*    lo_typedescr ?= cl_abap_elemdescr=>describe_by_name( <ls_fcat>-rollname ).
*
**    TYPE_KIND
*case
** Obtengo el tipo de datos segun el componente
*    le_component-type = cl_abap_elemdescr=>get_p( p_length   = lo_typedescr->length
*                                                  p_decimals = lo_typedescr->decimals ).
*
*    APPEND le_component TO r_components.
*    CLEAR le_component.
*    FREE lo_typedescr.
*
*  ENDLOOP.
  ENDMETHOD.
METHOD get_fs_int.
    FIELD-SYMBOLS <fs> TYPE any.

    CONCATENATE ini_columna columna fin_columna INTO campo.

    IF datos IS SUPPLIED.
      CONCATENATE 'DATOS-' campo INTO campo.
    ELSE.
      IF NOT cprog IS INITIAL.
        CONCATENATE '(' cprog ')' campo INTO campo.
      ENDIF.
    ENDIF.

    ASSIGN (campo) TO <fs>.
    IF sy-subrc = 0.
      int = <fs>.
    ENDIF.
  ENDMETHOD.
METHOD get_fs_string.
    FIELD-SYMBOLS <fs> TYPE any.

    CONCATENATE ini_columna columna fin_columna INTO campo.

    IF NOT cprog IS INITIAL.
      CONCATENATE '(' cprog ')' campo INTO campo.
    ENDIF.

    ASSIGN (campo) TO <fs>.
    IF sy-subrc = 0.
      string = <fs>.
    ENDIF.
  ENDMETHOD.
METHOD get_lista_campos_dif.
    FIELD-SYMBOLS: <fs1> TYPE any,
                   <fs2> TYPE any.

    CLEAR campos_dif.

    DATA(i_campos1) = zcl_ap_dev=>get_fieldcatalog( linea = var1 ).
    DATA(i_campos2) = zcl_ap_dev=>get_fieldcatalog( linea = var2 ).
    DELETE i_campos1 WHERE name = ''.
    DELETE i_campos2 WHERE name = ''.

    IF NOT solo_campos IS INITIAL.
      SPLIT solo_campos AT ',' INTO TABLE DATA(i_campos).
      LOOP AT i_campos1 INTO DATA(l_campo).
        IF NOT line_exists( i_campos[ table_line = l_campo-name ] ).
          DELETE i_campos1.
        ENDIF.
      ENDLOOP.
      LOOP AT i_campos2 INTO l_campo.
        IF NOT line_exists( i_campos[ table_line = l_campo-name ] ).
          DELETE i_campos2.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT i_campos1 INTO l_campo.
      ASSIGN COMPONENT l_campo-name OF STRUCTURE var1 TO <fs1>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT l_campo-name OF STRUCTURE var2 TO <fs2>.
        IF sy-subrc = 0.
          IF <fs1> <> <fs2>.
            __add_lista campos_dif l_campo-name.
          ENDIF.
        ELSE.
          __add_lista campos_dif l_campo-name.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
METHOD get_name_of_type.
    DATA ld_pos TYPE i.

    CLEAR r_name_type.

* Busco donde empieza el literal TYPE=.
    FIND FIRST OCCURRENCE OF d_text_type IN i_absolute_name MATCH OFFSET ld_pos.
    IF sy-subrc = 0.

* Obtengo el literal desde la posición donde esta el valor DE TYPE=
      r_name_type = i_absolute_name+ld_pos.

* Eliminio el TYPE= para quedarme con el nombre
      REPLACE ALL OCCURRENCES OF d_text_type IN r_name_type WITH space.

    ENDIF.
  ENDMETHOD.
METHOD get_tabla.
    DATA: i_campos  TYPE TABLE OF string,
          l_where   TYPE string,
          o_root    TYPE REF TO cx_root,
          l_message TYPE bapi_msg.

    FIELD-SYMBOLS <tabla> TYPE STANDARD TABLE.

    SPLIT campos AT ',' INTO TABLE i_campos.

    IF solo_campos_indicados IS INITIAL.
      create_it_from_struc( EXPORTING i_struc = tabla
                            IMPORTING e_table = datos ).
    ELSE.
      create_it_from_struc( EXPORTING i_struc = tabla
                                      solo_campos = campos
                            IMPORTING e_table = datos ).
    ENDIF.

    IF NOT where IS INITIAL.
      l_where = where.
      REPLACE ALL OCCURRENCES OF '|' IN l_where WITH ''''.
      REPLACE ALL OCCURRENCES OF '!!' IN l_where WITH '%'.
    ENDIF.

    ASSIGN datos->* TO <tabla>.
    TRY.
        SELECT (i_campos) FROM (tabla)
          INTO CORRESPONDING FIELDS OF TABLE <tabla>
         WHERE (l_where)
         ORDER BY PRIMARY KEY.
      CATCH cx_root INTO o_root.
        message = o_root->get_text( ).
        RETURN.
    ENDTRY.

    IF get_tabla = 'X'.
      tabla_datos = <tabla>.
    ENDIF.

    IF get_xml = 'X'.
      xml = zcl_ap_string=>transform_object_to_rawstring( tabla = <tabla> comprimir = '' ).
    ENDIF.

    IF get_json = 'X'.
      zcl_ap_segw=>tabla2json( EXPORTING tabla   = <tabla>
                               IMPORTING json    = json
                                         message = l_message ).
      message = l_message.
    ENDIF.

*      DATA datos TYPE REF TO data.
*      FIELD-SYMBOLS: <tabla> TYPE STANDARD TABLE,
*                     <linea> TYPE any,
*                     <fs>    TYPE any.
*
*      zcl_ap_fs=>get_tabla( EXPORTING tabla = 'T001' campos = 'BUTXT' where = l_where solo_campos_indicados = 'X'
*                            IMPORTING message = message datos = datos ).
*
*      ASSIGN datos->* TO <tabla>.
*      LOOP AT <tabla> ASSIGNING <linea>.
  ENDMETHOD.
METHOD get_val.
    FIELD-SYMBOLS <fs> TYPE any.

    ASSIGN COMPONENT campo OF STRUCTURE linea TO <fs>.
    IF sy-subrc = 0.
      valor = <fs>.
    ELSE.
      message = |Estructura { linea } no contiene campo { campo }|.
    ENDIF.
  ENDMETHOD.
METHOD get_valor_from_struc.
    DATA lo_struct_ref TYPE REF TO cl_abap_structdescr.
    DATA lt_components TYPE cl_abap_structdescr=>component_table.
    DATA lo_wa         TYPE REF TO cl_abap_structdescr.
    DATA e_workarea    TYPE REF TO data.
    DATA lo_error      TYPE REF TO cx_sy_struct_creation.

    FIELD-SYMBOLS: <datos> TYPE any,
                   <dato>  TYPE any.

* Obtengo los componentes del tipo de datos
    lo_struct_ref ?= cl_abap_typedescr=>describe_by_name( estructura ).

* Obtengo los componentes
    lt_components = lo_struct_ref->get_components( ).

* Creo un nuevo tipo de datos con los componentes pasados.
    TRY.
        lo_wa = cl_abap_structdescr=>create( lt_components ).

* Creamos el manejador de la nueva tabla
        CREATE DATA e_workarea TYPE HANDLE lo_wa.

        ASSIGN e_workarea->* TO <datos>.
        <datos> = datos.
        ASSIGN COMPONENT campo OF STRUCTURE <datos> TO <dato>.
        IF sy-subrc = 0.
          valor = <dato>.
        else.
          message = |No existe el campo { campo } en la estructura|.
        ENDIF.

      CATCH cx_sy_struct_creation INTO lo_error.
        message = lo_error->get_text( ).
    ENDTRY.
  ENDMETHOD.
METHOD get_vals.
    get_val( EXPORTING campo = campo
                       linea = linea
             IMPORTING valor = valor
                       " TODO: variable is assigned but never used (ABAP cleaner)
                       message = DATA(l_message) ).
  ENDMETHOD.
METHOD op_columnas.
    DATA: i_campos TYPE abap_component_tab,
          l_c1     TYPE string,
          l_c2     TYPE string,
          l_r      TYPE string.

    FIELD-SYMBOLS: <campo> TYPE abap_componentdescr,
                   <c1>    TYPE any,
                   <c2>    TYPE any,
                   <r>     TYPE any.

    i_campos = zcl_ap_dev=>get_fieldcatalog( linea = resul
                                             solo_tipos = 'P' ).

    LOOP AT i_campos ASSIGNING <campo>.
      CONCATENATE 'OP1-' <campo>-name INTO l_c1.
      ASSIGN (l_c1) TO <c1>.
      CONCATENATE 'OP2-' <campo>-name INTO l_c2.
      ASSIGN (l_c2) TO <c2>.
      CONCATENATE 'RESUL-' <campo>-name INTO l_r.
      ASSIGN (l_r) TO <r>.

      CASE operacion.
        WHEN '+'.
          <r> = <c1> + <c2>.
        WHEN '-'.
          <r> = <c1> - <c2>.
        WHEN '/'.
          IF <c2> = 0.
            <r> = 0.
          ELSE.
            <r> = <c1> / <c2>.
          ENDIF.
        WHEN '%'.
          IF <c2> = 0.
            <r> = 0.
          ELSE.
            <r> = ( 100 * <c1> ) / <c2>.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
METHOD pivot_table.
    DATA le_pivot TYPE REF TO data.
    DATA ld_field TYPE string.

    FIELD-SYMBOLS <le_pivot>       TYPE any.
    FIELD-SYMBOLS <le_source>      TYPE any.
    FIELD-SYMBOLS <le_field>       TYPE any.
    FIELD-SYMBOLS <le_pivot_field> TYPE any.
    FIELD-SYMBOLS <le_move_field>  TYPE any.

* Creo la workarea de la tabla pivote
    CREATE DATA le_pivot LIKE LINE OF c_pivot_table.
    ASSIGN le_pivot->* TO <le_pivot>.

* Recorro la tabla de origen
    LOOP AT i_itab_source ASSIGNING <le_source>.

* Muevo los campos comunes
      MOVE-CORRESPONDING <le_source> TO <le_pivot>.

* Obtengo el valor del campo que se va pivotar
      ASSIGN COMPONENT i_pivot_field OF STRUCTURE <le_source> TO <le_field>.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

* Creo el campo donde se pondra el valor en la tabla pivote.
      CONCATENATE '<LE_PIVOT>-' i_pivot_pattern <le_field> INTO ld_field.
      ASSIGN (ld_field) TO <le_pivot_field>.

      IF sy-subrc = 0.

* Obtengo el valor del campo que se va mover su valor al del
* campo de la tabla pivote.
        CONCATENATE '<LE_SOURCE>-' i_move_field INTO ld_field.
        ASSIGN (ld_field) TO <le_move_field>.
        IF sy-subrc = 0.

          <le_pivot_field> = <le_move_field>.
          COLLECT <le_pivot> INTO c_pivot_table.
          CLEAR <le_pivot>.

        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
METHOD recursive_get_components.
    DATA:
      lt_incl_comp TYPE abap_component_tab,
      lo_incl_stru TYPE REF TO cl_abap_structdescr,
      l_curr_index TYPE i.

    FIELD-SYMBOLS: <comp>      LIKE LINE OF rt_components,
                   <incl_comp> LIKE LINE OF lt_incl_comp.

    rt_components = io_structdescr->get_components( ).

    LOOP AT rt_components ASSIGNING <comp>.
      IF <comp>-as_include = 'X'.
        lo_incl_stru ?= <comp>-type.  " not the include struc type
        l_curr_index = sy-tabix.      " and the index it is to be included
        DELETE rt_components INDEX l_curr_index.

        lt_incl_comp = recursive_get_components( io_structdescr =  lo_incl_stru  ).
        LOOP AT lt_incl_comp ASSIGNING <incl_comp>.
          INSERT <incl_comp> INTO rt_components INDEX l_curr_index.
          l_curr_index = l_curr_index + 1.
        ENDLOOP.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
METHOD set.
    DATA l_campo TYPE c LENGTH 80.

    FIELD-SYMBOLS <fs> TYPE any.

    IF NOT sufijo IS INITIAL.
      CONCATENATE campo sufijo INTO l_campo.
    ENDIF.

    IF NOT cprog IS INITIAL.
      CONCATENATE '(' cprog ')' l_campo INTO l_campo.
    ENDIF.

    ASSIGN (campo) TO <fs>.
    IF sy-subrc = 0.
      <fs> = valor.
    ENDIF.
  ENDMETHOD.
METHOD set_fs.
    FIELD-SYMBOLS <fs> TYPE any.

    CONCATENATE ini_columna columna INTO campo.
    IF NOT cprog IS INITIAL.
      CONCATENATE '(' cprog ')' campo INTO campo.
    ENDIF.

    ASSIGN (campo) TO <fs>.
    IF sy-subrc = 0.
      IF incrementar IS INITIAL.
        <fs> = valor.
      ELSE.
        <fs> = <fs> + valor.
      ENDIF.
    ENDIF.
  ENDMETHOD.
METHOD set_fs_string.
    FIELD-SYMBOLS <fs> TYPE any.

    CONCATENATE ini_columna columna fin_columna INTO campo.
    IF NOT cprog IS INITIAL.
      CONCATENATE '(' cprog ')' campo INTO campo.
    ENDIF.

    ASSIGN (campo) TO <fs>.
    IF sy-subrc = 0.
      <fs> = string.
    ENDIF.
  ENDMETHOD.
