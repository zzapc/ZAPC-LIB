
class ZCL_AP_RMS definition
  public
  final
  create public .

public section.

  data I_LISTA type BAPIDOCTAB_T .
  data DOC_RMS type BAPIDOCTAB .
  data I_PROPERTY_SELECTION type RMPS_SEL_PROPERTIES_REC .
  data I_PROP type BAPIPROPT .
  data PROP type BAPIPROPTB .
  data I_ELEMENT type ZT_BAPISRMREC_ELEMENT .
  data ELEMENT type BAPISRMREC_ELEMENT .
  data ELEMENT_IDENTIFICATION type BAPISRMREC_ELEMENT_IDENT .
  data I_ELEMENT_IDENTIFICATION type ZT_BAPISRMREC_ELEMENT_IDENT .
  data ELEMENT_PROPERTY type BAPIPROPELEMENT .
  data I_ELEMENT_PROPERTIES type ZT_BAPIPROPELEMENT .
  data ELEMENT_VISIBILITY type BAPIPROPELEMENT .
  data I_ELEMENT_VISIBILITY type ZT_BAPIPROPELEMENT .
  data ELEMENT_RELATIONS type BAPIPROPELEMENT .
  data I_ELEMENT_RELATIONS type ZT_BAPIPROPELEMENT .
  data OBJECTID type BAPIGUID .
  data DOCCLASS type BAPIDCLASS .
  data RETURN type BAPIRET2 .

  methods CONSTRUCTOR
    importing
      !RMS_ID type BAPISRMREC-RMSID optional
      !SPS_ID type BAPISRMREC-SPSID optional .
  class-methods VER_ST
    importing
      !OBJECTID type BAPIGUID optional
      !DOCUMENTCLASS type BAPIDCLASS optional
      !RMS_ID type BAPISRMREC-RMSID optional
      !SPS_ID type BAPISRMREC-SPSID optional
      !CLAVE type ANY optional .
  methods GET_LISTA .
  methods ADD_SEL
    importing
      !PROPNAME type BAPIPROPNA
      !OPTION type BAPIOPTION default 'EQ'
      !SIGN type BAPISIGN default 'I'
      !LOW type ANY
      !HIGH type ANY default '' .
  methods GET_PROPERTIES
    importing
      !OBJECTID type BAPIGUID optional
      !DOCUMENTCLASS type BAPIDCLASS optional
      !DOC type BAPIDOCTAB optional
      !ELEMENT type BAPIPROPVA optional
      !WHOLE_DOCUMENT type ABAP_BOOL default '' .
  methods GET_PROP
    importing
      !PROPIEDAD type ANY
    returning
      value(VALOR) type BAPIPROPVA .
  methods GET_ELEMENTS
    importing
      !OBJECTID type BAPIGUID optional
      !DOCUMENTCLASS type BAPIDCLASS optional
      !DOC type BAPIDOCTAB optional .
  methods SET_PROPERTIES
    importing
      !OBJECTID type BAPIGUID optional
      !DOCUMENTCLASS type BAPIDCLASS optional
      !I_PROPERTIES type BAPIPROPT
      !I_PROPS_DELETE type BAPIPROPT optional
      !DOC type BAPIDOCTAB optional
      !ELEMENT type BAPIPROPVA optional
    returning
      value(RETURN) type BAPIRET2 .
  methods GET_DOCUMENTO
    importing
      !OBJECTID type BAPISRMREC-GUID
      !DOCUMENTCLASS type BAPISRMREC-DOCCLASS
    returning
      value(O_DOCUMENTO) type ref to IF_SRM_SP_DOCUMENT .
  methods GET_DOC_TIPO_DESCRIPCION
    importing
      !OBJECTID type BAPISRMREC-GUID optional
      !DOCUMENTCLASS type BAPISRMREC-DOCCLASS optional
    returning
      value(DESCRIPCION) type STRING .
  methods GET_DATOS_CLASE
    importing
      !CLASE type SDOKOBJECT-CLASS
    returning
      value(I_PROPIEDADES) type SDOKPROPTYS .
  methods SET_DOC_PROPERTIES
    importing
      !OBJECTID type BAPIGUID optional
      !DOCUMENTCLASS type BAPIDCLASS optional
      !I_PROPERTIES type BAPIPROPT
      !DOC type BAPIDOCTAB optional
      !ELEMENT type BAPIPROPVA optional
    returning
      value(RETURN) type BAPIRET2 .
  methods CREAR_DOC
    importing
      !DOCUMENTID type BAPISRMREC-DOCID
      !DESCRIPTION type BAPISRMREC-DESCR
      !MODEL_SPS_ID type BAPISRMREC-SPSID
      !MODEL type BAPISRMREC-DOCID .
  class-methods GET_PROP_ST
    importing
      !OBJECTID type BAPIGUID optional
      !DOCUMENTCLASS type BAPIDCLASS optional
      !RMS_ID type BAPISRMREC-RMSID optional
      !SPS_ID type BAPISRMREC-SPSID optional
      !CLAVE type ANY optional
      !PROPIEDAD type ANY default ''
    exporting
      !I_PROP type BAPIPROPT
    returning
      value(VALOR) type BAPIPROPVA .
protected section.
private section.

  data RMS_ID type BAPISRMREC-RMSID .
  data SPS_ID type BAPISRMREC-SPSID .
  data O_DOCUMENTO type ref to IF_SRM_SP_DOCUMENT .
endclass. "ZCL_AP_RMS definition
class ZCL_AP_RMS implementation.
METHOD ADD_SEL.
  DATA l_prop TYPE bapipropqy.

  l_prop-propname = propname.
  l_prop-sign = sign.
  l_prop-option = option.
  l_prop-propval_lo = low.
  l_prop-propval_hi = high.
  APPEND l_prop TO i_property_selection.

ENDMETHOD.
method CONSTRUCTOR.

  me->rms_id = rms_id.
  me->sps_id = sps_id.

endmethod.
METHOD crear_doc.

  CALL FUNCTION 'BAPI_RECORD_CREATE'
    EXPORTING
      rms_id                  = me->rms_id
      sps_id                  = me->sps_id
      documentid              = documentid
      description             = description
      model_sps_id            = model_sps_id
      model                   = model
      documentid_check_unique = 'X'
    IMPORTING
      return                  = return
      objectid                = objectid
      documentclass           = docclass.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.


ENDMETHOD.
METHOD GET_DATOS_CLASE.

  CALL FUNCTION 'SDOK_CLASS_PROPERTIES_GET'
    EXPORTING
      class                = clase
* IMPORTING
*   CLASS_TYPE           =
TABLES
   properties           = i_propiedades
 EXCEPTIONS
   not_existing         = 1
   not_authorized       = 2
   OTHERS               = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDMETHOD.
METHOD GET_DOC_TIPO_DESCRIPCION.
  DATA: lif_sp_obj         TYPE REF TO if_srm_sp_object,
        lif_poid           TYPE REF TO if_srm_poid,
        lif_sps            TYPE REF TO if_srm_sps_registry.

  CLEAR descripcion.

  IF NOT objectid IS INITIAL.
    me->o_documento = get_documento( objectid = objectid
                                     documentclass = documentclass ).
  ENDIF.

  lif_sp_obj ?= me->o_documento.

  lif_poid = lif_sp_obj->get_poid( ).

  lif_sps = lif_poid->get_sps_registry( ).
  descripcion = lif_sps->get_display_name( ).

ENDMETHOD.
METHOD GET_DOCUMENTO.
  DATA: l_version   TYPE bapisrmdoc-version,
        doc_context TYPE  bapidoccontext.

  l_version = '0'.
  PERFORM get_content_connection IN PROGRAM saplsrm_bapi_document
    USING objectid documentclass l_version doc_context
    CHANGING o_documento.
  me->o_documento = o_documento.

ENDMETHOD.
METHOD GET_ELEMENTS.
  DATA: l_objectid  TYPE bapiguid,
        l_documentclass	TYPE bapidclass.

  IF doc IS INITIAL.
    l_objectid = objectid.
    l_documentclass = documentclass.
  ELSE.
    l_objectid = doc-objectid.
    l_documentclass = doc-docclass.
  ENDIF.

  CLEAR: i_element, i_element_identification,
         i_element_properties, i_element_visibility,
         i_element_relations.

  CALL FUNCTION 'BAPI_RECORD_GETELEMENTS'
    EXPORTING
      objectid               = l_objectid
      documentclass          = l_documentclass
    TABLES
      element                = i_element
      element_identification = i_element_identification
      element_properties     = i_element_properties
      element_visibility     = i_element_visibility
      element_relations      = i_element_relations.


ENDMETHOD.
METHOD GET_LISTA.

  refresh i_lista. clear i_lista.

  CALL FUNCTION 'BAPI_RECORD_GETLIST'
    EXPORTING
      rms_id                   = rms_id
      sps_id                   = sps_id
*      max_hits                 = 1
* IMPORTING
*   RETURN                   =
    TABLES
      property_selection       = i_property_selection
      resulting_list           = i_lista.

ENDMETHOD.
METHOD GET_PROP.

  CLEAR valor.
  READ TABLE i_prop INTO prop WITH KEY name = propiedad.
  IF sy-subrc = 0.
    valor = prop-value.
  ENDIF.

ENDMETHOD.
METHOD get_prop_st.
  DATA: l_objectid           TYPE  bapiguid,
        l_documentclass      TYPE  bapidclass,
        i_property_selection TYPE TABLE OF bapipropqy,
        l_property_selection TYPE bapipropqy,
        i_list               TYPE TABLE OF bapidoctab,
        l_list               TYPE bapidoctab.

  CLEAR: i_prop, valor.

  IF NOT objectid IS INITIAL.
    l_objectid = objectid.
    l_documentclass = documentclass.
  ELSE.
    l_property_selection-propname = 'SRM_DOCUMENT_ID'.
    l_property_selection-sign = 'I'.
    l_property_selection-option = 'EQ'.
    l_property_selection-propval_lo = clave.
    APPEND l_property_selection TO i_property_selection.

    CALL FUNCTION 'BAPI_RECORD_GETLIST'
      EXPORTING
        rms_id             = rms_id
        sps_id             = sps_id
        max_hits           = 1
* IMPORTING
*       RETURN             =
      TABLES
        property_selection = i_property_selection
        resulting_list     = i_list.
    READ TABLE i_list INTO l_list INDEX 1.
    IF sy-subrc = 0.
      l_objectid      = l_list-objectid.
      l_documentclass = l_list-docclass.
    ENDIF.
  ENDIF.

  CHECK NOT l_objectid IS INITIAL AND NOT l_documentclass IS INITIAL.

  CALL FUNCTION 'BAPI_RECORD_GETPROPERTIES'
    EXPORTING
      objectid      = l_objectid
      documentclass = l_documentclass
*     whole_document       = ''
*     DOC_CONTEXT   =
*IMPORTING
*     RETURN        =
    TABLES
      properties    = i_prop.

  IF NOT propiedad IS INITIAL.
    READ TABLE i_prop INTO DATA(prop) WITH KEY name = propiedad.
    IF sy-subrc = 0.
      valor = prop-value.
    ENDIF.
  ENDIF.

ENDMETHOD.
METHOD GET_PROPERTIES.
  DATA: l_objectid  TYPE bapiguid,
        l_documentclass	TYPE bapidclass.

  IF not objectid IS INITIAL.
    l_objectid = objectid.
    l_documentclass = documentclass.
  ELSEIF NOT element IS INITIAL.
    l_documentclass = element(10).
    l_objectid = element+10.
  ELSEif not doc is initial.
    l_objectid = doc-objectid.
    l_documentclass = doc-docclass.
  ENDIF.

  CLEAR i_prop.
  CALL FUNCTION 'BAPI_RECORD_GETPROPERTIES'
    EXPORTING
      objectid             = l_objectid
      documentclass        = l_documentclass
      whole_document       = WHOLE_DOCUMENT
*    DOC_CONTEXT          =
*IMPORTING
*  RETURN               =
    TABLES
      properties           = i_prop.

ENDMETHOD.
METHOD SET_DOC_PROPERTIES.
  DATA: l_objectid  TYPE bapiguid,
        l_documentclass	TYPE bapidclass.

  IF NOT objectid IS INITIAL.
    l_objectid = objectid.
    l_documentclass = documentclass.
  ELSEIF NOT element IS INITIAL.
    l_documentclass = element(10).
    l_objectid = element+10.
  ELSEIF NOT doc IS INITIAL.
    l_objectid = doc-objectid.
    l_documentclass = doc-docclass.
  ENDIF.

  CALL FUNCTION 'BAPI_SRM_DOC_CHANGEPROPERTIES'
    EXPORTING
          objectid             =  l_objectid
          documentclass        =  l_documentclass
*   WHOLE_DOCUMENT       = ''
*   DO_COMMIT            =
*   DOC_CONTEXT          =
        IMPORTING
          return               = return
        TABLES
          properties           = i_properties
*       PROPS_DELETE         =
                .

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDMETHOD.
METHOD set_properties.
  DATA: l_objectid      TYPE bapiguid,
        l_documentclass	TYPE bapidclass.

  IF NOT objectid IS INITIAL.
    l_objectid = objectid.
    l_documentclass = documentclass.
  ELSEIF NOT element IS INITIAL.
    l_documentclass = element(10).
    l_objectid = element+10.
  ELSEIF NOT doc IS INITIAL.
    l_objectid = doc-objectid.
    l_documentclass = doc-docclass.
  ENDIF.

  CALL FUNCTION 'BAPI_RECORD_CHANGEPROPERTIES'
    EXPORTING
      objectid             = l_objectid
      documentclass        = l_documentclass
*     whole_document       = 'X'
      omit_authority_check = 'X'
*     DOC_CONTEXT          =
    IMPORTING
      return               = return
    TABLES
      properties           = i_properties
      props_delete         = i_props_delete.
*
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDMETHOD.
METHOD ver_st.
  DATA: l_objectid           TYPE  bapiguid,
        l_documentclass      TYPE  bapidclass,
        i_property_selection TYPE TABLE OF bapipropqy,
        l_property_selection TYPE bapipropqy,
        i_list               TYPE TABLE OF bapidoctab,
        l_list               TYPE bapidoctab.

  IF NOT objectid IS INITIAL.
    l_objectid = objectid.
    l_documentclass = documentclass.
  ELSE.
    l_property_selection-propname = 'SRM_DOCUMENT_ID'.
    l_property_selection-sign = 'I'.
    l_property_selection-option = 'EQ'.
    l_property_selection-propval_lo = clave.
    APPEND l_property_selection TO i_property_selection.

    CALL FUNCTION 'BAPI_RECORD_GETLIST'
      EXPORTING
        rms_id             = rms_id
        sps_id             = sps_id
        max_hits           = 1
* IMPORTING
*       RETURN             =
      TABLES
        property_selection = i_property_selection
        resulting_list     = i_list.
    READ TABLE i_list INTO l_list INDEX 1.
    IF sy-subrc = 0.
      l_objectid      = l_list-objectid.
      l_documentclass = l_list-docclass.
    ENDIF.
  ENDIF.

  CHECK NOT l_objectid IS INITIAL AND NOT l_documentclass IS INITIAL.

  CALL FUNCTION 'SRM_RECORD_DISPLAY'
    EXPORTING
      objectid        = l_objectid
      documentclass   = l_documentclass
*     MODIFY          = ' '
*     DOC_CONTEXT     =
    EXCEPTIONS
      not_authorized  = 1
      internal_error  = 2
      parameter_error = 3
      not_found       = 4
      OTHERS          = 5.

ENDMETHOD.
