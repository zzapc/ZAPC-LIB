
class ZCL_AP_EXCEL_OI definition
  public
  create public .

public section.

  data I_TIPOS_REGISTRADOS type SOI_DOCUMENT_TYPE_DESCR_LIST .
  data TIPO_REGISTRADO type SOI_DOCUMENT_TYPE_DESCR .
  data O_CONTROL type ref to I_OI_CONTAINER_CONTROL .
  data O_ERROR type ref to I_OI_ERROR .
  data O_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  data O_DOC type ref to I_OI_DOCUMENT_PROXY .

  methods CREA_INSTANCIA
    importing
      !CONTENEDOR type ANY
      !TIPO type ANY default 'Excel.Sheet'
      !INPLACE_ENABLED type C default 'X'
      !INPLACE_MODE type I default 0
      !INPLACE_RESIZE_DOCUMENTS type C default ' '
      !INPLACE_SCROLL_DOCUMENTS type C default 'X'
      !INPLACE_SHOW_TOOLBARS type C default 'X'
    returning
      value(MESSAGE) type BAPI_MSG .
  methods GET_MESSAGE_FROM_O_ERROR
    returning
      value(MESSAGE) type BAPI_MSG .
  methods GET_FILE
    importing
      !FICHERO type ANY
    returning
      value(MESSAGE) type BAPI_MSG .
protected section.
private section.
endclass. "ZCL_AP_EXCEL_OI definition
class ZCL_AP_EXCEL_OI implementation.
  METHOD crea_instancia.

    CALL METHOD c_oi_container_control_creator=>get_container_control
      IMPORTING
        control = o_control
        error   = o_error.
    IF o_error->error_code NE 'OK'.
      message = get_message_from_o_error( ).
      EXIT.
    ENDIF .

    CREATE OBJECT o_container
      EXPORTING
        container_name              = contenedor
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      EXIT.
    ENDIF.

    FREE o_error.

    CALL METHOD o_control->init_control
      EXPORTING
        r3_application_name      = sy-cprog
        parent                   = o_container
        inplace_enabled          = inplace_enabled
        inplace_show_toolbars    = inplace_show_toolbars
        inplace_scroll_documents = inplace_scroll_documents
      IMPORTING
        error                    = o_error
      EXCEPTIONS
        javabeannotsupported     = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      EXIT.
    ENDIF.
    IF o_error->error_code NE 'OK'.
      message = get_message_from_o_error( ).
      EXIT.
    ENDIF .

    FREE o_error.
    CALL METHOD o_control->get_registered_doc_types
      EXPORTING
        interface_type = 'E'
*       no_flush       = ' '
      IMPORTING
        descr_list     = i_tipos_registrados
        error          = o_error.
    IF o_error->error_code NE 'OK'.
      message = get_message_from_o_error( ).
      EXIT.
    ENDIF .

    IF NOT tipo IS INITIAL.
      DELETE i_tipos_registrados WHERE NOT document_type CS 'Excel.Sheet'.
    ENDIF.

    FREE o_error.
    READ TABLE i_tipos_registrados INTO tipo_registrado INDEX 1.
    IF sy-subrc EQ 0.
      CALL METHOD o_control->get_document_proxy
        EXPORTING
          document_type      = tipo_registrado-document_type
          register_container = 'X'
        IMPORTING
          document_proxy     = o_doc
          error              = o_error.

      IF o_error->error_code NE 'OK'.
        message = get_message_from_o_error( ).
        EXIT.
      ENDIF .

    ENDIF.

  ENDMETHOD.
  METHOD get_file.
    DATA l_file TYPE text255.

    l_file = fichero.
    IF l_file(7) NE 'FILE://'.
      CONCATENATE 'FILE://' fichero INTO l_file.
    ENDIF.

    CALL METHOD o_doc->open_document
      EXPORTING
*       document_title   = ' '
        document_url  = l_file
        open_inplace  = 'X'
        open_readonly = 'X'
      IMPORTING
        error         = o_error.

    IF o_error->error_code NE 'OK'.
      message = get_message_from_o_error( ).
      EXIT.
    ENDIF .

  ENDMETHOD.
  METHOD get_message_from_o_error.

    CHECK NOT o_error IS INITIAL.

    o_error->get_message( IMPORTING message_id     = sy-msgid
                                    message_number = sy-msgno
                                    param1         = sy-msgv1
                                    param2         = sy-msgv2
                                    param3         = sy-msgv3
                                    param4         = sy-msgv4
                            ).
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.

  ENDMETHOD.
