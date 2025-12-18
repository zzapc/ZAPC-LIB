class ZCL_AP_SPOOL definition
  public
  create public .

public section.

  data PRINT_PARAMETERS type PRI_PARAMS .
  data ARCHI_PARAMETERS type ARC_PARAMS .
  data TIME type TSP01-RQCRETIME .

  methods CONSTRUCTOR .
  methods GET_PARAMETERS
    importing
      !DEJAR_EN_SPOOL type ABAP_BOOL default 'X' .
  methods GET_HORA_IMPRESION .
  methods GET_SPOOL_FROM_HORA
    importing
      !UNAME type SY-UNAME default SY-UNAME
    returning
      value(RQIDENT) type TSP01-RQIDENT .
  class-methods GET_TXT_FROM_SPOOL
    importing
      !RQIDENT type ANY
    returning
      value(I_TEXTO) type TTTEXT255 .
  class-methods VER_SPOOL
    importing
      !RQIDENT type ANY .
  class-methods SAVE_PDF_FROM_SPOOL
    importing
      !RQIDENT type TSP01-RQIDENT
      !FICHERO type STRING .
  class-methods GET_CSV_FROM_SPOOL
    importing
      !RQIDENT type ANY
    returning
      value(I_TEXTO) type LIST_STRING_TABLE .
protected section.
private section.
endclass. "ZCL_AP_SPOOL definition
class ZCL_AP_SPOOL implementation.
method CONSTRUCTOR.

  get_parameters( ).

endmethod.
METHOD get_csv_from_spool.
  DATA: l_rqident TYPE tsp01-rqident,
        listobject TYPE  table_abaplist.

  l_rqident = rqident.

  SUBMIT rspolist EXPORTING LIST TO MEMORY AND RETURN
                  WITH rqident = l_rqident.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = listobject
    EXCEPTIONS
      not_found  = 1.

  CALL FUNCTION 'LIST_TO_DAT'
    EXPORTING
      list_index         = -1
    CHANGING
      listobject         = listobject
      listdat            = i_texto
    EXCEPTIONS
      empty_list         = 1
      list_index_invalid = 2.

ENDMETHOD.
method GET_HORA_IMPRESION.
  DATA: l_fecha LIKE sy-datum,
        l_hora LIKE sy-uzeit.

  l_fecha = sy-datum.
  l_hora  = sy-uzeit.

  CALL FUNCTION 'BDL_GET_GREENWICH_TIME'
    IMPORTING
      tag     = l_fecha
      uhrzeit = l_hora.

  CONCATENATE l_fecha l_hora '00' INTO time.

endmethod.
method GET_PARAMETERS.
  DATA: valid_flag(1) TYPE c.

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      no_dialog              = 'X'
    IMPORTING
      out_parameters         = print_parameters
      out_archive_parameters = archi_parameters
      valid                  = valid_flag
    EXCEPTIONS
      invalid_print_params   = 2
      OTHERS                 = 4.

  IF dejar_en_spool = 'X'.
    print_parameters-primm = ''. "Impresión inmediata
    print_parameters-prrel = ''.  "Borrar tras salida
    print_parameters-prnew = 'X'. "Orden de spool nueva
    print_parameters-armod = '1'. "Imprimir.
  ENDIF.


endmethod.
METHOD get_spool_from_hora.

  CLEAR rqident.
  SELECT rqident FROM tsp01                             "#EC CI_NOFIRST
    INTO rqident
   WHERE rqclient = sy-mandt
     AND rqowner  = uname
     AND rqcretime >= time.
  ENDSELECT.

ENDMETHOD.
METHOD get_txt_from_spool.
  DATA l_rqident TYPE tsp01-rqident.

  l_rqident = rqident.

  CALL FUNCTION 'RSPO_RETURN_ABAP_SPOOLJOB'
    EXPORTING
      rqident              = l_rqident
    TABLES
      buffer               = i_texto
    EXCEPTIONS
      no_such_job          = 1
      not_abap_list        = 2
      job_contains_no_data = 3
      selection_empty      = 4
      no_permission        = 5
      can_not_access       = 6
      read_error           = 7
      OTHERS               = 8.

ENDMETHOD.
method SAVE_PDF_FROM_SPOOL.
  DATA: l_bytecount TYPE tst01-dsize,
        i_pdf TYPE TABLE OF tline.

  CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
    EXPORTING
      src_spoolid              = rqident
      no_dialog                = ' '
      dst_device               = 'LOCL'
    IMPORTING
      pdf_bytecount            = l_bytecount
    TABLES
      pdf                      = i_pdf
    EXCEPTIONS
      err_no_abap_spooljob     = 1
      err_no_spooljob          = 2
      err_no_permission        = 3
      err_conv_not_possible    = 4
      err_bad_destdevice       = 5
      user_cancelled           = 6
      err_spoolerror           = 7
      err_temseerror           = 8
      err_btcjob_open_failed   = 9
      err_btcjob_submit_failed = 10
      err_btcjob_close_failed  = 11
      OTHERS                   = 12.

  IF sy-subrc = 0.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = l_bytecount
        filename                = fichero
        filetype                = 'BIN'
      TABLES
        data_tab                = i_pdf
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
  ENDIF.

endmethod.
method VER_SPOOL.
data l_spool type COM_SEARCH_TV_LAST_SPOOL.

  l_spool = rqident.
  CALL FUNCTION 'COM_SE_SPOOL_DISPLAY'
    EXPORTING
      iv_spool_no = l_Spool.

endmethod.
