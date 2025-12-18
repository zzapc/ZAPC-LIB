types: tt_excel type table of kcde_cells.
class ZCL_AP_IMPORT_EXCEL definition
  public
  final
  create public .

public section.

  data EXCEL type KCDE_CELLS .

  class-methods IMPORT_XLS
    importing
      !I_FILENAME type LOCALFILE
      !I_STRUCTURE type TABNAME
    exporting
      !E_TB_DATA type DATA
    exceptions
      ERROR_IMPORT_EXCEL .
  class-methods IMPORTAR
    importing
      !FICHERO type ANY default ''
      !LINE_HEADER type ABAP_BOOL default 'X'
      !POPUP_SELECT_FILE type ABAP_BOOL default ''
    changing
      !TABLA type TABLE
    exceptions
      ERROR_IMPORT_EXCEL .
  methods LEER
    importing
      !FICHERO type ANY
      !COL_INI type I default 1
      !ROW_INI type I default 1
      !COL_FIN type I default 10
      !ROW_FIN type I default 1000 .
  methods GET_MAX_ROW
    returning
      value(MAX_ROW) type I .
  methods GET_VALOR
    importing
      !FILA type ANY
      !COLUMNA type ANY
      !COL_AGRUPADA type ABAP_BOOL default ''
    returning
      value(VALOR) type KCD_TEXT32 .
  methods SEARCH_COLUMNA
    importing
      !VALOR type ANY
      !ROW type KCD_EX_ROW_N
    returning
      value(COL) type I .
  methods SEARCH_FILA
    importing
      !VALOR type ANY
      !COL type KCD_EX_COL_N
    returning
      value(ROW) type I .
  class-methods EXPORTAR_TABLA
    importing
      !TABLA type ANY
      !FICHERO type STRING .
  class-methods IMPORTAR_CSV
    importing
      !FICHERO type ANY default ''
      !LINE_HEADER type ABAP_BOOL default 'X'
      !POPUP_SELECT_FILE type ABAP_BOOL default ''
      !SERVIDOR type ABAP_BOOL default ''
      !FIELD_SEPARATOR type CHAR1 default ';'
      !FILEFORMAT type TRUXS_FILEFORMAT default 'CSV'
      !CODEPAGE type ANY default ''
      !UNIR_SI_NO_SEPARADOR type ABAP_BOOL default ''
    exporting
      !TABLA type TABLE
      !MESSAGE type BAPI_MSG
    exceptions
      ERROR_IMPORT_EXCEL .
  class-methods FILE_READ_AND_CONVERT_SAP_DATA
    importing
      !I_FILENAME type ANY
      !I_SERVERTYP type TRUXS_SERVER default 'APP'
      !I_FILEFORMAT type TRUXS_FILEFORMAT default 'CSV'
      !I_FIELD_SEPERATOR type CHAR01 default ';'
      !I_LINE_HEADER type CHAR01 default ''
      !CODEPAGE type ANY default ''
      !UNIR_SI_NO_SEPARADOR type ABAP_BOOL default ''
    exporting
      !E_BIN_FILELENGTH type INT4
      !I_TAB_RECEIVER type TABLE
    exceptions
      FILE_NOT_FOUND
      CLOSE_FAILED
      AUTHORIZATION_FAILED
      OPEN_FAILED
      CONVERSION_FAILED .
  class-methods FILE_GET_NAME
    importing
      !CLIENT type SY-MANDT default SY-MANDT
      !LOGICAL_FILENAME type ANY
      !PARAMETER_1 type ANY default ''
      !PARAMETER_2 type ANY default ''
      !PARAMETER_3 type ANY default ''
      !USE_PRESENTATION_SERVER type ANY default ''
      !WITH_FILE_EXTENSION type ANY default ''
      !USE_BUFFER type ANY default ''
      !ELEMINATE_BLANKS type SY-DATAR default SY-DATAR
      !INCLUDING_DIR type SY-DATAR default SY-DATAR
    exporting
      !EMERGENCY_FLAG type ANY
      !FILE_FORMAT type FILENAME-FILEFORMAT
      !FILE_NAME type ANY
    changing
      !OPERATING_SYSTEM type SY-OPSYS default SY-OPSYS
    exceptions
      FILE_NOT_FOUND .
  class-methods LINEA_CSV_2_DATOS_INT
    importing
      !FIELD_SEPARATOR type CHAR1 default ';'
      !LINEA type ANY
    exporting
      !DATOS type ANY
      !MESSAGE type BAPI_MSG .
  class-methods INPUT_DATA2SAP_DATA
    changing
      !SUBRC type ANY
      !TARGET type ANY
      !SOURCE type ANY .
  class-methods PREPARE_NUMBER
    changing
      !PARAMETER type ANY
      !DECIMAL_POS type I .
  class-methods LINEA_CSV_2_DATOS
    importing
      !FIELD_SEPARATOR type CHAR1 default ';'
      !LINEA type ANY
    exporting
      !DATOS type ANY
      !MESSAGE type BAPI_MSG .
  class-methods LINEA_DATOS_2_CSV
    importing
      !FIELD_SEPARATOR type CHAR1 default ';'
      !DATOS type ANY
    exporting
      !LINEA type ANY
      !MESSAGE type BAPI_MSG .
protected section.
private section.

  data I_EXCEL type TT_EXCEL .
endclass. "ZCL_AP_IMPORT_EXCEL definition
class ZCL_AP_IMPORT_EXCEL implementation.
method EXPORTAR_TABLA.
  DATA: v_default_file_name TYPE string,
        v_filename          TYPE string,
        v_file_path         TYPE string,
        wa_table            TYPE dd02l,
        check_path          TYPE string,
        v_select            TYPE string,
        t_fieldcat          TYPE lvc_t_fcat,
        v_xml_version       TYPE string,
        v_xml_flavour       TYPE string,
        v_xstring           TYPE xstring,
        v_size              TYPE i,
        gt_bintab           TYPE solix_tab.

  DATA: r_data        TYPE REF TO data,
        r_structdescr TYPE REF TO cl_abap_structdescr,
        r_table       TYPE REF TO cl_salv_table,
        r_columns     TYPE REF TO cl_salv_columns_table,
        r_aggreg      TYPE REF TO cl_salv_aggregations,
        r_result_data TYPE REF TO cl_salv_ex_result_data_table.

  FIELD-SYMBOLS: <table> TYPE ANY TABLE,
                 <fs_component> TYPE abap_compdescr.

  CREATE DATA r_data TYPE STANDARD TABLE OF (tabla).
  ASSIGN r_data->* TO <table>.

* Get all columns for select
  r_structdescr ?= cl_abap_structdescr=>describe_by_name( tabla ).
  IF r_structdescr IS BOUND.
    LOOP AT r_structdescr->components[] ASSIGNING <fs_component>.
      CONCATENATE v_select <fs_component>-name INTO v_select SEPARATED BY space.
    ENDLOOP.
  ENDIF.

* Select all data
  SELECT (v_select) FROM (tabla) INTO TABLE <table>.

  TRY.
      cl_salv_table=>factory(
      EXPORTING
        list_display = abap_false
      IMPORTING
        r_salv_table = r_table
      CHANGING
        t_table     = <table> ).
    CATCH cx_salv_msg.
  ENDTRY.

* Get columns and aggregation to create fieldcatalog
  r_columns  = r_table->get_columns( ).
  r_aggreg   = r_table->get_aggregations( ).
  t_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                r_columns     = r_columns
                                r_aggregations = r_aggreg ).

* Create result data table
  IF cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_25 OR
     cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_26.

    r_result_data = cl_salv_ex_util=>factory_result_data_table(
        r_data                     = r_data
        t_fieldcatalog             = t_fieldcat
    ).

* Get XML version
    CASE cl_salv_bs_a_xml_base=>get_version( ).
      WHEN if_salv_bs_xml=>version_25.
        v_xml_version = if_salv_bs_xml=>version_25.
      WHEN if_salv_bs_xml=>version_26.
        v_xml_version = if_salv_bs_xml=>version_26.
    ENDCASE.

* Get XML flavour
    v_xml_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export.

* Create excel data
    CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
      EXPORTING
        xml_type      = if_salv_bs_xml=>c_type_mhtml
        xml_version   = v_xml_version
        r_result_data = r_result_data
        xml_flavour   = v_xml_flavour
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = v_xstring.
  ENDIF.

  IF v_xstring IS NOT INITIAL.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = v_xstring
      IMPORTING
        output_length = v_size
      TABLES
        binary_tab    = gt_bintab.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize            = v_size
        filename                = fichero
        filetype                = 'BIN'
      CHANGING
        data_tab                = gt_bintab
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
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
*  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

endmethod.
method FILE_GET_NAME.

  data: len            type i,                              "#EC NEEDED
        work_path(255) type c,
        work_file(255) type c,
        work_fnam(255) type c,
        param_1(255)   type c,
        param_2(255)   type c,
        param_3(255)   type c.

  data: begin of i_filename.
          include type filename.
  data: end   of i_filename.

  data: begin of i_path.
          include type path.
  data: end   of i_path.

  data: begin of i_opsystem.
          include type opsystem.
  data: end   of i_opsystem.

  data: begin of i_filesys.
          include type filesys.
  data: end   of i_filesys.

  data  ld_pres_server_platform   type i.

  field-symbols: <string> type any.

* in den folgenden Strukturen werden die zuletzt aus der DB eingelesenen
* Daten gepuffert, bei rekurs. Aufrufen des FBs werden sie modifiziert
  statics: begin of buf_filename.
          include type filename.
  statics: end   of buf_filename.

  statics: begin of buf_path.
          include type path.
  statics: end   of buf_path.

  statics: begin of buf_opsystem.
          include type opsystem.
  statics: end   of buf_opsystem.

  statics: begin of buf_filesys.
          include type filesys.
  statics: end   of buf_filesys.


*---------------------------------------------------------------------
*    Initialisierung
*---------------------------------------------------------------------

* Rückgabeparameter initializieren
  clear: emergency_flag,
         file_format,
         file_name.

* fülle die Arbeitsbereiche mit dem Puffer
  i_filename = buf_filename.
  i_path     = buf_path.
  i_opsystem = buf_opsystem.
  i_filesys  = buf_filesys.

*---------------------------------------------------------------------
*    Ermittlung des phys. Dateinamens
*---------------------------------------------------------------------

* phys. Dateinamen, Dateianhang und logischen Dateipfad besorgen
  if    logical_filename ne i_filename-fileintern
     or client           ne i_filename-mandt
     or use_buffer       eq space.
    select single * from filename client specified
                    into i_filename
                    where mandt      eq client
                    and   fileintern eq logical_filename.
    if sy-subrc ne 0.
      data filenameci type filenameci.
      select single * from filenameci into filenameci
                      where fileintern eq logical_filename.
      if sy-subrc ne 0.
        message e001(sg)
          with logical_filename
          raising file_not_found.
      else.
        move-corresponding filenameci to i_filename.
        move client to i_filename-mandt.
      endif.
    endif.
  endif.

* begin "n1497003
  if  i_filename-fileformat = 'DIR'
  and including_dir = abap_false.
    message e001(sg)
      with logical_filename
      raising file_not_found.
  endif.
* end "n1497003

  work_file = i_filename-fileextern.

* Laufzeitvariablen in Phys. Dateinamen ersetzen
  param_1 = parameter_1.
  param_2 = parameter_2.
  param_3 = parameter_3.
  perform replace_parameters_file in PROGRAM SAPLSFIL
    using
      param_1
      param_2
      param_3
    changing
      work_file.

  perform eleminate_blanks in PROGRAM SAPLSFIL
    using
      work_file
      eleminate_blanks.

* Verarbeitung beenden, wenn kein logischer Pfad vorhanden
  if i_filename-pathintern eq space.

    file_name   = work_file.
    file_format = i_filename-fileformat.

    exit.
  endif.

*---------------------------------------------------------------------
*    Ermittlung des phys. Verzeichnisses in symbolischer Form
*---------------------------------------------------------------------

* Syntaxgruppe des Betriebssystems besorgen
  if use_presentation_server ne space.
    call method cl_gui_frontend_services=>get_platform
      receiving
        platform             = ld_pres_server_platform
      exceptions
        error_no_gui         = 1
        cntl_error           = 2
        not_supported_by_gui = 3
        others               = 4.
    if sy-subrc ne 0.
      raise file_not_found.
    endif.
    case ld_pres_server_platform.
      when 1.
        operating_system = 'WN32_95'.
      when 2.
        operating_system = 'WN32_98'.
      when 3 or 4 or 5 or 14 or 15 or 16 or 17. "Windows all versions
        operating_system = 'WN32'.
      when 6 or 13.                                         " MAC
        operating_system = 'MC' .
      when 7.                                               " OS/2
        operating_system = 'PM'.
      when 8 or 9 or 10 or 11 or 12.                      " UNIX/LINUX
        operating_system = 'MF'.
    endcase.
  endif.
  if i_opsystem-opsys ne operating_system.
    select  single *
      from  opsystem
      into  i_opsystem
      where opsys eq operating_system.
    if  sy-subrc           ne 0
     or i_opsystem-filesys eq space.
      perform emergency_action in PROGRAM SAPLSFIL
        using
          work_file
          operating_system
          including_dir                                     "n1497003
        changing
          work_fnam.

      perform eleminate_blanks in PROGRAM SAPLSFIL
        using
          work_fnam
          eleminate_blanks.

      file_name      = work_fnam.
      file_format    = i_filename-fileformat.
      emergency_flag = 'X'.


      exit.                    "<--- Leave this function module
    endif.
  endif.

* plattformspezifischen Pfad besorgen
  if use_buffer ne space.
    if i_path-pathintern ne i_filename-pathintern
    or i_path-filesys    ne i_opsystem-filesys.
      select  single *
        from  path
        into  i_path
        where pathintern eq i_filename-pathintern
        and   filesys    eq i_opsystem-filesys.
      if sy-subrc ne 0.
        perform emergency_action in PROGRAM SAPLSFIL
          using
            work_file
            operating_system
            including_dir                                   "n1497003
          changing
            work_fnam.

        perform eleminate_blanks in PROGRAM SAPLSFIL
          using
            work_fnam
            eleminate_blanks.

        file_name      = work_fnam.
        file_format    = i_filename-fileformat.
        emergency_flag = 'X'.


        exit.                    "<--- Leave this function module
      endif.
    endif.
  else.
    select  single *
      from  path
      into  i_path
      where pathintern eq i_filename-pathintern
      and   filesys    eq i_opsystem-filesys.
    if sy-subrc ne 0.
      perform emergency_action in PROGRAM SAPLSFIL
        using
          work_file
          operating_system
          including_dir                                     "n1497003
        changing
          work_fnam.

      perform eleminate_blanks in PROGRAM SAPLSFIL
        using
          work_fnam
          eleminate_blanks.


      file_name      = work_fnam.
      file_format    = i_filename-fileformat.
      emergency_flag = 'X'.




      exit.                    "<--- Leave this function module
    endif.
  endif.
  work_path = i_path-pathextern.

*---------------------------------------------------------------------
*    Plattformspezifische Bildung des gesamten Pfadnamens
*---------------------------------------------------------------------

* plattformspezifische Parameter besorgen
  if i_filesys-filesys ne i_opsystem-filesys.
    select  single *
      from  filesys
      into  i_filesys
      where filesys eq i_opsystem-filesys.
    if sy-subrc ne 0.  "Unbekannte Syntaxgruppe
      clear i_filesys.
      i_filesys-filesys   = i_opsystem-filesys.
      i_filesys-fsysactiv = 'X'.
    endif.
    if i_filesys-namelen eq 0.
      i_filesys-namelen = 50.
    endif.
  endif.

* Dateiname ab der maximalen Länge abschneiden
  assign work_file(i_filesys-namelen) to <string>.
  work_file = <string>.

  if including_dir = abap_false.                            "n1497003
* Dateianhang erzeugen
    if  with_file_extension   ne space
    and i_filename-fileformat ne space
    and i_filesys-fileext     ne space.
      concatenate work_file '.' i_filename-fileformat
        into work_file.
    endif.
  endif.                                                    "n1497003

* Laufzeitvariablen in phys. Pfad ersetzen
  perform replace_parameters_path in PROGRAM SAPLSFIL
    using
      work_file
      param_1
      param_2
      param_3
    changing
      work_path.

  file_format = i_filename-fileformat.

  perform eleminate_blanks in PROGRAM SAPLSFIL
    using
      work_path
      eleminate_blanks.


  file_name   = work_path.

*---------------------------------------------------------------------
*    Abschlußverarbeitung
*---------------------------------------------------------------------

* aktualisiere Puffer: i_filename, buf_filename etc. evtl. modifiziert
  buf_filename = i_filename.
  buf_path     = i_path.
  buf_opsystem = i_opsystem.
  buf_filesys  = i_filesys.
endmethod.
method FILE_READ_AND_CONVERT_SAP_DATA.
  CONSTANTS:
    c_ole_header       TYPE tfdir-funcname VALUE 'LOAD_',
    c_fm_header        TYPE tfdir-funcname VALUE 'TEXT_CONVERT_',
    c_fm_trailer       TYPE tfdir-funcname VALUE '_TO_SAP',
    c_file_type_binary TYPE  rlgrap-filetype VALUE 'BIN',
    c_file_type_xml    TYPE  rlgrap-filetype VALUE 'XML',
    c_file_type_ascii  TYPE  rlgrap-filetype VALUE 'ASC'.
  DATA:
    l_type,
    l_component       TYPE i,
    l_sys_cp          TYPE tcp00-cpcodepage, "system codepage
    l_lan_cp          TYPE tcp00-cpcodepage, "language codepage
    l_tcp00           TYPE tcp00,
    i_tab_input_data  TYPE truxs_t_text_data,
    i_tab_xinput_data TYPE truxs_xml_table,
    s_input_data      LIKE LINE OF i_tab_input_data,
    l_fm_name         TYPE tfdir-funcname,
    l_tfdir           TYPE tfdir,
    l_text80(80)      TYPE c,
    l_return(40)      TYPE c,
    l_file_format     TYPE filename-fileformat,
    l_totalsize       TYPE i,
    l_file_name       TYPE rlgrap-filename.
  DATA: l_file_name_string TYPE string,
        l_file_format_10   TYPE char10,
        l_string           TYPE string.
  FIELD-SYMBOLS: <fs_itab> TYPE table.
  FIELD-SYMBOLS: <fs_struc> TYPE any.

  CASE i_fileformat.
    WHEN c_file_type_binary.
      ASSIGN i_tab_receiver[] TO <fs_itab>.
    WHEN c_file_type_xml.
      ASSIGN i_tab_xinput_data[] TO <fs_itab>.
    WHEN OTHERS.
      ASSIGN i_tab_input_data[] TO <fs_itab>.
  ENDCASE.

*  assign local copy of initial line of <fs_itab> to <fs_struc>.
  DATA: nueva_linea TYPE REF TO data.
  CREATE DATA nueva_linea LIKE LINE OF <fs_itab>.
  ASSIGN nueva_linea->* TO <fs_struc>.

* Get physical file name first
  file_get_name( EXPORTING logical_filename = i_filename
                 IMPORTING
      file_format      = l_file_format
      file_name        = l_file_name
    EXCEPTIONS
      OTHERS           = 4 ).

  IF sy-subrc <> 0.
    CLEAR: sy-msgid, sy-msgno, sy-msgty.
    IF l_file_format IS INITIAL.
      l_file_format = c_file_type_ascii.
    ELSE.
      IF i_fileformat = c_file_type_xml.
        l_file_format = c_file_type_binary.
      ELSE.
        l_file_format = c_file_type_ascii.
      ENDIF.
    ENDIF.
    l_file_name   = i_filename.
  ENDIF.

* Read data from application server
  IF i_servertyp = 'APP'.
*   check code page first
    CALL FUNCTION 'SYSTEM_CODEPAGE'
      IMPORTING
        codepage = l_sys_cp.
    SELECT SINGLE * FROM tcp00 INTO l_tcp00 WHERE cpcodepage = l_sys_cp.
    CALL FUNCTION 'SCP_CODEPAGE_FOR_LANGUAGE'
      EXPORTING
        language = sy-langu
      IMPORTING
        codepage = l_lan_cp
      EXCEPTIONS
        OTHERS   = 4.
    IF sy-subrc <> 0.
      l_lan_cp = l_sys_cp.
    ENDIF.

*   Handle the file now
    IF l_file_format = c_file_type_ascii.
      IF codepage IS INITIAL.
        OPEN DATASET l_file_name FOR INPUT IN TEXT MODE
                                       ENCODING DEFAULT.
      ELSE.
        OPEN DATASET l_file_name FOR INPUT IN LEGACY TEXT MODE CODE PAGE codepage.
      ENDIF.
    ELSE.
      OPEN DATASET l_file_name FOR INPUT IN BINARY MODE.
    ENDIF.
    CATCH SYSTEM-EXCEPTIONS file_access_errors = 4.
      IF sy-subrc <> 0.
        MESSAGE e890(ux) WITH l_file_name RAISING open_failed.
      ENDIF.
    ENDCATCH.
    CLEAR sy-subrc.
    WHILE sy-subrc = 0.
      READ DATASET l_file_name INTO <fs_struc>.
      CATCH SYSTEM-EXCEPTIONS file_access_errors = 4.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDCATCH.

      IF unir_si_no_separador IS INITIAL.
        APPEND <fs_struc> TO <fs_itab>.
      ELSE.
        IF l_string IS INITIAL.
          l_string = <fs_struc>.
        ELSE.
          CONCATENATE l_string cl_abap_char_utilities=>cr_lf <fs_struc> INTO l_string.
        ENDIF.
        IF zcl_ap_string=>ultimo_caracter( <fs_struc> ) = i_field_seperator.
          APPEND l_string TO <fs_itab>.
          CLEAR l_string.
        ENDIF.
      ENDIF.
      IF sy-batch IS INITIAL.
        l_text80 = sy-tabix.
        CONDENSE l_text80 NO-GAPS.
        CONCATENATE TEXT-red l_text80 INTO l_text80
                     SEPARATED BY space.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = 0
            text       = l_text80
          EXCEPTIONS
            OTHERS     = 0.
      ENDIF.
    ENDWHILE.
    IF i_fileformat <> c_file_type_binary AND
      i_fileformat <> c_file_type_xml.
      IF l_lan_cp <> l_sys_cp.
        DATA: ld_lan_cp TYPE abap_encod.
        ld_lan_cp = l_lan_cp.
        CALL FUNCTION 'TRANSLATE_CODEPAGE_IN'
          EXPORTING
            codepage_from = ld_lan_cp
          TABLES
            t_data        = <fs_itab>
          EXCEPTIONS
            OTHERS        = 0.

      ENDIF.
    ENDIF.

    CLOSE DATASET l_file_name.
    CATCH SYSTEM-EXCEPTIONS file_access_errors = 4.
      IF sy-subrc = 4.
        MESSAGE e891(ux) WITH l_file_name RAISING close_failed.
      ENDIF.
    ENDCATCH.
  ENDIF.

* Upload data from presentation server
  IF i_servertyp = 'PRS'.
*   CALL FUNCTION 'WS_UPLOAD'
    MOVE l_file_name TO l_file_name_string.
    MOVE l_file_format TO l_file_format_10.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename        = l_file_name_string
        filetype        = l_file_format_10
      IMPORTING
        filelength      = l_totalsize
      CHANGING
        data_tab        = <fs_itab>
      EXCEPTIONS
        file_read_error = 4
        file_open_error = 8
        OTHERS          = 8.

    CASE sy-subrc.
      WHEN 0.
      WHEN 4.
        MESSAGE e890(ux) WITH l_file_name RAISING file_not_found.
      WHEN OTHERS.
        MESSAGE e890(ux) WITH l_file_name RAISING file_not_found.
    ENDCASE.
  ENDIF.
* Upload data from OLE2 server
  IF i_servertyp = 'OLE2'.
    CALL FUNCTION 'WS_QUERY'
      EXPORTING
        query    = 'FL'
        filename = l_file_name
      IMPORTING
        return   = l_return
      EXCEPTIONS
        OTHERS   = 4.
    IF sy-subrc <> 0 OR l_return IS INITIAL OR l_return = '0'.
      MESSAGE e890(ux) WITH l_file_name RAISING file_not_found.
    ENDIF.
  ENDIF.

  CASE i_fileformat.
    WHEN space.
      i_tab_receiver[] = <fs_itab>.
    WHEN c_file_type_binary.
    WHEN c_file_type_ascii.
*     i_tab_receiver[] = i_tab_input_data[].
      CALL FUNCTION 'TEXT_CONVERT_TEX_TO_SAP'
        EXPORTING
          i_line_header        = i_line_header
          i_tab_raw_data       = i_tab_input_data[]
          i_filename           = l_file_name
        TABLES
          i_tab_converted_data = i_tab_receiver
        EXCEPTIONS
          OTHERS               = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                                           RAISING conversion_failed.
      ENDIF.
    WHEN OTHERS.
      IF i_fileformat = 'CSV' AND i_field_seperator NE ';'.
        CALL FUNCTION 'TEXT_CONVERT_TEX_TO_SAP'
          EXPORTING
            i_field_seperator    = i_field_seperator
            i_line_header        = i_line_header
            i_tab_raw_data       = <fs_itab>
            i_filename           =  l_file_name
          TABLES
            i_tab_converted_data = i_tab_receiver
          EXCEPTIONS
            OTHERS               = 4.

        if sy-subrc ne 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING conversion_failed.
        endif.
      ELSE.
        l_fm_name    = c_fm_header.
        l_fm_name+13 = i_fileformat.
        l_fm_name+23 = c_fm_trailer.
        CONDENSE l_fm_name NO-GAPS.
        SELECT SINGLE * FROM tfdir INTO l_tfdir
                                   WHERE funcname = l_fm_name.
        IF sy-subrc = 0.
          CALL FUNCTION l_fm_name
            EXPORTING
              i_fileformat         = i_fileformat
              i_field_seperator    = i_field_seperator
              i_line_header        = i_line_header
              i_tab_raw_data       = <fs_itab>
              i_filename           = l_file_name
              i_totalsize          = l_totalsize
            TABLES
              i_tab_converted_data = i_tab_receiver
            EXCEPTIONS
              OTHERS               = 4.
        ELSE.
          MESSAGE e046(fl) WITH l_fm_name RAISING conversion_failed.
        ENDIF.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                                             RAISING conversion_failed.
        ENDIF.
      ENDIF.
  ENDCASE.
  e_bin_filelength = l_totalsize.


endmethod.
method GET_MAX_ROW.

  DESCRIBE TABLE i_excel LINES sy-tfill.
  READ TABLE i_excel INTO excel INDEX sy-tfill.
  max_row = excel-row.


endmethod.
method GET_VALOR.
data: l_ROW	type KCD_EX_ROW_N,
      l_COL	type KCD_EX_COL_N.

  l_row = fila.
  zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_row ).
  l_col = columna.
  zcl_ap_string=>poner_ceros_c( CHANGING cadena = l_col ).

  clear valor.
  read table i_excel into excel with key row = l_row
                                         col = l_col
                                binary search.
  if sy-subrc = 0.
    valor = excel-value.
  else.
    if col_agrupada = 'X'.
      loop at i_excel into excel where row <= l_row
                                   and col = l_col.
        valor = excel-value.
      endloop.
    endif.
  endif.

endmethod.
method IMPORT_XLS.

* declarations
* dynamic internal table
  FIELD-SYMBOLS: <tb_data> TYPE STANDARD TABLE.
* variables excel import
  TYPE-POOLS: truxs.
  DATA:
    tb_data TYPE REF TO data,
    it_raw TYPE truxs_t_text_data.

* create dynamic table from structure name
  CREATE DATA tb_data TYPE TABLE OF (i_structure).
  ASSIGN tb_data->* TO <tb_data>.

* load excel file into internal table
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = it_raw       " work table
      i_filename           = i_filename
    TABLES
      i_tab_converted_data = <tb_data>    " excel data
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
* error check
  IF sy-subrc <> 0.
    IF NOT sy-msgno IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error_import_excel.
    ELSE.
      RAISE error_import_excel.
    ENDIF.
  ELSE.
    e_tb_data = <tb_data>.
  ENDIF.

endmethod.
method IMPORTAR.

  TYPE-POOLS: truxs.
  DATA: it_raw TYPE truxs_t_text_data,
        l_fichero TYPE localfile.

  IF popup_select_file = 'X'.
    l_fichero = zcl_ap_ficheros=>popup_select_fichero( file_filter = zcl_ap_ficheros=>c_filtro_xls ).
  else.
    l_fichero = fichero.
  endif.

  check not l_fichero is initial.

* load excel file into internal table
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = line_header
      i_tab_raw_data       = it_raw       " work table
      i_filename           = l_fichero
    TABLES
      i_tab_converted_data = tabla    " excel data
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
* error check
  IF sy-subrc <> 0.
    IF NOT sy-msgno IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error_import_excel.
    ELSE.
      RAISE error_import_excel.
    ENDIF.
  ENDIF.

endmethod.
method IMPORTAR_CSV.

  TYPE-POOLS: truxs.
  DATA: it_raw TYPE truxs_t_text_data,
        l_fichero TYPE localfile,
        l_server TYPE truxs_server.

  IF popup_select_file = 'X'.
    l_fichero = zcl_ap_ficheros=>popup_select_fichero(  ).
  ELSE.
    l_fichero = fichero.
  ENDIF.

  CHECK NOT l_fichero IS INITIAL.

*  sy-tfill = STRLEN( l_fichero ).
*  IF sy-tfill > 60.
*    MESSAGE 'Función limitada a ruta ficheros máx. 60 carácteres' TYPE 'E'.
*  ENDIF.

  IF servidor IS INITIAL.
    l_server = 'PRS'.
  ELSE.
    l_server = 'APP'.
  ENDIF.

*  CALL FUNCTION 'FILE_READ_AND_CONVERT_SAP_DATA'
  file_read_and_convert_sap_data(
   EXPORTING
     i_filename                 = l_fichero
     i_servertyp                = l_server
   i_fileformat               = fileformat
   i_field_seperator          = field_separator
   i_line_header              = line_header
   codepage                   = codepage
   unir_si_no_separador       = unir_si_no_separador
 IMPORTING
*   E_BIN_FILELENGTH           =
*    TABLES
     i_tab_receiver             = tabla
  EXCEPTIONS
    file_not_found             = 1
    close_failed               = 2
    authorization_failed       = 3
    open_failed                = 4
    conversion_failed          = 5
    OTHERS                     = 6 ).
* error check
  IF sy-subrc <> 0.
    IF NOT sy-msgno IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
    ELSE.
      CASE sy-subrc.
        WHEN 1. message = 'Fichero no encontrado'.
        WHEN 2. message = 'Error al cerrar fichero'.
        WHEN 3. message = 'No hay autorización para leer fichero'.
        WHEN 4. message = 'Error al abrir fichero'.
        WHEN 5. message = 'Error de conversión'.
        WHEN OTHERS. message = 'Error al procesar fichero'.
      ENDCASE.
    ENDIF.
  ENDIF.

endmethod.
METHOD input_data2sap_data.
  DATA: pi_field_type TYPE c.
  DATA: l_date TYPE sydatum.
  DATA: l_decimals TYPE i.
  DATA: l_decimals_target TYPE i.
  FIELD-SYMBOLS: <fs_type_x> TYPE x.

  DESCRIBE FIELD target TYPE pi_field_type.

  CLEAR subrc.

  CASE pi_field_type.
    WHEN 'C'.
      target = source.

    WHEN 'D'.
* Bitte beachten:
* Die Funktion CONVERT_DATE_TO_INTERNAL erwartet das Datum analog den
* Einstellungen in den Festwerten des Benutzers. Ist dort das Format
* TT.MM.JJJJ eingestellt, dann muss der Routine das Datum in der
* Form 31.12.1999 oder 31121999 übergeben werden.
* Ist bei den Benutzerfestwerten das amerikanische Format JJJJ.MM.TT
* eingestellt, dann erwartet die Funktion das Datum in der Form
* 1999.12.31 oder 19991231
* Einige Excel Datumsformate haben die Eigenart, zusätzliche Leerzeichen
* und Punkte in das Datum einzufügen. Deshalb werden Punkte in der
* DO Schleife entfernt und anschliessend noch die Leerzeichen durch
* den condense entfernt.
      DO 3 TIMES.
        REPLACE '.' WITH ' ' INTO source.
      ENDDO.
      CONDENSE source NO-GAPS.

      IF  NOT source IS INITIAL   "Space
      AND source <> '00.00.0000'
      AND source <> '00000000'.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external = source
          IMPORTING
            date_internal = target
          EXCEPTIONS
            error_message = 4
            OTHERS        = 4.
        IF sy-subrc <> 0.
          subrc = 4.
        ENDIF.
      ENDIF.

    WHEN 'T'.
      IF NOT source IS INITIAL AND source <> '00:00:00'.
        CALL FUNCTION 'CONVERT_TIME_INPUT'
          EXPORTING
            input         = source
          IMPORTING
            output        = target
          EXCEPTIONS
            error_message = 4
            OTHERS        = 4.
        IF sy-subrc <> 0.
          subrc = 4.
        ENDIF.
      ENDIF.
    WHEN 'X'.
      ASSIGN source TO <fs_type_x> .
      IF sy-subrc <> 0.
        subrc = 8.
      ENDIF.
      target = <fs_type_x> .
    WHEN 'N'.
      DESCRIBE FIELD target DECIMALS l_decimals_target.
      prepare_number( CHANGING PARAMETER = source decimal_pos = l_decimals ).
      IF source CN '1234567890 '.
        subrc = 8.
      ELSE.
        l_decimals_target = l_decimals_target - l_decimals.
        target = source * ( 10 ** ( l_decimals_target ) ).
      ENDIF.
    WHEN 'I'.
      DESCRIBE FIELD target DECIMALS l_decimals_target.
      prepare_number( CHANGING PARAMETER = source decimal_pos = l_decimals ).
      IF source CN '1234567890 '.
        subrc = 8.
      ELSE.
        l_decimals_target = l_decimals_target - l_decimals.
        target = source * ( 10 ** ( l_decimals_target ) ).
      ENDIF.
    WHEN 'P'.
      DESCRIBE FIELD target DECIMALS l_decimals_target.
      prepare_number( CHANGING PARAMETER = source decimal_pos = l_decimals ).
* OSS VW problem
      DATA l_len_source TYPE i.
      DATA l_len_target TYPE i.
      DESCRIBE FIELD target LENGTH l_len_target
                               IN BYTE MODE.
      l_len_target = ( l_len_target * 2 ) - 1.
      l_len_source = STRLEN( source ).
      IF l_len_source > l_len_target.
        l_decimals_target = l_decimals_target -  l_len_target +
                            l_len_source.
        source = source(l_len_target).
      ENDIF.
* OSS VW problem
      IF source CN '1234567890 -+'.
        subrc = 8.
      ELSE.
        l_decimals_target = l_decimals_target - l_decimals.
        move source TO target.
        target = target * ( 10 ** ( l_decimals_target ) ).
      ENDIF.
    WHEN 'F'.
      CALL FUNCTION 'CHAR_FLTP_CONVERSION'
        EXPORTING
          string = source
        IMPORTING
          flstr  = target
        EXCEPTIONS
          OTHERS = 4.
      IF sy-subrc <> 0.
        subrc = 4.
      ENDIF.
    WHEN OTHERS.
      target = source.
  ENDCASE.

ENDMETHOD.
method LEER.
data l_FILENAME type  RLGRAP-FILENAME.

  l_filename = fichero.

  call function 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    exporting
      filename                = l_filename
      i_begin_col             = col_ini
      i_begin_row             = row_ini
      i_end_col               = col_fin
      i_end_row               = row_fin
    tables
      intern                  = i_excel
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.
  if sy-subrc ne 0.
    case sy-subrc.
      when 2.
        message e398(00) with 'Error al leer el fichero' l_filename.
      when others.
        message e398(00) with 'Error' sy-subrc
                              'al cargar fichero Excel'.
    endcase.
  else.
    sort i_Excel.
  endif.

endmethod.
METHOD LINEA_CSV_2_DATOS.
  DATA: i_fichero TYPE TABLE OF text4096,
        l_fichero TYPE text4096,
        i_datos   TYPE REF TO data.

  FIELD-SYMBOLS <tabla> TYPE table.

  CREATE DATA i_datos like table of datos.
  ASSIGN i_datos->* TO <tabla>.

  l_fichero = linea.
  APPEND l_fichero  TO i_fichero.
  CALL FUNCTION 'TEXT_CONVERT_TEX_TO_SAP'
    EXPORTING
      i_field_seperator          = field_separator
*    I_LINE_HEADER              =
      i_tab_raw_data             = i_fichero
*   I_FILENAME                 =
    TABLES
      i_tab_converted_data       = <tabla>
    EXCEPTIONS
      conversion_failed          = 1
      OTHERS                     = 2.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER  sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
  ELSE.
    READ TABLE <tabla> INTO datos INDEX 1.
    if sy-subrc ne 0.
      message = 'Error en conversión CSV'.
    endif.
  ENDIF.

ENDMETHOD.
METHOD LINEA_CSV_2_DATOS_INT.
  FIELD-SYMBOLS: <f_source> TYPE ANY , <f_target> TYPE ANY.

  DATA:
        l_percentage(4)  TYPE c,
        l_text80(80),
        l_text6(6),
        l_itab_entries LIKE sy-tabix,
        l_tabix LIKE sy-tabix,
        l_start_string LIKE sy-fdpos,
        l_token_next LIKE sy-fdpos,
        l_end_string LIKE sy-fdpos,
        l_eol_string LIKE sy-fdpos,
        l_len_string(6) TYPE n,
        l_field_type,
        l_field_decimals,
        l_struc_index LIKE sy-index,
        l_linea(65000),
        l_subrc TYPE i.

  l_linea = linea.
  DESCRIBE FIELD datos LENGTH l_eol_string IN CHARACTER MODE.

  DO.
    l_token_next = 2.
    l_struc_index = l_struc_index + 1.
    IF NOT field_separator IS INITIAL.
      SEARCH l_linea FOR field_separator STARTING AT
                                                  l_start_string
                                                  ENDING AT
                                                  l_eol_string.
      IF sy-subrc <> 0.
        IF l_start_string < l_eol_string.
          sy-fdpos = l_eol_string - l_start_string + 1.
          CLEAR sy-subrc.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.
      IF sy-subrc = 0.
        l_end_string = l_start_string + sy-fdpos - 1.
        l_len_string = l_end_string - l_start_string + 1.
*         Check leading control character
        IF l_len_string >= 4.
          l_start_string = l_start_string - 1.
          ASSIGN l_linea+l_start_string(3)
                         TO <f_source>.
          l_start_string = l_start_string + 1.
          IF sy-subrc = 0 AND <f_source> = '"""'.
            l_start_string = l_start_string + 3.
            l_end_string = l_end_string - 1.
            l_len_string = l_end_string - l_start_string + 1.
            l_token_next = l_token_next + 1.
          ENDIF.
        ENDIF.
        IF l_len_string > 0.
          l_start_string = l_start_string - 1.
          ASSIGN l_linea+l_start_string(l_len_string)
                         TO <f_source>.
        ELSE.
*           if l_start_string = 1.
*             l_struc_index = l_struc_index - 1.
*           endif.
          l_start_string = l_start_string + l_token_next - 1.
        ENDIF.
      ENDIF.
    ELSE.
      ASSIGN COMPONENT l_struc_index OF
             STRUCTURE datos TO <f_target>.
      IF sy-subrc = 0.
        DESCRIBE FIELD <f_target> LENGTH l_len_string
                                  IN CHARACTER MODE.
        ASSIGN l_linea+l_start_string(l_len_string)
             TO <f_source>.
        l_end_string = l_len_string + l_start_string - 1.
        l_token_next = 1.
      ENDIF.
    ENDIF.
    IF sy-subrc = 0.
      ASSIGN COMPONENT l_struc_index OF
             STRUCTURE datos TO <f_target>.
    ENDIF.
    IF sy-subrc = 0.
      CLEAR <f_target>.
      CHECK l_len_string > 0.
      input_data2sap_data( CHANGING source = <f_source>
                                    target = <f_target> subrc = l_subrc ).
      IF l_subrc <> 0.
        DESCRIBE FIELD <f_target> TYPE l_field_type.
      ENDIF.
      CASE sy-subrc.
        WHEN 0.
        WHEN 4.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                  INTO message.

        WHEN 8.
          MESSAGE ID 'UX' TYPE 'E' NUMBER '899'
                  WITH l_field_type  <f_source>
                       l_struc_index l_tabix
                  INTO message.
      ENDCASE.
      l_start_string = l_end_string + l_token_next.
*>>>>> Lines Deleted <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MurawskiW / 588278
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

ENDMETHOD.
METHOD linea_datos_2_csv.
  DATA: i_fichero TYPE TABLE OF text4096,
        l_fichero TYPE text4096,
        i_datos   TYPE REF TO data.

  FIELD-SYMBOLS <tabla> TYPE table.

  CREATE DATA i_datos LIKE TABLE OF datos.
  ASSIGN i_datos->* TO <tabla>.
  APPEND datos TO <tabla>.

  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
    EXPORTING
      i_field_seperator    = field_separator
*     I_LINE_HEADER        =
*     I_FILENAME           =
*     I_APPL_KEEP          = ' '
    TABLES
      i_tab_sap_data       = <tabla>
    CHANGING
      i_tab_converted_data = i_fichero
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER  sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
  ELSE.
    READ TABLE i_fichero INTO l_fichero INDEX 1.
    IF sy-subrc NE 0.
      message = 'Error en conversión CSV'.
    ELSE.
      linea = l_fichero.
    ENDIF.
  ENDIF.

ENDMETHOD.
method PREPARE_NUMBER.
  data l_strlen type i.
  data l_last_decimal type i.
  data l_hlpvz type i.

  l_strlen = strlen( parameter ).
  clear: sy-subrc, decimal_pos.
  while sy-subrc = 0.
    if parameter ca '.'.
      if sy-fdpos  > l_last_decimal.
        l_last_decimal = sy-fdpos + 1.
      endif.
    endif.
    replace '.' with space into parameter.
  endwhile.
  clear sy-subrc.
  while sy-subrc =  0.
    if parameter ca ','.
      if sy-fdpos  > l_last_decimal.
        l_last_decimal = sy-fdpos + 1.
      endif.
    endif.
    replace ',' with space into parameter.
  endwhile.
  clear sy-subrc.
  while sy-subrc =  0.
    if parameter ca ';'.
      if sy-fdpos  > l_last_decimal.
        l_last_decimal = sy-fdpos + 1.
      endif.
    endif.
    replace ';' with space into parameter.
  endwhile.
  clear sy-subrc.
  while sy-subrc =  0.
    if parameter ca '/'.
      if sy-fdpos  > l_last_decimal.
        l_last_decimal = sy-fdpos + 1.
      endif.
    endif.
    replace '/' with space into parameter.
  endwhile.
  if not l_last_decimal is initial.
    l_hlpvz = l_strlen - 1.
    if parameter+l_hlpvz(1) = '-'.
      decimal_pos = l_strlen - l_last_decimal - 1.
    else.
      decimal_pos = l_strlen - l_last_decimal.
    endif.
  endif.
  condense parameter no-gaps.
endmethod.
method SEARCH_COLUMNA.

  LOOP AT i_excel INTO excel WHERE row = row AND value = valor.
    col = excel-col.
    EXIT.
  ENDLOOP.

endmethod.
method SEARCH_FILA.

  LOOP AT i_excel INTO excel WHERE col = col AND value = valor.
    row = excel-row.
    EXIT.
  ENDLOOP.

endmethod.