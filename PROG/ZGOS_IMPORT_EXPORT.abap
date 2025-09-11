REPORT zbc_gos_export_import LINE-SIZE 180.
*&---------------------------------------------------------------------*
*& REPORT  ZBC_GOS_EXPORT_IMPORT
*&---------------------------------------------------------------------*
*& Author  :  François Henrotte
*&
*& Purpose :  This program is used to
*&            - extract and export selected attachments to local files
*&            - import and create attachments from local files
*&---------------------------------------------------------------------*
* https://community.sap.com/t5/application-development-blog-posts/generic-export-import-gos-attachments-of-business-objects/ba-p/13310577
* El formato de carga de fichero ha de ser GOS(&&)CLAVE(&&)nombre adjunto.pdf

* System fields for dynamic selection
INCLUDE rsdbcom4.
INCLUDE rsdbc1xx.
INCLUDE rssocons.

TYPES: BEGIN OF t_id,
         objtype TYPE sibflporb-objtype,
         objkey  TYPE borident-objkey,
         logsys  TYPE borident-logsys,
       END OF t_id.

TYPES: BEGIN OF zocs_claim_atta_single,
         id        TYPE t_id,
         dscrp(80),
         size      TYPE i,
         ext(20),
         content   TYPE soli_tab,
       END OF zocs_claim_atta_single.

***
*   Constants
***
CONSTANTS: gc_separator TYPE char4 VALUE '(&&)',            "#EC NOTEXT
           gc_sep_lengt TYPE i     VALUE 4.

CONSTANTS: gc_def_path TYPE text40 VALUE 'C:\temp\',        "#EC NOTEXT
           gc_def_file TYPE text40 VALUE 'GOS*.*'.          "#EC NOTEXT


***
*   Types definition
***
TYPES: BEGIN OF ty_attach,
         bor_id TYPE sibfboriid,
         filen  TYPE string,
         bdata  TYPE solix_tab,
       END OF ty_attach,
       tt_attach TYPE TABLE OF ty_attach.


***
*   Data definition
***
DATA: wt_keyfields TYPE TABLE OF rpyboke,
      ws_keyfields TYPE rpyboke,
      w_title      TYPE sy-title,
      w_key        TYPE sibfboriid.

DATA: ws_x030l TYPE x030l.

DATA: w_selid   TYPE rsdynsel-selid,
      wt_tables TYPE TABLE OF rsdstabs,
      ws_tables TYPE rsdstabs,
      ws_where  TYPE rsds_where,
      w_active  TYPE i,
      w_dbcnt   TYPE i.

DATA: ws_varidyn TYPE rsvaridyn.

DATA: wt_attach TYPE tt_attach,
      ws_attach TYPE ty_attach,
      ws_bdata  TYPE solix.

DATA: wt_sfile TYPE TABLE OF eps2fili,
      ws_sfile TYPE eps2fili,
      wt_files TYPE TABLE OF file_info,
      ws_files TYPE file_info,
      w_path   TYPE string,
      w_file   TYPE string,
      w_mess   TYPE string,
      w_fileid TYPE string,
      w_where  TYPE string,
      w_index  TYPE numc2,
      w_count  TYPE i.

DATA: ob_table TYPE REF TO data.

FIELD-SYMBOLS: <wt_table> TYPE STANDARD TABLE,
               <w_field>  TYPE any.


***
*   Selection screen
***
SELECTION-SCREEN: BEGIN OF BLOCK obj WITH FRAME TITLE TEXT-obj.
PARAMETERS: p_objtyp TYPE swo_objtyp OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK obj.
SELECTION-SCREEN: BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_exp RADIOBUTTON GROUP pro DEFAULT 'X'.
PARAMETERS: p_imp RADIOBUTTON GROUP pro.
SELECTION-SCREEN: SKIP,
                  BEGIN OF LINE.
SELECTION-SCREEN: COMMENT (30) seltab FOR FIELD p_table,
                  POSITION 33.
PARAMETERS: p_table TYPE tabname.
SELECTION-SCREEN: PUSHBUTTON 65(4) selopt USER-COMMAND sel,
                  COMMENT    71(6) selcnt,
                  END OF LINE.
SELECTION-SCREEN: END OF BLOCK b01.
SELECTION-SCREEN: BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
PARAMETERS: p_locl TYPE xflag AS CHECKBOX.
SELECTION-SCREEN: SKIP.
PARAMETERS: p_path TYPE string VISIBLE LENGTH 80 DEFAULT gc_def_path LOWER CASE,
            p_file TYPE string VISIBLE LENGTH 80 DEFAULT gc_def_file LOWER CASE.
SELECTION-SCREEN: END OF BLOCK b03.


***
* Initialization
***
INITIALIZATION.
  MOVE '@4G@' TO selopt.
  MOVE %_p_table_%_app_%-text TO seltab.

  IF sy-slset IS NOT INITIAL.
    SUPPRESS DIALOG.
  ENDIF.


***
*   PBO
***
AT SELECTION-SCREEN OUTPUT.
  IF w_active IS INITIAL.
    CLEAR: selcnt.
  ELSE.
    WRITE w_active TO selcnt LEFT-JUSTIFIED.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name = 'P_TABLE'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


***
* PAI
***
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_objtyp.
  CALL FUNCTION 'RS_HELP_HANDLING'
    EXPORTING
      dynpfield                 = 'P_OBJTYP'
      dynpname                  = '1000'
      object                    = 'SOBJ'
      progname                  = 'ZBC_GOS_EXPORT_IMPORT'
      suppress_selection_screen = 'X'
      variant                   = 'SAP&STANDARD'
    EXCEPTIONS
      OTHERS                    = 0.


AT SELECTION-SCREEN.
  IF ws_x030l-tabname IS INITIAL
  OR ws_x030l-tabname NE p_table.
    PERFORM f_init_table.
  ENDIF.

  IF w_selid IS INITIAL.
    PERFORM f_init_selections.
  ENDIF.

  IF sy-ucomm = 'SEL'.
*   Display free selection dialog
    CONCATENATE 'Selection in table'(sit) p_table
           INTO w_title SEPARATED BY space.
    CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
      EXPORTING
        selection_id            = w_selid
        title                   = w_title
        status                  = 1
        as_window               = 'X'
      IMPORTING
        expressions             = dyn_sel-texpr
        field_ranges            = dyn_sel-trange
        number_of_active_fields = w_active
      TABLES
        fields_tab              = dyns_fields
      EXCEPTIONS
        OTHERS                  = 1.
    IF sy-subrc EQ 0.
      REFRESH gl_varidyn.
      LOOP AT dyns_fields.
        MOVE-CORRESPONDING dyns_fields TO ws_varidyn.
        APPEND ws_varidyn TO gl_varidyn.
      ENDLOOP.
    ENDIF.
  ENDIF.


***
*   Start of selection
***
START-OF-SELECTION.
  FORMAT RESET.

  CLEAR w_path.
  IF p_path(1) = '/'.
    w_path = p_path.
    IF w_path NP '*/'.
      CONCATENATE w_path '/' INTO w_path.
    ENDIF.
  ELSEIF p_path(1) = '\'.
    w_path = p_path.
    IF w_path NP '*\'.
      CONCATENATE w_path '\' INTO w_path.
    ENDIF.
  ELSE.
    SELECT SINGLE dirname                               "#EC CI_NOFIELD
      FROM user_dir
      INTO w_path
     WHERE aliass = p_path.
    IF sy-subrc NE 0.
      SELECT SINGLE pathextern
        FROM path
        INTO w_path
       WHERE pathintern EQ p_path
         AND filesys    EQ sy-opsys.
    ENDIF.
    IF w_path(1) = '/'.
      IF w_path NP '*/'.
        CONCATENATE w_path '/' INTO w_path.
      ENDIF.
    ELSEIF w_path(1) = '\'.
      IF w_path NP '*\'.
        CONCATENATE w_path '\' INTO w_path.
      ENDIF.
    ELSEIF w_path IS NOT INITIAL.
      MESSAGE e002(rc).
    ENDIF.
  ENDIF.
  IF w_path IS INITIAL.
    MESSAGE e024(sg) WITH p_path sy-opsys.
  ENDIF.

  PERFORM f_create_table USING p_table.
  PERFORM f_select_table.

  IF p_exp IS NOT INITIAL.

    LOOP AT <wt_table> ASSIGNING FIELD-SYMBOL(<ws_struc>).
      CLEAR w_key.
      LOOP AT wt_keyfields INTO ws_keyfields
                          WHERE refstruct = p_table.
        ASSIGN COMPONENT ws_keyfields-reffield OF STRUCTURE <ws_struc> TO <w_field>.
        CHECK sy-subrc = 0.
        CONCATENATE w_key <w_field>
               INTO w_key RESPECTING BLANKS.
      ENDLOOP.

*     Retrieve the contents of attachments
      PERFORM f_read_attachments USING w_key
                              CHANGING wt_attach.

      CLEAR w_index.
      LOOP AT wt_attach INTO ws_attach.
        CONCATENATE w_path p_file INTO w_file.
        ADD 1 TO w_index.
        CONCATENATE '' ws_attach-bor_id w_index ws_attach-filen
               INTO w_fileid SEPARATED BY gc_separator.
        REPLACE '.*' WITH space INTO w_file.
        REPLACE '*' WITH w_fileid INTO w_file.

        IF p_locl IS INITIAL.
          OPEN DATASET w_file FOR OUTPUT IN BINARY MODE MESSAGE w_mess.
          IF sy-subrc EQ 0.
            LOOP AT ws_attach-bdata INTO ws_bdata.
              TRANSFER ws_bdata TO w_file.
            ENDLOOP.
            IF sy-subrc = 0.
              WRITE: / w_file.
            ELSE.
*             Implement suitable error handling here
            ENDIF.
            CLOSE DATASET w_file.
          ELSE.
            WRITE: / 'Error:', w_mess.
          ENDIF.
        ELSE.
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
*             BIN_FILESIZE =
              filename = w_file
              filetype = 'BIN'
            TABLES
              data_tab = ws_attach-bdata
            EXCEPTIONS
              OTHERS   = 1.
          IF sy-subrc = 0.
            WRITE: / w_file.
          ELSE.
*           Implement suitable error handling here
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ELSE.

    IF p_locl IS INITIAL.
      CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
        EXPORTING
          iv_dir_name  = CONV eps2filnam( w_path )
          file_mask    = CONV epsfilnam( p_file )
        IMPORTING
          file_counter = w_count
        TABLES
          dir_list     = wt_sfile
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc NE 0.
        MESSAGE e012(ps).
      ENDIF.
      LOOP AT wt_sfile INTO ws_sfile.
        ws_files-filename = ws_sfile-name.
        ws_files-filelength = ws_sfile-size.
        APPEND ws_files TO wt_files.
      ENDLOOP.
    ELSE.
      cl_gui_frontend_services=>directory_list_files(
        EXPORTING
          directory = w_path
          filter    = p_file
          files_only = abap_true
        CHANGING
          file_table = wt_files
          count      = w_count
        EXCEPTIONS
          OTHERS     = 1 ).
      IF sy-subrc NE 0.
        MESSAGE e012(ps).
      ENDIF.
    ENDIF.

    IF w_count NE 0.
      LOOP AT wt_files INTO ws_files.
        CHECK ws_files-filename CS gc_separator.
        CLEAR ws_attach.

*       Get file name and path
        ws_attach-filen = ws_files-filename.
        SHIFT ws_attach-filen LEFT UP TO gc_separator.
        SHIFT ws_attach-filen LEFT BY gc_sep_lengt PLACES.
        IF ws_attach-filen CS gc_separator.
          ws_attach-bor_id = ws_attach-filen(sy-fdpos).
        ELSE.
          CONTINUE.
        ENDIF.

*       Log source file
        WRITE: / ws_files-filename.

*       Check if business object exists
        READ TABLE <wt_table> WITH KEY (ws_keyfields-reffield) = ws_attach-bor_id TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          FORMAT COLOR COL_NEGATIVE.
          WRITE: 'object ID'(oid), ws_attach-bor_id NO-GAP, 'not found'(nof).
          FORMAT RESET.
          CONTINUE.
        ENDIF.

*       Upload file
        CONCATENATE w_path ws_files-filename INTO w_file.
        IF p_locl IS INITIAL.
          OPEN DATASET w_file FOR INPUT IN BINARY MODE MESSAGE w_mess.
          IF sy-subrc EQ 0.
            DO.
              READ DATASET w_file INTO ws_bdata.
              IF sy-subrc NE 0.
                EXIT.
              ENDIF.
              APPEND ws_bdata TO ws_attach-bdata.
            ENDDO.
            CLOSE DATASET w_file.
          ELSE.
            WRITE: / 'Error:', w_mess.
          ENDIF.
        ELSE.
          DATA l_size TYPE int4.
          CALL FUNCTION 'GUI_UPLOAD'
            EXPORTING
              filename   = w_file
              filetype   = 'BIN'
            IMPORTING
              filelength = l_size
            TABLES
              data_tab   = ws_attach-bdata
            EXCEPTIONS
              OTHERS     = 1.
        ENDIF.
        IF sy-subrc = 0.
          WHILE ws_attach-filen CS gc_separator.
            SHIFT ws_attach-filen LEFT UP TO gc_separator.
            SHIFT ws_attach-filen LEFT BY gc_sep_lengt PLACES.
          ENDWHILE.

          PERFORM f_create_attachment USING ws_attach l_size.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  f_init_table
*&---------------------------------------------------------------------*
FORM f_init_table.
  CALL FUNCTION 'RPY_OBJECTTYPE_READ'
    EXPORTING
      objecttype_id                = p_objtyp
      with_verbs                   = 'X'
      with_parameters              = ' '
      with_exceptions              = ' '
      with_texts                   = ' '
      with_formatted_documentation = ' '
      with_sapscript_documentation = ' '
    TABLES
      keyfields                    = wt_keyfields
    EXCEPTIONS
      not_found                    = 1
      permission_error             = 2
      OTHERS                       = 3.
  IF sy-subrc = 0.
    READ TABLE wt_keyfields INTO ws_keyfields INDEX 1.
    IF sy-subrc = 0.
      p_table = ws_keyfields-refstruct.
    ENDIF.
  ELSE.
    MESSAGE e520(ol) WITH p_objtyp.
  ENDIF.

* Prepare free selection on table
  PERFORM f_table_def USING p_table.

  REFRESH wt_tables.
  ws_tables-prim_tab = p_table.
  APPEND ws_tables TO wt_tables.

  CLEAR w_selid.

ENDFORM.                    "f_init_table


*&---------------------------------------------------------------------*
*&      Form  f_init_selections
*&---------------------------------------------------------------------*

FORM f_init_selections.

* Init free selection dialog
  CALL FUNCTION 'FREE_SELECTIONS_INIT'
    EXPORTING
      expressions             = dyn_sel-texpr
    IMPORTING
      selection_id            = w_selid
      expressions             = dyn_sel-texpr
      number_of_active_fields = w_active
    TABLES
      tables_tab              = wt_tables
      fields_tab              = dyns_fields
    EXCEPTIONS
      OTHERS                  = 0.

ENDFORM.                    "f_init_selections


*---------------------------------------------------------------------*
*       FORM f_table_def                                              *
*---------------------------------------------------------------------*
FORM f_table_def USING in_tabname.

  CALL FUNCTION 'DDIF_NAMETAB_GET'
    EXPORTING
      tabname  = in_tabname
    IMPORTING
      x030l_wa = ws_x030l
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc NE 0.
    CLEAR ws_x030l.
  ENDIF.

  IF ws_x030l IS INITIAL.
    MESSAGE e058(54) WITH in_tabname.
  ELSEIF ws_x030l-tabtype NE 'T'
    AND ws_x030l-tabtype NE 'J' .
    MESSAGE e126(ba) WITH in_tabname.
  ENDIF.

ENDFORM.                    "f_table_def


*---------------------------------------------------------------------*
*       FORM f_create_table                                           *
*---------------------------------------------------------------------*
FORM f_create_table USING in_tabname.

  CREATE DATA ob_table TYPE TABLE OF (in_tabname).
  IF sy-subrc = 0.
    ASSIGN ob_table->* TO <wt_table>.
  ELSE.
    MESSAGE e515(rsdme) WITH in_tabname.
  ENDIF.

ENDFORM.                    "f_create_table


*---------------------------------------------------------------------*
*       FORM f_select_table                                           *
*---------------------------------------------------------------------*
FORM f_select_table.

  IF w_active = 0.
    SELECT * FROM (p_table)
             INTO CORRESPONDING FIELDS OF TABLE <wt_table>.
  ELSE.
*   Selection with parameters
    CALL FUNCTION 'FREE_SELECTIONS_EX_2_WHERE'
      EXPORTING
        expressions              = dyn_sel-texpr
      IMPORTING
        where_clauses            = dyn_sel-clauses
      EXCEPTIONS
        expression_not_supported = 1
        OTHERS                   = 2.
    IF sy-subrc = 0.
      READ TABLE dyn_sel-clauses INTO ws_where WITH KEY tablename = p_table.
      IF sy-subrc = 0.
        SELECT * FROM (p_table)
                 INTO CORRESPONDING FIELDS OF TABLE <wt_table>
                WHERE (ws_where-where_tab).
      ENDIF.
    ENDIF.
  ENDIF.

  IF sy-dbcnt = 0.
    WRITE: 'No record selected'(nrs).
    STOP.
  ELSE.
    w_dbcnt = sy-dbcnt.
    MESSAGE s058(oz) WITH w_dbcnt.
  ENDIF.

ENDFORM.                    "f_select_table


*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ATTACHMENT
*&---------------------------------------------------------------------*
FORM f_read_attachments USING in_obj_id TYPE sibfboriid
                     CHANGING ct_attach TYPE tt_attach.
  DATA: ls_object TYPE sibflporb,
        lt_links  TYPE obl_t_link,
        ls_attach TYPE ty_attach.

  DATA: ls_sofolenti1 TYPE sofolenti1,
        lv_doc_id     TYPE sofolenti1-doc_id.


  CLEAR ct_attach.

  ls_object-instid = in_obj_id.
  ls_object-typeid = p_objtyp.
  ls_object-catid  = sibf_catid_bo.

  TRY.
      cl_binary_relation=>read_links_of_binrel(
      EXPORTING
        is_object   = ls_object
        ip_relation = cl_gos_api=>c_atta
      IMPORTING
        et_links    = lt_links ).
    CATCH cx_obl.
      CLEAR lt_links.
  ENDTRY.

  ls_attach-bor_id = in_obj_id.
  LOOP AT lt_links ASSIGNING FIELD-SYMBOL(<ls_link>).
*   46 characters = folder + doc + forwarder
    lv_doc_id = <ls_link>-instid_b(46).
    CLEAR ls_attach-bdata.
*   API1 returns the contents in binary format
    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id   = lv_doc_id
      IMPORTING
        document_data = ls_sofolenti1
      TABLES
        contents_hex  = ls_attach-bdata
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc = 0.
*     Build the original filename based on name + extension
      CONCATENATE ls_sofolenti1-obj_descr ls_sofolenti1-obj_type
             INTO ls_attach-filen SEPARATED BY '.'.
      APPEND ls_attach TO ct_attach.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_READ_ATTACHMENTS


*&---------------------------------------------------------------------*
*&      Form  F_CREATE_ATTACHMENT
*&---------------------------------------------------------------------*
FORM f_create_attachment USING in_attach TYPE ty_attach
                               size TYPE int4.
  DATA: ls_attachment TYPE zocs_claim_atta_single,
        lv_dummy      TYPE text132.                         "#EC NEEDED


* Target document id for Binary relationship
  MOVE p_objtyp         TO ls_attachment-id-objtype.
  MOVE in_attach-bor_id TO ls_attachment-id-objkey.
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system = ls_attachment-id-logsys
    EXCEPTIONS
      OTHERS             = 1.
  CHECK sy-subrc = 0.

* Title
  MOVE in_attach-filen TO ls_attachment-dscrp.


* Extension
  SPLIT in_attach-filen AT '.' INTO lv_dummy ls_attachment-ext.

  ls_attachment-size = size.
* Convert contents
  CALL FUNCTION 'SO_SOLIXTAB_TO_SOLITAB'
    EXPORTING
      ip_solixtab = in_attach-bdata
    IMPORTING
      ep_solitab  = ls_attachment-content.

* Create attachment
  PERFORM f_gos_add_attachment CHANGING ls_attachment.

ENDFORM.                    " F_CREATE_ATTACHMENT


*&---------------------------------------------------------------------*
*&      Form  F_GOS_ADD_ATTACHMENT
*&---------------------------------------------------------------------*
FORM f_gos_add_attachment CHANGING cs_attachment TYPE zocs_claim_atta_single.
  DATA: ls_folder_id  TYPE sofdk.
  DATA: ls_document   TYPE sofmk.
  DATA: ls_obj_id     TYPE soodk.
  DATA: ls_rel_doc    TYPE borident.
  DATA: ls_hd_dat     TYPE sood1.

  DATA: lt_objhead    TYPE soli_tab.


  CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
    EXPORTING
      region    = pa_fol
    IMPORTING
      folder_id = ls_folder_id
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

* Init
  MOVE cs_attachment-dscrp TO ls_hd_dat-objdes.
  MOVE cs_attachment-size  TO ls_hd_dat-objlen.
  MOVE cs_attachment-ext   TO ls_hd_dat-file_ext.

  CALL FUNCTION 'SO_OBJECT_INSERT'
    EXPORTING
      folder_id                  = ls_folder_id
      object_hd_change           = ls_hd_dat
      object_type                = ext                 "PC Document
    IMPORTING
      object_id                  = ls_obj_id
    TABLES
      objcont                    = cs_attachment-content "Must be changing parameter!!!!
      objhead                    = lt_objhead
    EXCEPTIONS
      active_user_not_exist      = 1
      communication_failure      = 2
      component_not_available    = 3
      dl_name_exist              = 4
      folder_not_exist           = 5
      folder_no_authorization    = 6
      object_type_not_exist      = 7
      operation_no_authorization = 8
      owner_not_exist            = 9
      parameter_error            = 10
      substitute_not_active      = 11
      substitute_not_defined     = 12
      system_failure             = 13
      x_error                    = 14
      OTHERS                     = 15.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  MOVE ls_folder_id-foltp TO ls_document-foltp.
  MOVE ls_folder_id-folyr TO ls_document-folyr.
  MOVE ls_folder_id-folno TO ls_document-folno.
  MOVE ls_obj_id-objtp    TO ls_document-doctp.
  MOVE ls_obj_id-objyr    TO ls_document-docyr.
  MOVE ls_obj_id-objno    TO ls_document-docno.

  MOVE ls_document           TO ls_rel_doc-objkey.
*  MOVE cl_gos_api=>c_message TO ls_rel_doc-objtype.
  MOVE 'MESSAGE' TO ls_rel_doc-objtype.

  DATA l_obj_rolea TYPE borident.
  l_obj_rolea-objtype = cs_attachment-id-objtype.
  l_obj_rolea-objkey = cs_attachment-id-objkey.
  l_obj_rolea-logsys = cs_attachment-id-logsys.

  CALL FUNCTION 'BINARY_RELATION_CREATE'
    EXPORTING
      obj_rolea    = l_obj_rolea
      obj_roleb    = ls_rel_doc
      relationtype = cl_gos_api=>c_atta(4)
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

  WRITE: / ls_hd_dat-objdes.

  COMMIT WORK AND WAIT.

ENDFORM.                    " F_GOS_ADD_ATTACHMENT
