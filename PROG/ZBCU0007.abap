*&---------------------------------------------------------------------*
*& Report RSSTAT26.                                                    *
**
*& Neue Einzelsatzstatistik mit Erfassung der Trans-ID. Anzeigemodi:   *
*& a) wie in Transaktion STAT, alter RSSTAT20                          *
*& b) geordnet nach Zusammengehörigkeit (aber nur Einzelsätze)         *
*& c) geordnet nach Trans-ID mit 'Summensätzen'                        *
*&---------------------------------------------------------------------*
*& Autorin: Daniela Hören                                              *
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Release 7.0                                                         *
*& Anpassungen nach Änderung von Kernel-Strukturen                   *
*& c5044722                                                            *
*&---------------------------------------------------------------------*

REPORT sapwl_stad MESSAGE-ID s1 NO STANDARD PAGE HEADING LINE-SIZE 1023
."255.

TABLES: sapwlwamai, trdir, sapwlslins.

CONTROLS: output_fields TYPE TABLEVIEW USING SCREEN '0030',
          server_selection TYPE TABLEVIEW USING SCREEN '0050'.

*-----------------------------------------------------------------------

INCLUDE <symbol>.
INCLUDE <line>.
INCLUDE sapwlstad_tt.
INCLUDE sapwlstad_co.
INCLUDE sapwlstad_gd.
INCLUDE perfincl.
INCLUDE sapwlstad_va.
INCLUDE sapwlstad_ex.
INCLUDE sapwlstad_ou.
INCLUDE sapwlstad_lv.


*------TABLAS INTERNAS-------------------------------------------------*
tables: tadir, zestadisticas.

DATA: BEGIN OF i_datos OCCURS 1000,
        account    LIKE stad_output-account,
        date       LIKE stad_output-date,
        endti      LIKE stad_output-endti,
        tcode      LIKE stad_output-tcode,
        terminalid LIKE stad_output-terminalid,
        report     LIKE stad_output-report,
        btcjobname    LIKE stad_output-btcjobname,
        jobstep    LIKE stad_output-btcstepnr,
        tabload    LIKE stad_output-tabload,
        dynpronr   LIKE stad_output-dynpronr,
      END OF i_datos.

DATA: BEGIN OF i_aux OCCURS 1000,
        account    LIKE stad_output-account,
        tcode      LIKE stad_output-tcode,
        report     LIKE stad_output-report,
        date       LIKE stad_output-date,
        endti      LIKE stad_output-endti,
        terminalid LIKE stad_output-terminalid,
        btcjobname    LIKE stad_output-btcjobname,
      END OF i_aux.
*------VARIABLES-------------------------------------------------------*


* selection parameters for single records-------------------------------
*
PARAMETERS:
                 rstartti      TYPE swlstart,
                 "ab Uhrzeit
                 rendti        TYPE swncuzeit,
                 "Endtime
                 rday          TYPE swldate DEFAULT sy-datum,
                 "Enddatum
                 rreadti       TYPE swlreadti DEFAULT '001000',
                 "Lesezeitraum
                 rmode         TYPE swlmode DEFAULT 'a'.
                 "Anzeigemodus
SELECTION-SCREEN ULINE.
PARAMETERS:      rmandt     LIKE sapwlpfnrm-mandt,
"Mandant
                 ruser      LIKE sapwlpfnrm-account,             "User
                 rtcode     LIKE sapwlpfnrm-tcode,               "Tcode
                 rprogram   LIKE sapwlpfnrm-report,              "Report
                 tasktype,
                 "Tasktyp nur in Modus a)
                 rscreen    LIKE sapwlpfnrm-dynpronr.            "Dynpro
*rwpid(2)                                        "Work Process
SELECTION-SCREEN ULINE.
PARAMETERS:      rrspti        TYPE swlrespti2,
"Resp. time
                 rdbti         TYPE sta_seltim,
                 "DB time
                 rcputi        TYPE sta_seltim,
                 "CPU time
                 rkbyte        TYPE sta_selbyt,
                 "Bytes trans
                 rchg          TYPE sta_selchg.
                 "Phys.Aenderg.
SELECTION-SCREEN ULINE.
PARAMETERS:
*rreaddel      LIKE sy-uzeit DEFAULT '000200',   "Lesezeitdelta
                 rwaitfac      TYPE i DEFAULT 150.
                 "Wartefaktor
SELECTION-SCREEN ULINE.
PARAMETERS:      statfile      LIKE sapwlpstrc-filename DEFAULT space,
                 as_statf      LIKE sapwlpstrc-filename DEFAULT space.
*Änderung tc----------------------------------------------------------
SELECTION-SCREEN ULINE.
PARAMETERS:      ronly   LIKE server_list-name DEFAULT space,
                 rtcodf  LIKE selection-stcod DEFAULT '*',
                 rprogf  LIKE selection-sprogram DEFAULT '*'.
*---------------------------------------------------------------------

*** INICIO MODIFICACIONES
parameters:      p_act as checkbox default ' ',
                 p_list as checkbox default 'X'.
*** FIN MODIFICACIONES

*@ZV 05.02.2006
PARAMETERS: funcname(30) DEFAULT 'SWNC_STAD_READ_STATRECS'  NO-DISPLAY.
PARAMETERS: simpmode(1) DEFAULT space  NO-DISPLAY.

*** INICIO MODIFICACIONES
********************************** ALV *********************************
type-pools: slis.

data: alv_fieldtab type slis_t_fieldcat_alv,
      alv_heading  type slis_t_listheader,
      alv_layout   type slis_layout_alv,
      alv_events   type slis_t_event,
      alv_sort     type slis_t_sortinfo_alv,
      alv_filter   type slis_t_filter_alv,
      alv_repname  like sy-repid,
      alv_f2code   like sy-ucomm value  '&ETA',
      alv_g_save(1) type c,
      alv_g_exit(1) type c.
data: alv_fieldcat type slis_fieldcat_alv.
*** FIN MODIFICACIONES

* initialize -----------------------------------------------------------
*
INITIALIZATION.
* Anbindung an andere Reports?
* import...
* fill selection criterion accordingly
  IF NOT rmode CA 'abcABC'.
    rmode = 'a'.
  ELSE.
    TRANSLATE rmode TO LOWER CASE.                       "#EC TRANSLANG
  ENDIF.
  PERFORM check_selection_criterion USING rmode.
  SET TITLEBAR '001'.
* fill internal output table to specify output field in mainlist
*
  PERFORM fill_ausgabe.

*change jtc test of instance-input
AT SELECTION-SCREEN.
  IF ronly <> space AND NOT ronly IS INITIAL.
    PERFORM get_server_list.
    LOOP AT server_list WHERE name = ronly.
    ENDLOOP.
    IF sy-subrc <> 0.
*      MESSAGE i347(00) WITH '->' %_RONLY_%_APP_%-TEXT.
      ronly = space.
    ENDIF.
  ELSE.
    ronly = space.
  ENDIF.

* start of selection ---------------------------------------------------
*
START-OF-SELECTION.
*@ZV 05.02.2006
  data_read_function = funcname.
  simple_mode = simpmode.

  PERFORM fill_selection.
  g_statistic_file    = statfile.
  g_as_statistic_file = as_statf.
  PERFORM select_command USING 'INIT'.

  perform agrupar_informacion.

* line selection -------------------------------------------------------
*
AT LINE-SELECTION.

* using pf-keys --------------------------------------------------------
*
AT USER-COMMAND.
*  PERFORM select_command USING sy-ucomm.


*
*--------------FORMS----------------------------------------------------
*

*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTION_CRITERION
*&---------------------------------------------------------------------*
*       Get selection criterion for sinlge record statistic especially
*       if any required value is initial and to set '*'.
*----------------------------------------------------------------------*
*  -->  mode      output mode
*----------------------------------------------------------------------*
FORM check_selection_criterion USING mode.

  IF rday = '00000000' OR rday = '        ' OR rday = '*       '
       OR rday > sy-datum
       OR ( rday = sy-datum AND rstartti > sy-uzeit ).
    rday = sy-datum.
    IF rstartti > sy-uzeit.
      rstartti = sy-uzeit - rreadti.
*      rstartti = sy-uzeit - '001000'.
      rstartti+4 = '00'.
    ENDIF.
  ENDIF.

  IF rstartti = '      ' OR rstartti = '*' OR rstartti IS INITIAL.
    rstartti = sy-uzeit - rreadti.
*    rstartti = sy-uzeit - '001000'.
    rstartti+4 = '00'.
  ENDIF.

  IF rreadti = '000000'.
    rreadti = '001000'.
  ELSEIF rreadti > '003000'.           " max. value = 30 min
    rreadti = '003000'.
  ENDIF.

*AD
*  IF rreaddel > '001500'.
*    rreaddel = '001500'.               " max. value = 15 min
*  ENDIF.
*
  IF rwaitfac = 0.
    rwaitfac = 150.
  ENDIF.

  CASE mode.
    WHEN 'b' OR 'c'.
      ruser = '*'.
      " -> evtl. machbar: User -> TransID
*   if ruser is initial.
*     ruser = '*'.
*   else.
*     translate ruser to upper case.
*   endif.
      rtcode = '*'.
      rprogram = '*'.
      tasktype = '*'.
      rscreen = '*'.
*      rwpid = '*'.
    WHEN OTHERS.
      IF rmandt IS INITIAL.
        rmandt = '*'.
      ENDIF.
      IF ruser IS INITIAL.
        ruser = '*'.
      ELSE.
        TRANSLATE ruser TO UPPER CASE.                   "#EC TRANSLANG
      ENDIF.
      IF rtcode IS INITIAL.
        rtcode = '*'.
      ENDIF.
      IF rprogram IS INITIAL.
        rprogram = '*'.
      ENDIF.
      IF tasktype IS INITIAL.
        tasktype = '*'.
      ENDIF.
      IF rscreen IS INITIAL.
        rscreen = '*'.
      ENDIF.
*      IF rwpid IS INITIAL OR rwpid(1) = '!'.
*        rwpid = '*'.
*      ELSEIF rwpid CO ' 0123456789'.
*        sy-tfill = rwpid.
*        WRITE sy-tfill TO rwpid USING EDIT MASK  'RR__'.
*      ENDIF.
  ENDCASE.

ENDFORM.                               " CHECK_SELECTION_CRITERION


*&---------------------------------------------------------------------*
*&      Form  SELECT_COMMAND
*&---------------------------------------------------------------------*
*       At user command...
*----------------------------------------------------------------------*
*      -->fcode  OK-code                                               *
*----------------------------------------------------------------------*
FORM select_command USING value(fcode).

* local data
*
  DATA position   TYPE i.

* save list position and sy-ucomm
*
  PERFORM save_list_position.
  save_fcode = fcode.

* execute selected command
*
  CASE fcode.
*
* initialization -------------------------------------------------------
    WHEN 'INIT'.
      "* Initialise button text
      cli_text-icon_text = 'Server actions only'.
      cli_text-quickinfo = 'Display only server actions/caller info'.
      cli_text-icon_id = '@KG@'.
      PERFORM prepare_selection_criterion.  "set further selection crit.
      PERFORM fill_statistics USING rmode.    "get data
      overall_opened_level = -1.      "allways after new data collection
      PERFORM zwrite_mainlist USING rmode.     "write mainlist

* leave program --------------------------------------------------------
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN 'MONT'.
      LEAVE.   EXIT.

  ENDCASE.

ENDFORM.                               " SELECT_COMMAND

*&---------------------------------------------------------------------*
*&      Form  PREPARE_SELECTION_CRITERION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_selection_criterion.

* change tasktype -> hex -----------------------------------------------
*
  PERFORM tt_convert_letter_to_number USING tasktype rtasktype.

*
*  seltime = rstartti.

* Authority-Check---------------------------------
*
*  IF ruser <> sy-uname.                "Fremde User anzeigen !
  AUTHORITY-CHECK OBJECT 'S_TOOLS_EX'
  ID 'AUTH' FIELD 'S_TOOLS_EX_A'.
  IF sy-subrc <> 0.
    noauthflag = 'X'.                      "Nicht authorisiert
    IF ruser <> '*'.
      ruser = selection-sbenu = sy-uname.
      "Nur eigene Daten sind erlaubt
    ENDIF.
    MESSAGE i090 WITH 'Monitortools'.
  ENDIF.
*  ENDIF.


ENDFORM.                               " PREPARE_SELECTION_CRITERION
*&---------------------------------------------------------------------*
*&      Form  FILL_SELECTION
*&---------------------------------------------------------------------*
*       Fill selection structure with selection data.
*----------------------------------------------------------------------*
FORM fill_selection.

  CLEAR selection.

  selection-swaitfac = rwaitfac.
*AD  selection-sreaddel = rreaddel.
  selection-smode = rmode.
  selection-sreadti = rreadti.
*AD  selection-swpid = rwpid.
  selection-smandt = rmandt.
  selection-sbenu = ruser.
  selection-stcod = rtcode.
  selection-sprogram = rprogram.
  selection-stask = tasktype.
  selection-sresptime = rrspti.
  selection-scputime = rcputi.
  selection-sdbtime = rdbti.
  selection-schg = rchg.
  selection-sbytes = rkbyte.
  selection-sdat = rday.
  selection-stime = rstartti.
  selection-sendtime = rstartti + rreadti.
  IF selection-sendtime <= rstartti.
    selection-senddat = rday + '00000001'.
  ELSE.
    selection-senddat = rday.
  ENDIF.
  IF rscreen IS INITIAL.
    rscreen = '*'.
  ELSEIF rscreen CO ' 0123456789'.
    sy-tfill = rscreen.
    WRITE sy-tfill TO rscreen USING EDIT MASK 'RR____'.
    TRANSLATE rscreen USING ' 0'.
  ENDIF.
  selection-sscreen = rscreen.
  selection-sonly = ronly.        "change jtc
*@AD new(7.0)
*  lv0_filter-tcode = rtcodf.
*  lv0_filter-program = rprogf.
  IF rtcodf IS NOT INITIAL AND rtcodf NE '*'.
    lv0_filter-tcode = rtcodf.
  ELSE.
    lv0_filter-tcode = selection-stcod.
  ENDIF.
  IF rprogf IS NOT INITIAL AND rprogf NE '*'.
    lv0_filter-program = rprogf.
  ELSE.
    lv0_filter-program = selection-sprogram.
  ENDIF.
ENDFORM.                               " FILL_SELECTION

*----------------------------------------------------------------------*
*       FORM RETRANS_SELECTION_DATA                                    *
*----------------------------------------------------------------------*
*       SELECTION mit Selektionsdaten fuellen (r___ -> selection-s___ )
*----------------------------------------------------------------------*
FORM retrans_selection_data.

  TRANSLATE ruser TO UPPER CASE.                         "#EC TRANSLANG

*AD
*  IF rwpid IS INITIAL OR rwpid(1) = '!'.
*    rwpid = '*'.
*  ELSEIF rwpid CO ' 0123456789'.
*    sy-tfill = rwpid.
*    WRITE sy-tfill TO rwpid USING EDIT MASK  'RR__'.
*  ENDIF.
*
*  selection-swpid = rwpid.
*  IF selection-swpid IS INITIAL.
*    selection-swpid = '*'.
*  ENDIF.

  selection-smandt = rmandt.
  IF selection-smandt IS INITIAL.
    selection-smandt = '*'.
  ENDIF.

  selection-sbenu = ruser.
  IF selection-sbenu IS INITIAL.
    selection-sbenu = '*'.
  ENDIF.

  selection-stcod = rtcode.
  IF selection-stcod IS INITIAL.
    selection-stcod = '*'.
  ENDIF.

  selection-sprogram = rprogram.
  IF selection-sprogram IS INITIAL.
    selection-sprogram = '*'.
  ENDIF.

  selection-stask = tasktype.
  IF selection-stask IS INITIAL.
    selection-stask = '*'.
  ENDIF.

  selection-sresptime = rrspti.
  selection-scputime = rcputi.
  selection-sdbtime = rdbti.
  selection-schg = rchg.
  selection-sbytes = rkbyte.

  IF rday IS INITIAL.
    rday = sy-datum.
  ENDIF.
  selection-sdat = rday.

  IF rreadti IS INITIAL.
    rreadti = '001000'.
  ENDIF.
  selection-sreadti = rreadti.

  IF rstartti IS INITIAL.
    rstartti = sy-uzeit - rreadti.
*    rstartti = sy-uzeit - '001000'.
    rstartti+4 = '00'.
  ENDIF.
  selection-stime = rstartti.

  IF rscreen IS INITIAL.
    rscreen = '*'.
  ELSEIF rscreen CO ' 0123456789'.
    sy-tfill = rscreen.
    WRITE sy-tfill TO rscreen USING EDIT MASK 'RR____'.
    TRANSLATE rscreen USING ' 0'.
  ENDIF.
  selection-sscreen = rscreen.

*AD
*  IF rreaddel IS INITIAL.
*    rreaddel = '000200'.
*  ENDIF.
*  selection-sreaddel = rreaddel.

  IF rwaitfac = 0.
    rwaitfac = 150.
  ENDIF.
  selection-swaitfac = rwaitfac.

ENDFORM.                               "RETRANS_SELECTION_DATA

*----------------------------------------------------------------------*
*       FORM TRANSFER_SELECTION_DATA                                   *
*----------------------------------------------------------------------*
*       Transfer der Selektionsdaten (selection-s___ -> r___ )         *
*----------------------------------------------------------------------*
FORM transfer_selection_data.

  rday = selection-sdat.
  IF rday IS INITIAL.
    rday = sy-datum.
    IF selection-stime < sy-uzeit.
      rstartti = selection-stime.
    ELSE.
      CLEAR rstartti.
    ENDIF.
  ELSE.
    rstartti = selection-stime.
  ENDIF.

  IF rday > sy-datum                             OR
     ( rday = sy-datum AND rstartti > sy-uzeit ).
    rday = sy-datum.
    CLEAR rstartti.
  ENDIF.

  IF rstartti IS INITIAL.
    rstartti = sy-uzeit - rreadti.
*    rstartti = sy-uzeit - '001000'.
    rstartti+4 = '00'.
  ENDIF.

  ruser = selection-sbenu.
  IF ruser IS INITIAL.
    ruser = '*'.
  ENDIF.

  rmandt = selection-smandt.
  IF rmandt IS INITIAL.
    rmandt = '*'.
  ENDIF.

  rtcode = selection-stcod.
  IF rtcode IS INITIAL.
    rtcode = '*'.
  ENDIF.

  rprogram = selection-sprogram.
  IF rprogram IS INITIAL.
    rprogram = '*'.
  ENDIF.

  tasktype = selection-stask.
  IF tasktype IS INITIAL.
    tasktype = '*'.
  ENDIF.

  IF selection-sscreen IS INITIAL.
    selection-sscreen = '*'.
  ELSEIF selection-sscreen CO ' 0123456789'.
    sy-tfill = selection-sscreen.
    WRITE sy-tfill TO selection-sscreen USING EDIT MASK 'RR____'.
    TRANSLATE selection-sscreen USING ' 0'.
  ENDIF.
  rscreen = selection-sscreen.

*AD
*  rwpid = selection-swpid.
*  IF rwpid IS INITIAL.
*    rwpid = '*'.
*  ENDIF.
*
  rrspti = selection-sresptime.
  rcputi = selection-scputime.
  rdbti  = selection-sdbtime.
  rchg   = selection-schg.
  rkbyte = selection-sbytes.

  IF selection-sseltime1 <> 'X'.       "Terminal ID statt Account nehmen
    TRANSLATE ruser TO UPPER CASE.
    "#EC TRANSLANG "Accountname in Grossbuchstaben
  ENDIF.

*AD
*  rreaddel = selection-sreaddel.
*  IF rreaddel IS INITIAL.
*    rreaddel = '000200'.
*  ENDIF.

  rreadti = selection-sreadti.
  IF rreadti IS INITIAL.
    rreadti = '001000'.
  ENDIF.

  rwaitfac = selection-swaitfac.
  IF rwaitfac = 0.
    rwaitfac = 150.
  ENDIF.

  rmode = selection-smode.
  IF rmode IS INITIAL.
    rmode = 'a'.
  ENDIF.

*@AD new(7.0)
  IF rtcodf IS NOT INITIAL AND rtcodf NE '*'.
    lv0_filter-tcode = rtcodf.
  ELSE.
    lv0_filter-tcode = selection-stcod.
  ENDIF.
  IF rprogf IS NOT INITIAL AND rprogf NE '*'.
    lv0_filter-program = rprogf.
  ELSE.
    lv0_filter-program = selection-sprogram.
  ENDIF.

ENDFORM.                               "TRANSFER_SELECTION_DATA

*----------------------------------------------------------------------*
*       MODULE D0010_ANWAHL                                            *
*----------------------------------------------------------------------*
* Setzt Datum und Status, Ausblenden von Selektionskriterien           *
*----------------------------------------------------------------------*
MODULE d0010_anwahl OUTPUT.

  SET TITLEBAR '002'.                  "Select statistical records
  SET PF-STATUS 'SELECT' EXCLUDING 'NOAU'.

* initialize some values if not yet set (r... -> selection-s...)
* (nur falls initial aufgerufen!)
*  if okcode = space and save_fcode = space.
  IF selection IS INITIAL.
    PERFORM retrans_selection_data.
  ELSE.
* falls Aufruf des Dynpros innerhalb des Programms (nicht initial)
    CASE selection-smode.
      WHEN 'a' OR 'A'.
        mode_button_1 = '@5B@'.
        mode_button_2 = mode_button_3 = '@5C@'.
      WHEN 'b' OR 'B'.
        mode_button_2 = '@5B@'.
        mode_button_1 = mode_button_3 = '@5C@'.
      WHEN 'c' OR 'C'.
        mode_button_3 = '@5B@'.
        mode_button_2 = mode_button_1 = '@5C@'.
*@AD new(7.0)
*        selection-stcod    = '*'.
*        selection-sprogram = '*'.
        selection-stask    = '*'.
    ENDCASE.
  ENDIF.

* include statistics from memory? default: yes
  buffer_checkbox = 'X'.
* include application statistics? default: no
*  include_appl_stat = ' '. nicht immer zurücksetzen!

* some selections not in all display modes possible
  LOOP AT SCREEN.
* selection of Tcode, Program and Tasktype not possible in mode c)
*    IF screen-group3 = 'TPT'.
*      IF mode_button_1 = '@5B@' OR mode_button_2 = '@5B@'.
*        screen-input = '1'.
*      ELSE.
*        screen-input = '0'.
*      ENDIF.
*    ENDIF.
*@AD new(7.0) only Tasktype not possible in mode c)
    IF screen-group3 = 'TPT' AND screen-name = 'SELECTION-STASK'.
      IF mode_button_1 = '@5B@' OR mode_button_2 = '@5B@'.
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                             "D0010_ANWAHL

*----------------------------------------------------------------------*
*       MODULE D0010_SUBMIT                                            *
*----------------------------------------------------------------------*
* Ansteuern des entsprechenden Reports, Umstellen der Modusknöpfe      *
*----------------------------------------------------------------------*
MODULE d0010_submit.

  DATA: text(50), text2(50),
        wa_server_choose_list TYPE sapwlslins.

  CASE okcode.
* Anwahl von display mode a) -------------------------------------------
    WHEN 'ALL'.
      mode_button_1 = '@5B@'.
      mode_button_2 = '@5C@'.
      mode_button_3 = '@5C@'.
      selection-smode = 'a'.
      SET SCREEN '0010'.

* Anwahl von display mode b) -------------------------------------------
    WHEN 'ALLS'.
      mode_button_1 = '@5C@'.
      mode_button_2 = '@5B@'.
      mode_button_3 = '@5C@'.
      selection-smode = 'b'.
      SET SCREEN '0010'.

* Anwahl von display mode c) -------------------------------------------
    WHEN 'SUMM'.
      mode_button_1 = '@5C@'.
      mode_button_2 = '@5C@'.
      mode_button_3 = '@5B@'.
      selection-smode = 'c'.
*@AD new(7.0)
*      selection-stcod    = '*'.
*      selection-sprogram = '*'.
      selection-stask    = '*'.
      SET SCREEN '0010'.

* Cancel processing ----------------------------------------------------
    WHEN 'CANC'.
      SET SCREEN  0. LEAVE SCREEN.

* Start processing -----------------------------------------------------
    WHEN 'STRT'.

* Fehler abfangen:
      IF selection-sreadti IS INITIAL OR selection-sreadti = space.
        text = 'The read time should not be zero!'.
        MESSAGE e333 WITH text.
* - The read time should not be zero.
      ENDIF.
      IF selection-stime = space OR selection-sdat IS INITIAL.
        MESSAGE e333 WITH 'Please select a start time and date!'.
* - Please select a start time and date.
      ENDIF.

*AD
* set read time delta if not set manually (OPTI)
*      IF read_delta_manually IS INITIAL.
*        selection-sreaddel = selection-sreadti DIV 5.
*      ENDIF.
*      IF selection-sreaddel < '000100'.
*        selection-sreaddel = '000100'.
*      ENDIF.

* include data from statistic buffer?
      IF buffer_checkbox = 'X'.
        buffer_flush = ' '.
      ELSE.
        buffer_flush = 'X'.
      ENDIF.

* set mode if it has not been changed
      IF selection-smode IS INITIAL.
        selection-smode = 'a'.
      ENDIF.

* retranslate selection -> r...
      PERFORM transfer_selection_data.

* start output and so on
      IF save_fcode IS INITIAL.
        LEAVE TO LIST-PROCESSING.
        PERFORM select_command USING 'INIT'.
        SET SCREEN 0. LEAVE SCREEN.
      ELSE.
        SET SCREEN 0. LEAVE SCREEN.
      ENDIF.

* further options (change the wait factor + set max. run time) ---------
    WHEN 'OPTI'.
      DATA: save_wait_factor    LIKE selection-swaitfac.
      CALL SCREEN '0070' STARTING AT 15 05 ENDING AT 75 23.
      SET SCREEN '0010'.

* select single servers ------------------------------------------------
    WHEN 'SERV'.
      IF server_choose_list IS INITIAL.
        PERFORM initialize_d0050.
      ENDIF.
      okcode = space.
      CALL SCREEN '0050' STARTING AT 12 03 ENDING AT 90 16.
      IF okcode = 'DONE'.
        selection-sonly = ' '.    "reinitialize server choice jtc
        REFRESH server_list.             "DK 14/01/2002
        PERFORM get_server_list.         "DK 14/01/2002
        LOOP AT server_choose_list INTO wa_server_choose_list.
          IF wa_server_choose_list-selected = space.
* remove servers from list
            DELETE server_list WHERE
                                  name = wa_server_choose_list-instance.
          ENDIF.
        ENDLOOP.
        okcode = space.
      ENDIF.
      SET SCREEN '0010'.

    WHEN OTHERS.
      MESSAGE e004.

  ENDCASE.
ENDMODULE.                             "D0010_SUBMIT

*---------------------------------------------------------------------*
*       MODULE AUTHORITY_CHECK                                        *
*---------------------------------------------------------------------*
*       Ausblenden Userauswahl bei Autorisierungsmangel               *
*---------------------------------------------------------------------*
MODULE authority_check OUTPUT.
  AUTHORITY-CHECK OBJECT 'S_TOOLS_EX'
  ID 'AUTH' FIELD 'S_TOOLS_EX_A'.
  IF sy-subrc <> 0.
    selection-sbenu = sy-uname.

    LOOP AT SCREEN.
      CHECK screen-group4 = 'USR'.
      MOVE '0' TO screen-input.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.                             "AUTHORITY_CHECK

*----------------------------------------------------------------------*
*       MODULE D0020_ANWAHL                                            *
*----------------------------------------------------------------------*
*       Setzt Status und Modus-Buttons.                                *
*----------------------------------------------------------------------*
MODULE d0020_anwahl OUTPUT.

  SET TITLEBAR '002'.                  "Select statistical records
  SET PF-STATUS 'SELECT' EXCLUDING 'STRT'.

  LOOP AT SCREEN.
    CASE selection-smode.
      WHEN 'c' OR 'C'.
        mode_button_1 = '@5C@'.
        mode_button_2 = '@5C@'.
        mode_button_3 = '@5B@'.
      WHEN 'b' OR 'B'.
        mode_button_1 = '@5C@'.
        mode_button_2 = '@5B@'.
        mode_button_3 = '@5C@'.
      WHEN OTHERS.
        mode_button_1 = '@5B@'.
        mode_button_2 = '@5C@'.
        mode_button_3 = '@5C@'.
    ENDCASE.
  ENDLOOP.
ENDMODULE.                             "D0020_ANWAHL

*----------------------------------------------------------------------*
*       MODULE D0020_SUBMIT                                            *
*----------------------------------------------------------------------*
*       Submit module 0020                                             *
*----------------------------------------------------------------------*
MODULE d0020_submit.
  SET SCREEN 0.
ENDMODULE.                             "D0020_SUBMIT

*&---------------------------------------------------------------------*
*&      Form  SUBMIT_CHANGE_MODE
*&---------------------------------------------------------------------*
*       Executes the change of the display mode.
*----------------------------------------------------------------------*
*      -->P_OKCODE  output from dynpro 0020                            *
*----------------------------------------------------------------------*
FORM submit_change_mode USING    p_okcode.

  DATA: save_mode        LIKE selection-smode,
        lines            TYPE i.

  save_mode = selection-smode.

  CASE p_okcode.
* Anwahl von display mode a)
    WHEN 'ALL'.
      selection-smode = rmode = 'a'.
      IF selection-smode <> save_mode.
        SUBTRACT 1 FROM sy-lsind.
        PERFORM write_mainlist USING rmode.
      ENDIF.
* Anwahl von display mode b)
    WHEN 'ALLS'.
      selection-smode = rmode = 'b'.
      IF selection-smode <> save_mode.
        SUBTRACT 1 FROM sy-lsind.
        DESCRIBE TABLE stats_b_lv1 LINES lines.
        IF lines = 0.
          PERFORM fill_b_level_1.
        ENDIF.
        PERFORM write_mainlist USING rmode.
      ENDIF.
* Anwahl von display mode c)
    WHEN 'SUMM'.
      selection-smode = rmode = 'c'.
      IF selection-smode <> save_mode.
        SUBTRACT 1 FROM sy-lsind.
        DESCRIBE TABLE stats_c_lv0 LINES lines.
        IF lines = 0.
          PERFORM fill_c_level_0.
        ENDIF.
        PERFORM write_mainlist USING rmode.
      ENDIF.
  ENDCASE.

ENDFORM.                               " SUBMIT_CHANGE_MODE

*&---------------------------------------------------------------------*
*&      Form  SELECT_LINE
*&---------------------------------------------------------------------*
*       What to do at line selection -> take-off to details or open
*       a folder (open summs).
*----------------------------------------------------------------------*
*      -->P_FIELDNAME  field from get cursor field                     *
*----------------------------------------------------------------------*
FORM select_line USING    p_fieldname.

* save hidden indeces and tabelle
  save_record_idx = navigate-record_idx.
  save_main_idx   = navigate-main_idx.
  save_tabelle    = tabelle.
  save_details_code = 'ALLD'.
  save_lilli      = sy-lilli.
* save list position
  PERFORM save_list_position.
  CASE rmode.
    WHEN 'c' OR 'C'.
      CASE p_fieldname.
        WHEN 'SYM_OPEN_FOLDER' OR 'SYM_PLUS_FOLDER'.
* open or close that level (0 or 1) of one record
          IF tabelle = 'stats_c_lv0'.
            READ TABLE stats_c_lv0 INTO wa_stats_c_lv0
                                                 INDEX save_clv0_tabix.
            IF wa_stats_c_lv0-opened = space.
              wa_stats_c_lv0-opened = 'X'.
            ELSE.
              wa_stats_c_lv0-opened = space.
            ENDIF.
            MODIFY stats_c_lv0 FROM wa_stats_c_lv0
                                                 INDEX save_clv0_tabix.
          ELSEIF tabelle = 'stats_c_lv1'.
            READ TABLE stats_c_lv1 INTO wa_stats_c_lv1
                                                 INDEX save_clv1_tabix.
            IF wa_stats_c_lv1-opened = space.
              wa_stats_c_lv1-opened = 'X'.
            ELSE.
              wa_stats_c_lv1-opened = space.
            ENDIF.
            MODIFY stats_c_lv1 FROM wa_stats_c_lv1
                                                 INDEX save_clv1_tabix.
          ENDIF.
          SUBTRACT 1 FROM sy-lsind.
          PERFORM write_mainlist USING rmode.
          PERFORM restore_list_position.
        WHEN 'SYM_FOLDER' OR 'SY-VLINE'
                  OR 'LINE_BOTTOM_LEFT_CRONER' OR 'SPACE'.
* do nothing (no valid place hit)
        WHEN 'WA_KEY_TEXTE-TEXT' OR 'WA_OKEY_TEXTE-TEXT'.
* application statistik record => get full text & techn. text
          IF tabelle = 'stats_c_lv1'.
            READ TABLE key_texte WITH KEY
                                     okey = wa_stats_c_lv1-action(30)
                                  INTO wa_key_texte.
            IF sy-subrc = 0.
              wa_key_texte-id = wa_stats_c_lv1-main-appl_info.
              PERFORM write_okeytext.
            ELSE.
              MESSAGE s333 WITH 'No detailed information found'.
            ENDIF.
          ELSE.
            READ TABLE key_texte WITH KEY okey = wa_main-okey
                                  INTO wa_key_texte.
            IF sy-subrc = 0.
              wa_key_texte-id = wa_main-appl_info.
              PERFORM write_okeytext.
            ELSE.
              MESSAGE s333 WITH 'No detailed information found'.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
* display details (sub list)
          PERFORM write_details USING rmode 'ALLD'.
      ENDCASE.                         "P_FIELDNAME
    WHEN OTHERS.
      IF p_fieldname = 'WA_KEY_TEXTE-TEXT'.
* application statistik record => get full text & techn. text
        IF tabelle = 'stats_c_lv1'.
          READ TABLE key_texte WITH KEY
                                   okey = wa_stats_c_lv1-action(30)
                                INTO wa_key_texte.
          IF sy-subrc = 0.
            wa_key_texte-id = wa_stats_c_lv1-main-appl_info.
            PERFORM write_okeytext.
          ELSE.
            MESSAGE s333 WITH 'No detailed information found'.
          ENDIF.
        ELSE.
          READ TABLE key_texte WITH KEY okey = wa_main-okey
                                INTO wa_key_texte.
          IF sy-subrc = 0.
            wa_key_texte-id = wa_main-appl_info.
            PERFORM write_okeytext.
          ELSE.
            MESSAGE s333 WITH 'No detailed information found'.
          ENDIF.
        ENDIF.
      ELSE.
* display details (sub list)
        PERFORM write_details USING rmode 'ALLD'.
      ENDIF.
  ENDCASE.                             "RMODE

ENDFORM.                               " SELECT_LINE

*&---------------------------------------------------------------------*
*&      Form INITIALIZE_D0030
*&---------------------------------------------------------------------*
*       See if internal table for output is filled/fill it, get the
*       number of rows and the expected current line size.
*----------------------------------------------------------------------*
FORM initialize_d0030.

* initialize save tables (for to cancel after calculation with changed
* fields and save header2 - overwritten by table control!)
  CLEAR: save_ausgabe, save_header2.
  REFRESH: save_ausgabe, save_header2.
  CLEAR fcode.

* fill, if necessary, table ausgabe (contains all possible outp. fields)
  READ TABLE ausgabe INDEX 1.
  IF sy-subrc <> 0.
    PERFORM fill_ausgabe.
  ENDIF.

* get number of possible output fields for to control the submit of the
* table control and fill the save tables
  DESCRIBE TABLE ausgabe LINES rows.
  LOOP AT ausgabe.
    IF ausgabe-header2 IS INITIAL.
* header2 ist bisweilen leer und wird dann fälschlicherweise vom Table
* Control überschrieben bzw. gefüllt!
      save_header2-index = sy-tabix.
      APPEND save_header2.
    ENDIF.
* um das Dynpro ohne Änderung verlassen zu können (CANC)
    save_ausgabe = ausgabe-ausgabe.
    APPEND save_ausgabe.
  ENDLOOP.

* get present line size (allways mode a) and b) which are wider)
  PERFORM get_breite USING 'a'.
  "Hostnamenerweiterung (allways without server abbrev. -> wider)
  IF server_abbrev = 'X'.
    breite = breite + 23.
  ENDIF.

ENDFORM.                               " INITIALIZE_D0030

*&---------------------------------------------------------------------*
*&      Module  D0030_ANWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       Fill the table control.
*----------------------------------------------------------------------*
MODULE d0030_anwahl OUTPUT.

  SET PF-STATUS 'OUTPUT'.
  SET TITLEBAR '005'.

* set mark line flag according to current ausgabe table
  tc_flag = ausgabe-ausgabe.

ENDMODULE.                             " D0030_ANWAHL  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  D0030_SUBMIT  INPUT
*&---------------------------------------------------------------------*
*       Change flag in table control, calculate new expected line size
*       and complete entry.
*----------------------------------------------------------------------*
MODULE d0030_submit INPUT.

  CASE fcode.
    WHEN 'CANC'.
* undo selection and leave
      LOOP AT save_ausgabe.
        READ TABLE ausgabe INDEX sy-tabix.
        IF ausgabe-ausgabe <> save_ausgabe-ausgabe.
          ausgabe-ausgabe = save_ausgabe-ausgabe.
        ENDIF.
        MODIFY ausgabe INDEX sy-tabix.
      ENDLOOP.
      SET SCREEN 0. LEAVE SCREEN.

    WHEN 'STND' OR 'TIME' OR 'DB'.
* use a number of preselected fields
      CASE fcode.
        WHEN 'STND'.
* use standard -> fill ausgabe from database (SAPWLWAMAI)
          REFRESH ausgabe. CLEAR ausgabe.
          PERFORM fill_ausgabe.
        WHEN OTHERS.
* use other preselected fields
          IF fcode = 'TIME'.
* time analysis
            LOOP AT ausgabe.
              IF ausgabe-name = 'respti'
                 OR ausgabe-name = 'timeinwp'
                 OR ausgabe-name = 'rollti'
*                 or ausgabe-name = 'cpicti'
                 OR ausgabe-name = 'cputi'
                 OR ausgabe-name = 'dbcallti'
                 OR ausgabe-name = 'generateti'
                 OR ausgabe-name = 'loadti'
                 OR ausgabe-name = 'lockti'
                 OR ausgabe-name = 'procti'
                 OR ausgabe-name = 'queueti'
                 OR ausgabe-name = 'guitime'
                 OR ausgabe-name = 'guinettime'.
                ausgabe-ausgabe = 'X'.
              ELSE.
                ausgabe-ausgabe = space.
              ENDIF.
              MODIFY ausgabe.
            ENDLOOP.
          ELSE.                        "fcode = 'DB'
* analysis of DB requests
            LOOP AT ausgabe.
*              if ausgabe-name = 'avtdb'   "wg. Hostnamenerw. herausgen.
              IF ausgabe-name = 'avtdel'
                 OR ausgabe-name = 'avtins'
                 OR ausgabe-name = 'avtreaddir'
                 OR ausgabe-name = 'avtreadseq'
                 OR ausgabe-name = 'avtupd'
*                 or ausgabe-name = 'dbcalls' "wg. Hostnamenerw. herausg
                 OR ausgabe-name = 'delcnt'
                 OR ausgabe-name = 'inscnt'
                 OR ausgabe-name = 'readdircnt'
                 OR ausgabe-name = 'readseqcnt'
                 OR ausgabe-name = 'updcnt'.
                ausgabe-ausgabe = 'X'.
              ELSE.
                ausgabe-ausgabe = space.
              ENDIF.
              MODIFY ausgabe.
            ENDLOOP.
          ENDIF.
      ENDCASE.
      PERFORM get_breite USING 'a'.
      "Hostnamenerweiterung
      IF server_abbrev = 'X'.
        breite = breite + 23.
      ENDIF.
      save_fcode = 'STRT'.
      SET SCREEN 0. LEAVE SCREEN.

    WHEN OTHERS.
* modify ausgabe according to tc_flag (but only the ausgabe field!)
      ausgabe-ausgabe = tc_flag.
* header2 muß wenn 'leer' zurückgesetzt werden (Table Control über-
* schreibt leere Felder mit Inhalten der ersten 14 Zeilen!)
      READ TABLE save_header2 WITH KEY
                                  INDEX = output_fields-current_line.
      IF sy-subrc = 0.
        ausgabe-header2 = space.
      ENDIF.
      MODIFY ausgabe INDEX output_fields-current_line.
* check last loop reached?
      size_modula = output_fields-current_line MOD 14.
      " 14 = Anzahl Zeilen Table Control
      IF size_modula = 0 OR output_fields-current_line = rows.
* calculate the new expected line size (always for modes a) and b) which
* are wider than c))
        PERFORM get_breite USING 'a'.
"Hostnamenerweiterung! Screensize muß auch für abbrev = ' ' reichen
        IF server_abbrev = 'X'.
          breite = breite + 23.
        ENDIF.
        IF fcode = 'DONE'.
*         or ( fcode <> 'CALS' and okcode = 'STRT' ).   " why???
* check line size < 256
*          IF breite > 255.
          IF breite > 1023.
            MESSAGE i333 WITH 'To many fields selected! The'
                              'expected line size exceeds the'
                              'maximum of 1023 figures.'.
            fcode = space.
          ELSE.
* leave the screen to start the new output
            save_fcode = 'STRT'.
            SET SCREEN 0. LEAVE SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDMODULE.                             " D0030_SUBMIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  D0040_ANWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       Dynpro for changing the wait factor.
*----------------------------------------------------------------------*
MODULE d0040_anwahl OUTPUT.

  SET TITLEBAR '006'.
  SET PF-STATUS 'SELECT'.

ENDMODULE.                             " D0040_ANWAHL  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  D0040_SUBMIT  INPUT
*&---------------------------------------------------------------------*
*       Dynpro for changing the wait factor.
*----------------------------------------------------------------------*
MODULE d0040_submit INPUT.

  IF okcode = 'STRT' AND NOT save_fcode IS INITIAL.
    save_fcode = 'CHWA'.
  ENDIF.
  LEAVE SCREEN.                        "set screen 0. vor ...

ENDMODULE.                             " D0040_SUBMIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  D0050_ANWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       Dynpro for the server selection.
*----------------------------------------------------------------------*
MODULE d0050_anwahl OUTPUT.

* status and so on
  SET PF-STATUS 'OUTPUT'.
  SET TITLEBAR '007'.

ENDMODULE.                             " D0050_ANWAHL  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_D0050
*&---------------------------------------------------------------------*
*       Neccessary for initialization of server selection. (Get list
*       of servers, create selection list...)
*----------------------------------------------------------------------*
FORM initialize_d0050.

  DATA: wa_server_choose_list  TYPE sapwlslins.

* clear selection list
  REFRESH server_choose_list. CLEAR wa_server_choose_list.

* get list of servers
  PERFORM get_server_list.

* fill selection list
  LOOP AT server_list.
    wa_server_choose_list-instance = server_list-name.
    wa_server_choose_list-host     = server_list-host.
    "initially all servers are selected
    wa_server_choose_list-selected = 'X'.
    APPEND wa_server_choose_list TO server_choose_list.
  ENDLOOP.

ENDFORM.                               " INITIALIZE_D0050

*&---------------------------------------------------------------------*
*&      Module  SELECT_SERVER_PAI  INPUT
*&---------------------------------------------------------------------*
*       Modify sever selection table from dynpro.
*----------------------------------------------------------------------*
MODULE select_server_pai INPUT.

  MODIFY server_choose_list FROM  sapwlslins
                              INDEX server_selection-current_line.

ENDMODULE.                             " SELECT_SERVER_PAI  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_D0050  INPUT
*&---------------------------------------------------------------------*
*       React on pushed buttons in dynpro for server selection.
*----------------------------------------------------------------------*
MODULE user_command_d0050 INPUT.

  CASE okcode.
    WHEN 'DONE' OR 'STRT' OR 'CANC'.                        "#EC NOTEXT
      SET SCREEN 0. LEAVE SCREEN.

    WHEN 'SLAL'.            "select all instances          "#EC NOTEXT
      LOOP AT server_choose_list INTO wa_server_choose_list.
        wa_server_choose_list-selected = 'X'.               "#EC NOTEXT
        MODIFY server_choose_list FROM wa_server_choose_list.
      ENDLOOP.

    WHEN 'DSAL'.            "deselect all instances        "#EC NOTEXT
      LOOP AT server_choose_list INTO wa_server_choose_list.
        wa_server_choose_list-selected = space.
        MODIFY server_choose_list FROM wa_server_choose_list.
      ENDLOOP.

  ENDCASE.

ENDMODULE.                             " USER_COMMAND_D0050  INPUT

*&---------------------------------------------------------------------*
*&      Module  D0060_ANWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       Dynpro to set the read time delta.
*----------------------------------------------------------------------*
*MODULE d0060_anwahl OUTPUT.
*
*  SET PF-STATUS 'SELECT'.
*  SET TITLEBAR '008'.
*
**AD  runtime = selection-sreadti + selection-sreaddel.
*  runtime = selection-sreadti.
*
*ENDMODULE.                             " D0060_ANWAHL  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  D0060_SUBMIT  INPUT
*&---------------------------------------------------------------------*
*       Dynpro to set the read time delta.
*----------------------------------------------------------------------*
*AD
*MODULE d0060_submit INPUT.
*
*  IF okcode = 'STRT' AND NOT save_fcode IS INITIAL.
*    save_fcode = 'MAXR'.
*  ENDIF.
*
*  CASE okcode.
*    WHEN 'STRT'.
*      selection-sreaddel = runtime - selection-sreadti.
*      IF selection-sreaddel > '001500'.
*        MESSAGE i333 WITH 'The max. run time must be bigger than'
*                          'the read time but less than'
*                          'read time + 15 min. !'.
*        runtime = selection-sreadti / 5 + selection-sreadti.
*        selection-sreaddel = runtime - selection-sreadti.
*        IF selection-sreaddel < '000100'.
*          runtime = selection-sreadti + '000100' .
*          selection-sreaddel = '000100'.
*        ENDIF.
*        SET SCREEN '0060'.
*      ELSE.
*        LEAVE SCREEN.
*      ENDIF.
*    WHEN OTHERS.
*      runtime = selection-sreadti / 5 + selection-sreadti.
*      selection-sreaddel = runtime - selection-sreadti.
*      IF selection-sreaddel < '000100'.
*        runtime = selection-sreadti + '000100' .
*        selection-sreaddel = '000100'.
*      ENDIF.
*      LEAVE SCREEN.
*  ENDCASE.
*
*ENDMODULE.                             " D0060_SUBMIT  INPUT
*
*&---------------------------------------------------------------------*
*&      Module  D0070_ANWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       Dynpro for further selection options, i.e. to change the
*       wait time (for data collection via RFCs) and to set the read
*       time delta ('Vor- und Nachlesezeit' für über den Meßzeitraum
*       hinausreichende Transaktionen). Initialize values.
*----------------------------------------------------------------------*
MODULE d0070_anwahl OUTPUT.

  SET PF-STATUS 'SELECT'.
  SET TITLEBAR '011'.

*AD
*  IF read_delta_manually IS INITIAL.
*    selection-sreaddel = selection-sreadti / 5.
*    runtime = selection-sreadti + selection-sreaddel.
*  ENDIF.

  save_wait_factor = selection-swaitfac.

ENDMODULE.                             " D0070_ANWAHL  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  D0070_SUBMIT  INPUT
*&---------------------------------------------------------------------*
*       Dynpro for further selection options, i.e. to change the
*       wait time (for data collection via RFCs) and to set the read
*       time delta ('Vor- und Nachlesezeit' für über den Meßzeitraum
*       hinausreichende Transaktionen). Realize changes.
*----------------------------------------------------------------------*
MODULE d0070_submit INPUT.

  CASE okcode.
    WHEN 'STRT'.
* waitfactor:
      IF selection-swaitfac < 1.
        selection-swaitfac = 1.
      ENDIF.
      rwaitfac = selection-swaitfac.
*AD
* read time delta:
*      selection-sreaddel = runtime - selection-sreadti.
*      IF selection-sreaddel > '001500'.
*        MESSAGE i333 WITH 'The max. run time must be bigger than'
*                          'the read time but less than'
*                          'read time + 15 min. !'.
*        runtime = selection-sreadti / 5 + selection-sreadti.
*        selection-sreaddel = runtime - selection-sreadti.
*        IF selection-sreaddel < '000100'.
*          runtime = selection-sreadti + '000100' .
*          selection-sreaddel = '000100'.
*        ENDIF.
*        SET SCREEN '0070'.
*      ELSE.
*        read_delta_manually = 'X'.
      LEAVE SCREEN.
*      ENDIF.
    WHEN OTHERS.
* reset both values:
      selection-swaitfac = save_wait_factor.
*      runtime = selection-sreadti / 5 + selection-sreadti.
*      selection-sreaddel = runtime - selection-sreadti.
*      IF selection-sreaddel < '000100'.
*        runtime = selection-sreadti + '000100' .
*        selection-sreaddel =  '000100'.
*      ENDIF.
*      read_delta_manually = ' '.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                             " D0070_SUBMIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  D0080_ANWAHL  OUTPUT
*&---------------------------------------------------------------------*
*       Dynpro mit AS-Funktions-Details (Hotspot)
*----------------------------------------------------------------------*
MODULE d0080_anwahl OUTPUT.

  SET PF-STATUS 'SELECT' EXCLUDING 'STRT'.
  SET TITLEBAR '014'.

ENDMODULE.                             " D0080_ANWAHL  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0090  OUTPUT
*&---------------------------------------------------------------------*
*       Dynpro für die Auswahl des Modus c)-Level 0-Filters.
*----------------------------------------------------------------------*
MODULE status_0090 OUTPUT.

  IF lv0_filter-tcode <> '*' OR lv0_filter-program <> '*' OR
     lv0_filter-transid <> '*'.
    SET PF-STATUS 'FILTER'.
  ELSE.
    SET PF-STATUS 'FILTER' EXCLUDING 'DELF'.
  ENDIF.
  SET TITLEBAR '016'.

  save_filter-tcode   = lv0_filter-tcode.
  save_filter-program = lv0_filter-program.
  save_filter-transid = lv0_filter-transid.
  CLEAR: fcode, save_fcode.

ENDMODULE.                             " STATUS_0090  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0090  INPUT
*&---------------------------------------------------------------------*
*       Get filter, close popup, ...
*----------------------------------------------------------------------*
MODULE user_command_0090 INPUT.

  CASE fcode.
    WHEN 'CANC'.
      lv0_filter-tcode   = save_filter-tcode.
      lv0_filter-program = save_filter-program.
      lv0_filter-transid = save_filter-transid.
    WHEN 'DELF'.
      lv0_filter-tcode   = lv0_filter-program = '*'.
      lv0_filter-transid = '*'.
    WHEN OTHERS.
  ENDCASE.

  save_fcode = fcode.
  SET SCREEN 0. LEAVE SCREEN.

ENDMODULE.                             " USER_COMMAND_0090  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_TASK_FORM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_task_form INPUT.
  DATA: BEGIN OF helptable OCCURS 10,
          field LIKE selection-stask,
          expl(32),
        END OF helptable.

  DATA: BEGIN OF field_tab OCCURS 10.
          INCLUDE STRUCTURE help_value.
  DATA: END OF field_tab.
  DATA: helpfield(30).


  REFRESH helptable.

  helptable-field = '*'.
  helptable-expl = '  Total'.
  APPEND helptable.
  helptable-field = 'D'.
  helptable-expl = '  Dialog'.
  APPEND helptable.
  helptable-field = 'U'.
  helptable-expl = '  Update'.
  APPEND helptable.
  helptable-field = 'B'.
  helptable-expl = '  Batch'.
  APPEND helptable.
  helptable-field = 'E'.
  helptable-expl = '  Enqueue'.
  APPEND helptable.
  helptable-field = 'S'.
  helptable-expl = '  Spool'.
  APPEND helptable.
  helptable-field = 'Y'.
  helptable-expl = '  Bufsync'.
  APPEND helptable.
  helptable-field = 'A'.
  helptable-expl = '  Autoabap'.
  APPEND helptable.
  helptable-field = '2'.
  helptable-expl = '  Update2'.
  APPEND helptable.
  helptable-field = 'C'.
  helptable-expl = '  CPIC'.
  APPEND helptable.
  helptable-field = 'R'.
  helptable-expl = '  RFC'.
  APPEND helptable.
  helptable-field = 'L'.
  helptable-expl = '  ALE'.
  APPEND helptable.
  helptable-field = 'H'.
  helptable-expl = '  HTTP'.
  APPEND helptable.
  helptable-field = 'T'.
  helptable-expl = '  HTTPS'.
  APPEND helptable.
  helptable-field = 'N'.
  helptable-expl = '  NNTP'.
  APPEND helptable.
  helptable-field = 'M'.
  helptable-expl = '  SMTP'.
  APPEND helptable.
  helptable-field = 'F'.
  helptable-expl = '  FTP'.
  APPEND helptable.

*@AD New (7.0)
  helptable-field = 'P'.
  helptable-expl = '  Ext. Plugin'.
  APPEND helptable.

  helptable-field = 'G'.
  helptable-expl = '  Auto-Task-Handler'.
  APPEND helptable.

  helptable-field = 'I'.
  helptable-expl = '  RPC(Remote Procedure Call)'.
  APPEND helptable.

  helptable-field = 'X'.
  helptable-expl = '  RFC inside VMC'.
  APPEND helptable.

  helptable-field = 'K'.
  helptable-expl = '  DDLog Cleanup'.
  APPEND helptable.

  helptable-field = 'Z'.
  helptable-expl = '  Delayed Task-Handler-Call'.
  APPEND helptable.

  helptable-field = 'J'.
  helptable-expl = '  Auto-Java'.
  APPEND helptable.

  helptable-field = 'O'.
  helptable-expl = '  LCOM'.
  APPEND helptable.

  helptable-field = 'V'.
  helptable-expl = '  HTTP/JSP'.
  APPEND helptable.

  helptable-field = 'W'.
  helptable-expl = '  HTTPS/JSP'.
  APPEND helptable.

  helptable-field = '?'.
  helptable-expl = '  Others'.
  APPEND helptable.

  REFRESH field_tab.

  field_tab-tabname = 'STATL'. field_tab-fieldname = 'STASK'.
  field_tab-selectflag = 'X'.       APPEND field_tab.


  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
*       EXPORTING
*            FIELDNAME                 = 'HOST_ID'
*            TABNAME                   = 'SYNCTAB'
       IMPORTING
            select_value              = selection-stask  " <----
       TABLES
            fields                    = field_tab
            valuetab                  = helptable
       EXCEPTIONS
            field_not_in_ddic         = 01
            more_then_one_selectfield = 02
            no_selectfield            = 03.

ENDMODULE.                 " HELP_TASK_FORM  INPUT

*APC *****************COPIAS DE FORMS

*&---------------------------------------------------------------------*
*&      Form  WRITE_MAINLIST
*&---------------------------------------------------------------------*
*       Set status for mainlist and create it.
*----------------------------------------------------------------------*
*  -->  rmode     Display mode
*----------------------------------------------------------------------*
FORM zwrite_mainlist USING rmode.

* local help data
  DATA: zeilen        TYPE i      VALUE 0,
        kbyte         TYPE p,
        output_flag.

* progress indicator
*
  output_flag = 'X'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = 'Preparing output'
    EXCEPTIONS
      OTHERS     = 1.
*
* clear workareas
  CLEAR: wa_stats_c_lv0, wa_stats_c_lv1, wa_main, wa_all_stats.
*
** set status, titlebar and start date
*  IF simple_mode = abap_true.
*    REFRESH buttons.
*    buttons-exclude = 'OPAL'.
*    APPEND buttons.
*    buttons-exclude = 'CLAL'.
*    APPEND buttons.
*    buttons-exclude = 'FILC'.
*    APPEND buttons.
*    buttons-exclude = 'CLII'.
*    APPEND buttons.
*    IF loghandle IS INITIAL.
*      buttons-exclude = 'ALOG'.
*      APPEND buttons.
*    ENDIF.
*    SET PF-STATUS 'MAIN_SMALL' EXCLUDING buttons.
*    SET TITLEBAR '012'.
*  ELSE.
*    CASE rmode.
*      WHEN 'c' OR 'C'.
*        IF overall_opened_level = 1.
*          SET PF-STATUS 'MAIN' EXCLUDING 'OPAL'.
*        ELSE.
*          SET PF-STATUS 'MAIN' EXCLUDING 'CLAL'.
*        ENDIF.
*        SET TITLEBAR '010'.
*      WHEN 'b' OR 'B'.
*        REFRESH buttons.
*        buttons-exclude = 'OPAL'.
*        APPEND buttons.
*        buttons-exclude = 'CLAL'.
*        APPEND buttons.
*        buttons-exclude = 'FILC'.
*        APPEND buttons.
*        SET PF-STATUS 'MAIN' EXCLUDING buttons.
*        SET TITLEBAR '012'.
*      WHEN OTHERS.
*        REFRESH buttons.
*        buttons-exclude = 'OPAL'.
*        APPEND buttons.
*        buttons-exclude = 'CLAL'.
*        APPEND buttons.
*        buttons-exclude = 'FILC'.
*        APPEND buttons.
*        buttons-exclude = 'CLII'.
*        APPEND buttons.
*        SET PF-STATUS 'MAIN' EXCLUDING buttons.
*        SET TITLEBAR '012'.
*    ENDCASE.
*  ENDIF.
  save_date = selection-sdat.

* check wether table ausgabe is filled (Outputfields)
  DESCRIBE TABLE ausgabe LINES zeilen.
  IF zeilen < 1.
    PERFORM fill_ausgabe.
  ENDIF.

* get and set line-size
  PERFORM get_breite USING rmode.
  NEW-PAGE LINE-SIZE breite.

* check wether records have been read
  READ TABLE all_stats TRANSPORTING NO FIELDS INDEX 1.
  IF sy-subrc <> 0.
    FORMAT COLOR 1 INVERSE.
    WRITE: /   sy-vline,
            30 '* No records received for current selection. *',
            AT breite sy-vline.
  ENDIF.

* write records ------------------------------------------------------ *
  CASE rmode.
*
    WHEN 'c' OR 'C'.
* display mode aggregation by trans-ID ---------------------------------
* level 0 ----------------------------
      DESCRIBE TABLE stats_c_lv0 LINES zeilen.
* check if stats_c_lv0 already has been filled
      IF zeilen = 0.
        PERFORM fill_c_level_0.
      ENDIF.
      " 11/99 copy-free loop:
      LOOP AT stats_c_lv0 INTO wa_stats_c_lv0.
* Sätze ohne Trans-ID ausfiltern
        CHECK wa_stats_c_lv0-main-transid <> space.
* check user is in selection
        CHECK wa_stats_c_lv0-main-account CP selection-sbenu.
* are further selection criteria set?
        IF selection-sresptime > 0 OR
           selection-scputime  > 0 OR
           selection-sdbtime   > 0 OR
           selection-sbytes    > 0 OR
           selection-schg      > 0.
          kbyte =   wa_stats_c_lv0-main-bytes / 1024.
          CHECK wa_stats_c_lv0-main-respti     >= selection-sresptime
          AND
                wa_stats_c_lv0-main-cputi      >= selection-scputime
                AND
                wa_stats_c_lv0-main-dbcallti   >= selection-sdbtime
                AND
                kbyte                          >= selection-sbytes
                AND
                  wa_stats_c_lv0-main-phychngcnt >= selection-schg.
        ENDIF.
* if a filter (mode c, level 0) is set, check selection
*@AD new(7.0)
*        lv0_filter-tcode = selection-stcod.
*        lv0_filter-program = selection-sprogram.
*
        IF lv0_filter-tcode <> '*'.
          CHECK ( wa_stats_c_lv0-main-tcode CP lv0_filter-tcode OR
                  wa_stats_c_lv0-action     CP lv0_filter-tcode ).
        ENDIF.
        IF lv0_filter-program <> '*'.
          CHECK ( wa_stats_c_lv0-main-report CP lv0_filter-program OR
                  wa_stats_c_lv0-action      CP lv0_filter-program ).
        ENDIF.
        IF lv0_filter-transid <> '*'.
          CHECK wa_stats_c_lv0-main-transid CP lv0_filter-transid.
        ENDIF.
* initialize some stuff
        save_clv0_tabix = sy-tabix.
        level = 0. opened_level = -1.
* get client info top level
        IF client_info_only = 'X'.
          CHECK wa_stats_c_lv0-subclir IS NOT INITIAL.
*AD          READ TABLE wa_stats_c_lv0-subclir INDEX 1 INTO wa_info.
          MOVE-CORRESPONDING wa_stats_c_lv0-subclir TO wa_info.
          PERFORM push_client_info." using wa_info.
        ENDIF.
* check if level 0 is opened
        IF wa_stats_c_lv0-opened <> space AND wa_stats_c_lv0-steps > 1.
          ADD 1 TO opened_level.
          PERFORM zwrite_main_record USING level opened_level.
* level 1 ----------------------------
          READ TABLE stats_c_lv1 INTO wa_stats_c_lv1 WITH KEY
                          main-transid = wa_stats_c_lv0-main-transid.
          IF sy-subrc <> 0.
            PERFORM fill_c_level_1 USING wa_stats_c_lv0-main-transid.
          ENDIF.
          " 11/99 copy-free loop:
          LOOP AT stats_c_lv1 INTO wa_stats_c_lv1
                     WHERE main-transid = wa_stats_c_lv0-main-transid.
            save_clv1_tabix = sy-tabix.
            level = 1. opened_level = 0.
* check if level 1 is opened and more than 1 step
            IF wa_stats_c_lv1-opened <> space
                               AND wa_stats_c_lv1-steps > 1.
              ADD 1 TO opened_level.
              PERFORM zwrite_main_record USING level opened_level.
* level 2 ----------------------------
              level = 2.
              LOOP AT navigate
                          WHERE transid = wa_stats_c_lv0-main-transid.
                CLEAR: wa_all_stats, wa_main.
                READ TABLE all_stats INTO wa_all_stats
                                            INDEX navigate-record_idx.
*                READ TABLE wa_all_stats-main INTO wa_main
*                                            INDEX navigate-main_idx.
                MOVE-CORRESPONDING wa_all_stats-main TO wa_main.
                CHECK wa_main-tasktype = wa_stats_c_lv1-main-tasktype.
                CHECK wa_main-instance = wa_stats_c_lv1-main-instance.
                CHECK wa_main-okey     = wa_stats_c_lv1-main-okey.
                PERFORM zwrite_main_record USING level opened_level.
              ENDLOOP.                 "navigate
* if level 1 is closed or only one step don't display level 2
            ELSE.
              PERFORM zwrite_main_record USING level opened_level.
            ENDIF.
*            clear wa_stats_c_lv1.     "copy-free loop instead
          ENDLOOP.                     "stats_c_lv1
* if level 0 is closed or only one step display only this level
        ELSE.
          PERFORM zwrite_main_record USING level opened_level.
        ENDIF.
*        clear wa_stats_c_lv0.         "copy-free loop instead
      ENDLOOP.                         "stats_c_lv0
    WHEN 'b' OR 'B'.
* display mode all records grouped by trans-ID -------------------------
      level = 2. opened_level = -1.
      DESCRIBE TABLE stats_b_lv1 LINES zeilen.
* check if stats_c_lv0 already has been filled
      IF zeilen = 0.
        PERFORM fill_b_level_1.
      ENDIF.
      LOOP AT stats_b_lv1.
* Sätze ohne Trans-ID ausfiltern
        CHECK stats_b_lv1-transid <> space.
* are further selection criteria set?
        IF selection-sresptime > 0 OR
           selection-scputime  > 0 OR
           selection-sdbtime   > 0 OR
           selection-sbytes    > 0 OR
           selection-schg      > 0.
          CHECK stats_b_lv1-maxrspti >= selection-sresptime AND
                stats_b_lv1-maxcputi >= selection-scputime  AND
                stats_b_lv1-maxdbti  >= selection-sdbtime   AND
                stats_b_lv1-maxkbyte >= selection-sbytes    AND
                stats_b_lv1-maxchg   >= selection-schg.
        ENDIF.
* set color and 'block flag' trans-ID
        IF sy-tabix = 1.
          save_transid = stats_b_lv1-transid.
          PERFORM flip_flop CHANGING colflag.
        ENDIF.
* check client info for client_info_only mode
        IF client_info_only = 'X'.
          PERFORM check_client_info USING output_flag wa_info.
        ENDIF.
* get records
        IF output_flag = 'X'.
          LOOP AT navigate WHERE transid = stats_b_lv1-transid.
            CLEAR: wa_all_stats, wa_main.
            READ TABLE all_stats INTO wa_all_stats
                                              INDEX navigate-record_idx.
*            READ TABLE wa_all_stats-main INTO wa_main
*                                              INDEX navigate-main_idx.
            MOVE-CORRESPONDING wa_all_stats-main TO wa_main.
* check selection criteria
            CHECK  wa_main-account CP selection-sbenu
               AND wa_main-tcode   CP selection-stcod
               AND wa_main-report  CP selection-sprogram
               AND ( wa_main-tasktype = rtasktype OR rtasktype = '00' ).
* output
            IF client_info_only = 'X' AND output_flag = 'X'.
              CLEAR output_flag.
              PERFORM push_client_info." using wa_info.
            ENDIF.
            PERFORM zwrite_main_record USING level opened_level.
          ENDLOOP.                       "navigate
        ENDIF.
      ENDLOOP.                         "stats_b_lv1
    WHEN OTHERS.
* display mode all records sorted by start time ------------------------
      SORT navigate BY startdate starttime wpid.
      level = 2. opened_level = -1.
      LOOP AT navigate.
        READ TABLE occurng_trids WITH KEY transid = navigate-transid.
        CHECK sy-subrc = 0.
        READ TABLE all_stats INTO wa_all_stats
                                            INDEX navigate-record_idx.
*        READ TABLE wa_all_stats-main INTO wa_main
*                                            INDEX navigate-main_idx.
        MOVE-CORRESPONDING wa_all_stats-main TO wa_main.
        CHECK  wa_main-account  CP selection-sbenu
           AND wa_main-tcode    CP selection-stcod
           AND wa_main-report   CP selection-sprogram
           AND wa_main-dynpronr CP selection-sscreen
           AND ( wa_main-tasktype = rtasktype OR rtasktype = '00' ).
* are further selection criteria set?
        IF selection-sresptime > 0 OR
           selection-scputime  > 0 OR
           selection-sdbtime   > 0 OR
           selection-sbytes    > 0 OR
           selection-schg      > 0.
          kbyte =   wa_main-bytes / 1024.
          CHECK wa_main-respti     >= selection-sresptime AND
                wa_main-cputi      >= selection-scputime  AND
                wa_main-dbcallti   >= selection-sdbtime   AND
                kbyte              >= selection-sbytes    AND
                wa_main-phychngcnt >= selection-schg.
        ENDIF.
        PERFORM zwrite_main_record USING level opened_level.
      ENDLOOP.                         "navigate
      SORT navigate BY transid startdate starttime.
  ENDCASE.
*  ULINE.

* get first and last row
  IF overall_opened_level > 0 AND ( rmode = 'C' OR rmode = 'c' ) .
    first_row = tcdef + 1.
* = Anzahl Headerzeilen + 1 jtc tcdef ist 10
  ELSE.
    first_row = tcdef.
  ENDIF.
  tcdef = 10."reinitialize tcdef change jtc
  last_row = sy-linno - 2.             " -2 wg. uline
* there is allways the client info in the first line.
  IF client_info_only = 'X'.
    first_row = first_row + 1.
  ENDIF.

ENDFORM.                               " ZWRITE_MAINLIST


*&---------------------------------------------------------------------*
*&      Form  ZWRITE_MAIN_RECORD
*&---------------------------------------------------------------------*
*       Write one record in main list.
*----------------------------------------------------------------------*
*      -->P_LEVEL  display level (0..2)                                *
*      -->P_OPENED_LEVEL  opened display level (-1..1)                 *
*----------------------------------------------------------------------*
FORM zwrite_main_record USING p_level p_opened_level.

* Borramos datos de usuarios especiales
  CHECK not ( wa_main-account = 'DDIC'
           OR wa_main-account = 'SAPSYS' ).

* Borramos datos de programas especiales.
  check NOT ( wa_main-report = 'RSABAPPROGRAM'
                  OR wa_main-report = 'SAPMSYST'
                  OR wa_main-report = 'RSBTCRTE'
                  OR wa_main-report = 'RSEDNAMT'
                  OR wa_main-report = 'RSCONN01'
                  OR wa_main-report = 'SAPRSEUT'
                  OR wa_main-report = 'SAPRSLOG'
                  OR WA_MAIN-REPORT = 'SAPMSJOB'
                  OR wa_main-report = 'RSDBAJOB'
                  or wa_main-report = 'RSM13000'
                  or wa_main-report = 'RSVRSRS3'
                  or wa_main-report(4) = 'RSWW'
                  OR wa_main-report(1) = '/'
                  OR wa_main-report = 'RFC'
                  OR wa_main-btcjobname(4) = 'SAP_'
                  OR wa_main-btcjobname(13) = 'AUTO_SESSION_'
                  OR wa_main-tcode = 'SMEN'
                  OR WA_MAIN-BTCJOBNAME = 'SWFSLSDLEX'
                  or wa_main-BTCJOBNAME = 'CODE_INSPECTOR_DELETION'
                  ).

  MOVE-CORRESPONDING wa_main TO i_aux.

    if i_aux-btcjobname is initial and
       i_aux-tcode(1) ne 'Z' and
       i_aux-tcode ne 'SE38' and
       i_aux-tcode(7) ne 'SESSION'.
      clear i_aux-report.
    endif.

  APPEND i_aux.
ENDFORM.                               " zWRITE_MAIN_RECORD
*&---------------------------------------------------------------------*
*&      Form  agrupar_informacion
*&---------------------------------------------------------------------*
form agrupar_informacion .

* Eliminamos ejecuciones muy próximas
  sort i_aux.
  data: l_time like stad_output-endti,
        l_secs type i,
        l_new,
        l_aux like i_aux.
  loop at i_aux.
    l_aux = i_aux.
    at new date.
      l_time = l_aux-endti.
      l_new  = 'X'.
    endat.
    if l_new = 'X'.
      clear l_new.
    else.
      l_secs = i_aux-endti - l_time.
      if l_secs < 300.
        l_time = i_aux-endti.
        continue.
      endif.
    endif.
    move-corresponding i_aux to i_datos.
    append i_datos.
    l_time = i_aux-endti.
  endloop.
  sort i_datos.

  loop at i_datos.
    if not i_datos-report is initial.
* Comprobamos que existan los registros
      select single * from  tadir
             where  pgmid     = 'R3TR'
             and    object    = 'PROG'
             and    obj_name  = i_datos-report.
      if sy-subrc ne 0.
        delete i_datos.
      endif.
    elseif i_datos-tcode is initial.
      delete i_datos.
    endif.
  endloop.

  if p_act = 'X'.
    loop at i_datos.
      clear zestadisticas.
      move-corresponding i_datos to zestadisticas.
      zestadisticas-fecha = i_datos-date.
      zestadisticas-jobname = i_datos-btcjobname.
      modify zestadisticas.
    endloop.

    submit ZBCU0007
      and return
     with s_FechaS = rday
     with p_actu  = 'X'.
  endif.

  if p_list = 'X'.
    perform alv_write_output tables i_datos using 'I_DATOS'.
  endif.


endform.                    " agrupar_informacion


*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EVENTS[]  text                                             *
*----------------------------------------------------------------------*
form alv_build_eventtab using p_events type slis_t_event.
  data: ls_event type slis_alv_event.
  call function 'REUSE_ALV_EVENTS_GET'
       exporting
            i_list_type = 0
       importing
            et_events   = p_events.
  read table p_events with key name = slis_ev_top_of_page
                           into ls_event.
  if sy-subrc = 0.
    move slis_ev_top_of_page to ls_event-form.
    modify p_events from ls_event index sy-tabix.
  endif.

*
  read table p_events with key name = slis_ev_pf_status_set
                           into ls_event.
  if sy-subrc = 0.
    move slis_ev_pf_status_set to ls_event-form.
    modify p_events from ls_event index sy-tabix.
  endif.

  read table p_events with key name = slis_ev_user_command
                           into ls_event.
  if sy-subrc = 0.
    move slis_ev_user_command to ls_event-form.
    modify p_events from ls_event index sy-tabix.
  endif.

endform.                               " BUILD_EVENTTAB

*&---------------------------------------------------------------------*
*&      Form  WRITE_OUTPUT
*&---------------------------------------------------------------------*
form alv_write_output tables pi_tabla
                      using pe_tabla.

  alv_repname = sy-repid.
  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
            i_program_name     = alv_repname
            i_internal_tabname = pe_tabla
            i_inclname         = alv_repname
       changing
            ct_fieldcat        = alv_fieldtab.
  if sy-subrc <> 0.
    write: 'SY-SUBRC: ', sy-subrc, 'REUSE_ALV_FIELDCATALOG_MERGE'.
  endif.
  call function 'REUSE_ALV_LIST_DISPLAY'
       exporting
            i_callback_program = alv_repname
            i_structure_name   = pe_tabla
            is_layout          = alv_layout
            it_fieldcat        = alv_fieldtab
            i_default          = 'A'
            i_save             = alv_g_save
            it_events          = alv_events[]
            it_sort            = alv_sort
            it_filter          = alv_filter
       tables
            t_outtab           = pi_tabla.
  if sy-subrc <> 0.
    write: 'SY-SUBRC: ', sy-subrc, 'REUSE_ALV_LIST_DISPLAY'.
  endif.
endform.                               " WRITE_OUTPUT
