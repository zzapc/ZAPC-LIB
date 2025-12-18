************************************************************************
* MÓDULO      : BC
* TIPO        : Utilidad
* TITULO      : Comenta programas con definiciones del diccionario
* DESCRIPCION :                                                        *
* AUTOR: Andrés Picazo  *
************************************************************************
REPORT zcomenta.

TABLES: dd03l,                                             "Campos de tabla
        dd04t, "R/3-DD: Textos de los elementos de datos
        dd02t,                                            "R/3-DD: Textos de tablas SAP
        dd40t, "Textos p.tipos de tabla
        t100.                                             "Mensajes



DATA: l_msgnr LIKE t100-msgnr,                             "Número de mensaje
      l_arbgb LIKE t100-arbgb,                             "Area funcional
      l_msgid LIKE t100-arbgb.                             "Area funcional

DATA: d_linea(72).

DATA: BEGIN OF source OCCURS 1000,
        line(255),
      END OF source.

DATA: v_entry   LIKE textpool-entry.                       "Language-dependent text in ABAP/4
DATA: i_cambios LIKE source OCCURS 1000 WITH HEADER LINE.

* ABAP Text Pool Definition
DATA: i_txt  LIKE textpool OCCURS 100 WITH HEADER LINE,
      l_long TYPE i.

* Definición de pool de textos ABAP
DATA: tabtext LIKE textpool OCCURS 1 WITH HEADER LINE.

PARAMETERS: program LIKE sy-repid DEFAULT sy-repid,
            tables  AS CHECKBOX DEFAULT 'X',
            like    AS CHECKBOX DEFAULT 'X',
            type    AS CHECKBOX DEFAULT 'X',
            param   AS CHECKBOX DEFAULT 'X',
            include AS CHECKBOX DEFAULT 'X',
            text    AS CHECKBOX DEFAULT 'X',
            message AS CHECKBOX DEFAULT 'X',
            test    AS CHECKBOX DEFAULT 'X',
            f72     AS CHECKBOX DEFAULT ''.




START-OF-SELECTION.
  DATA: l_aux1(80), l_aux2(80), l_aux3(80), l_grab, l_tabla(14),
  l_linea_ant(72), l_num_linea_ant TYPE i.


  FREE i_cambios.
  READ REPORT program INTO source.
  READ TEXTPOOL program INTO i_txt LANGUAGE sy-langu.
  CALL FUNCTION 'PRETTY_PRINTER'
    EXPORTING
      inctoo             = space
    TABLES
      ntext              = source
      otext              = source
    EXCEPTIONS
      enqueue_table_full = 1
      include_enqueued   = 2
      include_readerror  = 3
      include_writeerror = 4
      OTHERS             = 5.

  CLEAR l_grab.
  LOOP AT source.
    d_linea = source.
    IF d_linea(1) = '*'.
      l_linea_ant     = source.
      l_num_linea_ant = sy-tabix.
    ENDIF.
    CHECK d_linea(1) NE '*'.
    IF tables = 'X'.
      CLEAR l_tabla.
      SEARCH d_linea FOR 'TABLES'.
      IF sy-subrc = 0.
        TRANSLATE d_linea TO UPPER CASE.
        SPLIT d_linea AT 'TABLES'  INTO l_aux1 l_aux2.
        SPLIT l_aux2 AT ':'  INTO l_aux1 l_aux2.
        IF l_aux2 CA '.'.
          SPLIT l_aux2 AT '.'  INTO l_aux1 l_aux2.
          CONDENSE l_aux1.
          l_tabla = l_aux1.
        ELSE.
          l_grab = 'X'.
          SPLIT l_aux2 AT ','  INTO l_aux1 l_aux2.
          CONDENSE l_aux1.
          l_tabla = l_aux1.
        ENDIF.
      ENDIF.

      CHECK NOT d_linea CA '"'.
      TRANSLATE d_linea TO UPPER CASE.
      IF l_tabla IS INITIAL AND l_grab = 'X'.
        IF d_linea CA '.'.
          SPLIT d_linea AT '.'  INTO l_aux1 l_aux2.
          CONDENSE l_aux1.
          l_tabla = l_aux1.
          CLEAR l_grab.
        ELSE.
          SPLIT d_linea AT ','  INTO l_aux1 l_aux2.
          CONDENSE l_aux1.
          l_tabla = l_aux1.
        ENDIF.
      ENDIF.

      IF NOT l_tabla IS INITIAL.
        SELECT SINGLE ddtext FROM dd02t
          INTO dd02t-ddtext
         WHERE tabname = l_tabla
           AND ddlanguage = sy-langu.
        IF sy-subrc = 0.
          PERFORM anyade_comentario USING dd02t-ddtext.
        ENDIF.
        CLEAR l_tabla.
      ENDIF.
    ENDIF.

    IF param = 'X'.
      CLEAR l_tabla.
      SEARCH d_linea FOR 'PARAMETERS'.
      IF sy-subrc = 0.
        TRANSLATE d_linea TO UPPER CASE.
        SPLIT d_linea AT 'PARAMETERS'  INTO l_aux1 l_aux2.
        SPLIT l_aux2 AT ':'  INTO l_aux1 l_aux2.
        IF l_aux2 CA 'LIKE'.
          SPLIT l_aux2 AT 'LIKE'  INTO l_aux1 l_aux2.
          CONDENSE l_aux1.
          l_tabla = l_aux1.
        ENDIF.
        IF l_aux2 CN '.'.
          l_grab = 'X'.
        ENDIF.
      ENDIF.

      CHECK NOT d_linea CA '"'.
      TRANSLATE d_linea TO UPPER CASE.
      IF l_tabla IS INITIAL AND l_grab = 'X'.
        IF d_linea CA 'LIKE'.
          SPLIT d_linea AT 'LIKE'  INTO l_aux1 l_aux2.
          CONDENSE l_aux1.
          l_tabla = l_aux1.
        ENDIF.
        IF l_aux2 CA '.'.
          CLEAR l_grab.
        ENDIF.
      ENDIF.

      IF NOT l_tabla IS INITIAL.
        LOOP AT i_txt WHERE key = l_tabla
                        AND id = 'S'.
        ENDLOOP.
        IF sy-subrc = 0.
          v_entry = i_txt-entry+8.
          PERFORM anyade_comentario USING v_entry.
        ENDIF.
        CLEAR l_tabla.
      ENDIF.
    ENDIF.

    IF param = 'X'.
      CLEAR l_tabla.
      SEARCH d_linea FOR 'SELECT-OPTIONS'.
      IF sy-subrc = 0.
        TRANSLATE d_linea TO UPPER CASE.
        SPLIT d_linea AT 'SELECT-OPTIONS'  INTO l_aux1 l_aux2.
        SPLIT l_aux2 AT ':'  INTO l_aux1 l_aux2.
        IF l_aux2 CA 'FOR'.
          SPLIT l_aux2 AT 'FOR'  INTO l_aux1 l_aux2.
          CONDENSE l_aux1.
          l_tabla = l_aux1.
        ENDIF.
        IF l_aux2 CN '.'.
          l_grab = 'X'.
        ENDIF.
      ENDIF.

      CHECK NOT d_linea CA '"'.
      TRANSLATE d_linea TO UPPER CASE.
      IF l_tabla IS INITIAL AND l_grab = 'X'.
        IF d_linea CA 'FOR'.
          SPLIT d_linea AT 'FOR'  INTO l_aux1 l_aux2.
          CONDENSE l_aux1.
          l_tabla = l_aux1.
        ENDIF.
        IF l_aux2 CA '.'.
          CLEAR l_grab.
        ENDIF.
      ENDIF.

      IF NOT l_tabla IS INITIAL.
        LOOP AT i_txt WHERE key = l_tabla
                        AND id  = 'S'.
        ENDLOOP.
        IF sy-subrc = 0.
          IF i_txt-entry+8 = '.'.
            PERFORM get_descripcion_campo USING l_aux2
                                    CHANGING l_aux1.
            i_txt-entry+8 = l_aux1.
          ENDIF.
          v_entry = i_txt-entry+8.
          PERFORM anyade_comentario USING v_entry.
        ENDIF.
        CLEAR l_tabla.
      ENDIF.
    ENDIF.

    IF like = 'X'.
      CHECK NOT d_linea CA '"'.
      SEARCH d_linea FOR 'LIKE'.
      IF sy-subrc = 0.
        SPLIT d_linea AT 'LIKE'  INTO l_aux1 l_aux2.
        IF l_aux2 CA ','.
          SPLIT l_aux2 AT ','  INTO l_aux1 l_aux2.
        ELSE.
          SPLIT l_aux2 AT '.'  INTO l_aux1 l_aux2.
        ENDIF.
        CONDENSE l_aux1.
        SPLIT l_aux1 AT '-'  INTO l_aux1 l_aux2.
        IF l_aux2 IS INITIAL.
          SELECT SINGLE ddtext FROM dd02t
            INTO dd02t-ddtext
           WHERE tabname = l_aux1
             AND ddlanguage = sy-langu.
          IF sy-subrc = 0.
            PERFORM anyade_comentario USING dd02t-ddtext.
          ENDIF.
        ELSE.
          SELECT SINGLE rollname FROM dd03l
            INTO dd03l-rollname
           WHERE tabname = l_aux1
             AND fieldname = l_aux2.

          IF sy-subrc = 0.
            SELECT SINGLE ddtext FROM dd04t
              INTO dd04t-ddtext
             WHERE rollname = dd03l-rollname
               AND ddlanguage = sy-langu.
            IF sy-subrc = 0.
              PERFORM anyade_comentario USING dd04t-ddtext.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF type = 'X'.
      CHECK NOT d_linea CA '"'.
      SEARCH d_linea FOR 'TYPE'.
      IF sy-subrc = 0.
        IF d_linea CS 'TYPE TABLE OF'.
          SPLIT d_linea AT 'TYPE TABLE OF'  INTO l_aux1 l_aux2.
        ELSE.
          SPLIT d_linea AT 'TYPE'  INTO l_aux1 l_aux2.
        ENDIF.
        IF l_aux2 CA ','.
          SPLIT l_aux2 AT ','  INTO l_aux1 l_aux2.
        ELSE.
          SPLIT l_aux2 AT '.'  INTO l_aux1 l_aux2.
        ENDIF.
        CONDENSE l_aux1.
        SELECT SINGLE * FROM dd04t
         WHERE rollname = l_aux1
           AND ddlanguage = sy-langu.
        IF sy-subrc = 0.
          PERFORM anyade_comentario USING dd04t-ddtext.
        ELSE.
          SPLIT l_aux1 AT '-'  INTO l_aux1 l_aux2.
          IF l_aux2 IS INITIAL.
            SELECT SINGLE ddtext FROM dd02t
              INTO dd02t-ddtext
             WHERE tabname = l_aux1
             AND ddlanguage = sy-langu.
            IF sy-subrc = 0.
              PERFORM anyade_comentario USING dd02t-ddtext.
            ELSE.
              SELECT SINGLE * FROM dd40t
               WHERE typename = l_aux1
               AND ddlanguage = sy-langu.
              IF sy-subrc = 0.
                PERFORM anyade_comentario USING dd40t-ddtext.
              ENDIF.
            ENDIF.
          ELSE.
            SELECT SINGLE rollname FROM dd03l
              INTO dd03l-rollname
             WHERE tabname = l_aux1
               AND fieldname = l_aux2.

            IF sy-subrc = 0.
              SELECT SINGLE rollname FROM dd04t
                INTO dd04t-rollname
               WHERE rollname = dd03l-rollname
                 AND ddlanguage = sy-langu.
              IF sy-subrc = 0.
                PERFORM anyade_comentario USING dd04t-ddtext.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF include = 'X'.
      SEARCH d_linea FOR 'INCLUDE'.
      IF sy-subrc = 0.
        SPLIT d_linea AT 'INCLUDE'  INTO l_aux1 l_aux2.
        SEARCH l_aux2 FOR 'STRUCTURE'.
        IF sy-subrc NE 0.
          IF l_aux2 CA '.'.
            SPLIT l_aux2 AT '.'  INTO l_aux1 l_aux2.
          ELSE.
            SPLIT l_aux2 AT ','  INTO l_aux1 l_aux2.
          ENDIF.
          CONDENSE l_aux1.
          READ TEXTPOOL l_aux1 INTO tabtext LANGUAGE sy-langu.
          IF sy-subrc = 0.
            READ TABLE tabtext WITH KEY id = 'R'.
            IF sy-subrc = 0.
              PERFORM anyade_comentario USING tabtext-entry.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF l_linea_ant(1) NE '*'.
        SEARCH d_linea FOR 'WITH HEADER LINE'.
        IF sy-subrc = 0.
          IF NOT d_linea CA '"'.
            SPLIT d_linea AT 'OCCURS'  INTO l_aux1 l_aux2.
            IF sy-subrc = 0.
              SPLIT l_aux1 AT 'LIKE'  INTO l_aux1 l_aux2.
              IF sy-subrc = 0.
                CONDENSE l_aux2.
                IF NOT l_aux2 IS INITIAL.
                  SELECT SINGLE ddtext FROM dd02t
                    INTO dd02t-ddtext
                   WHERE tabname = l_aux2
                     AND ddlanguage = sy-langu.
                  IF sy-subrc = 0.
                    CONCATENATE '*' dd02t-ddtext INTO l_aux1
                         SEPARATED BY space.
                    INSERT l_aux1 INTO source.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF text = 'X'.
      SEARCH d_linea FOR ' TEXT-'.
      IF sy-subrc = 0.
        SPLIT d_linea AT ' TEXT-'  INTO l_aux1 l_aux2.
        l_aux1 = l_aux2(3).
        LOOP AT i_txt WHERE key(3) = l_aux1.
        ENDLOOP.
        IF sy-subrc = 0.
          PERFORM anyade_comentario USING i_txt-entry.
        ENDIF.
      ENDIF.
    ENDIF.

    IF message = 'X'.
      SEARCH d_linea FOR 'MESSAGE-ID '.
      IF sy-subrc = 0.
        SPLIT d_linea AT 'MESSAGE-ID '  INTO l_aux1 l_aux2.
        l_msgid = l_aux2(2).
      ENDIF.

      SEARCH d_linea FOR ' MESSAGE '.
      IF sy-subrc = 0.
        IF NOT d_linea CS '398'.
          SPLIT d_linea AT ' MESSAGE '  INTO l_aux1 l_aux2.
          l_aux2 = l_aux2+1.
          IF l_aux2 CA '('.
            SPLIT l_aux2 AT '('  INTO l_aux1 l_aux2.
            l_msgnr = l_aux1.
            SPLIT l_aux2 AT ')'  INTO l_aux1 l_aux2.
            l_arbgb = l_aux1.
          ELSE.
            l_arbgb = l_msgid.
            SEARCH l_aux2 FOR 'WITH'.
            IF sy-subrc = 0.
              SPLIT l_aux2 AT 'WITH'  INTO l_aux1 l_aux2.
            ELSE.
              SPLIT l_aux2 AT '.'  INTO l_aux1 l_aux2.
            ENDIF.
            l_msgnr = l_aux1.
          ENDIF.
          IF NOT l_msgnr IS INITIAL.
            SELECT SINGLE * FROM t100
             WHERE sprsl = sy-langu
               AND arbgb = l_arbgb
               AND msgnr = l_msgnr.
            IF sy-subrc = 0.
              d_linea = source.
              PERFORM anyade_comentario USING t100-text.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF f72 = 'X'.
      l_long = strlen( source ).
      IF l_long > 72.
        IF test IS INITIAL.
          i_cambios = source(72).
        ELSE.
          CONCATENATE source(72) '|' source+72 INTO i_cambios.
        ENDIF.
        APPEND i_cambios.
      ENDIF.
      d_linea = source.
      source = d_linea.
      MODIFY source.
    ENDIF.

    l_linea_ant     = source.
    l_num_linea_ant = sy-tabix.
  ENDLOOP.

  IF f72 IS INITIAL.
    PERFORM zadvanced_pretty_printer TABLES source.

    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo             = space
      TABLES
        ntext              = source
        otext              = source
      EXCEPTIONS
        enqueue_table_full = 1
        include_enqueued   = 2
        include_readerror  = 3
        include_writeerror = 4
        OTHERS             = 5.
  ENDIF.

  IF test = ''.
    INSERT REPORT program FROM source.                      "#EC *
  ENDIF.

  LOOP AT i_cambios.
    WRITE / i_cambios.
  ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  ANYADE_COMENTARIO
*&---------------------------------------------------------------------*
*       Añade comentario a la línea fuente
*&---------------------------------------------------------------------*
FORM anyade_comentario USING    pe_comen.

  CHECK pe_comen NE '' AND pe_comen NE '.'.

  CONCATENATE d_linea '"'  INTO d_linea SEPARATED BY space.
  CONCATENATE d_linea pe_comen INTO d_linea.
  source = d_linea.
  MODIFY source.

  i_cambios = d_linea.
  APPEND i_cambios.

ENDFORM.                                                   " ANYADE_COMENTARIO


TYPE-POOLS: swbse.

TYPES: gty_stokes_tab TYPE TABLE OF stokes.

TYPES: BEGIN OF gty_blocks,
         from      TYPE i, " stm
         to        TYPE i, " stm
         type      TYPE string,
         pos1      TYPE i,
         pos2      TYPE i,
         pos3      TYPE i,
         pos4      TYPE i,
         line_from TYPE i,
         line_to   TYPE i,
       END OF gty_blocks.

DATA: gt_blocks TYPE TABLE OF gty_blocks.

DATA: gv_user_setting_read TYPE flag,                      "Indicador general
*      gt_users type table of zadvpp_t_users,
*      gs_user type zadvpp_t_users,
      BEGIN OF gs_user,
        bname(12),
        active         VALUE 'X',
        align_comments VALUE 'X',
        min_pos	       TYPE i VALUE 30,
        move_to_pos	   TYPE i VALUE 60,
        align_decl     VALUE 'X',
      END OF gs_user,
      gv_inside_beginof TYPE flag,                         "Indicador general
      gv_last_line_from TYPE i,
      gv_beginof_col    TYPE i,
      gt_keywords       TYPE TABLE OF char128,             "txh01
      gt_tokens         TYPE stokesx_tab WITH HEADER LINE,
      gt_stm            TYPE sstmnt_tab WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Form  zadvanced_pretty_printer
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CT_SOURCE  text
*----------------------------------------------------------------------*
FORM zadvanced_pretty_printer TABLES ct_source.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      CT_SOURCE
*"----------------------------------------------------------------------

  DATA: lv_from_line           TYPE i,
        lv_to_line             TYPE i,
        lv_from_pos            TYPE i,
        lv_copy_len            TYPE i,
        lv_pos                 TYPE i,
        lv_pos2                TYPE i,
        lv_length              TYPE i,
        lv_to_pos              TYPE i,
        lv_line_part1          TYPE string,
        lv_line_part2          TYPE string,
        lv_line_part3          TYPE string,
        lv_line_part4          TYPE string,
        lv_spaces              TYPE string,
        lv_line_char           TYPE char256,               "Elemento de datos para 'ca
        lv_pos_diff            TYPE i,
        lv_pos_diff_prev       TYPE i,
        lv_last_row            TYPE i,
        lv_string              TYPE string,                "txh01
        lv_tabix               TYPE sytabix,               "Índice de tablas internas
        lv_chained_stm_counter TYPE i,
        lv_skip_first_token    TYPE flag,                  "Indicador general
        lv_new_line            TYPE string.

  FIELD-SYMBOLS: <ls_stm>        LIKE LINE OF gt_stm,
                 <lv_line>       TYPE any,                 "txh01
                 <ls_token>      LIKE LINE OF gt_tokens,
                 <ls_tokens_mod> LIKE LINE OF gt_tokens,
                 <ls_blocks>     LIKE LINE OF gt_blocks.

  IF gt_keywords IS INITIAL.
    PERFORM add_keywords.
  ENDIF.

  SCAN ABAP-SOURCE ct_source
    TOKENS      INTO gt_tokens
    STATEMENTS  INTO gt_stm
    KEYWORDS FROM gt_keywords
    WITH ANALYSIS.

  IF gs_user-align_decl EQ 'X'.

    PERFORM identify_blocks.

    LOOP AT gt_blocks ASSIGNING <ls_blocks>.

      CLEAR: lv_line_part1, lv_line_part2, lv_line_part3, lv_line_part4, lv_chained_stm_counter.

*     get first statement of block
      LOOP AT gt_stm ASSIGNING <ls_stm> FROM <ls_blocks>-from TO <ls_blocks>-to.

        ADD 1 TO lv_chained_stm_counter.

*       check if token is really there (chained statements)
        IF <ls_stm>-coloncol GT 0.
          lv_skip_first_token = 'X'.
        ELSE.
          CLEAR: lv_skip_first_token.
        ENDIF.

        CLEAR: lv_pos_diff_prev, lv_last_row.
        LOOP AT gt_tokens ASSIGNING <ls_token> FROM <ls_stm>-from TO <ls_stm>-to.

          lv_tabix = sy-tabix - <ls_stm>-from + 1.

          IF ( lv_skip_first_token EQ 'X' AND lv_tabix EQ 1 ) AND ( lv_chained_stm_counter NE 1 ).
            CONTINUE.
          ENDIF.
          CHECK lv_tabix LT 4.

          READ TABLE ct_source ASSIGNING <lv_line> INDEX <ls_token>-row.
          CHECK sy-subrc EQ 0.
          lv_string = <lv_line>.                                "txh01

          IF <ls_token>-row EQ lv_last_row.
            ADD lv_pos_diff_prev TO <ls_token>-col.
          ENDIF.

          CASE lv_tabix.
            WHEN 1.
              lv_pos_diff = <ls_blocks>-pos1 - <ls_token>-col.
            WHEN 2.
              lv_pos_diff = <ls_blocks>-pos2 - <ls_token>-col.
            WHEN 3.
              lv_pos_diff = <ls_blocks>-pos3 - <ls_token>-col.
            WHEN 4.
              CHECK <ls_blocks>-pos4 NE 0.
              lv_pos_diff = <ls_blocks>-pos4 - <ls_token>-col.
          ENDCASE.

          IF lv_pos_diff NE 0.

            lv_line_part1 = lv_string(<ls_token>-col).     "txh01
            lv_line_part2 = lv_string+<ls_token>-col.      "txh01
            IF lv_pos_diff GT 0.
              SHIFT lv_line_part2 BY lv_pos_diff PLACES RIGHT.
            ELSE.
              lv_copy_len   = <ls_token>-col + lv_pos_diff.
              lv_line_part1 = lv_line_part1(lv_copy_len).
            ENDIF.

            CONCATENATE lv_line_part1 lv_line_part2 INTO lv_string.     "txh01
            <lv_line> = lv_string.                              "txh01

            CASE lv_tabix.
              WHEN 1.
                <ls_token>-col = <ls_blocks>-pos1.
              WHEN 2.
                <ls_token>-col = <ls_blocks>-pos2.
              WHEN 3.
                <ls_token>-col = <ls_blocks>-pos3.
              WHEN 4.
                <ls_token>-col = <ls_blocks>-pos4.
            ENDCASE.

          ENDIF.

          lv_pos_diff_prev = lv_pos_diff_prev + lv_pos_diff.
          lv_last_row      = <ls_token>-row.
        ENDLOOP.

        IF strlen( lv_string ) GE <ls_blocks>-pos3.             "txh01

          lv_line_part1 = lv_string(<ls_blocks>-pos3).     "txh01
          lv_line_part2 = lv_string+<ls_blocks>-pos3.      "txh01
          CONDENSE lv_line_part2.

          CONCATENATE lv_line_part1 lv_line_part2 INTO lv_string.     "txh01
          <lv_line> = lv_string.                                "txh01

        ENDIF.

      ENDLOOP.

    ENDLOOP.

*   process begin of - end of
    CLEAR: gv_inside_beginof, gv_last_line_from.
    LOOP AT gt_stm ASSIGNING <ls_stm>.

      PERFORM update_beginof_status_stm USING <ls_stm>.

      LOOP AT gt_tokens ASSIGNING <ls_token> FROM <ls_stm>-from TO <ls_stm>-to.

        DATA lv_1st_token LIKE <ls_token>-str.

        lv_tabix = sy-tabix - <ls_stm>-from + 1.

        READ TABLE ct_source ASSIGNING <lv_line> INDEX <ls_token>-row.
        CHECK sy-subrc EQ 0.
        lv_string = <lv_line>.                                  "txh01

        IF lv_tabix EQ 1.
          lv_1st_token = <ls_token>-str.
        ELSEIF lv_tabix EQ 2 AND <ls_token>-str EQ 'END' AND lv_1st_token NE 'SELECTION-SCREEN'.

          lv_pos_diff = gv_beginof_col - <ls_token>-col.

          IF lv_pos_diff NE 0.

            lv_line_part1 = lv_string(<ls_token>-col).     "txh01
            lv_line_part2 = lv_string+<ls_token>-col.      "txh01
            IF lv_pos_diff GT 0.
              SHIFT lv_line_part2 BY lv_pos_diff PLACES RIGHT.
            ELSE.
              lv_copy_len   = <ls_token>-col + lv_pos_diff.
              lv_line_part1 = lv_line_part1(lv_copy_len).
            ENDIF.

            CONCATENATE lv_line_part1 lv_line_part2 INTO lv_string.     "txh01
            <lv_line> = lv_string.                              "txh01

            ADD lv_pos_diff TO <ls_token>-col.

            LOOP AT gt_tokens ASSIGNING <ls_tokens_mod> WHERE row EQ <ls_token>-row AND col GT <ls_token>-col.
              ADD lv_pos_diff TO <ls_tokens_mod>-col.
            ENDLOOP.

          ENDIF.

          EXIT.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDIF.

* adjust " comments

  IF gs_user-align_comments EQ 'X'.

    LOOP AT ct_source ASSIGNING <lv_line>.                 "txh01 WHERE table_line CS '"'.

      CHECK <lv_line> CS '"'.
      CHECK <lv_line>(1) NE '*'.

      lv_string = <lv_line>.                                    "txh01
      PERFORM get_comment_pos USING lv_string CHANGING lv_pos.     "txh01

      IF lv_pos GT gs_user-min_pos OR gs_user-min_pos IS INITIAL.

        IF lv_pos GT gs_user-move_to_pos.
          lv_to_pos = lv_pos + 1.
        ELSE.
          lv_to_pos = gs_user-move_to_pos.
        ENDIF.

        lv_pos_diff = lv_to_pos - lv_pos.

        SUBTRACT 1 FROM lv_pos.

        lv_line_part1 = lv_string(lv_pos).                 "txh01
        lv_line_part2 = lv_string+lv_pos.                  "txh01
        IF lv_pos_diff GT 0.
          SHIFT lv_line_part2 BY lv_pos_diff PLACES RIGHT.
        ELSE.
          lv_copy_len   = lv_pos + lv_pos_diff.
          lv_line_part1 = lv_line_part1(lv_copy_len).
        ENDIF.

        CONCATENATE lv_line_part1 lv_line_part2 INTO lv_string.     "txh01
        <lv_line> = lv_string.                                  "txh01

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    "zadvanced_pretty_printer

*&---------------------------------------------------------------------*
*&  Include           LZADVANCED_PRETTY_PRINTERF01
*&---------------------------------------------------------------------*

FORM identify_blocks.

  DATA: ls_blocks            LIKE LINE OF gt_blocks.
  DATA: lv_last_stm_type     TYPE string.
  DATA: lv_curr_stm_type     TYPE string,
        lv_tabix             TYPE i,
        lv_tabix_stm         TYPE i,
        lv_last_stm_last_row TYPE i,
        lv_prev_stm_row      TYPE i,
        lv_skip              TYPE flag,                    "Indicador general
        lv_chained_stm       TYPE flag,                    "Indicador general
        lv_diff              TYPE i,
        lv_pos1              TYPE i,
        lv_pos2              TYPE i,
        lv_pos3              TYPE i,
        lv_pos4              TYPE i,
        lv_row               TYPE i,
        lv_max_2nd_token_len TYPE i,
        lv_max_1st_token_len TYPE i,
        lv_rowid             TYPE i.

  FIELD-SYMBOLS: <ls_tokens> LIKE LINE OF gt_tokens,
                 <ls_stm>    LIKE LINE OF gt_stm.


  CLEAR gt_blocks.

  DELETE ADJACENT DUPLICATES FROM gt_stm COMPARING colonrow trow.     "txh01 remove multiple commands on the same line
  LOOP AT gt_stm ASSIGNING <ls_stm>.

    lv_tabix_stm = sy-tabix.

    PERFORM determine_stm_type USING <ls_stm>-from <ls_stm>-to CHANGING lv_curr_stm_type lv_row.

    lv_rowid = lv_row - 1. "<ls_stm>-trow - 1.

*   blokk kezdodhet/vegzodhet
    IF lv_curr_stm_type NE lv_last_stm_type OR ( lv_prev_stm_row NE lv_rowid ).

*     ha tobb mint egysoros a blokk
      IF ( ls_blocks-from NE ls_blocks-to ).
        ls_blocks-type = lv_last_stm_type.
        APPEND ls_blocks TO gt_blocks.
      ENDIF.

      CLEAR ls_blocks.
      ls_blocks-from = ls_blocks-to = lv_tabix_stm.

    ELSEIF lv_curr_stm_type EQ lv_last_stm_type.
      ADD 1 TO ls_blocks-to.
    ENDIF.

    lv_last_stm_type = lv_curr_stm_type.
    lv_prev_stm_row  = lv_row.                             "<ls_stm>-trow.
  ENDLOOP.

* utolso rekord
  IF ( ls_blocks-from NE ls_blocks-to ).
    ls_blocks-type = lv_last_stm_type.
    APPEND ls_blocks TO gt_blocks.
  ENDIF.

  DELETE gt_blocks WHERE type IS INITIAL.

* calculate positions
  DATA: lv_tabix_block TYPE i,
        lv_last_stm    TYPE flag,                          "Indicador general
        lv_lines_stm   TYPE i,
        lv_lines_tk    TYPE i,
        lv_len         TYPE i.

  LOOP AT gt_blocks INTO ls_blocks.

    lv_tabix_block = sy-tabix.

    CLEAR: lv_max_2nd_token_len, lv_max_1st_token_len, lv_last_stm.

*   get longest token
    LOOP AT gt_stm ASSIGNING <ls_stm> FROM ls_blocks-from TO ls_blocks-to.

      lv_tabix_stm = sy-tabix - ls_blocks-from + 1.
      lv_lines_stm = ls_blocks-to - ls_blocks-from.

      IF lv_tabix_stm EQ lv_lines_stm.
        lv_last_stm = 'X'.
      ENDIF.

      LOOP AT gt_tokens ASSIGNING <ls_tokens> FROM <ls_stm>-from TO <ls_stm>-to.

        lv_tabix    = sy-tabix - <ls_stm>-from + 1.
        lv_lines_tk = <ls_stm>-to - <ls_stm>-from.

        IF lv_tabix_stm EQ 1 AND lv_tabix EQ 1.
          ls_blocks-line_from = <ls_tokens>-row.
        ENDIF.

        IF lv_tabix EQ lv_lines_tk.
          IF lv_last_stm EQ 'X'.
            ls_blocks-line_to = <ls_tokens>-row.
          ENDIF.
        ENDIF.

        lv_len = <ls_tokens>-len1.
        IF <ls_tokens>-len2 GT 0.
          ADD <ls_tokens>-len2 TO lv_len.
          ADD 1 TO lv_len.
        ENDIF.
        IF <ls_tokens>-len3 GT 0.
          ADD <ls_tokens>-len3 TO lv_len.
          ADD 2 TO lv_len.
        ENDIF.

        IF ls_blocks-type EQ '='.
          IF lv_tabix EQ 1 AND lv_len GT lv_max_1st_token_len.
            lv_max_1st_token_len = lv_len.
            EXIT.
          ENDIF.
        ELSE.
          IF lv_tabix EQ 2 AND lv_len GT lv_max_2nd_token_len.
            lv_max_2nd_token_len = lv_len.
            EXIT.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    CLEAR: gv_inside_beginof, gv_last_line_from.
    LOOP AT gt_stm ASSIGNING <ls_stm> FROM ls_blocks-from TO ls_blocks-to.
      IF <ls_stm>-coloncol GT 0.
        lv_chained_stm = 'X'.
      ELSE.
        CLEAR lv_chained_stm.
      ENDIF.
      PERFORM update_beginof_status_stm USING <ls_stm>.

      PERFORM adjust_positions USING <ls_stm>-from
                                     <ls_stm>-to
                                     lv_chained_stm
                                     lv_max_1st_token_len
                                     lv_max_2nd_token_len
                               CHANGING ls_blocks.
    ENDLOOP.

    MODIFY gt_blocks FROM ls_blocks INDEX lv_tabix_block.

  ENDLOOP.

*  LOOP AT gt_blocks INTO ls_blocks.
*    IF ( ls_blocks-from = ls_blocks-to ) and ls_blocks-type NE 'DOBJ_BO'..
*      DELETE gt_blocks INDEX sy-tabix.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    "identify_blocks

*&---------------------------------------------------------------------*
*&      Form  adjust_positions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_FROM               text
*      -->IV_TO                 text
*      -->IV_CHAINED            text
*      -->IV_MAX_1ST_TOKEN_LEN  text
*      -->IV_MAX_2ND_TOKEN_LEN  text
*      -->CS_BLOCKS             text
*----------------------------------------------------------------------*
FORM adjust_positions USING iv_from TYPE i
                            iv_to   TYPE i
                            iv_chained TYPE flag           "Indicador general
                            iv_max_1st_token_len TYPE i
                            iv_max_2nd_token_len TYPE i
                      CHANGING cs_blocks TYPE gty_blocks.

  DATA: lv_tabix               TYPE sytabix,               "Índice de tablas internas
        lv_pos_diff_prev       TYPE i,
        lv_pos_diff            TYPE i,
        lv_stm_type            TYPE string,
        lv_col                 TYPE i,
        lv_tabix_correction    TYPE i,
        lv_tabix_orig          TYPE i,
        lv_newpos              TYPE i,
        lv_first_token_len     TYPE i,
        lv_second_token_len    TYPE i,
        lv_first_token_chained TYPE flag,                  "Indicador general
        lv_last_row            TYPE i.

  FIELD-SYMBOLS: <ls_tokens> LIKE LINE OF gt_tokens.

  LOOP AT gt_tokens ASSIGNING <ls_tokens> FROM iv_from TO iv_to.

    lv_tabix = sy-tabix - iv_from + 1.                         " - lv_tabix_correction.

    lv_col = <ls_tokens>-col.

    IF lv_tabix EQ 1.

      IF cs_blocks-pos1 LT lv_col.
        cs_blocks-pos1 = lv_col.
      ENDIF.

      lv_first_token_len = strlen( <ls_tokens>-str ).

    ELSEIF lv_tabix EQ 2.

      IF cs_blocks-type EQ '='.
        lv_newpos = cs_blocks-pos1 + iv_max_1st_token_len + 1.
      ELSE.

        lv_newpos = cs_blocks-pos1 + lv_first_token_len + 1.
        IF iv_chained EQ 'X'.
          ADD 1 TO lv_newpos.
        ENDIF.
        IF gv_inside_beginof EQ 'X'.
          ADD 2 TO lv_newpos.
        ENDIF.

      ENDIF.

      IF cs_blocks-pos2 LT lv_newpos.
        cs_blocks-pos2 = lv_newpos.
      ENDIF.

*      lv_pos_diff = cs_blocks-pos2 - lv_col.

*      lv_second_token_len = strlen( <ls_tokens>-str ).

    ELSEIF lv_tabix EQ 3.

      IF cs_blocks-type EQ '='.
        lv_newpos = cs_blocks-pos2 + 2.
      ELSE.
        lv_newpos = cs_blocks-pos2 + iv_max_2nd_token_len + 1.
      ENDIF.

      IF cs_blocks-pos3 LT lv_newpos.
        cs_blocks-pos3 = lv_newpos.
      ENDIF.

*      lv_pos_diff = cs_blocks-pos3 - lv_col.

    ELSEIF lv_tabix EQ 4.

**      IF cs_blocks-pos4 LT lv_col.
**        cs_blocks-pos4 = lv_col.
**      ENDIF.
**      lv_pos_diff = cs_blocks-pos4 - lv_col.

    ENDIF.

*    lv_pos_diff_prev = lv_pos_diff_prev + lv_pos_diff.
*    lv_last_row = <ls_tokens>-row.
  ENDLOOP.

ENDFORM.                    "adjust_positions
*&---------------------------------------------------------------------*
*&      Form  determine_stm_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_FROM      text
*      -->IV_TO        text
*      -->CV_STM_TYPE  text
*      -->CV_ROW       text
*----------------------------------------------------------------------*
FORM determine_stm_type USING iv_from TYPE i
                              iv_to   TYPE i
                        CHANGING cv_stm_type TYPE string
                                 cv_row      TYPE i.

  DATA: lv_beginof   TYPE flag,                            "Indicador general
        lv_endof_pos TYPE i.

  FIELD-SYMBOLS: <ls_tokens> LIKE LINE OF gt_tokens.

  CLEAR: cv_row, cv_stm_type.

  LOOP AT gt_tokens ASSIGNING <ls_tokens> FROM iv_from TO iv_to.

    cv_row = <ls_tokens>-row.

    IF sy-tabix EQ iv_from.

      CASE <ls_tokens>-str.
        WHEN 'DATA' OR 'TYPES' OR 'STATICS' OR 'CLASS-DATA' OR 'CONSTANTS' OR 'PARAMETERS' OR 'SELECT-OPTIONS'.     " or 'FIELD-SYMBOLS'.

          PERFORM check_beginof USING sy-tabix CHANGING lv_beginof.      " lv_endof_pos.

          IF lv_beginof EQ 'X'.
            cv_stm_type = 'DOBJ_BO'.
          ELSE.
            cv_stm_type = 'DOBJ'.
          ENDIF.

        WHEN 'MOVE'.
          cv_stm_type = 'MOVE'.
          RETURN.
        WHEN 'FIELD-SYMBOLS'.
          cv_stm_type = 'FS'.
        WHEN OTHERS.
          CONTINUE.

      ENDCASE.

    ELSE.

      CASE <ls_tokens>-str.
        WHEN '='.
          cv_stm_type = '='.
          RETURN.
        WHEN OTHERS.
          RETURN.
      ENDCASE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "determine_stm_type

*&---------------------------------------------------------------------*
*&      Form  check_beginof
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_TABIX   text
*      -->CV_BEGINOF text
*----------------------------------------------------------------------*
FORM check_beginof USING iv_tabix TYPE i
                   CHANGING cv_beginof TYPE flag.          "Indicador general
*                                                  cv_endof_pos TYPE i.

  DATA lv_tabix TYPE i.
  FIELD-SYMBOLS: <ls_token>  LIKE LINE OF gt_tokens.
  FIELD-SYMBOLS: <ls_token2> LIKE LINE OF gt_tokens.

  CLEAR cv_beginof.

  lv_tabix = iv_tabix + 1.

  READ TABLE gt_tokens INDEX lv_tabix ASSIGNING <ls_token>.
  CHECK sy-subrc EQ 0.

  IF <ls_token>-str EQ 'BEGIN' OR <ls_token>-str EQ 'END'.

*    lv_tabix = iv_tabix - 1.                                  "txh01
*
*    READ TABLE gt_tokens INDEX lv_tabix ASSIGNING <ls_token>. "txh01
*    CHECK sy-subrc EQ 0.                                      "txh01
*
*    IF <ls_token>-str EQ 'AT'.  "txh01
*      RETURN.                   "txh01
*    ENDIF.                      "txh01
*
*    lv_tabix = iv_tabix + 2.    "txh01
    lv_tabix = iv_tabix + 1.

    READ TABLE gt_tokens INDEX lv_tabix ASSIGNING <ls_token>.
    CHECK sy-subrc EQ 0.

    IF <ls_token>-str EQ 'OF'.
      cv_beginof = 'X'.

**      lv_tabix = iv_tabix + 2.
**
**      LOOP AT gt_tokens FROM lv_tabix ASSIGNING <ls_token>.
**
**        IF <ls_token>-str EQ 'END'.
**
**          lv_tabix = iv_tabix + 1.
**
**          READ TABLE gt_tokens INDEX lv_tabix ASSIGNING <ls_token2>.
**          CHECK sy-subrc EQ 0.
**
**          IF <ls_token2>-str EQ 'OF'.
**            cv_endof_pos = lv_tabix - 1.
**            EXIT.
**          ENDIF.
**
**        ENDIF.
**
**      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.                    "check_beginof

*&---------------------------------------------------------------------*
*&      Form  get_comment_pos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_LINE    text
*      -->CV_POS     text
*----------------------------------------------------------------------*
FORM get_comment_pos USING iv_line TYPE string                 "SWBSE_MAX_LINE
                     CHANGING cv_pos TYPE i.

  DATA: lv_length    TYPE i,
        lv_offset    TYPE i,
        lv_apth_open TYPE flag.                            "Indicador general

  CLEAR cv_pos.

  lv_length = strlen( iv_line ).

  DO lv_length TIMES.

    lv_offset = sy-index - 1.

    IF iv_line+lv_offset(1) EQ ''''.
      IF lv_apth_open IS INITIAL.
        lv_apth_open = 'X'.
      ELSE.
        CLEAR lv_apth_open.
      ENDIF.
    ELSEIF iv_line+lv_offset(1) EQ '"' AND lv_apth_open IS INITIAL.
      cv_pos = sy-index.
      RETURN.
    ENDIF.

  ENDDO.

ENDFORM.                    "get_comment_pos

*&---------------------------------------------------------------------*
*&      Form  update_beginof_status_stm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_STM     text
*----------------------------------------------------------------------*
FORM update_beginof_status_stm USING is_stm TYPE sstmnt.   "Descripción de

  DATA: lv_tabix   TYPE i,
        lv_keyword TYPE string.

  FIELD-SYMBOLS: <ls_token>      LIKE LINE OF gt_tokens,
                 <ls_token_next> LIKE LINE OF gt_tokens.

  LOOP AT gt_tokens ASSIGNING <ls_token>
    WHERE row BETWEEN gv_last_line_from AND is_stm-trow
          AND ( str EQ 'BEGIN' OR str EQ 'END' ).

    lv_tabix = sy-tabix.

    SUBTRACT 1 FROM lv_tabix.
    READ TABLE gt_tokens ASSIGNING <ls_token_next> INDEX lv_tabix.
    CHECK sy-subrc EQ 0.
    CHECK <ls_token_next>-str NE 'SELECTION-SCREEN' AND <ls_token_next>-str NE 'AT'.     "txh01
    ADD 1 TO lv_tabix.

    IF <ls_token>-row EQ is_stm-trow.
      CHECK <ls_token>-col LT is_stm-tcol.
    ENDIF.

    lv_keyword = <ls_token>-str.
    ADD 1 TO lv_tabix.

    IF <ls_token>-str EQ 'BEGIN'.
      gv_beginof_col = <ls_token>-col.
    ENDIF.

    READ TABLE gt_tokens ASSIGNING <ls_token_next> INDEX lv_tabix.

    IF sy-subrc EQ 0.

      IF <ls_token_next>-row EQ is_stm-trow.
        CHECK <ls_token_next>-col LT is_stm-tcol.
      ENDIF.

      IF <ls_token_next>-str EQ 'OF'.

        IF lv_keyword EQ 'BEGIN'.
          gv_inside_beginof = 'X'.
        ELSEIF lv_keyword EQ 'END'.
          CLEAR: gv_inside_beginof.
        ENDIF.

      ENDIF.

    ENDIF.

*    gv_last_line_from = is_stm-trow.

  ENDLOOP.

  gv_last_line_from = is_stm-trow.

ENDFORM.                    " UPDATE_BEGINOF_STATUS
*&---------------------------------------------------------------------*
*&      Form  add_keywords
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_keywords.

  APPEND 'DATA' TO gt_keywords.
  APPEND 'TYPES' TO gt_keywords.
  APPEND 'SELECTION-SCREEN' TO gt_keywords.
  APPEND 'FIELD-SYMBOLS' TO gt_keywords.
  APPEND 'STATICS' TO gt_keywords.
  APPEND 'CONSTANTS' TO gt_keywords.
  APPEND 'CLASS-DATA' TO gt_keywords.
  APPEND 'SELECT-OPTIONS' TO gt_keywords.
  APPEND 'PARAETERS' TO gt_keywords.
  APPEND 'MOVE' TO gt_keywords.
  APPEND 'COMPUTE' TO gt_keywords.

ENDFORM.                    "add_keywords


*&---------------------------------------------------------------------*
*&      Form  get_descripcion_campo
*&---------------------------------------------------------------------*
FORM get_descripcion_campo USING pe_campo
                        CHANGING ps_descripcion.

  CLEAR ps_descripcion.
  IF pe_campo CA ','.
    SPLIT pe_campo AT ','  INTO l_aux1 l_aux2.
  ELSE.
    SPLIT pe_campo AT '.'  INTO l_aux1 l_aux2.
  ENDIF.
  CONDENSE l_aux1.
  SPLIT l_aux1 AT '-'  INTO l_aux1 l_aux2.
  SPLIT l_aux2 AT ' ' INTO l_aux2 l_aux3.
  SELECT SINGLE rollname FROM dd03l
    INTO dd03l-rollname
   WHERE tabname = l_aux1
     AND fieldname = l_aux2.
  IF sy-subrc = 0.
    SELECT SINGLE ddtext FROM dd04t
      INTO dd04t-ddtext
     WHERE rollname = dd03l-rollname
       AND ddlanguage = sy-langu.
    IF sy-subrc = 0.
      ps_descripcion = dd04t-ddtext.
    ENDIF.
  ENDIF.

ENDFORM.                    "get_descripcion_campo
