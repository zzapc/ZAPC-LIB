CLASS zcl_ap_sgpi DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA total TYPE i.

    METHODS constructor
      IMPORTING commit_work       TYPE abap_bool DEFAULT ''
                break_condicional TYPE abap_bool DEFAULT ''.

    CLASS-METHODS text
      IMPORTING texto                TYPE any
                texto2               TYPE any       OPTIONAL
                texto3               TYPE any       OPTIONAL
                i_processed          TYPE sy-tabix  OPTIONAL
                i_total              TYPE sy-tabix  OPTIONAL
                i_output_immediately TYPE boole_d   DEFAULT 'X'
                !commit              TYPE any       DEFAULT ''
                break_condicional    TYPE abap_bool DEFAULT ''.

    METHODS texto
      IMPORTING texto1       TYPE any
                texto2       TYPE any       OPTIONAL
                texto3       TYPE any       OPTIONAL
                quitar_ceros TYPE abap_bool DEFAULT ''.

    METHODS get_filas_tabla
      IMPORTING tabla TYPE table.

    METHODS porcentaje
      IMPORTING texto                TYPE any
                cantidad             TYPE i
                i_output_immediately TYPE boole_d DEFAULT ''.


  PRIVATE SECTION.
    DATA contador          TYPE i.
    DATA commit_work       TYPE c LENGTH 1.
    DATA aux               TYPE i.
    DATA break_condicional TYPE c LENGTH 1 VALUE '' ##NO_TEXT.

    METHODS refresh.
endclass. "ZCL_AP_SGPI definition
class ZCL_AP_SGPI implementation.
  METHOD constructor.
    me->commit_work       = commit_work.

    me->break_condicional = break_condicional.
  ENDMETHOD.
  METHOD get_filas_tabla.
    CLEAR: contador, aux.

    total = lines( tabla ).
  ENDMETHOD.
  METHOD porcentaje.
*  DATA l_porc TYPE i.

    IF    sy-uname = 'WF-BATCH'
       OR sy-uname = 'SAP_WFRT'.
      RETURN.
    ENDIF.

    IF total <= 0.
      RETURN.
    ENDIF.
    contador = contador + 1.
*  l_porc = ( contador * 100 ) / total.
    IF contador <= aux.
      RETURN.
    ENDIF.

*    IF sy-batch IS INITIAL.
*      CALL METHOD refresh.
    text( texto = texto
          i_total = total
          i_processed = contador
          i_output_immediately = i_output_immediately
          commit = commit_work
          break_condicional = break_condicional ).
*      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*        EXPORTING
*          percentage = l_porc
*          text       = texto
*        EXCEPTIONS
*          OTHERS     = 1.
*    ELSE.
*      MESSAGE s004(ken_01) WITH texto l_porc '%'.
    IF sy-batch = 'X'.
      IF commit_work = 'X'.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
    aux = aux + cantidad.
  ENDMETHOD.
  METHOD refresh.
*    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'. " EC *

    cl_progress_indicator=>progress_indicate(
         i_text               = ''
        i_output_immediately = '' ).

  ENDMETHOD.
  METHOD text.
    DATA l_texto TYPE string.

    IF    sy-uname = 'WF-BATCH'
       OR sy-uname = 'SAP_WFRT'.
      RETURN.
    ENDIF.

    CONCATENATE texto texto2 texto3 INTO l_texto SEPARATED BY space.

    cl_progress_indicator=>progress_indicate(
         i_text               = l_texto
        i_processed          = i_processed    " Number of Objects Already Processed
        i_total              = i_total    " Total Number of Objects to Be Processed
        i_output_immediately = i_output_immediately ). " X = Display Progress Immediately; EC *

    IF i_output_immediately = 'X' AND commit = 'X'.
      IF commit = 'X'.
        COMMIT WORK AND WAIT.
      ENDIF.
      IF sy-batch IS INITIAL.
        cl_gui_cfw=>flush( ).
      ENDIF.
    ENDIF.

    IF break_condicional = 'X'.
      zcl_ap_dev=>break_condicional( texto = l_texto ).
    ENDIF.
  ENDMETHOD.
  METHOD texto.
    DATA: l_texto1 TYPE string,
          l_texto2 TYPE string,
          l_texto3 TYPE string,
          l_linea  TYPE c LENGTH 100.

    IF    sy-uname = 'WF-BATCH'
       OR sy-uname = 'SAP_WFRT'.
      RETURN.
    ENDIF.

    l_texto1 = texto1.
    l_texto2 = texto2.
    l_texto3 = texto3.

    IF quitar_ceros = 'X'.
      zcl_ap_string=>quitar_ceros_c( CHANGING cadena = l_texto1 ).
      zcl_ap_string=>quitar_ceros_c( CHANGING cadena = l_texto2 ).
      zcl_ap_string=>quitar_ceros_c( CHANGING cadena = l_texto3 ).
    ENDIF.
    CONCATENATE l_texto1 l_texto2 l_texto3 INTO l_linea SEPARATED BY space.

    IF sy-batch IS INITIAL.
      IF commit_work = 'X' AND sy-batch IS INITIAL.
        text( texto = l_linea i_output_immediately = 'X' break_condicional = break_condicional ).
      ELSE.
        text( texto = l_linea i_output_immediately = '' break_condicional = break_condicional ).
      ENDIF.
    ELSE.
      MESSAGE i003(aq) WITH l_linea.
    ENDIF.

    IF commit_work = 'X'.
      COMMIT WORK AND WAIT.
      IF sy-batch IS INITIAL.
        cl_gui_cfw=>flush( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
