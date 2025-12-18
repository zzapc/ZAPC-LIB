************************************************************************
* MÓDULO      : FI                                                     *
* TIPO        : Include                                                *
* TITULO      : Rutinas utilización función estandar contabilización
* DESCRIPCION : SAP dispone de unas funciones para poder contabilizar
*               documentos contables mediante las siguientes
*               transacciones:
*                  FB01 - Contabilizar documento
*                  FBS1 - Crear documento de periodificación
*                  FB41 - Contabilizar cargo IVA
*                  ABF1 - Contabilidad activos fijos
*                  FBB1 - Contab. valoración de ME
*                  FBVB - Contab. documento preliminar
*                  FBV1 - Crear documento preliminar
*                  FBD5 - Realizar contab. periódica
*               En este include se declaran las variables necesarias,
*               así como se encapsulan las funciones estándar para
*               poder contabilizar documentos de forma fácil.
* Ejemplo:
*    ct_posting_interface_start 'B' 'N' 'PRUEBA' sy-uname ' '.
*    ct_nuevo_documento.
*    ct_add_campo: 'BKPF-BLDAT' '01.12.2002',
*                  'BKPF-BUDAT' '01.12.2002',
*                  'BKPF-BLART' 'RE',
*                  'BKPF-BUKRS' '0012',
*                  'BKPF-WAERS' 'EUR',
*                  'BKPF-BKTXT' 'Prueba interface'.
*    ct_nueva_posicion.
*    ct_add_campo: 'RF05A-NEWKO' '400000',
*                  'RF05A-NEWBS' '40',
*                  'BSEG-WRBTR'  '10'.
*    ct_nueva_posicion.
*    ct_add_campo: 'RF05A-NEWKO' '500000',
*                  'RF05A-NEWBS' '50',
*                  'BSEG-WRBTR'  '10'.
*    ct_posting_interface_document 'FB01'.
*    ct_posting_interface_end.
*
* AUTOR: Andrés Picazo (Altimia)                FECHA: 09/01/2003      *
*                                                                      *
* MODIFICACIONES                                                       *
* -------------                                                        *
* FECHA        NOMBRE            DESCRIPCION                           *
* -------------------------------------------------------------------- *
* dd.mm.yyyy   username                                                *
************************************************************************

*------TABLAS INTERNAS-------------------------------------------------*
* Cabecera y posiciones de doc. para interfase interna contab.
  DATA: i_ftpost LIKE ftpost OCCURS 100 WITH HEADER LINE,
* Tabla de números de documento para Contabilidad
        i_blntab LIKE blntab OCCURS 2   WITH HEADER LINE,
* Impuestos para interfase de contab. interna
        i_fttax  LIKE fttax  OCCURS 1   WITH HEADER LINE,
        l_ftpost TYPE ftpost.

* Variables de contadores de documentos y posición
  DATA: v_ct_num_doc        TYPE i,
        v_ct_num_pos        TYPE i,
        v_belnr_new         TYPE bkpf-belnr,
        v_message_error_doc TYPE bapi_msg,
        v_imp_pos_n         TYPE bseg-dmbtr.

* Variables para almacener los posibles errores de contabilización
  DATA: v_ct_msgid LIKE sy-msgid,
        v_ct_msgty LIKE sy-msgty,
        v_ct_msgno LIKE sy-msgno,
        v_ct_msgv1 LIKE sy-msgv1,
        v_ct_msgv2 LIKE sy-msgv2,
        v_ct_msgv3 LIKE sy-msgv3,
        v_ct_msgv4 LIKE sy-msgv4,
        v_ct_subrc LIKE sy-subrc.

*----------------------------------------------------------------------
* DEFINES
*----------------------------------------------------------------------*
* Informa el valor de un nuevo campo
  DEFINE ct_add_campo.
    i_ftpost-fnam = &1.                "Nombre de campo
    i_ftpost-fval = &2.                "Valor
    APPEND i_ftpost.
  END-OF-DEFINITION.

  DEFINE ct_add_campof.
    i_ftpost-fnam = &1.                "Nombre de campo
    WRITE &2 TO i_ftpost-fval.         "Valor
    CONDENSE i_ftpost-fval NO-GAPS.
    APPEND i_ftpost.
  END-OF-DEFINITION.

  DEFINE ct_add_campo_cl.
    l_ftpost-fnam = &1.                "Nombre de campo
    l_ftpost-fval = &2.                "Valor
    APPEND l_ftpost TO i_ftpost.
  END-OF-DEFINITION.

  DEFINE ct_add_campo_cl_siv.
    IF NOT &2 IS INITIAL.
      l_ftpost-fnam = &1.                "Nombre de campo
      l_ftpost-fval = &2.                "Valor
      APPEND l_ftpost TO i_ftpost.
      ENDIF.
  END-OF-DEFINITION.

  DEFINE ct_add_campof_cl.
    l_ftpost-fnam = &1.                "Nombre de campo
    WRITE &2 TO l_ftpost-fval.         "Valor
    CONDENSE l_ftpost-fval NO-GAPS.
    APPEND l_ftpost TO i_ftpost.
  END-OF-DEFINITION.

* Informa el valor de un nuevo campo (sólo si está informado)
  DEFINE ct_add_campo_ni.
    IF NOT &2 IS INITIAL.
      i_ftpost-fnam = &1.              "Nombre de campo
      i_ftpost-fval = &2.              "Valor
      APPEND i_ftpost.
    ENDIF.
  END-OF-DEFINITION.

* Informe el valor de un campo fecha, formateando este valor
  DEFINE ct_add_campo_fecha.
    i_ftpost-fnam = &1.                "Nombre de campo
    WRITE  &2 TO i_ftpost-fval.        "Variable tipo fecha
    APPEND i_ftpost.
  END-OF-DEFINITION.

* Informe el valor de un campo importe, formateando este valor en
* función de la moneda
  DEFINE ct_add_campo_importe.
    i_ftpost-fnam = &1.                "Nombre de campo
    CLEAR i_ftpost-fval.
    v_imp_pos_n = abs( &2 ).
  WRITE v_imp_pos_n TO l_ftpost-fval(13) CURRENCY &3. "Variable importe y moneda
    APPEND i_ftpost.
  END-OF-DEFINITION.

  DEFINE ct_add_campo_importe_cl.
    l_ftpost-fnam = &1.                "Nombre de campo
    CLEAR l_ftpost-fval.
    v_imp_pos_n = abs( &2 ).
  WRITE v_imp_pos_n TO l_ftpost-fval(13) CURRENCY &3. "Variable importe y moneda
    APPEND l_ftpost TO i_ftpost.
  END-OF-DEFINITION.

* Inicia la creación del interface, indicando si es batch input o
* call transaction, y los parámetros de los mismos
  DEFINE ct_posting_interface_start.
    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_function = &1       "Modo ejecución   ( B - Batch Input
                              "                   C - Call Transaction
        i_mode     = &2                "Modo Call Trans. ( A - Visible
                                       "                   N - INVISIBLE
                              "                   E - Parar en errores
        i_group    = &3                "Nombre juego de datos
        i_user     = &4                "Usuario
        i_keep     = &5.               "Retener juego de datos

    CLEAR v_ct_num_doc.
  END-OF-DEFINITION.

* Graba el documento en el juego de datos batch input, o si se está
* contabilizando mediante Call Transaction, se intenta contabilizar,
* devolviendo el resultado en las variables de error.
  DEFINE ct_posting_interface_document.
    CLEAR v_belnr_new.
      CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT'
        EXPORTING
          i_tcode  = &1
        IMPORTING
          e_msgid  = v_ct_msgid
          e_msgno  = v_ct_msgno
          e_msgty  = v_ct_msgty
          e_msgv1  = v_ct_msgv1
          e_msgv2  = v_ct_msgv2
          e_msgv3  = v_ct_msgv3
          e_msgv4  = v_ct_msgv4
          e_subrc  = v_ct_subrc
        TABLES
          t_blntab = i_blntab
          t_ftpost = i_ftpost
          t_fttax  = i_fttax.
  END-OF-DEFINITION.

* Cierra el batch input
  DEFINE ct_posting_interface_end.
    CLEAR v_belnr_new.
    CALL FUNCTION 'POSTING_INTERFACE_END'.

   IF sy-msgid = 'F5' AND sy-msgno = '312'.
      zcl_ap_string=>poner_ceros( EXPORTING cadena = sy-msgv1
                                  CHANGING  salida = v_belnr_new ).
   ELSE.
       MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
       INTO v_message_error_doc.
    ENDIF.

  END-OF-DEFINITION.

* Inicia un nuevo documento
  DEFINE ct_nuevo_documento.
    REFRESH: i_ftpost, i_blntab, i_fttax.
    CLEAR: i_ftpost, l_ftpost.
    i_ftpost-stype = l_ftpost-stype = 'K'.
    i_ftpost-count = l_ftpost-count = v_ct_num_doc.
    CLEAR v_ct_num_pos.
  END-OF-DEFINITION.

* Inicia un nueva posición
  DEFINE ct_nueva_posicion.
    ADD 1 TO v_ct_num_pos.
    CLEAR: i_ftpost, l_ftpost.
    i_ftpost-stype = l_ftpost-stype = 'P'.
    i_ftpost-count = l_ftpost-count = v_ct_num_pos.
  END-OF-DEFINITION.
