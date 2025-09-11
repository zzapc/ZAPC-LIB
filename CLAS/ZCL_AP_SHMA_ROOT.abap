class ZCL_AP_SHMA_ROOT definition
  public
  create public
  shared memory enabled .

public section.

  interfaces IF_SHM_BUILD_INSTANCE .

  types:
    BEGIN OF t_cache_mem,
        tabla   TYPE tabname,
        clave   TYPE string,
        clave2  TYPE string,
        clave3  TYPE string,
        clave4  TYPE string,
        clave5  TYPE string,
        valor   TYPE string,
        valor2  TYPE string,
        valor3  TYPE string,
        xstring TYPE xstring,
      END OF t_cache_mem .
  types:
    BEGIN OF t_report,
        cprog  TYPE cprog,
        tabla  TYPE tabname,
        clave  TYPE string,
        clave2 TYPE string,
        clave3 TYPE string,
        clave4 TYPE string,
        clave5  TYPE string,
        time(14),
      END OF t_report .
  types:
    tt_cache_mem TYPE SORTED TABLE OF t_cache_mem WITH NON-UNIQUE KEY tabla clave clave2 clave3 clave4 clave5 .
  types:
    tt_reports TYPE SORTED TABLE OF t_report WITH NON-UNIQUE KEY cprog tabla clave clave2 clave3 clave4 clave5 .

  methods SET_CACHE
    importing
      !I_CACHE type TT_CACHE_MEM
      !CPROG type SY-CPROG default SY-CPROG .
  methods GET_CACHE
    importing
      !CPROG type SY-CPROG default SY-CPROG
    exporting
      !I_CACHE type TT_CACHE_MEM .
  methods DELETE_CACHE
    importing
      !CPROG type SY-CPROG default SY-CPROG
    exporting
      !I_CACHE type TT_CACHE_MEM .
protected section.
private section.

  data I_CACHE_MEM type TT_CACHE_MEM .
  data I_REPORTS type TT_REPORTS .
endclass. "ZCL_AP_SHMA_ROOT definition
class ZCL_AP_SHMA_ROOT implementation.
METHOD delete_cache.
    FIELD-SYMBOLS: <cache>  TYPE t_cache_mem,
                   <report> TYPE t_report.

    LOOP AT i_reports ASSIGNING <report> WHERE cprog = cprog.
      READ TABLE i_cache_mem ASSIGNING <cache> WITH KEY tabla   = <report>-tabla
                                                         clave   = <report>-clave
                                                         clave2  = <report>-clave2
                                                         clave3  = <report>-clave3
                                                         clave4  = <report>-clave4
                                                         clave5  = <report>-clave5.
      IF sy-subrc = 0.
        DELETE i_cache_mem.
      ENDIF.
      DELETE i_reports.
    ENDLOOP.

  ENDMETHOD.
METHOD get_cache.
    FIELD-SYMBOLS: <cache>  TYPE t_cache_mem,
                   <report> TYPE t_report.

    LOOP AT i_reports ASSIGNING <report> WHERE cprog = cprog.
      READ TABLE i_cache_mem ASSIGNING <cache> WITH KEY tabla    = <report>-tabla
                                                         clave   = <report>-clave
                                                         clave2  = <report>-clave2
                                                         clave3  = <report>-clave3
                                                         clave4  = <report>-clave4
                                                         clave5  = <report>-clave5.
      IF sy-subrc = 0.
        APPEND <cache> TO i_cache.
      ENDIF..
    ENDLOOP.

  ENDMETHOD.
METHOD if_shm_build_instance~build.

  CALL FUNCTION 'DB_COMMIT'.

  DATA: area         TYPE REF TO zcl_ap_shma_area,
        root         TYPE REF TO zcl_ap_shma_root,
        lx_exception TYPE REF TO cx_root,
        l_cache      TYPE t_cache_mem,
        i_cache      TYPE tt_cache_mem.

  TRY.
      area = zcl_ap_shma_area=>attach_for_write( ).
    CATCH cx_shm_error INTO lx_exception.
      RETURN.
*      RAISE EXCEPTION TYPE cx_shm_build_failed
*        EXPORTING
*          previous = lx_exception.
    CATCH cx_root.
      RETURN.
  ENDTRY.

  TRY.
      CREATE OBJECT root AREA HANDLE area.
      area->set_root( root ).

      APPEND l_cache TO i_cache.
      root->set_cache( i_cache = i_cache cprog = 'ROOT' ).
      area->detach_commit( ).
    CATCH cx_shm_error INTO lx_exception.
*      RAISE EXCEPTION TYPE cx_shm_build_failed
*        EXPORTING
*          previous = lx_exception.
      return.
  ENDTRY.
  IF invocation_mode = cl_shm_area=>invocation_mode_auto_build.
    CALL FUNCTION 'DB_COMMIT'.
  ENDIF.


ENDMETHOD.
METHOD set_cache.
    FIELD-SYMBOLS: <cache>  TYPE t_cache_mem,
                   <cachem> TYPE t_cache_mem.
    DATA l_report TYPE  t_report.

    LOOP AT i_cache ASSIGNING <cache>.
      READ TABLE i_cache_mem ASSIGNING <cachem> WITH KEY tabla   = <cache>-tabla
                                                         clave   = <cache>-clave
                                                         clave2  = <cache>-clave2
                                                         clave3  = <cache>-clave3
                                                         clave4  = <cache>-clave4
                                                         clave5  = <cache>-clave5.
      IF sy-subrc = 0.
        IF <cache> = <cachem>.
          CONTINUE.
        ELSE.
          DELETE i_cache_mem INDEX sy-tabix.
          DELETE i_reports WHERE tabla   = <cache>-tabla "#EC CI_SORTSEQ
                             AND clave   = <cache>-clave
                             AND clave2  = <cache>-clave2
                             AND clave3  = <cache>-clave3
                             AND clave4  = <cache>-clave4
                             AND clave5  = <cache>-clave5.
        ENDIF.
      ENDIF.

      INSERT <cache> INTO TABLE i_cache_mem.
      CLEAR l_report.
      MOVE-CORRESPONDING <cache> TO l_report.
      l_report-cprog = cprog.
      CONCATENATE sy-datum sy-uzeit INTO l_report-time.
      INSERT l_report INTO TABLE i_reports.
    ENDLOOP.

  ENDMETHOD.
