REPORT zdescarga_datos_ui5.

TABLES ekpo.
SELECT-OPTIONS: s_report FOR ekpo-txz01 DEFAULT 'UI5',
                s_clave  FOR ekpo-txz01.

PARAMETERS: grabar RADIOBUTTON GROUP g,
            cargar RADIOBUTTON GROUP g.


PARAMETERS: fichero TYPE string DEFAULT 'C:\temp\cache_ui5.ZIP'.

IF grabar = 'X'.
  __data_set_vart zcache.
  DATA: izip      TYPE REF TO cl_abap_zip,
        l_xstring TYPE xstring,
        l_xml     TYPE xstring.

  SELECT * FROM zcache
    INTO TABLE i_zcache
   WHERE report IN s_report
     AND clave  IN s_clave.

  IF sy-subrc NE 0.
    MESSAGE 'No hay datos en zcache' TYPE 'E'.
  ELSE.
    TRY.
        CALL TRANSFORMATION id
         SOURCE i_zcache = i_zcache
         RESULT XML  l_xml.
      CATCH cx_root.
        MESSAGE 'Error en transformacion' TYPE 'E'.
    ENDTRY.

    CREATE OBJECT izip.

    izip->add( name    = 'CACHE.XML' content = l_xml ).

    l_xstring = izip->save( ).


    zcl_ap_ficheros=>grabar_xstring( EXPORTING fichero = fichero xstring = l_xstring mostrar_error = 'X' ).

    MESSAGE i398(00) WITH 'Se ha grabado fichero' fichero.

  ENDIF.
ELSE.
  CREATE OBJECT izip.
  zcl_ap_ficheros=>leer_xstring( EXPORTING fichero = fichero IMPORTING xstring = l_xstring ).
  CHECK NOT l_xstring IS INITIAL.
  izip->load( l_xstring ).

  izip->get( EXPORTING name = 'CACHE.XML'
             IMPORTING content = l_xml ).

  IF l_xml IS INITIAL.
    MESSAGE 'No hay registros' TYPE 'I'.
  ELSE.
    CALL TRANSFORMATION id
    SOURCE XML l_xml
    RESULT i_zcache = i_zcache.

    MODIFY zcache FROM TABLE i_zcache.

    MESSAGE 'Se ha actualizado ZCACHE' TYPE 'I'.
  ENDIF.
ENDIF.
