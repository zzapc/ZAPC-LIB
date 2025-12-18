*&---------------------------------------------------------------------*
*& Report ZDESCARGA_LOGO_SE78
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdescarga_logo_se78.

PARAMETERS: p_name TYPE stxbitmaps-tdname OBLIGATORY,
            p_path TYPE string OBLIGATORY.

DATA: lt_data    TYPE sbdst_content,
      lv_objid   TYPE stxbitmaps-tdname,
      lv_rc      TYPE sy-subrc,
      l_long     TYPE int4,
      l_long_bmp TYPE int4.

TYPES: BEGIN OF t_graphic_table,
         line TYPE x LENGTH 255,
       END OF t_graphic_table.

DATA: i_graphic_table TYPE TABLE OF t_graphic_table.

START-OF-SELECTION.

  lv_objid = p_name.

  CALL FUNCTION 'SAPSCRIPT_GET_GRAPHIC_BDS'
    EXPORTING
      i_object       = 'GRAPHICS'
      i_name         = lv_objid
      i_id           = 'BMAP'
      i_btype        = 'BMON'
    IMPORTING
      e_bytecount    = l_long
    TABLES
      content        = lt_data
    EXCEPTIONS
      not_found      = 1
      bds_get_failed = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    MESSAGE 'No se pudo obtener el gráfico' TYPE 'E'.
  ELSE.

    CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP'
      EXPORTING
        old_format               = 'BDS'
        new_format               = 'BMP'
        bitmap_file_bytecount_in = l_long
      IMPORTING
        bitmap_file_bytecount    = l_long_bmp
      TABLES
        bds_bitmap_file          = lt_data
        bitmap_file              = i_graphic_table
      EXCEPTIONS
        OTHERS                   = 1.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_path
        filetype                = 'BIN'
        bin_filesize            = l_long_bmp
      TABLES
        data_tab                = i_graphic_table
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        OTHERS                  = 5.

    IF sy-subrc = 0.
      MESSAGE 'Logo exportado correctamente' TYPE 'S'.
    ELSE.
      MESSAGE 'Error al guardar el archivo' TYPE 'E'.
    ENDIF.
  ENDIF.
