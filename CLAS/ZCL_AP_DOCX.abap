CLASS zcl_ap_docx DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_marcadores,
        id    TYPE string,
        name  TYPE string,
        texto TYPE string,
      END OF t_marcadores.
    TYPES tt_marcadores TYPE STANDARD TABLE OF t_marcadores.
    TYPES:
      BEGIN OF ty_border,
* Border width
* Integer: 8 = 1pt
* Default: no border
        width TYPE i,
* Space between border and content
* Integer
* Default: document default
        space TYPE i,
* Border color
* You can use the predefined font color constants or specify any rgb hexa color code
* Default : document default
        color TYPE string,
* Border style
* You can use the predefined border style constants
* Default : document default
        style TYPE string, " border style
      END OF ty_border.
    TYPES:
      BEGIN OF ty_character_style_effect,
* Font name to use for the character text fragment
* You can use the predefined font constants
* Default : document default
        font      TYPE string,
* Size of the font in pt
* Default : document default
        size      TYPE i,
* Font color to apply to the character text fragment
* You can use the predefined font color constants or specify any rgb hexa color code
* Default : document default
        color     TYPE string,
* Background color to apply to the character text fragment
* You can use the predefined font color constants or specify any rgb hexa color code
* Default : document default
        bgcolor   TYPE string,
* Highlight color to apply to the character text fragment
* You must use the predefined highlight color constants (limited color choice)
* If you want to use other color, please use bgcolor instead of highlight
* Default : document default
        highlight TYPE string,
* Set character text fragment as bold (boolean)
* You must use predefined true/false constants
* Default : not bold
        bold      TYPE i,
* Set character text fragment as italic (boolean)
* You must use predefined true/false constants
* Default : not italic
        italic    TYPE i,
* Set character text fragment as underline (boolean)
* You must use predefined true/false constants
* Default : not underline
        underline TYPE i,
* Set character text fragment as strike (boolean)
* You must use predefined true/false constants
* Default : not strike
        strike    TYPE i,
* Set character text fragment as exponent (boolean)
* You must use predefined true/false constants
* Default : not exponent
        sup       TYPE i,
* Set character text fragment as subscript (boolean)
* You must use predefined true/false constants
* Default : not subscript
        sub       TYPE i,
* Set character text fragment as upper case (boolean)
* You must use predefined true/false constants
* Default : not upper case
        caps      TYPE i,
* Set character text fragment as small upper case (boolean)
* You must use predefined true/false constants
* Default : not small upper case
        smallcaps TYPE i,
* Letter spacing to apply to the character text fragment
* 0 = normal, +20 = expand 1pt, -20 = condense 1pt
* Default : document default
        spacing   TYPE string,
* Name of the label to use for this text fragment
* Be carefull that if c_label_anchor is not found in text fragment,
* this attribute is ignored
        label     TYPE string,
      END OF ty_character_style_effect.
    TYPES:
      BEGIN OF ty_paragraph_style_effect,
* Set alignment for the paragraph (left, right, center, justify)
* Use the predefined alignment constants
* Default : document default
        alignment           TYPE string,
* Set spacing before paragraph to "auto" (boolean)
* Use the predefined true/false constants
* Default : document default
        spacing_before_auto TYPE i, " boolean
* Set spacing before paragraph
* Integer value, 20 = 1pt
* Default : document default
        spacing_before      TYPE string,
* Set spacing after paragraph to "auto" (boolean)
* Use the predefined true/false constants
* Default : document default
        spacing_after_auto  TYPE i, " boolean
* Set spacing after paragraph
* Integer value: 20 = 1pt
* Default : document default
        spacing_after       TYPE string,
* Set interline in paragraph
* Integer value: 240 = normal interline, 120 = multiple x0.5, 480 = multiple x2
* Default : document default
        interline           TYPE i,
* Set left indentation in paragraph
* Integer value: 567 = 1cm
* Default : document default
        leftindent          TYPE string,
* Set right indentation in paragraph
* Integer value: 567 = 1cm
* Default : document default
        rightindent         TYPE string,
* Set left indentation for first line in paragraph
* Integer value: 567 = 1cm. Negative value allowed
* Default : document default
        firstindent         TYPE string,
* Add a breakpage before paragraph (boolean)
* Use the predefined true/false constants
* Default : no breakpage
        break_before        TYPE i,
* Set the hierarchical title level of the paragraph
* 1 for title1, 2 for title2...
* Default : not a hierarchical title
        hierarchy_level     TYPE i,
* Set the left border of the paragraph
* See class type ty_border for details
* Default : No border
        border_left         TYPE ty_border,
* Set the top border of the paragraph
* See class type ty_border for details
* Default : No border
        border_top          TYPE ty_border,
* Set the right border of the paragraph
* See class type ty_border for details
* Default : No border
        border_right        TYPE ty_border,
* Set the bottom border of the paragraph
* See class type ty_border for details
* Default : No border
        border_bottom       TYPE ty_border,
* Background color to apply to the paragraph
* You can use the predefined font color constants or specify any rgb hexa color code
* Default : document default
        bgcolor             TYPE string,
      END OF ty_paragraph_style_effect.
    TYPES:
      BEGIN OF ty_list_style,
        type TYPE string,
        name TYPE string,
      END OF ty_list_style.
    TYPES ty_list_style_table TYPE STANDARD TABLE OF ty_list_style.
    TYPES:
      BEGIN OF ty_list_object,
        id   TYPE string,
        type TYPE string,
        path TYPE string,
      END OF ty_list_object.
    TYPES ty_list_object_table TYPE STANDARD TABLE OF ty_list_object.
    TYPES:
      BEGIN OF ty_table_style_field,
* Content of the cell
        textline          TYPE string,
* Character style name
        style             TYPE string,
* Direct character style effect
        style_effect      TYPE ty_character_style_effect,
* Paragraph style name
        line_style        TYPE string,
* Direct paragraph style effect
        line_style_effect TYPE ty_paragraph_style_effect,
* Cell background color in hexa RGB
        bgcolor           TYPE string,
* Set vertical alignment for cell
        valign            TYPE string,
* Set number of horizontal cell merged
* Start from 2 to merge the next cell with current
* Next cell will be completely ignored
* 0 or 1 to ignore this parameter
        merge             TYPE i,
* Instead of text, insert an image in the cell
* textline, style, style_effect are ignored
        image_id          TYPE string,
* For complex cell content, insert xml paragraph fragment
* You cannot insert xml fragment that are not in a (or many) paragraph
* textline, style, style_effect, line_style, line_style_effect, image_id are ignored
        xml               TYPE string,
      END OF ty_table_style_field.
    TYPES:
      BEGIN OF ty_style_table,
        firstcol TYPE i, " boolean, first col is different
        firstrow TYPE i, " boolean, first row is different
        lastcol  TYPE i, " boolean, last col is different
        lastrow  TYPE i, " boolean, last row is different
        nozebra  TYPE i, " boolean, no line zebra
        novband  TYPE i, " boolean, no column zebra
      END OF ty_style_table.

    CONSTANTS c_true                         TYPE i          VALUE 1 ##NO_TEXT.
    CONSTANTS c_false                        TYPE i          VALUE 0 ##NO_TEXT.
    CONSTANTS c_field_pagecount              TYPE string     VALUE '##FIELD#PAGE##' ##NO_TEXT.
    CONSTANTS c_field_pagetotal              TYPE string     VALUE '##FIELD#NUMPAGES##' ##NO_TEXT.
    CONSTANTS c_field_title                  TYPE string     VALUE '##FIELD#TITLE##' ##NO_TEXT.
    CONSTANTS c_field_author                 TYPE string     VALUE '##FIELD#AUTHOR##' ##NO_TEXT.
    CONSTANTS c_field_authorlastmod          TYPE string     VALUE '##FIELD#LASTSAVEDBY##' ##NO_TEXT.
    CONSTANTS c_field_comments               TYPE string     VALUE '##FIELD#COMMENTS##' ##NO_TEXT.
    CONSTANTS c_field_keywords               TYPE string     VALUE '##FIELD#KEYWORDS##' ##NO_TEXT.
    CONSTANTS c_field_category               TYPE string     VALUE '##FIELD#DOCPROPERTY CATEGORY##' ##NO_TEXT.
    CONSTANTS c_field_subject                TYPE string     VALUE '##FIELD#SUBJECT##' ##NO_TEXT.
    CONSTANTS c_field_revision               TYPE string     VALUE '##FIELD#REVNUM##' ##NO_TEXT.
    CONSTANTS c_field_creationdate           TYPE string     VALUE '##FIELD#CREATEDATE##' ##NO_TEXT.
    CONSTANTS c_field_moddate                TYPE string     VALUE '##FIELD#SAVEDATE##' ##NO_TEXT.
    CONSTANTS c_field_todaydate              TYPE string     VALUE '##FIELD#DATE##' ##NO_TEXT.
    CONSTANTS c_field_filename               TYPE string     VALUE '##FIELD#FILENAME##' ##NO_TEXT.
    CONSTANTS c_label_anchor                 TYPE string     VALUE '##LABEL##' ##NO_TEXT.
    CONSTANTS c_sapwr_prefix                 TYPE c LENGTH 6 VALUE 'SAPWR:' ##NO_TEXT.
    CONSTANTS c_font_arial                   TYPE string     VALUE 'Arial' ##NO_TEXT.
    CONSTANTS c_font_times                   TYPE string     VALUE 'Times New Roman' ##NO_TEXT.
    CONSTANTS c_font_comic                   TYPE string     VALUE 'Comic Sans MS' ##NO_TEXT.
    CONSTANTS c_font_calibri                 TYPE string     VALUE 'Calibri' ##NO_TEXT.
    CONSTANTS c_font_cambria                 TYPE string     VALUE 'Cambria' ##NO_TEXT.
    CONSTANTS c_font_courier                 TYPE string     VALUE 'Courier New' ##NO_TEXT.
    CONSTANTS c_font_symbol                  TYPE string     VALUE 'Wingdings' ##NO_TEXT.
    CONSTANTS c_style_title                  TYPE string     VALUE 'Title' ##NO_TEXT.
    CONSTANTS c_style_title1                 TYPE string     VALUE 'Title1' ##NO_TEXT.
    CONSTANTS c_style_title2                 TYPE string     VALUE 'Title2' ##NO_TEXT.
    CONSTANTS c_style_title3                 TYPE string     VALUE 'Title3' ##NO_TEXT.
    CONSTANTS c_style_title4                 TYPE string     VALUE 'Title4' ##NO_TEXT.
    CONSTANTS c_style_normal                 TYPE string     VALUE 'Normal' ##NO_TEXT.
    CONSTANTS c_breaktype_line               TYPE i          VALUE 6 ##NO_TEXT.
    CONSTANTS c_breaktype_page               TYPE i          VALUE 7 ##NO_TEXT.
    CONSTANTS c_breaktype_column             TYPE i          VALUE 1 ##NO_TEXT.
    CONSTANTS c_breaktype_section            TYPE i          VALUE 2 ##NO_TEXT.
    CONSTANTS c_breaktype_section_continuous TYPE i          VALUE 3 ##NO_TEXT.
    CONSTANTS c_symbol_checkbox_checked      TYPE string     VALUE 'þ' ##NO_TEXT.
    CONSTANTS c_symbol_checkbox              TYPE string     VALUE 'o' ##NO_TEXT.
    CONSTANTS c_draw_image                   TYPE i          VALUE 0 ##NO_TEXT.
    CONSTANTS c_draw_rectangle               TYPE i          VALUE 1 ##NO_TEXT.
    CONSTANTS c_ddraw_rectangle              TYPE string     VALUE 'rect' ##NO_TEXT.
    CONSTANTS c_ddraw_rectangle_rounded      TYPE string     VALUE 'roundrect' ##NO_TEXT.
    CONSTANTS c_ddraw_ovale                  TYPE string     VALUE 'oval' ##NO_TEXT.
    CONSTANTS c_align_left                   TYPE string     VALUE 'left' ##NO_TEXT.
    CONSTANTS c_align_center                 TYPE string     VALUE 'center' ##NO_TEXT.
    CONSTANTS c_align_right                  TYPE string     VALUE 'right' ##NO_TEXT.
    CONSTANTS c_align_justify                TYPE string     VALUE 'both' ##NO_TEXT.
    CONSTANTS c_valign_top                   TYPE string     VALUE 'top' ##NO_TEXT.
    CONSTANTS c_valign_middle                TYPE string     VALUE 'center' ##NO_TEXT.
    CONSTANTS c_valign_bottom                TYPE string     VALUE 'bottom' ##NO_TEXT.
    CONSTANTS c_imgtype_inline               TYPE i          VALUE 1 ##NO_TEXT.
    CONSTANTS c_imgtype_float                TYPE i          VALUE 2 ##NO_TEXT.
    CONSTANTS c_imgtype_over                 TYPE i          VALUE 3 ##NO_TEXT.
    CONSTANTS c_imgtype_behind               TYPE i          VALUE 4 ##NO_TEXT.
    CONSTANTS c_imgposx_left                 TYPE string     VALUE 'left' ##NO_TEXT.
    CONSTANTS c_imgposx_right                TYPE string     VALUE 'right' ##NO_TEXT.
    CONSTANTS c_imgposx_center               TYPE string     VALUE 'center' ##NO_TEXT.
    CONSTANTS c_imgposy_top                  TYPE string     VALUE 'top' ##NO_TEXT.
    CONSTANTS c_imgposy_middle               TYPE string     VALUE 'center' ##NO_TEXT.
    CONSTANTS c_imgposy_bottom               TYPE string     VALUE 'bottom' ##NO_TEXT.
    CONSTANTS c_type_header                  TYPE string     VALUE 'header' ##NO_TEXT.
    CONSTANTS c_type_footer                  TYPE string     VALUE 'footer' ##NO_TEXT.
    CONSTANTS c_type_paragraph               TYPE string     VALUE 'paragraph' ##NO_TEXT.
    CONSTANTS c_type_character               TYPE string     VALUE 'character' ##NO_TEXT.
    CONSTANTS c_type_table                   TYPE string     VALUE 'table' ##NO_TEXT.
    CONSTANTS c_type_numbering               TYPE string     VALUE 'numbering' ##NO_TEXT.
    CONSTANTS c_type_image                   TYPE string     VALUE 'image' ##NO_TEXT.
    CONSTANTS c_orient_landscape             TYPE i          VALUE 1 ##NO_TEXT.
    CONSTANTS c_orient_portrait              TYPE i          VALUE 0 ##NO_TEXT.
    CONSTANTS c_notetype_foot                TYPE i          VALUE 1 ##NO_TEXT.
    CONSTANTS c_notetype_end                 TYPE i          VALUE 2 ##NO_TEXT.
    CONSTANTS c_color_black                  TYPE string     VALUE '000000' ##NO_TEXT.
    CONSTANTS c_color_blue                   TYPE string     VALUE '0000FF' ##NO_TEXT.
    CONSTANTS c_color_turquoise              TYPE string     VALUE '00FFFF' ##NO_TEXT.
    CONSTANTS c_color_brightgreen            TYPE string     VALUE '00FF00' ##NO_TEXT.
    CONSTANTS c_color_pink                   TYPE string     VALUE 'FF00FF' ##NO_TEXT.
    CONSTANTS c_color_red                    TYPE string     VALUE 'FF0000' ##NO_TEXT.
    CONSTANTS c_color_yellow                 TYPE string     VALUE 'FFFF00' ##NO_TEXT.
    CONSTANTS c_color_white                  TYPE string     VALUE 'FFFFFF' ##NO_TEXT.
    CONSTANTS c_color_darkblue               TYPE string     VALUE '000080' ##NO_TEXT.
    CONSTANTS c_color_teal                   TYPE string     VALUE '008080' ##NO_TEXT.
    CONSTANTS c_color_green                  TYPE string     VALUE '008000' ##NO_TEXT.
    CONSTANTS c_color_violet                 TYPE string     VALUE '800080' ##NO_TEXT.
    CONSTANTS c_color_darkred                TYPE string     VALUE '800000' ##NO_TEXT.
    CONSTANTS c_color_darkyellow             TYPE string     VALUE '808000' ##NO_TEXT.
    CONSTANTS c_color_gray                   TYPE string     VALUE '808080' ##NO_TEXT.
    CONSTANTS c_color_lightgray              TYPE string     VALUE 'C0C0C0' ##NO_TEXT.
    CONSTANTS c_border_simple                TYPE string     VALUE 'single' ##NO_TEXT.
    CONSTANTS c_border_double                TYPE string     VALUE 'double' ##NO_TEXT.
    CONSTANTS c_border_triple                TYPE string     VALUE 'triple' ##NO_TEXT.
    CONSTANTS c_border_dot                   TYPE string     VALUE 'dotted' ##NO_TEXT.
    CONSTANTS c_border_dash                  TYPE string     VALUE 'dashed' ##NO_TEXT.
    CONSTANTS c_border_wave                  TYPE string     VALUE 'wave' ##NO_TEXT.
    CONSTANTS c_highlight_yellow             TYPE string     VALUE 'yellow' ##NO_TEXT.
    CONSTANTS c_highlight_green              TYPE string     VALUE 'green' ##NO_TEXT.
    CONSTANTS c_highlight_cyan               TYPE string     VALUE 'cyan' ##NO_TEXT.
    CONSTANTS c_highlight_magenta            TYPE string     VALUE 'magenta' ##NO_TEXT.
    CONSTANTS c_highlight_blue               TYPE string     VALUE 'blue' ##NO_TEXT.
    CONSTANTS c_highlight_red                TYPE string     VALUE 'red' ##NO_TEXT.
    CONSTANTS c_highlight_darkblue           TYPE string     VALUE 'darkBlue' ##NO_TEXT.
    CONSTANTS c_highlight_darkcyan           TYPE string     VALUE 'darkCyan' ##NO_TEXT.
    CONSTANTS c_highlight_darkgreen          TYPE string     VALUE 'darkGreen' ##NO_TEXT.
    CONSTANTS c_highlight_darkmagenta        TYPE string     VALUE 'darkMagenta' ##NO_TEXT.
    CONSTANTS c_highlight_darkred            TYPE string     VALUE 'darkRed' ##NO_TEXT.
    CONSTANTS c_highlight_darkyellow         TYPE string     VALUE 'darkYellow' ##NO_TEXT.
    CONSTANTS c_highlight_darkgray           TYPE string     VALUE 'darkGray' ##NO_TEXT.
    CONSTANTS c_highlight_lightgray          TYPE string     VALUE 'lightGray' ##NO_TEXT.
    CONSTANTS c_highlight_black              TYPE string     VALUE 'black' ##NO_TEXT.

    DATA mw_doc_inicial TYPE string.
    DATA mw_docxml      TYPE string.
    DATA i_marcadores   TYPE tt_marcadores.

    METHODS constructor
      IMPORTING tpl              TYPE any       OPTIONAL
                keep_tpl_content TYPE i         DEFAULT c_true
                content          TYPE xstring   OPTIONAL
                tcode            TYPE any       OPTIONAL
                plantilla        TYPE any       OPTIONAL
                get_marcadores   TYPE abap_bool DEFAULT ''.

    METHODS write_text
      IMPORTING textline           TYPE any
                !style             TYPE string                    OPTIONAL
                style_effect       TYPE ty_character_style_effect OPTIONAL
                line_style         TYPE string                    OPTIONAL
                line_style_effect  TYPE ty_paragraph_style_effect OPTIONAL
      EXPORTING virtual            TYPE string
                invalid_style      TYPE i
                invalid_line_style TYPE i.

    METHODS write_line
      IMPORTING !style        TYPE string                    OPTIONAL
                style_effect  TYPE ty_paragraph_style_effect OPTIONAL
      EXPORTING invalid_style TYPE i
      CHANGING  virtual       TYPE string                    OPTIONAL.

    METHODS write_break
      IMPORTING breaktype  TYPE i      DEFAULT c_breaktype_line
                write_line TYPE i      OPTIONAL
      CHANGING  virtual    TYPE string OPTIONAL.

    METHODS write_symbol
      IMPORTING !symbol TYPE string.

    METHODS write_table
      IMPORTING content         TYPE STANDARD TABLE
                !style          TYPE string         OPTIONAL
                style_overwrite TYPE ty_style_table OPTIONAL
                border          TYPE i              DEFAULT c_true
                tblwidth        TYPE i              DEFAULT 0
      EXPORTING virtual         TYPE string
                invalid_style   TYPE i.

    METHODS write_headerfooter
      IMPORTING !type              TYPE string                    DEFAULT c_type_header
                textline           TYPE string
                usenow_default     TYPE i                         DEFAULT c_true
                usenow_first       TYPE i                         DEFAULT c_true
                !style             TYPE string                    OPTIONAL
                style_effect       TYPE ty_character_style_effect OPTIONAL
                line_style         TYPE string                    OPTIONAL
                line_style_effect  TYPE ty_paragraph_style_effect OPTIONAL
      EXPORTING !id                TYPE string
                invalid_style      TYPE i
                invalid_line_style TYPE i.

    METHODS set_title
      IMPORTING !title TYPE string.

    METHODS write_newpage.

    METHODS write_toc
      IMPORTING !default TYPE string OPTIONAL
                label    TYPE string OPTIONAL.

    METHODS write_note
      IMPORTING !text              TYPE string
                !type              TYPE i                         DEFAULT c_notetype_foot
                !style             TYPE string                    OPTIONAL
                style_effect       TYPE ty_character_style_effect OPTIONAL
                line_style         TYPE string                    OPTIONAL
                line_style_effect  TYPE ty_paragraph_style_effect OPTIONAL
                link_style         TYPE string                    OPTIONAL
                link_style_effect  TYPE ty_character_style_effect OPTIONAL
      EXPORTING invalid_style      TYPE i
                invalid_link_style TYPE i
                invalid_line_style TYPE i.

    METHODS write_comment
      IMPORTING !text              TYPE string
                !style             TYPE string                    OPTIONAL
                style_effect       TYPE ty_character_style_effect OPTIONAL
                line_style         TYPE string                    OPTIONAL
                line_style_effect  TYPE ty_paragraph_style_effect OPTIONAL
                head_style         TYPE string                    OPTIONAL
                head_style_effect  TYPE ty_character_style_effect OPTIONAL
                !datum             TYPE d                         DEFAULT sy-datum
                !uzeit             TYPE t                         DEFAULT sy-uzeit
                author             TYPE string                    OPTIONAL
                initials           TYPE string                    OPTIONAL
      EXPORTING invalid_style      TYPE i
                invalid_head_style TYPE i
                invalid_line_style TYPE i.

    METHODS direct_draw
      IMPORTING !type           TYPE string
                !width          TYPE p
                height          TYPE p
                posx            TYPE p      OPTIONAL
                posy            TYPE p      OPTIONAL
                zindex          TYPE i      DEFAULT 100
                bg_color        TYPE string DEFAULT ''
                border_color    TYPE string OPTIONAL
                border_width    TYPE p      OPTIONAL
                !unit           TYPE string DEFAULT 'pt'
                !text           TYPE string OPTIONAL
                text_style      TYPE string OPTIONAL
                text_line_style TYPE string DEFAULT c_style_normal
                text_xml        TYPE string OPTIONAL.

    METHODS draw_init
      IMPORTING !left   TYPE i
                !top    TYPE i
                !width  TYPE i
                height  TYPE i
                bgcolor TYPE string OPTIONAL
                bdcolor TYPE string OPTIONAL
                bdwidth TYPE f      OPTIONAL.

    METHODS draw
      IMPORTING !object       TYPE i
                !left         TYPE i      DEFAULT 0
                !top          TYPE i      DEFAULT 0
                !width        TYPE i      DEFAULT 0
                height        TYPE i      DEFAULT 0
                url           TYPE string OPTIONAL
                bgcolor       TYPE string OPTIONAL
                bdcolor       TYPE string OPTIONAL
                bdwidth       TYPE f      OPTIONAL
      EXPORTING invalid_image TYPE i
      CHANGING  !id           TYPE string OPTIONAL.

    METHODS draw_finalize.

    METHODS insert_custom_field
      IMPORTING !field TYPE string.

    METHODS insert_virtual_field
      IMPORTING !field             TYPE string
                field_as_paragraph TYPE i DEFAULT c_false.

    METHODS replace_virtual_field
      IMPORTING !field        TYPE string
                !value        TYPE string
                value_as_xml  TYPE i                         DEFAULT c_false
                style_effect  TYPE ty_character_style_effect OPTIONAL
                !style        TYPE string                    OPTIONAL
      EXPORTING invalid_style TYPE i.

    METHODS create_custom_field
      IMPORTING !field TYPE string
                !value TYPE string.

    METHODS create_character_style
      IMPORTING output_name   TYPE string
                style_effect  TYPE ty_character_style_effect
                style_ref     TYPE string OPTIONAL
      EXPORTING !name         TYPE string
                invalid_style TYPE i.

    METHODS create_paragraph_style
      IMPORTING output_name       TYPE string
                style_effect      TYPE ty_character_style_effect
                line_style_effect TYPE ty_paragraph_style_effect
                style_ref         TYPE string OPTIONAL
      EXPORTING !name             TYPE string
                invalid_style     TYPE i.

    METHODS insert_image
      IMPORTING url           TYPE string                    OPTIONAL
                file_content  TYPE xstring                   OPTIONAL
                zoom          TYPE f                         OPTIONAL
                as_paragraph  TYPE i                         DEFAULT c_true
                !style        TYPE string                    OPTIONAL
                style_effect  TYPE ty_paragraph_style_effect OPTIONAL
                !type         TYPE i                         DEFAULT c_imgtype_inline
                posx          TYPE string                    DEFAULT c_imgposx_left
                posy          TYPE string                    DEFAULT c_imgposy_top
                !margin       TYPE i                         OPTIONAL
      EXPORTING invalid_image TYPE i
                invalid_style TYPE i
                virtual       TYPE string
      CHANGING  !id           TYPE string                    OPTIONAL.

    METHODS insert_attachment
      IMPORTING url           TYPE string
                file_content  TYPE xstring                   OPTIONAL
                url_img       TYPE string                    OPTIONAL
                as_paragraph  TYPE i                         DEFAULT c_true
                !style        TYPE string                    OPTIONAL
                style_effect  TYPE ty_paragraph_style_effect OPTIONAL
      EXPORTING invalid_image TYPE i
                invalid_file  TYPE i
                invalid_style TYPE i
                virtual       TYPE string
      CHANGING  id_img        TYPE string                    OPTIONAL.

    METHODS set_properties
      IMPORTING !title       TYPE string OPTIONAL
                author       TYPE string OPTIONAL
                !description TYPE string OPTIONAL
                !object      TYPE string OPTIONAL
                category     TYPE string OPTIONAL
                !keywords    TYPE string OPTIONAL
                !status      TYPE string OPTIONAL
                creationdate TYPE d      OPTIONAL
                creationtime TYPE t      OPTIONAL
                revision     TYPE i      OPTIONAL.

    METHODS set_params
      IMPORTING orientation   TYPE i         DEFAULT c_orient_portrait
                border_left   TYPE ty_border OPTIONAL
                border_top    TYPE ty_border OPTIONAL
                border_right  TYPE ty_border OPTIONAL
                border_bottom TYPE ty_border OPTIONAL
                nospellcheck  TYPE i         DEFAULT c_false.

    METHODS save
      IMPORTING url    TYPE string
                !local TYPE i         DEFAULT c_true
                abrir  TYPE abap_bool DEFAULT ''.

    METHODS get_docx_file
      EXPORTING xcontent TYPE xstring.

    METHODS header_footer_direct_assign
      IMPORTING !header        TYPE string OPTIONAL
                header_first   TYPE string OPTIONAL
                footer         TYPE string OPTIONAL
                footer_first   TYPE string OPTIONAL
      EXPORTING invalid_header TYPE i
                invalid_footer TYPE i.

    METHODS get_list_style
      EXPORTING style_list TYPE ty_list_style_table.

    METHODS get_list_image
      EXPORTING image_list TYPE ty_list_object_table.

    METHODS get_list_headerfooter
      EXPORTING headerfooter_list TYPE ty_list_object_table.

    METHODS insert_xml_fragment
      IMPORTING !xml TYPE string.

    METHODS insert_xml
      IMPORTING !xml TYPE string.

    METHODS mostrar_en_pantalla.

    METHODS reemplazar_valor
      IMPORTING !var         TYPE any
                valor        TYPE any
                car_marca    TYPE any       DEFAULT '&'
                todas        TYPE abap_bool DEFAULT 'X'
                format_fecha TYPE char1     DEFAULT '/'
                format_hora  TYPE char1     DEFAULT '5'
                quitar_ceros TYPE abap_bool DEFAULT ''
      CHANGING  doc          TYPE string    OPTIONAL
      RETURNING VALUE(error) TYPE abap_bool.

    METHODS duplicar_contenido
      IMPORTING salto_pagina TYPE abap_bool DEFAULT 'X'.

    METHODS get_bloque
      IMPORTING tag           TYPE string
                doc           TYPE string    OPTIONAL
                incluir_tag   TYPE abap_bool DEFAULT ''
      RETURNING VALUE(string) TYPE string.

    METHODS get_bloque_marcador
      IMPORTING marcador      TYPE string
                doc           TYPE string    OPTIONAL
                tag           TYPE string
                incluir_tag   TYPE abap_bool DEFAULT 'X'
      RETURNING VALUE(string) TYPE string.

    METHODS set_tabla
      IMPORTING tabla           TYPE table
                marcador        TYPE string
                i_var           TYPE apb_lpd_t_key_value
                quitar_marcador TYPE abap_bool DEFAULT 'X'
                format_fecha    TYPE char1     DEFAULT '/'
                format_hora     TYPE char1     DEFAULT '5'
                quitar_ceros    TYPE abap_bool DEFAULT ''
                tag             TYPE string    DEFAULT 'tr'
      CHANGING  doc             TYPE string    OPTIONAL.

    METHODS get_marcadores
      IMPORTING doc          TYPE string DEFAULT ''
      EXPORTING i_marcadores TYPE tt_marcadores.

    METHODS get_texto_marcador
      IMPORTING marcador TYPE any
      EXPORTING texto    TYPE string
                !message TYPE bapi_msg.

    METHODS borrar_marcador
      IMPORTING marcador TYPE any
      EXPORTING !message TYPE bapi_msg.

    METHODS reemplazar_marcador
      IMPORTING marcador         TYPE any
                nuevo_contenido  TYPE string
                add_marcador_end TYPE abap_bool DEFAULT ''
      EXPORTING !message         TYPE bapi_msg
      CHANGING  doc              TYPE string    OPTIONAL.


  PRIVATE SECTION.
    DATA mw_fragxml  TYPE string.
    DATA mw_imgmaxid TYPE i                  VALUE 100 ##NO_TEXT.
    DATA mw_attach   TYPE i                  VALUE 10000 ##NO_TEXT.
    DATA mo_zip      TYPE REF TO cl_abap_zip.
    DATA:
      BEGIN OF ms_section,
        landscape     TYPE i,
        continuous    TYPE i,
        header_first  TYPE string,
        header        TYPE string,
        footer_first  TYPE string,
        footer        TYPE string,
        border_left   TYPE ty_border,
        border_top    TYPE ty_border,
        border_right  TYPE ty_border,
        border_bottom TYPE ty_border,
      END OF ms_section.
    DATA mw_section_xml     TYPE string.
    DATA mw_tpl_section_xml TYPE string.
    DATA mt_list_style      TYPE ty_list_style_table.
    DATA mt_list_object     TYPE ty_list_object_table.
    DATA mw_author          TYPE string.

    CONSTANTS c_basesize TYPE i VALUE 12700 ##NO_TEXT.

    METHODS _write_section.

    METHODS _get_zip_file
      IMPORTING filename TYPE string
      EXPORTING content  TYPE string.

    METHODS _update_zip_file
      IMPORTING filename TYPE string
                content  TYPE string.

    METHODS _load_file
      IMPORTING filename TYPE string
      EXPORTING xcontent TYPE xstring.

    METHODS _load_image
      IMPORTING url          TYPE string
                file_content TYPE xstring OPTIONAL
      EXPORTING imgres_x     TYPE i
                imgres_y     TYPE i
                !extension   TYPE string
      CHANGING  !id          TYPE string.

    METHODS _create_note
      IMPORTING !text              TYPE string
                !type              TYPE i
                !style             TYPE string                    OPTIONAL
                style_effect       TYPE ty_character_style_effect OPTIONAL
                line_style         TYPE string                    OPTIONAL
                line_style_effect  TYPE ty_paragraph_style_effect OPTIONAL
                link_style         TYPE string                    OPTIONAL
                link_style_effect  TYPE ty_character_style_effect OPTIONAL
      EXPORTING invalid_style      TYPE i
                invalid_line_style TYPE i
                !id                TYPE string.

    METHODS _protect_string
      IMPORTING !in  TYPE string
      EXPORTING !out TYPE string.

    METHODS _protect_label
      IMPORTING !in  TYPE string
      EXPORTING !out TYPE string.

    METHODS _build_character_style
      IMPORTING !style        TYPE string                    OPTIONAL
                style_effect  TYPE ty_character_style_effect OPTIONAL
      EXPORTING !xml          TYPE string
                invalid_style TYPE i.

    METHODS _build_paragraph_style
      IMPORTING !style        TYPE string                    OPTIONAL
                style_effect  TYPE ty_paragraph_style_effect OPTIONAL
      EXPORTING !xml          TYPE string
                invalid_style TYPE i.

    METHODS _get_xml_ns
      EXPORTING !xml TYPE string.
endclass. "ZCL_AP_DOCX definition
class ZCL_AP_DOCX implementation.
  METHOD _build_character_style.
    DATA : lw_string  TYPE string,
           lw_intsize TYPE i,
           lw_char6   TYPE c LENGTH 6.

    CLEAR xml.

    IF style_effect IS SUPPLIED.
      IF NOT style_effect-color IS INITIAL.
        CONCATENATE xml
                    '<w:color w:val="'
                    style_effect-color
                    '"/>'
                    INTO xml.
      ENDIF.

      IF NOT style_effect-bgcolor IS INITIAL.
        CONCATENATE xml
                    '<w:shd w:val="clear" w:color="auto" w:fill="'
                    style_effect-bgcolor
                    '"/>'
                    INTO xml.
      ENDIF.

      IF style_effect-bold = c_true.
        CONCATENATE xml
                    '<w:b/>'
                    INTO xml.
      ENDIF.

      IF style_effect-italic = c_true.
        CONCATENATE xml
                    '<w:i/>'
                    INTO xml.
      ENDIF.

      IF style_effect-underline = c_true.
        CONCATENATE xml
                    '<w:u w:val="single"/>'
                    INTO xml.
      ENDIF.

      IF style_effect-strike = c_true.
        CONCATENATE xml
                    '<w:strike/>'
                    INTO xml.
      ENDIF.

      IF style_effect-caps = c_true.
        CONCATENATE xml
                    '<w:caps/>'
                    INTO xml.
      ENDIF.

      IF style_effect-smallcaps = c_true.
        CONCATENATE xml
                    '<w:smallCaps/>'
                    INTO xml.
      ENDIF.

      IF NOT style_effect-highlight IS INITIAL.
        CONCATENATE xml
                    '<w:highlight w:val="'
                    style_effect-highlight
                    '"/>'
                    INTO xml.
      ENDIF.

      IF NOT style_effect-spacing IS INITIAL AND style_effect-spacing CO '0123456789 -'.
        IF style_effect-spacing > 0.
          lw_string = style_effect-spacing.
          CONDENSE lw_string NO-GAPS.
        ELSE.
          lw_string = - style_effect-spacing.
          CONDENSE lw_string NO-GAPS.
          CONCATENATE '-' lw_string INTO lw_string.
        ENDIF.
        CONCATENATE xml
                    '<w:spacing w:val="'
                    lw_string
                    '"/>'
                    INTO xml.
      ENDIF.

      IF style_effect-size IS NOT INITIAL.
        lw_intsize = style_effect-size * 2.
        lw_char6 = lw_intsize.
        CONDENSE lw_char6 NO-GAPS.
        CONCATENATE xml
                    '<w:sz w:val="'
                    lw_char6
                    '"/>'
                    '<w:szCs w:val="'
                    lw_char6
                    '"/>'
                    INTO xml.
      ENDIF.

      IF style_effect-sup = c_true.
        CONCATENATE xml
                    '<w:vertAlign w:val="superscript"/>'
                    INTO xml.
      ELSEIF style_effect-sub = c_true.
        CONCATENATE xml
                    '<w:vertAlign w:val="subscript"/>'
                    INTO xml.
      ENDIF.

      IF NOT style_effect-font IS INITIAL.
        CONCATENATE xml
                    '<w:rFonts w:ascii="'
                    style_effect-font
                    '" w:hAnsi="'
                    style_effect-font
                    '"/>'
                    INTO xml.
      ENDIF.
    ENDIF.

    IF style IS SUPPLIED AND style IS NOT INITIAL.
      IF line_exists( mt_list_style[ type = c_type_character
                                     name = style ] ).
        CONCATENATE xml
                    '<w:rStyle w:val="'
                    style
                    '"/>'
                    INTO xml.
      ELSE.
        invalid_style = c_true.
      ENDIF.
    ENDIF.

    IF NOT xml IS INITIAL.
      CONCATENATE '<w:rPr>'
                  xml
                  '</w:rPr>'
                  INTO xml.
    ENDIF.
  ENDMETHOD.
  METHOD _build_paragraph_style.
    DATA : lw_size     TYPE string,
           lw_substyle TYPE string,
           lw_space    TYPE string,
           lw_indent   TYPE string.

    IF style IS SUPPLIED AND NOT style IS INITIAL.
      IF line_exists( mt_list_style[ type = c_type_paragraph
                                     name = style ] ).
        CONCATENATE xml
                    '<w:pStyle w:val="'
                    style
                    '"/>'
                    INTO xml.
      ELSE.
        invalid_style = c_true.
      ENDIF.
    ENDIF.

    IF style_effect-break_before = c_true.
      CONCATENATE xml
                  '<w:pageBreakBefore/>'
                  INTO xml.
    ENDIF.

    IF NOT style_effect-hierarchy_level IS INITIAL.
      lw_size = style_effect-hierarchy_level - 1.
      CONDENSE lw_size NO-GAPS.
      CONCATENATE xml
                  '<w:outlineLvl w:val="'
                  lw_size
                  '"/>'
                  INTO xml.
    ENDIF.

    IF NOT style_effect-alignment IS INITIAL.
      CONCATENATE xml
                  '<w:jc w:val="'
                  style_effect-alignment
                  '"/>'
                  INTO xml.
    ENDIF.

    IF NOT style_effect-bgcolor IS INITIAL.
      CONCATENATE xml
                  '<w:shd w:val="clear" w:color="auto" w:fill="'
                  style_effect-bgcolor
                  '"/>'
                  INTO xml.
    ENDIF.

    CLEAR lw_substyle.
    IF style_effect-spacing_before_auto = c_true.
      lw_substyle = ' w:beforeAutospacing="1"'.
    ELSEIF NOT style_effect-spacing_before IS INITIAL AND style_effect-spacing_before CO '0123456789 '.
      lw_space = style_effect-spacing_before.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  ' w:beforeAutospacing="0" w:before="'
                  lw_space
                  '"'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.

    IF style_effect-spacing_after_auto = c_true.
      CONCATENATE lw_substyle
                  ' w:afterAutospacing="1"'
                  INTO lw_substyle RESPECTING BLANKS.
    ELSEIF NOT style_effect-spacing_after IS INITIAL AND style_effect-spacing_after CO '0123456789 '.
      lw_space = style_effect-spacing_after.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  ' w:afterAutospacing="0" w:after="'
                  lw_space
                  '"'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.

    IF NOT style_effect-interline IS INITIAL.
      lw_space = style_effect-interline.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  ' w:line="'
                  lw_space
                  '"'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.

    IF NOT lw_substyle IS INITIAL.
      CONCATENATE xml
                  '<w:spacing '
                  lw_substyle
                  '/>'
                  INTO xml RESPECTING BLANKS.
    ENDIF.

    CLEAR lw_substyle.
    IF NOT style_effect-leftindent IS INITIAL AND style_effect-leftindent CO '0123456789 '.
      lw_indent = style_effect-leftindent.
      CONDENSE lw_indent NO-GAPS.
      CONCATENATE ' w:left="'
                  lw_indent
                  '"'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.

    IF NOT style_effect-rightindent IS INITIAL AND style_effect-rightindent CO '0123456789 '.
      lw_indent = style_effect-rightindent.
      CONDENSE lw_indent NO-GAPS.
      CONCATENATE lw_substyle
                  ' w:right="'
                  lw_indent
                  '"'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.

    IF NOT style_effect-firstindent IS INITIAL AND style_effect-firstindent CO '-0123456789 '.
      IF style_effect-firstindent < 0.
        lw_indent = - style_effect-firstindent.
        CONDENSE lw_indent NO-GAPS.
        CONCATENATE lw_substyle
                    ' w:hanging="'
                    lw_indent
                    '"'
                    INTO lw_substyle RESPECTING BLANKS.
      ELSE.
        lw_indent = style_effect-firstindent.
        CONDENSE lw_indent NO-GAPS.
        CONCATENATE lw_substyle
                    ' w:firstLine="'
                    lw_indent
                    '"'
                    INTO lw_substyle RESPECTING BLANKS.
      ENDIF.
    ENDIF.

    IF NOT lw_substyle IS INITIAL.
      CONCATENATE xml
                  '<w:ind '
                  lw_substyle
                  '/>'
                  INTO xml RESPECTING BLANKS.
    ENDIF.

* Borders
    CLEAR lw_substyle.
    IF     NOT style_effect-border_left-style IS INITIAL
       AND NOT style_effect-border_left-width IS INITIAL.
      lw_size = style_effect-border_left-width.
      CONDENSE lw_size NO-GAPS.
      lw_space = style_effect-border_left-space.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  '<w:left w:val="'
                  style_effect-border_left-style
                  '" w:sz="'
                  lw_size
                  '" w:space="'
                  lw_space
                  '" w:color="'
                  style_effect-border_left-color
                  '"/>'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.
    IF     NOT style_effect-border_top-style IS INITIAL
       AND NOT style_effect-border_top-width IS INITIAL.
      lw_size = style_effect-border_top-width.
      CONDENSE lw_size NO-GAPS.
      lw_space = style_effect-border_top-space.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  '<w:top w:val="'
                  style_effect-border_top-style
                  '" w:sz="'
                  lw_size
                  '" w:space="'
                  lw_space
                  '" w:color="'
                  style_effect-border_top-color
                  '"/>'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.
    IF     NOT style_effect-border_right-style IS INITIAL
       AND NOT style_effect-border_right-width IS INITIAL.
      lw_size = style_effect-border_right-width.
      CONDENSE lw_size NO-GAPS.
      lw_space = style_effect-border_right-space.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  '<w:right w:val="'
                  style_effect-border_right-style
                  '" w:sz="'
                  lw_size
                  '" w:space="'
                  lw_space
                  '" w:color="'
                  style_effect-border_right-color
                  '"/>'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.
    IF     NOT style_effect-border_bottom-style IS INITIAL
       AND NOT style_effect-border_bottom-width IS INITIAL.
      lw_size = style_effect-border_bottom-width.
      CONDENSE lw_size NO-GAPS.
      lw_space = style_effect-border_bottom-space.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  '<w:bottom w:val="'
                  style_effect-border_bottom-style
                  '" w:sz="'
                  lw_size
                  '" w:space="'
                  lw_space
                  '" w:color="'
                  style_effect-border_bottom-color
                  '"/>'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.
    IF NOT lw_substyle IS INITIAL.
      CONCATENATE xml
                  '<w:pBdr>'
                  lw_substyle
                  '</w:pBdr>'
                  INTO xml RESPECTING BLANKS.
    ENDIF.
* Add section info if required
    IF NOT mw_section_xml IS INITIAL.
      CONCATENATE xml
                  mw_section_xml
                  INTO xml.
      CLEAR mw_section_xml.
    ENDIF.

    IF NOT xml IS INITIAL.
      CONCATENATE '<w:pPr>'
                  xml
                  '</w:pPr>'
                  INTO xml.
    ENDIF.
  ENDMETHOD.
  METHOD _create_note.
    DATA : lw_filename   TYPE string,
           lw_file       TYPE string,
           lw_string     TYPE string,
           lw_id         TYPE string,
           lw_xmlns      TYPE string,
           lw_text       TYPE string,
           lw_link_style TYPE string,
           lw_line_style TYPE string.

* Search if notes file exist
    IF type = c_notetype_foot.
      lw_filename = 'word/footnotes.xml'.
    ELSEIF type = c_notetype_end.
      lw_filename = 'word/endnotes.xml'.
    ELSE.
      RETURN.
    ENDIF.

    IF line_exists( mo_zip->files[ name = lw_filename ] ).
* If foot/end notes exists, load the file
      _get_zip_file( EXPORTING filename = lw_filename
                     IMPORTING content  = lw_file ).
    ELSE.
* If footnotes doesn't exist, declare it and create it
* Add footnotes in content_types
      _get_zip_file( EXPORTING filename = '[Content_Types].xml'
                     IMPORTING content  = lw_file ).

      IF type = c_notetype_foot.
        CONCATENATE '<Override'
                    ' ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml"'
                    ' PartName="/word/footnotes.xml"/></Types>'
                    INTO lw_string RESPECTING BLANKS.
      ELSEIF type = c_notetype_end.
        CONCATENATE '<Override'
                    ' ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml"'
                    ' PartName="/word/endnotes.xml"/></Types>'
                    INTO lw_string RESPECTING BLANKS.
      ENDIF.
      REPLACE '</Types>' WITH lw_string
              INTO lw_file.

      _update_zip_file( filename = '[Content_Types].xml'
                        content  = lw_file ).

* Add footnotes in relation file
      _get_zip_file( EXPORTING filename = 'word/_rels/document.xml.rels'
                     IMPORTING content  = lw_file ).

* Create footnotes relation ID
      DO.
        lw_id = sy-index.
        CONDENSE lw_id NO-GAPS.
        CONCATENATE 'rId' lw_id INTO lw_id ##NO_TEXT.
        CONCATENATE 'Id="' lw_id '"' INTO lw_string ##NO_TEXT.
        FIND FIRST OCCURRENCE OF lw_string IN lw_file IGNORING CASE.
        IF sy-subrc <> 0.
          EXIT. " exit do
        ENDIF.
      ENDDO.

* Add relation
      IF type = c_notetype_foot.
        CONCATENATE '<Relationship Target="footnotes.xml"'
                    ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes"'
                    ' Id="'
                    lw_id
                    '"/>'
                    '</Relationships>'
                    INTO lw_string RESPECTING BLANKS.
      ELSEIF type = c_notetype_end.
        CONCATENATE '<Relationship Target="endnotes.xml"'
                    ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes"'
                    ' Id="'
                    lw_id
                    '"/>'
                    '</Relationships>'
                    INTO lw_string RESPECTING BLANKS.
      ENDIF.
      REPLACE '</Relationships>' WITH lw_string INTO lw_file.

* Update relation file
      _update_zip_file( filename = 'word/_rels/document.xml.rels'
                        content  = lw_file ).

      _get_xml_ns( IMPORTING xml = lw_xmlns ).

* Create notes file
      IF type = c_notetype_foot.
        CONCATENATE '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                    '<w:footnotes '
                    lw_xmlns
                    '>'
                    '<w:footnote w:id="-1" w:type="separator">'
                    '<w:p>'
                    '<w:pPr><w:spacing w:lineRule="auto" w:line="240" w:after="0"/></w:pPr>'
                    '<w:r><w:separator/></w:r>'
                    '</w:p>'
                    '</w:footnote>'
                    '<w:footnote w:id="0" w:type="continuationSeparator">'
                    '<w:p>'
                    '<w:pPr><w:spacing w:lineRule="auto" w:line="240" w:after="0"/></w:pPr>'
                    '<w:r><w:continuationSeparator/></w:r>'
                    '</w:p>'
                    '</w:footnote>'
                    '</w:footnotes>'
                    INTO lw_file RESPECTING BLANKS.
      ELSEIF type = c_notetype_end.
        CONCATENATE '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                    '<w:endnotes '
                    lw_xmlns
                    '>'
                    '<w:endnote w:id="-1" w:type="separator">'
                    '<w:p>'
                    '<w:pPr><w:spacing w:lineRule="auto" w:line="240" w:after="0"/></w:pPr>'
                    '<w:r><w:separator/></w:r>'
                    '</w:p>'
                    '</w:endnote>'
                    '<w:endnote w:id="0" w:type="continuationSeparator">'
                    '<w:p>'
                    '<w:pPr><w:spacing w:lineRule="auto" w:line="240" w:after="0"/></w:pPr>'
                    '<w:r><w:continuationSeparator/></w:r>'
                    '</w:p>'
                    '</w:endnote>'
                    '</w:endnotes>'
                    INTO lw_file RESPECTING BLANKS.
      ENDIF.
    ENDIF.

* Search available note id
    DO.
      id = sy-index.
      CONDENSE id NO-GAPS.
      CONCATENATE 'w:id="' id '"' INTO lw_string ##NO_TEXT.
      FIND FIRST OCCURRENCE OF lw_string IN lw_file IGNORING CASE.
      IF sy-subrc <> 0.
        EXIT. " exit do
      ENDIF.
    ENDDO.

* Add blank at start of note
    lw_text = text.
    IF lw_text IS INITIAL OR lw_text(1) <> space.
      CONCATENATE space lw_text INTO lw_text RESPECTING BLANKS.
    ENDIF.

    write_text( EXPORTING textline      = lw_text
                          style_effect  = style_effect
                          style         = style
                IMPORTING virtual       = lw_text
                          invalid_style = invalid_style ).

    IF NOT link_style_effect IS INITIAL OR NOT link_style IS INITIAL.
      _build_character_style( EXPORTING style        = link_style
                                        style_effect = link_style_effect
                              IMPORTING xml          = lw_link_style ).
    ENDIF.

    IF NOT line_style_effect IS INITIAL OR NOT line_style IS INITIAL.
      _build_paragraph_style( EXPORTING style         = line_style
                                        style_effect  = line_style_effect
                              IMPORTING xml           = lw_line_style
                                        invalid_style = invalid_line_style ).
    ENDIF.

* Add note
    IF type = c_notetype_foot.
      CONCATENATE '<w:footnote w:id="'
                  id
                  '">'
                  '<w:p>'
                  lw_line_style
                  '<w:r>'
                  lw_link_style
                  '<w:footnoteRef/>'
                  '</w:r>'
                  lw_text
                  '</w:p>'
                  '</w:footnote>'
                  '</w:footnotes>'
                  INTO lw_string RESPECTING BLANKS.
      REPLACE FIRST OCCURRENCE OF '</w:footnotes>' IN lw_file WITH lw_string.
    ELSEIF type = c_notetype_end.
      CONCATENATE '<w:endnote w:id="'
                  id
                  '">'
                  '<w:p>'
                  lw_line_style
                  '<w:r>'
                  lw_link_style
                  '<w:endnoteRef/>'
                  '</w:r>'
                  lw_text
                  '</w:p>'
                  '</w:endnote>'
                  '</w:endnotes>'
                  INTO lw_string RESPECTING BLANKS.
      REPLACE FIRST OCCURRENCE OF '</w:endnotes>' IN lw_file WITH lw_string.
    ENDIF.

* Update footnotes file
    _update_zip_file( filename = lw_filename
                      content  = lw_file ).
  ENDMETHOD.                    "_create_footnote
  METHOD _get_xml_ns.
    CLEAR xml.
    CONCATENATE
                ' xmlns:wpc="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"'
                ' xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"'
                ' xmlns:o="urn:schemas-microsoft-com:office:office"'
                ' xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"'
                ' xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"'
                ' xmlns:v="urn:schemas-microsoft-com:vml"'
                ' xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"'
                ' xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"'
                ' xmlns:w10="urn:schemas-microsoft-com:office:word"'
                ' xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"'
                ' xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"'
                ' xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"'
                ' xmlns:wpi="http://schemas.microsoft.com/office/word/2010/wordprocessingInk"'
                ' xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"'
                ' xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape"'
                ' mc:Ignorable="w14 wp14" ' ##NO_TEXT
                INTO xml RESPECTING BLANKS.
  ENDMETHOD.
  METHOD _get_zip_file.
    DATA : lw_xmlx        TYPE xstring,
           lw_xmlx_length TYPE i,
           lt_xmlx        TYPE STANDARD TABLE OF x255.

* Get zipped file
    mo_zip->get( EXPORTING  name    = filename
                 IMPORTING  content = lw_xmlx
                 EXCEPTIONS OTHERS  = 2 ).

* Transform xstring to string in 2 steps
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING buffer        = lw_xmlx
      IMPORTING output_length = lw_xmlx_length
      TABLES    binary_tab    = lt_xmlx.

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING input_length = lw_xmlx_length
      IMPORTING text_buffer  = content
      TABLES    binary_tab   = lt_xmlx.
  ENDMETHOD.
  METHOD _load_file.
    DATA : lw_url_begin TYPE c LENGTH 6,
           lw_length    TYPE i,
           lt_data_tab  TYPE STANDARD TABLE OF x255.
    DATA : ls_query  TYPE w3query,
           lt_query  TYPE TABLE OF w3query,
           lt_html   TYPE TABLE OF w3html,
           lt_mime   TYPE TABLE OF w3mime,
           lw_return TYPE w3param-ret_code,
           lw_type   TYPE w3param-cont_type.

    CLEAR xcontent.
    lw_url_begin = filename.

* Load image
    IF lw_url_begin = c_sapwr_prefix.
* For SAPWR, read file from DB
      ls_query-name  = '_OBJECT_ID'.
      ls_query-value = filename+6.
      APPEND ls_query TO lt_query.
      CALL FUNCTION 'WWW_GET_MIME_OBJECT'
        TABLES     query_string   = lt_query
                   html           = lt_html
                   mime           = lt_mime
        CHANGING   return_code    = lw_return
                   content_type   = lw_type
                   content_length = lw_length
        EXCEPTIONS OTHERS         = 3.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

* Convert xchar table to xstring
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING  input_length = lw_length
        IMPORTING  buffer       = xcontent
        TABLES     binary_tab   = lt_mime
        EXCEPTIONS OTHERS       = 2.
    ELSE.
* For others, read file
      cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = filename
                                                       filetype   = 'BIN'
                                            IMPORTING  filelength = lw_length
                                            CHANGING   data_tab   = lt_data_tab
                                            EXCEPTIONS OTHERS     = 19 ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING  input_length = lw_length
        IMPORTING  buffer       = xcontent
        TABLES     binary_tab   = lt_data_tab
        EXCEPTIONS OTHERS       = 2.
    ENDIF.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.
  METHOD _load_image.
    DATA : ls_list_object LIKE LINE OF mt_list_object,
           lw_filex       TYPE xstring,
           lw_url_begin   TYPE c LENGTH 6,
           lw_filename    TYPE string,
           lw_file        TYPE string,
           lw_string      TYPE string.

    CLEAR : extension,
            imgres_x,
            imgres_y.

* For existing image (ID given), just get img_res and extension.
    IF NOT id IS INITIAL.
      READ TABLE mt_list_object WITH KEY type = c_type_image
                                         id   = id
           INTO ls_list_object.
      IF sy-subrc <> 0.
        CLEAR id.
        RETURN.
      ENDIF.
      IF extension IS SUPPLIED.
        FIND ALL OCCURRENCES OF '.' IN ls_list_object-path
             MATCH OFFSET sy-fdpos.
        IF sy-subrc = 0.
          sy-fdpos = sy-fdpos + 1.
          extension = ls_list_object-path+sy-fdpos.
          extension = to_lower( extension ).
          IF extension = 'jpeg'.
            extension = 'jpg'.
          ENDIF.
        ENDIF.
      ENDIF.
      IF imgres_x IS SUPPLIED OR imgres_y IS SUPPLIED.
        mo_zip->get( EXPORTING  name    = ls_list_object-path
                     IMPORTING  content = lw_filex
                     EXCEPTIONS OTHERS  = 2 ).
        IF sy-subrc = 0.
          cl_fxs_image_info=>determine_info( EXPORTING iv_data = lw_filex
                                             IMPORTING ev_xres = imgres_x
                                                       ev_yres = imgres_y ).
        ENDIF.
      ENDIF.
      RETURN.
    ENDIF.

* For new image, load image
    lw_url_begin = url.
    lw_url_begin = to_upper( lw_url_begin ).
* For image from sapwr, get file extension from DB
    IF lw_url_begin = c_sapwr_prefix.
      SELECT SINGLE value INTO extension
        FROM wwwparams
        WHERE relid = 'MI'
          AND objid = url+6
          AND name  = 'fileextension'.
      IF NOT extension IS INITIAL AND extension(1) = '.'.
        extension = extension+1.
      ENDIF.
    ELSE.
* For other image, get file extension from filename
      FIND ALL OCCURRENCES OF '.' IN url MATCH OFFSET sy-fdpos.
      IF sy-subrc = 0.
        sy-fdpos = sy-fdpos + 1.
        extension = url+sy-fdpos.
        extension = to_lower( extension ).
        IF extension = 'jpeg'.
          extension = 'jpg'.
        ENDIF.
      ENDIF.
    ENDIF.
* Cannot add image other than jpg/png/gif
    IF     extension <> 'jpg'
       AND extension <> 'png'
       AND extension <> 'gif'.
      RETURN.
    ENDIF.

* Load image
    IF file_content IS INITIAL.
      _load_file( EXPORTING filename = url
                  IMPORTING xcontent = lw_filex ).
      IF lw_filex IS INITIAL.
        RETURN.
      ENDIF.
    ELSE.
      lw_filex = file_content.
    ENDIF.

* Get image resolution
    IF imgres_x IS SUPPLIED OR imgres_y IS SUPPLIED.
      cl_fxs_image_info=>determine_info( EXPORTING iv_data = lw_filex
                                         IMPORTING ev_xres = imgres_x
                                                   ev_yres = imgres_y ).
    ENDIF.

* Search available image name
    DO.
      lw_filename = sy-index.
      CONDENSE lw_filename NO-GAPS.
      CONCATENATE 'word/media/image' lw_filename '.' extension
                  INTO lw_filename.
      IF NOT line_exists( mo_zip->files[ name = lw_filename ] ).
        EXIT. " exit do
      ENDIF.
    ENDDO.

* Add image in ZIP
    mo_zip->add( name    = lw_filename
                 content = lw_filex ).

* Get file extension list
    _get_zip_file( EXPORTING filename = '[Content_Types].xml'
                   IMPORTING content  = lw_file ).

* Search if file extension exist
    CONCATENATE 'extension="' extension '"' INTO lw_string.
    FIND FIRST OCCURRENCE OF lw_string IN lw_file IGNORING CASE.
    IF sy-subrc <> 0.
* If extension is not yet declared, it's time !
      CASE extension.
        WHEN 'jpg'.
          REPLACE '</Types>' WITH '<Default ContentType="image/jpeg" Extension="jpg"/></Types>'
                  INTO lw_file.
        WHEN 'png'.
          REPLACE '</Types>' WITH '<Default ContentType="image/png" Extension="png"/></Types>'
                  INTO lw_file.
        WHEN 'gif'.
          REPLACE '</Types>' WITH '<Default ContentType="image/gif" Extension="gif"/></Types>'
                  INTO lw_file.
      ENDCASE.

* Update file extension list
      _update_zip_file( filename = '[Content_Types].xml'
                        content  = lw_file ).
    ENDIF.

* Get relation file
    _get_zip_file( EXPORTING filename = 'word/_rels/document.xml.rels'
                   IMPORTING content  = lw_file ).

* Create Image ID
    DO.
      id = sy-index.
      CONDENSE id NO-GAPS.
      CONCATENATE 'rId' id INTO id ##NO_TEXT.
      CONCATENATE 'Id="' id '"' INTO lw_string ##NO_TEXT.
      FIND FIRST OCCURRENCE OF lw_string IN lw_file IGNORING CASE.
      IF sy-subrc <> 0.
        EXIT. " exit do
      ENDIF.
    ENDDO.

* Update object list
    CLEAR ls_list_object.
    ls_list_object-id   = id.
    ls_list_object-type = c_type_image.
    ls_list_object-path = lw_filename.
    APPEND ls_list_object TO mt_list_object.

* Add relation
    lw_filename = lw_filename+5.
    CONCATENATE '<Relationship Target="'
                lw_filename
                '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Id="'
                id
                '"/>'
                '</Relationships>'
                INTO lw_string.
    REPLACE '</Relationships>' WITH lw_string INTO lw_file.

* Update relation file
    _update_zip_file( filename = 'word/_rels/document.xml.rels'
                      content  = lw_file ).
  ENDMETHOD.
  METHOD _protect_label.
    out = in.
    out = translate( val  = out
                     from = ` `
                     to   = `_` ).
  ENDMETHOD.
  METHOD _protect_string.
    out = in.
    REPLACE ALL OCCURRENCES OF '&' IN out WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN out WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN out WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"' IN out WITH '&quot;'.
  ENDMETHOD.
  METHOD _update_zip_file.
    DATA lw_docx TYPE xstring.

* File content string => xstring
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING text   = content
      IMPORTING buffer = lw_docx.

* If target file already exist, remove it
    IF line_exists( mo_zip->files[ name = filename ] ).
      mo_zip->delete( name = filename ).
    ENDIF.

* Add modified file into zip
    mo_zip->add( name    = filename
                 content = lw_docx ).
  ENDMETHOD.
  METHOD _write_section.
    DATA : lw_substyle TYPE string,
           lw_size     TYPE string,
           lw_space    TYPE string.

    CLEAR mw_section_xml.

* In case of keep template content, first document section is the last
* template section
    IF NOT mw_tpl_section_xml IS INITIAL.
      mw_section_xml = mw_tpl_section_xml.
      CLEAR mw_tpl_section_xml.
      CLEAR ms_section.
      RETURN.
    ENDIF.

* Define header/footer
    IF NOT ms_section-header IS INITIAL.
      CONCATENATE mw_section_xml
                  '<w:headerReference w:type="default" r:id="'
                  ms_section-header
                  '"/>'
                  INTO mw_section_xml.
    ENDIF.
    IF NOT ms_section-footer IS INITIAL.
      CONCATENATE mw_section_xml
                  '<w:footerReference w:type="default" r:id="'
                  ms_section-footer
                  '"/>'
                  INTO mw_section_xml.
    ENDIF.
    IF NOT ms_section-header_first IS INITIAL.
      CONCATENATE mw_section_xml
                  '<w:headerReference w:type="first" r:id="'
                  ms_section-header_first
                  '"/>'
                  INTO mw_section_xml.
    ENDIF.
    IF NOT ms_section-footer_first IS INITIAL.
      CONCATENATE mw_section_xml
                  '<w:footerReference w:type="first" r:id="'
                  ms_section-footer_first
                  '"/>'
                  INTO mw_section_xml.
    ENDIF.

* Define page orientation
    IF ms_section-landscape = c_true.
      CONCATENATE mw_section_xml
                  '<w:pgSz w:w="16838" w:h="11906" w:orient="landscape"/>'
                  INTO mw_section_xml.
    ELSE.
      CONCATENATE mw_section_xml
                  '<w:pgSz w:w="11906" w:h="16838"/>'
                  INTO mw_section_xml.
    ENDIF.

* Border ?
    CLEAR lw_substyle.
    IF     NOT ms_section-border_left-style IS INITIAL
       AND NOT ms_section-border_left-width IS INITIAL.
      lw_size = ms_section-border_left-width.
      CONDENSE lw_size NO-GAPS.
      lw_space = ms_section-border_left-space.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  '<w:left w:val="'
                  ms_section-border_left-style
                  '" w:sz="'
                  lw_size
                  '" w:space="'
                  lw_space
                  '" w:color="'
                  ms_section-border_left-color
                  '"/>'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.
    IF     NOT ms_section-border_top-style IS INITIAL
       AND NOT ms_section-border_top-width IS INITIAL.
      lw_size = ms_section-border_top-width.
      CONDENSE lw_size NO-GAPS.
      lw_space = ms_section-border_top-space.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  '<w:top w:val="'
                  ms_section-border_top-style
                  '" w:sz="'
                  lw_size
                  '" w:space="'
                  lw_space
                  '" w:color="'
                  ms_section-border_top-color
                  '"/>'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.
    IF     NOT ms_section-border_right-style IS INITIAL
       AND NOT ms_section-border_right-width IS INITIAL.
      lw_size = ms_section-border_right-width.
      CONDENSE lw_size NO-GAPS.
      lw_space = ms_section-border_right-space.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  '<w:right w:val="'
                  ms_section-border_right-style
                  '" w:sz="'
                  lw_size
                  '" w:space="'
                  lw_space
                  '" w:color="'
                  ms_section-border_right-color
                  '"/>'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.
    IF     NOT ms_section-border_bottom-style IS INITIAL
       AND NOT ms_section-border_bottom-width IS INITIAL.
      lw_size = ms_section-border_bottom-width.
      CONDENSE lw_size NO-GAPS.
      lw_space = ms_section-border_bottom-space.
      CONDENSE lw_space NO-GAPS.
      CONCATENATE lw_substyle
                  '<w:bottom w:val="'
                  ms_section-border_bottom-style
                  '" w:sz="'
                  lw_size
                  '" w:space="'
                  lw_space
                  '" w:color="'
                  ms_section-border_bottom-color
                  '"/>'
                  INTO lw_substyle RESPECTING BLANKS.
    ENDIF.
    IF NOT lw_substyle IS INITIAL.
      CONCATENATE mw_section_xml
                  '<w:pgBorders w:offsetFrom="page">'
                  lw_substyle
                  '</w:pgBorders>'
                  INTO mw_section_xml RESPECTING BLANKS.
    ENDIF.

* Default section values / Standard page
    CONCATENATE mw_section_xml
                '<w:cols w:space="708"/>'
                '<w:docGrid w:linePitch="360"/>'
                '<w:pgMar w:top="1417" w:right="1417" w:bottom="1417" w:left="1417" w:header="708" w:footer="708" w:gutter="0"/>'
                INTO mw_section_xml.

    IF ms_section-continuous = c_true.
      CONCATENATE mw_section_xml
                  '<w:type w:val="continuous"/>'
                  INTO mw_section_xml.
    ENDIF.

    IF    NOT ms_section-header_first IS INITIAL
       OR NOT ms_section-footer_first IS INITIAL.
      CONCATENATE mw_section_xml
                  '<w:titlePg/>'
                  INTO mw_section_xml.
    ENDIF.

    CONCATENATE '<w:sectPr>'
                mw_section_xml
                '</w:sectPr>'
                INTO mw_section_xml.

    CLEAR ms_section.
  ENDMETHOD.                    " write_section
  METHOD borrar_marcador.
    CLEAR message.
    DELETE i_marcadores WHERE name = marcador.
    IF sy-subrc <> 0.
      message = |No existe el marcador { marcador }|.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    DATA : lt_find_result TYPE match_result_tab,
           lw_docx        TYPE xstring,
           lw_file        TYPE string,
           lw_url_begin   TYPE c LENGTH 6,
           lw_extension   TYPE string,
           ls_find_result LIKE LINE OF lt_find_result,
           ls_list_style  TYPE ty_list_style,
           lw_string      TYPE string,
           ls_list_object TYPE ty_list_object.

    mo_zip = NEW #( ).

    IF content IS SUPPLIED AND NOT content IS INITIAL.
      lw_docx = content.
    ELSEIF tpl IS SUPPLIED AND NOT tpl IS INITIAL.
* Load template document
      _load_file( EXPORTING filename = tpl
                  IMPORTING xcontent = lw_docx ).
      IF lw_docx IS INITIAL.
        MESSAGE e398(00) WITH 'No se puede abrir plantilla' tpl.
      ENDIF.
    ELSEIF plantilla IS SUPPLIED AND NOT plantilla IS INITIAL AND NOT tcode IS INITIAL.
      SELECT SINGLE xstring FROM zdocumentos
        INTO lw_docx
        WHERE tcode  = tcode
          AND nombre = plantilla.
    ELSE.
* Empty docx creation
      TRY.
          lw_docx = cl_docx_form=>create_form( ).
        CATCH cx_openxml_not_allowed
              cx_openxml_not_found
              cx_openxml_format
              cx_docx_form_not_unicode.
          MESSAGE 'Cannot create empty doc, please use template' TYPE 'A'.
      ENDTRY.
    ENDIF.

* Load docx into zip object
    mo_zip->load( EXPORTING  zip             = lw_docx
                  EXCEPTIONS zip_parse_error = 1
                             OTHERS          = 2 ).

* Keep actual content
    IF keep_tpl_content = c_true.
      _get_zip_file( EXPORTING filename = 'word/document.xml'                    ##NO_TEXT
                     IMPORTING content  = lw_file ).

      FIND FIRST OCCURRENCE OF '<w:body' IN lw_file
           MATCH OFFSET sy-fdpos IGNORING CASE.
      IF sy-subrc = 0.
        lw_file = lw_file+sy-fdpos.
        FIND FIRST OCCURRENCE OF '>' IN lw_file
             MATCH OFFSET sy-fdpos IGNORING CASE.
      ENDIF.
      IF sy-subrc = 0.
        sy-fdpos = sy-fdpos + 1.
        lw_file = lw_file+sy-fdpos.
        FIND FIRST OCCURRENCE OF '</w:body' IN lw_file
             MATCH OFFSET sy-fdpos IGNORING CASE.
      ENDIF.
      IF sy-subrc = 0.
        mw_docxml = lw_file(sy-fdpos).
        mw_doc_inicial = mw_docxml.
        FIND ALL OCCURRENCES OF '<w:sectPr' IN mw_docxml
             MATCH OFFSET sy-fdpos IGNORING CASE.
        IF sy-subrc = 0.
          mw_tpl_section_xml = mw_docxml+sy-fdpos.
          mw_docxml = mw_docxml(sy-fdpos).
        ENDIF.
      ELSE.
        MESSAGE 'Cannot parse template content: empty document created'
                TYPE 'I'.
      ENDIF.
    ENDIF.

* Remove docx body
    mo_zip->delete( EXPORTING  name            = 'word/document.xml'               ##NO_TEXT
                    EXCEPTIONS zip_index_error = 1
                               OTHERS          = 2 ).

* If modele is a template, transform it to document
    IF tpl IS SUPPLIED AND NOT tpl IS INITIAL.
      lw_url_begin = tpl.
      IF lw_url_begin = c_sapwr_prefix.
        SELECT SINGLE value INTO lw_extension
          FROM wwwparams
          WHERE relid = 'MI'
            AND objid = tpl+6
            AND name  = 'fileextension'.
        IF NOT lw_extension IS INITIAL AND lw_extension(1) = '.'.
          lw_extension = lw_extension+1.
        ENDIF.
      ELSE.
        FIND ALL OCCURRENCES OF '.' IN tpl MATCH OFFSET sy-fdpos.
        IF sy-subrc = 0.
          sy-fdpos = sy-fdpos + 1.
          lw_extension = tpl+sy-fdpos.
        ELSE.
          lw_extension = tpl.
        ENDIF.
      ENDIF.
      lw_extension = to_lower( lw_extension ).

* Template without macro
      IF lw_extension = 'dotx'.
        _get_zip_file( EXPORTING filename = '[Content_Types].xml'                ##NO_TEXT
                       IMPORTING content  = lw_file ).

        REPLACE ALL OCCURRENCES OF 'wordprocessingml.template'
                IN lw_file WITH 'wordprocessingml.document' ##NO_TEXT.

        _update_zip_file( filename = '[Content_Types].xml'
                          content  = lw_file ).
* Template with macro
      ELSEIF lw_extension = 'dotm'.
        _get_zip_file( EXPORTING filename = '[Content_Types].xml'                ##NO_TEXT
                       IMPORTING content  = lw_file ).

        REPLACE ALL OCCURRENCES OF 'template.macroEnabledTemplate'
                IN lw_file WITH 'document.macroEnabled' ##NO_TEXT.

        _update_zip_file( filename = '[Content_Types].xml'
                          content  = lw_file ).
      ENDIF.
    ENDIF.

* Get author name
    CLEAR mw_author.
    SELECT SINGLE name_textc INTO mw_author
      FROM user_addr
      WHERE bname = sy-uname ##WARN_OK.
    IF sy-subrc <> 0.
      mw_author = sy-uname.
    ENDIF.

* Set Author, Creation date and Version number properties
    set_properties( author       = mw_author
                    creationdate = sy-datlo
                    creationtime = sy-timlo
                    revision     = 1 ).

* Get style file
    _get_zip_file( EXPORTING filename = 'word/styles.xml'                        ##NO_TEXT
                   IMPORTING content  = lw_file ).

* Scan style file to search all styles
    FIND ALL OCCURRENCES OF REGEX '<w:style ([^>]*)>'
         IN lw_file RESULTS lt_find_result
         IGNORING CASE.
    LOOP AT lt_find_result INTO ls_find_result.
      CLEAR ls_list_style.
      FIND FIRST OCCURRENCE OF REGEX 'w:styleId="([^"]*)"'
           IN SECTION OFFSET ls_find_result-offset
           LENGTH ls_find_result-length
           OF lw_file
           SUBMATCHES lw_string
           IGNORING CASE.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ls_list_style-name = lw_string.
      FIND FIRST OCCURRENCE OF REGEX 'w:type="(paragraph|character|numbering|table)"'
           IN SECTION OFFSET ls_find_result-offset
           LENGTH ls_find_result-length
           OF lw_file
           SUBMATCHES lw_string
           IGNORING CASE.
      IF sy-subrc = 0.
        ls_list_style-type = lw_string.
      ENDIF.
      APPEND ls_list_style TO mt_list_style.
    ENDLOOP.
    SORT mt_list_style BY type
                          name.

* Get relation file
    _get_zip_file( EXPORTING filename = 'word/_rels/document.xml.rels'           ##NO_TEXT
                   IMPORTING content  = lw_file ).

* Scan relation file to get all objects
    FIND ALL OCCURRENCES OF REGEX '<Relationship ([^>]*)/>'
         IN lw_file RESULTS lt_find_result
         IGNORING CASE.
    LOOP AT lt_find_result INTO ls_find_result.
      CLEAR ls_list_object.
* Search id of object
      FIND FIRST OCCURRENCE OF REGEX 'Id="([^"]*)"'
           IN SECTION OFFSET ls_find_result-offset
           LENGTH ls_find_result-length
           OF lw_file
           SUBMATCHES lw_string
           IGNORING CASE.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ls_list_object-id = lw_string.
* Search type of object
      FIND FIRST OCCURRENCE OF REGEX 'Type=".*(footer|header|image)"'
           IN SECTION OFFSET ls_find_result-offset
           LENGTH ls_find_result-length
           OF lw_file
           SUBMATCHES lw_string
           IGNORING CASE.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ls_list_object-type = lw_string.

* Search path of file
      FIND FIRST OCCURRENCE OF REGEX 'Target="([^"]*)"'
           IN SECTION OFFSET ls_find_result-offset
           LENGTH ls_find_result-length
           OF lw_file
           SUBMATCHES lw_string
           IGNORING CASE.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      CONCATENATE 'word/' lw_string INTO ls_list_object-path.

      APPEND ls_list_object TO mt_list_object.
    ENDLOOP.
    SORT mt_list_object BY type
                           id.

    IF get_marcadores = 'X'.
      get_marcadores( IMPORTING i_marcadores = i_marcadores ).
    ENDIF.
  ENDMETHOD.
  METHOD create_character_style.
    DATA : lw_style      TYPE string,
           lw_string     TYPE string,
           lw_file       TYPE string,
           ls_list_style LIKE LINE OF mt_list_style.

* Build simple style internal name
    name = output_name.
    REPLACE ALL OCCURRENCES OF REGEX '[^A-Za-z0-9]' IN name WITH space.
    CONDENSE name NO-GAPS.

* Check style internal name is available
    IF line_exists( mt_list_style[ type = c_type_character
                                   name = name ] ).
      CLEAR name.
      RETURN.
    ENDIF.

* Build character style
    _build_character_style( EXPORTING style_effect = style_effect
                            IMPORTING xml          = lw_style ).

    IF style_ref IS SUPPLIED AND NOT style_ref IS INITIAL.
      IF line_exists( mt_list_style[ type = c_type_character
                                     name = style_ref ] ).
        CONCATENATE '<w:basedOn w:val="'
                    style_ref
                    '"/>'
                    INTO lw_string.
      ELSE.
        invalid_style = c_true.
      ENDIF.
    ENDIF.
    CONCATENATE '<w:style w:type="'
                c_type_character
                '" w:customStyle="1" w:styleId="'
                name
                '">'
                '<w:name w:val="'
                output_name
                '"/>'
                lw_string
                lw_style
                '</w:style>'
                '</w:styles>'
                INTO lw_style.

* Get styles file
    _get_zip_file( EXPORTING filename = 'word/styles.xml'
                   IMPORTING content  = lw_file ).

* Update style file content
    REPLACE '</w:styles>' IN lw_file WITH lw_style.

* Update zipped style file
    _update_zip_file( filename = 'word/styles.xml'
                      content  = lw_file ).

* Update style list
    CLEAR ls_list_style.
    ls_list_style-type = c_type_character.
    ls_list_style-name = name.
    APPEND ls_list_style TO mt_list_style.
  ENDMETHOD.
  METHOD create_custom_field. " not managed
    DATA : lw_file   TYPE string,
           lw_id     TYPE string,
           lw_string TYPE string.

* If customproperties does not exist, create it
    IF NOT line_exists( mo_zip->files[ name = 'docProps/custom.xml' ] ).

* Declare new file in relations
      _get_zip_file( EXPORTING filename = '_rels/.rels'
                     IMPORTING content  = lw_file ).
* search available id
      DO.
        lw_id = sy-index.
        CONDENSE lw_id NO-GAPS.
        CONCATENATE 'rId' lw_id INTO lw_id ##NO_TEXT.
        CONCATENATE 'Id="' lw_id '"' INTO lw_string ##NO_TEXT.
        FIND lw_string IN lw_file IGNORING CASE.
        IF sy-subrc <> 0.
          EXIT. " exit do
        ENDIF.
      ENDDO.

      CONCATENATE '<Relationship Target="docProps/custom.xml"'
                  ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/custom-properties"'
                  ' Id="'
                  lw_id
                  '"/></Relationships>'
                  INTO lw_string RESPECTING BLANKS.
      REPLACE '</Relationships>' IN lw_file WITH lw_string.
      _update_zip_file( filename = '_rels/.rels'
                        content  = lw_file ).

* Declare new file in content type
      _get_zip_file( EXPORTING filename = '[Content_Types].xml'
                     IMPORTING content  = lw_file ).
      REPLACE '</Types>' IN lw_file
              WITH '<Override ContentType="application/vnd.openxmlformats-officedocument.custom-properties+xml" PartName="/docProps/custom.xml"/></Types>'.
      _update_zip_file( filename = '[Content_Types].xml'
                        content  = lw_file ).

      CONCATENATE '<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>'
                  '<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"'
                  ' xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">'
                  '</Properties>'
                  INTO lw_file RESPECTING BLANKS.
      _update_zip_file( filename = 'docProps/custom.xml'
                        content  = lw_file ).
    ENDIF.

* Open custom properties
    _get_zip_file( EXPORTING filename = 'docProps/custom.xml'
                   IMPORTING content  = lw_file ).

* Search available ID
    DO.
      IF sy-index = 1.
        CONTINUE.
      ENDIF.
      lw_id = sy-index.
      CONDENSE lw_id NO-GAPS.
      CONCATENATE 'pid="' lw_id '"' INTO lw_string.
      FIND lw_string IN lw_file IGNORING CASE.
      IF sy-subrc <> 0.
        EXIT. " exit do
      ENDIF.
    ENDDO.

* Add property
    CONCATENATE '<property fmtid="{D5CDD505-2E9C-101B-9397-08002B2CF9AE}"'
                ' pid="'
                lw_id
                '" name="'
                field
                '">'
                '<vt:lpwstr>'
                value
                '</vt:lpwstr>'
                '</property>'
                '</Properties>'
                INTO lw_string RESPECTING BLANKS.
    REPLACE '</Properties>' IN lw_file WITH lw_string.

* Update custopproperties file
    _update_zip_file( filename = 'docProps/custom.xml'
                      content  = lw_file ).
  ENDMETHOD.
  METHOD create_paragraph_style.
    DATA : lw_style      TYPE string,
           lw_stylepr    TYPE string,
           lw_string     TYPE string,
           lw_file       TYPE string,
           ls_list_style LIKE LINE OF mt_list_style.

* Build simple style internal name
    name = output_name.
    REPLACE ALL OCCURRENCES OF REGEX '[^A-Za-z0-9]' IN name WITH space.
    CONDENSE name NO-GAPS.

* Check style internal name is available
    IF line_exists( mt_list_style[ type = c_type_paragraph
                                   name = name ] ).
      CLEAR name.
      RETURN.
    ENDIF.

* Build character style
    _build_character_style( EXPORTING style_effect = style_effect
                            IMPORTING xml          = lw_style ).

* Build paragraph style
    _build_paragraph_style( EXPORTING style_effect = line_style_effect
                            IMPORTING xml          = lw_stylepr ).

    IF style_ref IS SUPPLIED AND NOT style_ref IS INITIAL.
      IF line_exists( mt_list_style[ type = c_type_paragraph
                                     name = style_ref ] ).
        CONCATENATE '<w:basedOn w:val="'
                    style_ref
                    '"/>'
                    INTO lw_string.
      ELSE.
        invalid_style = c_true.
      ENDIF.
    ENDIF.
    CONCATENATE '<w:style w:type="'
                c_type_paragraph
                '" w:customStyle="1" w:styleId="'
                name
                '">'
                '<w:name w:val="'
                output_name
                '"/>'
                lw_string
                lw_stylepr
                lw_style
                '</w:style>'
                '</w:styles>'
                INTO lw_style.

* Get styles file
    _get_zip_file( EXPORTING filename = 'word/styles.xml'
                   IMPORTING content  = lw_file ).

* Update style file content
    REPLACE '</w:styles>' IN lw_file WITH lw_style.

* Update zipped style file
    _update_zip_file( filename = 'word/styles.xml'
                      content  = lw_file ).

* Update style list
    CLEAR ls_list_style.
    ls_list_style-type = c_type_paragraph.
    ls_list_style-name = name.
    APPEND ls_list_style TO mt_list_style.
  ENDMETHOD.
  METHOD direct_draw.
    DATA : lw_texte  TYPE string,
           lw_string TYPE string,
           lw_style  TYPE string.

* Prepare text content
    IF text_xml IS NOT INITIAL.
      lw_texte = text_xml.
    ELSEIF text IS NOT INITIAL.
      write_text( EXPORTING textline   = text
                            style      = text_style
                            line_style = text_line_style
                  IMPORTING virtual    = lw_texte ).
    ENDIF.
    IF NOT lw_texte IS INITIAL.
      CONCATENATE '<v:textbox>'
                  '<w:txbxContent>'
                  lw_texte
                  '</w:txbxContent>'
                  '</v:textbox>'
                  INTO lw_texte.
    ENDIF.

* Prepare object style
    lw_string = posx.
    CONDENSE lw_string NO-GAPS.
    CONCATENATE ' style="'
                'position:absolute;'
                'margin-left:'
                lw_string
                unit
                ';'
                INTO lw_style RESPECTING BLANKS.

    lw_string = posy.
    CONDENSE lw_string NO-GAPS.
    CONCATENATE lw_style
                'margin-top:'
                lw_string
                unit
                ';'
                INTO lw_style RESPECTING BLANKS.
    lw_string = width.
    CONDENSE lw_string NO-GAPS.
    CONCATENATE lw_style
                'width:'
                lw_string
                unit
                ';'
                INTO lw_style RESPECTING BLANKS.
    lw_string = height.
    CONDENSE lw_string NO-GAPS.
    CONCATENATE lw_style
                'height:'
                lw_string
                unit
                ';'
                INTO lw_style RESPECTING BLANKS.
    lw_string = zindex.
    CONDENSE lw_string NO-GAPS.
    CONCATENATE lw_style
                'z-index:'
                lw_string
                ';"'
                INTO lw_style RESPECTING BLANKS.

    IF bg_color IS INITIAL.
      CONCATENATE lw_style
                  ' filled="f"'
                  INTO lw_style RESPECTING BLANKS.
    ELSE.
      CONCATENATE lw_style
                  ' fillcolor="'
                  bg_color
                  '"'
                  INTO lw_style RESPECTING BLANKS.
    ENDIF.

    IF border_width IS INITIAL.
      CONCATENATE lw_style
                  ' stroked="f"'
                  INTO lw_style RESPECTING BLANKS.
    ELSEIF NOT border_color IS INITIAL.
      CONCATENATE lw_style
                  ' strokecolor="'
                  border_color
                  '"'
                  INTO lw_style RESPECTING BLANKS.
    ENDIF.

    lw_string = border_width.
    CONDENSE lw_string NO-GAPS.
    CONCATENATE lw_style
                ' strokeweight="'
                lw_string
                unit
                '"'
                INTO lw_style RESPECTING BLANKS.

    CONCATENATE mw_fragxml
                '<w:r>'
                '<w:pict>'
                '<v:'
                type
                lw_style
                '>'
                lw_texte
                '</v:'
                type
                '>'
                '</w:pict>'
                '</w:r>'
                INTO mw_fragxml RESPECTING BLANKS.
  ENDMETHOD.
  METHOD draw.
    DATA : lw_width    TYPE i,
           lw_string_x TYPE string,
           lw_string_y TYPE string,
           lw_string_w TYPE string,
           lw_string_h TYPE string,
           lw_style    TYPE string,
           lw_string   TYPE string,
           lw_color    TYPE string.

    lw_width = c_basesize * left.
    lw_string_x = lw_width.
    CONDENSE lw_string_x NO-GAPS.
    lw_width = c_basesize * top.
    lw_string_y = lw_width.
    CONDENSE lw_string_y NO-GAPS.
    lw_width = c_basesize * width.
    lw_string_w = lw_width.
    CONDENSE lw_string_w NO-GAPS.
    lw_width = c_basesize * height.
    lw_string_h = lw_width.
    CONDENSE lw_string_h NO-GAPS.

    CASE object.
      WHEN c_draw_image.
        invalid_image = c_false.
        _load_image( EXPORTING url = url
                     CHANGING  id  = id ).
        IF id IS INITIAL.
          invalid_image = c_true.
          RETURN.
        ENDIF.
        CLEAR lw_style.
        IF bgcolor IS SUPPLIED AND NOT bgcolor IS INITIAL.
          CONCATENATE lw_style
                      '<a:solidFill>'
                      '<a:srgbClr val="'
                      bgcolor
                      '" />'
                      '</a:solidFill>'
                      INTO lw_style.
        ENDIF.

        IF     bdcolor IS SUPPLIED AND NOT bdcolor IS INITIAL
           AND bdwidth IS SUPPLIED AND NOT bdwidth IS INITIAL.
          lw_width = c_basesize * bdwidth.
          lw_string = lw_width.
          CONDENSE lw_string NO-GAPS.

          CONCATENATE lw_style
                      '<a:ln w="'
                      lw_string
                      '">'
                      '<a:solidFill>'
                      '<a:srgbClr val="'
                      bdcolor
                      '" />'
                      '</a:solidFill>'
                      '</a:ln>'
                      INTO lw_style.
        ENDIF.

        CONCATENATE mw_fragxml
                    '<pic:pic xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">'
                    '<pic:nvPicPr>'
                    '<pic:cNvPr name="Image" id="1"/>'
                    '<pic:cNvPicPr>'
                    '<a:picLocks/>'
                    '</pic:cNvPicPr>'
                    '</pic:nvPicPr>'
                    '<pic:blipFill>'
                    '<a:blip r:embed="'
                    id
                    '">'
                    '</a:blip>'
                    '<a:stretch><a:fillRect/></a:stretch>'
                    '</pic:blipFill>'
                    '<pic:spPr>'
                    '<a:xfrm>'
                    '<a:off y="'
                    lw_string_y
                    '" x="'
                    lw_string_x
                    '"/>'
                    '<a:ext cy="'
                    lw_string_h
                    '" cx="'
                    lw_string_w
                    '"/>'
                    '</a:xfrm>'
                    '<a:prstGeom prst="rect"/>'
                    lw_style
                    '</pic:spPr>'
                    '</pic:pic>'
                    INTO mw_fragxml.

      WHEN c_draw_rectangle.
* Default : white rect with 1pt black border
        CLEAR lw_style.
        IF bgcolor IS SUPPLIED AND NOT bgcolor IS INITIAL.
          lw_color = bgcolor.
        ELSE.
          lw_color = c_color_white.
        ENDIF.
        CONCATENATE lw_style
                    '<a:solidFill>'
                    '<a:srgbClr val="'
                    lw_color
                    '" />'
                    '</a:solidFill>'
                    INTO lw_style.

        IF bdcolor IS SUPPLIED AND NOT bdcolor IS INITIAL.
          lw_color = bdcolor.
        ELSE.
          lw_color = c_color_black.
        ENDIF.
        IF bdwidth IS SUPPLIED AND NOT bdwidth IS INITIAL.
          lw_width = c_basesize * bdwidth.
        ELSE.
          lw_width = c_basesize.
        ENDIF.
        lw_string = lw_width.
        CONDENSE lw_string NO-GAPS.

        CONCATENATE lw_style
                    '<a:ln w="'
                    lw_string
                    '">'
                    '<a:solidFill>'
                    '<a:srgbClr val="'
                    lw_color
                    '" />'
                    '</a:solidFill>'
                    '</a:ln>'
                    INTO lw_style.

        CONCATENATE mw_fragxml
                    '<wps:wsp>'
                    '<wps:cNvPr name="Rectangle" id="1"/>'
                    '<wps:cNvSpPr/>'
                    '<wps:spPr>'
                    '<a:xfrm>'
                    '<a:off y="'
                    lw_string_y
                    '" x="'
                    lw_string_x
                    '"/>'
                    '<a:ext cy="'
                    lw_string_h
                    '" cx="'
                    lw_string_w
                    '"/>'
                    '</a:xfrm>'
                    '<a:prstGeom prst="rect"/>'
                    lw_style
                    '</wps:spPr>'
                    '<wps:bodyPr />'
                    '</wps:wsp>'
                    INTO mw_fragxml RESPECTING BLANKS.
    ENDCASE.
  ENDMETHOD.
  METHOD draw_finalize.
    CONCATENATE mw_docxml
                '<w:p>'
                mw_fragxml
                '</wpc:wpc>'
                '</a:graphicData>'
                '</a:graphic>'
                '</wp:anchor>'
                '</w:drawing>'
                '</mc:Choice>'
                '<mc:Fallback>'
                '<w:t>Canvas graphic cannot be loaded (compatible only from Word 2010)</w:t>'
                '</mc:Fallback>'
                '</mc:AlternateContent>'
                '</w:r>'
                '</w:p>'
                INTO mw_docxml.
    CLEAR mw_fragxml.
  ENDMETHOD.
  METHOD draw_init.
    DATA : lw_style    TYPE string,
           lw_width    TYPE i,
           lw_string   TYPE string,
           lw_string_x TYPE string,
           lw_string_y TYPE string,
           lw_string_w TYPE string,
           lw_string_h TYPE string.

    CLEAR mw_fragxml.
    IF bgcolor IS SUPPLIED AND NOT bgcolor IS INITIAL.
      CONCATENATE lw_style
                  '<wpc:bg>'
                  '<a:solidFill>'
                  '<a:srgbClr val="'
                  bgcolor
                  '" />'
                  '</a:solidFill>'
                  '</wpc:bg>'
                  INTO lw_style.
    ENDIF.

    IF     bdcolor IS SUPPLIED AND NOT bdcolor IS INITIAL
       AND bdwidth IS SUPPLIED AND NOT bdwidth IS INITIAL.
      lw_width = c_basesize * bdwidth.
      lw_string = lw_width.
      CONDENSE lw_string NO-GAPS.

      CONCATENATE lw_style
                  '<wpc:whole>'
                  '<a:ln w="'
                  lw_string
                  '">'
                  '<a:solidFill>'
                  '<a:srgbClr val="'
                  bdcolor
                  '" />'
                  '</a:solidFill>'
                  '</a:ln>'
                  '</wpc:whole>'
                  INTO lw_style.
    ENDIF.

    lw_width = c_basesize * left.
    lw_string_x = lw_width.
    CONDENSE lw_string_x NO-GAPS.
    lw_width = c_basesize * top.
    lw_string_y = lw_width.
    CONDENSE lw_string_y NO-GAPS.
    lw_width = c_basesize * width.
    lw_string_w = lw_width.
    CONDENSE lw_string_w NO-GAPS.
    lw_width = c_basesize * height.
    lw_string_h = lw_width.
    CONDENSE lw_string_h NO-GAPS.

    CONCATENATE '<w:r>'
                '<mc:AlternateContent>'
                '<mc:Choice Requires="wpc">'
                '<w:drawing>'
                '<wp:anchor distR="0" distL="0" distB="0" distT="0" allowOverlap="0" layoutInCell="1" locked="0" behindDoc="1" relativeHeight="0" simplePos="0">'
                '<wp:simplePos y="0" x="0"/>'
                '<wp:positionH relativeFrom="column">'
                '<wp:posOffset>'
                lw_string_x
                '</wp:posOffset>'
                '</wp:positionH>'
                '<wp:positionV relativeFrom="paragraph">'
                '<wp:posOffset>'
                lw_string_y
                '</wp:posOffset>'
                '</wp:positionV>'
                '<wp:extent cy="'
                lw_string_h
                '" cx="'
                lw_string_w
                '"/>'
                '<wp:wrapNone/>'
                '<wp:docPr name="Draw container" id="3"/>'
                '<a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">'
                '<a:graphicData uri="http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas">'
                '<wpc:wpc>'
                lw_style
                INTO mw_fragxml RESPECTING BLANKS.
  ENDMETHOD.
  METHOD duplicar_contenido.
    IF salto_pagina = 'X'.
      write_break( breaktype = c_breaktype_page ).
    ENDIF.

    CONCATENATE mw_docxml mw_doc_inicial INTO mw_docxml.
  ENDMETHOD.
  METHOD get_bloque.
    DATA: l_doc TYPE string,
          l_ini TYPE string,
          l_fin TYPE string.

    IF doc IS INITIAL.
      l_doc = mw_docxml.
    ELSE.
      l_doc = doc.
    ENDIF.

    CLEAR string.

    CONCATENATE '<w:' tag '>' INTO l_ini.

    FIND FIRST OCCURRENCE OF l_ini IN l_doc MATCH OFFSET sy-fdpos IGNORING CASE.
    IF sy-subrc = 0.
      string = l_doc+sy-fdpos.
      IF incluir_tag IS INITIAL.
        FIND FIRST OCCURRENCE OF '>' IN string MATCH OFFSET sy-fdpos IGNORING CASE.
      ENDIF.
    ELSE.
      CONCATENATE '<w:' tag space INTO l_ini RESPECTING BLANKS.
      FIND FIRST OCCURRENCE OF l_ini IN l_doc MATCH OFFSET sy-fdpos IGNORING CASE.
      IF sy-subrc = 0.
        string = l_doc+sy-fdpos.
        IF incluir_tag IS INITIAL.
          FIND FIRST OCCURRENCE OF '>' IN string MATCH OFFSET sy-fdpos IGNORING CASE.
        ENDIF.
      ENDIF.
    ENDIF.

    IF string IS INITIAL.
      RETURN.
    ENDIF.

    IF incluir_tag IS INITIAL.
      sy-fdpos = sy-fdpos + 1.
    ENDIF.
    string = string+sy-fdpos.

    CONCATENATE '</w:' tag '>' INTO l_fin.
    FIND FIRST OCCURRENCE OF l_fin IN string MATCH OFFSET sy-fdpos IGNORING CASE.
    IF sy-subrc = 0.
      IF incluir_tag IS INITIAL.
        string = string(sy-fdpos).
      ELSE.
        DATA(l_pos) = sy-fdpos.
        FIND FIRST OCCURRENCE OF '>' IN string+l_pos MATCH OFFSET sy-fdpos IGNORING CASE.
        IF sy-subrc = 0.
          sy-fdpos = l_pos + sy-fdpos + 1.
          string = string(sy-fdpos).
        ENDIF.
      ENDIF.
    ELSE.
      CONCATENATE '</w:' tag space INTO l_fin RESPECTING BLANKS.
      FIND FIRST OCCURRENCE OF l_fin IN string MATCH OFFSET sy-fdpos IGNORING CASE.
      IF sy-subrc = 0.
        IF incluir_tag IS INITIAL.
          string = string(sy-fdpos).
        ELSE.
          l_pos = sy-fdpos.
          FIND FIRST OCCURRENCE OF '>' IN string+l_pos MATCH OFFSET sy-fdpos IGNORING CASE.
          IF sy-subrc = 0.
            sy-fdpos = l_pos + sy-fdpos + 1.
            string = string(sy-fdpos).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_bloque_marcador.
    DATA: l_doc      TYPE string,
          l_aux      TYPE string,
          l_pos      TYPE i,
          l_ini_tag1 TYPE string,
          l_ini_tag2 TYPE string,
          l_long     TYPE i,
          " TODO: variable is assigned but never used (ABAP cleaner)
          l_ini      TYPE i.

    IF doc IS INITIAL.
      l_doc = mw_docxml.
    ELSE.
      l_doc = doc.
    ENDIF.

    CLEAR string.

    l_aux = |w:name="{ marcador }"/><w:bookmarkEnd|.

    FIND FIRST OCCURRENCE OF l_aux IN l_doc MATCH OFFSET sy-fdpos IGNORING CASE.
    IF sy-subrc = 0.
      l_pos = sy-fdpos - 10.
      DO 3000 TIMES.
        IF l_pos < 0.
          EXIT.
        ENDIF.
        CONCATENATE '<w:' tag space INTO l_ini_tag1 RESPECTING BLANKS.
        CONCATENATE '<w:' tag '>' INTO l_ini_tag2 RESPECTING BLANKS.
        l_long = strlen( l_ini_tag1 ).
        IF l_doc+l_pos(l_long) = l_ini_tag1 OR l_doc+l_pos(l_long) = l_ini_tag2.
          DATA(l_ok) = 'X'.
          l_ini = l_pos.
          EXIT.
        ENDIF.
        l_pos = l_pos - 1.
      ENDDO.
      IF l_ok = 'X'.
        l_aux = l_doc+l_pos.
        string = get_bloque( tag         = tag
                             doc         = l_aux
                             incluir_tag = incluir_tag ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_docx_file.
    DATA lw_xmlns TYPE string.

    CLEAR xcontent.

* Add final section info
    _write_section( ).
    CONCATENATE mw_docxml mw_section_xml INTO mw_docxml.

    _get_xml_ns( IMPORTING xml = lw_xmlns ).

* Add complete xml header
    CONCATENATE
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
    '<w:document '
    lw_xmlns
    '>'
    '<w:body>'
    mw_docxml
    '</w:body></w:document>'
    INTO mw_docxml RESPECTING BLANKS.

* Add custom body into docx
    _update_zip_file( filename = 'word/document.xml'
                      content  = mw_docxml ).

* Get final docx
    xcontent = mo_zip->save( ).
  ENDMETHOD.
  METHOD get_list_headerfooter.
    DATA ls_list_object LIKE LINE OF mt_list_object.

    REFRESH headerfooter_list.
    LOOP AT mt_list_object INTO ls_list_object
         WHERE type = c_type_header OR type = c_type_footer.
      APPEND ls_list_object TO headerfooter_list.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_list_image.
    DATA ls_list_object LIKE LINE OF mt_list_object.

    REFRESH image_list.
    LOOP AT mt_list_object INTO ls_list_object WHERE type = c_type_image.
      APPEND ls_list_object TO image_list.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_list_style.
    style_list[] = mt_list_style[].
  ENDMETHOD.
  METHOD get_marcadores.
    DATA: l_doc      TYPE string,
          l_marcador TYPE t_marcadores,
          aux1       TYPE string,
          aux2       TYPE string.

    CLEAR i_marcadores.
    IF doc IS INITIAL.
      l_doc = mw_docxml.
    ELSE.
      l_doc = doc.
    ENDIF.

    zcl_ap_regexp=>buscar_patron( EXPORTING string    = l_doc
                                            patron    = '<w:bookmark[a-zA-Z\/][^>]*>'
                                  IMPORTING busquedas = DATA(marcadores) ).

    LOOP AT marcadores ASSIGNING FIELD-SYMBOL(<marc>).
      IF <marc>-texto NS '<w:bookmarkStart'.
        CONTINUE.
      ENDIF.

      CLEAR l_marcador.
      SPLIT <marc>-texto AT '<w:bookmarkStart w:id="' INTO aux1 aux2.
      SPLIT aux2 AT '"' INTO l_marcador-id aux1.
      SPLIT aux1 AT 'w:name="' INTO aux1 aux2.
      SPLIT aux2 AT '"' INTO l_marcador-name aux2.

      aux1 = |<w:bookmarkEnd w:id="{ l_marcador-id }"/>|.
      READ TABLE marcadores INTO DATA(l_end) WITH KEY texto = aux1.
      IF sy-subrc = 0.
        DATA(l_long) = l_end-offset - <marc>-offset + l_end-length.
        l_marcador-texto = l_doc+<marc>-offset(l_long).
      ELSE.
        l_marcador-texto = <marc>-texto.
      ENDIF.
      APPEND l_marcador TO i_marcadores.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_texto_marcador.
    CLEAR: texto,
           message.
    ASSIGN i_marcadores[ name = marcador ] TO FIELD-SYMBOL(<marcador>).
    IF sy-subrc = 0.
      texto = <marcador>-texto.
    ELSE.
      message = |No existe el marcador { marcador }|.
    ENDIF.
  ENDMETHOD.
  METHOD header_footer_direct_assign.
    invalid_header = c_false.
    invalid_footer = c_false.
    IF header IS SUPPLIED.
      sy-subrc = 0.
      IF NOT header IS INITIAL.
        READ TABLE mt_list_object WITH KEY type = c_type_header
                                           id   = header
             TRANSPORTING NO FIELDS.
      ENDIF.
      IF sy-subrc = 0.
        ms_section-header = header.
      ELSE.
        invalid_header = c_true.
      ENDIF.
    ENDIF.
    IF footer IS SUPPLIED.
      sy-subrc = 0.
      IF NOT footer IS INITIAL.
        READ TABLE mt_list_object WITH KEY type = c_type_footer
                                           id   = footer
             TRANSPORTING NO FIELDS.
      ENDIF.
      IF sy-subrc = 0.
        ms_section-footer = footer.
      ELSE.
        invalid_footer = c_true.
      ENDIF.
    ENDIF.
    IF header_first IS SUPPLIED.
      sy-subrc = 0.
      IF NOT header_first IS INITIAL.
        READ TABLE mt_list_object WITH KEY type = c_type_header
                                           id   = header_first
             TRANSPORTING NO FIELDS.
      ENDIF.
      IF sy-subrc = 0.
        ms_section-header_first = header_first.
      ELSE.
        invalid_header = c_true.
      ENDIF.
    ENDIF.
    IF footer_first IS SUPPLIED.
      sy-subrc = 0.
      IF NOT footer_first IS INITIAL.
        READ TABLE mt_list_object WITH KEY type = c_type_footer
                                           id   = footer_first
             TRANSPORTING NO FIELDS.
      ENDIF.
      IF sy-subrc = 0.
        ms_section-footer_first = footer_first.
      ELSE.
        invalid_footer = c_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD insert_attachment.
    DATA : lw_filex      TYPE xstring,
           lw_url_begin  TYPE c LENGTH 6,
           lw_extension  TYPE string,
           lw_ct_string  TYPE string,
           lw_rel_string TYPE string,
           lw_filename   TYPE string,
           lw_file       TYPE string,
           lw_string     TYPE string,
           lw_id         TYPE string,
           lw_style      TYPE string.

    invalid_image = c_false.
    invalid_file = c_false.

    IF url IS INITIAL AND file_content IS INITIAL.
      invalid_file = c_true.
      RETURN.
    ENDIF.

* Load file
    IF file_content IS INITIAL.
      _load_file( EXPORTING filename = url
                  IMPORTING xcontent = lw_filex ).
      IF lw_filex IS INITIAL.
        invalid_file = c_true.
        RETURN.
      ENDIF.
    ELSE.
      lw_filex = file_content.
    ENDIF.

* Get file extension
* For file from sapwr, get file extension from DB
    lw_url_begin = url.
    IF lw_url_begin = c_sapwr_prefix.
      SELECT SINGLE value INTO lw_extension
        FROM wwwparams
        WHERE relid = 'MI'
          AND objid = url+6
          AND name  = 'fileextension'.
      IF NOT lw_extension IS INITIAL AND lw_extension(1) = '.'.
        lw_extension = lw_extension+1.
      ENDIF.
    ELSE.
* For other file, get file extension from filename
      FIND ALL OCCURRENCES OF '.' IN url MATCH OFFSET sy-fdpos.
      IF sy-subrc = 0.
        sy-fdpos = sy-fdpos + 1.
        lw_extension = url+sy-fdpos.
        lw_extension = to_lower( lw_extension ).
        IF lw_extension = 'jpeg'.
          lw_extension = 'jpg'.
        ENDIF.
      ENDIF.
    ENDIF.
    lw_extension = to_lower( lw_extension ).
    CASE lw_extension.
      WHEN 'pptx'.
        lw_ct_string = 'application/vnd.openxmlformats-officedocument.presentationml.presentation'.
        lw_rel_string = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/package'.
      WHEN 'ppt'.
        lw_ct_string = 'application/vnd.ms-powerpoint'.
        lw_rel_string = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/oleObject'.
      WHEN 'docx'.
        lw_ct_string = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'.
        lw_rel_string = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/package'.
      WHEN 'docm'.
        lw_ct_string = 'application/vnd.ms-word.document.macroEnabled.12'.
        lw_rel_string = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/package'.
      WHEN 'doc'.
        lw_ct_string = 'application/msword'.
        lw_rel_string = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/oleObject'.
      WHEN 'xlsx'.
        lw_ct_string = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
        lw_rel_string = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/package'.
      WHEN 'xls'.
        lw_ct_string = 'application/vnd.ms-excel'.
        lw_rel_string = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/oleObject'.
      WHEN OTHERS.
        invalid_file = c_true.
        RETURN.
    ENDCASE.

* Search available file name
    DO.
      lw_filename = sy-index.
      CONDENSE lw_filename NO-GAPS.
      CONCATENATE 'word/embeddings/attached' lw_filename '.' lw_extension
                  INTO lw_filename.
      IF NOT line_exists( mo_zip->files[ name = lw_filename ] ).
        EXIT. " exit do
      ENDIF.
    ENDDO.

* Add file in ZIP
    mo_zip->add( name    = lw_filename
                 content = lw_filex ).

* Get file extension list
    _get_zip_file( EXPORTING filename = '[Content_Types].xml'
                   IMPORTING content  = lw_file ).

* Add attachment exception in content type file
    CONCATENATE '<Override PartName="/'
                lw_filename
                '" ContentType="'
                lw_ct_string
                '"/></Types>'
                INTO lw_string.
    REPLACE '</Types>' WITH lw_string
            INTO lw_file.

* Update file extension list
    _update_zip_file( filename = '[Content_Types].xml'
                      content  = lw_file ).

* Get relation file
    _get_zip_file( EXPORTING filename = 'word/_rels/document.xml.rels'
                   IMPORTING content  = lw_file ).

* Create file ID
    DO.
      lw_id = sy-index.
      CONDENSE lw_id NO-GAPS.
      CONCATENATE 'rId' lw_id INTO lw_id ##NO_TEXT.
      CONCATENATE 'Id="' lw_id '"' INTO lw_string ##NO_TEXT.
      FIND FIRST OCCURRENCE OF lw_string IN lw_file IGNORING CASE.
      IF sy-subrc <> 0.
        EXIT. " exit do
      ENDIF.
    ENDDO.

* Add relation
    lw_filename = lw_filename+5.
    CONCATENATE '<Relationship Target="'
                lw_filename
                '" Type="'
                lw_rel_string
                '" Id="'
                lw_id
                '"/>'
                '</Relationships>'
                INTO lw_string.
    REPLACE '</Relationships>' WITH lw_string INTO lw_file.

* Update relation file
    _update_zip_file( filename = 'word/_rels/document.xml.rels'
                      content  = lw_file ).

* Load icon
    _load_image( EXPORTING url = url_img
*      IMPORTING
*                           imgres_x = lw_imgres_x
*                           imgres_y = lw_imgres_y
                 CHANGING  id  = id_img ).
    IF id_img IS INITIAL.
      invalid_image = c_true.
      RETURN.
    ENDIF.

* Search next id for some useless mandatory word attributes
    mw_attach = mw_attach + 1.
    lw_string = mw_attach.
    CONDENSE lw_string NO-GAPS.

* Everything fine, build xml fragment
    CONCATENATE '<w:r>'
                " lw_style
                '<w:object>'
                '<v:shape id="_x0000_'
                lw_string
                '" type="#_x0000_t75" o:ole="">'
                '<v:imagedata r:id="'
                id_img
                '" o:title=""/>'
                '</v:shape>'
                '<o:OLEObject Type="Embed" ProgID="Package" DrawAspect="Icon" ShapeID="_x0000_'
                lw_string
                '" ObjectID="_10000'
                lw_string
                '" r:id="'
                lw_id
                '"/>'
                '</w:object>'
                '</w:r>'
                INTO virtual RESPECTING BLANKS.

    IF as_paragraph = c_true.
* Add paragraphe style
      IF NOT style IS INITIAL OR NOT style_effect IS INITIAL.
        _build_paragraph_style( EXPORTING style         = style
                                          style_effect  = style_effect
                                IMPORTING xml           = lw_style
                                          invalid_style = invalid_style ).
      ENDIF.

      CONCATENATE '<w:p>'
                  lw_style
                  virtual
                  '</w:p>'
                  INTO virtual.
    ENDIF.

* Insert attachment in document
    IF NOT virtual IS SUPPLIED.
      IF as_paragraph = c_true.
        CONCATENATE mw_docxml
                    virtual
                    INTO mw_docxml.
      ELSE.
        CONCATENATE mw_fragxml
                    virtual
                    INTO mw_fragxml.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD insert_custom_field.
    CONCATENATE mw_fragxml
                '<w:fldSimple w:instr=" DOCPROPERTY '
                field
                ' \* MERGEFORMAT "><w:r><w:rPr><w:b/></w:rPr><w:t>Please update field</w:t></w:r></w:fldSimple>'
                INTO mw_fragxml RESPECTING BLANKS.
  ENDMETHOD.
  METHOD insert_image.
    DATA : lw_imgres_x TYPE i,
           lw_imgres_y TYPE i,
           lw_string   TYPE string.
    DATA : lw_scalex    TYPE f,
           lw_scaley    TYPE f,
           lw_scale_max TYPE i,
           lw_scale     TYPE i,
           lw_x         TYPE i,
           lw_y         TYPE i,
           lw_x_string  TYPE string,
           lw_y_string  TYPE string,
           lw_margin    TYPE string,
           lw_type_on   TYPE string,
           lw_type_in   TYPE string,
           lw_type_off  TYPE string,
           lw_posx      TYPE string,
           lw_posy      TYPE string,
           lw_style     TYPE string.

    invalid_image = c_false.
    _load_image( EXPORTING url          = url
                           file_content = file_content
                 IMPORTING imgres_x     = lw_imgres_x
                           imgres_y     = lw_imgres_y
                 CHANGING  id           = id ).
    IF id IS INITIAL.
      invalid_image = c_true.
      RETURN.
    ENDIF.

* Calculate image scale
    IF ms_section-landscape = c_true.
* Max X in landscape : 8877300
* Max Y in landscape : 5743575 "could be less... depend of header/footer
      lw_scalex = 8877300 / lw_imgres_x.
      lw_scaley = 5762625 / lw_imgres_y.
    ELSE.
* Max X in portrait : 5762625
* Max Y in portrait : 8886825 "could be less... depend of header/footer
      lw_scalex = 5762625 / lw_imgres_x.
      lw_scaley = 8886825 / lw_imgres_y.
    ENDIF.
    IF lw_scalex < lw_scaley.
      lw_scale_max = floor( lw_scalex ).
    ELSE.
      lw_scale_max = floor( lw_scaley ).
    ENDIF.
* Image is smaler than page
    IF lw_scale_max > 9525.
      lw_scale = 9525. " no zoom
      IF zoom IS SUPPLIED.
        lw_scale = lw_scale * zoom.
        IF lw_scale > lw_scale_max.
          lw_scale = lw_scale_max.
        ENDIF.
      ENDIF.
    ELSE.
* Image is greater than page
      lw_scale = lw_scale_max.
      IF zoom IS SUPPLIED AND zoom < 1.
        lw_scale_max = 9525 * zoom.
        IF lw_scale_max < lw_scale.
          lw_scale = lw_scale_max.
        ENDIF.
      ENDIF.
    ENDIF.
    lw_x = lw_imgres_x * lw_scale.
    lw_y = lw_imgres_y * lw_scale.
    lw_x_string = lw_x.
    CONDENSE lw_x_string NO-GAPS.
    lw_y_string = lw_y.
    CONDENSE lw_y_string NO-GAPS.

* Finally insert image in document
    mw_imgmaxid = mw_imgmaxid + 1.
    lw_string = mw_imgmaxid.
    CONDENSE lw_string NO-GAPS.

    lw_margin = margin.
    CONDENSE lw_margin NO-GAPS.

    CASE type.
      WHEN c_imgtype_inline.
        CONCATENATE '<wp:inline distT="'
                    lw_margin
                    '" distB="'
                    lw_margin
                    '" distL="'
                    lw_margin
                    '" distR="'
                    lw_margin
                    '">'
                    INTO lw_type_on.
        lw_type_in = ''.
        lw_type_off = '</wp:inline>'.

      WHEN c_imgtype_float OR c_imgtype_over OR c_imgtype_behind.
        IF type = c_imgtype_behind.
          lw_type_on = '1'.
        ELSE.
          lw_type_on = '0'.
        ENDIF.
        IF posx CO '0123456789-'.
          CONCATENATE '<wp:posOffset>' posx '</wp:posOffset>' INTO lw_posx.
        ELSE.
          CONCATENATE '<wp:align>' posx '</wp:align>' INTO lw_posx.
        ENDIF.
        IF posy CO '0123456789-'.
          CONCATENATE '<wp:posOffset>' posy '</wp:posOffset>' INTO lw_posy.
        ELSE.
          CONCATENATE '<wp:align>' posy '</wp:align>' INTO lw_posy.
        ENDIF.

        CONCATENATE '<wp:anchor distT="'
                    lw_margin
                    '" distB="'
                    lw_margin
                    '" distL="'
                    lw_margin
                    '" distR="'
                    lw_margin
                    '" behindDoc="'
                    lw_type_on
                    '" allowOverlap="1"'
                    ' simplePos="0" locked="0" layoutInCell="1" relativeHeight="251658240" >'
                    '<wp:simplePos x="0" y="0"/>'
                    '<wp:positionH relativeFrom="column">'
                    lw_posx
                    '</wp:positionH>'
                    '<wp:positionV relativeFrom="paragraph">'
                    lw_posy
                    '</wp:positionV>'
                    INTO lw_type_on RESPECTING BLANKS.

        IF type = c_imgtype_float.
          lw_type_in = '<wp:wrapSquare wrapText="bothSides"/>'.
        ELSE.
          lw_type_in = '<wp:wrapNone/>'.
        ENDIF.

        lw_type_off = '</wp:anchor>'.
    ENDCASE.
* Prepare image insertion xml fragment
    CONCATENATE
    '<w:r>'
    '<w:drawing>'
    lw_type_on
    '<wp:extent cx="'
    lw_x_string
    '" cy="'
    lw_y_string
    '"/>'
    lw_type_in
    '<wp:docPr id="'
    lw_string " mw_imgmaxid
    '" name=""/>'
    '<wp:cNvGraphicFramePr/>'
    '<a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">'
    '<a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">'
    '<pic:pic xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">'
    '<pic:nvPicPr>'
    '<pic:cNvPr id="0" name=""/>'
    '<pic:cNvPicPr/>'
    '</pic:nvPicPr>'
    '<pic:blipFill>'
    '<a:blip r:embed="'
    id
    '"/>'
    '<a:stretch>'
    '<a:fillRect/>'
    '</a:stretch>'
    '</pic:blipFill>'
    '<pic:spPr>'
    '<a:xfrm>'
    '<a:off x="0" y="0"/>'
    '<a:ext cx="'
    lw_x_string
    '" cy="'
    lw_y_string
    '"/>'
    '</a:xfrm>'
    '<a:prstGeom prst="rect">'
    '<a:avLst/>'
    '</a:prstGeom>'
    '</pic:spPr>'
    '</pic:pic>'
    '</a:graphicData>'
    '</a:graphic>'
    lw_type_off
    '</w:drawing>'
    '</w:r>'
    INTO virtual.

    IF as_paragraph = c_true.
* Add paragraphe style
      IF NOT style IS INITIAL OR NOT style_effect IS INITIAL.
        _build_paragraph_style( EXPORTING style         = style
                                          style_effect  = style_effect
                                IMPORTING xml           = lw_style
                                          invalid_style = invalid_style ).
      ENDIF.

      CONCATENATE '<w:p>'
                  lw_style
                  virtual
                  '</w:p>'
                  INTO virtual.
    ENDIF.

* Insert image in document
    IF NOT virtual IS SUPPLIED.
      IF as_paragraph = c_true.
        CONCATENATE mw_docxml
                    virtual
                    INTO mw_docxml.
      ELSE.
        CONCATENATE mw_fragxml
                    virtual
                    INTO mw_fragxml.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD insert_virtual_field.
    IF field_as_paragraph = c_false.
      CONCATENATE mw_fragxml
                  '<w:virtual>'
                  field
                  '</w:virtual>'
                  INTO mw_fragxml.
    ELSE.
      CONCATENATE mw_docxml
                  '<w:virtual>'
                  field
                  '</w:virtual>'
                  INTO mw_docxml.
    ENDIF.
  ENDMETHOD.
  METHOD insert_xml.
    CONCATENATE mw_docxml xml INTO mw_docxml.
  ENDMETHOD.
  METHOD insert_xml_fragment.
    CONCATENATE mw_fragxml xml INTO mw_fragxml.
  ENDMETHOD.
  METHOD mostrar_en_pantalla.
    DATA: xdata     TYPE xstring,   " Will be used for sending as email
          t_rawdata TYPE solix_tab, " Will be used for downloading or open directly
          bytecount TYPE i.         " Will be used for downloading or open directly
    DATA: cl_control  TYPE REF TO i_oi_container_control, " OIContainerCtrl
          error       TYPE REF TO i_oi_error,
          " TODO: variable is assigned but never used (ABAP cleaner)
          t_errors    TYPE STANDARD TABLE OF REF TO i_oi_error WITH NON-UNIQUE DEFAULT KEY,
          cl_document TYPE REF TO i_oi_document_proxy.    " Office Dokument

    get_docx_file( IMPORTING xcontent = xdata ).

    t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring = xdata ).

    bytecount = xstrlen( xdata ).

    c_oi_container_control_creator=>get_container_control( IMPORTING control = cl_control
                                                                     error   = error ).
    APPEND error TO t_errors.

    cl_control->init_control( EXPORTING  inplace_enabled     = 'X'
                                         no_flush            = 'X'
                                         r3_application_name = 'Demo Document Container'
                                         parent              = cl_gui_container=>screen0
                              IMPORTING  error               = error
                              EXCEPTIONS OTHERS              = 2 ).
    APPEND error TO t_errors.

    cl_control->get_document_proxy( EXPORTING document_type  = 'Word.Document'
                                              no_flush       = ' '
                                    IMPORTING document_proxy = cl_document
                                              error          = error ).
    APPEND error TO t_errors.
* Errorhandling should be inserted here

    cl_document->open_document_from_table( document_size  = bytecount
                                           document_table = t_rawdata
                                           open_inplace   = 'X' ).
    WRITE '.'.  " To create an output.  That way screen0 will exist
  ENDMETHOD.
  METHOD reemplazar_marcador.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: l_doc             TYPE string,
          l_nuevo_contenido TYPE string.

    IF doc IS INITIAL.
      l_doc = mw_docxml.
    ELSE.
      l_doc = doc.
    ENDIF.

    CLEAR message.
    get_texto_marcador( EXPORTING marcador = marcador
                        IMPORTING texto    = DATA(texto_marcador)
                                  message  = message ).

    IF message IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF add_marcador_end IS INITIAL.
      l_nuevo_contenido = nuevo_contenido.
    ELSE.
      CONCATENATE nuevo_contenido texto_marcador INTO l_nuevo_contenido.
    ENDIF.

    IF doc IS INITIAL.
      REPLACE texto_marcador INTO mw_docxml WITH l_nuevo_contenido.
    ELSE.
      REPLACE texto_marcador INTO l_doc WITH l_nuevo_contenido.
    ENDIF.
    IF sy-subrc <> 0.
      message = |Error reemplazando marcador { marcador }|.
    ENDIF.
  ENDMETHOD.
  METHOD reemplazar_valor.
    DATA: l_var   TYPE string,
          l_marca TYPE string,
          l_valor TYPE text1024.
    DATA l_string TYPE string.

    IF car_marca IS INITIAL.
      l_var = var.
    ELSE.
      IF car_marca = '&'.
        l_marca = '&amp;'.
      ELSE.
        l_marca = car_marca.
      ENDIF.

      l_var = var.
      CONCATENATE l_marca l_var l_marca INTO l_var.
    ENDIF.

    DESCRIBE FIELD valor TYPE DATA(l_tipo).

    IF l_tipo = 'C' OR l_tipo = 'g' AND valor CS cl_abap_char_utilities=>cr_lf.
      IF strlen( valor ) > 1000.
        SPLIT valor AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(i_lineas2).
        LOOP AT i_lineas2 ASSIGNING FIELD-SYMBOL(<lin2>).
          CONCATENATE <lin2> '<w:br/>' l_var INTO l_string.
          reemplazar_valor( EXPORTING var   = var
                                      valor = l_string
                            CHANGING  doc   = doc ).
        ENDLOOP.
        RETURN.
*      DATA: l_txt    TYPE text4096,
*            l_string TYPE string.
*      l_txt = valor.
*      DO 5 TIMES.
*        l_string = l_txt(800).
*        l_txt = l_txt+800.
*        IF NOT l_txt IS INITIAL.
*          CONCATENATE l_string l_var INTO l_string.
*        ENDIF.
*        reemplazar_valor( EXPORTING var = var valor = l_string CHANGING doc = doc ).
*        IF l_txt IS INITIAL.
*          RETURN.
*        ENDIF.
*      ENDDO.
      ENDIF.
    ENDIF.

    IF l_tipo = 'I' OR l_tipo = 'P' OR l_tipo = 'D' OR l_tipo = 'T'.
      WRITE valor TO l_valor LEFT-JUSTIFIED.
      CONDENSE l_valor NO-GAPS.
      IF l_tipo = 'D' AND NOT format_fecha IS INITIAL.
        IF format_fecha = '/'.
          REPLACE ALL OCCURRENCES OF '.' IN l_valor WITH '/'.
        ENDIF.
      ENDIF.
      IF l_tipo = 'T' AND NOT format_hora IS INITIAL.
        IF format_hora = '5'.
          l_valor = l_valor(5).
        ENDIF.
      ENDIF.
    ELSEIF valor CS cl_abap_char_utilities=>cr_lf.
*    l_valor = '<w:p><w:r><w:t>'.
      SPLIT valor AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(i_lineas).
      DATA(l_tot_lineas) = lines( i_lineas ).
      LOOP AT i_lineas ASSIGNING FIELD-SYMBOL(<linea>).
*      IF sy-tabix < l_tot_lineas.
*        CONCATENATE l_valor <linea> '</w:t></w:r><w:r><w:br/></w:r><w:r><w:t>' INTO l_valor.
*      ELSE.
*        CONCATENATE l_valor <linea> '</w:t></w:r></w:p>' INTO l_valor.
*      ENDIF.
        IF l_valor IS INITIAL.
          l_valor = <linea>.
        ELSE.
          CONCATENATE l_valor '<w:br/>' <linea> INTO l_valor.
        ENDIF.
      ENDLOOP.
    ELSE.
      WRITE valor TO l_valor LEFT-JUSTIFIED.
      IF quitar_ceros = 'X'.
        __quitar_ceros l_valor.
      ENDIF.
    ENDIF.

    IF todas IS INITIAL.
      IF NOT doc IS INITIAL.
        REPLACE l_var IN doc WITH l_valor IGNORING CASE.
        IF sy-subrc <> 0.
          error = 'X'.
        ENDIF.
      ELSE.
        REPLACE l_var IN mw_fragxml WITH l_valor IGNORING CASE.
        IF sy-subrc <> 0.
          REPLACE l_var IN mw_docxml WITH l_valor IGNORING CASE.
          IF sy-subrc <> 0.
            error = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF NOT doc IS INITIAL.
        REPLACE ALL OCCURRENCES OF l_var IN doc WITH l_valor IGNORING CASE.
        IF sy-subrc <> 0.
          error = 'X'.
        ENDIF.
      ELSE.
        REPLACE ALL OCCURRENCES OF l_var IN mw_fragxml WITH l_valor IGNORING CASE.
        IF sy-subrc <> 0.
          REPLACE ALL OCCURRENCES OF l_var IN mw_docxml WITH l_valor IGNORING CASE.
          IF sy-subrc <> 0.
            error = 'X'.
*            zcl_ap_string=>to_clipboard( mw_docxml ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    " replace_virtual_field
  METHOD replace_virtual_field.
    DATA : lw_field TYPE string,
           lw_value TYPE string.

    CONCATENATE '<w:virtual>'
                field
                '</w:virtual>'
                INTO lw_field.

    IF value_as_xml = c_true.
      lw_value = value.
    ELSE.
      write_text( EXPORTING textline      = value
                            style_effect  = style_effect
                            style         = style
                  IMPORTING virtual       = lw_value
                            invalid_style = invalid_style ).
    ENDIF.

    REPLACE lw_field IN mw_fragxml WITH lw_value IGNORING CASE.
    IF sy-subrc <> 0.
      REPLACE lw_field IN mw_docxml WITH lw_value IGNORING CASE.
    ENDIF.
  ENDMETHOD.
  METHOD save.
    " TODO: parameter LOCAL is only used in commented-out code (ABAP cleaner)

    DATA lw_docx TYPE xstring.

*        lt_data_tab TYPE STANDARD TABLE OF x255,
*        lw_lraw     TYPE x255,
*        lw_xlen     TYPE i,
*        lw_count    TYPE i,
*        lw_off      TYPE i,
*        lw_mod      TYPE i.

    get_docx_file( IMPORTING xcontent = lw_docx ).

    zcl_ap_ficheros=>grabar_xstring( fichero = url
                                     xstring = lw_docx ).

    IF abrir = 'X'.
      zcl_ap_gos=>visualizar_fichero_st( fichero = url ).
    ENDIF.
*
** Convert docx xString to xTable(255)
*    refresh lt_data_tab.
*    clear   lw_off.
*    lw_xlen  = xstrlen( lw_docx ).
*    lw_count = lw_xlen div 255.
*    do lw_count times.
*      lw_lraw = lw_docx+lw_off(255).
*      lw_off = lw_off + 255.
*      append lw_lraw to lt_data_tab.
*    enddo.
*    lw_mod = lw_xlen mod 255.
*    if lw_mod > 0.
*      lw_lraw = lw_docx+lw_off(lw_mod).
*      append lw_lraw to lt_data_tab.
*    endif.
*    clear lw_docx.
*
** Save document on server
*    if local = c_false.
*      open dataset url for output in binary mode.
*      if sy-subrc ne 0.
** Error opening the file
*        message 'Cannot create remote file' type 'E'.
*        return.
*      endif.
*      loop at lt_data_tab into lw_lraw.
*        if lw_xlen > 255.
*          lw_mod = 255.
*        else.
*          lw_mod = lw_xlen.
*        endif.
*        transfer lw_lraw to url length lw_mod.
*        lw_xlen = lw_xlen - lw_mod.
*      endloop.
*      close dataset url.
*
** Save document on local computer
*    elseif local = c_true.
*      call method cl_gui_frontend_services=>gui_download
*        exporting
*          bin_filesize            = lw_xlen
*          filename                = url
*          filetype                = 'BIN'
*          confirm_overwrite       = 'X'
*        changing
*          data_tab                = lt_data_tab
*        exceptions
*          file_write_error        = 1
*          no_batch                = 2
*          gui_refuse_filetransfer = 3
*          invalid_type            = 4
*          no_authority            = 5
*          unknown_error           = 6
*          header_not_allowed      = 7
*          separator_not_allowed   = 8
*          filesize_not_allowed    = 9
*          header_too_long         = 10
*          dp_error_create         = 11
*          dp_error_send           = 12
*          dp_error_write          = 13
*          unknown_dp_error        = 14
*          access_denied           = 15
*          dp_out_of_memory        = 16
*          disk_full               = 17
*          dp_timeout              = 18
*          file_not_found          = 19
*          dataprovider_exception  = 20
*          control_flush_error     = 21
*          not_supported_by_gui    = 22
*          error_no_gui            = 23
*          others                  = 24.
** Error on save
*      if sy-subrc ne 0.
*        message id sy-msgid type sy-msgty number sy-msgno.
*      endif.
*    endif.
  ENDMETHOD.
  METHOD set_params.
    DATA : lw_xml   TYPE string,
           lw_xmlns TYPE string.

* Define orientation
    IF orientation = c_orient_landscape.
      ms_section-landscape = c_true.
    ENDIF.

* Define Border
    IF border_left IS SUPPLIED.
      ms_section-border_left = border_left.
    ENDIF.
    IF border_top IS SUPPLIED.
      ms_section-border_top = border_top.
    ENDIF.
    IF border_right IS SUPPLIED.
      ms_section-border_right = border_right.
    ENDIF.
    IF border_bottom IS SUPPLIED.
      ms_section-border_bottom = border_bottom.
    ENDIF.

* Hide spellcheck for this document
    IF nospellcheck = c_false.
      RETURN.
    ENDIF.

    _get_zip_file( EXPORTING filename = 'word/settings.xml'
                   IMPORTING content  = lw_xml ).
* File doesn't exist ? create it !
    IF lw_xml IS INITIAL.
      _get_xml_ns( IMPORTING xml = lw_xmlns ).
      CONCATENATE '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                  '<w:settings '
                  lw_xmlns
                  '>'
                  '</w:settings>'
                  INTO lw_xml RESPECTING BLANKS.
    ENDIF.
    FIND 'hideSpellingErrors' IN lw_xml.
    IF sy-subrc <> 0.
      REPLACE FIRST OCCURRENCE OF '</w:settings>'
              IN lw_xml
              WITH '<w:hideSpellingErrors/></w:settings>'
              IGNORING CASE.
    ENDIF.
    FIND 'hideGrammaticalErrors' IN lw_xml.
    IF sy-subrc <> 0.
      REPLACE FIRST OCCURRENCE OF '</w:settings>'
              IN lw_xml
              WITH '<w:hideGrammaticalErrors/></w:settings>'
              IGNORING CASE.
    ENDIF.
    _update_zip_file( filename = 'word/settings.xml'
                      content  = lw_xml ).
  ENDMETHOD.
  METHOD set_properties.
    DATA : lw_xml    TYPE string,
           lw_string TYPE string.

* Get prperties file
    _get_zip_file( EXPORTING filename = 'docProps/core.xml'
                   IMPORTING content  = lw_xml ).

    IF title IS SUPPLIED.
* Replace existing title by new one
      _protect_string( EXPORTING in  = title
                       IMPORTING out = lw_string ).
      CONCATENATE '<dc:title>'
                  lw_string
                  '</dc:title>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<dc:title>(.*)</dc:title>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<dc:title\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no title property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
    ENDIF.

    IF object IS SUPPLIED.
* Replace existing object by new one
      _protect_string( EXPORTING in  = object
                       IMPORTING out = lw_string ).
      CONCATENATE '<dc:subject>'
                  lw_string
                  '</dc:subject>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<dc:subject>(.*)</dc:subject>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<dc:subject\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no object property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
    ENDIF.

    IF author IS SUPPLIED.
* Replace existing author by new one
      _protect_string( EXPORTING in  = author
                       IMPORTING out = mw_author ).
      CONCATENATE '<dc:creator>'
                  mw_author
                  '</dc:creator>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<dc:creator>(.*)</dc:creator>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<dc:creator\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no author property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
* Set also last modified property
      _protect_string( EXPORTING in  = author
                       IMPORTING out = lw_string ).
      CONCATENATE '<cp:lastModifiedBy>'
                  lw_string
                  '</cp:lastModifiedBy>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<cp:lastModifiedBy>(.*)</cp:lastModifiedBy>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<cp:lastModifiedBy\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no lastmodified property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
    ENDIF.

    IF creationdate IS SUPPLIED.
* Replace existing creation date by new one
      CONCATENATE '<dcterms:created xsi:type="dcterms:W3CDTF">'
                  creationdate(4)
                  '-'
                  creationdate+4(2)
                  '-'
                  creationdate+6(2)
                  'T'
                  creationtime(2)
                  ':'
                  creationtime+2(2)
                  ':'
                  creationtime+2(2)
                  'Z'
                  '</dcterms:created>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<dcterms:created(.*)</dcterms:created>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<dcterms:created\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no creation date property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
* Replace also last modification date by new one
      CONCATENATE '<dcterms:modified xsi:type="dcterms:W3CDTF">'
                  creationdate(4)
                  '-'
                  creationdate+4(2)
                  '-'
                  creationdate+6(2)
                  'T'
                  creationtime(2)
                  ':'
                  creationtime+2(2)
                  ':'
                  creationtime+2(2)
                  'Z'
                  '</dcterms:modified>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<dcterms:modified(.*)</dcterms:modified>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<dcterms:modified\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no modification date property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
    ENDIF.

    IF description IS SUPPLIED.
* Replace existing description by new one
      _protect_string( EXPORTING in  = description
                       IMPORTING out = lw_string ).
      CONCATENATE '<dc:description>'
                  lw_string
                  '</dc:description>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<dc:description>(.*)</dc:description>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<dc:description\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no description property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
    ENDIF.

    IF category IS SUPPLIED.
* Replace existing category by new one
      _protect_string( EXPORTING in  = category
                       IMPORTING out = lw_string ).
      CONCATENATE '<cp:category>'
                  lw_string
                  '</cp:category>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<cp:category>(.*)</cp:category>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<cp:category\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no category property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
    ENDIF.

    IF keywords IS SUPPLIED.
* Replace existing keywords by new one
      _protect_string( EXPORTING in  = keywords
                       IMPORTING out = lw_string ).
      CONCATENATE '<cp:keywords>'
                  lw_string
                  '</cp:keywords>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<cp:keywords>(.*)</cp:keywords>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<cp:keywords\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no keywords property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
    ENDIF.

    IF status IS SUPPLIED.
* Replace existing status by new one
      _protect_string( EXPORTING in  = status
                       IMPORTING out = lw_string ).
      CONCATENATE '<cp:contentStatus>'
                  lw_string
                  '</cp:contentStatus>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<cp:contentStatus>(.*)</cp:contentStatus>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<cp:contentStatus\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no status property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
    ENDIF.

    IF revision IS SUPPLIED.
* Replace existing status by new one
      lw_string = revision.
      CONDENSE lw_string NO-GAPS.
      CONCATENATE '<cp:revision>'
                  lw_string
                  '</cp:revision>'
                  INTO lw_string.
      REPLACE FIRST OCCURRENCE OF REGEX '<cp:revision>(.*)</cp:revision>' IN lw_xml WITH lw_string.
      IF sy-subrc <> 0.
        REPLACE FIRST OCCURRENCE OF REGEX '<cp:revision\s*/>' IN lw_xml WITH lw_string.
      ENDIF.
* If no revision property found, add it at end of xml
      IF sy-subrc <> 0.
        CONCATENATE lw_string
                    '</cp:coreProperties>'
                    INTO lw_string.
        REPLACE '</cp:coreProperties>'
                WITH lw_string
                INTO lw_xml.
      ENDIF.
    ENDIF.

* Update properties file
    _update_zip_file( filename = 'docProps/core.xml'
                      content  = lw_xml ).
  ENDMETHOD.
  METHOD set_tabla.
    DATA : ls_content   TYPE REF TO data,
           l_doc        TYPE string,
           l_bloque_edt TYPE string,
           l_aux        TYPE string,
           l_linea      TYPE string,
           l_tabla      TYPE string.

    FIELD-SYMBOLS <line>  TYPE any.
    FIELD-SYMBOLS <field> TYPE any.

    CREATE DATA ls_content LIKE LINE OF tabla.
    ASSIGN ls_content->* TO <line>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* count number of lines and columns of the table
    DATA(lw_lines) = LINES( tabla ).
    IF lw_lines = 0.
*    RETURN. "Aunque hay 0 registros la pintamos
    ENDIF.

    DESCRIBE FIELD <line> TYPE DATA(lw_type) COMPONENTS DATA(lw_cols).
    IF lw_cols = 0.
      RETURN.
    ENDIF.

    IF doc IS INITIAL.
      l_doc = mw_docxml.
    ELSE.
      l_doc = doc.
    ENDIF.

    DATA(bloque) = get_bloque_marcador( marcador    = marcador
                                        tag         = tag
                                        doc         = l_doc
                                        incluir_tag = 'X' ).

    IF bloque IS INITIAL.
      MESSAGE |No encuentro marcador { marcador }| TYPE 'E'.
    ELSE.
      l_bloque_edt = bloque.
      IF quitar_marcador = 'X'.
        l_aux = |w:name="{ marcador }"/><w:bookmarkEnd|.
        l_linea = |w:name="{ marcador }_DESACTIVADO"/><w:bookmarkEnd|.
        REPLACE l_aux IN l_bloque_edt WITH l_linea.
      ENDIF.
    ENDIF.

    LOOP AT tabla INTO <line>.
      l_linea = l_bloque_edt.
      LOOP AT i_var ASSIGNING FIELD-SYMBOL(<var>).
        ASSIGN COMPONENT <var>-value OF STRUCTURE <line> TO <field>.
        IF sy-subrc = 0.
          reemplazar_valor( EXPORTING var          = <var>-key
                                      valor        = <field>
                                      format_fecha = format_fecha
                                      format_hora  = format_hora
                                      quitar_ceros = quitar_ceros
                            CHANGING  doc          = l_linea ).
        ENDIF.
      ENDLOOP.
      IF l_tabla IS INITIAL.
        l_tabla = l_linea.
      ELSE.
        CONCATENATE l_tabla l_linea INTO l_tabla.
      ENDIF.
    ENDLOOP.
    IF sy-subrc <> 0. " Ningun registro, valores por dfecto
      l_linea = l_bloque_edt.
      LOOP AT i_var ASSIGNING <var>.
        ASSIGN COMPONENT <var>-value OF STRUCTURE <line> TO <field>.
        IF sy-subrc = 0.
          reemplazar_valor( EXPORTING var          = <var>-key
                                      valor        = <field>
                                      format_fecha = format_fecha
                                      format_hora  = format_hora
                                      quitar_ceros = quitar_ceros
                            CHANGING  doc          = l_linea ).
        ENDIF.
      ENDLOOP.
      l_tabla = l_linea.
    ENDIF.

    IF doc IS INITIAL.
      REPLACE bloque IN mw_docxml WITH l_tabla.
    ELSE.
      REPLACE bloque IN doc WITH l_tabla.
    ENDIF.
  ENDMETHOD.
  METHOD set_title.
    set_properties( title = title ).
  ENDMETHOD.
  METHOD write_break.
    CASE breaktype.
      WHEN c_breaktype_line.
        IF virtual IS SUPPLIED.
          CONCATENATE virtual
                      '<w:r><w:br/></w:r>'
                      INTO virtual.
        ELSE.
          CONCATENATE mw_fragxml
                      '<w:r><w:br/></w:r>'
                      INTO mw_fragxml.
        ENDIF.
      WHEN c_breaktype_page.
        IF virtual IS SUPPLIED.
          CONCATENATE virtual
                      '<w:br w:type="page"/>'
                      INTO virtual.
        ELSE.
          CONCATENATE mw_fragxml
                      '<w:br w:type="page"/>'
                      INTO mw_fragxml.
        ENDIF.
      WHEN c_breaktype_section.
        IF virtual IS SUPPLIED.
          sy-subrc = 8.
          RETURN.
        ENDIF.
        _write_section( ).
      WHEN c_breaktype_section_continuous.
        IF virtual IS SUPPLIED.
          sy-subrc = 8.
          RETURN.
        ENDIF.
        _write_section( ).
        ms_section-continuous = c_true.
      WHEN OTHERS. " invalid breaktype
        sy-subrc = 8.
        RETURN.
    ENDCASE.
* Write line if required
* But also automatically except for simple break line
    IF    write_line = c_true
       OR ( NOT write_line IS SUPPLIED AND breaktype <> c_breaktype_line ).
      write_line( ).
    ENDIF.
  ENDMETHOD.
  METHOD write_comment.
    DATA : lw_file              TYPE string,
           lw_string            TYPE string,
           lw_id                TYPE string,
           lw_xmlns             TYPE string,
           lw_text              TYPE string,
           ls_head_style_effect TYPE ty_character_style_effect,
           lw_head_style        TYPE string,
           lw_line_style        TYPE string,
           lw_author            TYPE string,
           lw_initials          TYPE string.

    IF line_exists( mo_zip->files[ name = 'word/comments.xml' ] ).
* If comment file exists, load the file
      _get_zip_file( EXPORTING filename = 'word/comments.xml'
                     IMPORTING content  = lw_file ).
    ELSE.
* If comments file doesn't exist, declare it and create it
* Add comments in content_types
      _get_zip_file( EXPORTING filename = '[Content_Types].xml'
                     IMPORTING content  = lw_file ).

      CONCATENATE '<Override'
                  ' ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml"'
                  ' PartName="/word/comments.xml"/></Types>'
                  INTO lw_string RESPECTING BLANKS.
      REPLACE '</Types>' WITH lw_string
              INTO lw_file.

      _update_zip_file( filename = '[Content_Types].xml'
                        content  = lw_file ).

* Add comments in relation file
      _get_zip_file( EXPORTING filename = 'word/_rels/document.xml.rels'
                     IMPORTING content  = lw_file ).

* Create comments relation ID
      DO.
        lw_id = sy-index.
        CONDENSE lw_id NO-GAPS.
        CONCATENATE 'rId' lw_id INTO lw_id ##NO_TEXT.
        CONCATENATE 'Id="' lw_id '"' INTO lw_string ##NO_TEXT.
        FIND FIRST OCCURRENCE OF lw_string IN lw_file IGNORING CASE.
        IF sy-subrc <> 0.
          EXIT. " exit do
        ENDIF.
      ENDDO.

* Add relation
      CONCATENATE '<Relationship Target="comments.xml"'
                  ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments"'
                  ' Id="'
                  lw_id
                  '"/>'
                  '</Relationships>'
                  INTO lw_string RESPECTING BLANKS.
      REPLACE '</Relationships>' WITH lw_string INTO lw_file.

* Update relation file
      _update_zip_file( filename = 'word/_rels/document.xml.rels'
                        content  = lw_file ).

      _get_xml_ns( IMPORTING xml = lw_xmlns ).

* Create empty comments file
      CONCATENATE '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                  " cl_abap_char_utilities=>cr_lf
                  cl_abap_char_utilities=>newline
                  '<w:comments '
                  lw_xmlns
                  '>'
                  '</w:comments>'
                  INTO lw_file RESPECTING BLANKS.
    ENDIF.

* Search available comment id
    DO.
      " sy-index = sy-index + 4.
      lw_id = sy-index.
      CONDENSE lw_id NO-GAPS.
      CONCATENATE 'w:id="' lw_id '"' INTO lw_string ##NO_TEXT.
      FIND FIRST OCCURRENCE OF lw_string IN lw_file IGNORING CASE.
      IF sy-subrc <> 0.
        EXIT. " exit do
      ENDIF.
    ENDDO.

* Add blank at start of note
    lw_text = text.
    IF lw_text IS INITIAL OR lw_text(1) <> space.
      CONCATENATE space lw_text INTO lw_text RESPECTING BLANKS.
    ENDIF.

    write_text( EXPORTING textline      = lw_text
                          style_effect  = style_effect
                          style         = style
                IMPORTING virtual       = lw_text
                          invalid_style = invalid_style ).

* Define default style for comment head
    ls_head_style_effect = head_style_effect.
    IF ls_head_style_effect IS INITIAL AND head_style IS INITIAL.
      ls_head_style_effect-bold = c_true.
    ENDIF.

    _build_character_style( EXPORTING style         = head_style
                                      style_effect  = ls_head_style_effect
                            IMPORTING xml           = lw_head_style
                                      invalid_style = invalid_head_style ).

    IF NOT line_style_effect IS INITIAL OR NOT line_style IS INITIAL.
      _build_paragraph_style( EXPORTING style         = line_style
                                        style_effect  = line_style_effect
                              IMPORTING xml           = lw_line_style
                                        invalid_style = invalid_line_style ).
    ENDIF.

* Define author property
    IF author IS INITIAL.
      lw_author = mw_author.
    ELSE.
      lw_author = author.
    ENDIF.

* Define initial property
    IF initials IS INITIAL.
      lw_initials = lw_author.
    ELSE.
      lw_initials = initials.
    ENDIF.

    CONCATENATE '<w:comment w:initials="'
                lw_initials
                '"'
                ' w:date="'
                datum(4)
                '-'
                datum+4(2)
                '-'
                datum+6(2)
                'T'
                uzeit(2)
                ':'
                uzeit+2(2)
                ':'
                uzeit+4(2)
                'Z"'
                ' w:author="'
                lw_author
                '"'
                ' w:id="'
                lw_id
                '">'
                '<w:p>'
                lw_line_style
                '<w:r>'
                lw_head_style
                '<w:annotationRef/>'
                '</w:r>'
                lw_text
                '</w:p>'
                '</w:comment>'
                '</w:comments>'
                INTO lw_string RESPECTING BLANKS.
    REPLACE '</w:comments>' WITH lw_string INTO lw_file.

    _update_zip_file( filename = 'word/comments.xml'
                      content  = lw_file ).

* Finally insert reference to comment in current text fragment
    CONCATENATE mw_fragxml
                '<w:r><w:commentReference w:id="'
                lw_id
                '"/></w:r>'
                INTO mw_fragxml.
  ENDMETHOD.
  METHOD write_headerfooter.
    DATA : lw_xml         TYPE string,
           lw_style       TYPE string,
           lw_xmlns       TYPE string,
           lw_string      TYPE string,
           lw_filename    TYPE string,
           lw_file        TYPE string,
           ls_list_object LIKE LINE OF mt_list_object.

    IF type <> c_type_header AND type <> c_type_footer.
      RETURN.
    ENDIF.

* Build header/footer xml fragment
    write_text( EXPORTING textline      = textline
                          style_effect  = style_effect
                          style         = style
                IMPORTING virtual       = lw_xml
                          invalid_style = invalid_style ).

    CLEAR lw_style.
    IF NOT line_style IS INITIAL OR NOT line_style_effect IS INITIAL.
      _build_paragraph_style( EXPORTING style         = line_style
                                        style_effect  = line_style_effect
                              IMPORTING xml           = lw_style
                                        invalid_style = invalid_line_style ).
    ENDIF.

    _get_xml_ns( IMPORTING xml = lw_xmlns ).

    IF type = c_type_header.
      lw_string = 'hdr'.
    ELSE.
      lw_string = 'ftr'.
    ENDIF.
    CONCATENATE '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                '<w:'
                lw_string
                lw_xmlns
                '>'
                '<w:p>'
                lw_style
                lw_xml
                '</w:p>'
                '</w:'
                lw_string
                '>'
                INTO lw_xml RESPECTING BLANKS.

* Search available header/footer name
    DO.
      lw_filename = sy-index.
      CONCATENATE 'word/'
                  type
                  lw_filename
                  '.xml'
                  INTO lw_filename.
      CONDENSE lw_filename NO-GAPS.

      IF NOT line_exists( mo_zip->files[ name = lw_filename ] ).
        EXIT. " exit do
      ENDIF.
    ENDDO.

* Add header/footer file into zip
    _update_zip_file( filename = lw_filename
                      content  = lw_xml ).

* Add content type exception for new header/footer
    _get_zip_file( EXPORTING filename = '[Content_Types].xml'
                   IMPORTING content  = lw_file ).
    CONCATENATE '<Override PartName="/'
                lw_filename
                '" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.'
                type
                '+xml"/>'
                '</Types>'
                INTO lw_string.
    REPLACE '</Types>' WITH lw_string INTO lw_file.

* Update content type file
    _update_zip_file( filename = '[Content_Types].xml'
                      content  = lw_file ).

* Get relation file
    _get_zip_file( EXPORTING filename = 'word/_rels/document.xml.rels'
                   IMPORTING content  = lw_file ).

* Create header/footer ID
    DO.
      id = sy-index.
      CONDENSE id NO-GAPS.
      CONCATENATE 'rId' id INTO id ##NO_TEXT.
      CONCATENATE 'Id="' id '"' INTO lw_string ##NO_TEXT.
      FIND FIRST OCCURRENCE OF lw_string IN lw_file IGNORING CASE.
      IF sy-subrc <> 0.
        EXIT. " exit do
      ENDIF.
    ENDDO.

* Update object list
    ls_list_object-id   = id.
    ls_list_object-type = type.
    ls_list_object-path = lw_filename.
    APPEND ls_list_object TO mt_list_object.

* Add relation
    lw_filename = lw_filename+5.
    CONCATENATE '<Relationship Id="'
                id
                '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/'
                type
                '" Target="'
                lw_filename
                '"/>'
                '</Relationships>'
                INTO lw_string.
    REPLACE '</Relationships>' WITH lw_string INTO lw_file.

* Update relation file
    _update_zip_file( filename = 'word/_rels/document.xml.rels'
                      content  = lw_file ).

    IF usenow_default = c_true AND type = c_type_header.
      header_footer_direct_assign( header = id ).
    ELSEIF usenow_default = c_true AND type = c_type_footer.
      header_footer_direct_assign( footer = id ).
    ENDIF.

    IF usenow_first = c_true AND type = c_type_header.
      header_footer_direct_assign( header_first = id ).
    ELSEIF usenow_first = c_true AND type = c_type_footer.
      header_footer_direct_assign( footer_first = id ).
    ENDIF.
  ENDMETHOD.
  METHOD write_line.
    DATA lw_style TYPE string.

    _build_paragraph_style( EXPORTING style         = style
                                      style_effect  = style_effect
                            IMPORTING xml           = lw_style
                                      invalid_style = invalid_style ).

    IF virtual IS SUPPLIED.
      IF virtual IS INITIAL.
        CONCATENATE mw_docxml
                    '<w:p>'
                    lw_style
                    mw_fragxml
                    '</w:p>'
                    INTO virtual.
        CLEAR mw_fragxml.
      ELSE.
        CONCATENATE '<w:p>'
                    lw_style
                    virtual
                    '</w:p>'
                    INTO virtual.
      ENDIF.
    ELSE.
      CONCATENATE mw_docxml
                  '<w:p>'
                  lw_style
                  mw_fragxml
                  '</w:p>'
                  INTO mw_docxml.
      CLEAR mw_fragxml.
    ENDIF.
  ENDMETHOD.
  METHOD write_newpage.
    write_break( breaktype = c_breaktype_page ).
  ENDMETHOD.
  METHOD write_note.
    DATA : ls_link_style_effect TYPE ty_character_style_effect,
           ls_line_style_effect TYPE ty_paragraph_style_effect,
           lw_id                TYPE string,
           lw_style             TYPE string,
           lw_string            TYPE string.

* Define a default style for link
    ls_link_style_effect = link_style_effect.
    IF link_style IS INITIAL AND ls_link_style_effect IS INITIAL.
      ls_link_style_effect-sup = c_true.
    ENDIF.

* Define a default style for footnote
    ls_line_style_effect = line_style_effect.
    IF line_style IS INITIAL AND ls_line_style_effect IS INITIAL.
      ls_line_style_effect-spacing_before = '0'.
      ls_line_style_effect-spacing_after  = '0'.
      ls_line_style_effect-interline      = 240.
    ENDIF.

* Create a new footnote
    _create_note( EXPORTING text               = text
                            type               = type
                            style              = style
                            style_effect       = style_effect
                            line_style         = line_style
                            line_style_effect  = ls_line_style_effect
                            link_style         = link_style
                            link_style_effect  = ls_link_style_effect
                  IMPORTING invalid_style      = invalid_style
                            invalid_line_style = invalid_line_style
                            id                 = lw_id ).

    IF lw_id IS INITIAL.
      RETURN.
    ENDIF.

* Prepare style for the footnote link
    _build_character_style( EXPORTING style         = link_style
                                      style_effect  = ls_link_style_effect
                            IMPORTING xml           = lw_style
                                      invalid_style = invalid_link_style ).

* Now insert note in document
    IF type = c_notetype_foot.
      lw_string = '<w:footnoteReference w:id="'.
    ELSEIF type = c_notetype_end.
      lw_string = '<w:endnoteReference w:id="'.
    ENDIF.

    CONCATENATE mw_fragxml
                '<w:r>'
                lw_style
                lw_string
                lw_id
                '"/>'
                '</w:r>'
                INTO mw_fragxml.
  ENDMETHOD.                    " write_footnote
  METHOD write_symbol.
    DATA lw_style_effect TYPE ty_character_style_effect.

    lw_style_effect-font = c_font_symbol.
    write_text( textline     = symbol
                style_effect = lw_style_effect ).
  ENDMETHOD.
  METHOD write_table.
    DATA : ls_content     TYPE REF TO data,
           lw_lines       TYPE i,
           lw_type        TYPE c LENGTH 1 ##NEEDED,
           lw_cols        TYPE i,
           lw_style_table TYPE i,
           lw_xml         TYPE string,
           lw_style       TYPE string,
           lw_tblwidth    TYPE string,
           lw_col         TYPE i,
           lw_col_inc     TYPE i,
           lw_string      TYPE string,
           lw_stylep      TYPE string,
           lw_merge       TYPE string.

    FIELD-SYMBOLS <line>        TYPE any.
    FIELD-SYMBOLS <field>       TYPE any.
    FIELD-SYMBOLS <field_style> TYPE ty_table_style_field.

    CREATE DATA ls_content LIKE LINE OF content.
    ASSIGN ls_content->* TO <line>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* count number of lines and columns of the table
    lw_lines = LINES( content ).
    IF lw_lines = 0.
      RETURN.
    ENDIF.
    DESCRIBE FIELD <line> TYPE lw_type COMPONENTS lw_cols.
    IF lw_cols = 0.
      RETURN.
    ENDIF.

* search if data table is simple or have style infos for each field
    ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <field>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    DESCRIBE FIELD <field> TYPE lw_type COMPONENTS lw_style_table.

* Write table properties
    CLEAR lw_xml.
    CONCATENATE '<w:tbl>'
                '<w:tblPr>'
                INTO lw_xml.

    " Styled table : define style
    IF style IS SUPPLIED AND NOT style IS INITIAL.
      IF line_exists( mt_list_style[ type = c_type_table
                                     name = style ] ).
        CONCATENATE lw_xml
                    '<w:tblStyle w:val="'
                    style
                    '"/>'
                    INTO lw_xml.
      ELSE.
        invalid_style = c_true.
      ENDIF.

* If defined, overwrite style table layout (no effect without table style defined)
      CLEAR lw_style.
      IF style_overwrite IS SUPPLIED.
        lw_tblwidth = style_overwrite-firstrow.
        CONDENSE lw_tblwidth NO-GAPS.
        CONCATENATE lw_style
                    ' w:firstRow="'
                    lw_tblwidth
                    '"'
                    INTO lw_style RESPECTING BLANKS.
        lw_tblwidth = style_overwrite-firstcol.
        CONDENSE lw_tblwidth NO-GAPS.
        CONCATENATE lw_style
                    ' w:firstColumn="'
                    lw_tblwidth
                    '"'
                    INTO lw_style RESPECTING BLANKS.
        lw_tblwidth = style_overwrite-nozebra.
        CONDENSE lw_tblwidth NO-GAPS.
        CONCATENATE lw_style
                    ' w:noHBand="'
                    lw_tblwidth
                    '"'
                    INTO lw_style RESPECTING BLANKS.
        lw_tblwidth = style_overwrite-novband.
        CONDENSE lw_tblwidth NO-GAPS.
        CONCATENATE lw_style
                    ' w:noVBand="'
                    lw_tblwidth
                    '"'
                    INTO lw_style RESPECTING BLANKS.
        lw_tblwidth = style_overwrite-lastrow.
        CONDENSE lw_tblwidth NO-GAPS.
        CONCATENATE lw_style
                    ' w:lastRow="'
                    lw_tblwidth
                    '"'
                    INTO lw_style RESPECTING BLANKS.
        lw_tblwidth = style_overwrite-lastcol.
        CONDENSE lw_tblwidth NO-GAPS.
        CONCATENATE lw_style
                    ' w:lastColumn="'
                    lw_tblwidth
                    '"'
                    INTO lw_style RESPECTING BLANKS.

        CONCATENATE lw_xml
                    '<w:tblLook'
                    lw_style
                    '/>'
                    INTO lw_xml RESPECTING BLANKS.
        CLEAR lw_style.
      ENDIF.
    ENDIF.

* Default not styled table : add border
    IF NOT style IS SUPPLIED.
      IF border = c_true.
        CONCATENATE lw_xml
                    '<w:tblBorders>'
                    '<w:top w:color="auto" w:space="0" w:sz="4" w:val="single"/>'
                    '<w:left w:color="auto" w:space="0" w:sz="4" w:val="single"/>'
                    '<w:bottom w:color="auto" w:space="0" w:sz="4" w:val="single"/>'
                    '<w:right w:color="auto" w:space="0" w:sz="4" w:val="single"/>'
                    '<w:insideH w:color="auto" w:space="0" w:sz="4" w:val="single"/>'
                    '<w:insideV w:color="auto" w:space="0" w:sz="4" w:val="single"/>'
                    '</w:tblBorders>'
                    INTO lw_xml.
      ENDIF.
    ENDIF.

* Define table width
    lw_tblwidth = tblwidth.
    CONDENSE lw_tblwidth NO-GAPS.
    IF tblwidth = 0.
* If no table width given, set it to "auto"
      CONCATENATE lw_xml
                  '<w:tblW w:w="'
                  lw_tblwidth
                  '" w:type="auto"/>'
                  '</w:tblPr>'
                  INTO lw_xml.
    ELSE.
      CONCATENATE lw_xml
                  '<w:tblW w:w="'
                  lw_tblwidth
                  '" w:type="dxa"/>'
                  '</w:tblPr>'
                  INTO lw_xml.
    ENDIF.

* Fill table content
    LOOP AT content INTO <line>.
      CONCATENATE lw_xml
                  '<w:tr>'
                  INTO lw_xml.
      lw_col = 1.
      DO lw_cols TIMES.
        lw_col_inc = 1.
*--Filling the cell
        IF lw_style_table = 0. " fields are plain text
          CONCATENATE lw_xml
                      '<w:tc>'
                      INTO lw_xml.

          ASSIGN COMPONENT lw_col OF STRUCTURE <line> TO <field>.
          lw_string = <field>.
          write_text( EXPORTING textline = lw_string
                      IMPORTING virtual  = lw_string ).
          CONCATENATE lw_xml
                      '<w:p>'
                      lw_string
                      '</w:p>'
                      INTO lw_xml.
        ELSE. " fields have ty_table_style_field structure, apply styles
          ASSIGN COMPONENT lw_col OF STRUCTURE <line> TO <field_style>.
          IF sy-subrc <> 0. " Occurs when merged cell
            EXIT. " exit do
          ENDIF.
          CONCATENATE lw_xml
                      '<w:tc>'
                      INTO lw_xml.

          CLEAR : lw_style,
                  lw_stylep.
          IF NOT <field_style>-bgcolor IS INITIAL.
            CONCATENATE lw_style
                        '<w:shd w:fill="'
                        <field_style>-bgcolor
                        '"/>'
                        INTO lw_style.
          ENDIF.
          IF NOT <field_style>-valign IS INITIAL.
            CONCATENATE lw_style
                        '<w:vAlign w:val="'
                        <field_style>-valign
                        '"/>'
                        INTO lw_style.
          ENDIF.
          IF <field_style>-merge > 1.
            lw_col_inc = <field_style>-merge.
            lw_merge = <field_style>-merge.
            CONDENSE lw_merge NO-GAPS.
            CONCATENATE lw_style
                        '<w:gridSpan w:val="'
                        lw_merge
                        '"/>'
                        INTO lw_style.
          ENDIF.

          IF NOT lw_style IS INITIAL.
            CONCATENATE '<w:tcPr>' lw_style '</w:tcPr>' INTO lw_style.
          ENDIF.

          CLEAR lw_string.
          IF NOT <field_style>-xml IS INITIAL.
            CONCATENATE lw_xml
                        lw_style
                        <field_style>-xml
                        INTO lw_xml.
          ELSEIF NOT <field_style>-image_id IS INITIAL.
            CLEAR lw_string.
            insert_image( EXPORTING style        = <field_style>-line_style
                                    style_effect = <field_style>-line_style_effect
                          IMPORTING virtual      = lw_string
                          CHANGING  id           = <field_style>-image_id ).

            CONCATENATE lw_xml
                        lw_style
                        lw_string
                        INTO lw_xml.
          ELSE.
            write_text( EXPORTING textline     = <field_style>-textline
                                  style_effect = <field_style>-style_effect
                                  style        = <field_style>-style
                        IMPORTING virtual      = lw_string ).

            IF    NOT <field_style>-line_style        IS INITIAL
               OR NOT <field_style>-line_style_effect IS INITIAL.
              _build_paragraph_style( EXPORTING style        = <field_style>-line_style
                                                style_effect = <field_style>-line_style_effect
                                      IMPORTING xml          = lw_stylep ).
            ENDIF.

            CONCATENATE lw_xml
                        lw_style
                        '<w:p>'
                        lw_stylep
                        lw_string
                        '</w:p>'
                        INTO lw_xml.
          ENDIF.
        ENDIF.
        CONCATENATE lw_xml '</w:tc>' INTO lw_xml.
        lw_col = lw_col + lw_col_inc.
      ENDDO.
      CONCATENATE lw_xml '</w:tr>' INTO lw_xml.
    ENDLOOP.

    CONCATENATE lw_xml
                '</w:tbl>'
                INTO lw_xml.

    IF virtual IS SUPPLIED.
      virtual = lw_xml.
    ELSE.
      CONCATENATE mw_docxml
                  lw_xml
                  INTO mw_docxml.
    ENDIF.
  ENDMETHOD.
  METHOD write_text.
    DATA : lw_style  TYPE string,
           lw_string TYPE string,
           lw_field  TYPE string.
    DATA : lt_find_result TYPE match_result_tab,
           ls_find_result LIKE LINE OF lt_find_result,
           lw_off         TYPE i,
           lw_len         TYPE i.

* Get font style section
    IF style_effect IS SUPPLIED OR NOT style IS INITIAL.
      _build_character_style( EXPORTING style         = style
                                        style_effect  = style_effect
                              IMPORTING xml           = lw_style
                                        invalid_style = invalid_style ).
    ENDIF.

    lw_string = textline.
* Escape invalid character
    _protect_string( EXPORTING in  = lw_string
                     IMPORTING out = lw_string ).

* Replace fields in content
    IF lw_string CS '##FIELD#'.
* Regex to search all fields to replace
      FIND ALL OCCURRENCES OF REGEX '##FIELD#([A-Z ])*##' IN lw_string RESULTS lt_find_result.
      SORT lt_find_result BY offset DESCENDING.
* For each result, replace
      LOOP AT lt_find_result INTO ls_find_result.
        lw_off = ls_find_result-offset + 8.
        lw_len = ls_find_result-length - 10.
        CASE lw_string+ls_find_result-offset(ls_find_result-length).
          WHEN c_field_pagecount OR c_field_pagetotal.
            CONCATENATE lw_string+lw_off(lw_len) '\* Arabic'
                        INTO lw_field SEPARATED BY space.
          WHEN c_field_filename.
            CONCATENATE lw_string+lw_off(lw_len) '\p'
                        INTO lw_field SEPARATED BY space.
          WHEN c_field_creationdate OR c_field_moddate
          OR c_field_todaydate.
            CONCATENATE lw_string+lw_off(lw_len)
                        '\@ &quot;dd/MM/yyyy&quot;'
                        INTO lw_field SEPARATED BY space.
          WHEN OTHERS.
            lw_field = lw_string+lw_off(lw_len).
        ENDCASE.
        CONCATENATE '<w:fldSimple w:instr="'
                    lw_field
                    ' \* MERGEFORMAT"/>'
                    INTO lw_field RESPECTING BLANKS.
        REPLACE lw_string+ls_find_result-offset(ls_find_result-length)
                IN lw_string WITH lw_field.
      ENDLOOP.
    ENDIF.

* Replace label anchor by it's value
    IF NOT style_effect-label IS INITIAL AND lw_string CS c_label_anchor.
      _protect_label( EXPORTING in  = style_effect-label
                      IMPORTING out = lw_field ).
      CONCATENATE '<w:fldSimple w:instr=" SEQ '
                  lw_field
                  ' \* ARABIC "/>'
                  INTO lw_field RESPECTING BLANKS.
      REPLACE c_label_anchor IN lw_string WITH lw_field.
    ENDIF.

    IF virtual IS SUPPLIED.
      CONCATENATE '<w:r>'
                  lw_style
                  '<w:t xml:space="preserve">'
                  lw_string
                  '</w:t>'
                  '</w:r>'
                  INTO virtual RESPECTING BLANKS.

      IF    (     line_style        IS SUPPLIED AND line_style IS NOT INITIAL )
         OR (     line_style_effect IS SUPPLIED
              AND line_style_effect IS NOT INITIAL ).
        write_line( EXPORTING style         = line_style
                              style_effect  = line_style_effect
                    IMPORTING invalid_style = invalid_line_style
                    CHANGING  virtual       = virtual ).
      ENDIF.

      RETURN.
    ELSE.
      CONCATENATE mw_fragxml
                  '<w:r>'
                  lw_style
                  '<w:t xml:space="preserve">'
                  lw_string
                  '</w:t>'
                  '</w:r>'
                  INTO mw_fragxml RESPECTING BLANKS.

      IF    (     line_style        IS SUPPLIED AND line_style IS NOT INITIAL )
         OR (     line_style_effect IS SUPPLIED
              AND line_style_effect IS NOT INITIAL ).
        write_line( EXPORTING style         = line_style
                              style_effect  = line_style_effect
                    IMPORTING invalid_style = invalid_line_style ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD write_toc.
    DATA : lw_default TYPE string,
           lw_content TYPE string.

* Classic TOC
    IF label IS INITIAL.
      lw_default = '--== Table of content - please refresh ==--'.
      lw_content = '\o "1-9"'.

* Table of content for specific label (table, figure, ...)
    ELSE.
      CONCATENATE '--== Table of '
                  label
                  ' - please refresh ==--'
                  INTO lw_default RESPECTING BLANKS.

      _protect_label( EXPORTING in  = label
                      IMPORTING out = lw_content ).

      CONCATENATE '\h \h \c "'
                  lw_content
                  '"'
                  INTO lw_content.
    ENDIF.

* If specified, use given default text
    IF NOT default IS INITIAL.
      lw_default = default.
    ENDIF.

* Write TOC
    CONCATENATE mw_docxml
                '<w:p>'
                '<w:r><w:fldChar w:fldCharType="begin"/></w:r>'
                '<w:r><w:instrText> TOC '
                lw_content
                ' </w:instrText></w:r>'
                '</w:p>'

* Add a default text for initial TOC value
                '<w:p>'
                '<w:pPr><w:jc w:val="center"/></w:pPr>'
                '<w:r>'
                '<w:fldChar w:fldCharType="separate"/></w:r>'
                '<w:r>'
                '<w:rPr><w:sz w:val="36"/><w:szCs w:val="36"/></w:rPr>'
                '<w:t>'
                lw_default
                '</w:t></w:r>'
                '</w:p>'

                '<w:p>'
                '<w:r><w:fldChar w:fldCharType="end"/></w:r>'
                '</w:p>'

                INTO mw_docxml RESPECTING BLANKS.
  ENDMETHOD.
