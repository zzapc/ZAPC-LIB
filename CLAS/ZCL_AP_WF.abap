TYPE-POOLS: swlc.
include: CNTN01_SWC.

*INCLUDE <cntain>.

*************************** Macros *************************************
* Types
*TYPES:
*  swc_object type swotobjid.
*
*DATA swo_%handle type obj_record.
*DATA swo_%ret type swotreturn.

*DATA BEGIN OF swo_%return.
*  INCLUDE STRUCTURE swotreturn.
*DATA END OF swo_%return.

* DATA Container
DEFINE swc_container.
data begin of &1 occurs 0.
  include structure swcont.
data end of &1.
END-OF-DEFINITION.
************************* Container Macros *****************************
* Create Container
DEFINE swc_create_container.
  clear &1.
  refresh &1.
END-OF-DEFINITION.

* Release Container
DEFINE swc_release_container.
  clear &1.
  refresh &1.
END-OF-DEFINITION.

* Clear Container
DEFINE swc_clear_container.
  refresh &1.
  clear &1.
END-OF-DEFINITION.

* Container Create Element
DEFINE swc_create_element.
  call function 'SWC_ELEMENT_CREATE'
       exporting
            element   = &2
       tables
            container = &1
       exceptions already_exists = 4
                  others = 1.
END-OF-DEFINITION.

* Container Set Element
DEFINE swc_set_element.
  call function 'SWC_ELEMENT_SET'
       exporting
            element       = &2
            field         = &3
       tables
            container     = &1
       exceptions others = 1.
END-OF-DEFINITION.

* Container Set Table
DEFINE swc_set_table.
  call function 'SWC_TABLE_SET'
       exporting
            element       = &2
       tables
            container     = &1
            table         = &3
       exceptions others = 1.
END-OF-DEFINITION.

* Container Get Element
DEFINE swc_get_element.
  call function 'SWC_ELEMENT_GET'
       exporting
            element       = &2
       importing
            field         = &3
       tables
            container     = &1
       exceptions not_found = 8
                  is_null   = 4
                  others = 1.
END-OF-DEFINITION.

* Container Get Table
DEFINE swc_get_table.
  call function 'SWC_TABLE_GET'
       exporting
            element       = &2
       tables
            container     = &1
            table         = &3
       exceptions not_found = 8
                  is_null   = 4
                  others = 1.
END-OF-DEFINITION.

* Container Löschen Element
DEFINE swc_delete_element.
  call function 'SWC_ELEMENT_DELETE'
       exporting
            element   = &2
       tables
            container = &1
       exceptions others = 1.
END-OF-DEFINITION.

* Kopieren Container1/Element1 -> Container2/Element2
* Parameters:  P1  Quellcontainer
*              P2  Quellelement
*              P3  Zielcontainer
*              P4  Zielelement
DEFINE swc_copy_element.
  call function 'SWC_ELEMENT_COPY'
       exporting
            source_element   = &2
            target_element   = &4
       tables
            source_container = &1
            target_container = &3
       exceptions
            not_found        = 1
            is_null          = 2
            others           = 3.
END-OF-DEFINITION.


*************** Container Object Macros ********************************
DEFINE swc_create_object.
  call function 'SWC_OBJECT_CREATE'
       exporting
            objtype           =  &2
            objkey            =  &3
       importing
            object            =  &1
       exceptions
            objtype_not_found    = 04
            logsys_not_found     = 08
            objtype_not_released = 12.
END-OF-DEFINITION.

DEFINE swc_get_object_type.
   &2 = &1-objtype.
END-OF-DEFINITION.

DEFINE swc_get_object_key.
   &2 = &1-objkey.
END-OF-DEFINITION.

* Get Property
DEFINE swc_get_property.
  call function 'SWO_PROPERTY_GET'
       exporting
            object          = &1
            attribute       = &2
       changing
            value           = &3
       exceptions
            error_create    = 01
            error_invoke    = 02
            error_container = 03.
END-OF-DEFINITION.

* Get Table Property
DEFINE swc_get_table_property.
  call function 'SWO_TABLE_PROPERTY_GET'
       exporting
            object          = &1
            attribute       = &2
       tables
            value           = &3
       exceptions
            error_create    = 01
            error_invoke    = 02
            error_container = 03.
END-OF-DEFINITION.

* Refresh Object Properties
*DATA: swo_%obj LIKE obj_record.
DEFINE swc_refresh_object.
  call function 'SWO_CREATE'
       exporting
            objtype = &1-objtype
            objkey  = &1-objkey
       importing
            object  = swo_%handle
            return  = swo_%return.
  if swo_%return = 0.
    swo_%obj-handle = swo_%handle.
    call function 'SWO_OBJECT_REFRESH'
         exporting
              object       = swo_%obj
         exceptions
              error_create = 02.
  else.
    sy-subrc = 02.
  endif.
END-OF-DEFINITION.

* Compress container
* Parameters:   P   container,     Changing
DEFINE swc_container_compress.
  call function 'SWC_CONT_COMPRESS'
    tables
      container       = &1.
END-OF-DEFINITION.
class ZCL_AP_WF definition
  public
  final
  create public .

public section.

  types SWC_OBJECT type SWOTOBJID .

  class-methods GET_ID
    importing
      !CLAVE type ANY
      !TASK type ANY
      !TASK2 type ANY optional
    returning
      value(WI_ID) type SWW_WI2OBJ-WI_ID .
  class-methods FINALIZAR_ID
    importing
      !ID type SWW_WI2OBJ-WI_ID .
  class-methods GET_STATUS
    importing
      !ID type SWW_WI2OBJ-WI_ID
    returning
      value(STATUS) type SWLC_WORKITEM-WI_STAT .
  class-methods GET_SIGUIENTE_USUARIO
    importing
      !ID type SWW_WI2OBJ-WI_ID
    returning
      value(USUARIO) type SY-UNAME .
  class-methods GET_AGENTES
    importing
      !ID type SWW_WI2OBJ-WI_ID
    returning
      value(I_AGENTES) type TSWHACTOR .
  class-methods VISUALIZAR
    importing
      !ID type SWW_WI2OBJ-WI_ID .
  class-methods GET_SIGUIENTES_USUARIOS
    importing
      !ID type SWW_WI2OBJ-WI_ID
    returning
      value(USUARIOS) type HRASR00SYUNAME_TAB .
  class-methods LANZAR_EVENTO
    importing
      !OBJETO type ANY
      !CLAVE type ANY
      !EVENTO type ANY
      !COMMIT type ABAP_BOOL default 'X'
      !DELAY type ABAP_BOOL default ''
      !UPDATE_TASK type ABAP_BOOL default ''
    returning
      value(ERROR) type ABAP_BOOL .
  class-methods INICIAR_WF
    importing
      !TASK type SWR_STRUCT-TASK
      !TIPO type SWOTOBJID-OBJTYPE
      !CLAVE type ANY
      !I_AGENTES type SWRTAGENT optional
      !USUARIO type SY-UNAME optional
      !ELEMENTO_CLAVE type ANY default ''
      !I_CONTAINER type SWRTCONT optional
    exporting
      !I_MSG type SAPI_MSG_LINES
      !WORKITEM_ID type SWR_STRUCT-WORKITEMID
      !RC type SY-SUBRC
      !MESSAGE type BAPI_MSG .
protected section.
private section.
endclass. "ZCL_AP_WF definition
class ZCL_AP_WF implementation.
METHOD finalizar_id.
  TRY.
      CALL FUNCTION 'SWW_WI_ADMIN_COMPLETE'
        EXPORTING
          wi_id                 = id
          do_commit             = 'X'
          authorization_checked = ''
          return_code           = '0000'
*          importing
*         new_status            = i_listado-wi_stat
        EXCEPTIONS
          update_failed         = 01
          no_authorization      = 02.
    CATCH cx_root INTO DATA(o_root).
  ENDTRY.

ENDMETHOD.
method GET_AGENTES.

  CLEAR i_agentes.
  CALL FUNCTION 'RH_WI_AGENTS_GET'
    EXPORTING
      act_wi_id       = id
    TABLES
      wi_agent_list   = i_agentes
    EXCEPTIONS
      no_active_plvar = 1
      no_agent_found  = 2
      general_task    = 3
      background_task = 4
      OTHERS          = 5.

endmethod.
METHOD get_id.
  DATA: r_task     TYPE RANGE OF sww_wi2obj-wi_rh_task,
        lr_task    LIKE LINE OF r_task,
        l_crea_tmp TYPE sww_wi2obj-crea_tmp.

  CLEAR lr_task.
  lr_task-option = 'EQ'.
  lr_task-sign = 'I'.
  lr_task-low = task.
  APPEND lr_task TO r_task.

  IF NOT task2 IS INITIAL.
    lr_task-low = task2.
    APPEND lr_task TO r_task.
  ENDIF.
  IF clave CS '%'.
    SELECT wi_id crea_tmp FROM sww_wi2obj               "#EC CI_NOFIRST
      INTO (wi_id, l_crea_tmp)
     WHERE wi_rh_task IN r_task
       AND instid LIKE clave
     ORDER BY crea_tmp.
    ENDSELECT.
  ELSE.
    SELECT wi_id crea_tmp FROM sww_wi2obj               "#EC CI_NOFIRST
      INTO (wi_id, l_crea_tmp)
     WHERE wi_rh_task IN r_task
       AND instid = clave
     ORDER BY crea_tmp.
    ENDSELECT.
  ENDIF.
ENDMETHOD.
method GET_SIGUIENTE_USUARIO.
  DATA: i_usuarios TYPE hrtb_objid.

  i_usuarios = get_siguientes_usuarios( id ).

  CLEAR usuario.
  READ TABLE i_usuarios INTO usuario INDEX 1.

endmethod.
METHOD get_siguientes_usuarios.
  DATA: l_swpsteplog TYPE swpsteplog,
        l_id         TYPE sww_wi2obj-wi_id,
        i_agentes    TYPE tswhactor,
        l_agente     TYPE swhactor.

  CLEAR usuarios.

  CLEAR l_swpsteplog.
  SELECT wi_id FROM swpsteplog
    INTO l_swpsteplog-wi_id
   WHERE wf_id = id
     AND wi_agent = ''
   ORDER BY wi_id.
  ENDSELECT.
  IF sy-subrc NE 0.
    SELECT wf_id FROM swpsteplog                        "#EC CI_NOFIELD
      INTO l_id
      UP TO 1 ROWS
     WHERE wi_id = id
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      CLEAR l_swpsteplog.
      SELECT wi_id FROM swpsteplog
        INTO l_swpsteplog-wi_id
       WHERE wf_id = l_id
         AND wi_agent = ''
       ORDER BY wi_id.
      ENDSELECT.
    ENDIF.
  ENDIF.
  IF NOT l_swpsteplog-wi_id IS INITIAL.
    i_agentes = get_agentes( l_swpsteplog-wi_id ).
    IF sy-subrc = 0.
      LOOP AT i_agentes INTO l_agente WHERE otype = 'US'.
        APPEND l_agente-objid TO usuarios.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMETHOD.
method GET_STATUS.
data l_wf_workitem type  swlc_workitem.

  call function 'SWL_WI_HEADER_READ'
    exporting
      wi_id    = id
    changing
      workitem = l_wf_workitem
    exceptions
      others   = 1.
  if sy-subrc = 0.
    status = l_wf_workitem-wi_stat.
  endif.

endmethod.
  METHOD iniciar_wf.
    DATA: objeto         TYPE swc_object,
          wf_cont        TYPE TABLE OF swr_cont,
          agents         TYPE TABLE OF swragent,
          message_struct TYPE TABLE OF swr_mstruc,
          key(255).

    wf_cont = i_container.

    IF NOT elemento_clave IS INITIAL.
      key = clave.
      swc_create_object objeto tipo key.
      APPEND VALUE #( element = elemento_clave value = objeto ) TO wf_cont.
    ENDIF.


    agents = i_agentes.

    IF NOT usuario IS INITIAL.
      APPEND VALUE #( otype = 'US' objid = usuario ) TO agents.
    ENDIF.

    CALL FUNCTION 'SAP_WAPI_START_WORKFLOW'
      EXPORTING
        task            = task
*       LANGUAGE        = SY-LANGU
      IMPORTING
        return_code     = rc
        workitem_id     = workitem_id
      TABLES
        input_container = wf_cont
        agents          = agents
        message_lines   = i_msg
        message_struct  = message_struct.

    LOOP AT i_msg ASSIGNING FIELD-SYMBOL(<msl>) WHERE msg_type = 'E'.
      CONCATENATE message <msl>-line INTO message SEPARATED BY space.
    ENDLOOP.
    CONDENSE message.

  ENDMETHOD.
METHOD lanzar_evento.
  DATA: objtype         TYPE swetypecou-objtype,
        event           TYPE swetypecou-event,
        creator         TYPE swhactor,
        objkey          TYPE sweinstcou-objkey,
        eventid         TYPE swedumevid-evtid,
        i_container     TYPE TABLE OF swcont,
        l_container     TYPE swcont,
        event_container TYPE TABLE OF swcont,
        l_utask         TYPE tfdir-utask.

  objtype = objeto.
  objkey = clave.
  event   = evento.

  creator-otype = 'US'.
  creator-objid = sy-uname.


  IF update_task IS INITIAL.
    CALL FUNCTION 'SWE_EVENT_CREATE'
      EXPORTING
        objtype           = objtype
        objkey            = objkey
        event             = event
        creator           = creator
        start_with_delay  = delay
      IMPORTING
        event_id          = eventid
      TABLES
        event_container   = i_container
      EXCEPTIONS
        objtype_not_found = 01.
  ELSE.
    SELECT SINGLE utask FROM tfdir
      INTO l_utask
     WHERE funcname = 'SWE_EVENT_CREATE_FOR_UPD_TASK'.
    IF l_utask IS INITIAL.
      CALL FUNCTION 'SWE_EVENT_CREATE_FOR_UPD_TASK'
        EXPORTING
          objtype           = objtype
          objkey            = objkey
          event             = event
*         CREATOR           = ' '
          start_with_delay  = delay
*         TAKE_WORKITEM_REQUESTER       = ' '
        TABLES
          event_container   = event_container
        EXCEPTIONS
          objtype_not_found = 1.
    ELSE.
      CALL FUNCTION 'SWE_EVENT_CREATE_FOR_UPD_TASK' IN UPDATE TASK
        EXPORTING
          objtype           = objtype
          objkey            = objkey
          event             = event
          creator           = creator
*         TAKE_WORKITEM_REQUESTER = ' '
          start_with_delay  = delay
*         START_RECFB_SYNCHRON    = ' '
*         DEBUG_FLAG        = ' '
*    IMPORTING
*         EVENT_ID          =
        TABLES
          event_container   = event_container
        EXCEPTIONS
          objtype_not_found = 1
          OTHERS            = 2.
    ENDIF.
  ENDIF.
  IF sy-subrc NE 0.
    error = 'X'.
  ELSE.
    IF commit = 'X' AND update_task = ''.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.


ENDMETHOD.
method VISUALIZAR.
  IF NOT id IS INITIAL.
    CALL FUNCTION 'SWL_WI_DISPLAY'
      EXPORTING
        wi_id       = id
      EXCEPTIONS
        read_failed = 1
        OTHERS      = 2.
  ENDIF.
endmethod.
