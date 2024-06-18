module esp_comp_nuopc
  ! this is a dummy DART Component to be used to temporarily build a dummy libesp.a
  
    use ESMF             , only : ESMF_VM, ESMF_VMBroadcast
    use ESMF             , only : ESMF_Mesh, ESMF_GridComp, ESMF_SUCCESS, ESMF_LogWrite
    use ESMF             , only : ESMF_GridCompSetEntryPoint, ESMF_METHOD_INITIALIZE
    use ESMF             , only : ESMF_MethodRemove, ESMF_State, ESMF_Clock, ESMF_TimeInterval
    use ESMF             , only : ESMF_State, ESMF_Field, ESMF_LOGMSG_INFO, ESMF_ClockGet
    use ESMF             , only : ESMF_Time, ESMF_Alarm, ESMF_TimeGet, ESMF_TimeInterval
    use ESMF             , only : operator(+), ESMF_TimeIntervalGet, ESMF_ClockGetAlarm
    use ESMF             , only : ESMF_AlarmIsRinging, ESMF_AlarmRingerOff, ESMF_StateGet
    use ESMF             , only : ESMF_FieldGet, ESMF_MAXSTR, ESMF_VMBroadcast
    use ESMF             , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit, ESMF_GridCompGet
    use ESMF             , only : ESMF_KIND_R8, ESMF_LogFoundError
    use ESMF             , only : ESMF_LOGERR_PASSTHRU, ESMF_LOGWRITE
    
    
    use NUOPC            , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
    use NUOPC            , only : NUOPC_CompAttributeGet, NUOPC_Advertise
    use NUOPC            , only : NUOPC_CompFilterPhaseMap
    
    
    use NUOPC_Model      , only : model_routine_SS        => SetServices
    use NUOPC_Model      , only : model_label_Advance     => label_Advance
    use NUOPC_Model      , only : model_label_SetRunClock => label_SetRunClock
    use NUOPC_Model      , only : model_label_Finalize    => label_Finalize
    use NUOPC_Model      , only : NUOPC_ModelGet, setVM
  
    implicit none
  
    private ! except
  
    public  :: SetServices
    public  :: SetVM
  
    character(len=*),parameter :: u_FILE_u = &
       __FILE__
  
    !----------------------------------------------------------------------------------
    contains
    !----------------------------------------------------------------------------------
    ! The user-written part of Gridded component is associated with an ESMF_GridComp  !
    ! derived type through a routine called ESMF_SetServices(). A Gridded component   !
    ! is a computational entity which consumes and produce data. It uses a State      !
    ! object to manage time, and a VM to describe its own and its child components'   !
    ! computational resources.                                                        !
  
    subroutine SetServices(dgcomp, rc)
      ! In Fortran, you declare a variable of the derived type and then set its attributes.
      type(ESMF_GridComp)  :: dgcomp ! ESMF gridded component that represent DART which has specific computational function.  
      integer, intent(out) :: rc
  
      rc = ESMF_SUCCESS
  
      ! here goes all the local variables
      character(len=*),parameter  :: subname='(DART_cap:SetServices)'
  
  
  
  
  
      call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)
  
      ! derive from NUOPC_Model
      call NUOPC_CompDerive(dgcomp, model_routine_SS, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return ! bail out
  
      ! specialize the derived grid component, in this case dart grid component
      call NUOPC_CompSpecialize(dgcomp, specLabel=label_Advertise, &
        specRoutine=Advertise, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out
  
      call NUOPC_CompSpecialize(dgcomp, specLabel=label_RealizrProvided, &
        specRoutine=Realize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
      return ! bail out
  
      call NUOPC_CompSpecialize(dgcomp, specLabel=label_Advance, &
        specRoutine=Advance, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
      return ! bail out
  
      call NUOPC_CompSpecialize(dgcomp, specLabel=label_Finalize, &
        specRoutine=Finalize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
      return ! bail out
  
    end subroutine SetServices
  
    subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gcomp                    !< ESMF_GridComp object
      type(ESMF_State)      :: importState, exportState !< ESMF_State object for
                                                        !! import/export fields
      type(ESMF_Clock)      :: clock                    !< ESMF_Clock object
      integer, intent(out)  :: rc                       !< return code
  
      rc = ESMF_SUCCESS
  
      ! Switch to IPDv03 by filtering all other phaseMap entries
      call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
           acceptStringList=(/"IPDv03p"/), rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end subroutine InitializeP0
  
    subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)            :: gcomp                    !< ESMF_GridComp object
      type(ESMF_State)               :: importState, exportState !< ESMF_State object for
                                                                 !! import/export fields
      type(ESMF_Clock)               :: clock                    !< ESMF_Clock object
      integer, intent(out)           :: rc                       !< return code
  
      rc = ESMF_SUCCESS
    end subroutine InitializeAdvertise
  
    subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)  :: gcomp                    !< ESMF_GridComp object
      type(ESMF_State)     :: importState, exportState !< ESMF_State object for
                                                       !! import/export fields
      type(ESMF_Clock)     :: clock                    !< ESMF_Clock object
      integer, intent(out) :: rc                       !< return code
  
      rc = ESMF_SUCCESS
    end subroutine InitializeRealize
    
    subroutine ModelAdvance(gcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState, exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
  
      rc = ESMF_SUCCESS
  
      ! Import data
      call ESMF_StateGet(importState, "temperature", temperatureData, rc=rc)
      if (ChkErr(rc,__LINE__,__FILE__)) return
  
      ! Process data (e.g., model time-stepping, physics)
      call ModelStep(temperatureData, processedData, rc=rc)
      if (ChkErr(rc,__LINE__,__FILE__)) return
  
      ! Export data
      call ESMF_StateSet(exportState, "temperature", processedData, rc=rc)
      if (ChkErr(rc,__LINE__,__FILE__)) return
    end subroutine ModelAdvance
    
    ! Finalization phase invloves cleaning up the resources
    subroutine Finalize(gcomp, rc)
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
  
      rc = ESMF_SUCCESS
  
      ! Finalize model component
      call ESMF_Finalize(gcomp, rc=rc)
      if (ChkErr(rc,__LINE__,__FILE__)) return
    end subroutine Finalize
  
  
  !> Returns true if ESMF_LogFoundError() determines that rc is an error code. Otherwise false.
  logical function ChkErr(rc, line, file)
    integer, intent(in) :: rc            !< return code to check
    integer, intent(in) :: line          !< Integer source line number
    character(len=*), intent(in) :: file !< User-provided source file name
    integer :: lrc
    ChkErr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
      ChkErr = .true.
    endif
  end function ChkErr
  
  end module esp_comp_nuopc
  
  
  
  
  
  !!! modified Realize subroutine !!!
  !! To modify the RealizeProvided subroutine for a DART NUOPC cap where it's specifically designed 
  !! to accept state variables from an ocean model on the same grid, we need to focus on the interaction 
  !! where DART acts as a receiver of the grid and associated fields. The following modifications are 
  !! tailored to reflect a scenario where DART realizes fields provided by the ocean model, using the 
  !! ocean model's grid without altering it.
  
  subroutine RealizeProvided(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
  
    ! Local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: receivedGrid
    character(ESMF_MAXSTR)  :: transferAction
  
    rc = ESMF_SUCCESS
  
    ! Retrieve import and export states from the model
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
  
    ! Iterate through key fields that DART will use from the ocean model
    ! Example: Sea Surface Temperature (SST)
    call ESMF_StateGet(importState, field=field, itemName="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
  
    ! Check the transfer action to confirm if the ocean model is providing the grid
    call NUOPC_GetAttribute(field, name="ConsumerTransferAction", &
      value=transferAction, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return
  
    ! Accept the grid only if the action is 'provide'
    if (trim(transferAction) == "provide") then
      ! Get the grid associated with the field
      call ESMF_FieldGet(field, grid=receivedGrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
  
      ! Log that DART is accepting the grid
      call ESMF_LogWrite("DART is accepting the provided Grid for Field 'sst'.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
  
      ! Realize the field on the received grid
      call NUOPC_Realize(importState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return
    endif
  
    ! Additional fields can be handled similarly
    ! Include handling for other fields such as Sea Surface Salinity (SSS) or others
  
  end subroutine
  