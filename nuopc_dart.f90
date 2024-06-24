module dart_comp_nuopc
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
        specRoutine=InitializeAdvertise, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out
  
      call NUOPC_CompSpecialize(dgcomp, specLabel=label_RealizrProvided, &
        specRoutine=InitializeRealize, rc=rc)
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
  
  
  
  
    subroutine InitializeAdvertise(dgcomp, rc)
      type(ESMF_GridComp)            :: dgcomp                   !< ESMF_GridComp object
      integer, intent(out)           :: rc                       !< return code
  
      ! local variables
      type(ESMF_State)               :: importState, exportState ! ESMF state object for 
                                                                 ! import/export fields
      type(ESMF_Clock)               :: clock
                                                          
      rc = ESMF_SUCCESS
  
      ! query for importState and exportState to ensure that data dependencies (import and export states) 
      ! are correctly initialized and available before the model performs any computations that depend on 
      ! them.
      call NUOPC_ModelGet(dgcomp, importState=importState, &
        exportState=exportable, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LogFoundError, &
        line=__LINE__, &
        file=__FILE__))&
        return ! bail out
      
  
  
      ! If it founds a import and export state then the following code would execute
      call NUOPC_Advertise(importState, &
        StandardName="temperature", name="temp", &
        TransferOfferGeomObject="cannot provide", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
        return ! bail out

      call NUOPC_Advertise(exportState, &
        StandardName="temperature", name="temp", &
        TransferOfferGeomObject="cannot provide", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
        return ! bail out
  
      
    end subroutine InitializeAdvertise
  
  
  
  
    subroutine InitializeRealize(dgcomp, rc)
      type(ESMF_GridComp)    :: dgcomp                    !< ESMF_GridComp object
      type(ESMF_State)       :: importState, exportState !< ESMF_State object for
                                                       !! import/export fields
      type(ESMF_Grid)        :: gridIn, gridOut
      type(ESMF_Clock)       :: clock                    !< ESMF_Clock object
      type(ESMF_Field)       :: field
      character(ESMF_MAXSTR) :: transferAction
      integer, intent(out)   :: rc                       !< return code
  
      rc = ESMF_SUCCESS ! initial setup: the return code 'rc' is initialized to 'ESMF_SUCCESS'
  
      ! query for importState and exportState
      call NUOPC_ModelGet(dgcomp, importState=importState, &
        exportState=exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        file=__FILE__,&
        line=__LINE__))&
        return ! bail out
  
      ! create Grid objects for import Fields
      gridIn = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/100, 150/), &
        minCornerCoord=(/0._ESMF_KIND_R8, -60._ESMF_KIND_R8/), &
        maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
        staggerLocList=(/ESMF_STAGGERLOC_CENTER/), name="OCN-GridIn", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        file=__FILE__, &
        line=__LINE__))&
        return ! bail out
  
      gridOut = gridIn ! grid in should be equal to grid out
  
      ! Get field from the import state
      call ESMF_StateGet(importState, field=field, itemName="temp", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, & 
        file=__FILE__))&
        return ! bail out
  
      call NUOPC_GetAttribute(field, name="ConsumerTransferAction", &
        value=transferAction, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
        return ! bail out
      if (trim(transferAction)=="provide") then
        ! the Connector instructed the DART to provide the Grid object for "temp"
        call ESMF_LOGWRITE("DART is providing Grid for Field 'temp'.", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__))&
          return ! bail out
  
        call NUOPC_Realize(importState, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__))&
          return ! bail out
      else ! transferAction=="accept"
        ! the connector instructed the DART to accept the Grid from OCN for "temp"
        call ESMF_LOGWRITE("DART is accepting Grid for the Field 'temp'.", &
          ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__))&
          return ! bail out 
      endif

      !! Realizing Export Field

      call ESMF_LogWrite("DART is providing Grid for Field 'Salinity'.", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
        return ! bail out

      field = ESMF_FieldCreate(name="sal", grid=gridOut, &
        typekind=ESMF_TYPEKIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out

      call NUOPC_Realize(exportState, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out

      call ESMF_LOGWRITE("Done realizing fields in DART import/exportStates"// &
      "that do not need grid grid transfer, all Grids are provided by DART", ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out


    end subroutine InitializeRealize
    

    ! In this routine the DART component can access the transferred grid/mesh/locstream on the field that have "accept" value. 
    ! However, only the DistGrid, i.e, the decomposition and distribution information of the grid/mesh/locstream is available at 
    ! this stage, not the full physical grid information such as the coordinates.
    subroutine AcceptTransfer(dgcomp, rc)
      type(ESMF_GridComp)   :: dgcomp
      integer, intent(out)  :: rc  

      !local variable
      type(ESMF_State)             :: importState, exportState
      type(ESMF_Field)             :: field
      type(ESMF_Grid)              :: grid
      integer                      :: localDeCount
      character(80)                :: name
      character(160)               :: msgString

      type(ESMF_DistGrid)          :: distgrid
      integer                      :: dimCount, tileCount, arbDimCount
      integer, allocatable         :: minIndexPTile(:,:), maxIndexPTile(:,:)
      integer                      :: connectionCount
      type(ESMF_DistGridConnection), allocatable :: connectionList(:)
      charachter(ESMF_MAXSTR)      :: transferAction
      logical                      :: regDecompFlag

      rc= ESMF_SUCCESS

      ! query for import state and export state
      call NUOPC_ModelGet(dgcomp, importState=importState, &
        exportState=exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! accecc the "temp" field in the exportState, and ESMF_StateGet is a function to retrieve a field 
      ! from a state (in this case exportState). When it is called it searches for a field with the name
      ! "temp" within exportState. If the field is found, it is assigned to the variable 'field'. This means
      ! 'field' now references the "temp" field from the exportState.
      call ESMF_StateGet(exportState, field=field, itemName="temp", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! while this is still an empty field, it hold a Grid with DistGrid. This step would retrieve the grid 
      ! associated with the 'field' (which is now "temp")
      call ESMF_FieldGet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! check the grid name and print it out, this is just a step in logging the information.
      call ESMF_GridGet(grid, name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      write(msgString,*) "DART - InitializeP4: transferred Grid name= ", name
      call ESMF_LOGWRITE(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! access localDeCount to show this is a real Grid. By checking `localDeCount`, the code verifies that the grid is not just
      ! an empty placeholder but a real, decomposed grid. The logged `localDeCount` helps confirm this in the log output.
      call ESMG_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      write(msgString, *) "DART - InitializeP4: localDeCount = ", localDeCount
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out


      ! At this stage DART can modify the distribution and decomposition information of the retrieved grid by replacing the DistGrid
      ! object in the grid. The DistGrid that is set on the grid/mesh/locstream obejcts when leaving the model/mediator phase 
      ! label_AcceptTransfer will consequently be used by the generic CONNECTOR to fully transfer the Grid/Mesh/Locstream object.
      ! The fully trasferred objects are available on the Fields with "accpet" during Model/Mediator phase label_RealizeAccepted, where
      ! they are used to realize the respective Field objects.

      ! Create a custom DistGrid, based on the minIndex and maxIndex of the accpeted DistGrid, but with a default regDecomp
      ! (regular decomposition- its a way to partition the grid into regular subdomains. This is typically simpler and more
      ! efficient than arbitrary decompositions) for the current VM that leads to 1DE/PET (1 decomposition per processing
      ! element (PET)). If you want to add routine for arbitrary decomposition you can do it here!
      
      ! get dimCount and tileCount
      call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
        connectionCount=connectionCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out 

      ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
      allocate(minIndexPTile(dimCount, tileCount), &
        maxIndexPTile(dimCount, tileCount)) 
      allocate(connectionList(connectionCount))

      !! `dimCount`: Number of dimensions in the grid (e.g, for a 2D grid, 3 for a 3D grid)
      !! `connectionCount`: Number of connections or adjacency relationships between tiles or decompotion elements in the grid,
      !! used for managing data exchange and communication. 

      ! get minIndex and maxIndex arrays to populate memory that allocated above!
      call ESMF_DistGridGet(distgrid, maxIndexPTile=maxIndexPTile, &
        minIndexPTile=minIndexPTile, connectionList=connectionCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
      ! but use default multi-tile regDecomp if the default regDecomp is not suitable, a custom one could be set
      ! up here and used.
      distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
        maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      deallocate(minIndexPTile, maxIndexPTile, connectionList) ! deallocate the memory because we already used the array to form distgrid

      ! Create a new Grid object based on the newly created distribution grid (`DistGrid`)
      grid = ESMF_GridCreate(distgrid, name="DRT-custom-"//trim(name), rc=rc)     !`distgrid` describes how the overall grid is partitioned into smaller subdomains!
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! access localDeCount of the final Grid, this would retrieve local decomposition count of the newly created grid.
      call ESMF_GridGet(grid, localDeCount=localDeCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
      !! The `localDeCount` represents the number of decomposition elements (DEs) that are local to the current processing elements (PET).
      !! This count is important for verifying that the grid has been properly decomposed and distributed. It provides insight into the 
      !! workload distribution among the available PETs

      write (msgString,*) "DART - InitializeP4: final Grid localDeCount = ", &
        localDeCount
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Swap out the transferred for new Grid in "temp" Field
      call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      !! The grid received from another model component is tailored to the computational resources and domain decomposition of the sending model. 
      !! However, to optimize performance and ensure compatibility with the receiving model's computational framework, the grid often needs to be customized.
      !! This is the reason to swap the trsnferred grid to the new grid created.

      !------------- transferred grids in the importState ----------------------------------------------------

      call ESMF_LogWrite("DART - InitializeP4: grid transfer for importState", &
        ESMF_LOGMSG_INFO, rc=rc)

      ! access the "temp" field in the importState and set the Grid 
      call ESMF_StateGet(importState, field=field, itemName="temp", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
      ! construct a local Grid according to the transferred grid
      call ESMF_FieldGet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
        
      call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, &
        connectionCount=connectionCount, regDecompFlag=regDecompFlag, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      
      allocate(minIndexPTile(dimCount, tileCount), &
        maxIndexPTile(dimCount, tileCount))
      allocate(connectionList(connectionCount))

      call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
        maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (regDecompFlag) then
        ! The provider used a regular decomposition (regDecomp) for the DistGrid:
        ! This means that the entire grid is covered without any gaps,
        ! so it is easiest to use a regular decomposition (regDecomp) on the receiving side too.

        distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
          maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        grid = ESMF_GridCreate(distgrid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        ! now swap out the transferred grid for the newly created one
        call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call ESMF_LogWrite("DART - Done with setting the Grid for `temp` field", &
          ESMF_LOGMSG_INFO, rc=rc)

      else 

        ! Directly use the provided grid
        grid = ESMF_GridCreate(distgrid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          file=__FILE__, &
          line=__LINE__))&
          return ! bail out

        ! swap out the transferred grid for the newly created one
        call ESMF_FieldEmptySet(field=field, grid=grid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          file=__FILE__, &
          line=__LINE__))&
          return ! bail out
        
        call ESMF_LogWrite("DART - Just set Grid for 'temp' field using deBlock scheme", &
          ESMF_LOGMSG_INFO, rc=rc)
        
        
      endif

      deallocate(minIndexPTile, maxIndexPTile, connectionList)

      call ESMF_LogWrite("DART - InitializeP4: DONE!", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    end subroutine


    !----------------------------------------------------------------------------------------------

    subroutine RealizeAccepted(dgcomp, rc)
      type(ESMF_GridComp)       :: dgcomp
      integer, intent(out)      :: rc

      ! local variables
      type(ESMF_State)          :: importState, exportState
      type(ESMF_Field)          :: field
      type(ESMF_Grid)           :: grid
      type(ESMF_Array)          :: array
      character(80)             :: name
      character(160)            :: msgString
      integer                   :: staggerEdgeLWidth(2)
      integer                   :: staggerEdgeUWidth(2)
      integer                   :: staggerAlign(2)

      rc = ESMF_SUCCESS
      
      ! query for the importState and exportState
      call NUOPC_ModelGet(importState=importState, exportState=exportState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out


      ! realize the 'temp' field in the importState
      call NUOPC_Realize(importState, fieldName="temp", field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! log a message about the field
      if (ESMF_FieldIsCreated(field, rc=rc)) then
        write (msgString, *) "DART - Just realized the 'temp' Field in importState."
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else
        write(msgString,*) "DART - 'temp' Field not realized in importState."
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif


#ifdef TEST_MULTI_TILE_GRID
      ! write cubed sphere grid out to VTK

      ! Get the grid associated with the field
      call ESMF_FieldGet(field, grid=grid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Write the grid to a VTK file for visualization
      call ESMF_GridWriteVTK(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="DART-accepted-Grid-temp_centers", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
#endif

      
      ! realize the "temp" field in the exportState with specified totalLWidth/totalLWidth
      ! this is th halo region, the number of extra layers of grid points surrounding the
      ! the computational domain. One shown below!
      !  |-----|-----|-----|-----|-----|
      !  | H11 | H12 | H13 | H14 | H15 |
      !  |-----|-----|-----|-----|-----|
      !  | H21 | G11 | G12 | G13 | H25 |
      !  |-----|-----|-----|-----|-----|
      !  | H31 | G21 | G22 | G23 | H35 |
      !  |-----|-----|-----|-----|-----|
      !  | H41 | G31 | G32 | G33 | H45 |
      !  |-----|-----|-----|-----|-----|
      !  | H51 | H52 | H53 | H54 | H55 |
      !  |-----|-----|-----|-----|-----|

      call NUOPC_Realize(exportState, fieldName="temp", &
        totalLWidth=(/1,1/), totalUWidth=(/1,1/), field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
        return ! bail out
      
      ! log a message
      if (ESMF_FieldIsCreated(field, rc)) then
        write (msgString, *) "DART - Just realized the 'temp' Field in exportState."
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__))&
          return ! bail out 
      else
        write(msgString, *) "DART - 'temp' field not realized in exportState."
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__))&
          return ! bail out
      endif

      ! after realizing the fields which had accept grid as status, let's inspect the Grid name
      call ESMF_FieldGet(field, grid=grid, rc=rc) 
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
        return ! bail out
      
      call ESMF_GridGet(grid, name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
        return ! bail out
      
      write (msgString, *) "DART - InitializeP5: transferred Grid name = ", name
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__))&
        return ! bail out

#if 1
      ! write out the Grid into VTK file for inspection
      call ESMF_GridWriteVTK(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="DART-accepted-Grid-temp_centers", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_LogWrite("Done writing DART-accepted-Grid-temp_centers VTK", &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
#endif
    end subroutine 
        

        ! representation of staggered grid
        !*-------*-------*------ * |
        !|       |       |       | |
        !|  +    |   +   |   +   | |
        !*------ *------ *------ * |
        !|       |       |       | |
        !|   +   |   +   |   +   | |
        !*------ *------ *------ * |
        !|       |       |       | |
        !|    +  |   +   |   +   | |
        !*------ *------ *------ * |
        

    subroutine DataInitialize(dgcomp, rc)
      type(ESMF_GridComp)              ::
      integer, intent(out)             ::

      ! local variables
      type(ESMF_State)                 ::
      type(ESMF_Field)                 ::
      real(kind=ESMF_KIND_R8), pointer ::
      integer                          ::
      integer                          ::






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
  
end module dart_comp_nuopc
  
  
  
  
  
