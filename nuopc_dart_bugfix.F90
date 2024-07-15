# test code 
module dart_comp_nuopc
  ! this is a dummy DART Component to be used to temporarily build a dummy libesp.a
 
    use ESMF  !  HK might be best to have use ESMF, use NUOPC while developing

    use ESMF             , only : ESMF_VM, ESMF_VMBroadcast
    use ESMF             , only : ESMF_Mesh, ESMF_GridComp, ESMF_SUCCESS, ESMF_Grid, ESMF_DistGrid, ESMF_DistGridConnection
    use ESMF             , only : ESMF_GridCompSetEntryPoint, ESMF_METHOD_INITIALIZE, ESMF_DistGridCreate
    use ESMF             , only : ESMF_MethodRemove, ESMF_State, ESMF_Clock, ESMF_TimeInterval
    use ESMF             , only : ESMF_Field, ESMF_LOGMSG_INFO, ESMF_ClockGet
    use ESMF             , only : ESMF_Time, ESMF_Alarm, ESMF_TimeGet
    ! use ESMF             , only : operator(+), ESMF_TimeIntervalGet
    use ESMF             , only : ESMF_TimeIntervalGet, ESMF_ClockGetAlarm
    use ESMF             , only : ESMF_AlarmIsRinging, ESMF_AlarmRingerOff, ESMF_StateGet
    use ESMF             , only : ESMF_FieldGet, ESMF_MAXSTR, ESMF_VMBroadcast, ESMF_Array
    use ESMF             , only : ESMF_TraceRegionEnter, ESMF_TraceRegionExit, ESMF_GridCompGet
    use ESMF             , only : ESMF_KIND_R8, ESMF_LogFoundError
    use ESMF             , only : ESMF_LOGERR_PASSTHRU, ESMF_LOGWRITE
    
    
    use NUOPC            , only : NUOPC_CompDerive, NUOPC_CompSetEntryPoint, NUOPC_CompSpecialize
    use NUOPC            , only : NUOPC_CompAttributeGet, NUOPC_Advertise
    use NUOPC            , only : NUOPC_CompFilterPhaseMap
    use NUOPC, only : NUOPC_Write ! HK what is the write for?
    use NUOPC, only : NUOPC_SetAttribute, NUOPC_CompAttributeSet, NUOPC_Realize, NUOPC_GetAttribute

    !HK three of these rename on import did not seem to work, using the model_ version for now
    use NUOPC_Model, model_routine_SS        => SetServices 
    use NUOPC_Model, model_label_Advance     => label_Advance  !HK model_label_Advance
    use NUOPC_Model, model_label_SetRunClock => label_SetRunClock 
    use NUOPC_Model, model_label_Advertise    => label_Advertise !HK model_label_Advertise
    use NUOPC_Model, only: label_ModifyAdvertised   
    use NUOPC_Model, only : NUOPC_ModelGet, setVM

    implicit none
    private 

    ! public module subroutine
    public :: SetServices
    public :: SetVM

    character(len=*), parameter :: u_FILE_u = &
        __FILE__

    !------------------------------------------------
    contains
    !------------------------------------------------

    subroutine SetServices(dgcomp, rc)
        ! In Fortran, you declare a variable of the derived type and then set its attributes.
        type(ESMF_GridComp)  :: dgcomp
        integer, intent(out) :: rc

        ! here goes all the local variables
        character(len=*), parameter  :: subname='(DART_cap:SetServices)'

        rc = ESMF_SUCCESS

        call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

        ! derive from NUOPC_Model 
        call NUOPC_CompDerive(dgcomp, model_routine_SS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__))&
            return ! bail out

        ! specialize the model

        call NUOPC_CompSpecialize(dgcomp, specLabel=model_label_Advertise, &
          specRoutine=InitializeAdvertise, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return ! bail out

        call NUOPC_CompSpecialize(dgcomp, specLabel=label_ModifyAdvertised, specRoutine=ModifyAdvertise, rc=rc)
        if (ChkErr(rc,__LINE__, u_FILE_u)) return

        call NUOPC_CompSpecialize(dgcomp, specLabel=label_RealizeAccepted, &
          specRoutine=RealizeAccepted, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return ! bail out

        call NUOPC_CompSpecialize(dgcomp, specLabel=model_label_Advance, &
          specRoutine=ModelAdvance, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return ! bail out
         
    end subroutine SetServices


    subroutine InitializeAdvertise(dgcomp, rc)

        ! input/output variable
        type(ESMF_GridComp)         :: dgcomp
        integer, intent(out)        :: rc

        ! local variables
        type(ESMF_State)            :: importState, exportState
        type(ESMF_Clock)            :: clock
        character(len=*), parameter :: subname = trim(modName)//':(Advertise) '

        rc = ESMF_SUCCESS
        call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

        ! query the import and export state

        call NUOPC_ModelGet(dgcomp, importState=importState, exportState=exportState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out

        !=========================================
        ! set attribute for field mirroring
        !=========================================

        call NUOPC_SetAttribute(importState, "FieldTransferPolicy", "transferAll", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return ! bail out

        call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)
    
    end subroutine InitializeAdvertise

    subroutine ModifyAdvertise(dgcomp, rc)

        !input/output variables
        type(ESMF_GridComp)  :: dgcomp
        integer, intent(out) :: rc


        !local variables
        integer              :: n, m
        integer              :: fieldCount
        logical              :: isPresent, isSet
        type(ESMF_Field)     :: field
        type(ESMF_State)     :: importState, exportState
        character(ESMF_MAXSTR), allocatable :: rfieldnamelist(:)
        character(ESMF_MAXSTR), allocatable :: lfieldnamelist(:)
        character(ESMF_MAXSTR):: message, cname, cvalue, scalar_field_name = ''
        character(len=*), parameter :: subname = trim(modName)//':(ModifyAdvertise) '
        !----------------------------------------------------------------------------
        



        rc = ESMF_SUCCESS
        call ESMF_LOGWRITE(subname//' called', ESMF_LOGMSG_INFO)

        !--------------------------------------------------------
        ! query for ScalerFieldName
        !--------------------------------------------------------

        scalar_field_name = ""
        call NUOPC_CompAttributeGet(dgcomp, name="ScalarFieldName", value=cvalue, &
            isPresent=isPresent, isSet=isSet, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return ! bail out
        if (isPresent .and. isSet) then
            scalar_field_name = trim(cvalue)
            call ESMF_LogWrite(trim(subname)//":ScalarFieldName = "//trim(scalar_field_name), ESMF_LOGMSG_INFO)
        endif


        !---------------------------------------------------------
        ! query for RemoveFieldList
        !---------------------------------------------------------
        
        call NUOPC_CompAttributeGet(dgcomp, name="RemoveFieldList", value=cvalue, &
            isPresent=isPresent, isSet=isSet, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return ! bail out
        
        m=0
        if (isPresent .and. isSet) then
            ! Get number of fields in the list
            m = StringListGetNum(cvalue, ":")
            if (m > 0) then
                ! Allocate temporary array for field list
                allocate(rfieldnamelist(m))

                ! Loop over occurances and fill the field list
                do n = 1, m 
                    call StringListGetName(cvalue, n, cname, ':', rc)        
                    if (ChkErr(rc,__LINE__,u_FILE_u)) return
                    rfieldnamelist(n) = trim(cname)
                    write(message, fmt='(A,I2.2,A)') trim(subname)//':RemoveFieldList(',n,') = '//trim(rfieldnamelist(n))
                    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
                end do
            end if
        endif

        !------------------
        ! query for importState and exportState
        !------------------

        call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        !------------------
        ! loop over import fields and remove scalar field
        !------------------

        call ESMF_StateGet(importState, itemCount=fieldCount, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        if (.not. allocated(lfieldnamelist)) allocate(lfieldnamelist(fieldCount))
        call ESMF_StateGet(importState, itemNameList=lfieldnamelist, rc=rc)    
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        do n = 1, fieldCount
            ! Get field from import state
            call ESMF_StateGet(importState, field=field, itemName=trim(lfieldnamelist(n)), rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return

            ! Remove field if it is cpl_scalars
            if (trim(lfieldnamelist(n)) == trim(scalar_field_name)) then
                ! Print out mirrored field name
                call ESMF_LogWrite(trim(subname)//": "//trim(lfieldnamelist(n))//" will be removed", ESMF_LOGMSG_INFO)
            
                ! Remove field from import state
                call ESMF_StateRemove(importState, itemNameList=(/trim(lfieldnamelist(n))/), relaxedFlag=.true., rc=rc) 
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
            end if
        end do

        !------------------
        ! remove fields in the list
        !------------------

        if (m > 0) then
        ! Print out mirrored field name
        do n = 1, m
            call ESMF_LogWrite(trim(subname)//": "//trim(rfieldnamelist(n))//" will be removed", ESMF_LOGMSG_INFO)
        end do

        ! Remove field/s from import state
        call ESMF_StateRemove(importState, itemNameList=rfieldnamelist, relaxedFlag=.true., rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return
        end if

        ! Clean memory
        if (allocated(lfieldnamelist)) deallocate(lfieldnamelist)
        if (allocated(rfieldnamelist)) deallocate(rfieldnamelist)

        call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)
        
    end subroutine ModifyAdvertise



    subroutine RealizeAccepted(dgcomp, rc)

        ! input/output variables
        type(ESMF_GridComp) :: dgcomp
        integer, intent(out) :: rc

        ! local variables
        integer :: n, m
        integer :: fieldCount, arbDimCount
        integer :: dimCount, tileCount, connectionCount
        integer, allocatable :: minIndexPTile(:,:), maxIndexPTile(:,:)
        type(ESMF_Grid) :: newgrid, grid
        type(ESMF_DistGrid) :: distgrid
        type(ESMF_Field) :: field
        type(ESMF_GeomType_Flag) :: geomType
        type(ESMF_FieldStatus_Flag) :: fieldStatus
        type(ESMF_State) :: importState, exportState
        type(ESMF_DistGridConnection) , allocatable :: connectionList(:)
        character(ESMF_MAXSTR), allocatable :: lfieldnamelist(:)
        character(len=*), parameter :: subname = trim(modName)//':(RealizeAccepted) '
        !---------------------------------------------------------------------------

        rc = ESMF_SUCCESS
        call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

        !------------------
        ! query for importState and exportState
        !------------------

        call NUOPC_ModelGet(dgcomp, importState=importState, exportState=exportState, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        !------------------
        ! loop over import fields
        !------------------

        call ESMF_StateGet(importState, itemCount=fieldCount, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        call StateWriteVTK(importState, 'test', rc=rc)
        if (ChkErr(rc, __LINE__, u_FILE_u)) return

        allocate(lfieldnamelist(fieldCount))
        call ESMF_StateGet(importState, itemNameList=lfieldnamelist, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        do n = 1, fieldCount
            ! Get field from import state
            call ESMF_StateGet(importState, field=field, itemName=trim(lfieldnamelist(n)), rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return

            ! Get field status
            call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
            if (ChkErr(rc,__LINE__,u_FILE_u)) return

            if (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) then
                ! Get geom type
                call ESMF_FieldGet(field, geomtype=geomType, rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return

                ! Get grid/mesh and update decomposition
                if (geomtype == ESMF_GEOMTYPE_GRID) then
                    call ESMF_LogWrite(trim(subname)//": geomType is ESMF_GEOMTYPE_GRID for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)

                    ! query field to get the grid
                    call ESMF_FieldGet(field, grid=grid, rc=rc)
                    if (ChkErr(rc, __LINE__, u_FILE_u)) return

                    ! query grid for its decomposition
                    call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
                    if (ChkErr(rc, __LINE__, u_FILE_u)) return

                    ! query DistGrid
                    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, rc=rc)
                    if (ChkErr(rc, __LINE__, u_FILE_u)) return

                    ! Allocated required arrays
                    allocate(minIndexPTile(dimCount, tileCount))
                    allocate(maxIndexPTile(dimCount, tileCount))

                    ! Query DistGrid
                    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, rc=rc)
                    if (ChkErr(rc, __LINE__, u_FILE_u)) return

                    ! Create grid with new decomposition
                    if (dimCount == 2) then
                        !Query DistGrid
                        call ESMF_DistGridGet(distgrid, connectionCount=connectionCount, rc=rc)
                        if (ChkErr(rc, __LINE__, u_FILE_u)) return

                        ! Allocate connectionList and fill it
                        allocate(connectionList(connectionCount))
                        call ESMF_DistGridGet(distgrid, connectionList=connectionList, rc=rc)
                        if (ChkErr(rc, __LINE__, u_FILE_u)) return

                        ! Create DistGrid
                        distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, &
                            connectionList=connectionList, rc=rc)
                        if (ChkErr(rc, __LINE__, u_FILE_u)) return

                        ! Create new grid
                        newgrid = ESMF_GridCreate(distgrid, gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), rc=rc)
                        if (ChkErr(rc, __LINE__, u_FILE_u)) return

                        ! remove temporary array
                        deallocate(connectionList)

                    else
                        ! Create DistGrid
                        distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, rc=rc)
                        if (ChkErr(rc, __LINE__, u_FILE_u)) return

                        ! Create new grid
                        newgrid = ESMF_GridCreate(distgrid, gridEdgeLWidth=(/0/), gridEdgeUWidth=(/0/), rc=rc)
                        if (ChkErr(rc, __LINE__, u_FILE_u)) return

                    end if

                    ! Remove temporary arrays
                    deallocate(minIndexPTile, maxIndexPTile)

                    ! Swap grid
                    if (fieldStatus==ESMF_FIELDSTATUS_EMPTY .or. fieldStatus==ESMF_FIELDSTATUS_GRIDSET) then
                        call ESMF_LogWrite(trim(subname)//": replace grid for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)
                        call ESMF_FieldEmptySet(field, grid=newgrid, rc=rc)
                        if (ChkErr(rc, __LINE__, u_FILE_u)) return
                    else
                        call ESMF_LogWrite(trim(subname)//": NOT replacing grid for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)
                    end if

                elseif(geomtype == ESMF_GEOMTYPE_MESH) then
                    call ESMF_LogWrite(trim(subname)//": geomType is ESMF_GEOMTYPE_MESH for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)


                end if ! geomType

            elseif (fieldStatus=ESMF_FIELDSTATUS_EMPTY) then
                call ESMF_LogWrite(trim(subname)//": provide grid for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)

            elseif (fieldStatus=ESMF_FIELDSTATUS_COMPLETE) then
                call ESMF_LogWrite(trim(subname)//": no grid provided for "//trim(lfieldnamelist(n)), ESMF_LOGMSG_INFO)

            else 
                call ESMF_LogWrite(trim(subname)//": ERROR fieldStatus not supported ", ESMF_LOGMSG_INFO)

                rc=ESMF_FAILURE
                return

            end if ! fieldStatus

            ! Realize the advertised field
            call NUOPC_Realize(importState, fieldName=trim(lfieldnamelist(n)), rc=rc)
            if (ChkErr(rc, __LINE__, u_FILE_u)) return

        end do

        ! Clean memory
        if (allocated(lfieldnamelist)) deallocate(lfieldnamelist)

        call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)

    end subroutine RealizeAccepted

    subroutine Advance(dgcomp, rc)

        ! input/output variables
        type(ESMF_GridComp) :: dgcomp
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Time) :: currTime
        type(ESMF_Clock) :: clock
        type(ESMF_State) :: importState, exportState
        logical :: isPresent, isSet 
        character(len=ESMF_MAXSTR) :: message
        character(len=ESMF_MAXSTR) :: cvalue 
        character(len=ESMF_MAXSTR) :: timeStr
        character(len=*), parameter :: subname = trim(modName)//':(Advance) '
        !---------------------------------------------------------------------------

        rc = ESMF_SUCCESS
        call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

        !-------------------------------
        ! query for DebugLevel 
        !-------------------------------

        call NUOPC_CompAttributeGet(dgcomp, name='DebugLevel', value=cvalue, &
        isPresent=isPresent, isSet=isSet, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        if (isPresent .and. isSet) then
        read(cvalue,*) dbug
        end if
        write(message, fmt='(A,L)') trim(subname)//': DebugLevel = ', dbug
        call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)

        !------------------
        ! query for clock, importState and exportState
        !------------------

        call NUOPC_ModelGet(dgcomp, modelClock=clock, importState=importState, exportState=exportState, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return

        !------------------
        ! Write import state (for debugging)
        !------------------

        if (dbug > 5) then
        call ESMF_TimeGet(currTime, timeStringISOFrac=timeStr , rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return  

        call StateWrite(importState, 'import_'//trim(timeStr), rc=rc)
        if (ChkErr(rc,__LINE__,u_FILE_u)) return 
        end if

        call ESMF_LogWrite(subname//' done', ESMF_LOGMSG_INFO)



    end subroutine Advance

    subroutine StateWriteVTK(state, prefix, rc)

        implicit none

        !> Input/output variables
        type(ESMF_State), intent(in) :: state
        character(len=*), intent(in) :: prefix
        integer, intent(out), optional :: rc

        !> local variables
        integer :: i, itemCount
        type(ESMF_Field) :: field
        character(ESMF_MAXSTR), allocatable :: itemNameList(:)
        character(len=*),parameter :: subname='(StateWriteVTK)'
        !--------------------------------

        rc = ESMF_SUCCESS
        call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)

        !> Get number of fields in the state
        call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        !> Get item names
        if (.not. allocated(itemNameList)) allocate(itemNameList(itemCount))

        call ESMF_StateGet(state, itemNameList=itemNameList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        !> Loop over fields and write them
        do i = 1, itemCount
          !> Get field
          call ESMF_StateGet(state, itemName=trim(itemNameList(i)), field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out

          !> Write field
          call ESMF_FieldWriteVTK(field, trim(prefix)//'_'//trim(itemNameList(i)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
        end do

        !> Clean temporary variables
        if (allocated(itemNameList)) deallocate(itemNameList)

        call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

    end subroutine StateWriteVTK

end module dart_comp_nuopc

