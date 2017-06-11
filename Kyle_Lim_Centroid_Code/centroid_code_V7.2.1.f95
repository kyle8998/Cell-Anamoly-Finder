    Program centroid

    implicit none

!Program File Name Reading

! arg is a dummy argument variable. Assuming the lenght is 60
! characters per argument. The lenght is arbitrary and just for
! the purpose of having enough space for the argument.

      character*60 arg

! arg_null is a null argument variable. It should match the "arg"
! variable.

      character*60 arg_null

! File names will be extracted from the arguments and therefore
! we will make them of equal length

      character*60 file_name_input, file_name_output

!Parameters used for arrays
    integer max_roi, max_bins, max_arguments
    parameter (max_bins=10000)
    parameter (max_roi=10000)
    parameter (max_arguments=10)

! The argument array carries all the arguments. The total
! number of arguments is arbitrary but they take very little
! space so we will be generous and allocate enough space so
! that we can extend the list of arguments in the future.
! the type of arguments should match arg

      character*60 argument(max_arguments)

!Logical Statement Declarations
    logical verbose
    logical long_output
!Centroid+Distance Declarations
    real diff
    real*8 x1(max_roi), y1(max_roi)
    real*8 sumX1, sumY1, Center_X1, Center_Y1, Distance1(max_roi)

    integer i, j, total_points1, ios
!Dot Product+Angle Declarations --------- *4 and *8 used for double precision
    integer*4 max_index
    parameter (max_index = 10)
    integer*4 N
    real*8 A(max_index),B(max_index),SUM
    real*8 final_angle, cos_angle, a_length, b_length
    real*8 Radians_to_degrees, angle1(max_roi)
    real vector1x(max_roi), vector1y(max_roi)

!Binning Declarations
    integer bin1
    real*8 distance1_bin(max_bins)

!Logical Statements default to false unless stated true in control file.
      verbose = .false.
      long_output = .false.

!--------------------------------------------------
!--------------------------------------------------
! Read Control file

    open(unit=10, file='control.txt')
        read(10,*)verbose
        read(10,*)long_output
    close(10)

!--------------------------------------------------
!Program file name reading
      do i=1,max_arguments
      write(arg,'(a60)')arg_null
      call getarg(i,arg)
      write(argument(i),'(a60)')arg
      enddo

      do i=1,max_arguments
      if(argument(i).eq.'-no_print')verbose=.false.
      if(argument(i).eq.'-Input')write(file_name_Input,'(a60)')argument(i+1)
      if(argument(i).eq.'-Output')write(file_name_Output,'(a60)')argument(i+1)
      enddo

      open(unit=15,file=file_name_input,err=15,status='unknown')
      open(unit=16,file=file_name_output,err=16,status='unknown')
      goto 17
15    continue
      write(*,*)'Error reading input file'
      stop
16    continue
      write(*,*)'Error opening output file'
      stop
17    continue

        do i=1,max_roi
            read(10,*,end=1000)x1(i),y1(i)
        enddo
1000    continue
    total_points1 = i - 1
        close (unit=15)

!--------------------------------------------------
!--------------------------------------------------
! Internal Checks

    if(total_points1.lt.1)then
        write(*,*)'Too few points in the ROI '
        write(*,*)'ROI 1 :',total_points1
    stop
    endif

!--------------------------------------------------
!--------------------------------------------------
! Compute Centroids
!
! First ROI:

    sumX1 = 0.0
    sumY1 = 0.0
    do i=1,total_points1
        sumX1 = sumX1 + x1(i)
        sumY1 = sumY1 + y1(i)
    enddo

    Center_X1 = sumX1/total_points1
    Center_Y1 = sumY1/total_points1

    write(*,*) "Centroid of Image 1 ", Center_X1, Center_Y1


!--------------------------------------------------
!--------------------------------------------------
!Distance from centroid to border

    if(verbose)write(*,*) "Image 1 Distance"
    do i=1, total_points1
!Finding Vectors
        vector1x(i)=x1(i) - Center_X1
        vector1y(i)=y1(i) - Center_Y1
        Distance1(i)=SQRT(((Center_X1 - x1(i))**2)+((Center_Y1 - y1(i))**2))
        if(verbose)print*, Distance1(i)
    enddo


!--------------------------------------------------
!   External File Creation: Distance from Centroid to Outline

    if(long_output)then


    open(unit=10, FILE="Centroid_Distances_1.txt")
        do i=1, total_points1
        write(10,*) Distance1(i)
        enddo
    close (unit=10)



      endif
!--------------------------------------------------
!--------------------------------------------------
!Dot Product - angle computation for ROI_1
!
! WARNING!: Vector A - Internal reference: (1,0)

      n = 2
      a(1) = 1.0
      a(2) = 0.0
      radians_to_degrees =  90.0D0 / dcos(0.0D0)

    write(*,*) 'ROI_1 Angles:'
    do i=1,total_points1

        b(1) = vector1x(i)
        b(2) = vector1y(i)

    call dot(SUM,A,B,N)

! WARNING!: The length of vector A is 1 by definition so it does not
! appear in the equation below

    cos_angle = SUM / Distance1(i)

! Radians_to_degrees = (180.0D0 / 3.1415927D0)

    angle1(i) = dacos (cos_angle) * radians_to_degrees

    if(b(2).lt.0.0)then
        angle1(i) = 360.0D0 - angle1(i)
    endif

    if(verbose) write (*,*) ' ANGLE = ',angle1(i)

    enddo

    continue



!---------------------------------------------------
!   External File Creation: Dot Product Angles
!ROI_1
    if(long_output)then

    open(unit=10, FILE="Dot_Product_Angles_1.txt")
        do i=1, total_points1
        write(10,*) angle1(i)
        enddo
    close (unit=10)

    endif

!--------------------------------------------------
!--------------------------------------------------
! Binning the angles for pre-processing

!ROI_1
    do i=1, total_points1
        bin1 = int(angle1(i))
        Distance1_bin(bin1) = Distance1(i)
    enddo

!--------------------------------------------------
!   External File Creation: Binning

    do i=1,360
        write(16,*)Distance1_bin(i)
    enddo
      close (unit=16)


    if(long_output)then
!ROI_1
    open(unit=10, FILE="Angles_binning_1.txt")
        do i=1, 360
        write(10,*) Distance1_bin(i)
        enddo
    close (unit=10)

    endif

      stop

!--------------------------------------------------
!--------------------------------------------------
    end program centroid
!--------------------------------------------------
!--------------------------------------------------

!Dot Product Subroutine
      subroutine dot(SUM,A,B,N)

!     RETURNS DP SCALAR PRODUCT OF TWO DP VECTORS

      implicit none

      integer*4 max_index
      parameter (max_index = 10)

      integer*4 i, N
      REAL*8 A(max_index),B(max_index),SUM
      Real*8 final_angle, cos_angle, a_length, b_length

      SUM=0.D0

      DO I=1,N
         SUM = SUM + A(I) * B(I)
      enddo

      RETURN
      END
!-----------------------------------------------------
