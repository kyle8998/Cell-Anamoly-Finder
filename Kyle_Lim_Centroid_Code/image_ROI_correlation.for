
      program image_RGB_correlation

! Data structure definitions

      include 'image_RGB_correlation_INCLUDE.for'


! Read Control Data

      OPEN (UNIT=10, FILE="control_data.txt")
            read(10,*)Image_Length
      close(unit=10)

      Image_Width = 2
      skip = 1
      boundary = 1

! Read R, G and B files

      open(unit=10, FILE="ROI_1.txt")
            DO i=1,Image_Length
                  read(10, *)(pixel(1,i,j), j=1,Image_width)
            END DO
      close(unit=10)

      open(unit=10, FILE="ROI_2.txt")
            DO i=1,Image_Length
                  read(10, *)(pixel(2,i,j), j=1,Image_width)
            END DO
      close(unit=10)

! Computer correlation statistics for ROI1 vs ROI2

      call compute_correlation (1,2)
      write(*,*)' R-G correlation (R, R**2): ',R,R**2


! Print R/G , G/B and R/B pairs for external verification

      open(unit=10, FILE="CORR_ROI_1_2.txt")
            do i=1+boundary,Image_Length-boundary,skip
                  do j=1,Image_Width, skip
                        write(10,*)pixel(1,i,j),pixel(2,i,j)
                  enddo
            enddo
      close (unit=10)

      stop

      end program



!---------------------------------------------------------------
! Routine that compute the actual correlations

      subroutine compute_correlation(k1,k2)

      include 'image_RGB_correlation_INCLUDE.for'

! Local definitions

      integer K1, K2
      real*8 x_sum, y_sum, x_ave, y_ave, XY_dev_sum, x_sd, y_sd

      x_sum = 0.0d0
      y_sum = 0.0d0
      x_ave = 0.0d0
      y_ave = 0.0d0

      do i=1+boundary,Image_Length-boundary,skip
      do j=1+boundary,Image_Width-boundary,skip
            x_sum = x_sum + pixel(k1,i,j)
            y_sum = y_sum + pixel(k2,i,j)
      enddo
      enddo

      n=(Image_Length-2*boundary)*(Image_Width-2*boundary)

      x_ave = x_sum / (float(n) / float(skip)**2 )
      y_ave = y_sum / (float(n) / float(skip)**2 )

      XY_dev_sum = 0.0d0
      x_sd = 0.0d0
      y_sd = 0.0d0
      do i=1+boundary,Image_Length-boundary,skip
      do j=1+boundary,Image_Width-boundary,skip
      XY_dev_sum=XY_dev_sum+(pixel(K1,i,j)-x_ave)*(pixel(K2,i,j)-y_ave)
      x_sd = x_sd + (pixel(K1,i,j)-x_ave)**2
      y_sd = y_sd + (pixel(K2,i,j)-y_ave)**2
      enddo
      enddo

      R = XY_dev_sum / ( DSQRT(x_sd) * DSQRT(y_sd) )

      return
      end
















