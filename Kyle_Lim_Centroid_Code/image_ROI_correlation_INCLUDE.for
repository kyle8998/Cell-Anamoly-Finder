
      implicit none

      INTEGER*8 :: max_width
      INTEGER*8 :: max_length
      PARAMETER (max_width  = 10000)
      PARAMETER (max_length = 10000)

      INTEGER*8 :: i, j, k, l, m, n, skip
      INTEGER*8 :: Image_Width, Image_Length, boundary
      real*8 , dimension (3,max_width,max_length):: pixel
      real*8 :: R

      common /RGB_001/ i, j, k, l, m, n, skip
      common /RGB_002/ Image_Width, Image_Length,boundary
      common /RGB_003/ pixel
      common /RGB_004/ R

      save /RGB_001/
      save /RGB_002/
      save /RGB_003/
      save /RGB_004/


