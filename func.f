      module func
         implicit none
         real, parameter :: sigma = 1, mu = 0
         real, parameter :: pi = 3.141592653589
         real, parameter :: A_1 = -15, B_1 = 4, phi = 0
         contains
            
            subroutine fun(F,X)
               real :: X(:)
               real, allocatable :: F(:)
               real :: N, C
               N = sigma*sqrt(2*pi)
               C = 2*sigma**2
               if(allocated(F)) then
                  deallocate(F)
               end if
               allocate(F(size(X)))
               X = X - mu
               F = 1/N * exp(-X**2/C)
            end subroutine
             
            subroutine dfun(F,X)
               real :: X(:)
               real, allocatable :: F(:)
               real :: N, C
               N = sigma*sqrt(2*pi)
               C = 2*sigma**2
               if(allocated(F)) then
                  deallocate(F)
               end if
               allocate(F(size(X)))
               X = X-mu
               F = -(2*X*exp(-X**2/C))/(C*N)
            end subroutine
              
            subroutine ddfun(F,X)
               real :: X(:)
               real, allocatable :: F(:)
               real :: N, C
               N = sigma*sqrt(2*pi)
               C = 2*sigma**2
               if(allocated(F)) then
                  deallocate(F)
               end if
               allocate(F(size(X)))
               X = X-mu
               F = -(2*exp(-X**2/C)*(C-2*X**2))/(C**2*N)
            end subroutine
            
            subroutine pot(F,X)
               real :: X(:)
               real, allocatable :: F(:)
               if(allocated(F)) then
                  deallocate(F)
               end if
               allocate(F(size(X)))
               F = A_1*(cos(X*B_1+phi))
            end subroutine
   
            subroutine dpot(F,X)
               real :: X(:)
               real, allocatable :: F(:)
               if(allocated(F)) then
                  deallocate(F)
               end if
               allocate(F(size(X)))
               F = -A_1*sin(X*B_1+phi)*B_1
            end subroutine 
      end module 
