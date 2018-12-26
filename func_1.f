      module func
         implicit none
         contains
            
            subroutine fun(F,X)
               real :: X(:)
               real, allocatable :: F(:)
               if(allocated(F)) then
                  deallocate(F)
               end if
               allocate(F(size(X)))
               F = 4*X*(1-X)
            end subroutine 
             subroutine dfun(F,X)
               real :: X(:)
               real, allocatable :: F(:)
               if(allocated(F)) then
                  deallocate(F)
               end if
               allocate(F(size(X)))
               F = 4 - 8*X
            end subroutine   
            subroutine ddfun(F,X)
               real :: X(:)
               real, allocatable :: F(:)
               if(allocated(F)) then
                  deallocate(F)
               end if
               allocate(F(size(X)))
               F = -8
            end subroutine 
      end module
