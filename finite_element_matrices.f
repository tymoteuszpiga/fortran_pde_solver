      module finite_element_matrices
         implicit none
         contains 
         subroutine Fmat(A, length) !Forward matrice first deriv
            integer :: length, i
            real, allocatable :: A(:,:)
            if(allocated(A)) then
               deallocate(A)
            end if
            allocate(A(length,length))
            A = 0
            do i = 1,(length-1)
            A(i,i) = -1
            A(i,i+1) = 1
            end do
            A(length,length)=-1
         end subroutine Fmat
         subroutine Bmat(A, length) !Backward matrice first deriv
            integer :: length, i
            real, allocatable :: A(:,:)
            if(allocated(A)) then
               deallocate(A)
            end if
            allocate(A(length,length))
            A = 0
            do i = 1,(length-1)
            A(i,i) = 1
            A(i+1,i) = -1
            end do
            A(length,length) = 1
         end subroutine Bmat
         subroutine Smat(A, length) !Symmetric matrice first deriv
            integer :: length, i
            real, allocatable :: A(:,:)
            if(allocated(A)) then
               deallocate(A)
            end if
            allocate(A(length,length))
            A = 0
            do i = 1,(length-1)
            A(i,i+1) = 1
            A(i+1,i) = -1
            end do
         end subroutine Smat
         subroutine SECmat(A, length) !Symmetric matrice second deriv
            integer :: length, i
            real, allocatable :: A(:,:)
            if(allocated(A)) then
               deallocate(A)
            end if
            allocate(A(length,length))
            A = 0
            do i = 1,(length-1)
            A(i,i+1) = 1
            A(i,i) = -2
            A(i+1,i) = 1
            end do
            A(length,length) = -2
         end subroutine SECmat



      end module

