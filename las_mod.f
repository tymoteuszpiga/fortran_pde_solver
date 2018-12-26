      module las_mod
         implicit none
         contains
c     KRONECKER PRODUCT     
            function kron(A,B) result (AB)
               real :: A(:,:), B(:,:), start, finish
               real, allocatable :: AB(:,:)
               integer :: s, i, j, m, n, di_n, di_m
               call cpu_time(start)
               di_n = size(A,1)*size(B,1)-1
               di_m = size(A,2)*size(B,2)-1
               if(allocated(AB)) then
                 deallocate (AB)
               end if
               allocate (AB(0:di_n,0:di_m))
               
               print *, 'Kronecker product...'
               do s = 0,(size(A)-1)
                 i = s/size(A,2)
                 j = mod(s,size(A,2))
                 m = i*size(B,1)
                 n = j*size(B,2)
                 AB(m:(m+size(B,1)),n:(n+size(B,2))) = A(i+1,j+1)*B
               end do
               call cpu_time(finish)
               print *, 'Done in', finish-start ,'s'
            end function kron
c     IDENTITY MATRICES
            function eye(n,m) result(Y)
               real, allocatable :: Y(:,:)
               integer i, j, k, n, m, length
               if(allocated(Y)) then
                  deallocate(Y)
               end if
               allocate(Y(0:(n-1),0:(m-1)))
               Y = 0 
               do i = 0,(size(Y)-1)
                 n = i/size(Y,2)
                 m = mod(i,size(Y,2))
                 if (n == m) then
                   Y(n,m) = 1
                 end if
               end do
            end function eye
c     PRINTING MATRICES
            subroutine show(A)
               integer :: i
               real :: A(:,:)
               do i = 1,ubound(A,DIM=1)
                 print *, A(i,:)
1                format(666I3)
               end do
            end subroutine show
c     MAKEING VECTOR BY STEP, SEQUENCE
            subroutine vec_by_step(A, b, s, n)  
               !A -- vector b -- begining, s -- step, n -- number of
               !steps
               real, allocatable :: A(:)
               real :: b, s
               integer :: n, i
               if(allocated(A)) then
                  deallocate(A)
               end if
               allocate(A(0:n))
               do i = 0,n
               A(i) = b+i*s
               end do
            end subroutine vec_by_step
c     WRITING MATRICE TO FILE
            subroutine mat2file(A)
               integer :: i 
               real :: A(:,:), start, finish
               call cpu_time(start)
               print *, 'Writing array to file...'
               open(unit = 1,file= 'solve.txt', status = 'replace')
               do i = 1,ubound(A,DIM=1)
               write(1,*) A(i,:)
1              format(666I3)
               end do
               call cpu_time(finish)
               print *, 'Done in', finish - start, 's'
            end subroutine mat2file
      end module las_mod


