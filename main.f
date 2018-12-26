      program main
         use las_mod
         use func
         use finite_element_matrices
         implicit none
         real :: start,finish, start_1, finish_1
         real, allocatable :: T(:), X(:), R(:,:), B(:,:), A(:,:)
         real, allocatable :: E_x(:,:), E_t(:,:), f(:,:), funk(:)
         real, allocatable :: ddg(:), P(:,:), dP(:,:), p_1(:), dp_1(:)
         real, allocatable :: SS(:,:), dg(:),g(:)
         integer, allocatable :: ipvi(:)
         real :: t0, x0, dt, dx, D
         integer :: t_step, x_step, i
         integer :: n, nrhs, lda, ldb, info
         call cpu_time(start)
         t0 = 0; x0 = -8; t_step = 250; x_step=80
         dx = 0.2; dt=0.5*dx
         D = 20 
         if(allocated(E_x)) then
            deallocate(E_x)
         end if
         if(allocated(E_t)) then
            deallocate(E_t)
         end if
         if(allocated(P)) then
            deallocate(P)
         end if
         if(allocated(dP)) then
            deallocate(dP)
         end if
         call vec_by_step(T,t0,dt,t_step)
         call vec_by_step(X,x0,dx,x_step)
         allocate(E_x(size(X),size(X)))
         allocate(E_t(size(T),size(T)))
         E_x = eye(size(X),size(X))
         E_t = eye(size(T),size(T))

         call SECmat(R,size(X))
         call Bmat(B,size(T))
         call Smat(SS, size(X))
         allocate(P(size(X),size(X)))
         allocate(dP(size(X),size(X)))
         call dpot(dp_1,X)
         call pot(p_1,X)
         do i = 1,size(X)
            P(:,i) = p_1
            dP(:,i) = dp_1
         end do
         P = SS*P
         dP = eye(size(X), size(X))*dP
         if(allocated(A)) then
            deallocate(A)
         end if
         allocate(A(size(E_t,1)*size(R,1),size(E_t,2)*size(R,2)))

         A = kron(B/dt,E_x) - D*kron(E_t,R/dx**2) + kron(E_t,P/(2*dx)) 
     +       + kron(E_t,dP)
         deallocate(E_x, B, E_t,R)
         
         if(allocated(f)) then
            deallocate(f)
         end if
         allocate(f(size(T)*size(X),1))
         if(allocated(g)) then
            deallocate(g)
         end if
         allocate(g(size(X)))
         call ddfun(ddg,X)
         call dfun(dg,X)
         call fun(g,X)
         f(1:(size(X)),1)= -D*ddg + dg*p_1 + g*dp_1
         do i=1,(size(T))
         f((i*size(X)+1):(i*size(X)+size(X)),1) = f(1:size(X),1)
         end do
         
         
         n = size(A,1); nrhs = size(f,2)
         lda = n; ldb = n
         
         if(allocated(ipvi)) then
            deallocate(ipvi)
         end if 
         allocate(ipvi(n))
         print *, 'Solving linear equations...'
         call cpu_time(start_1)
         call SGESV(n, nrhs, A, lda,ipvi, f, ldb, info)
         call cpu_time(finish_1)
         print *, 'Done in', finish_1-start_1, 's'
         if(allocated(A)) then
            deallocate(A)
         end if
         allocate(A(size(T), size(X)))

         do i=0,(size(f)-1)
         call fun(funk,X)
         A(i/size(X)+1,mod(i,size(X))+1)=-f(i+1,1)
     +     + funk(mod(i,size(X))+1)
         end do

         call mat2file(A) 
         call cpu_time(finish)
         print *, 'Time=', finish-start
         print *, 'Info=', info
      end program
