************************************************************************
***                           maNNipulate
***         manipulating and processing ANN data structures
***
************************************************************************

      subroutine expandnet(par,neupop_add,laystr,nlay,nset)
      implicit none
!     Changes the network-topology of a given set of ANNs 
!     by adding neurons. Input and output layer cannot be altered.
!     Parameters not recognized by ANN will be destroyed.
!     Until changed, all new neurons have type 0 and weight and
!     bias contributions of 0; neuronal parameters may remain undefined.
!     
!     nlay:         number of layers of the network
!     nset:         number of parameter sets.
!     par:          parameter vector(s); contain weights and biases.
!     neupop_add:   number of neurons to be increased.
!       neupop_add(n):    specific to layer n.
!     
      
      include 'params.incl'
      include 'common.incl'
      include 'nnparams.incl'
      include 'nndbg.incl'

      real*8 par(px,nx)
      integer laystr(3,maxlay)
      integer neupop_add(maxlay)
      integer nset,nlay

      real*8 par_new(px,nx)
      integer neupop_new(maxlay), laystr_new(3,maxlay)
      integer poswei,poswei_new,posbi,posbi_new

      integer n,k

      if (any(neupop_add(1:nlay).lt.0)) then
         write(6,'(A)') 'ERROR: CANNOT REMOVE NEURONS (expandnet)'
         stop
      else if (neupop_add(1).ne.0) then
         write(6,'(A)') 'ERROR: CANNOT CHANGE INPUT LAYER (expandnet)'
         stop
      else if (neupop_add(nlay).ne.0) then
         write(6,'(A)') 'ERROR: CANNOT CHANGE OUTPUT LAYER (expandnet)'
         stop
      endif
      
      par_new=0.0d0

!     construct new laystr
      do k=1,nlay
         neupop_new(k)=laystr(1,k)+neupop_add(k)
      enddo
      call mknet(laystr_new,neupop_new,nlay)

!     transform weight matrices
      do k=1,nlay-1
!        find position in par-vector
         poswei=laystr(3,k)
         poswei_new=laystr_new(3,k)
!        copy par to par_new and pad out weight matrices
!        if expansion is necessary, for all networks
         do n=1,nset
            call expandmat(par(poswei,n),par_new(poswei_new,n),
     >                     laystr(1,k),laystr(1,k+1),
     >                     laystr_new(1,k),laystr_new(1,k+1))
         enddo
      enddo

!     evaluate starting postion for biases
      posbi=laystr(3,nlay)
      posbi_new=laystr_new(3,nlay)
      
!     transform bias vectors
!     since input biases don't exist, begin at 2
      do k=2,nlay
         do n=1,nset
            call expandmat(par(posbi,n),par_new(posbi_new,n),
     >                     laystr(1,k),1,laystr_new(1,k),1)
         enddo
!        continue to next vector
         posbi=posbi+laystr(1,k)
         posbi_new=posbi_new+laystr_new(1,k)
      enddo

!     replace old parameter set with new one
      par=par_new
      laystr=laystr_new
      
      end

      
