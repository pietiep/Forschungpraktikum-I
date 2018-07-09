


!--------------------------------------------------------------------------------------

      subroutine backprop(del,W,deriv,laystr,nlay)
      implicit none
!     Backpropagates full FF-NN with given output-errors.
!     nlay:      number of given layers
!     nprop:     number of single-layer propagations 
!     neu_io:    number of neurons in giv./prev. layer
!     neu_oi:    number of neurons in prev./giv. layer
!     poslay:    starting position of layer in L- and B-vector
!     poswei:    starting position of Weight-Matrix in W-vector
!                
!     del:       Neuron Error Vector
!     deriv:     Activation function derivatives
!     W:         Weight Matrix Vector
!     laystr:    Layer Structure Matrix
!
!      laystr(1,N):   number of neurons in layer N
!      laystr(2,N):   starting pos. for layer N
!      laystr(3,N):   starting pos. for Weight Matrix from layer N to N+1

      include 'nnparams.incl'
      include 'nndbg.incl'

      integer nlay,laystr(3,*)
      real*8 del(*),deriv(*)
      real*8 W(*)

      integer poslay,poswei,nprop,neu_io,neu_oi
      integer k 

      nprop=nlay-1

!     evaluate deltas
      neu_io=laystr(1,nlay) !number of input neurons

      do k=nprop,2,-2 
         neu_oi=laystr(1,k)      !number of output neurons
         poslay=laystr(2,k) 
         poswei=laystr(3,k)

         call backlay(neu_io,neu_oi,del(poslay),W(poswei),
     >                deriv(poslay))

         neu_io=laystr(1,k-1) !new number of output neurons
         poslay=laystr(2,k-1)
         poswei=laystr(3,k-1)

!        former output neurons are now input, _io and _oi switch places!
!        (done for efficiency)
 
         call backlay(neu_oi,neu_io,del(poslay),W(poswei),
     >                deriv(poslay))

      enddo

      do k=1,mod(nprop,2) !for odd nprop one iteration is left
         neu_oi=laystr(1,1) 
         poslay=laystr(2,1) 
         poswei=laystr(3,1)

         call backlay(neu_oi,neu_io,del(poslay),W(poswei),
     >                deriv(poslay))
      enddo
      
      end

!--------------------------------------------------------------------------------------

      subroutine backlay(neu_in,neu_out,del,W,deriv)
      implicit none      
!     Backpropagates given layer in FF-NN.

!     neu_in:  number of neurons in layer N+1
!     neu_out: number of neurons in layer N
!
!
!     del:     Neuron Error Vector
!     W:       Weight Matrix (layer N to layer N+1)
!     deriv:   Derivative Vector (beginning at layer N)
!
      include 'nnparams.incl'
      include 'nndbg.incl'

      integer neu_in,neu_out
      real*8 del(*),deriv(*)
      real*8 W(neu_out,*)

      integer neu_tot,pos_in
      integer j,k

      neu_tot=neu_in+neu_out    !total number of neurons in both layers
      pos_in=neu_out+1          !position of first element in layer N+1

      do j=1,neu_out
         del(j)=0.0D0
      enddo

      do k=1,neu_in
         do j=1,neu_out
            del(j)=del(j)+del(neu_out+k)*W(j,k)
         enddo
      enddo
      do k=1,neu_out
         del(k)=del(k)*deriv(k)
      enddo

      end

