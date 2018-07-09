



!--------------------------------------------------------------------------------------

      subroutine propagate(W,B,L,deriv,typop,laystr,nlay)
      implicit none
!     Propagates full FF-NN.
!     nlay:      number of given layers
!     nprop:     number of single-layer propagations 
!     neu_io:    number of neurons in giv./prev. layer
!     neu_oi:    number of neurons in prev./giv. layer
!     poslay:    starting position of layer in L- and B-vector
!     poswei:    starting position of Weight-Matrix in W-vector
!                
!     W:         Weight Matrix Vector
!     B:         Bias Vector
!     L:         Layer Vector
!     deriv:     Activation function derivatives
!     typop:     Type Population Matrix
!     laystr:    Layer Structure Matrix
!                
!      laystr(1,N):   number of neurons in layer N
!      laystr(2,N):   starting pos. for layer N
!      laystr(3,N):   starting pos. for Weight Matrix from layer N to N+1
!
      include 'nnparams.incl'
      include 'nndbg.incl'

      integer nlay,laystr(3,*),typop(maxtypes,*)
      real*8 W(*),B(*),L(*),deriv(*)

      integer poslay,poswei,nprop,neu_io,neu_oi
      integer k 

      nprop=nlay-1

      neu_io=laystr(1,1) !number of input neurons
      poslay=laystr(2,1) 
      poswei=laystr(3,1)

      do k=2,2*(nprop/2),2
         neu_oi=laystr(1,k) !number of output neurons

         call proplay(neu_io,neu_oi,W(poswei),B(poslay),L(poslay),
     >                deriv(poslay),typop(1,k))

         poslay=laystr(2,k)
         poswei=laystr(3,k)
         neu_io=laystr(1,k+1) !new number of output neurons

!        former output neurons are now input, _io and _oi switch places!
!        (done for efficiency)
 
         call proplay(neu_oi,neu_io,W(poswei),B(poslay),L(poslay),
     >                deriv(poslay),typop(1,k+1))
        
         poslay=laystr(2,k+1)
         poswei=laystr(3,k+1)
      enddo

      do k=1,mod(nprop,2) !for odd nprop one iteration is left
         neu_oi=laystr(1,nlay) 
         call proplay(neu_io,neu_oi,W(poswei),B(poslay),L(poslay),
     >                deriv(poslay),typop(1,nlay))
      enddo
      
      end

!--------------------------------------------------------------------------------------

      subroutine proplay(neu_in,neu_out,W,B,L,deriv,typop)
      implicit none      
!     Evaluates given layer in FF-NN.
!
!     neu_in:  number of neurons in previous Layer
!     neu_out: number of neurons in given Layer
!
!     W:       Weight Matrix Vector
!     B:       Bias Vector (beginning at prev. layer)
!     L:       Layer Vector (beginning at prev. layer)
!     deriv:   Activation function derivatives
!     typop:   Type Population Matrix (beginning at curr. layer)

      include 'nnparams.incl'
      include 'nndbg.incl'

      integer neu_in,neu_out,typop(*)
      real*8 W(neu_in,*),B(*),L(*),deriv(*)

      integer neu_tot,pos_out
      integer j,k

      neu_tot=neu_in+neu_out !total number of neurons in both layers
      pos_out=neu_in+1       !pos. of current layer's first element

      do j=pos_out,neu_tot
         L(j)=B(j) !add bias
         do k=1,neu_in
            L(j)=L(j)+W(k,j-neu_in)*L(k)  !matrix x vector
         enddo
      enddo

!     apply nonlin. functions to vector elements
      call neurons(L(pos_out),deriv(pos_out),typop) 

      end

!--------------------------------------------------------------------------------------

      subroutine neurons(L,deriv,typop)
      implicit none
!     Applies non-linear activation functions to given layer.
!     type-unspecified neurons have the activation function f(x)=x.
!
!     ni: Number of neurons of type i in given layer.   
!
!     L:      Layer Vector containing accumulated neuronal input
!     deriv:  Activation function derivatives
!     typop:     Type Population Matrix

      include 'nnparams.incl'
      include 'nndbg.incl'

      real*8 L(*),deriv(*)
      integer typop(maxtypes,*)

      integer n1,n2!,n3

      n1=typop(1,1)
      n2=typop(2,1)
 !     n3=typop(3)

      call deriv1(L(1),deriv(1),n1)
      call neuron1(L(1),n1)

      call deriv2(deriv(n1+1),n2) !deriv2 is L-independent
      call neuron2(L(n1+1),n2)

!      call deriv3(L(n1+n2+1),deriv(n1+n2+1),n3)
!      call neuron3(L(n1+n2+1),n3)
      
      end

