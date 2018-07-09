**** Define subroutine to generate output layer error for one pattern. 
***
*** 
*** 
****Conventions:
***
*** Most of the data structures are larger than declared;
*** however, only their relevant subsection is passed to the
*** subroutine and hence considered.
***
*** nnerror:   subroutine evaluating the output layer's error.
***
*** pat_err:   segment of the total error vector for one pattern.
***
*** len_in:    Actual number of input neurons
*** len_out:   Actual number of output neurons
*** 
*** pat_in:    Input pattern
***  pat_in(i):   value of ith input neuron for given pattern
*** 
*** pat_out:   Desired output pattern
***  pat_out(i):  desired value of ith output neuron for given pattern
***
*** L:         Final layer, starting at the first neuron
***************************************************************************
****Note:
*** neu_in and neu_out, if needed, should ideally not be hardcoded.
*** Instead they should be passed by the main program to ensure
*** matching dimensionalities.
***
***************************************************************************

      subroutine nnerror(pat_err,pat_in,pat_out,nn_out)
      implicit none

!     Internal variables and parameters
!
!     nn_out:        output from neural network
!     pat_err:       error vector for single pattern

      include 'params.incl'
      include 'nnparams.incl'
      include 'nndbg.incl'
      include 'nncommon.incl'

      real*8 nn_out(maxnout)
      real*8 pat_in(maxnin),pat_out(maxpout),pat_err(maxpout)

      call nnstderror(pat_err,pat_out,nn_out)

      end

!--------------------------------------------------------------------------------------

      subroutine nnoutgrad(en_grads,pat_in,nn_out)
      implicit none
!     Compute the derivative of each output value (i.e. adiab. energies)
!     with respect to the Neural Network output-neurons.
!
!     nn_out:           output from neural network
!     en_grads:         derivatives as described above
!       en_grads(:,i):  gradient of energy i
!     eps:              finite differences for derivatives
!       eps(i):           finite difference for output neuron i
!     dis_out:          ANN output displaced by finite differences
!       dis_out(:,1):     equivalent to L + eps
!       dis_out(:,2):     equivalent to L - eps 
!     dis_en:           yielded adiabatic energies for current displacements      
!
!     ddelta:        factor used to determine eps.

      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

      real*8 nn_out(maxnout)
      real*8 pat_in(maxnin)
      real*8 en_grads(maxnout,maxpout)

      call nnstdoutgrad(en_grads)

      end

!--------------------------------------------------------------------------------------

      subroutine nnstderror(pat_err,pat_out,L)
      implicit none
!     Standard error evaluation routine for MQL-learning.

      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

      real*8 L(*)
      real*8 pat_out(maxpout),pat_err(maxpout)

      integer k

!     trivial case: patterns and neurons are directly comparable
      do k=1,len_out
         pat_err(k)=pat_out(k) - L(k)
      enddo

      end

!--------------------------------------------------------------------------------------

      subroutine nnstdoutgrad(en_grads)
      implicit none
!     Standard derivative routine for MQL-learning.
!
!     nn_out:           output from neural network
!     en_grads:         derivatives as described above
!       en_grads(:,i):  gradient of energy i

      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

      real*8 en_grads(maxnout,maxpout)

      integer j

      en_grads=0.0D0

      do j=1,len_out
         en_grads(j,j)=1.0D0
      enddo
      
      end
