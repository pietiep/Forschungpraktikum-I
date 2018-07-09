**** Define model function used to generate output pattern.
*** 
***  So far only transformations of the reference output are supported.
***
****Conventions:
***
*** nnmodel:   subroutine generating reference output patterns
*** nndiab:    subroutine generating diabatic model out of ANN output
*** nnadia:    subroutine generating adiabatic energies out of ANN
***            output.
***
*** neupop:    neuronal Population vector: contains number of neurons for each layer
***
*** neu_in:    actual number of input neurons
*** neu_out:   actual number of output neurons
*** 
*** pat_in:    input patterns
***  pat_in(i,N):   value of ith input neuron for pattern N
*** 
*** pat_out:   desired output patterns
***  pat_out(i,N):  desired value of ith output neuron for pattern N
***
***************************************************************************
****Note:
*** neu_in and neu_out, if needed, should ideally not be hardcoded.
*** Instead they should be passed by the main program to ensure
*** matching dimensionalities.
***
***************************************************************************


      subroutine nnmodel(pat_in,pat_out,neupop)
      implicit none

!     Internal variables and parameters
!     offsets:       offset params for tmcs
!     scales:        scaling factors for tmcs
!     ppars:         polynomial parameters for tmcs
!
!     tmcs:          primitive coordinates transformed into tmcs 
!     symtmcs:       symmetry transformed tmcs.
!     pat_in:        input coordinates as generated previously
!     trans_in:      transformed input coords.
!     hmodel:        model hamitonian
!     diab_terms:    monomial terms for diabatic model to be fitted.

      include 'nnparams.incl'
      include 'nndbg.incl'
      include 'params.incl'
      include 'nncommon.incl'

      real*8  dtmc1
      real*8  pat_in(maxnin),pat_out(maxpout)
      integer neupop(*)

      integer neu_in,neu_out
      real*8 x
!      integer n,k

!     sinc-function
      x=pi*dsqrt(dsqrt(pat_in(1)**2 + pat_in(2)**2))
      pat_out(1)=dsin(x)/x

!     sine wave
!      pat_out(1)=0.5d0 + 0.25d0*dsin(3*pat_in(1)*pi)

      
      end

!--------------------------------------------------------------------------------------

      subroutine nnadia(coords,nn_out,energies)
      implicit none
!     returns adiabatic energies for given parameters
!     
!     coords:      vector containing symmetry-adapted coordinates.
!     nn_out:      output from NN
!
      include 'nnparams.incl'

      real*8 coords(maxnin),nn_out(maxnout),energies(maxnout)
      
      energies=nn_out
      
      end



