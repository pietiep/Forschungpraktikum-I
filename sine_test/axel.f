************************************************************************
***                               aXel
***      convergence accelerators for ANN's Marquardt-Levenberg
***
************************************************************************

      subroutine mqlimstep(W,B,wbsteps,wbnorm,lay_end,wei_end)
      implicit none
!     Rescales step if stepsize restrictions are violated.
!
!     Depending on the size of each parameter, this subroutine
!     establishes a maximum allowed size for the parameter's
!     corresponding step.  This maximum size is absolute for small
!     parameter sizes (below the given threshold), but scales relatively
!     to the current parameter value for larger ones.  The entire step
!     vector is normalized in such a way that the largest offending
!     stepsize is reduced to it's maximum allowed value.

!     wei_end:   position of last defined weight in W-vector
!     lay_end:   position of last defined bias in B-vector
!     len_in:    number of input-neurons. Since they have no biases
!                B(1:len_in) is meaningless.
!
!     W:         weight matrix vector
!     B:         bias vector
!     wbsteps:   steps for weights and biases, same order as a single
!                jacobian-row.
!
!     stepsize-control parameters for WEIghts and BIases
!
!     *_alim:    threshold for switching from relative to absolute scaling 
!     *_ascale:  multiplicative factor by which parameter size is multiplied
!                to determine relative maximum step size
!     *_amax:    hard step limit for small parameter values

      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

      real*8  W(maxwei),B(neucap),wbsteps(wbcap)
      real*8  wbnorm
      integer lay_end,wei_end

      real*8  parsize,stepsize,steplimit
      real*8  scale,stepscale
      integer jac_end
      integer largesteps

      integer k

      jac_end=wei_end+lay_end-len_in

!     set rescaling factor to impossible value
      scale=-1.0d0

!     set violation counter to 0
      largesteps=0

      do k=1,wei_end
         parsize=dabs(W(k))
         stepsize=dabs(wbsteps(k))
         
!        determine maximal allowed step size for current
!        parameter.
         if (parsize.gt.wei_alim) then
            steplimit=wei_ascale*parsize
         else
            steplimit=wei_amax
         endif

         if (stepsize.gt.steplimit) then
!           determine rescaling factor to normalize current parameter step
            stepscale=stepsize/steplimit
            if (stepscale.gt.scale) then
               scale=stepscale
            endif
            largesteps=largesteps+1
         endif
      enddo

      do k=len_in+1,lay_end
         parsize=dabs(B(k))
         stepsize=dabs(wbsteps(wei_end+k-len_in))
         
!        determine maximal allowed step size for current
!        parameter.
         if (parsize.gt.bi_alim) then
            steplimit=bi_ascale*parsize
         else
            steplimit=bi_amax
         endif

         if (stepsize.gt.steplimit) then
!           determine rescaling factor to normalize current parameter step
            stepscale=stepsize/steplimit
            if (stepscale.gt.scale) then
               scale=stepscale
            endif
            largesteps=largesteps+1
         endif
      enddo

!     scale entire step vector with the largest found factor if any violation
!     has been found
      if (largesteps.gt.0) then
         if (dbg) then
            write(6,400) largesteps
            write(6,401) scale
            write(6,402) wbnorm,wbnorm/scale
         endif
         if (ldbg) then
            write(*,'(A)') 'OLD WSTEPS:'
            write(*,smform) wbsteps(1:wei_end)
            write(*,'(A)') 'OLD BSTEPS:'
            write(*,smform) wbsteps(wei_end+1:jac_end)
         endif

         do k=1,jac_end
            wbsteps(k)=wbsteps(k)/scale
         enddo
         wbnorm=wbnorm/scale

      endif

 400  format('WBSTEPS:',I6,'violations of step size limits ',
     >       'have been found.')
 401  format('dividing step vector by rescaling factor of:',ES12.4)
 402  format('OLD WBNORM:',ES12.4,'NEW WBNORM:',ES12.4)

      end

            
