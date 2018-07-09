!--------------------------------------------------------------------------------------

      subroutine nnmarq(sqerr,mqpar,mqfact,W,B,wterr,pat_in,pat_out,
     >                  sqopt,npat,maxbpit,mingrad,minwbstep,maxfails)
      implicit none
!     Optimizes weights and biases using least squares errors
!     and Marquard Levenberg 
!     nlay:      number of given layers
!     npat:      number of given pattern pairs
!     sqerr:     (weighted) sum of squares error 
!     gradnorm:  norm of error gradient
!     wbnorm:    norm of steps
!     last:      sqerr of prev. iteration
!     sqopt:     error threshold
!     mingrad:   error gradient threshold
!     minwbstep: threshold for wbnorm
!     maxbpit:   max. number of iterations for ML-algorithm
!     update:    Decides whether or not jsquare and grad need to be reevaluated.
!     fails:     number of unsuccessful steps
!     maxfails:  maximum number of fails
!     macroit:   number of J-updates
!
!     pat_in:    input patterns
!     pat_out:   desired output patterns
!      pat_*(i,N):   value of ith in-/output neuron for pattern N
!
!     wterr:     weight factors for each element of the error vector e
!
!     J:         Jacobian matrix. A single row consists of matrix elements
!                corresponding to  all Weights as they are kept in memory (W),
!                followed by all Biases. It is never fully kept in memory.
!                Instead, required matrix elements are generated when needed.
!
!     e:         Error vector. It consists of the differences between the NN's
!                output-vector and the desired output patterns, for all patterns.
!                Like J it is never really kept in memory.
!
!     jsquare:   J^T J
!     wbsteps:   Steps for weights and biases, same order as a single jacobian-row.
!     mqpar:     initial parameter (lambda) for Marquard-Levenberg
!     mqfact:    factor by which lambda is multiplied/divided after each iteration
!     wsave:     saves W with best outcome
!     bsave:     saves B with best outcome
!
!     grad:      total error vector transformed by transposed jacobian
!     W:         weight matrix vector
!     B:         bias vector
!     typop:     type population matrix
!     laystr:    layer structure matrix

      include 'nnparams.incl'
      include 'nndbg.incl'
      include 'nncommon.incl'

      integer npat,maxbpit,maxfails
      real*8  W(*),B(*)
      real*8  pat_in(maxnin,*),pat_out(maxpout,*),wterr(*)
      real*8  sqerr,sqopt,mqfact,mqpar,mingrad,minwbstep
      
      real*8  wsave(maxwei),bsave(neucap),wbsteps(wbcap)
      real*8  grad(wbcap)
      real*8  jsquare(wbcap,wbcap)
      real*8  lambda,last,gradnorm,wbnorm
      integer pos_out,len_err
      integer lay_end,wei_end,jac_end
      integer fails,macroit
      integer j
      character*30 fname
      logical update

!     Initialize values
      lambda=mqpar

      pos_out=laystr(2,nlay)-1 !increment leading to output layer pos.

!     position of last defined neuron/W-matrix element
      lay_end=pos_out+len_out  
      wei_end=len_out*laystr(1,nlay-1) + laystr(3,nlay-1) - 1 

      len_err=npat*len_out            !length of error vector
      jac_end=wei_end+lay_end-len_in  !width of jacobian matrix

!     evaluate initial jsquare and gradient

      call mkjsq(W,B,wterr,jsquare,last,grad,gradnorm,
     >           jac_end,pat_in,pat_out,wei_end,npat)

      if (dbg) then
         write(*,asline)
         write(*,'(A)') 'INITIAL WEIGHTS:'
         write(*,mform) W(1:wei_end)
         write(*,'(A)') 'INITIAL BIASES:'
         write(*,mform) B(len_in+1:lay_end)
      endif

      call mkerr(W,B,wterr,sqerr,pat_in,pat_out,npat)

      wsave(1:wei_end)=W(1:wei_end)
      bsave(1:lay_end)=B(1:lay_end)
      fails=0
      macroit=0
      update=.true.
      
      if (vbs) then
         write(*,'(A1,A8,4A15)') '#','ITER','RMS','GRADNORM',
     >        'WBNORM','LAMBDA'

         write(*,'(I9,3ES15.3,ES15.2)') 0, sqrt(sqerr), gradnorm,
     >        wbnorm, lambda
      endif

!     Iterates over maximum number of backprop. !? wrong name.
      do j=1,maxbpit
!        evaluate step
         call marqstep(jsquare,wbsteps,lambda,jac_end,grad,wbnorm)

         if (limstep) then
!           modify step for better convergence
            call mqlimstep(W,B,wbsteps,wbnorm,lay_end,wei_end)
         endif

!        apply changes
         call mqupdate(W,B,wbsteps,lay_end,wei_end)

!        check for improvement
         call mkerr(W,B,wterr,sqerr,pat_in,pat_out,npat)

         if (vbs) then
            write(*,'(I9,3ES15.3,ES15.2)') j, sqrt(sqerr), gradnorm,
     >           wbnorm, lambda
         endif

         if (dbg) then
            write(*,sline)
            write(*,*) 'LAST:',sqrt(last),'RMS:',sqrt(sqerr)
            write(*,*) 'LAMBDA:', lambda
            write(*,*) 'GRADNORM:', gradnorm
            write(*,*) 'WBNORM:', wbnorm
         endif

         if ((1.0D0 - zero)*last < sqerr) then
!           if improvement becomes negligible (or none occurs),
!           count as failure
            W(1:wei_end)=wsave(1:wei_end)
            B(1:lay_end)=bsave(1:lay_end)
            lambda=lambda*mqfact
            update=.false.
            fails=fails+1
            if (dbg) then
               if (last >= sqerr) then
                  write(*,'(A)') 'CHANGE IN ERROR IRRELEVANT.'
               endif
               write(*,'(A)') 'UNDO, WSTEPS:'
               write(*,smform) wbsteps(1:wei_end)
               write(*,'(A)') 'BSTEPS:'
               write(*,smform) wbsteps(wei_end+1:jac_end)
            endif
         else               
!           save changes
            wsave(1:wei_end)=W(1:wei_end)
            bsave(1:lay_end)=B(1:lay_end)
            if (lambda > mqfact*zero) then
               lambda=lambda/mqfact
            endif
            update=.true.
            if (ldbg) then
               write(*,'(A)') 'SAVE, WSTEPS:'
               write(*,smform) wbsteps(1:wei_end)
               write(*,'(A)') 'BSTEPS:'
               write(*,smform) wbsteps(wei_end+1:jac_end)
            endif

!           check for convergence
            if ((wbnorm <= minwbstep).and.(gradnorm <= mingrad)) exit

            if (.not.ignthresh) then
!              if considered relevant, check absolute error
               if (sqerr <= sqopt) exit
            endif

         endif

         if (fails > maxfails) then
!           give up
            if (dbg) write(*,'(A)') 'MAXFAILS REACHED, GIVE UP'
            exit
         endif


         if (dbg) then
            if (update) then 
               write(*,*) 'SAVED WITH',lambda*mqfact
               write(*,'(A16,2(I4,'':''),I4)') 'SAVE:UNDO:TOTAL',
     >                  macroit+1,j-macroit-1,j
            else
               write(*,*) 'UNDONE WITH',lambda/mqfact
               write(*,'(A16,2(I4,'':''),I4)') 'SAVE:UNDO:TOTAL',
     >                  macroit,j-macroit,j
            endif
         endif

         if (update) then
!        reevaluate jacobian
            last=sqerr
            fails=0
            macroit=macroit+1

            call  mkjsq(W,B,wterr,jsquare,sqerr,grad,gradnorm,
     >                  jac_end,pat_in,pat_out,wei_end,npat)


!            if (vbs) write(*,'(I9,4ES20.4)') j,sqerr,gradnorm,wbnorm,
!     >      lambda
         endif

         if (dbg) then
            write(*,'(A)') 'WEIGHTS:'
            write(*,mform) W(1:wei_end)
            write(*,'(A)') 'BIASES:'
            write(*,mform) B(len_in+1:lay_end)
         endif

         if ((dbg.or.vbs).and.(.not.rats)) then
            if (j.le.1000) then
               write(fname,'(A10,A6,I4.4,A4)') nnfdir,'NN_fit',j,'.dat'
               call nnshowcase(W,B,pat_in,pat_out,npat,fname)
               if (dbg) write(*,*) 'MADE ',fname
            endif
         endif   
      enddo

      if (j >= maxbpit) then
         if (dbg.or.vbs) then 
            write(*,'(A)') '#CONVERGENCE ERROR: '
     >           // 'ITERATIONS EXCEEDED MAXBPIT'
            write(*,*) '#MAXBPIT:', maxbpit
         endif
         if (rats) then
            write(unit=perfunit,fmt=401) dsqrt(sqerr),wbnorm,gradnorm,
     >           macroit,j-1
         endif
      else if (fails > maxfails) then
         if (dbg) then
            write(*,'(A)') '#CONVERGENCE ERROR: MAXFAILS REACHED.'
            write(*,*) '#MAXFAILS:', maxfails
         endif
         if (rats) then
            write(unit=perfunit,fmt=400) dsqrt(sqerr),wbnorm,
     >           gradnorm,macroit,j-1,'#FAILED'
         endif
      else
         if (vbs) then
            if (wbnorm <= minwbstep) then
               if (gradnorm <= mingrad) then
                  write(6,'(A)') '#CONVERGENCE REACHED.'
     >                 // ' (GRADIENT & STEP)'
               endif
               write(6,'(A)') '#CONVERGED GRADIENT ONLY.'
            else if (gradnorm <= mingrad) then
               write(6,'(A)') '#CONVERGED STEPSIZE ONLY.'
            endif
         endif
         if (rats) then
            write(unit=perfunit,fmt=400) dsqrt(sqerr),wbnorm,
     >           gradnorm, macroit,j-1,'#CONVERGED'
         endif
      endif

 400  format(3ES10.2,4X,I6,"/",I6,4X,A)
 401  format(3ES10.2,4X,I6,"/",I6)

      if ((dbg.or.vbs).and.(.not.rats)) then
         fname=trim(nnfdir)//'NNDATA.dat'
         call nnshowcase(W,B,pat_in,pat_out,npat,fname)
      endif

      if (dbg) then
         j=j-1
         write(*,asline)
         write(*,'(A)') 'OPT END.'
         write(*,'(A12,I9)') 'ITERATIONS:',j
         write(*,'(A9,I12)') 'MACROIT:', macroit
         write(*,'(A26,3(I4,'' :''),F6.2,''%'')') 'SAVED:UNDONE:TOTAL'
     >           // ':%SAVED', macroit,j-macroit,j,
     >           1.0D2*dble(macroit)/dble(j)
         write(*,'(A)') 'FINAL W:'
         write(*,mform)  W(1:wei_end)
         write(*,'(A)') 'FINAL B:'
         write(*,mform) B(1:lay_end)
         write(*,*) 'FINAL RMS:', sqrt(sqerr)
         write(*,*) 'FINAL LAMBDA',lambda*mqfact
         write(*,asline)
      endif

      end

!--------------------------------------------------------------------------------------

      subroutine mkjsq(W,B,wterr,jsquare,sqerr,grad,gradnorm,jac_end,
     >                 pat_in,pat_out,wei_end,npat)
      implicit none
!     Directly evaluates J^T J and gradient. 
!     Since jsquare is symmetric, only the upper triangle is evaluated.
!     nlay:      number of given layers
!     npat:      number of given pattern pairs
!     sqerr:     (weighted) sum of squares error
!     wei_end:   position of last defined W-matrix-element in W-vector!
!     jac_end:   number of all weights and biases together
!
!     pat_in:    input patterns
!     pat_out:   desired output patterns
!      pat_*(i,N):   value of ith in-/output neuron for pattern N
!
!     wterr:     weight factors for each element of the error vector e
!
!     jacrow:    One row of the jacobian matrix. It consists of matrix elements
!                corresponding to  all Weights as they are kept in memory (W),
!                followed by all Biases.
!
!     jsquare:   J^T J
!
!     grad:      total error vector transformed by transposed jacobian
!     gradnorm:  norm of gradient grad
!
!     del:       neuron error vector
!     pat_err:   segment of the total error vector for one pattern.
!     deriv:     activation function derivatives
!     W:         weight matrix vector
!     B:         bias vector
!     L:         layer vector
!     typop:     type population matrix
!     laystr:    layer structure matrix
!      laystr(1,N):   number of neurons in layer N
!      laystr(2,N):   starting pos. for layer N
!      laystr(3,N):   starting pos. for weight matrix from layer N to N+1

      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

      integer npat,wei_end,jac_end
      real*8  W(*),B(*),jsquare(wbcap,wbcap),grad(wbcap)
      real*8  pat_in(maxnin,*),pat_out(maxpout,*),wterr(*)
      real*8  sqerr,gradnorm
      
      real*8  L(neucap),jacrow(wbcap)
      real*8  del(neucap),deriv(neucap),pat_err(maxpout)
      real*8  outgrads(maxnout,maxpout)
      integer pos_out,pos_jac
      integer j,k,n,m

      pos_out=laystr(2,nlay)-1 !increment leading to output layer pos.

      sqerr=0.0D0
      del=0.0D0

      do j=1,jac_end
         grad(j)=0.0D0
         do k=j,jac_end
            jsquare(j,k)=0.0D0
         enddo
      enddo

      do n=1,npat
         do k=1,len_in
            L(k)=pat_in(k,n)
         enddo

         call propagate(W,B,L,deriv,typop,laystr,nlay)
         pos_jac=(n-1)*inp_out   !increment for err-vector pos.

!        evaluate pat_err
         call nnerror(pat_err,pat_in(1,n),pat_out(1,n),L(pos_out+1))
!        evaluate output value derivatives
         call nnoutgrad(outgrads,pat_in(1,n),L(pos_out+1))

         do k=1,inp_out
            sqerr=sqerr+(wterr(pos_jac+k)*pat_err(k))**2
!           prepare sensitivity vector
            del=0.0D0
            do j=1,len_out
               del(pos_out+j)=-deriv(pos_out+j)*outgrads(j,k)
            enddo
!           evaluate J-Matrix elements for a single row
            call backprop(del,W,deriv,laystr,nlay)
            call nnjacob(del,L,jacrow,wei_end)
!           build up jsquare and gradient
            do j=1,jac_end
               do m=j,jac_end
                  jsquare(j,m)=jsquare(j,m)+jacrow(j)*jacrow(m) 
               enddo
               grad(j)=grad(j)+jacrow(j)*pat_err(k)
            enddo
         enddo
      enddo

      gradnorm=sqrt(dot_product(grad(1:jac_end),grad(1:jac_end)))

      end

!--------------------------------------------------------------------------------------

      subroutine nnjacob(del,L,jacob,wei_end)
      implicit none
!     Evaluates one row of the Jacobian
!
!     nlay:      number of given layers
!     nprop:     number of single-layer propagations 
!     neu_io:    number of neurons in giv./prev. layer
!     neu_oi:    number of neurons in prev./giv. layer
!     poslay:    starting position of layer in L- and B-vector
!     poswei:    starting position of weight-matrix in W-vector
!     wei_end:   position of last defined W-matrix-element in W-vector
!
!     del:     neuron error vector      
!     L:       layer vector
!     jacob:   Jacobian Matrix. A single row consists of all weights as they
!              are kept in memory (W), followed by all biases.
!     laystr:  layer structure matrix
!
!      laystr(1,N):   number of neurons in layer N
!      laystr(2,N):   starting pos. for layer N
!      laystr(3,N):   starting pos. for Weight Matrix from layer N to N+1

      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

      integer wei_end
      real*8 del(*),L(*),jacob(*)

      integer poslay,poswei,nprop,neu_io,neu_oi
      integer k 

      nprop=nlay-1

      neu_io=laystr(1,1) !number of input neurons ('layer N')
      poslay=laystr(2,1) 
      poswei=laystr(3,1)
      do k=2,2*(nprop/2),2
         neu_oi=laystr(1,k) !number of output neurons ('layer N+1')

!        input neurons have no bias (no J-elements), hence -len_in
         call jalay(neu_io,neu_oi,del(poslay),L(poslay),jacob(poswei),
     >              jacob(wei_end+poslay-len_in))

         poslay=laystr(2,k)
         poswei=laystr(3,k)
         neu_io=laystr(1,k+1) !new number of output neurons ('layer N')

!        former output neurons are now input, _io and _oi switch places!
!        (done for efficiency)

         call jalay(neu_oi,neu_io,del(poslay),L(poslay),jacob(poswei),
     >              jacob(wei_end+poslay-len_in))

         poslay=laystr(2,k+1)
         poswei=laystr(3,k+1)
      enddo

      do k=1,mod(nprop,2) !for odd nprop one iteration is left
         neu_oi=laystr(1,nlay) 
         call jalay(neu_io,neu_oi,del(poslay),L(poslay),jacob(poswei),
     >              jacob(wei_end+poslay-len_in))
      enddo

      end

!--------------------------------------------------------------------------------------

      subroutine jalay(neu_in,neu_out,del,L,welems,belems)
      implicit none      
!     Evaluates Matrix-elements corresponding to one NN-layer.
!
!     neu_in:  number of neurons in layer N
!     neu_out: number of neurons in layer N+1
!
!     del:       neuron error vector
!     L:         layer vector (beginning at layer N)
!     welems:    elements corresponding to W-matrix
!     belems:    elements corresponding to B-vector
!
      include 'nnparams.incl'
      include 'nndbg.incl'

      integer neu_in,neu_out
      real*8 del(*)
      real*8 welems(neu_in,*),belems(*),L(*)

      integer neu_tot,pos_out
      integer j,k

      neu_tot=neu_in+neu_out    !total number of neurons in both layers
      pos_out=neu_in+1         !position of first element in layer N+1

      do j=pos_out,neu_tot
         belems(j)=del(j)
      enddo
      do j=1,neu_out
         do k=1,neu_in
            welems(k,j)=L(k)*del(neu_in+j)
         enddo
      enddo

      end

!--------------------------------------------------------------------------------------

      subroutine mkerr(W,B,wterr,sqerr,pat_in,pat_out,npat)
      implicit none
!     Evaluates sqerr only.
!     nlay:      number of given layers
!     npat:      number of given pattern pairs
!     sqerr:     (weighted) sum of squares error
!
!     pat_in:    input patterns
!     pat_out:   desired output patterns
!      pat_*(i,N):   value of ith in-/output neuron for pattern N
!
!     wterr:     weight factors for each element of the error vector e
!     pat_err:   segment of the total error vector for one pattern.
!
!     deriv:     activation function derivatives (discarded)
!     W:         weight matrix vector
!     B:         bias vector
!     L:         layer vector
!     deriv:     activation function derivatives
!     typop:     type population matrix
!     laystr:    layer structure matrix
!      laystr(1,N):   number of neurons in layer N
!      laystr(2,N):   starting pos. for layer N
!      laystr(3,N):   starting pos. for weight matrix from layer N to N+1

      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

      integer npat
      real*8  W(*),B(*)
      real*8  pat_in(maxnin,*),pat_out(maxpout,*),wterr(*)
      real*8  sqerr
      
      real*8  L(neucap),deriv(neucap),pat_err(maxpout)
      real*8  werr
      integer pos_out,pos_jac
      integer n,k

      pos_out=laystr(2,nlay)-1 !increment leading to output layer pos.
      
      sqerr=0.0D0

      do n=1,npat
         L=0.0d0
         do k=1,len_in
            L(k)=pat_in(k,n)
         enddo
         call propagate(W,B,L,deriv,typop,laystr,nlay)
         pos_jac=(n-1)*len_out   !increment for err-vector pos.
!        evaluate pat_err
         call nnerror(pat_err,pat_in(1,n),pat_out(1,n),L(pos_out+1))
         do k=1,len_out
            werr=wterr(pos_jac+k)*pat_err(k)
            sqerr=sqerr+werr**2
         enddo
      enddo

      end

!--------------------------------------------------------------------------------------

      subroutine marqstep(jsquare,wbsteps,lambda,jac_end,grad,wbnorm)
      implicit none
!     Evaluates steps for W and B using the equation
!     (J^T J + lambda*1) wbsteps = -J^T e,
!     where "J^T e" is already given as grad and "J^T J" as jsquare,
!     
!     wbsteps:   Steps for weights and biases, same order as a single jacobian-row.
!     lambda:    parameter for Marquard-Levenberg
!     jac_end:   number of all weights and biases together
!     grad:      error vector transformed by transposed jacobian
!     jsquare:   J^T J. Only the upper triangle of it is required, 
!                that is jsquare(i,j) needs only to be defined for i>=j.
!     jsqcop:    working copy of jsquare
!     
      include 'nnparams.incl'
      include 'nndbg.incl'

      real*8  jsquare(wbcap,wbcap),grad(wbcap),wbsteps(wbcap)
      real*8  lambda,wbnorm
      integer jac_end

      real*8  jscop(wbcap,wbcap)
      integer je
      integer k

!     shorthands for better readability
      je=jac_end 

      jscop(1:je,1:je)=jsquare(1:je,1:je)

      do k=1,jac_end
         jscop(k,k)=jscop(k,k)+lambda
      enddo

!     solve for wbsteps using Cholesky decomposition
      call choldcsol(jscop,grad,wbsteps,je,wbcap)

      wbnorm=sqrt(dot_product(wbsteps(1:jac_end),wbsteps(1:jac_end)))

      end

!--------------------------------------------------------------------------------------

      subroutine mqupdate(W,B,wbsteps,lay_end,wei_end)
      implicit none
!     Updates W and B according to wbsteps.
!
!     wei_end:   position of last defined weight in W-vector
!     lay_end:   position of last defined bias in B-vector
!     len_in:    number of input-neurons. Since they have no biases
!                B(1:len_in) is meaningless.
!
!     W:         weight matrix vector
!     B:         bias vector
!     wbsteps:   steps for weights and biases, same order as a single jacobian-row.

      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

      real*8  W(maxwei),B(neucap),wbsteps(wbcap)
      integer lay_end,wei_end
      integer k

      do k=1,wei_end
         W(k)=W(k)-wbsteps(k)
      enddo

      do k=len_in+1,lay_end
         B(k)=B(k)-wbsteps(wei_end+k-len_in)
      enddo
      
      end

