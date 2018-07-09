

******************************************************************************
***                            iNNterface
***                    connecting ANN with genetic
***                        by David Williams
***                         & Jerome Collet
***
*** name(s) [synonyms or equivalents]
*** wertnum [npat] 
*** q,x1    [pat_in]
*** y       [pat_out]
*** par     [W,B]
*** num     [npat]
***
***
***
***
***
***
***
***
***
***
***
***
***
******************************************************************************

      subroutine NNmin(npat,par,act,rms,pat_in,pat_out,wterr,
     >                 maxbpit,maxfails,sqopt,mingrad,minwbstep)
      implicit none
!     Non-linear least-squares fitting using feed forward NN and
!     Marquardt-Levenberg learning.
!
!     q:         (primitive) input coordinates
!     x1:        (transformed) input coordinates
!     x2:        alternate set of input coordinates (redundant)
!     y:         output-values
!?    [Note: genetic-style coords currently not supported]
!
!     parnum:    number of parameters
!     par:       Parameter vector containing all weights and biases
!                (weights first, biases second). Weights are in the
!                natural order of the memory: 
!                W1(1,1) W1(2,1) ... W1(1,2) ... W2(1,1) ...  
!                ... B1(1) B1(2) ... B2(1) ... 
!                these may be followed by further parameters 
!                *not* used by ANN.
!     act:        vector, indicating the active parameters
!     rms:       (weighted) root mean square
!
!     name [ANN-synonym]
!******************************************************************
!     npat:           number of given pattern pairs
!     sqerr:          (weighted) sum of squares error 
!     sqopt:          error threshold
!     mingrad:        error gradient threshold
!     minwbstep:      threshold for wbnorm
!     maxbpit:        max. number of iterations for ML-algorithm
!     maxfails:       maximum number of fails
!                     
!     pat_in:         input patterns
!     pat_out:        desired output patterns
!      pat_*(i,N):       value of ith in-/output neuron for pattern N
!                     
!     wt [wterr]:     weight factors for each element of the error vector e
!                     
!     wbsteps:        Steps for weights and biases, same order as a single jacobian-row.
!     mqpar:          initial parameter (lambda) for Marquard-Levenberg
!     mqfact:         factor by which lambda is multiplied/divided after each iteration
!                     
!     par [W]:        weight matrix vector
!     par [B]:        bias vector
!
!     common-block /ann/
!******************************************************************
!     nlay:           number of given layers
!     typop:          type population matrix
!     laystr:         layer structure matrix
!        laystr(1,N):    number of neurons in layer N
!        laystr(2,N):    starting pos. for layer N
!        laystr(3,N):    starting pos. for weight matrix from layer N to N+1

      include 'params.incl'
      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

!      real*8  q,x1(qn,*),x2(qn,*),y(*),wt(*)
      real*8  par(*)
      real*8  pat_in(maxnin,*),pat_out(maxpout,*)
      real*8  wterr(*)
      real*8  rms,sqopt,mingrad,minwbstep
      integer npat
      integer maxbpit,maxfails
      integer act(*)

      real*8  sqerr
      real*8  mqpar,mqfact

      integer pos_out
      integer lay_end,wei_end,bi_pos,bi_end
      logical skip

      integer k

      pos_out=laystr(2,nlay)-1 !increment leading to output layer pos.

!     total number of neurons / W-matrix elements
      lay_end=pos_out+len_out  
      wei_end=len_out*laystr(1,nlay-1) + laystr(3,nlay-1) - 1 
!     starting position and end position of Bias vector in par
      bi_pos=wei_end - len_in +1 !fills the biases of input neurons with garbage
      bi_end=bi_pos + lay_end -1

      skip=.false. !nothing happened yet

      if (dbg) write(6,*) 'NNmin entered'
      if (dbg) write(6,*) 'NPAT:', npat

      rms=infty

!     hardcoded for now. If custom changes become necessary, add a card
      mqpar=1.0D-2
      mqfact=10.0D0

!     fits currently pattern pairs (x1,y)

      if (all(act(1:bi_end).eq.0)) then
         write(6,*) 'Parameters frozen, skip to output'
         return
      else if (any(act(1:bi_end).eq.0)) then
         write(6,*) 'ERROR: ACTIVITIES NOT IMPLEMENTED FOR ANN.'
         stop
      endif

      if (ldbg) then
         write(6,sline)
         write(6,'(A)') 'W:'
         write(6,mform) par(1:wei_end)
         write(6,sline)
         write(6,'(A)') 'B:'
         write(6,mform) par(bi_pos:bi_end)
         write(6,sline)
         write(6,'(A)') 'TYPOP:'
         do k=1,nlay
            write(6,'(2I6,:)') typop(1:2,k)
         enddo
         write(6,sline)
         write(6,*)
         write(6,'(A)') 'X,Y'
         do k=1,npat
            write(unit=6,advance='no',fmt='(A2,1000(ES14.6,:))') 'X:',
     >           pat_in(:,k)
            write(6,'(A3,1000(ES14.6,:))') ' Y:', pat_out(1:inp_out,k)
         enddo
      endif
      
!                                           W      ,         B 
      call nnmarq(sqerr,mqpar,mqfact,par(1:wei_end),par(bi_pos:bi_end),
     >            wterr,pat_in,pat_out,sqopt,npat,maxbpit,mingrad,
     >            minwbstep,maxfails)

      rms=sqrt(sqerr)

      end

!--------------------------------------------------------------------------------------

      subroutine neunet(x,y,par)
      implicit none
!     Wrapper for ANN's propagate
!
!     y:         neural network output value(s)
!     x:         input coordinate set
!     par:       Parameter vector containing all weights and biases
!                (weights first, biases second). Weights are in the
!                natural order of the memory: 
!                W1(1,1) W1(2,1) ... W1(1,2) ... W2(1,1) ...  
!
!     nlay:      number of given layers (in common block /ann/)
!                
!     L:         layer vector
!     deriv:     activation function derivatives (currently discarded)
!     typop:     type population matrix (in common black /ann/)
!     laystr:    layer structure matrix (in common block /ann/)
!                
!      laystr(1,N):   number of neurons in layer N
!      laystr(2,N):   starting pos. for layer N
!      laystr(3,N):   starting pos. for Weight Matrix from layer N to N+1

      include 'nnparams.incl'
      include 'nncommon.incl'

      real*8 y(*),x(*),par(*)
      
      real*8 L(neucap)
      real*8 deriv(neucap)
      integer wei_end,lay_end
      integer pos_out
      integer bi_pos,bi_end

      pos_out=laystr(2,nlay)-1 !increment leading to output layer pos.

!     total number of neurons / W-matrix elements
      lay_end=pos_out+len_out  
      wei_end=len_out*laystr(1,nlay-1) + laystr(3,nlay-1) - 1 
!     starting position and end position of Bias vector in par
      bi_pos=wei_end - len_in +1 !fills the biases of input neurons with garbage
      bi_end=bi_pos + lay_end -1

!     write coordinate values on input neurons
      L(1:len_in)=x(1:len_in)

      call propagate(par(1:wei_end),par(bi_pos:bi_end),L,deriv,typop,
     >               laystr,nlay)

!     write output on y
      y(1:len_out)=L(pos_out+1:pos_out+len_out)
      
      end

!--------------------------------------------------------------------------------------

      subroutine traiNNsets(nset,npar,npat,par,act,rms,pat_in,pat_out,
     >                      wt,maxbpit,maxfails,sqopt,mingrad,minwbstep)
      implicit none
!     Uses NNmin to train all nset initialized neural networks using NNmin.
!     The first parameterset with be overwritten with the ANN yielding the
!     smallest rms. Remaining sets will be sorted in ascending order.
!
!     par:       Parameter vector containing all weights and biases
!                (weights first, biases second). Weights are in the
!                natural order of the memory: 
!                W1(1,1) W1(2,1) ... W1(1,2) ... W2(1,1) ...  
!                ... B1(1) B1(2) ... B2(1) ... 
!                these may be followed by further parameters 
!                *not* used by ANN.
!     act:       vector, indicating the active parameters
!     rms:       (weighted) root mean square for each parameter set.
!      rms(1):        best rms achieved, overwrites set #1.
!      rms(n):        final rms of set #n.

!
!     name [ANN-synonym]
!******************************************************************
!     nset:           number of ANNs to be optimized.
!     npat:           number of given pattern pairs
!     sqerr:          (weighted) sum of squares error 
!     sqopt:          error threshold
!     mingrad:        error gradient threshold
!     minwbstep:      threshold for wbnorm
!     maxbpit:        max. number of iterations for ML-algorithm
!     maxfails:       maximum number of fails
!                     
!     pat_in:         input patterns
!     pat_out:        desired output patterns
!      pat_*(i,N):       value of ith in-/output neuron for pattern N
!                     
!     wt [wterr]:     weight factors for each element of the error vector e
!                     
!     wbsteps:        Steps for weights and biases, same order as a single jacobian-row.
!     mqpar:          initial parameter (lambda) for Marquard-Levenberg
!     mqfact:         factor by which lambda is multiplied/divided after each iteration
!                     
!     par [W]:        weight matrix vector
!     par [B]:        bias vector
!

      include 'params.incl'
      include 'common.incl'
      include 'nnparams.incl'   !this includes ANN-parameters 
      include 'nndbg.incl'
      include 'nncommon.incl'

      real*8 par(npar,nx),rms(nx),wt(*)
      real*8 pat_in(*),pat_out(*)
      real*8 sqopt,mingrad,minwbstep
      integer act(*)
      integer nset,npar,npat
      integer maxbpit,maxfails

      real*8 rms_copy(nx),par_copy(npar,nx)
      character*30 fname
      integer rms_pos(nx)
      integer wei_end, bi_pos, bi_end

      real*8 loc_par(npar),loc_rms

      integer i
      

      if (rats) then
!        create logfile
         open(unit=perfunit,file=trim(perfile),status='REPLACE')
         write(unit=perfunit,fmt='("#",A9,2A10,A14)') 'RMS','WBNORM',
     >        'GRADNORM', 'UPDATE/IT.'
      endif

!     total number of W-matrix elements in vector par
      wei_end=laystr(1,nlay)*laystr(1,nlay-1) + laystr(3,nlay-1) - 1
!     starting position and end position of Bias vector in par
      bi_pos=wei_end - laystr(1,1) + 1 !fills the biases of input neurons with garbage
      bi_end=bi_pos + laystr(2,nlay) + laystr(1,nlay) -2

      if (mprun) write(6,asline)

!$omp parallel do default(shared) private(i,loc_par,loc_rms,fname)
!$omp&            schedule(dynamic)
      do i=1, nset
         if (.not.mprun) write(6,asline)
         write(6,'(A,I4.4,A)')'Neural Network #',i,': Start!'

!     make a local copy of current parameter set
         loc_par=par(:,i)

!     train ANN with ith parameter set
         call NNmin(npat,loc_par,act,loc_rms,pat_in,pat_out,
     >              wt,maxbpit,maxfails,sqopt,mingrad,minwbstep)

         write(6,'("Final rms:",ES21.14,"(#",I4.4,")")') loc_rms,i 

!     create plotting file if desired
         if ((dbg) .and. (rats)) then 
            write(fname,'(A10,A3,I4.4,A4)') nnfdir,'NN_',i,'.dat'
!                               'W'        'B'
            call nnshowcase(loc_par,loc_par(bi_pos),
     >                      pat_in,pat_out,npat,fname)
         endif

!     return optimized values to global data structures.
         par(:,i)=loc_par
         rms(i)=loc_rms

      enddo
!$omp end parallel do

      write(6,'(A)')

      if (rats) then
         close(unit=perfunit)
      endif

      write(6,asline)
      write(6,'(A)') 'Training complete.'
      
!     create copy of rms-vector to be sorted
      do i=1,nset
         par_copy(:,i)=par(:,i)
         rms_copy(i)=rms(i)
         rms_pos(i)=i
      enddo

!     rank individual trained NNs by rms error.
      write(6,'(A)') 'Searching for best fit..'
      call dqsort2(nset,rms_copy,rms_pos)

      write(6,'(A,ES21.4)') 'Best rms found:', rms_copy(1)
      write(6,sline)
      if (dbg) then
         write(6,'(/,A)') 'Ranking errors:'
         write(6,'(2A12)') 'FIT#','RMS'
         do i=1,nset
            write(6,'(I12,ES12.4)') rms_pos(i),rms_copy(i)
         enddo
      else
         write(6,'(/,A)') 'Ranking errors (top 10%):'
         write(6,'(2A12)') 'FIT#','RMS'
         do i=1,(nset/10)
            write(6,'(I12,ES12.4)') rms_pos(i),rms_copy(i)
         enddo
      endif
      write(6,sline)
      write(6,'(/,A)') 'Sorting parameter vector..'
      do i=1,nset
         par(:,i)=par_copy(:,rms_pos(i))
      enddo
      write(6,'(A)') 'Done.'
      end

!--------------------------------------------------------------------------------------

      subroutine NNerr(par,pat_in,pat_out,wterr,sqerr,npat)
!     uses the best parameterset in par (first set written in par) to
!     calculate pat_out via the NN, furthermore a file with the data is
!     created
      implicit none

      include 'params.incl'
      include 'nnparams.incl'
      include 'nncommon.incl'
      include 'nndbg.incl'

      real*8 par(*),wterr(*)
      real*8 pat_in(maxnin,*),pat_out(maxpout,*)
      real*8 sqerr
      integer npat
      integer wei_end,bi_pos,bi_end
      character fname*30

!     total number of neurons / W-matrix elements in vector par
      wei_end=laystr(1,nlay)*laystr(1,nlay-1) + laystr(3,nlay-1) - 1
!     starting position and end position of Bias vector in par
      bi_pos=wei_end - laystr(1,1) + 1 !fills the biases of input neurons with garbage
      bi_end=bi_pos + laystr(2,nlay) + laystr(1,nlay) -2

!                          W,           B
      call mkerr(par(1:wei_end),par(bi_pos:bi_end),wterr,sqerr,
     >               pat_in,pat_out,npat) 

      write(fname,'(A10,A13)') nnfdir,'NN_Result.dat'
!                      'W'        'B'
      call nnshowcase(par(1),par(bi_pos),pat_in,pat_out,npat,fname)

      write(6,400) sqrt(sqerr)
 400  format('RMS of given data and parameter set:',ES10.2)

      end


!--------------------------------------------------------------------------------------

      subroutine savepar(par,rms,npar)
!     saves the parameterset as parset.data, if there is an old file the
!     better rms will be used and saved as parset.data
      implicit none

      real*8 par(*),rms(*)
      integer npar

      integer i, lmax
      real*8 oldrms
      character datnam*30
      logical check

      lmax=npar+3
      datnam='parset.data'

!..   inquire looks for a file named datnam, if it is there it is
!..   assumed, that it's been made with this structure and the rms will
!..   be compared
      inquire(file=datnam,exist=check)
      if (.not. check) then
         open(10,file=datnam,status='new')
      else
         open(10,file=datnam,status='old')
         read (10,*)
         read (10,*) oldrms
         close(10)
         if (oldrms.gt.rms(1)) then
            open(10,file=datnam,status='replace')
         else
            write(6,100)
            return
         endif
      endif

!..   check if the old rms is higher than the new one, if yes save
!..   the parameters, if not, don't save them
      write(10,110)
      write(10,*) rms(1)
      write(10,120)

      do i=1, npar
         write(10,*) par(i)
      enddo

      close(10)
      write(6,130) 

100   format('The old set of parameters has a lower rms, no changes',
     >       ' saved.')
110   format('rms of the best fit until now:')
120   format('parameters of the best fit until now')
130   format('new set of parameters has been saved in parset.data')

      end

!--------------------------------------------------------------------------------------

      subroutine loadpar(par,rms,npar)
!     loads the parameterset from parset.data, in order to change use
!     the specific shellscript like in usepar.sh
      implicit none

      real*8 par(*),rms(*)
      integer npar

      integer i, lmax
      character datnam*30
      logical check

      lmax=npar+3
      datnam='parset.data'

      inquire(file=datnam,exist=check)
      if(.not. check) then
        stop 'No Parameters found, error with parset.data'
      else
        open(20,file=datnam,status='old')
        read(20,*)
        read(20,*) rms(1)
        read(20,*)
        do i=1, npar
          read(20,*) par(i)
        enddo
        write(*,*)'The parameters have been loaded!'
      endif

      end


!!--------------------------------------------------------------------------------------
!
!      subroutine NNgrid(pat_in,pat_out,npat,ska,pat_inref,pat_outref)
!!     scales the number of points, npat in pat_in & pat_out by ska
!!     (if npat=20 and ska=4 the new npat will be npat/ska=5)
!!     new pat_in is saved in pat_in, the other points are placed on
!!     pat_inref, same with pat_out
!!?    CURRENTLY NOT ADAPTED TO NEW DEIMENSIONING OF PAT_OUT (maxpout)
!
!!     the reduction is linear, an option which changes this to a
!!     statistical reduction has to be added
!      implicit none
!
!      include 'nnparams.incl'
!
!      real*8 pat_in(maxnin,*), pat_out(maxpout,*)
!      real*8 insave(maxnin,maxpats), outsave(maxnout,maxpats)
!      real*8 pat_inref(maxnin,*), pat_outref(maxpout,*)
!      integer ska, npat
!      integer i,j,k,m,n,o,p
!
!
!!.....saving of the data
!      do i=1, npat
!        do j=1, maxnin
!          insave(j,i)=pat_in(j,i)
!          pat_in(j,i)=0.D0
!        enddo
!        do k=1, maxnout
!          outsave(k,i)=pat_out(k,i)
!          pat_out(k,i)=0.D0
!        enddo
!      enddo
!
!
!!.....writing every ska datapoint on pat_in, the unused point on
!!.....pat_inref
!      do j=1, maxnin
!        m=0
!        n=0
!        do i=1, npat, ska
!!.....saving every ska point
!          m=m+1
!!        write(*,*)m,i,'m,i'
!          pat_in(j,m)=insave(j,i)
!!......place the other datapoints in pat_inref
!          do o=1, ska-1
!!        write(*,*) o,'Durchlaufnummer immer 1, sonst falsch'
!            if (npat .ge. i+o) then
!              n=n+1
!!        write(*,*) n,i+o,'n,i+o'
!              pat_inref(j,n)=insave(j,i+o)
!            endif
!          enddo
!        enddo
!      enddo
!!.....Check if every point was saved
!      if (m+n.ne.npat) stop 'ERROR: Problem with saving the input data
!     > points'
!      p=m
!
!!.....writing every ska datapoint on pat_out, the unused point on
!!.....pat_outref
!      do j=1, maxnout
!        m=0
!        n=0
!        do i=1, npat, ska
!!.....saving every ska point
!          m=m+1
!!        write(*,*)m,i,'m,i'
!          pat_out(j,m)=outsave(j,i)
!!......place the other datapoints in pat_inref
!          do o=1, ska-1
!!        write(*,*) o,'Durchlaufnummer immer 1, sonst falsch'
!            if (npat .ge. i+o) then
!              n=n+1
!!        write(*,*) n,i+o,'n,i+o'
!              pat_outref(j,n)=outsave(j,i+o)
!            endif
!          enddo
!        enddo
!      enddo
!!.....Check if every point was saved
!      if (m+n.ne.npat) stop 'ERROR: Problem with saving the output data
!     > points'
!!.....Check if for both in and out the same number of points was saved
!      if(p.ne.m) stop 'Error, new number of data points is different for
!     > pat_in and pat_out'
!
!
!
!!TEST
!!.....check to know if the right points of in have been saved
!        k=0
!        if (modulo(npat,ska) .ne. 0) k=1
!        do o=1, maxnin
!          m=0
!          n=0
!          do i=1, npat/ska+k
!            m=m+1
!            if(insave(o,m).ne.pat_in(o,i)) then
!              write(*,*) 'Error'
!              write(*,*) insave(o,m)
!              write(*,*) pat_in(o,i)
!            endif
!            do j=1, ska-1
!              if(npat.gt.m) then
!                m=m+1
!                n=n+1
!                if(insave(1,m).ne.pat_inref(1,n)) then
!                  write(*,*) 'Error'
!                  write(*,*) insave(o,m)
!                  write(*,*) pat_inref(o,n)
!                endif
!              endif
!            enddo
!          enddo
!        enddo
!!.....check to know if the right points of out have been saved
!        k=0
!        if (modulo(npat,ska) .ne. 0) k=1
!        do o=1, maxnout
!          m=0
!          n=0
!          do i=1, npat/ska+k
!            m=m+1
!            if(outsave(o,m).ne.pat_out(o,i)) then
!              write(*,*) 'Error'
!              write(*,*) outsave(o,m)
!              write(*,*) pat_out(o,i)
!            endif
!            do j=1, ska-1
!              if(npat.gt.m) then
!                m=m+1
!                n=n+1
!                if(outsave(1,m).ne.pat_outref(1,n)) then
!                  write(*,*) 'Error'
!                  write(*,*) outsave(o,m)
!                  write(*,*) pat_outref(o,n)
!                endif
!              endif
!            enddo
!          enddo
!        enddo
!!ENDTEST
!
!!.....New number of used data points
!      npat=p
!
!
!
!      end
