

!--------------------------------------------------------------------------------------

      subroutine mkinput(neupop,seed_nn,nlay,npat,scans,xranges,rtype)
      implicit none      
!     Initialize pattern pairs for later fitting and write them as
!     genetic input file.
!
!     nlay:      number of given layers
!     npat:      number of pattern-pairs desired
!     scans:     number of random scans, if applicable.
!     rtype:     type of random distribution: 
!               -3 -- even neu_in-dim. grid in ND
!               -2 -- even maxnin-dim. grid in ND
!                0 -- even distribution
!                1 -- elliptic random scans from origin.
!                     Datapoints on scans are arranged in concentric
!                     similar ellipsoids around the origin. The outermost
!                     ellipsoid is bounded by xranges(2,i) as it's
!                     principal axes. The innermost ellipsoid
!                     is the smallest one where no possible point
!                     between the two ellipsoids violates
!                     the smallest allowed value in any dimension.
!
!
!     pat_in:    input patterns
!     pat_out:   desired output patterns
!       pat_*(i,N):   value of ith in-/output neuron for pattern N
!     super:     superindex for grid generation
!     grid_dim:  grid dimension
!
!     neupop:    Neuronal Population vector: Contains number of neurons for each layer
!     xranges:   range for (random) input distribution: 
!                xranges(1,i) < pat_in(i,k) < xranges(2,i) forall k
!                xranges(3,i) may contain metadata for certain modes.
!     do_grid:   toggles grid generation internally
!     

      include 'params.incl'
      include 'common.incl'
      include 'nnparams.incl'
      include 'nndbg.incl'

      real*8 xranges(3,maxnin)
      integer neupop(*)
      integer nlay,npat,scans
      integer rtype,seed_nn

      real*8  pat_in(maxnin,maxpats),pat_out(maxpout,maxpats)
      real*8  len_xr(maxnin)
      integer neu_in,neu_out
      integer super(maxnin),grid_dim
      logical do_grid
      integer n,k

      neu_in=neupop(1) !number of input neurons (dim. of x-vector)
      neu_out=neupop(nlay) !number of output neurons (dim. of f(x)-vector)

      if (rtype.eq.-2) then
         grid_dim=maxnin
         do_grid=.true.
      else if (rtype.eq.-3) then
         grid_dim=neu_in
         do_grid=.true.
      else
         grid_dim=neu_in
         do_grid=.false.
      endif

      if (dbg.or.vbs) write(*,'(A)') 'Initializing patterns...'
      if (npat.lt.2) stop 'ERROR: TOO FEW NPATS (<2)'

      do k=1,grid_dim
         len_xr(k)=xranges(2,k)-xranges(1,k) !length of L(k)-intervals
         if (xranges(2,k).le.xranges(1,k)) then
            write(*,*) 'ERROR: INVALID RANGES: XRANGES'
            write(*,'(A8,I4)') 'XRANGE #',k
            stop
         else if (rtype.eq.1) then
            if (xranges(1,k).lt.0.0d0) then
               write(*,*) 'ERROR: INVALID RANGES:'
               write(*,*) 'BOUNDARIES CANNOT BE NEGATIVE FOR GIVEN MODE'
               write(*,'(A17,I4)') 'OFFENDING XRANGE:',k
               stop
            else if (xranges(2,k).le.0.0d0) then
               write(*,*) 'ERROR: INVALID RANGES:'
               write(*,*) 'UPPER BOUNDS MUST BE POSITIVE FOR GIVEN MODE'
               write(*,'(A17,I4)') 'OFFENDING XRANGE:',k
               stop
            endif
         endif
      enddo

!     Initialize random number generator
      call vranf(pat_in,0,seed_nn,iout)

!     Generate primitive coordinates
      if (rtype.eq.0) then
         do n=1,npat
!           Generate random numbers within [0,1)
            call vranf(pat_in(1,n),neu_in,0,iout)
!           Shift range accordingly
            do k=1,neu_in
               pat_in(k,n)=xranges(1,k)+pat_in(k,n)*len_xr(k)
            enddo
         enddo
      else if (rtype.eq.1) then
!        generate randscans
         call mkescans(pat_in,neu_in,npat,scans,xranges)
!        set plotting parameter "sets"
         sets=scans
!        specify number of data points per scan
         do k=1,scans
            ndata(k)=npat
         enddo

!        update npat to actual number of patterns
         npat=npat*scans

      else if (do_grid) then
!        Generate grid_dim-dimensional grid
         do n=1,npat**grid_dim
!        Even grid => super index equivalent to n-1 in base grid_dim
            call ibaserep(n-1,npat,super,grid_dim)
            do k=1,grid_dim
               pat_in(k,n) = xranges(1,k) 
     >                       + len_xr(k)*dble(super(k))/dble(npat-1)
            enddo
         enddo
!        update npat to actual number of patterns
         npat=npat**grid_dim
      else
         stop 'ERROR: INVALID TYPE OF RANDOM NUMBER GENERATION (RTYPE)'
      endif

!     Build model
!?    for now also transforms coords.
      write(*,*)
      do n=1,npat
         call nnmodel(pat_in(1,n),pat_out(1,n),neupop)
      enddo
      write(*,*)

      call punch_data(pat_in,pat_out,grid_dim,npat,.false.)

      if (rtype.eq.1) then
         call punch_scans(pat_in,pat_out,neu_in)
      endif

      if (dbg.or.vbs) then
         write(*,'(A)') 'Init finished.'
         write(*,'(A,I6)') 'Input dimension: ',grid_dim
         write(*,'(A,I6)') 'Output dimension: ',neu_out
      endif

      end

!--------------------------------------------------------------------------------------

      subroutine mkescans(pat_in,neu_in,npat,scans,xranges)
      implicit none
!     Generate input patterns for elliptic random scans. Datapoints
!     on scans are arranged in concentric ellipsoids around the
!     origin. The outermost ellipsoid is bounded by xranges(2,i) as it's
!     principal axes. The innermost ellipsoid is the smallest one where
!     no possible point violates the smallest allowed value in any
!     dimension.
!
!     For the sake of simplicity, the coordinate system is effectively
!     rescaled such that the outer ellipsoid becomes a unit sphere
!     and then transfomed back.
!
!     npat:      number points on the scan
!     scans:     number of scans
!     pat_in:    input patterns
!       pat_*(i,N):   value of ith in-/output neuron for pattern N
!     rmin:      radius of the smallest sphere enclosing all forbidden areas
!                in the rescaled coordinate system
!     len_step:  radius step size for rescaled coordinate system
!
!     xranges:   range for (random) input distribution: 
!                xranges(1,i) < pat_in(i,k) < xranges(2,i) forall k
!                xranges(3,i) : if it's absolute value is >=1,
!                dimension i will always have the same sign as xranges(3,i).


      include 'nnparams.incl'
      include 'nndbg.incl'

      real*8  xranges(3,maxnin)
      real*8  pat_in(maxnin,npat,*)
      integer npat,scans
      integer neu_in

      real*8  rvec(maxnin)
      real*8  rmin,len_step

      real*8  dum
      integer n,k,j

!     rescale minimum distances such that corresponding 
!     outer ellipsoid transforms to a unit sphere and
!     pick the largest of these minimal radii to actually use.
      rmin=0.0d0
      do k=1,neu_in
         dum=xranges(1,k)/xranges(2,k)
         if (dum.gt.rmin) rmin=dum
      enddo

      if (dbg) then
         write(*,*) 'MINIMAL EFFECTIVE RADIUS:', rmin
      endif

!     evaluate step size
      len_step=(1.0d0-rmin)/dble(npat-1)

      do n=1,scans
!        generate normalized isotropic random vector
         call normal_grv(rvec,neu_in,1)

!        adjust signs
         do j=1,neu_in
            if (dabs(xranges(3,j)).ge.1.0d0) then
               rvec(j)=sign(rvec(j),xranges(3,j))
            endif
         enddo

         do k=1,npat
            do j=1,neu_in
!              make equidistant steps along that direction
               pat_in(j,k,n) = (rmin + (k-1)*len_step)*rvec(j)
!              rescale
               pat_in(j,k,n) = pat_in(j,k,n)*xranges(2,j)
            enddo
         enddo
      enddo
      end

!--------------------------------------------------------------------------------------

      subroutine mkpars(par,spread,nlay,rtype)
      implicit none      
!     Initialize Weights and Biases exclusively.
!     
!
!     par:       one parameter vector
!     spread:    factor by which each weight or bias spreads around 0
!     nlay:      number of given layers
!     rtype:     type of random distribution: 
!                0 -- even distribution
!                1 -- angular uniform distribution,
!                     vectors normalized to sqrt(wb_end)
!     Initialization of the random numbers must have been done before
!     call vranf(par(1:1),0,seed,iout) is used for the initialization
     

      include 'nnparams.incl'
      include 'params.incl'
      include 'common.incl'
      include 'nndbg.incl'

      real*8  par(*),spread(*)
      integer rtype,nlay

      integer wb_end
      
      integer k

!     evalute length of entire weight- and bias-vector together
      wb_end=pst(1,2*(nlay-1))+pst(2,2*(nlay-1))-1

      if (rtype.eq.0) then
!        generate random numbers within [0,1)
         call vranf(par,wb_end,0,iout)
!        shift range accordingly
         do k=1,wb_end
            par(k)=(par(k)-0.5D0)*2.0D0*spread(k)
         enddo
      else if (rtype.eq.1) then
!        generate random numbers
         call nnorm_grv(par,wb_end,1)
!        shift range accordingly
         do k=1,wb_end
            par(k)=par(k)*2.0D0*spread(k)
         enddo
      else
         stop 'ERROR: INVALID TYPE OF RANDOM NUMBER GENERATION (RTYPE)'
      endif

      end

!--------------------------------------------------------------------------------------

      subroutine mknet(laystr,neupop,nlay)
      implicit none      
!     Initialize layer structure matrix exclusively.
!
!     nlay:      number of given layers
!     neupop:    Neuronal Population vector: Contains number of neurons for each layer
!     laystr:    Layer Structure Matrix

      include 'nnparams.incl'
      include 'nndbg.incl'

      integer laystr(3,*),neupop(*)
      integer nlay
      
      integer neu_tot,wei_tot
      integer n

      if (dbg.or.vbs) write(*,*) '#INITIALIZING NN....'

      neu_tot=0 !number of reserved neurons spaces in memory so far
      wei_tot=0 !number of reserved weight matrix element spaces in mem. so far

      do n=1,nlay-1
         laystr(1,n)=neupop(n) !number of neurons in layer n
         laystr(2,n)=neu_tot+1 !starting pos. for layer n
         laystr(3,n)=wei_tot+1 !starting pos. for W-matrix
                               !from layer N to N+1
         neu_tot=neu_tot+neupop(n)
         wei_tot=wei_tot+neupop(n)*neupop(n+1)
      enddo
      laystr(1,nlay)=neupop(nlay)
      laystr(2,nlay)=neu_tot+1
      laystr(3,nlay)=wei_tot+1  !technically oversteps boundary
                                !of W-vector; should point to first bias

      if (dbg.or.vbs) then
         write(*,*) '#INIT SUCCESSFUL.'
         write(*,'(A10,I6)') '#NEU_TOT =',neu_tot
         write(*,'(A10,I6)') '#WEI_TOT =',wei_tot
      endif
      end

