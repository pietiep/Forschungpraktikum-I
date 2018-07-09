************************************************************************
***                              long_io
***             reading & writing genetic's long input format
***
************************************************************************

      subroutine write_longint(f_unit,params,plen,intfmt,maxvals)
      implicit none
!     Routine writing long integer output of the form
!     x1 x2 x3 .... xN &
!            ...       &
!     
!     f_unit:      UNIT to be written on, directly passed to write
!     params:      integer vector to be written out
!     plen:        number of elements to be printed
!     maxvals:     (maximum) number of values per line
!     intfmt:      format of a single interger, e.g. '(I6)'
      
      integer f_unit
      integer params(*)
      integer plen,maxvals
      character*16 intfmt
      
      integer pcount

      integer j,k

      pcount=0 ! count parameters written so far

!     write all values that fill entire lines.
      do k=1,(plen/maxvals)
         do j=1,maxvals
            write(unit=f_unit,fmt=intfmt,advance='NO') params(pcount+j)
         enddo
         pcount=pcount+maxvals
         if (pcount.lt.plen) then
            write(unit=f_unit,fmt='(A)') ' &'
         endif
      enddo

      pcount=pcount+1

!     write remaining few
      do k=pcount,plen
         write(unit=f_unit,fmt=intfmt,advance='NO') params(k)
      enddo
      
      write(f_unit,'(A)') ''

      end

!----------------------------------------------------------------------------

      subroutine write_longreal(f_unit,params,plen,dfmt,maxvals)
      implicit none
!     Routine writing long real(*8) output of the form
!     x1 x2 x3 .... xN &
!            ...       &
!
!     f_unit:      UNIT to be written on, directly passed to write
!     params:      integer vector to be written out
!     plen:        number of elements to be printed
!     maxvals:     (maximum) number of values per line
!     dfmt:        format of a single real, e.g. '(ES23.15)'
      
      real*8 params(*)
      integer f_unit
      integer plen,maxvals
      character*16 dfmt
      
      integer pcount

      integer j,k

      pcount=0 ! count parameters written so far

!     write all values that fill entire lines.
      do k=1,(plen/maxvals)
         do j=1,maxvals
            write(unit=f_unit,fmt=dfmt,advance='NO') params(pcount+j)
         enddo
         pcount=pcount+maxvals
         if (pcount.lt.plen) then
            write(unit=f_unit,fmt='(A)') ' &'
         endif
      enddo

      pcount=pcount+1

!     write remaining few
      do k=pcount,plen
         write(unit=f_unit,fmt=dfmt,advance='NO') params(k)
      enddo
      
      write(f_unit,'(A)') ''

      end
