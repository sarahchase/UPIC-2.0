!-----------------------------------------------------------------------
! Fortran Library for reordering particles
! 3D Vector/MPI/OpenMP PIC Codes:
! VPPPORDER32LA performs first part of particle reordering into tiles,
!               creates list of particles which are leaving tile, and
!               buffers outgoing particles
! VPPPORDERF32LA performs first part of particle reordering into tiles,
!               buffers outgoing particles
! VPPPORDER2LB performs second part of particle reordering into tiles,
!              copies buffers into particle array
! written by Viktor K. Decyk, UCLA
! copyright 2016, regents of the university of california
! update: october 17, 2018
!-----------------------------------------------------------------------
      subroutine VPPPORDER32LA(ppart,ppbuff,sbufl,sbufr,kpic,ncl,ihole, &
     &ncll,nclr,noff,nyzp,idimp,nppmx,nx,ny,nz,mx,my,mz,mx1,myp1,mzp1,  &
     &mxzyp1,npbmx,ntmax,nbmax,idds,irc2)
! this subroutine performs first part of a particle sort by x,y,z grid
! in tiles of mx, my, mz
! linear interpolation, with periodic boundary conditions
! for distributed data, with 2d domain decomposition in y/z.
! tiles are assumed to be arranged in 3D linear memory
! this part of the algorithm has 3 steps.  first, one finds particles
! leaving tile and stores their number in each directon, location, and
! destination in ncl and ihole.  then, a prefix scan of ncl is performed
! and departing particles are buffered in ppbuff in direction order.
! finally, we buffer particles leaving the processor in y/z direction in
! sbufl and sbufr, and store particle number offsets in ncll and nclr.
! input: all except ppbuff, sbufl, sbufr, ncl, ihole, ncll, nclr, irc2
! output: ppart, ppbuff, sbufl, sbufr, ncl, ihole, ncll, nclr, irc2
! ppart(1,n,m) = position x of particle n in tile m
! ppart(2,n,m) = position y of particle n in tile m
! ppart(3,n,m) = position z of particle n in tile m
! ppbuff(i,n,l) = i co-ordinate of particle n in tile l
! sbufl = buffer for particles being sent to lower/back processor
! sbufr = buffer for particles being sent to upper/forward processor
! kpic(l) = number of particles in tile l
! ncl(i,l) = number of particles going to destination i, tile l
! ihole(1,:,l) = location of hole in array left by departing particle
! ihole(2,:,l) = direction destination of particle leaving hole
! all for tile l
! ihole(1,1,l) = ih, number of holes left (error, if negative)
! ncll = number offset being sent to lower/back processor
! nclr = number offset being sent to upper/forward processor
! noff(1) = lowermost global gridpoint in y in particle partition
! noff(2) = backmost global gridpoint in z in particle partition
! nyzp(1:2) = number of primary (complete) gridpoints in y/z
! idimp = size of phase space = 6
! nppmx = maximum number of particles in tile
! nx/ny/nz = system length in x/y/z direction
! mx/my/mz = number of grids in sorting cell in x/y/z
! mx1 = (system length in x direction - 1)/mx + 1
! myp1 = (partition length in y direction - 1)/my + 1
! mzp1 = (partition length in z direction - 1)/mz + 1
! mxzyp1 = mx1*max(myp1,mzp1)
! npbmx = size of buffer array ppbuff
! ntmax = size of hole array for particles leaving tiles
! nbmax =  size of buffers for passing particles between processors
! idds = dimensionality of domain decomposition
! irc2 = error codes, returned only if error occurs, when irc2(1) > 0
! when (irc2(1).eq.1), ihole overflow, irc2(2) = new ntmax required
! when (irc2(1).eq.2), ppbuff overflow, irc2(2) = new npbmx required
! when (irc2(1).eq.3), sbufr/sbufl overflow, irc2(2)=new nbmax required
      implicit none
      integer idimp, nppmx, nx, ny, nz, mx, my, mz, mx1, myp1, mzp1
      integer mxzyp1, npbmx, ntmax, nbmax, idds
      real ppart, ppbuff, sbufl, sbufr
      integer kpic, noff, nyzp, ncl, ihole, ncll, nclr, irc2
      dimension ppart(idimp,nppmx,mx1*myp1*mzp1)
      dimension ppbuff(idimp,npbmx,mx1*myp1*mzp1)
      dimension sbufl(idimp,nbmax,2), sbufr(idimp,nbmax,2)
      dimension kpic(mx1*myp1*mzp1), noff(idds), nyzp(idds)
      dimension ncl(26,mx1*myp1*mzp1), ihole(2,ntmax+1,mx1*myp1*mzp1)
      dimension ncll(3,mxzyp1,3,2), nclr(3,mxzyp1,3,2)
      dimension irc2(2)
! local data
      integer npblk, lvect
      parameter(npblk=32,lvect=4)
      integer mxyp1, mxzp1, mxyzp1, noffp, moffp, loffp, nppp, ipp, joff
      integer i, j, k, l, m, in, ii, ih, nh, ist, nn, mm, ll, nps
      integer ip, j1, j2, k1, k2, kk, nr, nl, lb, kxs
      real anx, any, anz, edgelx, edgely, edgelz, edgerx, edgery, edgerz
      real dx, dy, dz
      integer sncl, ks
!dir$ attributes align : 64 :: sncl, ks
      dimension sncl(26), ks(26)
! scratch array
      integer n
!dir$ attributes align : 64 :: n
      dimension n(npblk,lvect)
      mxyp1 = mx1*myp1
      mxyzp1 = mxyp1*mzp1
      anx = real(nx)
      any = real(ny)
      anz = real(nz)
! find and count particles leaving tiles and determine destination
! update ppart, ihole, ncl
! loop over tiles
!$OMP PARALLEL DO                                                       &
!$OMP& PRIVATE(j,k,l,m,noffp,moffp,loffp,nppp,ipp,joff,nps,nn,mm,ll,ih, &
!$OMP& nh,ist,dx,dy,dz,edgelx,edgely,edgelz,edgerx,edgery,edgerz,n)
      do 60 l = 1, mxyzp1
      loffp = (l - 1)/mxyp1
      k = l - mxyp1*loffp
      loffp = mz*loffp
      noffp = (k - 1)/mx1
      moffp = my*noffp
      noffp = mx*(k - mx1*noffp - 1)
      nppp = kpic(l)
      nn = min(mx,nx-noffp)
      mm = min(my,nyzp(1)-moffp)
      ll = min(mz,nyzp(2)-loffp)
      edgelx = noffp
      edgerx = noffp + nn
      edgely = noff(1) + moffp
      edgery = noff(1) + moffp + mm
      edgelz = noff(2) + loffp
      edgerz = noff(2) + loffp + ll
      ih = 0
      nh = 0
! clear counters
      do 10 j = 1, 26
      ncl(j,l) = 0
   10 continue
! loop over particles in tlie
      ipp = nppp/npblk
! outer loop over number of full blocks
      do 40 m = 1, ipp
      joff = npblk*(m - 1)
! inner loop over particles in block
! !dir$ vector aligned
      do 20 j = 1, npblk
      dx = ppart(1,j+joff,l)
      dy = ppart(2,j+joff,l)
      dz = ppart(3,j+joff,l)
! find particles going out of bounds
      ist = 0
! count how many particles are going in each direction in ncl
! save their address and destination in ihole
! check for roundoff error
! ist = direction particle is going
      if (dx.ge.edgerx) then
         ist = 2
      else if (dx.lt.edgelx) then
         ist = 1
         if (dx.lt.0.0) then
            dx = dx + anx
            if (dx.ge.anx) then
               ppart(1,j+joff,l) = 0.0
               ist = 0
            endif
         endif
      endif
      if (dy.ge.edgery) then
         ist = ist + 6
      else if (dy.lt.edgely) then
         ist = ist + 3
         if (dy.lt.0.0) then
            dy = dy + any
            if (dy.ge.any) then
               ppart(2,j+joff,l) = 0.0
               ist = ist - 3
            endif
         endif
      endif
      if (dz.ge.edgerz) then
         ist = ist + 18
      else if (dz.lt.edgelz) then
         ist = ist + 9
         if (dz.lt.0.0) then
            dz = dz + anz
            if (dz.ge.anz) then
               ppart(3,j+joff,l) = 0.0
               ist = ist - 9
            endif
         endif
      endif
      n(j,1) = ist
   20 continue
! store outgoing particle address and destination
      do 30 j = 1, npblk
      ist = n(j,1)
      if (ist.gt.0) then
         ncl(ist,l) = ncl(ist,l) + 1
         ih = ih + 1
         if (ih.le.ntmax) then
            ihole(1,ih+1,l) = j + joff
            ihole(2,ih+1,l) = ist
         else
            nh = 1
         endif
      endif
   30 continue
   40 continue
      nps = npblk*ipp + 1
! loop over remaining particles
      do 50 j = nps, nppp
      dx = ppart(1,j,l)
      dy = ppart(2,j,l)
      dz = ppart(3,j,l)
! find particles going out of bounds
      ist = 0
! count how many particles are going in each direction in ncl
! save their address and destination in ihole
! check for roundoff error
! ist = direction particle is going
      if (dx.ge.edgerx) then
         ist = 2
      else if (dx.lt.edgelx) then
         ist = 1
         if (dx.lt.0.0) then
            dx = dx + anx
            if (dx.ge.anx) then
               ppart(1,j,l) = 0.0
               ist = 0
            endif
         endif
      endif
      if (dy.ge.edgery) then
         ist = ist + 6
      else if (dy.lt.edgely) then
         ist = ist + 3
         if (dy.lt.0.0) then
            dy = dy + any
            if (dy.ge.any) then
               ppart(2,j,l) = 0.0
               ist = ist - 3
            endif
         endif
      endif
      if (dz.ge.edgerz) then
         ist = ist + 18
      else if (dz.lt.edgelz) then
         ist = ist + 9
         if (dz.lt.0.0) then
            dz = dz + anz
            if (dz.ge.anz) then
               ppart(3,j,l) = 0.0
               ist = ist - 9
            endif
         endif
      endif
! increment counters
      if (ist.gt.0) then
         ncl(ist,l) = ncl(ist,l) + 1
         ih = ih + 1
         if (ih.le.ntmax) then
            ihole(1,ih+1,l) = j
            ihole(2,ih+1,l) = ist
         else
            nh = 1
         endif
      endif
   50 continue
! set error and end of file flag
      if (nh.gt.0) irc2(1) = 1
      ihole(1,1,l) = ih
   60 continue
!$OMP END PARALLEL DO
! ihole overflow
      if (irc2(1).gt.0) then
         ih = 0
         do 70 l = 1, mxyzp1
         ih = max(ih,ihole(1,1,l))
   70    continue
         irc2(2) = ih
         return
      endif
!
! buffer particles that are leaving tile: update ppbuff, ncl
! loop over tiles
!$OMP PARALLEL DO                                                       &
!$OMP& PRIVATE(i,j,l,m,kxs,lb,ist,nh,ip,ipp,nps,joff,j1,ii,sncl,ks,n)
      do 200 l = 1, mxyzp1
! find address offset for ordered ppbuff array
! !dir$ vector aligned
      do 80 j = 1, 26
      sncl(j) = ncl(j,l)
      ks(j) = j - 1
   80 continue
      kxs = 1
   90 if (kxs.lt.26) then
!dir$ ivdep
         do 100 j = 1, 13
         lb = kxs*ks(j)
         if ((j+lb+kxs).le.26) then
            sncl(j+lb+kxs) = sncl(j+lb+kxs) + sncl(2*lb+kxs)
         endif
         ks(j) = ks(j)/2
  100    continue
         kxs = kxs + kxs
         go to 90
      endif
      do 110 j = 1, 26
      sncl(j) = sncl(j) - ncl(j,l)
  110 continue
      nh = ihole(1,1,l)
      ip = 0
! buffer particles that are leaving tile, in direction order
! loop over particles leaving tile
      ipp = nh/npblk
! outer loop over number of full blocks
      do 160 m = 1, ipp
      joff = npblk*(m - 1) + 1
! inner loop over particles in block
      do 120 j = 1, npblk
      n(j,1) = ihole(1,j+joff,l)
      n(j,2) = ihole(2,j+joff,l)
  120 continue
! calculate offsets
      do 130 j = 1, npblk
      ist = n(j,2)
      ii = sncl(ist) + 1
      n(j,2) = ii
      sncl(ist) = ii
  130 continue
! buffer particles that are leaving tile, in direction order
      do 150 j = 1, npblk
      j1 = n(j,1)
      ii = n(j,2)
      if (ii.le.npbmx) then
         do 140 i = 1, idimp
         ppbuff(i,ii,l) = ppart(i,j1,l)
  140    continue
      else
         ip = 1
      endif
  150 continue
  160 continue
      nps = npblk*ipp + 1
! loop over remaining particles
      do 180 j = nps, nh
! buffer particles that are leaving tile, in direction order
      j1 = ihole(1,j+1,l)
      ist = ihole(2,j+1,l)
      ii = sncl(ist) + 1
      if (ii.le.npbmx) then
         do 170 i = 1, idimp
         ppbuff(i,ii,l) = ppart(i,j1,l)
  170    continue
      else
         ip = 1
      endif
      sncl(ist) = ii
  180 continue
      do 190 j = 1, 26
      ncl(j,l) = sncl(j)
  190 continue
! set error
      if (ip.gt.0) irc2(1) = 2
  200 continue
!$OMP END PARALLEL DO
! ppbuff overflow
      if (irc2(1).gt.0) then
         ii = 0
         do 210 l = 1, mxyzp1
         ii = max(ii,ncl(26,l))
  210    continue
         irc2(2) = ii
         return
      endif
!
! buffer particles and their number leaving the node up or down:
! update sbufl(:,1), sbufr(:,1), ncll(:,1), nclr(:,1)
      mxzp1 = mx1*mzp1
      kk = mx1*(myp1 - 1)
!$OMP PARALLEL DO PRIVATE(k,l,ll)
      do 220 ll = 1, mxzp1
      l = (ll - 1)/mx1
      k = ll + (mxyp1 - mx1)*l
! going straight up and down
      ncll(1,ll,1,1) = ncl(5,k) - ncl(2,k)
      nclr(1,ll,1,1) = ncl(8,k+kk) - ncl(5,k+kk)
! particles going up and back
      ncll(1,ll,2,1) = ncl(14,k) - ncl(11,k)
! particles going down and back
      nclr(1,ll,2,1) = ncl(17,k+kk) - ncl(14,k+kk)
! particles going up and forward
      ncll(1,ll,3,1) = ncl(23,k) - ncl(20,k)
! particles going down and forward
      nclr(1,ll,3,1) = ncl(26,k+kk) - ncl(23,k+kk)
  220 continue
!$OMP END PARALLEL DO
! perform prefix scan
      kk = 1
  230 if (kk.ge.mxzp1) go to 250
!$OMP PARALLEL DO PRIVATE(ll,ii,nn,mm)
      do 240 ll = 1, mxzp1
      ii = (ll - 1)/kk
      nn = kk*ii
      mm = 2*nn + kk - 1
      nn = nn + ll + kk
      if (nn.le.mxzp1) then
         ncll(1,nn,1,1) = ncll(1,nn,1,1) + ncll(1,mm+1,1,1)
         nclr(1,nn,1,1) = nclr(1,nn,1,1) + nclr(1,mm+1,1,1)
         ncll(1,nn,2,1) = ncll(1,nn,2,1) + ncll(1,mm+1,2,1)
         nclr(1,nn,2,1) = nclr(1,nn,2,1) + nclr(1,mm+1,2,1)
         ncll(1,nn,3,1) = ncll(1,nn,3,1) + ncll(1,mm+1,3,1)
         nclr(1,nn,3,1) = nclr(1,nn,3,1) + nclr(1,mm+1,3,1)
      endif
  240 continue
!$OMP END PARALLEL DO
      kk = kk + kk
      go to 230
  250 kk = mx1*(myp1 - 1)
      j1 = ncll(1,mxzp1,1,1)
      k1 = j1 + ncll(1,mxzp1,2,1)
      j2 = nclr(1,mxzp1,1,1)
      k2 = j2 + nclr(1,mxzp1,2,1)
!$OMP PARALLEL DO PRIVATE(i,j,k,l,ll,ii,nn,mm)
      do 440 ll = 1, mxzp1
      l = (ll - 1)/mx1
      k = ll + (mxyp1 - mx1)*l
! particles going straight up
      ii = ncl(5,k) - ncl(2,k)
      nn = ncll(1,ll,1,1) - ii
      do 270 j = 1, min(ii,nbmax-nn)
      do 260 i = 1, idimp
      sbufl(i,j+nn,1) = ppbuff(i,j+ncl(2,k),k)
  260 continue
  270 continue
      do 280 i = 1, 3
      ncll(i,ll,1,1) = ncl(i+2,k) - ncl(2,k) + nn
  280 continue
! particles going straight down
      ii = ncl(8,k+kk) - ncl(5,k+kk)
      mm = nclr(1,ll,1,1) - ii
      do 300 j = 1, min(ii,nbmax-mm)
      do 290 i = 1, idimp
      sbufr(i,j+mm,1) = ppbuff(i,j+ncl(5,k+kk),k+kk)
  290 continue
  300 continue
      do 310 i = 1, 3
      nclr(i,ll,1,1) = ncl(i+5,k+kk) - ncl(5,k+kk) + mm
  310 continue
! particles going up and back
      ii = ncl(14,k) - ncl(11,k)
      nn = j1 + ncll(1,ll,2,1) - ii
      do 330 j = 1, min(ii,nbmax-nn)
      do 320 i = 1, idimp
      sbufl(i,j+nn,1) = ppbuff(i,j+ncl(11,k),k)
  320 continue
  330 continue
      do 340 i = 1, 3
      ncll(i,ll,2,1) = ncl(i+11,k) - ncl(11,k) + nn
  340 continue
! particles going down and back
      ii = ncl(17,k+kk) - ncl(14,k+kk)
      mm = j2 + nclr(1,ll,2,1) - ii
      do 360 j = 1, min(ii,nbmax-mm)
      do 350 i = 1, idimp
      sbufr(i,j+mm,1) = ppbuff(i,j+ncl(14,k+kk),k+kk)
  350 continue
  360 continue
      do 370 i = 1, 3
      nclr(i,ll,2,1) = ncl(i+14,k+kk) - ncl(14,k+kk) + mm
  370 continue
! particles going up and forward
      ii = ncl(23,k) - ncl(20,k)
      nn = k1 + ncll(1,ll,3,1) - ii
      do 390 j = 1, min(ii,nbmax-nn)
      do 380 i = 1, idimp
      sbufl(i,j+nn,1) = ppbuff(i,j+ncl(20,k),k)
  380 continue
  390 continue
      do 400 i = 1, 3
      ncll(i,ll,3,1) = ncl(i+20,k) - ncl(20,k) + nn
  400 continue
! particles going down and forward, to different node
      ii = ncl(26,k+kk) - ncl(23,k+kk)
      mm = k2 + nclr(1,ll,3,1) - ii
      do 420 j = 1, min(ii,nbmax-mm)
      do 410 i = 1, idimp
      sbufr(i,j+mm,1) = ppbuff(i,j+ncl(23,k+kk),k+kk)
  410 continue
  420 continue
      do 430 i = 1, 3
      nclr(i,ll,3,1) = ncl(i+23,k+kk) - ncl(23,k+kk) + mm
  430 continue
  440 continue
!$OMP END PARALLEL DO
!
! buffer particles and their number leaving the node back or forward:
! update sbufl(:,2), sbufr(:,2), ncll(:,2), nclr(:,2)
      kk = mxyp1*(mzp1 - 1)
!$OMP PARALLEL DO PRIVATE(k,ll)
      do 450 ll = 1, mxyp1
      k = ll
! going straight back or forward
      ncll(1,ll,1,2) = ncl(11,k) - ncl(8,k)
      nclr(1,ll,1,2) = ncl(20,k+kk) - ncl(17,k+kk)
! particles going back and up
      ncll(1,ll,2,2) = ncl(14,k) - ncl(11,k)
! particles going forward and up
      nclr(1,ll,2,2) = ncl(23,k+kk) - ncl(20,k+kk)
! particles going back and down
      ncll(1,ll,3,2) = ncl(17,k) - ncl(14,k)
! particles going forward and down
      nclr(1,ll,3,2) = ncl(26,k+kk) - ncl(23,k+kk)
  450 continue
!$OMP END PARALLEL DO
! perform prefix scan
      kk = 1
  460 if (kk.ge.mxyp1) go to 480
!$OMP PARALLEL DO PRIVATE(ll,ii,nn,mm)
      do 470 ll = 1, mxyp1
      ii = (ll - 1)/kk
      nn = kk*ii
      mm = 2*nn + kk - 1
      nn = nn + ll + kk
      if (nn.le.mxyp1) then
         ncll(1,nn,1,2) = ncll(1,nn,1,2) + ncll(1,mm+1,1,2)
         nclr(1,nn,1,2) = nclr(1,nn,1,2) + nclr(1,mm+1,1,2)
         ncll(1,nn,2,2) = ncll(1,nn,2,2) + ncll(1,mm+1,2,2)
         nclr(1,nn,2,2) = nclr(1,nn,2,2) + nclr(1,mm+1,2,2)
         ncll(1,nn,3,2) = ncll(1,nn,3,2) + ncll(1,mm+1,3,2)
         nclr(1,nn,3,2) = nclr(1,nn,3,2) + nclr(1,mm+1,3,2)
      endif
  470 continue
!$OMP END PARALLEL DO
      kk = kk + kk
      go to 460
  480 kk = mxyp1*(mzp1 - 1)
      j1 = ncll(1,mxyp1,1,2)
      k1 = j1 + ncll(1,mxyp1,2,2)
      j2 = nclr(1,mxyp1,1,2)
      k2 = j2 + nclr(1,mxyp1,2,2)
!$OMP PARALLEL DO PRIVATE(i,j,k,ll,ii,nn,mm)
      do 670 ll = 1, mxyp1
      k = ll
! particles going straight up
      ii = ncl(11,k) - ncl(8,k)
      nn = ncll(1,ll,1,2) - ii
      do 500 j = 1, min(ii,nbmax-nn)
      do 490 i = 1, idimp
      sbufl(i,j+nn,2) = ppbuff(i,j+ncl(8,k),k)
  490 continue
  500 continue
      do 510 i = 1, 3
      ncll(i,ll,1,2) = ncl(i+8,k) - ncl(8,k) + nn
  510 continue
! particles going straight down
      ii = ncl(20,k+kk) - ncl(17,k+kk)
      mm = nclr(1,ll,1,2) - ii
      do 530 j = 1, min(ii,nbmax-mm)
      do 520 i = 1, idimp
      sbufr(i,j+mm,2) = ppbuff(i,j+ncl(17,k+kk),k+kk)
  520 continue
  530 continue
      do 540 i = 1, 3
      nclr(i,ll,1,2) = ncl(i+17,k+kk) - ncl(17,k+kk) + mm
  540 continue
! particles going up and back
      ii = ncl(14,k) - ncl(11,k)
      nn = j1 + ncll(1,ll,2,2) - ii
      do 560 j = 1, min(ii,nbmax-nn)
      do 550 i = 1, idimp
      sbufl(i,j+nn,2) = ppbuff(i,j+ncl(11,k),k)
  550 continue
  560 continue
      do 570 i = 1, 3
      ncll(i,ll,2,2) = ncl(i+11,k) - ncl(11,k) + nn
  570 continue
! particles going down and back
      ii = ncl(23,k+kk) - ncl(20,k+kk)
      mm = j2 + nclr(1,ll,2,2) - ii
      do 590 j = 1, min(ii,nbmax-mm)
      do 580 i = 1, idimp
      sbufr(i,j+mm,2) = ppbuff(i,j+ncl(20,k+kk),k+kk)
  580 continue
  590 continue
      do 600 i = 1, 3
      nclr(i,ll,2,2) = ncl(i+20,k+kk) - ncl(20,k+kk) + mm
  600 continue
! particles going up and forward
      ii = ncl(17,k) - ncl(14,k)
      nn = k1 + ncll(1,ll,3,2) - ii
      do 620 j = 1, min(ii,nbmax-nn)
      do 610 i = 1, idimp
      sbufl(i,j+nn,2) = ppbuff(i,j+ncl(14,k),k)
  610 continue
  620 continue
      do 630 i = 1, 3
      ncll(i,ll,3,2) = ncl(i+14,k) - ncl(14,k) + nn
  630 continue
! particles going down and forward, to different node
      ii = ncl(26,k+kk) - ncl(23,k+kk)
      mm = k2 + nclr(1,ll,3,2) - ii
      do 650 j = 1, min(ii,nbmax-mm)
      do 640 i = 1, idimp
      sbufr(i,j+mm,2) = ppbuff(i,j+ncl(23,k+kk),k+kk)
  640 continue
  650 continue
      do 660 i = 1, 3
      nclr(i,ll,3,2) = ncl(i+23,k+kk) - ncl(23,k+kk) + mm
  660 continue
  670 continue
!$OMP END PARALLEL DO
! sbufl or sbufr overflow
      kk = max(ncll(3,mxzp1,3,1),nclr(3,mxzp1,3,1))
      ll = max(ncll(3,mxyp1,3,2),nclr(3,mxyp1,3,2))
      ii = max(kk,ll)
! corners overflow
      nn = nclr(3,mx1,2,1) - nclr(3,mxzp1,1,1)
      mm = ncll(3,mx1,2,1) - ncll(3,mxzp1,1,1)
      in = mx1*(mzp1 - 1)
      if (in.gt.0) then
         nr = nclr(3,in,3,1)
         nl = ncll(3,in,3,1)
      else
         nr = nclr(3,mxzp1,2,1)
         nl = ncll(3,mxzp1,2,1)
      endif
      kk = nclr(3,in+mx1,3,1) - nr
      ll = ncll(3,in+mx1,3,1) - nl
! total overflow: result valid only for one processor case
      ii = ii + max(nn+kk,mm+ll)
      if (ii.gt.nbmax) then
         irc2(1) = 3
         irc2(2) = ii
      endif
      return
      end
!-----------------------------------------------------------------------
      subroutine VPPPORDERF32LA(ppart,ppbuff,sbufl,sbufr,ncl,ihole,ncll,&
     &nclr,idimp,nppmx,mx1,myp1,mzp1,mxzyp1,npbmx,ntmax,nbmax,irc2)
! this subroutine performs first part of a particle sort by x,y,z grid
! in tiles of mx, my, mz
! linear interpolation, with periodic boundary conditions
! for distributed data, with 2d domain decomposition in y/z.
! tiles are assumed to be arranged in 3D linear memory
! this part of the algorithm has 2 steps.  first, a prefix scan of ncl
! is performed and departing particles are buffered in ppbuff in
! direction order. then, we buffer particles leaving the processor in
! sbufl and sbufr, and store particle number offsets in ncll and nclr.
! it assumes that the number, location, and destination of particles 
! leaving a tile have been previously stored in ncl and ihole by the
! PPGPPUSHF32L subroutine.
! input: all except ppbuff, sbufl, sbufr, ncll, nclr, irc2
! output: ppart, ppbuff, sbufl, sbufr, ncl, ncll, nclr, irc2
! ppart(1,n,m) = position x of particle n in tile m
! ppart(2,n,m) = position y of particle n in tile m
! ppart(3,n,m) = position z of particle n in tile m
! ppbuff(i,n,l) = i co-ordinate of particle n in tile l
! sbufl = buffer for particles being sent to lower/back processor
! sbufr = buffer for particles being sent to upper/forward processor
! ncl(i,l) = number of particles going to destination i, tile l
! ihole(1,:,l) = location of hole in array left by departing particle
! ihole(2,:,l) = direction destination of particle leaving hole
! all for tile l
! ihole(1,1,l) = ih, number of holes left (error, if negative)
! ncll = number offset being sent to lower/back processor
! nclr = number offset being sent to upper/forward processor
! noff(1) = lowermost global gridpoint in y in particle partition
! noff(2) = backmost global gridpoint in z in particle partition
! nyzp(1:2) = number of primary (complete) gridpoints in y/z
! idimp = size of phase space = 6
! nppmx = maximum number of particles in tile
! mx1 = (system length in x direction - 1)/mx + 1
! myp1 = (partition length in y direction - 1)/my + 1
! mzp1 = (partition length in z direction - 1)/mz + 1
! mxzyp1 = mx1*max(myp1,mzp1)
! npbmx = size of buffer array ppbuff
! ntmax = size of hole array for particles leaving tiles
! nbmax =  size of buffers for passing particles between processors
! when (irc2(1).eq.2), ppbuff overflow, irc2(2) = new npbmx required
! when (irc2(1).eq.3), sbufr/sbufl overflow, irc2(2)=new nbmax required
      implicit none
      integer idimp, nppmx, mx1, myp1, mzp1, mxzyp1, npbmx, ntmax, nbmax
      real ppart, ppbuff, sbufl, sbufr
      integer ncl, ihole, ncll, nclr, irc2
      dimension ppart(idimp,nppmx,mx1*myp1*mzp1)
      dimension ppbuff(idimp,npbmx,mx1*myp1*mzp1)
      dimension sbufl(idimp,nbmax,2), sbufr(idimp,nbmax,2)
      dimension ncl(26,mx1*myp1*mzp1), ihole(2,ntmax+1,mx1*myp1*mzp1)
      dimension ncll(3,mxzyp1,3,2), nclr(3,mxzyp1,3,2)
      dimension irc2(2)
! local data
      integer npblk, lvect
      parameter(npblk=32,lvect=4)
      integer mxyp1, mxzp1, mxyzp1
      integer i, j, k, l, n, m, in, ii, nh, ist, nn, mm, ll, ipp, joff
      integer ip, j1, j2, k1, k2, kk, nr, nl, nps, lb, kxs
      integer sncl, ks
!dir$ attributes align : 64 :: sncl, ks
      dimension sncl(26), ks(26)
!dir$ attributes align : 64 :: n
      dimension n(npblk,lvect)
      mxyp1 = mx1*myp1
      mxyzp1 = mxyp1*mzp1
! buffer particles that are leaving tile: update ppbuff, ncl
! loop over tiles
!$OMP PARALLEL DO                                                       &
!$OMP& PRIVATE(i,j,l,m,kxs,lb,ist,nh,ip,ipp,nps,joff,j1,ii,sncl,ks,n)
      do 130 l = 1, mxyzp1
! find address offset for ordered ppbuff array
! !dir$ vector aligned
      do 10 j = 1, 26
      sncl(j) = ncl(j,l)
      ks(j) = j - 1
   10 continue
      kxs = 1
   20 if (kxs.lt.26) then
!dir$ ivdep
         do 30 j = 1, 13
         lb = kxs*ks(j)
         if ((j+lb+kxs).le.26) then
            sncl(j+lb+kxs) = sncl(j+lb+kxs) + sncl(2*lb+kxs)
         endif
         ks(j) = ks(j)/2
   30    continue
         kxs = kxs + kxs
         go to 20
      endif
      do 40 j = 1, 26
      sncl(j) = sncl(j) - ncl(j,l)
   40 continue
      nh = ihole(1,1,l)
      ip = 0
! buffer particles that are leaving tile, in direction order
! loop over particles leaving tile
      ipp = nh/npblk
! outer loop over number of full blocks
      do 90 m = 1, ipp
      joff = npblk*(m - 1) + 1
! inner loop over particles in block
      do 50 j = 1, npblk
      n(j,1) = ihole(1,j+joff,l)
      n(j,2) = ihole(2,j+joff,l)
   50 continue
! calculate offsets
      do 60 j = 1, npblk
      ist = n(j,2)
      ii = sncl(ist) + 1
      n(j,2) = ii
      sncl(ist) = ii
   60 continue
! buffer particles that are leaving tile, in direction order
      do 80 j = 1, npblk
      j1 = n(j,1)
      ii = n(j,2)
      if (ii.le.npbmx) then
         do 70 i = 1, idimp
         ppbuff(i,ii,l) = ppart(i,j1,l)
   70    continue
      else
         ip = 1
      endif
   80 continue
   90 continue
      nps = npblk*ipp + 1
! loop over remaining particles
      do 110 j = nps, nh
! buffer particles that are leaving tile, in direction order
      j1 = ihole(1,j+1,l)
      ist = ihole(2,j+1,l)
      ii = sncl(ist) + 1
      if (ii.le.npbmx) then
         do 100 i = 1, idimp
         ppbuff(i,ii,l) = ppart(i,j1,l)
  100    continue
      else
         ip = 1
      endif
      sncl(ist) = ii
  110 continue
      do 120 j = 1, 26
      ncl(j,l) = sncl(j)
  120 continue
! set error
      if (ip.gt.0) irc2(1) = 2
  130 continue
!$OMP END PARALLEL DO
! ppbuff overflow
      if (irc2(1).gt.0) then
         ii = 0
         do 140 l = 1, mxyzp1
         ii = max(ii,ncl(26,l))
  140    continue
         irc2(2) = ii
         return
      endif
!
! buffer particles and their number leaving the node up or down:
! update sbufl(:,1), sbufr(:,1), ncll(:,1), nclr(:,1)
      mxzp1 = mx1*mzp1
      kk = mx1*(myp1 - 1)
!$OMP PARALLEL DO PRIVATE(k,l,ll)
      do 150 ll = 1, mxzp1
      l = (ll - 1)/mx1
      k = ll + (mxyp1 - mx1)*l
! going straight up and down
      ncll(1,ll,1,1) = ncl(5,k) - ncl(2,k)
      nclr(1,ll,1,1) = ncl(8,k+kk) - ncl(5,k+kk)
! particles going up and back
      ncll(1,ll,2,1) = ncl(14,k) - ncl(11,k)
! particles going down and back
      nclr(1,ll,2,1) = ncl(17,k+kk) - ncl(14,k+kk)
! particles going up and forward
      ncll(1,ll,3,1) = ncl(23,k) - ncl(20,k)
! particles going down and forward
      nclr(1,ll,3,1) = ncl(26,k+kk) - ncl(23,k+kk)
  150 continue
!$OMP END PARALLEL DO
! perform prefix scan
      kk = 1
  160 if (kk.ge.mxzp1) go to 180
!$OMP PARALLEL DO PRIVATE(ll,ii,nn,mm)
      do 170 ll = 1, mxzp1
      ii = (ll - 1)/kk
      nn = kk*ii
      mm = 2*nn + kk - 1
      nn = nn + ll + kk
      if (nn.le.mxzp1) then
         ncll(1,nn,1,1) = ncll(1,nn,1,1) + ncll(1,mm+1,1,1)
         nclr(1,nn,1,1) = nclr(1,nn,1,1) + nclr(1,mm+1,1,1)
         ncll(1,nn,2,1) = ncll(1,nn,2,1) + ncll(1,mm+1,2,1)
         nclr(1,nn,2,1) = nclr(1,nn,2,1) + nclr(1,mm+1,2,1)
         ncll(1,nn,3,1) = ncll(1,nn,3,1) + ncll(1,mm+1,3,1)
         nclr(1,nn,3,1) = nclr(1,nn,3,1) + nclr(1,mm+1,3,1)
      endif
  170 continue
!$OMP END PARALLEL DO
      kk = kk + kk
      go to 160
  180 kk = mx1*(myp1 - 1)
      j1 = ncll(1,mxzp1,1,1)
      k1 = j1 + ncll(1,mxzp1,2,1)
      j2 = nclr(1,mxzp1,1,1)
      k2 = j2 + nclr(1,mxzp1,2,1)
!$OMP PARALLEL DO PRIVATE(i,j,k,l,ll,ii,nn,mm)
      do 370 ll = 1, mxzp1
      l = (ll - 1)/mx1
      k = ll + (mxyp1 - mx1)*l
! particles going straight up
      ii = ncl(5,k) - ncl(2,k)
      nn = ncll(1,ll,1,1) - ii
      do 200 j = 1, min(ii,nbmax-nn)
      do 190 i = 1, idimp
      sbufl(i,j+nn,1) = ppbuff(i,j+ncl(2,k),k)
  190 continue
  200 continue
      do 210 i = 1, 3
      ncll(i,ll,1,1) = ncl(i+2,k) - ncl(2,k) + nn
  210 continue
! particles going straight down
      ii = ncl(8,k+kk) - ncl(5,k+kk)
      mm = nclr(1,ll,1,1) - ii
      do 230 j = 1, min(ii,nbmax-mm)
      do 220 i = 1, idimp
      sbufr(i,j+mm,1) = ppbuff(i,j+ncl(5,k+kk),k+kk)
  220 continue
  230 continue
      do 240 i = 1, 3
      nclr(i,ll,1,1) = ncl(i+5,k+kk) - ncl(5,k+kk) + mm
  240 continue
! particles going up and back
      ii = ncl(14,k) - ncl(11,k)
      nn = j1 + ncll(1,ll,2,1) - ii
      do 260 j = 1, min(ii,nbmax-nn)
      do 250 i = 1, idimp
      sbufl(i,j+nn,1) = ppbuff(i,j+ncl(11,k),k)
  250 continue
  260 continue
      do 270 i = 1, 3
      ncll(i,ll,2,1) = ncl(i+11,k) - ncl(11,k) + nn
  270 continue
! particles going down and back
      ii = ncl(17,k+kk) - ncl(14,k+kk)
      mm = j2 + nclr(1,ll,2,1) - ii
      do 290 j = 1, min(ii,nbmax-mm)
      do 280 i = 1, idimp
      sbufr(i,j+mm,1) = ppbuff(i,j+ncl(14,k+kk),k+kk)
  280 continue
  290 continue
      do 300 i = 1, 3
      nclr(i,ll,2,1) = ncl(i+14,k+kk) - ncl(14,k+kk) + mm
  300 continue
! particles going up and forward
      ii = ncl(23,k) - ncl(20,k)
      nn = k1 + ncll(1,ll,3,1) - ii
      do 320 j = 1, min(ii,nbmax-nn)
      do 310 i = 1, idimp
      sbufl(i,j+nn,1) = ppbuff(i,j+ncl(20,k),k)
  310 continue
  320 continue
      do 330 i = 1, 3
      ncll(i,ll,3,1) = ncl(i+20,k) - ncl(20,k) + nn
  330 continue
! particles going down and forward, to different node
      ii = ncl(26,k+kk) - ncl(23,k+kk)
      mm = k2 + nclr(1,ll,3,1) - ii
      do 350 j = 1, min(ii,nbmax-mm)
      do 340 i = 1, idimp
      sbufr(i,j+mm,1) = ppbuff(i,j+ncl(23,k+kk),k+kk)
  340 continue
  350 continue
      do 360 i = 1, 3
      nclr(i,ll,3,1) = ncl(i+23,k+kk) - ncl(23,k+kk) + mm
  360 continue
  370 continue
!$OMP END PARALLEL DO
!
! buffer particles and their number leaving the node back or forward:
! update sbufl(:,2), sbufr(:,2), ncll(:,2), nclr(:,2)
      kk = mxyp1*(mzp1 - 1)
!$OMP PARALLEL DO PRIVATE(k,ll)
      do 380 ll = 1, mxyp1
      k = ll
! going straight back or forward
      ncll(1,ll,1,2) = ncl(11,k) - ncl(8,k)
      nclr(1,ll,1,2) = ncl(20,k+kk) - ncl(17,k+kk)
! particles going back and up
      ncll(1,ll,2,2) = ncl(14,k) - ncl(11,k)
! particles going forward and up
      nclr(1,ll,2,2) = ncl(23,k+kk) - ncl(20,k+kk)
! particles going back and down
      ncll(1,ll,3,2) = ncl(17,k) - ncl(14,k)
! particles going forward and down
      nclr(1,ll,3,2) = ncl(26,k+kk) - ncl(23,k+kk)
  380 continue
!$OMP END PARALLEL DO
! perform prefix scan
      kk = 1
  390 if (kk.ge.mxyp1) go to 410
!$OMP PARALLEL DO PRIVATE(ll,ii,nn,mm)
      do 400 ll = 1, mxyp1
      ii = (ll - 1)/kk
      nn = kk*ii
      mm = 2*nn + kk - 1
      nn = nn + ll + kk
      if (nn.le.mxyp1) then
         ncll(1,nn,1,2) = ncll(1,nn,1,2) + ncll(1,mm+1,1,2)
         nclr(1,nn,1,2) = nclr(1,nn,1,2) + nclr(1,mm+1,1,2)
         ncll(1,nn,2,2) = ncll(1,nn,2,2) + ncll(1,mm+1,2,2)
         nclr(1,nn,2,2) = nclr(1,nn,2,2) + nclr(1,mm+1,2,2)
         ncll(1,nn,3,2) = ncll(1,nn,3,2) + ncll(1,mm+1,3,2)
         nclr(1,nn,3,2) = nclr(1,nn,3,2) + nclr(1,mm+1,3,2)
      endif
  400 continue
!$OMP END PARALLEL DO
      kk = kk + kk
      go to 390
  410 kk = mxyp1*(mzp1 - 1)
      j1 = ncll(1,mxyp1,1,2)
      k1 = j1 + ncll(1,mxyp1,2,2)
      j2 = nclr(1,mxyp1,1,2)
      k2 = j2 + nclr(1,mxyp1,2,2)
!$OMP PARALLEL DO PRIVATE(i,j,k,ll,ii,nn,mm)
      do 600 ll = 1, mxyp1
      k = ll
! particles going straight up
      ii = ncl(11,k) - ncl(8,k)
      nn = ncll(1,ll,1,2) - ii
      do 430 j = 1, min(ii,nbmax-nn)
      do 420 i = 1, idimp
      sbufl(i,j+nn,2) = ppbuff(i,j+ncl(8,k),k)
  420 continue
  430 continue
      do 440 i = 1, 3
      ncll(i,ll,1,2) = ncl(i+8,k) - ncl(8,k) + nn
  440 continue
! particles going straight down
      ii = ncl(20,k+kk) - ncl(17,k+kk)
      mm = nclr(1,ll,1,2) - ii
      do 460 j = 1, min(ii,nbmax-mm)
      do 450 i = 1, idimp
      sbufr(i,j+mm,2) = ppbuff(i,j+ncl(17,k+kk),k+kk)
  450 continue
  460 continue
      do 470 i = 1, 3
      nclr(i,ll,1,2) = ncl(i+17,k+kk) - ncl(17,k+kk) + mm
  470 continue
! particles going up and back
      ii = ncl(14,k) - ncl(11,k)
      nn = j1 + ncll(1,ll,2,2) - ii
      do 490 j = 1, min(ii,nbmax-nn)
      do 480 i = 1, idimp
      sbufl(i,j+nn,2) = ppbuff(i,j+ncl(11,k),k)
  480 continue
  490 continue
      do 500 i = 1, 3
      ncll(i,ll,2,2) = ncl(i+11,k) - ncl(11,k) + nn
  500 continue
! particles going down and back
      ii = ncl(23,k+kk) - ncl(20,k+kk)
      mm = j2 + nclr(1,ll,2,2) - ii
      do 520 j = 1, min(ii,nbmax-mm)
      do 510 i = 1, idimp
      sbufr(i,j+mm,2) = ppbuff(i,j+ncl(20,k+kk),k+kk)
  510 continue
  520 continue
      do 530 i = 1, 3
      nclr(i,ll,2,2) = ncl(i+20,k+kk) - ncl(20,k+kk) + mm
  530 continue
! particles going up and forward
      ii = ncl(17,k) - ncl(14,k)
      nn = k1 + ncll(1,ll,3,2) - ii
      do 550 j = 1, min(ii,nbmax-nn)
      do 540 i = 1, idimp
      sbufl(i,j+nn,2) = ppbuff(i,j+ncl(14,k),k)
  540 continue
  550 continue
      do 560 i = 1, 3
      ncll(i,ll,3,2) = ncl(i+14,k) - ncl(14,k) + nn
  560 continue
! particles going down and forward, to different node
      ii = ncl(26,k+kk) - ncl(23,k+kk)
      mm = k2 + nclr(1,ll,3,2) - ii
      do 580 j = 1, min(ii,nbmax-mm)
      do 570 i = 1, idimp
      sbufr(i,j+mm,2) = ppbuff(i,j+ncl(23,k+kk),k+kk)
  570 continue
  580 continue
      do 590 i = 1, 3
      nclr(i,ll,3,2) = ncl(i+23,k+kk) - ncl(23,k+kk) + mm
  590 continue
  600 continue
!$OMP END PARALLEL DO
! sbufl or sbufr overflow
      kk = max(ncll(3,mxzp1,3,1),nclr(3,mxzp1,3,1))
      ll = max(ncll(3,mxyp1,3,2),nclr(3,mxyp1,3,2))
      ii = max(kk,ll)
! corners overflow
      nn = nclr(3,mx1,2,1) - nclr(3,mxzp1,1,1)
      mm = ncll(3,mx1,2,1) - ncll(3,mxzp1,1,1)
      in = mx1*(mzp1 - 1)
      if (in.gt.0) then
         nr = nclr(3,in,3,1)
         nl = ncll(3,in,3,1)
      else
         nr = nclr(3,mxzp1,2,1)
         nl = ncll(3,mxzp1,2,1)
      endif
      kk = nclr(3,in+mx1,3,1) - nr
      ll = ncll(3,in+mx1,3,1) - nl
! total overflow: result valid only for one processor case
      ii = ii + max(nn+kk,mm+ll)
      if (ii.gt.nbmax) then
         irc2(1) = 3
         irc2(2) = ii
      endif
      return
      end
!-----------------------------------------------------------------------
      subroutine VPPPORDER32LB(ppart,ppbuff,rbufl,rbufr,kpic,ncl,ihole, &
     &mcll,mclr,mcls,idimp,nppmx,nx,ny,nz,mx1,myp1,mzp1,mxzyp1,npbmx,   &
     &ntmax,nbmax,irc2)
! this subroutine performs second part of a particle sort by x,y,z grid
! in tiles of mx, my, mz
! linear interpolation, with periodic boundary conditions
! for distributed data, with 2d domain decomposition in y/z.
! tiles are assumed to be arranged in 3D linear memory
! incoming particles from other tiles are copied from ppbuff, rbufl, and
! rbufr into ppart
! input: all except ppart, kpic, irc2
! output: ppart, kpic, irc2
! ppart(1,n,m) = position x of particle n in tile m
! ppart(2,n,m) = position y of particle n in tile m
! ppart(3,n,m) = position z of particle n in tile m
! ppbuff(i,n,l) = i co-ordinate of particle n in tile l
! rbufl = buffer for particles being received from lower/back processor
! rbufr = buffer for particles being received from upper/forward
! processor
! kpic(k) = number of particles in tile k
! ncl(i,k) = number of particles going to destination i, tile k
! ihole(1,:,k) = location of hole in array left by departing particle
! ihole(2,:,k) = direction destination of particle leaving hole
! all for tile k
! ihole(1,1,k) = ih, number of holes left (error, if negative)
! mcll = number offset being received from lower/back processor
! mclr = number offset being received from upper/forward processor
! mcls = number ofsets received from corner processors
! idimp = size of phase space = 6
! nppmx = maximum number of particles in tile
! nx/ny/nz = system length in x/y/z direction
! mx1 = (system length in x direction - 1)/mx + 1
! myp1 = (partition length in y direction - 1)/my + 1
! mzp1 = (partition length in z direction - 1)/mz + 1
! mxzyp1 = mx1*max(myp1,mzp1)
! npbmx = size of buffer array ppbuff
! ntmax = size of hole array for particles leaving tiles
! nbmax =  size of buffers for passing particles between processors
! when (irc2(1).eq.4), ppart overflow, irc2(2) = new nppmx required
      implicit none
      integer idimp, nppmx, nx, ny, nz, mx1, myp1, mzp1, mxzyp1, npbmx
      integer ntmax, nbmax
      real ppart, ppbuff, rbufl, rbufr
      integer kpic, ncl, ihole, mcll, mclr, mcls, irc2
      dimension ppart(idimp,nppmx,mx1*myp1*mzp1)
      dimension ppbuff(idimp,npbmx,mx1*myp1*mzp1)
      dimension rbufl(idimp,nbmax,2), rbufr(idimp,nbmax,2)
      dimension kpic(mx1*myp1*mzp1), ncl(26,mx1*myp1*mzp1)
      dimension ihole(2,ntmax+1,mx1*myp1*mzp1)
      dimension mcll(3,mxzyp1,3,2), mclr(3,mxzyp1,3,2), mcls(3,mx1+1,4)
      dimension irc2(2)
! local data
      integer npblk, lvect
      parameter(npblk=32,lvect=4)
      integer mxyp1, mxyzp1, nppp, ncoff, joff, koff, ipp, nps
      integer i, j, k, l, m, nn, ii, in, kx, ky, kz, ih, nh, ist, lorr
      integer ip, j1, j2, kxl, kxr, kk, kl, kr, ll, lk, lr, mm, kzs
      real anx, any, anz, dx, dy, dz
      logical inside
      integer ks
!dir$ attributes align : 64 :: ks
      dimension ks(26)
! scratch integer array
      integer n
!dir$ attributes align : 64 :: n
      dimension n(npblk,lvect)
      mxyp1 = mx1*myp1
      mxyzp1 = mxyp1*mzp1
      anx = real(nx)
      any = real(ny)
      anz = real(nz)
! copy incoming particles from buffer into ppart: update ppart, kpic
! loop over tiles
!$OMP PARALLEL DO                                                       &
!$OMP& PRIVATE(i,j,k,l,ii,kk,nppp,kx,ky,kz,kl,kr,kxl,kxr,lk,ll,lr,mm,kzs&
!$OMP& ,ih,nh,ncoff,joff,koff,ist,j1,ip,dx,dy,dz,lorr,inside,ks)
      do 160 l = 1, mxyzp1
      nppp = kpic(l)
      kz = (l - 1)/mxyp1
      k = l - mxyp1*kz
      kzs = kz*mx1
      kz = kz + 1
! loop over tiles in z
      lk = (kz - 1)*mxyp1
! find tile behind
      ll = (kz - 2)*mxyp1
! find tile in front
      lr = kz*mxyp1
      ky = (k - 1)/mx1 + 1
! loop over tiles in y
      kk = (ky - 1)*mx1
! find tile above
      kl = (ky - 2)*mx1
! find tile below
      kr = ky*mx1
! loop over tiles in x, assume periodic boundary conditions
      kx = k - (ky - 1)*mx1
      kxl = kx - 1 
      if (kxl.lt.1) kxl = kxl + mx1
      kxr = kx + 1
      if (kxr.gt.mx1) kxr = kxr - mx1
! find tile number for different directions
      ks(1) = kxr + kk + lk
      ks(2) = kxl + kk + lk
      if (ky.eq.myp1) then
         ks(3) = -kx
         ks(4) = -kxr
         ks(5) = -kxl
      else
         ks(3) = kx + kr + lk
         ks(4) = kxr + kr + lk
         ks(5) = kxl + kr + lk
      endif
      if (ky.eq.1) then
         ks(6) = -kx
         ks(7) = -kxr 
         ks(8) = -kxl
      else
         ks(6) = kx + kl + lk
         ks(7) = kxr + kl + lk
         ks(8) = kxl + kl + lk
      endif
      if (kz.eq.mzp1) then
         ks(9) = -kx
         ks(10) = -kxr
         ks(11) = -kxl
      else
         ks(9) = kx + kk + lr
         ks(10) = kxr + kk + lr
         ks(11) = kxl + kk + lr
      endif
      if ((ky.eq.myp1).or.(kz.eq.mzp1)) then
         ks(12) = -kx
         ks(13) = -kxr
         ks(14) = -kxl
      else
         ks(12) = kx + kr + lr
         ks(13) = kxr + kr + lr
         ks(14) = kxl + kr + lr
      endif
      if ((ky.eq.1).or.(kz.eq.mzp1)) then
         ks(15) = -kx
         ks(16) = -kxr
         ks(17) = -kxl
      else
         ks(15) = kx + kl + lr
         ks(16) = kxr + kl + lr
         ks(17) = kxl + kl + lr
      endif
      if (kz.eq.1) then
         ks(18) = -kx
         ks(19) = -kxr 
         ks(20) = -kxl
      else
         ks(18) = kx + kk + ll
         ks(19) = kxr + kk + ll
         ks(20) = kxl + kk + ll
      endif
      if ((ky.eq.myp1).or.(kz.eq.1)) then
         ks(21) = -kx
         ks(22) = -kxr
         ks(23) = -kxl
      else
         ks(21) = kx + kr + ll
         ks(22) = kxr + kr + ll
         ks(23) = kxl + kr + ll
      endif
      if ((ky.eq.1).or.(kz.eq.1)) then
         ks(24) = -kx
         ks(25) = -kxr
         ks(26) = -kxl
      else
         ks(24) = kx + kl + ll
         ks(25) = kxr + kl + ll
         ks(26) = kxl + kl + ll
      endif
! identify interior
      if ((ky.gt.1).and.(ky.lt.myp1).and.(kz.gt.1).and.(kz.lt.mzp1))    &
     & then
         inside = .true.
      else
         inside = .false.
      endif
! loop over directions
      nh = ihole(1,1,l)
      joff = 0
      koff = 0
      ncoff = 0
      ih = 0
      ist = 0
      j1 = 0
      do 150 ii = 1, 26
      lorr = 0
      ip = -1
! ip = number of particles coming from direction ii
! interior layers
      if (inside) then
         if (ii.gt.1) ncoff = ncl(ii-1,ks(ii))
         ip = ncl(ii,ks(ii)) - ncoff
! edges
      else
! top layer
         if (ky.eq.1) then
            if ((ii.ge.6).and.(ii.le.8)) then
               lorr = -1
               joff = 0
               mm = kzs - ks(ii)
               if (ii.eq.6) then
                  if (mm.gt.1) joff = mcll(3,mm-1,1,1)
               else
                  joff = mcll(ii-6,mm,1,1)
               endif
               ip = mcll(ii-5,mm,1,1) - joff
            else if ((ii.ge.15).and.(ii.le.17)) then
               lorr = -2
               joff = mcll(3,mx1*mzp1,1,1)
               if (kz.lt.mzp1) then
                  mm = kzs + mx1 - ks(ii)
                  if (ii.eq.15) then
                     if (mm.gt.1) joff = mcll(3,mm-1,2,1)
                  else
                     joff = mcll(ii-15,mm,2,1)
                  endif
                  ip = mcll(ii-14,mm,2,1) - joff
! corner data, (ky=1,kz=mzp1)
               else
                  mm = -ks(ii)
                  if (ii.eq.15) then
                     if (mm.eq.1) joff = mcls(1,mx1+1,1)
                     if (mm.gt.1) joff = mcls(3,mm-1,1)
                  else
                     joff = mcls(ii-15,mm,1)
                  endif
                  ip = mcls(ii-14,mm,1) - joff
               endif
            else if ((ii.ge.24).and.(ii.le.26)) then
               lorr = -3
               joff = mcll(3,mx1*mzp1,2,1)
               if (kz.gt.1) then
                  mm = kzs - mx1 - ks(ii)
                  if (ii.eq.24) then
                     if (mm.gt.1) joff = mcll(3,mm-1,3,1)
                  else
                     joff = mcll(ii-24,mm,3,1)
                  endif
                  ip = mcll(ii-23,mm,3,1) - joff
! corner data, (ky=1,kz=1)
               else
                  mm = -ks(ii)
                  if (ii.eq.24) then
                     if (mm.eq.1) joff = mcls(1,mx1+1,2)
                     if (mm.gt.1) joff = mcls(3,mm-1,2)
                  else
                     joff = mcls(ii-24,mm,2)
                  endif
                  ip = mcls(ii-23,mm,2) - joff
               endif
! internal data
            else
               if (ks(ii).gt.0) then
                  if (ii.gt.1) ncoff = ncl(ii-1,ks(ii))
                  ip = ncl(ii,ks(ii)) - ncoff
               endif
            endif
         endif
! bottom layer
         if (ky.eq.myp1) then
            if ((ii.ge.3).and.(ii.le.5)) then
               lorr = 1
               joff = 0
               mm = kzs - ks(ii)
               if (ii.eq.3) then
                  if (mm.gt.1) joff = mclr(3,mm-1,1,1)
               else
                  joff = mclr(ii-3,mm,1,1)
               endif
               ip = mclr(ii-2,mm,1,1) - joff
            else if ((ii.ge.12).and.(ii.le.14)) then
               lorr = 2
               joff = mclr(3,mx1*mzp1,1,1)
               if (kz.lt.mzp1) then
                  mm = kzs + mx1 - ks(ii)
                  if (ii.eq.12) then
                     if (mm.gt.1) joff = mclr(3,mm-1,2,1)
                  else
                     joff = mclr(ii-12,mm,2,1)
                  endif
                  ip = mclr(ii-11,mm,2,1) - joff
! corner data, (ky=myp1,kz=mzp1)
               else
                  mm = -ks(ii)
                  if (ii.eq.12) then
                     if (mm.eq.1) joff = mcls(1,mx1+1,3)
                     if (mm.gt.1) joff = mcls(3,mm-1,3)
                  else
                     joff = mcls(ii-12,mm,3)
                  endif
                  ip = mcls(ii-11,mm,3) - joff
               endif
            else if ((ii.ge.21).and.(ii.le.23)) then
               lorr = 3
               joff = mclr(3,mx1*mzp1,2,1)
               if (kz.gt.1) then
                  mm = kzs - mx1 - ks(ii)
                  if (ii.eq.21) then
                    if (mm.gt.1) joff = mclr(3,mm-1,3,1)
                  else
                     joff = mclr(ii-21,mm,3,1)
                  endif
                  ip = mclr(ii-20,mm,3,1) - joff
! corner data, (ky=myp1,kz=1)
               else
                  mm = -ks(ii)
                  if (ii.eq.21) then
                     if (mm.eq.1) joff = mcls(1,mx1+1,4)
                     if (mm.gt.1) joff = mcls(3,mm-1,4)
                  else
                     joff = mcls(ii-21,mm,4)
                  endif
                  ip = mcls(ii-20,mm,4) - joff
               endif
! internal data
            else
               if (ks(ii).gt.0) then
                  if (ky.gt.1) then
                     if (ii.gt.1) ncoff = ncl(ii-1,ks(ii))
                     ip = ncl(ii,ks(ii)) - ncoff
                  endif
               endif
            endif
         endif
! front layer
         if (kz.eq.1) then
            if ((ii.ge.18).and.(ii.le.20)) then
               koff = 0
               mm = kk - ks(ii)
               if (ii.eq.18) then
                  if (mm.gt.1) koff = mcll(3,mm-1,1,2)
               else
                  koff = mcll(ii-18,mm,1,2)
               endif
               ip = mcll(ii-17,mm,1,2) - koff
            else if ((ii.ge.21).and.(ii.le.23)) then
               koff = mcll(3,mx1*myp1,1,2)
               if (ky.lt.myp1) then
                  mm = kr - ks(ii)
                  if (ii.eq.21) then
                     if (mm.gt.1) koff = mcll(3,mm-1,2,2)
                  else
                     koff = mcll(ii-21,mm,2,2)
                  endif
                  ip = mcll(ii-20,mm,2,2) - koff
! corner data, already done
!              else
               endif
            else if ((ii.ge.24).and.(ii.le.26)) then
               koff = mcll(3,mx1*myp1,2,2)
               if (ky.gt.1) then
                  mm = kl - ks(ii)
                  if (ii.eq.24) then
                     if (mm.gt.1) koff = mcll(3,mm-1,3,2)
                  else
                     koff = mcll(ii-24,mm,3,2)
                  endif
                  ip = mcll(ii-23,mm,3,2) - koff
! corner data, already done
!              else
               endif
! internal data
            else
               if (ks(ii).gt.0) then
                  if ((ky.gt.1).and.(ky.lt.myp1)) then
                     if (ii.gt.1) ncoff = ncl(ii-1,ks(ii))
                     ip = ncl(ii,ks(ii)) - ncoff
                  endif
               endif
            endif
         endif
! back layer
         if (kz.eq.mzp1) then
            if ((ii.ge.9).and.(ii.le.11)) then
               koff = 0
               mm = kk - ks(ii)
               if (ii.eq.9) then
                  if (mm.gt.1) koff = mclr(3,mm-1,1,2)
               else
                  koff = mclr(ii-9,mm,1,2)
               endif
               ip = mclr(ii-8,mm,1,2) - koff
            else if ((ii.ge.12).and.(ii.le.14)) then
               koff = mclr(3,mx1*myp1,1,2)
               if (ky.lt.myp1) then
                  mm = kr - ks(ii)
                  if (ii.eq.12) then
                     if (mm.gt.1) koff = mclr(3,mm-1,2,2)
                  else
                     koff = mclr(ii-12,mm,2,2)
                  endif
                  ip = mclr(ii-11,mm,2,2) - koff
! corner data, already done
!              else
               endif
            else if ((ii.ge.15).and.(ii.le.17)) then
               koff = mclr(3,mx1*myp1,2,2)
               if (ky.gt.1) then
                  mm = kl - ks(ii)
                  if (ii.eq.15) then
                     if (mm.gt.1) koff = mclr(3,mm-1,3,2)
                  else
                     koff = mclr(ii-15,mm,3,2)
                  endif
                  ip = mclr(ii-14,mm,3,2) - koff
! corner data, already done
!              else
               endif
! internal data
            else
               if (ks(ii).gt.0) then
                  if ((ky.gt.1).and.(ky.lt.myp1)) then
                     if (ii.gt.1) ncoff = ncl(ii-1,ks(ii))
                     ip = ncl(ii,ks(ii)) - ncoff
                  endif
               endif
            endif
         endif
      endif
!
      if (ip.lt.0) write (*,*) 'help, ip undefined:l,ii=',l,ii
! copy incoming particles
      do 140 j = 1, ip
      ih = ih + 1
! insert incoming particles into holes
      if (ih.le.nh) then
         j1 = ihole(1,ih+1,l)
! place overflow at end of array
      else
         j1 = nppp + 1
         nppp = j1
      endif
      if (j1.le.nppmx) then
! interior layers
         if (inside) then
! check for periodic boundary conditions
            dx = ppbuff(1,j+ncoff,ks(ii))
            if (dx.lt.0.0) dx = dx + anx
            if (dx.ge.anx) dx = dx - anx
            ppart(1,j1,l) = dx
            dy = ppbuff(2,j+ncoff,ks(ii))
            if (dy.lt.0.0) dy = dy + any
            if (dy.ge.any) dy = dy - any
            ppart(2,j1,l) = dy
            dz = ppbuff(3,j+ncoff,ks(ii))
            if (dz.lt.0.0) dz = dz + anz
            if (dz.ge.anz) dz = dz - anz
            ppart(3,j1,l) = dz
! copy remaining particle data
            do 10 i = 4, idimp
            ppart(i,j1,l) = ppbuff(i,j+ncoff,ks(ii))
   10       continue
! edges
         else
! top layer
            if (ky.eq.1) then
! external data
               if (lorr.lt.0) then
! check for periodic boundary conditions
                  dx = rbufl(1,j+joff,1)
                  if (dx.lt.0.0) dx = dx + anx
                  if (dx.ge.anx) dx = dx - anx
                  ppart(1,j1,l) = dx
                  dy = rbufl(2,j+joff,1)
                  if (dy.lt.0.0) dy = dy + any
                  if (dy.ge.any) dy = dy - any
                  ppart(2,j1,l) = dy
                  dz = rbufl(3,j+joff,1)
                  if (dz.lt.0.0) dz = dz + anz
                  if (dz.ge.anz) dz = dz - anz
                  ppart(3,j1,l) = dz
! copy remaining particle data
                  do 20 i = 4, idimp
                  ppart(i,j1,l) = rbufl(i,j+joff,1)
   20             continue
! internal data
               else if (ks(ii).gt.0) then
! check for periodic boundary conditions
                  dx = ppbuff(1,j+ncoff,ks(ii))
                  if (dx.lt.0.0) dx = dx + anx
                  if (dx.ge.anx) dx = dx - anx
                  ppart(1,j1,l) = dx
                  dy = ppbuff(2,j+ncoff,ks(ii))
                  if (dy.lt.0.0) dy = dy + any
                  if (dy.ge.any) dy = dy - any
                  ppart(2,j1,l) = dy
                  dz = ppbuff(3,j+ncoff,ks(ii))
                  if (dz.lt.0.0) dz = dz + anz
                  if (dz.ge.anz) dz = dz - anz
                  ppart(3,j1,l) = dz
! copy remaining particle data
                  do 30 i = 4, idimp
                  ppart(i,j1,l) = ppbuff(i,j+ncoff,ks(ii))
   30             continue
               endif
            endif
! bottom layer
            if (ky.eq.myp1) then
! external data
               if (lorr.gt.0) then
! check for periodic boundary conditions
                  dx = rbufr(1,j+joff,1)
                  if (dx.lt.0.0) dx = dx + anx
                  if (dx.ge.anx) dx = dx - anx
                  ppart(1,j1,l) = dx
                  dy = rbufr(2,j+joff,1)
                  if (dy.lt.0.0) dy = dy + any
                  if (dy.ge.any) dy = dy - any
                  ppart(2,j1,l) = dy
                  dz = rbufr(3,j+joff,1)
                  if (dz.lt.0.0) dz = dz + anz
                  if (dz.ge.anz) dz = dz - anz
                  ppart(3,j1,l) = dz
! copy remaining particle data
                  do 40 i = 4, idimp
                  ppart(i,j1,l) = rbufr(i,j+joff,1)
   40             continue
! internal data
               else if (ks(ii).gt.0) then
                  if (ky.gt.1) then
! check for periodic boundary conditions
                     dx = ppbuff(1,j+ncoff,ks(ii))
                     if (dx.lt.0.0) dx = dx + anx
                     if (dx.ge.anx) dx = dx - anx
                     ppart(1,j1,l) = dx
                     dy = ppbuff(2,j+ncoff,ks(ii))
                     if (dy.lt.0.0) dy = dy + any
                     if (dy.ge.any) dy = dy - any
                     ppart(2,j1,l) = dy
                     dz = ppbuff(3,j+ncoff,ks(ii))
                     if (dz.lt.0.0) dz = dz + anz
                     if (dz.ge.anz) dz = dz - anz
                     ppart(3,j1,l) = dz
! copy remaining particle data
                     do 50 i = 4, idimp
                     ppart(i,j1,l) = ppbuff(i,j+ncoff,ks(ii))
   50                continue
                  endif
               endif
            endif
! front layer
            if (kz.eq.1) then
              if ((ii.ge.18).and.(ii.le.20)) then
! check for periodic boundary conditions
                  dx = rbufl(1,j+koff,2)
                  if (dx.lt.0.0) dx = dx + anx
                  if (dx.ge.anx) dx = dx - anx
                  ppart(1,j1,l) = dx
                  dy = rbufl(2,j+koff,2)
                  if (dy.lt.0.0) dy = dy + any
                  if (dy.ge.any) dy = dy - any
                  ppart(2,j1,l) = dy
                  dz = rbufl(3,j+koff,2)
                  if (dz.lt.0.0) dz = dz + anz
                  if (dz.ge.anz) dz = dz - anz
                  ppart(3,j1,l) = dz
! copy remaining particle data
                  do 60 i = 4, idimp
                  ppart(i,j1,l) = rbufl(i,j+koff,2)
   60             continue
               else if ((ii.ge.21).and.(ii.le.23)) then
                  if (ky.lt.myp1) then
! check for periodic boundary conditions
                     dx = rbufl(1,j+koff,2)
                     if (dx.lt.0.0) dx = dx + anx
                     if (dx.ge.anx) dx = dx - anx
                     ppart(1,j1,l) = dx
                     dy = rbufl(2,j+koff,2)
                     if (dy.lt.0.0) dy = dy + any
                     if (dy.ge.any) dy = dy - any
                     ppart(2,j1,l) = dy
                     dz = rbufl(3,j+koff,2)
                     if (dz.lt.0.0) dz = dz + anz
                     if (dz.ge.anz) dz = dz - anz
                     ppart(3,j1,l) = dz
! copy remaining particle data
                     do 70 i = 4, idimp
                     ppart(i,j1,l) = rbufl(i,j+koff,2)
   70                continue
                  endif
               else if ((ii.ge.24).and.(ii.le.26)) then
                  if (ky.gt.1) then
! check for periodic boundary conditions
                     dx = rbufl(1,j+koff,2)
                     if (dx.lt.0.0) dx = dx + anx
                     if (dx.ge.anx) dx = dx - anx
                     ppart(1,j1,l) = dx
                     dy = rbufl(2,j+koff,2)
                     if (dy.lt.0.0) dy = dy + any
                     if (dy.ge.any) dy = dy - any
                     ppart(2,j1,l) = dy
                     dz = rbufl(3,j+koff,2)
                     if (dz.lt.0.0) dz = dz + anz
                     if (dz.ge.anz) dz = dz - anz
                     ppart(3,j1,l) = dz
! copy remaining particle data
                     do 80 i = 4, idimp
                     ppart(i,j1,l) = rbufl(i,j+koff,2)
   80                continue
                  endif
! internal data
               else if (ks(ii).gt.0) then
                  if ((ky.gt.1).and.(ky.lt.myp1)) then
! check for periodic boundary conditions
                     dx = ppbuff(1,j+ncoff,ks(ii))
                     if (dx.lt.0.0) dx = dx + anx
                     if (dx.ge.anx) dx = dx - anx
                     ppart(1,j1,l) = dx
                     dy = ppbuff(2,j+ncoff,ks(ii))
                     if (dy.lt.0.0) dy = dy + any
                     if (dy.ge.any) dy = dy - any
                     ppart(2,j1,l) = dy
                     dz = ppbuff(3,j+ncoff,ks(ii))
                     if (dz.lt.0.0) dz = dz + anz
                     if (dz.ge.anz) dz = dz - anz
                     ppart(3,j1,l) = dz
! copy remaining particle data
                     do 90 i = 4, idimp
                     ppart(i,j1,l) = ppbuff(i,j+ncoff,ks(ii))
   90                continue
                  endif
               endif
            endif
! back layer
            if (kz.eq.mzp1) then
               if ((ii.ge.9).and.(ii.le.11)) then
! check for periodic boundary conditions
                  dx = rbufr(1,j+koff,2)
                  if (dx.lt.0.0) dx = dx + anx
                  if (dx.ge.anx) dx = dx - anx
                  ppart(1,j1,l) = dx
                  dy = rbufr(2,j+koff,2)
                  if (dy.lt.0.0) dy = dy + any
                  if (dy.ge.any) dy = dy - any
                  ppart(2,j1,l) = dy
                  dz = rbufr(3,j+koff,2)
                  if (dz.lt.0.0) dz = dz + anz
                  if (dz.ge.anz) dz = dz - anz
                  ppart(3,j1,l) = dz
! copy remaining particle data
                  do 100 i = 4, idimp
                  ppart(i,j1,l) = rbufr(i,j+koff,2)
  100             continue
               else if ((ii.ge.12).and.(ii.le.14)) then
                  if (ky.lt.myp1) then
! check for periodic boundary conditions
                     dx = rbufr(1,j+koff,2)
                     if (dx.lt.0.0) dx = dx + anx
                     if (dx.ge.anx) dx = dx - anx
                     ppart(1,j1,l) = dx
                     dy = rbufr(2,j+koff,2)
                     if (dy.lt.0.0) dy = dy + any
                     if (dy.ge.any) dy = dy - any
                     ppart(2,j1,l) = dy
                     dz = rbufr(3,j+koff,2)
                     if (dz.lt.0.0) dz = dz + anz
                     if (dz.ge.anz) dz = dz - anz
                     ppart(3,j1,l) = dz
! copy remaining particle data
                     do 110 i = 4, idimp
                     ppart(i,j1,l) = rbufr(i,j+koff,2)
  110                continue
                  endif
               else if ((ii.ge.15).and.(ii.le.17)) then
                  if (ky.gt.1) then
! check for periodic boundary conditions
                     dx = rbufr(1,j+koff,2)
                     if (dx.lt.0.0) dx = dx + anx
                     if (dx.ge.anx) dx = dx - anx
                     ppart(1,j1,l) = dx
                     dy = rbufr(2,j+koff,2)
                     if (dy.lt.0.0) dy = dy + any
                     if (dy.ge.any) dy = dy - any
                     ppart(2,j1,l) = dy
                     dz = rbufr(3,j+koff,2)
                     if (dz.lt.0.0) dz = dz + anz
                     if (dz.ge.anz) dz = dz - anz
                     ppart(3,j1,l) = dz
! copy remaining particle data
                     do 120 i = 4, idimp
                     ppart(i,j1,l) = rbufr(i,j+koff,2)
  120                continue
                  endif
! internal data
               else if (ks(ii).gt.0) then
                  if ((ky.gt.1).and.(ky.lt.myp1)) then
! check for periodic boundary conditions
                     dx = ppbuff(1,j+ncoff,ks(ii))
                     if (dx.lt.0.0) dx = dx + anx
                     if (dx.ge.anx) dx = dx - anx
                     ppart(1,j1,l) = dx
                     dy = ppbuff(2,j+ncoff,ks(ii))
                     if (dy.lt.0.0) dy = dy + any
                     if (dy.ge.any) dy = dy - any
                     ppart(2,j1,l) = dy
                     dz = ppbuff(3,j+ncoff,ks(ii))
                     if (dz.lt.0.0) dz = dz + anz
                     if (dz.ge.anz) dz = dz - anz
                     ppart(3,j1,l) = dz
! copy remaining particle data
                     do 130 i = 4, idimp
                     ppart(i,j1,l) = ppbuff(i,j+ncoff,ks(ii))
  130                continue
                  endif
               endif
            endif
         endif
      else
         ist = 1
      endif
  140 continue
  150 continue
! save parameters for next loop
      if (ih.lt.nh) then
         ihole(2,1,l) = -(ih+1)
      else
         ihole(2,1,l) = nppp
      endif
! set error
      if (ist.gt.0) irc2(1) = 4
  160 continue
!$OMP END PARALLEL DO
! ppart overflow
      if (irc2(1).gt.0) then
         j1 = 0
         do 170 l = 1, mxyzp1
         j1 = max(j1,ihole(2,1,l))
  170    continue
         irc2(2) = j1
         return
      endif
! fill up remaining holes in particle array with particles from bottom
! holes with locations great than npp-ip do not need to be filled
!$OMP PARALLEL DO                                                       &
!$OMP& PRIVATE(i,j,l,m,ih,nppp,nh,ip,ii,ipp,joff,nps,in,nn,mm,j1,j2,n)
      do 250 l = 1, mxyzp1
      ih = ihole(2,1,l)
      if (ih.lt.0) then
         nppp = kpic(l)
         nh = ihole(1,1,l)
         ip = nh + ih + 1
! move particles from end into remaining holes
! holes are processed in increasing order
         ii = nh + 1
         ih = 1 - ih
         ipp = ip/npblk
! outer loop over number of full blocks
         do 220 m = 1, ipp
         joff = npblk*(m - 1)
! inner loop over particles in block
         do 180 j = 1, npblk
         n(j,2) = ihole(1,ih+j-1,l)
         n(j,3) = ihole(1,ii-j+1,l)
  180    continue
         in = 1
         mm = 1
         nn = n(in,3)
         do 190 j = 1, npblk
         j1 = nppp - j - joff + 1
         n(j,1) = n(mm,2)
         if (j1.eq.nn) then
            n(j,1) = -1
            in = in + 1
            if (j.lt.npblk) nn = n(in,3)
         else
            mm = mm + 1
         endif
  190    continue
         do 210 j = 1, npblk
         j1 = nppp - j - joff + 1
         j2 = n(j,1)
         if (j2.gt.0) then
            do 200 i = 1, idimp
            ppart(i,j2,l) = ppart(i,j1,l)
  200       continue
         endif
  210    continue
         ii = ii - in + 1
         ih = ih + mm - 1
  220    continue
         nps = npblk*ipp + 1
         nn = ihole(1,ii,l)
         j2 = ihole(1,ih,l)
! loop over remaining particles
         do 240 j = nps, ip
         j1 = nppp - j + 1
         if (j1.eq.nn) then
            ii = ii - 1
            nn = ihole(1,ii,l)
         else
            do 230 i = 1, idimp
            ppart(i,j2,l) = ppart(i,j1,l)
  230       continue
            ih = ih + 1
            if (j.lt.ip) j2 = ihole(1,ih,l)
         endif
  240    continue
         kpic(l) = nppp - ip
      else
         kpic(l) = ihole(2,1,l)
      endif
  250 continue
!$OMP END PARALLEL DO
      return
      end
