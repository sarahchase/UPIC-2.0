module mextdriv3

        use in3
	implicit none
	private
	public :: donothing, planewavedriv

	contains

	subroutine donothing()
		return
	end subroutine

	subroutine planewavedriv(fext,t, nx,nxe,nvp,idproc)
		implicit none
		real :: t
		integer :: nx,nxe,nvp,idproc
		real,dimension(:,:,:,:) :: fext
		real :: wavek, tfac, xpos, tempamp
		integer :: i

		wavek = 6.283185307/real(nx)
		tfac = 1.

		if (t < timerise) then
			tfac = t / timerise
		else if (t < timerise + timeflat + timefall) then
			tfac = 1. - (t - (timerise+timeflat))/timefall
		endif

		tempamp = tfac * amp
		
		if (t < timerise + timeflat + timefall) then
			do i = 1, nxe
				xpos = real(i)-2.
				fext(1,i,:,:) = tempamp * sin(wavek*xpos-wavew*t)
			enddo
		endif
	end subroutine

end module
