Function UH_band_ave, x, k, n, ban, lambda = lam

    on_error, 1
    ban = Default(ban,0)
    if Isnum(x,/double) then wpi = !dpi else wpi = !pi
    pin = wpi*n
    lam = Default(lam,1/wpi,/dtype)
    zeps = (3*Toler(x))^(1./3.)

    if ban ne 0 then begin
	wx = Cast(x,4)
	res = wx
	dum = where(wx eq 0, ndum)
	if ndum ne 0 then res(dum) = Sp_beselj(pin*k,0)^2
	dum = where(wx ne 0, ndum)
	if ndum ne 0 then begin
	    i = complex(0,1)
	    ro = complex(1,1)
	    iwb = 2*i*pin*ban*wx(dum)
	    tb = 1/ban*(1 - k/wx(dum))
	    z1 = lam*ro*iwb
	    z2 = tb*iwb

	    z = z1 + z2
	    rp = exp(z) - 1 - z
	    ddum = where(Abs_mm(z) lt zeps, nddum)
	    if nddum gt 0 then rp(ddum) = 0.5*z(ddum)^2
	    rp = ro/z^2*rp

	    z = z1 - z2
	    rn = exp(z) - 1 - z
	    ddum = where(Abs_mm(z) lt zeps, nddum)
	    if nddum gt 0 then rn(ddum) = 0.5*z(ddum)^2
	    rn = ro/z^2*rn

	    res(dum) = imaginary(rp + rn)
	endif
    endif else res = Sp_beselj(pin*(x-k),0)^2

    return, res
end
