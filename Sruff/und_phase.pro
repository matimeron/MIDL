Function UND_phase, s, erat=ert, k=k, gtet=gte, phi=phi, nper=npr, taper=tap, $
	eta= eta, deriv= der, _extra= _e

	on_error, 1

	gte = Default(gte,0d,/dtyp)
	phi = Default(phi,0d,/dtyp)
	eta = Default(eta,1,/dtyp)
	der = Default(der,0,/dtyp) > 0
	if der gt 2 then message, 'Derivatives of order >2 not supported!'
	check = Codims(ert,k,gte,phi,dim=dim,same=sam)
	if sam and (dim eq 0) then begin
		typ = Calctype(ert,k,0.)
		wk = Cast(k,5)
		fac0 = 1 + wk^2/2
		stet = gte/wk
		fac = fac0 + (wk*stet)^2
		u = wk^2/(4*fac)
		v = 8*u*stet*cos(phi)
		srt = ert*fac/fac0
		t = Default(tap,0d,/dtyp)/(2*!dpi*npr)
		ots = 1 - t*s

		case der of
			0	:	res = srt*(s+ u*((ots^2- t^2/2)*sin(2*s)- t*ots*cos(2*s))- $
					v*(ots*sin(s) - t*cos(s)) - 2*t*u*s^2*(2 + ots)/3) + eta*s
			1	:	res = srt*(1 + 2*u*ots^2*cos(2*s) - v*ots*cos(s) - $
					2*t*u*s*(1 + ots)) + eta
			2	:	res = srt*(-4*u*ots*(t*cos(2*s) + ots*sin(2*s)) + $
					v*(t*cos(s) + ots*sin(s)) - 4*t*u*ots)
		endcase
	endif else message, 'ERAT, K, GTE and PHI must be scalars!'

	return, Cast(res,typ,typ,/fix)
end