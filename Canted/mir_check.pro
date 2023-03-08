Pro Mir_check, ene, und_length = udl, length= len, thick= thi, angle= ang, $
	ape = ape, mirloc= mloc, _extra = _e

	on_error, 1

	if Streq(!blpar.sync,'apsu',4) then dudl = 2.1 else dudl = 2.4
	wudl = Default(udl,dudl,/dtyp)

	if len gt 10 then wlen = 1e-3*len else wlen = Cast(len,4)
	if thi gt 1 then wthi = 1e-3*thi else wthi = Cast(thi,4)
	if ang gt 0.01 then wang = 1e-3*ang else wang = Cast(ang,4)

	dape = 1e-3*!blpar.aper[1]*mloc/!blpar.dist
	if Isnum(ape) then begin
		if ape gt 0.01 then mape = 1e-3*ape else mape = Cast(ape,4)
		mape = mape < dape
	endif else mape = dape

	wape = wlen*wang
	if wape ge mape then begin
		wape = mape
		exc = 0
	endif else exc = 1
	foot = wape/wang

	sig = (Und_beamsize(ene,/def,und=wudl,dis=mloc,_extra=_e))[1]
	wsig = 1e-3*sig
	thru = 2*gaussint(wape/(2*wsig)) - 1
	psig = wsig/wang
	serr = Grav_serr(foot,psig,thi=wthi)
	print
	print, mloc, form = '("	Mirror loc.	=	",f5.2," m")'
	print, 1e3*wlen, form = '("	Optical length	=	",f5.1," mm")'
	print, 1e3*wthi, form = '("	Thickness	=	",f4.1," mm")'
	print, 1e3*wang, form = '("	Mirror angle	=	",f5.2," mr")'
	if exc then print,1e3*wape,form='("	Aperture 	=	",f5.2," mm (red.)")'$
	else print, 1e3*wape, form = '("	Aperture 	=	",f5.2," mm")'
	print
	print, ene, form = '("	Energy		=	",f5.2," keV")'
	print, sig, form = '("	Vertical sigma	=	",f6.3," mm")'
	print, 1e3*psig, form = '("	projected sigma	=	",f5.1," mm")'
	print, wape/wsig, form = '("	Aperture/sigma	=	",f5.3)'
	print, 1e3*foot, form = '("	Footprint	=	",f5.1," mm")'
	print, thru, form = '("	Acceptance	=	",f5.3)'
	print, 1e6*serr, form = '("	RMS slope error	=	",f5.2," microrad")'
	print
	
	return
end