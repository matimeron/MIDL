Function DCMM_thru, ene, alp = alp, qz = qzv, chi = chi, $
	fcrys = fcr, find = fnd, scrys = scr, sind = snd, $
	fmtet = fmt, fmeta = fme, fmref = fmr, $
	smtet = smt, smeta = sme, smref = smr, $
	radians= rad, degrees= deg,_extra = _e

	on_error, 1
	if (One_of(deg,rad) > 0) then amult = 1d else amult = !dpi/180
	m15 = [0.8,0.0027,0.58]
	m20 = [0.6,0.0085,0.75]

	fcr = Default(fcr,'si')
	fnd = Default(fnd,[1,1,1],/dtyp)
	scr = Default(scr,'ge')
	snd = Default(snd,[1,1,1],/dtyp)

	ftet = Bragg_angle(ene=ene,crys=fcr,ind=fnd,eta=feta,/rad,/unc)
	stet = Bragg_angle(ene=ene,crys=scr,ind=snd,eta=seta,/rad,/unc)

	fref = Ref_curve(0,ene=ene,crys=fcr,ind=fnd)
	sref = Ref_curve(0,ene=ene,crys=scr,ind=snd)

	whi = How_many(fir=fmt,sec=fme,thi=fmr)
	if whi eq 3 then begin
		ftet = (stet = amult*fmt)
		feta = (seta = fme)
		fref = (sref = fmr)
	endif else if whi ne 0 then message, 'Missing Multilayer values!'

	whi = How_many(fir=smt,sec=sme,thi=smr)
	if whi eq 3 then begin
		stet = amult*smt
		seta = sme
		sref = smr
	endif else if whi ne 0 then message, 'Missing Multilayer values!'

	if n_elements(ene) ne 1 then message, 'Scalar energy required!'
;	if (One_of(deg,rad) > 0) then amult = 1d else amult = !dpi/180
	case One_of(alp,qzv,chi) of
		0	:	walp = amult*alp
		1	:	begin
			k = 2*!dpi*ene/!srcon.conv
			walp = asin(qzv/(2*k))
		end
		2	:	wchi = amult*chi
		else:	message, 'One of Alpha, Qz or Chi must be given!'
	endcase
	res = 0*wchi

	amp = Und_Brilliance(ene,har=5,per=28,und=2.1,ban=1,/def)
	abc0 = ABC_gen(ene,rsig=rsg,asig=asg,amp=amp,ban=1,_extra=_e)
	abc1 = ABC_refc(abc0,ftet,!dpi/2,eta=feta,ref=fref,/rad,/cle)
	abc2 = ABC_refc(abc1,ftet,-!dpi/2,eta=feta,ref=fref,/rad,/cle)
	iflux = ABC_int(abc2)

	nang = n_elements(wchi)
	for i = 0, nang - 1 do begin
		abc3 = ABC_refc(abc2,stet,wchi[i],eta=seta,ref=sref,/rad,tmat=tmt)
		res[i] = ABC_int(abc3)
	endfor

	return, res
end