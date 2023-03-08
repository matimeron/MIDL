Pro ID_15_harm_eval, ene, mirror=mir, angle=ang, fele=fel, fnum=fnm, icsen=sen,$
	icrat = icr, _extra = _e

	on_error, 1

	wmir = Default(mir,'si')
	wfnm = Default(fnm,1)
	wsen = Default(sen,[1.,1.],/dtyp)
	if n_elements(wsen) eq 1 then wsen = [wsen,wsen]
	hfac = [1.,3]
	wene = ene*hfac
	flu = fltarr(2)

	bethi = 2e-2*2.54
	kapel = ['h','c','n','o']
	kapwei = [10,22,2,5]
	kapden = 1.42

	airel = ['n','o','ar']
	airwei = [78,21,1]
	airden = 1.3e-3

	fab = exp(-bethi*Abs_coeff(wene,ele='be'))* $
	exp(-18*2.54e-3*Abs_coeff(wene,ele=kapel,wei=kapwei,/form,den=kapden))* $
	exp(-5*Abs_coeff(wene,ele=airel,wei=airwei,/form,den=airden))
	sab = exp(-139*Abs_coeff(wene,ele=airel,wei=airwei,/form,den=airden))
	if Type(fel) eq 7 then sab = sab*exp(-wfnm*2.5e-3*Abs_coeff(wene,ele=fel))
	
	for i = 0, 1 do begin
		ref = Ref_curve(0,ene=wene[i],crys='si',ind=hfac[i]*[1,1,1])* $
		Mirror(wene[i],1e-3*ang,ele=wmir)
		dum = Bragg_angle(ene=wene[i],crys='si',ind=hfac[i]*[1,1,1],eta=eta)
		flu[i]=ref^2*$
		Und_flux(wene[i],per=33,har=hfac[i],/def,ban=eta,ape=[2,1],_extra=_e)
	endfor

	pflu = fab*flu
	cflu = sab*pflu
	monp = [Icflux(pflu[0],wene[0],sen=wsen[0],/inv), $
			Icflux(pflu[1],wene[1],sen=wsen[0],/inv)]*1e-3
	monc = [Icflux(cflu[0],wene[0],sen=wsen[1],/inv), $
			Icflux(cflu[1],wene[1],sen=wsen[1],/inv)]*1e-3

	print, monp, total(monp)
	print, monc, total(monc)
	icr = total(monc)/total(monp)
	print
	print, icr

	return
end