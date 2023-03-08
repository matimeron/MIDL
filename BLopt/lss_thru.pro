Function LSS_thru, ene, alp = alp, qz = qzv, chi = chi, rsig = rsg, asig= asg, $
	sorloc=sol, monloc=mol, mirloc=mil, slope_err=sle, crysloc=cyl, samloc=sal,$
	fcrys = fcr, find = fnd, scrys = scr, sind = snd, tcrys = tcr, tind = tnd, $
	compound= cmp, radians= rad, degrees= deg, show=sho, project=prj, wait=wai,$
	hrange = hrn, vrange = vrn, cumulative = cum, _extra = _e

	on_error, 1

	cfl = keyword_set(cmp)
	wsol = Default(sol,1.25,/dtyp)
	wmol = Default(mol,28.5,/dtyp)
	wmil = Default(mil,32.5,/dtyp)
	wsle = Default(sle,0.0,/dtyp)
	wcyl = Default(cyl,[55.0,55.5],/dtyp)
	if cfl and n_elements(wcyl) lt 2 then message, $
	'2 crystal locations required for COMPOUND option!'
	wsal = Default(sal,56.,/dtyp)
	seg = [wmol,wmil,wcyl,wsal] - [wsol,wmol,wmil,wcyl]
print, seg
	fcr = Default(fcr,'si')
	fnd = Default(fnd,[1,1,1],/dtyp)
	scr = Default(scr,'ge')
	snd = Default(snd,[1,1,1],/dtyp)
	tcr = Default(tcr,'ge')
	tnd = Default(tnd,[2,2,0],/dtyp)

	ftet = Bragg_angle(ene=ene,crys=fcr,ind=fnd,eta=feta,/rad,/unc)
	stet = Bragg_angle(ene=ene,crys=scr,ind=snd,eta=seta,/rad,/unc)
	ttet = Bragg_angle(ene=ene,crys=tcr,ind=tnd,eta=teta,/rad,/unc)

	fref = Ref_curve(0,ene=ene,crys=fcr,ind=fnd)
	sref = Ref_curve(0,ene=ene,crys=scr,ind=snd)
	tref = Ref_curve(0,ene=ene,crys=tcr,ind=tnd)

	if n_elements(ene) ne 1 then message, 'Scalar energy required!'
	if (One_of(deg,rad) > 0) then amult = 1d else amult = !dpi/180
	case One_of(alp,qzv,chi) of
		0	:	walp = amult*alp
		1	:	begin
			k = 2*!dpi*ene/!srcon.conv
			walp = asin(qzv/(2*k))
		end
		2	:	wchi = amult*chi
		else:	message, 'One of Alpha, Qz or Chi must be given!'
	endcase

	if cfl then begin
		tt0 = 2*(ttet-stet)
		if Isnum(wchi) then walp = asin(sin(wchi)*sin(tt0)) $
		else wchi = asin(sin(walp)/sin(tt0))
	endif else begin
		tt0 = 2*stet
		if Isnum(wchi) then begin
			wchi = - abs(wchi)
			walp = -asin(sin(wchi)*sin(tt0))
		endif else wchi = -asin(sin(walp)/sin(tt0))
	endelse
	if min(finite(wchi)) eq 0 then message, 'Input values out of range!'
	tt = acos(cos(tt0)/cos(walp) < 1)
	res = 0*wchi

	if keyword_set(sho) then begin
		shofl = 1
		prjfl = keyword_set(prj)
		wai = Default(wai,1.)
	endif else shofl = 0
	
	abc0 = ABC_gen(ene,rsig=rsg,asig=asg,ban=1,_extra=_e)
	abct = ABC_prop(abc0,dis=seg[0])
	abct = ABC_refc(abct,ftet,!dpi/2,eta=feta,ref=fref,/rad,/cle)
	abc1 = ABC_refc(abct,ftet,-!dpi/2,eta=feta,ref=fref,/rad,/cle)
	if seg[1] gt 0 then begin
		abct = ABC_prop(abc1,dis=seg[1])
		imat = SVD_invert(abct.amat)
		focl = 1/(1./(wsal-wmil) + imat[3,1]/imat[1,1])
		abct = ABC_foc(abct,focl = [0,focl])
		abc2 = ABC_sler(abct,sler=[0,wsle],/cle)
		abc3 = ABC_prop(abc2,dis=seg[2],/cle)
	endif else abc3 = ABC_prop(abc1,dis=seg[1]+seg[2],/cle)
	iflux = ABC_int(abc3)

	cumfl = keyword_set(cum)
	nang = n_elements(wchi)
	for i = 0, nang - 1 do begin
		last = i eq (nang-1)
		abct = ABC_refc(abc3,stet,wchi[i],eta=seta,ref=sref,/rad,tmat=tmt)
		if cfl then begin
			abct = ABC_prop(abct,dis=seg[3])
			nv= [-cos(2*stet-ttet)*[cos(wchi[i]),sin(wchi[i])],sin(2*stet-ttet)]
			nv = transpose(tmt##nv)
			abct = ABC_refc(abct,nvec=nv,eta=teta,ref=tref,/rad)
			abc4 = ABC_prop(abct,dis=seg[4],/cle)
		endif else abc4 = ABC_prop(abct,dis=seg[3],/cle)
		fflux= ABC_int(abc4)
		res[i] = fflux/iflux
		if keyword_set(sho) then begin
			if prjfl then begin
				whrn = Default(hrn,10.,/dtyp)
				wvrn = Default(vrn,100.,/dtyp)
				ABC_show, abc4, /project, hran= whrn, vran= wvrn, $
				hor= (tt0-tt[i])*cfl, ver= walp[i], /radians, $
				cumul = cumfl*(1+last), _extra=_e
			endif else ABC_show, abc4, cum = cumfl*(1+last), _extra=_e
			if not last then wait, wai
		endif
	endfor

	return, res
end