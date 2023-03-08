Function Irina_opt, ene, slitloc = sll, optloc = opl, samloc = sml, $
	aperture= ape, diffract= dif, slope_error = sle, $
	fwhm= fwhm, ang_size = asz, tran = trn, show_virtual = shv, _extra = _e

	on_error, 1

	if keyword_set(fwhm) then mult = 1e3*sqrt(alog(256)) else mult = 1e3
	if n_elements(ene) eq 1 then begin
		typ = Calctype(ene,0.)
		wene = Cast(ene[0],5)
		if Default(dif,1) then dene = wene else dene = []
	endif else message, 'Energy must be a scalar!'

	sol = 1.25
	sll = Default(sll,55.,/dtyp)
	sml = Default(sml,60.,/dtyp)
	if sll ge sml then message, 'Slit must precede sample!'
	if n_elements(opl) eq 1 then begin
		if opl le sll or opl ge sml then $
		message, 'Optics must be between slit and sample!'
	endif else message, 'Scalar optics location needed!'

	if n_elements(ape) eq 2 then w = ape/(sqrt(4*!dpi)) $
	else message, 'Aperture needs two values, [horizontal, vertical] !'
	wsle = 1e-3*Default(sle,[0,0])
	if n_elements(wsle) eq 1 then wsle = [wsle,wsle] $
	else if n_elements(wsle) ne 2 then message, '1 or 2 values for slope error!'

	rsg = Und_beamsize(wene,/def,ang=asg,_extra=_e)
	pqri = [rsg^2*asg^2,rsg^2,0*asg^2]
	pqrj = PQR_prop(pqri,l=sll-sol)
	pqrk = PQR_prop(pqrj,ene=dene,w=w,l=opl-sll,tsca='sig',tsq=tsq)
	pqr0 = PQR_prop(pqrk,s=wsle,rpsq= psq0,rqsq= qsq0,rr= r0)

	if keyword_set(shv) then begin
		thor = Opvar_conv(pqr=pqr0[[0,2,4]])
		tver = Opvar_conv(pqr=pqr0[[1,3,5]])
		vrsg = sqrt([thor[0],tver[0]])
		vloc = opl - [thor[2],tver[2]]
		print, vrsg
		print, vloc
	endif

	f = 1d/(1./(sml-opl) + r0/qsq0)
	pqr1 = PQR_prop(pqr0,f=f,l=sml-opl,rqsq= qsq1)
	asz = dblarr(2)
	asz[0] = (Opvar_conv(pqr = pqr1[[0,2,4]]))[1]
	asz[1] = (Opvar_conv(pqr = pqr1[[1,3,5]]))[1]
	asz = mult*Cast(sqrt(asz),typ,typ)
	trn = sqrt(tsq[0]*tsq[1])
	res = mult*Cast(sqrt(qsq1),typ,typ)

	return, res
end