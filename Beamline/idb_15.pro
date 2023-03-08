Pro IDB_15, ene, cur = cur, slit = sli, focsize = foc, _extra = _e

	on_error, 1

	e13 = 12.

	ape = [2.,1.]
	dist = 27.5

	crys = 'si'
	ind = [1,1,1]
	bw = 1.324e-4

	mirel = ['Si','Rh','Pt']
	miren = [12,22]
	miran = 2e-3
	mirof = 3

	bethi = 2e-2*2.54

	airel = ['n','o','ar']
	airwei = [.78,.21,.01]
	airden = 1.2e-3
	airthi = 240.

	kapel = ['h','c','n','o']
	kapwei = [0.0264,0.6911,0.0733,0.2092]
	kapden = 1.42
	kapthi = 3e-2*2.54

	sdist = 51.5
	dsli = [0.1,0.1]
	if Isnum(sli) then begin
		if n_elements(sli) eq 2 then begin
			if max(sli) ge 1 then wsli = 1e-3*sli else wsli = sli
		endif else message, 'Slit needs two elements!'
	endif else wsli = dsli

	dfoc = 0.08
	if Isnum(foc) then begin
		if foc gt 1 then wfoc = 1e-3*foc else wfoc = foc
	endif else wfoc = dfoc
	wsiz = wfoc/sqrt(alog(256))

	if n_elements(ene) eq 1 then ene = ene[0] $
	else message, 'Scalar energy required'
	if ene le e13 then har = 1 else har = 3
	if ene le miren[0] then mirel = mirel[0] $
	else if ene le miren[1] then mirel = mirel[1] $
	else mirel = mirel[2]

	uene = Und_ecorr(ene,har=har,nper=72,/def,fac=ecofac,_extra=_e)
	monfac = Ref_curve(0,ene=ene,crys=crys,ind=ind)^2
	mirfac = Mirror(ene,miran,elem=mirel,rough=mirof)^2
	abcoefs = [Abs_coeff(ene,elem='be'), $
			Abs_coeff(ene,elem=airel,wei=airwei,/form,den=airden), $
			Abs_coeff(ene,elem=kapel,wei=kapwei,den=kapden)]
	attfac = exp(-total([bethi,airthi,kapthi]*abcoefs))

	if Isnum(cur) then if cur gt 1 then wcur = 1e-3*cur else wcur = cur
	rawflux = Und_flux(uene,$
	per=33,nper=72,har=har,ban=bw,cur=wcur,/def,ape=ape,dist=dist,_extra=_e)
	delflux = ecofac*monfac*mirfac*attfac*rawflux

	bsiz = Und_beamsize(ene,/def,dist=sdist,_extra=_e)
	bsiz[1] = wsiz
	arg = wsli/(sqrt(8)*bsiz)
	samflux = Errorf_mm(arg[0])*Errorf_mm(arg[1])*delflux

	print
	print, har, form = '("	Harmonic	- ",i1)
	print, uene, form = '("	Undulator	- ",f6.3," keV")'
	print, ene, form = '("	Monochromator	- ",f6.3," keV")'
	print, mirel, form = '("	Mirror		- ",a2)'
	print, delflux, form = '("	Station flux	= ",e9.2," phot/sec")'
	print, samflux, form = '("	Sample flux	= ",e9.2," phot/sec")'
	print

	return
end