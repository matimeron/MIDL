Pro IDC_15he, ene, harmonic = har, cur = cur, slit = sli, _extra = _e

	on_error, 1

	if n_elements(ene) eq 1 then ene = ene[0] $
	else message, 'Scalar energy required'
	if not Isnum(har) then message, 'Harmonic number needed!'

	crys = 'si'
	ind = [[1,1,1],[3,1,1]]
	bw = [1.324e-4,2.773e-5]
	fefl = ene gt 30
	ind = reform(ind[*,fefl])
	bw = bw[fefl]

	mirel = ['Rh','Pt']
	sefl = ene gt 22
	mirel = mirel[sefl]
	miran = 2e-3*(1 < 36./ene)
	mirof = 3

	ape = [2.,1. < 600.*miran]
	dist = 30.

	bethi = 2e-2*2.54

	airel = ['n','o','ar']
	airwei = [.78,.21,.01]
	airden = 1.2e-3
	airthi = 400.

	kapel = ['h','c','n','o']
	kapwei = [0.0264,0.6911,0.0733,0.2092]
	kapden = 1.42
	kapthi = 4e-2*2.54

	if Isnum(cur) then if cur gt 1 then wcur = 1e-3*cur else wcur = cur
	rawflux = Und_flux( ene,per=33,nper=72,$
	har=har,ban=bw,/def,ape=ape,dist=dist,/opt,set=uene,_extra=_e)
	monfac = Ref_curve(0,ene=ene,crys=crys,ind=ind)^2
	mirfac = Mirror(ene,miran,elem=mirel,rough=mirof)^2
	abcoefs = [Abs_coeff(ene,elem='be'), $
			Abs_coeff(ene,elem=airel,wei=airwei,/form,den=airden), $
			Abs_coeff(ene,elem=kapel,wei=kapwei,den=kapden)]
	attfac = exp(-total([bethi,airthi,kapthi]*abcoefs))
	delflux = monfac*mirfac*attfac*rawflux
	
	hirat = Hihar_ratio(ene,uharm=har,/all,per=33,ape=ape,dist=dist, $
	ncrys=2,crys=crys,ind=ind,nmir=2,mirr=mirel,angl=miran,roug=mirof,_extra=_e)
	hirat = FPU_fix(hirat)

	print
	print, har, form = '("	Harmonic	- ",i2)
	print, uene, form = '("	Undulator	- ",f6.3," keV")'
	print, ene, form = '("	Monochromator	- ",f6.3," keV")'
	print, mirel, form = '("	Mirror		- ",a2)'
	print, 1e3*miran, form = '("	Mir. angle	- ",f5.3," mrad")'
	print, delflux, form = '("	Flux		- ",e9.2," phot/sec")'
	print, hirat, form = '("	Hi-har ratio	- ",e9.2)'
	print

	return
end