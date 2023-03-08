Pro Dbounce, d1, d2, refer_ene= rene, range = ran, radians= rad, degrees= deg, $
	_extra = _e

	on_error, 1
	if (One_of(deg,rad) > 0) eq 0 then auni = '(deg)'else auni = '(rad)'
	conv = float(!srcon.conv)

	if d2 lt d1 then message, 'd2 must be bigger than d1!'
	emin = (conv/(2*d1))*(1 + Toler())
	wran = Default(ran,[emin,rene])
	if n_elements(wran) eq 1 then begin
		if wran lt rene then wran = [wran,rene] else wran = [emin,wran]
	endif
	wene = Make_grid(wran,1001)
	kap = wene/rene
	bang = Dbounce_ang(wene,d1,d2,ref=rene,radians=rad,degrees=deg)
	ckap = Dbounce_zfac(d1,d2,ref=rene,status=sta)
	if sta then cene = rene*ckap else cene = ckap

	Plvar_keep, act = 'sav'
	plot, wene, bang, xtit = 'Energy (keV)', ytit = 'Relative angle ' + auni, $
	tit = string(rene,form='("Reference energy = ",f6.3,"keV")'), $
	subtit=string(ckap,cene,form='("!7j!x!lc!n = ",f5.3," ;	E!lc!n = ",f7.3)'),$
	ymar=[6,2], _extra = _e 
	oplot, wene, 0*bang, line = 2
	Plvar_keep, act = 'res'

	return
end