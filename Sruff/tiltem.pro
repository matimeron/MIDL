Function tiltem, x, loc, foc

	common id_14_pars, exs, filf, fils, milfac, micfac, aper, elem, eloc, $
		filt, flth, wind, wnth, mircot, mirlen, mirrad, mirser, curap, unafl

	on_error, 1
	mirr = 'hmirr'
	van = 2.7

	if Type(loc) eq 7 then begin
		dum = Strmatch_mm(loc,elem,3)
		if dum ge 0 then wloc = eloc[dum] else message, 'Unknown location!'
	endif else if Isnum(loc) then wloc = loc else message, 'Unknown location!'
	dum = Strmatch_mm(mirr,elem,3)
	dist = wloc - eloc[dum]

;	if Type(foc) eq 7 then begin
;		dum = Strmatch_mm(foc,elem,3)
;		if dum ge 0 then wfoc = eloc[dum] else message, 'Unknown location!'
;	endif else wfoc = Default(foc,wloc)

	n = n_elements(x)

	res = fltarr(6,n)
	res[0,*] = x
	res[1,*] = x/(2*dist)
	han = res[1,*]
	dum = where(x lt 1.,ndum)
	if ndum gt 0 then han[dum] = 0

	for i = 0, n-1 do begin
		id_14, loc,han=han[i],van=van,esize=siz,pow=pow
		res[2:3,i] = siz
		res[4,i] = pow
		tem = bc_tilt(275,siz,pow,kap=0.365,h=0.015,thi=25,/xtilt,val=val)
;		tem = bc_tilt(1175,siz,pow,kap=0.163,h=0.015,thi=25,/xtilt,val=val)
		res[5,i] = tem
		print
		print, n-1, i
		print, res[*,i]
		wait, 0.01
	endfor

	return, res
end






