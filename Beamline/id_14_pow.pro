Function ID_14_pow, loc, foc= foc, hang= han, vang= van, abso= abs, abth= abt,$
	no_abs = nab, air = air, show= sho, _extra=_e

	common id_14_pars, exs, milfac, micfac, elem, eloc, $
	filt, flth, wind, wnth, mircot, mirlen, mirrad, mirser, $
	aper, curap, filf, fils, curcnf, sunfl

	on_error, 1

	if (Streq(Default(loc,'windo'),'windo',3) and not keyword_set(nab)) or $
	(n_elements(abs) gt 0 and n_elements(abt) gt 0) then abfl = 1 else abfl = 0

	nxy = [n_elements(han) > 1,n_elements(van) > 1]
	whan = Default(han,[0.],/dtyp)
	wvan = Default(van,[0.],/dtyp)
	res = reform(fltarr(5,nxy[0],nxy[1]),5,nxy[0],nxy[1])

	for i = 0l, nxy[0]-1 do begin
		for j = 0l, nxy[1] - 1 do begin
			res[0,i,j] = whan[i]
			res[1,i,j] = wvan[j]
			if abfl then res[2,i,j] = ID_14_abs( $
			loc,foc=foc,han=whan[i],van=wvan[j],abs=abs,abt=abt,spo=spt) else $
			res[2,i,j] = ID_14_inc( $
			loc,foc=foc,han=whan[i],van=wvan[j],air = air, spo=spt)
			res[3:4,i,j] = spt
			povar = res[2,i,j]/(res[3,i,j]*res[4,i,j])
			povci = res[2,i,j]/(2*(res[3,i,j]+res[4,i,j]))
			print, i*nxy[1]+j+1,nxy[0]*nxy[1],res[*,i,j], povar, povci, $
			form = '(i4,": ",i4," |",5(f8.3," "),"|",2(f8.2," "))'
			wait, 0.01
		endfor
	endfor

	if abfl and Type(loc) eq 0 then loc = 'windo'
	if Type(loc) eq 7 then begin
		dum = Strmatch_mm(loc,elem,3)
		wloc = eloc[dum]
	endif else wloc = loc

	dist = wloc - eloc[[Strmatch_mm('hmirr',elem),Strmatch_mm('vmirr',elem)]]

	res[0,*,*] = 2*dist[0]*res[0,*,*]
	res[1,*,*] = 2*dist[1]*res[1,*,*]
	if keyword_set(sho) and not Arreq((size(res))[2:3],[1,1]) $
	then Display_mm, reform(res[0:2,*,*]), /auz, /rest, _extra = _e

	return, res
end