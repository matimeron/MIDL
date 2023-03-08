Function Hirat, cur, dist, files = fil, energy = ene, slice= sli, mharm= mhr, $
	aperture= ape, loban= lbw, hiban= hbw, smear= sme,$
	mirrel= mre, angle= ang, dfac = dfc, show = sho, wait = wai, pack = pac

	on_error, 1

	sfl = keyword_set(sho)
	wfl = keyword_set(wai)
	n = n_elements(fil)
	if n gt 0 then begin
		efl = 0
		if n_elements(ene) eq n then begin
			wene = sli*ene
			efl = 1
		endif else if n_elements(n) ne 0 then message, 'Energy-Files mismatch!'
		if keyword_set(mre) then begin
			if not efl then message, 'Energy input missing!'
			qc = Qcrit(elem=mre)
			q = Make_grid(qc*[0.9,1],101)
			mrfl = 1
		endif else mrfl = 0
		if not (Isnum(lbw) and Isnum(hbw)) then message, 'Missing bandwidths!'
		wsme = Default(sme,1)
		res = fltarr(n)
		for i = 0, n-1 do begin
			Und_slice,cur,dist,file=fil[i],sli=sli,win=0.5*ape,ban=lbw,$
			/har,sme=wsme,tflux=fir
			if wfl then wait, wai
			Und_slice,cur,dist,file=fil[i],sli=mhr*sli,win=0.5*ape,ban=hbw,$
			/har,sme=wsme,tflux=sec
			if wfl then wait, wai
			if mrfl then begin
				if keyword_set(ang) $
				then fac = Reflect(thet=ang,ene=wene[i],ele=mre,dfac=dfc)/ $
				Reflect(thet=ang,ene=mhr*wene[i],ele=mre,dfac=dfc) $
				else fac= max(Reflect(q=q,ene=wene[i],ele=mre,dfac=dfc)/ $
				Reflect(q=mhr*q,ene=mhr*wene[i],ele=mre,dfac=dfc))
			endif else fac = 1.
			if sfl then print, wene[i], fac
			res[i]=sec/fir/fac^2
		endfor
		if keyword_set(pac) and efl then res = transpose([[wene],[res]])
	endif else message, 'File input?!

	return, res
end