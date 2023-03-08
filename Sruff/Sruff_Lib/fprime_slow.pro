Function Fprime_slow, ene, element = ele, edge = edg, hwid = hwi, delta = del, $
	_extra = _e

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam
	common fprime_com, elz, ede, pow, hwd, muv

	on_error, 1
	Load_abs_coeffs

	kern = 'Fprime_kern'
	edlis = ['','K ','L1','L2','L3','M1','M2','M3','M4','M5',$
		'N1','N2','N3','N4','N5','N6','N7']
	eps = 8*Toler()
	itol = 1d-4
	gcoe = 1d-4/(!dpi*!srcon.re*!srcon.hc*!srcon.scal*!pcon.na)
	typ = Calctype(0.,ene)

	case n_elements(ele) of
		0	:	message, 'Missing input!'
		1	:	begin
					if Type(ele) eq 7 then begin
						slen = strlen(ele)
						if slen le 2 then comp = abctab.csym $
						else comp = abctab.name
						elz = Strmatch_mm(ele,comp,slen>2,/all,num=num)
						case num of
							0	:	message, 'Not found!'
							1	:	elz = elz[0]
							else:	message, 'Not unique!
						endcase
					endif else begin
						elz = where(abctab.z eq round(ele))
						if elz lt 0 then message, 'Not in table!'
					endelse
					eldat = abctab[elz]
				end
		else:	message, 'Only scalar inputs accepted!'
	endcase

	eind = Strmatch_mm(edg,edlis,strlen(edg))
	if eind le eldat.edlen then begin
		ninf = 0
		eind = eldat.edlen-eind+1
		edgpars = eldat.edtab[eind,*]
		bef = [0.,eldat.edtab[0:eind-1,0]]
		if eind lt eldat.edlen then begin
			aft = [eldat.edtab[eind+1:eldat.edlen,0],500.]
			ninf = 1
		endif else aft = 500.
	endif else message, 'Nonexistant or not available Edge!'

	ede = edgpars[0]
	pow = edgpars[2]
	muv = Abs_coeff(ede*[1-eps,1],elem=ele)
	hwd = hwi
	wdel = Default(del,hwi/2d,/dtyp)

	dum = where(ene ge bef[-1] and ene lt aft[0], nen)
	if nen gt 0 then begin
		wen= ene[dum]
		res = 0.*wen
	endif else message,'No input energy within range!'

	for i = 0, n_elements(bef)-2 do begin
		for j = 0, nen-1 do begin
			eve = wen[j]
			res[j] = Romberg( $
			kern,[bef[i],(1-eps)*bef[i+1]],eval=eve,/stan,/rel,/try,_extra=_e)
		endfor
	endfor

	if ninf then begin
		for i = 0, n_elements(aft)-2 do begin
			for j = 0, nen-1 do begin
				eve = wen[j]
				res[j] = res[j] + Romberg(kern,[aft[i],(1-eps)*aft[i+1]],$
				eval=eve,/stan,/rel,/try,_extra=_e)
			endfor
		endfor	
	endif

	bbef = bef[-1]
	aaft = aft[0]
	lo = where(wen lt (ede-wdel),nlo)
	md = where(abs(wen-ede) le wdel,nmd)
	hi = where(wen gt (ede+wdel),nhi)

	for i = 0, nlo-1 do begin
		eve = wen[lo[i]]
		fir = Romberg(kern,[bbef,eve-wdel],itol,eval=eve,/rel,/try,_extra=_e)
		sec = Romberg(kern,[eve+wdel,ede],itol,eval=eve,/rel,/try,_extra=_e)
		if ninf then $ 
		thi = Romberg(kern,[ede,aaft],itol,eval=eve,/rel,/try,_extra=_e) $
		else thi = Romberg(kern,ede,itol,eval=eve,/infi,/rel,/try,_extra=_e)
		res[lo[i]] = res[lo[i]] + fir + sec + thi
	endfor

	for i = 0, nmd-1 do begin
		eve = wen[md[i]]
		fir = Romberg(kern,[bbef,eve-wdel],itol,eval=eve,/rel,/try,_extra=_e)
		if ninf then thi = Romberg( $
		kern,[eve+wdel,aaft],itol,eval=eve,/rel,/try,_extra=_e) else $
		thi = Romberg(kern,eve+wdel,itol,eval=eve,/infi,/rel,/try,_extra=_e)
		res[md[i]] = res[md[i]] + fir + thi
	endfor

	for i = 0, nhi-1 do begin
		eve = wen[hi[i]]
		fir = Romberg(kern,[bbef,ede],itol,eval=eve,/rel,/try,_extra=_e)
		sec = Romberg(kern,[ede,eve-wdel],itol,eval=eve,/rel,/try,_extra=_e)
		if ninf then thi= Romberg( $
		kern,[eve+wdel,aaft],itol,eval=eve,/rel,/try,_extra=_e) else $
		thi = Romberg(kern,eve+wdel,itol,eval=eve,/infi,/rel,/try,_extra=_e)
		res[hi[i]] = res[hi[i]] + fir + sec + thi
	endfor

	coe = gcoe*eldat.a/eldat.ro
	cor = (Fprime_fun(wen,/esq)- 2*wen*Fprime_fun(wen,/esq,/der))*wdel/(2*wen^2)
	res = coe*(res + cor)

	return, Cast(res,typ,typ,/fix)
end