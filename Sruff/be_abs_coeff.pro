Function Be_abs_coeff, e, pure = pur

	on_error, 1

	bedens = 1.848
	posib = ['pure', 'if-1', 'pf-60']
	npur = Strmatch_mm(pur,posib,2,/nosub) > 0

	if Isnum(e) then begin
		fname = getenv('sruff_data') + 'be_foil.dat'
		dum = Rascline(fname,lines=data)
		nel = n_elements(data)

		ele = strarr(nel)
		wei = fltarr(nel)
		for i = 0, nel-1 do begin
			dum = Strparse_mm(data[i],' 	',lis)
			ele[i] = lis[0]
			wei[i] = lis[npur+1]
		endfor
		res = Abs_coeff(e,ele=ele,wei=wei,den=bedens)
	endif else message, 'Missing energy input!'

	return, res
end