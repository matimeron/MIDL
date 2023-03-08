Function SBM_span, off, eran, ind = ind, lohi = lohi, full = ful

	on_error, 1

	wind = Default(ind,[1,1,1])
	if n_elements(eran) eq 2 then begin
		elo = min(eran,max=ehi)
		tlo = Bragg_angle(ene=elo,crys='si',ind=wind,/rad)
		thi = Bragg_angle(ene=ehi,crys='si',ind=wind,/rad)
		lohi = off*[1/tan(2*tlo),1/tan(2*thi)]
		res = lohi[1]-lohi[0]
	endif else message, 'ERAN needs two elements!'
	if keyword_set(ful) then res = [lohi,res]

	return, res
end