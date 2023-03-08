Function Spread, r, mud = mud, stat = sta, reflect = ref, _extra = _e

	on_error, 1

	res = 0*r
	sta = fix(res)
	refl = keyword_set(ref)

	for i = 0l, n_elements(res) - 1 do begin
		res[i] = Romberg('spread_fun',[0,1],rad=r[i],mud=mud,stat=s,_extra=_e)
		sta[i] = s
		if refl then begin
			res[i] = (Romberg('spread_fun',[0,1],rad=r[i],mud=mud,stat=s,$
				/pos,_extra=_e) + res[i])/2
			sta[i] = sta[i] and s
		endif 
	endfor

	return, res
end