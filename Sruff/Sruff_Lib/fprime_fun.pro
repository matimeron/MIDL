Function Fprime_fun, ene, edge = ede, power = pow, hwid = hwd, muvals = muv, $
	esqu = esq, deriv = der, standard = sta

	common fprime_com, elz, wede, wpow, whwd, wmuv 
	on_error, 1

	typ = Calctype(0.,ene)

	if keyword_set(sta) then begin
		res = Abs_coeff(ene>1,elem=elz)
		if keyword_set(esq) then res = ene^2*res
	endif else begin		
		wede = Default(ede,wede)
		wpow = Default(pow,wpow)
		whwd = Default(hwd,whwd)
		wmuv = Default(muv,wmuv)
	
		if keyword_set(esq) then epow = wpow else epow = 2 + wpow
		gen = wede^(2+wpow)/ene^epow
		sfun = 0.5d + atan(ene-wede,whwd)/!dpi
		if keyword_set(der) then tfun = whwd/(!dpi*((ene-wede)^2 + whwd^2))
		lmu = wmuv[0]
		dmu = wmuv[1] - wmuv[0]
		if keyword_set(der) then res = dmu*tfun -epow/ene*(lmu + dmu*sfun) $
		else res = lmu + dmu*sfun
		res = gen*res
	endelse

	return, Cast(res,typ,typ,/fix)
end