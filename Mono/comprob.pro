Function Comprob, eps, zet

	on_error, 1

	if Codims(eps,zet,dim=dim) then begin
		typ = Calctype(0.,eps,zet,def=4)
		weps = Cast(eps,5)
		res = 0*weps + 1
		dum = where(weps gt 0, ndum)
		if ndum gt 0 then res[dum] = 1 - weps[dum]*alog(1 + 1./weps[dum])
		np = n_elements(eps)
		nz = n_elements(zet)
		if nz gt 0 then begin
			wzet = Cast(zet,5)
			if nz gt 1 then begin
				if np eq nz then weps = weps $
				else weps = replicate(weps,nz)
			endif else begin
				weps = weps
				if np eq 1 then wzet = wzet else wzet = replicate(wzet,np)
			endelse
			eres = 0*wzet + 1.
			dum = where(weps gt 0 and wzet eq 0, ndum)
			if ndum gt 0 then eres[dum] = 1 - weps[dum]*alog(1 + 1./weps[dum])
			dum = where(weps gt 0 and wzet gt 0, ndum)
			if ndum gt 0 then begin
				ez = (weps*wzet)[dum]
				eres[dum] = exp(-wzet)*(expint(2,ez,/doub) - weps[dum]* $
				expint(1,ez,/doub)) + weps[dum]*expint(1,ez+wzet[dum],/doub)
			endif
			res = res - eres
		endif
	endif else message, 'Missing or inconsistent input!'

	return, Cast(res,typ,typ,/fix)
end