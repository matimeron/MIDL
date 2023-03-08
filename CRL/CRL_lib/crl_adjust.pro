Pro CRL_adjust, len, rat, wlen = wln, awlen = awln

	on_error, 1

	if rat le 1 then begin
		n = floor(1./rat)
		wln = (awln = 0*len)
		flen = len - floor(len)
		for i = 1, n do begin
			dum = where(flen gt (i-0.5)*rat and flen le ((i+0.5)*rat),/null)
			wln[dum] = i*rat
			awln[dum] = i
		endfor
		dum = where(flen gt (1.+n*rat)/2 and flen le 1,/null)
		wln[dum] = 1
		awln[dum] = 1
		wln = wln + floor(len)
		awln = awln + floor(len)
	endif else message, 'Ratio must be <= 1'

	return
end