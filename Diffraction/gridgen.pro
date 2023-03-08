Function Gridgen, len, amp, step = ste, correct = cor, sum = sum

	wlen = Default(len,1l,/dtyp)
	wamp = Default(amp, 0.,/dtyp)
	wste = Default(ste,1.,/dtyp)

	res = wste*findgen(wlen) + wamp*Rancor(sed,wlen,_extra=_e)
	res = res(sort(res))
	if keyword_set(cor) and wlen gt 1 then begin
		res = res - res[0]
		res = res - (res[wlen-1]/(wlen-1) - wste)*findgen(wlen)
	endif

	return, res
end