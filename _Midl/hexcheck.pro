Function hexcheck, k, show = sho

	on_error, 1

	kk = Default(k,1l,/dtyp) > 1l
	ll = long(Make_grid([-((kk-1)/2),kk],1l,/step,dim=len))
	lal = lindgen(kk + (kk+1)/2) - (kk-1)/2
	print, Arreq(ll, lal)

	res = kk^2 + kk*ll + ll^2
	kk = replicate(kk,len)

	if keyword_set(sho) then tabulate, kk, ll, res, head = ['K','L','R^2']

	return, res
end
