Function Smear, arr, u, dist, set_min = sem, smearstring = smest

	common un_consts, hcove, alpha, oovee, ecf

	on_error, 1

	sigsq = u.rsig^2 + (dist*u.asig)^2
	steps = dist*[u.ganx[1] - u.ganx[0], u.gany[1] - u.gany[0]]

	n = ceil(sqrt(4*!pi*sigsq)/steps) < u.nxy
	tem = Make_grid([-1,1]#(steps*n), 2*n+1)
	sigsq = sigsq > abs(steps^2/(2*(machar(double=Isnum(arr,/double))).minexp))

	kern = exp(-.5*reform(tem[0,*,*]^2/sigsq[0] + tem[1,*,*]^2/sigsq[1]))
	kern = kern/total(kern)

	smest = 'Corrected for emittance'

	if keyword_set(sem) then res = Convol_mm(arr,kern,edge_val=min(arr)) $
	else res = Convol_mm(arr,kern,/edge_trunc)

	return, res
end