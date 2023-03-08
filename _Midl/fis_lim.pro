Function Fis_lim, lam, kap, delta = del

	on_error, 1

	typ = Calctype(0.,lam,kap,del,def=4)
	if Isnum(del) then par = [kap,del] else par = [kap]
	n = n_elements(lam)
	res = make_array(n,typ=typ)
	lohi = make_array(2,n,typ=typ)
	lohi[0,*] = exp(-lam*kap)/2
	lohi[1,*] = 1/(lam*kap)
	for i = 0, n_elements(lam)-1 do $
	res[i] = Root('fis_check',lohi[*,i],par = [lam[i],par])

	return, res
end