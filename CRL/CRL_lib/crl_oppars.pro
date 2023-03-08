Pro CRL_oppars, ene, pars = cpr, $
	foclen = focl, dval = dval, mu = mu, afoclen= xfocl, xdval= xdval, xmu= xmu

	on_error, 1

	if n_elements(ene) gt 0 then begin
		k = 2*!pi/float(!srcon.conv)*ene
		if Streq(tag_names(cpr,/str),'crl_pars') then begin
			die = Dielect(ene,elem=cpr.len.ele,dfac=cpr.len.dfc)
			alp = Real_mm(die)
			mu = -1e7*k*Imaginary_mm(die)
			focl = 1e-3*cpr.len.rad/alp
			dval = (k*cpr.len.rof*alp/2)^2
			if cpr.lenset eq 2 then begin
				xdie = Dielect(ene,elem=cpr.xlen.ele,dfac=cpr.xlen.dfc)
				xalp = Real_mm(xdie)
				xmu = -1e7*k*Imaginary_mm(xdie)
				xfocl = 1e-3*cpr.xlen.rad/xalp
				xdval = (k*cpr.xlen.rof*xalp/2)^2
			endif else xfocl = (xdval = (xmu = !null))
		endif else message, 'CRL_PARS type structure required!'
	endif else message, 'Missing energy input!'

	return
end