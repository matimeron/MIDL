Function Smiley_fun, x, pars

	; Pars = [eps,K,theta,tau]

	on_error, 1

	typ = Calctype(x,0.)
	wx = Cast(x,5)
	if n_elements(pars) lt 4 then wpars = [pars,0.] else wpars = pars
	fir = wpars[1]*sin(wpars[2])*sin(wx)
	sec = wpars[1]*2*sin(wx/2)^2
	tem = fir*cos(wpars[3]) + sec*sin(wpars[3])
	res = tem/sqrt(tem^2 + (1 + fir*sin(wpars[3]) - sec*cos(wpars[3]))^2) - $
		sin(wpars[0])

	return, Cast(res,typ,typ,/fix)
end