Pro Radcool, pc, xc, xm, eps = eps, corr_coef = corf,  count = cnt, _extra = _e

	on_error, 1

	wxc = Cast(xc,4)
	case n_elements(xm) of
		0	: 	wxm = [-wxc,wxc]
		1	:	begin
					wxm = xm > wxc
					wxm = [-wxm,wxm]
				end
		else:	begin
					wxm = (Sign(xm)*(abs(xm) > wxc))[0:1]
					if wxm[0] gt 0 then message, 'Low limit cannot be positive!'
				end
	endcase
	span = total(abs(wxm))
	rat = 2*wxc*pc/span

;	cnf = 0
	for k = 0, 1 do begin
		if k then spl = Splin_coeffs(xev,yres)
		xev = Make_grid(wxm,2^(2*k+1)*(round(span/(2*wxc)) > 256)+1,dim=dim)
		if k then yin = Splin_eval(xev,spl) $
		else yin = (-Rhfun(xev,[wxc,pc]))^(1./4)
		eps = sqrt(Toler(/double))*2.^(-3-2*k)
		yres = ODE_relax(xev,yin,eps,lo=[1.,0,0],hi=[1.,0,0],try=3, $
			gfun='rgfun',hfun='rhfun',hpar=[wxc,pc],corr=corf,/non,iter=iter,$
			_extra=_e)
		mult = (span*(1 + rat)/Integ(xev,yres^4,/val))^(1./4)
		yres = mult*yres
;		cnf = 1
		if cnt then print, dim, iter
	endfor

	plot, xev, (-Rhfun(xev,[wxc,pc]))^(1./4), /ynoz, _extra = _e
	oplot, xev, yres

	return
end