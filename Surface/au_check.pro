Pro Au_check, qz = qzv, energy = ekv, element = elm, thick = thi, frac = frc, $
	rough = roff, ures = ures, mres = mres

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

	on_error, 1
	Load_abs_coeffs

	ro = abctab(Elecomp(elm)).ro
	a  = abctab(Elecomp(elm)).a
	rat = 18*ro/a
	wthi = ([thi,thi])[0:1]
	wfrc = Default(frc,Toler(),/dtyp)
	reff = Reflect(q= qzv, ene= ekv, elem= ['h','o',elm,'h','o'], num= [2,1,2],$
		wei= [2,1,1,2,1], /form, den= [1,ro,1], thi= wthi,rough= roff)
	refs = Reflect(q= qzv, ene= ekv, elem= [elm,'h','o'], num= [1,2], $
		wei= [1,2,1], /form, den= [0,1], thi= wthi[0] + wthi[1], rough= roff)
	refu = Reflect(q= qzv, ene= ekv, elem= [elm,'h','o',elm,'h','o'], $
		num= [3,1,2], wei= [wfrc*rat,2*(1-wfrc),(1-wfrc),1,2,1], /form, $
		den= [wfrc*ro+1-wfrc,ro,1], thi= wthi, rough= roff)
	refm = (1 - wfrc)*reff + wfrc*refs

	plot, qzv, refu, /ylog, /nodata
	oplot, qzv, refu, col=!pcol.green
	oplot, qzv, refm, col = !pcol.red

	if arg_present(ures) then ures = transpose([[qzv],[refu],[0*qzv+1]])
	if arg_present(mres) then mres = transpose([[qzv],[refm],[0*qzv+1]])

	return
end



