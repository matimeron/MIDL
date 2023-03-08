Pro GID_pars, snum, l4 = l4, l5 = l5, s1h = s1, s1v = s1v, s4 = s4, s5 = s5, $
	alpha= alp, dth = phi, _extra = _e

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if Isnum(snum) then begin
		Spec_file_check, snum, /sing, /pildet		
		cur = fildat.scan[snum]
		l5 = Default(l5,cur.pdist,/dtyp)
		l4 = Default(l4,l5 - cur.pdsdist,/dtyp)
		s1v = Default(s1v,mean(cur.sl_val[0:1,1]),/dtyp)
		s1 = Default(s1,mean(cur.sl_val[2:3,1]),/dtyp)
		s4 = Default(s4,mean(cur.sl_val[2:3,4]),/dtyp)
		s5 = Default(s5,mean(cur.sl_val[2:3,5]),/dtyp)
		ralp = !dtor*Default(alp,(cur.angs)[0],/dtyp)
		rphi = !dtor*Default(phi,0.,/dtyp)
	endif

	lh = s1v/sin(ralp)
	Box, 1.6*[-lh,lh], 1.6*[-s1,s1], /bord
	Rectan, xli=[-lh,lh], yli=[-s1,s1], shap = shap
	polyfill, shap, /line_fill, thick = 1, col=!pcol.cyan, orient=-29

	dv = (l4*s5 + l5*s4)/sqrt((l5-l4)^2 + (s4+s5)^2)
	psi = atan(s4+s5,l5-l4)
	omg = asin(s1/sqrt(s1^2 + lh^2))
	ksi = asin(dv/sqrt(s1^2 + lh^2))

	cang = fltarr(3)
	cang[0] = (s1 + s4)/l4 < (s1 + s5)/l5
	cang[1] = ksi - psi - omg
	cang[2] = ksi + psi + omg	

	pang = string(!radeg*cang,form='(" dth =  	",f6.3," (deg)")')
	print
	print, '	Direct beam region up to', pang[0]
	if cang[1] gt cang[0] then print, '	Full footprint view up to', pang[1] $
		else print, '	Full footprint view not available'
	print, '	Inverse sine scaling above', pang[2]   
	print

	r = [[cos(rphi),-sin(rphi)],[sin(rphi),cos(rphi)]]
	cshap = Shape_edge(shap,[[reform(r##[-l5,s5])],[reform(r##[-l4,-s4])]],/seg)
	cshap= Shape_edge(cshap,[[reform(r##[-l4,s4])],[reform(r##[-l5,-s5])]],/seg)
	plots, cshap, line = 2
	polyfill, cshap, /line_fill, thick = 1, col=!pcol.pink, orient=-31

	nshap = Shape_edge(shap,[[-s5,-l5],[s4,-l4]],/seg)
	nshap = Shape_edge(nshap,[[-s4,-l4],[s5,-l5]],/seg)

	rat = Shape_area(cshap)/Shape_area(nshap)

	print, rat, rat*sin(rphi)

	return
end