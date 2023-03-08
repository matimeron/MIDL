Function CAB_eval, a = a, b = b, c = c, noise = nos, progress = prg, _extra=_e

	on_error, 1

	if Codims(a,b,c,/same,dim=dim) then begin
		typ = Calctype(0.,a,b,c)
		arr = make_array(3,dim,typ=typ)
		arr[0,*] = 1
		arr[1,*] = a
		arr[2,*] = b
		cc = c*(1 + Default(nos,0.,/dtyp)*randomn(s,n_elements(c)))
		tres = (Solve_linsys(arr,cc,/row,/svd))[0:1]
		qarr = arr
		qarr[0,*] = cc
		res1 = fgh_ext('cab1_fun',x_ini=10*tres,par=qarr,prog=prg,_extra=_e)
		res2 = fgh_ext('cab2_fun',x_ini=10*tres,par=qarr,prog=prg,_extra=_e)
		print
		print, '	Linear result:	', tres
		print, '	Residual		', cab_fun(tres,qarr)
		print
		print, '	Non-linear fit 1', res1
		print, '	Residual		', cab1_fun(res1,qarr)
		print
		print, '	Non-linear fit 2', res2
		print, '	Residual		', cab2_fun(res2,qarr)
		print
	endif else message, 'Inconsistent inputs!'

	return, res2
end