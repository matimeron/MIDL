Pro PD_XRE, snum, verify= ver, slit= sli, back_ang= bag, left= lef, right= rig,$
	center = cnt, locate = lct, free = fre, tolerance = tol, $
	snorm= snm, vnorm= vnm, result = res, _extra= _e

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum,/pil,par_con=['Det_th'],nsc=nsc,lis=lis,_extra=_e
	res = fltarr(3,nsc)
	sqz = fltarr(nsc)
	for i = 0, nsc-1 do begin
		res[*,i]= Scan_PD_XR(lis[i],slit=sli,back=bag,left=lef,right=rig,$
		ver=ver,/sum,/glob,cen=wcnt,loc=wlct,fre=fre,tol=tol,tit=stit,_extra=_e)
		sqz[i] = Scan_column(lis[i],'l',/const)
	endfor
	ene = float(!srcon.conv/Scan_field_read(lis,'lam'))
	res[0,*] = ene
	res = Scan_sort(res,sord=s)
	slis = lis[s]
	qz = total(sqz)/nsc
	if max(abs(sqz-qz)) gt 5e-4 then message, 'Qz not constant!', /con
	if nsc eq 1 then tit=stit else tit=fildat.name+ ' S# '+ Range_comp(slis)

	Scan_show, res, xtit = 'E (keV)', ytit = 'a.u.', ymar = [5,3], tit = tit, $
	subtit = string(qz,form='("Q!dz!n = ",f5.3," (A!e-1!n)")'),_extra = _e
	return
end