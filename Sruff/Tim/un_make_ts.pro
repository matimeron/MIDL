Pro UN_make_ts, k, lamb, rgam, nper, nam, rsig, asig, efirst = efi, $
	harmonics = harms, ang_lims = als, npoints = np, gscale = gsca, $
	file = fname, show_progress = shop

;    Units:
;    lamb	-  meters
;    asig	-  miliradians
;    ang_lims	-  miliradians

	common un_consts, hcove, alpha, oovee, ecf
	common output_make, display, m_outfile      ;T.S. 7/3/2003

	on_error, 1
	eps = 2*Toler()
	u = UN_struct(npo_max = nmx, nhar_max = nhmx)

	if n_elements(efi) ne 0 then begin
		efimax = 2*ecf*hcove*rgam*rgam/lamb
		if efi le efimax then begin
			kk = sqrt(2*(efimax/efi - 1))
			;print, 'Calculated K = ', kk   T.S 7/7/2003
			new_k = string('Calculated K = ', kk, form = '(a, f8.5)') ; T.S.
			Widget_control, display, set_value = [new_k, '','Progress', $ ;T.S.
        'harmonic    time elapsed              harmonic     time remaining']; T.S.
		endif else message, 'First harmonic energy cannot exceed ' + $
		string(efimax, form ='(f8.3)') + ' keV'
	endif else kk = k

	unam = Default(nam,'undulator',/strict)
	u = Unpri_sto(u, kk, lamb, rgam, nper, name=unam)
	if not Streq(unam,u.name) then print, 'New name is ' + u.name

	harms = Default(harms, 1 + lindgen(1 + 2*round(3*u.k*(1+0.5*u.k^2))),/dtype)
	if (size(harms))(0) eq 0 then harms = 1 + lindgen(harms > 1)
	u.nh = n_elements(harms) < nhmx
	u.harms(1:u.nh) = harms(0:u.nh-1)

	u = Unsec_sto(u,rsig,asig,ang=als,npo=np,/new,xyg=angs,fun=garr,gsc=gsca)

	acon = 1e-3*u.gamm
	angs = acon*angs
	gtet = sqrt(total(angs^2,1))
	tem = gtet
	tem(u.nxy(0),u.nxy(1)) = 1
	cosphi = reform(angs(0,*,*))/tem
	tem = gtet^2 + 0.5*u.k^2 + 1
	uarr = 0.25*u.k^2/tem
	varr = 2.*u.k*gtet*cosphi/tem

	;defname = getenv('sruff_data') + u.name + '.dat'
	m_out = m_outfile + '.dat'   ;T.S. 7/3/2003
	openw, datun, Default(fname, m_out, /strict), /get_lun, /block
	writeu, datun, u
	point_lun, -datun, off
	udat = assoc(datun,uarr,off)
	udat(0) = uarr

	shof = keyword_set(shop)
	ts = systime(1)
	for n = 1l, u.nh do begin
		if shof and n gt 1 then begin
	       t = systime(1)
;		   print, n - 1, '  done, ', (t - ts)/60, ' min. elapsed ;    ', $ ;T.S. 7/3/2003
;	   	   (u.nh- n+ 1), '  to go, ', (u.nh- n+ 1)*(t- ts)/(60*(n- 1)), $ ;T.S. 7/3/2003
;		   ' min. remaining (est.)', form ='(i3,a,f6.2,a,i3,a,f6.2,a)' ;T.S. 7/3/2003
           ;***********T.S. 7/3/2003***********
           Widget_control, display, get_value = temp

           if n_elements(temp) eq 54 then temp = temp(0:2)

           result = string( n - 1, ' done, ', (t - ts)/60, '  min. elapsed ;    ', $
			(u.nh- n+ 1), '  to go, ', (u.nh- n+ 1)*(t- ts)/(60*(n- 1)), $
			'  min. remaining (est.)', form ='(i3,a,f6.2,a,i3,a,f6.2,a)')
		   Widget_control, display, set_value = [temp, result]
           ;***********T.S. 7/3/2003***********
		endif
		for i = 0l, u.nxy(0) do begin
			for j = 0l, u.nxy(1) do begin
				garr(i,j) = $
				Ug_harm(u.harms(n),uarr(i,j),varr(i,j),u.k,gtet(i,j),eps,/ver)
			endfor
		endfor
		garr(0:u.nxy(0),u.nxy(1)+1:2*u.nxy(1)) = $
			rotate(garr(0:u.nxy(0),0:u.nxy(1)-1),7)
		garr(u.nxy(0)+1:2*u.nxy(0),*) = rotate(garr(0:u.nxy(0)-1,*),5)
		udat(n) = garr
	endfor
	free_lun, datun


	return
end