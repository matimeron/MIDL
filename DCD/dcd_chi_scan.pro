Pro DCD_chi_scan, ene, chiran=crn,step=stp,length=len, degrees=deg,radians=rad,$
	p0_del = dp0, q1_del = dq1, q2_del = dq2, q3_del = dq3, $
	precise = pre, no_show = nos, layout = lay, next = nxt,$
	wchi = chi, toff = toff, loff = loff, _extra = _e

	on_error, 1

	typ = Calctype(ene,0.)
	phi1 = Bragg_angle(ene=1d*ene,crys='ge',ind=[1,1,1],/rad)
	phi2 = Bragg_angle(ene=1d*ene,crys='ge',ind=[2,2,0],/rad)
	if len le 10 then wlen = 1d3*len else wlen = 1d*len
	if (One_of(deg,rad) > 0) eq 0 then begin
		amult = !dpi/180
		uni = ' (deg)'
		dstp = Default(stp,0.1d,/dtyp)
	endif else begin
		amult = 1d
		uni = ' (rad)'
		dstp = Default(stp,0.001d,/dtyp)
	endelse

	case n_elements(crn) of
		1	:	wchi = amult*crn
		2	:	wchi = amult*Make_grid(crn,dstp,/step)
		else:	message, 'CHIRAN needs 1 or 2 entries!'
	endcase
	nchi = n_elements(wchi)
	mfl = (nchi gt 1)
	toff = (loff = dblarr(nchi))

	xyproj = Mat_proj([0,0,1d],/com)
	gtfl = 0
	p0fl = n_elements(dp0) gt 0
	whi = One_of(dq1,dq2,dq3) + 1
	if p0fl and ((whi mod 3) ne 0) then message, $
	'P0_del together with Q1_del or Q2_del not allowed!'
	egtit = ''

	if p0fl then begin
		wdp0 = Mat_proj([0,1d,0],/com)##([Cast(dp0,5),0,0])[0:2]
		str = string(wdp0,form='(f6.3)')
		egtit = 'Inc. beam error: [' + strjoin(str,', ') + '] mm'
	endif else wdp0 = [0d,0,0]

	if whi eq 1 then begin
		wdq1 = xyproj##([Cast(dq1,5),0,0])[0:2]
		str = string(wdq1,form='(f6.3)')
		egtit = '1!ust!n crystal error: [' + strjoin(str,', ') + '] mm'
	endif else wdq1 = [0d,0,0]

	if whi eq 2 then begin
		wdq2 = xyproj##([Cast(dq2,5),0,0])[0:2]
		str = string(wdq2,form='(f6.3)')
		egtit = '2!und!n crystal error: [' + strjoin(str,', ') + '] mm'
	endif else wdq2 = [0d,0,0]

	if whi eq 3 then begin
		wdq3 = Mat_proj([0,0,1d])##([Cast(dq3,5),0,0])[0:2]
		str = string(wdq3,form='(f6.3)')
		eegtit = 'Surface error: [' + strjoin(str,', ') + '] mm'
		gtfl = p0fl
		if gtfl then egtit = [egtit,eegtit] else egtit = eegtit
	endif else wdq3 = [0d,0,0]

	for i = 0, nchi-1 do begin
		prot = Mat_rot(wchi[i],ax=[0,1,0])
		nrot = transpose(prot)

		n1 = [cos(phi1),-sin(phi1),0]
		n2 = [-cos(2*phi1-phi2),sin(2*phi1-phi2),0]
		n3 = prot##[0,0,1d]

		v0 = prot##[0,1d,0]
		v1 = Mat_refl(n1)##v0
		v2 = Mat_refl(n2)##v1

		s01 = Mat_proj(v0,n1,/com)
		s12 = Mat_proj(v1,n2,/com)
		s23 = Mat_proj(v2,n3,/com)

		q1 = [0d,0,0] + wdq1
		q2 = $
		wlen*sin(2*(phi2-phi1))/sin(2*phi2)*[sin(2*phi1),cos(2*phi1),0] + wdq2
		q3 = prot##(wlen*[0,1d,0] + wdq3)

		p0 = prot##([0d,0,0] + wdp0)
		p1 = q1 + s01##(p0-q1)
		p2 = q2 + s12##(p1-q2)
		p3 = q3 + s23##(p2-q3)

		off = nrot##(p3 - q3)
		yy = [-cos(wchi[i])*sin(2*(phi2-phi1)),cos(2*(phi2-phi1)),0]
		yy = yy/Vnorm(yy)
		xx = crossp(yy,[0,0,1d])
		toff[i] = Vinp(off,xx)
		toff[where(abs(toff) lt Toler(),/null)] = 0d
		loff[i] = Vinp(off,yy)	
		loff[where(abs(loff) lt Toler(),/null)] = 0d
	endfor

	chi = Cast(wchi/amult,typ,typ)
	toff = Cast(toff,typ,typ)
	loff = Cast(loff,typ,typ)

	if not keyword_set(nos) then begin
		if mfl then begin
			bsiz = 32
			posib = ['hor','ver']
			wlay = abs(Strmatch_mm(lay,posib,1))
			case wlay of
				0	:	begin
							xysi = [28,12+gtfl]*bsiz
							phi = 0.86 - 0.06*gtfl
							ylab = ([0.95,0.89,0.83])[0:1+gtfl]
							pp = [[0,0,0.5,phi],[0.5,0,1,phi]]
						end
				1	:	begin
							xysi = [16,24]*bsiz
							phi = 0.92 - 0.02*gtfl
							ylab = ([0.98,0.95,0.92])[0:1+gtfl]
							pp = [[0,phi/2,1,phi],[0,0,1,phi/2]]
						end
			endcase
			dwin = (!d.window + keyword_set(nxt)) > 0
			ftit = 'Footprint, longitudinal error'
			stit = 'Footprint, transverse error'
			xtit = '!7v!x' + uni
			ytit = 'Error (mm)'
			gtit = string(ene,len,form='(f5.2," keV; Lenght = ",f7.2," mm")')
			gtit = [gtit,egtit]
			pcol = !pcol.dred
			xlab = replicate(0.5,2+p0fl)

			Plvar_keep, act = 'sav'
			!x.margin = [11,3]
			window, dwin, xsi = xysi[0], ysi = xysi[1]
			!p.region = pp[*,0]
			plot, chi, loff, /nodata, /ynoz, xstyle= 1, $
				tit= ftit, xtit= xtit, ytit=ytit, _extra=_e
			oplot, chi, loff, thi=2, col=pcol
			!p.region = pp[*,1]
			plot, chi, toff, /nodata, /ynoz, xstyle= 1, /noerase, $
				tit= stit, xtit= xtit, ytit=ytit, _extra=_e
			oplot, chi, toff, thi=2, col=pcol
			dum = Wherinstruct('chars',_e)
			if dum ge 0 then _e.(dum) = 1.2*_e.(dum)
			Labels, xlab,ylab,gtit, align= 0.5, /normal, charsize=1.5, _extra=_e
			Plvar_keep, act = 'res'
		endif else begin
			if keyword_set(pre) then pform='f11.6," mm")' $
			else pform='f9.4," mm")'
			print
			print, loff, form= '("	Longitudinal error	= ",' + pform
			print, toff, form= '("	Transverse error	= ",' + pform
			print
		endelse
	endif

	return
end