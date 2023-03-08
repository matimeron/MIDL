Pro NDCD_chi_dpscan, ene, chiran = crn, qzran = qrn, step = stp, $
	length = len, dlength = dln, degrees = deg, radians = rad, $
	p0_del = dp0, q1_del = dq1, q2_del = dq2, q3_del = dq3, $
	precise = pre, no_show = nos, layout = lay, next = nxt, $
	wchi = chi, hoff = hoff, voff = voff, _extra = _e

	on_error, 1

	typ = Calctype(ene,0.)
	phi1 = Bragg_angle(ene=1d*ene,crys='ge',ind=[1,1,1],/rad)
	phi2 = Bragg_angle(ene=1d*ene,crys='ge',ind=[2,2,0],/rad)
	if len le 10 then wlen = 1d3*len else wlen = 1d*len
	if dln le 10 then wdln = 1d3*dln else wdln = 1d*dln

	if (One_of(deg,rad) > 0) eq 0 then begin
		amult = !dpi/180
		uni = ' (deg)'
		dstp = Default(stp,0.1d,/dtyp)
	endif else begin
		amult = 1d
		uni = ' (rad)'
		dstp = Default(stp,0.001d,/dtyp)
	endelse

	who = One_of(crn,qrn,val=wrn)
	case who of
		0	:	begin
					case n_elements(crn) of
						1	:	wchi = amult*crn
						2	:	wchi = amult*Make_grid(crn,dstp,/step)
						else:	message, 'CHIRAN needs 1 or 2 entries!'
					endcase
				end
		1	:	begin
					k = 2*!dpi*ene/!srcon.conv
					sfac = 2*k*sin(2*(phi2-phi1))
					dstp = Default(stp,0.005d,/dtyp)
					case n_elements(qrn) of
						1	:	wqz = qrn
						2	:	wqz = Make_grid(qrn,dstp,/step)
						else:	message, 'QZRAN needs 1 or 2 entries!'
					endcase
					wchi = wqz/sfac
				end
		else:	message, 'Either CHIRAN or QZRAN must be provided!'								
	endcase

	dum = where(wchi ne 0, nchi)
	if nchi gt 0 then begin
		wchi = wchi[dum]
		if who then wqz = wqz[dum]
	endif else message, 'Non-zero Chi/Qz needed!'
	mfl = (nchi gt 1)
	hoff = (voff = dblarr(nchi))

	gtfl = 0
	p0fl = n_elements(dp0) gt 0
	whi = One_of(dq1,dq2,dq3) + 1
	if p0fl and ((whi mod 3) ne 0) then message, $
	'P0_del together with Q1_del or Q2_del not allowed!'
	egtit = ''

	if p0fl then begin
		wdp0 = Mat_proj([0,1d,0],/com)##([Cast(dp0,5),0,0])[0:2]
		str = string(wdp0,form='(f6.3)')
		egtit = $
		'Inc. beam error: ['+strjoin(strcompress(str,/rem),', ')+'] mm'
	endif else wdp0 = [0d,0,0]

	if whi eq 1 then begin
		wdq1 = Mat_proj([0,0,1d],/com)##([Cast(dq1,5),0,0])[0:2]
		str = string(wdq1,form='(f6.3)')
		egtit = $
		'1!ust!n crystal error: ['+strjoin(strcompress(str,/rem),', ')+'] mm'
	endif else wdq1 = [0d,0,0]

	if whi eq 2 then begin
		wdq2 = Mat_proj([0,0,1d],/com)##([Cast(dq2,5),0,0])[0:2]
		str = string(wdq2,form='(f6.3)')
		egtit = $
		'2!und!n crystal error: ['+strjoin(strcompress(str,/rem),', ')+'] mm'
	endif else wdq2 = [0d,0,0]

	if whi eq 3 then begin
		if n_elements(dq3) eq 1 then wdq3 = Cast([0,0,dq3],5) $
		else wdq3 = Mat_proj([0,0,1d])##([Cast(dq3,5),0,0])[0:2]
		str = string(wdq3,form='(f6.3)')
		eegtit = $
		'Surface error: ['+strjoin(strcompress(str,/rem),', ')+'] mm'
		gtfl = p0fl
		if gtfl then egtit = [egtit,eegtit] else egtit = eegtit
	endif else wdq3 = [0d,0,0]

	fpsi = 2*phi1 - phi2
	spsi = 2*(phi2-phi1)
	alp = asin(sin(wchi)*sin(spsi))
	ttet = atan(cos(wchi)*tan(spsi))

	for i = 0, nchi-1 do begin
		prot = Mat_rot(wchi[i],ax=[0,1d,0])
		nrot = transpose(prot)
		pdrot = Mat_rot(ttet[i],ax=[0,0,1d])##Mat_rot(alp[i],ax=[1d,0,0])
		ndrot = transpose(pdrot)

		n1 = [cos(phi1),-sin(phi1),0]
		n2 = [-cos(2*phi1-phi2),sin(2*phi1-phi2),0]
		n3 = prot##[0,0,1d]
		n4 = prot##pdrot##[0,1d,0]

		v0 = prot##[0,1d,0]
		v1 = Mat_refl(n1)##v0
		v2 = Mat_refl(n2)##v1
		v3 = Mat_refl(n3)##v2

		sc01 = Mat_proj(v0,n1,/com)
		sc12 = Mat_proj(v1,n2,/com)
		sc23 = Mat_proj(v2,n3,/com)
		sc34 = Mat_proj(v3,n4,/com)

		q1 = [0d,0,0] + wdq1
		q2 = $
		wlen*sin(2*(phi2-phi1))/sin(2*phi2)*[sin(2*phi1),cos(2*phi1),0] + wdq2
		q3 = prot##(wlen*[0,1d,0] + wdq3)
		q4 = prot##(wlen*[0,1d,0] + wdln*pdrot##[0,1d,0])

		p0 = prot##([0d,0,0] + wdp0)
		p1 = q1 + sc01##(p0-q1)
		p2 = q2 + sc12##(p1-q2)
		p3 = q3 + sc23##(p2-q3)
		p4 = q4 + sc34##(p3-q4)

		off = ndrot##nrot##(p4 - q4)
		hoff[i] = off[0]
		voff[i] = off[2]
	endfor
	hoff[where(abs(hoff) lt Toler(),/null)] = 0d
	voff[where(abs(voff) lt Toler(),/null)] = 0d

	chi = Cast(wchi/amult,typ,typ)

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
			ftit = 'Detector, vertical offset'
			stit = 'Detector, horizontal offset'
			if who then begin
				pvar = wqz
				xtit = 'Q!dz!n (A!u-1!n)'
			endif else begin
				pvar = chi
				xtit = '!7v!x' + uni
			endelse
			ytit = 'offset (mm)'
			gtit = string(ene,len,dln,$
			form='(f5.2," keV; Len =",f6.1,"mm, Dlen =",f6.1, "mm" )')
			gtit = [gtit,egtit]
			pcol = !pcol.dred
			xlab = replicate(0.5,2+p0fl)

			Plvar_keep, act = 'sav'
			!x.margin = [11,3]
			window, dwin, xsi = xysi[0], ysi = xysi[1]
			!p.region = pp[*,0]
			plot, pvar, voff, /nodata, /ynoz, xstyle= 1, $
				tit= ftit, xtit= xtit, ytit=ytit, _extra=_e
			oplot, pvar, voff, thi=2, col=pcol
			!p.region = pp[*,1]
			plot, pvar, hoff, /nodata, /ynoz, xstyle= 1, /noerase, $
				tit= stit, xtit= xtit, ytit=ytit, _extra=_e
			oplot, pvar, hoff, thi=2, col=pcol
			dum = Wherinstruct('chars',_e)
			if dum ge 0 then _e.(dum) = 1.2*_e.(dum)
			Labels, xlab,ylab,gtit, align= 0.5, /normal, charsize=1.5, _extra=_e
			Plvar_keep, act = 'res'
		endif else begin
			if keyword_set(pre) then pform='f11.6," mm")' $
			else pform='f9.4," mm")'
			print
			print, voff, form= '("	Detector, vertical offset	= ",' + pform
			print, hoff, form= '("	Detector, horizontal offset	= ",' + pform
			print
		endelse
	endif

	hoff = Cast(hoff,typ,typ)
	voff = Cast(voff,typ,typ)

	return
end