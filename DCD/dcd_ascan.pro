Pro DCD_ascan, ene, chiran = crn, qzran = qrn, step = stp, $
	length = len, dlength = dln, degrees = deg, radians = rad, $
	v0_del = dv0, ch1_del = dc1, ch2_del = dc2, q3_del = dq3, balanced = bal, $
	precise = pre, no_show = nos, layout = lay, angerr = anr, next = nxt, $
	wchi = chi, toff = toff, loff = loff, _extra = _e

	on_error, 1

	typ = Calctype(ene,0.)
	phi1 = Bragg_angle(ene=1d*ene,crys='ge',ind=[1,1,1],/rad)
	phi2 = Bragg_angle(ene=1d*ene,crys='ge',ind=[2,2,0],/rad)
	if len le 10 then wlen = 1d3*len else wlen = 1d*len
	if Isnum(dln) then begin
		if dln le 10 then wdln = 1d3*dln else wdln = 1d*dln
		defl = 1
		anfl = 0
	endif else begin
		defl = 0
		anfl = keyword_set(anr)
	endelse

	if (One_of(deg,rad) > 0) eq 0 then begin
		amult = !dpi/180
		uni = ' (deg)'
		suni = ' deg'
		dstp = Default(stp,0.1d,/dtyp)
		frm = '(f6.3)'
	endif else begin
		amult = 1d
		uni = ' (rad)'
		suni = ' rad'
		dstp = Default(stp,0.001d,/dtyp)
		frm = '(g8.3)'
	endelse

	who = One_of(crn,qrn)
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
	toff = (loff = (dalp = (dttet = dblarr(nchi))))

	gtfl = 0
	whi = One_of(dv0,dc1,dc2)
	egtit = ''

	if whi eq 0 then begin
		wdv0 = Mat_proj([0,1d,0],/com)##([amult*dv0,0,0])[0:2]
		str = string(wdv0/amult,form=frm)
		egtit = $
		'Beam ang. error: ['+strjoin(strcompress(str,/rem),', ')+']' + uni
	endif else wdv0 = [0d,0,0]

	if whi eq 1 then begin
		if n_elements(dc1) eq 1 then begin
			wdc1 = amult*dc1[0]
			if keyword_set(bal) then wdc2 = - sin(phi1)/sin(phi2)*wdc1
			str = string(wdc1/amult,form=frm)
			egtit = '1!ust!n crystal !7v!x error: ' + str + uni
		endif else message, 'DC1 must be a scalar!'
	endif else wdc1 = 0d

	if whi eq 2 then begin
		if n_elements(dc2) eq 1 then begin
			wdc2 = amult*dc2[0]
			str = string(wdc2/amult,form=frm)
			egtit = '2!und!n crystal !7v!x error: ' + str + uni
		endif else message, 'DC2 must be a scalar!'
	endif else if not keyword_set(bal) then wdc2 = 0d

	if Isnum(dq3) and (Type(egtit) eq 7) then begin
		if n_elements(dq3) eq 1 then begin
			wdq3 = Cast([0,0,dq3],5)
			str = string(dq3,form='(f6.3)')
			eegtit = 'Surface offset: ' + str + ' mm'
			egtit = [egtit,eegtit]
			gtfl = 1
		endif else message, 'DQ3 must be a scalar!'
	endif else wdq3 = [0d,0,0]

	fpsi = 2*phi1 - phi2
	spsi = 2*(phi2-phi1)
	alp = asin(sin(wchi)*sin(spsi))
	ttet = atan(cos(wchi)*tan(spsi))

	for i = 0, nchi-1 do begin
		prot = Mat_rot(wchi[i],ax=[0,1d,0])
		nrot = transpose(prot)

		n1 = Mat_rot(wdc1,ax=[0,1d,0])##[1d,0,0]
		n1 = Mat_rot(-phi1,ax=[0,0,1d])##n1
		n2 = Mat_rot(wdc2,ax=[0,1d,0])##[1d,0,0]
		n2 = Mat_rot(!dpi-fpsi,ax=[0,0,1d])##n2
		n3 = prot##[0,0,1d]

		v0 = [0,1d,0] + wdv0
		v0 = prot##(v0/Vnorm(v0))
		v1 = Mat_refl(n1)##v0
		v2 = Mat_refl(n2)##v1

		sc01 = Mat_proj(v0,n1,/com)
		sc12 = Mat_proj(v1,n2,/com)
		sc23 = Mat_proj(v2,n3,/com)

		q1 = [0d,0,0]
		q2 = $
		wlen*sin(spsi)/sin(2*phi2)*[sin(2*phi1),cos(2*phi1),0]
		q3 = prot##(wlen*[0,1d,0] + wdq3)

		p0 = prot##([0d,0,0])
		p1 = q1 + sc01##(p0-q1)
		p2 = q2 + sc12##(p1-q2)
		p3 = q3 + sc23##(p2-q3)

		if defl then begin
			pdrot = Mat_rot(ttet[i],ax=[0,0,1d])##Mat_rot(alp[i],ax=[1d,0,0])
			ndrot = transpose(pdrot)
			n4 = prot##pdrot##[0,1d,0]
			v3 = Mat_refl(n3)##v2
			sc34 = Mat_proj(v3,n4,/com)
			q4 = prot##(wlen*[0,1d,0] + wdln*pdrot##[0,1d,0])
			p4 = q4 + sc34##(p3-q4)
			off = ndrot##nrot##(p4 - q4)
			toff[i] = off[0]
			loff[i] = off[2]	
		endif else begin
			off = nrot##(p3 - q3)
			yy = [-sin(ttet[i]),cos(ttet[i]),0]
			xx = crossp(yy,[0,0,1d])
			toff[i] = Vinp(off,xx)
			loff[i] = Vinp(off,yy)
			v2_lab = nrot##v2
			dalp[i] = -asin(v2_lab[2]) - alp[i]
			dttet[i] = -atan(v2_lab[0],v2_lab[1]) - ttet[i]
		endelse
	endfor
	toff[where(abs(toff) lt Toler(),/null)] = 0d
	loff[where(abs(loff) lt Toler(),/null)] = 0d
	dalp[where(abs(dalp) lt Toler(),/null)] = 0d
	dttet[where(abs(dttet) lt Toler(),/null)] = 0d

	chi = Cast(wchi/amult,typ,typ)
	toff = Cast(toff,typ,typ)
	loff = Cast(loff,typ,typ)
	if not defl then begin
		dalp = Cast(dalp/amult,typ,typ)
		dttet = Cast(dttet/amult,typ,typ)
	endif else dttet = (dalp = [])

	if defl then begin
		ftit = 'Detector, horizontal error'
		stit = 'Detector, vertical error'
	endif else begin
		ftit = 'Sample, transverse error'
		stit = 'Sample, longitudinal error'
	endelse

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
			if defl then gtit = string(ene,len,dln,$
			form='(f5.2," keV; Len =",f6.1,"mm, Dlen =",f6.1, "mm" )') $
			else gtit = string(ene,len,$
			form='(f5.2," keV; Length = ",f7.2," mm")')
			gtit = [gtit,egtit]
			if who then begin
				pvar = wqz
				xtit = 'Q!dz!n (A!u-1!n)'
			endif else begin
				pvar = chi
				xtit = '!7v!x' + uni
			endelse
			if anfl then begin
				ftit = '2!7h!x-error'
				stit = '!7a!x-error'
				ytit = 'Error' + uni
				fir = dttet
				sec = dalp
			endif else begin
				ytit = 'Error (mm)'
				fir = toff
				sec = loff
			endelse
			pcol = !pcol.dred
			xlab = replicate(0.5,2+gtfl)

			Plvar_keep, act = 'sav'
			!x.margin = [11,3]
			window, dwin, xsi = xysi[0], ysi = xysi[1]
			!p.region = pp[*,0]
			plot, pvar, fir, /nodata, /ynoz, xstyle= 1, $
				tit= ftit, xtit= xtit, ytit=ytit, _extra=_e
			oplot, pvar, fir, thi=2, col=pcol
			!p.region = pp[*,1]
			plot, pvar, sec, /nodata, /ynoz, xstyle= 1, /noerase, $
				tit= stit, xtit= xtit, ytit=ytit, _extra=_e
			oplot, pvar, sec, thi=2, col=pcol
			dum = Wherinstruct('chars',_e)
			if dum ge 0 then _e.(dum) = 1.2*_e.(dum)
			Labels, xlab,ylab,gtit, align= 0.5, /normal, charsize=1.5, _extra=_e
			Plvar_keep, act = 'res'
		endif else begin
			if keyword_set(pre) then begin
				pform='f11.6," mm")'
				aform = 'g10.4,"' + suni + '")'
			endif else  begin
				pform='f9.4," mm")'
				aform = 'g8.2,"' + suni + '")'
			endelse
			print
			print, toff, form = '("	' + ftit + '	= ",' + pform
			print, loff, form = '("	' + stit + '	= ",' + pform
			print
			print, dttet, form = '("	2-theta error		= ",' + aform
			print, dalp, form= '("	Alpha error		= ",' + aform
			print
		endelse
	endif

	return
end