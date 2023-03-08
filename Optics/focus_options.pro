Pro Focus_options, ls, li, zran = zrn, lscale = lsc, rsize = rsz, asize = asz,$
	display = dis, _extra = _e

	on_error, 1

	nsi = [n_elements(ls),n_elements(li)]
	if min(nsi) ne 1 then message, 'Unacceptable inputs!'
	if n_elements(zrn) eq 2 then zfl = 1 else zfl = 0

	check = How_many(fir=lsc,sec=rsz,thi=asz,which=whi)
	case whi of
		0	:	wlsc = 1.
		1	:	wlsc = 1.*lsc
		6	:	wlsc = 1.*rsz/asz
		else:	message, 'Unacceptable parameter combination!'
	endcase

	zis = ls/wlsc
	zii = li/wlsc

	if (max(nsi,whi) gt 1) then begin
		if whi then begin
			if zfl then begin
				dum = where(zii ge min(zrn,max=max) and zii le max)
				zii = zii[dum]
			endif
			zis = zis[0]
			zco = zii
			tit = '!7f!x!ds!n = ' + string(zis,form = '(f0.2)')
			xtit = '!7f!x'
			sfl = 1
		endif else begin
			if zfl then begin
				dum = where(zis ge min(zrn,max=max) and zis le max)
				zis = zis[dum]
			endif
			zco = zis
			zii = zii[0]
			tit = '!7f!x = ' + string(zii,form = '(f0.2)')
			xtit = '!7f!x!ds!n'
			sfl = 0
		endelse
		hs = 1 + zis^2

		phi = zis*zii/(zis+zii)
		sizi = sqrt(zii^2/(zis^2 > Toler()))

		dum = where(hs ge abs(2*zii), ndum)
		if ndum gt 0 then begin
			zcof = zco[dum]
			hsf = hs[dum]
			if sfl then begin
				zisf = zis
				ziif = zcof
			endif else begin
				zisf = zcof
				ziif = zii
			endelse
			tem = hsf + sqrt(hsf^2 - 4*ziif^2)
			phf = 2*ziif*hsf/(tem + 2*zisf*ziif)
			sizf = sqrt(2*ziif^2/tem)
			ffl = 1
		endif else ffl = 0

		phm = zii*hs/(zis*zii + hs)
		sizm = sqrt(zii^2/hs)

		posib = ['siz','foc']
		wha = Strmatch_mm(dis,posib,2) > 0
		ltext = ['Imaging','Focusing','Minimum']
		lcol = [!pcol.blue, !pcol.red, !pcol.green]
		llin = [0,0,0]
		dum = (Wherinstruct('charsi',_e))[0]
		if dum ge 0 then lchs = (_e.(dum) + 1.)/2 else lchs = 1.
		if wha then begin
			yran = [min(phi) < min(phm) < min(Default(phf,0)), $
				max(phi) > max(phm) > max(Default(phf,0))]
			plot, zco, phi, yran = yran, /nodata, tit = tit, $
				xtit = xtit, ytit = '!7u!x', _extra = _e
			oplot, zco, phi, col = !pcol.blue
			if ffl then oplot, zcof, phf, col = !pcol.red
			oplot, zco, phm, col = !pcol.green
			Legend_mm, loc= 'LR', text= ltext, line=llin, col=lcol, charsi=lchs
		endif else begin
			yran = [min(sizi) < min(sizm) < min(Default(sizf,0)), $
				max(sizi) > max(sizm) > max(Default(sizf,0))]
			plot, zco, sizi, yran = yran, /nodata, tit = tit, $
				xtit = xtit, ytit = '!7g!x', _extra = _e
			oplot, zco, sizi, col = !pcol.blue
			if ffl then oplot, zcof, sizf, col = !pcol.red
			oplot, zco, sizm, col = !pcol.green
			if whi then lloc = 'LR' else lloc = 'UR'
			Legend_mm, loc= lloc, text= ltext, line=llin, col=lcol, charsi=lchs
		endelse
	endif else message, 'Need more points', /con

	return
end