Pro Mir_study, beam, foclen, fwhm = fwhm, _extra = _e

	on_error, 1

	mult = 1/sqrt(4*!pi)
	if keyword_set(fwhm) then mult = mult*sqrt(8*alog(2))
	ind_x = [10,16,17]
	ind_xp = 17
	nf = n_elements(foclen)
	sig_x = fltarr(3,nf)
	sig_xp = fltarr(nf)
	for i = 0, nf-1 do begin
		tbeam = Beam_edit(beam,modi=4,val=foclen[i])
		sig_x[*,i] = Hsize(tbeam.xsec[2*ind_x].bpars)
		sig_xp[i] = Asize(tbeam.xsec[2*ind_xp].bpars)
	endfor
	sig_x = mult*sig_x
	sig_xp = mult*sig_xp
	sig_x_ini = mult*Hsize(beam.xsec[0].bpars)
	sig_xp_ini = mult*Asize(beam.xsec[2].bpars)
	tbeam = Beam_edit(beam,off=4)
	sig_x_off = mult*Hsize(tbeam.xsec[2*ind_x].bpars)
	sig_xp_off = mult*Asize(tbeam.xsec[2*ind_xp].bpars)

	window, 0
	ymin = min(sig_x) < sig_x_ini
	ymax = max(sig_x) > max(sig_x_off)
	col = [!pcol.cyan,!pcol.red,!pcol.green,!pcol.blue]
	plot, foclen, sig_x[0,*], /nodata, yran=[ymin,ymax], $
	xtit = 'Focal length (m)', ytit = 'Beam size (!7l!xm)', _extra = _e
	oplot, foclen, sig_x[0,*], col=col[1]
	oplot, foclen, sig_x[1,*], col=col[2]
	oplot, foclen, sig_x[2,*], col=col[3]
	oplot, foclen, replicate(sig_x_ini,nf), col = col[0], line=1
	oplot, foclen, replicate(sig_x_off[0],nf), col=col[1], line=2
	oplot, foclen, replicate(sig_x_off[1],nf), col=col[2], line=2
	oplot, foclen, replicate(sig_x_off[2],nf), col=col[3], line=2
	sloc = Wherinstruct('char',_e)
	if sloc ge 0 then chs = _e.(sloc) else chs = 1
	Legend_mm, loc = 'lr', text = ['Source','14IDB','14IDC','14IDD'], char = chs, $
		col = col,line=[1,0,0,0]

	window, 1
	plot, foclen, sig_xp, /nodata, $
	xtit = 'Focal length (m)', ytit = 'Beam angular size (!7l!xr)', _extra = _e
	oplot, foclen, sig_xp, col = !pcol.purple
	oplot, foclen, replicate(sig_xp_ini,nf), col = !pcol.purple, line = 2

	wset, 0

	return
end
