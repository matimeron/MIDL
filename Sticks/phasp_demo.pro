Pro Phasp_demo, box= box, title= tit, rad= rad, dist=dst, focl=fcl, nstep=nst, $
	wait = wai, save = sav, rate = rat, _extra = _e

	on_error, 1
	brep = 30
	ticn = replicate(' ',30)
	ticl = 0.01

	box, box[0]*[-1,1], box[1]*[-1,1], xmargin=[4,4], ymargin=[3,3], tit= tit, $
		charsize = 1.2, charthi = 1.5
	ang = make_grid([0,2*!pi],181)
	elli = transpose([[rad[0]*cos(ang)],[rad[1]*sin(ang)]])

	stp = 1.*dst/nst
	wai = Default(wai,1)
	sfl = keyword_set(sav)
	if sfl then begin
		frep = round(brep/Default(rat,1)) > 1
		handle = mpeg_open([!d.x_size,!d.y_size],_extra=_e)		
	endif

	for i = 0, nst do begin
		polyfill, elli, _extra = _e
		axis, 0, 0, xaxis = 0, xtickname = ticn, ticklen = ticl, $
		xtit = '                        x', charsize = 1.5, charthi=1.5
		axis, 0, 0, yaxis = 0, ytickname = ticn, ticklen = ticl, $
		ytit = '                        !7h!x!n', charsize = 1.5, charthi=1.5
		if sfl then mpeg_put, handle, window= !d.window, frame= i*frep+1, /ord $
		else wait, Default(wai,0.1)
		if i ne nst then begin
			polyfill, elli, col = !pcol.white
			elli[0,*] = elli[0,*] + stp*elli[1,*]
		endif
	endfor

	polyfill, elli, col = !pcol.white
	elli[1,*] = elli[1,*] - elli[0,*]/fcl
	edst = $
	1.*fcl*(rad[0]^2 - (fcl-dst)*dst*rad[1]^2)/(rad[0]^2 + (fcl-dst)^2*rad[1]^2)
	enst =  nst/2 > ceil(edst/stp) < 2*nst
	estp = 1.*edst/enst

	for i = 0, enst do begin
		j = i + nst + 1
		polyfill, elli, _extra = _e
		axis, 0, 0, xaxis = 0, xtickname = ticn, ticklen = ticl, $
		xtit = '                        x', charsize = 1.5, charthi=1.5
		axis, 0, 0, yaxis = 0, ytickname = ticn, ticklen = ticl, $
		ytit = '                        !7h!x!n', charsize = 1.5, charthi=1.5
		if sfl then mpeg_put, handle, window= !d.window, frame= j*frep+1, /ord $
		else wait, Default(wai,0.1)
		if i ne enst then begin
			polyfill, elli, col = !pcol.white
			elli[0,*] = elli[0,*] + estp*elli[1,*]
		endif
	endfor

	if sfl then begin
		mpeg_put, handle, window= !d.window, frame= 0, /ord
		mpfile = File_get(fnam,stat=stat,/write,/over,def='mpg',_extra=_e)
		if stat then mpeg_save, handle, file = mpfile
		mpeg_close, handle
	endif

	return
end
	