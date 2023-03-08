Pro DCM_pic, ang, voff= vof, loff= lof, aperture= ape, border= bor, _extra= _e

	on_error, 1

	Box, [-150,300], [-100,300], tru='ylo', xmar=[3,3], ymar=[2,2], border=bor

	rang = !dtor*Default(ang,0.)
	vof = Default(vof,15.,/dtyp)
	lof = Default(lof,5.,/dtyp)
	ape = Default(ape,1.,/dtyp)
	flen = 150.
	slen = 170.

	wof = vof/(2*cos(rang))
	Rectan, xli = flen*[-0.8,0.2], yli=[-30,0], /no_sho, shape = fcr
	Rectan, xli = lof+[0,slen], yli = wof+[0,30], /no_sho, shape=scr
	rfcr = Shape_trans(fcr,rang)
	rscr = shape_trans(scr,rang)

	fsur = transpose([[flen*[-0.75,0.25]],[0,0]])
	ssur = transpose([[lof+ [0,slen]],[wof,wof]])
	afsur = transpose([[flen*[-0.75,-0.75]],[-30,0]])
	assur = transpose([[lof*[1,1]],[wof+[0,30]]])
	rfsur = Shape_trans(fsur,rang)
	rssur = Shape_trans(ssur,rang)
	rafsur = Shape_trans(afsur,rang)
	rassur = Shape_trans(assur,rang)

	polyfill, rfcr, col = !pcol.lblue
	plots, rfcr, col = !pcol.blue, thi=2, _extra = _e
	polyfill, rscr, col = !pcol.lblue
	plots, rscr, col = !pcol.blue, thi=2, _extra = _e

	if rang gt 0 then begin
		hval = 0.5*[-ape,ape,0]
		thi = [4,4,2]
		cols = [!pcol.red,!pcol.red,!pcol.green]
		for i = 0, 2 do begin
			fray = [[-150,hval[i]],[1,0]]
			fcc = Lincross(fray,rfsur,lin=1,cross=fcloc)
			if fcc then begin
				fseg = [[fray[*,0]],[fcloc]]
				plots, fseg, thi=thi[i], col=cols[i], _extra = _e
				plots, fcloc, psym=8, col=!pcol.cyan, _extra = _e
				sray = [[fcloc],[cos(2*rang),sin(2*rang)]]
				scc = Lincross(sray,rssur,lin=1,cross=scloc)
				if scc then begin
					sseg = [[sray[*,0]],[scloc]]
					plots, sseg, thi=thi[i], col=cols[i], _extra = _e
					plots, scloc, psym=8, col=!pcol.cyan, _extra = _e
					tseg = [[scloc],[300,scloc[1]]]
					plots, tseg, thi=thi[i], col=cols[i], noclip=0, _extra = _e
				endif else begin
					ascc = Lincross(sray,rassur,lin=1,cross=ascloc)
					if ascc then begin
						asseg = [[sray[*,0]],[ascloc]]
						plots, asseg, thi=thi[i], col=cols[i], _extra = _e
						plots, ascloc, psym=8, col=!pcol.cyan, _extra = _e
					endif else begin
						asseg = [[sray[*,0]],[sray[*,0] + 1000*sray[*,1]]]
						plots, asseg, line=2, thi=thi[i], col=cols[i], noclip=0, $
						_extra = _e
					endelse	
				endelse
			endif else begin
				afcc = Lincross(fray,rafsur,lin=1,cross=afcloc)
				if afcc then begin
					afseg = [[fray[*,0]],[afcloc]]
					plots, afseg, thi=thi[i], col=cols[i], _extra = _e
					plots, afcloc, psym=8, col=!pcol.cyan, _extra = _e
				endif else begin
					afseg = [[fray[*,0]],[fray[*,0] + 1000*fray[*,1]]]
					plots, afseg, line=2, thi=thi[i], col=cols[i], noclip=0, $
					_extra = _e
				endelse
			endelse
		endfor
	endif

	return
end