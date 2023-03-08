Pro DCM_bpic, ang, voff= vof, aperture= ape, border= bor, _extra= _e

	on_error, 1

	Box, [-50,350], [-100,300], tru='ylo', xmar=[3,3], ymar=[2,2], border=bor

	rang = !dtor*Default(ang,0.)
	vof = Default(vof,100.,/dtyp)
	lof = Default(lof,5.,/dtyp)
	ape = Default(ape,0.,/dtyp)
	flen = 40.
	slen = 40.

	wof = vof
	hof = vof/tan(2*rang)
	croff = vof*[1/tan(2*rang),1]
	Rectan, xli = flen*[-0.5,0.5], yli=[-20,0], /no_sho, shape = fcr
	Rectan, xli = slen*[-0.5,0.5], yli = [0,20], /no_sho, shape=scr
	Rectan, xli=[0,350], yli= vof + [-5,5], /no_sho, shape=rail
	rfcr = Shape_trans(fcr,rang)
	rscr = shape_trans(scr,rang,1,croff)

	fsur = transpose([[flen*[-0.5,0.5]],[0,0]])
	ssur = transpose([[slen*[-0.5,0.5]],[0,0]])
	afsur = transpose([[flen*[-0.5,-0.5]],[-20,0]])
	assur = transpose([[slen*[-0.5,-0.5]],[0,20]])
	rfsur = Shape_trans(fsur,rang)
	rssur = Shape_trans(ssur,rang,1,croff)
	rafsur = Shape_trans(afsur,rang)
	rassur = Shape_trans(assur,rang,1,croff)


	polyfill, rail, col = !pcol.dred
	Circle_mm, cen=[0,0],rad=30,thi=4, col=!pcol.purple
	Circle_mm, cen=croff, rad=30, thi=4, col=!pcol.purple
	polyfill, rfcr, col = !pcol.lblue
	plots, rfcr, col = !pcol.blue, thi=2, _extra = _e
	polyfill, rscr, col = !pcol.lblue
	plots, rscr, col = !pcol.blue, thi=2, _extra = _e

;	plots, rfsur, thi=2, col=!pcol.pink
;	plots, rssur, thi=2, col=!pcol.pink
;	plots, rafsur, thi=2, col=!pcol.pink
;	plots, rassur, thi=2, col=!pcol.pink

	if rang gt 0 then begin
		hval = 0.5*[-ape,ape,0]
		thi = [2,2,2]
		cols = [!pcol.red,!pcol.red,!pcol.green]
		for i = 0, 2 do begin
			fray = [[-100,hval[i]],[1,0]]
			fcc = Lincross(fray,rfsur,lin=1,cross=fcloc)
			if fcc then begin
				fseg = [[fray[*,0]],[fcloc]]
				plots, fseg, thi=thi[i], col=cols[i], noclip=0, _extra = _e
				plots, fcloc, psym=8, col=!pcol.cyan, _extra = _e
				sray = [[fcloc],[cos(2*rang),sin(2*rang)]]
				scc = Lincross(sray,rssur,lin=1,cross=scloc)
				if scc then begin
					sseg = [[sray[*,0]],[scloc]]
					plots, sseg, thi=thi[i], col=cols[i], _extra = _e
					plots, scloc, psym=8, col=!pcol.cyan, _extra = _e
					tseg = [[scloc],[400,scloc[1]]]
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