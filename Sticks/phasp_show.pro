Pro Phasp_show, beam, distance = dst, step = stp, foclen = foc

	on_error, 1

	z = Make_grid([0,dst],stp,/step,dim=n)
	for i = 0, n[0]-1 do begin
		wbeam = Phasp_prop(beam,z[i])
		Box, [-1,1], [-1,1], truasp='xcen', xmar=[2,2], ymar=[2,2]
		polyfill, wbeam, col=!pcol.cyan
		axis, 0, 0, /xaxis, xstyle=1, xtickname = ['',' ',' ',' ','']
		axis, 0, 0, /yaxis, ystyle=1, ytickname = replicate(' ',4)
	endfor

	fbeam = Phasp_foc(wbeam,foc)
	xsav = total(fbeam[0,*]^2)
	ysav = total(fbeam[1,*]^2)
	xyav = total(product(fbeam,1))
	udst = -xyav/ysav
	print, udst
	z = Make_grid([0,udst],stp,/step,dim=n)
	for i = 0, n[0]-1 do begin
		wbeam = Phasp_prop(fbeam,z[i])
		Box, [-1,1], [-1,1], truasp='xcen', xmar=[2,2], ymar=[2,2]
		polyfill, wbeam, col=!pcol.cyan
		axis, 0, 0, /xaxis, xstyle=1, xtickname = replicate(' ',4)
		axis, 0, 0, /yaxis, ystyle=1, ytickname = replicate(' ',4)
	endfor

	return
end